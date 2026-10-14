module Feature.Move exposing (init, retire, update, reconcile, view, viewAction, filterQuery, destinationReason, moveFields, visibleRows)

import Api
import Feature.DataLoading
import Dict exposing (Dict)
import Feature.Mutations
import Feature.WebSocket
import Helpers
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Permissions
import Set exposing (Set)
import Types exposing (..)
import Url


init : MoveModel
init =
    { dialog = Nothing, resumeOrdinary = False, refreshOrdinary = False, nextToken = 1, nextReadId = 1, admissions = Dict.empty }


{-| Retirement never releases physical HTTP occupancy. Exact callbacks do that.
Counters survive workspace, authorization, filter, collapse and dialog changes.
-}
retire : MoveModel -> MoveModel
retire state =
    { state | dialog = Nothing, resumeOrdinary = True }


emptyBranch : MoveBranch
emptyBranch =
    { ids = [], offset = 0, complete = False, pending = Nothing, error = Nothing, lastIssued = 0, staged = Nothing }


{-| No status parameters would mean all statuses to the server. An intentionally
empty filter instead has locally complete, empty destination membership; focus
still supplies canonical location context and readiness.
-}
emptyFilteredBranches : MoveDialog -> MoveDialog
emptyFilteredBranches dialog =
    if Set.isEmpty dialog.statuses then
        { dialog | branches = Dict.map (\_ branch -> { branch | ids = [], offset = 0, complete = True, pending = Nothing, error = Nothing, staged = Nothing }) dialog.branches
            , stagedProjects = Dict.empty
        }
    else dialog


setDialog : MoveDialog -> Model -> Model
setDialog dialog model =
    let state = model.move in
    { model | move = { state | dialog = Just (align model.move.dialog dialog) } }


reconcile : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
reconcile ( model, command ) =
    case model.move.dialog of
        Nothing ->
            if model.move.resumeOrdinary || (model.move.refreshOrdinary && Dict.isEmpty model.move.admissions) then
                let
                    state = model.move
                    ready = { model | move = { state | resumeOrdinary = False, refreshOrdinary = state.refreshOrdinary && not (Dict.isEmpty state.admissions) } }
                    ( refreshed, refresh ) = if state.refreshOrdinary && Dict.isEmpty state.admissions then Feature.DataLoading.revalidateNavigationForFilters ready else ( ready, Cmd.none )
                    ( next, navigation ) = Feature.DataLoading.pumpNavigation refreshed
                    ( detailed, details ) = Feature.DataLoading.ensurePresentedCardDetails next
                in
                ( detailed, Cmd.batch [ command, refresh, navigation, details ] )
            else ( model, command )
        Just dialog ->
            if dialog.refreshRequested && dialog.saving == Nothing
                && dialog.guard.sessionEpoch == model.sessionRequestEpoch
                && dialog.guard.routeWorkspace == model.selectedWorkspaceId
                && (model.sessionContext |> Maybe.map (.principal >> .actorId >> (==) dialog.guard.audienceId) |> Maybe.withDefault False)
                && Permissions.canEditCurrentWorkspace model && targetExists dialog model then
                case Helpers.prepareProjectRequest model.selectedWorkspaceId model.sessionRequestEpoch dialog.guard.targetKey model of
                    Just ( guard, guarded ) ->
                        reconcile (setDialog (beginRefresh { dialog | guard = guard }) guarded, command)
                    Nothing -> ( close model, command )
            else if not (Permissions.canEditCurrentWorkspace model)
                || not (Helpers.projectRequestIsCurrent dialog.guard model)
                || not (targetExists dialog model) then
                let state = model.move in
                reconcile ( { model | move = retire state }, command )
            else
                let
                    ( next, reads ) = pump model
                    ( navigated, navigation ) = Feature.DataLoading.pumpNavigation next
                in
                ( navigated, Cmd.batch [ command, reads, navigation ] )


targetExists : MoveDialog -> Model -> Bool
targetExists dialog model =
    if dialog.entityType == "project" then
        Dict.get dialog.entityId model.projects |> Maybe.map (\p -> Just p.workspaceId == model.selectedWorkspaceId) |> Maybe.withDefault False
    else
        Dict.get dialog.entityId model.tasks |> Maybe.map (\t -> Just t.workspaceId == model.selectedWorkspaceId) |> Maybe.withDefault False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenMove kind id ->
            if Permissions.canEditCurrentWorkspace model then
                open kind id model
            else ( model, Cmd.none )

        CloseMove ->
            let state = model.move in
            ( { model | move = retire state }, Cmd.none )

        MoveBranchFetched readId result ->
            finishRead readId (acceptBranch readId result) model

        MoveFocusFetched readId result ->
            finishRead readId (\request -> if request.branch == "@selection" then acceptSelection readId result request else acceptFocus readId result request) model

        MoveMatchFetched readId result ->
            finishRead readId (acceptMatch readId result) model

        MoveProjectSaved token guard result ->
            if mutationCurrent token guard model then
                case result of
                    Ok mutation ->
                        if mutation.project.id == (model.move.dialog |> Maybe.map .entityId |> Maybe.withDefault "")
                            && Just mutation.project.workspaceId == model.selectedWorkspaceId then
                            Feature.Mutations.update (ProjectUpdated guard result) (close model)
                                |> (\( next, command ) -> ( Helpers.finishProjectOperation guard next, command ))
                        else failed "The move response did not match this project. Reopen the chooser to refresh." model
                    Err error ->
                        failed (failureMessage error) (Helpers.finishProjectOperation guard model)
            else ( Helpers.finishProjectOperation guard model, Cmd.none )

        MoveTaskSaved token guard result ->
            if mutationCurrent token guard model then
                case result of
                    Ok mutation ->
                        if mutation.task.id == (model.move.dialog |> Maybe.map .entityId |> Maybe.withDefault "")
                            && Just mutation.task.workspaceId == model.selectedWorkspaceId then
                            let
                                old = model.move.dialog |> Maybe.andThen .currentProject
                                destination = mutation.task.projectId
                                ( canonical, command ) = Feature.Mutations.update (TaskUpdated result) (close model)
                                ( refreshed, refreshCmd ) = Feature.WebSocket.refreshTaskPlacement mutation.task.id (model.move.dialog |> Maybe.andThen .taskParent) old destination canonical
                            in
                            ( refreshed, Cmd.batch [ command, refreshCmd ] )
                        else failed "The move response did not match this task. Reopen the chooser to refresh." model
                    Err error -> failed (failureMessage error) model
            else ( model, Cmd.none )

        _ ->
            case model.move.dialog of
                Nothing -> ( model, Cmd.none )
                Just dialog ->
                    if dialog.saving /= Nothing || not (Helpers.projectRequestIsCurrent dialog.guard model) then
                        ( model, Cmd.none )
                    else
                        case msg of
                            MoveQueryChanged query -> resetFilters { dialog | query = query } model
                            MoveStatusChanged status ->
                                if List.member status [ "active", "paused", "completed", "archived" ] then
                                    resetFilters { dialog | statuses = if Set.member status dialog.statuses then Set.remove status dialog.statuses else Set.insert status dialog.statuses } model
                                else ( model, Cmd.none )
                            MoveToggleBranch id ->
                                if Set.member id dialog.expanded then
                                    let
                                        hidden = \branch -> branch /= "" && isWithin dialog.projects id branch
                                        branches = Dict.filter (\branch _ -> not (hidden branch)) dialog.branches
                                    in
                                    pump (setDialog { dialog | expanded = Set.filter (\key -> not (isWithin dialog.projects id key)) dialog.expanded, branches = branches, rowOffset = 0
                                        , matchPending = Dict.filter (\key _ -> key == id || not (isWithin dialog.projects id key)) dialog.matchPending
                                        } model)
                                else if Dict.member id dialog.projects then
                                    pump (setDialog { dialog | expanded = Set.insert id dialog.expanded, branches = Dict.update id (Maybe.withDefault emptyBranch >> Just) dialog.branches } model)
                                else ( model, Cmd.none )
                            MoveSelect destination ->
                                if destinationReason model dialog destination == Nothing then
                                    pump (setDialog (settle model { dialog | selected = Just destination, selectedPath = Set.empty, selectionValidation = if dialog.revalidating then Maybe.map emptySelection destination else Nothing, error = Nothing }) model)
                                else ( model, Cmd.none )
                            MoveRows offset ->
                                pump (setDialog { dialog | rowOffset = clamp 0 (Basics.max 0 (List.length (visibleRows dialog) - 25)) offset } model)
                            MoveScroll top ->
                                let
                                    index = Basics.max 0 (floor ((top - 56) / 56))
                                    anchor = visibleRows dialog |> List.drop index |> List.head |> Maybe.map Tuple.first
                                    offset = clamp 0 (Basics.max 0 (List.length (visibleRows dialog) - 25)) (index - 3)
                                in
                                pump (setDialog { dialog | rowOffset = offset, anchor = anchor, anchorDelta = top - toFloat ((index + 1) * 56), scrollTop = top } model)
                            MoveRowFocused id ->
                                ( setDialog { dialog | activeRow = Just id } model, Cmd.none )
                            MoveFocusRow index ->
                                let
                                    rows = visibleRows dialog
                                    bounded = clamp 0 (Basics.max 0 (List.length rows - 1)) index
                                    target = rows |> List.drop bounded |> List.head |> Maybe.map Tuple.first
                                    offset = clamp 0 (Basics.max 0 (List.length rows - 25)) (bounded - 3)
                                    next = { dialog | rowOffset = offset, activeRow = target, focusTarget = target, revealTarget = target, reveal = dialog.reveal + 1 }
                                in
                                pump (setDialog next model)
                            MoveRetry branch ->
                                if branch == "@focus" then
                                    resetFilters { dialog | focusError = Nothing, focusOffset = 0, focusComplete = False, taskParentName = Nothing, projects = Dict.empty } model
                                else if branch == "@selection" then
                                    pump (setDialog { dialog | selectionValidation = Maybe.map (.id >> emptySelection) dialog.selectionValidation } model)
                                else if String.startsWith "@match:" branch then
                                    pump (setDialog { dialog | matchErrors = Dict.remove (String.dropLeft 7 branch) dialog.matchErrors } model)
                                else
                                    pump (setDialog { dialog | branches = Dict.update branch (Maybe.map (\b -> { b | error = Nothing })) dialog.branches } model)
                            MoveConfirm -> confirm dialog model
                            _ -> ( model, Cmd.none )


close : Model -> Model
close model =
    let state = model.move in
    { model | move = retire state }


failed : String -> Model -> ( Model, Cmd Msg )
failed message model =
    case model.move.dialog of
        Just dialog -> ( setDialog { dialog | saving = Nothing, error = Just message } model, Cmd.none )
        Nothing -> ( model, Cmd.none )


failureMessage : Api.ApiError -> String
failureMessage error =
    Api.apiErrorToUserMessage "Could not move this item." error
        ++ " Dependencies, including closed dependencies, must remain in the same project. Related tasks are never moved automatically. Check destination lifecycle and permissions, then retry or choose another project."


mutationCurrent : Int -> CanonicalRequestGuard -> Model -> Bool
mutationCurrent token guard model =
    Permissions.canEditCurrentWorkspace model
        && Helpers.projectRequestIsCurrent guard model
        && (model.move.dialog |> Maybe.map (\d -> d.token == token && d.guard == guard && d.saving /= Nothing && targetExists d model) |> Maybe.withDefault False)


open : String -> String -> Model -> ( Model, Cmd Msg )
open kind id model =
    let
        target =
            if kind == "project" then
                Dict.get id model.projects |> Maybe.map (\p -> ( p.name, p.parentId, Nothing ))
            else if kind == "task" then
                Dict.get id model.tasks |> Maybe.map (\t -> ( t.title, t.projectId, t.parentId ))
            else Nothing
    in
    case ( target, Helpers.prepareProjectRequest model.selectedWorkspaceId model.sessionRequestEpoch ("move:" ++ kind ++ ":" ++ id) model ) of
        ( Just ( name, current, parent ), Just ( guard, guarded ) ) ->
            let
                state = guarded.move
                dialog =
                    { token = state.nextToken, guard = guard, entityType = kind, entityId = id, name = name
                    , currentProject = current, taskParent = parent, taskParentName = Nothing
                    , sourceOpen = if kind == "project" then Dict.get id model.projects |> Maybe.map (\p -> List.member p.status [ Api.ProjActive, Api.ProjPaused ]) |> Maybe.withDefault True else Dict.get id model.tasks |> Maybe.map (\t -> not (List.member t.status [ Api.Done, Api.Cancelled ])) |> Maybe.withDefault True
                    , sourceRunning = False
                    , query = "", statuses = Set.fromList [ "active", "paused" ], generation = 0
                    , expanded = Set.empty, projects = Dict.empty, matches = Dict.empty, matchPending = Dict.empty, matchErrors = Dict.empty, branches = Dict.singleton "" emptyBranch
                    , selected = Nothing, selectedPath = Set.empty, selectionValidation = Nothing, revealTarget = Nothing, focusTarget = Nothing, rowOffset = 0, activeRow = Nothing, anchor = Nothing, anchorDelta = 0, scrollTop = 0, layout = 0, reveal = 0, revealPending = True, refreshRequested = False, revalidating = False, contextIds = Set.empty, stagedProjects = Dict.empty, focusOffset = 0, focusPending = Nothing, focusComplete = False, focusError = Nothing
                    , saving = Nothing, error = Nothing
                    }
            in
            pump { guarded | move = { state | dialog = Just dialog, nextToken = state.nextToken + 1 } }
        _ -> ( model, Cmd.none )


resetFilters : MoveDialog -> Model -> ( Model, Cmd Msg )
resetFilters dialog model =
    pump (setDialog (unfoldContext { dialog | generation = dialog.generation + 1, branches = Dict.singleton "" emptyBranch, selected = Nothing, selectedPath = Set.empty, selectionValidation = Nothing, revealTarget = Nothing, focusTarget = Nothing, rowOffset = 0, activeRow = Nothing, anchor = Nothing, scrollTop = 0, layout = dialog.layout + 1, revealPending = True, stagedProjects = Dict.empty, revalidating = False, error = Nothing, focusPending = Nothing, matches = Dict.empty, matchPending = Dict.empty, matchErrors = Dict.empty }) model)


beginRefresh : MoveDialog -> MoveDialog
beginRefresh dialog =
    { dialog | generation = dialog.generation + 1, refreshRequested = False, revalidating = True
        , branches = Dict.map (\_ branch -> { branch | offset = 0, complete = False, pending = Nothing, error = Nothing, staged = Just [] }) dialog.branches
        , stagedProjects = Dict.empty, focusComplete = False, focusPending = Nothing, focusOffset = 0, focusError = Nothing
        , matches = Dict.empty, matchPending = Dict.empty, matchErrors = Dict.empty
        , selectionValidation = dialog.selected |> Maybe.andThen identity |> Maybe.map emptySelection
    }
        |> emptyFilteredBranches


filterQuery : MoveDialog -> String
filterQuery dialog =
    "&show_only=projects&show_empty_projects=true"
        ++ (if Set.isEmpty dialog.statuses then
                -- The server treats no status parameters as all statuses.
                -- An empty picker filter is handled locally without a request.
                ""
            else Set.toList dialog.statuses |> List.map ((++) "&project_status=") |> String.concat)
        ++ (if String.trim dialog.query == "" then "" else "&query=" ++ Url.percentEncode (String.trim dialog.query))


effectiveBranch : MoveDialog -> String -> Bool
effectiveBranch dialog branch =
    branch == "" || (Set.member branch dialog.expanded && List.any (\( id, _ ) -> id == branch) (visibleRows dialog))


{-| Four independent physical slots, one current logical request per branch.
Fair ordering uses last admission, never dictionary ordering alone.
-}
pump : Model -> ( Model, Cmd Msg )
pump model =
    case model.move.dialog of
        Nothing -> ( model, Cmd.none )
        Just dialog ->
            let
                state = model.move
                pendingFocus = not dialog.focusComplete && dialog.focusPending == Nothing && dialog.focusError == Nothing
                pendingSelection = dialog.selectionValidation |> Maybe.map (\validation -> not validation.complete && validation.pending == Nothing && validation.error == Nothing) |> Maybe.withDefault False
                ready =
                    Dict.toList dialog.branches
                        |> List.filter (\( key, branch ) -> effectiveBranch dialog key && not branch.complete && branch.pending == Nothing && branch.error == Nothing && not (Set.isEmpty dialog.statuses))
                        |> List.sortBy (Tuple.second >> .lastIssued)
                matchDemand =
                    (mountedRows dialog |> List.map (Tuple.second >> Tuple.first))
                        ++ (dialog.selected |> Maybe.andThen identity |> Maybe.map List.singleton |> Maybe.withDefault [])
                        |> Set.fromList |> Set.toList
                        |> List.filter (\id -> Dict.get id dialog.projects |> Maybe.map (\p ->
                            String.trim dialog.query /= ""
                                && Set.member (Api.projectStatusToString p.status) dialog.statuses
                                && not (String.contains (String.toLower (String.trim dialog.query)) (String.toLower p.name))
                                && not (Dict.member id dialog.matches || Dict.member id dialog.matchPending || Dict.member id dialog.matchErrors)) |> Maybe.withDefault False)
                candidate =
                    if pendingFocus then Just ( "@focus", dialog.focusOffset )
                    else if pendingSelection then dialog.selectionValidation |> Maybe.map (\validation -> ( "@selection", validation.offset ))
                    else case List.head ready of
                        Just ( key, branch ) -> Just ( key, branch.offset )
                        Nothing ->
                            if Set.size model.dataLoading.cardDetailAdmissions + (Dict.values state.admissions |> List.filter (.branch >> String.startsWith "@match:") |> List.length) >= 6 then Nothing
                            else List.head matchDemand |> Maybe.map (\id -> ( "@match:" ++ id, 0 ))
            in
            if Dict.size state.admissions + Dict.size model.dataLoading.navigationAdmissions + (if model.dataLoading.rootNavigationRequest |> Maybe.map .inFlight |> Maybe.withDefault False then 1 else 0) >= 4 then ( model, Cmd.none )
            else
                case ( candidate, model.selectedWorkspaceId ) of
                    ( Just ( branch, offset ), Just workspace ) ->
                        let
                            readId = state.nextReadId
                            request = { token = dialog.token, generation = dialog.generation, branch = branch, offset = offset }
                            nextDialog =
                                if branch == "@focus" then { dialog | focusPending = Just readId }
                                else if branch == "@selection" then { dialog | selectionValidation = Maybe.map (\validation -> { validation | pending = Just readId }) dialog.selectionValidation }
                                else if String.startsWith "@match:" branch then { dialog | matchPending = Dict.insert (String.dropLeft 7 branch) readId dialog.matchPending }
                                else { dialog | branches = Dict.update branch (Maybe.map (\b -> { b | pending = Just readId, lastIssued = readId })) dialog.branches }
                            admitted = { model | move = { state | nextReadId = readId + 1, admissions = Dict.insert readId request state.admissions, dialog = Just nextDialog } }
                            readCmd =
                                if branch == "@focus" then Api.fetchNavigationFocus model.flags.apiUrl workspace dialog.entityType dialog.entityId offset (MoveFocusFetched readId)
                                else if branch == "@selection" then Api.fetchNavigationFocus model.flags.apiUrl workspace "project" (dialog.selectionValidation |> Maybe.map .id |> Maybe.withDefault "") offset (MoveFocusFetched readId)
                                else if String.startsWith "@match:" branch then Api.fetchProject model.flags.apiUrl (String.dropLeft 7 branch) (MoveMatchFetched readId)
                                else Api.fetchNavigationBranch model.flags.apiUrl workspace (if branch == "" then "workspace_root" else "project") (if branch == "" then Nothing else Just branch) offset 0 (if dialog.entityType == "project" && branch /= "" && isWithin dialog.projects dialog.entityId branch then "&show_only=projects&show_empty_projects=true" else filterQuery dialog) (MoveBranchFetched readId)
                            ( next, more ) = pump admitted
                        in
                        ( next, Cmd.batch [ readCmd, more ] )
                    _ -> ( model, Cmd.none )


finishRead : Int -> (MoveRead -> MoveDialog -> MoveDialog) -> Model -> ( Model, Cmd Msg )
finishRead readId accept model =
    let
        state = model.move
        dialog =
            case ( Dict.get readId state.admissions, state.dialog ) of
                ( Just request, Just current ) ->
                    if request.token == current.token && request.generation == current.generation
                        && Helpers.projectRequestIsCurrent current.guard model && Permissions.canEditCurrentWorkspace model then
                        Just (accept request current)
                    else Just current
                _ -> state.dialog
    in
    pump { model | move = { state | admissions = Dict.remove readId state.admissions, dialog = Maybe.map (settle model >> align state.dialog) dialog, resumeOrdinary = state.resumeOrdinary || dialog == Nothing } }


acceptBranch : Int -> Result Http.Error Api.NavigationBranchResponse -> MoveRead -> MoveDialog -> MoveDialog
acceptBranch readId result request dialog =
    case Dict.get request.branch dialog.branches of
        Just branch ->
            if branch.pending /= Just readId || not (effectiveBranch dialog request.branch) then dialog
            else
                let
                    rejected message = { dialog | branches = Dict.insert request.branch { branch | pending = Nothing, error = Just message } dialog.branches }
                in
                case result of
                    Err _ -> rejected "Projects could not be loaded. This list is incomplete."
                    Ok response ->
                        if Just response.workspaceId /= dialog.guard.routeWorkspace
                            || List.any (\p -> p.workspaceId /= response.workspaceId || p.parentId /= (if request.branch == "" then Nothing else Just request.branch)) response.projects.items then
                            rejected "The project response did not match this branch. This list is incomplete."
                        else
                            let
                                ids = List.map .id response.projects.items
                                previous = Maybe.withDefault branch.ids branch.staged
                                fresh = List.filter (\id -> not (List.member id previous)) ids
                                nextOffset = request.offset + 50
                                error =
                                    if response.projects.hasMore && (List.length ids /= 50 || List.isEmpty fresh) then Just "The project page made no progress. This list is incomplete."
                                    else if response.projects.hasMore && nextOffset >= 10000 then Just "The project navigation limit was reached. Narrow the search to see more projects."
                                    else Nothing
                                membership = previous ++ fresh
                                stagedProjects = List.foldl (\p -> Dict.insert p.id p) dialog.stagedProjects response.projects.items
                                nextBranch = { branch | ids = if branch.staged == Nothing || not response.projects.hasMore then membership else branch.ids
                                    , staged = if branch.staged /= Nothing && response.projects.hasMore then Just membership else Nothing
                                    , offset = nextOffset, complete = not response.projects.hasMore, pending = Nothing, error = error }
                                removed id =
                                    not (List.member id membership || Set.member id dialog.contextIds || Set.member id dialog.selectedPath)
                                        && not (dialog.revalidating && dialog.selected == Just (Just id))
                                        && (Dict.get id dialog.projects |> Maybe.map (\p -> p.parentId == (if request.branch == "" then Nothing else Just request.branch)) |> Maybe.withDefault False)
                                        && not (Dict.toList dialog.branches |> List.any (\( key, other ) -> key /= request.branch && List.member id (Maybe.withDefault [] other.staged)))
                                projects = if branch.staged == Nothing then List.foldl (\p -> Dict.insert p.id p) dialog.projects response.projects.items
                                    else if response.projects.hasMore then dialog.projects
                                    else List.foldl (\id all -> Dict.get id stagedProjects |> Maybe.map (\p -> Dict.insert id p all) |> Maybe.withDefault all)
                                        (List.foldl Dict.remove dialog.projects (List.filter removed branch.ids)) membership
                            in
                            { dialog | projects = projects, stagedProjects = stagedProjects, branches = Dict.insert request.branch nextBranch dialog.branches }
        Nothing -> dialog


acceptFocus : Int -> Result Http.Error Api.NavigationFocusResponse -> MoveRead -> MoveDialog -> MoveDialog
acceptFocus readId result request dialog =
    if dialog.focusPending /= Just readId then dialog
    else
        case result of
            Err _ -> { dialog | focusPending = Nothing, focusError = Just "Current location could not be loaded. Retry before moving." }
            Ok response ->
                let
                    matches = case response.target of
                        Api.NavigationProjectSummary p -> dialog.entityType == "project" && p.id == dialog.entityId && p.workspaceId == response.workspaceId
                        Api.NavigationTaskSummary t -> dialog.entityType == "task" && t.id == dialog.entityId && t.workspaceId == response.workspaceId
                    authoritative = case response.target of
                        Api.NavigationProjectSummary p ->
                            { dialog | currentProject = p.parentId, name = p.name, sourceOpen = List.member p.status [ Api.ProjActive, Api.ProjPaused ], sourceRunning = p.readinessRollup.inProgressTaskCount > 0 }
                        Api.NavigationTaskSummary t ->
                            { dialog | currentProject = t.projectId, taskParent = t.parentId, name = t.title, sourceOpen = not (List.member t.status [ Api.Done, Api.Cancelled ]), sourceRunning = t.status == Api.InProgress || t.readinessRollup.inProgressSubtaskCount > 0 }
                    projects = List.foldl (\summary all -> case summary of
                        Api.NavigationProjectSummary p -> Dict.insert p.id p all
                        _ -> all) dialog.projects (response.target :: response.ancestors)
                    taskParentName = response.ancestors |> List.filterMap (\summary -> case summary of
                        Api.NavigationTaskSummary t -> if Just t.id == authoritative.taskParent then Just t.title else Nothing
                        _ -> Nothing) |> List.head
                    next = response.nextAncestorOffset |> Maybe.withDefault request.offset
                    error = if response.ancestorsTruncated && (next <= request.offset || next >= 10000) then Just "Current location is incomplete. The ancestor navigation limit was reached." else Nothing
                in
                if not matches || Just response.workspaceId /= dialog.guard.routeWorkspace then
                    { dialog | focusPending = Nothing, focusError = Just "The current location response did not match this item." }
                else if request.offset > 0 && (authoritative.currentProject /= dialog.currentProject || authoritative.taskParent /= dialog.taskParent || authoritative.name /= dialog.name || authoritative.sourceOpen /= dialog.sourceOpen || authoritative.sourceRunning /= dialog.sourceRunning) then
                    { dialog | focusPending = Nothing, focusComplete = False, focusError = Just "The current location changed while loading its ancestors. Retry before moving." }
                else
                    unfoldContext { authoritative | projects = projects
                    , contextIds = Set.union (if request.offset == 0 then Set.empty else dialog.contextIds) (response.target :: response.ancestors |> List.filterMap (\summary -> case summary of
                        Api.NavigationProjectSummary p -> Just p.id
                        _ -> Nothing) |> Set.fromList)
                    , taskParentName = case taskParentName of
                        Just name -> Just name
                        Nothing -> if authoritative.taskParent == dialog.taskParent then dialog.taskParentName else Nothing
                    , focusPending = Nothing, focusComplete = not response.ancestorsTruncated, focusOffset = next, focusError = error }


emptySelection : String -> MoveSelectionValidation
emptySelection id =
    { id = id, offset = 0, pending = Nothing, complete = False, error = Nothing, projects = Dict.empty }


selectionReady : MoveDialog -> Bool
selectionReady dialog =
    dialog.selectionValidation |> Maybe.map .complete |> Maybe.withDefault True


{-| Membership of an expanded branch cannot establish whether a destination
moved beneath a collapsed parent. Validate its target and bounded ancestry
directly, under the same dialog generation and physical admission fences.
-}
acceptSelection : Int -> Result Http.Error Api.NavigationFocusResponse -> MoveRead -> MoveDialog -> MoveDialog
acceptSelection readId result request dialog =
    case dialog.selectionValidation of
        Nothing -> dialog
        Just validation ->
            if validation.pending /= Just readId || dialog.selected /= Just (Just validation.id) then dialog
            else
                let
                    rejected message = { dialog | selectionValidation = Just { validation | pending = Nothing, error = Just message } }
                    absent = { dialog | selected = Nothing, selectedPath = Set.empty, selectionValidation = Nothing, error = Just "Selected destination is unavailable. Choose another destination." }
                in
                case result of
                    Err (Http.BadStatus status) ->
                        if status == 404 || status == 403 then absent
                        else rejected "Selected destination could not be verified. Retry before moving."
                    Err _ -> rejected "Selected destination could not be verified. Retry before moving."
                    Ok response ->
                        case response.target of
                            Api.NavigationTaskSummary _ -> rejected "The destination response did not match this project."
                            Api.NavigationProjectSummary project ->
                                let
                                    next = response.nextAncestorOffset |> Maybe.withDefault request.offset
                                    summaries = response.target :: response.ancestors |> List.filterMap (\summary -> case summary of
                                        Api.NavigationProjectSummary p -> Just p
                                        _ -> Nothing)
                                    projects = List.foldl (\p -> Dict.insert p.id p) validation.projects summaries
                                    changed = request.offset > 0 && Dict.get validation.id validation.projects /= Just project
                                in
                                if project.id /= validation.id || Just response.workspaceId /= dialog.guard.routeWorkspace
                                    || List.any (\p -> p.workspaceId /= response.workspaceId) summaries then
                                    rejected "The destination response did not match this project."
                                else if changed then rejected "Selected destination changed while loading. Retry before moving."
                                else if response.ancestorsTruncated && (next <= request.offset || next >= 10000) then
                                    rejected "Selected destination ancestry is incomplete. Retry before moving."
                                else if response.ancestorsTruncated then
                                    { dialog | selectionValidation = Just { validation | pending = Nothing, offset = next, projects = projects } }
                                else
                                    let
                                        path = Dict.keys projects |> Set.fromList
                                        parents = Set.remove validation.id path
                                        previousPath = projectChain dialog.projects validation.id |> List.map .id |> Set.fromList
                                        revealSelected = previousPath /= path || not (Set.isEmpty (Set.diff parents dialog.expanded))
                                        expanded = Set.union parents dialog.expanded
                                        branches = Set.foldl (\id all -> Dict.update id (Maybe.withDefault emptyBranch >> Just) all) dialog.branches parents
                                    in
                                    emptyFilteredBranches { dialog | projects = Dict.union projects dialog.projects, selectedPath = path, expanded = expanded, branches = branches
                                        , selectionValidation = Just { validation | pending = Nothing, complete = True, offset = next, projects = projects }
                                        , revealPending = dialog.revealPending || revealSelected
                                    }


acceptMatch : Int -> Result Http.Error Api.Project -> MoveRead -> MoveDialog -> MoveDialog
acceptMatch readId result request dialog =
    let id = String.dropLeft 7 request.branch in
    if Dict.get id dialog.matchPending /= Just readId then dialog
    else
        let
            cleared = { dialog | matchPending = Dict.remove id dialog.matchPending }
            failedMatch = { cleared | matchErrors = Dict.insert id "Search match could not be checked. Retry." cleared.matchErrors }
        in
        case result of
            Err _ -> failedMatch
            Ok project ->
                if project.id /= id || Just project.workspaceId /= dialog.guard.routeWorkspace then failedMatch
                else
                    let
                        query = String.toLower (String.trim dialog.query)
                        matches = String.contains query (String.toLower project.name) || (project.description |> Maybe.map (String.toLower >> String.contains query) |> Maybe.withDefault False)
                    in
                    { cleared | matches = Dict.insert id matches cleared.matches }


isWithin : Dict String Api.ProjectCardSummary -> String -> String -> Bool
isWithin projects ancestor id =
    let
        walk seen current =
            if current == ancestor then True
            else if Set.member current seen then False
            else Dict.get current projects |> Maybe.andThen .parentId |> Maybe.map (walk (Set.insert current seen)) |> Maybe.withDefault False
    in
    walk Set.empty id


{-| Canonical focus context stays visible independently of destination filters. -}
unfoldContext : MoveDialog -> MoveDialog
unfoldContext dialog =
    let
        expanded = Set.union dialog.expanded dialog.contextIds
        branches = Set.foldl (\id all -> Dict.update id (Maybe.withDefault emptyBranch >> Just) all) dialog.branches expanded
    in
    emptyFilteredBranches { dialog | expanded = expanded, branches = branches }


visibleRows : MoveDialog -> List ( String, Int )
visibleRows dialog =
    let
        children branch =
            let
                expected = if branch == "" then Nothing else Just branch
                memberIds = Dict.get branch dialog.branches |> Maybe.map .ids |> Maybe.withDefault []
                context = Set.union dialog.contextIds dialog.selectedPath |> Set.toList |> List.filter (\id -> Dict.get id dialog.projects |> Maybe.map (\p -> p.parentId == expected) |> Maybe.withDefault False)
                order p = ( Api.projectStatusOrder p.status, negate p.priority, ( String.toLower p.name, p.id ) )
            in
            Set.fromList (memberIds ++ context) |> Set.toList |> List.filterMap (\id -> Dict.get id dialog.projects)
                |> List.filter (\p -> p.parentId == expected) |> List.sortBy order |> List.map .id
        walk visited depth branch =
            children branch |> List.concatMap (\id ->
                if Set.member id visited then []
                else ( id, depth ) :: (if Set.member id dialog.expanded then walk (Set.insert id visited) (depth + 1) id else []))
    in
    walk Set.empty 0 ""


mountedRows : MoveDialog -> List ( Int, ( String, Int ) )
mountedRows dialog =
    visibleRows dialog |> List.indexedMap Tuple.pair |> List.filter (\( index, ( id, _ ) ) ->
        (index >= dialog.rowOffset && index < dialog.rowOffset + 25) || Just id == dialog.activeRow)


{-| Fixed two-line rows give the picker exact geometry. Preserve a logical row
and its intrarow offset when membership or expansion changes its position.
-}
align : Maybe MoveDialog -> MoveDialog -> MoveDialog
align previous dialog =
    let
        rows = visibleRows dialog
        changed = previous |> Maybe.map (visibleRows >> (/=) rows) |> Maybe.withDefault False
        index = rows |> List.indexedMap Tuple.pair |> List.filter (\( _, ( id, _ ) ) -> Just id == dialog.anchor) |> List.head |> Maybe.map Tuple.first
    in
    if changed then
        case index of
            Just i -> { dialog | rowOffset = clamp 0 (Basics.max 0 (List.length rows - 25)) (i - 3), scrollTop = toFloat ((i + 1) * 56) + dialog.anchorDelta, layout = dialog.layout + 1 }
            Nothing -> { dialog | anchor = Nothing, rowOffset = clamp 0 (Basics.max 0 (List.length rows - 25)) dialog.rowOffset, layout = dialog.layout + 1 }
    else dialog


settle : Model -> MoveDialog -> MoveDialog
settle model pending =
    let
        refreshing = pending.revalidating && not (pending.focusComplete && (pending.selectionValidation |> Maybe.map .complete |> Maybe.withDefault True) && (Dict.toList pending.branches |> List.all (\( key, b ) -> not (effectiveBranch pending key) || b.complete)))
        -- A selected destination may move between parents whose passes complete
        -- in either order. Keep its summary until all effective membership is
        -- authoritative, then remove summaries without a current owner.
        dialog =
            if pending.revalidating && not refreshing then
                { pending | projects = Dict.filter (\id project -> Set.member id pending.contextIds || Set.member id pending.selectedPath
                    || (Dict.toList pending.branches |> List.any (\( key, branch ) -> effectiveBranch pending key && List.member id branch.ids
                        && project.parentId == (if key == "" then Nothing else Just key)))) pending.projects }
            else pending
        rows = visibleRows dialog
        rootComplete = Dict.get "" dialog.branches |> Maybe.map .complete |> Maybe.withDefault False
        selected = case dialog.selected of
            Just destination ->
                let
                    reason = destinationReason model dialog destination
                    checking = reason == Just "Checking search match…"
                        || (destination |> Maybe.andThen (\id -> Dict.get id dialog.matchErrors) |> Maybe.map (\error -> reason == Just error) |> Maybe.withDefault False)
                in
                if reason /= Nothing && not checking && dialog.focusComplete
                    && (dialog.selectionValidation |> Maybe.map .complete |> Maybe.withDefault True) then Nothing
                else if not refreshing && destination /= Nothing && not (destination |> Maybe.map (\id -> Set.member id dialog.selectedPath) |> Maybe.withDefault False) && not (List.any (\( id, _ ) -> Just id == destination) rows) then Nothing
                else dialog.selected
            Nothing -> Nothing
        target = if dialog.selectionValidation |> Maybe.map .complete |> Maybe.withDefault False then dialog.selected |> Maybe.andThen identity
            else if dialog.entityType == "project" then Just dialog.entityId else dialog.currentProject
        index = rows |> List.indexedMap Tuple.pair |> List.filter (\( _, ( id, _ ) ) -> Just id == target) |> List.head |> Maybe.map Tuple.first
        reveal = dialog.revealPending && dialog.focusComplete && rootComplete
        offset = if reveal then index |> Maybe.map (\i -> clamp 0 (Basics.max 0 (List.length rows - 25)) (i - 3)) |> Maybe.withDefault 0
            else clamp 0 (Basics.max 0 (List.length rows - 25)) dialog.rowOffset
    in
    { dialog | selected = selected, selectedPath = if selected == dialog.selected then dialog.selectedPath else Set.empty
        , selectionValidation = if selected == dialog.selected then dialog.selectionValidation else Nothing
        , revalidating = refreshing, rowOffset = offset
        , revealPending = if reveal then False else dialog.revealPending
        , reveal = if reveal then dialog.reveal + 1 else dialog.reveal
        , revealTarget = if reveal then target else dialog.revealTarget
        , focusTarget = if reveal then Nothing else dialog.focusTarget
    }


destinationReason : Model -> MoveDialog -> Maybe String -> Maybe String
destinationReason model dialog destination =
    if destination == dialog.currentProject then Just "This is the current location."
    else
        case destination of
            Nothing -> Nothing
            Just id ->
                case Dict.get id dialog.projects of
                    Nothing -> Just "Load this project before selecting it."
                    Just project ->
                        let
                            ancestors = projectChain dialog.projects id
                            closed = List.any (\p -> List.member p.status [ Api.ProjCompleted, Api.ProjArchived ]) ancestors
                            paused = List.any (\p -> p.status == Api.ProjPaused) ancestors
                            openSource = dialog.sourceOpen
                            running = dialog.sourceRunning ||
                                (if dialog.entityType == "project" then
                                    Dict.get dialog.entityId model.dataLoading.projectCardSummaries |> Maybe.map (.readinessRollup >> .inProgressTaskCount >> (\count -> count > 0)) |> Maybe.withDefault False
                                else
                                    (Dict.get dialog.entityId model.tasks |> Maybe.map (\t -> t.status == Api.InProgress) |> Maybe.withDefault False)
                                        || (Dict.get dialog.entityId model.dataLoading.taskCardSummaries |> Maybe.map (.readinessRollup >> .inProgressSubtaskCount >> (\count -> count > 0)) |> Maybe.withDefault False)
                                        || (Dict.get dialog.entityId model.dependencies.taskReadinessRollups |> Maybe.map (.inProgressSubtaskCount >> (\count -> count > 0)) |> Maybe.withDefault False))
                        in
                        if dialog.entityType == "project" && isWithin dialog.projects dialog.entityId id then Just "A project cannot move into itself or its descendants."
                        else if closed && openSource then Just "Reopen this destination before moving open work here."
                        else if paused && running then Just "Running work cannot move into paused ancestry."
                        else if not (Set.member (Api.projectStatusToString project.status) dialog.statuses) then
                            Just "Ancestor context: expand to find matching projects."
                        else if String.trim dialog.query /= "" && not (String.contains (String.toLower (String.trim dialog.query)) (String.toLower project.name)) then
                            case Dict.get id dialog.matches of
                                Just True -> Nothing
                                Just False -> Just "Ancestor context: expand to find matching projects."
                                Nothing -> Just (Dict.get id dialog.matchErrors |> Maybe.withDefault "Checking search match…")
                        else Nothing


projectChain : Dict String Api.ProjectCardSummary -> String -> List Api.ProjectCardSummary
projectChain projects id =
    let
        walk visited current =
            if Set.member current visited then []
            else case Dict.get current projects of
                Nothing -> []
                Just p -> (p.parentId |> Maybe.map (walk (Set.insert current visited)) |> Maybe.withDefault []) ++ [ p ]
    in
    walk Set.empty id


moveFields : MoveDialog -> Maybe String -> List ( String, Encode.Value )
moveFields dialog destination =
    let value = destination |> Maybe.map Encode.string |> Maybe.withDefault Encode.null in
    if dialog.entityType == "project" then [ ( "parent_id", value ) ]
    else [ ( "project_id", value ), ( "parent_id", Encode.null ) ]


confirm : MoveDialog -> Model -> ( Model, Cmd Msg )
confirm dialog model =
    case dialog.selected of
        Nothing -> ( model, Cmd.none )
        Just destination ->
            if destinationReason model dialog destination /= Nothing || not dialog.focusComplete || not (selectionReady dialog) || dialog.revalidating || not (Permissions.canEditCurrentWorkspace model) then ( model, Cmd.none )
            else
                let
                    trackedIds = dialog.entityId :: (destination |> Maybe.map List.singleton |> Maybe.withDefault [])
                    ( tracked, requestId, trackCmd ) = Helpers.beginTrackedMutation trackedIds model
                in
                case Helpers.prepareProjectRequest model.selectedWorkspaceId model.sessionRequestEpoch ((if dialog.entityType == "project" then "entity:" else "move:") ++ dialog.entityType ++ ":" ++ dialog.entityId) tracked of
                    Nothing -> ( model, Cmd.none )
                    Just ( guard, guarded ) ->
                        let
                            saving = setDialog { dialog | guard = guard, saving = Just requestId, error = Nothing } guarded
                            fields = moveFields dialog destination ++ [ ( "request_id", Encode.string requestId ) ]
                            mutations = saving.mutations
                            registered = if dialog.entityType == "project" then { saving | mutations = { mutations | projectOperations = Dict.insert dialog.entityId { requestId = Just requestId, guard = guard } mutations.projectOperations } } else saving
                            command = if dialog.entityType == "project" then Api.updateProject model.flags.apiUrl dialog.entityId fields (MoveProjectSaved dialog.token guard)
                                else Api.updateTask model.flags.apiUrl dialog.entityId fields (MoveTaskSaved dialog.token guard)
                        in
                        ( registered, Cmd.batch [ trackCmd, command ] )


viewAction : Model -> String -> String -> Html Msg
viewAction model kind id =
    if Permissions.canEditCurrentWorkspace model then
        button [ class "btn-icon btn-move", Html.Attributes.id ("move-origin-" ++ kind ++ "-" ++ id), title "Move to another project", attribute "aria-label" ("Move " ++ kind), onClick (OpenMove kind id) ] [ text "↗" ]
    else text ""


view : Model -> Html Msg
view model =
    case model.move.dialog of
        Nothing -> text ""
        Just dialog ->
            let
                saving = dialog.saving /= Nothing
                rows = visibleRows dialog
                destination = dialog.selected
                valid = destination |> Maybe.map (\d -> destinationReason model dialog d == Nothing) |> Maybe.withDefault False
                badge label = span [ class "move-role" ] [ text label ]
                role id =
                    if dialog.entityType == "project" && id == dialog.entityId then Just "Moving"
                    else if Just id == dialog.currentProject then Just "Current parent"
                    else if dialog.entityType == "project" && isWithin dialog.projects dialog.entityId id then Just "Moves with"
                    else if destination == Just (Just id) then Just "Selected"
                    else Nothing
                keys index =
                    Decode.field "key" Decode.string |> Decode.andThen (\key ->
                        case key of
                            "ArrowDown" -> Decode.succeed ( MoveFocusRow (index + 1), True )
                            "ArrowUp" -> Decode.succeed ( MoveFocusRow (index - 1), True )
                            "Home" -> Decode.succeed ( MoveFocusRow 0, True )
                            "End" -> Decode.succeed ( MoveFocusRow (List.length rows - 1), True )
                            _ -> Decode.fail "Native key")
                row ( index, ( projectId, depth ) ) =
                    case Dict.get projectId dialog.projects of
                        Nothing -> text ""
                        Just project ->
                            let
                                reason = destinationReason model dialog (Just projectId)
                                moving = dialog.entityType == "project" && isWithin dialog.projects dialog.entityId projectId
                                current = Just projectId == dialog.currentProject
                                rowClass = "move-project-row" ++ (if moving then " move-source" else "") ++ (if current then " move-current" else "") ++ (if destination == Just (Just projectId) then " move-selected" else "")
                            in
                            div [ class rowClass, attribute "data-project-id" projectId, attribute "data-row-index" (String.fromInt index), style "padding-left" (String.fromInt (Basics.min depth 8 * 18) ++ "px") ]
                                [ span [ class "move-tree-guides", attribute "aria-hidden" "true" ] (List.repeat (Basics.min depth 8) (span [] []))
                                , if project.directProjectCount > 0 then
                                    button [ class "tree-toggle", disabled saving, attribute "aria-label" ((if Set.member projectId dialog.expanded then "Collapse " else "Expand ") ++ project.name), attribute "aria-expanded" (if Set.member projectId dialog.expanded then "true" else "false"), onClick (MoveToggleBranch projectId), onFocus (MoveRowFocused projectId), custom "keydown" (keys index |> Decode.map (\( message, _ ) -> { message = message, stopPropagation = True, preventDefault = True })) ] [ text (if Set.member projectId dialog.expanded then "▾" else "▸") ]
                                  else span [ class "tree-toggle-spacer" ] []
                                , button [ Html.Attributes.id ("move-choice-" ++ projectId), class ("move-project-choice" ++ (if destination == Just (Just projectId) then " selected" else "")), disabled saving
                                    , attribute "aria-disabled" (if reason /= Nothing then "true" else "false")
                                    , title (Maybe.withDefault ("Select " ++ project.name) reason)
                                    , attribute "aria-label" (project.name ++ (role projectId |> Maybe.map ((++) ", ") |> Maybe.withDefault "") ++ (reason |> Maybe.map ((++) ". ") |> Maybe.withDefault ""))
                                    , attribute "aria-pressed" (if destination == Just (Just projectId) then "true" else "false"), onClick (MoveSelect (Just projectId)), onFocus (MoveRowFocused projectId), custom "keydown" (keys index |> Decode.map (\( message, _ ) -> { message = message, stopPropagation = True, preventDefault = True })) ]
                                    [ span [ class "move-project-name" ] [ text project.name ]
                                    , span [ class "move-project-meta" ] [ span [ class "move-project-status" ] [ text (Api.projectStatusToString project.status) ], role projectId |> Maybe.map badge |> Maybe.withDefault (text "") ] ]
                                , if Dict.member projectId dialog.matchErrors then button [ class "btn", disabled saving, onClick (MoveRetry ("@match:" ++ projectId)), title "Retry search match", attribute "aria-label" ("Retry search match for " ++ project.name) ] [ text "Retry" ] else text ""
                                ]
                pieces =
                    let
                        spacer count = div [ class "move-tree-spacer", style "height" (String.fromInt (count * 56) ++ "px"), attribute "aria-hidden" "true" ] []
                        walk from mounted =
                            case mounted of
                                [] -> [ spacer (List.length rows - from) ]
                                (( index, _ ) as item) :: rest -> spacer (index - from) :: row item :: walk (index + 1) rest
                    in
                    walk 0 (mountedRows dialog)
                branchNotice ( key, branch ) =
                    let name = if key == "" then "Projects" else Dict.get key dialog.projects |> Maybe.map .name |> Maybe.withDefault key in
                    if not (effectiveBranch dialog key) then text ""
                    else case branch.error of
                        Just message -> div [ class "move-load-error", attribute "role" "status" ]
                            [ text (name ++ ": " ++ message), button [ disabled saving, onClick (MoveRetry key) ] [ text ("Retry " ++ name) ] ]
                        Nothing ->
                            if not branch.complete then div [ class "move-loading", attribute "role" "status" ] [ text ("Loading " ++ name ++ "…") ]
                            else text ""
                keyDecoder =
                    Decode.map2 (\key tag ->
                        if key == "Escape" then ( CloseMove, True )
                        else if key == "Enter" && not (List.member tag [ "BUTTON", "INPUT" ]) then ( MoveConfirm, True )
                        else ( NoOp, False ))
                        (Decode.field "key" Decode.string)
                        (Decode.at [ "target", "tagName" ] Decode.string)
            in
            Html.node "dialog"
                [ id "move-dialog", class "move-dialog", attribute "aria-labelledby" "move-dialog-title", attribute "data-move-token" (String.fromInt dialog.token)
                , attribute "aria-description" ((if dialog.entityType == "project" then "Moving project " else "Moving task ") ++ dialog.name ++ (if dialog.entityType == "task" && dialog.taskParent /= Nothing then ". Moving to another project detaches this subtask and makes it a top-level task. Live subtasks follow this move." else ". The subtree follows this move."))
                , attribute "data-move-origin" ("move-origin-" ++ dialog.entityType ++ "-" ++ dialog.entityId)
                , preventDefaultOn "cancel" (Decode.succeed ( CloseMove, True ))
                , preventDefaultOn "keydown" keyDecoder
                ]
                [ h2 [ id "move-dialog-title" ] [ text "Select Destination" ]
                , input [ id "move-search", type_ "search", placeholder "Search projects", attribute "aria-label" "Search destination projects", value dialog.query, disabled saving, onInput MoveQueryChanged ] []
                , div [ class "move-status-filters" ] (List.map (\status ->
                    button [ class ("filter-pill" ++ (if Set.member status dialog.statuses then " filter-pill-active" else "")), attribute "aria-pressed" (if Set.member status dialog.statuses then "true" else "false"), disabled saving, onClick (MoveStatusChanged status) ] [ text status ]) [ "active", "paused", "completed", "archived" ])
                , div [ id "move-tree", class "move-tree", attribute "aria-label" "Destination projects", tabindex 0
                    , attribute "data-move-reveal" (String.fromInt dialog.reveal), attribute "data-move-offset" (String.fromInt dialog.rowOffset)
                    , attribute "data-move-layout" (String.fromInt dialog.layout), attribute "data-move-scroll-top" (String.fromFloat dialog.scrollTop)
                    , attribute "data-move-focus" (Maybe.withDefault "" dialog.focusTarget)
                    , attribute "data-move-location" (dialog.revealTarget |> Maybe.withDefault (if dialog.entityType == "project" then dialog.entityId else Maybe.withDefault "" dialog.currentProject))
                    , on "scroll" (Decode.at [ "target", "scrollTop" ] Decode.float |> Decode.map MoveScroll)
                    , preventDefaultOn "keydown" (keys (dialog.rowOffset - 1))
                    ]
                    ([ div [ class ("move-project-row move-root" ++ (if dialog.currentProject == Nothing then " move-current" else "") ++ (if destination == Just Nothing then " move-selected" else "")) ]
                        [ button [ class ("move-project-choice" ++ (if destination == Just Nothing then " selected" else "")), disabled saving
                            , attribute "aria-label" (if dialog.entityType == "project" then "Workspace root" else "No project")
                            , attribute "aria-disabled" (if destinationReason model dialog Nothing /= Nothing then "true" else "false")
                            , title (destinationReason model dialog Nothing |> Maybe.withDefault "Select root placement"), onClick (MoveSelect Nothing), attribute "aria-pressed" (if destination == Just Nothing then "true" else "false") ]
                            [ span [ class "move-project-name" ] [ text (if dialog.entityType == "project" then "Workspace root" else "No project") ]
                            , if dialog.currentProject == Nothing then badge "Current parent" else if destination == Just Nothing then badge "Selected" else text "" ] ] ]
                        ++ pieces
                        ++ (if List.isEmpty rows && (Dict.get "" dialog.branches |> Maybe.map .complete |> Maybe.withDefault False) then [ p [] [ text "No matching projects." ] ] else [])
                        ++ (if Set.isEmpty dialog.statuses then [ p [] [ text "Select a status to browse." ] ] else [])
                    )
                , div [] (List.map branchNotice (Dict.toList dialog.branches))
                , if dialog.focusComplete then text "" else
                    div [ class "move-loading", attribute "role" "status" ]
                        [ text (Maybe.withDefault "Loading current location…" dialog.focusError)
                        , if dialog.focusError /= Nothing then button [ disabled saving, onClick (MoveRetry "@focus") ] [ text "Retry current location" ] else text ""
                        ]
                , dialog.selectionValidation |> Maybe.andThen .error |> Maybe.map (\message -> div [ class "move-load-error", attribute "role" "status" ] [ text message, button [ disabled saving, onClick (MoveRetry "@selection") ] [ text "Retry destination" ] ]) |> Maybe.withDefault (text "")
                , dialog.error |> Maybe.map (\message -> div [ class "move-load-error", attribute "role" "alert" ] [ text message ]) |> Maybe.withDefault (text "")
                , div [ class "modal-actions" ]
                    [ button [ class "btn", onClick CloseMove ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", disabled (saving || not valid || not dialog.focusComplete || not (selectionReady dialog) || dialog.revalidating), onClick MoveConfirm ] [ text (if saving then "Moving…" else "Confirm") ]
                    ]
                ]

