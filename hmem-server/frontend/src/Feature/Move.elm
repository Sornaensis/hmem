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
    { dialog = Nothing, resumeOrdinary = False, nextToken = 1, nextReadId = 1, admissions = Dict.empty }


{-| Retirement never releases physical HTTP occupancy. Exact callbacks do that.
Counters survive workspace, authorization, filter, collapse and dialog changes.
-}
retire : MoveModel -> MoveModel
retire state =
    { state | dialog = Nothing, resumeOrdinary = True }


emptyBranch : MoveBranch
emptyBranch =
    { ids = [], offset = 0, complete = False, pending = Nothing, error = Nothing, lastIssued = 0 }


setDialog : MoveDialog -> Model -> Model
setDialog dialog model =
    let state = model.move in
    { model | move = { state | dialog = Just dialog } }


reconcile : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
reconcile ( model, command ) =
    case model.move.dialog of
        Nothing ->
            if model.move.resumeOrdinary then
                let
                    state = model.move
                    ( next, navigation ) = Feature.DataLoading.pumpNavigation { model | move = { state | resumeOrdinary = False } }
                    ( detailed, details ) = Feature.DataLoading.ensurePresentedCardDetails next
                in
                ( detailed, Cmd.batch [ command, navigation, details ] )
            else ( model, command )
        Just dialog ->
            if not (Permissions.canEditCurrentWorkspace model)
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
            finishRead readId (acceptFocus readId result) model

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
                                    pump (setDialog { dialog | expanded = Set.insert id dialog.expanded, branches = Dict.insert id emptyBranch dialog.branches } model)
                                else ( model, Cmd.none )
                            MoveSelect destination ->
                                if destinationReason model dialog destination == Nothing then
                                    ( setDialog { dialog | selected = Just destination, error = Nothing } model, Cmd.none )
                                else ( model, Cmd.none )
                            MoveRows offset ->
                                ( setDialog { dialog | rowOffset = clamp 0 (Basics.max 0 (List.length (visibleRows dialog) - 1)) offset } model, Cmd.none )
                            MoveRetry branch ->
                                if branch == "@focus" then
                                    resetFilters { dialog | focusError = Nothing, focusOffset = 0, focusComplete = False, taskParentName = Nothing, projects = Dict.empty } model
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
    case ( target, Helpers.prepareProjectRequest model.selectedWorkspaceId model.sessionRequestEpoch ((if kind == "project" then "entity:" else "move:") ++ kind ++ ":" ++ id) model ) of
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
                    , selected = Nothing, rowOffset = 0, focusOffset = 0, focusPending = Nothing, focusComplete = False, focusError = Nothing
                    , saving = Nothing, error = Nothing
                    }
            in
            pump { guarded | move = { state | dialog = Just dialog, nextToken = state.nextToken + 1 } }
        _ -> ( model, Cmd.none )


resetFilters : MoveDialog -> Model -> ( Model, Cmd Msg )
resetFilters dialog model =
    pump (setDialog { dialog | generation = dialog.generation + 1, expanded = Set.empty, branches = Dict.singleton "" emptyBranch, selected = Nothing, rowOffset = 0, error = Nothing, focusPending = Nothing, matches = Dict.empty, matchPending = Dict.empty, matchErrors = Dict.empty } model)


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
                ready =
                    Dict.toList dialog.branches
                        |> List.filter (\( key, branch ) -> effectiveBranch dialog key && not branch.complete && branch.pending == Nothing && branch.error == Nothing && not (Set.isEmpty dialog.statuses))
                        |> List.sortBy (Tuple.second >> .lastIssued)
                matchDemand =
                    visibleRows dialog |> List.drop dialog.rowOffset |> List.take 25 |> List.map Tuple.first
                        |> List.filter (\id -> Dict.get id dialog.projects |> Maybe.map (\p ->
                            String.trim dialog.query /= ""
                                && Set.member (Api.projectStatusToString p.status) dialog.statuses
                                && not (String.contains (String.toLower (String.trim dialog.query)) (String.toLower p.name))
                                && not (Dict.member id dialog.matches || Dict.member id dialog.matchPending || Dict.member id dialog.matchErrors)) |> Maybe.withDefault False)
                candidate =
                    if pendingFocus then Just ( "@focus", dialog.focusOffset )
                    else case List.head ready of
                        Just ( key, branch ) -> Just ( key, branch.offset )
                        Nothing ->
                            if Set.size model.dataLoading.cardDetailAdmissions + (Dict.values state.admissions |> List.filter (.branch >> String.startsWith "@match:") |> List.length) >= 6 then Nothing
                            else List.head matchDemand |> Maybe.map (\id -> ( "@match:" ++ id, 0 ))
            in
            if Dict.size state.admissions + Dict.size model.dataLoading.navigationAdmissions >= 4 then ( model, Cmd.none )
            else
                case ( candidate, model.selectedWorkspaceId ) of
                    ( Just ( branch, offset ), Just workspace ) ->
                        let
                            readId = state.nextReadId
                            request = { token = dialog.token, generation = dialog.generation, branch = branch, offset = offset }
                            nextDialog =
                                if branch == "@focus" then { dialog | focusPending = Just readId }
                                else if String.startsWith "@match:" branch then { dialog | matchPending = Dict.insert (String.dropLeft 7 branch) readId dialog.matchPending }
                                else { dialog | branches = Dict.update branch (Maybe.map (\b -> { b | pending = Just readId, lastIssued = readId })) dialog.branches }
                            admitted = { model | move = { state | nextReadId = readId + 1, admissions = Dict.insert readId request state.admissions, dialog = Just nextDialog } }
                            readCmd =
                                if branch == "@focus" then Api.fetchNavigationFocus model.flags.apiUrl workspace dialog.entityType dialog.entityId offset (MoveFocusFetched readId)
                                else if String.startsWith "@match:" branch then Api.fetchProject model.flags.apiUrl (String.dropLeft 7 branch) (MoveMatchFetched readId)
                                else Api.fetchNavigationBranch model.flags.apiUrl workspace (if branch == "" then "workspace_root" else "project") (if branch == "" then Nothing else Just branch) offset 0 (filterQuery dialog) (MoveBranchFetched readId)
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
    pump { model | move = { state | admissions = Dict.remove readId state.admissions, dialog = dialog, resumeOrdinary = state.resumeOrdinary || dialog == Nothing } }


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
                                fresh = List.filter (\id -> not (List.member id branch.ids)) ids
                                nextOffset = request.offset + 50
                                error =
                                    if response.projects.hasMore && (List.length ids /= 50 || List.isEmpty fresh) then Just "The project page made no progress. This list is incomplete."
                                    else if response.projects.hasMore && nextOffset >= 10000 then Just "The project navigation limit was reached. Narrow the search to see more projects."
                                    else Nothing
                                nextBranch = { branch | ids = branch.ids ++ fresh, offset = nextOffset, complete = not response.projects.hasMore, pending = Nothing, error = error }
                            in
                            { dialog | projects = List.foldl (\p -> Dict.insert p.id p) dialog.projects response.projects.items, branches = Dict.insert request.branch nextBranch dialog.branches }
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
                    { authoritative | projects = projects, taskParentName = case taskParentName of
                        Just name -> Just name
                        Nothing -> if authoritative.taskParent == dialog.taskParent then dialog.taskParentName else Nothing
                    , focusPending = Nothing, focusComplete = not response.ancestorsTruncated, focusOffset = next, focusError = error }


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


visibleRows : MoveDialog -> List ( String, Int )
visibleRows dialog =
    let
        walk visited depth branch =
            Dict.get branch dialog.branches |> Maybe.map (\b -> b.ids |> List.concatMap (\id ->
                if Set.member id visited then []
                else ( id, depth ) :: (if Set.member id dialog.expanded then walk (Set.insert id visited) (depth + 1) id else []))) |> Maybe.withDefault []
    in
    if Set.isEmpty dialog.statuses then [] else walk Set.empty 0 ""


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


projectPath : MoveDialog -> Maybe String -> String
projectPath dialog destination =
    case destination of
        Nothing -> if dialog.entityType == "project" then "Workspace root" else "No project"
        Just id ->
            let chain = projectChain dialog.projects id in
            if List.isEmpty chain then "Loading location…"
            else "Workspace root / " ++ String.join " / " (List.map .name chain)


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
            if destinationReason model dialog destination /= Nothing || not dialog.focusComplete || not (Permissions.canEditCurrentWorkspace model) then ( model, Cmd.none )
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
                row ( id, depth ) =
                    case Dict.get id dialog.projects of
                        Nothing -> text ""
                        Just project ->
                            let reason = destinationReason model dialog (Just id) in
                            div [ class "move-project-row", style "padding-left" (String.fromInt (depth * 16) ++ "px") ]
                                [ if project.directProjectCount > 0 then
                                    button [ class "tree-toggle", disabled saving, attribute "aria-label" ((if Set.member id dialog.expanded then "Collapse " else "Expand ") ++ project.name), attribute "aria-expanded" (if Set.member id dialog.expanded then "true" else "false"), onClick (MoveToggleBranch id) ] [ text (if Set.member id dialog.expanded then "▼" else "▶") ]
                                  else span [ class "tree-toggle-spacer" ] []
                                , button [ class ("move-project-choice" ++ (if destination == Just (Just id) then " selected" else "")), disabled (saving || reason /= Nothing), title (Maybe.withDefault ("Select " ++ project.name) reason), attribute "aria-pressed" (if destination == Just (Just id) then "true" else "false"), onClick (MoveSelect (Just id)) ]
                                    [ text project.name, span [ class "move-project-status" ] [ text (Api.projectStatusToString project.status) ] ]
                                , if Dict.member id dialog.matchErrors then button [ disabled saving, onClick (MoveRetry ("@match:" ++ id)) ] [ text "Retry search match" ] else text ""
                                , reason |> Maybe.map (\message -> span [ class "move-choice-reason" ] [ text message ]) |> Maybe.withDefault (text "")
                                ]
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
                , attribute "data-move-origin" ("move-origin-" ++ dialog.entityType ++ "-" ++ dialog.entityId)
                , preventDefaultOn "cancel" (Decode.succeed ( CloseMove, True ))
                , preventDefaultOn "keydown" keyDecoder
                ]
                [ h2 [ id "move-dialog-title" ] [ text ("Move " ++ dialog.name) ]
                , div [ class "move-location" ] [ strong [] [ text "Current: " ], text (projectPath dialog dialog.currentProject ++ (dialog.taskParentName |> Maybe.map ((++) " / ") |> Maybe.withDefault "")) ]
                , input [ id "move-search", type_ "search", placeholder "Search projects", attribute "aria-label" "Search destination projects", value dialog.query, disabled saving, onInput MoveQueryChanged, preventDefaultOn "keydown" (Decode.field "key" Decode.string |> Decode.map (\key -> ( if key == "Enter" then MoveConfirm else NoOp, key == "Enter" ))) ] []
                , div [ class "move-status-filters" ] (List.map (\status ->
                    label [] [ input [ type_ "checkbox", checked (Set.member status dialog.statuses), disabled saving, onCheck (\_ -> MoveStatusChanged status) ] [], text status ]) [ "active", "paused", "completed", "archived" ])
                , div [ class "move-tree", attribute "aria-label" "Destination projects" ]
                    ([ button [ class ("move-project-choice" ++ (if destination == Just Nothing then " selected" else "")), disabled (saving || destinationReason model dialog Nothing /= Nothing), title (destinationReason model dialog Nothing |> Maybe.withDefault "Select root placement"), onClick (MoveSelect Nothing) ] [ text (if dialog.entityType == "project" then "Workspace root" else "No project") ] ]
                        ++ List.map row (rows |> List.drop dialog.rowOffset |> List.take 25)
                        ++ (if List.isEmpty rows && (Dict.get "" dialog.branches |> Maybe.map .complete |> Maybe.withDefault False) then [ p [] [ text "No matching projects." ] ] else [])
                        ++ (if Set.isEmpty dialog.statuses then [ p [] [ text "Select a project status to browse." ] ] else [])
                    )
                , div [ class "move-row-pages" ]
                    [ button [ disabled (saving || dialog.rowOffset == 0), onClick (MoveRows (Basics.max 0 (dialog.rowOffset - 25))) ] [ text "Previous projects" ]
                    , text (String.fromInt (Basics.min (List.length rows) (dialog.rowOffset + 1)) ++ "–" ++ String.fromInt (Basics.min (List.length rows) (dialog.rowOffset + 25)) ++ " of " ++ String.fromInt (List.length rows) ++ " loaded")
                    , button [ disabled (saving || dialog.rowOffset + 25 >= List.length rows), onClick (MoveRows (dialog.rowOffset + 25)) ] [ text "Next projects" ]
                    ]
                , div [] (List.map branchNotice (Dict.toList dialog.branches))
                , if dialog.focusComplete then text "" else
                    div [ attribute "role" "status" ]
                        [ text (Maybe.withDefault "Loading current location…" dialog.focusError)
                        , if dialog.focusError /= Nothing then button [ disabled saving, onClick (MoveRetry "@focus") ] [ text "Retry current location" ] else text ""
                        ]
                , div [ class "move-location" ] [ strong [] [ text "Destination: " ], text (destination |> Maybe.map (projectPath dialog) |> Maybe.withDefault "Choose a project or root placement") ]
                , p [ class "move-effects" ] [ text (if dialog.entityType == "project" then "The entire project subtree follows this move."
                    else if dialog.taskParent /= Nothing then "Moving to another project detaches this subtask from its parent and makes it a top-level task."
                    else "Live subtasks follow this task to its destination.") ]
                , p [ class "move-effects" ] [ text "Paused destinations can block readiness. Running work cannot enter paused ancestry. Dependencies, including closed dependencies, must stay in the same project." ]
                , dialog.error |> Maybe.map (\message -> div [ class "move-load-error", attribute "role" "alert" ] [ text message ]) |> Maybe.withDefault (text "")
                , div [ class "modal-actions" ]
                    [ button [ class "btn", onClick CloseMove ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", disabled (saving || not valid || not dialog.focusComplete), onClick MoveConfirm ] [ text (if saving then "Moving…" else "Move") ]
                    ]
                ]
