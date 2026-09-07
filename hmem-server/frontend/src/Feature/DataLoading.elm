module Feature.DataLoading exposing (acceptWorkspaceLoad, beginNavigationBranch, beginNavigationBranchPage, beginNavigationBranchPreviousPage, beginNavigationFocus, beginRootNavigation, beginRootNavigationPage, beginRootNavigationPreviousPage, ensureAllNavigationPresentations, ensureNavigationPresentation, finishWorkspaceLoad, init, listObservationResponseMatches, mergeNavigationSummaries, mergeObservationPage, nextPageOffset, observationResponseMatches, prepareForPageLoad, prepareRootNavigationRequest, resetNavigationPresentations, revalidateNavigationForFilters, revalidateNavigationForAffectedBranches, reloadNavigationForFilters, update)

import Api
import Dict
import Feature.Observation
import Helpers exposing (indexBy, navigationPresentationWindow, presentationOrdinaryCapacity)
import Permissions
import Set
import String
import Toast exposing (addToast)
import Types exposing (..)
import Url


init : DataLoadingModel
init =
    { loadingWorkspaces = True
    , activeWorkspaceListLoadToken = Nothing
    , nextWorkspaceListLoadToken = 1
    , loadingWorkspaceData = False
    , pendingWorkspaceLoads = 0
    , activeWorkspaceLoadToken = Nothing
    , nextWorkspaceLoadToken = 1
    , cardHydrationLoaded = False
    , navigationGeneration = 0
    , rootNavigationRequest = Nothing
    , loadedNavigationBranches = Dict.empty
    , rootNavigationPresentation = Nothing
    , navigationPresentations = Dict.empty
    , projectCardSummaries = Dict.empty
    , taskCardSummaries = Dict.empty
    , projectCardDetailRequests = Dict.empty
    , taskCardDetailRequests = Dict.empty
    , nextCardDetailRequestId = 1
    , navigationVisibleProjectIds = Set.empty
    , navigationVisibleTaskIds = Set.empty
    , navigationVisibilityActive = False
    , activeNavigationFocus = Nothing
    , navigationFocuses = Dict.empty
    }


navigationFilterFingerprint : Model -> String
navigationFilterFingerprint model =
    let
        showOnly =
            case model.search.filterShowOnly of
                ShowAll -> "all"
                ShowProjectsOnly -> "projects"
                ShowTasksOnly -> "tasks"

        priority =
            case model.search.filterPriority of
                AnyPriority -> "any"
                ExactPriority value -> "exact:" ++ String.fromInt value
                AbovePriority value -> "above:" ++ String.fromInt value
                BelowPriority value -> "below:" ++ String.fromInt value
    in
    String.join "|" [ showOnly, priority, String.join "," (List.sort model.search.filterProjectStatuses), String.join "," (List.sort model.search.filterTaskStatuses), String.trim model.search.query ]


navigationFilterQuery : Model -> String
navigationFilterQuery model =
    let
        showOnly =
            case model.search.filterShowOnly of
                ShowAll -> ""
                ShowProjectsOnly -> "&show_only=projects"
                ShowTasksOnly -> "&show_only=tasks"

        priority =
            case model.search.filterPriority of
                AnyPriority -> "&priority_mode=any"
                ExactPriority value -> "&priority_mode=exact&priority_value=" ++ String.fromInt value
                AbovePriority value -> "&priority_mode=above&priority_value=" ++ String.fromInt value
                BelowPriority value -> "&priority_mode=below&priority_value=" ++ String.fromInt value

        projectStatuses =
            model.search.filterProjectStatuses |> List.map (\status -> "&project_status=" ++ status) |> String.concat

        taskStatuses =
            model.search.filterTaskStatuses |> List.map (\status -> "&task_status=" ++ status) |> String.concat

        query =
            case String.trim model.search.query of
                "" -> ""
                value -> "&query=" ++ Url.percentEncode value
    in
    showOnly ++ priority ++ projectStatuses ++ taskStatuses ++ query


navigationBranchKey : String -> Maybe String -> String
navigationBranchKey parentKind maybeParentId =
    parentKind ++ ":" ++ Maybe.withDefault "root" maybeParentId


beginNavigationBranch : String -> String -> Maybe String -> Model -> ( Model, Cmd Msg )
beginNavigationBranch parentKind workspaceId maybeParentId model =
    let
        key =
            navigationBranchKey parentKind maybeParentId

        generation =
            model.dataLoading.navigationGeneration + 1

        request =
            { workspaceId = workspaceId
            , sessionEpoch = model.sessionRequestEpoch
            , generation = generation
                , filterFingerprint = navigationFilterFingerprint model
                , projectOffset = 0
                , taskOffset = 0
                , inFlight = True
                , succeeded = False
                , projectHasMore = False
                , taskHasMore = False
                , projectCardCount = 0
                , taskCardCount = 0
                , projectRequestPending = True
                , taskRequestPending = True
            }

        currentDataLoading =
            model.dataLoading

        dataLoading =
            { currentDataLoading
                | navigationGeneration = generation
                , loadedNavigationBranches = Dict.insert key request model.dataLoading.loadedNavigationBranches
                , navigationPresentations = Dict.insert key (initialPresentation request) model.dataLoading.navigationPresentations
            }
    in
    ( { model | dataLoading = dataLoading }
    , Api.fetchNavigationBranch model.flags.apiUrl workspaceId parentKind maybeParentId 0 0 (navigationFilterQuery model)
        (GotNavigationBranch workspaceId model.sessionRequestEpoch generation key (navigationFilterFingerprint model) 0 0)
    )


transportPageSize : Int
transportPageSize =
    50


initialPresentation : NavigationBranchState -> NavigationPresentationState
initialPresentation request =
    { workspaceId = request.workspaceId
    , sessionEpoch = request.sessionEpoch
    , generation = request.generation
    , filterFingerprint = request.filterFingerprint
    , projectOffset = 0
    , taskOffset = 0
    }


{-| Focus and collapse can change the pinned-path capacity.  Presentation
cursors count ordinary cards, so discard only those reversible cursors when
that capacity changes; cached transport membership and its stale guards remain
untouched. -}
resetNavigationPresentations : Model -> Model
resetNavigationPresentations model =
    let
        loading =
            model.dataLoading
    in
    { model
        | dataLoading =
            { loading
                | rootNavigationPresentation = loading.rootNavigationRequest |> Maybe.map initialPresentation
                , navigationPresentations = Dict.map (\_ request -> initialPresentation request) loading.loadedNavigationBranches
            }
    }


presentationFor : NavigationBranchState -> Maybe NavigationPresentationState -> NavigationPresentationState
presentationFor request maybePresentation =
    case maybePresentation of
        Just presentation ->
            if presentation.workspaceId == request.workspaceId && presentation.sessionEpoch == request.sessionEpoch && presentation.generation == request.generation && presentation.filterFingerprint == request.filterFingerprint then
                presentation

            else
                initialPresentation request

        Nothing ->
            initialPresentation request


presentationPinnedIds : String -> Model -> Set.Set String
presentationPinnedIds entityKind model =
    let
        editTarget =
            case model.editing.editState of
                Just (EditingField state) ->
                    Just ( state.entityType, state.entityId )

                Nothing ->
                    Nothing

        taskPath taskId =
            let
                climb current seen =
                    if Set.member current seen then
                        seen

                    else
                        case Dict.get current model.tasks of
                            Just task ->
                                climb (Maybe.withDefault "" task.parentId) (Set.insert current seen)

                            Nothing ->
                                seen
            in
            climb taskId Set.empty

        projectPath projectId =
            let
                climb current seen =
                    if Set.member current seen then
                        seen

                    else
                        case Dict.get current model.projects of
                            Just project ->
                                climb (Maybe.withDefault "" project.parentId) (Set.insert current seen)

                            Nothing ->
                                seen
            in
            climb projectId Set.empty

        focused =
            model.focus.focusedEntity

        edited =
            editTarget

        inline =
            case model.editing.inlineCreate of
                Just (InlineCreateProject state) ->
                    Just ( "project", Maybe.withDefault "" state.parentId )

                Just (InlineCreateTask state) ->
                    if entityKind == "project" then
                        Maybe.map (Tuple.pair "project") state.projectId

                    else
                        Maybe.map (Tuple.pair "task") state.parentId

                Just (InlineCreateMemory _) ->
                    Nothing

                Nothing ->
                    Nothing

        roots =
            [ focused, edited, inline ]
                |> List.filterMap identity
    in
    roots
        |> List.foldl
            (\( kind, entityId ) pinned ->
                if String.isEmpty entityId then
                    pinned

                else if entityKind == "task" then
                    if kind == "task" then
                        Set.union pinned (taskPath entityId)

                    else
                        pinned

                else if kind == "project" then
                    Set.union pinned (projectPath entityId)

                else
                    model.tasks
                        |> Dict.get entityId
                        |> Maybe.andThen .projectId
                        |> Maybe.map projectPath
                        |> Maybe.map (Set.union pinned)
                        |> Maybe.withDefault pinned
            )
            Set.empty


localPresentationPinnedIds : Maybe ( String, String ) -> String -> Model -> Set.Set String
localPresentationPinnedIds maybeParent entityKind model =
    let
        workspaceId =
            Maybe.withDefault "" model.selectedWorkspaceId

        isVisible entityId =
            not model.dataLoading.navigationVisibilityActive
                || (if entityKind == "project" then
                        Set.member entityId model.dataLoading.navigationVisibleProjectIds

                    else
                        Set.member entityId model.dataLoading.navigationVisibleTaskIds
                   )

        isLocalPinned entityId =
            if entityKind == "project" then
                Dict.get entityId model.projects
                    |> Maybe.map
                        (\project ->
                            isVisible entityId
                                && project.workspaceId == workspaceId
                                && (case maybeParent of
                                        Just ( "project", parentId ) ->
                                            project.parentId == Just parentId

                                        _ ->
                                            project.parentId == Nothing
                                   )
                        )
                    |> Maybe.withDefault False

            else
                Dict.get entityId model.tasks
                    |> Maybe.map
                        (\task ->
                            isVisible entityId
                                && task.workspaceId == workspaceId
                                && (case maybeParent of
                                        Just ( "project", parentId ) ->
                                            task.projectId == Just parentId && task.parentId == Nothing

                                        Just ( "task", parentId ) ->
                                            task.parentId == Just parentId

                                        _ ->
                                            task.projectId == Nothing && task.parentId == Nothing
                                   )
                        )
                    |> Maybe.withDefault False

    in
    presentationPinnedIds entityKind model
        |> Set.filter isLocalPinned


presentationCapacity : String -> Maybe ( String, String ) -> String -> Model -> Int
presentationCapacity _ maybeParent entityKind model =
    localPresentationPinnedIds maybeParent entityKind model
        |> Set.size
        |> presentationOrdinaryCapacity


cachedOrdinaryCount : Maybe ( String, String ) -> String -> Model -> NavigationBranchState -> Int
cachedOrdinaryCount maybeParent entityKind model transport =
    Basics.max 0
        ((if entityKind == "project" then transport.projectCardCount else transport.taskCardCount)
            - (localPresentationPinnedIds maybeParent entityKind model |> Set.size)
        )


advancePresentation : String -> Maybe ( String, String ) -> String -> Model -> NavigationBranchState -> NavigationPresentationState -> NavigationPresentationState
advancePresentation parentKind maybeParent entityKind model transport presentation =
    let
        capacity =
            presentationCapacity parentKind maybeParent entityKind model

        nextOffset currentOffset =
            Basics.min (currentOffset + capacity) (cachedOrdinaryCount maybeParent entityKind model transport)
    in
    if entityKind == "project" then
        { presentation | projectOffset = nextOffset presentation.projectOffset }

    else
        { presentation | taskOffset = nextOffset presentation.taskOffset }


retreatPresentation : String -> Maybe ( String, String ) -> String -> Model -> NavigationPresentationState -> NavigationPresentationState
retreatPresentation parentKind maybeParent entityKind model presentation =
    if entityKind == "project" then
        { presentation | projectOffset = max 0 (presentation.projectOffset - presentationCapacity parentKind maybeParent entityKind model) }

    else
        { presentation | taskOffset = max 0 (presentation.taskOffset - presentationCapacity parentKind maybeParent entityKind model) }


presentationNeedsTransport : String -> Maybe ( String, String ) -> String -> Model -> NavigationPresentationState -> NavigationBranchState -> Bool
presentationNeedsTransport _ maybeParent entityKind model presentation transport =
    let
        presentationOffset =
            if entityKind == "project" then
                presentation.projectOffset

            else
                presentation.taskOffset

        cachedOrdinaryEnd =
            cachedOrdinaryCount maybeParent entityKind model transport
    in
    presentationOffset >= cachedOrdinaryEnd


{-| Advance one presentation window.  A new network page is requested only
when that window leaves the cached transport page; retries retain the same
transport offsets and identity guard. -}
beginNavigationBranchPage : String -> String -> String -> Model -> ( Model, Cmd Msg )
beginNavigationBranchPage parentKind parentId entityKind model =
    case ( model.selectedWorkspaceId, Dict.get (navigationBranchKey parentKind (Just parentId)) model.dataLoading.loadedNavigationBranches ) of
        ( Just workspaceId, Just previous ) ->
            let
                key =
                    navigationBranchKey parentKind (Just parentId)

                previousPresentation =
                    presentationFor previous (Dict.get key model.dataLoading.navigationPresentations)

                retrying =
                    not previous.succeeded

                nextPresentation =
                    if retrying then
                        previousPresentation

                    else
                        advancePresentation parentKind (Just ( parentKind, parentId )) entityKind model previous previousPresentation

                hasMore =
                    if entityKind == "project" then previous.projectHasMore else previous.taskHasMore

                needsTransport =
                    retrying || presentationNeedsTransport parentKind (Just ( parentKind, parentId )) entityKind model nextPresentation previous

                canAdvance =
                    not previous.inFlight && (retrying || not needsTransport || hasMore)

                canLoad =
                    canAdvance && needsTransport

                projectOffset =
                    if retrying then
                        previous.projectOffset

                    else if entityKind == "project" && canLoad then
                        previous.projectOffset + transportPageSize

                    else
                        previous.projectOffset

                taskOffset =
                    if retrying then
                        previous.taskOffset

                    else if entityKind == "task" && canLoad then
                        previous.taskOffset + transportPageSize

                    else
                        previous.taskOffset

                request =
                    { previous
                        | projectOffset = projectOffset
                        , taskOffset = taskOffset
                        , inFlight = canLoad
                        , succeeded = if canLoad then False else previous.succeeded
                        , projectRequestPending = if canLoad then (if retrying then previous.projectRequestPending else entityKind == "project") else previous.projectRequestPending
                        , taskRequestPending = if canLoad then (if retrying then previous.taskRequestPending else entityKind == "task") else previous.taskRequestPending
                    }

                currentLoading =
                    model.dataLoading
            in
            if not canAdvance then
                ( model, Cmd.none )

            else if canLoad then
                ( { model
                    | dataLoading =
                        { currentLoading
                            | loadedNavigationBranches = Dict.insert key request currentLoading.loadedNavigationBranches
                            , navigationPresentations = Dict.insert key nextPresentation currentLoading.navigationPresentations
                        }
                  }
                , Api.fetchNavigationBranch model.flags.apiUrl workspaceId parentKind (Just parentId) projectOffset taskOffset (navigationFilterQuery model)
                    (GotNavigationBranch workspaceId model.sessionRequestEpoch request.generation key request.filterFingerprint projectOffset taskOffset)
                )

            else
                { model | dataLoading = { currentLoading | navigationPresentations = Dict.insert key nextPresentation currentLoading.navigationPresentations } }
                    |> ensureNavigationPresentation parentKind (Just parentId)

        _ ->
            ( model, Cmd.none )


beginNavigationBranchPreviousPage : String -> String -> String -> Model -> ( Model, Cmd Msg )
beginNavigationBranchPreviousPage parentKind parentId entityKind model =
    let
        key =
            navigationBranchKey parentKind (Just parentId)
    in
    case Dict.get key model.dataLoading.loadedNavigationBranches of
        Just request ->
            let
                previous =
                    presentationFor request (Dict.get key model.dataLoading.navigationPresentations)

                loading =
                    model.dataLoading
            in
            { model
                | dataLoading =
                    { loading
                        | navigationPresentations = Dict.insert key (retreatPresentation parentKind (Just ( parentKind, parentId )) entityKind model previous) model.dataLoading.navigationPresentations
                    }
            }
                |> ensureNavigationPresentation parentKind (Just parentId)

        Nothing ->
            ( model, Cmd.none )


beginRootNavigation : Model -> Cmd Msg
beginRootNavigation model =
    case ( model.selectedWorkspaceId, model.dataLoading.activeWorkspaceLoadToken ) of
        ( Just workspaceId, Just token ) ->
            Api.fetchRootNavigation model.flags.apiUrl workspaceId (navigationFilterQuery model)
                (GotRootNavigation workspaceId model.sessionRequestEpoch (Just token) model.dataLoading.navigationGeneration (navigationFilterFingerprint model) 0 0)

        _ ->
            Cmd.none


{-| Root presentation is also independent from its transport cursor. -}
beginRootNavigationPage : String -> Model -> ( Model, Cmd Msg )
beginRootNavigationPage entityKind model =
    case ( model.selectedWorkspaceId, model.dataLoading.rootNavigationRequest ) of
        ( Just workspaceId, Just previous ) ->
            let
                previousPresentation =
                    presentationFor previous model.dataLoading.rootNavigationPresentation

                retrying =
                    not previous.succeeded

                nextPresentation =
                    if retrying then
                        previousPresentation

                    else
                        advancePresentation "root" Nothing entityKind model previous previousPresentation

                hasMore =
                    if entityKind == "project" then previous.projectHasMore else previous.taskHasMore

                needsTransport =
                    retrying || presentationNeedsTransport "root" Nothing entityKind model nextPresentation previous

                canAdvance =
                    not previous.inFlight && (retrying || not needsTransport || hasMore)

                canLoad =
                    canAdvance && needsTransport

                projectOffset =
                    if retrying then
                        previous.projectOffset

                    else if entityKind == "project" && canLoad then
                        previous.projectOffset + transportPageSize

                    else
                        previous.projectOffset

                taskOffset =
                    if retrying then
                        previous.taskOffset

                    else if entityKind == "task" && canLoad then
                        previous.taskOffset + transportPageSize

                    else
                        previous.taskOffset

                request =
                    { previous
                        | projectOffset = projectOffset
                        , taskOffset = taskOffset
                        , inFlight = canLoad
                        , succeeded = if canLoad then False else previous.succeeded
                        , projectRequestPending = if canLoad then (if retrying then previous.projectRequestPending else entityKind == "project") else previous.projectRequestPending
                        , taskRequestPending = if canLoad then (if retrying then previous.taskRequestPending else entityKind == "task") else previous.taskRequestPending
                    }

                loading =
                    model.dataLoading
            in
            if not canAdvance then
                ( model, Cmd.none )

            else if canLoad then
                ( { model | dataLoading = { loading | rootNavigationRequest = Just request, rootNavigationPresentation = Just nextPresentation } }
                , Api.fetchRootNavigation model.flags.apiUrl workspaceId (navigationFilterQuery model)
                    (GotRootNavigation workspaceId model.sessionRequestEpoch Nothing request.generation request.filterFingerprint projectOffset taskOffset)
                )

            else
                { model | dataLoading = { loading | rootNavigationPresentation = Just nextPresentation } }
                    |> ensureNavigationPresentation "workspace_root" Nothing

        _ ->
            ( model, Cmd.none )


beginRootNavigationPreviousPage : String -> Model -> ( Model, Cmd Msg )
beginRootNavigationPreviousPage entityKind model =
    case model.dataLoading.rootNavigationRequest of
        Just request ->
            let
                previous =
                    presentationFor request model.dataLoading.rootNavigationPresentation

                loading =
                    model.dataLoading
            in
            { model | dataLoading = { loading | rootNavigationPresentation = Just (retreatPresentation "root" Nothing entityKind model previous) } }
                |> ensureNavigationPresentation "workspace_root" Nothing

        Nothing ->
            ( model, Cmd.none )


{-| Install the exact root-branch guard before AppShell issues bootstrap
navigation.  The command is dispatched from the session bootstrap, while the
model transition happens in the session reducer; keeping them paired prevents a
valid first response from being mistaken for a stale one.
-}
prepareRootNavigationRequest : Maybe String -> Model -> Model
prepareRootNavigationRequest expectedWorkspace model =
    case ( expectedWorkspace, model.selectedWorkspaceId, model.dataLoading.activeWorkspaceLoadToken ) of
        ( Just workspaceId, Just selectedWorkspaceId, Just _ ) ->
            if workspaceId == selectedWorkspaceId then
                let
                    loading =
                        model.dataLoading
                in
                { model
                    | dataLoading =
                        let
                            request =
                                { workspaceId = workspaceId
                                , sessionEpoch = model.sessionRequestEpoch
                                , generation = loading.navigationGeneration
                                , filterFingerprint = navigationFilterFingerprint model
                                , projectOffset = 0
                                , taskOffset = 0
                                , inFlight = True
                                , succeeded = False
                                , projectHasMore = False
                                , taskHasMore = False
                                , projectCardCount = 0
                                , taskCardCount = 0
                                , projectRequestPending = True
                                , taskRequestPending = True
                                }
                        in
                        { loading
                            | rootNavigationRequest = Just request
                            , rootNavigationPresentation = Just (initialPresentation request)
                            , navigationVisibilityActive = True
                        }
                }

            else
                model

        _ ->
            model


reloadNavigationForFilters : Model -> ( Model, Cmd Msg )
reloadNavigationForFilters model =
    case model.selectedWorkspaceId of
        Nothing ->
            ( model, Cmd.none )

        Just workspaceId ->
            let
                current =
                    model.dataLoading

                generation =
                    current.navigationGeneration + 1

                resetLoading =
                    { current
                        | loadingWorkspaceData = False
                        , pendingWorkspaceLoads = 0
                        , activeWorkspaceLoadToken = Nothing
                        , navigationGeneration = generation
                        , loadedNavigationBranches = Dict.empty
                        , rootNavigationPresentation = Nothing
                        , navigationPresentations = Dict.empty
                        , projectCardSummaries = Dict.empty
                        , taskCardSummaries = Dict.empty
                        , projectCardDetailRequests = Dict.empty
                        , taskCardDetailRequests = Dict.empty
                        , navigationVisibleProjectIds = Set.empty
                        , navigationVisibleTaskIds = Set.empty
                        , navigationVisibilityActive = True
                        , activeNavigationFocus = Nothing
                        , navigationFocuses = Dict.empty
                        , rootNavigationRequest =
                            Just
                                { workspaceId = workspaceId
                                , sessionEpoch = model.sessionRequestEpoch
                                , generation = generation
                                , filterFingerprint = navigationFilterFingerprint model
                                , projectOffset = 0
                                , taskOffset = 0
                                , inFlight = True
                                , succeeded = False
                                , projectHasMore = False
                                , taskHasMore = False
                                , projectCardCount = 0
                                , taskCardCount = 0
                                , projectRequestPending = True
                                , taskRequestPending = True
                                }
                    }

                resetModel =
                    { model | dataLoading = resetLoading }
            in
            ( resetModel
            , Api.fetchRootNavigation model.flags.apiUrl workspaceId (navigationFilterQuery resetModel)
                (GotRootNavigation workspaceId model.sessionRequestEpoch Nothing generation (navigationFilterFingerprint resetModel) 0 0)
            )


{-| Revalidate a filtered navigation tree after a live card update without
dropping already-expanded branch membership.  A root response cannot prove a
descendant branch no longer matches, so clearing those IDs before replay would
make matching expanded cards disappear permanently.  The pending root request
still establishes a new generation; branch responses remain authoritative.
-}
revalidateNavigationForFilters : Model -> ( Model, Cmd Msg )
revalidateNavigationForFilters model =
    revalidateNavigationForAffectedBranches [] [] model


{-| In addition to branches the user has already opened, revalidate a changed
card's new direct owner.  This is essential for a reparent into an unloaded
branch: the old branch cannot establish membership after the summary has its
new parent, while the new branch can authoritatively re-admit or remove it.
-}
revalidateNavigationForAffectedBranches : List Api.ProjectCardSummary -> List Api.TaskCardSummary -> Model -> ( Model, Cmd Msg )
revalidateNavigationForAffectedBranches affectedProjects affectedTasks model =
    let
        preserved =
            model.dataLoading

        ( reloaded, command ) =
            reloadNavigationForFilters model

        loading =
            reloaded.dataLoading

        preservedModel =
            { reloaded
                | dataLoading =
                    { loading
                        | projectCardSummaries = preserved.projectCardSummaries
                        , taskCardSummaries = preserved.taskCardSummaries
                        , navigationVisibleProjectIds = preserved.navigationVisibleProjectIds
                        , navigationVisibleTaskIds = preserved.navigationVisibleTaskIds
                    }
            }

        ( replayedModel, replayCommands ) =
            replayLoadedNavigationBranches preserved.loadedNavigationBranches preservedModel

        ( affectedModel, affectedCommands ) =
            replayAffectedNavigationBranches affectedProjects affectedTasks replayedModel
    in
    ( affectedModel, Cmd.batch (command :: replayCommands ++ affectedCommands) )


{-| Replay every already-loaded expanded branch with the root's new generation.
Keeping the old cards visible prevents flicker while the requests are pending;
each successful response replaces membership for exactly its branch scope.
-}
replayLoadedNavigationBranches : Dict.Dict String NavigationBranchState -> Model -> ( Model, List (Cmd Msg) )
replayLoadedNavigationBranches previousBranches model =
    case model.selectedWorkspaceId of
        Nothing ->
            ( model, [] )

        Just workspaceId ->
            let
                generation =
                    model.dataLoading.navigationGeneration

                fingerprint =
                    navigationFilterFingerprint model

                replay ( branchKey, previous ) ( currentModel, commands ) =
                    case String.split ":" branchKey of
                        [ "project", parentId ] ->
                            replayBranch workspaceId generation fingerprint "project" parentId previous currentModel commands

                        [ "task", parentId ] ->
                            replayBranch workspaceId generation fingerprint "task" parentId previous currentModel commands

                        _ ->
                            ( currentModel, commands )
            in
            List.foldl replay ( model, [] ) (Dict.toList previousBranches)


replayAffectedNavigationBranches : List Api.ProjectCardSummary -> List Api.TaskCardSummary -> Model -> ( Model, List (Cmd Msg) )
replayAffectedNavigationBranches affectedProjects affectedTasks model =
    case model.selectedWorkspaceId of
        Nothing ->
            ( model, [] )

        Just workspaceId ->
            let
                projectTargets =
                    affectedProjects
                        |> List.filterMap (.parentId >> Maybe.map (\parentId -> ( "project", parentId )))

                taskTargets =
                    affectedTasks
                        |> List.filterMap
                            (\summary ->
                                case ( summary.parentId, summary.projectId ) of
                                    ( Just parentId, _ ) ->
                                        Just ( "task", parentId )

                                    ( Nothing, Just projectId ) ->
                                        Just ( "project", projectId )

                                    _ ->
                                        Nothing
                            )

                targets =
                    Set.fromList (projectTargets ++ taskTargets)
                        |> Set.toList

                replay target ( currentModel, commands ) =
                    let
                        ( parentKind, parentId ) =
                            target

                        branchKey =
                            navigationBranchKey parentKind (Just parentId)
                    in
                    if Dict.member branchKey currentModel.dataLoading.loadedNavigationBranches then
                        ( currentModel, commands )

                    else
                        replayBranch workspaceId currentModel.dataLoading.navigationGeneration (navigationFilterFingerprint currentModel) parentKind parentId initialNavigationBranchState currentModel commands
            in
            List.foldl replay ( model, [] ) targets


initialNavigationBranchState : NavigationBranchState
initialNavigationBranchState =
    { workspaceId = ""
    , sessionEpoch = 0
    , generation = 0
    , filterFingerprint = ""
    , projectOffset = 0
    , taskOffset = 0
    , inFlight = False
    , succeeded = False
    , projectHasMore = False
    , taskHasMore = False
    , projectCardCount = 0
    , taskCardCount = 0
    , projectRequestPending = False
    , taskRequestPending = False
    }


replayBranch : String -> Int -> String -> String -> String -> NavigationBranchState -> Model -> List (Cmd Msg) -> ( Model, List (Cmd Msg) )
replayBranch workspaceId generation fingerprint parentKind parentId previous model commands =
    let
        branchKey =
            navigationBranchKey parentKind (Just parentId)

        request =
            { previous
                | workspaceId = workspaceId
                , sessionEpoch = model.sessionRequestEpoch
                , generation = generation
                , filterFingerprint = fingerprint
                , projectOffset = 0
                , taskOffset = 0
                , inFlight = True
                , succeeded = False
                , projectHasMore = False
                , taskHasMore = False
                , projectCardCount = 0
                , taskCardCount = 0
                , projectRequestPending = True
                , taskRequestPending = True
            }

        loading =
            model.dataLoading

        replayed =
            { model
                | dataLoading =
                    { loading
                        | loadedNavigationBranches = Dict.insert branchKey request loading.loadedNavigationBranches
                        , navigationPresentations = Dict.insert branchKey (initialPresentation request) loading.navigationPresentations
                    }
            }

        command =
            Api.fetchNavigationBranch model.flags.apiUrl workspaceId parentKind (Just parentId) 0 0 (navigationFilterQuery replayed)
                (GotNavigationBranch workspaceId model.sessionRequestEpoch generation branchKey fingerprint 0 0)
    in
    ( replayed, command :: commands )


beginNavigationFocus : String -> String -> String -> Model -> ( Model, Cmd Msg )
beginNavigationFocus workspaceId entityType entityId model =
    let
        alreadyPresent =
            case entityType of
                "project" -> Dict.member entityId model.projects
                "task" -> Dict.member entityId model.tasks
                _ -> True

        generation =
            model.dataLoading.navigationGeneration + 1

        requestKey =
            entityType ++ ":" ++ entityId

        previousRequest =
            Dict.get requestKey model.dataLoading.navigationFocuses

        requestedAncestorOffset =
            previousRequest
                |> Maybe.map .ancestorOffset
                |> Maybe.withDefault 0

        retryable =
            previousRequest
                |> Maybe.map (\previous -> not previous.inFlight && not previous.succeeded)
                |> Maybe.withDefault False

        request =
            { workspaceId = workspaceId
            , sessionEpoch = model.sessionRequestEpoch
            , generation = generation
            , filterFingerprint = navigationFilterFingerprint model
            , entityType = entityType
            , entityId = entityId
            , ancestorOffset = requestedAncestorOffset
            , inFlight = True
            , succeeded = False
            }

        currentDataLoading =
            model.dataLoading

        alreadyInFlight =
            case currentDataLoading.activeNavigationFocus of
                Just activeRequest ->
                    activeRequest.inFlight
                        && activeRequest.workspaceId == workspaceId
                        && activeRequest.entityType == entityType
                        && activeRequest.entityId == entityId

                Nothing ->
                    False
    in
    if (alreadyPresent && not retryable) || alreadyInFlight || (entityType /= "project" && entityType /= "task") then
        ( model, Cmd.none )

    else
        ( { model | dataLoading = { currentDataLoading | navigationGeneration = generation, activeNavigationFocus = Just request, navigationFocuses = Dict.insert requestKey request currentDataLoading.navigationFocuses } }
        , Api.fetchNavigationFocus model.flags.apiUrl workspaceId entityType entityId requestedAncestorOffset
            (GotNavigationFocus workspaceId model.sessionRequestEpoch generation (navigationFilterFingerprint model) entityType entityId requestedAncestorOffset)
        )


prepareForPageLoad : Page -> DataLoadingModel -> DataLoadingModel
prepareForPageLoad page dataLoading =
    { dataLoading
        | loadingWorkspaceData =
            case page of
                WorkspacePage _ ->
                    True

                _ ->
                    False
        , pendingWorkspaceLoads =
            case page of
                WorkspacePage _ ->
                    0

                _ ->
                    0
        , activeWorkspaceLoadToken =
            case page of
                WorkspacePage _ ->
                    Just dataLoading.nextWorkspaceLoadToken

                _ ->
                    Nothing
        , cardHydrationLoaded = False
        , nextWorkspaceLoadToken =
            case page of
                WorkspacePage _ ->
                    dataLoading.nextWorkspaceLoadToken + 1

                _ ->
                    dataLoading.nextWorkspaceLoadToken
    }


finishWorkspaceLoad : Maybe Int -> DataLoadingModel -> DataLoadingModel
finishWorkspaceLoad maybeToken dataLoading =
    let
        remaining =
            case maybeToken of
                Just token ->
                    if dataLoading.activeWorkspaceLoadToken == Just token then
                        max 0 (dataLoading.pendingWorkspaceLoads - 1)

                    else
                        dataLoading.pendingWorkspaceLoads

                Nothing ->
                    dataLoading.pendingWorkspaceLoads
    in
    { dataLoading
        | pendingWorkspaceLoads = remaining
        , loadingWorkspaceData = dataLoading.loadingWorkspaceData && remaining > 0
        , activeWorkspaceLoadToken =
            if remaining == 0 then
                Nothing

            else
                dataLoading.activeWorkspaceLoadToken
        , cardHydrationLoaded = remaining == 0
    }


addInitialHydrationWork : Maybe Int -> Int -> DataLoadingModel -> DataLoadingModel
addInitialHydrationWork maybeToken count dataLoading =
    if count > 0 && acceptWorkspaceLoad maybeToken dataLoading then
        { dataLoading | pendingWorkspaceLoads = dataLoading.pendingWorkspaceLoads + count, loadingWorkspaceData = True, cardHydrationLoaded = False }

    else
        dataLoading


mergeTaskDependencyLinks : String -> List Api.TaskDependencySummary -> List Api.WorkspaceTaskDependencyLink -> List Api.WorkspaceTaskDependencyLink
mergeTaskDependencyLinks taskId summaries links =
    let
        withoutTask =
            List.filter (\link -> link.taskId /= taskId) links
    in
    withoutTask ++ List.map (\summary -> { taskId = taskId, dependsOnId = summary.id }) summaries


acceptWorkspaceLoad : Maybe Int -> DataLoadingModel -> Bool
acceptWorkspaceLoad maybeToken dataLoading =
    case maybeToken of
        Just token ->
            dataLoading.activeWorkspaceLoadToken == Just token

        Nothing ->
            True


maxWorkspacePageOffset : Int
maxWorkspacePageOffset =
    10000


nextPageOffset : Int -> Api.PaginatedResult a -> Maybe Int
nextPageOffset offset paginated =
    let
        nextOffset =
            offset + List.length paginated.items
    in
    if paginated.hasMore && not (List.isEmpty paginated.items) && nextOffset <= maxWorkspacePageOffset then
        Just nextOffset

    else
        Nothing


mergePageById : Int -> List { item | id : String } -> Dict.Dict String { item | id : String } -> Dict.Dict String { item | id : String }
mergePageById offset items existing =
    let
        pageItems =
            indexBy .id items
    in
    if offset == 0 then
        pageItems

    else
        Dict.union pageItems existing


{-| A navigation response is authoritative for its own direct-child scope.
Retain details in the entity dictionaries, but replace card summaries and the
visible membership set so an item that has become nonmatching cannot survive a
completed filtered replay.
-}
replaceNavigationBranchMembershipForKey : String -> Api.NavigationBranchResponse -> Model -> Model
replaceNavigationBranchMembershipForKey branchKey navigation model =
    case String.split ":" branchKey of
        [ parentKind, parentId ] ->
            replaceNavigationBranchMembership parentKind (Just parentId) navigation model

        _ ->
            mergeNavigationSummaries navigation.projects.items navigation.tasks.items model


replaceNavigationBranchMembership : String -> Maybe String -> Api.NavigationBranchResponse -> Model -> Model
replaceNavigationBranchMembership parentKind maybeParentId navigation model =
    let
        matchesProject summary =
            case ( parentKind, maybeParentId ) of
                ( "workspace_root", Nothing ) ->
                    summary.parentId == Nothing

                ( "project", Just parentId ) ->
                    summary.parentId == Just parentId

                _ ->
                    False

        matchesTask summary =
            case ( parentKind, maybeParentId ) of
                ( "workspace_root", Nothing ) ->
                    summary.projectId == Nothing && summary.parentId == Nothing

                ( "project", Just parentId ) ->
                    summary.projectId == Just parentId && summary.parentId == Nothing

                ( "task", Just parentId ) ->
                    summary.parentId == Just parentId

                _ ->
                    False

        loading =
            model.dataLoading

        pruned =
            { model
                | dataLoading =
                    { loading
                        | projectCardSummaries = Dict.filter (\_ summary -> not (matchesProject summary)) loading.projectCardSummaries
                        , taskCardSummaries = Dict.filter (\_ summary -> not (matchesTask summary)) loading.taskCardSummaries
                        , navigationVisibleProjectIds = Set.filter (\projectId -> Dict.member projectId loading.projectCardSummaries && not (Dict.get projectId loading.projectCardSummaries |> Maybe.map matchesProject |> Maybe.withDefault False)) loading.navigationVisibleProjectIds
                        , navigationVisibleTaskIds = Set.filter (\taskId -> Dict.member taskId loading.taskCardSummaries && not (Dict.get taskId loading.taskCardSummaries |> Maybe.map matchesTask |> Maybe.withDefault False)) loading.navigationVisibleTaskIds
                    }
            }
    in
    mergeNavigationSummaries navigation.projects.items navigation.tasks.items pruned


mergeNavigationSummaries : List Api.ProjectCardSummary -> List Api.TaskCardSummary -> Model -> Model
mergeNavigationSummaries projectSummaries taskSummaries model =
    let
        dependencies =
            model.dependencies

        projects =
            List.foldl
                (\summary values ->
                    let
                        card =
                            Api.projectFromCardSummary summary

                        merged =
                            Dict.get summary.id values
                                |> Maybe.map
                                    (\existing ->
                                        if existing.updatedAt == card.updatedAt then
                                            { card | description = existing.description }

                                        else
                                            card
                                    )
                                |> Maybe.withDefault card
                    in
                    Dict.insert summary.id merged values
                )
                model.projects
                projectSummaries

        tasks =
            List.foldl
                (\summary values ->
                    let
                        card =
                            Api.taskFromCardSummary summary

                        merged =
                            Dict.get summary.id values
                                |> Maybe.map
                                    (\existing ->
                                        if existing.updatedAt == card.updatedAt then
                                            { card | description = existing.description, memoryLinkCount = existing.memoryLinkCount }

                                        else
                                            { card | memoryLinkCount = existing.memoryLinkCount }
                                    )
                                |> Maybe.withDefault card
                    in
                    Dict.insert summary.id merged values
                )
                model.tasks
                taskSummaries

        projectReadinessRollups =
            List.foldl
                (\summary values -> Dict.insert summary.id summary.readinessRollup values)
                dependencies.projectReadinessRollups
                projectSummaries

        taskReadinessRollups =
            List.foldl
                (\summary values -> Dict.insert summary.id summary.readinessRollup values)
                dependencies.taskReadinessRollups
                taskSummaries

        projectCards =
            List.foldl (\summary values -> Dict.insert summary.id summary values) model.dataLoading.projectCardSummaries projectSummaries

        taskCards =
            List.foldl (\summary values -> Dict.insert summary.id summary values) model.dataLoading.taskCardSummaries taskSummaries

        updatedDataLoading =
            let
                loading =
                    model.dataLoading
            in
            { loading
                | projectCardSummaries = projectCards
                , taskCardSummaries = taskCards
                , navigationVisibleProjectIds = List.foldl (\summary values -> Set.insert summary.id values) loading.navigationVisibleProjectIds projectSummaries
                , navigationVisibleTaskIds = List.foldl (\summary values -> Set.insert summary.id values) loading.navigationVisibleTaskIds taskSummaries
            }
    in
    { model
        | projects = projects
        , tasks = tasks
        , dataLoading = updatedDataLoading
        , dependencies =
            { dependencies
                | projectReadinessRollups = projectReadinessRollups
                , taskReadinessRollups = taskReadinessRollups
            }
    }


projectSummaryBelongsTo : String -> Maybe String -> Api.ProjectCardSummary -> Bool
projectSummaryBelongsTo parentKind maybeParentId summary =
    case ( parentKind, maybeParentId ) of
        ( "workspace_root", Nothing ) ->
            summary.parentId == Nothing

        ( "project", Just parentId ) ->
            summary.parentId == Just parentId

        _ ->
            False


taskSummaryBelongsTo : String -> Maybe String -> Api.TaskCardSummary -> Bool
taskSummaryBelongsTo parentKind maybeParentId summary =
    case ( parentKind, maybeParentId ) of
        ( "workspace_root", Nothing ) ->
            summary.projectId == Nothing && summary.parentId == Nothing

        ( "project", Just parentId ) ->
            summary.projectId == Just parentId && summary.parentId == Nothing

        ( "task", Just parentId ) ->
            summary.parentId == Just parentId

        _ ->
            False


presentationOffsets : String -> Maybe String -> Model -> ( Int, Int )
presentationOffsets parentKind maybeParentId model =
    let
        presentation =
            case maybeParentId of
                Nothing ->
                    model.dataLoading.rootNavigationPresentation

                Just parentId ->
                    Dict.get (navigationBranchKey parentKind (Just parentId)) model.dataLoading.navigationPresentations
    in
    presentation
        |> Maybe.map (\value -> ( value.projectOffset, value.taskOffset ))
        |> Maybe.withDefault ( 0, 0 )


presentedNavigationSummaries : String -> Maybe String -> Model -> ( List Api.ProjectCardSummary, List Api.TaskCardSummary )
presentedNavigationSummaries parentKind maybeParentId model =
    let
        ( projectOffset, taskOffset ) =
            presentationOffsets parentKind maybeParentId model

        parent =
            Maybe.map (\parentId -> ( parentKind, parentId )) maybeParentId

        projects =
            model.dataLoading.projectCardSummaries
                |> Dict.values
                |> List.filter (projectSummaryBelongsTo parentKind maybeParentId)
                |> List.sortBy (\project -> ( Api.projectStatusOrder project.status, negate project.priority, String.toLower project.name ))
                |> navigationPresentationWindow .id (localPresentationPinnedIds parent "project" model) projectOffset

        tasks =
            model.dataLoading.taskCardSummaries
                |> Dict.values
                |> List.filter (taskSummaryBelongsTo parentKind maybeParentId)
                |> List.sortBy (\task -> ( Api.taskStatusOrder task.status, negate task.priority, String.toLower task.title ))
                |> navigationPresentationWindow .id (localPresentationPinnedIds parent "task" model) taskOffset
    in
    ( projects, tasks )


beginProjectCardDetail : Api.ProjectCardSummary -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
beginProjectCardDetail summary ( model, commands ) =
    let
        alreadyCurrent =
            Dict.get summary.id model.dataLoading.projectCardDetailRequests
                |> Maybe.map
                    (\request ->
                        (Dict.get summary.id model.projects |> Maybe.map .updatedAt)
                            == Just request.expectedUpdatedAt
                            && (request.inFlight || request.succeeded)
                    )
                |> Maybe.withDefault False
    in
    if alreadyCurrent then
        ( model, commands )

    else
        let
            loading =
                model.dataLoading

            request =
                { workspaceId = summary.workspaceId
                , sessionEpoch = model.sessionRequestEpoch
                , navigationGeneration = loading.navigationGeneration
                , requestId = loading.nextCardDetailRequestId
                , expectedUpdatedAt = summary.updatedAt
                , inFlight = True
                , succeeded = False
                }

            updatedLoading =
                { loading
                    | projectCardDetailRequests = Dict.insert summary.id request loading.projectCardDetailRequests
                    , nextCardDetailRequestId = request.requestId + 1
                }
    in
    ( { model | dataLoading = updatedLoading }
    , Api.fetchProject model.flags.apiUrl summary.id (GotProjectCardDetail request summary.id) :: commands
    )


beginTaskCardDetail : Api.TaskCardSummary -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
beginTaskCardDetail summary ( model, commands ) =
    let
        alreadyCurrent =
            Dict.get summary.id model.dataLoading.taskCardDetailRequests
                |> Maybe.map
                    (\request ->
                        (Dict.get summary.id model.tasks |> Maybe.map .updatedAt)
                            == Just request.expectedUpdatedAt
                            && (request.inFlight || request.succeeded)
                    )
                |> Maybe.withDefault False
    in
    if alreadyCurrent then
        ( model, commands )

    else
        let
            loading =
                model.dataLoading

            request =
                { workspaceId = summary.workspaceId
                , sessionEpoch = model.sessionRequestEpoch
                , navigationGeneration = loading.navigationGeneration
                , requestId = loading.nextCardDetailRequestId
                , expectedUpdatedAt = summary.updatedAt
                , inFlight = True
                , succeeded = False
                }

            updatedLoading =
                { loading
                    | taskCardDetailRequests = Dict.insert summary.id request loading.taskCardDetailRequests
                    , nextCardDetailRequestId = request.requestId + 1
                }
    in
    ( { model | dataLoading = updatedLoading }
    , Api.fetchTask model.flags.apiUrl summary.id (GotTaskCardDetail request summary.id) :: commands
    )


ensureExpandedProjectBranch : Api.ProjectCardSummary -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
ensureExpandedProjectBranch summary ( model, commands ) =
    let
        key =
            navigationBranchKey "project" (Just summary.id)

        expanded =
            not (Dict.get ("proj-" ++ summary.id) model.cards.collapsedNodes |> Maybe.withDefault False)

        needsRequest =
            Dict.get key model.dataLoading.loadedNavigationBranches
                |> Maybe.map (\request -> not request.inFlight && not request.succeeded)
                |> Maybe.withDefault True
    in
    if summary.hasChildren && expanded && needsRequest then
        case model.selectedWorkspaceId of
            Just workspaceId ->
                let
                    ( updated, command ) =
                        beginNavigationBranch "project" workspaceId (Just summary.id) model
                in
                ( updated, command :: commands )

            Nothing ->
                ( model, commands )

    else
        ( model, commands )


ensureExpandedTaskBranch : Api.TaskCardSummary -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
ensureExpandedTaskBranch summary ( model, commands ) =
    let
        key =
            navigationBranchKey "task" (Just summary.id)

        expanded =
            not (Dict.get ("task-" ++ summary.id) model.cards.collapsedNodes |> Maybe.withDefault False)

        needsRequest =
            Dict.get key model.dataLoading.loadedNavigationBranches
                |> Maybe.map (\request -> not request.inFlight && not request.succeeded)
                |> Maybe.withDefault True
    in
    if summary.hasChildren && expanded && needsRequest then
        case model.selectedWorkspaceId of
            Just workspaceId ->
                let
                    ( updated, command ) =
                        beginNavigationBranch "task" workspaceId (Just summary.id) model
                in
                ( updated, command :: commands )

            Nothing ->
                ( model, commands )

    else
        ( model, commands )


ensureSummaries : List Api.ProjectCardSummary -> List Api.TaskCardSummary -> Model -> ( Model, Cmd Msg )
ensureSummaries projects tasks model =
    let
        ( detailedProjects, projectDetailCommands ) =
            List.foldl beginProjectCardDetail ( model, [] ) projects

        ( detailedTasks, taskDetailCommands ) =
            List.foldl beginTaskCardDetail ( detailedProjects, projectDetailCommands ) tasks

        ( withProjectBranches, projectBranchCommands ) =
            List.foldl ensureExpandedProjectBranch ( detailedTasks, taskDetailCommands ) projects

        ( withTaskBranches, commands ) =
            List.foldl ensureExpandedTaskBranch ( withProjectBranches, projectBranchCommands ) tasks
    in
    ( withTaskBranches, Cmd.batch commands )


ensureNavigationPresentation : String -> Maybe String -> Model -> ( Model, Cmd Msg )
ensureNavigationPresentation parentKind maybeParentId model =
    let
        ( projects, tasks ) =
            presentedNavigationSummaries parentKind maybeParentId model
    in
    ensureSummaries projects tasks model


ensureAllNavigationPresentations : Model -> ( Model, Cmd Msg )
ensureAllNavigationPresentations model =
    let
        ensureBranch key ( current, accumulatedCommands ) =
            case String.split ":" key of
                parentKind :: parentId :: [] ->
                    let
                        ( updated, command ) =
                            ensureNavigationPresentation parentKind (Just parentId) current
                    in
                    ( updated, command :: accumulatedCommands )

                _ ->
                    ( current, accumulatedCommands )

        ( rootModel, rootCommand ) =
            ensureNavigationPresentation "workspace_root" Nothing model

        ( finalModel, commands ) =
            model.dataLoading.loadedNavigationBranches
                |> Dict.keys
                |> List.filter ((/=) (navigationBranchKey "workspace_root" Nothing))
                |> List.foldl ensureBranch ( rootModel, [ rootCommand ] )
    in
    ( finalModel, Cmd.batch commands )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadRootNavigationPage entityKind ->
            beginRootNavigationPage entityKind model

        ShowPreviousRootNavigationPage entityKind ->
            beginRootNavigationPreviousPage entityKind model

        GotWorkspaces token result ->
            if model.auth.status /= AuthReady || model.dataLoading.activeWorkspaceListLoadToken /= Just token then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        let
                            currentDataLoading =
                                model.dataLoading

                            updatedDataLoading =
                                { currentDataLoading | loadingWorkspaces = False, activeWorkspaceListLoadToken = Nothing }
                        in
                        ( { model
                            | workspaces = indexBy .id paginated.items
                            , dataLoading = updatedDataLoading
                          }
                        , Cmd.none
                        )

                    Err _ ->
                        let
                            currentDataLoading =
                                model.dataLoading

                            updatedDataLoading =
                                { currentDataLoading | loadingWorkspaces = False, activeWorkspaceListLoadToken = Nothing }
                        in
                        addToast Error
                            "Failed to load workspaces"
                            { model | dataLoading = updatedDataLoading }

        GotWorkspace expectedWsId token result ->
            if model.auth.status /= AuthReady || model.selectedWorkspaceId /= Just expectedWsId || not (Permissions.canReadCurrentWorkspace model) || model.dataLoading.activeWorkspaceLoadToken /= Just token then
                ( model, Cmd.none )

            else
                case result of
                    Ok workspace ->
                        if workspace.id == expectedWsId then
                            let
                                isRepository =
                                    workspace.workspaceType == Api.Repository

                                currentLoading =
                                    model.dataLoading

                                dataLoading =
                                    { currentLoading
                                        | loadingWorkspaceData = True
                                        , pendingWorkspaceLoads =
                                            if isRepository then
                                                2

                                             else
                                                 1
                                         , rootNavigationRequest =
                                             Just
                                                 { workspaceId = expectedWsId
                                                , sessionEpoch = model.sessionRequestEpoch
                                                , generation = currentLoading.navigationGeneration
                                                , filterFingerprint = navigationFilterFingerprint model
                                                , projectOffset = 0
                                                , taskOffset = 0
                                                , inFlight = True
                                                , succeeded = False
                                                , projectHasMore = False
                                                 , taskHasMore = False
                                                 , projectCardCount = 0
                                                 , taskCardCount = 0
                                                 , projectRequestPending = True
                                                 , taskRequestPending = True
                                                 }
                                         , rootNavigationPresentation =
                                             Just
                                                 { workspaceId = expectedWsId
                                                 , sessionEpoch = model.sessionRequestEpoch
                                                 , generation = currentLoading.navigationGeneration
                                                 , filterFingerprint = navigationFilterFingerprint model
                                                 , projectOffset = 0
                                                 , taskOffset = 0
                                                 }
                                     }

                                currentObservations =
                                    model.observations

                                observations =
                                    if isRepository then
                                        Feature.Observation.startReloadForSession model.sessionRequestEpoch expectedWsId currentObservations

                                    else
                                        { currentObservations
                                            | items = Dict.empty
                                            , orderedIds = []
                                            , hasMore = False
                                            , loading = False
                                            , error = Nothing
                                            , expectedOffset = Nothing
                                            , nextOffset = 0
                                        }

                                rootCommand =
                                    Api.fetchRootNavigation model.flags.apiUrl expectedWsId (navigationFilterQuery model) (GotRootNavigation expectedWsId model.sessionRequestEpoch (Just token) model.dataLoading.navigationGeneration (navigationFilterFingerprint model) 0 0)

                                focusCommand =
                                    case model.focus.focusedEntity of
                                        Just ( entityType, entityId ) ->
                                            Api.fetchNavigationFocus model.flags.apiUrl expectedWsId entityType entityId 0
                                                (GotNavigationFocus expectedWsId model.sessionRequestEpoch dataLoading.navigationGeneration (navigationFilterFingerprint model) entityType entityId 0)

                                        Nothing ->
                                            Cmd.none

                                commands =
                                    [ rootCommand, focusCommand ]
                                        ++ (if isRepository then
                                                [ Api.fetchObservations model.flags.apiUrl
                                                    (Feature.Observation.listQuery expectedWsId 0 observations)
                                                    (GotObservations expectedWsId (Just token) observations.requestGeneration observations.queryFingerprint 0)
                                                ]

                                            else
                                                []
                                           )
                            in
                            let
                                navigationDataLoading =
                                    case model.focus.focusedEntity of
                                        Just ( entityType, entityId ) ->
                                            { dataLoading
                                                | activeNavigationFocus =
                                                    Just
                                                        { workspaceId = expectedWsId
                                                        , sessionEpoch = model.sessionRequestEpoch
                                                        , generation = dataLoading.navigationGeneration
                                                        , filterFingerprint = navigationFilterFingerprint model
                                                         , entityType = entityType
                                                         , entityId = entityId
                                                         , ancestorOffset = 0
                                                         , inFlight = True
                                                        , succeeded = False
                                                        }
                                            }

                                        Nothing ->
                                            dataLoading

                                loadedModel =
                                    { model
                                        | workspaces = Dict.insert workspace.id workspace model.workspaces
                                        , dataLoading = navigationDataLoading
                                        , observations = observations
                                    }

                                ( detailModel, detailCmd ) =
                                    if isRepository then
                                        case observations.selectedId of
                                            Just observationId ->
                                                Feature.Observation.selectObservation observationId loadedModel

                                            Nothing ->
                                                ( loadedModel, Cmd.none )

                                    else
                                        ( loadedModel, Cmd.none )
                            in
                            ( detailModel, Cmd.batch (detailCmd :: commands) )

                        else
                            ( model, Cmd.none )

                    Err _ ->
                        let
                            currentLoading =
                                model.dataLoading

                            updatedLoading =
                                { currentLoading
                                    | loadingWorkspaceData = False
                                    , pendingWorkspaceLoads = 0
                                    , activeWorkspaceLoadToken = Nothing
                                }
                        in
                        addToast Error "Failed to load workspace" { model | dataLoading = updatedLoading }

        GotRootNavigation wsId sessionEpoch maybeToken generation fingerprint projectOffset taskOffset result ->
            let
                valid =
                    case model.dataLoading.rootNavigationRequest of
                        Just request ->
                            model.selectedWorkspaceId == Just wsId
                                && model.sessionRequestEpoch == sessionEpoch
                                && request.workspaceId == wsId
                                && request.sessionEpoch == sessionEpoch
                                && request.generation == generation
                                && request.filterFingerprint == fingerprint
                                && request.projectOffset == projectOffset
                                && request.taskOffset == taskOffset
                                && request.inFlight
                                && acceptWorkspaceLoad maybeToken model.dataLoading

                        Nothing ->
                            False
            in
            if not valid then
                ( model, Cmd.none )

            else
                case result of
                    Ok navigation ->
                        if navigation.workspaceId /= wsId then
                            ( model, Cmd.none )

                        else
                            let
                                updatedModel =
                                    (if projectOffset == 0 && taskOffset == 0 then
                                        replaceNavigationBranchMembership "workspace_root" Nothing navigation model

                                     else
                                        mergeNavigationSummaries navigation.projects.items navigation.tasks.items model
                                    )
                                        |> (\next ->
                                                let
                                                    mergedLoading =
                                                        next.dataLoading

                                                    rootBranchLoading =
                                                        { mergedLoading
                                                            | loadedNavigationBranches =
                                                                Dict.insert (navigationBranchKey "workspace_root" Nothing)
                                                                    { workspaceId = wsId
                                                                    , sessionEpoch = sessionEpoch
                                                                    , generation = generation
                                                                    , filterFingerprint = fingerprint
                                                                    , projectOffset = projectOffset
                                                                    , taskOffset = taskOffset
                                                                    , inFlight = False
                                                                    , succeeded = True
                                                                    , projectHasMore = navigation.projects.hasMore
                                                                    , taskHasMore = navigation.tasks.hasMore
                                                                    , projectCardCount =
                                                                        if projectOffset == 0 && taskOffset == 0 then
                                                                            List.length navigation.projects.items

                                                                        else
                                                                            mergedLoading.rootNavigationRequest
                                                                                |> Maybe.map
                                                                                    (\request ->
                                                                                        if request.projectRequestPending then
                                                                                            request.projectCardCount + List.length navigation.projects.items

                                                                                        else
                                                                                            request.projectCardCount
                                                                                    )
                                                                                |> Maybe.withDefault 0
                                                                    , taskCardCount =
                                                                        if projectOffset == 0 && taskOffset == 0 then
                                                                            List.length navigation.tasks.items

                                                                        else
                                                                            mergedLoading.rootNavigationRequest
                                                                                |> Maybe.map
                                                                                    (\request ->
                                                                                        if request.taskRequestPending then
                                                                                            request.taskCardCount + List.length navigation.tasks.items

                                                                                        else
                                                                                            request.taskCardCount
                                                                                    )
                                                                                |> Maybe.withDefault 0
                                                                    , projectRequestPending = False
                                                                    , taskRequestPending = False
                                                                    }
                                                                    mergedLoading.loadedNavigationBranches
                                                            , rootNavigationRequest =
                                                                mergedLoading.rootNavigationRequest
                                                                    |> Maybe.map
                                                                        (\request ->
                                                                            { request
                                                                                | inFlight = False
                                                                                , succeeded = True
                                                                                , projectHasMore = navigation.projects.hasMore
                                                                                , taskHasMore = navigation.tasks.hasMore
                                                                                , projectCardCount =
                                                                                    if projectOffset == 0 && taskOffset == 0 then
                                                                                        List.length navigation.projects.items

                                                                                    else
                                                                                        if request.projectRequestPending then
                                                                                            request.projectCardCount + List.length navigation.projects.items

                                                                                        else
                                                                                            request.projectCardCount
                                                                                , taskCardCount =
                                                                                    if projectOffset == 0 && taskOffset == 0 then
                                                                                        List.length navigation.tasks.items

                                                                                    else
                                                                                        if request.taskRequestPending then
                                                                                            request.taskCardCount + List.length navigation.tasks.items

                                                                                        else
                                                                                            request.taskCardCount
                                                                                , projectRequestPending = False
                                                                                , taskRequestPending = False
                                                                            }
                                                                        )
                                                        }
                                                in
                                                { next | dataLoading = finishWorkspaceLoad maybeToken rootBranchLoading }
                                           )
                            in
                            ensureNavigationPresentation "workspace_root" Nothing updatedModel

                    Err _ ->
                        addToast Error "Failed to load workspace navigation"
                            { model
                                | dataLoading =
                                    model.dataLoading
                                        |> finishWorkspaceLoad maybeToken
                                        |> (\loading ->
                                                { loading
                                                    | rootNavigationRequest =
                                                        loading.rootNavigationRequest
                                                            |> Maybe.map (\request -> { request | inFlight = False, succeeded = False })
                                                }
                                           )
                            }

        GotNavigationBranch wsId sessionEpoch generation branchKey fingerprint projectOffset taskOffset result ->
            let
                currentDataLoading =
                    model.dataLoading

                expected =
                    Dict.get branchKey model.dataLoading.loadedNavigationBranches

                valid =
                    case expected of
                        Just request ->
                            model.selectedWorkspaceId == Just wsId
                                && model.sessionRequestEpoch == sessionEpoch
                                && request.workspaceId == wsId
                                && request.sessionEpoch == sessionEpoch
                                && request.generation == generation
                                && request.filterFingerprint == fingerprint
                                && request.projectOffset == projectOffset
                                && request.taskOffset == taskOffset
                                && request.inFlight

                        Nothing ->
                            False
            in
            if not valid then
                ( model, Cmd.none )

            else
                case result of
                    Ok navigation ->
                        if navigation.workspaceId /= wsId then
                            ( model, Cmd.none )

                        else
                            (if projectOffset == 0 && taskOffset == 0 then
                                    replaceNavigationBranchMembershipForKey branchKey navigation model

                               else
                                    mergeNavigationSummaries navigation.projects.items navigation.tasks.items model
                              )
                                |> (\next ->
                                    let
                                        mergedLoading =
                                            next.dataLoading
                                    in
                                    { next
                                        | dataLoading =
                                            { mergedLoading
                                                | loadedNavigationBranches =
                                                    Dict.update branchKey
                                                        (Maybe.map
                                                            (\request ->
                                                                { request
                                                                    | inFlight = False
                                                                    , succeeded = True
                                                                    , projectHasMore = navigation.projects.hasMore
                                                                    , taskHasMore = navigation.tasks.hasMore
                                                                    , projectCardCount =
                                                                        if projectOffset == 0 && taskOffset == 0 then
                                                                            List.length navigation.projects.items

                                                                        else
                                                                            if request.projectRequestPending then
                                                                                request.projectCardCount + List.length navigation.projects.items

                                                                            else
                                                                                request.projectCardCount
                                                                    , taskCardCount =
                                                                        if projectOffset == 0 && taskOffset == 0 then
                                                                            List.length navigation.tasks.items

                                                                        else
                                                                            if request.taskRequestPending then
                                                                                request.taskCardCount + List.length navigation.tasks.items

                                                                            else
                                                                                request.taskCardCount
                                                                    , projectRequestPending = False
                                                                    , taskRequestPending = False
                                                                }
                                                            )
                                                        )
                                                        mergedLoading.loadedNavigationBranches
                                            }
                                    }
                                   )
                                |> ensureNavigationPresentation
                                    (String.split ":" branchKey |> List.head |> Maybe.withDefault "")
                                    (String.split ":" branchKey |> List.drop 1 |> List.head)

                    Err _ ->
                        addToast Error "Failed to load workspace branch"
                            { model
                                | dataLoading =
                                    { currentDataLoading
                                        | loadedNavigationBranches =
                                            Dict.update branchKey (Maybe.map (\request -> { request | inFlight = False, succeeded = False })) currentDataLoading.loadedNavigationBranches
                                    }
                            }

        GotNavigationFocus wsId sessionEpoch generation fingerprint entityType entityId ancestorOffset result ->
            let
                valid =
                    case model.dataLoading.activeNavigationFocus of
                        Just request ->
                            model.selectedWorkspaceId == Just wsId
                                && model.sessionRequestEpoch == sessionEpoch
                                && request.workspaceId == wsId
                                && request.sessionEpoch == sessionEpoch
                                && request.generation == generation
                                && request.filterFingerprint == fingerprint
                                 && request.entityType == entityType
                                 && request.entityId == entityId
                                 && request.ancestorOffset == ancestorOffset

                        Nothing ->
                            False

                mergeSummary summary currentModel =
                    case summary of
                        Api.NavigationProjectSummary project ->
                            mergeNavigationSummaries [ project ] [] currentModel

                        Api.NavigationTaskSummary task ->
                            mergeNavigationSummaries [] [ task ] currentModel
            in
            if not valid then
                ( model, Cmd.none )

            else
                case result of
                    Ok focus ->
                        if focus.workspaceId /= wsId then
                            ( model, Cmd.none )

                        else if focus.ancestorsTruncated /= (focus.nextAncestorOffset /= Nothing) then
                            ( model, Cmd.none )

                        else
                            let
                                merged =
                                    List.foldl mergeSummary (mergeSummary focus.target model) focus.ancestors

                                mergedLoading =
                                    merged.dataLoading

                                requestKey =
                                    entityType ++ ":" ++ entityId

                                completeFocus =
                                    { merged
                                        | dataLoading =
                                            { mergedLoading
                                                | activeNavigationFocus = Nothing
                                                , navigationFocuses = Dict.update requestKey (Maybe.map (\request -> { request | inFlight = False, succeeded = True })) mergedLoading.navigationFocuses
                                            }
                                    }
                            in
                            case focus.nextAncestorOffset of
                                Just nextAncestorOffset ->
                                    if nextAncestorOffset <= ancestorOffset then
                                        ( model, Cmd.none )

                                    else
                                        let
                                            continuation =
                                                { workspaceId = wsId
                                                , sessionEpoch = sessionEpoch
                                                , generation = generation
                                                , filterFingerprint = fingerprint
                                                , entityType = entityType
                                                , entityId = entityId
                                                , ancestorOffset = nextAncestorOffset
                                                , inFlight = True
                                                , succeeded = False
                                                }

                                            continuedLoading =
                                                { mergedLoading
                                                    | activeNavigationFocus = Just continuation
                                                    , navigationFocuses = Dict.insert requestKey continuation mergedLoading.navigationFocuses
                                                }
                                        in
                                        ( { merged | dataLoading = continuedLoading }
                                        , Api.fetchNavigationFocus model.flags.apiUrl wsId entityType entityId nextAncestorOffset
                                            (GotNavigationFocus wsId sessionEpoch generation fingerprint entityType entityId nextAncestorOffset)
                                        )

                                Nothing ->
                                    let
                                        projectSummaries =
                                            focus.target :: focus.ancestors
                                                |> List.filterMap
                                                    (\summary ->
                                                        case summary of
                                                            Api.NavigationProjectSummary project ->
                                                                Just project

                                                            _ ->
                                                                Nothing
                                                    )

                                        taskSummaries =
                                            focus.target :: focus.ancestors
                                                |> List.filterMap
                                                    (\summary ->
                                                        case summary of
                                                            Api.NavigationTaskSummary task ->
                                                                Just task

                                                            _ ->
                                                                Nothing
                                                    )
                                    in
                                    ensureSummaries projectSummaries taskSummaries completeFocus

                    Err _ ->
                        let
                            currentDataLoading =
                                model.dataLoading
                        in
                        ( { model
                            | dataLoading =
                                { currentDataLoading
                                    | activeNavigationFocus = Nothing
                                    , navigationFocuses = Dict.update (entityType ++ ":" ++ entityId) (Maybe.map (\request -> { request | inFlight = False, succeeded = False })) currentDataLoading.navigationFocuses
                                }
                          }
                        , Cmd.none
                        )

        RetryCardDetail entityType entityId ->
            case entityType of
                "project" ->
                    case Dict.get entityId model.dataLoading.projectCardSummaries of
                        Just summary ->
                            let
                                ( updated, commands ) =
                                    beginProjectCardDetail summary ( model, [] )
                            in
                            ( updated, Cmd.batch commands )

                        Nothing ->
                            ( model, Cmd.none )

                "task" ->
                    case Dict.get entityId model.dataLoading.taskCardSummaries of
                        Just summary ->
                            let
                                ( updated, commands ) =
                                    beginTaskCardDetail summary ( model, [] )
                            in
                            ( updated, Cmd.batch commands )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotProjectCardDetail request projectId result ->
            let
                currentRequest =
                    Dict.get projectId model.dataLoading.projectCardDetailRequests

                currentSummary =
                    Dict.get projectId model.dataLoading.projectCardSummaries

                currentProject =
                    Dict.get projectId model.projects

                valid =
                    currentRequest == Just request
                        && request.inFlight
                        && model.selectedWorkspaceId == Just request.workspaceId
                        && model.sessionRequestEpoch == request.sessionEpoch
                        && (currentSummary |> Maybe.map .updatedAt) == Just request.expectedUpdatedAt
                        && (currentProject |> Maybe.map .updatedAt) == Just request.expectedUpdatedAt

                loading =
                    model.dataLoading
            in
            if not valid then
                ( model, Cmd.none )

            else
                case result of
                    Ok project ->
                        if project.id == projectId && project.workspaceId == request.workspaceId then
                            ( { model
                                | projects = Dict.insert projectId project model.projects
                                , dataLoading =
                                    { loading
                                        | projectCardDetailRequests =
                                            Dict.insert projectId { request | expectedUpdatedAt = project.updatedAt, inFlight = False, succeeded = True } loading.projectCardDetailRequests
                                    }
                              }
                            , Cmd.none
                            )

                        else
                            ( { model | dataLoading = { loading | projectCardDetailRequests = Dict.insert projectId { request | inFlight = False, succeeded = False } loading.projectCardDetailRequests } }, Cmd.none )

                    Err _ ->
                        ( { model | dataLoading = { loading | projectCardDetailRequests = Dict.insert projectId { request | inFlight = False, succeeded = False } loading.projectCardDetailRequests } }, Cmd.none )

        GotTaskCardDetail request taskId result ->
            let
                currentRequest =
                    Dict.get taskId model.dataLoading.taskCardDetailRequests

                currentSummary =
                    Dict.get taskId model.dataLoading.taskCardSummaries

                currentTask =
                    Dict.get taskId model.tasks

                valid =
                    currentRequest == Just request
                        && request.inFlight
                        && model.selectedWorkspaceId == Just request.workspaceId
                        && model.sessionRequestEpoch == request.sessionEpoch
                        && (currentSummary |> Maybe.map .updatedAt) == Just request.expectedUpdatedAt
                        && (currentTask |> Maybe.map .updatedAt) == Just request.expectedUpdatedAt

                loading =
                    model.dataLoading
            in
            if not valid then
                ( model, Cmd.none )

            else
                case result of
                    Ok task ->
                        if task.id == taskId && task.workspaceId == request.workspaceId then
                            ( { model
                                | tasks = Dict.insert taskId task model.tasks
                                , dataLoading =
                                    { loading
                                        | taskCardDetailRequests =
                                            Dict.insert taskId { request | expectedUpdatedAt = task.updatedAt, inFlight = False, succeeded = True } loading.taskCardDetailRequests
                                    }
                              }
                            , Cmd.none
                            )

                        else
                            ( { model | dataLoading = { loading | taskCardDetailRequests = Dict.insert taskId { request | inFlight = False, succeeded = False } loading.taskCardDetailRequests } }, Cmd.none )

                    Err _ ->
                        ( { model | dataLoading = { loading | taskCardDetailRequests = Dict.insert taskId { request | inFlight = False, succeeded = False } loading.taskCardDetailRequests } }, Cmd.none )

        GotProjects wsId maybeToken offset result ->
            if model.selectedWorkspaceId /= Just wsId then
                ( model, Cmd.none )

            else if not (acceptWorkspaceLoad maybeToken model.dataLoading) then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        let
                            updatedProjects =
                                mergePageById offset paginated.items model.projects

                            modelWithPage =
                                { model | projects = updatedProjects }
                        in
                        -- Workspace entry is deliberately a single capped page.
                        -- Branches and full card detail are loaded on demand;
                        -- never recursively walk the entire workspace or fan
                        -- out one overview request per entity during bootstrap.
                        ( { modelWithPage | dataLoading = finishWorkspaceLoad maybeToken model.dataLoading }, Cmd.none )

                    Err _ ->
                        let
                            currentDataLoading =
                                model.dataLoading

                            updatedDataLoading =
                                finishWorkspaceLoad maybeToken currentDataLoading
                        in
                        addToast Error
                            "Failed to load projects"
                            { model | dataLoading = updatedDataLoading }

        GotTasks wsId maybeToken offset result ->
            if model.selectedWorkspaceId /= Just wsId then
                ( model, Cmd.none )

            else if not (acceptWorkspaceLoad maybeToken model.dataLoading) then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        let
                            updatedTasks =
                                mergePageById offset paginated.items model.tasks

                            modelWithPage =
                                { model | tasks = updatedTasks }
                        in
                        -- See project page above: keep initial task data bounded
                        -- and leave dependency/readiness detail to explicit use.
                        ( { modelWithPage | dataLoading = finishWorkspaceLoad maybeToken model.dataLoading }, Cmd.none )

                    Err _ ->
                        let
                            updatedDataLoading =
                                finishWorkspaceLoad maybeToken model.dataLoading
                        in
                        addToast Error "Failed to load tasks" { model | dataLoading = updatedDataLoading }

        GotInitialTaskOverview wsId token taskId result ->
            if model.selectedWorkspaceId /= Just wsId || not (acceptWorkspaceLoad (Just token) model.dataLoading) || not (Permissions.canReadCurrentWorkspace model) || not (Dict.member taskId model.tasks) then
                ( model, Cmd.none )

            else
                let
                    completedLoading =
                        finishWorkspaceLoad (Just token) model.dataLoading

                    dependencies =
                        model.dependencies

                    updatedDependencies =
                        case result of
                            Ok overview ->
                                { dependencies
                                    | taskDependencies = Dict.insert taskId overview.dependencies dependencies.taskDependencies
                                    , taskDependencyHasMore = Dict.insert taskId False dependencies.taskDependencyHasMore
                                    , taskDependencyNextOffset = Dict.insert taskId (List.length overview.dependencies) dependencies.taskDependencyNextOffset
                                    , taskDependencyLoading = Dict.insert taskId False dependencies.taskDependencyLoading
                                    , taskDependencyRequests = Dict.remove taskId dependencies.taskDependencyRequests
                                    , taskDependencyRefreshItems = Dict.remove taskId dependencies.taskDependencyRefreshItems
                                    , taskReadinessRollups = Dict.insert taskId overview.readinessRollup dependencies.taskReadinessRollups
                                    , taskDependencyLinks = mergeTaskDependencyLinks taskId overview.dependencies dependencies.taskDependencyLinks
                                }

                            Err _ ->
                                dependencies
                in
                ( { model | dependencies = updatedDependencies, dataLoading = completedLoading }, Cmd.none )

        GotInitialProjectOverview wsId token projectId result ->
            if model.selectedWorkspaceId /= Just wsId || not (acceptWorkspaceLoad (Just token) model.dataLoading) || not (Permissions.canReadCurrentWorkspace model) || not (Dict.member projectId model.projects) then
                ( model, Cmd.none )

            else
                let
                    completedLoading =
                        finishWorkspaceLoad (Just token) model.dataLoading

                    dependencies =
                        model.dependencies

                    updatedDependencies =
                        case result of
                            Ok overview ->
                                { dependencies | projectReadinessRollups = Dict.insert projectId overview.readinessRollup dependencies.projectReadinessRollups }

                            Err _ ->
                                dependencies
                in
                ( { model | dependencies = updatedDependencies, dataLoading = completedLoading }, Cmd.none )

        GotObservations wsId maybeToken generation fingerprint offset result ->
            if model.selectedWorkspaceId /= Just wsId || model.observations.requestSessionEpoch /= model.sessionRequestEpoch || not (acceptWorkspaceLoad maybeToken model.dataLoading) || not (listObservationResponseMatches generation fingerprint offset model.observations) then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        let
                            currentObservations =
                                model.observations

                            observations =
                                mergeObservationPage offset paginated currentObservations

                            updatedDataLoading =
                                finishWorkspaceLoad maybeToken model.dataLoading
                        in
                        ( { model | observations = observations, dataLoading = updatedDataLoading }, Cmd.none )

                    Err _ ->
                        let
                            currentObservations =
                                model.observations

                            observations =
                                { currentObservations | loading = False, error = Just "Failed to load observations.", expectedOffset = Nothing }

                            updatedDataLoading =
                                finishWorkspaceLoad maybeToken model.dataLoading
                        in
                        ( { model | observations = observations, dataLoading = updatedDataLoading }, Cmd.none )

        _ ->
            ( model, Cmd.none )


observationResponseMatches : Int -> String -> Int -> ObservationModel -> Bool
observationResponseMatches generation fingerprint offset observations =
    observations.requestGeneration
        == generation
        && observations.queryFingerprint
        == fingerprint
        && observations.expectedOffset
        == Just offset


listObservationResponseMatches : Int -> String -> Int -> ObservationModel -> Bool
listObservationResponseMatches generation fingerprint offset observations =
    (observations.requestMode == ObservationFlatMode || observations.requestMode == ObservationExactSubjectMode)
        && observationResponseMatches generation fingerprint offset observations


mergeObservationPage : Int -> Api.PaginatedResult Api.Observation -> ObservationModel -> ObservationModel
mergeObservationPage offset paginated observations =
    let
        receivedIds =
            List.map .id paginated.items

        orderedIds =
            if offset == 0 then
                receivedIds

            else
                observations.orderedIds ++ List.filter (\observationId -> not (List.member observationId observations.orderedIds)) receivedIds

        pageItems =
            List.foldl
                (\observation accumulatedItems ->
                    Dict.insert observation.id
                        (Dict.get observation.id observations.items
                            |> Maybe.map (Feature.Observation.preferNewerObservation observation)
                            |> Maybe.withDefault observation
                        )
                        accumulatedItems
                )
                Dict.empty
                paginated.items

        items =
            if offset == 0 then
                pageItems

            else
                Dict.union pageItems observations.items
    in
    { observations
        | items = items
        , orderedIds = orderedIds
        , hasMore = paginated.hasMore
        , loading = False
        , error = Nothing
        , expectedOffset = Nothing
        , nextOffset = offset + List.length paginated.items
        , resultsStale = if offset == 0 then False else observations.resultsStale
    }
        |> (\merged -> List.foldl Feature.Observation.applyAuthoritativeObservation merged paginated.items)
