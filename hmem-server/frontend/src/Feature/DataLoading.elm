module Feature.DataLoading exposing (mergeNavigationSummariesAt, markMissingNavigationSummaries, ensurePresentedCardDetails, ensureExpandedBranches, pumpNavigation, emptyPlanningCounts, retirePlanningCounts, invalidatePlanningCounts, syncPlanningCounts, syncPlanningCountsResult, startupBackgroundReady, acceptBackgroundPaint, backgroundPaintNonce, navigationFilterFingerprint, revalidateNavigationForChangedSummaries, revalidateNavigationForTaskReadiness, revalidateNavigationForTasksReadiness, acceptWorkspaceLoad, beginNavigationBranch, beginNavigationBranchPage, beginNavigationBranchPreviousPage, beginRootNavigation, beginRootNavigationPage, beginRootNavigationPreviousPage, beginNavigationFocus, revalidateNavigationFocus, invalidateNavigationRequests, ensureAllNavigationPresentations, ensureNavigationPresentation, ensureVisibleCardDetails, ensureViewportCardDetails, finishWorkspaceLoad, retireInitialObservationLoad, init, listObservationResponseMatches, mergeNavigationSummaries, mergeObservationPage, nextPageOffset, observationResponseMatches, prepareForPageLoad, prepareRootNavigationRequest, rootNavigationContextMatches, resetNavigationPresentations, revalidateNavigationForFilters, revalidateNavigationForAffectedBranches, reloadNavigationForFilters, update)

import Api
import Dict
import Feature.Observation
import Feature.ChangeStream
import Feature.FocusedFilters
import Helpers exposing (indexBy, navigationPresentationWindow, presentationOrdinaryCapacity, taskPresentationOrder)
import Http
import Json.Encode as Encode
import Permissions
import Set
import String
import StartupAdmission
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
    , initialObservationLoad = Nothing
    , nextWorkspaceLoadToken = 1
    , cardHydrationLoaded = False
    , planningCounts = emptyPlanningCounts
    , startupAdmission = StartupAdmission.init
    , navigationGeneration = 0
    , rootNavigationRequest = Nothing
    , loadedNavigationBranches = Dict.empty
    , navigationQueue = []
    , navigationAdmissions = Dict.empty
    , navigationPasses = Dict.empty
    , backgroundAdmission = { workspaceId = Nothing, sessionEpoch = -1, filterFingerprint = "", rootGeneration = Nothing, nonce = 1, acknowledged = False, remaining = 0, branches = 0, details = 0, branchRequests = Set.empty, detailRequests = Set.empty }
    , cardDetailAdmissions = Set.empty
    , cardDetailRetries = Set.empty
    , visibleDetailDemand = Nothing
                        , viewportDetailPins = Set.empty
    , rootNavigationPresentation = Nothing
    , navigationPresentations = Dict.empty
    , projectCardSummaries = Dict.empty
    , taskCardSummaries = Dict.empty
    , navigationSummaryAuthority = Dict.empty
    , nextNavigationSummaryRevision = 0
    , planningReadRequests = Dict.empty
    , projectCardDetailRequests = Dict.empty
    , taskCardDetailRequests = Dict.empty
    , nextCardDetailRequestId = 1
    , navigationVisibleProjectIds = Set.empty
    , navigationVisibleTaskIds = Set.empty
    , navigationVisibilityActive = False
    , activeNavigationFocus = Nothing
    , navigationFocuses = Dict.empty
    }


emptyPlanningCounts : PlanningCountState
emptyPlanningCounts =
    { owner = Nothing, active = Nothing, nextToken = 1, generation = 0, pending = False, current = False, value = Nothing, error = Nothing }


{-| Retire logical ownership without freeing a physical HTTP request. Its exact
callback releases the slot before a new owner can request counts.
-}
retirePlanningCounts : PlanningCountState -> PlanningCountState
retirePlanningCounts previous =
    { emptyPlanningCounts | active = previous.active, nextToken = previous.nextToken, generation = previous.generation + 1 }


invalidatePlanningCounts : Model -> Model
invalidatePlanningCounts model =
    let
        loading = model.dataLoading
        counts = loading.planningCounts
    in
    { model | dataLoading = { loading | planningCounts = { counts | generation = counts.generation + 1, pending = True, current = False, error = Nothing } } }


planningCountOwner : Model -> Maybe PlanningCountGuard
planningCountOwner model =
    case ( model.page, model.selectedWorkspaceId, model.sessionContext ) of
        ( WorkspacePage pageWorkspace, Just workspaceId, Just session ) ->
            if pageWorkspace == workspaceId && model.auth.status == AuthReady && Permissions.canReadCurrentWorkspace model && model.workspaceAdmin.authorizationPending == Nothing then
                Just
                    { workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch, token = 0, generation = 0
                    , audience = Encode.encode 0 (Encode.list Encode.string [ session.authMode, session.principal.actorType, session.principal.actorId, session.principal.authority, Maybe.withDefault "" session.principal.grantUserId, Permissions.currentWorkspaceRoleLabel model ])
                    }
            else Nothing
        _ -> Nothing


syncPlanningCounts : Model -> ( Model, Cmd Msg )
syncPlanningCounts source =
    let
        model = reconcileStartup source
        loading = model.dataLoading
        counts = loading.planningCounts
        ownedStartup = loading.startupAdmission
    in
    case planningCountOwner model of
        Nothing ->
            ( { model | dataLoading = { loading | planningCounts = if counts.owner == Nothing && counts.value == Nothing && not counts.pending then counts else retirePlanningCounts counts } }, Cmd.none )

        Just owner ->
            let
                owned = if counts.owner /= Just owner then
                    let retired = retirePlanningCounts counts in { retired | owner = Just owner, pending = True }
                    else counts
            in
            if owned.active == Nothing && owned.pending && owned.error == Nothing then
                let
                    guard = { owner | token = owned.nextToken, generation = owned.generation }
                    started = { owned | active = Just guard, nextToken = owned.nextToken + 1, pending = False }
                in
                case StartupAdmission.admit (planningPhysicalKey guard) ownedStartup of
                    Just admission ->
                        ( { model | dataLoading = { loading | planningCounts = started, startupAdmission = admission } }
                        , Api.fetchWorkspacePlanningCounts model.flags.apiUrl owner.workspaceId (GotWorkspacePlanningCounts guard)
                        )
                    Nothing ->
                        ( { model | dataLoading = { loading | planningCounts = owned } }, Cmd.none )
            else
                ( { model | dataLoading = { loading | planningCounts = owned } }, Cmd.none )


syncPlanningCountsResult : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
syncPlanningCountsResult ( model, command ) =
    let
        ( counted, countCommand ) = syncPlanningCounts model
        ( observed, observationCommand ) = Feature.Observation.syncCounts counted
    in
    ( reconcileStartup observed, Cmd.batch [ command, countCommand, observationCommand ] )


acceptPlanningCounts : PlanningCountGuard -> Result error Api.WorkspacePlanningCounts -> Model -> ( Model, Cmd Msg )
acceptPlanningCounts guard result source =
    let
        sourceLoading = source.dataLoading
        model = { source | dataLoading = { sourceLoading | startupAdmission = StartupAdmission.complete (planningPhysicalKey guard) sourceLoading.startupAdmission } }
        loading = model.dataLoading
        counts = loading.planningCounts
    in
    if counts.active /= Just guard then ( model, Cmd.none )
    else
        let
            owner = { guard | token = 0, generation = 0 }
            owned = planningCountOwner model == Just owner && counts.owner == Just owner
            current = owned && guard.generation == counts.generation && not counts.pending
            completed = { counts | active = Nothing }
            settled = if not current then { completed | pending = counts.pending || owned, error = Nothing }
                else case result of
                    Ok value -> { completed | value = Just value, current = True, error = Nothing }
                    Err _ -> { completed | current = False, error = Just "Planning totals unavailable" }
        in
        syncPlanningCounts { model | dataLoading = { loading | planningCounts = settled } }


planningPhysicalKey : PlanningCountGuard -> String
planningPhysicalKey guard =
    "planning|" ++ Encode.encode 0 (Encode.list Encode.string [ guard.workspaceId, String.fromInt guard.sessionEpoch, guard.audience, String.fromInt guard.token, String.fromInt guard.generation ])


rootPhysicalKey : String -> Int -> Int -> String -> Int -> Int -> String
rootPhysicalKey workspace epoch generation fingerprint projectOffset taskOffset =
    "root|" ++ Encode.encode 0 (Encode.list Encode.string [ workspace, String.fromInt epoch, String.fromInt generation, fingerprint, String.fromInt projectOffset, String.fromInt taskOffset ])


recordRootRecovery : NavigationBranchState -> Model -> Model
recordRootRecovery request source =
    let
        prepared = reconcileStartup source
        model = if request.readRevision == 0 then stampNavigationRead (navigationBranchKey "workspace_root" Nothing) request prepared else prepared
        loading = model.dataLoading
        key = rootPhysicalKey request.workspaceId request.sessionEpoch request.generation request.filterFingerprint request.projectOffset request.taskOffset
    in
    { model | dataLoading = { loading | startupAdmission = StartupAdmission.recordRecovery key loading.startupAdmission } }


stampNavigationRead : String -> NavigationBranchState -> Model -> Model
stampNavigationRead key request model =
    let
        ( revision, issued ) = Helpers.allocatePlanningRead model
        loading = issued.dataLoading
        stamped = { request | readRevision = revision }
    in
    { issued | dataLoading = { loading
        | rootNavigationRequest = if key == navigationBranchKey "workspace_root" Nothing then Just stamped else loading.rootNavigationRequest
        , loadedNavigationBranches = if key == navigationBranchKey "workspace_root" Nothing && not (Dict.member key loading.loadedNavigationBranches) then loading.loadedNavigationBranches else Dict.insert key stamped loading.loadedNavigationBranches
        } }


startupFingerprint : Model -> String
startupFingerprint model =
    let
        root = model.dataLoading.rootNavigationRequest
        counts = model.dataLoading.planningCounts
        observation = model.observations.counts
        stream = model.selectedWorkspaceId |> Maybe.andThen (\id -> Dict.get ("workspace:" ++ id) model.webSocket.streams)
    in
    Encode.encode 0 (Encode.list Encode.string
        [ navigationFilterFingerprint model
        , root |> Maybe.map (\request -> String.fromInt request.generation ++ ":" ++ String.fromInt request.projectOffset ++ ":" ++ String.fromInt request.taskOffset ++ ":" ++ (if request.inFlight then "busy" else "settled")) |> Maybe.withDefault "none"
        , String.fromInt counts.generation ++ ":" ++ (if counts.pending then "pending" else "settled")
        , if model.selectedWorkspaceId |> Maybe.andThen (\id -> Dict.get id model.workspaces) |> Maybe.map (\workspace -> workspace.workspaceType == Api.Repository) |> Maybe.withDefault True then
            String.fromInt observation.generation ++ ":" ++ observation.settledFingerprint ++ ":" ++ (if observation.pending then "pending" else "settled")
          else "no-observations"
        , stream |> Maybe.map (\state -> Maybe.withDefault "" state.resumeToken ++ ":" ++ (if state.live then "live" else Maybe.withDefault "replay" state.failure)) |> Maybe.withDefault "none"
        ])


startupSettled : Model -> Bool
startupSettled model =
    let
        loading = model.dataLoading
        state = loading.startupAdmission
        counts = loading.planningCounts
        observation = model.observations.counts
        parked = StartupAdmission.remaining state == 0
        needsObservations = model.selectedWorkspaceId |> Maybe.andThen (\id -> Dict.get id model.workspaces) |> Maybe.map (\workspace -> workspace.workspaceType == Api.Repository) |> Maybe.withDefault True
        headers = counts.active == Nothing && observation.active == Nothing
            && (counts.current || counts.error /= Nothing || (parked && counts.pending))
            && (not needsObservations || observation.current || observation.error /= Nothing || (parked && observation.pending))
        root = loading.rootNavigationRequest |> Maybe.map (\request -> Just request.workspaceId == model.selectedWorkspaceId && request.sessionEpoch == model.sessionRequestEpoch && not request.inFlight) |> Maybe.withDefault (model.focus.focusedEntity /= Nothing)
        canonical = model.selectedWorkspaceId |> Maybe.andThen (\id -> Dict.get ("workspace:" ++ id) model.webSocket.streams)
            |> Maybe.map (\stream -> Just stream.scope == Maybe.map Feature.ChangeStream.Workspace model.selectedWorkspaceId && (stream.live || stream.failure /= Nothing)) |> Maybe.withDefault False
    in
    state.authorized && canonical && root && headers && Set.isEmpty state.physical


reconcileStartup : Model -> Model
reconcileStartup model =
    let
        loading = model.dataLoading
        owner = planningCountOwner model |> Maybe.map (\guard -> { workspaceId = guard.workspaceId, sessionEpoch = guard.sessionEpoch, audience = guard.audience })
        owned = StartupAdmission.reconcile owner loading.startupAdmission
        prepared = { model | dataLoading = { loading | startupAdmission = owned } }
        settled = startupSettled prepared
        fingerprint = startupFingerprint prepared
        hasHierarchy = model.focus.focusedEntity /= Nothing || (loading.rootNavigationRequest |> Maybe.map (\request -> request.projectCardCount > 0 || request.taskCardCount > 0) |> Maybe.withDefault False)
        next =
            if settled && (model.activeTab /= ProjectsTab || not hasHierarchy) then
                StartupAdmission.headerOnly owned
            else if model.activeTab == ProjectsTab && owned.phase == StartupAdmission.HeaderOnly && settled then
                { owned | phase = StartupAdmission.AwaitNormalPaint fingerprint }
            else
                StartupAdmission.arm fingerprint settled owned
        changed = next.phase /= owned.phase
        permit = loading.backgroundAdmission
    in
    { prepared | dataLoading = { loading | startupAdmission = next
        , backgroundAdmission = if changed then { permit | nonce = permit.nonce + 1, acknowledged = False, remaining = 0 } else permit } }


startupBackgroundReady : Model -> Bool
startupBackgroundReady model =
    case model.dataLoading.startupAdmission.phase of
        StartupAdmission.Normal -> True
        StartupAdmission.HeaderOnly -> False
        _ -> startupSettled model


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
    String.join "|" [ showOnly, if model.search.filterShowEmptyProjects then "empty" else "nonempty", priority, String.join "," (List.sort model.search.filterProjectStatuses), String.join "," (List.sort model.search.filterTaskStatuses), String.trim model.search.query ]


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
    showOnly ++ "&show_empty_projects=" ++ (if model.search.filterShowEmptyProjects then "true" else "false") ++ priority ++ projectStatuses ++ taskStatuses ++ query


navigationBranchKey : String -> Maybe String -> String
navigationBranchKey parentKind maybeParentId =
    parentKind ++ ":" ++ Maybe.withDefault "root" maybeParentId


beginNavigationBranch : String -> String -> Maybe String -> Model -> ( Model, Cmd Msg )
beginNavigationBranch parentKind workspaceId maybeParentId model =
    pumpNavigation (stageNavigationBranch parentKind workspaceId maybeParentId model)


stageNavigationBranch : String -> String -> Maybe String -> Model -> Model
stageNavigationBranch parentKind workspaceId maybeParentId model =
    let
        key = navigationBranchKey parentKind maybeParentId
        loading = model.dataLoading
        generation = loading.navigationGeneration + 1
        fingerprint = navigationFilterFingerprint model
        previous = Dict.get key loading.loadedNavigationBranches
        progress = Dict.get key loading.navigationPasses |> Maybe.withDefault (emptyNavigationPass False)
        matching = previous |> Maybe.map (\state -> state.workspaceId == workspaceId && state.sessionEpoch == model.sessionRequestEpoch && state.filterFingerprint == fingerprint) |> Maybe.withDefault False
        incomplete = previous |> Maybe.map (\state -> state.projectRequestPending || state.taskRequestPending || progress.projectError /= Nothing || progress.taskError /= Nothing) |> Maybe.withDefault False
        resume = matching && incomplete
        request =
            case previous of
                Just current ->
                    if resume then
                        { current | generation = generation, inFlight = False, succeeded = False
                            , projectRequestPending = current.projectRequestPending || progress.projectError /= Nothing
                            , taskRequestPending = current.taskRequestPending || progress.taskError /= Nothing
                        }
                    else
                        { initialNavigationBranchState | workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch, generation = generation, filterFingerprint = fingerprint
                            , projectCardCount = if matching then current.projectCardCount else 0
                            , taskCardCount = if matching then current.taskCardCount else 0
                            , projectRequestPending = True, taskRequestPending = True
                        }
                Nothing ->
                    { initialNavigationBranchState | workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch, generation = generation, filterFingerprint = fingerprint, projectRequestPending = True, taskRequestPending = True }
        pass =
            if resume then
                { progress | projectError = Nothing, taskError = Nothing }
            else
                emptyNavigationPass matching
        presentation =
            if resume then
                { workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch, generation = generation, filterFingerprint = fingerprint
                    , projectOffset = Dict.get key loading.navigationPresentations |> Maybe.map .projectOffset |> Maybe.withDefault 0
                    , taskOffset = Dict.get key loading.navigationPresentations |> Maybe.map .taskOffset |> Maybe.withDefault 0
                }
            else initialPresentation request
        updated =
            { model | dataLoading = { loading | navigationGeneration = generation
                , loadedNavigationBranches = Dict.insert key request loading.loadedNavigationBranches
                , navigationPresentations = Dict.insert key presentation loading.navigationPresentations
                , navigationPasses = Dict.insert key pass loading.navigationPasses
            } }
    in
    enqueueNavigation key updated


{-| Admissions outlive logical requests. Retiring a generation cannot free an
HTTP slot: only its completion (including stale/error replies) does that.
Root paging is manual and has its own single request; expanded branches share
four slots and details share six slots.
-}
navigationConcurrency : Int
navigationConcurrency =
    4


emptyNavigationPass : Bool -> NavigationPass
emptyNavigationPass refreshing =
    { refreshing = refreshing, rootDemand = Nothing, projects = Dict.empty, tasks = Dict.empty, projectError = Nothing, taskError = Nothing }


navigationAdmissionKey : String -> NavigationBranchState -> String
navigationAdmissionKey key request =
    String.join "|" [ key, request.workspaceId, String.fromInt request.sessionEpoch, String.fromInt request.generation, String.fromInt request.projectOffset, String.fromInt request.taskOffset, request.filterFingerprint ]


enqueueNavigation : String -> Model -> Model
enqueueNavigation key model =
    let
        loading = model.dataLoading
    in
    if List.member key loading.navigationQueue then
        model
    else
        { model | dataLoading = { loading | navigationQueue = loading.navigationQueue ++ [ key ] } }


{-| Follow displayed ancestry up to the focused root, retaining canonical
parents for breadcrumbs and the ordinary tree. External ancestry cannot hide
the focused subtree, and unrelated cached branches cannot become demand.
-}
navigationBranchExpanded : String -> Model -> Bool
navigationBranchExpanded key model =
    let
        focused kind id = model.focus.focusedEntity == Just ( kind, id )
        ordinaryRoot = model.focus.focusedEntity == Nothing

        projectExpanded visited id =
            if Set.member ("project:" ++ id) visited || (Dict.get ("proj-" ++ id) model.cards.collapsedNodes |> Maybe.withDefault False) then
                False
            else
                case Dict.get id model.dataLoading.projectCardSummaries of
                    Nothing -> ordinaryRoot && not model.dataLoading.navigationVisibilityActive && Set.isEmpty visited
                    Just summary ->
                        Just summary.workspaceId == model.selectedWorkspaceId
                            && (focused "project" id || not model.dataLoading.navigationVisibilityActive || Set.member id model.dataLoading.navigationVisibleProjectIds)
                            && (focused "project" id || (summary.parentId |> Maybe.map (projectExpanded (Set.insert ("project:" ++ id) visited)) |> Maybe.withDefault ordinaryRoot))

        taskExpanded visited id =
            if Set.member ("task:" ++ id) visited || (Dict.get ("task-" ++ id) model.cards.collapsedNodes |> Maybe.withDefault False) then
                False
            else
                case Dict.get id model.dataLoading.taskCardSummaries of
                    Nothing -> ordinaryRoot && not model.dataLoading.navigationVisibilityActive && Set.isEmpty visited
                    Just summary ->
                        Just summary.workspaceId == model.selectedWorkspaceId
                            && (focused "task" id || not model.dataLoading.navigationVisibilityActive || Set.member id model.dataLoading.navigationVisibleTaskIds)
                            && (focused "task" id || (case summary.parentId of
                                    Just parent -> taskExpanded (Set.insert ("task:" ++ id) visited) parent
                                    Nothing -> summary.projectId |> Maybe.map (projectExpanded visited) |> Maybe.withDefault ordinaryRoot
                               ))
    in
    case String.split ":" key of
        [ "project", id ] -> projectExpanded Set.empty id
        [ "task", id ] -> taskExpanded Set.empty id
        _ -> False



prepareBackgroundAdmission : Model -> Model
prepareBackgroundAdmission source =
    let
        model = reconcileStartup source
        loading = model.dataLoading
        permit = loading.backgroundAdmission
        fingerprint = navigationFilterFingerprint model
        rootGeneration = loading.rootNavigationRequest |> Maybe.map .generation
    in
    if permit.rootGeneration == rootGeneration && permit.workspaceId == model.selectedWorkspaceId && permit.sessionEpoch == model.sessionRequestEpoch && permit.filterFingerprint == fingerprint then model
    else { model | dataLoading = { loading | backgroundAdmission = { workspaceId = model.selectedWorkspaceId, sessionEpoch = model.sessionRequestEpoch, filterFingerprint = fingerprint, rootGeneration = rootGeneration, nonce = permit.nonce + 1, acknowledged = False, remaining = 0, branches = 0, details = 0, branchRequests = Set.empty, detailRequests = Set.empty } } }


backgroundPaintNonce : Model -> Int
backgroundPaintNonce model =
    (prepareBackgroundAdmission model).dataLoading.backgroundAdmission.nonce


yieldBackgroundAdmission : Model -> Model
yieldBackgroundAdmission source =
    let
        model = prepareBackgroundAdmission source
        loading = model.dataLoading
        permit = loading.backgroundAdmission
    in
    if permit.acknowledged && permit.remaining < 5 && Set.isEmpty (Set.intersect permit.branchRequests (Set.fromList (Dict.keys loading.navigationAdmissions))) && Set.isEmpty (Set.intersect permit.detailRequests loading.cardDetailAdmissions) then
        { model | dataLoading = { loading | backgroundAdmission = { permit | nonce = permit.nonce + 1, acknowledged = False, remaining = 0, branches = 0, details = 0, branchRequests = Set.empty, detailRequests = Set.empty } } }
    else model


acceptBackgroundPaint : Int -> Model -> ( Model, Cmd Msg )
acceptBackgroundPaint nonce source =
    let
        model = prepareBackgroundAdmission source
        loading = model.dataLoading
        permit = loading.backgroundAdmission
    in
    if nonce /= permit.nonce || permit.acknowledged || model.activeTab /= ProjectsTab || model.auth.status /= AuthReady then ( model, Cmd.none )
    else
        case loading.startupAdmission.phase of
            StartupAdmission.ClosureArmed fingerprint ->
                let
                    next = StartupAdmission.acknowledgeClosure fingerprint loading.startupAdmission
                in
                ( { model | dataLoading = { loading | startupAdmission = next, backgroundAdmission = { permit | nonce = permit.nonce + 1, acknowledged = False, remaining = 0 } } }, Cmd.none )

            StartupAdmission.AwaitNormalPaint fingerprint ->
                openBackgroundWave (StartupAdmission.acknowledgeNormal fingerprint loading.startupAdmission) 5 model

            StartupAdmission.Initial ->
                if loading.startupAdmission.initialWaveOpened || not (startupSettled model) then
                    ( model, Cmd.none )
                else
                    openBackgroundWave (StartupAdmission.openInitialWave loading.startupAdmission) (min 5 (StartupAdmission.remaining loading.startupAdmission)) model

            StartupAdmission.Normal ->
                openBackgroundWave loading.startupAdmission 5 model

            StartupAdmission.HeaderOnly ->
                ( model, Cmd.none )


openBackgroundWave : StartupAdmission.State -> Int -> Model -> ( Model, Cmd Msg )
openBackgroundWave state credits model =
    let
        loading = model.dataLoading
        permit = loading.backgroundAdmission
        opened = { model | dataLoading = { loading | startupAdmission = state, backgroundAdmission = { permit | acknowledged = True, remaining = credits } } }
        ( counted, countCommand ) = if state.phase == StartupAdmission.Normal then syncPlanningCountsResult ( opened, Cmd.none ) else ( opened, Cmd.none )
        ( branched, branchCommand ) = pumpNavigation counted
        ( detailed, detailCommand ) = ensurePresentedCardDetails branched
    in
    ( detailed, Cmd.batch [ countCommand, branchCommand, detailCommand ] )


consumeBackground : Maybe String -> Model -> Maybe Model
consumeBackground branchKey model =
    let
        loading = model.dataLoading
        permit = loading.backgroundAdmission
        branch = branchKey /= Nothing
        key = branchKey |> Maybe.map ((++) "branch|") |> Maybe.withDefault ("detail|" ++ String.fromInt (loading.nextCardDetailRequestId - 1))
    in
    StartupAdmission.admitOrdinary key loading.startupAdmission
        |> Maybe.map (\startup -> { model | dataLoading = { loading | startupAdmission = startup, backgroundAdmission = { permit | branchRequests = Maybe.map (\requestKey -> Set.insert requestKey permit.branchRequests) branchKey |> Maybe.withDefault permit.branchRequests, detailRequests = if branch then permit.detailRequests else Set.insert (loading.nextCardDetailRequestId - 1) permit.detailRequests, remaining = permit.remaining - 1, branches = permit.branches + (if branch then 1 else 0), details = permit.details + (if branch then 0 else 1) } } })


ordinaryDetailsPending : Model -> Bool
ordinaryDetailsPending model =
    let
        loading = model.dataLoading
        ( projects, tasks ) = Maybe.withDefault ( loading.navigationVisibleProjectIds, loading.navigationVisibleTaskIds ) loading.visibleDetailDemand
        pending summaries requests cached id =
            case Dict.get id summaries of
                Nothing -> False
                Just summary ->
                    case Dict.get id requests of
                        Nothing -> True
                        Just request -> not (request.inFlight || request.expectedUpdatedAt == summary.updatedAt || (Dict.get id cached |> Maybe.map .updatedAt) == Just request.expectedUpdatedAt)
    in
    List.any (pending loading.projectCardSummaries loading.projectCardDetailRequests model.projects) (Set.toList projects |> List.take 25)
        || List.any (pending loading.taskCardSummaries loading.taskCardDetailRequests model.tasks) (Set.toList tasks |> List.take 25)

pumpNavigation : Model -> ( Model, Cmd Msg )
pumpNavigation model =
    let
        ( admitted, requests ) = pumpNavigationQueue (List.length model.dataLoading.navigationQueue) (prepareBackgroundAdmission model)
        command =
            case List.head requests of
                Nothing -> Cmd.none
                Just ( _, first ) -> Api.fetchNavigationBatch model.flags.apiUrl first.workspaceId (navigationBatchFilters model) (List.map navigationBatchItem requests) (GotNavigationBatch requests)
    in
    ( admitted, command )


navigationBatchItem : ( String, NavigationBranchState ) -> Api.NavigationBatchItem
navigationBatchItem ( key, request ) =
    { requestId = String.fromInt request.readRevision
    , parentKind = String.split ":" key |> List.head |> Maybe.withDefault ""
    , parentId = String.split ":" key |> List.drop 1 |> List.head |> Maybe.withDefault ""
    , projectOffset = request.projectOffset, taskOffset = request.taskOffset
    , includeProjects = request.projectRequestPending, includeTasks = request.taskRequestPending
    }


navigationBatchFilters : Model -> Encode.Value
navigationBatchFilters model =
    let
        showOnly = case model.search.filterShowOnly of
            ShowAll -> []
            ShowProjectsOnly -> [ ( "show_only", Encode.string "projects" ) ]
            ShowTasksOnly -> [ ( "show_only", Encode.string "tasks" ) ]
        priority = case model.search.filterPriority of
            AnyPriority -> [ ( "priority_mode", Encode.string "any" ) ]
            ExactPriority value -> [ ( "priority_mode", Encode.string "exact" ), ( "priority_value", Encode.int value ) ]
            AbovePriority value -> [ ( "priority_mode", Encode.string "above" ), ( "priority_value", Encode.int value ) ]
            BelowPriority value -> [ ( "priority_mode", Encode.string "below" ), ( "priority_value", Encode.int value ) ]
        query = if String.trim model.search.query == "" then [] else [ ( "query", Encode.string (String.trim model.search.query) ) ]
    in
    Encode.object (showOnly ++ priority ++ query ++
        [ ( "project_statuses", Encode.list Encode.string model.search.filterProjectStatuses )
        , ( "task_statuses", Encode.list Encode.string model.search.filterTaskStatuses )
        , ( "show_empty_projects", Encode.bool model.search.filterShowEmptyProjects ) ])


pumpNavigationQueue : Int -> Model -> ( Model, List ( String, NavigationBranchState ) )
pumpNavigationQueue budget model =
    let
        loading = model.dataLoading
    in
    if budget <= 0 || model.move.dialog /= Nothing || Dict.size loading.navigationAdmissions + Dict.size model.move.admissions >= navigationConcurrency || model.activeTab /= ProjectsTab || loading.backgroundAdmission.remaining <= 0 || not (StartupAdmission.canOrdinary loading.startupAdmission) || (loading.backgroundAdmission.branches >= 2 && ordinaryDetailsPending model) then
        ( model, [] )
    else
        case loading.navigationQueue of
            [] -> ( model, [] )
            key :: remaining ->
                let
                    popped = { model | dataLoading = { loading | navigationQueue = remaining } }
                    alreadyAdmitted =
                        Dict.keys loading.navigationAdmissions |> List.any (String.startsWith (key ++ "|"))
                in
                case ( String.split ":" key, Dict.get key loading.loadedNavigationBranches ) of
                    ( [ kind, id ], Just previous ) ->
                        if previous.inFlight || not (previous.projectRequestPending || previous.taskRequestPending) || not (navigationBranchExpanded key model) || model.selectedWorkspaceId /= Just previous.workspaceId || model.sessionRequestEpoch /= previous.sessionEpoch then
                            pumpNavigationQueue (budget - 1) popped
                        else if alreadyAdmitted then
                            pumpNavigationQueue (budget - 1) (enqueueNavigation key popped)
                        else
                            let
                                request = { previous | inFlight = True, readRevision = loading.nextNavigationSummaryRevision + 1 }
                                poppedLoading = popped.dataLoading
                                admitted = consumeBackground (Just (navigationAdmissionKey key request)) { popped | dataLoading = { poppedLoading | nextNavigationSummaryRevision = request.readRevision, loadedNavigationBranches = Dict.insert key request poppedLoading.loadedNavigationBranches, navigationAdmissions = Dict.insert (navigationAdmissionKey key request) request poppedLoading.navigationAdmissions } }
                            in
                            case admitted of
                                Nothing -> ( model, [] )
                                Just accepted ->
                                    let ( next, requests ) = pumpNavigationQueue (budget - 1) accepted in
                                    ( next, ( key, request ) :: requests )
                    _ -> pumpNavigationQueue (budget - 1) popped


releaseNavigationAdmission : Msg -> Model -> Model
releaseNavigationAdmission msg model =
    let
        loading = model.dataLoading
    in
    case msg of
        GotRootNavigation ws epoch _ generation fingerprint projectOffset taskOffset _ ->
            { model | dataLoading = { loading | startupAdmission = StartupAdmission.complete (rootPhysicalKey ws epoch generation fingerprint projectOffset taskOffset) loading.startupAdmission } }
        GotNavigationBranch ws epoch generation key fingerprint projectOffset taskOffset _ ->
            let identity = String.join "|" [ key, ws, String.fromInt epoch, String.fromInt generation, String.fromInt projectOffset, String.fromInt taskOffset, fingerprint ] in
            { model | dataLoading = { loading | startupAdmission = StartupAdmission.complete ("branch|" ++ identity) loading.startupAdmission, navigationAdmissions = Dict.remove identity loading.navigationAdmissions } }
        GotNavigationBatch requests _ ->
            List.foldl (\( key, request ) current ->
                let
                    currentLoading = current.dataLoading
                    identity = navigationAdmissionKey key request
                in
                if Dict.get identity currentLoading.navigationAdmissions /= Just request then current
                else { current | dataLoading = { currentLoading
                    | startupAdmission = StartupAdmission.complete ("branch|" ++ identity) currentLoading.startupAdmission
                    , navigationAdmissions = Dict.remove identity currentLoading.navigationAdmissions } }) model requests
        GotProjectCardDetail request id _ ->
            { model | dataLoading = { loading | startupAdmission = StartupAdmission.complete ("detail|" ++ String.fromInt request.requestId) loading.startupAdmission, cardDetailAdmissions = Set.remove request.requestId loading.cardDetailAdmissions, cardDetailRetries = if Dict.get id loading.projectCardDetailRequests == Just request then Set.remove ( "project", id ) loading.cardDetailRetries else loading.cardDetailRetries } }
        GotTaskCardDetail request id _ ->
            { model | dataLoading = { loading | startupAdmission = StartupAdmission.complete ("detail|" ++ String.fromInt request.requestId) loading.startupAdmission, cardDetailAdmissions = Set.remove request.requestId loading.cardDetailAdmissions, cardDetailRetries = if Dict.get id loading.taskCardDetailRequests == Just request then Set.remove ( "task", id ) loading.cardDetailRetries else loading.cardDetailRetries } }
        _ -> model


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
        |> retireHiddenNavigation


retireHiddenNavigation : Model -> Model
retireHiddenNavigation model =
    let
        loading = model.dataLoading
        hidden key request =
            key /= navigationBranchKey "workspace_root" Nothing
                && not (navigationBranchExpanded key model)
                && (request.inFlight || List.member key loading.navigationQueue)
        hiddenKeys =
            Dict.filter hidden loading.loadedNavigationBranches |> Dict.keys |> Set.fromList
        generation =
            loading.navigationGeneration + 1
        retire key request =
            if Set.member key hiddenKeys then
                let
                    refreshing = Dict.get key loading.navigationPasses |> Maybe.map .refreshing |> Maybe.withDefault False
                in
                { request | generation = generation, inFlight = False
                    , projectOffset = if refreshing then 0 else request.projectOffset
                    , taskOffset = if refreshing then 0 else request.taskOffset
                    , projectRequestPending = if refreshing then True else request.projectRequestPending
                    , taskRequestPending = if refreshing then True else request.taskRequestPending
                }
            else request
        retirePass key pass =
            if Set.member key hiddenKeys && pass.refreshing then emptyNavigationPass True else pass
    in
    if Set.isEmpty hiddenKeys then model
    else
        { model | dataLoading = { loading
            | navigationGeneration = generation
            , loadedNavigationBranches = Dict.map retire loading.loadedNavigationBranches
            , navigationQueue = List.filter (\key -> not (Set.member key hiddenKeys)) loading.navigationQueue
            , navigationPasses = Dict.map retirePass loading.navigationPasses
        } }


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
                    (not previous.inFlight || not needsTransport) && (retrying || not needsTransport || hasMore)

                canLoad =
                    canAdvance && needsTransport && not previous.inFlight

                previousPass =
                    Dict.get key model.dataLoading.navigationPasses

                healthyProjectPending =
                    previous.projectRequestPending && (previousPass |> Maybe.andThen .projectError) == Nothing

                healthyTaskPending =
                    previous.taskRequestPending && (previousPass |> Maybe.andThen .taskError) == Nothing

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
                        | readRevision = 0, projectOffset = projectOffset
                        , taskOffset = taskOffset
                        , generation = if retrying && canLoad then model.dataLoading.navigationGeneration + 1 else previous.generation
                        , inFlight = False
                        , succeeded = if canLoad then False else previous.succeeded
                        , projectRequestPending = if canLoad then entityKind == "project" || healthyProjectPending else previous.projectRequestPending
                        , taskRequestPending = if canLoad then entityKind == "task" || healthyTaskPending else previous.taskRequestPending
                    }

                currentLoading =
                    model.dataLoading
            in
            if not canAdvance then
                ( model, Cmd.none )

            else if canLoad then
                pumpNavigation (enqueueNavigation key { model
                    | dataLoading =
                        { currentLoading
                            | loadedNavigationBranches = Dict.insert key request currentLoading.loadedNavigationBranches
                            , navigationGeneration = max currentLoading.navigationGeneration request.generation
                            , navigationPresentations = Dict.insert key { nextPresentation | generation = request.generation } currentLoading.navigationPresentations
                            , navigationPasses =
                                Dict.update key
                                    (Maybe.map (\pass -> { pass | projectError = if entityKind == "project" then Nothing else pass.projectError, taskError = if entityKind == "task" then Nothing else pass.taskError }))
                                    currentLoading.navigationPasses
                        }
                  })

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

                progress = Dict.get (navigationBranchKey "workspace_root" Nothing) model.dataLoading.navigationPasses
                refreshing = progress |> Maybe.map .refreshing |> Maybe.withDefault False
                projectPaused = progress |> Maybe.andThen .projectError |> (/=) Nothing
                taskPaused = progress |> Maybe.andThen .taskError |> (/=) Nothing
                retrying =
                    if refreshing then
                        (if entityKind == "project" then projectPaused || previous.projectRequestPending else taskPaused || previous.taskRequestPending)
                    else not previous.succeeded

                nextPresentation =
                    if retrying then
                        previousPresentation

                    else
                        advancePresentation "root" Nothing entityKind model previous previousPresentation

                hasMore =
                    if entityKind == "project" then previous.projectHasMore else previous.taskHasMore

                needsTransport =
                    retrying || hasMore

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

                refreshRetry = retrying && refreshing

                generation = if canLoad && retrying then model.dataLoading.navigationGeneration + 1 else previous.generation

                request =
                    { previous
                        | generation = generation
                        , readRevision = 0
                        , projectOffset = projectOffset
                        , taskOffset = taskOffset
                        , inFlight = canLoad
                        , succeeded = if canLoad then False else previous.succeeded
                        , projectRequestPending = if canLoad then (if retrying then (if projectPaused || taskPaused then entityKind == "project" && (projectPaused || previous.projectRequestPending) else previous.projectRequestPending) else entityKind == "project") else previous.projectRequestPending
                        , taskRequestPending = if canLoad then (if retrying then (if projectPaused || taskPaused then entityKind == "task" && (taskPaused || previous.taskRequestPending) else previous.taskRequestPending) else entityKind == "task") else previous.taskRequestPending
                    }

                loading = model.dataLoading
                extendDemand pass =
                    let
                        demand = pass.rootDemand |> Maybe.withDefault (previous.projectCardCount,previous.taskCardCount)
                    in
                    { pass | rootDemand = Just ( if request.projectRequestPending then max (Tuple.first demand) (projectOffset + transportPageSize) else Tuple.first demand, if request.taskRequestPending then max (Tuple.second demand) (taskOffset + transportPageSize) else Tuple.second demand ) }
            in
            if not canAdvance then
                ( model, Cmd.none )

            else if canLoad then
                ( recordRootRecovery request { model | dataLoading = { loading | navigationGeneration = if retrying then generation else loading.navigationGeneration, rootNavigationRequest = Just request, rootNavigationPresentation = Just { nextPresentation | generation = generation }, navigationPasses = if refreshing && not retrying then Dict.update (navigationBranchKey "workspace_root" Nothing) (Maybe.map extendDemand) loading.navigationPasses else loading.navigationPasses } }
                , Api.fetchNavigationBranchWithDemand model.flags.apiUrl workspaceId "workspace_root" Nothing projectOffset taskOffset request.projectRequestPending request.taskRequestPending (navigationFilterQuery model)
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
prepareRootNavigationRequest expectedWorkspace original =
    let model = Feature.FocusedFilters.initialize original in
    case ( expectedWorkspace, model.selectedWorkspaceId, model.dataLoading.activeWorkspaceLoadToken ) of
        ( Just workspaceId, Just selectedWorkspaceId, Just _ ) ->
            if workspaceId == selectedWorkspaceId then
                let
                    loading =
                        model.dataLoading
                in
                ({ model
                    | dataLoading =
                        let
                            request =
                                { workspaceId = workspaceId
                                , readRevision = 0
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
                            , navigationPasses = Dict.remove (navigationBranchKey "workspace_root" Nothing) loading.navigationPasses
                            , rootNavigationPresentation = Just (initialPresentation request)
                            , navigationVisibilityActive = True
                        }
                } |> (\prepared -> prepared.dataLoading.rootNavigationRequest |> Maybe.map (\request -> recordRootRecovery request prepared) |> Maybe.withDefault prepared))

            else
                model

        _ ->
            model


rootNavigationContextMatches : String -> Model -> Bool
rootNavigationContextMatches workspaceId model =
    model.dataLoading.rootNavigationRequest
        |> Maybe.map (\request -> request.workspaceId == workspaceId && request.sessionEpoch == model.sessionRequestEpoch && request.filterFingerprint == navigationFilterFingerprint model
            && (request.inFlight || request.succeeded || request.projectRequestPending || request.taskRequestPending))
        |> Maybe.withDefault False


reloadNavigationForFilters : Model -> ( Model, Cmd Msg )
reloadNavigationForFilters model =
    let
        ( reset, command ) = resetNavigationForFilters model
        ( focused, focusCommand ) = case ( reset.selectedWorkspaceId, reset.focus.focusedEntity ) of
            ( Just workspaceId, Just ( kind, id ) ) -> beginNavigationFocus workspaceId kind id reset
            _ -> ( reset, Cmd.none )
    in
    ( focused, Cmd.batch [ command, focusCommand ] )


{-| Live same-filter refresh owns membership separately from an exact active
focus request. Only explicit filter resets recreate direct focus demand.
-}
resetNavigationForFilters : Model -> ( Model, Cmd Msg )
resetNavigationForFilters model =
    case model.selectedWorkspaceId of
        Nothing ->
            ( model, Cmd.none )

        Just workspaceId ->
            let
                current =
                    model.dataLoading

                socket = model.webSocket
                prefix = "workspace:" ++ workspaceId ++ "|navigation-summar"
                retiredSocket =
                    { socket | targetGenerations = Dict.map (\key revision -> if String.startsWith prefix key then revision + 1 else revision) socket.targetGenerations }

                generation =
                    current.navigationGeneration + 1

                resetLoading =
                    { current
                        | loadingWorkspaceData = False
                        , pendingWorkspaceLoads = 0
                        , activeWorkspaceLoadToken = Nothing
                        , navigationGeneration = generation
                        , loadedNavigationBranches = Dict.empty
                        , navigationQueue = []
                        , navigationPasses = Dict.empty
                        , visibleDetailDemand = Nothing
                        , viewportDetailPins = Set.empty
                        , cardDetailRetries = Set.empty
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
                                , readRevision = 0
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
                    { model | dataLoading = resetLoading, webSocket = retiredSocket }
            in
            ( resetModel |> (\prepared -> prepared.dataLoading.rootNavigationRequest |> Maybe.map (\request -> recordRootRecovery request prepared) |> Maybe.withDefault prepared)
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
            resetNavigationForFilters model

        loading =
            reloaded.dataLoading

        retainedRoot =
            case ( preserved.rootNavigationRequest, loading.rootNavigationRequest ) of
                ( Just previous, Just current ) ->
                    if previous.workspaceId == current.workspaceId && previous.sessionEpoch == current.sessionEpoch && previous.filterFingerprint == current.filterFingerprint then
                        Just
                            ( { current
                                | projectCardCount = previous.projectCardCount
                                , taskCardCount = previous.taskCardCount
                                , projectHasMore = previous.projectHasMore
                                , taskHasMore = previous.taskHasMore
                              }
                            , rootRefreshDemand preserved previous
                            )

                    else
                        Nothing

                _ ->
                    Nothing

        refreshedPasses =
            case retainedRoot of
                Just ( _, demand ) ->
                    let
                        pass =
                            emptyNavigationPass True
                    in
                    Dict.insert (navigationBranchKey "workspace_root" Nothing) { pass | rootDemand = Just demand } loading.navigationPasses

                Nothing ->
                    loading.navigationPasses

        -- A live tree refresh owns root/branch membership, not the independent
        -- direct-focus read. Keep its exact page identity while that demand is
        -- still current, including an in-flight ancestor continuation.
        retainedFocus =
            preserved.activeNavigationFocus
                |> Maybe.andThen
                    (\request ->
                        if request.inFlight
                            && model.selectedWorkspaceId == Just request.workspaceId
                            && model.sessionRequestEpoch == request.sessionEpoch
                            && navigationFilterFingerprint model == request.filterFingerprint
                            && model.focus.focusedEntity == Just ( request.entityType, request.entityId ) then
                            Just request

                        else
                            Nothing
                    )

        preservedModel =
            { reloaded
                | dataLoading =
                    { loading
                        | projectCardSummaries = preserved.projectCardSummaries
                        , taskCardSummaries = preserved.taskCardSummaries
                        , navigationVisibleProjectIds = preserved.navigationVisibleProjectIds
                        , navigationVisibleTaskIds = preserved.navigationVisibleTaskIds
                        , rootNavigationRequest = retainedRoot |> Maybe.map (Tuple.first >> Just) |> Maybe.withDefault loading.rootNavigationRequest
                        , navigationPasses = refreshedPasses
                        , activeNavigationFocus = retainedFocus
                        , navigationFocuses = retainedFocus |> Maybe.map (\request -> Dict.singleton (request.entityType ++ ":" ++ request.entityId) request) |> Maybe.withDefault Dict.empty
                    }
            }

        ( replayedModel, replayCommands ) =
            replayLoadedNavigationBranches preserved.loadedNavigationBranches preservedModel

        ( affectedModel, affectedCommands ) =
            replayAffectedNavigationBranches affectedProjects affectedTasks replayedModel

        currentLoading =
            affectedModel.dataLoading

        retainPresentation presentation =
            { presentation | generation = currentLoading.navigationGeneration }

        withPresentations =
            { affectedModel
                | dataLoading =
                    { currentLoading
                        | rootNavigationPresentation = Maybe.map retainPresentation preserved.rootNavigationPresentation
                        , navigationPresentations = Dict.union (Dict.map (\_ -> retainPresentation) preserved.navigationPresentations) currentLoading.navigationPresentations
                    }
            }
    in
    let ( admitted, batchCommand ) = pumpNavigation withPresentations in
    ( admitted, Cmd.batch (batchCommand :: command :: replayCommands ++ affectedCommands) )


rootRefreshDemand : DataLoadingModel -> NavigationBranchState -> ( Int, Int )
rootRefreshDemand loading previous =
    let
        inherited =
            Dict.get (navigationBranchKey "workspace_root" Nothing) loading.navigationPasses
                |> Maybe.andThen .rootDemand
                |> Maybe.withDefault ( 0, 0 )

        demanded accepted offset pending old =
            max old (max accepted (if pending then offset + transportPageSize else 0))
    in
    ( demanded previous.projectCardCount previous.projectOffset previous.projectRequestPending (Tuple.first inherited)
    , demanded previous.taskCardCount previous.taskOffset previous.taskRequestPending (Tuple.second inherited)
    )


{-| Summary invalidations own only affected direct lists. Keep unrelated passes,
physical admissions and presentations intact; the allocator is not their lifetime.
-}
revalidateNavigationForChangedSummaries : Model -> Api.NavigationSummariesResponse -> Model -> ( Model, Cmd Msg )
revalidateNavigationForChangedSummaries before summaries model =
    let
        filtered = not before.search.filterShowEmptyProjects || before.search.filterShowOnly /= ShowAll || before.search.filterProjectStatuses /= [] || before.search.filterTaskStatuses /= [] || before.search.filterPriority /= AnyPriority || String.trim before.search.query /= ""
        queried = String.trim before.search.query /= ""
        projectOwner summary = summary.parentId |> Maybe.map (\id -> ( "project", id )) |> Maybe.withDefault ( "workspace_root", "root" )
        taskOwner = taskNavigationOwner
        ownerPath source visited owner =
            if Set.member owner visited then []
            else
                let
                    ancestors =
                        if not filtered then []
                        else case owner of
                            ( "project", id ) -> Dict.get id source.dataLoading.projectCardSummaries |> Maybe.map (projectOwner >> ownerPath source (Set.insert owner visited)) |> Maybe.withDefault [ ( "workspace_root", "root" ) ]
                            ( "task", id ) -> Dict.get id source.dataLoading.taskCardSummaries |> Maybe.map (taskOwner >> ownerPath source (Set.insert owner visited)) |> Maybe.withDefault [ ( "workspace_root", "root" ) ]
                            _ -> []
                in
                owner :: ancestors
        projectsChanged summary =
            let old = Dict.get summary.id before.dataLoading.projectCardSummaries in
            case old of
                Nothing -> ownerPath model Set.empty (projectOwner summary)
                Just previous ->
                    let
                        placement = previous.parentId /= summary.parentId || previous.status /= summary.status || previous.priority /= summary.priority || String.toLower previous.name /= String.toLower summary.name || queried
                        children = previous.directProjectCount /= summary.directProjectCount || previous.directTaskCount /= summary.directTaskCount || previous.hasChildren /= summary.hasChildren
                    in
                    (if placement then ownerPath before Set.empty (projectOwner previous) ++ ownerPath model Set.empty (projectOwner summary) else [])
                        ++ (if children then ownerPath model Set.empty ( "project", summary.id ) else [])
        tasksChanged summary =
            let old = Dict.get summary.id before.dataLoading.taskCardSummaries in
            case old of
                Nothing -> ownerPath model Set.empty (taskOwner summary)
                Just previous ->
                    let
                        placement = previous.parentId /= summary.parentId || previous.projectId /= summary.projectId || previous.status /= summary.status || previous.priority /= summary.priority || String.toLower previous.title /= String.toLower summary.title || taskPresentationOrder before previous /= taskPresentationOrder model summary || queried
                        children = previous.directSubtaskCount /= summary.directSubtaskCount || previous.hasChildren /= summary.hasChildren
                    in
                    (if placement then ownerPath before Set.empty (taskOwner previous) ++ ownerPath model Set.empty (taskOwner summary) else [])
                        ++ (if children then ownerPath model Set.empty ( "task", summary.id ) else [])
        missingProjects = List.concatMap (\id -> Dict.get id before.dataLoading.projectCardSummaries |> Maybe.map (projectOwner >> ownerPath before Set.empty) |> Maybe.withDefault []) summaries.missingProjectIds
        missingTasks = List.concatMap (\id -> Dict.get id before.dataLoading.taskCardSummaries |> Maybe.map (taskOwner >> ownerPath before Set.empty) |> Maybe.withDefault []) summaries.missingTaskIds
        owners = Set.fromList (List.concatMap projectsChanged summaries.projects ++ List.concatMap tasksChanged summaries.tasks ++ missingProjects ++ missingTasks) |> Set.toList
    in
    revalidateNavigationOwners owners model


taskNavigationOwner : { a | parentId : Maybe String, projectId : Maybe String } -> ( String, String )
taskNavigationOwner task =
    case task.parentId of
        Just id -> ( "task", id )
        Nothing -> task.projectId |> Maybe.map (\id -> ( "project", id )) |> Maybe.withDefault ( "workspace_root", "root" )


{-| Readiness responses carry placement even for a parent outside the cached page.
Retire its owner's cursor when the server ordering changes, before accepting more offsets.
-}
revalidateNavigationForTaskReadiness : Model -> Api.Task -> Model -> ( Model, Cmd Msg )
revalidateNavigationForTaskReadiness before task model =
    revalidateNavigationForTasksReadiness before [ task ] model


revalidateNavigationForTasksReadiness : Model -> List Api.Task -> Model -> ( Model, Cmd Msg )
revalidateNavigationForTasksReadiness before tasks model =
    let
        owners = tasks
            |> List.filter (\task -> taskPresentationOrder before task /= taskPresentationOrder model task)
            |> List.map taskNavigationOwner
            |> Set.fromList |> Set.toList
    in
    if List.isEmpty owners then ( model, Cmd.none )
    else revalidateNavigationOwners owners model


revalidateNavigationOwners : List ( String, String ) -> Model -> ( Model, Cmd Msg )
revalidateNavigationOwners owners model =
    let
        replay owner ( current, commands ) =
            let
                loading = current.dataLoading
                generation = loading.navigationGeneration + 1
                fingerprint = navigationFilterFingerprint current
                allocated = { current | dataLoading = { loading | navigationGeneration = generation } }
            in
            case ( current.selectedWorkspaceId, owner ) of
                ( Just workspaceId, ( "workspace_root", _ ) ) ->
                    case loading.rootNavigationRequest of
                        Just previous ->
                            let
                                demand = rootRefreshDemand loading previous
                                pass = emptyNavigationPass True
                                rootPass = { pass | rootDemand = Just demand }
                                request = { previous | generation = generation, readRevision = 0, projectOffset = 0, taskOffset = 0, inFlight = True, succeeded = False, projectRequestPending = True, taskRequestPending = True }
                                nextLoading = allocated.dataLoading
                                presentation = loading.rootNavigationPresentation |> Maybe.map (\value -> { value | generation = generation })
                                next = { allocated | dataLoading = { nextLoading | rootNavigationRequest = Just request, rootNavigationPresentation = presentation, navigationPasses = Dict.insert (navigationBranchKey "workspace_root" Nothing) rootPass nextLoading.navigationPasses } }
                            in
                            ( recordRootRecovery request next, Api.fetchRootNavigation current.flags.apiUrl workspaceId (navigationFilterQuery current) (GotRootNavigation workspaceId current.sessionRequestEpoch current.dataLoading.activeWorkspaceLoadToken generation fingerprint 0 0) :: commands )
                        Nothing -> ( current, commands )
                ( Just workspaceId, ( kind, id ) ) ->
                    let key = navigationBranchKey kind (Just id) in
                    if (kind == "project" && Dict.member id current.dataLoading.projectCardSummaries) || (kind == "task" && Dict.member id current.dataLoading.taskCardSummaries) then
                        let
                            previous = Dict.get key loading.loadedNavigationBranches |> Maybe.withDefault initialNavigationBranchState
                            presentation = Dict.get key loading.navigationPresentations
                            ( restarted, restartedCommands ) = replayBranch workspaceId generation fingerprint kind id previous allocated commands
                            restartedLoading = restarted.dataLoading
                            retained = presentation |> Maybe.map (\value -> { value | generation = generation })
                        in
                        ( { restarted | dataLoading = { restartedLoading | navigationPresentations = retained |> Maybe.map (\value -> Dict.insert key value restartedLoading.navigationPresentations) |> Maybe.withDefault restartedLoading.navigationPresentations } }, restartedCommands )
                    else ( current, commands )
                _ -> ( current, commands )
        ( replayed, replayCommands ) = List.foldl replay ( model, [] ) owners
        ( presented, detailCommand ) = ensureAllNavigationPresentations replayed
    in
    ( presented, Cmd.batch (detailCommand :: replayCommands) )


{-| Retire pending bounded responses as soon as the scoped stream is invalidated,
while retaining the last canonical cards and the user's presentation positions.
-}
invalidateNavigationRequests : Model -> Model
invalidateNavigationRequests model =
    let
        loading =
            model.dataLoading

        generation =
            loading.navigationGeneration + 1

        retire request =
            { request | generation = generation, inFlight = False, succeeded = False }

        retain presentation =
            { presentation | generation = generation }

        retainedRootDemand =
            case loading.rootNavigationRequest of
                Just request ->
                    if Just request.workspaceId == model.selectedWorkspaceId && request.sessionEpoch == model.sessionRequestEpoch && request.filterFingerprint == navigationFilterFingerprint model then
                        let
                            pass =
                                emptyNavigationPass True
                        in
                        Dict.singleton (navigationBranchKey "workspace_root" Nothing) { pass | rootDemand = Just (rootRefreshDemand loading request) }

                    else
                        Dict.empty

                Nothing ->
                    Dict.empty
    in
    { model
        | dataLoading =
            { loading
                | navigationGeneration = generation
                , rootNavigationRequest = Maybe.map (\request -> let retired = retire request in { retired | projectRequestPending = False, taskRequestPending = False }) loading.rootNavigationRequest
                , loadedNavigationBranches = Dict.map (\_ -> retire) loading.loadedNavigationBranches
                , navigationQueue = []
                , navigationPasses = retainedRootDemand
                , rootNavigationPresentation = Maybe.map retain loading.rootNavigationPresentation
                , navigationPresentations = Dict.map (\_ -> retain) loading.navigationPresentations
                , projectCardDetailRequests = Dict.empty
                , taskCardDetailRequests = Dict.empty
                , activeNavigationFocus = Nothing
                , navigationFocuses = Dict.empty
                , activeWorkspaceLoadToken = Nothing
                , loadingWorkspaceData = False
                , pendingWorkspaceLoads = 0
            }
    }


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
    , readRevision = 0
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
                , inFlight = False
                , succeeded = False
                , projectHasMore = False
                , taskHasMore = False
                , projectCardCount = previous.projectCardCount
                , taskCardCount = previous.taskCardCount
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
                        , navigationPasses = Dict.insert branchKey (emptyNavigationPass True) loading.navigationPasses
                    }
            }

    in
    ( enqueueNavigation branchKey replayed, commands )


beginNavigationFocus : String -> String -> String -> Model -> ( Model, Cmd Msg )
beginNavigationFocus workspaceId entityType entityId model =
    requestNavigationFocus False workspaceId entityType entityId (Feature.FocusedFilters.initialize model)


revalidateNavigationFocus : String -> String -> String -> Model -> ( Model, Cmd Msg )
revalidateNavigationFocus workspaceId entityType entityId model =
    requestNavigationFocus True workspaceId entityType entityId (Feature.FocusedFilters.initialize model)


requestNavigationFocus : Bool -> String -> String -> String -> Model -> ( Model, Cmd Msg )
requestNavigationFocus force workspaceId entityType entityId model =
    let
        projectAncestry visited id =
            if Set.member ( "project", id ) visited then False
            else
                Dict.get id model.projects
                    |> Maybe.map (\project -> project.workspaceId == workspaceId && (project.parentId |> Maybe.map (projectAncestry (Set.insert ( "project", id ) visited)) |> Maybe.withDefault True))
                    |> Maybe.withDefault False

        taskAncestry visited id =
            if Set.member ( "task", id ) visited then False
            else
                Dict.get id model.tasks
                    |> Maybe.map (\task -> task.workspaceId == workspaceId && (case task.parentId of
                        Just parent -> taskAncestry (Set.insert ( "task", id ) visited) parent
                        Nothing -> task.projectId |> Maybe.map (projectAncestry visited) |> Maybe.withDefault True))
                    |> Maybe.withDefault False

        alreadyPresent =
            case entityType of
                "project" -> Dict.member entityId model.dataLoading.projectCardSummaries && projectAncestry Set.empty entityId
                "task" -> Dict.member entityId model.dataLoading.taskCardSummaries && taskAncestry Set.empty entityId
                _ -> True

        generation =
            model.dataLoading.navigationGeneration + 1

        requestKey =
            entityType ++ ":" ++ entityId

        previousRequest =
            Dict.get requestKey model.dataLoading.navigationFocuses

        matchesContext focusRequest =
            focusRequest.workspaceId == workspaceId
                && focusRequest.sessionEpoch == model.sessionRequestEpoch
                && focusRequest.filterFingerprint == navigationFilterFingerprint model
                && focusRequest.entityType == entityType
                && focusRequest.entityId == entityId

        currentRequest =
            previousRequest |> Maybe.andThen (\focusRequest -> if matchesContext focusRequest then Just focusRequest else Nothing)

        requestedAncestorOffset =
            if force || (currentRequest |> Maybe.map .succeeded |> Maybe.withDefault False) && not alreadyPresent then
                0

            else
                currentRequest |> Maybe.map .ancestorOffset |> Maybe.withDefault 0

        retryable =
            currentRequest
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
            , summaryRevision = model.dataLoading.nextNavigationSummaryRevision + 1
            , inFlight = True
            , succeeded = False
            }

        currentDataLoading =
            model.dataLoading

        alreadyInFlight =
            case currentDataLoading.activeNavigationFocus of
                Just activeRequest ->
                    activeRequest.inFlight
                        && matchesContext activeRequest

                Nothing ->
                    False

        alreadyCompleted =
            alreadyPresent && (currentRequest |> Maybe.map .succeeded |> Maybe.withDefault False)

        retiredRequest =
            previousRequest /= Nothing && currentRequest == Nothing
    in
    if ((alreadyPresent && not retiredRequest && not retryable) || alreadyCompleted || alreadyInFlight) && not force || (entityType /= "project" && entityType /= "task") then
        ( model, Cmd.none )

    else
        ( { model | dataLoading = { currentDataLoading | navigationGeneration = generation, nextNavigationSummaryRevision = request.summaryRevision, activeNavigationFocus = Just request, navigationFocuses = Dict.insert requestKey request currentDataLoading.navigationFocuses } }
        , Api.fetchNavigationFocus model.flags.apiUrl workspaceId entityType entityId requestedAncestorOffset
            (GotNavigationFocus workspaceId model.sessionRequestEpoch generation (navigationFilterFingerprint model) entityType entityId requestedAncestorOffset)
        )


prepareForPageLoad : Page -> DataLoadingModel -> DataLoadingModel
prepareForPageLoad page dataLoading =
    { dataLoading
        | planningCounts = retirePlanningCounts dataLoading.planningCounts
        , loadingWorkspaceData =
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
    case maybeToken of
        Just token ->
            if dataLoading.activeWorkspaceLoadToken == Just token then
                let
                    remaining =
                        max 0 (dataLoading.pendingWorkspaceLoads - 1)
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

            else
                dataLoading

        Nothing ->
            dataLoading


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
        request =
            if parentKind == "workspace_root" then model.dataLoading.rootNavigationRequest else Dict.get (navigationBranchKey parentKind maybeParentId) model.dataLoading.loadedNavigationBranches

        revision =
            (if parentKind == "workspace_root" then model.dataLoading.rootNavigationRequest else Dict.get (navigationBranchKey parentKind maybeParentId) model.dataLoading.loadedNavigationBranches)
                |> Maybe.map .readRevision |> Maybe.withDefault 0
        matchesProject summary =
            (request |> Maybe.map .projectRequestPending |> Maybe.withDefault True) && projectInScope summary

        projectInScope summary =
            case ( parentKind, maybeParentId ) of
                ( "workspace_root", Nothing ) ->
                    summary.parentId == Nothing

                ( "project", Just parentId ) ->
                    summary.parentId == Just parentId

                _ ->
                    False

        matchesTask summary =
            (request |> Maybe.map .taskRequestPending |> Maybe.withDefault True) && taskInScope summary

        taskInScope summary =
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
    mergeNavigationSummariesAt revision navigation.projects.items navigation.tasks.items pruned


mergeNavigationSummaries : List Api.ProjectCardSummary -> List Api.TaskCardSummary -> Model -> Model
mergeNavigationSummaries projectSummaries taskSummaries model =
    let ( revision, issued ) = Helpers.allocatePlanningRead model
    in mergeNavigationSummariesAt revision projectSummaries taskSummaries issued


mergeNavigationSummariesAt : Int -> List Api.ProjectCardSummary -> List Api.TaskCardSummary -> Model -> Model
mergeNavigationSummariesAt revision incomingProjects incomingTasks model =
    let
        protect summary =
            let key = case summary of
                    Api.NavigationProjectSummary project -> "project:" ++ project.id
                    Api.NavigationTaskSummary task -> "task:" ++ task.id
            in
            if Helpers.planningReadIsCurrent revision key model then Just summary
            else
                case Dict.get key model.dataLoading.navigationSummaryAuthority of
                    Just authority ->
                        case authority.summary of
                            Just current -> Just current
                            Nothing -> case ( summary, authority.readiness ) of
                                ( Api.NavigationTaskSummary task, Just (TaskReadiness rollup) ) -> Just (Api.NavigationTaskSummary { task | readinessRollup = rollup })
                                ( Api.NavigationProjectSummary project, Just (ProjectReadiness rollup) ) -> Just (Api.NavigationProjectSummary { project | readinessRollup = rollup })
                                _ -> Nothing
                    Nothing -> Just summary

        projectSummaries = incomingProjects |> List.filterMap (Api.NavigationProjectSummary >> protect) |> List.filterMap (\summary -> case summary of
            Api.NavigationProjectSummary project -> Just project
            _ -> Nothing)
        taskSummaries = incomingTasks |> List.filterMap (Api.NavigationTaskSummary >> protect) |> List.filterMap (\summary -> case summary of
            Api.NavigationTaskSummary task -> Just task
            _ -> Nothing)

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
        |> Helpers.recordPlanningSummaries revision
            (List.map (\summary -> ( "project:" ++ summary.id, Just (Api.NavigationProjectSummary summary) )) projectSummaries
                ++ List.map (\summary -> ( "task:" ++ summary.id, Just (Api.NavigationTaskSummary summary) )) taskSummaries
            )


{-| Keep a changed-payload fence separate from transport generations. Identical
root/branch replay must not starve a pending focus read; deletion is authority too.
-}
markMissingNavigationSummaries : List String -> List String -> Model -> Model
markMissingNavigationSummaries projectIds taskIds model =
    let ( revision, issued ) = Helpers.allocatePlanningRead model
    in
    Helpers.recordPlanningSummaries revision
        (List.map (\id -> ( "project:" ++ id, Nothing )) projectIds
            ++ List.map (\id -> ( "task:" ++ id, Nothing )) taskIds
        )
        issued


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


{-| A transition-local lookup over committed summaries. Queue discovery keeps
its separate project-then-task Dict order; these buckets serve presentation only.
-}
type alias NavigationSummaryProjection =
    { projects : Dict.Dict String (List Api.ProjectCardSummary)
    , tasks : Dict.Dict String (List Api.TaskCardSummary)
    }


navigationSummaryProjection : Model -> NavigationSummaryProjection
navigationSummaryProjection model =
    navigationSummaryProjectionFor (always True) model


navigationSummaryProjectionFor : (String -> Bool) -> Model -> NavigationSummaryProjection
navigationSummaryProjectionFor includes model =
    let
        insert key summary groups =
            if includes key then Dict.update key (Maybe.withDefault [] >> (::) summary >> Just) groups else groups
        projectParent summary =
            navigationBranchKey (if summary.parentId == Nothing then "workspace_root" else "project") summary.parentId
        taskParent summary =
            case summary.parentId of
                Just parent -> navigationBranchKey "task" (Just parent)
                Nothing -> navigationBranchKey (if summary.projectId == Nothing then "workspace_root" else "project") summary.projectId
    in
    { projects =
        Dict.foldl (\_ summary -> insert (projectParent summary) summary) Dict.empty model.dataLoading.projectCardSummaries
            |> Dict.map (\_ -> List.sortBy (\project -> ( Api.projectStatusOrder project.status, negate project.priority, ( String.toLower project.name, project.id ) )))
    , tasks =
        Dict.foldl (\_ summary -> insert (taskParent summary) summary) Dict.empty model.dataLoading.taskCardSummaries
            |> Dict.map (\_ -> List.sortBy (\task -> ( taskPresentationOrder model task, negate task.priority, ( String.toLower task.title, task.id ) )))
    }


presentedNavigationSummaries : NavigationSummaryProjection -> String -> Maybe String -> Model -> ( List Api.ProjectCardSummary, List Api.TaskCardSummary )
presentedNavigationSummaries projection parentKind maybeParentId model =
    let
        ( projectOffset, taskOffset ) = presentationOffsets parentKind maybeParentId model
        parent = Maybe.map (\parentId -> ( parentKind, parentId )) maybeParentId
        valid =
            case ( parentKind, maybeParentId ) of
                ( "workspace_root", Nothing ) -> True
                ( "project", Just _ ) -> True
                ( "task", Just _ ) -> True
                _ -> False
        key = navigationBranchKey parentKind maybeParentId
        members groups = if valid then Dict.get key groups |> Maybe.withDefault [] else []
    in
    ( members projection.projects |> navigationPresentationWindow .id (localPresentationPinnedIds parent "project" model) projectOffset
    , members projection.tasks |> navigationPresentationWindow .id (localPresentationPinnedIds parent "task" model) taskOffset
    )


beginProjectCardDetail : Api.ProjectCardSummary -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
beginProjectCardDetail summary ( model, commands ) =
    let
        alreadyCurrent =
            Dict.get summary.id model.dataLoading.projectCardDetailRequests
                |> Maybe.map
                    (\request ->
                        (Dict.get summary.id model.projects |> Maybe.map .updatedAt)
                            == Just request.expectedUpdatedAt
                            && (request.inFlight || request.succeeded || request.expectedUpdatedAt == summary.updatedAt)
                    )
                |> Maybe.withDefault False
    in
    if alreadyCurrent || model.move.dialog /= Nothing || Set.size model.dataLoading.cardDetailAdmissions + Dict.size model.auditDiff.admissions + (Dict.values model.move.admissions |> List.filter (.branch >> String.startsWith "@match:") |> List.length) >= 6 then
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
                    , cardDetailAdmissions = Set.insert request.requestId loading.cardDetailAdmissions
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
                            && (request.inFlight || request.succeeded || request.expectedUpdatedAt == summary.updatedAt)
                    )
                |> Maybe.withDefault False
    in
    if alreadyCurrent || model.move.dialog /= Nothing || Set.size model.dataLoading.cardDetailAdmissions + Dict.size model.auditDiff.admissions + (Dict.values model.move.admissions |> List.filter (.branch >> String.startsWith "@match:") |> List.length) >= 6 then
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
                    , cardDetailAdmissions = Set.insert request.requestId loading.cardDetailAdmissions
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
            navigationBranchExpanded key model

        needsRequest =
            Dict.get key model.dataLoading.loadedNavigationBranches
                |> Maybe.map (\request -> not request.inFlight && (request.projectRequestPending || request.taskRequestPending))
                |> Maybe.withDefault True
    in
    if summary.hasChildren && expanded && needsRequest then
        case model.selectedWorkspaceId of
            Just workspaceId ->
                let
                    ( updated, command ) =
                        resumeNavigationBranch "project" workspaceId summary.id model
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
            navigationBranchExpanded key model

        needsRequest =
            Dict.get key model.dataLoading.loadedNavigationBranches
                |> Maybe.map (\request -> not request.inFlight && (request.projectRequestPending || request.taskRequestPending))
                |> Maybe.withDefault True
    in
    if summary.hasChildren && expanded && needsRequest then
        case model.selectedWorkspaceId of
            Just workspaceId ->
                let
                    ( updated, command ) =
                        resumeNavigationBranch "task" workspaceId summary.id model
                in
                ( updated, command :: commands )

            Nothing ->
                ( model, commands )

    else
        ( model, commands )


ensureSummaries : List Api.ProjectCardSummary -> List Api.TaskCardSummary -> Model -> ( Model, Cmd Msg )
ensureSummaries projects tasks model =
    let
        ( withProjectBranches, projectBranchCommands ) =
            List.foldl ensureExpandedProjectBranch ( model, [] ) projects

        ( withTaskBranches, commands ) =
            List.foldl ensureExpandedTaskBranch ( withProjectBranches, projectBranchCommands ) tasks
    in
    ( withTaskBranches, Cmd.batch commands )


resumeNavigationBranch : String -> String -> String -> Model -> ( Model, Cmd Msg )
resumeNavigationBranch kind workspaceId id model =
    let
        key = navigationBranchKey kind (Just id)
    in
    case Dict.get key model.dataLoading.loadedNavigationBranches of
        Nothing -> ( stageNavigationBranch kind workspaceId (Just id) model, Cmd.none )
        Just request ->
            if request.projectRequestPending || request.taskRequestPending then
                ( enqueueNavigation key model, Cmd.none )
            else
                ( model, Cmd.none )


ensureExpandedBranches : Model -> ( Model, Cmd Msg )
ensureExpandedBranches model =
    ensureSummaries (Dict.values model.dataLoading.projectCardSummaries) (Dict.values model.dataLoading.taskCardSummaries) model


{-| Only a current viewport can replace detail demand. Cached entities outside
canonical membership cannot become demands through a stale layout callback.
-}
ensureViewportCardDetails : String -> Int -> Int -> Set.Set String -> Set.Set String -> Set.Set ( String, String ) -> Model -> ( Model, Cmd Msg )
ensureViewportCardDetails workspaceId sessionEpoch generation projectIds taskIds pins model =
    if model.selectedWorkspaceId /= Just workspaceId || model.sessionRequestEpoch /= sessionEpoch || model.dataLoading.navigationGeneration /= generation || model.auth.status /= AuthReady then
        ( model, Cmd.none )
    else
        let
            loading = model.dataLoading
            current ( kind, id ) =
                if kind == "project" then Set.member id loading.navigationVisibleProjectIds
                else kind == "task" && Set.member id loading.navigationVisibleTaskIds
            bounded = pins |> Set.filter current |> Set.toList |> List.take 6 |> Set.fromList
        in
        ensureVisibleCardDetails workspaceId sessionEpoch generation projectIds taskIds { model | dataLoading = { loading | viewportDetailPins = bounded } }


ensureVisibleCardDetails : String -> Int -> Int -> Set.Set String -> Set.Set String -> Model -> ( Model, Cmd Msg )
ensureVisibleCardDetails workspaceId sessionEpoch generation projectIds taskIds model =
    if model.selectedWorkspaceId /= Just workspaceId || model.sessionRequestEpoch /= sessionEpoch || model.dataLoading.navigationGeneration /= generation || model.auth.status /= AuthReady then
        ( model, Cmd.none )
    else
        let
            loading = model.dataLoading
            demand =
                List.map (Tuple.pair "project") (Set.toList (Set.intersect projectIds loading.navigationVisibleProjectIds))
                    ++ List.map (Tuple.pair "task") (Set.toList (Set.intersect taskIds loading.navigationVisibleTaskIds))
                    |> List.take 25
            projects = demand |> List.filter (Tuple.first >> (==) "project") |> List.map Tuple.second |> Set.fromList
            tasks = demand |> List.filter (Tuple.first >> (==) "task") |> List.map Tuple.second |> Set.fromList
        in
        ensurePresentedCardDetails { model | dataLoading = { loading | visibleDetailDemand = Just ( projects, tasks ) } }


{-| Transitional presentation demand is globally bounded, rather than 25
details per expanded branch. The viewport may replace it with explicit IDs;
focus/edit paths remain demanded independently of ordinary presentation.
-}
detailPinnedIds : String -> Model -> Set.Set String
detailPinnedIds kind model =
    let
        edited =
            model.editing.editState |> Maybe.map (\(EditingField state) -> ( state.entityType, state.entityId ))
        inline =
            case model.editing.inlineCreate of
                Just (InlineCreateProject state) -> state.parentId |> Maybe.map (Tuple.pair "project")
                Just (InlineCreateTask state) ->
                    if kind == "task" then state.parentId |> Maybe.map (Tuple.pair "task")
                    else state.projectId |> Maybe.map (Tuple.pair "project")
                _ -> Nothing
    in
    [ model.focus.focusedEntity, edited, inline ]
        |> List.filterMap identity
        |> (\pins -> pins ++ Set.toList model.dataLoading.cardDetailRetries ++ Set.toList model.dataLoading.viewportDetailPins)
        |> List.filter (Tuple.first >> (==) kind)
        |> List.map Tuple.second
        |> Set.fromList


ensurePresentedCardDetails : Model -> ( Model, Cmd Msg )
ensurePresentedCardDetails source =
    ensurePresentedCardDetailsWith
        (if source.dataLoading.visibleDetailDemand == Nothing then
            let
                owners = navigationBranchKey "workspace_root" Nothing :: (Dict.keys source.dataLoading.loadedNavigationBranches |> List.filter (\key -> navigationBranchExpanded key source)) |> Set.fromList
            in
            navigationSummaryProjectionFor (\key -> Set.member key owners) source
         else { projects = Dict.empty, tasks = Dict.empty }) source


ensurePresentedCardDetailsWith : NavigationSummaryProjection -> Model -> ( Model, Cmd Msg )
ensurePresentedCardDetailsWith projection source =
    let
        model = prepareBackgroundAdmission source
        ( projectIds, taskIds ) =
            case model.dataLoading.visibleDetailDemand of
                Just demand -> demand
                Nothing ->
                    let
                        branches =
                            ( "workspace_root", Nothing ) ::
                                (Dict.keys model.dataLoading.loadedNavigationBranches
                                    |> List.filter (\key -> navigationBranchExpanded key model)
                                    |> List.filterMap (\key -> case String.split ":" key of
                                        [ kind, id ] -> Just ( kind, Just id )
                                        _ -> Nothing))
                
                        presented =
                            List.concatMap (\( kind, parent ) ->
                                let
                                    ( presentedProjects, presentedTasks ) = presentedNavigationSummaries projection kind parent model
                                in
                                List.map (\item -> ( "project", item.id )) presentedProjects ++ List.map (\item -> ( "task", item.id )) presentedTasks
                                ) branches
                                |> List.take 25
                
                                    in
                    ( presented |> List.filter (Tuple.first >> (==) "project") |> List.map Tuple.second |> Set.fromList
                    , presented |> List.filter (Tuple.first >> (==) "task") |> List.map Tuple.second |> Set.fromList
                    )

        projects =
            Set.union projectIds (detailPinnedIds "project" model) |> Set.toList |> List.filterMap (\id -> Dict.get id model.dataLoading.projectCardSummaries)

        tasks =
            Set.union taskIds (detailPinnedIds "task" model) |> Set.toList |> List.filterMap (\id -> Dict.get id model.dataLoading.taskCardSummaries)

        projectPins = detailPinnedIds "project" model
        taskPins = detailPinnedIds "task" model
        ( focused, focusedCommands ) =
            case model.focus.focusedEntity of
                Just ( "project", id ) -> Dict.get id model.dataLoading.projectCardSummaries |> Maybe.map (\summary -> beginProjectCardDetail summary ( model, [] )) |> Maybe.withDefault ( model, [] )
                Just ( "task", id ) -> Dict.get id model.dataLoading.taskCardSummaries |> Maybe.map (\summary -> beginTaskCardDetail summary ( model, [] )) |> Maybe.withDefault ( model, [] )
                _ -> ( model, [] )
        ( pinnedProjects, pinnedProjectCommands ) = List.foldl beginProjectCardDetail ( focused, focusedCommands ) (List.filter (\summary -> Set.member summary.id projectPins) projects)
        ( pinned, pinCommands ) = List.foldl beginTaskCardDetail ( pinnedProjects, pinnedProjectCommands ) (List.filter (\summary -> Set.member summary.id taskPins) tasks)
        ordinary begin summary ( current, accumulated ) =
            let permit = current.dataLoading.backgroundAdmission in
            if current.activeTab /= ProjectsTab || permit.remaining <= 0 || not (StartupAdmission.canOrdinary current.dataLoading.startupAdmission) || (permit.details >= 3 && not (List.isEmpty current.dataLoading.navigationQueue)) then ( current, accumulated )
            else
                let ( next, nextCommands ) = begin summary ( current, accumulated ) in
                if Set.size next.dataLoading.cardDetailAdmissions > Set.size current.dataLoading.cardDetailAdmissions then
                    consumeBackground Nothing next |> Maybe.map (\admitted -> ( admitted, nextCommands )) |> Maybe.withDefault ( current, accumulated )
                else ( next, nextCommands )
        ( hydratedProjects, projectCommands ) = List.foldl (ordinary beginProjectCardDetail) ( pinned, pinCommands ) projects
        ( hydrated, commands ) = List.foldl (ordinary beginTaskCardDetail) ( hydratedProjects, projectCommands ) tasks
    in
    ( hydrated, Cmd.batch commands )


{-| Refill only as far as the retained presentation window after a bounded
revalidation; never chase all remaining workspace pages.
-}
restoreNavigationPresentation : String -> Maybe String -> Model -> ( Model, Cmd Msg )
restoreNavigationPresentation parentKind maybeParentId model =
    let
        loading =
            model.dataLoading

        key =
            navigationBranchKey parentKind maybeParentId

        rootBranch =
            parentKind == "workspace_root"

        expected =
            if rootBranch then loading.rootNavigationRequest else Dict.get key loading.loadedNavigationBranches
    in
    case expected of
        Just previous ->
            if not rootBranch || previous.inFlight || not previous.succeeded then
                ( model, Cmd.none )

            else
                let
                    parent =
                        Maybe.map (Tuple.pair parentKind) maybeParentId

                    presentation =
                        presentationFor previous (if rootBranch then loading.rootNavigationPresentation else Dict.get key loading.navigationPresentations)

                    needs kind offset hasMore =
                        offset > 0 && presentationNeedsTransport parentKind parent kind model presentation previous && hasMore

                    projectPending =
                        needs "project" presentation.projectOffset previous.projectHasMore

                    taskPending =
                        needs "task" presentation.taskOffset previous.taskHasMore

                    clamp kind offset hasMore =
                        if hasMore then offset else
                            let
                                capacity = max 1 (presentationCapacity parentKind parent kind model)
                                lastOffset = (max 0 (cachedOrdinaryCount parent kind model previous - 1) // capacity) * capacity
                            in
                            min offset lastOffset

                    retained =
                        { presentation | projectOffset = clamp "project" presentation.projectOffset previous.projectHasMore, taskOffset = clamp "task" presentation.taskOffset previous.taskHasMore }

                    request =
                        { previous
                            | readRevision = 0, projectOffset = previous.projectOffset + (if projectPending then transportPageSize else 0)
                            , taskOffset = previous.taskOffset + (if taskPending then transportPageSize else 0)
                            , projectRequestPending = projectPending
                            , taskRequestPending = taskPending
                            , inFlight = projectPending || taskPending
                            , succeeded = not (projectPending || taskPending)
                        }

                    updated =
                        { model | dataLoading =
                            if rootBranch then
                                { loading | rootNavigationRequest = Just request, rootNavigationPresentation = Just retained }
                            else
                                { loading | loadedNavigationBranches = Dict.insert key request loading.loadedNavigationBranches, navigationPresentations = Dict.insert key retained loading.navigationPresentations }
                        }
                in
                if request.inFlight then
                    ( if rootBranch then recordRootRecovery request updated else stampNavigationRead key request updated
                    , Api.fetchNavigationBranchWithDemand model.flags.apiUrl request.workspaceId parentKind maybeParentId request.projectOffset request.taskOffset request.projectRequestPending request.taskRequestPending (navigationFilterQuery model)
                        (if rootBranch then
                            GotRootNavigation request.workspaceId request.sessionEpoch Nothing request.generation request.filterFingerprint request.projectOffset request.taskOffset
                         else
                            GotNavigationBranch request.workspaceId request.sessionEpoch request.generation key request.filterFingerprint request.projectOffset request.taskOffset)
                    )

                else
                    ( updated, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


ensureNavigationPresentation : String -> Maybe String -> Model -> ( Model, Cmd Msg )
ensureNavigationPresentation parentKind maybeParentId model =
    ensureNavigationPresentationWith
        (navigationSummaryProjectionFor ((==) (navigationBranchKey parentKind maybeParentId)) model)
        ensurePresentedCardDetails parentKind maybeParentId model


ensureNavigationPresentationWith : NavigationSummaryProjection -> (Model -> ( Model, Cmd Msg )) -> String -> Maybe String -> Model -> ( Model, Cmd Msg )
ensureNavigationPresentationWith projection hydrate parentKind maybeParentId model =
    let
        ( restored, transportCmd ) =
            restoreNavigationPresentation parentKind maybeParentId model

        ( projects, tasks ) =
            presentedNavigationSummaries projection parentKind maybeParentId restored

        ( expanded, branchCmd ) =
            ensureSummaries projects tasks restored

        ( admitted, admissionCmd ) =
            pumpNavigation expanded

        ( hydrated, detailCmd ) =
            hydrate admitted

        key = navigationBranchKey parentKind maybeParentId
        ready =
            Dict.get key hydrated.dataLoading.loadedNavigationBranches
                |> Maybe.map (\request -> not request.inFlight && (request.projectRequestPending || request.taskRequestPending) && navigationBranchExpanded key hydrated)
                |> Maybe.withDefault False
        ( resumed, resumeCommand ) =
            pumpNavigation (if ready then enqueueNavigation key hydrated else hydrated)
    in
    ( resumed, Cmd.batch [ transportCmd, branchCmd, admissionCmd, detailCmd, resumeCommand ] )


ensureAllNavigationPresentations : Model -> ( Model, Cmd Msg )
ensureAllNavigationPresentations model =
    let
        projection = navigationSummaryProjection model
        ensureBranch key ( current, accumulatedCommands ) =
            case String.split ":" key of
                parentKind :: parentId :: [] ->
                    let
                        ( updated, command ) =
                            ensureNavigationPresentationWith projection (ensurePresentedCardDetailsWith projection) parentKind (Just parentId) current
                    in
                    ( updated, command :: accumulatedCommands )

                _ ->
                    ( current, accumulatedCommands )

        ( rootModel, rootCommand ) =
            ensureNavigationPresentationWith projection (ensurePresentedCardDetailsWith projection) "workspace_root" Nothing model

        ( finalModel, commands ) =
            model.dataLoading.loadedNavigationBranches
                |> Dict.keys
                |> List.filter ((/=) (navigationBranchKey "workspace_root" Nothing))
                |> List.foldl ensureBranch ( rootModel, [ rootCommand ] )
    in
    ( finalModel, Cmd.batch commands )


pauseNavigationBranch : String -> Model -> ( Model, Cmd Msg )
pauseNavigationBranch key model =
    let
        loading = model.dataLoading
        previous = Dict.get key loading.loadedNavigationBranches
        pass = Dict.get key loading.navigationPasses |> Maybe.withDefault (emptyNavigationPass False)
        mark request =
            { request | inFlight = False, succeeded = False, projectRequestPending = False, taskRequestPending = False }
        error = Just "Navigation request failed; retry or reopen this branch"
        failedPass =
            { pass
                | projectError = if previous |> Maybe.map .projectRequestPending |> Maybe.withDefault False then error else pass.projectError
                , taskError = if previous |> Maybe.map .taskRequestPending |> Maybe.withDefault False then error else pass.taskError
            }
    in
    addToast Error "Failed to load workspace branch; retry or reopen to continue"
        { model | dataLoading = { loading | loadedNavigationBranches = Dict.update key (Maybe.map mark) loading.loadedNavigationBranches, navigationPasses = Dict.insert key failedPass loading.navigationPasses, navigationQueue = List.filter ((/=) key) loading.navigationQueue } }


{-| Stage authority when each transport page arrives. A later membership commit
uses those per-entity accepted values, never the last page's read stamp for all
earlier pages in the pass.
-}
recordNavigationPage : NavigationBranchState -> Api.NavigationBranchResponse -> Model -> Model
recordNavigationPage request navigation model =
    Helpers.recordPlanningSummaries request.readRevision
        ((if request.projectRequestPending then List.map (\summary -> ( "project:" ++ summary.id, Just (Api.NavigationProjectSummary summary) )) navigation.projects.items else [])
            ++ (if request.taskRequestPending then List.map (\summary -> ( "task:" ++ summary.id, Just (Api.NavigationTaskSummary summary) )) navigation.tasks.items else [])) model


acceptNavigationPage : String -> Api.NavigationBranchResponse -> Model -> ( Model, Cmd Msg )
acceptNavigationPage key navigation model =
    case ( String.split ":" key, Dict.get key model.dataLoading.loadedNavigationBranches ) of
        ( [ kind, id ], Just request ) ->
            let
                loading = model.dataLoading
                pass = Dict.get key loading.navigationPasses |> Maybe.withDefault (emptyNavigationPass False)
                authoritative = recordNavigationPage request navigation model
                projects = if request.projectRequestPending then Dict.union (indexBy .id navigation.projects.items) pass.projects else pass.projects
                tasks = if request.taskRequestPending then Dict.union (indexBy .id navigation.tasks.items) pass.tasks else pass.tasks

                pageError pending hasMore offset oldCount newCount =
                    if not pending || not hasMore then Nothing
                    else if newCount <= oldCount then Just "Navigation returned no new IDs; branch is incomplete"
                    else if offset >= maxWorkspacePageOffset then Just "Navigation reached the client offset ceiling; branch is incomplete"
                    else Nothing

                projectError =
                    if request.projectRequestPending then pageError True navigation.projects.hasMore request.projectOffset (Dict.size pass.projects) (Dict.size projects) else pass.projectError
                taskError =
                    if request.taskRequestPending then pageError True navigation.tasks.hasMore request.taskOffset (Dict.size pass.tasks) (Dict.size tasks) else pass.taskError

                projectMore = if request.projectRequestPending then navigation.projects.hasMore else request.projectHasMore
                taskMore = if request.taskRequestPending then navigation.tasks.hasMore else request.taskHasMore
                projectPending = request.projectRequestPending && projectMore && projectError == Nothing
                taskPending = request.taskRequestPending && taskMore && taskError == Nothing

                commitProjects source =
                    let
                        sourceLoading = source.dataLoading
                        keep summary = not (projectSummaryBelongsTo kind (Just id) summary)
                        retained = Dict.filter (\_ summary -> keep summary) sourceLoading.projectCardSummaries
                    in
                    mergeNavigationSummariesAt 0 (Dict.values projects) [] { source | dataLoading = { sourceLoading | projectCardSummaries = retained, navigationVisibleProjectIds = Set.filter (\projectId -> Dict.member projectId retained) sourceLoading.navigationVisibleProjectIds } }

                commitTasks source =
                    let
                        sourceLoading = source.dataLoading
                        keep summary = not (taskSummaryBelongsTo kind (Just id) summary)
                        retained = Dict.filter (\_ summary -> keep summary) sourceLoading.taskCardSummaries
                    in
                    mergeNavigationSummariesAt 0 [] (Dict.values tasks) { source | dataLoading = { sourceLoading | taskCardSummaries = retained, navigationVisibleTaskIds = Set.filter (\taskId -> Dict.member taskId retained) sourceLoading.navigationVisibleTaskIds } }

                withProjects =
                    if not request.projectRequestPending || (projectError /= Nothing && Dict.size projects == Dict.size pass.projects) then authoritative
                    else if pass.refreshing then
                        if projectMore || projectError /= Nothing then authoritative else commitProjects authoritative
                    else if request.projectOffset == 0 then commitProjects authoritative
                    else mergeNavigationSummariesAt request.readRevision navigation.projects.items [] authoritative

                withTasks =
                    if not request.taskRequestPending || (taskError /= Nothing && Dict.size tasks == Dict.size pass.tasks) then withProjects
                    else if pass.refreshing then
                        if taskMore || taskError /= Nothing then withProjects else commitTasks withProjects
                    else if request.taskOffset == 0 then commitTasks withProjects
                    else mergeNavigationSummariesAt request.readRevision [] navigation.tasks.items withProjects

                nextLoading = withTasks.dataLoading
                nextRequest =
                    { request | inFlight = False, succeeded = projectError == Nothing && taskError == Nothing
                        , projectHasMore = projectMore, taskHasMore = taskMore
                        , projectOffset = request.projectOffset + (if projectPending then transportPageSize else 0)
                        , taskOffset = request.taskOffset + (if taskPending then transportPageSize else 0)
                        , projectRequestPending = projectPending, taskRequestPending = taskPending
                        , projectCardCount = if pass.refreshing && projectMore then request.projectCardCount else if request.projectRequestPending then Dict.size projects else request.projectCardCount
                        , taskCardCount = if pass.refreshing && taskMore then request.taskCardCount else if request.taskRequestPending then Dict.size tasks else request.taskCardCount
                    }
                accepted =
                    { withTasks | dataLoading = { nextLoading | loadedNavigationBranches = Dict.insert key nextRequest nextLoading.loadedNavigationBranches, navigationPasses = Dict.insert key { pass | projects = projects, tasks = tasks, projectError = projectError, taskError = taskError } nextLoading.navigationPasses } }
                queued = if projectPending || taskPending then enqueueNavigation key accepted else accepted
            in
            case List.head (List.filterMap identity [ projectError, taskError ]) of
                Just error -> addToast Error error queued
                Nothing -> ( queued, Cmd.none )
        _ -> ( model, Cmd.none )


{-| A live root refresh reconciles only pages already demanded by the user.
Each kind keeps its displayed membership until its own staged span is complete.
-}
acceptRootRefresh : Maybe Int -> Api.NavigationBranchResponse -> Model -> ( Model, Cmd Msg )
acceptRootRefresh maybeToken navigation model =
    case model.dataLoading.rootNavigationRequest of
        Nothing -> ( model, Cmd.none )
        Just request ->
            let
                key = navigationBranchKey "workspace_root" Nothing
                loading = model.dataLoading
                pass = Dict.get key loading.navigationPasses |> Maybe.withDefault (emptyNavigationPass True)
                authoritative = recordNavigationPage request navigation model
                demand = pass.rootDemand |> Maybe.withDefault ( request.projectCardCount, request.taskCardCount )
                projects = if request.projectRequestPending then Dict.union (indexBy .id navigation.projects.items) pass.projects else pass.projects
                tasks = if request.taskRequestPending then Dict.union (indexBy .id navigation.tasks.items) pass.tasks else pass.tasks
                projectMore = if request.projectRequestPending then navigation.projects.hasMore else request.projectHasMore
                taskMore = if request.taskRequestPending then navigation.tasks.hasMore else request.taskHasMore
                pageError pending more offset oldCount newCount =
                    if not pending || not more then Nothing
                    else if newCount <= oldCount then Just "Navigation returned no new IDs; roots are incomplete"
                    else if offset >= maxWorkspacePageOffset then Just "Navigation reached the client offset ceiling; roots are incomplete"
                    else Nothing
                projectError = if request.projectRequestPending then pageError True projectMore request.projectOffset (Dict.size pass.projects) (Dict.size projects) else pass.projectError
                taskError = if request.taskRequestPending then pageError True taskMore request.taskOffset (Dict.size pass.tasks) (Dict.size tasks) else pass.taskError
                projectContinue = request.projectRequestPending && projectMore && request.projectOffset + List.length navigation.projects.items < Tuple.first demand
                taskContinue = request.taskRequestPending && taskMore && request.taskOffset + List.length navigation.tasks.items < Tuple.second demand
                projectComplete = request.projectRequestPending && not projectContinue && projectError == Nothing
                taskComplete = request.taskRequestPending && not taskContinue && taskError == Nothing
                commitProjects source =
                    let
                        current = source.dataLoading
                        retained = Dict.filter (\_ summary -> not (projectSummaryBelongsTo "workspace_root" Nothing summary)) current.projectCardSummaries
                    in
                    mergeNavigationSummariesAt 0 (Dict.values projects) [] { source | dataLoading = { current | projectCardSummaries = retained, navigationVisibleProjectIds = Set.filter (\id -> Dict.member id retained) current.navigationVisibleProjectIds } }
                commitTasks source =
                    let
                        current = source.dataLoading
                        retained = Dict.filter (\_ summary -> not (taskSummaryBelongsTo "workspace_root" Nothing summary)) current.taskCardSummaries
                    in
                    mergeNavigationSummariesAt 0 [] (Dict.values tasks) { source | dataLoading = { current | taskCardSummaries = retained, navigationVisibleTaskIds = Set.filter (\id -> Dict.member id retained) current.navigationVisibleTaskIds } }
                committed = authoritative |> (if projectComplete then commitProjects else identity) |> (if taskComplete then commitTasks else identity)
                nextLoading = committed.dataLoading
                pendingProject = projectContinue && projectError == Nothing
                pendingTask = taskContinue && taskError == Nothing
                dispatch = pendingProject || pendingTask
                nextRequest =
                    { request | readRevision = 0, inFlight = dispatch, succeeded = not pendingProject && not pendingTask && projectError == Nothing && taskError == Nothing
                        , projectOffset = request.projectOffset + (if projectContinue && projectError == Nothing then List.length navigation.projects.items else 0)
                        , taskOffset = request.taskOffset + (if taskContinue && taskError == Nothing then List.length navigation.tasks.items else 0)
                        , projectHasMore = projectMore, taskHasMore = taskMore
                        , projectRequestPending = pendingProject, taskRequestPending = pendingTask
                        , projectCardCount = if projectComplete then Dict.size projects else request.projectCardCount
                        , taskCardCount = if taskComplete then Dict.size tasks else request.taskCardCount
                    }
                nextPass = { pass | projects = projects, tasks = tasks, projectError = projectError, taskError = taskError }
                accepted =
                    { committed | dataLoading = { nextLoading
                        | rootNavigationRequest = Just nextRequest
                        , loadedNavigationBranches = Dict.insert key nextRequest nextLoading.loadedNavigationBranches
                        , navigationPasses = if pendingProject || pendingTask || projectError /= Nothing || taskError /= Nothing then Dict.insert key nextPass nextLoading.navigationPasses else Dict.remove key nextLoading.navigationPasses
                        } }
                settled = if dispatch then recordRootRecovery nextRequest accepted else { accepted | dataLoading = finishWorkspaceLoad maybeToken accepted.dataLoading }
                ( presented, presentationCommand ) = ensureNavigationPresentation "workspace_root" Nothing settled
                command =
                    if dispatch then
                        Api.fetchNavigationBranchWithDemand model.flags.apiUrl request.workspaceId "workspace_root" Nothing nextRequest.projectOffset nextRequest.taskOffset nextRequest.projectRequestPending nextRequest.taskRequestPending (navigationFilterQuery model)
                            (GotRootNavigation request.workspaceId request.sessionEpoch maybeToken request.generation request.filterFingerprint nextRequest.projectOffset nextRequest.taskOffset)
                    else Cmd.none
                ( notified, errorCommand ) =
                    case List.head (List.filterMap identity [ projectError, taskError ]) of
                        Just error -> addToast Error error presented
                        Nothing -> ( presented, Cmd.none )
            in
            ( notified, Cmd.batch [ presentationCommand, command, errorCommand ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( updated, responseCommand ) =
            updateResponse msg (releaseNavigationAdmission msg model)

        detailsOnly =
            case msg of
                GotProjectCardDetail _ _ _ -> True
                GotTaskCardDetail _ _ _ -> True
                RetryCardDetail _ _ -> True
                _ -> False

        ( expanded, branchCommand ) =
            if detailsOnly then ( updated, Cmd.none ) else ensureExpandedBranches (retireHiddenNavigation updated)

        ( pumped, navigationCommand ) =
            if detailsOnly then ( expanded, Cmd.none ) else pumpNavigation expanded

        ( hydrated, detailCommand ) =
            ensurePresentedCardDetails pumped
    in
    ( yieldBackgroundAdmission hydrated, Cmd.batch [ responseCommand, branchCommand, navigationCommand, detailCommand ] )


updateResponse : Msg -> Model -> ( Model, Cmd Msg )
updateResponse msg model =
    case msg of
        GotNavigationBatch requests result ->
            let
                checked =
                    case List.head requests of
                        Nothing -> Err (Http.BadBody "Navigation batch has no captured owners")
                        Just ( _, first ) -> result |> Result.andThen (Api.validateNavigationBatchResponse first.workspaceId (List.map navigationBatchItem requests))
                ownerResults =
                    case checked of
                        Ok response -> List.map2 (\owner outcome -> ( owner, outcome.result )) requests response.outcomes
                        Err error -> List.map (\owner -> ( owner, Err error )) requests
                apply ( ( key, request ), outcome ) ( current, accumulated ) =
                    if (Dict.get key current.dataLoading.loadedNavigationBranches |> Maybe.map .readRevision) /= Just request.readRevision then
                        ( current, accumulated )
                    else
                        let
                            ( next, command ) = updateResponse (GotNavigationBranch request.workspaceId request.sessionEpoch request.generation key request.filterFingerprint request.projectOffset request.taskOffset outcome) current
                        in
                        ( next, command :: accumulated )
                ( reduced, commands ) = List.foldl apply ( model, [] ) ownerResults
            in
            ( reduced, Cmd.batch commands )

        GotWorkspacePlanningCounts guard result ->
            acceptPlanningCounts guard result model

        RetryWorkspacePlanningCounts ->
            syncPlanningCounts (invalidatePlanningCounts model)

        LoadRootNavigationPage entityKind ->
            beginRootNavigationPage entityKind model

        ShowPreviousRootNavigationPage entityKind ->
            beginRootNavigationPreviousPage entityKind model

        GotWorkspaces token result ->
            if (model.auth.status /= AuthReady && (model.auth.status /= AuthBooting || model.groups.catalogueOwner == Nothing)) || model.dataLoading.activeWorkspaceListLoadToken /= Just token then
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
                            | workspaces = indexBy .id (List.filter (\workspace -> not (Set.member workspace.id model.groups.deletedWorkspaces)) paginated.items)
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

                                rootContextRetained =
                                    rootNavigationContextMatches expectedWsId model

                                rootModel =
                                    if rootContextRetained then
                                        model

                                    else
                                        prepareRootNavigationRequest (Just expectedWsId)
                                            { model | dataLoading = { currentLoading | navigationGeneration = currentLoading.navigationGeneration + 1 } }

                                rootLoading =
                                    rootModel.dataLoading

                                initialRootPending =
                                    rootLoading.rootNavigationRequest
                                        |> Maybe.map (\request -> request.inFlight && request.projectOffset == 0 && request.taskOffset == 0 && request.projectRequestPending && request.taskRequestPending)
                                        |> Maybe.withDefault False

                                initialObservationPending =
                                    isRepository && Feature.Observation.filterValidationError model.observations == Nothing
                                        && List.member model.observations.requestMode [ ObservationFlatMode, ObservationExactSubjectMode ]

                                pendingLoads =
                                    -- Demand pages are independent of the initial workspace load.
                                    (if initialRootPending then 1 else 0) + (if initialObservationPending then 1 else 0)

                                dataLoading =
                                    { rootLoading
                                        | loadingWorkspaceData = pendingLoads > 0
                                        , pendingWorkspaceLoads = pendingLoads
                                        , activeWorkspaceLoadToken = if pendingLoads == 0 then Nothing else Just token
                                        , cardHydrationLoaded = pendingLoads == 0
                                    }

                                currentObservations =
                                    model.observations

                                observations =
                                    if isRepository then
                                        currentObservations

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
                                    if rootContextRetained then Cmd.none else beginRootNavigation rootModel

                                commands =
                                    [ rootCommand ]
                            in
                            let
                                loadedModel =
                                    { model
                                        | workspaces = Dict.insert workspace.id workspace model.workspaces
                                        , dataLoading = dataLoading
                                        , observations = observations
                                    }

                                ( startedObservationModel, observationCmd ) =
                                    if isRepository then Feature.Observation.bootstrapObservation token loadedModel else ( loadedModel, Cmd.none )

                                observationModel =
                                    let
                                        loading = startedObservationModel.dataLoading
                                        state = startedObservationModel.observations
                                    in
                                    { startedObservationModel | dataLoading = { loading | initialObservationLoad =
                                        if initialObservationPending then
                                            Just { workspaceId = expectedWsId, sessionEpoch = model.sessionRequestEpoch, token = token, generation = state.requestGeneration, fingerprint = state.queryFingerprint }
                                        else
                                            Nothing
                                        } }

                                ( focusedModel, focusCmd ) =
                                    case model.focus.focusedEntity of
                                        Just ( entityType, entityId ) ->
                                            beginNavigationFocus expectedWsId entityType entityId observationModel

                                        Nothing ->
                                            ( observationModel, Cmd.none )

                                ( detailModel, detailCmd ) =
                                    if isRepository then
                                        case observations.selectedId of
                                            Just observationId ->
                                                Feature.Observation.selectObservation observationId focusedModel

                                            Nothing ->
                                                ( focusedModel, Cmd.none )

                                    else
                                        ( focusedModel, Cmd.none )
                            in
                            if detailModel.observations.linkNotice /= Nothing && detailModel.observations.linkNotice /= Just Helpers.excludedObservationNotice then
                                let
                                    ( repaired, repairCmd ) = Helpers.writeObservationHistory False detailModel
                                in
                                ( repaired, Cmd.batch (repairCmd :: observationCmd :: detailCmd :: focusCmd :: commands) )
                            else
                                ( detailModel, Cmd.batch (observationCmd :: detailCmd :: focusCmd :: commands) )

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
                    Ok received ->
                        let
                            currentRequest = model.dataLoading.rootNavigationRequest
                            projectPending = currentRequest |> Maybe.map .projectRequestPending |> Maybe.withDefault False
                            taskPending = currentRequest |> Maybe.map .taskRequestPending |> Maybe.withDefault False
                            navigation =
                                { received
                                    | projects = if projectPending then received.projects else { items = [], hasMore = currentRequest |> Maybe.map .projectHasMore |> Maybe.withDefault False }
                                    , tasks = if taskPending then received.tasks else { items = [], hasMore = currentRequest |> Maybe.map .taskHasMore |> Maybe.withDefault False }
                                }
                        in
                        if navigation.workspaceId /= wsId then
                            ( model, Cmd.none )

                        else if Dict.get (navigationBranchKey "workspace_root" Nothing) model.dataLoading.navigationPasses |> Maybe.map .refreshing |> Maybe.withDefault False then
                            acceptRootRefresh maybeToken navigation model

                        else
                            let
                                updatedModel =
                                    (if projectOffset == 0 && taskOffset == 0 then
                                        replaceNavigationBranchMembership "workspace_root" Nothing navigation model

                                     else
                                        mergeNavigationSummariesAt (model.dataLoading.rootNavigationRequest |> Maybe.map .readRevision |> Maybe.withDefault 0) navigation.projects.items navigation.tasks.items model
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
                                                                    , readRevision = model.dataLoading.rootNavigationRequest |> Maybe.map .readRevision |> Maybe.withDefault 0
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
                                                                        if projectOffset == 0 && taskOffset == 0 && projectPending then
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
                                                                        if projectOffset == 0 && taskOffset == 0 && taskPending then
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
                                                                                    if projectOffset == 0 && taskOffset == 0 && projectPending then
                                                                                        List.length navigation.projects.items

                                                                                    else
                                                                                        if request.projectRequestPending then
                                                                                            request.projectCardCount + List.length navigation.projects.items

                                                                                        else
                                                                                            request.projectCardCount
                                                                                , taskCardCount =
                                                                                    if projectOffset == 0 && taskOffset == 0 && taskPending then
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
                                && navigationBranchExpanded branchKey model

                        Nothing ->
                            False
            in
            if not valid then
                ( model, Cmd.none )

            else
                case result of
                    Ok navigation ->
                        if navigation.workspaceId /= wsId then
                            pauseNavigationBranch branchKey model

                        else
                            acceptNavigationPage branchKey navigation model

                    Err _ ->
                        pauseNavigationBranch branchKey model

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
                                && navigationFilterFingerprint model == fingerprint
                                && model.focus.focusedEntity == Just ( entityType, entityId )
                                && request.entityType == entityType
                                && request.entityId == entityId
                                && request.ancestorOffset == ancestorOffset
                                && request.inFlight

                        Nothing ->
                            False

                mergeSummary summary currentModel =
                    case summary of
                        Api.NavigationProjectSummary project ->
                            mergeNavigationSummariesAt (model.dataLoading.activeNavigationFocus |> Maybe.map .summaryRevision |> Maybe.withDefault 0) [ project ] [] currentModel

                        Api.NavigationTaskSummary task ->
                            mergeNavigationSummariesAt (model.dataLoading.activeNavigationFocus |> Maybe.map .summaryRevision |> Maybe.withDefault 0) [] [ task ] currentModel
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

                        else if model.dataLoading.activeNavigationFocus
                            |> Maybe.map
                                (\request ->
                                    List.any
                                        (\summary ->
                                            let key = case summary of
                                                    Api.NavigationProjectSummary project -> "project:" ++ project.id
                                                    Api.NavigationTaskSummary task -> "task:" ++ task.id
                                            in
                                            Dict.get key model.dataLoading.navigationSummaryAuthority
                                                |> Maybe.map (\authority -> authority.readRevision > request.summaryRevision && authority.summary /= Just summary)
                                                |> Maybe.withDefault False
                                        )
                                        (focus.target :: focus.ancestors)
                                )
                            |> Maybe.withDefault False then
                            -- A newer target/ancestor was accepted while this page
                            -- was in flight. Keep demand, but read under that authority.
                            revalidateNavigationFocus wsId entityType entityId model

                        else
                            let
                                merged =
                                    List.foldl mergeSummary (mergeSummary focus.target model) focus.ancestors

                                mergedLoading =
                                    let
                                        loading =
                                            merged.dataLoading
                                    in
                                    -- Focus summaries are unfiltered metadata for the
                                    -- target and breadcrumbs. Only filtered navigation
                                    -- pages may admit ordinary hierarchy members.
                                    { loading
                                        | navigationVisibleProjectIds = model.dataLoading.navigationVisibleProjectIds
                                        , navigationVisibleTaskIds = model.dataLoading.navigationVisibleTaskIds
                                    }

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
                                                , summaryRevision = mergedLoading.nextNavigationSummaryRevision + 1
                                                , inFlight = True
                                                , succeeded = False
                                                }

                                            continuedLoading =
                                                { mergedLoading
                                                    | activeNavigationFocus = Just continuation
                                                    , nextNavigationSummaryRevision = continuation.summaryRevision
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
            let
                loading = model.dataLoading
                retireFailed maybeRequest =
                    maybeRequest |> Maybe.andThen (\request -> if request.inFlight || request.succeeded then Just request else Nothing)
                demand =
                    { model | dataLoading = { loading
                        | projectCardDetailRequests = if entityType == "project" then Dict.update entityId retireFailed loading.projectCardDetailRequests else loading.projectCardDetailRequests
                        , taskCardDetailRequests = if entityType == "task" then Dict.update entityId retireFailed loading.taskCardDetailRequests else loading.taskCardDetailRequests
                        , cardDetailRetries = Set.insert ( entityType, entityId ) loading.cardDetailRetries
                    } }
            in
            if entityType == "project" || entityType == "task" then ensurePresentedCardDetails demand else ( model, Cmd.none )

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
                                    , taskDependencyLinks = mergeTaskDependencyLinks taskId overview.dependencies dependencies.taskDependencyLinks
                                }

                            Err _ ->
                                dependencies
                in
                -- These retired bootstrap callbacks have no current dispatch site.
                -- Treat their readiness as older than every admitted planning read.
                ( { model | dependencies = updatedDependencies, dataLoading = completedLoading }
                    |> (\completed -> case result of
                        Ok overview -> Helpers.applyTaskReadiness 0 taskId overview.readinessRollup completed
                        Err _ -> completed)
                , Cmd.none )

        GotInitialProjectOverview wsId token projectId result ->
            if model.selectedWorkspaceId /= Just wsId || not (acceptWorkspaceLoad (Just token) model.dataLoading) || not (Permissions.canReadCurrentWorkspace model) || not (Dict.member projectId model.projects) then
                ( model, Cmd.none )

            else
                let
                    completedLoading =
                        finishWorkspaceLoad (Just token) model.dataLoading

                in
                ( { model | dataLoading = completedLoading }
                    |> (\completed -> case result of
                        Ok overview -> Helpers.applyProjectReadiness 0 projectId overview.readinessRollup completed
                        Err _ -> completed)
                , Cmd.none )

        GotObservations wsId maybeToken generation fingerprint offset result ->
            if model.selectedWorkspaceId /= Just wsId || model.observations.requestSessionEpoch /= model.sessionRequestEpoch || not (acceptWorkspaceLoad maybeToken model.dataLoading) || not (listObservationResponseMatches generation fingerprint offset model.observations) then
                ( settleInitialObservationResponse wsId maybeToken generation fingerprint offset model, Cmd.none )

            else if model.observations.refreshPending && model.observations.refreshPass == Nothing then
                Feature.Observation.completeSupersededRead { model | dataLoading = completeObservationWorkspaceLoad wsId maybeToken generation fingerprint offset model }
            else
                case result of
                    Ok paginated ->
                        if model.observations.refreshPass /= Nothing then
                            Feature.Observation.acceptResultRefresh offset paginated model
                        else if not (Feature.Observation.validPage offset paginated.hasMore (List.map .id paginated.items) model.observations.orderedIds) then
                            ( { model | observations = Feature.Observation.failResultPage wsId offset "Observations are incomplete: the page made no valid progress." model.observations, dataLoading = completeObservationWorkspaceLoad wsId maybeToken generation fingerprint offset model }, Cmd.none )
                        else
                        let
                            currentObservations =
                                model.observations

                            observations =
                                mergeObservationPage offset paginated currentObservations

                            updatedDataLoading =
                                completeObservationWorkspaceLoad wsId maybeToken generation fingerprint offset model
                        in
                        ( { model | observations = observations, dataLoading = updatedDataLoading }, Cmd.none )

                    Err _ ->
                        let
                            currentObservations =
                                model.observations

                            observations =
                                Feature.Observation.failResultPage wsId offset "Failed to load observations." currentObservations

                            updatedDataLoading =
                                completeObservationWorkspaceLoad wsId maybeToken generation fingerprint offset model
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
        , completedQuery = Just (Helpers.observationAppliedQuery observations)
        , orderedIds = orderedIds
        , hasMore = paginated.hasMore
        , loading = False
        , error = Nothing
        , expectedOffset = Nothing
        , nextOffset = offset + List.length paginated.items
        , resultsStale = if offset == 0 then False else observations.resultsStale
        , failedRequest = Nothing
    }
        |> (\merged -> List.foldl Feature.Observation.applyAuthoritativeObservation merged paginated.items)


{-| The initial Observation page is one workspace-load obligation, separate
from later untagged pages. Route supersession retires that exact obligation;
a late response may settle it, but can never contribute records or errors.
-}
retireInitialObservationLoad : Model -> Model
retireInitialObservationLoad model =
    case model.dataLoading.initialObservationLoad of
        Just request ->
            if model.selectedWorkspaceId == Just request.workspaceId && model.sessionRequestEpoch == request.sessionEpoch then
                let
                    loading = finishWorkspaceLoad (Just request.token) model.dataLoading
                in
                { model | dataLoading = { loading | initialObservationLoad = Nothing } }
            else
                model
        Nothing ->
            model


completeObservationWorkspaceLoad : String -> Maybe Int -> Int -> String -> Int -> Model -> DataLoadingModel
completeObservationWorkspaceLoad workspaceId maybeToken generation fingerprint offset model =
    let
        loading =
            finishWorkspaceLoad maybeToken model.dataLoading

        initial =
            model.dataLoading.initialObservationLoad
                |> Maybe.andThen (\request ->
                    if request.workspaceId == workspaceId && request.sessionEpoch == model.sessionRequestEpoch && maybeToken == Just request.token && request.generation == generation && request.fingerprint == fingerprint && offset == 0 then
                        Nothing
                    else
                        Just request
                    )
    in
    { loading | initialObservationLoad = initial }


settleInitialObservationResponse : String -> Maybe Int -> Int -> String -> Int -> Model -> Model
settleInitialObservationResponse workspaceId maybeToken generation fingerprint offset model =
    case model.dataLoading.initialObservationLoad of
        Just request ->
            if request.workspaceId == workspaceId && maybeToken == Just request.token && request.generation == generation && request.fingerprint == fingerprint && offset == 0 then
                retireInitialObservationLoad model
            else
                model
        Nothing ->
            model
