module Feature.WebSocket exposing
    ( connectCmd
    , init
    , subscriptions
    , update
    )

import Api
import Dict
import Feature.ChangeStream as ChangeStream
import Feature.DataLoading
import Feature.Dependencies as Dependencies
import Feature.Observation as Observation
import Feature.Timeline as Timeline
import Helpers exposing (applyDependencyMutationResult, applyTaskDependencyLinkMutation, beginWorkspaceDataReload, replaceFragment)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Permissions
import Ports exposing (clearChangeStreamScope, connectWebSocket, disconnectChangeStreamScope, wsConnected, wsConnecting, wsConnectionFailed, wsDisconnected, wsMessage)
import Set
import String
import Task
import Toast exposing (addToast)
import Types exposing (..)


init : WebSocketModel
init =
    { state = Disconnected, streams = Dict.empty, targetGenerations = Dict.empty }


connectCmd : Flags -> Maybe Api.SessionContext -> ChangeStream.Scope -> Bool -> Cmd Msg
connectCmd flags sessionContext scope forceResync =
    case sessionContext of
        Nothing ->
            Cmd.none

        Just session ->
            connectWebSocket
                (Encode.object
                    (( "audienceId", Encode.string session.principal.actorId )
                        :: (case scope of
                                ChangeStream.Workspace workspaceId ->
                                    [ ( "scope", Encode.string "workspace" )
                                    , ( "workspaceId", Encode.string workspaceId )
                                    , ( "forceResync", Encode.bool forceResync )
                                    ]

                                ChangeStream.Global ->
                                    [ ( "scope", Encode.string "global" )
                                    , ( "forceResync", Encode.bool forceResync )
                                    ]
                           )
                    )
                )


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ wsConnected (\_ -> WsConnectedMsg)
        , wsConnecting (\_ -> WsConnectingMsg)
        , wsDisconnected (\_ -> WsDisconnectedMsg)
        , wsConnectionFailed WsConnectionFailed
        , wsMessage WsMessageReceived
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WsConnectingMsg ->
            if model.auth.status == AuthReady then
                ( setWebSocketState Connecting model, Cmd.none )

            else
                ( setWebSocketState Disconnected model, Cmd.none )

        WsConnectedMsg ->
            if model.auth.status == AuthReady then
                -- A transport open is only replaying.  The canonical terminal
                -- checkpoint below is the sole transition to live/Connected.
                ( setWebSocketState Connecting model, Cmd.none )

            else
                ( setWebSocketState Disconnected model, Cmd.none )

        WsDisconnectedMsg ->
            if model.auth.status == AuthReady then
                case model.webSocket.state of
                    Connecting ->
                        ( model, Cmd.none )

                    _ ->
                        addToast Warning
                            "Connection lost. Reconnecting..."
                            (setWebSocketState Connecting model)

            else
                ( setWebSocketState Disconnected model, Cmd.none )

        WsConnectionFailed reason ->
            if model.auth.status == AuthReady then
                addToast Warning
                    "Canonical change stream is reconnecting or requires scoped resync"
                    (setWebSocketState (ConnectionFailed reason) model)

            else
                ( setWebSocketState Disconnected model, Cmd.none )

        WsMessageReceived raw ->
            if model.auth.status == AuthReady then
                applyWebSocketChange raw model

            else
                ( model, Cmd.none )

        CanonicalWorkspaceFetched guard workspaceId result ->
            if canonicalGuardIsCurrent guard model then
                case result of
                    Ok workspace ->
                        ( { model | workspaces = Dict.insert workspace.id workspace model.workspaces }, Cmd.none )

                    Err (Http.BadStatus 404) ->
                        ( { model | workspaces = Dict.remove workspaceId model.workspaces }, Cmd.none )

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalProjectFetched guard projectId result ->
            if canonicalGuardIsCurrent guard model then
                case result of
                    Ok project ->
                        ( { model | projects = Dict.insert project.id project model.projects }, Cmd.none )

                    Err (Http.BadStatus 404) ->
                        ( { model | projects = Dict.remove projectId model.projects }, Cmd.none )

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalTaskFetched guard taskId result ->
            if canonicalGuardIsCurrent guard model then
                case result of
                    Ok task ->
                        if task.id == taskId && Just task.workspaceId == model.selectedWorkspaceId then
                            ( { model | tasks = Dict.insert task.id task model.tasks }, Cmd.none )

                        else
                            canonicalHttpFailure guard (Http.BadBody "Task identity did not match the canonical request") model

                    Err (Http.BadStatus 404) ->
                        ( { model | tasks = Dict.remove taskId model.tasks }, Cmd.none )

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalObservationFetched guard workspaceId observationId result ->
            if model.selectedWorkspaceId == Just workspaceId && canonicalGuardIsCurrent guard model then
                case result of
                    Ok observation ->
                        if observation.id == observationId && observation.workspaceId == workspaceId then
                            ( { model | observations = Observation.applyCanonicalObservation observation model.observations }, Cmd.none )

                        else
                            canonicalHttpFailure guard (Http.BadBody "Observation identity did not match the canonical request") model

                    Err (Http.BadStatus 404) ->
                        Observation.reconcileDeletedObservation observationId model

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalTaskOverviewFetched guard taskId result ->
            if canonicalGuardIsCurrent guard model then
                case result of
                    Ok overview ->
                        let
                            dependencies =
                                model.dependencies

                            links =
                                overview.dependencies
                                    |> List.map (\dependency -> Api.WorkspaceTaskDependencyLink taskId dependency.id)

                            retained =
                                List.filter (\link -> link.taskId /= taskId) dependencies.taskDependencyLinks

                            completeDependencies =
                                Dependencies.cacheCompleteTaskDependencies taskId overview.dependencies dependencies
                        in
                        ( { model
                            | dependencies =
                                { completeDependencies
                                    | taskReadinessRollups = Dict.insert taskId overview.readinessRollup completeDependencies.taskReadinessRollups
                                    , taskDependencyLinks = links ++ retained
                                }
                          }
                        , Cmd.none
                        )

                    Err (Http.BadStatus 404) ->
                        let
                            invalidated =
                                Dependencies.invalidateDependencyPage taskId model

                            dependencies =
                                invalidated.dependencies
                        in
                        ( { invalidated
                            | dependencies =
                                { dependencies
                                    | taskReadinessRollups = Dict.remove taskId dependencies.taskReadinessRollups
                                    , taskDependencyLinks = List.filter (\link -> link.taskId /= taskId && link.dependsOnId /= taskId) dependencies.taskDependencyLinks
                                }
                          }
                        , Cmd.none
                        )

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalTaskReadinessFetched guard taskId result ->
            if canonicalGuardIsCurrent guard model then
                case result of
                    Ok overview ->
                        if overview.task.id == taskId && Just overview.task.workspaceId == model.selectedWorkspaceId then
                            let
                                dependencies =
                                    model.dependencies
                            in
                            ( { model | dependencies = { dependencies | taskReadinessRollups = Dict.insert taskId overview.readinessRollup dependencies.taskReadinessRollups } }, Cmd.none )

                        else
                            canonicalHttpFailure guard (Http.BadBody "Task overview identity did not match the readiness request") model

                    Err (Http.BadStatus 404) ->
                        let
                            dependencies =
                                model.dependencies
                        in
                        ( { model | dependencies = { dependencies | taskReadinessRollups = Dict.remove taskId dependencies.taskReadinessRollups } }, Cmd.none )

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalProjectOverviewFetched guard projectId result ->
            if canonicalGuardIsCurrent guard model then
                case result of
                    Ok overview ->
                        let
                            dependencies =
                                model.dependencies
                        in
                        ( { model | dependencies = { dependencies | projectReadinessRollups = Dict.insert projectId overview.readinessRollup dependencies.projectReadinessRollups } }, Cmd.none )

                    Err (Http.BadStatus 404) ->
                        let
                            dependencies =
                                model.dependencies
                        in
                        ( { model | dependencies = { dependencies | projectReadinessRollups = Dict.remove projectId dependencies.projectReadinessRollups } }, Cmd.none )

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalNavigationSummariesFetched guard workspaceId projectIds taskIds result ->
            if model.selectedWorkspaceId == Just workspaceId && canonicalGuardIsCurrent guard model then
                case result of
                    Ok summaries ->
                        let
                            projectIdsMatch =
                                List.all (\summary -> List.member summary.id projectIds && summary.workspaceId == workspaceId) summaries.projects

                            taskIdsMatch =
                                List.all (\summary -> List.member summary.id taskIds && summary.workspaceId == workspaceId) summaries.tasks

                            returnedProjectIds =
                                List.map .id summaries.projects ++ summaries.missingProjectIds

                            returnedTaskIds =
                                List.map .id summaries.tasks ++ summaries.missingTaskIds

                            complete =
                                List.sort returnedProjectIds
                                    == List.sort projectIds
                                    && List.sort returnedTaskIds
                                    == List.sort taskIds
                                    && projectIdsMatch
                                    && taskIdsMatch
                        in
                        if complete then
                            let
                                merged =
                                    Feature.DataLoading.mergeNavigationSummaries summaries.projects summaries.tasks model

                                dependencies =
                                    merged.dependencies

                                loading =
                                    merged.dataLoading

                                -- Without an active server-owned filter every
                                -- revalidated existing card remains eligible
                                -- for the current tree; only missing IDs leave
                                -- membership. Under a filter, summary payloads
                                -- cannot safely reproduce descendant-aware
                                -- matching, so remove invalidated IDs until a
                                -- fresh bounded branch response re-admits them.
                                filteredNavigation =
                                    model.search.filterShowOnly
                                        /= ShowAll
                                        || model.search.filterProjectStatuses
                                        /= []
                                        || model.search.filterTaskStatuses
                                        /= []
                                        || model.search.filterPriority
                                        /= AnyPriority
                                        || String.trim model.search.query
                                        /= ""

                                -- A summary endpoint is authoritative about
                                -- deletion, but not descendant-retained branch
                                -- membership under filters.  Keep existing
                                -- cards visible until an authoritative branch
                                -- replay can decide their filtered placement.
                                staleProjectIds =
                                    summaries.missingProjectIds

                                staleTaskIds =
                                    summaries.missingTaskIds

                                mergedProjects =
                                    merged.projects

                                mergedTasks =
                                    merged.tasks

                                updated =
                                    { merged
                                        | projects = List.foldl Dict.remove mergedProjects summaries.missingProjectIds
                                        , tasks = List.foldl Dict.remove mergedTasks summaries.missingTaskIds
                                        , dataLoading =
                                            { loading
                                                | projectCardSummaries = List.foldl Dict.remove loading.projectCardSummaries summaries.missingProjectIds
                                                , taskCardSummaries = List.foldl Dict.remove loading.taskCardSummaries summaries.missingTaskIds

                                                -- A summary response is authoritative for card state,
                                                -- but not for server-side filtered branch membership.
                                                -- Under a filtered tree, remove every
                                                -- invalidated card until the next bounded
                                                -- branch response re-admits it. This prevents
                                                -- stale status, priority, search, or parent
                                                -- membership from surviving a live mutation.
                                                , navigationVisibleProjectIds = List.foldl Set.remove loading.navigationVisibleProjectIds staleProjectIds
                                                , navigationVisibleTaskIds = List.foldl Set.remove loading.navigationVisibleTaskIds staleTaskIds
                                            }
                                        , dependencies =
                                            { dependencies
                                                | projectReadinessRollups = List.foldl Dict.remove dependencies.projectReadinessRollups summaries.missingProjectIds
                                                , taskReadinessRollups = List.foldl Dict.remove dependencies.taskReadinessRollups summaries.missingTaskIds
                                            }
                                    }
                            in
                            if filteredNavigation && (not (List.isEmpty projectIds) || not (List.isEmpty taskIds)) then
                                -- Summary payloads establish card state, but only the
                                -- bounded branch endpoint owns descendant-aware filtered
                                -- membership. Reissue its root page so matching cards are
                                -- re-admitted and moved/nonmatching cards disappear.
                                Feature.DataLoading.revalidateNavigationForAffectedBranches summaries.projects summaries.tasks updated

                            else
                                Feature.DataLoading.ensureAllNavigationPresentations updated

                        else
                            canonicalHttpFailure guard (Http.BadBody "Navigation summary response did not match its targeted revalidation request") model

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalCatalogueFetched guard result ->
            if canonicalGuardIsCurrent guard model then
                case result of
                    Ok paginated ->
                        let
                            workspaces =
                                List.foldl (\workspace -> Dict.insert workspace.id workspace) Dict.empty paginated.items
                        in
                        ( { model | workspaces = workspaces }, Cmd.none )

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalGroupsFetched guard result ->
            if canonicalGuardIsCurrent guard model then
                case result of
                    Ok paginated ->
                        let
                            groups =
                                List.foldl (\group -> Dict.insert group.id group) Dict.empty paginated.items

                            current =
                                model.groups

                            updated =
                                { model | groups = { current | workspaceGroups = groups, groupMembers = Dict.filter (\groupId _ -> Dict.member groupId groups) current.groupMembers } }
                        in
                        ( updated, Cmd.none )

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalGroupMembersFetched guard groupId result ->
            if canonicalGuardIsCurrent guard model then
                case result of
                    Ok members ->
                        let
                            groups =
                                model.groups
                        in
                        ( { model | groups = { groups | groupMembers = Dict.insert groupId members groups.groupMembers } }, Cmd.none )

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalMembershipsFetched guard workspaceId result ->
            if canonicalGuardIsCurrent guard model then
                case result of
                    Ok paginated ->
                        let
                            admin =
                                model.workspaceAdmin
                        in
                        ( { model | workspaceAdmin = { admin | memberships = Dict.insert workspaceId paginated.items admin.memberships, loadingMemberships = Dict.insert workspaceId False admin.loadingMemberships } }, Cmd.none )

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalSessionFetched guard expectedWorkspace result ->
            if canonicalGuardIsCurrent guard model then
                ( model, Task.perform identity (Task.succeed (GotSessionContext guard.sessionEpoch expectedWorkspace result)) )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


canonicalGuardIsCurrent : CanonicalRequestGuard -> Model -> Bool
canonicalGuardIsCurrent guard model =
    model.auth.status
        == AuthReady
        && (model.sessionContext
                |> Maybe.map
                    (\session ->
                        ChangeStream.requestGuardMatches guard
                            model.sessionRequestEpoch
                            model.selectedWorkspaceId
                            session.principal.actorId
                            model.webSocket.targetGenerations
                    )
                |> Maybe.withDefault False
           )


setWebSocketState : WSState -> Model -> Model
setWebSocketState state model =
    updateWebSocket (\webSocket -> { webSocket | state = state }) model


updateWebSocket : (WebSocketModel -> WebSocketModel) -> Model -> Model
updateWebSocket change model =
    { model | webSocket = change model.webSocket }


applyWebSocketChange : String -> Model -> ( Model, Cmd Msg )
applyWebSocketChange raw model =
    case Api.decodeCanonicalFrame raw of
        Just frame ->
            applyCanonicalFrame frame model

        Nothing ->
            -- Unknown/malformed frames are deliberately not interpreted as
            -- legacy events.  The transport reconnects with its last durable
            -- checkpoint; no handler-local event path remains reachable.
            let
                webSocket =
                    model.webSocket

                failedScope =
                    Api.decodeCanonicalTransportScope raw
                        |> Maybe.map toPolicyScope
                        |> Maybe.withDefault (currentScope model)
            in
            beginScopedResync failedScope { model | webSocket = { webSocket | state = ConnectionFailed "canonical:malformed-frame" } }


applyCanonicalFrame : Api.CanonicalFrame -> Model -> ( Model, Cmd Msg )
applyCanonicalFrame frame model =
    case frame of
        Api.CanonicalScoped apiScope nested ->
            applyScopedFrame (toPolicyScope apiScope) nested model

        Api.CanonicalSnapshot apiScope items token profile ->
            applyCanonicalSnapshot (toPolicyScope apiScope) profile items token model

        Api.CanonicalBatch apiScope frames ->
            applyScopedFrames (toPolicyScope apiScope) frames model

        -- Socket frames always carry the JS-owned scope wrapper.  A bare
        -- frame cannot safely bind a checkpoint/control to an audience, so it
        -- fails closed rather than guessing from the selected route.
        _ ->
            beginScopedResync (currentScope model) model


toPolicyScope : Api.ChangeStreamScope -> ChangeStream.Scope
toPolicyScope apiScope =
    case apiScope of
        Api.WorkspaceScope workspaceId ->
            ChangeStream.Workspace workspaceId

        Api.GlobalScope ->
            ChangeStream.Global


currentScope : Model -> ChangeStream.Scope
currentScope model =
    model.selectedWorkspaceId |> Maybe.map ChangeStream.Workspace |> Maybe.withDefault ChangeStream.Global


applyScopedFrame : ChangeStream.Scope -> Api.CanonicalFrame -> Model -> ( Model, Cmd Msg )
applyScopedFrame scope frame model =
    applyScopedFrames scope [ frame ] model


applyScopedFrames : ChangeStream.Scope -> List Api.CanonicalFrame -> Model -> ( Model, Cmd Msg )
applyScopedFrames scope frames model =
    let
        key =
            ChangeStream.scopeKey scope

        prior =
            Dict.get key model.webSocket.streams |> Maybe.withDefault (ChangeStream.init scope [])

        ( next, actions ) =
            ChangeStream.reduceFrames frames prior

        streams =
            Dict.insert key next model.webSocket.streams

        connectionState =
            if next.live then
                Connected

            else
                model.webSocket.state

        updated =
            updateWebSocket (\webSocket -> { webSocket | streams = streams, state = connectionState }) model
    in
    applyActions scope actions updated


applyCanonicalSnapshot : ChangeStream.Scope -> String -> List Api.SnapshotItem -> String -> Model -> ( Model, Cmd Msg )
applyCanonicalSnapshot scope profile items token model =
    case ChangeStream.applySnapshotProfile scope profile items of
        Err _ ->
            beginScopedResync scope model

        Ok snapshot ->
            let
                withoutStale =
                    invalidateScopeRequests scope model

                key =
                    ChangeStream.scopeKey scope

                stream =
                    Dict.get key withoutStale.webSocket.streams
                        |> Maybe.withDefault (ChangeStream.init scope [])

                resumed =
                    { stream | resumeToken = Just token, live = False }

                withStream =
                    updateWebSocket (\webSocket -> { webSocket | streams = Dict.insert key resumed webSocket.streams, state = Connecting }) withoutStale
            in
            case scope of
                ChangeStream.Global ->
                    let
                        groups =
                            withStream.groups

                        loading =
                            withStream.dataLoading
                    in
                    requestGroupMembers ChangeStream.Global
                        (Dict.keys snapshot.groups)
                        { withStream
                            | workspaces = snapshot.workspaces
                            , groups = { groups | workspaceGroups = snapshot.groups, groupMembers = Dict.filter (\groupId _ -> Dict.member groupId snapshot.groups) groups.groupMembers }
                            , dataLoading = { loading | loadingWorkspaces = False, activeWorkspaceListLoadToken = Nothing }
                        }

                ChangeStream.Workspace workspaceId ->
                    if model.selectedWorkspaceId /= Just workspaceId then
                        ( model, Cmd.none )

                    else if profile == "workspace_shell_v1" then
                        -- workspace_shell_v1 carries only the durable root and
                        -- replay hand-off.  Never replace newer bounded REST
                        -- branch data with this intentionally sparse snapshot.
                        let
                            shellModel =
                                { withStream | workspaces = Dict.union snapshot.workspaces withStream.workspaces }

                            ( observationModel, observationCmd ) =
                                Observation.refreshActiveResults shellModel
                        in
                        case observationModel.focus.focusedEntity of
                            Just ( entityType, entityId ) ->
                                let
                                    ( focusedModel, focusCmd ) =
                                        Feature.DataLoading.beginNavigationFocus workspaceId entityType entityId observationModel
                                in
                                ( focusedModel, Cmd.batch [ observationCmd, focusCmd ] )

                            Nothing ->
                                ( observationModel, observationCmd )

                    else
                        let
                            observations =
                                withStream.observations

                            selectedWasRemoved =
                                observations.selectedId
                                    |> Maybe.map (\observationId -> not (Dict.member observationId snapshot.observations))
                                    |> Maybe.withDefault False

                            reconciledObservations =
                                reconcileSnapshotObservations snapshot.observations observations

                            dependencies =
                                withStream.dependencies

                            resetDependencies =
                                Dependencies.resetCache dependencies

                            cards =
                                withStream.cards

                            loading =
                                withStream.dataLoading

                            snapshotModel =
                                { withStream
                                    | workspaces = Dict.union snapshot.workspaces withStream.workspaces
                                    , projects = snapshot.projects
                                    , tasks = snapshot.tasks
                                    , observations = reconciledObservations
                                    , dependencies = { resetDependencies | taskDependencyLinks = snapshot.dependencies }
                                    , cards = { cards | projectNextTasks = Dict.empty, projectNextTaskDiagnostics = Dict.empty, projectNextTasksLoading = Dict.empty, projectNextTaskDiagnosticsLoading = Dict.empty, projectNextTasksErrors = Dict.empty, projectNextTaskDiagnosticsErrors = Dict.empty }
                                    , dataLoading = { loading | loadingWorkspaceData = False, pendingWorkspaceLoads = 0, activeWorkspaceLoadToken = Nothing, cardHydrationLoaded = True, navigationVisibilityActive = False }
                                }

                            ( dirtyModel, dirtyCmd ) =
                                Timeline.markDirty snapshotModel

                            ( refreshedModel, refreshCmd ) =
                                Observation.refreshActiveResults dirtyModel
                        in
                        ( refreshedModel
                        , Cmd.batch
                            [ dirtyCmd
                            , refreshCmd
                            , if selectedWasRemoved then
                                replaceFragment refreshedModel

                              else
                                Cmd.none
                            ]
                        )


reconcileSnapshotObservations : Dict.Dict String Api.Observation -> ObservationModel -> ObservationModel
reconcileSnapshotObservations canonicalById observations =
    let
        retainedItems =
            observations.items
                |> Dict.foldl
                    (\observationId listed retainedDict ->
                        canonicalById
                            |> Dict.get observationId
                            |> Maybe.map
                                (\canonical ->
                                    Dict.insert observationId (Observation.preferNewerObservation canonical listed) retainedDict
                                )
                            |> Maybe.withDefault retainedDict
                    )
                    Dict.empty

        retainedEvidence =
            observations.matchEvidence
                |> Dict.foldl
                    (\observationId evidence retainedDict ->
                        canonicalById
                            |> Dict.get observationId
                            |> Maybe.map
                                (\canonical ->
                                    Dict.insert observationId
                                        { evidence | observation = Observation.preferNewerObservation canonical evidence.observation }
                                        retainedDict
                                )
                            |> Maybe.withDefault retainedDict
                    )
                    Dict.empty

        retained =
            { observations
                | items = retainedItems
                , orderedIds = List.filter (\observationId -> Dict.member observationId retainedItems) observations.orderedIds
                , matchEvidence = retainedEvidence
                , detailLoading = False
                , detailError = Nothing
                , activeDetailRequest = Nothing
            }
    in
    case observations.selectedId of
        Just observationId ->
            case Dict.get observationId canonicalById of
                Just canonical ->
                    let
                        wasCached =
                            Dict.member observationId retainedItems

                        reconciled =
                            Observation.applyCanonicalObservation canonical retained
                    in
                    if wasCached then
                        reconciled

                    else
                        { reconciled
                            | items = Dict.remove observationId reconciled.items
                            , orderedIds = List.filter ((/=) observationId) reconciled.orderedIds
                        }

                Nothing ->
                    Observation.removeObservation observationId retained

        Nothing ->
            retained


beginScopedResync : ChangeStream.Scope -> Model -> ( Model, Cmd Msg )
beginScopedResync scope model =
    let
        invalidated =
            invalidateScopeRequests scope model

        cacheCleared =
            if scopeMatchesSelectedWorkspace scope invalidated then
                { invalidated | dependencies = Dependencies.resetCache invalidated.dependencies }

            else
                invalidated
    in
    ( setWebSocketState Connecting cacheCleared
    , connectCmd model.flags model.sessionContext scope True
    )


invalidateScopeRequests : ChangeStream.Scope -> Model -> Model
invalidateScopeRequests scope model =
    let
        prefix =
            ChangeStream.scopeKey scope ++ "|"
    in
    updateWebSocket
        (\webSocket ->
            { webSocket
                | streams = Dict.update (ChangeStream.scopeKey scope) (Maybe.map (\stream -> { stream | live = False, resumeToken = Nothing })) webSocket.streams
                , targetGenerations = Dict.filter (\target _ -> not (String.startsWith prefix target)) webSocket.targetGenerations
            }
        )
        model


scopePortValue : Maybe Api.SessionContext -> ChangeStream.Scope -> Encode.Value
scopePortValue sessionContext scope =
    let
        audienceFields =
            case sessionContext of
                Just session ->
                    [ ( "audienceId", Encode.string session.principal.actorId ) ]

                Nothing ->
                    []

        scopeFields =
            case scope of
                ChangeStream.Workspace workspaceId ->
                    [ ( "scope", Encode.string "workspace" )
                    , ( "workspaceId", Encode.string workspaceId )
                    , ( "snapshotProfile", Encode.string "workspace_shell_v1" )
                    ]

                ChangeStream.Global ->
                    [ ( "scope", Encode.string "global" ), ( "snapshotProfile", Encode.string "full_v1" ) ]
    in
    Encode.object (audienceFields ++ scopeFields)


applyActions : ChangeStream.Scope -> List ChangeStream.Action -> Model -> ( Model, Cmd Msg )
applyActions scope actions model =
    let
        navigationTargets =
            List.filterMap
                (\action ->
                    case action of
                        ChangeStream.RevalidateNavigationSummary entityType entityId ->
                            Just ( entityType, entityId )

                        _ ->
                            Nothing
                )
                actions

        otherActions =
            List.filter
                (\action ->
                    case action of
                        ChangeStream.RevalidateNavigationSummary _ _ ->
                            False

                        _ ->
                            True
                )
                actions

        ( revalidationModel, revalidationCmd ) =
            if scopeMatchesSelectedWorkspace scope model then
                requestNavigationSummaryBatches scope navigationTargets model

            else
                ( model, Cmd.none )
    in
    List.foldl (applyAction scope) ( revalidationModel, revalidationCmd ) otherActions


applyAction : ChangeStream.Scope -> ChangeStream.Action -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
applyAction scope action ( model, accumulated ) =
    let
        append command nextModel =
            ( nextModel, Cmd.batch [ accumulated, command ] )
    in
    case action of
        ChangeStream.RefetchEntity entity identity ->
            requestEntity scope entity identity accumulated model

        ChangeStream.RemoveEntity entity identity ->
            if entity == "task_dependency" && not (scopeMatchesSelectedWorkspace scope model) then
                ( model, accumulated )

            else
                removeEntity scope entity identity accumulated model

        ChangeStream.RefreshTaskDependencies taskId dependsOnId present requestId ->
            if scopeMatchesSelectedWorkspace scope model then
                let
                    actionName =
                        if present then
                            "add"

                        else
                            "remove"

                    linkReconciledModel =
                        applyDependencyMutationResult
                            { action = actionName
                            , taskId = taskId
                            , dependsOnId = dependsOnId
                            , affectedTasks = []
                            }
                            model

                    ( correlatedModel, shouldRefresh ) =
                        Dependencies.prepareDependencyEventRefresh taskId dependsOnId actionName requestId linkReconciledModel

                    ( readinessModel, readinessCmd ) =
                        revalidateDependencyReadiness scope taskId dependsOnId present correlatedModel

                    commands =
                        Cmd.batch [ accumulated, readinessCmd ]
                in
                if not shouldRefresh then
                    ( readinessModel, commands )

                else if Dict.get taskId readinessModel.cards.expandedCards |> Maybe.withDefault False then
                    let
                        ( next, command ) =
                            Dependencies.beginDependencyRefresh taskId readinessModel
                    in
                    ( next, Cmd.batch [ commands, command ] )

                else
                    ( Dependencies.invalidateDependencyPage taskId readinessModel, commands )

            else
                ( model, accumulated )

        ChangeStream.RefreshTaskOverview taskId ->
            let
                ( next, command ) =
                    requestNavigationSummaryBatches scope [ ( "task", taskId ) ] model
            in
            ( next, Cmd.batch [ accumulated, command ] )

        ChangeStream.RefreshReadiness entity identity ->
            let
                ( next, command ) =
                    requestNavigationSummaryBatches scope [ ( entity, identity ) ] model
            in
            ( next, Cmd.batch [ accumulated, command ] )

        ChangeStream.RevalidateNavigationSummary entity identity ->
            let
                ( next, command ) =
                    requestNavigationSummaryBatches scope [ ( entity, identity ) ] model
            in
            ( next, Cmd.batch [ accumulated, command ] )

        ChangeStream.RefreshNextTasks workspaceId ->
            if model.selectedWorkspaceId == Just workspaceId then
                let
                    cards =
                        model.cards

                    invalidated =
                        advanceCanonicalTarget scope
                            ("next:" ++ workspaceId)
                            { model
                                | cards =
                                    { cards
                                        | projectNextTasks = Dict.empty
                                        , projectNextTaskDiagnostics = Dict.empty
                                        , projectNextTasksLoading = Dict.empty
                                        , projectNextTaskDiagnosticsLoading = Dict.empty
                                        , projectNextTasksErrors = Dict.empty
                                        , projectNextTaskDiagnosticsErrors = Dict.empty
                                    }
                            }
                in
                ( invalidated, accumulated )

            else
                append Cmd.none model

        ChangeStream.RefreshSearch _ ->
            if scopeMatchesSelectedWorkspace scope model then
                let
                    search =
                        model.search
                in
                append Cmd.none { model | search = { search | unifiedResults = Nothing, activeRequest = Nothing } }

            else
                ( model, accumulated )

        ChangeStream.RefreshObservations ->
            let
                ( nextModel, command ) =
                    Observation.refreshActiveResults model
            in
            append command nextModel

        ChangeStream.RefreshCatalogue ->
            requestCanonical scope "catalogue" (\guard -> Api.fetchWorkspaces model.flags.apiUrl (CanonicalCatalogueFetched guard)) accumulated model

        ChangeStream.RefreshGroups ->
            requestCanonical scope "groups" (\guard -> Api.fetchWorkspaceGroups model.flags.apiUrl (CanonicalGroupsFetched guard)) accumulated model

        ChangeStream.RefreshGroupMembers groupId ->
            requestCanonical scope ("group-members:" ++ groupId) (\guard -> Api.fetchGroupMembers model.flags.apiUrl groupId (CanonicalGroupMembersFetched guard groupId)) accumulated model

        ChangeStream.RefreshMemberships workspaceId ->
            requestCanonical scope ("memberships:" ++ workspaceId) (\guard -> Api.fetchWorkspaceMemberships model.flags.apiUrl workspaceId (CanonicalMembershipsFetched guard workspaceId)) accumulated model

        ChangeStream.RefreshSessionAuthorization ->
            requestCanonical scope "session" (\guard -> Api.fetchSessionContext model.flags.apiUrl model.selectedWorkspaceId (CanonicalSessionFetched guard model.selectedWorkspaceId)) accumulated model

        ChangeStream.AccessGranted workspaceId ->
            append (clearChangeStreamScope (scopePortValue model.sessionContext (ChangeStream.Workspace workspaceId))) model

        ChangeStream.ClearWorkspace workspaceId ->
            let
                revokedScope =
                    ChangeStream.Workspace workspaceId

                observations =
                    model.observations

                cards =
                    model.cards

                webSocket =
                    model.webSocket

                scopePrefix =
                    ChangeStream.scopeKey revokedScope ++ "|"

                remainingStreams =
                    Dict.remove (ChangeStream.scopeKey revokedScope) webSocket.streams

                remainingState =
                    if Dict.values remainingStreams |> List.any .live then
                        Connected

                    else
                        Disconnected

                scopedWebSocket =
                    { webSocket
                        | state = remainingState
                        , streams = remainingStreams
                        , targetGenerations = Dict.filter (\target _ -> not (String.startsWith scopePrefix target)) webSocket.targetGenerations
                    }

                scopeCommand =
                    Cmd.batch [ clearChangeStreamScope (scopePortValue model.sessionContext revokedScope), disconnectChangeStreamScope (scopePortValue model.sessionContext revokedScope) ]

                cleared =
                    { model
                        | workspaces = Dict.remove workspaceId model.workspaces
                        , projects = Dict.empty
                        , tasks = Dict.empty
                        , observations = Observation.clearSelection { observations | items = Dict.empty, orderedIds = [], matchEvidence = Dict.empty }
                        , dependencies = Dependencies.resetCache model.dependencies
                        , cards = { cards | projectNextTasks = Dict.empty, projectNextTaskDiagnostics = Dict.empty, projectNextTasksLoading = Dict.empty, projectNextTaskDiagnosticsLoading = Dict.empty, projectNextTasksErrors = Dict.empty, projectNextTaskDiagnosticsErrors = Dict.empty }
                        , sessionRequestEpoch = model.sessionRequestEpoch + 1
                        , timeline = Timeline.reset (model.sessionRequestEpoch + 1)
                        , webSocket =
                            scopedWebSocket
                    }
            in
            if model.selectedWorkspaceId == Just workspaceId then
                append scopeCommand cleared

            else
                append scopeCommand { model | workspaces = Dict.remove workspaceId model.workspaces, webSocket = scopedWebSocket }

        ChangeStream.RefreshTimeline ->
            let
                ( nextModel, command ) =
                    Timeline.markDirty model
            in
            append command nextModel

        ChangeStream.BeginResync ->
            let
                ( nextModel, command ) =
                    beginScopedResync scope model
            in
            append command nextModel

        ChangeStream.NoAction ->
            ( model, accumulated )


removeEntity : ChangeStream.Scope -> String -> String -> Cmd Msg -> Model -> ( Model, Cmd Msg )
removeEntity scope entity identity accumulated model =
    let
        target =
            "entity:" ++ entity ++ ":" ++ identity

        withGeneration =
            advanceCanonicalTarget scope target model

        observations =
            withGeneration.observations

        dependencies =
            withGeneration.dependencies

        groups =
            withGeneration.groups

        removed =
            case entity of
                "workspace" ->
                    Just ( { withGeneration | workspaces = Dict.remove identity withGeneration.workspaces }, Cmd.none )

                "project" ->
                    Just ( { withGeneration | projects = Dict.remove identity withGeneration.projects }, Cmd.none )

                "task" ->
                    let
                        invalidated =
                            Dependencies.invalidateDependencyPage identity withGeneration

                        invalidatedDependencies =
                            invalidated.dependencies
                    in
                    Just
                        ( { invalidated
                            | tasks = Dict.remove identity invalidated.tasks
                            , dependencies =
                                { invalidatedDependencies
                                    | taskDependencyLinks = List.filter (\link -> link.taskId /= identity && link.dependsOnId /= identity) invalidatedDependencies.taskDependencyLinks
                                    , taskReadinessRollups = Dict.remove identity invalidatedDependencies.taskReadinessRollups
                                }
                          }
                        , Cmd.none
                        )

                "observation" ->
                    if scopeMatchesSelectedWorkspace scope withGeneration then
                        Just (Observation.reconcileDeletedObservation identity withGeneration)

                    else
                        Just ( withGeneration, Cmd.none )

                "task_dependency" ->
                    case String.split ":" identity of
                        taskId :: dependsOnId :: _ ->
                            Just
                                ( { withGeneration
                                    | dependencies =
                                        { dependencies
                                            | taskDependencies = Dict.update taskId (Maybe.map (List.filter (\dependency -> dependency.id /= dependsOnId))) dependencies.taskDependencies
                                            , taskDependencyLinks = List.filter (\link -> link.taskId /= taskId || link.dependsOnId /= dependsOnId) dependencies.taskDependencyLinks
                                        }
                                  }
                                , Cmd.none
                                )

                        _ ->
                            Nothing

                "group" ->
                    Just ( { withGeneration | groups = { groups | workspaceGroups = Dict.remove identity groups.workspaceGroups, groupMembers = Dict.remove identity groups.groupMembers } }, Cmd.none )

                "group_membership" ->
                    case String.split ":" identity of
                        groupId :: workspaceId :: _ ->
                            Just ( { withGeneration | groups = { groups | groupMembers = Dict.update groupId (Maybe.map (List.filter ((/=) workspaceId))) groups.groupMembers } }, Cmd.none )

                        _ ->
                            Nothing

                _ ->
                    Nothing
    in
    case removed of
        Just ( nextModel, command ) ->
            ( nextModel, Cmd.batch [ accumulated, command ] )

        Nothing ->
            let
                ( nextModel, command ) =
                    beginScopedResync scope model
            in
            ( nextModel, Cmd.batch [ accumulated, command ] )


advanceCanonicalTarget : ChangeStream.Scope -> String -> Model -> Model
advanceCanonicalTarget scope targetKey model =
    let
        target =
            ChangeStream.scopeKey scope ++ "|" ++ targetKey

        generation =
            Dict.get target model.webSocket.targetGenerations |> Maybe.withDefault 0 |> (+) 1
    in
    updateWebSocket (\webSocket -> { webSocket | targetGenerations = Dict.insert target generation webSocket.targetGenerations }) model


requestCanonical : ChangeStream.Scope -> String -> (CanonicalRequestGuard -> Cmd Msg) -> Cmd Msg -> Model -> ( Model, Cmd Msg )
requestCanonical scope targetKey makeCommand accumulated model =
    case model.sessionContext of
        Nothing ->
            let
                ( next, command ) =
                    beginScopedResync scope model
            in
            ( next, Cmd.batch [ accumulated, command ] )

        Just session ->
            let
                target =
                    ChangeStream.scopeKey scope ++ "|" ++ targetKey

                generation =
                    Dict.get target model.webSocket.targetGenerations |> Maybe.withDefault 0 |> (+) 1

                guard =
                    { scopeKey = ChangeStream.scopeKey scope
                    , targetKey = targetKey
                    , targetGeneration = generation
                    , sessionEpoch = model.sessionRequestEpoch
                    , routeWorkspace = model.selectedWorkspaceId
                    , audienceId = session.principal.actorId
                    }

                updated =
                    updateWebSocket (\webSocket -> { webSocket | targetGenerations = Dict.insert target generation webSocket.targetGenerations }) model
            in
            ( updated, Cmd.batch [ accumulated, makeCommand guard ] )


requestEntity : ChangeStream.Scope -> String -> String -> Cmd Msg -> Model -> ( Model, Cmd Msg )
requestEntity scope entity identity accumulated model =
    case entity of
        "workspace" ->
            requestCanonical scope ("entity:workspace:" ++ identity) (\guard -> Api.fetchWorkspace model.flags.apiUrl identity (CanonicalWorkspaceFetched guard identity)) accumulated model

        "project" ->
            requestCanonical scope ("entity:project:" ++ identity) (\guard -> Api.fetchProject model.flags.apiUrl identity (CanonicalProjectFetched guard identity)) accumulated model

        "task" ->
            requestCanonical scope ("entity:task:" ++ identity) (\guard -> Api.fetchTask model.flags.apiUrl identity (CanonicalTaskFetched guard identity)) accumulated model

        "observation" ->
            if scopeMatchesSelectedWorkspace scope model && Observation.isLoadedOrSelected identity model.observations then
                requestCanonical scope ("entity:observation:" ++ identity) (\guard -> Api.fetchObservation model.flags.apiUrl identity (CanonicalObservationFetched guard (model.selectedWorkspaceId |> Maybe.withDefault "") identity)) accumulated model

            else if scopeMatchesSelectedWorkspace scope model then
                ( { model | observations = Observation.markResultsStale model.observations }, accumulated )

            else
                ( model, accumulated )

        "task_dependency" ->
            case String.split ":" identity of
                taskId :: _ :: [] ->
                    requestTaskOverview scope taskId accumulated model

                _ ->
                    let
                        ( next, command ) =
                            beginScopedResync scope model
                    in
                    ( next, Cmd.batch [ accumulated, command ] )

        _ ->
            let
                ( next, command ) =
                    beginScopedResync scope model
            in
            ( next, Cmd.batch [ accumulated, command ] )


scopeMatchesSelectedWorkspace : ChangeStream.Scope -> Model -> Bool
scopeMatchesSelectedWorkspace scope model =
    case scope of
        ChangeStream.Workspace workspaceId ->
            model.selectedWorkspaceId == Just workspaceId

        ChangeStream.Global ->
            False


requestNavigationSummaryBatches : ChangeStream.Scope -> List ( String, String ) -> Model -> ( Model, Cmd Msg )
requestNavigationSummaryBatches scope targets model =
    let
        -- A summary batch is a revalidation of cards the current bounded
        -- projection already owns.  Never let an event for an unloaded or
        -- filtered-out branch populate the tree behind the server filter.
        loadedTarget ( entityType, entityId ) =
            case entityType of
                "project" ->
                    Set.member entityId model.dataLoading.navigationVisibleProjectIds
                        || Dict.member entityId model.dataLoading.projectCardSummaries

                "task" ->
                    Set.member entityId model.dataLoading.navigationVisibleTaskIds
                        || Dict.member entityId model.dataLoading.taskCardSummaries

                _ ->
                    False

        uniqueTargets =
            List.foldl
                (\target values ->
                    if List.member target values then
                        values

                    else
                        target :: values
                )
                []
                targets
                |> List.reverse
                |> List.filter loadedTarget

        chunks remaining =
            case remaining of
                [] ->
                    []

                _ ->
                    List.take 100 remaining :: chunks (List.drop 100 remaining)

        requestBatch workspaceId batch ( current, accumulated ) =
            let
                projectIds =
                    batch
                        |> List.filterMap
                            (\( entityType, entityId ) ->
                                if entityType == "project" then
                                    Just entityId

                                else
                                    Nothing
                            )

                taskIds =
                    batch
                        |> List.filterMap
                            (\( entityType, entityId ) ->
                                if entityType == "task" then
                                    Just entityId

                                else
                                    Nothing
                            )

                targetKey =
                    "navigation-summaries:" ++ String.join "," (List.map (\( entityType, entityId ) -> entityType ++ ":" ++ entityId) batch)
            in
            requestCanonical scope
                targetKey
                (\guard ->
                    Api.fetchNavigationSummaries current.flags.apiUrl
                        workspaceId
                        projectIds
                        taskIds
                        (CanonicalNavigationSummariesFetched guard workspaceId projectIds taskIds)
                )
                accumulated
                current
    in
    if List.isEmpty uniqueTargets then
        ( model, Cmd.none )

    else if List.any (\( entityType, entityId ) -> String.isEmpty entityId || not (List.member entityType [ "project", "task" ])) uniqueTargets then
        beginScopedResync scope model

    else
        case scope of
            ChangeStream.Workspace workspaceId ->
                List.foldl (requestBatch workspaceId) ( model, Cmd.none ) (chunks uniqueTargets)

            ChangeStream.Global ->
                beginScopedResync scope model


requestTaskOverview : ChangeStream.Scope -> String -> Cmd Msg -> Model -> ( Model, Cmd Msg )
requestTaskOverview scope taskId accumulated model =
    requestCanonical scope ("task-overview:" ++ taskId) (\guard -> Api.fetchTaskOverview model.flags.apiUrl taskId (CanonicalTaskOverviewFetched guard taskId)) accumulated model


requestTaskReadiness : ChangeStream.Scope -> String -> Cmd Msg -> Model -> ( Model, Cmd Msg )
requestTaskReadiness scope taskId accumulated model =
    requestCanonical scope ("dependency-readiness:task:" ++ taskId) (\guard -> Api.fetchTaskOverview model.flags.apiUrl taskId (CanonicalTaskReadinessFetched guard taskId)) accumulated model


revalidateDependencyReadiness : ChangeStream.Scope -> String -> String -> Bool -> Model -> ( Model, Cmd Msg )
revalidateDependencyReadiness scope taskId dependsOnId present model =
    let
        collectAncestors getParent maybeId seen =
            case maybeId of
                Just identity ->
                    if List.member identity seen then
                        seen

                    else
                        collectAncestors getParent (getParent identity) (identity :: seen)

                Nothing ->
                    seen

        taskAncestors =
            Dict.get taskId model.tasks
                |> Maybe.andThen .parentId
                |> (\parentId -> collectAncestors (\identity -> Dict.get identity model.tasks |> Maybe.andThen .parentId) parentId [])

        projectAncestors =
            Dict.get taskId model.tasks
                |> Maybe.andThen .projectId
                |> (\projectId -> collectAncestors (\identity -> Dict.get identity model.projects |> Maybe.andThen .parentId) projectId [])

        dependencies =
            model.dependencies

        invalidatedModel =
            { model
                | dependencies =
                    { dependencies
                        | taskReadinessRollups = List.foldl Dict.remove dependencies.taskReadinessRollups (taskId :: taskAncestors)
                        , projectReadinessRollups = List.foldl Dict.remove dependencies.projectReadinessRollups projectAncestors
                    }
            }

        ( prerequisiteModel, prerequisiteCmd ) =
            if present && not (Dict.member dependsOnId invalidatedModel.tasks) then
                requestEntity scope "task" dependsOnId Cmd.none invalidatedModel

            else
                ( invalidatedModel, Cmd.none )

        ( taskModel, taskCmd ) =
            List.foldl
                (\ancestorId ( current, accumulated ) -> requestTaskReadiness scope ancestorId accumulated current)
                ( prerequisiteModel, Cmd.none )
                taskAncestors

        ( projectModel, projectCmd ) =
            List.foldl
                (\projectId ( current, accumulated ) -> requestProjectOverview scope ("dependency-readiness:project:" ++ projectId) projectId accumulated current)
                ( taskModel, Cmd.none )
                projectAncestors
    in
    ( projectModel, Cmd.batch [ prerequisiteCmd, taskCmd, projectCmd ] )


requestProjectOverview : ChangeStream.Scope -> String -> String -> Cmd Msg -> Model -> ( Model, Cmd Msg )
requestProjectOverview scope target projectId accumulated model =
    requestCanonical scope target (\guard -> Api.fetchProjectOverview model.flags.apiUrl projectId (CanonicalProjectOverviewFetched guard projectId)) accumulated model


requestGroupMembers : ChangeStream.Scope -> List String -> Model -> ( Model, Cmd Msg )
requestGroupMembers scope groupIds model =
    List.foldl
        (\groupId ( current, commands ) ->
            requestCanonical scope ("group-members:" ++ groupId) (\guard -> Api.fetchGroupMembers current.flags.apiUrl groupId (CanonicalGroupMembersFetched guard groupId)) commands current
        )
        ( model, Cmd.none )
        groupIds


canonicalHttpFailure : CanonicalRequestGuard -> Http.Error -> Model -> ( Model, Cmd Msg )
canonicalHttpFailure guard error model =
    case error of
        Http.BadStatus 401 ->
            ( model, Task.perform identity (Task.succeed AuthUnauthorized) )

        Http.BadStatus 403 ->
            case scopeFromKey guard.scopeKey of
                ChangeStream.Workspace workspaceId ->
                    applyActions (ChangeStream.Workspace workspaceId)
                        [ ChangeStream.ClearWorkspace workspaceId, ChangeStream.RefreshCatalogue, ChangeStream.RefreshSessionAuthorization ]
                        model

                ChangeStream.Global ->
                    applyActions ChangeStream.Global [ ChangeStream.RefreshSessionAuthorization ] model

        _ ->
            ( model, Cmd.none )


scopeFromKey : String -> ChangeStream.Scope
scopeFromKey key =
    if key == "global" then
        ChangeStream.Global

    else
        ChangeStream.Workspace (String.dropLeft (String.length "workspace:") key)


handleChangeEvent : Api.ChangeEvent -> Model -> ( Model, Cmd Msg )
handleChangeEvent event model =
    let
        ( updatedModel, refreshCmd ) =
            applyChangeEvent event model

        isSelfEvent =
            eventTouchesPendingMutation event model.mutations.pendingMutationIds model.mutations.pendingRequestIds
    in
    if isSelfEvent then
        ( updatedModel
        , if requiresSelfRefresh event then
            refreshCmd

          else
            Cmd.none
        )

    else
        let
            toastMsg =
                changeEventDescription event

            ( toastedModel, toastCmd ) =
                addToast Info toastMsg updatedModel
        in
        ( toastedModel, Cmd.batch [ refreshCmd, toastCmd ] )


eventMatchesCurrentWorkspace : Api.ChangeEvent -> Model -> Bool
eventMatchesCurrentWorkspace event model =
    case event.workspaceId of
        Just eventWsId ->
            model.selectedWorkspaceId == Just eventWsId

        Nothing ->
            case event.entityType of
                Api.EWorkspaceGroup ->
                    Permissions.isSuperadmin model

                _ ->
                    False


eventTouchesPendingMutation : Api.ChangeEvent -> Dict.Dict String Bool -> Dict.Dict String Bool -> Bool
eventTouchesPendingMutation event _ pendingRequestIds =
    let
        requestMatched =
            case event.requestId of
                Just requestId ->
                    Dict.member requestId pendingRequestIds

                Nothing ->
                    case payloadField "request_id" event.payload of
                        Just requestId ->
                            Dict.member requestId pendingRequestIds

                        Nothing ->
                            False
    in
    requestMatched


{-| Try to apply the change event payload directly into the model.
Falls back to a full re-fetch when the payload is missing or cannot be decoded.
-}
reloadAfterCascadeDelete : Model -> ( Model, Cmd Msg )
reloadAfterCascadeDelete model =
    let
        cards =
            model.cards

        cacheCleared =
            { model
                | dependencies = Dependencies.resetCache model.dependencies
                , cards =
                    { cards
                        | projectNextTasks = Dict.empty
                        , projectNextTaskDiagnostics = Dict.empty
                        , projectNextTasksLoading = Dict.empty
                        , projectNextTaskDiagnosticsLoading = Dict.empty
                        , projectNextTasksErrors = Dict.empty
                        , projectNextTaskDiagnosticsErrors = Dict.empty
                    }
            }
    in
    beginWorkspaceDataReload False cacheCleared


applyChangeEvent : Api.ChangeEvent -> Model -> ( Model, Cmd Msg )
applyChangeEvent event model =
    case event.entityType of
        Api.EWorkspace ->
            case event.changeType of
                Api.Deleted ->
                    let
                        currentGroups =
                            model.groups

                        updatedGroups =
                            { currentGroups
                                | groupMembers = Dict.map (\_ memberIds -> List.filter ((/=) event.entityId) memberIds) model.groups.groupMembers
                            }
                    in
                    ( { model | workspaces = Dict.remove event.entityId model.workspaces, groups = updatedGroups }
                    , Cmd.none
                    )

                _ ->
                    case Maybe.andThen (tryDecode Api.workspaceDecoder) event.payload of
                        Just ws ->
                            ( { model | workspaces = Dict.insert ws.id ws model.workspaces }
                            , Cmd.none
                            )

                        Nothing ->
                            beginWorkspaceDataReload False model

        Api.EProject ->
            case event.changeType of
                Api.Deleted ->
                    reloadAfterCascadeDelete model

                _ ->
                    case Maybe.andThen (tryDecode Api.projectDecoder) event.payload of
                        Just proj ->
                            let
                                updatedModel =
                                    { model | projects = Dict.insert proj.id proj model.projects }
                            in
                            ( updatedModel
                            , refreshReadinessCaches updatedModel
                            )

                        Nothing ->
                            let
                                ( reloadModel, reloadCmd ) =
                                    beginWorkspaceDataReload False model
                            in
                            ( reloadModel, Cmd.batch [ reloadCmd, refreshCachedEntityData reloadModel ] )

        Api.ETask ->
            case event.changeType of
                Api.Deleted ->
                    reloadAfterCascadeDelete model

                _ ->
                    case Maybe.andThen (tryDecode Api.taskDecoder) event.payload of
                        Just task ->
                            let
                                updatedModel =
                                    { model | tasks = Dict.insert task.id task model.tasks }
                            in
                            ( updatedModel
                            , refreshReadinessCaches updatedModel
                            )

                        Nothing ->
                            let
                                ( reloadModel, reloadCmd ) =
                                    beginWorkspaceDataReload False model
                            in
                            ( reloadModel, Cmd.batch [ reloadCmd, refreshCachedEntityData reloadModel ] )

        Api.EMemory ->
            beginWorkspaceDataReload False model

        Api.EObservation ->
            case event.changeType of
                Api.Deleted ->
                    let
                        ( cleaned, cleanupCmd ) =
                            Observation.reconcileDeletedObservation event.entityId model

                        ( refreshed, refreshCmd ) =
                            Observation.refreshActiveResults cleaned
                    in
                    ( refreshed, Cmd.batch [ cleanupCmd, refreshCmd ] )

                _ ->
                    Observation.refreshActiveResults model

        Api.EMemoryLink ->
            beginWorkspaceDataReload False model

        Api.ECategory ->
            beginWorkspaceDataReload False model

        Api.ETaskDependency ->
            let
                mutationResult =
                    Maybe.andThen (tryDecode Api.dependencyMutationResultDecoder) event.payload

                taskId =
                    mutationResult
                        |> Maybe.map .taskId
                        |> Maybe.withDefault (payloadField "task_id" event.payload |> Maybe.withDefault event.entityId)

                ( patchedModel, reloadCmd ) =
                    case mutationResult of
                        Just result ->
                            let
                                statusPatchedModel =
                                    applyDependencyMutationResult result model

                                dependenciesModel =
                                    statusPatchedModel.dependencies

                                updatedLinks =
                                    applyTaskDependencyLinkMutation result dependenciesModel.taskDependencyLinks
                            in
                            ( { statusPatchedModel | dependencies = { dependenciesModel | taskDependencyLinks = updatedLinks } }, Cmd.none )

                        Nothing ->
                            beginWorkspaceDataReload False model
            in
            ( patchedModel
            , Cmd.batch
                [ Api.fetchTaskOverview model.flags.apiUrl taskId (GotTaskDependencies taskId model.selectedWorkspaceId model.sessionRequestEpoch model.dependencies.nextTaskDependencyRequestGeneration)
                , refreshTaskReadinessCaches patchedModel
                , refreshProjectReadinessCaches patchedModel
                , reloadCmd
                ]
            )

        Api.ECategoryLink ->
            beginWorkspaceDataReload False model

        Api.ETag ->
            ( model, Cmd.none )

        Api.EWorkspaceGroup ->
            let
                workspaceListLoadToken =
                    model.dataLoading.nextWorkspaceListLoadToken

                currentLoading =
                    model.dataLoading

                loadingModel =
                    { model
                        | workspaces = Dict.empty
                        , dataLoading =
                            { currentLoading
                                | loadingWorkspaces = True
                                , activeWorkspaceListLoadToken = Just workspaceListLoadToken
                                , nextWorkspaceListLoadToken = workspaceListLoadToken + 1
                            }
                    }

                groupCmds =
                    if Permissions.isSuperadmin model then
                        [ Api.fetchWorkspaceGroups model.flags.apiUrl GotWorkspaceGroups ]

                    else
                        []
            in
            ( loadingModel
            , Cmd.batch (Api.fetchWorkspaces model.flags.apiUrl (GotWorkspaces workspaceListLoadToken) :: groupCmds)
            )

        Api.ESavedView ->
            ( model, Cmd.none )

        Api.EOther _ ->
            ( model, Cmd.none )


withWorkspace : Model -> (String -> Cmd Msg) -> ( Model, Cmd Msg )
withWorkspace model mkCmd =
    case model.selectedWorkspaceId of
        Just wsId ->
            ( model, mkCmd wsId )

        Nothing ->
            ( model, Cmd.none )


tryDecode : Decode.Decoder a -> Decode.Value -> Maybe a
tryDecode decoder val =
    Result.toMaybe (Decode.decodeValue decoder val)


payloadField : String -> Maybe Decode.Value -> Maybe String
payloadField fieldName payload =
    payload
        |> Maybe.andThen
            (\value ->
                Decode.decodeValue (Decode.field fieldName Decode.string) value
                    |> Result.toMaybe
            )


requiresSelfRefresh : Api.ChangeEvent -> Bool
requiresSelfRefresh event =
    case event.entityType of
        Api.ECategory ->
            True

        Api.ETag ->
            True

        Api.ETaskDependency ->
            True

        Api.EMemoryLink ->
            True

        Api.EMemory ->
            True

        Api.EProject ->
            event.changeType == Api.Deleted || payloadField "linked_memory" event.payload /= Nothing || payloadField "unlinked_memory" event.payload /= Nothing

        Api.ETask ->
            event.changeType == Api.Deleted || payloadField "linked_memory" event.payload /= Nothing || payloadField "unlinked_memory" event.payload /= Nothing

        _ ->
            False


refreshCachedEntityData : Model -> Cmd Msg
refreshCachedEntityData model =
    let
        dependencyCmds =
            model.dependencies.taskDependencies
                |> Dict.keys
                |> List.map (\taskId -> Api.fetchTaskOverview model.flags.apiUrl taskId (GotTaskDependencies taskId model.selectedWorkspaceId model.sessionRequestEpoch model.dependencies.nextTaskDependencyRequestGeneration))

        projectReadinessCmds =
            model.dependencies.projectReadinessRollups
                |> Dict.keys
                |> List.map (\projectId -> Api.fetchProjectOverview model.flags.apiUrl projectId (GotProjectOverview projectId))

        projectNextTaskCmds =
            model.cards.projectNextTasks
                |> Dict.keys
                |> List.map (refreshProjectNextTaskCache model)
    in
    Cmd.batch (dependencyCmds ++ projectReadinessCmds ++ projectNextTaskCmds)


refreshReadinessCaches : Model -> Cmd Msg
refreshReadinessCaches model =
    Cmd.batch
        [ refreshTaskReadinessCaches model
        , refreshProjectReadinessCaches model
        , refreshProjectNextTaskCaches model
        ]


refreshTaskReadinessCaches : Model -> Cmd Msg
refreshTaskReadinessCaches model =
    model.dependencies.taskDependencies
        |> Dict.keys
        |> List.map (\taskId -> Api.fetchTaskOverview model.flags.apiUrl taskId (GotTaskDependencies taskId model.selectedWorkspaceId model.sessionRequestEpoch model.dependencies.nextTaskDependencyRequestGeneration))
        |> Cmd.batch


refreshProjectReadinessCaches : Model -> Cmd Msg
refreshProjectReadinessCaches model =
    model.dependencies.projectReadinessRollups
        |> Dict.keys
        |> List.map (\projectId -> Api.fetchProjectOverview model.flags.apiUrl projectId (GotProjectOverview projectId))
        |> Cmd.batch


refreshProjectNextTaskCaches : Model -> Cmd Msg
refreshProjectNextTaskCaches model =
    model.cards.projectNextTasks
        |> Dict.keys
        |> List.map (refreshProjectNextTaskCache model)
        |> Cmd.batch


refreshProjectNextTaskCache : Model -> String -> Cmd Msg
refreshProjectNextTaskCache model projectId =
    Cmd.batch
        [ Api.fetchProjectNextTasks model.flags.apiUrl projectId 5 False (GotProjectNextTasks projectId)
        , Api.fetchProjectNextTasks model.flags.apiUrl projectId 200 True (GotProjectNextTaskDiagnostics projectId)
        ]


changeEventDescription : Api.ChangeEvent -> String
changeEventDescription event =
    let
        action =
            case event.changeType of
                Api.Created ->
                    "created"

                Api.Updated ->
                    "updated"

                Api.Deleted ->
                    "deleted"

        entity =
            case event.entityType of
                Api.EWorkspace ->
                    "Workspace"

                Api.EProject ->
                    "Project"

                Api.ETask ->
                    "Task"

                Api.EMemory ->
                    "Memory"

                Api.EObservation ->
                    "Observation"

                Api.EMemoryLink ->
                    "Memory link"

                Api.ECategory ->
                    "Category"

                Api.EWorkspaceGroup ->
                    "Workspace group"

                Api.ESavedView ->
                    "Saved view"

                Api.ETaskDependency ->
                    "Task dependency"

                Api.ECategoryLink ->
                    "Category link"

                Api.ETag ->
                    "Tags"

                Api.EOther s ->
                    s
    in
    entity ++ " " ++ action ++ actorSuffix event


actorSuffix : Api.ChangeEvent -> String
actorSuffix event =
    case event.actorLabel of
        Just label ->
            " by " ++ label

        Nothing ->
            ""
