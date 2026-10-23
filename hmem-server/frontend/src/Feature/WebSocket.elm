module Feature.WebSocket exposing
    ( readinessTargetBatches
    , refreshProjectCascade
    , refreshMutationEntities
    , refreshTaskPlacement
    , connectCmd
    , init
    , subscriptions
    , update
    )

import ActorAttribution
import Api
import Dict
import Feature.ChangeStream as ChangeStream
import Feature.DataLoading
import Feature.Dependencies as Dependencies
import Feature.Observation as Observation
import Feature.Search
import Feature.SessionDelivery as SessionDelivery
import Feature.Timeline as Timeline
import Feature.WorkspaceAdmin
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
    let
        ( next, command ) = updateRaw msg model
        ( refreshed, refreshCommand ) = pumpSessionResources (Helpers.retireProjectOperations next)
    in
    ( refreshed, Cmd.batch [ command, refreshCommand ] )


updateRaw : Msg -> Model -> ( Model, Cmd Msg )
updateRaw msg model =
    case msg of
        DemandSessionResource workspace target ->
            if model.selectedWorkspaceId == Just workspace && model.auth.status == AuthReady && Permissions.canReadCurrentWorkspace model then
                ( { model | sessionDelivery = SessionDelivery.demand workspace target model.sessionDelivery }, Cmd.none )
            else ( model, Cmd.none )

        RemoveSessionResource workspace target ->
            ( { model | sessionDelivery = SessionDelivery.removeDemand workspace target model.sessionDelivery }, Cmd.none )

        SessionResourceFetched id guard result ->
            let
                current = workspaceGuardIsCurrent guard model && Permissions.canReadCurrentWorkspace model
                    && (request |> Maybe.map (\owned -> guard.scopeKey == ChangeStream.scopeKey (ChangeStream.Workspace owned.workspace) && guard.routeWorkspace == Just owned.workspace && guard.targetKey == "session-resource:" ++ owned.key) |> Maybe.withDefault False)
                request = Dict.get id model.sessionDelivery.physical
                released = { model | sessionDelivery = SessionDelivery.settle id current result model.sessionDelivery }
            in
            if not current then ( released, Cmd.none )
            else case result of
                Err (Http.BadStatus 401) ->
                    canonicalHttpFailure guard (Http.BadStatus 401) { released | sessionDelivery = SessionDelivery.retireAll released.sessionDelivery }
                Err (Http.BadStatus 403) ->
                    canonicalHttpFailure guard (Http.BadStatus 403) { released | sessionDelivery = SessionDelivery.retireAll released.sessionDelivery }
                Err (Http.BadStatus 409) ->
                    case request |> Maybe.map .target of
                        Just (SessionDelivery.SessionBytes session _ _) ->
                            ( { released | sessionDelivery = SessionDelivery.invalidateSession (Maybe.withDefault "" guard.routeWorkspace) session released.sessionDelivery }, Cmd.none )
                        _ -> ( released, Cmd.none )
                _ -> ( released, Cmd.none )

        RefreshTaskStatusEligibility workspace epoch taskId ->
            if workspace /= model.selectedWorkspaceId || epoch /= model.sessionRequestEpoch || model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) || not (Dict.get taskId model.tasks |> Maybe.map (\task -> Just task.workspaceId == workspace) |> Maybe.withDefault False) then
                ( model, Cmd.none )
            else case workspace of
                Just workspaceId ->
                    let
                        parent = Dict.get taskId model.tasks |> Maybe.andThen .parentId |> Maybe.map (\id -> [ ( "task", id ) ]) |> Maybe.withDefault []
                    in
                    requestNavigationSummaryBatches (ChangeStream.Workspace workspaceId) (( "task", taskId ) :: parent) model
                Nothing -> ( model, Cmd.none )

        WsConnectingMsg ->
            if model.auth.status == AuthReady then
                ( Feature.DataLoading.invalidatePlanningCounts (setWebSocketState Connecting model), Cmd.none )

            else
                ( setWebSocketState Disconnected model, Cmd.none )

        WsConnectedMsg ->
            if model.auth.status == AuthReady then
                -- A transport open is only replaying.  The canonical terminal
                -- checkpoint below is the sole transition to live/Connected.
                ( Feature.DataLoading.invalidatePlanningCounts (setWebSocketState Connecting model), Cmd.none )

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
                            (Feature.DataLoading.invalidatePlanningCounts (setWebSocketState Connecting model))

            else
                ( setWebSocketState Disconnected model, Cmd.none )

        WsConnectionFailed reason ->
            if model.auth.status == AuthReady then
                addToast Warning
                    "Canonical change stream is reconnecting or requires scoped resync"
                    (Feature.DataLoading.invalidatePlanningCounts (setWebSocketState (ConnectionFailed reason) model))

            else
                ( setWebSocketState Disconnected model, Cmd.none )

        WsMessageReceived raw ->
            if model.auth.status == AuthReady then
                applyWebSocketChange raw model

            else if model.auth.status == AuthBooting && scopeAuthority ChangeStream.Global model /= Nothing then
                case Api.decodeCanonicalFrame raw of
                    Just (Api.CanonicalScoped Api.GlobalScope nested) ->
                        applyScopedFrame ChangeStream.Global nested model

                    Just (Api.CanonicalSnapshot Api.GlobalScope items token profile) ->
                        applyCanonicalSnapshot ChangeStream.Global profile items token model

                    Just (Api.CanonicalBatch Api.GlobalScope frames) ->
                        applyScopedFrames ChangeStream.Global frames model

                    Just (Api.CanonicalStatus Api.GlobalScope status) ->
                        applyScopedFrame ChangeStream.Global (Api.CanonicalStatus Api.GlobalScope status) model

                    _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        CanonicalWorkspaceFetched guard workspaceId result ->
            if canonicalGuardIsCurrent guard model then
                case result of
                    Ok workspace ->
                        ( { model | workspaces = if Set.member workspace.id model.groups.deletedWorkspaces then model.workspaces else Dict.insert workspace.id workspace model.workspaces }, Cmd.none )

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
                                    | taskDependencyLinks = links ++ retained
                                }
                          } |> (\updated -> Helpers.planningReadRevision guard model |> Maybe.map (\revision -> Helpers.applyTaskReadiness revision taskId overview.readinessRollup updated) |> Maybe.withDefault updated)
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
                          } |> (\updated -> Helpers.planningReadRevision guard model |> Maybe.map (\revision -> if Helpers.planningReadIsCurrent revision ("task:" ++ taskId) model then Helpers.applyMissingPlanningEntity revision "task" taskId updated else model) |> Maybe.withDefault model)
                        , Cmd.none
                        )

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalReadinessSummariesFetched workspaceId targets result ->
            let
                currentTargets = List.filter (\target -> canonicalGuardIsCurrent target.guard model) targets
                projectIds = targets |> List.filter (\target -> target.kind == "project") |> List.map .id
                taskIds = targets |> List.filter (\target -> target.kind == "task") |> List.map .id
                failure error =
                    case List.head currentTargets of
                        Just target -> canonicalHttpFailure target.guard error model
                        Nothing -> ( model, Cmd.none )
            in
            if model.selectedWorkspaceId /= Just workspaceId || List.isEmpty currentTargets then
                ( model, Cmd.none )
            else
                case result of
                    Ok summaries ->
                        if List.sort (List.map .id summaries.projects ++ summaries.missingProjectIds) /= List.sort projectIds
                            || List.sort (List.map .id summaries.tasks ++ summaries.missingTaskIds) /= List.sort taskIds
                            || not (List.all (\value -> value.workspaceId == workspaceId) summaries.projects)
                            || not (List.all (\value -> value.workspaceId == workspaceId) summaries.tasks) then
                            failure (Http.BadBody "Readiness summaries did not match their targeted request")
                        else
                            let
                                projects = Dict.fromList (List.map (\value -> ( value.id, value.readinessRollup )) summaries.projects)
                                tasks = Dict.fromList (List.map (\value -> ( value.id, value.readinessRollup )) summaries.tasks)
                                apply target current =
                                    case Helpers.planningReadRevision target.guard model of
                                        Nothing -> current
                                        Just revision ->
                                            if target.kind == "task" then
                                                case Dict.get target.id tasks of
                                                    Just rollup -> Helpers.applyTaskReadiness revision target.id rollup current
                                                    Nothing -> Helpers.applyMissingPlanningEntity revision "task" target.id current
                                            else
                                                case Dict.get target.id projects of
                                                    Just rollup -> Helpers.applyProjectReadiness revision target.id rollup current
                                                    Nothing -> Helpers.applyMissingPlanningEntity revision "project" target.id current
                            in
                            ( List.foldl apply model currentTargets, Cmd.none )
                    Err error -> failure error

        CanonicalNavigationSummariesFetched guard workspaceId projectIds taskIds result ->
            if model.selectedWorkspaceId == Just workspaceId && canonicalGuardIsCurrent guard.request model && guard.filterFingerprint == Feature.DataLoading.navigationFilterFingerprint model then
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
                                currentEntity kind entityId =
                                    let
                                        key =
                                            guard.request.scopeKey ++ "|navigation-summary:" ++ kind ++ ":" ++ entityId
                                    in
                                    Dict.get key guard.entityGenerations /= Nothing && Dict.get key guard.entityGenerations == Dict.get key model.webSocket.targetGenerations

                                currentMissing kind entityId =
                                    currentEntity kind entityId && (navigationSummaryReadRevision guard kind entityId model |> Maybe.map (\revision -> Helpers.planningReadIsCurrent revision (kind ++ ":" ++ entityId) model) |> Maybe.withDefault False)

                                currentSummaries =
                                    { projects = List.filter (\summary -> currentEntity "project" summary.id) summaries.projects
                                    , tasks = List.filter (\summary -> currentEntity "task" summary.id) summaries.tasks
                                    , missingProjectIds = List.filter (currentMissing "project") summaries.missingProjectIds
                                    , missingTaskIds = List.filter (currentMissing "task") summaries.missingTaskIds
                                    }

                                merged =
                                    let
                                        mergeProject summary current =
                                            navigationSummaryReadRevision guard "project" summary.id model
                                                |> Maybe.map (\revision -> Feature.DataLoading.mergeNavigationSummariesAt revision [ summary ] [] current) |> Maybe.withDefault current
                                        mergeTask summary current =
                                            navigationSummaryReadRevision guard "task" summary.id model
                                                |> Maybe.map (\revision -> Feature.DataLoading.mergeNavigationSummariesAt revision [] [ summary ] current) |> Maybe.withDefault current
                                    in List.foldl mergeTask (List.foldl mergeProject model currentSummaries.projects) currentSummaries.tasks

                                dependencies =
                                    merged.dependencies

                                loading =
                                    merged.dataLoading

                                -- A summary endpoint is authoritative about
                                -- deletion, but not descendant-retained branch
                                -- membership under filters.  Keep existing
                                -- cards visible until an authoritative branch
                                -- replay can decide their filtered placement.
                                staleProjectIds =
                                    currentSummaries.missingProjectIds

                                staleTaskIds =
                                    currentSummaries.missingTaskIds

                                mergedProjects =
                                    merged.projects

                                mergedTasks =
                                    merged.tasks

                                updated =
                                    { merged
                                        | projects = List.foldl Dict.remove mergedProjects currentSummaries.missingProjectIds
                                        , tasks = List.foldl Dict.remove mergedTasks currentSummaries.missingTaskIds
                                        , dataLoading =
                                            { loading
                                                | projectCardSummaries = List.foldl Dict.remove loading.projectCardSummaries currentSummaries.missingProjectIds
                                                , taskCardSummaries = List.foldl Dict.remove loading.taskCardSummaries currentSummaries.missingTaskIds

                                                -- Unknown summaries populate metadata only.
                                                -- Existing membership stays staged until its owner response.
                                                , navigationVisibleProjectIds = List.foldl Set.remove model.dataLoading.navigationVisibleProjectIds staleProjectIds
                                                , navigationVisibleTaskIds = List.foldl Set.remove model.dataLoading.navigationVisibleTaskIds staleTaskIds
                                            }
                                        , dependencies =
                                            { dependencies
                                                | projectReadinessRollups = List.foldl Dict.remove dependencies.projectReadinessRollups currentSummaries.missingProjectIds
                                                , taskReadinessRollups = List.foldl Dict.remove dependencies.taskReadinessRollups currentSummaries.missingTaskIds
                                            }
                                    }
                            in
                            let
                                missing kind id current = navigationSummaryReadRevision guard kind id model |> Maybe.map (\revision -> Helpers.applyMissingPlanningEntity revision kind id current) |> Maybe.withDefault current
                            in
                            deferMoveNavigation (Feature.DataLoading.revalidateNavigationForChangedSummaries model currentSummaries)
                                (List.foldl (missing "task") (List.foldl (missing "project") updated currentSummaries.missingProjectIds) currentSummaries.missingTaskIds)

                        else
                            canonicalHttpFailure guard.request (Http.BadBody "Navigation summary response did not match its targeted revalidation request") model

                    Err error ->
                        canonicalHttpFailure guard.request error model

            else
                ( model, Cmd.none )

        CanonicalCatalogueFetched guard result ->
            if canonicalGuardIsCurrent guard model then
                case result of
                    Ok paginated ->
                        let
                            workspaces =
                                List.foldl (\workspace -> if Set.member workspace.id model.groups.deletedWorkspaces then identity else Dict.insert workspace.id workspace) Dict.empty paginated.items
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
                        ( { model | groups = { groups | groupMembers = Dict.insert groupId (List.filter (\workspaceId -> not (Set.member workspaceId groups.deletedWorkspaces)) members) groups.groupMembers } }, Cmd.none )

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalMembershipsFetched guard workspaceId result ->
            if canonicalGuardIsCurrent guard model then
                case result of
                    Ok paginated ->
                        ( Feature.WorkspaceAdmin.acceptCanonicalMemberships workspaceId paginated.items model, Cmd.none )

                    Err error ->
                        canonicalHttpFailure guard error model

            else
                ( model, Cmd.none )

        CanonicalSessionFetched guard requestSessionEpoch expectedWorkspace result ->
            if canonicalGuardIsCurrent guard model && requestSessionEpoch == model.sessionRequestEpoch && expectedWorkspace == model.selectedWorkspaceId then
                ( model, Task.perform identity (Task.succeed (GotSessionContext requestSessionEpoch expectedWorkspace result)) )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


canonicalGuardIsCurrent : CanonicalRequestGuard -> Model -> Bool
canonicalGuardIsCurrent guard model =
    if guard.scopeKey == "global" then
        scopeAuthority ChangeStream.Global model
            |> Maybe.map (\session -> ChangeStream.requestGuardMatches guard model.groups.catalogueEpoch Nothing session.principal.actorId model.webSocket.targetGenerations)
            |> Maybe.withDefault False

    else
        workspaceGuardIsCurrent guard model


workspaceGuardIsCurrent : CanonicalRequestGuard -> Model -> Bool
workspaceGuardIsCurrent guard model =
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

        Api.CanonicalStatus apiScope _ ->
            applyScopedFrame (toPolicyScope apiScope) frame model

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

        step frame ( current, accumulated, pendingForeignRoots ) =
            let
                ( nextState, additions ) = ChangeStream.reduceFrame frame current
                roots =
                    case frame of
                        Api.CanonicalChange envelope ->
                            if envelope.entityType == "project"
                                && not (List.member envelope.eventId current.eventIds)
                                && List.member envelope.eventId nextState.eventIds
                                && not (Helpers.projectOperationMatches envelope.entityId envelope.requestId model) then
                                Set.insert envelope.entityId pendingForeignRoots
                            else pendingForeignRoots
                        _ -> pendingForeignRoots
            in
            ( nextState, accumulated ++ additions, roots )

        ( next, rawActions, foreignRoots ) =
            List.foldl step ( prior, [], Set.empty ) frames

        actions = ChangeStream.coalesce rawActions

        planningChanged =
            scopeMatchesSelectedWorkspace scope model
                && List.any (\frame -> case frame of
                    Api.CanonicalChange envelope ->
                        List.member envelope.entityType [ "project", "task" ]
                            && not (List.member envelope.eventId prior.eventIds)
                            && List.member envelope.eventId next.eventIds
                    _ -> False) frames

        streams =
            Dict.insert key next model.webSocket.streams

        connectionState =
            scopedConnectionState (requiredScopes model) streams

        updated =
            updateWebSocket (\webSocket -> { webSocket | streams = streams, state = connectionState }) model
                |> (\current -> if planningChanged || (scopeMatchesSelectedWorkspace scope model && next.live && not prior.live) then Feature.DataLoading.invalidatePlanningCounts current else current)
                |> (\current ->
                    case current.move.dialog of
                        Nothing -> current
                        Just dialog ->
                            let
                                supersedes frame =
                                    case frame of
                                        Api.CanonicalChange envelope ->
                                            not (List.member envelope.eventId prior.eventIds)
                                                && List.member envelope.eventId next.eventIds
                                                && (envelope.entityType == "project"
                                                    || (envelope.entityType == "task" && (envelope.entityId == dialog.entityId || Just envelope.entityId == dialog.taskParent)))
                                                && not (dialog.saving /= Nothing && envelope.requestId == dialog.saving)
                                        _ -> False
                                move = current.move
                            in
                            if scopeMatchesSelectedWorkspace scope current && List.any supersedes frames then
                                if dialog.saving /= Nothing then
                                    { current | move = { move | dialog = Nothing, resumeOrdinary = True } }
                                else
                                    { current | move = { move | dialog = Just { dialog | refreshRequested = True } } }
                            else current)
    in
    applyActions scope actions
        (if scopeMatchesSelectedWorkspace scope updated then
            Set.toList foreignRoots |> List.concatMap (\id -> loadedProjectCascadeTargets id updated)
                |> List.filter (Tuple.first >> (==) "project") |> List.map Tuple.second |> Set.fromList |> Set.toList
                |> List.foldl (\id current -> advanceCanonicalTarget scope ("entity:project:" ++ id) current) updated
         else updated)


{-| Picker and ordinary root revalidation cannot admit separate HTTP waves.
Keep canonical entity summaries current; defer ordinary membership until picker
transport has physically completed and the dialog retires.
-}
deferMoveNavigation : (Model -> ( Model, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
deferMoveNavigation refresh model =
    if model.move.dialog /= Nothing || not (Dict.isEmpty model.move.admissions) then
        let move = model.move in
        ( { model | move = { move | refreshOrdinary = True } }, Cmd.none )
    else refresh model


requiredScopes : Model -> List ChangeStream.Scope
requiredScopes model =
    (if scopeAuthority ChangeStream.Global model /= Nothing then
        [ ChangeStream.Global ]

     else
        []
    )
        ++ (case model.page of
                WorkspacePage workspaceId ->
                    if model.selectedWorkspaceId == Just workspaceId && Permissions.canReadCurrentWorkspace model then
                        [ ChangeStream.Workspace workspaceId ]

                    else
                        []

                _ ->
                    []
           )


scopedConnectionState : List ChangeStream.Scope -> Dict.Dict String ChangeStream.State -> WSState
scopedConnectionState scopes streams =
    let
        required =
            List.map (\scope -> Dict.get (ChangeStream.scopeKey scope) streams) scopes

        failure =
            required
                |> List.filterMap identity
                |> List.filterMap
                    (\stream ->
                        stream.failure
                            |> Maybe.map
                                (\reason ->
                                    "canonical:"
                                        ++ reason
                                        ++ ":"
                                        ++ (case stream.scope of
                                                ChangeStream.Global ->
                                                    "global:"

                                                ChangeStream.Workspace workspaceId ->
                                                    "workspace:" ++ workspaceId
                                           )
                                )
                    )
                |> List.head
    in
    case failure of
        Just reason ->
            ConnectionFailed reason

        Nothing ->
            if List.isEmpty scopes then
                Disconnected

            else if List.all (Maybe.map .live >> Maybe.withDefault False) required then
                Connected

            else
                Connecting


applyCanonicalSnapshot : ChangeStream.Scope -> String -> List Api.SnapshotItem -> String -> Model -> ( Model, Cmd Msg )
applyCanonicalSnapshot scope profile items token model =
    case ChangeStream.applySnapshotProfile scope profile items of
        Err _ ->
            beginScopedResync scope model

        Ok snapshot ->
            let
                withoutStale =
                    invalidateScopeRequests scope model
                        |> (\current -> if scopeMatchesSelectedWorkspace scope model then Feature.DataLoading.invalidatePlanningCounts current else current)

                key =
                    ChangeStream.scopeKey scope

                stream =
                    Dict.get key withoutStale.webSocket.streams
                        |> Maybe.withDefault (ChangeStream.init scope [])

                resumed =
                    { stream | resumeToken = Just token, snapshotApplied = True, eventIds = [], live = False, failure = Nothing }

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
                            | workspaces = (Dict.filter (\catalogueWorkspaceId _ -> not (Set.member catalogueWorkspaceId model.groups.deletedWorkspaces)) snapshot.workspaces)
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
                                { withStream | workspaces = Dict.union (Dict.filter (\catalogueWorkspaceId _ -> not (Set.member catalogueWorkspaceId model.groups.deletedWorkspaces)) snapshot.workspaces) withStream.workspaces }

                            initialHandoff =
                                not stream.snapshotApplied

                            retainedBootstrapRoot =
                                Feature.DataLoading.rootNavigationContextMatches workspaceId shellModel

                            ( navigationModel, navigationCmd ) =
                                if initialHandoff && retainedBootstrapRoot then
                                    -- The first shell completes the stream handoff;
                                    -- root REST may already have completed independently.
                                    ( shellModel, Cmd.none )

                                else
                                    Feature.DataLoading.revalidateNavigationForFilters shellModel

                            ( observationModel, observationCmd ) =
                                Observation.refreshActiveResults navigationModel
                        in
                        case observationModel.focus.focusedEntity of
                            Just ( entityType, entityId ) ->
                                let
                                    ( focusedModel, focusCmd ) =
                                        if initialHandoff then
                                            Feature.DataLoading.beginNavigationFocus workspaceId entityType entityId observationModel

                                        else
                                            Feature.DataLoading.revalidateNavigationFocus workspaceId entityType entityId observationModel
                                in
                                ( focusedModel, Cmd.batch [ navigationCmd, observationCmd, focusCmd ] )

                            Nothing ->
                                ( observationModel, Cmd.batch [ navigationCmd, observationCmd ] )

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
                                    | workspaces = Dict.union (Dict.filter (\catalogueWorkspaceId _ -> not (Set.member catalogueWorkspaceId model.groups.deletedWorkspaces)) snapshot.workspaces) withStream.workspaces
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
    List.filterMap identity [ observations.selectedId, Maybe.map .observationId observations.edit ]
        |> Set.fromList
        |> Set.toList
        |> List.foldl
            (\observationId current ->
                case Dict.get observationId canonicalById of
                    Just canonical ->
                        Observation.applyCanonicalObservation canonical current

                    Nothing ->
                        Observation.removeObservation observationId current
            )
            retained


beginScopedResync : ChangeStream.Scope -> Model -> ( Model, Cmd Msg )
beginScopedResync scope model =
    let
        invalidated =
            invalidateScopeRequests scope model

        cacheCleared =
            if scopeMatchesSelectedWorkspace scope invalidated then
                Feature.DataLoading.invalidateNavigationRequests { invalidated | dependencies = Dependencies.resetCache invalidated.dependencies }
                    |> Feature.DataLoading.invalidatePlanningCounts

            else
                invalidated
    in
    ( setWebSocketState Connecting cacheCleared
    , connectCmd model.flags (scopeAuthority scope model) scope True
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
                | streams = Dict.update (ChangeStream.scopeKey scope) (Maybe.map (\stream -> { stream | live = False, resumeToken = Nothing, failure = Nothing })) webSocket.streams
                , targetGenerations = Dict.map (\target generation -> if String.startsWith prefix target || (scope == ChangeStream.Global && String.contains "|session-resource:" target) then generation + 1 else generation) webSocket.targetGenerations
            }
        )
        (case scope of
            ChangeStream.Workspace workspace -> { model | sessionDelivery = SessionDelivery.invalidateWorkspace workspace model.sessionDelivery }
            ChangeStream.Global -> { model | sessionDelivery = SessionDelivery.invalidateAll model.sessionDelivery })


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

                        ChangeStream.RefreshNavigation ->
                            False

                        _ ->
                            True
                )
                actions

        ( navigationModel, navigationCmd ) =
            if scopeMatchesSelectedWorkspace scope model && List.member ChangeStream.RefreshNavigation actions then
                deferMoveNavigation Feature.DataLoading.revalidateNavigationForFilters model

            else
                ( model, Cmd.none )

        cascadeRoots =
            navigationTargets |> List.filterMap (\( kind, id ) -> if kind == "project" then Just id else Nothing)

        cascadeTargets =
            List.concatMap (\id -> loadedProjectCascadeTargets id navigationModel) cascadeRoots

        ( revalidationModel, revalidationCmd ) =
            if scopeMatchesSelectedWorkspace scope model then
                requestNavigationSummaryBatches scope (navigationTargets ++ cascadeTargets) (invalidateCascadeRequests scope cascadeTargets navigationModel)

            else
                ( model, Cmd.none )
    in
    let
        readinessTargets = List.concatMap (\action -> case action of
            ChangeStream.RefreshTaskDependencies taskId _ _ _ -> dependencyReadinessTargets taskId model
            _ -> []) actions
        ( acted, actionCmd ) = List.foldl (applyAction scope) ( revalidationModel, Cmd.batch [ navigationCmd, revalidationCmd ] ) otherActions
        ( refreshed, readinessCmd ) =
            if scopeMatchesSelectedWorkspace scope acted then requestReadinessSummaryBatches scope readinessTargets acted
            else ( acted, Cmd.none )
    in ( refreshed, Cmd.batch [ actionCmd, readinessCmd ] )


applyAction : ChangeStream.Scope -> ChangeStream.Action -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
applyAction scope action ( model, accumulated ) =
    let
        append command nextModel =
            ( nextModel, Cmd.batch [ accumulated, command ] )
    in
    case action of
        ChangeStream.RefreshAgentSession workspace id ->
            if scope == ChangeStream.Workspace workspace && scopeMatchesSelectedWorkspace scope model then
                ( { model | sessionDelivery = SessionDelivery.invalidateSession workspace id model.sessionDelivery }, accumulated )
            else ( model, accumulated )

        ChangeStream.RefreshAgentSessionList workspace ->
            if scope == ChangeStream.Workspace workspace && scopeMatchesSelectedWorkspace scope model then
                ( { model | sessionDelivery = SessionDelivery.invalidateLists workspace model.sessionDelivery }, accumulated )
            else ( model, accumulated )

        ChangeStream.RefreshTaskSessions workspace task ->
            if scope == ChangeStream.Workspace workspace && scopeMatchesSelectedWorkspace scope model then
                ( { model | sessionDelivery = SessionDelivery.invalidateTask workspace task model.sessionDelivery }, accumulated )
            else ( model, accumulated )

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

        ChangeStream.RefreshNavigation ->
            ( model, accumulated )

        ChangeStream.RefreshNextTasks workspaceId ->
            if model.selectedWorkspaceId == Just workspaceId then
                let
                    ids = Dict.keys model.cards.projectNextTasks ++ Dict.keys model.cards.projectNextTaskDiagnostics ++ Dict.keys model.cards.projectNextTasksLoading ++ Dict.keys model.cards.projectNextTaskDiagnosticsLoading |> Set.fromList |> Set.toList
                    invalidated = List.foldl
                        (\id current -> advanceCanonicalTarget scope ("project-next:" ++ id) current |> advanceCanonicalTarget scope ("project-next-diagnostics:" ++ id)) model ids
                in
                ( invalidated, Cmd.batch (accumulated :: List.map (refreshProjectNextTaskCache invalidated) ids) )
            else
                append Cmd.none model

        ChangeStream.RefreshSearch _ ->
            if scopeMatchesSelectedWorkspace scope model then
                let
                    ( refreshed, command ) = Feature.Search.refreshAcceptedSearch model
                in
                append command refreshed

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
            requestCanonical scope "session" (\guard -> Api.fetchSessionContext model.flags.apiUrl model.selectedWorkspaceId (CanonicalSessionFetched guard model.sessionRequestEpoch model.selectedWorkspaceId)) accumulated model

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
                    scopedConnectionState (List.filter ((/=) revokedScope) (requiredScopes model)) remainingStreams

                scopedWebSocket =
                    { webSocket
                        | state = remainingState
                        , streams = remainingStreams
                        , targetGenerations = Dict.map (\target generation -> if String.startsWith scopePrefix target then generation + 1 else generation) webSocket.targetGenerations
                    }

                scopeCommand =
                    Cmd.batch [ clearChangeStreamScope (scopePortValue model.sessionContext revokedScope), disconnectChangeStreamScope (scopePortValue model.sessionContext revokedScope) ]

                cleared =
                    { model
                        | workspaces = Dict.remove workspaceId model.workspaces
                        , projects = Dict.empty
                        , tasks = Dict.empty
                        , observations = Observation.clearSelection { observations | items = Dict.empty, orderedIds = [], matchEvidence = Dict.empty, edit = Nothing, failedRequest = Nothing }
                        , dependencies = Dependencies.resetCache model.dependencies
                        , cards = { cards | projectNextTasks = Dict.empty, projectNextTaskDiagnostics = Dict.empty, projectNextTasksLoading = Dict.empty, projectNextTaskDiagnosticsLoading = Dict.empty, projectNextTasksErrors = Dict.empty, projectNextTaskDiagnosticsErrors = Dict.empty }
                        , sessionRequestEpoch = model.sessionRequestEpoch + 1
                        , sessionDelivery = SessionDelivery.retireWorkspace workspaceId model.sessionDelivery
                        , timeline = Timeline.reset (model.sessionRequestEpoch + 1)
                        , webSocket =
                            scopedWebSocket
                    }
            in
            if model.selectedWorkspaceId == Just workspaceId then
                append scopeCommand cleared

            else
                append scopeCommand { model | workspaces = Dict.remove workspaceId model.workspaces, webSocket = scopedWebSocket, sessionDelivery = SessionDelivery.retireWorkspace workspaceId model.sessionDelivery }

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

        canonical =
            advanceCanonicalTarget scope target model

        withGeneration =
            if scopeMatchesSelectedWorkspace scope canonical then
                -- The accepted event is already deletion authority; a delayed
                -- focus page must not wait for a summary HTTP result to see it.
                case entity of
                    "project" -> Feature.DataLoading.markMissingNavigationSummaries [ identity ] [] canonical
                    "task" -> Feature.DataLoading.markMissingNavigationSummaries [] [ identity ] canonical
                    _ -> canonical

            else
                canonical

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


prepareCanonicalRequest : ChangeStream.Scope -> String -> Model -> Maybe ( CanonicalRequestGuard, Model )
prepareCanonicalRequest scope targetKey model =
    scopeAuthority scope model |> Maybe.map (\session ->
        let
            target = ChangeStream.scopeKey scope ++ "|" ++ targetKey
            generation = Dict.get target model.webSocket.targetGenerations |> Maybe.withDefault 0 |> (+) 1
            guard =
                { scopeKey = ChangeStream.scopeKey scope, targetKey = targetKey, targetGeneration = generation
                , sessionEpoch = if scope == ChangeStream.Global then model.groups.catalogueEpoch else model.sessionRequestEpoch
                , routeWorkspace = if scope == ChangeStream.Global then Nothing else model.selectedWorkspaceId
                , audienceId = session.principal.actorId
                }
            updated = updateWebSocket (\webSocket -> { webSocket | targetGenerations = Dict.insert target generation webSocket.targetGenerations }) model
            captured =
                if List.any (\prefix -> String.startsWith prefix targetKey) [ "task-overview:", "dependency-readiness:task:", "project-overview:" ] then
                    Helpers.capturePlanningRead target generation updated
                else updated
        in ( guard, captured ))


requestCanonical : ChangeStream.Scope -> String -> (CanonicalRequestGuard -> Cmd Msg) -> Cmd Msg -> Model -> ( Model, Cmd Msg )
requestCanonical scope targetKey makeCommand accumulated model =
    case prepareCanonicalRequest scope targetKey model of
        Nothing ->
            let ( next, command ) = beginScopedResync scope model
            in ( next, Cmd.batch [ accumulated, command ] )
        Just ( guard, updated ) ->
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
        -- Fetch every invalidated ID, including uncached cards. Metadata alone
        -- cannot admit membership; affected owner responses remain authoritative.
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
                entityModel =
                    List.foldl (\( entityType, entityId ) currentModel ->
                        let target = "navigation-summary:" ++ entityType ++ ":" ++ entityId
                            key = ChangeStream.scopeKey scope ++ "|" ++ target
                            advanced = advanceCanonicalTarget scope target currentModel
                        in Helpers.capturePlanningRead key (Dict.get key advanced.webSocket.targetGenerations |> Maybe.withDefault 0) advanced)
                        (Helpers.invalidateTaskPrerequisiteEligibility taskIds current) batch

                entityGenerations =
                    batch |> List.filterMap (\( entityType, entityId ) ->
                        let
                            key = ChangeStream.scopeKey scope ++ "|navigation-summary:" ++ entityType ++ ":" ++ entityId
                        in
                        Dict.get key entityModel.webSocket.targetGenerations |> Maybe.map (Tuple.pair key)) |> Dict.fromList
            in
            requestCanonical scope
                targetKey
                (\guard ->
                    Api.fetchNavigationSummaries current.flags.apiUrl
                        workspaceId
                        projectIds
                        taskIds
                        (CanonicalNavigationSummariesFetched { request = guard, entityGenerations = entityGenerations, navigationGeneration = entityModel.dataLoading.navigationGeneration, filterFingerprint = Feature.DataLoading.navigationFilterFingerprint entityModel } workspaceId projectIds taskIds)
                )
                accumulated
                entityModel
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


navigationSummaryReadRevision : CanonicalNavigationRequestGuard -> String -> String -> Model -> Maybe Int
navigationSummaryReadRevision guard kind id model =
    let
        request = guard.request
        target = "navigation-summary:" ++ kind ++ ":" ++ id
    in
    Helpers.planningReadRevision { request | targetKey = target, targetGeneration = Dict.get (request.scopeKey ++ "|" ++ target) guard.entityGenerations |> Maybe.withDefault -1 } model


dependencyReadinessTargets : String -> Model -> List ( String, String )
dependencyReadinessTargets taskId model =
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

    in
    List.map (Tuple.pair "task") taskAncestors ++ List.map (Tuple.pair "project") projectAncestors


{-| Raw IDs must be unique across both kinds within each at-most-100 ID batch.
A coincident project/task UUID therefore starts another batch, rather than
silently losing one target or violating the summaries API contract.
-}
readinessTargetBatches : List ( String, String ) -> List (List ( String, String ))
readinessTargetBatches targets =
    let
        unique = List.foldl (\target ( seen, values ) -> if Set.member target seen then ( seen, values ) else ( Set.insert target seen, target :: values )) ( Set.empty, [] ) targets |> Tuple.second |> List.reverse
        collect target ( completed, current, ids ) =
            if List.length current == 100 || Set.member (Tuple.second target) ids then
                ( List.reverse current :: completed, [ target ], Set.singleton (Tuple.second target) )
            else ( completed, target :: current, Set.insert (Tuple.second target) ids )
        ( batches, final, _ ) = List.foldl collect ( [], [], Set.empty ) unique
    in
    List.reverse (if List.isEmpty final then batches else List.reverse final :: batches)


requestReadinessSummaryBatches : ChangeStream.Scope -> List ( String, String ) -> Model -> ( Model, Cmd Msg )
requestReadinessSummaryBatches scope targets model =
    let
        prepare ( kind, id ) ( current, prepared ) =
            let targetKey = (if kind == "task" then "dependency-readiness:task:" else "project-overview:") ++ id
            in case prepareCanonicalRequest scope targetKey current of
                Just ( guard, updated ) -> ( updated, { kind = kind, id = id, guard = guard } :: prepared )
                Nothing -> ( current, prepared )
        dispatch workspaceId batch ( current, commands ) =
            let
                ( prepared, reversed ) = List.foldl prepare ( Helpers.invalidateTaskPrerequisiteEligibility (batch |> List.filter (Tuple.first >> (==) "task") |> List.map Tuple.second) current, [] ) batch
                guarded = List.reverse reversed
                ids kind = guarded |> List.filter (\target -> target.kind == kind) |> List.map .id
            in
            ( prepared, Cmd.batch [ commands, Api.fetchNavigationSummaries model.flags.apiUrl workspaceId (ids "project") (ids "task") (CanonicalReadinessSummariesFetched workspaceId guarded) ] )
    in
    if List.isEmpty targets then ( model, Cmd.none )
    else case ( scope, scopeAuthority scope model ) of
        ( ChangeStream.Workspace workspaceId, Just _ ) ->
            List.foldl (dispatch workspaceId) ( model, Cmd.none ) (readinessTargetBatches targets)
        _ -> beginScopedResync scope model


revalidateDependencyReadiness : ChangeStream.Scope -> String -> String -> Bool -> Model -> ( Model, Cmd Msg )
revalidateDependencyReadiness scope taskId dependsOnId present model =
    let
        targets = ( "task", taskId ) :: dependencyReadinessTargets taskId model
        dependencies = model.dependencies
        invalidated =
            { model | dependencies =
                { dependencies
                    | taskReadinessRollups = List.foldl (\( kind, id ) rollups -> if kind == "task" then Dict.remove id rollups else rollups) dependencies.taskReadinessRollups targets
                    , projectReadinessRollups = List.foldl (\( kind, id ) rollups -> if kind == "project" then Dict.remove id rollups else rollups) dependencies.projectReadinessRollups targets
                }
            }
    in
    if present && not (Dict.member dependsOnId invalidated.tasks) then requestEntity scope "task" dependsOnId Cmd.none invalidated
    else ( invalidated, Cmd.none )


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
                [ Helpers.taskOverviewCmd model taskId
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
                |> List.map (\taskId -> Helpers.taskOverviewCmd model taskId)

        projectReadinessCmds =
            model.dependencies.projectReadinessRollups
                |> Dict.keys
                |> List.map (\projectId -> Helpers.projectOverviewCmd model projectId)

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
        |> List.map (\taskId -> Helpers.taskOverviewCmd model taskId)
        |> Cmd.batch


refreshProjectReadinessCaches : Model -> Cmd Msg
refreshProjectReadinessCaches model =
    model.dependencies.projectReadinessRollups
        |> Dict.keys
        |> List.map (\projectId -> Helpers.projectOverviewCmd model projectId)
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
        [ Helpers.projectNextTasksCmd model projectId False
        , Helpers.projectNextTasksCmd model projectId True
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
    " by " ++ ActorAttribution.label event

scopeAuthority : ChangeStream.Scope -> Model -> Maybe Api.SessionContext
scopeAuthority scope model =
    case scope of
        ChangeStream.Global ->
            if model.auth.status == AuthReady || model.auth.status == AuthBooting then
                (if model.groups.catalogueOwner /= Nothing then model.groups.catalogueOwner else if model.auth.status == AuthReady then model.sessionContext else Nothing) |> Maybe.andThen (\session -> if session.globalPermissions.superadmin then Just session else Nothing)

            else
                Nothing

        ChangeStream.Workspace _ ->
            if model.auth.status == AuthReady then model.sessionContext else Nothing


loadedProjectCascadeTargets : String -> Model -> List ( String, String )
loadedProjectCascadeTargets root model =
    let
        parent id =
            Dict.get id model.dataLoading.projectCardSummaries |> Maybe.map .parentId
                |> Maybe.withDefault (Dict.get id model.projects |> Maybe.andThen .parentId)
        within visited id =
            if id == root then True
            else if Set.member id visited then False
            else parent id |> Maybe.map (within (Set.insert id visited)) |> Maybe.withDefault False
        projectIds = Dict.keys model.dataLoading.projectCardSummaries ++ Dict.keys model.projects |> Set.fromList |> Set.toList |> List.filter (within Set.empty)
        taskProjects = Dict.values model.dataLoading.taskCardSummaries |> List.map (\task -> ( task.id, task.projectId ))
            |> (++) (Dict.values model.tasks |> List.map (\task -> ( task.id, task.projectId )))
        taskIds = taskProjects |> List.filter (Tuple.second >> Maybe.map (within Set.empty) >> Maybe.withDefault False) |> List.map Tuple.first |> Set.fromList |> Set.toList
    in
    List.map (Tuple.pair "project") projectIds ++ List.map (Tuple.pair "task") taskIds


refreshProjectCascade : String -> Model -> ( Model, Cmd Msg )
refreshProjectCascade root model =
    case model.selectedWorkspaceId of
        Nothing ->
            ( model, Cmd.none )

        Just workspaceId ->
            let
                ( navigationModel, navigationCmd ) = deferMoveNavigation Feature.DataLoading.revalidateNavigationForFilters model
                targets = loadedProjectCascadeTargets root model
                ( summaryModel, summaryCmd ) = requestNavigationSummaryBatches (ChangeStream.Workspace workspaceId) targets (invalidateCascadeRequests (ChangeStream.Workspace workspaceId) targets navigationModel)
            in
            ( summaryModel, Cmd.batch [ navigationCmd, summaryCmd, refreshProjectNextTaskCaches summaryModel ] )


invalidateCascadeRequests : ChangeStream.Scope -> List ( String, String ) -> Model -> Model
invalidateCascadeRequests scope targets model =
    List.foldl
        (\( kind, id ) current ->
            if kind == "project" then
                current |> advanceCanonicalTarget scope ("project-overview:" ++ id)
                    |> advanceCanonicalTarget scope ("project-next:" ++ id)
                    |> advanceCanonicalTarget scope ("project-next-diagnostics:" ++ id)
            else
                current)
        model targets


{- Reconcile loaded task descendants, old parent and both project scopes after
placement acknowledgement, including no-project placements.
-}
{-| Client read admission is not server commit order. Refresh unsafe mutation
payloads through the existing bounded summary, membership and detail paths.
-}
refreshMutationEntities : List ( String, String ) -> List ( String, String ) -> Model -> ( Model, Cmd Msg )
refreshMutationEntities targets rejected model =
    case model.selectedWorkspaceId of
        Nothing -> ( model, Cmd.none )
        Just workspaceId ->
            let
                loading = model.dataLoading
                invalidated = { model | dataLoading = { loading
                    | taskCardDetailRequests = List.foldl (\( kind, id ) requests -> if kind == "task" then Dict.remove id requests else requests) loading.taskCardDetailRequests rejected
                    , projectCardDetailRequests = List.foldl (\( kind, id ) requests -> if kind == "project" then Dict.remove id requests else requests) loading.projectCardDetailRequests rejected
                    } }
                ( navigation, navigationCmd ) = deferMoveNavigation Feature.DataLoading.revalidateNavigationForFilters invalidated
                owners = targets |> List.concatMap (\( kind, id ) -> if kind == "task" then
                    (Dict.get id model.tasks |> Maybe.map (\task -> List.filterMap identity [ Maybe.map (Tuple.pair "task") task.parentId, Maybe.map (Tuple.pair "project") task.projectId ]) |> Maybe.withDefault [])
                    else [])
                ( refreshed, summaryCmd ) = requestNavigationSummaryBatches (ChangeStream.Workspace workspaceId) (Set.fromList (targets ++ owners) |> Set.toList) navigation
                refreshedLoading = refreshed.dataLoading
                retain kind id request = request.succeeded && not request.inFlight && request.workspaceId == workspaceId && request.sessionEpoch == model.sessionRequestEpoch && not (List.member ( kind, id ) rejected)
                rebase _ request = { request | navigationGeneration = refreshedLoading.navigationGeneration }
                current = { refreshed | dataLoading = { refreshedLoading
                    | projectCardDetailRequests = Dict.union refreshedLoading.projectCardDetailRequests (Dict.filter (retain "project") loading.projectCardDetailRequests |> Dict.map rebase)
                    , taskCardDetailRequests = Dict.union refreshedLoading.taskCardDetailRequests (Dict.filter (retain "task") loading.taskCardDetailRequests |> Dict.map rebase)
                    } }
            in
            ( current, Cmd.batch [ navigationCmd, summaryCmd, refreshProjectNextTaskCaches current ] )


refreshTaskPlacement : String -> Maybe String -> Maybe String -> Maybe String -> Model -> ( Model, Cmd Msg )
refreshTaskPlacement taskId oldParent source destination model =
    case model.selectedWorkspaceId of
        Nothing -> ( model, Cmd.none )
        Just workspaceId ->
            let
                parent id = Dict.get id model.tasks |> Maybe.andThen .parentId
                    |> (\known -> if known /= Nothing then known else Dict.get id model.dataLoading.taskCardSummaries |> Maybe.andThen .parentId)
                within visited id =
                    id == taskId || (not (Set.member id visited) && (parent id |> Maybe.map (within (Set.insert id visited)) |> Maybe.withDefault False))
                taskIds = Dict.keys model.tasks ++ Dict.keys model.dataLoading.taskCardSummaries |> Set.fromList |> Set.toList |> List.filter (within Set.empty)
                projectTargets = List.filterMap identity [ source, destination ] |> Set.fromList |> Set.toList |> List.concatMap (\id -> loadedProjectCascadeTargets id model)
                targets = List.map (Tuple.pair "task") (taskIds ++ (oldParent |> Maybe.map List.singleton |> Maybe.withDefault [])) ++ projectTargets |> Set.fromList |> Set.toList
                ( navigation, navigationCmd ) = deferMoveNavigation Feature.DataLoading.revalidateNavigationForFilters model
                ( summaries, summaryCmd ) = requestNavigationSummaryBatches (ChangeStream.Workspace workspaceId) targets (invalidateCascadeRequests (ChangeStream.Workspace workspaceId) targets navigation)
            in
            ( summaries, Cmd.batch [ navigationCmd, summaryCmd, refreshProjectNextTaskCaches summaries ] )


pumpSessionResources : Model -> ( Model, Cmd Msg )
pumpSessionResources model =
    case model.selectedWorkspaceId of
        Just workspace ->
            if model.auth.status == AuthReady && model.page == WorkspacePage workspace && Permissions.canReadCurrentWorkspace model && scopeAuthority (ChangeStream.Workspace workspace) model /= Nothing then
                let
                    ( admitted, requests ) = SessionDelivery.admit workspace model.sessionDelivery
                    issue request ( current, pendingCommands ) =
                        case prepareCanonicalRequest (ChangeStream.Workspace workspace) ("session-resource:" ++ request.key) current of
                            Just ( guard, guarded ) ->
                                ( guarded, SessionDelivery.fetch model.flags.apiUrl request (SessionResourceFetched request.id guard) :: pendingCommands )
                            Nothing ->
                                ( { current | sessionDelivery = SessionDelivery.settle request.id False (Err (Http.BadBody "Retired session authority")) current.sessionDelivery }, pendingCommands )
                    ( updated, commands ) = List.foldl issue ( { model | sessionDelivery = admitted }, [] ) requests
                in
                ( updated, Cmd.batch commands )
            else
                ( model, Cmd.none )
        Nothing ->
            ( model, Cmd.none )
