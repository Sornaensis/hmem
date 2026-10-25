module ChangeStreamTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.ChangeStream as ChangeStream
import Feature.SessionDelivery as Delivery
import Feature.WebSocket as WebSocket
import Http
import Json.Encode as Encode
import Test exposing (Test, describe, test)
import Types exposing (AuthStatus(..), Model, Msg(..), Page(..), WSState(..), WorkspaceTab(..))
import Url


suite : Test
suite =
    describe "canonical change stream policy"
        [ test "activity controls validate workspace identity without advancing canonical events or checkpoint" <| \_ ->
            let
                workspace="90000000-0000-4000-8000-000000000001"
                initial=ChangeStream.init ChangeStream.Global []
                (next,actions)=ChangeStream.reduceFrame (Api.CanonicalSessionActivityChanged workspace) initial
                malformed=Api.decodeCanonicalFrame """{"type":"session_activity_changed","schema_version":1,"workspace_id":"invalid"}"""
            in Expect.equal (initial,[ChangeStream.RefreshSessionActivity workspace],Nothing) (next,actions,malformed)
        , test "session wire names decode and duplicated out-of-order metadata/link events coalesce into dedicated scoped refreshes" <|
            \_ ->
                let
                    linked = sessionEvent True
                    metadata = sessionEvent False
                    actions = ChangeStream.reduceFrames
                        [ Api.CanonicalChange linked, Api.CanonicalChange metadata, Api.CanonicalChange linked ] (ChangeStream.init (ChangeStream.Workspace "w") []) |> Tuple.second
                    wire event = canonicalWire event.entityAction (List.map (\item -> ( item.kind, item.target )) event.invalidations)
                        |> String.replace "\"type\":\"task\"" ("\"type\":\"" ++ event.entityType ++ "\"")
                        |> String.replace "\"id\":\"t\"" ("\"id\":\"" ++ event.entityId ++ "\"")
                in
                Expect.equal
                    { decoded = [True,True], actions = [ ChangeStream.RefreshAgentSession "w" agentId, ChangeStream.RefreshTaskSessions "w" linkedTaskId, ChangeStream.RefreshAgentSessionList "w" ] }
                    { decoded = List.map (wire >> Api.decodeCanonicalFrame >> (/=) Nothing) [linked,metadata], actions = actions }
        , test "ordinary session updates refresh progress without broad membership or activity reads" <| \_ ->
            let
                event = sessionEvent False
                progress = ChangeStream.reduceFrame (Api.CanonicalChange event) (ChangeStream.init (ChangeStream.Workspace "w") []) |> Tuple.second
                membership = {event | invalidations=event.invalidations ++ [{kind="collection",target="active_sessions:w"}]}
                changed = ChangeStream.reduceFrame (Api.CanonicalChange membership) (ChangeStream.init (ChangeStream.Workspace "w") []) |> Tuple.second
            in Expect.equal {progress=[ChangeStream.RefreshAgentSessionProgress "w" agentId],lists=True,activity=True,membershipProgress=False}
                {progress=progress,lists=List.member (ChangeStream.RefreshAgentSessionList "w") changed,activity=List.member (ChangeStream.RefreshSessionActivityMembership "w") changed,membershipProgress=List.member (ChangeStream.RefreshAgentSessionProgress "w" agentId) changed}
        , test "session composites, invalidation ownership and global scope fail closed without generic entity reads" <|
            \_ ->
                let
                    original = sessionEvent True
                    bad = [ { original | entityId = original.entityId ++ ":extra" }
                        , { original | entityId = "not-a-uuid:" ++ linkedTaskId }
                        , { original | invalidations = List.map (\item -> if item.target == "sessions:w" then { item | target = "sessions:other" } else item) original.invalidations }
                        , { original | scope = Api.GlobalScope, workspaceId = Nothing } ]
                    reduced event = ChangeStream.reduceFrame (Api.CanonicalChange event) (ChangeStream.init (if event.scope == Api.GlobalScope then ChangeStream.Global else ChangeStream.Workspace "w") []) |> Tuple.second
                in
                Expect.equal (List.repeat 4 [ChangeStream.BeginResync]) (List.map reduced bad)
        , test "new demand enters once and progress invalidation preserves a proven page without releasing physical occupancy" <|
            \_ ->
                let
                    target = Delivery.WorkspaceSessions Nothing
                    wanted = Delivery.init |> Delivery.demand "w" target |> Delivery.demand "w" target
                    ( admitted, requests ) = Delivery.admit "w" wanted
                in
                case requests of
                    [request] ->
                        let
                            dirty = Delivery.invalidateLists "w" admitted
                            ( blocked, duplicates ) = Delivery.admit "w" dirty
                            released = Delivery.settle request.id True (Ok (Delivery.SessionsPage {items=[],nextCursor=Nothing})) blocked
                            ( fresh, refreshed ) = Delivery.admit "w" released
                        in
                        Expect.equal { queued = 1, occupied = 1, duplicateReads = 0, cached = Just (Delivery.SessionsPage {items=[],nextCursor=Nothing}), refreshes = 1, remaining = 1 }
                            { queued = List.length wanted.order, occupied = Dict.size blocked.physical, duplicateReads = List.length duplicates
                            , cached = Dict.get request.key released.resources |> Maybe.andThen .data
                            , refreshes = List.length refreshed, remaining = Dict.size fresh.physical }
                    _ -> Expect.fail "Initial demand did not admit exactly one bounded request"
        , test "focus ceiling rejects atomically and scope retirement retains both physical slots until exact late callbacks" <|
            \_ ->
                let
                    fifteen = List.range 1 15 |> List.foldl (\id -> Delivery.demand "w" (Delivery.SessionMetadata (String.fromInt id))) Delivery.init
                    rejected = Delivery.demand "w" (Delivery.SessionBytes agentId 0 False) fifteen
                    ( busy, old ) = Delivery.admit "w" rejected
                    switched = Delivery.retireAll busy |> Delivery.demand "other" (Delivery.WorkspaceSessions Nothing)
                    ( blocked, none ) = Delivery.admit "other" switched
                    drained = List.foldl (\request -> Delivery.settle request.id False (Err Http.Timeout)) blocked old
                    repeated = List.foldl (\request -> Delivery.settle request.id False (Err Http.Timeout)) drained old
                    ( fresh, requests ) = Delivery.admit "other" repeated
                in
                Expect.equal { rejected = Just "demand_limit", targets = 15, occupied = 2, premature = 0, drained = 0, fresh = 1, monotonic = True }
                    { rejected = rejected.admissionError, targets = Dict.size rejected.resources, occupied = Dict.size blocked.physical
                    , premature = List.length none, drained = Dict.size repeated.physical, fresh = List.length requests
                    , monotonic = fresh.nextId > busy.nextId }
        , test "focused reads follow only missing byte offsets and preserve bounded exact base64 windows" <|
            \_ ->
                let
                    start = Delivery.demand "w" (Delivery.SessionBytes agentId 0 False) Delivery.init
                    withMetadata = deliverOne (Delivery.Metadata (agentMetadata 6)) start
                    first = deliverOne (Delivery.Bytes {session=agentMetadata 6,offset=0,dataBase64="e30K",nextOffset=3,hasMore=True}) withMetadata
                    caughtUp = deliverOne (Delivery.Bytes {session=agentMetadata 6,offset=3,dataBase64="W10K",nextOffset=6,hasMore=False}) first
                    changed = Delivery.invalidateSession "w" agentId caughtUp |> deliverOne (Delivery.Metadata (agentMetadata 9))
                    ( _, missing ) = Delivery.admit "w" changed
                    range = Dict.get (Delivery.key "w" (Delivery.SessionBytes agentId 0 False)) caughtUp.resources
                in
                Expect.equal { target = [Delivery.SessionBytes agentId 6 False], windows = Just ["W10K","e30K"], settled = Just True }
                    { target = List.map .target missing, windows = Maybe.map (.ranges >> List.map .dataBase64) range
                    , settled = Maybe.map (\resource -> resource.dirty == resource.settled) range }
        , test "malformed, short, noncanonical, empty-advance and false-final ranges never advance the evidence cursor" <|
            \_ ->
                let
                    prepared = Delivery.demand "w" (Delivery.SessionBytes agentId 0 False) Delivery.init |> deliverOne (Delivery.Metadata (agentMetadata 65536))
                    invalid = [ {session=agentMetadata 65536,offset=0,dataBase64="",nextOffset=65536,hasMore=False}
                        , {session=agentMetadata 65536,offset=0,dataBase64="e30K",nextOffset=4,hasMore=True}
                        , {session=agentMetadata 65536,offset=0,dataBase64="AB==",nextOffset=1,hasMore=True}
                        , {session=agentMetadata 65536,offset=0,dataBase64="e30K",nextOffset=3,hasMore=False}
                        , {session=agentMetadata 65536,offset=0,dataBase64="e30K\n",nextOffset=3,hasMore=True} ]
                    rejected range = prepared |> deliverOne (Delivery.Bytes range) |> .resources |> Dict.get (Delivery.key "w" (Delivery.SessionBytes agentId 0 False))
                        |> Maybe.map (\resource -> resource.target == Delivery.SessionBytes agentId 0 False && resource.error == Just "invalid_response" && List.isEmpty resource.ranges) |> Maybe.withDefault False
                    valid = prepared |> deliverOne (Delivery.Bytes {session=agentMetadata 65536,offset=0,dataBase64="8J+Y",nextOffset=3,hasMore=True})
                        |> .resources |> Dict.get (Delivery.key "w" (Delivery.SessionBytes agentId 0 False)) |> Maybe.map .target
                in
                Expect.equal ( List.repeat 5 True, Just (Delivery.SessionBytes agentId 3 False) ) ( List.map rejected invalid, valid )
        , test "accepted old full, root-only full/shell and empty global snapshots retire session responses while keeping admission" <|
            \_ ->
                let
                    source = WebSocket.update (DemandSessionResource "a" (Delivery.WorkspaceSessions Nothing)) streamModel |> Tuple.first
                    requests = Dict.values source.sessionDelivery.physical
                    root = Encode.object [("schema_version",Encode.int 1),("kind",Encode.string "workspace"),("data",(workspaceItem "a").data)]
                    snapshot scope profile = receiveTransport
                        ([ ("transport",Encode.string "snapshot"),("scope",scopeValue scope)
                         , ("items",Encode.list identity (if scope == "global" then [] else [root])),("resume_token",Encode.string "old-valid-token") ]
                            ++ (profile |> Maybe.map (\name -> [("snapshot_profile",Encode.string name)]) |> Maybe.withDefault [])) source
                    accepted = [snapshot "workspace" Nothing,snapshot "workspace" (Just "full_v1"),snapshot "workspace" (Just "workspace_shell_v1"),snapshot "global" (Just "full_v1")]
                    check after = case requests of
                        [request] ->
                            let
                                late = WebSocket.update (SessionResourceFetched request.id (sessionGuard source request) (Ok (Delivery.SessionsPage {items=[],nextCursor=Nothing}))) after |> Tuple.first
                            in
                            Dict.size after.sessionDelivery.physical == 1 && Dict.size late.sessionDelivery.physical == 1
                                && late.sessionDelivery.nextId > after.sessionDelivery.nextId
                                && (Dict.get request.key late.sessionDelivery.resources |> Maybe.andThen .data) == Nothing
                        _ -> False
                in
                Expect.equal (List.repeat 4 True) (List.map check accepted)
        , test "closed and unauthorized session demand releases stale callbacks without scheduling refresh" <|
            \_ ->
                let
                    target = Delivery.WorkspaceSessions Nothing
                    source = WebSocket.update (DemandSessionResource "a" target) streamModel |> Tuple.first
                in
                case Dict.values source.sessionDelivery.physical of
                    [request] ->
                        let
                            closed = WebSocket.update (RemoveSessionResource "a" target) source |> Tuple.first
                            unauthorized = { source | auth = {status=AuthRequired,mode=Nothing} }
                            finish model = WebSocket.update (SessionResourceFetched request.id (sessionGuard source request) (Ok (Delivery.SessionsPage {items=[],nextCursor=Nothing}))) model |> Tuple.first
                            done = finish closed
                            refused = finish unauthorized
                        in
                        Expect.equal (0,0,Nothing) (Dict.size done.sessionDelivery.physical,Dict.size refused.sessionDelivery.physical,Dict.get request.key refused.sessionDelivery.resources |> Maybe.andThen .data)
                    _ -> Expect.fail "Expected one current request"
        , test "closing focus retires its implicit metadata but preserves a separately demanded detail owner" <|
            \_ ->
                let
                    focus = Delivery.SessionBytes agentId 0 False
                    source = Delivery.demand "w" focus Delivery.init
                    ( busy, requests ) = Delivery.admit "w" source
                    closed = Delivery.removeDemand "w" focus busy
                    completed = List.foldl (\request -> Delivery.settle request.id False (Ok (Delivery.Metadata (agentMetadata 3)))) closed requests
                    explicit = source |> Delivery.demand "w" (Delivery.SessionMetadata agentId) |> Delivery.removeDemand "w" focus
                in
                Expect.equal (0,0,1) (Dict.size completed.resources,Dict.size completed.physical,Dict.size explicit.resources)
        , test "only a valid accepted snapshot marks its scoped handoff complete" <|
            \_ ->
                let
                    accepted = receiveMarkerSnapshot "global" [] streamModel
                    malformed = receiveMarkerSnapshot "workspace" [] streamModel
                    markerWorkspaceItem = Encode.object
                        [ ( "schema_version", Encode.int 1 ), ( "kind", Encode.string "workspace" )
                        , ( "data", Encode.object [ ( "id", Encode.string "a" ), ( "name", Encode.string "Workspace" ), ( "workspace_type", Encode.string "repository" ), ( "created_at", Encode.string "now" ), ( "updated_at", Encode.string "now" ) ] ) ]
                    workspaceAccepted = receiveMarkerSnapshot "workspace" [ markerWorkspaceItem ] streamModel
                    foreign = receiveMarkerSnapshot "workspace" [ markerWorkspaceItem ] { streamModel | selectedWorkspaceId = Just "other" }
                    unauthorized = receiveMarkerSnapshot "workspace" [ markerWorkspaceItem ] { streamModel | auth = { status = AuthRequired, mode = Nothing } }
                    marked scope source = Dict.get scope source.webSocket.streams |> Maybe.map .snapshotApplied |> Maybe.withDefault False
                in
                Expect.equal [ True, True, False, False, False ]
                    [ marked "global" accepted, marked "workspace:a" workspaceAccepted, marked "workspace:a" malformed, marked "workspace:a" foreign, marked "workspace:a" unauthorized ]
        , test "accepted snapshot identity survives reconnect and resync but fresh scope state resets it" <|
            \_ ->
                let
                    accepted = receiveMarkerSnapshot "global" [] streamModel
                    state = Dict.get "global" accepted.webSocket.streams |> Maybe.withDefault (ChangeStream.init ChangeStream.Global [])
                    reconnect = ChangeStream.reduceFrame (Api.CanonicalStatus Api.GlobalScope "reconnecting") state |> Tuple.first
                    resync = ChangeStream.reduceFrame Api.CanonicalResyncRequired reconnect |> Tuple.first
                in
                Expect.equal ( True, Nothing, [ False, False ] )
                    ( resync.snapshotApplied, resync.resumeToken
                    , [ (ChangeStream.init ChangeStream.Global []).snapshotApplied, (ChangeStream.init (ChangeStream.Workspace "a") []).snapshotApplied ] )
        , test "both required scopes need terminal checkpoints in either arrival order" <|
            \_ ->
                let
                    states order =
                        List.foldl (\scope models -> receiveCheckpoint scope (List.head models |> Maybe.withDefault streamModel) :: models) [] order
                            |> List.reverse
                            |> List.map (.webSocket >> .state)
                in
                Expect.equal [ [ Connecting, Connected ], [ Connecting, Connected ] ]
                    [ states [ "global", "workspace" ], states [ "workspace", "global" ] ]
        , test "another scope checkpoint cannot hide reconnecting or exhausted workspace recovery" <|
            \_ ->
                let
                    live =
                        streamModel |> receiveCheckpoint "global" |> receiveCheckpoint "workspace"

                    lost =
                        receiveStatus "workspace" "reconnecting" live

                    exhausted =
                        receiveStatus "workspace" "retry_exhausted" lost

                    otherCheckpoint =
                        receiveCheckpoint "global" exhausted

                    restarting =
                        receiveStatus "workspace" "connecting" otherCheckpoint

                    restored =
                        receiveCheckpoint "workspace" restarting
                in
                Expect.equal
                    [ Connecting, ConnectionFailed "canonical:retry_exhausted:workspace:a", ConnectionFailed "canonical:retry_exhausted:workspace:a", Connecting, Connected ]
                    (List.map (.webSocket >> .state) [ lost, exhausted, otherCheckpoint, restarting, restored ])
        , test "resync and control closure cannot leave a scope live" <|
            \_ ->
                let
                    live =
                        streamModel |> receiveCheckpoint "global" |> receiveCheckpoint "workspace"

                    recovering phase =
                        live |> receiveStatus "workspace" phase |> receiveCheckpoint "global"
                in
                [ recovering "resyncing", recovering "control_closed", recovering "replaying" ]
                    |> List.map (.webSocket >> .state)
                    |> Expect.equal [ Connecting, Connecting, Connecting ]
        , test "required streams follow session permissions for readers and global views" <|
            \_ ->
                let
                    reader =
                        { streamModel | sessionContext = Maybe.map (\session -> { session | globalPermissions = { createWorkspace = False, superadmin = False } }) streamModel.sessionContext }

                    globalView =
                        { streamModel | selectedWorkspaceId = Nothing, page = HomePage }

                    noStreams =
                        { reader | selectedWorkspaceId = Nothing, page = HomePage }
                in
                [ receiveCheckpoint "workspace" reader, receiveCheckpoint "global" globalView, receiveCheckpoint "global" noStreams ]
                    |> List.map (.webSocket >> .state)
                    |> Expect.equal [ Connected, Connected, Disconnected ]
        , test "deduplicates and ignores wrong scope events" <|
            \_ ->
                let
                    state =
                        ChangeStream.init (ChangeStream.Workspace "a") []

                    event =
                        { actor = { actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing }, eventId = "event", scope = Api.WorkspaceScope "a", workspaceId = Just "a", entityType = "task", entityId = "t", entityAction = "updated", invalidations = [ { kind = "entity", target = "task:t" } ], requestId = Nothing }

                    wrong =
                        { event | scope = Api.WorkspaceScope "b" }
                in
                [ ChangeStream.reduceFrame (Api.CanonicalChange event) state |> Tuple.second
                , ChangeStream.reduceFrame (Api.CanonicalChange event) state |> Tuple.first |> ChangeStream.reduceFrame (Api.CanonicalChange event) |> Tuple.second
                , ChangeStream.reduceFrame (Api.CanonicalChange wrong) state |> Tuple.second
                ]
                    |> Expect.equal [ [ ChangeStream.RevalidateNavigationSummary "task" "t", ChangeStream.RefreshTimeline ], [], [] ]
        , test "workspace-scoped rename refetches the canonical workspace" <|
            \_ ->
                let
                    state =
                        ChangeStream.init (ChangeStream.Workspace "w") []

                    event =
                        { actor = { actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing }, eventId = "rename", scope = Api.WorkspaceScope "w", workspaceId = Just "w", entityType = "workspace", entityId = "w", entityAction = "updated", invalidations = [ { kind = "entity", target = "workspace:w" } ], requestId = Nothing }
                in
                ChangeStream.reduceFrame (Api.CanonicalChange event) state
                    |> Tuple.second
                    |> Expect.equal [ ChangeStream.RefetchEntity "workspace" "w" ]
        , test "checkpoint becomes live without mutation actions" <|
            \_ ->
                let
                    ( state, actions ) =
                        ChangeStream.reduceFrame (Api.CanonicalCheckpoint "opaque") (ChangeStream.init ChangeStream.Global [])
                in
                Expect.equal ( True, Just "opaque", [] ) ( state.live, state.resumeToken, actions )
        , test "Timeline eligibility is one coalesced action for lifecycle envelopes only" <|
            \_ ->
                let
                    state =
                        ChangeStream.init (ChangeStream.Workspace "w") []

                    actions entityType entityAction invalidations =
                        ChangeStream.reduceFrame
                            (Api.CanonicalChange
                                { actor = { actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing }, eventId = entityType ++ "-" ++ entityAction
                                , scope = Api.WorkspaceScope "w"
                                , workspaceId = Just "w"
                                , entityType = entityType
                                , entityId = "id"
                                , entityAction = entityAction
                                , invalidations = invalidations
                                , requestId = Nothing
                                }
                            )
                            state
                            |> Tuple.second
                            |> List.filter ((==) ChangeStream.RefreshTimeline)
                in
                [ actions "project" "created" [ { kind = "entity", target = "project:id" }, { kind = "collection", target = "projects:w" } ]
                , actions "project" "updated" [ { kind = "entity", target = "project:id" } ]
                , actions "project" "deleted" [ { kind = "entity", target = "project:id" } ]
                , actions "project" "restored" [ { kind = "entity", target = "project:id" } ]
                , actions "task" "created" [ { kind = "entity", target = "task:id" } ]
                , actions "task" "updated" [ { kind = "entity", target = "task:id" } ]
                , actions "task" "deleted" [ { kind = "entity", target = "task:id" } ]
                , actions "task" "restored" [ { kind = "entity", target = "task:id" } ]
                , actions "observation" "created" [ { kind = "entity", target = "observation:id" } ]
                , actions "observation" "deleted" [ { kind = "entity", target = "observation:id" } ]
                , actions "observation" "updated" [ { kind = "entity", target = "observation:id" } ]
                , actions "task_dependency" "updated" [ { kind = "entity", target = "task_dependency:id:other" } ]
                ]
                    |> Expect.equal
                        [ [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , [ ChangeStream.RefreshTimeline ]
                        , []
                        , []
                        ]
        , test "coalesces a burst and fails closed for an unknown invalidation" <|
            \_ ->
                let
                    state =
                        ChangeStream.init (ChangeStream.Workspace "a") []

                    duplicate =
                        { kind = "entity", target = "project:p" }

                    event =
                        { actor = { actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing }, eventId = "event", scope = Api.WorkspaceScope "a", workspaceId = Just "a", entityType = "project", entityId = "p", entityAction = "updated", invalidations = [ duplicate, duplicate ], requestId = Nothing }

                    unknown =
                        { event | eventId = "next", invalidations = [ { kind = "unrecognised", target = "project:p" } ] }

                    deleted =
                        { event | eventId = "deleted", entityAction = "deleted", invalidations = [ duplicate, { kind = "readiness", target = "project:p" }, { kind = "entity", target = "task:t" } ] }

                    groupMembership =
                        { event | eventId = "group-membership", entityType = "workspace_group_membership", entityId = "g:a", entityAction = "created", invalidations = [ { kind = "entity", target = "group_membership:g:a" }, { kind = "collection", target = "group:g:members" }, { kind = "collection", target = "workspace:a:groups" } ] }

                    workspaceMembership =
                        { event | eventId = "workspace-membership", entityType = "workspace_membership", entityId = "a:u", entityAction = "updated", invalidations = [ { kind = "collection", target = "workspace:a:memberships" } ] }
                in
                [ ChangeStream.reduceFrame (Api.CanonicalChange event) state |> Tuple.second
                , ChangeStream.reduceFrame (Api.CanonicalChange unknown) state |> Tuple.second
                , ChangeStream.reduceFrame (Api.CanonicalChange deleted) state |> Tuple.second
                , ChangeStream.reduceFrame (Api.CanonicalChange groupMembership) state |> Tuple.second
                , ChangeStream.reduceFrame (Api.CanonicalChange workspaceMembership) state |> Tuple.second
                ]
                    |> Expect.equal [ [ ChangeStream.RevalidateNavigationSummary "project" "p", ChangeStream.RefreshTimeline ], [ ChangeStream.BeginResync ], [ ChangeStream.RemoveEntity "project" "p", ChangeStream.RevalidateNavigationSummary "project" "p", ChangeStream.RevalidateNavigationSummary "task" "t", ChangeStream.RefreshTimeline ], [ ChangeStream.RefreshGroupMembers "g", ChangeStream.NoAction ], [ ChangeStream.RefreshMemberships "a" ] ]
        , test "revocation clears only the current workspace scope" <|
            \_ ->
                let
                    state =
                        ChangeStream.init (ChangeStream.Workspace "a") []

                    actions =
                        ChangeStream.reduceFrame (Api.CanonicalAccessRevoked (Just "a")) state |> Tuple.second
                in
                Expect.equal [ ChangeStream.ClearWorkspace "a", ChangeStream.RefreshCatalogue, ChangeStream.RefreshSessionAuthorization ] actions
        , test "grant clears only the newly readable workspace bearer before authorization refresh" <|
            \_ ->
                let
                    state =
                        ChangeStream.init ChangeStream.Global []
                in
                ChangeStream.reduceFrame (Api.CanonicalAccessGranted "a") state
                    |> Tuple.second
                    |> Expect.equal [ ChangeStream.AccessGranted "a", ChangeStream.RefreshCatalogue, ChangeStream.RefreshSessionAuthorization ]
        , test "snapshot kinds are scope-validated before any replacement" <|
            \_ ->
                let
                    item =
                        { kind = "workspace_group", data = Encode.null }
                in
                ChangeStream.applySnapshot (ChangeStream.Workspace "a") [ item ]
                    |> Result.map (\_ -> ())
                    |> Expect.err
        , test "workspace shell snapshots admit only the scoped durable workspace root" <|
            \_ ->
                let
                    workspace =
                        { kind = "workspace"
                        , data =
                            Encode.object
                                [ ( "id", Encode.string "a" )
                                , ( "name", Encode.string "Workspace A" )
                                , ( "workspace_type", Encode.string "repository" )
                                , ( "created_at", Encode.string "2026-01-01T00:00:00Z" )
                                , ( "updated_at", Encode.string "2026-01-01T00:00:00Z" )
                                ]
                        }

                    project =
                        { kind = "project", data = Encode.null }
                in
                [ ChangeStream.applySnapshotProfile (ChangeStream.Workspace "a") "workspace_shell_v1" [ workspace ] |> Result.map (\snapshot -> Dict.member "a" snapshot.workspaces)
                , ChangeStream.applySnapshotProfile (ChangeStream.Workspace "a") "workspace_shell_v1" [ project ] |> Result.map (\_ -> True)
                ]
                    |> Expect.equal [ Ok True, Err "snapshot kind is invalid for scope" ]
        , test "every V022 target shape is explicit and no-colon workspace-groups is handled" <|
            \_ ->
                let
                    workspaceState =
                        ChangeStream.init (ChangeStream.Workspace "w") []

                    workspaceEvent =
                        { actor = { actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing }, eventId = "workspace-matrix"
                        , scope = Api.WorkspaceScope "w"
                        , workspaceId = Just "w"
                        , entityType = "task"
                        , entityId = "t"
                        , entityAction = "updated"
                        , requestId = Nothing
                        , invalidations =
                            [ { kind = "entity", target = "task:t" }
                            , { kind = "collection", target = "tasks:w" }
                            , { kind = "tree", target = "workspace:w" }
                            , { kind = "readiness", target = "task:t" }
                            , { kind = "readiness", target = "project:p" }
                            , { kind = "next_task", target = "workspace:w" }
                            , { kind = "search", target = "workspace:w" }
                            , { kind = "collection", target = "workspace:w:memberships" }
                            , { kind = "collection", target = "group:g:members" }
                            , { kind = "collection", target = "workspace:w:groups" }
                            , { kind = "catalogue", target = "workspace-catalog" }
                            , { kind = "session_authorization", target = "session-authorization" }
                            , { kind = "permission_cache", target = "permission-cache" }
                            ]
                        }

                    globalEvent =
                        { actor = { actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing }, eventId = "global-matrix"
                        , scope = Api.GlobalScope
                        , workspaceId = Nothing
                        , entityType = "workspace"
                        , entityId = "w"
                        , entityAction = "updated"
                        , requestId = Nothing
                        , invalidations =
                            [ { kind = "entity", target = "workspace:w" }
                            , { kind = "catalogue", target = "workspace-catalog" }
                            , { kind = "collection", target = "workspace-groups" }
                            ]
                        }
                in
                [ ChangeStream.reduceFrame (Api.CanonicalChange workspaceEvent) workspaceState |> Tuple.second
                , ChangeStream.reduceFrame (Api.CanonicalChange globalEvent) (ChangeStream.init ChangeStream.Global []) |> Tuple.second
                ]
                    |> Expect.equal
                        [ [ ChangeStream.RevalidateNavigationSummary "task" "t"
                          , ChangeStream.RefreshNavigation
                          , ChangeStream.RevalidateNavigationSummary "project" "p"
                          , ChangeStream.RefreshNextTasks "w"
                          , ChangeStream.RefreshSearch "w"
                          , ChangeStream.RefreshMemberships "w"
                          , ChangeStream.RefreshGroupMembers "g"
                          , ChangeStream.NoAction
                          , ChangeStream.RefreshCatalogue
                          , ChangeStream.RefreshSessionAuthorization
                          , ChangeStream.RefreshTimeline
                          ]
                        , [ ChangeStream.RefetchEntity "workspace" "w", ChangeStream.RefreshCatalogue, ChangeStream.RefreshGroups ]
                        ]
        , test "unknown kind-target combinations fail closed without partial effects" <|
            \_ ->
                let
                    state =
                        ChangeStream.init (ChangeStream.Workspace "w") []

                    base =
                        { actor = { actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing }, eventId = "bad"
                        , scope = Api.WorkspaceScope "w"
                        , workspaceId = Just "w"
                        , entityType = "task"
                        , entityId = "t"
                        , entityAction = "updated"
                        , requestId = Nothing
                        , invalidations = []
                        }

                    invalids =
                        [ [ { kind = "entity", target = "unknown:x" } ]
                        , [ { kind = "collection", target = "workspace-groups" } ]
                        , [ { kind = "collection", target = "tasks:other" } ]
                        , [ { kind = "tree", target = "project:p" } ]
                        , [ { kind = "next_task", target = "workspace:other" } ]
                        , [ { kind = "readiness", target = "workspace:w" } ]
                        , [ { kind = "catalogue", target = "wrong" } ]
                        , [ { kind = "entity", target = "task:" } ]
                        , [ { kind = "entity", target = "task:t" }, { kind = "future", target = "task:t" } ]
                        ]
                in
                invalids
                    |> List.indexedMap (\index invalidations -> ChangeStream.reduceFrame (Api.CanonicalChange { base | eventId = "bad-" ++ String.fromInt index, invalidations = invalidations }) state |> Tuple.second)
                    |> Expect.equal (List.repeat (List.length invalids) [ ChangeStream.BeginResync ])
        , test "coalesces duplicate targets across frames and checkpoint" <|
            \_ ->
                let
                    state =
                        ChangeStream.init (ChangeStream.Workspace "w") []

                    event eventId =
                        Api.CanonicalChange
                            { actor = { actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing }, eventId = eventId
                            , scope = Api.WorkspaceScope "w"
                            , workspaceId = Just "w"
                            , entityType = "task"
                            , entityId = "t"
                            , entityAction = "updated"
                            , requestId = Nothing
                            , invalidations = [ { kind = "entity", target = "task:t" }, { kind = "next_task", target = "workspace:w" } ]
                            }

                    ( final, actions ) =
                        ChangeStream.reduceFrames [ event "one", event "two", Api.CanonicalCheckpoint "replacement" ] state
                in
                Expect.equal ( True, Just "replacement", [ ChangeStream.RevalidateNavigationSummary "task" "t", ChangeStream.RefreshNextTasks "w", ChangeStream.RefreshTimeline ] ) ( final.live, final.resumeToken, actions )
        , test "entity transition coalescing preserves the final workspace, project, and task state" <|
            \_ ->
                let
                    workspace action eventId =
                        Api.CanonicalChange
                            { actor = { actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing }, eventId = eventId
                            , scope = Api.GlobalScope
                            , workspaceId = Nothing
                            , entityType = "workspace"
                            , entityId = "w"
                            , entityAction = action
                            , requestId = Nothing
                            , invalidations = [ { kind = "entity", target = "workspace:w" } ]
                            }

                    entity kind identity action eventId =
                        Api.CanonicalChange
                            { actor = { actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing }, eventId = eventId
                            , scope = Api.WorkspaceScope "w"
                            , workspaceId = Just "w"
                            , entityType = kind
                            , entityId = identity
                            , entityAction = action
                            , requestId = Nothing
                            , invalidations = [ { kind = "entity", target = kind ++ ":" ++ identity } ]
                            }

                    actions frames initial =
                        ChangeStream.reduceFrames frames initial |> Tuple.second
                in
                [ actions [ workspace "deleted" "wd", workspace "restored" "wr" ] (ChangeStream.init ChangeStream.Global [])
                , actions [ workspace "restored" "wr", workspace "deleted" "wd" ] (ChangeStream.init ChangeStream.Global [])
                , actions [ entity "project" "p" "deleted" "pd", entity "project" "p" "restored" "pr" ] (ChangeStream.init (ChangeStream.Workspace "w") [])
                , actions [ entity "project" "p" "restored" "pr", entity "project" "p" "deleted" "pd" ] (ChangeStream.init (ChangeStream.Workspace "w") [])
                , actions [ entity "task" "t" "deleted" "td", entity "task" "t" "restored" "tr" ] (ChangeStream.init (ChangeStream.Workspace "w") [])
                , actions [ entity "task" "t" "restored" "tr", entity "task" "t" "deleted" "td" ] (ChangeStream.init (ChangeStream.Workspace "w") [])
                ]
                    |> Expect.equal
                        [ [ ChangeStream.RefetchEntity "workspace" "w", ChangeStream.RefreshSessionAuthorization ]
                        , [ ChangeStream.RemoveEntity "workspace" "w", ChangeStream.ClearWorkspace "w", ChangeStream.RefreshSessionAuthorization ]
                        , [ ChangeStream.RemoveEntity "project" "p", ChangeStream.RefreshTimeline, ChangeStream.RevalidateNavigationSummary "project" "p" ]
                        , [ ChangeStream.RevalidateNavigationSummary "project" "p", ChangeStream.RefreshTimeline, ChangeStream.RemoveEntity "project" "p" ]
                        , [ ChangeStream.RemoveEntity "task" "t", ChangeStream.RefreshTimeline, ChangeStream.RevalidateNavigationSummary "task" "t" ]
                        , [ ChangeStream.RevalidateNavigationSummary "task" "t", ChangeStream.RefreshTimeline, ChangeStream.RemoveEntity "task" "t" ]
                        ]
        , test "dependency invalidations coalesce one dependency-page refresh and one task-summary revalidation" <|
            \_ ->
                let
                    event action eventId requestId =
                        Api.CanonicalChange
                            { actor = { actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing }, eventId = eventId
                            , scope = Api.WorkspaceScope "w"
                            , workspaceId = Just "w"
                            , entityType = "task_dependency"
                            , entityId = "t:d"
                            , entityAction = action
                            , requestId = Just requestId
                            , invalidations =
                                [ { kind = "entity", target = "task_dependency:t:d" }
                                , { kind = "entity", target = "task_dependency:t:d" }
                                , { kind = "readiness", target = "task:t" }
                                , { kind = "readiness", target = "task:t" }
                                , { kind = "entity", target = "task:t" }
                                ]
                            }
                in
                [ ChangeStream.reduceFrame (event "updated" "u" "dependency-request") (ChangeStream.init (ChangeStream.Workspace "w") []) |> Tuple.second
                , ChangeStream.reduceFrame (event "deleted" "d" "dependency-request") (ChangeStream.init (ChangeStream.Workspace "w") []) |> Tuple.second
                , ChangeStream.reduceFrames
                    [ event "updated" "local" "local-request"
                    , event "updated" "remote" "remote-request"
                    ]
                    (ChangeStream.init (ChangeStream.Workspace "w") [])
                    |> Tuple.second
                ]
                    |> Expect.equal
                        [ [ ChangeStream.RefreshTaskDependencies "t" "d" True (Just "dependency-request"), ChangeStream.RevalidateNavigationSummary "task" "t" ]
                        , [ ChangeStream.RefreshTaskDependencies "t" "d" False (Just "dependency-request"), ChangeStream.RevalidateNavigationSummary "task" "t" ]
                        , [ ChangeStream.RefreshTaskDependencies "t" "d" True (Just "local-request")
                          , ChangeStream.RevalidateNavigationSummary "task" "t"
                          , ChangeStream.RefreshTaskDependencies "t" "d" True (Just "remote-request")
                          ]
                        ]
        , test "observation invalidations target canonical detail and automatically refresh the applied collection" <|
            \_ ->
                let
                    event action eventId =
                        Api.CanonicalChange
                            { actor = { actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing }, eventId = eventId
                            , scope = Api.WorkspaceScope "w"
                            , workspaceId = Just "w"
                            , entityType = "observation"
                            , entityId = "o"
                            , entityAction = action
                            , requestId = Nothing
                            , invalidations =
                                [ { kind = "entity", target = "observation:o" }
                                , { kind = "collection", target = "observations:w" }
                                , { kind = "collection", target = "observations:w" }
                                ]
                            }

                    actions frames =
                        ChangeStream.reduceFrames frames (ChangeStream.init (ChangeStream.Workspace "w") []) |> Tuple.second
                in
                [ actions [ event "updated" "u" ]
                , actions [ event "updated" "u", event "deleted" "d" ]
                ]
                    |> Expect.equal
                        [ [ ChangeStream.RefetchEntity "observation" "o", ChangeStream.RefreshObservations ]
                        , [ ChangeStream.RemoveEntity "observation" "o", ChangeStream.RefreshObservations, ChangeStream.RefreshTimeline ]
                        ]
        , test "dedupe history is bounded and evicts the oldest accepted event" <|
            \_ ->
                let
                    final =
                        List.range 1 129
                            |> List.foldl
                                (\index state -> ChangeStream.acceptEvent (String.fromInt index) state |> Maybe.withDefault state)
                                (ChangeStream.init ChangeStream.Global [])
                in
                Expect.equal
                    { count = 128, oldestRetained = False, oldestCanBeAcceptedAgain = True, newestIsDuplicate = True }
                    { count = List.length final.eventIds
                    , oldestRetained = List.member "1" final.eventIds
                    , oldestCanBeAcceptedAgain = ChangeStream.acceptEvent "1" final /= Nothing
                    , newestIsDuplicate = ChangeStream.acceptEvent "129" final == Nothing
                    }
        , test "request guards reject stale target, route, auth epoch, and audience responses" <|
            \_ ->
                let
                    guard =
                        { scopeKey = "workspace:w"
                        , targetKey = "task-overview:t"
                        , targetGeneration = 4
                        , sessionEpoch = 7
                        , routeWorkspace = Just "w"
                        , audienceId = "actor-a"
                        }

                    generations =
                        Dict.fromList [ ( "workspace:w|task-overview:t", 4 ) ]
                in
                [ ChangeStream.requestGuardMatches guard 7 (Just "w") "actor-a" generations
                , ChangeStream.requestGuardMatches guard 7 (Just "w") "actor-a" (Dict.insert "workspace:w|task-overview:t" 5 generations)
                , ChangeStream.requestGuardMatches guard 7 (Just "other") "actor-a" generations
                , ChangeStream.requestGuardMatches guard 8 (Just "w") "actor-a" generations
                , ChangeStream.requestGuardMatches guard 7 (Just "w") "actor-b" generations
                ]
                    |> Expect.equal [ True, False, False, False, False ]
        , test "overlapping dependency and readiness responses share one generation fence" <|
            \_ ->
                let
                    oldGuard =
                        { scopeKey = "workspace:w"
                        , targetKey = "task-overview:t"
                        , targetGeneration = 4
                        , sessionEpoch = 7
                        , routeWorkspace = Just "w"
                        , audienceId = "actor"
                        }

                    newGuard =
                        { oldGuard | targetGeneration = 5 }

                    current =
                        Dict.fromList [ ( "workspace:w|task-overview:t", 5 ) ]
                in
                Expect.equal
                    { dependencyResponseAccepted = False, readinessResponseAccepted = True }
                    { dependencyResponseAccepted = ChangeStream.requestGuardMatches oldGuard 7 (Just "w") "actor" current
                    , readinessResponseAccepted = ChangeStream.requestGuardMatches newGuard 7 (Just "w") "actor" current
                    }
        , test "selected-workspace catalogue deletion explicitly removes and clears only that scope" <|
            \_ ->
                let
                    event =
                        { actor = { actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing }, eventId = "workspace-deleted"
                        , scope = Api.GlobalScope
                        , workspaceId = Nothing
                        , entityType = "workspace"
                        , entityId = "w"
                        , entityAction = "deleted"
                        , requestId = Nothing
                        , invalidations =
                            [ { kind = "entity", target = "workspace:w" }
                            , { kind = "catalogue", target = "workspace-catalog" }
                            , { kind = "collection", target = "workspace-groups" }
                            ]
                        }
                in
                ChangeStream.reduceFrame (Api.CanonicalChange event) (ChangeStream.init ChangeStream.Global [])
                    |> Tuple.second
                    |> Expect.equal [ ChangeStream.RemoveEntity "workspace" "w", ChangeStream.ClearWorkspace "w", ChangeStream.RefreshSessionAuthorization, ChangeStream.RefreshCatalogue, ChangeStream.RefreshGroups ]
        , test "strict v1 decoder requires nested metadata, identities, and terminal tokens" <|
            \_ ->
                let
                    valid =
                        canonicalWire "restored" [ ( "entity", "task:t" ) ]

                    invalid =
                        [ "{\"schema_version\":1,\"type\":\"checkpoint\",\"catch_up\":\"complete\",\"resume_token\":\"\"}"
                        , "{\"schema_version\":1,\"type\":\"access_revoked\",\"workspace_id\":\"\"}"
                        , "{\"schema_version\":1,\"type\":\"change\",\"event\":{\"event_id\":\"e\",\"scope\":\"workspace\",\"workspace_id\":\"w\",\"entity\":{\"type\":\"task\",\"id\":\"t\",\"action\":\"updated\"},\"invalidations\":[{\"kind\":\"entity\",\"target\":\"task:t\"}]}}"
                        , "{\"schema_version\":1,\"type\":\"change\",\"event\":{\"schema_version\":1,\"event_id\":\"e\",\"scope\":\"global\",\"workspace_id\":\"other\",\"occurred_at\":\"now\",\"transaction\":{\"id\":\"tx\",\"cause\":\"rest\",\"request_id\":null},\"actor\":{\"type\":\"system\",\"id\":null},\"entity\":{\"type\":\"workspace\",\"id\":\"other\",\"action\":\"updated\"},\"invalidations\":[{\"kind\":\"entity\",\"target\":\"workspace:other\"}]}}"
                        , "{\"transport\":\"snapshot\",\"scope\":{\"scope\":\"global\"},\"items\":[],\"resume_token\":\"token\"}"
                        , "{\"schema_version\":1,\"transport\":\"snapshot\",\"scope\":{\"scope\":\"global\"},\"items\":[{\"schema_version\":1,\"kind\":\"future\",\"data\":{}}],\"resume_token\":\"token\"}"
                        ]
                in
                Expect.equal
                    { valid = True, invalid = List.repeat (List.length invalid) Nothing, failedScope = Just Api.GlobalScope }
                    { valid = Api.decodeCanonicalFrame valid /= Nothing
                    , invalid = List.map Api.decodeCanonicalFrame invalid
                    , failedScope = Api.decodeCanonicalTransportScope "{\"schema_version\":1,\"transport\":\"frames\",\"scope\":{\"scope\":\"global\"},\"frames\":[]}"
                    }
        , test "snapshot replacement validates dependency ownership after all pages are accumulated" <|
            \_ ->
                let
                    valid =
                        ChangeStream.applySnapshot (ChangeStream.Workspace "w")
                            [ workspaceItem "w", taskItem "a" "w", taskItem "b" "w", dependencyItem "a" "b" ]

                    missingTask =
                        ChangeStream.applySnapshot (ChangeStream.Workspace "w")
                            [ workspaceItem "w", taskItem "a" "w", dependencyItem "a" "missing" ]

                    duplicate =
                        ChangeStream.applySnapshot (ChangeStream.Workspace "w") [ workspaceItem "w", workspaceItem "w" ]
                in
                case valid of
                    Ok snapshot ->
                        Expect.equal
                            { workspaces = 1, tasks = 2, dependencies = 1, missingTaskRejected = True, duplicateRejected = True }
                            { workspaces = Dict.size snapshot.workspaces
                            , tasks = Dict.size snapshot.tasks
                            , dependencies = List.length snapshot.dependencies
                            , missingTaskRejected = Result.toMaybe missingTask == Nothing
                            , duplicateRejected = Result.toMaybe duplicate == Nothing
                            }

                    Err error ->
                        Expect.fail error
        ]


canonicalWire : String -> List ( String, String ) -> String
canonicalWire action invalidations =
    Encode.object
        [ ( "schema_version", Encode.int 1 )
        , ( "type", Encode.string "change" )
        , ( "event"
          , Encode.object
                [ ( "schema_version", Encode.int 1 )
                , ( "event_id", Encode.string "event" )
                , ( "scope", Encode.string "workspace" )
                , ( "workspace_id", Encode.string "w" )
                , ( "occurred_at", Encode.string "2026-01-01T00:00:00Z" )
                , ( "transaction", Encode.object [ ( "id", Encode.string "tx" ), ( "cause", Encode.string "rest" ), ( "request_id", Encode.null ) ] )
                , ( "actor", Encode.object [ ( "type", Encode.string "system" ), ( "id", Encode.null ) ] )
                , ( "entity", Encode.object [ ( "type", Encode.string "task" ), ( "id", Encode.string "t" ), ( "action", Encode.string action ) ] )
                , ( "invalidations", Encode.list (\( kind, target ) -> Encode.object [ ( "kind", Encode.string kind ), ( "target", Encode.string target ) ]) invalidations )
                ]
          )
        ]
        |> Encode.encode 0


workspaceItem : String -> Api.SnapshotItem
workspaceItem workspaceId =
    { kind = "workspace"
    , data =
        Encode.object
            [ ( "id", Encode.string workspaceId )
            , ( "name", Encode.string "Workspace" )
            , ( "workspace_type", Encode.string "repository" )
            , ( "created_at", Encode.string "2026-01-01T00:00:00Z" )
            , ( "updated_at", Encode.string "2026-01-01T00:00:00Z" )
            ]
    }


taskItem : String -> String -> Api.SnapshotItem
taskItem taskId workspaceId =
    { kind = "task"
    , data =
        Encode.object
            [ ( "id", Encode.string taskId )
            , ( "workspace_id", Encode.string workspaceId )
            , ( "title", Encode.string ("Task " ++ taskId) )
            , ( "status", Encode.string "todo" )
            , ( "priority", Encode.int 5 )
            , ( "dependency_count", Encode.int 0 )
            , ( "created_at", Encode.string "2026-01-01T00:00:00Z" )
            , ( "updated_at", Encode.string "2026-01-01T00:00:00Z" )
            ]
    }


dependencyItem : String -> String -> Api.SnapshotItem
dependencyItem taskId dependsOnId =
    { kind = "task_dependency"
    , data = Encode.object [ ( "task_id", Encode.string taskId ), ( "depends_on_id", Encode.string dependsOnId ) ]
    }


streamModel : Model
streamModel =
    let
        flags =
            { apiUrl = "https://api.example", wsUrl = "wss://api.example/ws", sessionId = "test", runtimeMode = "test", authTokenStorageKey = "test", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }

        url =
            { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/a", query = Nothing, fragment = Nothing }

        base =
            AppShell.initModel Nothing url (WorkspacePage "a") flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
    in
    { base
        | auth = { status = AuthReady, mode = Just "test" }
        , selectedWorkspaceId = Just "a"
        , sessionContext = Just { authMode = "test", effectiveActor = Nothing, principal = { actorType = "user", actorId = "actor", actorLabel = "Actor", authority = "local", grantUserId = Nothing }, globalPermissions = { createWorkspace = False, superadmin = True }, workspace = Just { workspaceId = "a", role = Just "read", canRead = True, canEdit = False, canAdmin = False } }
    }


scopeValue : String -> Encode.Value
scopeValue scope =
    Encode.object
        (( "scope", Encode.string scope )
            :: (if scope == "workspace" then
                    [ ( "workspace_id", Encode.string "a" ) ]

                else
                    []
               )
        )


receiveMarkerSnapshot : String -> List Encode.Value -> Model -> Model
receiveMarkerSnapshot scope items model =
    receiveTransport
        [ ( "transport", Encode.string "snapshot" )
        , ( "scope", scopeValue scope )
        , ( "snapshot_profile", Encode.string (if scope == "workspace" then "workspace_shell_v1" else "full_v1") )
        , ( "items", Encode.list identity items )
        , ( "resume_token", Encode.string "marker-token" )
        ] model


receiveStatus : String -> String -> Model -> Model
receiveStatus scope state model =
    receiveTransport
        [ ( "transport", Encode.string "status" )
        , ( "scope", scopeValue scope )
        , ( "state", Encode.string state )
        ]
        model


receiveCheckpoint : String -> Model -> Model
receiveCheckpoint scope model =
    receiveTransport
        [ ( "transport", Encode.string "frame" )
        , ( "scope", scopeValue scope )
        , ( "frame", Encode.object [ ( "schema_version", Encode.int 1 ), ( "type", Encode.string "checkpoint" ), ( "catch_up", Encode.string "complete" ), ( "resume_token", Encode.string "opaque" ) ] )
        ]
        model


receiveTransport : List ( String, Encode.Value ) -> Model -> Model
receiveTransport fields model =
    WebSocket.update (WsMessageReceived (Encode.encode 0 (Encode.object (( "schema_version", Encode.int 1 ) :: fields)))) model
        |> Tuple.first


agentId : String
agentId = "11111111-1111-4111-8111-111111111111"


linkedTaskId : String
linkedTaskId = "22222222-2222-4222-8222-222222222222"


agentMetadata : Int -> Api.AgentSession
agentMetadata bytes =
    { id=agentId, workspaceId="w", formatLabel="jsonl", state=Api.SessionActive
    , creator={actorType=Nothing,actorId=Nothing,actorLabel=Nothing,botIdentity=Nothing}
    , createdAt="2026-01-01T00:00:00Z",lastSeenAt="2026-01-01T00:00:00Z",committedBytes=bytes,committedRecords=2,revision=2,endsWithNewline=True
    , terminatedAt=Nothing,finalBytes=Nothing,finalSha256=Nothing, title=Nothing, parentSessionId=Nothing, ancestors=[], ancestorsTruncated=False, runtimeIdentity=Nothing, runtimeRevision=0 }


sessionEvent : Bool -> Api.CanonicalEnvelope
sessionEvent linked =
    { actor={actorType=Nothing,actorId=Nothing,actorLabel=Nothing,botIdentity=Nothing},eventId=if linked then "linked" else "metadata"
    , scope=Api.WorkspaceScope "w",workspaceId=Just "w",entityType=if linked then "task_session" else "agent_session"
    , entityId=if linked then agentId ++ ":" ++ linkedTaskId else agentId,entityAction=if linked then "created" else "updated",requestId=Nothing
    , invalidations=(if linked then [{kind="entity",target="task_session:" ++ agentId ++ ":" ++ linkedTaskId}] else [])
        ++ [{kind="entity",target="agent_session:" ++ agentId},{kind="collection",target="sessions:w"}]
        ++ (if linked then [{kind="collection",target="task_sessions:" ++ linkedTaskId}] else []) }


deliverOne : Delivery.Payload -> Delivery.State -> Delivery.State
deliverOne payload state =
    let ( admitted, requests ) = Delivery.admit "w" state in
    case requests of
        [request] -> Delivery.settle request.id True (Ok payload) admitted
        _ -> state


sessionGuard : Model -> Delivery.Request -> Types.CanonicalRequestGuard
sessionGuard model request =
    let target = "session-resource:" ++ request.key in
    { scopeKey="workspace:a",targetKey=target,targetGeneration=Dict.get ("workspace:a|" ++ target) model.webSocket.targetGenerations |> Maybe.withDefault -1
    , sessionEpoch=model.sessionRequestEpoch,routeWorkspace=Just "a",audienceId="actor" }
