module AppShellSessionTest exposing (suite)

import Api
import AppShell exposing (AppShellOwnedMsg(..))
import Dict
import Expect
import Feature.Cards as Cards
import Feature.ChangeStream as ChangeStream
import Feature.DataLoading as DataLoading
import Feature.WebSocket as WebSocket
import Http
import Json.Encode as Encode
import Set
import Test exposing (Test, describe, test)
import Types exposing (AuthStatus(..), Model, Msg(..), Page(..), WSState(..), WorkspaceTab(..))
import Url


suite : Test
suite =
        ([ HomePage, WorkspacePage "a" ]
            |> List.concatMap
                (\page ->
                    [ test ("unchanged superadmin refresh preserves catalogue, loading and live guards on " ++ pageName page) <|
                        \_ ->
                            let
                                before =
                                    populated page

                                after =
                                    refresh superadmin before
                            in
                            Expect.equal
                                ( before.workspaces, before.groups, ( before.webSocket, before.dataLoading ) )
                                ( after.workspaces, after.groups, ( after.webSocket, after.dataLoading ) )
                    , test ("pending global canonical response survives repeated refresh on " ++ pageName page) <|
                        \_ ->
                            populated page
                                |> refresh superadmin
                                |> refresh superadmin
                                |> WebSocket.update (CanonicalWorkspaceFetched (globalGuard page) "a" (Ok { workspace | name = "Updated" }))
                                |> Tuple.first
                                |> .workspaces
                                |> Dict.get "a"
                                |> Maybe.map .name
                                |> Expect.equal (Just "Updated")
                    ]
                )
        )
            ++ [ test "downgrade retains readable workspace stream but retires global projection and guard" <|
                    \_ ->
                        let
                            after =
                                refresh reader (populated (WorkspacePage "a"))

                            late =
                                WebSocket.update (CanonicalWorkspaceFetched (globalGuard (WorkspacePage "a")) "a" (Ok workspace)) after |> Tuple.first
                        in
                        Expect.equal
                            { streams = [ "workspace:a" ], generations = Dict.fromList [ ( "global|entity:workspace:a", 5 ), ( "workspace:a|entity:workspace:a", 4 ) ], workspaces = Dict.empty, groups = Dict.empty, state = Connected, late = Dict.empty }
                            { streams = Dict.keys after.webSocket.streams, generations = after.webSocket.targetGenerations, workspaces = after.workspaces, groups = after.groups.workspaceGroups, state = after.webSocket.state, late = late.workspaces }
               , test "Home downgrade retires all streams and global data" <|
                    \_ ->
                        let
                            after =
                                refresh reader (populated HomePage)
                        in
                        Expect.equal ( Disconnected, Dict.empty, Dict.empty )
                            ( after.webSocket.state, after.webSocket.streams, after.groups.workspaceGroups )
               , test "losing workspace read access retires its stream, guards and cached data" <|
                    \_ ->
                        let
                            before =
                                refresh reader (populated (WorkspacePage "a"))

                            denied =
                                { reader | workspace = Nothing }

                            after =
                                refresh denied before
                        in
                        Expect.equal ( Disconnected, Dict.empty, Dict.fromList [ ( "global|entity:workspace:a", 5 ), ( "workspace:a|entity:workspace:a", 5 ) ] )
                            ( after.webSocket.state, after.webSocket.streams, after.webSocket.targetGenerations )
               , test "audience replacement drops old checkpoints and rejects old global replies" <|
                    \_ ->
                        let
                            principal =
                                superadmin.principal

                            after =
                                refresh { superadmin | principal = { principal | actorId = "other" } } (populated HomePage)

                            late =
                                WebSocket.update (CanonicalWorkspaceFetched (globalGuard HomePage) "a" (Ok workspace)) after |> Tuple.first
                        in
                        Expect.equal ( Dict.empty, Dict.empty, Dict.empty )
                            ( after.webSocket.streams, after.groups.workspaceGroups, late.workspaces )
               , test "global downgrade and regrant cannot resurrect a pending response" <|
                    \_ ->
                        expectRegrantFence ChangeStream.Global HomePage superadmin reader
               , test "workspace revocation and regrant cannot resurrect a pending response" <|
                    \_ ->
                        expectRegrantFence (ChangeStream.Workspace "a") (WorkspacePage "a") reader { reader | workspace = Nothing }
               , test "authority replacement with the same actor cannot reuse old global guards" <|
                    \_ ->
                        let
                            principal =
                                superadmin.principal

                            after =
                                refresh { superadmin | principal = { principal | authority = "replacement" } } (populated HomePage)

                            late =
                                WebSocket.update (CanonicalWorkspaceFetched (globalGuard HomePage) "a" (Ok workspace)) after |> Tuple.first
                        in
                        Expect.equal ( Dict.empty, Dict.empty ) ( after.webSocket.targetGenerations, late.workspaces )
               , test "authorization failure clears scopes and advances session epoch" <|
                    \_ ->
                        let
                            before =
                                populated HomePage

                            after =
                                AppShell.handleOwned (SessionContextLoadedMsg before.sessionRequestEpoch Nothing (Err (Http.BadStatus 401))) before |> Tuple.first
                        in
                        Expect.equal ( AuthRequired, before.sessionRequestEpoch + 1, Dict.empty )
                            ( after.auth.status, after.sessionRequestEpoch, after.webSocket.streams )
               , test "session retirement holds physical navigation and detail slots until stale completion" <|
                    \_ ->
                        let
                            admitted = List.foldl (\id current -> DataLoading.beginNavigationBranch "project" "a" (Just id) current |> Tuple.first) (populated (WorkspacePage "a")) [ "one", "two", "three", "four" ] |> paintEmptyRoot
                            loading = admitted.dataLoading
                            before = { admitted | dataLoading = { loading | cardDetailAdmissions = Set.fromList [ 41, 42, 43, 44, 45, 46 ], nextCardDetailRequestId = 47 } }
                            retired = AppShell.handleOwned AuthUnauthorizedMsg before |> Tuple.first
                            authorized = refresh superadmin retired |> paintEmptyRoot
                            queued = DataLoading.beginNavigationBranch "project" "a" (Just "replacement") authorized |> Tuple.first
                            released = case Dict.get "project:one" before.dataLoading.loadedNavigationBranches of
                                Just request -> DataLoading.update (GotNavigationBranch "a" request.sessionEpoch request.generation "project:one" request.filterFingerprint 0 0 (Err Http.Timeout)) queued |> Tuple.first
                                Nothing -> queued
                        in
                        Expect.equal { physical = 4, details = 6, identity = 47, retired = True, queued = Just False, released = Just True, finalCap = 4 }
                            { physical = Dict.size retired.dataLoading.navigationAdmissions
                            , details = Set.size retired.dataLoading.cardDetailAdmissions
                            , identity = retired.dataLoading.nextCardDetailRequestId
                            , retired = Dict.isEmpty retired.dataLoading.loadedNavigationBranches && retired.sessionRequestEpoch > before.sessionRequestEpoch
                            , queued = Dict.get "project:replacement" queued.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight
                            , released = Dict.get "project:replacement" released.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight
                            , finalCap = Dict.size released.dataLoading.navigationAdmissions
                            }
               , test "stale epoch and wrong workspace session responses are inert" <|
                    \_ ->
                        let
                            before =
                                populated (WorkspacePage "a")
                        in
                        [ SessionContextLoadedMsg (before.sessionRequestEpoch - 1) (Just "a") (Ok reader)
                        , SessionContextLoadedMsg before.sessionRequestEpoch (Just "other") (Ok reader)
                        ]
                            |> List.map (\message -> AppShell.handleOwned message before |> Tuple.first |> .webSocket)
                            |> Expect.equal [ before.webSocket, before.webSocket ]
               , test "initial superadmin bootstrap waits for a canonical global snapshot" <|
                    \_ ->
                        let
                            after =
                                refresh superadmin (initial HomePage)
                        in
                        Expect.equal ( AuthReady, True, Dict.empty )
                            ( after.auth.status, after.dataLoading.loadingWorkspaces, after.webSocket.streams )
               , test "initial ordinary catalogue response matches the reserved load token" <|
                    \_ ->
                        let
                            before =
                                initial HomePage

                            after =
                                refresh reader before

                            loaded =
                                DataLoading.update (GotWorkspaces before.dataLoading.nextWorkspaceListLoadToken (Ok { items = [ workspace ], hasMore = False })) after |> Tuple.first
                        in
                        Expect.equal ( Just workspace, False ) ( Dict.get "a" loaded.workspaces, loaded.dataLoading.loadingWorkspaces )
               ]
            |> describe "actual session handler"


pageName : Page -> String
pageName page =
    case page of
        WorkspacePage _ ->
            "Workspace"

        _ ->
            "Home"


expectRegrantFence : ChangeStream.Scope -> Page -> Api.SessionContext -> Api.SessionContext -> Expect.Expectation
expectRegrantFence scope page allowed denied =
    let
        target =
            ChangeStream.scopeKey scope ++ "|entity:workspace:a"

        authorized =
            refresh allowed (populated page)

        socket =
            authorized.webSocket

        pending =
            requestWorkspace scope "before-revoke" { authorized | webSocket = { socket | targetGenerations = Dict.remove target socket.targetGenerations } }

        oldGuard =
            { scopeKey = ChangeStream.scopeKey scope, targetKey = "entity:workspace:a", targetGeneration = 1, sessionEpoch = if scope == ChangeStream.Global then pending.groups.catalogueEpoch else pending.sessionRequestEpoch, routeWorkspace = if scope == ChangeStream.Global then Nothing else pending.selectedWorkspaceId, audienceId = "actor" }

        requested =
            pending |> refresh denied |> refresh allowed |> requestWorkspace scope "after-regrant"

        generation =
            Dict.get target requested.webSocket.targetGenerations |> Maybe.withDefault 0

        stale =
            WebSocket.update (CanonicalWorkspaceFetched oldGuard "a" (Ok { workspace | name = "Stale" })) requested |> Tuple.first

        currentGuard =
            { oldGuard | targetGeneration = generation, sessionEpoch = if scope == ChangeStream.Global then requested.groups.catalogueEpoch else requested.sessionRequestEpoch }

        current =
            WebSocket.update (CanonicalWorkspaceFetched currentGuard "a" (Ok { workspace | name = "Current" })) stale |> Tuple.first
    in
    Expect.equal
        { firstGeneration = Just 1, generationAdvanced = True, staleIgnored = True, current = Just "Current" }
        { firstGeneration = Dict.get target pending.webSocket.targetGenerations
        , generationAdvanced = generation > oldGuard.targetGeneration
        , staleIgnored = stale.workspaces == requested.workspaces
        , current = Dict.get "a" current.workspaces |> Maybe.map .name
        }


requestWorkspace : ChangeStream.Scope -> String -> Model -> Model
requestWorkspace scope eventId model =
    let
        isGlobal =
            scope == ChangeStream.Global

        scopeFields =
            [ ( "scope", Encode.string (if isGlobal then "global" else "workspace") ) ]
                ++ (if isGlobal then [] else [ ( "workspace_id", Encode.string "a" ) ])

        event =
            Encode.object
                ([ ( "scope", Encode.string (if isGlobal then "global" else "workspace") ) ]
                    ++ [ ( "schema_version", Encode.int 1 )
                       , ( "event_id", Encode.string eventId )
                       , ( "workspace_id", if isGlobal then Encode.null else Encode.string "a" )
                       , ( "occurred_at", Encode.string "2026-10-02T00:00:00Z" )
                       , ( "transaction", Encode.object [ ( "id", Encode.string eventId ), ( "cause", Encode.string "rest" ), ( "request_id", Encode.null ) ] )
                       , ( "actor", Encode.object [ ( "type", Encode.string "system" ), ( "id", Encode.null ) ] )
                       , ( "entity", Encode.object [ ( "type", Encode.string "workspace" ), ( "id", Encode.string "a" ), ( "action", Encode.string "updated" ) ] )
                       , ( "invalidations", Encode.list identity [ Encode.object [ ( "kind", Encode.string "entity" ), ( "target", Encode.string "workspace:a" ) ] ] )
                       ]
                )
    in
    Encode.object
        [ ( "schema_version", Encode.int 1 )
        , ( "transport", Encode.string "frame" )
        , ( "scope", Encode.object scopeFields )
        , ( "frame", Encode.object [ ( "schema_version", Encode.int 1 ), ( "type", Encode.string "change" ), ( "event", event ) ] )
        ]
        |> Encode.encode 0
        |> WsMessageReceived
        |> (\message -> WebSocket.update message model |> Tuple.first)


refresh : Api.SessionContext -> Model -> Model
refresh session model =
    AppShell.handleOwned (SessionContextLoadedMsg model.sessionRequestEpoch model.selectedWorkspaceId (Ok session)) model |> Tuple.first


initial : Page -> Model
initial page =
    AppShell.initModel Nothing
        { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/", query = Nothing, fragment = Nothing }
        page
        { apiUrl = "https://api.example", wsUrl = "wss://api.example/ws", sessionId = "test", runtimeMode = "test", authTokenStorageKey = "test", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }
        Nothing
        { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
        |> AppShell.finalizeInit page


populated : Page -> Model
populated page =
    let
        base =
            initial page

        loading =
            base.dataLoading

        groups =
            base.groups

        live scope =
            let
                stream =
                    ChangeStream.init scope [ "accepted-event" ]
            in
            { stream | live = True, resumeToken = Just "accepted-token" }

        workspaceStreams =
            case page of
                WorkspacePage _ ->
                    [ ( "workspace:a", live (ChangeStream.Workspace "a") ) ]

                _ ->
                    []

        workspaceGuards =
            case page of
                WorkspacePage _ ->
                    [ ( "workspace:a|entity:workspace:a", 4 ) ]

                _ ->
                    []
    in
    { base
        | auth = { status = AuthReady, mode = Just "test" }
        , sessionContext = Just superadmin
        , workspaces = Dict.singleton "a" workspace
        , groups = { groups | catalogueOwner = Just superadmin, workspaceGroups = Dict.singleton "g" { id = "g", name = "Group", description = Nothing, createdAt = "now", updatedAt = "now" } }
        , dataLoading = { loading | loadingWorkspaces = False, activeWorkspaceListLoadToken = Nothing, loadingWorkspaceData = False, activeWorkspaceLoadToken = Nothing, pendingWorkspaceLoads = 0 }
        , webSocket = { state = Connected, streams = Dict.fromList (( "global", live ChangeStream.Global ) :: workspaceStreams), targetGenerations = Dict.fromList (( "global|entity:workspace:a", 4 ) :: workspaceGuards) }
    }


globalGuard : Page -> Types.CanonicalRequestGuard
globalGuard page =
    { scopeKey = "global", targetKey = "entity:workspace:a", targetGeneration = 4, sessionEpoch = 0, routeWorkspace = Nothing, audienceId = "actor" }


superadmin : Api.SessionContext
superadmin =
    { authMode = "test", principal = { actorType = "user", actorId = "actor", actorLabel = "Actor", authority = "local", grantUserId = Nothing }, globalPermissions = { createWorkspace = True, superadmin = True }, workspace = Just { workspaceId = "a", role = Just "read", canRead = True, canEdit = False, canAdmin = False } }


reader : Api.SessionContext
reader =
    { superadmin | globalPermissions = { createWorkspace = False, superadmin = False } }


workspace : Api.Workspace
workspace =
    { id = "a", name = "Workspace", workspaceType = Api.Repository, ghOwner = Nothing, ghRepo = Nothing, createdAt = "now", updatedAt = "now" }


paintEmptyRoot : Model -> Model
paintEmptyRoot source =
    let
        prepared = DataLoading.prepareRootNavigationRequest (Just "a") source
        loaded = case prepared.dataLoading.rootNavigationRequest of
            Just request -> DataLoading.update (GotRootNavigation "a" request.sessionEpoch prepared.dataLoading.activeWorkspaceLoadToken request.generation request.filterFingerprint 0 0 (Ok { workspaceId = "a", projects = { items = [], hasMore = False }, tasks = { items = [], hasMore = False } })) prepared |> Cards.refreshViewport prepared |> Tuple.first
            Nothing -> prepared
        viewport = loaded.cards.viewport
    in
    Cards.updateViewport (Encode.object
        [ ( "workspace", Encode.string "a" ), ( "epoch", Encode.int loaded.sessionRequestEpoch ), ( "generation", Encode.int loaded.dataLoading.navigationGeneration ), ( "revision", Encode.int viewport.revision )
        , ( "paintNonce", Encode.int (DataLoading.backgroundPaintNonce loaded) ), ( "paintFilter", Encode.string (DataLoading.navigationFilterFingerprint loaded) )
        ]) loaded |> Tuple.first
