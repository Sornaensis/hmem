module WorkspaceRenameTest exposing (suite)

import Api
import AppShell exposing (AppShellOwnedMsg(..))
import Dict
import Expect
import Feature.Editing as Editing
import Feature.Mutations as Mutations
import Feature.WebSocket as WebSocket
import Helpers
import Route
import Test exposing (Test, describe, test)
import Types exposing (AuthStatus(..), CanonicalRequestGuard, Flags, Model, Msg(..), Page(..), WorkspaceTab(..))
import Url


suite : Test
suite =
    describe "workspace rename"
        [ test "success installs the canonical workspace and exits inline edit mode" <|
            \_ ->
                let
                    saving =
                        beginRename "Draft name"

                    requestId =
                        pendingRequestId saving |> Maybe.withDefault "missing-request-id"

                    canonical =
                        { workspace | name = "Canonical name", updatedAt = "2026-02-01T00:00:00Z" }

                    updated =
                        Mutations.update (WorkspaceUpdated requestId (Ok canonical)) saving
                            |> Tuple.first
                in
                Expect.equal
                    ( Just canonical, Nothing )
                    ( Dict.get workspaceId updated.workspaces, updated.editing.editState )
        , test "a local response arriving before a canonical invalidation remains authoritative until a newer refetch exists" <|
            \_ ->
                let
                    saving =
                        beginRename "Draft name"

                    requestId =
                        pendingRequestId saving |> Maybe.withDefault "missing-request-id"

                    localCanonical =
                        { workspace | name = "Local canonical" }

                    updated =
                        Mutations.update (WorkspaceUpdated requestId (Ok localCanonical)) saving
                            |> Tuple.first
                in
                Expect.equal ( Just localCanonical, Nothing ) ( Dict.get workspaceId updated.workspaces, updated.editing.editState )
        , test "structured server failure retains the attempted value for retry or cancel" <|
            \_ ->
                let
                    saving =
                        beginRename "   "

                    requestId =
                        pendingRequestId saving |> Maybe.withDefault "missing-request-id"

                    serverError =
                        Api.decodeApiErrorBody 400 "{\"error\":\"validation_failed\",\"message\":\"Workspace name cannot be blank.\"}"

                    updated =
                        Mutations.update (WorkspaceUpdated requestId (Err serverError)) saving
                            |> Tuple.first
                in
                Expect.equal
                    ( Just { value = "   ", original = "Original workspace", requestId = Nothing, error = Just "Workspace name cannot be blank." } )
                    (editSnapshot updated)
        , test "a stale rename correlation ID cannot overwrite the pending edit" <|
            \_ ->
                let
                    saving =
                        beginRename "First attempt"

                    staleResponse =
                        Mutations.update
                            (WorkspaceUpdated "superseded-request-id" (Ok { workspace | name = "Stale server value" }))
                            saving
                            |> Tuple.first
                in
                Expect.equal
                    ( pendingRequestId saving, Nothing, Just "Original workspace" )
                    ( pendingRequestId staleResponse, editError staleResponse, editOriginal staleResponse )
        , test "saving a non-workspace inline edit still exits edit mode" <|
            \_ ->
                let
                    saved =
                        workspaceModel
                            |> Editing.update (StartEdit "project" "project-1" "name" "Original project")
                            |> Tuple.first
                            |> Editing.update (EditInput "Draft project")
                            |> Tuple.first
                            |> Editing.update (SaveEdit "project-1" "name")
                            |> Tuple.first
                in
                Expect.equal Nothing saved.editing.editState
        , test "tab switch, Escape, and expand/edit preserve a pending workspace rename" <|
            \_ ->
                let
                    pending =
                        beginRename "Draft name"

                    switched =
                        AppShell.handleOwned (SwitchTabMsg TimelineTab) pending |> Tuple.first

                    escaped =
                        AppShell.handleOwned (GlobalKeyDownMsg 27) pending |> Tuple.first

                    expanded =
                        Editing.update (ExpandAndEdit "project-1" "project" "project-1" "name" "Project") pending |> Tuple.first
                in
                Expect.equal
                    ( editSnapshot pending, editSnapshot pending, editSnapshot pending )
                    ( editSnapshot switched, editSnapshot escaped, editSnapshot expanded )
        , test "the URL change after a tab switch preserves a pending workspace rename" <|
            \_ ->
                let
                    pending =
                        beginRename "Draft name"

                    afterRoute =
                        tabRouteTransition TimelineTab pending
                in
                Expect.equal (editSnapshot pending) (editSnapshot afterRoute)
        , test "the URL change after a tab switch preserves an errored workspace rename for retry" <|
            \_ ->
                let
                    pending =
                        beginRename "Draft name"

                    requestId =
                        pendingRequestId pending |> Maybe.withDefault "missing-request-id"

                    errored =
                        Mutations.update
                            (WorkspaceUpdated requestId (Err (Api.decodeApiErrorBody 400 "{\"error\":\"validation_error\",\"message\":\"Invalid\"}")))
                            pending
                            |> Tuple.first

                    afterRoute =
                        tabRouteTransition TimelineTab errored

                    retried =
                        Editing.update (SaveEdit workspaceId "name") afterRoute |> Tuple.first
                in
                Expect.equal
                    ( editSnapshot errored, True )
                    ( editSnapshot afterRoute, pendingRequestId retried /= Nothing )
        , test "an errored workspace rename is not implicitly retried or replaced and remains cancellable" <|
            \_ ->
                let
                    pending =
                        beginRename "Draft name"

                    requestId =
                        pendingRequestId pending |> Maybe.withDefault "missing-request-id"

                    errored =
                        Mutations.update
                            (WorkspaceUpdated requestId (Err (Api.decodeApiErrorBody 400 "{\"error\":\"validation_error\",\"message\":\"Invalid\"}")))
                            pending
                            |> Tuple.first

                    expanded =
                        Editing.update (ExpandAndEdit "project-1" "project" "project-1" "name" "Project") errored |> Tuple.first

                    directStart =
                        Editing.update (StartEdit "project" "project-1" "name" "Project") errored |> Tuple.first

                    retried =
                        Editing.update (SaveEdit workspaceId "name") errored |> Tuple.first

                    cancelled =
                        Editing.update CancelEdit errored |> Tuple.first
                in
                Expect.equal
                    { expanded = editSnapshot errored, directStart = editSnapshot errored, retried = True, cancelled = Nothing }
                    { expanded = editSnapshot expanded, directStart = editSnapshot directStart, retried = pendingRequestId retried /= Nothing, cancelled = cancelled.editing.editState }
        , test "a WebSocket invalidation before the local response retains the newer canonical refetch" <|
            \_ ->
                let
                    saving =
                        remoteModel
                            |> Editing.update (StartEdit "workspace" workspaceId "name" workspace.name)
                            |> Tuple.first
                            |> Editing.update (EditInput "Local draft")
                            |> Tuple.first
                            |> Editing.update (SaveEdit workspaceId "name")
                            |> Tuple.first

                    requestId =
                        pendingRequestId saving |> Maybe.withDefault "missing-request-id"

                    existingWebSocket =
                        saving.webSocket

                    nextWebSocket =
                        { existingWebSocket
                            | targetGenerations = Dict.insert ("workspace:" ++ workspaceId ++ "|entity:workspace:" ++ workspaceId) 3 existingWebSocket.targetGenerations
                        }

                    eventFirst =
                        { saving
                            | workspaces = Dict.insert workspaceId { workspace | name = "Remote canonical" } saving.workspaces
                            , webSocket = nextWebSocket
                        }

                    afterLocalResponse =
                        Mutations.update (WorkspaceUpdated requestId (Ok { workspace | name = "Stale local response" })) eventFirst
                            |> Tuple.first

                    guard =
                        { scopeKey = "workspace:" ++ workspaceId
                        , targetKey = "entity:workspace:" ++ workspaceId
                        , targetGeneration = 3
                        , sessionEpoch = 3
                        , routeWorkspace = Just workspaceId
                        , audienceId = "editor"
                        }

                    refetched =
                        { workspace | name = "Newer canonical refetch" }

                    converged =
                        WebSocket.update (CanonicalWorkspaceFetched guard workspaceId (Ok refetched)) afterLocalResponse
                            |> Tuple.first
                in
                Expect.equal
                    ( Just "Remote canonical", Nothing, Just "Newer canonical refetch" )
                    ( Dict.get workspaceId afterLocalResponse.workspaces |> Maybe.map .name, afterLocalResponse.editing.editState, Dict.get workspaceId converged.workspaces |> Maybe.map .name )
        , test "a current remote canonical workspace response converges while an older response is ignored" <|
            \_ ->
                let
                    guard =
                        { scopeKey = "workspace:" ++ workspaceId
                        , targetKey = "entity:workspace:" ++ workspaceId
                        , targetGeneration = 2
                        , sessionEpoch = 3
                        , routeWorkspace = Just workspaceId
                        , audienceId = "editor"
                        }

                    staleGuard =
                        { guard | targetGeneration = 1 }

                    source =
                        remoteModel

                    canonical =
                        { workspace | name = "Renamed by another client", updatedAt = "2026-02-02T00:00:00Z" }

                    converged =
                        WebSocket.update (CanonicalWorkspaceFetched guard workspaceId (Ok canonical)) source
                            |> Tuple.first

                    afterStale =
                        WebSocket.update (CanonicalWorkspaceFetched staleGuard workspaceId (Ok { workspace | name = "Older response" })) converged
                            |> Tuple.first
                in
                Expect.equal
                    ( Just canonical, Just canonical )
                    ( Dict.get workspaceId converged.workspaces, Dict.get workspaceId afterStale.workspaces )
        ]


beginRename : String -> Model
beginRename value =
    workspaceModel
        |> Editing.update (StartEdit "workspace" workspaceId "name" workspace.name)
        |> Tuple.first
        |> Editing.update (EditInput value)
        |> Tuple.first
        |> Editing.update (SaveEdit workspaceId "name")
        |> Tuple.first


tabRouteTransition : WorkspaceTab -> Model -> Model
tabRouteTransition tab source =
    let
        afterSwitch =
            AppShell.handleOwned (SwitchTabMsg tab) source |> Tuple.first

        routeUrl =
            { url | fragment = Just (Helpers.buildFragment tab Nothing Nothing) }
    in
    Route.handleUrlChange routeUrl afterSwitch |> Tuple.first


pendingRequestId : Model -> Maybe String
pendingRequestId source =
    case source.editing.editState of
        Just (Types.EditingField state) ->
            state.requestId

        Nothing ->
            Nothing


type alias EditSnapshot =
    { value : String
    , original : String
    , requestId : Maybe String
    , error : Maybe String
    }


editSnapshot : Model -> Maybe EditSnapshot
editSnapshot source =
    case source.editing.editState of
        Just (Types.EditingField state) ->
            Just { value = state.value, original = state.original, requestId = state.requestId, error = state.error }

        Nothing ->
            Nothing


editError : Model -> Maybe String
editError source =
    editSnapshot source |> Maybe.andThen .error


editOriginal : Model -> Maybe String
editOriginal source =
    editSnapshot source |> Maybe.map .original


workspaceModel : Model
workspaceModel =
    AppShell.initModel Nothing url (WorkspacePage workspaceId) flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
        |> AppShell.finalizeInit (WorkspacePage workspaceId)
        |> (\base -> { base | workspaces = Dict.singleton workspaceId workspace })


remoteModel : Model
remoteModel =
    let
        baseWebSocket =
            workspaceModel.webSocket
    in
    { workspaceModel
        | auth = { status = AuthReady, mode = Just "test" }
        , sessionContext = Just editorSession
        , sessionRequestEpoch = 3
        , selectedWorkspaceId = Just workspaceId
        , webSocket =
            { baseWebSocket
                | targetGenerations = Dict.singleton ("workspace:" ++ workspaceId ++ "|entity:workspace:" ++ workspaceId) 2
            }
    }


workspaceId : String
workspaceId =
    "workspace-1"


workspace : Api.Workspace
workspace =
    { id = workspaceId
    , name = "Original workspace"
    , workspaceType = Api.Repository
    , ghOwner = Just "owner"
    , ghRepo = Just "repo"
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    }


editorSession : Api.SessionContext
editorSession =
    { authMode = "test"
    , principal = { actorType = "user", actorId = "editor", actorLabel = "Editor", authority = "local", grantUserId = Nothing }
    , globalPermissions = { createWorkspace = False, superadmin = False }
    , workspace = Just { workspaceId = workspaceId, role = Just "edit", canRead = True, canEdit = True, canAdmin = False }
    }


flags : Flags
flags =
    { apiUrl = "https://api.example"
    , wsUrl = "wss://api.example"
    , sessionId = "session-1"
    , runtimeMode = "test"
    , authTokenStorageKey = "hmem-auth-token"
    , authTokenPresent = False
    , loginUrl = Nothing
    , logoutUrl = Nothing
    }


url : Url.Url
url =
    { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/workspace-1", query = Nothing, fragment = Nothing }
