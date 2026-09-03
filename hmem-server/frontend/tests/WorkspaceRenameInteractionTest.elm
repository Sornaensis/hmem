module WorkspaceRenameInteractionTest exposing (suite)

import Api
import AppShell
import Expect
import Feature.Editing as Editing
import Feature.Mutations as Mutations
import Test exposing (Test, describe, test)
import Types exposing (Flags, Model, Msg(..), Page(..), WorkspaceTab(..))
import Url


suite : Test
suite =
    describe "workspace rename interaction ordering"
        [ test "an errored edit suppresses the blur save that precedes Cancel" <|
            \_ ->
                let
                    pending =
                        workspaceModel
                            |> Editing.update (StartEdit "workspace" workspaceId "name" "Original")
                            |> Tuple.first
                            |> Editing.update (EditInput "Draft")
                            |> Tuple.first
                            |> Editing.update (SaveEdit workspaceId "name")
                            |> Tuple.first

                    requestId =
                        editRequestId pending |> Maybe.withDefault "missing-request-id"

                    failed =
                        Mutations.update
                            (WorkspaceUpdated requestId (Err (Api.decodeApiErrorBody 400 "{\"error\":\"validation_error\",\"message\":\"Invalid\"}")))
                            pending
                            |> Tuple.first

                    cancelled =
                        Editing.update CancelEdit failed |> Tuple.first
                in
                Expect.equal
                    ( False, Nothing )
                    ( Editing.shouldSaveOnBlur False (editError failed), cancelled.editing.editState )
        , test "starting another field while a rename is pending retains its correlated state" <|
            \_ ->
                let
                    pending =
                        workspaceModel
                            |> Editing.update (StartEdit "workspace" workspaceId "name" "Original")
                            |> Tuple.first
                            |> Editing.update (EditInput "First")
                            |> Tuple.first
                            |> Editing.update (SaveEdit workspaceId "name")
                            |> Tuple.first

                    switched =
                        Editing.update (StartEdit "workspace" workspaceId "name" "Original") pending |> Tuple.first
                in
                Expect.equal (editSnapshot pending) (editSnapshot switched)
        ]


editError : Model -> Maybe String
editError source =
    case source.editing.editState of
        Just (Types.EditingField state) ->
            state.error

        Nothing ->
            Nothing


editRequestId : Model -> Maybe String
editRequestId source =
    case source.editing.editState of
        Just (Types.EditingField state) ->
            state.requestId

        Nothing ->
            Nothing


editSnapshot : Model -> Maybe { value : String, requestId : Maybe String }
editSnapshot source =
    case source.editing.editState of
        Just (Types.EditingField state) ->
            Just { value = state.value, requestId = state.requestId }

        Nothing ->
            Nothing


workspaceModel : Model
workspaceModel =
    AppShell.initModel Nothing url (WorkspacePage workspaceId) flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
        |> AppShell.finalizeInit (WorkspacePage workspaceId)


workspaceId : String
workspaceId =
    "workspace-rename-interaction"


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
    { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/workspace-rename-interaction", query = Nothing, fragment = Nothing }
