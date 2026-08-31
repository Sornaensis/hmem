module CardsFocusNavigationTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.Cards as Cards
import Feature.DataLoading as DataLoading
import Feature.Focus as Focus
import Test exposing (Test, describe, test)
import Types exposing (Flags, Model, Msg(..), Page(..), WorkspaceTab(..))
import Url


suite : Test
suite =
    describe "bounded Cards and Focus loading"
        [ test "an unloaded expanded project card starts exactly its branch request" <|
            \_ ->
                let
                    seeded =
                        DataLoading.mergeNavigationSummaries [ project "parent" ] [] model

                    expanded =
                        Cards.update (ToggleCardExpand "parent") seeded
                            |> Tuple.first
                in
                case Dict.get "project:parent" expanded.dataLoading.loadedNavigationBranches of
                    Just request ->
                        Expect.equal
                            { workspace = workspaceId, offset = 0, inFlight = True, succeeded = False }
                            { workspace = request.workspaceId, offset = request.projectOffset, inFlight = request.inFlight, succeeded = request.succeeded }

                    Nothing ->
                        Expect.fail "Expected an unloaded project expansion to request its branch"
        , test "direct focus requests only a target absent from the bounded card cache" <|
            \_ ->
                let
                    missing =
                        Focus.update (FocusEntity "project" "outside-root-page") model
                            |> Tuple.first

                    knownModel =
                        DataLoading.mergeNavigationSummaries [ project "inside-root-page" ] [] model

                    known =
                        Focus.update (FocusEntity "project" "inside-root-page") knownModel
                            |> Tuple.first
                in
                Expect.equal
                    { missingTarget = Just "outside-root-page"
                    , missingInFlight = True
                    , knownHasNoRequest = True
                    }
                    { missingTarget = Dict.get "project:outside-root-page" missing.dataLoading.navigationFocuses |> Maybe.map .entityId
                    , missingInFlight = missing.dataLoading.activeNavigationFocus |> Maybe.map .inFlight |> Maybe.withDefault False
                    , knownHasNoRequest = Dict.member "project:inside-root-page" known.dataLoading.navigationFocuses |> not
                    }
        ]


workspaceId : String
workspaceId =
    "workspace-1"


model : Model
model =
    AppShell.initModel Nothing url (WorkspacePage workspaceId) flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
        |> AppShell.finalizeInit (WorkspacePage workspaceId)


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


project : String -> Api.ProjectCardSummary
project id =
    { id = id
    , workspaceId = workspaceId
    , parentId = Nothing
    , name = id
    , status = Api.ProjActive
    , priority = 1
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    , directProjectCount = 0
    , directTaskCount = 0
    , hasChildren = True
    , readinessRollup = { openProjectCount = 0, closedProjectCount = 0, openTaskCount = 0, doneTaskCount = 0, cancelledTaskCount = 0, blockedTaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = True }
    }
