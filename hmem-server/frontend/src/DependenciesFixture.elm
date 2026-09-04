module DependenciesFixture exposing (main)

import Api
import AppShell
import Browser
import Dict
import Feature.Dependencies as Dependencies
import Feature.WebSocket as WebSocket
import Helpers
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (attribute, class)
import Ports exposing (wsMessage)
import Set
import Types exposing (AuthStatus(..), Flags, Model, Msg(..), Page(..), WorkspaceTab(..))
import Url


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( fixtureModel, Cmd.none )
        , update = update
        , subscriptions = \_ -> wsMessage WsMessageReceived
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WsMessageReceived _ ->
            WebSocket.update msg model

        _ ->
            Dependencies.update msg model


view : Model -> Html Msg
view model =
    div [ class "dependencies-fixture", attribute "data-testid" "dependencies-fixture" ]
        [ h1 [] [ text "Dependency selector fixture" ]
        , p [] [ text "Select Prerequisite from + Dep to issue the production dependency command." ]
        , Dependencies.viewTaskDependencies model dependentId (Helpers.taskDependencySummariesForTask model dependentId)
        ]


fixtureModel : Model
fixtureModel =
    let
        base =
            AppShell.initModel Nothing url (WorkspacePage workspaceId) flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
                |> AppShell.finalizeInit (WorkspacePage workspaceId)

        dependencies =
            base.dependencies

        cards =
            base.cards

        loading =
            base.dataLoading

        search =
            base.search
    in
    { base
        | selectedWorkspaceId = Just workspaceId
        , auth = { status = AuthReady, mode = Just "test" }
        , sessionContext = Just editorSession
        , cards = { cards | expandedCards = Dict.singleton dependentId True }
        , dataLoading =
            { loading
                | taskCardSummaries = Dict.singleton dependentId dependentTaskCardSummary
                , navigationVisibleTaskIds = Set.singleton dependentId
                , navigationVisibilityActive = True
            }
        , search =
            { search
                | query = "selected query"
                , unifiedResults = Just { observations = [], projects = [], tasks = [] }
                , isSearching = True
                , activeRequestQuery = Just "selected query"
                , activeRequest = Just { workspaceId = workspaceId, token = 5, query = "selected query" }
                , nextRequestToken = 6
            }
        , tasks =
            Dict.fromList
                [ ( dependentId, task dependentId "Current task" )
                , ( prerequisiteId, task prerequisiteId "Prerequisite" )
                ]
        , dependencies = { dependencies | taskDependencies = Dict.singleton dependentId [] }
    }


dependentTaskCardSummary : Api.TaskCardSummary
dependentTaskCardSummary =
    { id = dependentId
    , workspaceId = workspaceId
    , projectId = Nothing
    , parentId = Nothing
    , title = "Current task"
    , status = Api.Todo
    , priority = 1
    , dueAt = Nothing
    , completedAt = Nothing
    , dependencyCount = 0
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    , directSubtaskCount = 0
    , hasChildren = False
    , readinessRollup =
        { openSubtaskCount = 0
        , doneSubtaskCount = 0
        , cancelledSubtaskCount = 0
        , blockedSubtaskCount = 0
        , dependencyBlockedTaskCount = 0
        , openDependencyCount = 0
        , completionReady = True
        }
    }


workspaceId : String
workspaceId =
    "workspace-1"


dependentId : String
dependentId =
    "current-task"


prerequisiteId : String
prerequisiteId =
    "selected-prerequisite"


task : String -> String -> Api.Task
task id title =
    { id = id
    , workspaceId = workspaceId
    , projectId = Nothing
    , parentId = Nothing
    , title = title
    , description = Nothing
    , status = Api.Todo
    , priority = 1
    , dueAt = Nothing
    , completedAt = Nothing
    , dependencyCount = 0
    , memoryLinkCount = 0
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    }


flags : Flags
flags =
    { apiUrl = ""
    , wsUrl = ""
    , sessionId = "fixture-session"
    , runtimeMode = "test"
    , authTokenStorageKey = "hmem-auth-token"
    , authTokenPresent = False
    , loginUrl = Nothing
    , logoutUrl = Nothing
    }


url : Url.Url
url =
    { protocol = Url.Http, host = "127.0.0.1", port_ = Nothing, path = "/workspace/workspace-1", query = Nothing, fragment = Nothing }


editorSession : Api.SessionContext
editorSession =
    { authMode = "test"
    , principal = { actorType = "user", actorId = "editor", actorLabel = "Editor", authority = "local", grantUserId = Nothing }
    , globalPermissions = { createWorkspace = False, superadmin = False }
    , workspace = Just { workspaceId = workspaceId, role = Just "edit", canRead = True, canEdit = True, canAdmin = False }
    }
