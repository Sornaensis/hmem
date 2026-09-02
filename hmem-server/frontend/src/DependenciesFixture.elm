module DependenciesFixture exposing (main)

import Api
import AppShell
import Browser
import Dict
import Feature.Dependencies as Dependencies
import Helpers
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (attribute, class)
import Types exposing (Flags, Model, Msg, Page(..), WorkspaceTab(..))
import Url


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( fixtureModel, Cmd.none )
        , update = Dependencies.update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


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
    in
    { base
        | selectedWorkspaceId = Just workspaceId
        , sessionContext = Just editorSession
        , tasks =
            Dict.fromList
                [ ( dependentId, task dependentId "Current task" )
                , ( prerequisiteId, task prerequisiteId "Prerequisite" )
                ]
        , dependencies = { dependencies | taskDependencies = Dict.singleton dependentId [] }
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
