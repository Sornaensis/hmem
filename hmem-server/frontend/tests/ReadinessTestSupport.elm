module ReadinessTestSupport exposing (taskFetched, projectFetched, taskSummary, projectSummary, prepare)

import Api
import Helpers
import Http
import Types exposing (CanonicalReadinessTarget, CanonicalRequestGuard, Model, Msg(..))


taskSummary : Api.TaskOverview -> Api.TaskCardSummary
taskSummary overview =
    let task = overview.task
    in
    { id = task.id, workspaceId = task.workspaceId, projectId = task.projectId, parentId = task.parentId
    , title = task.title, status = task.status, priority = task.priority, completedAt = task.completedAt
    , dependencyCount = task.dependencyCount, createdAt = task.createdAt, updatedAt = task.updatedAt
    , directSubtaskCount = 0, hasChildren = False, readinessRollup = overview.readinessRollup
    }


projectSummary : Api.ProjectOverview -> Api.ProjectCardSummary
projectSummary overview =
    let project = overview.project
    in
    { id = project.id, workspaceId = project.workspaceId, parentId = project.parentId
    , name = project.name, status = project.status, priority = project.priority
    , createdAt = project.createdAt, updatedAt = project.updatedAt
    , directProjectCount = 0, directTaskCount = 0, hasChildren = False, readinessRollup = overview.readinessRollup
    }


taskFetched : CanonicalRequestGuard -> String -> Result Http.Error Api.TaskOverview -> Msg
taskFetched guard id result =
    CanonicalReadinessSummariesFetched (guard.routeWorkspace |> Maybe.withDefault "") [ { kind = "task", id = id, guard = guard } ]
        (case result of
            Ok overview -> Ok { projects = [], tasks = [ taskSummary overview ], missingProjectIds = [], missingTaskIds = [] }
            Err (Http.BadStatus 404) -> Ok { projects = [], tasks = [], missingProjectIds = [], missingTaskIds = [ id ] }
            Err error -> Err error
        )


projectFetched : CanonicalRequestGuard -> String -> Result Http.Error Api.ProjectOverview -> Msg
projectFetched guard id result =
    CanonicalReadinessSummariesFetched (guard.routeWorkspace |> Maybe.withDefault "") [ { kind = "project", id = id, guard = guard } ]
        (case result of
            Ok overview -> Ok { projects = [ projectSummary overview ], tasks = [], missingProjectIds = [], missingTaskIds = [] }
            Err (Http.BadStatus 404) -> Ok { projects = [], tasks = [], missingProjectIds = [ id ], missingTaskIds = [] }
            Err error -> Err error
        )


prepare : List ( String, String ) -> Model -> ( Model, List CanonicalReadinessTarget )
prepare targets model =
    List.foldl (\( kind, id ) ( current, guards ) ->
        let key = (if kind == "task" then "dependency-readiness:task:" else "project-overview:") ++ id
        in case Helpers.prepareProjectRequest current.selectedWorkspaceId current.sessionRequestEpoch key current of
            Just ( guard, updated ) ->
                ( Helpers.capturePlanningRead (guard.scopeKey ++ "|" ++ key) guard.targetGeneration updated
                , guards ++ [ { kind = kind, id = id, guard = guard } ] )
            Nothing -> ( current, guards )) ( model, [] ) targets
