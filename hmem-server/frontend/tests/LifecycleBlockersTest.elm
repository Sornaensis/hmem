module LifecycleBlockersTest exposing (suite)

import Api
import Expect
import Feature.Cards
import Feature.DataLoading
import String
import Test exposing (..)


suite : Test
suite =
    describe "lifecycle blocker frontend handling"
        [ test "task completion lifecycle errors use task-specific copy and blocker detail" <|
            \_ ->
                let
                    body =
                        """{"error":"lifecycle_conflict","code":"TASK_COMPLETION_BLOCKED","message":"Cannot mark task done while descendant tasks are still open.","hint":"Complete or cancel open descendant tasks first.","required_action":"Complete or cancel open descendant tasks first.","detail":{"blocker_count":2,"blocker_ids":["aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee","11111111-2222-3333-4444-555555555555"]}}"""
                in
                Api.decodeApiErrorBody 409 body
                    |> Api.apiErrorToUserMessage "Failed to update task"
                    |> Expect.equal "Finish or cancel all subtasks before marking this task done. (2 blockers; examples: aaaaaaaa, 11111111)"
        , test "project completion lifecycle errors use project-specific copy and counts" <|
            \_ ->
                let
                    body =
                        """{"error":"lifecycle_conflict","code":"PROJECT_COMPLETION_BLOCKED","message":"Cannot close project while descendant projects or tasks are still open.","hint":"Complete or archive child projects and complete or cancel open tasks first.","required_action":"Complete or archive child projects and complete or cancel open tasks first.","detail":{"open_project_count":1,"open_project_ids":["aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"],"open_task_count":3,"open_task_ids":["11111111-2222-3333-4444-555555555555"]}}"""
                in
                Api.decodeApiErrorBody 409 body
                    |> Api.apiErrorToUserMessage "Failed to update project"
                    |> Expect.equal "Complete/archive child projects and finish or cancel all tasks before closing this project. (1 open project, 3 open tasks; examples: aaaaaaaa, 11111111)"
        , test "create under done or closed ancestors uses reopen guidance" <|
            \_ ->
                let
                    doneTaskBody =
                        """{"error":"lifecycle_conflict","code":"TASK_OPEN_UNDER_DONE_TASK","message":"Cannot place an open task under a done task.","hint":"Reopen the ancestor task before adding or reopening open subtasks."}"""

                    closedProjectBody =
                        """{"error":"lifecycle_conflict","code":"TASK_OPEN_UNDER_CLOSED_PROJECT","message":"Cannot place an open task inside a completed or archived project.","hint":"Reopen the project before adding or reopening open tasks."}"""
                in
                [ Api.decodeApiErrorBody 409 doneTaskBody
                    |> Api.apiErrorToUserMessage "Failed to create task"
                , Api.decodeApiErrorBody 409 closedProjectBody
                    |> Api.apiErrorToUserMessage "Failed to create task"
                ]
                    |> Expect.equal
                        [ "Reopen the parent task before adding or reopening open subtasks."
                        , "Reopen the project before adding or reopening open tasks."
                        ]
        , test "known project and task blockers produce control tooltip reasons" <|
            \_ ->
                [ Feature.Cards.projectCompletionBlockerReason 1 2
                , Feature.Cards.taskCompletionBlockerReason 3
                ]
                    |> Expect.equal
                        [ Just "Complete/archive 1 child project and finish/cancel 2 tasks before closing this project."
                        , Just "Finish or cancel 3 subtasks before marking this task done."
                        ]
        , test "workspace data pagination advances only when a non-empty page has more results" <|
            \_ ->
                [ Feature.DataLoading.nextPageOffset 0 { items = [ "a", "b" ], hasMore = True }
                , Feature.DataLoading.nextPageOffset 200 { items = [ "c" ], hasMore = False }
                , Feature.DataLoading.nextPageOffset 200 { items = [], hasMore = True }
                , Feature.DataLoading.nextPageOffset 9800 { items = List.repeat 200 "x", hasMore = True }
                , Feature.DataLoading.nextPageOffset 10000 { items = [ "x" ], hasMore = True }
                ]
                    |> Expect.equal [ Just 2, Nothing, Nothing, Just 10000, Nothing ]
        , test "task cascade delete preview covers empty, small, and large descendant trees" <|
            \_ ->
                let
                    root =
                        task "root" Nothing (Just "project-a")

                    smallTree =
                        [ root
                        , task "child-a" (Just "root") (Just "project-a")
                        , task "child-b" (Just "root") (Just "project-a")
                        , task "unrelated" Nothing (Just "project-a")
                        ]

                    largeTree =
                        root
                            :: (List.range 1 30
                                    |> List.map (\n -> task ("child-" ++ String.fromInt n) (Just "root") (Just "project-a"))
                               )
                in
                [ Feature.Cards.taskCascadePreview "root" [ root ] |> Maybe.map .taskCount
                , Feature.Cards.taskCascadePreview "root" smallTree |> Maybe.map .taskCount
                , Feature.Cards.taskCascadePreview "root" largeTree |> Maybe.map .taskCount
                , Feature.Cards.taskCascadePreview "missing" smallTree |> Maybe.map .taskCount
                ]
                    |> Expect.equal [ Just 1, Just 3, Just 31, Nothing ]
        , test "project cascade delete preview counts subprojects, tasks, and task descendants" <|
            \_ ->
                let
                    projects =
                        [ project "root-project" Nothing
                        , project "child-project" (Just "root-project")
                        , project "grandchild-project" (Just "child-project")
                        , project "unrelated-project" Nothing
                        ]

                    tasks =
                        [ task "root-task" Nothing (Just "root-project")
                        , task "child-project-task" Nothing (Just "child-project")
                        , task "drifted-descendant-task" (Just "child-project-task") (Just "unrelated-project")
                        , task "unrelated-task" Nothing (Just "unrelated-project")
                        ]
                in
                Feature.Cards.projectCascadePreview "root-project" projects tasks
                    |> Expect.equal (Just { affected = 6, projectCount = 3, taskCount = 3 })
        , test "stale cascade delete result communicates final server counts" <|
            \_ ->
                let
                    confirmation =
                        { entityType = "project"
                        , entityId = "root-project"
                        , preview = Just { affected = 2, projectCount = 1, taskCount = 1 }
                        }

                    result =
                        { affected = 4
                        , projectCount = 2
                        , taskCount = 2
                        , memoryCount = 2
                        , dependencyCount = 1
                        }
                in
                Feature.Cards.cascadeDeleteSuccessMessage confirmation result
                    |> Expect.equal "Deleted project subtree: 2 projects (including 1 subproject) and 2 tasks were deleted. Server counts changed since preview (preview: 1 project and 1 task; final: 2 projects and 2 tasks). Also updated 2 linked memories and 1 task dependency."
        , test "cascade delete error fallback prompts a refresh-safe retry" <|
            \_ ->
                Feature.Cards.cascadeDeleteFailureFallback { entityType = "task", entityId = "root", preview = Nothing }
                    |> Expect.equal "Failed to delete task. The item may already have changed; refreshing workspace data."
        ]


project : String -> Maybe String -> Api.Project
project id parentId =
    { id = id
    , workspaceId = "workspace-a"
    , parentId = parentId
    , name = id
    , description = Nothing
    , status = Api.ProjActive
    , priority = 5
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    }


task : String -> Maybe String -> Maybe String -> Api.Task
task id parentId projectId =
    { id = id
    , workspaceId = "workspace-a"
    , projectId = projectId
    , parentId = parentId
    , title = id
    , description = Nothing
    , status = Api.Todo
    , priority = 5
    , dueAt = Nothing
    , completedAt = Nothing
    , dependencyCount = 0
    , memoryLinkCount = 0
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    }
