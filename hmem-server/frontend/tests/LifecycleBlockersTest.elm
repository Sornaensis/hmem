module LifecycleBlockersTest exposing (suite)

import Api
import Dict
import Expect
import Feature.AuditLog
import Feature.Cards
import Feature.DataLoading
import Feature.Editing
import Feature.Focus
import Feature.Memory
import Feature.Timeline
import Helpers
import Json.Decode as Decode
import Page.Workspace
import Permissions
import String
import Test exposing (..)
import Types exposing (FocusReturnSource(..), Msg(..), TimelineEntityFilter(..), TimelineEventFilter(..), WorkspaceTab(..))


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
        , test "memory creation validation details are shown to users" <|
            \_ ->
                let
                    body =
                        """{"error":"validation","message":"Request validation failed","details":["memory_type is required and must be short_term or long_term","project_id or task_id is required"]}"""
                in
                Api.decodeApiErrorBody 400 body
                    |> Api.apiErrorToUserMessage "Failed to create memory"
                    |> Expect.equal "memory_type is required and must be short_term or long_term; project_id or task_id is required"
        , test "known project and task blockers produce control tooltip reasons" <|
            \_ ->
                [ Feature.Cards.projectCompletionBlockerReason 1 2
                , Feature.Cards.taskCompletionBlockerReason 3
                ]
                    |> Expect.equal
                        [ Just "Complete/archive 1 child project and finish/cancel 2 tasks before closing this project."
                        , Just "Finish or cancel 3 subtasks before marking this task done."
                        ]
        , test "local implicit superadmin hides only local authorization presentation details" <|
            \_ ->
                let
                    localImplicitSuperadmin =
                        sessionContext "local" "user" "local_superadmin" True True (Just "admin")

                    deployedSuperadmin =
                        sessionContext "deployed" "user" "grant_user" True True (Just "admin")

                    deployedWorkspaceAdmin =
                        sessionContext "deployed" "user" "grant_user" False False (Just "admin")

                    localBot =
                        sessionContext "local" "bot" "bot_token" False False Nothing
                in
                [ Permissions.isImplicitLocalSuperadminSession localImplicitSuperadmin
                , Permissions.isImplicitLocalSuperadminSession deployedSuperadmin
                , Permissions.shouldShowAuthDetailsForSession Nothing
                , Permissions.shouldShowAuthDetailsForSession (Just localImplicitSuperadmin)
                , Permissions.shouldShowAuthDetailsForSession (Just deployedSuperadmin)
                , Permissions.shouldShowAuthDetailsForSession (Just deployedWorkspaceAdmin)
                , Permissions.shouldShowAuthDetailsForSession (Just localBot)
                ]
                    |> Expect.equal [ True, False, True, False, True, True, True ]
        , test "workspace data pagination advances only when a non-empty page has more results" <|
            \_ ->
                [ Feature.DataLoading.nextPageOffset 0 { items = [ "a", "b" ], hasMore = True }
                , Feature.DataLoading.nextPageOffset 200 { items = [ "c" ], hasMore = False }
                , Feature.DataLoading.nextPageOffset 200 { items = [], hasMore = True }
                , Feature.DataLoading.nextPageOffset 9800 { items = List.repeat 200 "x", hasMore = True }
                , Feature.DataLoading.nextPageOffset 10000 { items = [ "x" ], hasMore = True }
                ]
                    |> Expect.equal [ Just 2, Nothing, Nothing, Just 10000, Nothing ]
        , test "workspace summary hides partial counts while workspace data is still paging" <|
            \_ ->
                let
                    activeProject =
                        project "active-project" Nothing

                    closedProject =
                        projectWithStatus "closed-project" Nothing Api.ProjCompleted

                    openTask =
                        task "open-task" Nothing (Just "active-project")

                    blockedTask =
                        taskWithStatus "blocked-task" Nothing (Just "active-project") Api.Blocked

                    doneTask =
                        taskWithStatus "done-task" Nothing (Just "active-project") Api.Done

                    otherWorkspaceTask =
                        taskWithWorkspace "other-workspace-task" Nothing Nothing "workspace-b"

                    workspaceMemory =
                        memory "memory-a"

                    otherWorkspaceMemory =
                        memoryWithWorkspace "memory-b" "workspace-b"
                in
                [ Page.Workspace.workspaceSummaryParts True "workspace-a" [ activeProject ] [ openTask ] [ workspaceMemory ]
                , Page.Workspace.workspaceSummaryParts False "workspace-a" [ activeProject, closedProject ] [ openTask, blockedTask, doneTask, otherWorkspaceTask ] [ workspaceMemory, otherWorkspaceMemory ]
                ]
                    |> Expect.equal [ [], [ "1 open project", "2 open tasks", "1 memory" ] ]
        , test "timeline tab fragment round-trips and exposes a workspace tab label" <|
            \_ ->
                [ (Helpers.parseFragment (Just "tab=timeline")).tab == TimelineTab
                , Helpers.buildFragment TimelineTab Nothing == "tab=timeline"
                , (Helpers.parseFragment (Just "tab=timeline&focus=task:abc")).focus == Just ( "task", "abc" )
                , Page.Workspace.workspaceTabLabel TimelineTab == "Timeline"
                ]
                    |> Expect.equal [ True, True, True, True ]
        , test "workspace timeline event decoder preserves basic rendering fields" <|
            \_ ->
                let
                    json =
                        """{"id":"audit:11111111-1111-1111-1111-111111111111","workspace_id":"workspace-1","event_type":"subtask_completed","entity_type":"subtask","entity_id":"task-1","title":"Child task","occurred_at":"2026-05-25T00:00:00Z","actor":{"type":"bot","id":"actor-1","label":"Timeline bot"},"project":{"id":"project-1","name":"Timeline project"},"parent_task":{"id":"parent-1","title":"Parent task"},"status_transition":{"from":"todo","to":"done"},"navigation":{"entity_type":"task","entity_id":"task-1"},"source_audit_id":"11111111-1111-1111-1111-111111111111"}"""
                in
                case Decode.decodeString Api.workspaceTimelineEventDecoder json of
                    Ok event ->
                        [ Feature.Timeline.timelineEventLabel event.eventType
                        , Feature.Timeline.timelineEventToneClass event.eventType
                        , event.entityType
                        , event.navigation.entityType
                        , event.navigation.entityId
                        , event.project |> Maybe.map .name |> Maybe.withDefault ""
                        , event.parentTask |> Maybe.map .title |> Maybe.withDefault ""
                        , Feature.Timeline.timelineStatusSummary event.statusTransition |> Maybe.withDefault ""
                        ]
                            |> Expect.equal [ "Subtask completed", "timeline-event-completed", "subtask", "task", "task-1", "Timeline project", "Parent task", "Todo → Done" ]

                    Err error ->
                        Expect.fail (Decode.errorToString error)
        , test "workspace timeline bucket decoder preserves action and entity counts" <|
            \_ ->
                let
                    json =
                        """{"workspace_id":"workspace-a","since":"2026-01-01T00:00:00Z","until":"2026-02-01T00:00:00Z","bucket":"week","buckets":[{"bucket_start":"2026-01-01T00:00:00Z","bucket_end":"2026-01-08T00:00:00Z","label":"2026-01-01","counts":{"project":{"created":1,"completed":0,"cancelled":0},"subproject":{"created":1,"completed":1,"cancelled":0},"task":{"created":2,"completed":0,"cancelled":1},"subtask":{"created":1,"completed":1,"cancelled":0}},"totals":{"created":5,"completed":2,"cancelled":1}}]}"""
                in
                case Decode.decodeString Api.workspaceTimelineBucketsResponseDecoder json of
                    Ok response ->
                        case response.buckets of
                            bucket :: [] ->
                                [ response.workspaceId
                                , response.bucket
                                , bucket.label
                                , String.fromInt (Feature.Timeline.timelineBucketTotal bucket)
                                , String.fromInt bucket.counts.project.created
                                , String.fromInt bucket.counts.subproject.completed
                                , String.fromInt bucket.counts.task.cancelled
                                , String.fromInt bucket.counts.subtask.completed
                                ]
                                    |> Expect.equal [ "workspace-a", "week", "2026-01-01", "8", "1", "1", "1", "1" ]

                            _ ->
                                Expect.fail "Expected one decoded bucket"

                    Err error ->
                        Expect.fail (Decode.errorToString error)
        , test "workspace timeline events sort newest first with a stable tie-breaker" <|
            \_ ->
                [ timelineEvent "older" "2026-05-24T00:00:00Z" "audit-older"
                , timelineEvent "same-a" "2026-05-25T00:00:00Z" "audit-a"
                , timelineEvent "newer" "2026-05-26T00:00:00Z" "audit-newer"
                , timelineEvent "same-z" "2026-05-25T00:00:00Z" "audit-z"
                ]
                    |> Feature.Timeline.sortTimelineEvents
                    |> List.map .id
                    |> Expect.equal [ "newer", "same-z", "same-a", "older" ]
        , test "workspace timeline filter state defaults to the curated all-events view" <|
            \_ ->
                [ Feature.Timeline.init.entityFilter == TimelineAllEntities
                , Feature.Timeline.init.eventFilter == TimelineAllEvents
                ]
                    |> Expect.equal [ True, True ]
        , test "timeline and audit navigation create return context metadata" <|
            \_ ->
                let
                    timelineContext =
                        timelineEvent "timeline-event" "2026-05-25T00:00:00Z" "audit-source"
                            |> Feature.Focus.timelineReturnContext "workspace-a" Feature.Timeline.init

                    auditContextResult =
                        decodeAuditFixture createProjectAuditJson
                            |> Result.map
                                (\entry ->
                                    ( Feature.Focus.auditReturnContext ReturnFromWorkspaceAudit "workspace-a" Feature.AuditLog.init.filters False entry ( "project", "project-1" )
                                    , Feature.Focus.auditReturnContext ReturnFromWorkspaceAudit "workspace-a" Feature.AuditLog.init.filters True entry ( "project", "project-1" )
                                    )
                                )
                in
                case auditContextResult of
                    Ok ( auditContext, expandedAuditContext ) ->
                        [ timelineContext.source == ReturnFromTimeline
                        , timelineContext.tab == TimelineTab
                        , timelineContext.entryId == "timeline-event"
                        , timelineContext.timelineSourceAuditId == Just "audit-source"
                        , timelineContext.timelineEntityFilter == Just TimelineAllEntities
                        , timelineContext.entityType == "task"
                        , timelineContext.entityId == "timeline-event"
                        , auditContext.source == ReturnFromWorkspaceAudit
                        , auditContext.tab == AuditTab
                        , auditContext.entityType == "project"
                        , auditContext.entityId == "project-1"
                        , auditContext.auditFilters == Just Feature.AuditLog.init.filters
                        , auditContext.auditExpandedEntryId == Nothing
                        , auditContext.auditEntryExpanded == Just False
                        , expandedAuditContext.auditExpandedEntryId == Just expandedAuditContext.entryId
                        , expandedAuditContext.auditEntryExpanded == Just True
                        ]
                            |> Expect.equal [ True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, True ]

                    Err error ->
                        Expect.fail (Decode.errorToString error)
        , test "direct focus helpers clear source return context" <|
            \_ ->
                let
                    context =
                        timelineEvent "timeline-event" "2026-05-25T00:00:00Z" "audit-source"
                            |> Feature.Focus.timelineReturnContext "workspace-a" Feature.Timeline.init

                    initialFocus =
                        Feature.Focus.init (Just ( "task", "timeline-event" ))

                    focusWithContext =
                        { initialFocus | returnContext = Just context }
                in
                [ Feature.Focus.shouldShowReturnContext focusWithContext
                , Feature.Focus.focusReturnContextLabel context == "Back to Timeline event"
                , (Feature.Focus.clearReturnContext focusWithContext |> .returnContext) == Nothing
                ]
                    |> Expect.equal [ True, True, True ]
        , test "audit return filters preserve source pages for accumulated and returned windows" <|
            \_ ->
                case decodeAuditFixture createProjectAuditJson of
                    Ok baseEntry ->
                        let
                            entryWithId n =
                                { baseEntry | id = "entry-" ++ String.fromInt n }

                            accumulatedEntries =
                                List.range 0 59 |> List.map entryWithId

                            returnedWindowEntries =
                                List.range 150 159 |> List.map entryWithId

                            initialAuditLog =
                                Feature.AuditLog.init

                            initialAuditFilters =
                                initialAuditLog.filters

                            filtersAtOffset offset =
                                { initialAuditFilters | offset = Just offset }

                            accumulatedAudit =
                                { initialAuditLog
                                    | entries = accumulatedEntries
                                    , entryBaseOffset = 0
                                    , filters = filtersAtOffset 50
                                }

                            returnedWindowAudit =
                                { initialAuditLog
                                    | entries = returnedWindowEntries
                                    , entryBaseOffset = 150
                                    , filters = filtersAtOffset 150
                                }
                        in
                        [ Feature.AuditLog.nextAuditOffset accumulatedAudit
                        , Feature.AuditLog.auditReturnFilters "entry-5" accumulatedAudit |> .offset |> Maybe.withDefault -1
                        , Feature.AuditLog.auditReturnFilters "entry-55" accumulatedAudit |> .offset |> Maybe.withDefault -1
                        , Feature.AuditLog.nextAuditOffset returnedWindowAudit
                        , Feature.AuditLog.auditReturnFilters "entry-155" returnedWindowAudit |> .offset |> Maybe.withDefault -1
                        ]
                            |> Expect.equal [ 60, -1, 50, 160, 150 ]

                    Err error ->
                        Expect.fail (Decode.errorToString error)
        , test "workspace timeline histogram accepts only the active range request" <|
            \_ ->
                let
                    activeRequest =
                        { workspaceId = "workspace-a"
                        , since = "2026-01-01T00:00:00Z"
                        , until = "2026-02-01T00:00:00Z"
                        , bucket = "week"
                        }

                    staleRequest =
                        { activeRequest | until = "2026-03-01T00:00:00Z" }

                    initialTimeline =
                        Feature.Timeline.init

                    timeline =
                        { initialTimeline | histogramActiveRequest = Just activeRequest }
                in
                [ Feature.Timeline.timelineHistogramAcceptsResponse activeRequest timeline
                , Feature.Timeline.timelineHistogramAcceptsResponse staleRequest timeline
                , Feature.Timeline.timelineHistogramAcceptsResponse activeRequest { timeline | histogramActiveRequest = Nothing }
                ]
                    |> Expect.equal [ True, False, False ]
        , test "workspace timeline histogram selection uses inclusive start and exclusive end" <|
            \_ ->
                let
                    selection =
                        { label = "Jan 1"
                        , since = "2026-01-01T00:00:00Z"
                        , until = "2026-01-02T00:00:00Z"
                        }
                in
                [ timelineEvent "before" "2025-12-31T23:59:59.999Z" "audit-before"
                , timelineEvent "start" "2026-01-01T00:00:00Z" "audit-start"
                , timelineEvent "start-fractional" "2026-01-01T00:00:00.123Z" "audit-start-fractional"
                , timelineEvent "inside" "2026-01-01T12:00:00Z" "audit-inside"
                , timelineEvent "end" "2026-01-02T00:00:00Z" "audit-end"
                ]
                    |> List.filter (Feature.Timeline.eventInTimelineSelection (Just selection))
                    |> List.map .id
                    |> Expect.equal [ "start", "start-fractional", "inside" ]
        , test "workspace timeline selected bucket combines with entity and lifecycle filters" <|
            \_ ->
                let
                    selection =
                        { label = "Jan 1"
                        , since = "2026-01-01T00:00:00Z"
                        , until = "2026-01-02T00:00:00Z"
                        }

                    insideTaskCompleted =
                        timelineEvent "inside-task" "2026-01-01T10:00:00Z" "audit-inside-task"
                            |> (\event -> { event | entityType = "task", eventType = "task_completed" })

                    insideProjectCompleted =
                        timelineEvent "inside-project" "2026-01-01T11:00:00Z" "audit-inside-project"
                            |> (\event -> { event | entityType = "project", eventType = "project_completed" })

                    outsideTaskCompleted =
                        timelineEvent "outside-task" "2026-01-02T10:00:00Z" "audit-outside-task"
                            |> (\event -> { event | entityType = "task", eventType = "task_completed" })
                in
                [ insideTaskCompleted, insideProjectCompleted, outsideTaskCompleted ]
                    |> Feature.Timeline.filterTimelineEventsForSelection (Just selection) TimelineTasksOnly TimelineCompletedEvents
                    |> List.map .id
                    |> Expect.equal [ "inside-task" ]
        , test "workspace timeline filters entity and lifecycle types" <|
            \_ ->
                let
                    projectCreated =
                        timelineEvent "project-created" "2026-05-25T00:00:00Z" "audit-project"
                            |> (\event -> { event | entityType = "project", eventType = "project_created" })

                    taskCompleted =
                        timelineEvent "task-completed" "2026-05-25T00:00:01Z" "audit-task"
                            |> (\event -> { event | entityType = "task", eventType = "task_completed" })

                    subtaskCancelled =
                        timelineEvent "subtask-cancelled" "2026-05-25T00:00:02Z" "audit-subtask"
                            |> (\event -> { event | entityType = "subtask", eventType = "subtask_cancelled" })

                    events =
                        [ projectCreated, taskCompleted, subtaskCancelled ]
                in
                [ Feature.Timeline.filterTimelineEvents TimelineProjectsOnly TimelineAllEvents events |> List.map .id
                , Feature.Timeline.filterTimelineEvents TimelineAllEntities TimelineCompletedEvents events |> List.map .id
                , Feature.Timeline.filterTimelineEvents TimelineSubtasksOnly TimelineCancelledEvents events |> List.map .id
                ]
                    |> Expect.equal [ [ "project-created" ], [ "task-completed" ], [ "subtask-cancelled" ] ]
        , test "workspace timeline groups sorted events by date" <|
            \_ ->
                [ timelineEvent "newer-a" "2026-05-26T08:00:00Z" "audit-newer-a"
                , timelineEvent "newer-b" "2026-05-26T07:00:00Z" "audit-newer-b"
                , timelineEvent "older" "2026-05-25T09:00:00Z" "audit-older"
                ]
                    |> Feature.Timeline.groupTimelineEvents
                    |> List.map (\( label, events ) -> ( label, List.map .id events ))
                    |> Expect.equal [ ( "2026-05-26", [ "newer-a", "newer-b" ] ), ( "2026-05-25", [ "older" ] ) ]
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
        , test "task filters keep parent context when only a subtask matches" <|
            \_ ->
                let
                    parent =
                        task "parent" Nothing (Just "project-a")

                    matchingSubtask =
                        taskWithStatus "matching-subtask" (Just "parent") (Just "project-a") Api.Done

                    hiddenSubtask =
                        taskWithStatus "hidden-subtask" (Just "parent") (Just "project-a") Api.Todo

                    allTasks =
                        [ parent, matchingSubtask, hiddenSubtask ]

                    doneOnly candidate =
                        candidate.status == Api.Done
                in
                ( Feature.Cards.visibleTaskTreeForCriteria "" False doneOnly allTasks [ parent ] |> List.map .id
                , Feature.Cards.visibleTaskTreeForCriteria "" False doneOnly allTasks [ matchingSubtask, hiddenSubtask ] |> List.map .id
                , Feature.Cards.taskShownForMatchingDescendant "" False doneOnly allTasks parent
                )
                    |> Expect.equal ( [ "parent" ], [ "matching-subtask" ], True )
        , test "task text search includes parents for matching subtasks" <|
            \_ ->
                let
                    parent =
                        task "parent" Nothing (Just "project-a")

                    matchingSubtask =
                        taskWithTitle "matching-subtask" (Just "parent") (Just "project-a") "Needle child"

                    allTasks =
                        [ parent, matchingSubtask ]

                    anyTask _ =
                        True
                in
                ( Feature.Cards.visibleTaskTreeForCriteria "needle" True anyTask allTasks [ parent ] |> List.map .id
                , Feature.Cards.taskShownForMatchingDescendant "needle" True anyTask allTasks parent
                )
                    |> Expect.equal ( [ "parent" ], True )
        , test "combined task search and filters must match the same subtask" <|
            \_ ->
                let
                    parent =
                        task "parent" Nothing (Just "project-a")

                    searchOnlySubtask =
                        taskWithTitleStatus "search-only" (Just "parent") (Just "project-a") "Needle but todo" Api.Todo

                    filterOnlySubtask =
                        taskWithTitleStatus "filter-only" (Just "parent") (Just "project-a") "Done without query" Api.Done

                    matchingSubtask =
                        taskWithTitleStatus "matching" (Just "parent") (Just "project-a") "Needle and done" Api.Done

                    allTasks =
                        [ parent, searchOnlySubtask, filterOnlySubtask, matchingSubtask ]

                    doneOnly candidate =
                        candidate.status == Api.Done
                in
                ( Feature.Cards.visibleTaskTreeForCriteria "needle" True doneOnly allTasks [ parent ] |> List.map .id
                , Feature.Cards.visibleTaskTreeForCriteria "needle" True doneOnly allTasks [ searchOnlySubtask, filterOnlySubtask, matchingSubtask ] |> List.map .id
                )
                    |> Expect.equal ( [ "parent" ], [ "matching" ] )
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
        , test "blocked task status surfaces use dependency text instead of badge classes" <|
            \_ ->
                [ Helpers.taskStatusDisplayText Api.Blocked
                , Helpers.taskStatusBadgeClass Api.Blocked
                , Helpers.taskPopoverStatusClass Api.Blocked
                , Helpers.taskCardStatusClass Api.Blocked
                ]
                    |> Expect.equal
                        [ "blocked by dependencies"
                        , "task-status-text task-status-dependency-blocked"
                        , "popover-card-status task-status-text task-status-dependency-blocked"
                        , ""
                        ]
        , test "dependency-blocked task status options hide todo and in-progress transitions" <|
            \_ ->
                let
                    blockedTask =
                        taskWithStatus "blocked" Nothing (Just "project-a") Api.Blocked

                    normalTask =
                        task "normal" Nothing (Just "project-a")

                    staleTodoWithOpenDependencies =
                        task "stale" Nothing (Just "project-a")
                in
                [ Feature.Cards.taskStatusOptionsForTask True blockedTask
                , Feature.Cards.taskStatusOptionsForTask False blockedTask
                , Feature.Cards.taskStatusOptionsForTask False normalTask
                , Feature.Cards.taskStatusOptionsForTask True staleTodoWithOpenDependencies
                ]
                    |> Expect.equal
                        [ [ Api.Blocked, Api.Cancelled ]
                        , Api.allTaskStatuses
                        , Api.allTaskStatuses
                        , [ Api.Todo, Api.Blocked, Api.Cancelled ]
                        ]
        , test "task status option disabled reasons preserve subtask and completion gates" <|
            \_ ->
                [ Feature.Cards.taskStatusOptionDisabledReason Nothing True (Just Api.Todo) Api.InProgress
                , Feature.Cards.taskStatusOptionDisabledReason Nothing True (Just Api.InProgress) Api.InProgress
                , Feature.Cards.taskStatusOptionDisabledReason (Just "Finish subtasks first.") False Nothing Api.Done
                ]
                    |> Expect.equal
                        [ Just "Start the parent task before moving this subtask to in progress."
                        , Nothing
                        , Just "Finish subtasks first."
                        ]
        , test "focus double-click threshold accepts only fast repeated clicks" <|
            \_ ->
                [ Feature.Cards.focusClickIntervalTriggers 249
                , Feature.Cards.focusClickIntervalTriggers 250
                , Feature.Cards.focusClickIntervalTriggers 251
                , Feature.Cards.focusClickIntervalTriggers -1
                ]
                    |> Expect.equal [ True, True, False, False ]
        , test "dependency mutation responses decode affected task status patches" <|
            \_ ->
                let
                    body =
                        """{"action":"add","task_id":"dependent","depends_on_id":"dependency","affected_tasks":[{"task":{"id":"dependent","workspace_id":"workspace-a","project_id":"project-a","parent_id":null,"title":"Dependent","description":null,"status":"blocked","priority":5,"due_at":null,"completed_at":null,"dependency_count":1,"memory_link_count":0,"created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:01Z"},"previous_status":"todo","current_status":"blocked","previous_auto_blocked":false,"auto_blocked":true,"previous_open_dependency_count":0,"open_dependency_count":1,"reason":"blocked_by_open_dependencies"}]}"""
                in
                case Decode.decodeString Api.dependencyMutationResultDecoder body of
                    Ok result ->
                        case result.affectedTasks of
                            [ change ] ->
                                [ result.action == "add"
                                , result.taskId == "dependent"
                                , change.task.status == Api.Blocked
                                , change.task.dependencyCount == 1
                                , change.previousStatus == Api.Todo
                                , change.currentStatus == Api.Blocked
                                , change.autoBlocked == True
                                , change.reason == "blocked_by_open_dependencies"
                                ]
                                    |> Expect.equal (List.repeat 8 True)

                            other ->
                                Expect.fail ("Expected one affected task, got " ++ String.fromInt (List.length other))

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "task update responses decode flattened task plus dependency effects" <|
            \_ ->
                let
                    body =
                        """{"id":"dependency","workspace_id":"workspace-a","project_id":"project-a","parent_id":null,"title":"Dependency","description":null,"status":"done","priority":5,"due_at":null,"completed_at":"2026-01-01T00:00:02Z","dependency_count":0,"memory_link_count":0,"created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:02Z","dependency_effects":[{"task":{"id":"dependent","workspace_id":"workspace-a","project_id":"project-a","parent_id":null,"title":"Dependent","description":null,"status":"todo","priority":5,"due_at":null,"completed_at":null,"dependency_count":1,"memory_link_count":0,"created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:02Z"},"previous_status":"blocked","current_status":"todo","previous_auto_blocked":true,"auto_blocked":false,"previous_open_dependency_count":1,"open_dependency_count":0,"reason":"unblocked_dependencies_resolved"}]}"""
                in
                case ( Decode.decodeString Api.taskDecoder body, Decode.decodeString Api.taskMutationResultDecoder body ) of
                    ( Ok plainTask, Ok mutationResult ) ->
                        [ plainTask.id == "dependency"
                        , plainTask.status == Api.Done
                        , mutationResult.task.id == "dependency"
                        , mutationResult.task.status == Api.Done
                        , List.map (\change -> ( change.task.id, change.task.status, change.reason )) mutationResult.dependencyEffects == [ ( "dependent", Api.Todo, "unblocked_dependencies_resolved" ) ]
                        ]
                            |> Expect.equal (List.repeat 5 True)

                    ( Err err, _ ) ->
                        Expect.fail (Decode.errorToString err)

                    ( _, Err err ) ->
                        Expect.fail (Decode.errorToString err)
        , test "overview readiness rollups decode for tasks and projects" <|
            \_ ->
                let
                    taskOverviewBody =
                        """{"task":{"id":"task-a","workspace_id":"workspace-a","project_id":"project-a","parent_id":null,"title":"Task A","description":null,"status":"todo","priority":5,"due_at":null,"completed_at":null,"dependency_count":1,"memory_link_count":0,"created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:00Z"},"dependencies":[],"connected_memories":[],"readiness_rollup":{"open_subtask_count":2,"done_subtask_count":3,"cancelled_subtask_count":1,"blocked_subtask_count":1,"dependency_blocked_task_count":1,"open_dependency_count":4,"completion_ready":false}}"""

                    projectOverviewBody =
                        """{"project":{"id":"project-a","workspace_id":"workspace-a","parent_id":null,"name":"Project A","description":null,"status":"active","priority":5,"created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:00Z"},"tasks":[],"subprojects":[],"linked_memories":[],"connected_memories":[],"readiness_rollup":{"open_project_count":1,"closed_project_count":2,"open_task_count":3,"done_task_count":4,"cancelled_task_count":1,"blocked_task_count":1,"dependency_blocked_task_count":1,"open_dependency_count":5,"completion_ready":false}}"""
                in
                case ( Decode.decodeString Api.taskOverviewDecoder taskOverviewBody, Decode.decodeString Api.projectOverviewDecoder projectOverviewBody ) of
                    ( Ok taskOverview, Ok projectOverview ) ->
                        [ taskOverview.readinessRollup.openSubtaskCount == 2
                        , taskOverview.readinessRollup.openDependencyCount == 4
                        , taskOverview.readinessRollup.completionReady == False
                        , projectOverview.readinessRollup.openProjectCount == 1
                        , projectOverview.readinessRollup.openTaskCount == 3
                        , projectOverview.readinessRollup.openDependencyCount == 5
                        , projectOverview.readinessRollup.completionReady == False
                        ]
                            |> Expect.equal (List.repeat 7 True)

                    ( Err err, _ ) ->
                        Expect.fail (Decode.errorToString err)

                    ( _, Err err ) ->
                        Expect.fail (Decode.errorToString err)
        , test "next task candidates decode and keep completion gates separate from dependency blockers" <|
            \_ ->
                let
                    body =
                        """{"task":{"id":"task-a","workspace_id":"workspace-a","project_id":"project-a","parent_id":null,"title":"Task A","description":null,"status":"todo","priority":8,"due_at":null,"completed_at":null,"dependency_count":0,"memory_link_count":0,"created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:00Z"},"completion_gated":true,"open_descendant_count":2,"dependency_blocked":false,"open_dependency_count":0}"""
                in
                case Decode.decodeString Api.nextTaskCandidateDecoder body of
                    Ok candidate ->
                        [ candidate.task.id == "task-a"
                        , candidate.completionGated == True
                        , candidate.openDescendantCount == 2
                        , candidate.dependencyBlocked == False
                        , Feature.Cards.nextTaskRationale candidate == "Ready now; completion is gated by 2 open subtasks."
                        ]
                            |> Expect.equal (List.repeat 5 True)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "workspace card hydration decodes links and computes task readiness before card expansion" <|
            \_ ->
                let
                    hydrationBody =
                        """{"project_memory_links":[{"project_id":"project-a","memory_id":"project-memory"}],"task_memory_links":[{"task_id":"root","memory_id":"task-memory"},{"task_id":"child","memory_id":"child-memory"}],"task_dependencies":[{"task_id":"root","depends_on_id":"dependency"}]}"""

                    root =
                        task "root" Nothing (Just "project-a")

                    child =
                        task "child" (Just "root") (Just "project-a")

                    dependency =
                        task "dependency" Nothing (Just "project-a")
                in
                case Decode.decodeString Api.workspaceCardHydrationDecoder hydrationBody of
                    Ok hydration ->
                        let
                            rollup =
                                Helpers.computeTaskReadinessRollupFrom [ root, child, dependency ] hydration.taskDependencies "root"
                        in
                        [ List.length hydration.taskMemoryLinks == 2
                        , List.length hydration.projectMemoryLinks == 1
                        , rollup.openSubtaskCount == 1
                        , rollup.openDependencyCount == 1
                        , rollup.dependencyBlockedTaskCount == 1
                        , rollup.completionReady == False
                        ]
                            |> Expect.equal (List.repeat 6 True)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "next task empty-state copy distinguishes blocked diagnostics from parent-gated subtasks" <|
            \_ ->
                let
                    blockedTask =
                        task "blocked" Nothing (Just "project-a")

                    blockedCandidate =
                        { task = { blockedTask | status = Api.Blocked }
                        , completionGated = False
                        , openDescendantCount = 0
                        , dependencyBlocked = True
                        , openDependencyCount = 1
                        }

                    waitingSubtask =
                        task "waiting-child" (Just "parent") (Just "project-a")
                in
                [ Feature.Cards.noReadyNextTaskMessage [] [] True
                , Feature.Cards.noReadyNextTaskMessage [] [ blockedCandidate ] False
                , Feature.Cards.noReadyNextTaskMessage [ waitingSubtask ] [] False
                ]
                    |> Expect.equal
                        [ "No ready tasks found. Checking blocked diagnostics…"
                        , "No ready tasks found. Blocked candidates are listed below with their dependency/manual-blocking rationale."
                        , "No ready tasks found. Start parent tasks, then resolve any remaining blockers, to make waiting subtasks actionable."
                        ]
        , test "next task card actions expose only a focusing Jump action" <|
            \_ ->
                let
                    candidate =
                        { task = task "jump-task" Nothing (Just "project-a")
                        , completionGated = False
                        , openDescendantCount = 0
                        , dependencyBlocked = False
                        , openDependencyCount = 0
                        }
                in
                case Feature.Cards.nextTaskCardActions candidate of
                    [ action ] ->
                        case action.msg of
                            FocusEntity entityType entityId ->
                                ( action.label, entityType, entityId )
                                    |> Expect.equal ( "Jump", "task", "jump-task" )

                            _ ->
                                Expect.fail "Expected Jump to focus the task"

                    other ->
                        Expect.fail ("Expected only one next-task action, got " ++ String.fromInt (List.length other))
        , test "memory target options include projects and top-level tasks only" <|
            \_ ->
                let
                    otherWorkspaceProject =
                        let
                            base =
                                project "other-project" Nothing
                        in
                        { base | workspaceId = "workspace-b" }

                    otherWorkspaceTask =
                        let
                            base =
                                task "other-task" Nothing Nothing
                        in
                        { base | workspaceId = "workspace-b" }
                in
                Feature.Editing.memoryTargetOptionsForWorkspace "workspace-a"
                    [ project "project-a" Nothing
                    , project "project-b" (Just "project-a")
                    , otherWorkspaceProject
                    ]
                    [ task "parent-task" Nothing (Just "project-a")
                    , task "child-task" (Just "parent-task") (Just "project-a")
                    , otherWorkspaceTask
                    ]
                    |> Expect.equal
                        [ { value = "project:project-a", label = "Project: project-a" }
                        , { value = "project:project-b", label = "Project: project-b" }
                        , { value = "task:parent-task", label = "Top-level task: parent-task" }
                        ]
        , test "memory target selection does not silently fall back" <|
            \_ ->
                let
                    options =
                        [ { value = "project:project-a", label = "Project: project-a" }
                        , { value = "task:parent-task", label = "Top-level task: parent-task" }
                        ]
                in
                [ Feature.Editing.selectedMemoryTargetValueFrom options "project:project-a"
                , Feature.Editing.selectedMemoryTargetValueFrom options ""
                , Feature.Editing.selectedMemoryTargetValueFrom options "task:child-task"
                ]
                    |> Expect.equal [ Just "project:project-a", Nothing, Nothing ]
        , test "active-linked memory filter follows task and project lifecycle state" <|
            \_ ->
                let
                    activeProject =
                        project "active-project" Nothing

                    archivedProject =
                        projectWithStatus "archived-project" Nothing Api.ProjArchived

                    activeTask =
                        task "active-task" Nothing (Just "project-a")

                    doneTask =
                        taskWithStatus "done-task" Nothing (Just "project-a") Api.Done

                    cancelledTask =
                        taskWithStatus "cancelled-task" Nothing (Just "project-a") Api.Cancelled

                    links =
                        Dict.fromList
                            [ ( "active-task", [ "mem-task-active" ] )
                            , ( "done-task", [ "mem-task-done", "mem-mixed" ] )
                            , ( "cancelled-task", [ "mem-task-cancelled" ] )
                            , ( "active-project", [ "mem-project-active", "mem-mixed" ] )
                            , ( "archived-project", [ "mem-project-archived" ] )
                            ]

                    hasActiveUsage memoryId =
                        Feature.Memory.memoryHasActiveLinkedUsage memoryId [ activeProject, archivedProject ] [ activeTask, doneTask, cancelledTask ] links
                in
                [ hasActiveUsage "mem-task-active"
                , hasActiveUsage "mem-task-done"
                , hasActiveUsage "mem-task-cancelled"
                , hasActiveUsage "mem-project-active"
                , hasActiveUsage "mem-project-archived"
                , hasActiveUsage "mem-mixed"
                , hasActiveUsage "mem-unlinked"
                ]
                    |> Expect.equal [ True, False, False, True, False, True, False ]
        , test "memory context targets resolve subtasks to eligible ancestors" <|
            \_ ->
                let
                    projects =
                        [ project "project-a" Nothing ]

                    parentTask =
                        task "parent-task" Nothing (Just "project-a")

                    childTask =
                        task "child-task" (Just "parent-task") (Just "project-a")

                    orphanSubtask =
                        task "orphan-subtask" (Just "missing-parent") (Just "project-a")

                    unlinkedOrphanSubtask =
                        task "unlinked-orphan" (Just "missing-parent") Nothing

                    cyclicTask =
                        task "cycle-a" (Just "cycle-b") (Just "project-a")

                    cyclicParent =
                        task "cycle-b" (Just "cycle-a") (Just "project-a")

                    unlinkedCyclicTask =
                        task "unlinked-cycle-a" (Just "unlinked-cycle-b") Nothing

                    unlinkedCyclicParent =
                        task "unlinked-cycle-b" (Just "unlinked-cycle-a") Nothing

                    tasks =
                        [ parentTask, childTask, orphanSubtask, unlinkedOrphanSubtask, cyclicTask, cyclicParent, unlinkedCyclicTask, unlinkedCyclicParent ]
                in
                [ Feature.Editing.memoryContextTargetValueFrom projects tasks "project" "project-a"
                , Feature.Editing.memoryContextTargetValueFrom projects tasks "task" "parent-task"
                , Feature.Editing.memoryContextTargetValueFrom projects tasks "task" "child-task"
                , Feature.Editing.memoryContextTargetValueFrom projects tasks "task" "orphan-subtask"
                , Feature.Editing.memoryContextTargetValueFrom projects tasks "task" "unlinked-orphan"
                , Feature.Editing.memoryContextTargetValueFrom projects tasks "task" "cycle-a"
                , Feature.Editing.memoryContextTargetValueFrom projects tasks "task" "unlinked-cycle-a"
                ]
                    |> Expect.equal
                        [ Just "project:project-a"
                        , Just "task:parent-task"
                        , Just "task:parent-task"
                        , Just "project:project-a"
                        , Nothing
                        , Just "project:project-a"
                        , Nothing
                        ]
        , test "audit expanded context includes actor target action timestamp and request identifiers" <|
            \_ ->
                case decodeAuditFixture createProjectAuditJson of
                    Ok entry ->
                        let
                            contextRows =
                                Feature.AuditLog.auditContextDetailItems entry
                        in
                        [ List.member ( "Actor", "Local User" ) contextRows
                        , List.member ( "Actor type", "user" ) contextRows
                        , List.member ( "Actor ID", "user-1" ) contextRows
                        , List.member ( "Action type", "Create" ) contextRows
                        , List.member ( "Target entity", "project" ) contextRows
                        , List.member ( "Target ID", "project-1" ) contextRows
                        , List.member ( "Workspace ID", "workspace-a" ) contextRows
                        , List.member ( "Request ID", "req-create-project" ) contextRows
                        , List.member ( "Timestamp", "2026-01-01T00:00:00Z" ) contextRows
                        ]
                            |> Expect.equal (List.repeat 9 True)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "audit action detail fixtures cover create link unlink dependency archive and status updates" <|
            \_ ->
                let
                    decoded =
                        List.map decodeAuditFixture
                            [ createProjectAuditJson
                            , linkProjectMemoryAuditJson
                            , unlinkProjectMemoryAuditJson
                            , addDependencyAuditJson
                            , archiveProjectAuditJson
                            , taskStatusAuditJson
                            ]
                in
                case decoded of
                    [ Ok createEntry, Ok linkEntry, Ok unlinkEntry, Ok dependencyEntry, Ok archiveEntry, Ok statusEntry ] ->
                        let
                            createDetails =
                                Feature.AuditLog.auditActionDetailItems createEntry

                            linkDetails =
                                Feature.AuditLog.auditActionDetailItems linkEntry

                            unlinkDetails =
                                Feature.AuditLog.auditActionDetailItems unlinkEntry

                            dependencyDetails =
                                Feature.AuditLog.auditActionDetailItems dependencyEntry

                            archiveDetails =
                                Feature.AuditLog.auditActionDetailItems archiveEntry

                            archiveChanges =
                                Feature.AuditLog.auditChangedFieldItems archiveEntry
                                    |> List.map (\change -> ( change.label, change.oldValue, change.newValue ))

                            statusDetails =
                                Feature.AuditLog.auditActionDetailItems statusEntry

                            statusChanges =
                                Feature.AuditLog.auditChangedFieldItems statusEntry
                                    |> List.map (\change -> ( change.label, change.oldValue, change.newValue ))
                        in
                        [ List.member ( "Operation", "Created project" ) createDetails
                        , List.member ( "Name", "Launch plan" ) createDetails
                        , List.member ( "Operation", "Linked memory to project" ) linkDetails
                        , List.member ( "Project ID", "project-1" ) linkDetails
                        , List.member ( "Memory ID", "memory-1" ) linkDetails
                        , List.member ( "Operation", "Unlinked memory from project" ) unlinkDetails
                        , List.member ( "Operation", "Added task dependency" ) dependencyDetails
                        , List.member ( "Task ID", "task-dependent" ) dependencyDetails
                        , List.member ( "Depends on task ID", "task-prereq" ) dependencyDetails
                        , List.member ( "Operation", "Archived project" ) archiveDetails
                        , List.member ( "Status change", "active → archived" ) archiveDetails
                        , List.member ( "Status", Just "active", Just "archived" ) archiveChanges
                        , List.member ( "Operation", "Changed task status" ) statusDetails
                        , List.member ( "Status change", "todo → done" ) statusDetails
                        , List.member ( "Status", Just "todo", Just "done" ) statusChanges
                        ]
                            |> Expect.equal (List.repeat 15 True)

                    [ Err err, _, _, _, _, _ ] ->
                        Expect.fail (Decode.errorToString err)

                    [ _, Err err, _, _, _, _ ] ->
                        Expect.fail (Decode.errorToString err)

                    [ _, _, Err err, _, _, _ ] ->
                        Expect.fail (Decode.errorToString err)

                    [ _, _, _, Err err, _, _ ] ->
                        Expect.fail (Decode.errorToString err)

                    [ _, _, _, _, Err err, _ ] ->
                        Expect.fail (Decode.errorToString err)

                    [ _, _, _, _, _, Err err ] ->
                        Expect.fail (Decode.errorToString err)

                    _ ->
                        Expect.fail "Expected six audit fixtures"
        , test "audit changed field details show non-status update before and after values" <|
            \_ ->
                case decodeAuditFixture taskTitleUpdateAuditJson of
                    Ok entry ->
                        let
                            details =
                                Feature.AuditLog.auditActionDetailItems entry

                            changes =
                                Feature.AuditLog.auditChangedFieldItems entry
                                    |> List.map (\change -> ( change.label, change.oldValue, change.newValue ))
                        in
                        [ List.member ( "Operation", "Updated task fields" ) details
                        , List.member ( "Field updates", "Title" ) details
                        , List.member ( "Title", Just "Draft UI", Just "Ship UI" ) changes
                        ]
                            |> Expect.equal [ True, True, True ]

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "audit status update classification covers subtasks tasks projects and mixed updates" <|
            \_ ->
                let
                    decoded =
                        List.map decodeAuditFixture
                            [ subtaskStatusAuditJson
                            , taskStatusAuditJson
                            , projectCompletedAuditJson
                            , taskMixedStatusAndTitleAuditJson
                            ]
                in
                case decoded of
                    [ Ok subtaskEntry, Ok taskEntry, Ok projectEntry, Ok mixedEntry ] ->
                        let
                            subtaskDetails =
                                Feature.AuditLog.auditActionDetailItems subtaskEntry

                            taskDetails =
                                Feature.AuditLog.auditActionDetailItems taskEntry

                            projectDetails =
                                Feature.AuditLog.auditActionDetailItems projectEntry

                            mixedDetails =
                                Feature.AuditLog.auditActionDetailItems mixedEntry
                        in
                        [ List.member ( "Operation", "Changed subtask status" ) subtaskDetails
                        , List.member ( "Status change", "todo → in_progress" ) subtaskDetails
                        , List.member ( "Operation", "Changed task status" ) taskDetails
                        , List.member ( "Field updates", "Completed at" ) taskDetails
                        , List.member ( "Operation", "Completed project" ) projectDetails
                        , List.member ( "Status change", "active → completed" ) projectDetails
                        , List.member ( "Operation", "Changed task status and fields" ) mixedDetails
                        , List.member ( "Status change", "todo → done" ) mixedDetails
                        , List.member ( "Field updates", "Title" ) mixedDetails
                        ]
                            |> Expect.equal [ True, True, True, False, True, True, True, True, True ]

                    [ Err err, _, _, _ ] ->
                        Expect.fail (Decode.errorToString err)

                    [ _, Err err, _, _ ] ->
                        Expect.fail (Decode.errorToString err)

                    [ _, _, Err err, _ ] ->
                        Expect.fail (Decode.errorToString err)

                    [ _, _, _, Err err ] ->
                        Expect.fail (Decode.errorToString err)

                    _ ->
                        Expect.fail "Expected four audit status fixtures"
        , test "audit action detail helpers hide sensitive and internal snapshot fields" <|
            \_ ->
                case decodeAuditFixture accessTokenAuditJson of
                    Ok entry ->
                        let
                            details =
                                Feature.AuditLog.auditActionDetailItems entry

                            detailText =
                                details
                                    |> List.map (\( label, value ) -> label ++ "=" ++ value)
                                    |> String.join "|"
                        in
                        [ String.contains "token_hash" detailText
                        , String.contains "secret-digest" detailText
                        , String.contains "last_used_at" detailText
                        , List.member ( "Actor label", "CI Bot" ) details
                        ]
                            |> Expect.equal [ False, False, False, True ]

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        ]


timelineEvent : String -> String -> String -> Api.WorkspaceTimelineEvent
timelineEvent id occurredAt sourceAuditId =
    { id = id
    , workspaceId = "workspace-a"
    , eventType = "task_completed"
    , entityType = "task"
    , entityId = id
    , title = id
    , occurredAt = occurredAt
    , actor = Nothing
    , project = Nothing
    , parentTask = Nothing
    , statusTransition = Just { from = "in_progress", to = "done" }
    , navigation = { entityType = "task", entityId = id }
    , sourceAuditId = Just sourceAuditId
    }


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


memory : String -> Api.Memory
memory id =
    { id = id
    , workspaceId = "workspace-a"
    , content = id
    , summary = Nothing
    , memoryType = Api.ShortTerm
    , importance = 5
    , pinned = False
    , tags = []
    , createdAt = "2026-01-01T00:00:00Z"
    , updatedAt = "2026-01-01T00:00:00Z"
    }


projectWithStatus : String -> Maybe String -> Api.ProjectStatus -> Api.Project
projectWithStatus id parentId status =
    let
        base =
            project id parentId
    in
    { base | status = status }


taskWithStatus : String -> Maybe String -> Maybe String -> Api.TaskStatus -> Api.Task
taskWithStatus id parentId projectId status =
    let
        base =
            task id parentId projectId
    in
    { base | status = status }


taskWithWorkspace : String -> Maybe String -> Maybe String -> String -> Api.Task
taskWithWorkspace id parentId projectId workspaceId =
    let
        base =
            task id parentId projectId
    in
    { base | workspaceId = workspaceId }


taskWithTitle : String -> Maybe String -> Maybe String -> String -> Api.Task
taskWithTitle id parentId projectId title =
    let
        base =
            task id parentId projectId
    in
    { base | title = title }


taskWithTitleStatus : String -> Maybe String -> Maybe String -> String -> Api.TaskStatus -> Api.Task
taskWithTitleStatus id parentId projectId title status =
    let
        base =
            task id parentId projectId
    in
    { base | title = title, status = status }


memoryWithWorkspace : String -> String -> Api.Memory
memoryWithWorkspace id workspaceId =
    let
        base =
            memory id
    in
    { base | workspaceId = workspaceId }


sessionContext : String -> String -> String -> Bool -> Bool -> Maybe String -> Api.SessionContext
sessionContext authMode actorType authority createWorkspace superadmin workspaceRole =
    { authMode = authMode
    , principal =
        { actorType = actorType
        , actorId = "principal-a"
        , actorLabel = "Test Principal"
        , authority = authority
        , grantUserId =
            if authority == "grant_user" then
                Just "user-a"

            else
                Nothing
        }
    , globalPermissions =
        { createWorkspace = createWorkspace
        , superadmin = superadmin
        }
    , workspace =
        Maybe.map
            (\role ->
                { workspaceId = "workspace-a"
                , role = Just role
                , canRead = True
                , canEdit = role == "edit" || role == "admin"
                , canAdmin = role == "admin"
                }
            )
            workspaceRole
    }


decodeAuditFixture : String -> Result Decode.Error Api.AuditLogEntry
decodeAuditFixture =
    Decode.decodeString Api.auditLogEntryDecoder


createProjectAuditJson : String
createProjectAuditJson =
    """{"id":"audit-create-project","workspace_id":"workspace-a","entity_type":"project","entity_id":"project-1","action":"create","old_values":null,"new_values":{"id":"project-1","workspace_id":"workspace-a","name":"Launch plan","status":"active","priority":7,"created_at":"2026-01-01T00:00:00Z","updated_at":"2026-01-01T00:00:00Z"},"request_id":"req-create-project","actor_type":"user","actor_id":"user-1","actor_label":"Local User","changed_at":"2026-01-01T00:00:00Z"}"""


linkProjectMemoryAuditJson : String
linkProjectMemoryAuditJson =
    """{"id":"audit-link-project-memory","workspace_id":"workspace-a","entity_type":"project_memory_link","entity_id":"project-1:memory-1","action":"create","old_values":null,"new_values":{"project_id":"project-1","memory_id":"memory-1","created_at":"2026-01-01T00:00:00Z"},"request_id":"req-link-memory","actor_type":"user","actor_id":"user-1","actor_label":"Local User","changed_at":"2026-01-01T00:00:01Z"}"""


unlinkProjectMemoryAuditJson : String
unlinkProjectMemoryAuditJson =
    """{"id":"audit-unlink-project-memory","workspace_id":"workspace-a","entity_type":"project_memory_link","entity_id":"project-1:memory-1","action":"delete","old_values":{"project_id":"project-1","memory_id":"memory-1","created_at":"2026-01-01T00:00:00Z"},"new_values":null,"request_id":"req-unlink-memory","actor_type":"user","actor_id":"user-1","actor_label":"Local User","changed_at":"2026-01-01T00:00:02Z"}"""


addDependencyAuditJson : String
addDependencyAuditJson =
    """{"id":"audit-add-dependency","workspace_id":"workspace-a","entity_type":"task_dependency","entity_id":"task-dependent:task-prereq","action":"create","old_values":null,"new_values":{"task_id":"task-dependent","depends_on_id":"task-prereq","created_at":"2026-01-01T00:00:00Z"},"request_id":"req-add-dependency","actor_type":"user","actor_id":"user-1","actor_label":"Local User","changed_at":"2026-01-01T00:00:03Z"}"""


archiveProjectAuditJson : String
archiveProjectAuditJson =
    """{"id":"audit-archive-project","workspace_id":"workspace-a","entity_type":"project","entity_id":"project-1","action":"update","old_values":{"id":"project-1","workspace_id":"workspace-a","name":"Launch plan","status":"active","priority":7,"updated_at":"2026-01-01T00:00:00Z"},"new_values":{"id":"project-1","workspace_id":"workspace-a","name":"Launch plan","status":"archived","priority":7,"updated_at":"2026-01-01T00:00:04Z"},"request_id":"req-archive-project","actor_type":"user","actor_id":"user-1","actor_label":"Local User","changed_at":"2026-01-01T00:00:04Z"}"""


taskStatusAuditJson : String
taskStatusAuditJson =
    """{"id":"audit-task-status","workspace_id":"workspace-a","entity_type":"task","entity_id":"task-1","action":"update","old_values":{"id":"task-1","workspace_id":"workspace-a","parent_id":null,"title":"Ship UI","status":"todo","priority":5,"completed_at":null,"updated_at":"2026-01-01T00:00:00Z"},"new_values":{"id":"task-1","workspace_id":"workspace-a","parent_id":null,"title":"Ship UI","status":"done","priority":5,"completed_at":"2026-01-01T00:00:05Z","updated_at":"2026-01-01T00:00:05Z"},"request_id":"req-task-status","actor_type":"user","actor_id":"user-1","actor_label":"Local User","changed_at":"2026-01-01T00:00:05Z"}"""


subtaskStatusAuditJson : String
subtaskStatusAuditJson =
    """{"id":"audit-subtask-status","workspace_id":"workspace-a","entity_type":"task","entity_id":"subtask-1","action":"update","old_values":{"id":"subtask-1","workspace_id":"workspace-a","parent_id":"task-1","title":"Child work","status":"todo","priority":5,"updated_at":"2026-01-01T00:00:00Z"},"new_values":{"id":"subtask-1","workspace_id":"workspace-a","parent_id":"task-1","title":"Child work","status":"in_progress","priority":5,"updated_at":"2026-01-01T00:00:07Z"},"request_id":"req-subtask-status","actor_type":"user","actor_id":"user-1","actor_label":"Local User","changed_at":"2026-01-01T00:00:07Z"}"""


projectCompletedAuditJson : String
projectCompletedAuditJson =
    """{"id":"audit-project-completed","workspace_id":"workspace-a","entity_type":"project","entity_id":"project-1","action":"update","old_values":{"id":"project-1","workspace_id":"workspace-a","name":"Launch plan","status":"active","priority":7,"updated_at":"2026-01-01T00:00:00Z"},"new_values":{"id":"project-1","workspace_id":"workspace-a","name":"Launch plan","status":"completed","priority":7,"updated_at":"2026-01-01T00:00:08Z"},"request_id":"req-project-completed","actor_type":"user","actor_id":"user-1","actor_label":"Local User","changed_at":"2026-01-01T00:00:08Z"}"""


taskMixedStatusAndTitleAuditJson : String
taskMixedStatusAndTitleAuditJson =
    """{"id":"audit-task-mixed","workspace_id":"workspace-a","entity_type":"task","entity_id":"task-1","action":"update","old_values":{"id":"task-1","workspace_id":"workspace-a","parent_id":null,"title":"Draft UI","status":"todo","priority":5,"completed_at":null,"updated_at":"2026-01-01T00:00:00Z"},"new_values":{"id":"task-1","workspace_id":"workspace-a","parent_id":null,"title":"Ship UI","status":"done","priority":5,"completed_at":"2026-01-01T00:00:09Z","updated_at":"2026-01-01T00:00:09Z"},"request_id":"req-task-mixed","actor_type":"user","actor_id":"user-1","actor_label":"Local User","changed_at":"2026-01-01T00:00:09Z"}"""


taskTitleUpdateAuditJson : String
taskTitleUpdateAuditJson =
    """{"id":"audit-task-title","workspace_id":"workspace-a","entity_type":"task","entity_id":"task-1","action":"update","old_values":{"id":"task-1","workspace_id":"workspace-a","title":"Draft UI","status":"todo","priority":5,"updated_at":"2026-01-01T00:00:00Z"},"new_values":{"id":"task-1","workspace_id":"workspace-a","title":"Ship UI","status":"todo","priority":5,"updated_at":"2026-01-01T00:00:06Z"},"request_id":"req-task-title","actor_type":"user","actor_id":"user-1","actor_label":"Local User","changed_at":"2026-01-01T00:00:06Z"}"""


accessTokenAuditJson : String
accessTokenAuditJson =
    """{"id":"audit-access-token","workspace_id":null,"entity_type":"access_token","entity_id":"token-1","action":"create","old_values":null,"new_values":{"id":"token-1","actor_type":"bot","actor_label":"CI Bot","token_hash":"secret-digest","last_used_at":"2026-01-01T00:00:00Z","created_at":"2026-01-01T00:00:00Z"},"request_id":"req-token","actor_type":"bot","actor_id":"bot-1","actor_label":"CI Bot","changed_at":"2026-01-01T00:00:06Z"}"""
