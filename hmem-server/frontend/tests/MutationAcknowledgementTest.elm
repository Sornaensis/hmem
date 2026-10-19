module MutationAcknowledgementTest exposing (suite)

import Api
import AppShell
import Dict
import Expect
import Feature.DataLoading as DataLoading
import Helpers
import Http
import Json.Encode as Encode
import Main
import Set
import Test exposing (Test, describe, test)
import Types exposing (AuthStatus(..), CreateForm(..), CreationOperation, Flags, InlineCreate(..), Model, Msg(..), Page(..), TaskMutationOperation, WorkspaceTab(..))
import Url


suite : Test
suite =
    describe "mutation acknowledgement ownership"
        [ test "an old task update cannot repopulate a workspace after route retirement" <| \_ ->
            let
                pending = routed (ChangeTaskStatus "target" Api.InProgress) model
                    |> routed (RequestTaskUpdate (Helpers.mutationScope model) "target" "session-1-req-1" [])
                retired = routed (UrlChanged { url | path = "/workspace/other" }) pending
                late = Dict.values pending.mutations.taskAcknowledgements |> List.head |> Maybe.map (\operation -> routed (TaskUpdated operation (Ok { task = Api.taskFromCardSummary (task "target"), dependencyEffects = [] })) retired) |> Maybe.withDefault retired
            in
            Expect.equal ( Just "other", False ) ( late.selectedWorkspaceId, Dict.member "target" late.tasks )
        , test "all task editing and drag dispatch families admit scoped acknowledgements" <| \_ ->
            List.map taskDispatchCase [ "status", "priority", "save", "switch", "expand", "drag-project", "drag-subtask", "zone-project", "zone-task", "zone-orphan" ]
                |> Expect.equal (List.repeat 10 ( True, Just "acknowledged", True ))
        , test "canonical summary before acknowledgement completes the operation and requests fresh recovery" <| \_ ->
            let
                ( pending, operation ) = issueTask "target" "echo-operation" model
                fresh = let base = task "target" in { base | title = "canonical-newer" }
                echoed = DataLoading.mergeNavigationSummaries [] [ fresh ] pending
                completed = replyTask operation "old-ack" [] echoed
            in
            Expect.equal { title = Just "canonical-newer", completed = True, detail = False, revalidated = True, bounded = True }
                { title = Dict.get "target" completed.tasks |> Maybe.map .title
                , completed = not (Dict.member operation.request.revision completed.mutations.taskAcknowledgements)
                , detail = Dict.get "target" completed.dataLoading.taskCardDetailRequests |> Maybe.map .succeeded |> Maybe.withDefault False
                , revalidated = Dict.keys completed.webSocket.targetGenerations |> List.any (String.contains "navigation-summary:task:target")
                , bounded = Dict.size completed.dataLoading.navigationAdmissions <= 4 && Set.size completed.dataLoading.cardDetailAdmissions <= 6 }
        , test "independent entities and shared transport ids remain valid while a newer target operation wins" <| \_ ->
            let
                ( first, old ) = issueTask "target" "shared-swap" model
                ( second, independent ) = issueTask "independent" "shared-swap" first
                ( third, latest ) = issueTask "target" "newer-target" second
                completed = third |> replyTask latest "latest" [] |> replyTask old "stale" [] |> replyTask independent "independent-current" []
            in
            Expect.equal ( Just "latest", Just "independent-current", True )
                ( Dict.get "target" completed.tasks |> Maybe.map .title, Dict.get "independent" completed.tasks |> Maybe.map .title, Dict.isEmpty completed.mutations.taskAcknowledgements )
        , test "a newer operation on one dependency effect does not discard independent effects" <| \_ ->
            let
                ( first, origin ) = issueTask "target" "effects-origin" model
                ( pending, effectOperation ) = issueTask "effect" "newer-effect" first
                completed = pending |> replyTask effectOperation "effect-latest" [] |> replyTask origin "target-current" [ effect "effect" "effect-stale", effect "independent" "effect-independent" ]
            in
            Expect.equal ( Just "effect-latest", Just "effect-independent", True )
                ( Dict.get "effect" completed.tasks |> Maybe.map .title, Dict.get "independent" completed.tasks |> Maybe.map .title
                , not (Dict.get "effect" completed.dataLoading.taskCardDetailRequests |> Maybe.map .succeeded |> Maybe.withDefault False) )
        , test "newer canonical authority on a dependency effect preserves its payload independently" <| \_ ->
            let
                ( pending, operation ) = issueTask "target" "canonical-effect" model
                base = task "effect"
                canonical = DataLoading.mergeNavigationSummaries [] [ { base | title = "canonical-effect-current" } ] pending
                completed = replyTask operation "target-accepted" [ effect "effect" "effect-stale", effect "independent" "effect-accepted" ] canonical
            in
            Expect.equal ( Just "target-accepted", Just "canonical-effect-current", Just "effect-accepted" )
                ( Dict.get "target" completed.tasks |> Maybe.map .title, Dict.get "effect" completed.tasks |> Maybe.map .title, Dict.get "independent" completed.tasks |> Maybe.map .title )
        , test "superseded errors cannot change current notifications while current errors settle" <| \_ ->
            let
                ( first, old ) = issueTask "target" "old-error" model
                ( pending, latest ) = issueTask "target" "new-error" first
                stale = routed (TaskUpdated old (Err (Api.decodeApiErrorBody 500 "{}"))) pending
                failed = routed (TaskUpdated latest (Err (Api.decodeApiErrorBody 500 "{}"))) stale
            in
            Expect.equal ( True, True, True ) ( stale.toast == pending.toast, List.length failed.toast.toasts > List.length stale.toast.toasts, Dict.isEmpty failed.mutations.taskAcknowledgements )
        , test "task and project creation clear only the unchanged originating slot" <| \_ ->
            List.map creationCompletionCase [ "modal-project", "inline-project", "inline-child-project", "inline-task", "inline-subtask" ]
                |> Expect.equal (List.repeat 5 ( True, True, True ))
        , test "create acknowledgements preserve changed drafts and identical reopened forms" <| \_ ->
            List.concatMap (\kind -> List.map (creationReplacementCase kind) [ "draft", "reopen", "kind", "parent", "cancel", "escape", "tab", "route", "new-submit" ]) [ "modal-project", "inline-task" ]
                |> Expect.equal (List.repeat 18 True)
        , test "canonical create echo completes its unchanged origin without applying stale payload" <| \_ ->
            List.map creationEchoCase [ "modal-project", "inline-task" ] |> Expect.equal [ True, True ]
        , test "simultaneous modal and inline creations complete their own operations and slots" <| \_ ->
            let
                ( first, modal ) = issueCreation "modal-project" model
                ( pending, inline ) = issueCreation "inline-task" first
                modalDone = replyCreation "modal-project" modal pending
                completed = replyCreation "inline-task" inline modalDone
            in
            Expect.equal ( True, True, True )
                ( modalDone.editing.createForm == Nothing && modalDone.editing.inlineCreate /= Nothing
                , Dict.member inline.request.revision modalDone.mutations.creationOperations
                , completed.editing.createForm == Nothing && completed.editing.inlineCreate == Nothing && Dict.isEmpty completed.mutations.creationOperations )
        , test "task and creation callbacks retire across workspace return session and audience changes" <| \_ ->
            List.map scopeRetirementCase [ "workspace-return", "session", "audience", "unauthorized" ] |> Expect.equal (List.repeat 4 True)
        , test "current create errors retain drafts and retired form errors stay silent" <| \_ ->
            List.map creationErrorCase [ "modal-project", "inline-task" ] |> Expect.equal [ True, True ]
        , test "fresh recovery can reflect the server commit after a competing earlier read" <| \_ ->
            let
                ( pending, operation ) = issueTask "target" "server-commit-order" model
                base = task "target"
                readBeforeCommit = DataLoading.mergeNavigationSummaries [] [ { base | title = "before-server-commit" } ] pending
                acknowledged = replyTask operation "unsafe-ack" [] readBeforeCommit
                current = completeTaskRecovery "target" { base | title = "server-committed-last" } acknowledged
            in
            Expect.equal ( Just "before-server-commit", Just "server-committed-last" )
                ( Dict.get "target" acknowledged.tasks |> Maybe.map .title, Dict.get "target" current.tasks |> Maybe.map .title )
        ]


routed : Msg -> Model -> Model
routed msg source = Main.update msg source |> Tuple.first


issueTask : String -> String -> Model -> ( Model, TaskMutationOperation )
issueTask id requestId source =
    let
        pending = routed (RequestTaskUpdate (Helpers.mutationScope source) id requestId [ ( "request_id", Encode.string requestId ) ]) source
        operation = Dict.values pending.mutations.taskAcknowledgements |> List.filter (\candidate -> candidate.taskId == id && candidate.request.requestId == requestId) |> List.head
    in
    ( pending, operation |> Maybe.withDefault { request = { scope = Helpers.mutationScope source, requestId = requestId, revision = -1 }, taskId = id } )


replyTask : TaskMutationOperation -> String -> List Api.TaskDependencyStatusChange -> Model -> Model
replyTask operation title effects source =
    let value = Api.taskFromCardSummary (task operation.taskId)
    in routed (TaskUpdated operation (Ok { task = { value | title = title }, dependencyEffects = effects })) source


effect : String -> String -> Api.TaskDependencyStatusChange
effect id title =
    let value = Api.taskFromCardSummary (task id)
    in { task = { value | title = title }, previousStatus = Api.Todo, currentStatus = Api.Todo, previousAutoBlocked = False, autoBlocked = False, previousOpenDependencyCount = 0, openDependencyCount = 0, previousOpenSubtaskCount = 0, openSubtaskCount = 0, previousProjectPaused = False, projectPaused = False, reason = "dependency" }


taskDispatchCase : String -> ( Bool, Maybe String, Bool )
taskDispatchCase family =
    let
        editing = model |> routed (StartEdit "task" "target" "title" "target") |> routed (EditInput "changed")
        zone = { parentType = if family == "zone-task" then "task-subtasks" else if family == "zone-orphan" then "orphan" else "project-tasks", parentId = Just "effect", projectId = Just "container", abovePriority = Nothing, belowPriority = Nothing }
        dispatched = case family of
            "status" -> routed (ChangeTaskStatus "target" Api.InProgress) model
            "priority" -> routed (ChangeTaskPriority "target" 9) model
            "save" -> routed (SaveEdit "target" "title") editing
            "switch" -> routed (StartEdit "task" "independent" "title" "independent") editing
            "expand" -> routed (ExpandAndEdit "independent" "task" "independent" "title" "independent") editing
            "drag-project" -> model |> routed (DragStartCard "task" "target") |> routed (DropOnCard "project" "container")
            "drag-subtask" -> model |> routed (DragStartCard "task" "target") |> routed (DropOnCard "task" "effect") |> routed DropActionMakeSubtask
            _ -> model |> routed (DragStartCard "task" "target") |> routed (DropOnZone zone)
        requestId = flags.sessionId ++ "-req-" ++ String.fromInt (dispatched.mutations.nextRequestId - 1)
        ( pending, operation ) = issueTask "target" requestId dispatched
        completed = replyTask operation "acknowledged" [] pending
    in
    ( dispatched.mutations.nextRequestId > model.mutations.nextRequestId
    , Dict.get "target" completed.tasks |> Maybe.map .title
    , Dict.get "target" completed.dataLoading.taskCardDetailRequests |> Maybe.map .succeeded |> Maybe.withDefault False )


openCreation : String -> Model -> Model
openCreation kind source =
    if kind == "modal-project" then routed (ShowCreateForm (CreateProjectForm { name = "Created" })) source
    else if String.contains "project" kind then routed (ShowInlineCreate (InlineCreateProject { parentId = if kind == "inline-child-project" then Just "container" else Nothing, name = "Created" })) source
    else routed (ShowInlineCreate (InlineCreateTask { projectId = Just "container", parentId = if kind == "inline-subtask" then Just "target" else Nothing, title = "Created" })) source


issueCreation : String -> Model -> ( Model, CreationOperation )
issueCreation kind source =
    let
        pending = source |> openCreation kind |> routed (if kind == "modal-project" then SubmitCreateForm else SubmitInlineCreate)
        operation = Dict.values pending.mutations.creationOperations |> List.reverse |> List.head
    in
    ( pending, operation |> Maybe.withDefault { request = { scope = Helpers.mutationScope source, requestId = "invalid", revision = -1 }, slot = Types.ModalCreation, formRevision = -1 } )


replyCreation : String -> CreationOperation -> Model -> Model
replyCreation kind operation source =
    if String.contains "project" kind then routed (ProjectCreated operation (Ok (Api.projectFromCardSummary (project "created")))) source
    else routed (TaskCreated operation (Ok (Api.taskFromCardSummary (task "created")))) source


creationCompletionCase : String -> ( Bool, Bool, Bool )
creationCompletionCase kind =
    let
        seed = model |> openCreation "modal-project" |> openCreation "inline-task"
        ( pending, operation ) = issueCreation kind seed
        completed = replyCreation kind operation pending
        ownCleared = if kind == "modal-project" then completed.editing.createForm == Nothing else completed.editing.inlineCreate == Nothing
        otherRetained = if kind == "modal-project" then completed.editing.inlineCreate == pending.editing.inlineCreate else completed.editing.createForm == pending.editing.createForm
    in
    ( ownCleared, otherRetained, Dict.isEmpty completed.mutations.creationOperations && (if String.contains "project" kind then Dict.member "created" completed.projects else Dict.member "created" completed.tasks) )


creationReplacementCase : String -> String -> Bool
creationReplacementCase kind replacement =
    let
        ( pending, operation ) = issueCreation kind model
        modal = kind == "modal-project"
        changed = case replacement of
            "draft" -> if modal then routed (UpdateCreateForm (CreateProjectForm { name = "Changed draft" })) pending else routed (UpdateInlineCreate (InlineCreateTask { projectId = Just "container", parentId = Nothing, title = "Changed draft" })) pending
            "reopen" -> pending |> routed (if modal then CancelCreateForm else CancelInlineCreate) |> openCreation kind
            "kind" -> if modal then routed (ShowCreateForm (CreateWorkspaceForm { name = "Other kind", workspaceType = Api.Planning, ghOwner = "", ghRepo = "" })) pending else openCreation "inline-project" pending
            "parent" -> if modal then openCreation kind pending else openCreation "inline-subtask" pending
            "cancel" -> routed (if modal then CancelCreateForm else CancelInlineCreate) pending
            "escape" -> routed (if modal then CancelCreateForm else GlobalKeyDown 27) pending
            "tab" -> routed (SwitchTab TimelineTab) pending
            "route" -> routed (UrlChanged { url | fragment = Just "tab=timeline" }) pending
            "new-submit" -> routed (if modal then SubmitCreateForm else SubmitInlineCreate) pending
            _ -> pending
        completed = replyCreation kind operation changed
    in
    completed.editing == changed.editing


creationEchoCase : String -> Bool
creationEchoCase kind =
    let
        ( pending, operation ) = issueCreation kind model
        projectBase = project "created"
        taskBase = task "created"
        echoed = DataLoading.mergeNavigationSummaries (if String.contains "project" kind then [ { projectBase | name = "canonical-created" } ] else []) (if String.contains "project" kind then [] else [ { taskBase | title = "canonical-created" } ]) pending
        completed = replyCreation kind operation echoed
        value = if String.contains "project" kind then Dict.get "created" completed.projects |> Maybe.map .name else Dict.get "created" completed.tasks |> Maybe.map .title
    in
    value == Just "canonical-created" && Dict.isEmpty completed.mutations.creationOperations && (if kind == "modal-project" then completed.editing.createForm == Nothing else completed.editing.inlineCreate == Nothing)


scopeRetirementCase : String -> Bool
scopeRetirementCase kind =
    let
        ( taskPending, taskOperation ) = issueTask "target" "retired-task" model
        ( pending, creationOperation ) = issueCreation "inline-task" taskPending
        principal = session.principal
        alternate = { session | principal = { principal | actorId = "other-audience" } }
        retired = case kind of
            "workspace-return" -> pending |> routed (UrlChanged { url | path = "/workspace/other" }) |> routed (UrlChanged url)
            "session" -> routed (AuthTokenChanged True) pending
            "audience" -> routed (GotSessionContext pending.sessionRequestEpoch pending.selectedWorkspaceId (Ok alternate)) pending
            _ -> routed AuthUnauthorized pending
        newForm = openCreation "inline-subtask" retired
        lateTask = replyTask taskOperation "retired-payload" [] newForm
        lateCreate = replyCreation "inline-task" creationOperation lateTask
        lateError = routed (TaskUpdated taskOperation (Err (Api.decodeApiErrorBody 500 "{}"))) lateCreate
    in
    lateError.tasks == newForm.tasks && lateError.editing == newForm.editing && lateError.toast == newForm.toast
        && lateError.dataLoading.nextNavigationSummaryRevision >= pending.dataLoading.nextNavigationSummaryRevision


creationErrorCase : String -> Bool
creationErrorCase kind =
    let
        ( pending, operation ) = issueCreation kind model
        error current = routed (if String.contains "project" kind then ProjectCreated operation (Err (Api.decodeApiErrorBody 500 "{}")) else TaskCreated operation (Err (Api.decodeApiErrorBody 500 "{}"))) current
        currentFailure = error pending
        replaced = openCreation kind pending
        oldFailure = error replaced
    in
    currentFailure.editing == pending.editing && Dict.isEmpty currentFailure.mutations.creationOperations
        && List.length currentFailure.toast.toasts > List.length pending.toast.toasts
        && oldFailure.editing == replaced.editing && oldFailure.toast == replaced.toast


completeTaskRecovery : String -> Api.TaskCardSummary -> Model -> Model
completeTaskRecovery id summary source =
    let
        target = "navigation-summaries:task:" ++ id
        scope = "workspace:workspace-1"
        guard = { scopeKey = scope, targetKey = target, targetGeneration = Dict.get (scope ++ "|" ++ target) source.webSocket.targetGenerations |> Maybe.withDefault -1, sessionEpoch = source.sessionRequestEpoch, routeWorkspace = source.selectedWorkspaceId, audienceId = "editor" }
        entityKey = scope ++ "|navigation-summary:task:" ++ id
        batch = { request = guard, navigationGeneration = source.dataLoading.navigationGeneration, filterFingerprint = DataLoading.navigationFilterFingerprint source, entityGenerations = Dict.filter (\key _ -> key == entityKey) source.webSocket.targetGenerations }
    in
    routed (CanonicalNavigationSummariesFetched batch "workspace-1" [] [ id ] (Ok { projects = [], tasks = [ summary ], missingProjectIds = [], missingTaskIds = [] })) source


model : Model
model =
    AppShell.initModel Nothing url (WorkspacePage "workspace-1") flags Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
        |> AppShell.finalizeInit (WorkspacePage "workspace-1")
        |> (\initial -> { initial | auth = { status = AuthReady, mode = Just "test" }, sessionContext = Just session })
        |> DataLoading.mergeNavigationSummaries [ project "container" ] [ task "target", task "independent", task "effect" ]


project : String -> Api.ProjectCardSummary
project id =
    { id = id, workspaceId = "workspace-1", parentId = Nothing, name = id, status = Api.ProjActive, priority = 1, createdAt = "now", updatedAt = "now", directProjectCount = 0, directTaskCount = 0, hasChildren = False
    , readinessRollup = { openProjectCount = 0, inProgressTaskCount = 0, closedProjectCount = 0, openTaskCount = 0, doneTaskCount = 0, cancelledTaskCount = 0, blockedTaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = True } }


task : String -> Api.TaskCardSummary
task id =
    { id = id, workspaceId = "workspace-1", projectId = Nothing, parentId = Nothing, title = id, status = Api.Todo, priority = 1, completedAt = Nothing, dependencyCount = 0
    , createdAt = "now", updatedAt = "now", directSubtaskCount = 0, hasChildren = False
    , readinessRollup = { openSubtaskCount = 0, doneSubtaskCount = 0, cancelledSubtaskCount = 0, blockedSubtaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = True, inProgressSubtaskCount = 0, canStartSubtasks = False, ownOpenDependencyCount = Just 0 }
    }


session : Api.SessionContext
session =
    { authMode = "test", effectiveActor = Nothing, principal = { actorType = "user", actorId = "editor", actorLabel = "Editor", authority = "local", grantUserId = Nothing }
    , globalPermissions = { createWorkspace = False, superadmin = False }, workspace = Just { workspaceId = "workspace-1", role = Just "edit", canRead = True, canEdit = True, canAdmin = False }
    }


flags : Flags
flags =
    { apiUrl = "https://api.example", wsUrl = "wss://api.example", sessionId = "session-1", runtimeMode = "test", authTokenStorageKey = "hmem-auth-token", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing }


url : Url.Url
url =
    { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/workspace-1", query = Nothing, fragment = Nothing }
