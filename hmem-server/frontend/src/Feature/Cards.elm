module Feature.Cards exposing
    ( CardTreeProjection
    , NextTaskCardAction
    , cardTreeProjection
    , cascadeDeleteFailureFallback
    , cascadeDeletePreview
    , cascadeDeleteSuccessMessage
    , handleEscape
    , init
    , nextTaskCardActions
    , nextTaskRationale
    , noReadyNextTaskMessage
    , presentationWindow
    , focusClickIntervalTriggers
    , projectCascadePreview
    , projectCompletionBlockerReason
    , taskCascadePreview
    , taskCompletionBlockerReason
    , taskShownForMatchingDescendant
    , taskStatusOptionDisabledReason
    , taskStatusOptionsForTask
    , update
    , refreshViewport
    , refreshViewportFor
    , updateViewport
    , logicalRows
    , mountedViewportKeys
    , visibleTaskTreeForCriteria
    , viewDeleteConfirmModal
    , viewProjectsTree
    )

import Api
import Array
import Char
import Dict
import Feature.DataLoading
import Feature.AuditLog
import Feature.Dependencies
import Feature.DragDrop
import Feature.Editing
import Feature.Focus
import Permissions
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import HierarchyViewport as Viewport
import Json.Encode as Encode
import Json.Decode as Decode
import Ports exposing (copyToClipboard)
import Set
import Toast exposing (addToast)
import Types exposing (..)


init : CardsModel
init =
    { viewport =
        { workspaceId = Nothing, sessionEpoch = 0, generation = 0, revision = 0
        , rows = Dict.empty, index = Viewport.build 160 Dict.empty [], projection = Nothing
        , top = 0, height = 800, nativePins = Set.empty, target = Nothing, preserveScroll = False, filterExtent = 0
        }
    , expandedCards = Dict.empty
    , collapsedNodes = Dict.empty
    , deleteConfirmation = Nothing
    , lastFocusClick = Nothing
    , projectNextTasks = Dict.empty
    , projectNextTaskDiagnostics = Dict.empty
    , projectNextTasksLoading = Dict.empty
    , projectNextTaskDiagnosticsLoading = Dict.empty
    , projectNextTasksErrors = Dict.empty
    , projectNextTaskDiagnosticsErrors = Dict.empty
    }


{-| Focus mode uses a custom click interval instead of the browser dblclick
threshold so slower repeated clicks do not accidentally enter focus mode.
-}
focusDoubleClickThresholdMs : Float
focusDoubleClickThresholdMs =
    250


focusClickIntervalTriggers : Float -> Bool
focusClickIntervalTriggers deltaMs =
    deltaMs >= 0 && deltaMs <= focusDoubleClickThresholdMs



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RegisterFocusClick entityType entityId timeStampMs ->
            let
                currentClick =
                    { entityType = entityType, entityId = entityId, timeStampMs = timeStampMs }

                storeClick =
                    updateCardsModel (\records -> { records | lastFocusClick = Just currentClick }) model

                sameTarget previous =
                    previous.entityType == entityType && previous.entityId == entityId
            in
            case model.cards.lastFocusClick of
                Just previous ->
                    if sameTarget previous && focusClickIntervalTriggers (timeStampMs - previous.timeStampMs) then
                        Feature.Focus.update (FocusEntity entityType entityId)
                            (updateCardsModel (\records -> { records | lastFocusClick = Nothing }) model)

                    else
                        ( storeClick, Cmd.none )

                Nothing ->
                    ( storeClick, Cmd.none )

        ToggleCardExpand cardId ->
            let
                current =
                    Dict.get cardId model.cards.expandedCards |> Maybe.withDefault False

                newExpanded =
                    not current

                branchKind =
                    if Dict.member cardId model.projects then
                        Just "project"

                    else if Dict.member cardId model.tasks then
                        Just "task"

                    else
                        Nothing

                branchKey =
                    branchKind |> Maybe.map (\kind -> kind ++ ":" ++ cardId)

                shouldFetchBranch =
                    newExpanded
                        && (branchKey
                                |> Maybe.map
                                    (\key ->
                                        case Dict.get key model.dataLoading.loadedNavigationBranches of
                                            Nothing ->
                                                True

                                            Just state ->
                                                not state.inFlight && not state.succeeded
                                    )
                                |> Maybe.withDefault False
                           )

                fetchDepCmd =
                    if newExpanded && Dict.member cardId model.tasks && not (hasTaskDependencyData model cardId) then
                        case model.selectedWorkspaceId of
                            Just workspaceId ->
                                Api.fetchTaskDependencyPage model.flags.apiUrl cardId 0
                                    (GotTaskDependencyPage cardId workspaceId model.sessionRequestEpoch model.dependencies.nextTaskDependencyRequestGeneration 0)

                            Nothing ->
                                Cmd.none

                    else
                        Cmd.none

                shouldFetchProjectNextTasks =
                    newExpanded && Dict.member cardId model.projects && not (Dict.member cardId model.cards.projectNextTasks)

                fetchProjectNextTasksCmd =
                    if shouldFetchProjectNextTasks then
                        Cmd.batch
                            [ Api.fetchProjectNextTasks model.flags.apiUrl cardId 5 False (GotProjectNextTasks cardId)
                            , Api.fetchProjectNextTasks model.flags.apiUrl cardId 200 True (GotProjectNextTaskDiagnostics cardId)
                            ]

                    else
                        Cmd.none

                currentCards =
                    model.cards

                updatedCards =
                    { currentCards
                        | expandedCards = Dict.insert cardId newExpanded model.cards.expandedCards
                        , projectNextTasksLoading =
                            if shouldFetchProjectNextTasks then
                                Dict.insert cardId True model.cards.projectNextTasksLoading

                            else
                                model.cards.projectNextTasksLoading
                        , projectNextTasksErrors =
                            if shouldFetchProjectNextTasks then
                                Dict.remove cardId model.cards.projectNextTasksErrors

                            else
                                model.cards.projectNextTasksErrors
                        , projectNextTaskDiagnosticsLoading =
                            if shouldFetchProjectNextTasks then
                                Dict.insert cardId True model.cards.projectNextTaskDiagnosticsLoading

                            else
                                model.cards.projectNextTaskDiagnosticsLoading
                        , projectNextTaskDiagnosticsErrors =
                            if shouldFetchProjectNextTasks then
                                Dict.remove cardId model.cards.projectNextTaskDiagnosticsErrors

                            else
                                model.cards.projectNextTaskDiagnosticsErrors
                    }

                updatedDependencies =
                    if newExpanded && Dict.member cardId model.tasks && not (hasTaskDependencyData model cardId) then
                        let
                            dependencies =
                                model.dependencies
                        in
                        case model.selectedWorkspaceId of
                            Just workspaceId ->
                                let
                                    request =
                                        { workspaceId = workspaceId
                                        , sessionEpoch = model.sessionRequestEpoch
                                        , offset = 0
                                        , generation = dependencies.nextTaskDependencyRequestGeneration
                                        }
                                in
                                { dependencies
                                    | taskDependencyLoading = Dict.insert cardId True dependencies.taskDependencyLoading
                                    , taskDependencyRequests = Dict.insert cardId request dependencies.taskDependencyRequests
                                    , nextTaskDependencyRequestGeneration = request.generation + 1
                                }

                            Nothing ->
                                dependencies

                    else
                        model.dependencies

                currentEditing =
                    model.editing

                updatedEditing =
                    { currentEditing | editState = Nothing }
            in
            let
                updatedModel =
                    { model | cards = updatedCards, editing = updatedEditing, dependencies = updatedDependencies }

                ( branchModel, branchCmd ) =
                    if shouldFetchBranch then
                        case ( branchKind, model.selectedWorkspaceId ) of
                            ( Just kind, Just workspaceId ) ->
                                Feature.DataLoading.beginNavigationBranch kind workspaceId (Just cardId) updatedModel

                            _ ->
                                ( updatedModel, Cmd.none )

                    else
                        ( updatedModel, Cmd.none )
            in
            ( branchModel
            , Cmd.batch [ fetchDepCmd, fetchProjectNextTasksCmd, branchCmd ]
            )

        LoadNavigationBranchPage parentKind parentId entityKind ->
            Feature.DataLoading.beginNavigationBranchPage parentKind parentId entityKind model

        ShowPreviousNavigationBranchPage parentKind parentId entityKind ->
            Feature.DataLoading.beginNavigationBranchPreviousPage parentKind parentId entityKind model

        RefreshProjectNextTasks projectId ->
            let
                currentCards =
                    model.cards

                updatedCards =
                    { currentCards
                        | projectNextTasksLoading = Dict.insert projectId True currentCards.projectNextTasksLoading
                        , projectNextTaskDiagnosticsLoading = Dict.insert projectId True currentCards.projectNextTaskDiagnosticsLoading
                        , projectNextTasksErrors = Dict.remove projectId currentCards.projectNextTasksErrors
                        , projectNextTaskDiagnosticsErrors = Dict.remove projectId currentCards.projectNextTaskDiagnosticsErrors
                    }
            in
            ( { model | cards = updatedCards }
            , Cmd.batch
                [ Api.fetchProjectNextTasks model.flags.apiUrl projectId 5 False (GotProjectNextTasks projectId)
                , Api.fetchProjectNextTasks model.flags.apiUrl projectId 200 True (GotProjectNextTaskDiagnostics projectId)
                ]
            )

        GotProjectNextTasks projectId result ->
            let
                currentCards =
                    model.cards
            in
            case result of
                Ok candidates ->
                    ( { model
                        | cards =
                            { currentCards
                                | projectNextTasks = Dict.insert projectId candidates currentCards.projectNextTasks
                                , projectNextTasksLoading = Dict.insert projectId False currentCards.projectNextTasksLoading
                                , projectNextTasksErrors = Dict.remove projectId currentCards.projectNextTasksErrors
                            }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | cards =
                            { currentCards
                                | projectNextTasksLoading = Dict.insert projectId False currentCards.projectNextTasksLoading
                                , projectNextTasksErrors = Dict.insert projectId "Failed to load next tasks" currentCards.projectNextTasksErrors
                            }
                      }
                    , Cmd.none
                    )

        GotProjectNextTaskDiagnostics projectId result ->
            case result of
                Ok candidates ->
                    let
                        currentCards =
                            model.cards
                    in
                    ( { model
                        | cards =
                            { currentCards
                                | projectNextTaskDiagnostics = Dict.insert projectId candidates currentCards.projectNextTaskDiagnostics
                                , projectNextTaskDiagnosticsLoading = Dict.insert projectId False currentCards.projectNextTaskDiagnosticsLoading
                                , projectNextTaskDiagnosticsErrors = Dict.remove projectId currentCards.projectNextTaskDiagnosticsErrors
                            }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    let
                        currentCards =
                            model.cards
                    in
                    ( { model
                        | cards =
                            { currentCards
                                | projectNextTaskDiagnosticsLoading = Dict.insert projectId False currentCards.projectNextTaskDiagnosticsLoading
                                , projectNextTaskDiagnosticsErrors = Dict.insert projectId "Failed to load blocked diagnostics" currentCards.projectNextTaskDiagnosticsErrors
                            }
                      }
                    , Cmd.none
                    )

        ToggleTreeNode nodeId ->
            let
                current =
                    Dict.get nodeId model.cards.collapsedNodes |> Maybe.withDefault False

                newModel =
                    updateCardsModel
                        (\records -> { records | collapsedNodes = Dict.insert nodeId (not current) records.collapsedNodes })
                        model
                        |> Feature.DataLoading.resetNavigationPresentations

                -- A branch is loaded only when a node is opened and no
                -- response (successful or in-flight) exists for that branch.
                -- The DataLoading merge is ID based, so this never discards
                -- cards returned by a sibling branch.
                ( branchModel, loadBranch ) =
                    if current then
                        case model.selectedWorkspaceId of
                            Just workspaceId ->
                                if String.startsWith "proj-" nodeId then
                                    let
                                        projectId = String.dropLeft 5 nodeId
                                    in
                                    case Dict.get ("project:" ++ projectId) model.dataLoading.loadedNavigationBranches of
                                        Just state ->
                                            if state.inFlight then
                                                ( newModel, Cmd.none )

                                            else if state.succeeded then
                                                Feature.DataLoading.ensureNavigationPresentation "project" (Just projectId) newModel

                                            else
                                                Feature.DataLoading.beginNavigationBranch "project" workspaceId (Just projectId) newModel

                                        Nothing ->
                                            Feature.DataLoading.beginNavigationBranch "project" workspaceId (Just projectId) newModel

                                else if String.startsWith "task-" nodeId then
                                    let
                                        taskId = String.dropLeft 5 nodeId
                                    in
                                    case Dict.get ("task:" ++ taskId) model.dataLoading.loadedNavigationBranches of
                                        Just state ->
                                            if state.inFlight then
                                                ( newModel, Cmd.none )

                                            else if state.succeeded then
                                                Feature.DataLoading.ensureNavigationPresentation "task" (Just taskId) newModel

                                            else
                                                Feature.DataLoading.beginNavigationBranch "task" workspaceId (Just taskId) newModel

                                        Nothing ->
                                            Feature.DataLoading.beginNavigationBranch "task" workspaceId (Just taskId) newModel

                                else
                                    ( newModel, Cmd.none )

                            Nothing ->
                                ( newModel, Cmd.none )

                    else
                        ( newModel, Cmd.none )
            in
            ( branchModel, Cmd.batch [ saveFiltersCmd branchModel, loadBranch ] )

        ExpandAllNodes ->
            let
                newModel =
                    updateCardsModel (\records -> { records | collapsedNodes = Dict.empty }) model
                        |> Feature.DataLoading.resetNavigationPresentations
            in
            let
                ( loadingModel, loadingCommand ) =
                    Feature.DataLoading.ensureAllNavigationPresentations newModel
            in
            ( loadingModel, Cmd.batch [ saveFiltersCmd loadingModel, loadingCommand ] )

        CollapseAllNodes ->
            let
                projectNodes =
                    model.projects
                        |> Dict.values
                        |> List.map (\record -> ( "proj-" ++ record.id, True ))

                taskNodes =
                    model.tasks
                        |> Dict.values
                        |> (\tasks ->
                                tasks
                                    |> List.filter (\task -> List.any (\t2 -> t2.parentId == Just task.id) tasks)
                           )
                        |> List.map (\record -> ( "task-" ++ record.id, True ))

                newModel =
                    updateCardsModel (\records -> { records | collapsedNodes = Dict.fromList (projectNodes ++ taskNodes) }) model
                        |> Feature.DataLoading.resetNavigationPresentations
            in
            ( newModel, saveFiltersCmd newModel )

        ConfirmDelete entityType entityId ->
            let
                currentCards =
                    model.cards

                confirmation =
                    { entityType = entityType
                    , entityId = entityId
                    , preview = cascadeDeletePreview model entityType entityId
                    }

                updatedCards =
                    { currentCards | deleteConfirmation = Just confirmation }
            in
            ( { model | cards = updatedCards }, focusElement "delete-confirm-cancel" )

        PerformDelete ->
            case model.cards.deleteConfirmation of
                Just confirmation ->
                    if not (canDeleteEntity model confirmation.entityType) then
                        addToast Warning "You no longer have permission to delete this item"
                            (updateCardsModel (\records -> { records | deleteConfirmation = Nothing }) model)

                    else
                        let
                            entityId =
                                confirmation.entityId

                            currentCards =
                                model.cards

                            updatedCards =
                                { currentCards | deleteConfirmation = Nothing }

                            ( trackedModel, requestId, clearCmd ) =
                                beginTrackedMutation [ entityId ] { model | cards = updatedCards }

                            cmd =
                                case confirmation.entityType of
                                    "project" ->
                                        Api.deleteProject model.flags.apiUrl entityId requestId (CascadeDeleteDone confirmation)

                                    "task" ->
                                        Api.deleteTask model.flags.apiUrl entityId requestId (CascadeDeleteDone confirmation)

                                    "workspace" ->
                                        Api.deleteWorkspace model.flags.apiUrl entityId requestId (WorkspaceDeleted entityId)

                                    "group" ->
                                        Api.deleteWorkspaceGroup model.flags.apiUrl entityId requestId (WorkspaceGroupDeleted entityId)

                                    _ ->
                                        Cmd.none
                        in
                        ( trackedModel, Cmd.batch [ clearCmd, cmd ] )

                Nothing ->
                    ( model, Cmd.none )

        CascadeDeleteDone confirmation result ->
            case result of
                Ok cascade ->
                    let
                        ( toastedModel, toastCmd ) =
                            addToast Success (cascadeDeleteSuccessMessage confirmation cascade) model

                        currentCards =
                            toastedModel.cards

                        cacheClearedModel =
                            { toastedModel
                                | dependencies = Feature.Dependencies.resetCache toastedModel.dependencies
                                , cards =
                                    { currentCards
                                        | projectNextTasks = Dict.empty
                                        , projectNextTaskDiagnostics = Dict.empty
                                        , projectNextTasksLoading = Dict.empty
                                        , projectNextTaskDiagnosticsLoading = Dict.empty
                                        , projectNextTasksErrors = Dict.empty
                                        , projectNextTaskDiagnosticsErrors = Dict.empty
                                    }
                            }

                        ( reloadedModel, reloadCmd ) =
                            beginWorkspaceDataReload False cacheClearedModel
                    in
                    ( reloadedModel, Cmd.batch [ toastCmd, reloadCmd ] )

                Err err ->
                    let
                        ( toastedModel, toastCmd ) =
                            addToast Error (Api.apiErrorToUserMessage (cascadeDeleteFailureFallback confirmation) err) model

                        currentCards =
                            toastedModel.cards

                        cacheClearedModel =
                            { toastedModel
                                | dependencies = Feature.Dependencies.resetCache toastedModel.dependencies
                                , cards =
                                    { currentCards
                                        | projectNextTasks = Dict.empty
                                        , projectNextTaskDiagnostics = Dict.empty
                                        , projectNextTasksLoading = Dict.empty
                                        , projectNextTaskDiagnosticsLoading = Dict.empty
                                        , projectNextTasksErrors = Dict.empty
                                        , projectNextTaskDiagnosticsErrors = Dict.empty
                                    }
                            }

                        ( reloadedModel, reloadCmd ) =
                            beginWorkspaceDataReload False cacheClearedModel
                    in
                    ( reloadedModel, Cmd.batch [ toastCmd, reloadCmd ] )

        CancelDelete ->
            let
                currentCards =
                    model.cards

                updatedCards =
                    { currentCards | deleteConfirmation = Nothing }
            in
            ( { model | cards = updatedCards }, Cmd.none )

        CopyId idStr ->
            ( model, copyToClipboard idStr )

        ScrollToEntity entityId ->
            ( updateCardsModel
                (\records -> { records | expandedCards = Dict.insert entityId True records.expandedCards })
                model
            , scrollToElement ("entity-" ++ entityId)
            )

        _ ->
            ( model, Cmd.none )


handleEscape : Model -> Maybe Model
handleEscape model =
    if model.cards.deleteConfirmation /= Nothing then
        Just (updateCardsModel (\records -> { records | deleteConfirmation = Nothing }) model)

    else
        Nothing


updateCardsModel : (CardsModel -> CardsModel) -> Model -> Model
updateCardsModel fn model =
    { model | cards = fn model.cards }


canDeleteEntity : Model -> String -> Bool
canDeleteEntity model entityType =
    case entityType of
        "workspace" ->
            Permissions.canAdminCurrentWorkspace model

        "group" ->
            Permissions.isSuperadmin model

        _ ->
            Permissions.canEditCurrentWorkspace model


cascadeDeletePreview : Model -> String -> String -> Maybe CascadeDeletePreview
cascadeDeletePreview model entityType entityId =
    case entityType of
        "project" ->
            projectCascadePreview entityId (Dict.values model.projects) (Dict.values model.tasks)

        "task" ->
            taskCascadePreview entityId (Dict.values model.tasks)

        _ ->
            Nothing


taskCascadePreview : String -> List Api.Task -> Maybe CascadeDeletePreview
taskCascadePreview taskId tasks =
    if List.any (\task -> task.id == taskId) tasks then
        let
            taskIds =
                collectTaskSubtreeIds tasks [ taskId ] Dict.empty

            taskCount =
                Dict.size taskIds
        in
        Just { affected = taskCount, projectCount = 0, taskCount = taskCount }

    else
        Nothing


projectCascadePreview : String -> List Api.Project -> List Api.Task -> Maybe CascadeDeletePreview
projectCascadePreview projectId projects tasks =
    if List.any (\project -> project.id == projectId) projects then
        let
            projectIds =
                collectProjectSubtreeIds projects [ projectId ] Dict.empty

            seedTaskIds =
                tasks
                    |> List.filter (\task -> Maybe.map (\pid -> Dict.member pid projectIds) task.projectId |> Maybe.withDefault False)
                    |> List.map .id

            taskIds =
                collectTaskSubtreeIds tasks seedTaskIds Dict.empty

            projectCount =
                Dict.size projectIds

            taskCount =
                Dict.size taskIds
        in
        Just { affected = projectCount + taskCount, projectCount = projectCount, taskCount = taskCount }

    else
        Nothing


collectProjectSubtreeIds : List Api.Project -> List String -> Dict.Dict String Bool -> Dict.Dict String Bool
collectProjectSubtreeIds projects pending seen =
    case pending of
        [] ->
            seen

        projectId :: rest ->
            if Dict.member projectId seen then
                collectProjectSubtreeIds projects rest seen

            else
                let
                    childIds =
                        projects
                            |> List.filter (\project -> project.parentId == Just projectId)
                            |> List.map .id
                in
                collectProjectSubtreeIds projects (childIds ++ rest) (Dict.insert projectId True seen)


collectTaskSubtreeIds : List Api.Task -> List String -> Dict.Dict String Bool -> Dict.Dict String Bool
collectTaskSubtreeIds tasks pending seen =
    case pending of
        [] ->
            seen

        taskId :: rest ->
            if Dict.member taskId seen then
                collectTaskSubtreeIds tasks rest seen

            else
                let
                    childIds =
                        tasks
                            |> List.filter (\task -> task.parentId == Just taskId)
                            |> List.map .id
                in
                collectTaskSubtreeIds tasks (childIds ++ rest) (Dict.insert taskId True seen)


cascadeDeleteSuccessMessage : DeleteConfirmation -> Api.CascadeResult -> String
cascadeDeleteSuccessMessage confirmation result =
    let
        baseMessage =
            case confirmation.entityType of
                "project" ->
                    projectDeleteSuccessMessage result

                "task" ->
                    taskDeleteSuccessMessage result

                _ ->
                    "Deleted item."

        finalPreview =
            { affected = result.affected
            , projectCount = result.projectCount
            , taskCount = result.taskCount
            }

        staleSuffix =
            case confirmation.preview of
                Just preview ->
                    if preview == finalPreview then
                        ""

                    else
                        " Server counts changed since preview (preview: " ++ cascadePreviewCountsText preview ++ "; final: " ++ cascadePreviewCountsText finalPreview ++ ")."

                Nothing ->
                    " Final server counts: " ++ cascadePreviewCountsText finalPreview ++ "."
    in
    baseMessage ++ staleSuffix ++ cascadeCleanupSuffix result


cascadeDeleteFailureFallback : DeleteConfirmation -> String
cascadeDeleteFailureFallback confirmation =
    "Failed to delete " ++ deleteEntityNoun confirmation.entityType ++ ". The item may already have changed; refreshing workspace data."


projectDeleteSuccessMessage : Api.CascadeResult -> String
projectDeleteSuccessMessage result =
    let
        subprojectCount =
            Basics.max 0 (result.projectCount - 1)

        projectPart =
            if result.projectCount > 1 then
                Just (countPhrase result.projectCount "project" "projects" ++ " (including " ++ countPhrase subprojectCount "subproject" "subprojects" ++ ")")

            else if result.projectCount == 1 then
                Just "1 project"

            else
                Nothing

        taskPart =
            if result.taskCount > 0 then
                Just (countPhrase result.taskCount "task" "tasks")

            else
                Nothing

        deletedParts =
            List.filterMap identity [ projectPart, taskPart ]
    in
    if result.projectCount <= 1 && result.taskCount == 0 then
        "Deleted project."

    else
        "Deleted project subtree: " ++ joinHuman deletedParts ++ " were deleted."


taskDeleteSuccessMessage : Api.CascadeResult -> String
taskDeleteSuccessMessage result =
    let
        subtaskCount =
            Basics.max 0 (result.taskCount - 1)
    in
    if result.taskCount <= 1 then
        "Deleted task."

    else
        "Deleted task cascade: " ++ countPhrase result.taskCount "task" "tasks" ++ " (including " ++ countPhrase subtaskCount "subtask" "subtasks" ++ ") were deleted."


cascadeCleanupSuffix : Api.CascadeResult -> String
cascadeCleanupSuffix result =
    let
        cleanupParts =
            List.filterMap identity
                [ if result.dependencyLinkCount > 0 then
                    Just (countPhrase result.dependencyLinkCount "task dependency" "task dependencies")

                  else
                    Nothing
                ]
    in
    case cleanupParts of
        [] ->
            ""

        _ ->
            " Also updated " ++ joinHuman cleanupParts ++ "."


cascadePreviewCountsText : CascadeDeletePreview -> String
cascadePreviewCountsText preview =
    let
        parts =
            List.filterMap identity
                [ if preview.projectCount > 0 then
                    Just (countPhrase preview.projectCount "project" "projects")

                  else
                    Nothing
                , if preview.taskCount > 0 then
                    Just (countPhrase preview.taskCount "task" "tasks")

                  else
                    Nothing
                ]
    in
    case parts of
        [] ->
            "0 project/task items"

        _ ->
            String.join " and " parts


deleteEntityNoun : String -> String
deleteEntityNoun entityType =
    case entityType of
        "project" ->
            "project"

        "task" ->
            "task"

        _ ->
            "item"


isOpenProjectStatus : Api.ProjectStatus -> Bool
isOpenProjectStatus status =
    status == Api.ProjActive || status == Api.ProjPaused


isOpenTaskStatus : Api.TaskStatus -> Bool
isOpenTaskStatus status =
    status == Api.Todo || status == Api.InProgress || status == Api.Blocked


projectAndAncestorsAreOpenIndexed : Dict.Dict String Api.Project -> String -> Bool
projectAndAncestorsAreOpenIndexed projectsById projectId =
    case Dict.get projectId projectsById of
        Just project ->
            isOpenProjectStatus project.status
                && (case project.parentId of
                        Just parentId -> projectAndAncestorsAreOpenIndexed projectsById parentId

                        Nothing ->
                            True
                   )

        Nothing ->
            True


hasClosedTaskAncestorIndexed : Dict.Dict String Api.Task -> Api.Task -> Bool
hasClosedTaskAncestorIndexed tasksById task =
    case task.parentId |> Maybe.andThen (\parentId -> Dict.get parentId tasksById) of
        Just parent ->
            parent.status == Api.Done || parent.status == Api.Cancelled || hasClosedTaskAncestorIndexed tasksById parent

        Nothing ->
            False


projectCompletionBlockerReason : Int -> Int -> Maybe String
projectCompletionBlockerReason openProjectCount openTaskCount =
    if openProjectCount == 0 && openTaskCount == 0 then
        Nothing

    else
        let
            actions =
                List.filterMap identity
                    [ if openProjectCount > 0 then
                        Just ("complete/archive " ++ countPhrase openProjectCount "child project" "child projects")

                      else
                        Nothing
                    , if openTaskCount > 0 then
                        Just ("finish/cancel " ++ countPhrase openTaskCount "task" "tasks")

                      else
                        Nothing
                    ]
        in
        Just (sentenceCase (String.join " and " actions) ++ " before closing this project.")


taskCompletionBlockerReason : Int -> Maybe String
taskCompletionBlockerReason openTaskCount =
    if openTaskCount == 0 then
        Nothing

    else
        Just ("Finish or cancel " ++ countPhrase openTaskCount "subtask" "subtasks" ++ " before marking this task done.")


taskStatusOptionsForTask : Bool -> Api.Task -> List Api.TaskStatus
taskStatusOptionsForTask dependencyBlocked task =
    let
        transitionTargets =
            if dependencyBlocked then
                [ Api.Blocked, Api.Cancelled ]

            else
                Api.allTaskStatuses
    in
    ensureTaskStatusOption task.status transitionTargets


taskStatusOptionDisabledReason : Maybe String -> Bool -> Maybe Api.TaskStatus -> Api.TaskStatus -> Maybe String
taskStatusOptionDisabledReason completionBlockerReason isSubtask mParentStatus status =
    if status == Api.Done then
        completionBlockerReason

    else if isOpenTaskStatus status && mParentStatus == Just Api.Cancelled then
        Just "Reopen the cancelled parent task before reopening this subtask."

    else if isOpenTaskStatus status && mParentStatus == Just Api.Done then
        Just "Reopen the parent task before reopening this subtask."

    else if status == Api.InProgress && isSubtask && mParentStatus /= Just Api.InProgress then
        Just "Start the parent task before moving this subtask to in progress."

    else
        Nothing


ensureTaskStatusOption : Api.TaskStatus -> List Api.TaskStatus -> List Api.TaskStatus
ensureTaskStatusOption currentStatus statuses =
    if List.member currentStatus statuses then
        statuses

    else
        currentStatus :: statuses


directOpenDependencyCounts : Dict.Dict String Api.Task -> List Api.WorkspaceTaskDependencyLink -> Dict.Dict String Int
directOpenDependencyCounts tasksById links =
    links
        |> List.foldl
            (\link counts ->
                case Dict.get link.dependsOnId tasksById of
                    Just dependency ->
                        if isOpenTaskStatus dependency.status then
                            Dict.update link.taskId (Maybe.withDefault 0 >> (+) 1 >> Just) counts

                        else
                            counts

                    Nothing ->
                        counts
            )
            Dict.empty


onFocusClick : String -> String -> Attribute Msg
onFocusClick entityType entityId =
    on "click" (focusClickDecoder entityType entityId)


focusClickDecoder : String -> String -> Decode.Decoder Msg
focusClickDecoder entityType entityId =
    Decode.map2
        (\targetTag timeStampMs ->
            if focusClickTargetIsInteractive targetTag then
                NoOp

            else
                RegisterFocusClick entityType entityId timeStampMs
        )
        (Decode.oneOf [ Decode.at [ "target", "tagName" ] Decode.string, Decode.succeed "" ])
        (Decode.field "timeStamp" Decode.float)


focusClickTargetIsInteractive : String -> Bool
focusClickTargetIsInteractive tagName =
    List.member (String.toUpper tagName) [ "A", "BUTTON", "INPUT", "OPTION", "SELECT", "TEXTAREA" ]


joinHuman : List String -> String
joinHuman parts =
    case parts of
        [] ->
            ""

        [ one ] ->
            one

        [ one, two ] ->
            one ++ " and " ++ two

        first :: rest ->
            String.join ", " (first :: List.take (List.length rest - 1) rest)
                ++ ", and "
                ++ (List.reverse rest |> List.head |> Maybe.withDefault "")


countPhrase : Int -> String -> String -> String
countPhrase count singular pluralLabel =
    String.fromInt count ++ " " ++ (if count == 1 then singular else pluralLabel)


sentenceCase : String -> String
sentenceCase value =
    case String.uncons value of
        Just ( first, rest ) ->
            String.fromChar (Char.toUpper first) ++ rest

        Nothing ->
            value


-- VIEW





type alias CardTreeProjection =
    Types.CardTreeProjection


orderedProjects : List Api.Project -> List Api.Project
orderedProjects =
    List.sortBy (\project -> ( Api.projectStatusOrder project.status, negate project.priority, String.toLower project.name ))


orderedTasks : List Api.Task -> List Api.Task
orderedTasks =
    List.sortBy (\task -> ( Api.taskStatusOrder task.status, negate task.priority, String.toLower task.title ))


cardTreeProjection : String -> Model -> CardTreeProjection
cardTreeProjection workspaceId model =
    let
        focusedProjectId =
            model.focus.focusedEntity
                |> Maybe.andThen (\( kind, entityId ) -> if kind == "project" then Just entityId else Nothing)

        focusedTaskId =
            model.focus.focusedEntity
                |> Maybe.andThen (\( kind, entityId ) -> if kind == "task" then Just entityId else Nothing)

        projectIds =
            if model.dataLoading.navigationVisibilityActive then
                model.dataLoading.navigationVisibleProjectIds
                    |> Set.union (focusedProjectId |> Maybe.map Set.singleton |> Maybe.withDefault Set.empty)

            else
                -- Legacy/full snapshots remain supported, but the bounded
                -- shell path never walks the raw dictionary to discover
                -- presentation membership.
                model.projects |> Dict.keys |> Set.fromList

        taskIds =
            if model.dataLoading.navigationVisibilityActive then
                model.dataLoading.navigationVisibleTaskIds
                    |> Set.union (focusedTaskId |> Maybe.map Set.singleton |> Maybe.withDefault Set.empty)

            else
                model.tasks |> Dict.keys |> Set.fromList

        projects =
            projectIds
                |> Set.toList
                |> List.filterMap (\projectId -> Dict.get projectId model.projects)
                |> List.filter (\project -> project.workspaceId == workspaceId)
                |> orderedProjects

        tasks =
            taskIds
                |> Set.toList
                |> List.filterMap (\taskId -> Dict.get taskId model.tasks)
                |> List.filter (\task -> task.workspaceId == workspaceId)
                |> orderedTasks

        addProject project index =
            case project.parentId of
                Just parentId -> Dict.update parentId (Maybe.withDefault [] >> (\items -> project :: items) >> Just) index
                Nothing -> index

        addTask task ( byProject, byParent ) =
            let
                nextByProject =
                    case ( task.projectId, task.parentId ) of
                        ( Just projectId, Nothing ) -> Dict.update projectId (Maybe.withDefault [] >> (\items -> task :: items) >> Just) byProject
                        _ -> byProject

                nextByParent =
                    case task.parentId of
                        Just parentId -> Dict.update parentId (Maybe.withDefault [] >> (\items -> task :: items) >> Just) byParent
                        Nothing -> byParent
            in
            ( nextByProject, nextByParent )

        taskIndexes =
            List.foldl addTask ( Dict.empty, Dict.empty ) tasks

        projectChildren =
            List.foldl addProject Dict.empty projects
                |> Dict.map (\_ -> List.reverse)

        projectTasks =
            Tuple.first taskIndexes
                |> Dict.map (\_ -> List.reverse)

        taskChildren =
            Tuple.second taskIndexes
                |> Dict.map (\_ -> List.reverse)

        projectsById =
            indexBy .id projects

        tasksById =
            indexBy .id tasks

        projectAllowsOpenChildren =
            projectAncestorOpenIndex projectsById projects

        taskHasClosedAncestor =
            taskClosedAncestorIndex tasksById tasks

        taskDirectOpenDependencyCounts =
            directOpenDependencyCounts tasksById model.dependencies.taskDependencyLinks

        query =
            String.toLower (String.trim model.search.query)

        hasSearch =
            not (String.isEmpty query)

        taskCriteriaMatches =
            if treeCriteriaActive query model && not model.dataLoading.navigationVisibilityActive then
                tasks
                    |> List.filter (taskTreeMatchesIndexed query hasSearch (taskPassesCurrentFilters model) taskChildren)
                    |> List.map .id
                    |> Set.fromList

            else
                Set.empty

        projectCriteriaMatches =
            if treeCriteriaActive query model && not model.dataLoading.navigationVisibilityActive then
                projects
                    |> List.filter
                        (projectTreeMatchesIndexed
                            query
                            hasSearch
                            (projectPassesStatusFilter model)
                            (projectPassesCurrentFilters model)
                            projectChildren
                            projectTasks
                            taskCriteriaMatches
                        )
                    |> List.map .id
                    |> Set.fromList

            else
                Set.empty
    in
    { projects = projects
    , tasks = tasks
    , projectsById = projectsById
    , tasksById = tasksById
    , projectChildren = projectChildren
    , projectTasks = projectTasks
    , taskChildren = taskChildren
    , projectRollups = model.dependencies.projectReadinessRollups
    , taskRollups = model.dependencies.taskReadinessRollups
    , projectAllowsOpenChildren = projectAllowsOpenChildren
    , taskHasClosedAncestor = taskHasClosedAncestor
    , taskDirectOpenDependencyCounts = taskDirectOpenDependencyCounts
    , projectCriteriaMatches = projectCriteriaMatches
    , taskCriteriaMatches = taskCriteriaMatches
    }


{-| Resolve ancestry once per projection.  A wide/deep visible tree otherwise
repeatedly walked the same parents for every rendered card.  The `visiting`
set makes malformed cyclic cached data fail closed without recursing forever.
-}
projectAncestorOpenIndex : Dict.Dict String Api.Project -> List Api.Project -> Dict.Dict String Bool
projectAncestorOpenIndex projectsById projects =
    let
        resolve projectId visiting cache =
            case Dict.get projectId cache of
                Just value ->
                    ( value, cache )

                Nothing ->
                    if Set.member projectId visiting then
                        ( False, Dict.insert projectId False cache )

                    else
                        case Dict.get projectId projectsById of
                            Nothing ->
                                ( True, cache )

                            Just project ->
                                let
                                    ( parentOpen, afterParent ) =
                                        case project.parentId of
                                            Just parentId ->
                                                resolve parentId (Set.insert projectId visiting) cache

                                            Nothing ->
                                                ( True, cache )

                                    open =
                                        isOpenProjectStatus project.status && parentOpen
                                in
                                ( open, Dict.insert projectId open afterParent )
    in
    projects
        |> List.foldl
            (\project cache ->
                resolve project.id Set.empty cache
                    |> Tuple.second
            )
            Dict.empty


taskClosedAncestorIndex : Dict.Dict String Api.Task -> List Api.Task -> Dict.Dict String Bool
taskClosedAncestorIndex tasksById tasks =
    let
        resolve taskId visiting cache =
            case Dict.get taskId cache of
                Just value ->
                    ( value, cache )

                Nothing ->
                    if Set.member taskId visiting then
                        ( True, Dict.insert taskId True cache )

                    else
                        case Dict.get taskId tasksById of
                            Nothing ->
                                ( False, cache )

                            Just task ->
                                let
                                    ( ancestorDone, afterParent ) =
                                        case task.parentId of
                                            Just parentId ->
                                                case Dict.get parentId tasksById of
                                                    Just parent ->
                                                        if parent.status == Api.Done || parent.status == Api.Cancelled then
                                                            ( True, cache )

                                                        else
                                                            resolve parentId (Set.insert taskId visiting) cache

                                                    Nothing ->
                                                        ( False, cache )

                                            Nothing ->
                                                ( False, cache )
                                in
                                ( ancestorDone, Dict.insert taskId ancestorDone afterParent )
    in
    tasks
        |> List.foldl
            (\task cache ->
                resolve task.id Set.empty cache
                    |> Tuple.second
            )
            Dict.empty


taskTreeMatchesIndexed : String -> Bool -> (Api.Task -> Bool) -> Dict.Dict String (List Api.Task) -> Api.Task -> Bool
taskTreeMatchesIndexed query hasSearch taskFilter children task =
    taskMatchesActiveCriteria query hasSearch taskFilter task
        || (Dict.get task.id children
                |> Maybe.withDefault []
                |> List.any (taskTreeMatchesIndexed query hasSearch taskFilter children)
           )


projectTreeMatchesIndexed : String -> Bool -> (Api.Project -> Bool) -> (Api.Project -> Bool) -> Dict.Dict String (List Api.Project) -> Dict.Dict String (List Api.Task) -> Set.Set String -> Api.Project -> Bool
projectTreeMatchesIndexed query hasSearch projectStatusGate projectFilter children projectTasks matchingTaskIds project =
    projectStatusGate project
        && (projectMatchesActiveCriteria query hasSearch projectFilter project
                || (Dict.get project.id children
                        |> Maybe.withDefault []
                        |> List.any (projectTreeMatchesIndexed query hasSearch projectStatusGate projectFilter children projectTasks matchingTaskIds)
                   )
                || (Dict.get project.id projectTasks
                        |> Maybe.withDefault []
                        |> List.any (\task -> Set.member task.id matchingTaskIds)
                   )
           )


viewCardDescription : Model -> String -> String -> Maybe String -> Html Msg
viewCardDescription model entityType entityId description =
    let
        summaryPresent =
            if entityType == "project" then
                Dict.member entityId model.dataLoading.projectCardSummaries

            else
                Dict.member entityId model.dataLoading.taskCardSummaries

        request =
            if entityType == "project" then
                Dict.get entityId model.dataLoading.projectCardDetailRequests

            else
                Dict.get entityId model.dataLoading.taskCardDetailRequests
    in
    if not summaryPresent then
        Feature.Editing.viewEditableTextarea model entityType entityId "description" (Maybe.withDefault "" description)

    else
        case request of
            Just state ->
                if state.succeeded then
                    Feature.Editing.viewEditableTextarea model entityType entityId "description" (Maybe.withDefault "" description)

                else if state.inFlight then
                    div [ class "card-description-state", attribute "role" "status" ] [ text "Loading description…" ]

                else
                    div [ class "card-description-state card-description-error" ]
                        [ span [] [ text "Description unavailable." ]
                        , button [ class "btn-small btn-ghost", onClick (RetryCardDetail entityType entityId) ] [ text "Retry" ]
                        ]

            Nothing ->
                div [ class "card-description-state card-description-error" ]
                    [ span [] [ text "Description not loaded." ]
                    , button [ class "btn-small btn-ghost", onClick (RetryCardDetail entityType entityId) ] [ text "Retry" ]
                    ]


viewProjectsTree : String -> Model -> Html Msg
viewProjectsTree wsId model =
    viewHierarchyViewport wsId model


viewProjectsTreeLegacy : String -> Model -> Html Msg
viewProjectsTreeLegacy wsId model =
    let
        projection =
            cardTreeProjection wsId model

        wsProjects =
            projection.projects

        wsTasks =
            projection.tasks

        query =
            String.toLower (String.trim model.search.query)

        hasSearch =
            not (String.isEmpty query)

        hasTreeCriteria =
            treeCriteriaActive query model

        {- Navigation is filtered by the server because a card summary does not
           carry descriptions or unloaded descendants.  Re-evaluating the
           predicate locally would hide a retained ancestor/deep description
           match.  Legacy/full data keeps the old client-side behaviour. -}
        applyLocalTreeCriteria =
            hasTreeCriteria && not model.dataLoading.navigationVisibilityActive

        projectStatusGate =
            projectPassesStatusFilter model

        projectPassesFilters =
            projectPassesCurrentFilters model

        taskPassesFilters =
            taskPassesCurrentFilters model

        expandCollapseBar =
            div [ class "tree-toolbar" ]
                [ button [ class "btn-small btn-ghost", onClick ExpandAllNodes ] [ text "Expand All" ]
                , button [ class "btn-small btn-ghost", onClick CollapseAllNodes ] [ text "Collapse All" ]
                ]

        inlineCreateView =
            Feature.Editing.viewInlineCreateInput model Nothing "project"

        focusBreadcrumbBar =
            Feature.Focus.viewFocusBreadcrumbBar model

        treeContent =
            case model.focus.focusedEntity of
                Just ( "project", projId ) ->
                    case Dict.get projId model.projects of
                        Just proj ->
                            [ ( projId, viewProjectNode projection model 0 proj hasSearch query ) ]

                        Nothing ->
                            []

                Just ( "task", taskId ) ->
                    case Dict.get taskId model.tasks of
                        Just task ->
                            [ ( taskId, viewFocusedTaskNode projection model task ) ]

                        Nothing ->
                            []

                _ ->
                    case model.search.filterShowOnly of
                        ShowTasksOnly ->
                            let
                                rootTasks =
                                    wsTasks
                                        |> List.filter (\t -> t.parentId == Nothing)
                                        |> List.sortBy (\t -> ( Api.taskStatusOrder t.status, negate t.priority, String.toLower t.title ))

                                visibleRootTasks =
                                     rootTasks
                                         |> (if applyLocalTreeCriteria then
                                                List.filter (\task -> Set.member task.id projection.taskCriteriaMatches)

                                            else
                                                identity
                                           )
                            in
                            List.map (\t -> ( t.id, viewTaskCard projection False model t )) (rootPresentationWindow "task" .id (pinnedTaskIds model) model visibleRootTasks)
                                ++ viewRootLoadMore model "task" .id (pinnedTaskIds model) visibleRootTasks

                        _ ->
                            let
                                rootProjects =
                                    wsProjects
                                        |> List.filter (\p -> p.parentId == Nothing)
                                        |> List.sortBy (\p -> ( Api.projectStatusOrder p.status, negate p.priority, String.toLower p.name ))

                                visibleRootProjects =
                                     rootProjects
                                         |> (if applyLocalTreeCriteria then
                                                List.filter (\project -> Set.member project.id projection.projectCriteriaMatches)

                                            else
                                                identity
                                           )

                                orphanTasks =
                                    wsTasks
                                        |> List.filter (\t -> t.projectId == Nothing && t.parentId == Nothing)
                                        |> List.sortBy (\t -> ( Api.taskStatusOrder t.status, negate t.priority, String.toLower t.title ))

                                visibleOrphans =
                                     orphanTasks
                                         |> (if applyLocalTreeCriteria then
                                                List.filter (\task -> Set.member task.id projection.taskCriteriaMatches)

                                            else
                                                identity
                                           )
                            in
                            viewProjectsWithZones model (\p -> viewProjectNode projection model 0 p hasSearch query) Nothing (rootPresentationWindow "project" .id (pinnedProjectIds model) model visibleRootProjects)
                                ++ (if model.search.filterShowOnly /= ShowProjectsOnly && not (List.isEmpty visibleOrphans) then
                                        [ ( "orphan-tasks-section"
                                          , div [ class "orphan-tasks-section" ]
                                                [ div [ class "orphan-tasks-header" ] [ text "Unassigned Tasks" ]
                                                , Keyed.node "div" [ class "tree-tasks" ]
                                                    (viewTasksWithZones projection model "orphan" Nothing Nothing (rootPresentationWindow "task" .id (pinnedTaskIds model) model visibleOrphans))
                                                ]
                                          )
                                        ]

                                    else
                                        []
                                   )
                                ++ viewRootLoadMore model "project" .id (pinnedProjectIds model) visibleRootProjects
                                ++ (if model.search.filterShowOnly == ShowProjectsOnly then
                                        []

                                    else
                                        viewRootLoadMore model "task" .id (pinnedTaskIds model) visibleOrphans
                                   )

        contentWithEmptyState =
            if List.isEmpty treeContent then
                [ ( "empty-state"
                  , div [ class "empty-state" ]
                        [ text
                            (if applyLocalTreeCriteria then
                                "No projects or tasks match the current filters."

                             else
                                "No projects or tasks yet."
                            )
                        ]
                  )
                ]

            else
                treeContent
    in
    Keyed.node "div"
        [ class "tree-view" ]
        (( "expand-collapse-bar", expandCollapseBar )
            :: ( "inline-create", inlineCreateView )
            :: ( "focus-breadcrumb", focusBreadcrumbBar )
            :: contentWithEmptyState
        )


viewProjectNode : CardTreeProjection -> Model -> Int -> Api.Project -> Bool -> String -> Html Msg
viewProjectNode projection model depth project hasSearch query =
    viewProjectNodeBody True projection model depth project hasSearch query


viewProjectNodeBody : Bool -> CardTreeProjection -> Model -> Int -> Api.Project -> Bool -> String -> Html Msg
viewProjectNodeBody descendants projection model depth project hasSearch query =
    let
        hasTreeCriteria =
            treeCriteriaActive query model

        applyLocalTreeCriteria =
            hasTreeCriteria && not model.dataLoading.navigationVisibilityActive

        projectStatusGate =
            projectPassesStatusFilter model

        projectPassesFilters =
            projectPassesCurrentFilters model

        taskPassesFilters =
            taskPassesCurrentFilters model

        children =
            Dict.get project.id projection.projectChildren
                |> Maybe.withDefault []
                

        visibleChildren =
            if not descendants then [] else
            children
                |> (if applyLocalTreeCriteria then
                        List.filter (\child -> Set.member child.id projection.projectCriteriaMatches)

                    else
                        identity
                   )
                |> branchPresentationWindow "project" project.id "project" .id (pinnedProjectIds model) model

        hasChildren =
            (Dict.get project.id model.dataLoading.projectCardSummaries
                |> Maybe.map .hasChildren
                |> Maybe.withDefault False
            )
                || not (List.isEmpty children)

        collapsed =
            isCollapsed model ("proj-" ++ project.id)

        projectTasks =
            Dict.get project.id projection.projectTasks
                |> Maybe.withDefault []
                

        visibleTasks =
            if not descendants then [] else
            projectTasks
                |> (if applyLocalTreeCriteria then
                        List.filter (\task -> Set.member task.id projection.taskCriteriaMatches)

                    else
                        identity
                   )
                |> branchPresentationWindow "project" project.id "task" .id (pinnedTaskIds model) model

        maybeProjectRollup =
            case Dict.get project.id projection.projectRollups of
                Just rollup ->
                    Just rollup

                Nothing ->
                    -- A scoped resync retires readiness caches before its bounded
                    -- navigation refresh. Retain the last canonical card total.
                    Dict.get project.id model.dataLoading.projectCardSummaries |> Maybe.map .readinessRollup

        localProjectAggregate =
            case maybeProjectRollup of
                Just rollup ->
                    { openProjects = rollup.openProjectCount, openTasks = rollup.openTaskCount }

                Nothing ->
                    { openProjects = 0
                    , openTasks = 0
                    }

        openSubprojectCount =
            localProjectAggregate.openProjects

        openProjectTaskCount =
            localProjectAggregate.openTasks

        completionBlockerReason =
            projectCompletionBlockerReason openSubprojectCount openProjectTaskCount

        projectStatusDisabled status =
            if status == Api.ProjCompleted then
                completionBlockerReason

            else
                Nothing

        projectAllowsOpenChildren =
            Dict.get project.id projection.projectAllowsOpenChildren
                |> Maybe.withDefault True

        projectCreateChildReason =
            "Reopen this project before adding active child projects or open tasks."
    in
    div [ class "tree-node", style "margin-left" (String.fromInt (depth * 20) ++ "px"), id ("entity-" ++ project.id) ]
        [ div
            [ class ("card tree-card card-project card-status-" ++ Api.projectStatusToString project.status ++ (if maybeProjectRollup |> Maybe.map (\rollup -> rollup.inProgressTaskCount > 0) |> Maybe.withDefault False then " card-project-in-progress" else "") ++ Feature.DragDrop.dragOverClass model project.id)
            , draggable (if Permissions.canEditCurrentWorkspace model then "true" else "false")
            , on "dragstart" (Decode.succeed (DragStartCard "project" project.id))
            , preventDefaultOn "dragover" (Decode.succeed ( DragOverCard project.id, True ))
            , preventDefaultOn "drop" (Decode.succeed ( DropOnCard "project" project.id, True ))
            , on "dragend" (Decode.succeed DragEndCard)
            , onFocusClick "project" project.id
            ]
            [ div [ class "card-header" ]
                [ div [ class "tree-toggle-row" ]
                    [ if hasChildren || not (List.isEmpty projectTasks) then
                        button [ class "tree-toggle", onClick (ToggleTreeNode ("proj-" ++ project.id)) ]
                            [ text
                                (if collapsed then
                                    "▶"

                                 else
                                    "▼"
                                )
                            ]

                      else
                        span [ class "tree-toggle-spacer" ] []
                    , span [ class "entity-type-label entity-type-project" ] [ text "PRJ" ]
                    , Feature.Editing.viewEditableText model "project" project.id "name" project.name
                ]
            , div [ class "card-actions" ]
                [ Feature.Editing.viewStatusSelectWithDisabled model "project" project.id (Api.projectStatusToString project.status) Api.allProjectStatuses Api.projectStatusToString projectStatusDisabled ChangeProjectStatus
                , Feature.Editing.viewPrioritySelect model "project" project.id project.priority ChangeProjectPriority
                , if Permissions.canEditCurrentWorkspace model then
                        button [ class "btn-icon btn-danger", onClick (ConfirmDelete "project" project.id), title "Delete" ] [ text "✕" ]

                      else
                        text ""
                    ]
                ]
            , let
                remainingSubprojects =
                    maybeProjectRollup
                        |> Maybe.map .openProjectCount
                        |> Maybe.withDefault 0

                completedSubprojects =
                    maybeProjectRollup
                        |> Maybe.map .closedProjectCount
                        |> Maybe.withDefault 0

                remainingTasks =
                    maybeProjectRollup
                        |> Maybe.map .openTaskCount
                        |> Maybe.withDefault 0

                completedTasks =
                    maybeProjectRollup
                        |> Maybe.map (\rollup -> rollup.doneTaskCount + rollup.cancelledTaskCount)
                        |> Maybe.withDefault 0

                dependencyBlockedTasks =
                    maybeProjectRollup
                        |> Maybe.map .dependencyBlockedTaskCount
                        |> Maybe.withDefault 0

                openDependencyCount =
                    maybeProjectRollup
                        |> Maybe.map .openDependencyCount
                        |> Maybe.withDefault 0

                summaryParts =
                    List.filterMap identity
                        [ countLabel remainingSubprojects completedSubprojects "subproject" "subprojects"
                        , case maybeProjectRollup of
                            Nothing ->
                                Just "Task counts unavailable"

                            Just _ ->
                                countLabel remainingTasks completedTasks "task" "tasks" |> Maybe.withDefault "0 tasks" |> Just
                        , if dependencyBlockedTasks > 0 then
                            Just (countPhrase dependencyBlockedTasks "dependency-blocked task" "dependency-blocked tasks")

                          else
                            Nothing
                        , if openDependencyCount > 0 then
                            Just (countPhrase openDependencyCount "open dependency" "open dependencies")

                          else
                            Nothing
                        ]
              in
              if List.isEmpty summaryParts then
                text ""

              else
                div [ class "card-summary" ] [ text (String.join " · " summaryParts) ]
            , div [ class "card-body card-expanded" ]
                [ div [ class "card-desc-row" ]
                    [ button [ class "btn-extras-toggle", onClick (ToggleCardExpand project.id) ]
                        [ text
                            (if isExpanded model project.id then
                                "−"

                             else
                                "+"
                            )
                        ]
                    , viewCardDescription model "project" project.id project.description
                    ]
                , if isExpanded model project.id then
                    div [ class "card-extras" ]
                        [ viewProjectNextTasksPanel projection model project
                        , Feature.AuditLog.viewEntityHistory model "project" project.id
                        ]

                  else
                    text ""
                ]
            , div [ class "card-inline-actions" ]
                [ if Permissions.canEditCurrentWorkspace model then
                    button
                        [ class "btn-inline-create"
                        , disabled (not projectAllowsOpenChildren)
                        , title (if projectAllowsOpenChildren then "Add subproject" else projectCreateChildReason)
                        , onClick (ShowInlineCreate (InlineCreateProject { parentId = Just project.id, name = "" }))
                        ]
                        [ text "+ Subproject" ]

                  else
                    text ""
                , if Permissions.canEditCurrentWorkspace model then
                    button
                        [ class "btn-inline-create"
                        , disabled (not projectAllowsOpenChildren)
                        , title (if projectAllowsOpenChildren then "Add task" else projectCreateChildReason)
                        , onClick (ShowInlineCreate (InlineCreateTask { projectId = Just project.id, parentId = Nothing, title = "" }))
                        ]
                        [ text "+ Task" ]

                  else
                    text ""
                ]
            , Feature.Editing.viewInlineCreateInputForParent model (Just project.id) "project"
            , Feature.Editing.viewInlineCreateInputForParent model (Just project.id) "task"
            , div [ class "card-meta-group" ]
                [ div [ class "card-meta-row" ]
                    [ span [ class "card-meta" ] [ text ("Created: " ++ formatDate project.createdAt) ]
                    , span [ class "card-meta" ] [ text ("Updated: " ++ formatDate project.updatedAt) ]
                    , Helpers.copyableValue "card-meta card-id card-id-copy" "project ID" project.id project.id
                    ]
                ]
            ]
        , if descendants && not collapsed then
            Keyed.node "div" [ class "tree-children" ]
                (viewProjectsWithZones model (\c -> viewProjectNode projection model (depth + 1) c hasSearch query) (Just project.id) visibleChildren
                    ++ (if not (List.isEmpty visibleTasks) then
                            [ ( "project-tasks-" ++ project.id
                              , Keyed.node "div" [ class "tree-tasks", style "margin-left" "20px" ]
                                    (viewTasksWithZones projection model "project-tasks" (Just project.id) Nothing visibleTasks)
                              )
                            ]

                         else
                             []
                       )
                    ++ viewBranchLoadMore model "project" project.id "project" .id (pinnedProjectIds model) children
                    ++ viewBranchLoadMore model "project" project.id "task" .id (pinnedTaskIds model) projectTasks
                )

          else
            text ""
        ]


viewProjectNextTasksPanel : CardTreeProjection -> Model -> Api.Project -> Html Msg
viewProjectNextTasksPanel projection model project =
    let
        candidates =
            Dict.get project.id model.cards.projectNextTasks

        blockedDiagnostics =
            Dict.get project.id model.cards.projectNextTaskDiagnostics
                |> Maybe.withDefault []
                |> List.filter isBlockedNextTaskCandidate

        loading =
            Dict.get project.id model.cards.projectNextTasksLoading |> Maybe.withDefault False

        diagnosticsLoading =
            Dict.get project.id model.cards.projectNextTaskDiagnosticsLoading |> Maybe.withDefault False

        errorMessage =
            Dict.get project.id model.cards.projectNextTasksErrors

        diagnosticsErrorMessage =
            Dict.get project.id model.cards.projectNextTaskDiagnosticsErrors

        waitingSubtasks =
            waitingSubtasksForProject projection project

        candidateViews =
            candidates
                |> Maybe.withDefault []
                |> List.map (viewNextTaskCandidate model)

        blockedDiagnosticViews =
            blockedDiagnostics
                |> List.map (viewNextTaskCandidate model)
    in
    div [ class "project-next-tasks-section" ]
        [ div [ class "project-next-tasks-header" ]
            [ div []
                [ div [ class "project-next-tasks-title" ] [ text "Next tasks" ]
                , div [ class "project-next-tasks-subtitle" ]
                    [ text "Project priority first, then task priority; blocked diagnostics are separated below." ]
                ]
            , button
                [ class "btn-inline-create"
                , disabled loading
                , onClick (RefreshProjectNextTasks project.id)
                ]
                [ text (if loading then "Loading…" else "Refresh") ]
            ]
        , case errorMessage of
            Just message ->
                div [ class "project-next-tasks-error" ] [ text message ]

            Nothing ->
                text ""
        , case diagnosticsErrorMessage of
            Just message ->
                div [ class "project-next-tasks-error" ] [ text message ]

            Nothing ->
                text ""
        , case candidates of
            Nothing ->
                div [ class "project-next-tasks-empty" ]
                    [ text
                        (if loading then
                            "Loading next tasks…"

                         else
                            "Expand or refresh to load next tasks."
                        )
                    ]

            Just [] ->
                div [ class "project-next-tasks-empty" ]
                    [ text (noReadyNextTaskMessage waitingSubtasks blockedDiagnostics diagnosticsLoading) ]

            Just _ ->
                div [ class "project-next-tasks-list" ] candidateViews
        , if not (List.isEmpty blockedDiagnostics) then
            div [ class "project-next-tasks-diagnostics" ]
                [ div [ class "project-next-tasks-diagnostics-title" ] [ text "Blocked diagnostics" ]
                , div [ class "project-next-tasks-list" ] blockedDiagnosticViews
                ]

          else
            text ""
        , viewWaitingSubtasksNote waitingSubtasks
        ]


type alias NextTaskCardAction =
    { label : String
    , title : String
    , msg : Msg
    }


nextTaskCardActions : Api.NextTaskCandidate -> List NextTaskCardAction
nextTaskCardActions candidate =
    [ { label = "Jump"
      , title = "Jump to task"
      , msg = FocusEntity "task" candidate.task.id
      }
    ]


viewNextTaskCandidate : Model -> Api.NextTaskCandidate -> Html Msg
viewNextTaskCandidate model candidate =
    let
        task =
            candidate.task

        crumbs =
            Feature.Focus.buildTaskBreadcrumb model task []
                |> List.filter (\( eid, _, _ ) -> eid /= task.id)
    in
    div [ class ("next-task-candidate popover-card " ++ nextTaskCandidateClass candidate) ]
        [ div [ class "popover-card-header" ]
            [ span [ class ("entity-type-label " ++ (if task.parentId /= Nothing then "entity-type-subtask" else "entity-type-task")) ]
                [ text
                    (if task.parentId /= Nothing then
                        "SUB"

                     else
                        "TSK"
                    )
                ]
            , span [ class "popover-card-title" ] [ text task.title ]
            , div [ class "dep-item-actions" ]
                (List.map viewNextTaskCardAction (nextTaskCardActions candidate))
            ]
        , if not (List.isEmpty crumbs) then
            div [ class "popover-card-breadcrumb" ]
                (List.intersperse (span [] [ text " › " ])
                    (List.map (\( _, label, _ ) -> span [] [ text label ]) crumbs)
                )

          else
            text ""
        , div [ class "popover-card-meta" ]
            [ span [ class (taskPopoverStatusClass task.status), title (taskStatusTitle task.status) ]
                [ text (taskStatusDisplayText task.status) ]
            , span [ class "popover-card-priority" ] [ text ("P" ++ String.fromInt task.priority) ]
            ]
        , div [ class "next-task-rationale" ] [ text (nextTaskRationale candidate) ]
        ]


viewNextTaskCardAction : NextTaskCardAction -> Html Msg
viewNextTaskCardAction action =
    button
        [ class "btn-inline-create btn-jump"
        , onClick action.msg
        , title action.title
        ]
        [ text action.label ]


nextTaskCandidateClass : Api.NextTaskCandidate -> String
nextTaskCandidateClass candidate =
    if candidate.dependencyBlocked then
        "next-task-blocked"

    else if candidate.task.status == Api.Blocked then
        "next-task-blocked"

    else if candidate.completionGated then
        "next-task-completion-gated"

    else
        "next-task-ready"


isBlockedNextTaskCandidate : Api.NextTaskCandidate -> Bool
isBlockedNextTaskCandidate candidate =
    candidate.dependencyBlocked || candidate.task.status == Api.Blocked



nextTaskRationale : Api.NextTaskCandidate -> String
nextTaskRationale candidate =
    if candidate.dependencyBlocked then
        "Dependency-blocked by " ++ countPhrase candidate.openDependencyCount "open dependency" "open dependencies" ++ "."

    else if candidate.task.status == Api.Blocked then
        "Blocked; no open dependency is counted by the next-task query."

    else if candidate.completionGated then
        "Ready now; completion is gated by " ++ countPhrase candidate.openDescendantCount "open subtask" "open subtasks" ++ "."

    else if candidate.task.parentId /= Nothing then
        "Ready subtask: parent is already in progress."

    else
        "Ready to start."


waitingSubtasksForProject : CardTreeProjection -> Api.Project -> List Api.Task
waitingSubtasksForProject projection project =
    let
        parentNotInProgress parentId =
            Dict.get parentId projection.tasksById
                |> Maybe.map (\parent -> parent.status /= Api.InProgress)
                |> Maybe.withDefault True
    in
    (Dict.get project.id projection.projectTasks |> Maybe.withDefault [])
        |> List.filter
            (\task ->
                isOpenTaskStatus task.status
                    && (case task.parentId of
                            Just parentId ->
                                parentNotInProgress parentId

                            Nothing ->
                                False
                       )
            )
        |> List.sortBy (\task -> ( negate task.priority, String.toLower task.title ))


taskInProjectTree : List String -> Dict.Dict String Api.Task -> Api.Task -> Bool
taskInProjectTree projectIds tasksById task =
    let
        directlyInProjectTree =
            task.projectId
                |> Maybe.map (\projectId -> List.member projectId projectIds)
                |> Maybe.withDefault False
    in
    if directlyInProjectTree then
        True

    else
        case task.parentId |> Maybe.andThen (\parentId -> Dict.get parentId tasksById) of
            Just parent ->
                taskInProjectTree projectIds tasksById parent

            Nothing ->
                False


viewWaitingSubtasksNote : List Api.Task -> Html Msg
viewWaitingSubtasksNote waitingSubtasks =
    if List.isEmpty waitingSubtasks then
        text ""

    else
        let
            shown =
                waitingSubtasks
                    |> List.take 3
                    |> List.map .title

            suffix =
                if List.length waitingSubtasks > 3 then
                    " and " ++ String.fromInt (List.length waitingSubtasks - 3) ++ " more"

                else
                    ""
        in
        div [ class "project-next-tasks-note" ]
            [ text
                (countPhrase (List.length waitingSubtasks) "subtask" "subtasks"
                    ++ " waiting for parent to be in progress before other blockers are evaluated: "
                    ++ String.join ", " shown
                    ++ suffix
                    ++ "."
                )
            ]


noReadyNextTaskMessage : List Api.Task -> List Api.NextTaskCandidate -> Bool -> String
noReadyNextTaskMessage waitingSubtasks blockedDiagnostics diagnosticsLoading =
    if diagnosticsLoading then
        "No ready tasks found. Checking blocked diagnostics…"

    else if not (List.isEmpty blockedDiagnostics) then
        "No ready tasks found. Blocked candidates are listed below with their dependency/manual-blocking rationale."

    else if List.isEmpty waitingSubtasks then
        "No ready or blocked task candidates found for this project. Completed and cancelled work is hidden."

    else
        "No ready tasks found. Start parent tasks, then resolve any remaining blockers, to make waiting subtasks actionable."


viewFocusedTaskNode : CardTreeProjection -> Model -> Api.Task -> Html Msg
viewFocusedTaskNode projection model task =
    div [ class "tree-node" ]
        [ viewTaskCard projection False model task
        ]


viewTaskCard : CardTreeProjection -> Bool -> Model -> Api.Task -> Html Msg
viewTaskCard projection showProject model task =
    viewTaskCardBody True projection showProject model task


viewTaskCardBody : Bool -> CardTreeProjection -> Bool -> Model -> Api.Task -> Html Msg
viewTaskCardBody descendants projection showProject model task =
    let
        projectName =
            task.projectId
                |> Maybe.andThen (\pid -> Dict.get pid model.projects)
                |> Maybe.map .name

        hasChildren =
            (Dict.get task.id model.dataLoading.taskCardSummaries
                |> Maybe.map .hasChildren
                |> Maybe.withDefault False
            )
                || not (List.isEmpty (Dict.get task.id projection.taskChildren |> Maybe.withDefault []))

        collapsed =
            isCollapsed model ("task-" ++ task.id)

        isSubtask =
            task.parentId /= Nothing

        parentTask =
            task.parentId
                |> Maybe.andThen (\parentId -> Dict.get parentId model.tasks)

        typeLabel =
            if isSubtask then
                "SUB"

            else
                "TSK"

        typeClass =
            if isSubtask then
                "entity-type-subtask"

            else
                "entity-type-task"

        cardClass =
            if isSubtask then
                "card-subtask"

            else
                "card-task"

        childTasksForTask =
            Dict.get task.id projection.taskChildren
                |> Maybe.withDefault []

        maybeRollup =
            case Dict.get task.id projection.taskRollups of
                Just rollup -> Just rollup
                Nothing -> Dict.get task.id model.dataLoading.taskCardSummaries |> Maybe.map .readinessRollup

        openDependencyCount =
            maybeRollup
                |> Maybe.map .openDependencyCount
                |> Maybe.withDefault 0

        directOpenDependencyCount =
            Dict.get task.id projection.taskDirectOpenDependencyCounts
                |> Maybe.withDefault 0

        dependencyBlockedForStatusOptions =
            directOpenDependencyCount > 0

        taskStatusOptions =
            taskStatusOptionsForTask dependencyBlockedForStatusOptions task

        query =
            String.toLower (String.trim model.search.query)

        hasSearch =
            not (String.isEmpty query)

        hasTreeCriteria =
            treeCriteriaActive query model

        applyLocalTreeCriteria =
            hasTreeCriteria && not model.dataLoading.navigationVisibilityActive

        taskPassesFilters =
            taskPassesCurrentFilters model

        shownForMatchingSubtask =
            applyLocalTreeCriteria
                && Set.member task.id projection.taskCriteriaMatches
                && not (taskMatchesActiveCriteria query hasSearch taskPassesFilters task)

        openDescendantTaskCount =
            maybeRollup |> Maybe.map .openSubtaskCount |> Maybe.withDefault 0

        completionBlockerReason =
            taskCompletionBlockerReason openDescendantTaskCount

        taskStatusDisabled status =
            taskStatusOptionDisabledReason completionBlockerReason isSubtask (Maybe.map .status parentTask) status

        taskProjectAllowsOpenTasks =
            task.projectId
                |> Maybe.andThen (\projectId -> Dict.get projectId projection.projectAllowsOpenChildren)
                |> Maybe.withDefault True

        taskHasCompletedAncestor =
            Dict.get task.id projection.taskHasClosedAncestor
                |> Maybe.withDefault False

        taskAllowsOpenChildren =
            not isSubtask && task.status /= Api.Done && task.status /= Api.Cancelled && not taskHasCompletedAncestor && taskProjectAllowsOpenTasks

        taskCreateChildReason =
            if isSubtask then
                "Subtasks cannot have subtasks."

            else if task.status == Api.Done || task.status == Api.Cancelled then
                "Reopen this task before adding open subtasks."

            else if taskHasCompletedAncestor then
                "Reopen the parent task before adding open subtasks."

            else
                "Reopen the project before adding open subtasks."
    in
    div
        ([ class ("card tree-card " ++ cardClass ++ taskCardStatusClass task.status ++ Feature.DragDrop.dragOverClass model task.id)
        , draggable (if Permissions.canEditCurrentWorkspace model then "true" else "false")
        , id ("entity-" ++ task.id)
        , on "dragstart" (Decode.succeed (DragStartCard "task" task.id))
        , preventDefaultOn "dragover" (Decode.succeed ( DragOverCard task.id, True ))
        , preventDefaultOn "drop" (Decode.succeed ( DropOnCard "task" task.id, True ))
        , on "dragend" (Decode.succeed DragEndCard)
        ]
        ++ (if not isSubtask then
                [ onFocusClick "task" task.id ]
            else
                []
           )
        )
        [ div [ class "card-header" ]
            [ div [ class "tree-toggle-row" ]
                [ if hasChildren then
                    button [ class "tree-toggle", onClick (ToggleTreeNode ("task-" ++ task.id)) ]
                        [ text
                            (if collapsed then
                                "▶"

                             else
                                "▼"
                            )
                        ]

                  else
                    span [ class "tree-toggle-spacer" ] []
                , span [ class ("entity-type-label " ++ typeClass) ] [ text typeLabel ]
                , Feature.Editing.viewEditableText model "task" task.id "title" task.title
                ]
            , div [ class "card-actions" ]
                [ Feature.Editing.viewStatusSelectWithDisabled model "task" task.id (Api.taskStatusToString task.status) taskStatusOptions Api.taskStatusToString taskStatusDisabled ChangeTaskStatus
                , Feature.Editing.viewPrioritySelect model "task" task.id task.priority ChangeTaskPriority
                , if Permissions.canEditCurrentWorkspace model then
                    button [ class "btn-icon btn-danger", onClick (ConfirmDelete "task" task.id), title "Delete" ] [ text "✕" ]

                  else
                    text ""
                ]
            ]
        , let
            childTasks =
                childTasksForTask

            remainingSubtasks =
                case maybeRollup of
                    Just rollup -> rollup.openSubtaskCount
                    Nothing -> if descendants then childTasks |> List.filter (\t -> t.status == Api.Todo || t.status == Api.InProgress || t.status == Api.Blocked) |> List.length else 0

            completedSubtasks =
                case maybeRollup of
                    Just rollup -> rollup.doneSubtaskCount + rollup.cancelledSubtaskCount
                    Nothing -> if descendants then List.length childTasks - remainingSubtasks else 0

            depCount =
                task.dependencyCount

            subtaskLabel =
                let
                    total =
                        remainingSubtasks + completedSubtasks
                in
                if total == 0 then
                    Nothing

                else if completedSubtasks == 0 then
                    Just (String.fromInt total ++ " subtask" ++ (if total > 1 then "s" else ""))

                else if remainingSubtasks == 0 then
                    Just (String.fromInt total ++ " subtask" ++ (if total > 1 then "s" else "") ++ " (all done)")

                else
                    Just (String.fromInt remainingSubtasks ++ "/" ++ String.fromInt total ++ " subtasks remaining")

            summaryParts =
                List.filterMap identity
                    [ subtaskLabel
                    , if openDependencyCount > 0 then
                        Just (String.fromInt openDependencyCount ++ " open dep" ++ (if openDependencyCount > 1 then "s" else ""))

                      else if depCount > 0 then
                        Just (String.fromInt depCount ++ " dep" ++ (if depCount > 1 then "s" else ""))

                      else
                        Nothing
                    ]
          in
          if List.isEmpty summaryParts then
            text ""

          else
            div [ class "card-summary" ] [ text (String.join " · " summaryParts) ]
        , if shownForMatchingSubtask then
            div [ class "card-filter-context-note" ] [ text "Shown because a subtask matches the current filters." ]

          else
            text ""
        , let
            deps =
                if isExpanded model task.id then taskDependencySummariesForTask model task.id else []

            extrasExpanded =
                isExpanded model task.id
          in
          div [ class "card-body card-expanded" ]
            [ div [ class "card-desc-row" ]
                [ button [ class "btn-extras-toggle", onClick (ToggleCardExpand task.id) ]
                    [ text
                        (if extrasExpanded then
                            "−"

                         else
                            "+"
                        )
                    ]
                , viewCardDescription model "task" task.id task.description
                ]
            , if extrasExpanded then
                div [ class "card-extras" ]
                    [ if showProject then
                        case projectName of
                            Just pname ->
                                div [ class "card-meta" ] [ text ("Project: " ++ pname) ]

                            Nothing ->
                                text ""

                      else
                        text ""
                    , Feature.Dependencies.viewTaskDependencies model task.id deps
                    , Feature.AuditLog.viewEntityHistory model "task" task.id
                    ]

              else
                text ""
            ]
        , div [ class "card-inline-actions" ]
            [ if Permissions.canEditCurrentWorkspace model then
                button
                    [ class "btn-inline-create"
                    , disabled (not taskAllowsOpenChildren)
                    , title (if taskAllowsOpenChildren then "Add subtask" else taskCreateChildReason)
                    , onClick (ShowInlineCreate (InlineCreateTask { projectId = task.projectId, parentId = Just task.id, title = "" }))
                    ]
                    [ text "+ Subtask" ]

              else
                text ""
            ]
        , Feature.Editing.viewInlineCreateInputForParent model (Just task.id) "subtask"
        , div [ class "card-meta-group" ]
            [ div [ class "card-meta-row" ]
                [ span [ class "card-meta" ] [ text ("Created: " ++ formatDate task.createdAt) ]
                , span [ class "card-meta" ] [ text ("Updated: " ++ formatDate task.updatedAt) ]
                , Helpers.copyableValue "card-meta card-id card-id-copy" "task ID" task.id task.id
                ]
            , div [ class "card-meta-row" ]
                [ case task.dueAt of
                    Just due ->
                        span [ class "card-meta card-meta-due" ] [ text ("Due: " ++ formatDate due) ]

                    Nothing ->
                        text ""
                , case task.completedAt of
                    Just completed ->
                        span [ class "card-meta" ] [ text ("Completed: " ++ formatDate completed) ]

                    Nothing ->
                        text ""
                ]
            ]
        , if descendants && hasChildren && not collapsed then
            let
                childTasks =
                    childTasksForTask
                        |> (if applyLocalTreeCriteria then
                                List.filter (\child -> Set.member child.id projection.taskCriteriaMatches)

                            else
                                identity
                           )
                        |> List.sortBy (\t -> ( Api.taskStatusOrder t.status, negate t.priority, String.toLower t.title ))
                        |> branchPresentationWindow "task" task.id "task" .id (pinnedTaskIds model) model
            in
            Keyed.node "div" [ class "tree-children" ]
                (viewTasksWithZones projection model "task-subtasks" task.projectId (Just task.id) childTasks
                    ++ viewBranchLoadMore model "task" task.id "task" .id (pinnedTaskIds model) childTasksForTask
                )

          else
            text ""
        ]


{-| A branch page is loaded only after an explicit user action.  The server
caps every response at 100, while the browser asks for 50 to keep the rendered
tree bounded for the next task. -}
viewBranchLoadMore : Model -> String -> String -> String -> (a -> String) -> Set.Set String -> List a -> List ( String, Html Msg )
viewBranchLoadMore model parentKind parentId entityKind identify pinned cachedValues =
    let
        state =
            Dict.get (parentKind ++ ":" ++ parentId) model.dataLoading.loadedNavigationBranches

        transportHasMore =
            state
                |> Maybe.map
                    (\value ->
                        if entityKind == "project" then
                            value.projectHasMore

                        else
                            value.taskHasMore
                    )
                |> Maybe.withDefault False

        presentationOffset =
            Dict.get (parentKind ++ ":" ++ parentId) model.dataLoading.navigationPresentations
                |> Maybe.map
                    (\value -> if entityKind == "project" then value.projectOffset else value.taskOffset)
                |> Maybe.withDefault 0

        ordinaryCachedCount =
            cachedValues
                |> List.filter (identify >> (\entityId -> not (Set.member entityId pinned)))
                |> List.length

        ordinaryCapacity =
            presentationOrdinaryCapacity (List.length cachedValues - ordinaryCachedCount)

        hasMore =
            transportHasMore
                || presentationOffset + ordinaryCapacity < ordinaryCachedCount
                || (state |> Maybe.map (.succeeded >> not) |> Maybe.withDefault False)

        loading =
            state |> Maybe.map .inFlight |> Maybe.withDefault False
    in
    (if presentationOffset > 0 then
        [ ( "show-previous-" ++ entityKind ++ "-" ++ parentId
          , button
                [ class "navigation-load-more"
                , onClick (ShowPreviousNavigationBranchPage parentKind parentId entityKind)
                ]
                [ text ("Show previous " ++ entityKind ++ "s") ]
          )
        ]

     else
        []
    )
        ++ (if hasMore || loading then
                [ ( "load-more-" ++ entityKind ++ "-" ++ parentId
           , button
                [ class "navigation-load-more"
                , disabled loading
                , onClick (LoadNavigationBranchPage parentKind parentId entityKind)
                ]
                [ text
                    (if loading then
                        "Loading more…"

                     else if entityKind == "project" then
                        "Load more projects"

                     else
                        "Load more tasks"
                    )
                ]
                  )
                ]

            else
                []
           )


editTarget : Model -> Maybe ( String, String )
editTarget model =
    case model.editing.editState of
        Just (EditingField state) ->
            Just ( state.entityType, state.entityId )

        Nothing ->
            Nothing


taskPathIds : Model -> String -> Set.Set String
taskPathIds model taskId =
    let
        climb current seen =
            if Set.member current seen then
                seen

            else
                case Dict.get current model.tasks of
                    Just task ->
                        case task.parentId of
                            Just parentId ->
                                climb parentId (Set.insert current seen)

                            Nothing ->
                                Set.insert current seen

                    Nothing ->
                        seen
    in
    climb taskId Set.empty


projectPathIds : Model -> String -> Set.Set String
projectPathIds model projectId =
    let
        climb current seen =
            if Set.member current seen then
                seen

            else
                case Dict.get current model.projects of
                    Just project ->
                        case project.parentId of
                            Just parentId ->
                                climb parentId (Set.insert current seen)

                            Nothing ->
                                Set.insert current seen

                    Nothing ->
                        seen
    in
    climb projectId Set.empty


pinnedTaskIds : Model -> Set.Set String
pinnedTaskIds model =
    let
        focused =
            model.focus.focusedEntity
                |> Maybe.andThen (\( kind, entityId ) -> if kind == "task" then Just entityId else Nothing)

        edited =
            editTarget model
                |> Maybe.andThen (\( kind, entityId ) -> if kind == "task" then Just entityId else Nothing)

        inlineParent =
            case model.editing.inlineCreate of
                Just (InlineCreateTask state) ->
                    state.parentId

                _ ->
                    Nothing
    in
    [ focused, edited, inlineParent ]
        |> List.filterMap identity
        |> List.foldl (\entityId pins -> Set.union pins (taskPathIds model entityId)) Set.empty


pinnedProjectIds : Model -> Set.Set String
pinnedProjectIds model =
    let
        focusedProject =
            model.focus.focusedEntity
                |> Maybe.andThen (\( kind, entityId ) -> if kind == "project" then Just entityId else Nothing)

        editedProject =
            editTarget model
                |> Maybe.andThen (\( kind, entityId ) -> if kind == "project" then Just entityId else Nothing)

        inlineProject =
            case model.editing.inlineCreate of
                Just (InlineCreateProject state) ->
                    state.parentId

                Just (InlineCreateTask state) ->
                    state.projectId

                _ ->
                    Nothing

        taskProjects =
            pinnedTaskIds model
                |> Set.toList
                |> List.filterMap (\taskId -> Dict.get taskId model.tasks |> Maybe.andThen .projectId)
    in
    focusedProject :: editedProject :: inlineProject :: List.map Just taskProjects
        |> List.filterMap identity
        |> List.foldl (\projectId pins -> Set.union pins (projectPathIds model projectId)) Set.empty


{-| Data pages stay cached for interaction and drag/drop, but one card
presentation window is capped at 25 items so repeatedly loading navigation
pages cannot grow the mounted DOM without bound. Pinned focus/edit paths stay
mounted while the remainder of the window remains reversible. -}
presentationWindow : (a -> String) -> Set.Set String -> Int -> List a -> List a
presentationWindow identify pinned offset values =
    navigationPresentationWindow identify pinned offset values


rootPresentationWindow : String -> (a -> String) -> Set.Set String -> Model -> List a -> List a
rootPresentationWindow entityKind identify pinned model values =
    let
        offset =
            model.dataLoading.rootNavigationPresentation
                |> Maybe.map
                    (\state ->
                        if entityKind == "project" then
                            state.projectOffset

                        else
                            state.taskOffset
                    )
                |> Maybe.withDefault 0
    in
    presentationWindow identify pinned offset values


branchPresentationWindow : String -> String -> String -> (a -> String) -> Set.Set String -> Model -> List a -> List a
branchPresentationWindow parentKind parentId entityKind identify pinned model values =
    let
        offset =
            Dict.get (parentKind ++ ":" ++ parentId) model.dataLoading.navigationPresentations
                |> Maybe.map
                    (\state ->
                        if entityKind == "project" then
                            state.projectOffset

                        else
                            state.taskOffset
                    )
                |> Maybe.withDefault 0
    in
    presentationWindow identify pinned offset values


viewRootLoadMore : Model -> String -> (a -> String) -> Set.Set String -> List a -> List ( String, Html Msg )
viewRootLoadMore model entityKind identify pinned cachedValues =
    let
        root =
            model.dataLoading.rootNavigationRequest

        transportHasMore =
            root
                |> Maybe.map
                    (\state ->
                        if entityKind == "project" then
                            state.projectHasMore

                        else
                            state.taskHasMore
                    )
                |> Maybe.withDefault False

        presentationOffset =
            model.dataLoading.rootNavigationPresentation
                |> Maybe.map
                    (\state ->
                        if entityKind == "project" then state.projectOffset else state.taskOffset
                    )
                |> Maybe.withDefault 0

        ordinaryCachedCount =
            cachedValues
                |> List.filter (identify >> (\entityId -> not (Set.member entityId pinned)))
                |> List.length

        ordinaryCapacity =
            presentationOrdinaryCapacity (List.length cachedValues - ordinaryCachedCount)

        hasMore =
            transportHasMore
                || presentationOffset + ordinaryCapacity < ordinaryCachedCount
                || (root |> Maybe.map (.succeeded >> not) |> Maybe.withDefault False)

        loading =
            root |> Maybe.map .inFlight |> Maybe.withDefault False
    in
    (if presentationOffset > 0 then
        [ ( "root-show-previous-" ++ entityKind
          , button [ class "navigation-load-more", onClick (ShowPreviousRootNavigationPage entityKind) ] [ text ("Show previous " ++ entityKind ++ "s") ]
          )
        ]

     else
        []
    )
        ++ (if hasMore || loading then
                [ ( "root-load-more-" ++ entityKind
           , button
                [ class "navigation-load-more"
                , disabled loading
                , onClick (LoadRootNavigationPage entityKind)
                ]
                [ text
                    (if loading then
                        "Loading..."

                     else
                        "Load more " ++ entityKind ++ "s"
                    )
                ]
                  )
                ]

            else
                []
           )


viewDeleteConfirmModal : Model -> Html Msg
viewDeleteConfirmModal model =
    case model.cards.deleteConfirmation of
        Nothing ->
            text ""

        Just confirmation ->
            let
                entityType =
                    confirmation.entityType

                entityId =
                    confirmation.entityId

                entityName =
                    case entityType of
                        "project" ->
                            Dict.get entityId model.projects |> Maybe.map .name |> Maybe.withDefault "this project"

                        "task" ->
                            Dict.get entityId model.tasks |> Maybe.map .title |> Maybe.withDefault "this task"

                        "memory" ->
                            Dict.get entityId model.memories
                                |> Maybe.map (\m -> Maybe.withDefault (truncateText 50 m.content) m.summary)
                                |> Maybe.withDefault "this memory"

                        "group" ->
                            Dict.get entityId model.groups.workspaceGroups |> Maybe.map .name |> Maybe.withDefault "this group"

                        _ ->
                            "this item"

                typeLabel =
                    case entityType of
                        "project" ->
                            "project"

                        "task" ->
                            Dict.get entityId model.tasks
                                |> Maybe.andThen (\t -> t.parentId)
                                |> Maybe.map (\_ -> "subtask")
                                |> Maybe.withDefault "task"

                        "memory" ->
                            "memory"

                        "group" ->
                            "group"

                        _ ->
                            "item"

                modalTitle =
                    case entityType of
                        "project" ->
                            "Delete project subtree?"

                        "task" ->
                            "Delete task cascade?"

                        _ ->
                            "Delete " ++ typeLabel ++ "?"

                describedBy =
                    if isCascadeDelete confirmation then
                        "delete-confirm-desc delete-confirm-warning"

                    else
                        "delete-confirm-desc"
            in
            div [ class "modal-overlay", onClick CancelDelete ]
                [ div
                    [ class "modal delete-confirm-modal"
                    , attribute "role" "dialog"
                    , attribute "aria-modal" "true"
                    , attribute "aria-labelledby" "delete-confirm-title"
                    , attribute "aria-describedby" describedBy
                    , stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
                    ]
                    [ h3 [ class "modal-title", id "delete-confirm-title" ] [ text modalTitle ]
                    , p [ class "delete-confirm-desc", id "delete-confirm-desc" ]
                        [ text "Are you sure you want to delete "
                        , strong [] [ text (truncateText 60 entityName) ]
                        , text "? This action cannot be undone."
                        ]
                    , viewCascadeDeleteWarning confirmation
                    , div [ class "modal-actions" ]
                        [ button [ class "btn btn-danger", onClick PerformDelete ] [ text (deleteButtonLabel confirmation) ]
                        , button [ class "btn btn-secondary", id "delete-confirm-cancel", onClick CancelDelete ] [ text "Cancel" ]
                        ]
                    ]
                ]


viewCascadeDeleteWarning : DeleteConfirmation -> Html Msg
viewCascadeDeleteWarning confirmation =
    if isCascadeDelete confirmation then
        div [ class "delete-cascade-warning", id "delete-confirm-warning", attribute "role" "alert" ]
            [ strong [] [ text "Cascade delete warning" ]
            , p [] [ text (cascadeDeleteWarningText confirmation) ]
            , p [ class "delete-cascade-warning-note" ]
                [ text "The server will verify the tree again when you confirm. If anything changed, the final delete result will show the updated counts." ]
            ]

    else
        text ""


isCascadeDelete : DeleteConfirmation -> Bool
isCascadeDelete confirmation =
    confirmation.entityType == "project" || confirmation.entityType == "task"


cascadeDeleteWarningText : DeleteConfirmation -> String
cascadeDeleteWarningText confirmation =
    case ( confirmation.entityType, confirmation.preview ) of
        ( "task", Just preview ) ->
            let
                subtaskCount =
                    Basics.max 0 (preview.taskCount - 1)
            in
            if subtaskCount == 0 then
                "Loaded preview: only this task is shown in the delete tree. This is still a cascading task delete, not a simple row removal."

            else
                "Loaded preview: this task and " ++ countPhrase subtaskCount "subtask" "subtasks" ++ " will be deleted (" ++ countPhrase preview.taskCount "task" "tasks" ++ " total). This is not a single-row delete."

        ( "project", Just preview ) ->
            let
                subprojectCount =
                    Basics.max 0 (preview.projectCount - 1)

                descendantParts =
                    List.filterMap identity
                        [ if subprojectCount > 0 then
                            Just (countPhrase subprojectCount "subproject" "subprojects")

                          else
                            Nothing
                        , if preview.taskCount > 0 then
                            Just (countPhrase preview.taskCount "task" "tasks")

                          else
                            Nothing
                        ]
            in
            if subprojectCount == 0 && preview.taskCount == 0 then
                "Loaded preview: only this project is shown in the delete tree. This is still a cascading project-subtree delete, not a simple row removal."

            else
                "Loaded preview: this project and " ++ joinHuman descendantParts ++ " will be deleted (" ++ String.fromInt preview.affected ++ " affected project/task items). This is not a single-row delete."

        ( "task", Nothing ) ->
            "The current subtask preview is unavailable. Confirming will still perform a cascading task delete and then report the final server counts."

        ( "project", Nothing ) ->
            "The current project-subtree preview is unavailable. Confirming will still perform a cascading project delete and then report the final server counts."

        _ ->
            ""


deleteButtonLabel : DeleteConfirmation -> String
deleteButtonLabel confirmation =
    case ( confirmation.entityType, confirmation.preview ) of
        ( "task", Just preview ) ->
            if preview.taskCount > 1 then
                "Delete " ++ countPhrase preview.taskCount "task" "tasks"

            else
                "Delete task"

        ( "project", Just preview ) ->
            if preview.affected > 1 then
                "Delete " ++ cascadePreviewCountsText preview

            else
                "Delete project"

        ( "task", Nothing ) ->
            "Delete task cascade"

        ( "project", Nothing ) ->
            "Delete project subtree"

        _ ->
            "Delete"



-- INTERNAL HELPERS


viewDropZone : Model -> DropZoneInfo -> Html Msg
viewDropZone model zone =
    case model.dragDrop.dragging of
        Nothing ->
            text ""

        Just drag ->
            let
                relevant =
                    case ( drag.entityType, zone.parentType ) of
                        ( "task", "project-tasks" ) ->
                            True

                        ( "task", "task-subtasks" ) ->
                            case ( Dict.get drag.entityId model.tasks, zone.parentId |> Maybe.andThen (\parentId -> Dict.get parentId model.tasks) ) of
                                ( Just dragTask, Just targetTask ) ->
                                    Feature.DragDrop.canMakeSubtask model dragTask targetTask

                                _ ->
                                    False

                        ( "task", "orphan" ) ->
                            True

                        ( "project", "project" ) ->
                            True

                        _ ->
                            False

                isActive =
                    case model.dragDrop.dragOver of
                        Just (OverZone z) ->
                            z == zone

                        _ ->
                            False
            in
            if relevant then
                div
                    [ class
                        (if isActive then
                            "drop-zone drop-zone-active"

                         else
                            "drop-zone"
                        )
                    , preventDefaultOn "dragover" (Decode.succeed ( DragOverZone zone, True ))
                    , preventDefaultOn "drop" (Decode.succeed ( DropOnZone zone, True ))
                    ]
                    []

            else
                text ""


viewTasksWithZones : CardTreeProjection -> Model -> String -> Maybe String -> Maybe String -> List Api.Task -> List ( String, Html Msg )
viewTasksWithZones projection model zoneType projectId parentTaskId tasks =
    case model.dragDrop.dragging of
        Nothing ->
            List.map (\t -> ( t.id, viewTaskCard projection False model t )) tasks

        Just _ ->
            let
                makeZone abovePri belowPri =
                    { parentType = zoneType
                    , parentId = parentTaskId
                    , projectId = projectId
                    , abovePriority = abovePri
                    , belowPriority = belowPri
                    }

                go remaining idx prevPri =
                    case remaining of
                        [] ->
                            [ ( "dz-" ++ zoneType ++ "-end", viewDropZone model (makeZone prevPri Nothing) ) ]

                        t :: rest ->
                            ( "dz-" ++ zoneType ++ "-" ++ String.fromInt idx, viewDropZone model (makeZone prevPri (Just t.priority)) )
                                :: ( t.id, viewTaskCard projection False model t )
                                :: go rest (idx + 1) (Just t.priority)
            in
            go tasks 0 Nothing


viewProjectsWithZones : Model -> (Api.Project -> Html Msg) -> Maybe String -> List Api.Project -> List ( String, Html Msg )
viewProjectsWithZones model renderProject parentId projects =
    case model.dragDrop.dragging of
        Nothing ->
            List.map (\p -> ( p.id, renderProject p )) projects

        Just _ ->
            let
                makeZone abovePri belowPri =
                    { parentType = "project"
                    , parentId = parentId
                    , projectId = Nothing
                    , abovePriority = abovePri
                    , belowPriority = belowPri
                    }

                go remaining idx prevPri =
                    case remaining of
                        [] ->
                            [ ( "dz-proj-end", viewDropZone model (makeZone prevPri Nothing) ) ]

                        p :: rest ->
                            ( "dz-proj-" ++ String.fromInt idx, viewDropZone model (makeZone prevPri (Just p.priority)) )
                                :: ( p.id, renderProject p )
                                :: go rest (idx + 1) (Just p.priority)
            in
            go projects 0 Nothing


treeCriteriaActive : String -> Model -> Bool
treeCriteriaActive query model =
    not (String.isEmpty query)
        || not model.search.filterShowEmptyProjects
        || model.search.filterShowOnly /= ShowAll
        || model.search.filterPriority /= AnyPriority
        || not (List.isEmpty model.search.filterProjectStatuses)
        || not (List.isEmpty model.search.filterTaskStatuses)


projectPassesStatusFilter : Model -> Api.Project -> Bool
projectPassesStatusFilter model project =
    passesStatusFilter model.search.filterProjectStatuses (Api.projectStatusToString project.status)


projectPassesCurrentFilters : Model -> Api.Project -> Bool
projectPassesCurrentFilters model project =
    (model.search.filterShowOnly /= ShowTasksOnly)
        && projectPassesStatusFilter model project
        && passesPriorityFilter model.search.filterPriority project.priority


taskPassesCurrentFilters : Model -> Api.Task -> Bool
taskPassesCurrentFilters model task =
    (model.search.filterShowOnly /= ShowProjectsOnly)
        && passesStatusFilter model.search.filterTaskStatuses (Api.taskStatusToString task.status)
        && passesPriorityFilter model.search.filterPriority task.priority


projectMatchesActiveCriteria : String -> Bool -> (Api.Project -> Bool) -> Api.Project -> Bool
projectMatchesActiveCriteria query hasSearch projectFilter project =
    (not hasSearch || projectMatchesSearch query project)
        && projectFilter project


taskMatchesActiveCriteria : String -> Bool -> (Api.Task -> Bool) -> Api.Task -> Bool
taskMatchesActiveCriteria query hasSearch taskFilter task =
    (not hasSearch || taskMatchesSearch query task)
        && taskFilter task


projectTreeMatchesCriteria : String -> Bool -> (Api.Project -> Bool) -> List Api.Project -> List Api.Task -> (Api.Project -> Bool) -> (Api.Task -> Bool) -> Api.Project -> Bool
projectTreeMatchesCriteria query hasSearch projectStatusGate allProjects allTasks projectFilter taskFilter project =
    projectStatusGate project
        && (projectMatchesActiveCriteria query hasSearch projectFilter project
                || List.any (projectTreeMatchesCriteria query hasSearch projectStatusGate allProjects allTasks projectFilter taskFilter)
                    (List.filter (\p -> p.parentId == Just project.id) allProjects)
                || List.any (taskTreeMatchesCriteria query hasSearch allTasks taskFilter)
                    (List.filter (\t -> t.projectId == Just project.id && t.parentId == Nothing) allTasks)
           )


taskTreeMatchesCriteria : String -> Bool -> List Api.Task -> (Api.Task -> Bool) -> Api.Task -> Bool
taskTreeMatchesCriteria query hasSearch allTasks taskFilter task =
    taskMatchesActiveCriteria query hasSearch taskFilter task
        || List.any (taskTreeMatchesCriteria query hasSearch allTasks taskFilter)
            (List.filter (\t -> t.parentId == Just task.id) allTasks)


visibleTaskTreeForCriteria : String -> Bool -> (Api.Task -> Bool) -> List Api.Task -> List Api.Task -> List Api.Task
visibleTaskTreeForCriteria query hasSearch taskFilter allTasks tasks =
    List.filter (taskTreeMatchesCriteria query hasSearch allTasks taskFilter) tasks


taskShownForMatchingDescendant : String -> Bool -> (Api.Task -> Bool) -> List Api.Task -> Api.Task -> Bool
taskShownForMatchingDescendant query hasSearch taskFilter allTasks task =
    (not (taskMatchesActiveCriteria query hasSearch taskFilter task))
        && List.any (taskTreeMatchesCriteria query hasSearch allTasks taskFilter)
            (List.filter (\candidate -> candidate.parentId == Just task.id) allTasks)


projectMatchesSearch : String -> Api.Project -> Bool
projectMatchesSearch query project =
    matchesSearch query project.name
        || (project.description |> Maybe.map (matchesSearch query) |> Maybe.withDefault False)


projectTreeMatchesSearch : String -> List Api.Project -> List Api.Task -> Api.Project -> Bool
projectTreeMatchesSearch query allProjects allTasks project =
    projectMatchesSearch query project
        || List.any (projectTreeMatchesSearch query allProjects allTasks)
            (List.filter (\p -> p.parentId == Just project.id) allProjects)
        || List.any (taskMatchesSearch query)
            (List.filter (\t -> t.projectId == Just project.id) allTasks)


taskTreeMatchesSearch : String -> List Api.Task -> Api.Task -> Bool
taskTreeMatchesSearch query allTasks task =
    taskMatchesSearch query task
        || List.any (taskTreeMatchesSearch query allTasks)
            (List.filter (\t -> t.parentId == Just task.id) allTasks)


taskMatchesSearch : String -> Api.Task -> Bool
taskMatchesSearch query task =
    matchesSearch query task.title
        || (task.description |> Maybe.map (matchesSearch query) |> Maybe.withDefault False)


matchesSearch : String -> String -> Bool
matchesSearch query text_ =
    String.contains query (String.toLower text_)


projectTreePassesFilters : (Api.Project -> Bool) -> Api.Project -> List Api.Project -> List Api.Task -> (Api.Project -> Bool) -> (Api.Task -> Bool) -> Bool
projectTreePassesFilters projStatusGate project allProjects allTasks projFilter taskFilter =
    projStatusGate project
        && (projFilter project
                || List.any (\c -> projectTreePassesFilters projStatusGate c allProjects allTasks projFilter taskFilter)
                    (List.filter (\p -> p.parentId == Just project.id) allProjects)
                || List.any (\t -> taskTreePassesFilters t allTasks taskFilter)
                    (List.filter (\t -> t.projectId == Just project.id) allTasks)
           )


taskTreePassesFilters : Api.Task -> List Api.Task -> (Api.Task -> Bool) -> Bool
taskTreePassesFilters task allTasks taskFilter =
    taskFilter task
        || List.any (\t -> taskTreePassesFilters t allTasks taskFilter)
            (List.filter (\t -> t.parentId == Just task.id) allTasks)


countLabel : Int -> Int -> String -> String -> Maybe String
countLabel remaining completed noun pluralNoun =
    let
        total =
            remaining + completed
    in
    if total == 0 then
        Nothing

    else if completed == 0 then
        Just (String.fromInt total ++ " " ++ (if total > 1 then pluralNoun else noun))

    else if remaining == 0 then
        Just (String.fromInt total ++ " " ++ (if total > 1 then pluralNoun else noun) ++ " (all done)")

    else
        Just (String.fromInt remaining ++ "/" ++ String.fromInt total ++ " " ++ pluralNoun ++ " remaining")


{-| One preorder spans all projects, tasks, branch status and logical drop
boundaries. Cached transport pages never become independent DOM windows.
-}
logicalRows : CardTreeProjection -> Model -> List HierarchyRow
logicalRows projection model =
    let
        query = String.toLower (String.trim model.search.query)
        local = treeCriteriaActive query model && not model.dataLoading.navigationVisibilityActive
        projectVisible value = not local || Set.member value.id projection.projectCriteriaMatches
        taskVisible value = not local || Set.member value.id projection.taskCriteriaMatches
        row kind id depth parentKind parentId =
            { key = kind ++ ":" ++ id, kind = kind, entityId = id, depth = depth, parentKind = parentKind, parentId = parentId, zone = Nothing }
        status kind id depth =
            row "status" (kind ++ ":" ++ id) depth kind (Just id)
        branchStatus kind id depth tail =
            if branchStatusVisible model kind id then status kind id depth :: tail else tail
        boundary key depth zone =
            { key = "drop:" ++ key, kind = "drop", entityId = "", depth = depth, parentKind = zone.parentType, parentId = zone.parentId, zone = Just zone }
        zones kind parent projectId depth values identify priority render tail =
            let
                above = Nothing :: List.map (priority >> Just) values
                finalAbove = List.reverse values |> List.head |> Maybe.map priority
                ending = if model.dragDrop.dragging == Nothing then tail else
                    boundary (kind ++ ":" ++ Maybe.withDefault "root" projectId ++ ":" ++ Maybe.withDefault "root" parent ++ ":end") depth { parentType = kind, parentId = parent, projectId = projectId, abovePriority = finalAbove, belowPriority = Nothing } :: tail
                prepend ( prior, item ) rest =
                    if model.dragDrop.dragging == Nothing then render item rest else
                        boundary (kind ++ ":" ++ identify item) depth { parentType = kind, parentId = parent, projectId = projectId, abovePriority = prior, belowPriority = Just (priority item) } :: render item rest
            in
            List.map2 Tuple.pair above values |> List.foldr prepend ending
        tasks depth projectId parent visited values tail =
            zones (if parent /= Nothing then "task-subtasks" else if projectId /= Nothing then "project-tasks" else "orphan") parent projectId depth values .id .priority (task depth visited) tail
        task depth visited value tail =
            if Set.member ("task:" ++ value.id) visited then tail else
                let
                    seen = Set.insert ("task:" ++ value.id) visited
                    children = Dict.get value.id projection.taskChildren |> Maybe.withDefault [] |> List.filter taskVisible
                in
                row "task" value.id depth "task" value.parentId
                    :: (if isCollapsed model ("task-" ++ value.id) then tail else
                            tasks (depth + 1) value.projectId (Just value.id) seen children (branchStatus "task" value.id (depth + 1) tail))
        projects depth parent visited values tail =
            zones "project" parent Nothing depth values .id .priority (project depth visited) tail
        project depth visited value tail =
            if Set.member ("project:" ++ value.id) visited then tail else
                let
                    seen = Set.insert ("project:" ++ value.id) visited
                    children = Dict.get value.id projection.projectChildren |> Maybe.withDefault [] |> List.filter projectVisible
                    childrenTasks = Dict.get value.id projection.projectTasks |> Maybe.withDefault [] |> List.filter taskVisible
                in
                row "project" value.id depth "project" value.parentId
                    :: (if isCollapsed model ("proj-" ++ value.id) then tail else
                            projects (depth + 1) (Just value.id) seen children
                                (if model.search.filterShowOnly == ShowProjectsOnly then branchStatus "project" value.id (depth + 1) tail
                                 else tasks (depth + 1) (Just value.id) Nothing seen childrenTasks (branchStatus "project" value.id (depth + 1) tail)))
        roots = projection.projects |> List.filter (\p -> p.parentId == Nothing && projectVisible p)
        rootTasks = projection.tasks |> List.filter (\t -> t.parentId == Nothing && taskVisible t && (model.search.filterShowOnly == ShowTasksOnly || t.projectId == Nothing))
        rootStatus kind tail =
            if rootStatusVisible model kind then row "root-status" kind 0 "workspace_root" Nothing :: tail else tail
    in
    case model.focus.focusedEntity of
        Just ( "project", id ) -> Dict.get id model.projects |> Maybe.map (\value -> project 0 Set.empty value []) |> Maybe.withDefault []
        Just ( "task", id ) -> Dict.get id model.tasks |> Maybe.map (\value -> task 0 Set.empty value []) |> Maybe.withDefault []
        _ ->
            let
                taskRoots = if model.search.filterShowOnly == ShowProjectsOnly then [] else tasks 0 Nothing Nothing Set.empty rootTasks (rootStatus "task" [])
            in
            if model.search.filterShowOnly == ShowTasksOnly then taskRoots else projects 0 Nothing Set.empty roots (rootStatus "project" taskRoots)



viewportPins : Model -> Set.Set String
viewportPins model =
    let
        entityKey ( kind, id ) = kind ++ ":" ++ id
        drag = model.dragDrop.dragging |> Maybe.map (\value -> ( value.entityType, value.entityId ))
        viewport = model.cards.viewport
    in
    [ model.focus.focusedEntity, editTarget model, inlineCreateTarget model, drag ]
        |> List.filterMap identity |> List.map entityKey |> Set.fromList
        |> Set.union viewport.nativePins
        |> Set.union (viewport.target |> Maybe.map Set.singleton |> Maybe.withDefault Set.empty)


inlineCreateTarget : Model -> Maybe ( String, String )
inlineCreateTarget model =
    case model.editing.inlineCreate of
        Just (InlineCreateProject value) -> value.parentId |> Maybe.map (Tuple.pair "project")
        Just (InlineCreateTask value) ->
            case value.parentId of
                Just id -> Just ( "task", id )
                Nothing -> value.projectId |> Maybe.map (Tuple.pair "project")
        _ -> Nothing


mountedViewportKeys : Model -> List String
mountedViewportKeys model =
    let
        viewport = model.cards.viewport
    in
    Viewport.window viewport.top viewport.height 300 25 (viewportPins model) viewport.index
        |> List.filterMap (\piece -> case piece of
            Viewport.Row _ key _ -> Just key
            _ -> Nothing
        )


viewportDetailDemand : Model -> ( Model, Cmd Msg )
viewportDetailDemand model =
    let
        rows = mountedViewportKeys model |> List.filterMap (\key -> Dict.get key model.cards.viewport.rows)
        ids kind = rows |> List.filter (.kind >> (==) kind) |> List.map .entityId |> Set.fromList
    in
    case model.selectedWorkspaceId of
        Just ws ->
            if Set.isEmpty (ids "project") && Set.isEmpty (ids "task") && model.dataLoading.visibleDetailDemand == Nothing then ( model, Cmd.none )
            else
                let
                    pins = viewportPins model |> Set.toList |> List.filterMap (\key -> Dict.get key model.cards.viewport.rows)
                        |> List.filter (\row -> row.kind == "project" || row.kind == "task") |> List.map (\row -> ( row.kind, row.entityId )) |> Set.fromList
                in
                Feature.DataLoading.ensureViewportCardDetails ws model.sessionRequestEpoch model.dataLoading.navigationGeneration (ids "project") (ids "task") pins model
        Nothing -> ( model, Cmd.none )


refreshViewport : Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
refreshViewport previous result =
    refreshViewportWithStatusChange False previous result


refreshViewportFor : Msg -> Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
refreshViewportFor msg previous (( model, _ ) as result) =
    let
        affectedIds =
            case msg of
                CanonicalTaskFetched _ taskId _ -> [ taskId ]
                GotTaskCardDetail _ taskId _ -> [ taskId ]
                TaskCreated (Ok task) -> [ task.id ]
                TaskUpdated (Ok mutation) -> mutation.task.id :: List.map (.task >> .id) mutation.dependencyEffects
                DependencyMutationDone _ _ (Ok mutation) -> List.map (.task >> .id) mutation.affectedTasks
                GotTasks _ _ _ (Ok page) -> List.map .id page.items
                _ -> []

        statusChanged =
            List.any (\taskId -> (Dict.get taskId previous.tasks |> Maybe.map .status) /= (Dict.get taskId model.tasks |> Maybe.map .status)) affectedIds
    in
    refreshViewportWithStatusChange statusChanged previous result


refreshViewportWithStatusChange : Bool -> Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
refreshViewportWithStatusChange taskStatusesChanged previous ( model, command ) =
    let
        old = model.cards.viewport
        changed =
            old.workspaceId /= model.selectedWorkspaceId || old.sessionEpoch /= model.sessionRequestEpoch
                || old.generation /= model.dataLoading.navigationGeneration || old.projection == Nothing
                || previous.dataLoading.projectCardSummaries /= model.dataLoading.projectCardSummaries
                || previous.dataLoading.taskCardSummaries /= model.dataLoading.taskCardSummaries
                || previous.dataLoading.navigationVisibleProjectIds /= model.dataLoading.navigationVisibleProjectIds
                || previous.dataLoading.navigationVisibleTaskIds /= model.dataLoading.navigationVisibleTaskIds
                || previous.cards.collapsedNodes /= model.cards.collapsedNodes
                || previous.focus.focusedEntity /= model.focus.focusedEntity
                || previous.search.query /= model.search.query
                || previous.search.filterShowEmptyProjects /= model.search.filterShowEmptyProjects
                || previous.search.filterShowOnly /= model.search.filterShowOnly
                || previous.search.filterPriority /= model.search.filterPriority
                || previous.search.filterProjectStatuses /= model.search.filterProjectStatuses
                || previous.search.filterTaskStatuses /= model.search.filterTaskStatuses
                || (previous.dragDrop.dragging == Nothing) /= (model.dragDrop.dragging == Nothing)
                || (not model.dataLoading.navigationVisibilityActive && (previous.projects /= model.projects || previous.tasks /= model.tasks))
                || feedbackRowsChanged previous model
        sameContext = old.workspaceId == model.selectedWorkspaceId && old.sessionEpoch == model.sessionRequestEpoch
        filtersChanged =
            previous.search.query /= model.search.query
                || previous.search.filterShowEmptyProjects /= model.search.filterShowEmptyProjects
                || previous.search.filterShowOnly /= model.search.filterShowOnly
                || previous.search.filterPriority /= model.search.filterPriority
                || previous.search.filterProjectStatuses /= model.search.filterProjectStatuses
                || previous.search.filterTaskStatuses /= model.search.filterTaskStatuses
        preserveScroll =
            sameContext && (filtersChanged || (old.preserveScroll
                && (previous.dataLoading.rootNavigationRequest |> Maybe.map .generation) == (model.dataLoading.rootNavigationRequest |> Maybe.map .generation)
                && previous.cards.collapsedNodes == model.cards.collapsedNodes
                && previous.focus.focusedEntity == model.focus.focusedEntity))
        awaitingFilteredRoots =
            preserveScroll && (model.dataLoading.rootNavigationRequest
                |> Maybe.map (\request -> request.inFlight && not request.succeeded)
                |> Maybe.withDefault False)
        active = model.auth.status == AuthReady && model.activeTab == ProjectsTab
    in
    if not active then
        let
            cards = model.cards
            initial = init.viewport
            viewport = { initial | workspaceId = model.selectedWorkspaceId, sessionEpoch = model.sessionRequestEpoch, generation = model.dataLoading.navigationGeneration, revision = old.revision + 1 }
            retired = { model | cards = { cards | viewport = viewport } }
        in
        ( retired, Cmd.batch [ command, Ports.syncHierarchyViewport (viewportConfiguration retired Nothing 0) ] )
    else if not changed then
        let
            metadataCommand = Cmd.batch [ command, if previous.dataLoading.backgroundAdmission /= model.dataLoading.backgroundAdmission then Ports.syncHierarchyViewport (viewportConfiguration model Nothing 0) else Cmd.none ]
        in
        if previous.dependencies.taskDependencyLinks /= model.dependencies.taskDependencyLinks || taskStatusesChanged then
            let
                cards = model.cards
                viewport = cards.viewport
                projection = viewport.projection |> Maybe.map (\cached -> { cached | taskDirectOpenDependencyCounts = directOpenDependencyCounts model.tasks model.dependencies.taskDependencyLinks })
            in
            ( { model | cards = { cards | viewport = { viewport | projection = projection } } }, metadataCommand )
        else
            ( model, metadataCommand )
    else
        case model.selectedWorkspaceId of
            Nothing -> ( model, command )
            Just ws ->
                let
                    projection = if awaitingFilteredRoots then old.projection |> Maybe.withDefault (cardTreeProjection ws model) else cardTreeProjection ws model
                    rows = if awaitingFilteredRoots then Array.toList old.index.keys |> List.filterMap (\key -> Dict.get key old.rows) else logicalRows projection model
                    keys = List.map .key rows
                    defaults = rows |> List.filter (.kind >> (\kind -> kind /= "project" && kind /= "task")) |> List.map (\row -> ( row.key, if row.kind == "drop" then 8 else 40 )) |> Dict.fromList
                    measurements = if sameContext then Dict.union old.index.heights defaults else defaults
                    index = Viewport.build 160 measurements keys
                    anchorPosition = Viewport.positionAt old.top old.index
                    awaitingFirstRows =
                        old.top == 0 && not (Dict.values old.rows |> List.any (\row -> row.kind == "project" || row.kind == "task"))
                    -- Loading end-status rows precede the first real roots.
                    -- They are not a user anchor at untouched scroll origin.
                    anchor = if awaitingFirstRows then Nothing else Array.get anchorPosition old.index.keys
                    delta = old.top - Viewport.offset anchorPosition old.index
                    anchoredTop =
                        if preserveScroll then old.top
                        else if sameContext then
                            anchor |> Maybe.andThen (\key -> Dict.get key index.positions) |> Maybe.map (\i -> Basics.max 0 (Viewport.offset i index + delta)) |> Maybe.withDefault old.top
                        else 0
                    viewport = { old | workspaceId = Just ws, sessionEpoch = model.sessionRequestEpoch, generation = model.dataLoading.navigationGeneration
                        , revision = old.revision + 1, rows = rows |> List.map (\row -> ( row.key, row )) |> Dict.fromList
                        , index = index, projection = Just projection, top = anchoredTop
                        , nativePins = if sameContext then Set.filter (\key -> Dict.member key index.positions) old.nativePins else Set.empty
                        , target = Nothing
                        , preserveScroll = preserveScroll
                        , filterExtent =
                            if not preserveScroll then 0
                            else if filtersChanged then
                                if activeNavigationTransport previous then Basics.max old.filterExtent (Viewport.height old.index) else Viewport.height old.index
                            else old.filterExtent
                        }
                    cards = model.cards
                    rebuilt = { model | cards = { cards | viewport = viewport } }
                    ( demanded, details ) = viewportDetailDemand rebuilt
                in
                ( demanded, Cmd.batch [ command, details, Ports.syncHierarchyViewport (viewportConfiguration demanded (if sameContext then anchor else Nothing) delta) ] )


viewportBackgroundReady : Model -> Bool
viewportBackgroundReady model =
    let
        fingerprint = Feature.DataLoading.navigationFilterFingerprint model
        current request = Just request.workspaceId == model.selectedWorkspaceId && request.sessionEpoch == model.sessionRequestEpoch && request.filterFingerprint == fingerprint
        rootReady = model.dataLoading.rootNavigationRequest |> Maybe.map (\request -> current request && (request.succeeded || request.projectCardCount > 0 || request.taskCardCount > 0)) |> Maybe.withDefault False
        focusReady = model.focus.focusedEntity |> Maybe.andThen (\( kind, id ) -> Dict.get (kind ++ ":" ++ id) model.dataLoading.navigationFocuses) |> Maybe.map (\request -> current request && request.succeeded) |> Maybe.withDefault False
    in
    model.auth.status == AuthReady && model.activeTab == ProjectsTab && (rootReady || focusReady)


viewportConfiguration : Model -> Maybe String -> Float -> Encode.Value
viewportConfiguration model anchor delta =
    let
        viewport = model.cards.viewport
    in
    Encode.object
        [ ( "paintReady", Encode.bool (viewportBackgroundReady model) )
        , ( "paintNonce", Encode.int (Feature.DataLoading.backgroundPaintNonce model) )
        , ( "paintFilter", Encode.string (Feature.DataLoading.navigationFilterFingerprint model) )
        , ( "workspace", Encode.string (Maybe.withDefault "" viewport.workspaceId) )
        , ( "epoch", Encode.int viewport.sessionEpoch ), ( "generation", Encode.int viewport.generation ), ( "revision", Encode.int viewport.revision )
        , ( "anchor", anchor |> Maybe.map Encode.string |> Maybe.withDefault Encode.null ), ( "delta", Encode.float delta )
        , ( "top", Encode.float viewport.top ), ( "target", viewport.target |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
        , ( "preserveScroll", Encode.bool viewport.preserveScroll )
        ]


updateViewport : Encode.Value -> Model -> ( Model, Cmd Msg )
updateViewport payload model =
    let
        viewport = model.cards.viewport
        get name decoder fallback = Decode.decodeValue (Decode.field name decoder) payload |> Result.withDefault fallback
        valid = get "workspace" Decode.string "" == Maybe.withDefault "!" model.selectedWorkspaceId
            && get "epoch" Decode.int -1 == model.sessionRequestEpoch
            && get "generation" Decode.int -1 == model.dataLoading.navigationGeneration
            && get "revision" Decode.int -1 == viewport.revision
        measured = get "measurements" (Decode.list (Decode.map2 Tuple.pair (Decode.field "key" Decode.string) (Decode.field "height" Decode.float))) []
        incomingTop = Basics.max 0 (get "top" Decode.float viewport.top)
        anchorPosition = Viewport.positionAt incomingTop viewport.index
        anchorKey = Array.get anchorPosition viewport.index.keys
        anchorDelta = incomingTop - Viewport.offset anchorPosition viewport.index
        index = List.foldl (\( key, amount ) geometry -> if amount > 0 && amount < 100000 then Viewport.measure key amount geometry else geometry) viewport.index measured
        target =
            get "request" (Decode.nullable Decode.string) Nothing
                |> Maybe.andThen (\requested ->
                    let
                        key = if String.startsWith "entity:" requested then
                            let id = String.dropLeft 7 requested in
                            if Dict.member ("project:" ++ id) viewport.rows then "project:" ++ id else "task:" ++ id
                            else requested
                    in
                    if Dict.member key viewport.rows then Just key else Nothing
                )
        top = case target of
            Just key -> Dict.get key index.positions |> Maybe.map (\i -> Viewport.offset i index) |> Maybe.withDefault viewport.top
            Nothing ->
                if viewport.preserveScroll || List.isEmpty measured then incomingTop
                else anchorKey |> Maybe.andThen (\key -> Dict.get key index.positions) |> Maybe.map (\i -> Basics.max 0 (Viewport.offset i index + anchorDelta)) |> Maybe.withDefault viewport.top
        next = { viewport | top = top, height = Basics.max 1 (get "height" Decode.float viewport.height), index = index
            , preserveScroll = viewport.preserveScroll && target == Nothing
            , nativePins = get "pins" (Decode.list Decode.string) [] |> List.take 1 |> Set.fromList |> Set.filter (\key -> Dict.member key viewport.rows)
            , target = case target of
                Just _ -> target
                Nothing -> if get "acknowledged" Decode.bool False then Nothing else viewport.target
            }
        cards = model.cards
        updated = { model | cards = { cards | viewport = next } }
        ( demanded, details ) = viewportDetailDemand updated
    in
    if not valid || model.auth.status /= AuthReady then ( model, Cmd.none ) else
        let
            ( admitted, waveCommand ) = Feature.DataLoading.acceptBackgroundPaint (if get "paintFilter" Decode.string "" == Feature.DataLoading.navigationFilterFingerprint demanded then get "paintNonce" Decode.int -1 else -1) demanded
        in
        refreshViewport demanded ( admitted, Cmd.batch [ details, waveCommand, if not (List.isEmpty measured) || target /= Nothing then Ports.syncHierarchyViewport (viewportConfiguration admitted anchorKey anchorDelta) else Cmd.none ] )


viewHierarchyViewport : String -> Model -> Html Msg
viewHierarchyViewport ws incoming =
    let
        model =
            case incoming.cards.viewport.projection of
                Just _ -> incoming
                Nothing ->
                    let
                        initialProjection = cardTreeProjection ws incoming
                        rows = logicalRows initialProjection incoming
                        cards = incoming.cards
                        old = cards.viewport
                        initial = { old | workspaceId = Just ws, sessionEpoch = incoming.sessionRequestEpoch, generation = incoming.dataLoading.navigationGeneration
                            , projection = Just initialProjection, rows = rows |> List.map (\row -> ( row.key, row )) |> Dict.fromList
                            , index = Viewport.build 160 Dict.empty (List.map .key rows)
                            }
                    in
                    { incoming | cards = { cards | viewport = initial } }
        viewport = model.cards.viewport
        projection =
            case viewport.projection of
                Just cached ->
                    { cached | projectRollups = model.dependencies.projectReadinessRollups, taskRollups = model.dependencies.taskReadinessRollups }
                Nothing -> cardTreeProjection ws model
        config = viewportConfiguration model Nothing 0 |> Encode.encode 0
        neighborEntity direction position =
            case Array.get (position + direction) viewport.index.keys of
                Nothing -> ""
                Just key ->
                    case Dict.get key viewport.rows of
                        Just row -> if row.kind == "project" || row.kind == "task" then key else neighborEntity direction (position + direction)
                        Nothing -> ""
        taskFamily seen key =
            case Dict.get key viewport.rows of
                Just row ->
                    if Set.member key seen then Nothing
                    else if row.kind == "task" then
                        case row.parentId of
                            Just parent -> taskFamily (Set.insert key seen) ("task:" ++ parent)
                            Nothing -> Just ( key, row.depth )
                    else if row.parentKind == "task" || row.parentKind == "task-subtasks" then
                        row.parentId |> Maybe.andThen (\parent -> taskFamily (Set.insert key seen) ("task:" ++ parent))
                    else Nothing
                Nothing -> Nothing
        familyAt position = Array.get position viewport.index.keys |> Maybe.andThen (taskFamily Set.empty)
        pieceFamily piece =
            case piece of
                Viewport.Row position _ _ -> familyAt position
                Viewport.Gap start amount ->
                    let
                        first = familyAt start
                        last = familyAt (Viewport.positionAt (Viewport.offset start viewport.index + amount - 0.001) viewport.index)
                    in
                    if first == last then first else Nothing
        pieceView baseDepth piece =
            case piece of
                Viewport.Gap start amount ->
                    ( "gap-" ++ String.fromInt start, div [ class "hierarchy-spacer", style "height" (String.fromFloat amount ++ "px"), attribute "aria-hidden" "true" ] [] )
                Viewport.Row position key _ ->
                    let
                        row = Dict.get key viewport.rows
                        content value =
                            case value.kind of
                                "project" -> Dict.get value.entityId model.projects |> Maybe.map (\project -> viewProjectNodeBody False projection model 0 project False "") |> Maybe.withDefault (text "")
                                "task" -> Dict.get value.entityId model.tasks |> Maybe.map (viewTaskCardBody False projection False model) |> Maybe.withDefault (text "")
                                "drop" -> value.zone |> Maybe.map (viewDropZone model) |> Maybe.withDefault (text "")
                                "root-status" -> viewViewportRootStatus model value.entityId
                                _ -> viewViewportBranchStatus model value.parentKind (Maybe.withDefault "" value.parentId)
                    in
                    ( key, div [ class "hierarchy-row", attribute "data-hierarchy-key" key, attribute "data-hierarchy-index" (String.fromInt position)
                        , attribute "data-hierarchy-next" (neighborEntity 1 position)
                        , attribute "data-hierarchy-previous" (neighborEntity -1 position)
                        , style "padding-left" (String.fromInt (row |> Maybe.map .depth |> Maybe.withDefault 0 |> (\depth -> Basics.max 0 (depth - baseDepth) * 20)) ++ "px")
                        ] [ row |> Maybe.map content |> Maybe.withDefault (text "") ] )
        pieces = Viewport.window viewport.top viewport.height 300 25 (viewportPins model) viewport.index
        protectedInput =
            case editTarget model of
                Just target -> Just target
                Nothing -> inlineCreateTarget model
        segments = Viewport.partition (protectedInput |> Maybe.map (\( kind, entityId ) -> kind ++ ":" ++ entityId)) pieces
        -- Keep the current extent only while a real row can paint in this
        -- window. A shorter loaded prefix must clamp naturally so its painted
        -- rows can admit the next bounded discovery wave.
        pendingFilterMembership = viewport.preserveScroll && activeNavigationTransport model
            && viewport.top < Viewport.height viewport.index
        groupedPieces contents =
            case contents of
                [] -> []
                first :: rest ->
                    case pieceFamily first of
                        Nothing -> pieceView 0 first :: groupedPieces rest
                        Just ( family, depth ) ->
                            let
                                collect collected remaining =
                                    case remaining of
                                        next :: tail ->
                                            if pieceFamily next == Just ( family, depth ) then collect (next :: collected) tail
                                            else ( List.reverse collected, remaining )
                                        [] -> ( List.reverse collected, [] )
                                ( members, following ) = collect [ first ] rest
                            in
                            ( family, Keyed.node "div"
                                [ class "hierarchy-task-family", attribute "data-task-family" (String.dropLeft 5 family)
                                , style "margin-left" (String.fromInt (depth * 20) ++ "px")
                                ] (List.map (pieceView depth) members)
                            ) :: groupedPieces following
        segment key contents =
            ( key, Keyed.node "div" [ class "hierarchy-segment", style "display" "contents" ] (groupedPieces contents) )
    in
    div [ class "tree-view" ]
        [ div [ class "tree-toolbar" ]
            [ button [ class "btn-small btn-ghost", onClick ExpandAllNodes ] [ text "Expand All" ]
            , button [ class "btn-small btn-ghost", onClick CollapseAllNodes ] [ text "Collapse All" ]
            ]
        , Feature.Editing.viewInlineCreateInput model Nothing "project"
        , Feature.Focus.viewFocusBreadcrumbBar model
        , Keyed.node "div" [ class "hierarchy-viewport", id "hierarchy-viewport", attribute "data-hierarchy-context" config
            , style "min-height" (String.fromFloat (if pendingFilterMembership then viewport.filterExtent else 0) ++ "px")
            ]
            [ segment "before" segments.before, segment "editor" segments.pivot, segment "after" segments.after ]
        ]


activeNavigationTransport : Model -> Bool
activeNavigationTransport model =
    let
        pending request = request.inFlight && Just request.workspaceId == model.selectedWorkspaceId
            && request.sessionEpoch == model.sessionRequestEpoch
            && request.filterFingerprint == Feature.DataLoading.navigationFilterFingerprint model
    in
    (model.dataLoading.rootNavigationRequest |> Maybe.map pending |> Maybe.withDefault False)
        || not (List.isEmpty model.dataLoading.navigationQueue)
        || List.any pending (Dict.values model.dataLoading.loadedNavigationBranches)


feedbackRowsChanged : Model -> Model -> Bool
feedbackRowsChanged previous model =
    let
        branchKeys value =
            Set.fromList
                (value.dataLoading.navigationQueue
                    ++ (Dict.toList value.dataLoading.loadedNavigationBranches
                        |> List.filter (\( _, state ) -> state.inFlight || state.projectHasMore || state.taskHasMore)
                        |> List.map Tuple.first)
                    ++ (Dict.toList value.dataLoading.navigationPasses
                        |> List.filter (\( _, pass ) -> pass.projectError /= Nothing || pass.taskError /= Nothing)
                        |> List.map Tuple.first)
                )
        branchesChanged =
            previous.dataLoading.navigationQueue /= model.dataLoading.navigationQueue
                || previous.dataLoading.loadedNavigationBranches /= model.dataLoading.loadedNavigationBranches
                || previous.dataLoading.navigationPasses /= model.dataLoading.navigationPasses
    in
    rootStatusVisible previous "project" /= rootStatusVisible model "project"
        || rootStatusVisible previous "task" /= rootStatusVisible model "task"
        || (branchesChanged && branchKeys previous /= branchKeys model)


rootStatusVisible : Model -> String -> Bool
rootStatusVisible model kind =
    model.dataLoading.rootNavigationRequest
        |> Maybe.map (\request -> request.inFlight || not request.succeeded || (if kind == "project" then request.projectHasMore else request.taskHasMore))
        |> Maybe.withDefault False


branchStatusVisible : Model -> String -> String -> Bool
branchStatusVisible model kind id =
    let
        key = kind ++ ":" ++ id
        hasError = Dict.get key model.dataLoading.navigationPasses
            |> Maybe.map (\pass -> pass.projectError /= Nothing || pass.taskError /= Nothing)
            |> Maybe.withDefault False
        pending = Dict.get key model.dataLoading.loadedNavigationBranches
            |> Maybe.map (\state -> state.inFlight || state.projectHasMore || state.taskHasMore)
            |> Maybe.withDefault False
    in
    pending || hasError || List.member key model.dataLoading.navigationQueue


viewViewportRootStatus : Model -> String -> Html Msg
viewViewportRootStatus model kind =
    case ( rootStatusVisible model kind, model.dataLoading.rootNavigationRequest ) of
        ( True, Just request ) ->
            let
                more = if kind == "project" then request.projectHasMore else request.taskHasMore
            in
            if request.inFlight then div [ attribute "role" "status" ] [ text "Loading…" ]
            else if more || not request.succeeded then
                button [ class "navigation-load-more", onClick (LoadRootNavigationPage kind) ] [ text (if request.succeeded then "Load more " ++ kind ++ "s" else "Retry loading " ++ kind ++ "s") ]
            else text ""
        _ -> text ""


viewViewportBranchStatus : Model -> String -> String -> Html Msg
viewViewportBranchStatus model kind id =
    let
        key = kind ++ ":" ++ id
        pass = Dict.get key model.dataLoading.navigationPasses
        error stream = pass |> Maybe.andThen (if stream == "project" then .projectError else .taskError)
        viewError stream = error stream |> Maybe.map (\message -> div [ class "card-description-error", attribute "role" "status" ]
            [ text message, button [ class "btn-small btn-ghost", onClick (LoadNavigationBranchPage kind id stream) ] [ text "Retry" ] ]) |> Maybe.withDefault (text "")
        loading = List.member key model.dataLoading.navigationQueue
            || (Dict.get key model.dataLoading.loadedNavigationBranches |> Maybe.map .inFlight |> Maybe.withDefault False)
        viewMore stream =
            let
                more = Dict.get key model.dataLoading.loadedNavigationBranches
                    |> Maybe.map (if stream == "project" then .projectHasMore else .taskHasMore)
                    |> Maybe.withDefault False
            in
            if more && not loading && error stream == Nothing then
                button [ class "navigation-load-more", onClick (LoadNavigationBranchPage kind id stream) ] [ text ("Load more " ++ stream ++ "s") ]
            else text ""
    in
    if branchStatusVisible model kind id then
        div [ class "hierarchy-branch-status" ]
            [ if loading then span [ attribute "role" "status" ] [ text "Loading more…" ] else text ""
            , viewError "project", viewError "task"
            , viewMore "project", viewMore "task"
            ]
    else text ""
