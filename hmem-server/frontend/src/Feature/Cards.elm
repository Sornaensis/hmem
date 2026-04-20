module Feature.Cards exposing
    ( handleEscape
    , init
    , update
    , viewDeleteConfirmModal
    , viewProjectsTree
    )

import Api
import Dict
import Feature.AuditLog
import Feature.Dependencies
import Feature.DragDrop
import Feature.Editing
import Feature.Focus
import Feature.Memory
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Ports exposing (copyToClipboard)
import Toast exposing (addToast)
import Types exposing (..)


init : CardsModel
init =
    { expandedCards = Dict.empty
    , collapsedNodes = Dict.empty
    , deleteConfirmation = Nothing
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleCardExpand cardId ->
            let
                current =
                    Dict.get cardId model.cards.expandedCards |> Maybe.withDefault False

                newExpanded =
                    not current

                fetchMemCmd =
                    if newExpanded && not (Dict.member cardId model.memory.entityMemories) then
                        if Dict.member cardId model.projects then
                            Api.fetchProjectMemories model.flags.apiUrl cardId (GotEntityMemories cardId)

                        else if Dict.member cardId model.tasks then
                            Api.fetchTaskMemories model.flags.apiUrl cardId (GotEntityMemories cardId)

                        else
                            Cmd.none

                    else
                        Cmd.none

                fetchDepCmd =
                    if newExpanded && Dict.member cardId model.tasks && not (Dict.member cardId model.dependencies.taskDependencies) then
                        Api.fetchTaskOverview model.flags.apiUrl cardId (GotTaskDependencies cardId)

                    else
                        Cmd.none

                currentCards =
                    model.cards

                updatedCards =
                    { currentCards | expandedCards = Dict.insert cardId newExpanded model.cards.expandedCards }

                currentEditing =
                    model.editing

                updatedEditing =
                    { currentEditing | editState = Nothing }
            in
            ( { model
                | cards = updatedCards
                , editing = updatedEditing
              }
            , Cmd.batch [ fetchMemCmd, fetchDepCmd ]
            )

        ToggleTreeNode nodeId ->
            let
                current =
                    Dict.get nodeId model.cards.collapsedNodes |> Maybe.withDefault False

                newModel =
                    updateCardsModel
                        (\records -> { records | collapsedNodes = Dict.insert nodeId (not current) records.collapsedNodes })
                        model
            in
            ( newModel, saveFiltersCmd newModel )

        ExpandAllNodes ->
            let
                newModel =
                    updateCardsModel (\records -> { records | collapsedNodes = Dict.empty }) model
            in
            ( newModel, saveFiltersCmd newModel )

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
            in
            ( newModel, saveFiltersCmd newModel )

        ConfirmDelete entityType entityId ->
            let
                currentCards =
                    model.cards

                updatedCards =
                    { currentCards | deleteConfirmation = Just ( entityType, entityId ) }
            in
            ( { model | cards = updatedCards }, Cmd.none )

        PerformDelete ->
            case model.cards.deleteConfirmation of
                Just ( entityType, entityId ) ->
                    let
                        currentCards =
                            model.cards

                        updatedCards =
                            { currentCards | deleteConfirmation = Nothing }

                        ( trackedModel, requestId, clearCmd ) =
                            beginTrackedMutation [ entityId ] { model | cards = updatedCards }

                        cmd =
                            case entityType of
                                "project" ->
                                    Api.deleteProject model.flags.apiUrl entityId requestId (MutationDone "project")

                                "task" ->
                                    Api.deleteTask model.flags.apiUrl entityId requestId (MutationDone "task")

                                "memory" ->
                                    Api.deleteMemory model.flags.apiUrl entityId requestId (MutationDone "memory")

                                "group" ->
                                    Api.deleteWorkspaceGroup model.flags.apiUrl entityId requestId (WorkspaceGroupDeleted entityId)

                                _ ->
                                    Cmd.none
                    in
                    ( trackedModel, Cmd.batch [ clearCmd, cmd ] )

                Nothing ->
                    ( model, Cmd.none )

        CancelDelete ->
            let
                currentCards =
                    model.cards

                updatedCards =
                    { currentCards | deleteConfirmation = Nothing }
            in
            ( { model | cards = updatedCards }, Cmd.none )

        CopyId idStr ->
            let
                ( m2, toastCmd ) =
                    addToast Success "ID copied to clipboard" model
            in
            ( m2, Cmd.batch [ copyToClipboard idStr, toastCmd ] )

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



-- VIEW


viewProjectsTree : String -> Model -> Html Msg
viewProjectsTree wsId model =
    let
        wsProjects =
            model.projects
                |> Dict.values
                |> List.filter (\p -> p.workspaceId == wsId)

        wsTasks =
            model.tasks
                |> Dict.values
                |> List.filter (\t -> t.workspaceId == wsId)

        query =
            String.toLower (String.trim model.search.query)

        hasSearch =
            not (String.isEmpty query)

        hasActiveFilters =
            model.search.filterShowOnly /= ShowAll || model.search.filterPriority /= AnyPriority || not (List.isEmpty model.search.filterProjectStatuses) || not (List.isEmpty model.search.filterTaskStatuses)

        projectPassesFilters p =
            (model.search.filterShowOnly /= ShowTasksOnly)
                && passesStatusFilter model.search.filterProjectStatuses (Api.projectStatusToString p.status)
                && passesPriorityFilter model.search.filterPriority p.priority

        taskPassesFilters t =
            (model.search.filterShowOnly /= ShowProjectsOnly)
                && passesStatusFilter model.search.filterTaskStatuses (Api.taskStatusToString t.status)
                && passesPriorityFilter model.search.filterPriority t.priority

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
                            [ ( projId, viewProjectNode wsProjects model 0 proj hasSearch query ) ]

                        Nothing ->
                            []

                Just ( "task", taskId ) ->
                    case Dict.get taskId model.tasks of
                        Just task ->
                            [ ( taskId, viewFocusedTaskNode model task ) ]

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
                                        |> (if hasSearch then
                                                List.filter (taskTreeMatchesSearch query wsTasks)

                                            else
                                                identity
                                           )
                                        |> (if hasActiveFilters then
                                                List.filter (\t -> taskTreePassesFilters t wsTasks taskPassesFilters)

                                            else
                                                identity
                                           )
                            in
                            List.map (\t -> ( t.id, viewTaskCard False model t )) visibleRootTasks

                        _ ->
                            let
                                rootProjects =
                                    wsProjects
                                        |> List.filter (\p -> p.parentId == Nothing)
                                        |> List.sortBy (\p -> ( Api.projectStatusOrder p.status, negate p.priority, String.toLower p.name ))

                                visibleRootProjects =
                                    rootProjects
                                        |> (if hasSearch then
                                                List.filter (projectTreeMatchesSearch query wsProjects wsTasks)

                                            else
                                                identity
                                           )
                                        |> (if hasActiveFilters then
                                                List.filter (\p -> projectTreePassesFilters (\pp -> passesStatusFilter model.search.filterProjectStatuses (Api.projectStatusToString pp.status)) p wsProjects wsTasks projectPassesFilters taskPassesFilters)

                                            else
                                                identity
                                           )

                                orphanTasks =
                                    wsTasks
                                        |> List.filter (\t -> t.projectId == Nothing && t.parentId == Nothing)
                                        |> List.sortBy (\t -> ( Api.taskStatusOrder t.status, negate t.priority, String.toLower t.title ))

                                visibleOrphans =
                                    orphanTasks
                                        |> (if hasSearch then
                                                List.filter (taskTreeMatchesSearch query wsTasks)

                                            else
                                                identity
                                           )
                                        |> (if hasActiveFilters then
                                                List.filter (\t -> taskTreePassesFilters t wsTasks taskPassesFilters)

                                            else
                                                identity
                                           )
                            in
                            viewProjectsWithZones model (\p -> viewProjectNode wsProjects model 0 p hasSearch query) Nothing visibleRootProjects
                                ++ (if model.search.filterShowOnly /= ShowProjectsOnly && not (List.isEmpty visibleOrphans) then
                                        [ ( "orphan-tasks-section"
                                          , div [ class "orphan-tasks-section" ]
                                                [ div [ class "orphan-tasks-header" ] [ text "Unassigned Tasks" ]
                                                , Keyed.node "div" [ class "tree-tasks" ]
                                                    (viewTasksWithZones model "orphan" Nothing Nothing visibleOrphans)
                                                ]
                                          )
                                        ]

                                    else
                                        []
                                   )
    in
    Keyed.node "div"
        [ class "tree-view" ]
        (( "expand-collapse-bar", expandCollapseBar )
            :: ( "inline-create", inlineCreateView )
            :: ( "focus-breadcrumb", focusBreadcrumbBar )
            :: treeContent
        )


viewProjectNode : List Api.Project -> Model -> Int -> Api.Project -> Bool -> String -> Html Msg
viewProjectNode allProjects model depth project hasSearch query =
    let
        allTasks =
            Dict.values model.tasks

        hasActiveFilters =
            model.search.filterShowOnly /= ShowAll || model.search.filterPriority /= AnyPriority || not (List.isEmpty model.search.filterProjectStatuses) || not (List.isEmpty model.search.filterTaskStatuses)

        projectPassesFilters p =
            (model.search.filterShowOnly /= ShowTasksOnly)
                && passesStatusFilter model.search.filterProjectStatuses (Api.projectStatusToString p.status)
                && passesPriorityFilter model.search.filterPriority p.priority

        taskPassesFilters t =
            (model.search.filterShowOnly /= ShowProjectsOnly)
                && passesStatusFilter model.search.filterTaskStatuses (Api.taskStatusToString t.status)
                && passesPriorityFilter model.search.filterPriority t.priority

        children =
            allProjects
                |> List.filter (\p -> p.parentId == Just project.id)
                |> List.sortBy (\p -> ( Api.projectStatusOrder p.status, negate p.priority, String.toLower p.name ))

        visibleChildren =
            children
                |> (if hasSearch then
                        List.filter (projectTreeMatchesSearch query allProjects allTasks)

                    else
                        identity
                   )
                |> (if hasActiveFilters then
                        List.filter (\p -> projectTreePassesFilters (\pp -> passesStatusFilter model.search.filterProjectStatuses (Api.projectStatusToString pp.status)) p allProjects allTasks projectPassesFilters taskPassesFilters)

                    else
                        identity
                   )

        hasChildren =
            not (List.isEmpty children)

        collapsed =
            isCollapsed model ("proj-" ++ project.id)

        projectTasks =
            model.tasks
                |> Dict.values
                |> List.filter (\t -> t.projectId == Just project.id && t.parentId == Nothing)
                |> List.sortBy (\t -> ( Api.taskStatusOrder t.status, negate t.priority, String.toLower t.title ))

        visibleTasks =
            projectTasks
                |> (if hasSearch then
                        List.filter (taskTreeMatchesSearch query allTasks)

                    else
                        identity
                   )
                |> (if hasActiveFilters then
                        List.filter (\t -> taskTreePassesFilters t allTasks taskPassesFilters)

                    else
                        identity
                   )

        linkedMems =
            Dict.get project.id model.memory.entityMemories |> Maybe.withDefault []
    in
    div [ class "tree-node", style "margin-left" (String.fromInt (depth * 20) ++ "px"), id ("entity-" ++ project.id) ]
        [ div
            [ class ("card tree-card card-project card-status-" ++ Api.projectStatusToString project.status ++ Feature.DragDrop.dragOverClass model project.id)
            , draggable "true"
            , on "dragstart" (Decode.succeed (DragStartCard "project" project.id))
            , preventDefaultOn "dragover" (Decode.succeed ( DragOverCard project.id, True ))
            , preventDefaultOn "drop" (Decode.succeed ( DropOnCard "project" project.id, True ))
            , on "dragend" (Decode.succeed DragEndCard)
            , onDoubleClick (FocusEntity "project" project.id)
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
                    [ Feature.Editing.viewStatusSelect "project" project.id (Api.projectStatusToString project.status) Api.allProjectStatuses Api.projectStatusToString ChangeProjectStatus
                    , Feature.Editing.viewPrioritySelect "project" project.id project.priority ChangeProjectPriority
                    , button [ class "btn-icon btn-danger", onClick (ConfirmDelete "project" project.id), title "Delete" ] [ text "✕" ]
                    ]
                ]
            , let
                isProjectRemaining p =
                    p.status == Api.ProjActive || p.status == Api.ProjPaused

                isTaskRemaining t =
                    t.status == Api.Todo || t.status == Api.InProgress || t.status == Api.Blocked

                allDescendantProjectIds =
                    collectDescendantProjectIds allProjects project.id

                allDescendantProjects =
                    allProjects
                        |> List.filter (\p -> p.id /= project.id && List.member p.id allDescendantProjectIds)

                remainingSubprojects =
                    List.filter isProjectRemaining allDescendantProjects |> List.length

                completedSubprojects =
                    List.length allDescendantProjects - remainingSubprojects

                allProjectTasks =
                    model.tasks
                        |> Dict.values
                        |> List.filter (\t -> t.parentId == Nothing && (List.member (Maybe.withDefault "" (Maybe.map identity t.projectId)) allDescendantProjectIds))

                remainingTasks =
                    List.filter isTaskRemaining allProjectTasks |> List.length

                completedTasks =
                    List.length allProjectTasks - remainingTasks

                memCount =
                    List.length linkedMems

                summaryParts =
                    List.filterMap identity
                        [ countLabel remainingSubprojects completedSubprojects "subproject" "subprojects"
                        , countLabel remainingTasks completedTasks "task" "tasks"
                        , if memCount > 0 then
                            Just (String.fromInt memCount ++ " memor" ++ (if memCount > 1 then "ies" else "y"))

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
                    , Feature.Editing.viewEditableTextarea model "project" project.id "description" (Maybe.withDefault "" project.description)
                    ]
                , if isExpanded model project.id then
                    div [ class "card-extras" ]
                        [ Feature.Memory.viewLinkedMemories model "project" project.id linkedMems
                        , Feature.AuditLog.viewEntityHistory model "project" project.id
                        ]

                  else
                    text ""
                ]
            , div [ class "card-inline-actions" ]
                [ button
                    [ class "btn-inline-create"
                    , onClick (ShowInlineCreate (InlineCreateProject { parentId = Just project.id, name = "" }))
                    ]
                    [ text "+ Subproject" ]
                , button
                    [ class "btn-inline-create"
                    , onClick (ShowInlineCreate (InlineCreateTask { projectId = Just project.id, parentId = Nothing, title = "" }))
                    ]
                    [ text "+ Task" ]
                ]
            , Feature.Editing.viewInlineCreateInputForParent model (Just project.id) "project"
            , Feature.Editing.viewInlineCreateInputForParent model (Just project.id) "task"
            , div [ class "card-meta-group" ]
                [ div [ class "card-meta-row" ]
                    [ span [ class "card-meta" ] [ text ("Created: " ++ formatDate project.createdAt) ]
                    , span [ class "card-meta" ] [ text ("Updated: " ++ formatDate project.updatedAt) ]
                    , span [ class "card-meta card-id card-id-copy", onClick (CopyId project.id) ] [ text project.id ]
                    ]
                ]
            ]
        , if not collapsed then
            Keyed.node "div" [ class "tree-children" ]
                (viewProjectsWithZones model (\c -> viewProjectNode allProjects model (depth + 1) c hasSearch query) (Just project.id) visibleChildren
                    ++ (if not (List.isEmpty visibleTasks) then
                            [ ( "project-tasks-" ++ project.id
                              , Keyed.node "div" [ class "tree-tasks", style "margin-left" "20px" ]
                                    (viewTasksWithZones model "project-tasks" (Just project.id) Nothing visibleTasks)
                              )
                            ]

                        else
                            []
                       )
                )

          else
            text ""
        ]


viewFocusedTaskNode : Model -> Api.Task -> Html Msg
viewFocusedTaskNode model task =
    div [ class "tree-node" ]
        [ viewTaskCard False model task
        ]


viewTaskCard : Bool -> Model -> Api.Task -> Html Msg
viewTaskCard showProject model task =
    let
        projectName =
            task.projectId
                |> Maybe.andThen (\pid -> Dict.get pid model.projects)
                |> Maybe.map .name

        hasChildren =
            model.tasks
                |> Dict.values
                |> List.any (\t -> t.parentId == Just task.id)

        collapsed =
            isCollapsed model ("task-" ++ task.id)

        isSubtask =
            task.parentId /= Nothing

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

        linkedMems =
            Dict.get task.id model.memory.entityMemories |> Maybe.withDefault []
    in
    div
        ([ class ("card tree-card " ++ cardClass ++ " card-status-" ++ Api.taskStatusToString task.status ++ Feature.DragDrop.dragOverClass model task.id)
        , draggable "true"
        , id ("entity-" ++ task.id)
        , on "dragstart" (Decode.succeed (DragStartCard "task" task.id))
        , preventDefaultOn "dragover" (Decode.succeed ( DragOverCard task.id, True ))
        , preventDefaultOn "drop" (Decode.succeed ( DropOnCard "task" task.id, True ))
        , on "dragend" (Decode.succeed DragEndCard)
        ]
        ++ (if not isSubtask then
                [ onDoubleClick (FocusEntity "task" task.id) ]

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
                [ Feature.Editing.viewStatusSelect "task" task.id (Api.taskStatusToString task.status) Api.allTaskStatuses Api.taskStatusToString ChangeTaskStatus
                , Feature.Editing.viewPrioritySelect "task" task.id task.priority ChangeTaskPriority
                , button [ class "btn-icon btn-danger", onClick (ConfirmDelete "task" task.id), title "Delete" ] [ text "✕" ]
                ]
            ]
        , let
            childTasks =
                model.tasks |> Dict.values |> List.filter (\t -> t.parentId == Just task.id)

            remainingSubtasks =
                childTasks |> List.filter (\t -> t.status == Api.Todo || t.status == Api.InProgress || t.status == Api.Blocked) |> List.length

            completedSubtasks =
                List.length childTasks - remainingSubtasks

            depCount =
                task.dependencyCount

            memCount =
                task.memoryLinkCount

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
                    , if depCount > 0 then
                        Just (String.fromInt depCount ++ " dep" ++ (if depCount > 1 then "s" else ""))

                      else
                        Nothing
                    , if memCount > 0 then
                        Just (String.fromInt memCount ++ " memor" ++ (if memCount > 1 then "ies" else "y"))

                      else
                        Nothing
                    ]
          in
          if List.isEmpty summaryParts then
            text ""

          else
            div [ class "card-summary" ] [ text (String.join " · " summaryParts) ]
        , let
            deps =
                Dict.get task.id model.dependencies.taskDependencies |> Maybe.withDefault []

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
                , Feature.Editing.viewEditableTextarea model "task" task.id "description" (Maybe.withDefault "" task.description)
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
                    , Feature.Memory.viewLinkedMemories model "task" task.id linkedMems
                    , Feature.AuditLog.viewEntityHistory model "task" task.id
                    ]

              else
                text ""
            ]
        , div [ class "card-inline-actions" ]
            [ button
                [ class "btn-inline-create"
                , onClick (ShowInlineCreate (InlineCreateTask { projectId = task.projectId, parentId = Just task.id, title = "" }))
                ]
                [ text "+ Subtask" ]
            ]
        , Feature.Editing.viewInlineCreateInputForParent model (Just task.id) "subtask"
        , div [ class "card-meta-group" ]
            [ div [ class "card-meta-row" ]
                [ span [ class "card-meta" ] [ text ("Created: " ++ formatDate task.createdAt) ]
                , span [ class "card-meta" ] [ text ("Updated: " ++ formatDate task.updatedAt) ]
                , span [ class "card-meta card-id card-id-copy", onClick (CopyId task.id) ] [ text task.id ]
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
        , if hasChildren && not collapsed then
            let
                childTasks =
                    model.tasks
                        |> Dict.values
                        |> List.filter (\t -> t.parentId == Just task.id)
                        |> List.sortBy (\t -> ( Api.taskStatusOrder t.status, negate t.priority, String.toLower t.title ))
            in
            Keyed.node "div" [ class "tree-children" ]
                (viewTasksWithZones model "task-subtasks" task.projectId (Just task.id) childTasks)

          else
            text ""
        ]


viewDeleteConfirmModal : Model -> Html Msg
viewDeleteConfirmModal model =
    case model.cards.deleteConfirmation of
        Nothing ->
            text ""

        Just ( entityType, entityId ) ->
            let
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
            in
            div [ class "modal-overlay", onClick CancelDelete ]
                [ div [ class "modal delete-confirm-modal", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                    [ h3 [ class "modal-title" ] [ text ("Delete " ++ typeLabel ++ "?") ]
                    , p [ class "delete-confirm-desc" ]
                        [ text "Are you sure you want to delete "
                        , strong [] [ text (truncateText 60 entityName) ]
                        , text "? This action cannot be undone."
                        ]
                    , div [ class "modal-actions" ]
                        [ button [ class "btn btn-danger", onClick PerformDelete ] [ text "Delete" ]
                        , button [ class "btn btn-secondary", onClick CancelDelete ] [ text "Cancel" ]
                        ]
                    ]
                ]



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
                            True

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


viewTasksWithZones : Model -> String -> Maybe String -> Maybe String -> List Api.Task -> List ( String, Html Msg )
viewTasksWithZones model zoneType projectId parentTaskId tasks =
    case model.dragDrop.dragging of
        Nothing ->
            List.map (\t -> ( t.id, viewTaskCard False model t )) tasks

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
                                :: ( t.id, viewTaskCard False model t )
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


projectTreeMatchesSearch : String -> List Api.Project -> List Api.Task -> Api.Project -> Bool
projectTreeMatchesSearch query allProjects allTasks project =
    matchesSearch query project.name
        || (project.description |> Maybe.map (matchesSearch query) |> Maybe.withDefault False)
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
