module Feature.DragDrop exposing (dragOverClass, update, viewDropActionModal)

import Api
import Dict
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStartCard entType entId ->
            ( { model | dragging = Just { entityType = entType, entityId = entId } }, Cmd.none )

        DragOverCard targetId ->
            ( { model | dragOver = Just (OverCard targetId) }, Cmd.none )

        DragOverZone zone ->
            ( { model | dragOver = Just (OverZone zone) }, Cmd.none )

        DropOnCard targetType targetId ->
            case model.dragging of
                Just drag ->
                    if drag.entityId == targetId then
                        ( { model | dragging = Nothing, dragOver = Nothing }, Cmd.none )

                    else if drag.entityType == "task" && targetType == "project" then
                        -- Drop task/subtask on project → move as direct task in that project
                        ( { model | dragging = Nothing, dragOver = Nothing }
                        , Api.updateTask model.flags.apiUrl drag.entityId
                            [ ( "project_id", Encode.string targetId )
                            , ( "parent_id", Encode.null )
                            ]
                            TaskUpdated
                        )

                    else if drag.entityType == "project" && targetType == "project" then
                        -- Drop project on project → make it a subproject
                        ( { model | dragging = Nothing, dragOver = Nothing }
                        , Api.updateProject model.flags.apiUrl drag.entityId
                            [ ( "parent_id", Encode.string targetId ) ]
                            ProjectUpdated
                        )

                    else if drag.entityType == "task" && targetType == "task" then
                        -- Drop task on task → show modal asking Subtask or Dependency
                        ( { model
                            | dragging = Nothing
                            , dragOver = Nothing
                            , dropActionModal = Just { dragTaskId = drag.entityId, targetTaskId = targetId }
                          }
                        , Cmd.none
                        )

                    else if drag.entityType == targetType then
                        -- Same type → swap priorities
                        ( { model | dragging = Nothing, dragOver = Nothing }
                        , swapPriorities model.flags.apiUrl drag targetType targetId model
                        )

                    else
                        ( { model | dragging = Nothing, dragOver = Nothing }, Cmd.none )

                Nothing ->
                    ( { model | dragging = Nothing, dragOver = Nothing }, Cmd.none )

        DragEndCard ->
            ( { model | dragging = Nothing, dragOver = Nothing }, Cmd.none )

        DropOnZone zone ->
            case model.dragging of
                Just drag ->
                    let
                        newPriority =
                            computeDropPriority zone.abovePriority zone.belowPriority
                    in
                    case ( drag.entityType, zone.parentType ) of
                        ( "task", "project-tasks" ) ->
                            ( { model | dragging = Nothing, dragOver = Nothing }
                            , Api.updateTask model.flags.apiUrl
                                drag.entityId
                                [ ( "project_id"
                                  , case zone.projectId of
                                        Just pid ->
                                            Encode.string pid

                                        Nothing ->
                                            Encode.null
                                  )
                                , ( "parent_id", Encode.null )
                                , ( "priority", Encode.int newPriority )
                                ]
                                TaskUpdated
                            )

                        ( "task", "task-subtasks" ) ->
                            ( { model | dragging = Nothing, dragOver = Nothing }
                            , Api.updateTask model.flags.apiUrl
                                drag.entityId
                                [ ( "parent_id"
                                  , case zone.parentId of
                                        Just pid ->
                                            Encode.string pid

                                        Nothing ->
                                            Encode.null
                                  )
                                , ( "project_id"
                                  , case zone.projectId of
                                        Just pid ->
                                            Encode.string pid

                                        Nothing ->
                                            Encode.null
                                  )
                                , ( "priority", Encode.int newPriority )
                                ]
                                TaskUpdated
                            )

                        ( "task", "orphan" ) ->
                            ( { model | dragging = Nothing, dragOver = Nothing }
                            , Api.updateTask model.flags.apiUrl
                                drag.entityId
                                [ ( "project_id", Encode.null )
                                , ( "parent_id", Encode.null )
                                , ( "priority", Encode.int newPriority )
                                ]
                                TaskUpdated
                            )

                        ( "project", "project" ) ->
                            ( { model | dragging = Nothing, dragOver = Nothing }
                            , Api.updateProject model.flags.apiUrl
                                drag.entityId
                                [ ( "priority", Encode.int newPriority )
                                , ( "parent_id"
                                  , case zone.parentId of
                                        Just pid ->
                                            Encode.string pid

                                        Nothing ->
                                            Encode.null
                                  )
                                ]
                                ProjectUpdated
                            )

                        _ ->
                            ( { model | dragging = Nothing, dragOver = Nothing }, Cmd.none )

                Nothing ->
                    ( { model | dragging = Nothing, dragOver = Nothing }, Cmd.none )

        -- Drop action modal
        DropActionMakeSubtask ->
            case model.dropActionModal of
                Just modal ->
                    let
                        targetTask =
                            Dict.get modal.targetTaskId model.tasks
                    in
                    ( { model | dropActionModal = Nothing }
                    , Api.updateTask model.flags.apiUrl modal.dragTaskId
                        [ ( "parent_id", Encode.string modal.targetTaskId )
                        , ( "project_id"
                          , case targetTask |> Maybe.andThen .projectId of
                                Just pid ->
                                    Encode.string pid

                                Nothing ->
                                    Encode.null
                          )
                        ]
                        TaskUpdated
                    )

                Nothing ->
                    ( model, Cmd.none )

        DropActionMakeDependency ->
            case model.dropActionModal of
                Just modal ->
                    ( { model | dropActionModal = Nothing }
                    , Api.addTaskDependency model.flags.apiUrl modal.dragTaskId modal.targetTaskId
                        (DependencyMutationDone modal.dragTaskId)
                    )

                Nothing ->
                    ( model, Cmd.none )

        CancelDropAction ->
            ( { model | dropActionModal = Nothing }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- HELPERS


dragOverClass : Model -> String -> String
dragOverClass model entityId =
    case ( model.dragOver, model.dragging ) of
        ( Just (OverCard overId), Just drag ) ->
            if overId == entityId && drag.entityId /= entityId then
                -- Task or project dragged over a project → green "move" indicator
                if (drag.entityType == "task" || drag.entityType == "project") && Dict.member entityId model.projects then
                    " drag-over drag-over-move"

                else
                    " drag-over"

            else
                ""

        _ ->
            ""


swapPriorities : String -> DragInfo -> String -> String -> Model -> Cmd Msg
swapPriorities apiUrl drag targetType targetId model =
    case drag.entityType of
        "project" ->
            let
                dragPri =
                    Dict.get drag.entityId model.projects |> Maybe.map .priority |> Maybe.withDefault 5

                targetPri =
                    Dict.get targetId model.projects |> Maybe.map .priority |> Maybe.withDefault 5
            in
            Cmd.batch
                [ Api.updateProject apiUrl drag.entityId [ ( "priority", Encode.int targetPri ) ] ProjectUpdated
                , Api.updateProject apiUrl targetId [ ( "priority", Encode.int dragPri ) ] ProjectUpdated
                ]

        "task" ->
            let
                dragPri =
                    Dict.get drag.entityId model.tasks |> Maybe.map .priority |> Maybe.withDefault 5

                targetPri =
                    Dict.get targetId model.tasks |> Maybe.map .priority |> Maybe.withDefault 5
            in
            Cmd.batch
                [ Api.updateTask apiUrl drag.entityId [ ( "priority", Encode.int targetPri ) ] TaskUpdated
                , Api.updateTask apiUrl targetId [ ( "priority", Encode.int dragPri ) ] TaskUpdated
                ]

        "memory" ->
            let
                dragImp =
                    Dict.get drag.entityId model.memories |> Maybe.map .importance |> Maybe.withDefault 5

                targetImp =
                    Dict.get targetId model.memories |> Maybe.map .importance |> Maybe.withDefault 5
            in
            Cmd.batch
                [ Api.updateMemory apiUrl drag.entityId [ ( "importance", Encode.int targetImp ) ] MemoryUpdated
                , Api.updateMemory apiUrl targetId [ ( "importance", Encode.int dragImp ) ] MemoryUpdated
                ]

        _ ->
            Cmd.none



-- VIEW


viewDropActionModal : Model -> Html Msg
viewDropActionModal model =
    case model.dropActionModal of
        Nothing ->
            text ""

        Just modal ->
            let
                dragName =
                    Dict.get modal.dragTaskId model.tasks
                        |> Maybe.map .title
                        |> Maybe.withDefault "Task"

                targetName =
                    Dict.get modal.targetTaskId model.tasks
                        |> Maybe.map .title
                        |> Maybe.withDefault "Task"
            in
            div [ class "modal-overlay", onClick CancelDropAction ]
                [ div [ class "modal drop-action-modal", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                    [ h3 [ class "modal-title" ] [ text "Move Task" ]
                    , p [ class "drop-action-desc" ]
                        [ text "What should "
                        , strong [] [ text (truncateText 40 dragName) ]
                        , text " become relative to "
                        , strong [] [ text (truncateText 40 targetName) ]
                        , text "?"
                        ]
                    , div [ class "drop-action-buttons" ]
                        [ button [ class "btn drop-action-btn", onClick DropActionMakeSubtask ]
                            [ span [ class "drop-action-icon" ] [ text "↳" ]
                            , span [] [ text "Subtask" ]
                            ]
                        , button [ class "btn drop-action-btn", onClick DropActionMakeDependency ]
                            [ span [ class "drop-action-icon" ] [ text "⟶" ]
                            , span [] [ text "Dependency" ]
                            ]
                        ]
                    , div [ class "modal-actions" ]
                        [ button [ class "btn btn-secondary", onClick CancelDropAction ] [ text "Cancel" ]
                        ]
                    ]
                ]
