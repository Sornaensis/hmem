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
            ( updateDragDropModel (
                \dd -> { dd | dragging = Just { entityType = entType, entityId = entId } }
              ) model
            , Cmd.none
            )

        DragOverCard targetId ->
            ( updateDragDropModel (
                \dd -> { dd | dragOver = Just (OverCard targetId) }
              ) model
            , Cmd.none
            )

        DragOverZone zone ->
            ( updateDragDropModel (
                \dd -> { dd | dragOver = Just (OverZone zone) }
              ) model
            , Cmd.none
            )

        DropOnCard targetType targetId ->
            case model.dragDrop.dragging of
                Just drag ->
                    if drag.entityId == targetId then
                        ( clearDragState model, Cmd.none )

                    else if drag.entityType == "task" && targetType == "project" then
                        trackedTaskMutation [ drag.entityId, targetId ] drag.entityId
                            [ ( "project_id", Encode.string targetId )
                            , ( "parent_id", Encode.null )
                            ]
                            model

                    else if drag.entityType == "project" && targetType == "project" then
                        trackedProjectMutation [ drag.entityId, targetId ] drag.entityId
                            [ ( "parent_id", Encode.string targetId ) ]
                            model

                    else if drag.entityType == "task" && targetType == "task" then
                        ( updateDragDropModel
                            (
                                \dd ->
                                    { dd
                                        | dragging = Nothing
                                        , dragOver = Nothing
                                        , dropActionModal = Just { dragTaskId = drag.entityId, targetTaskId = targetId }
                                    }
                            )
                            model
                        , Cmd.none
                        )

                    else if drag.entityType == targetType then
                        trackedSwapPriorities drag targetType targetId model

                    else
                        ( clearDragState model, Cmd.none )

                Nothing ->
                    ( clearDragState model, Cmd.none )

        DragEndCard ->
            ( clearDragState model, Cmd.none )

        DropOnZone zone ->
            case model.dragDrop.dragging of
                Just drag ->
                    let
                        newPriority =
                            computeDropPriority zone.abovePriority zone.belowPriority
                    in
                    case ( drag.entityType, zone.parentType ) of
                        ( "task", "project-tasks" ) ->
                            trackedTaskMutation [ drag.entityId ] drag.entityId
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
                                model

                        ( "task", "task-subtasks" ) ->
                            trackedTaskMutation [ drag.entityId ] drag.entityId
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
                                model

                        ( "task", "orphan" ) ->
                            trackedTaskMutation [ drag.entityId ] drag.entityId
                                [ ( "project_id", Encode.null )
                                , ( "parent_id", Encode.null )
                                , ( "priority", Encode.int newPriority )
                                ]
                                model

                        ( "project", "project" ) ->
                            trackedProjectMutation [ drag.entityId ] drag.entityId
                                [ ( "priority", Encode.int newPriority )
                                , ( "parent_id"
                                  , case zone.parentId of
                                        Just pid ->
                                            Encode.string pid

                                        Nothing ->
                                            Encode.null
                                  )
                                ]
                                model

                        _ ->
                            ( clearDragState model, Cmd.none )

                Nothing ->
                    ( clearDragState model, Cmd.none )

        DropActionMakeSubtask ->
            case model.dragDrop.dropActionModal of
                Just modal ->
                    let
                        ( trackedModel, requestId, clearCmd ) =
                            beginTrackedMutation [ modal.dragTaskId, modal.targetTaskId ]
                                (updateDragDropModel (
                                    \dd -> { dd | dropActionModal = Nothing }
                                  ) model)

                        targetTask =
                            Dict.get modal.targetTaskId model.tasks
                    in
                    ( trackedModel
                    , Cmd.batch
                        [ clearCmd
                        , Api.updateTask model.flags.apiUrl modal.dragTaskId
                            [ ( "parent_id", Encode.string modal.targetTaskId )
                            , ( "project_id"
                              , case targetTask |> Maybe.andThen .projectId of
                                    Just pid ->
                                        Encode.string pid

                                    Nothing ->
                                        Encode.null
                              )
                            , ( "request_id", Encode.string requestId )
                            ]
                            TaskUpdated
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        DropActionMakeDependency ->
            case model.dragDrop.dropActionModal of
                Just modal ->
                    let
                        ( trackedModel, requestId, clearCmd ) =
                            beginTrackedMutation [ modal.dragTaskId, modal.targetTaskId ]
                                (updateDragDropModel (
                                    \dd -> { dd | dropActionModal = Nothing }
                                  ) model)
                    in
                    ( trackedModel
                    , Cmd.batch
                        [ clearCmd
                        , Api.addTaskDependency model.flags.apiUrl modal.dragTaskId modal.targetTaskId requestId
                            (DependencyMutationDone modal.dragTaskId)
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        CancelDropAction ->
            ( updateDragDropModel (
                \dd -> { dd | dropActionModal = Nothing }
              ) model
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


updateDragDropModel : (DragDropModel -> DragDropModel) -> Model -> Model
updateDragDropModel fn model =
    { model | dragDrop = fn model.dragDrop }


clearDragState : Model -> Model
clearDragState =
    updateDragDropModel (
        \dd -> { dd | dragging = Nothing, dragOver = Nothing }
    )


trackedTaskMutation : List String -> String -> List ( String, Encode.Value ) -> Model -> ( Model, Cmd Msg )
trackedTaskMutation trackedIds taskId fields model =
    let
        ( trackedModel, requestId, clearCmd ) =
            beginTrackedMutation trackedIds (clearDragState model)
    in
    ( trackedModel
    , Cmd.batch
        [ clearCmd
        , Api.updateTask model.flags.apiUrl taskId (fields ++ [ ( "request_id", Encode.string requestId ) ]) TaskUpdated
        ]
    )


trackedProjectMutation : List String -> String -> List ( String, Encode.Value ) -> Model -> ( Model, Cmd Msg )
trackedProjectMutation trackedIds projectId fields model =
    let
        ( trackedModel, requestId, clearCmd ) =
            beginTrackedMutation trackedIds (clearDragState model)
    in
    ( trackedModel
    , Cmd.batch
        [ clearCmd
        , Api.updateProject model.flags.apiUrl projectId (fields ++ [ ( "request_id", Encode.string requestId ) ]) ProjectUpdated
        ]
    )


trackedSwapPriorities : DragInfo -> String -> String -> Model -> ( Model, Cmd Msg )
trackedSwapPriorities drag targetType targetId model =
    let
        ( trackedModel, requestId, clearCmd ) =
            beginTrackedMutation [ drag.entityId, targetId ] (clearDragState model)
    in
    ( trackedModel
    , Cmd.batch
        [ clearCmd
        , swapPriorities model.flags.apiUrl requestId drag targetType targetId model
        ]
    )


dragOverClass : Model -> String -> String
dragOverClass model entityId =
    case ( model.dragDrop.dragOver, model.dragDrop.dragging ) of
        ( Just (OverCard overId), Just drag ) ->
            if overId == entityId && drag.entityId /= entityId then
                if (drag.entityType == "task" || drag.entityType == "project") && Dict.member entityId model.projects then
                    " drag-over drag-over-move"

                else
                    " drag-over"

            else
                ""

        _ ->
            ""


swapPriorities : String -> String -> DragInfo -> String -> String -> Model -> Cmd Msg
swapPriorities apiUrl requestId drag targetType targetId model =
    case drag.entityType of
        "project" ->
            let
                dragPri =
                    Dict.get drag.entityId model.projects |> Maybe.map .priority |> Maybe.withDefault 5

                targetPri =
                    Dict.get targetId model.projects |> Maybe.map .priority |> Maybe.withDefault 5
            in
            Cmd.batch
                [ Api.updateProject apiUrl drag.entityId [ ( "priority", Encode.int targetPri ), ( "request_id", Encode.string requestId ) ] ProjectUpdated
                , Api.updateProject apiUrl targetId [ ( "priority", Encode.int dragPri ), ( "request_id", Encode.string requestId ) ] ProjectUpdated
                ]

        "task" ->
            let
                dragPri =
                    Dict.get drag.entityId model.tasks |> Maybe.map .priority |> Maybe.withDefault 5

                targetPri =
                    Dict.get targetId model.tasks |> Maybe.map .priority |> Maybe.withDefault 5
            in
            Cmd.batch
                [ Api.updateTask apiUrl drag.entityId [ ( "priority", Encode.int targetPri ), ( "request_id", Encode.string requestId ) ] TaskUpdated
                , Api.updateTask apiUrl targetId [ ( "priority", Encode.int dragPri ), ( "request_id", Encode.string requestId ) ] TaskUpdated
                ]

        "memory" ->
            let
                dragImp =
                    Dict.get drag.entityId model.memories |> Maybe.map .importance |> Maybe.withDefault 5

                targetImp =
                    Dict.get targetId model.memories |> Maybe.map .importance |> Maybe.withDefault 5
            in
            Cmd.batch
                [ Api.updateMemory apiUrl drag.entityId [ ( "importance", Encode.int targetImp ), ( "request_id", Encode.string requestId ) ] MemoryUpdated
                , Api.updateMemory apiUrl targetId [ ( "importance", Encode.int dragImp ), ( "request_id", Encode.string requestId ) ] MemoryUpdated
                ]

        _ ->
            Cmd.none


viewDropActionModal : Model -> Html Msg
viewDropActionModal model =
    case model.dragDrop.dropActionModal of
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
                        [ button [ class "btn btn-secondary", onClick CancelDropAction ] [ text "Cancel" ] ]
                    ]
                ]
