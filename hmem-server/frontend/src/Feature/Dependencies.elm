module Feature.Dependencies exposing
    ( handleEscape
    , init
    , update
    , viewTaskDependencies
    )

import Api
import Dict
import Feature.Focus exposing (buildTaskBreadcrumb)
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Toast exposing (addToast)
import Types exposing (..)


init : DependenciesModel
init =
    { taskDependencies = Dict.empty
    , addingDependencyFor = Nothing
    }


handleEscape : Model -> Maybe Model
handleEscape model =
    if model.dependencies.addingDependencyFor /= Nothing then
        let
            currentDependencies =
                model.dependencies
        in
        Just { model | dependencies = { currentDependencies | addingDependencyFor = Nothing } }

    else
        Nothing



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTaskDependencies taskId result ->
            case result of
                Ok overview ->
                    let
                        dependenciesModel =
                            model.dependencies
                    in
                    ( { model | dependencies = { dependenciesModel | taskDependencies = Dict.insert taskId overview.dependencies dependenciesModel.taskDependencies } }, Cmd.none )

                Err _ ->
                    addToast Error "Failed to load task dependencies" model

        StartAddDependency taskId ->
            let
                dependenciesModel =
                    model.dependencies
            in
            ( { model | dependencies = { dependenciesModel | addingDependencyFor = Just { taskId = taskId, search = "" } } }, Cmd.none )

        DependencySearch query ->
            case model.dependencies.addingDependencyFor of
                Just st ->
                    let
                        dependenciesModel =
                            model.dependencies
                    in
                    ( { model | dependencies = { dependenciesModel | addingDependencyFor = Just { st | search = query } } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CancelAddDependency ->
            let
                dependenciesModel =
                    model.dependencies
            in
            ( { model | dependencies = { dependenciesModel | addingDependencyFor = Nothing } }, Cmd.none )

        PerformAddDependency taskId dependsOnId ->
            let
                dependenciesModel =
                    model.dependencies

                ( trackedModel, requestId, trackCmd ) =
                    beginTrackedMutation [ taskId, dependsOnId ]
                        { model | dependencies = { dependenciesModel | addingDependencyFor = Nothing } }
            in
            ( trackedModel
            , Cmd.batch
                [ trackCmd
                , Api.addTaskDependency model.flags.apiUrl taskId dependsOnId requestId (DependencyMutationDone taskId)
                ]
            )

        PerformRemoveDependency taskId dependsOnId ->
            let
                ( trackedModel, requestId, trackCmd ) =
                    beginTrackedMutation [ taskId, dependsOnId ] model
            in
            ( trackedModel
            , Cmd.batch
                [ trackCmd
                , Api.removeTaskDependency model.flags.apiUrl taskId dependsOnId requestId (DependencyMutationDone taskId)
                ]
            )

        DependencyMutationDone taskId result ->
            case result of
                Ok () ->
                    ( model, Api.fetchTaskOverview model.flags.apiUrl taskId (GotTaskDependencies taskId) )

                Err _ ->
                    addToast Error "Failed to update dependency" model

        _ ->
            ( model, Cmd.none )



-- VIEW


viewTaskDependencies : Model -> String -> List Api.TaskDependencySummary -> Html Msg
viewTaskDependencies model taskId deps =
    let
        selectorOpen =
            case model.dependencies.addingDependencyFor of
                Just st ->
                    st.taskId == taskId

                Nothing ->
                    False
    in
    div [ class "task-dependencies-section" ]
        [ div [ class "task-dependencies-header" ]
            [ span [ class "task-dependencies-title" ] [ text ("Dependencies (" ++ String.fromInt (List.length deps) ++ ")") ]
            ]
        , if List.isEmpty deps then
            div [ class "task-dependencies-empty" ] [ text "No dependencies" ]

          else
            div [ class "task-dependencies-list" ]
                (List.map (viewDependencyItem model taskId) deps)
        , div [ class "card-inline-actions" ]
            [ div [ class "popover-anchor" ]
                [ button
                    [ class
                        (if selectorOpen then
                            "btn-inline-create popover-trigger-active"

                         else
                            "btn-inline-create"
                        )
                    , onClick
                        (if selectorOpen then
                            CancelAddDependency

                         else
                            StartAddDependency taskId
                        )
                    ]
                    [ text "+ Dep" ]
                , viewAddDependencyPopover model taskId deps
                ]
            ]
        ]


viewAddDependencyPopover : Model -> String -> List Api.TaskDependencySummary -> Html Msg
viewAddDependencyPopover model taskId deps =
    case model.dependencies.addingDependencyFor of
        Just st ->
            if st.taskId == taskId then
                let
                    depIds =
                        List.map .id deps

                    query =
                        String.toLower st.search

                    availableTasks =
                        model.tasks
                            |> Dict.values
                            |> List.filter (\t -> t.id /= taskId && not (List.member t.id depIds))
                            |> List.filter
                                (\t ->
                                    if String.isEmpty query then
                                        True

                                    else
                                        let
                                            crumbText =
                                                buildTaskBreadcrumb model t []
                                                    |> List.map (\( _, label, _ ) -> String.toLower label)
                                                    |> String.join " "
                                        in
                                        String.contains query (String.toLower t.title)
                                            || (t.description |> Maybe.map (\d -> String.contains query (String.toLower d)) |> Maybe.withDefault False)
                                            || String.contains query crumbText
                                )
                            |> List.sortBy (\t -> ( Api.taskStatusOrder t.status, negate t.priority, String.toLower t.title ))
                            |> List.take 15
                in
                div [ class "popover-container" ]
                    [ div [ class "popover-overlay", onClick CancelAddDependency ] []
                    , div [ class "popover-menu", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                        [ input
                            [ class "popover-search"
                            , placeholder "Search tasks..."
                            , value st.search
                            , onInput DependencySearch
                            , autofocus True
                            ]
                            []
                        , div [ class "popover-results" ]
                            (if List.isEmpty availableTasks then
                                [ div [ class "popover-empty" ] [ text "No matching tasks" ] ]

                             else
                                List.map
                                    (\t ->
                                        let
                                            crumbs =
                                                buildTaskBreadcrumb model t []
                                                    |> List.filter (\( eid, _, _ ) -> eid /= t.id)
                                        in
                                        div
                                            [ class "popover-card"
                                            , onClick (PerformAddDependency taskId t.id)
                                            ]
                                            [ div [ class "popover-card-header" ]
                                                [ span [ class ("entity-type-label " ++ (if t.parentId /= Nothing then "entity-type-subtask" else "entity-type-task")) ]
                                                    [ text
                                                        (if t.parentId /= Nothing then
                                                            "SUB"

                                                         else
                                                            "TSK"
                                                        )
                                                    ]
                                                , span [ class "popover-card-title" ] [ text t.title ]
                                                ]
                                            , if not (List.isEmpty crumbs) then
                                                div [ class "popover-card-breadcrumb" ]
                                                    (List.intersperse (span [] [ text " › " ])
                                                        (List.map (\( _, label, _ ) -> span [] [ text label ]) crumbs)
                                                    )

                                              else
                                                text ""
                                            , div [ class "popover-card-meta" ]
                                                [ span [ class ("popover-card-status card-status-" ++ Api.taskStatusToString t.status) ]
                                                    [ text (Api.taskStatusToString t.status |> String.replace "_" " ") ]
                                                , span [ class "popover-card-priority" ] [ text ("P" ++ String.fromInt t.priority) ]
                                                , case t.description of
                                                    Just d ->
                                                        span [ class "popover-card-desc" ] [ text d ]

                                                    Nothing ->
                                                        text ""
                                                ]
                                            ]
                                    )
                                    availableTasks
                            )
                        ]
                    ]

            else
                text ""

        Nothing ->
            text ""


viewDependencyItem : Model -> String -> Api.TaskDependencySummary -> Html Msg
viewDependencyItem model taskId dep =
    let
        depTask =
            Dict.get dep.id model.tasks
    in
    div [ class "dep-item popover-card" ]
        (case depTask of
            Just t ->
                let
                    crumbs =
                        buildTaskBreadcrumb model t []
                            |> List.filter (\( eid, _, _ ) -> eid /= t.id)
                in
                [ div [ class "popover-card-header" ]
                    [ span [ class ("entity-type-label " ++ (if t.parentId /= Nothing then "entity-type-subtask" else "entity-type-task")) ]
                        [ text
                            (if t.parentId /= Nothing then
                                "SUB"

                             else
                                "TSK"
                            )
                        ]
                    , span [ class "popover-card-title" ] [ text t.title ]
                    , div [ class "dep-item-actions" ]
                        [ button
                            [ class "btn-icon btn-jump"
                            , onClick (FocusEntity "task" dep.id)
                            , title "Jump to task"
                            ]
                            [ text "↗" ]
                        , button
                            [ class "btn-icon btn-danger"
                            , onClick (PerformRemoveDependency taskId dep.id)
                            , title "Remove dependency"
                            ]
                            [ text "✕" ]
                        ]
                    ]
                , if not (List.isEmpty crumbs) then
                    div [ class "popover-card-breadcrumb" ]
                        (List.intersperse (span [] [ text " › " ])
                            (List.map (\( _, label, _ ) -> span [] [ text label ]) crumbs)
                        )

                  else
                    text ""
                , div [ class "popover-card-meta" ]
                    [ span [ class ("popover-card-status card-status-" ++ Api.taskStatusToString t.status) ]
                        [ text (Api.taskStatusToString t.status |> String.replace "_" " ") ]
                    , span [ class "popover-card-priority" ] [ text ("P" ++ String.fromInt t.priority) ]
                    , case t.description of
                        Just d ->
                            span [ class "popover-card-desc" ] [ text d ]

                        Nothing ->
                            text ""
                    ]
                ]

            Nothing ->
                [ div [ class "popover-card-header" ]
                    [ span [ class "entity-type-label entity-type-task" ] [ text "TSK" ]
                    , span [ class "popover-card-title" ] [ text dep.name ]
                    , div [ class "dep-item-actions" ]
                        [ button
                            [ class "btn-icon btn-jump"
                            , onClick (FocusEntity "task" dep.id)
                            , title "Jump to task"
                            ]
                            [ text "↗" ]
                        , button
                            [ class "btn-icon btn-danger"
                            , onClick (PerformRemoveDependency taskId dep.id)
                            , title "Remove dependency"
                            ]
                            [ text "✕" ]
                        ]
                    ]
                ]
        )
