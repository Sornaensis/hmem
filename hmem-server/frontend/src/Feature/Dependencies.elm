module Feature.Dependencies exposing
    ( beginDependencyRefresh
    , cacheCompleteTaskDependencies
    , handleEscape
    , init
    , invalidateDependencyPage
    , prepareDependencyEventRefresh
    , resetCache
    , trackDependencyMutationRequest
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
import Http
import Json.Decode as Decode
import Permissions
import Toast exposing (addToast)
import Types exposing (..)


init : DependenciesModel
init =
    { taskDependencies = Dict.empty
    , taskDependencyLinks = []
    , taskReadinessRollups = Dict.empty
    , projectReadinessRollups = Dict.empty
    , addingDependencyFor = Nothing
    , taskDependencyHasMore = Dict.empty
    , taskDependencyNextOffset = Dict.empty
    , taskDependencyLoading = Dict.empty
    , taskDependencyRequests = Dict.empty
    , taskDependencyRefreshItems = Dict.empty
    , taskDependencyMutations = []
    , nextTaskDependencyRequestGeneration = 1
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


dependencyPageSize : Int
dependencyPageSize =
    50


deduplicateAndSortDependencies : List Api.TaskDependencySummary -> List Api.TaskDependencySummary
deduplicateAndSortDependencies items =
    items
        |> List.foldl
            (\item values ->
                if List.any (\value -> value.id == item.id) values then
                    values

                else
                    item :: values
            )
            []
        |> List.sortBy (\item -> ( String.toLower item.name, item.id ))


resetCache : DependenciesModel -> DependenciesModel
resetCache dependencies =
    { dependencies
        | taskDependencies = Dict.empty
        , taskDependencyLinks = []
        , taskReadinessRollups = Dict.empty
        , projectReadinessRollups = Dict.empty
        , addingDependencyFor = Nothing
        , taskDependencyHasMore = Dict.empty
        , taskDependencyNextOffset = Dict.empty
        , taskDependencyLoading = Dict.empty
        , taskDependencyRequests = Dict.empty
        , taskDependencyRefreshItems = Dict.empty
        , taskDependencyMutations = []
        , nextTaskDependencyRequestGeneration = dependencies.nextTaskDependencyRequestGeneration + 1
    }


cacheCompleteTaskDependencies : String -> List Api.TaskDependencySummary -> DependenciesModel -> DependenciesModel
cacheCompleteTaskDependencies taskId items dependencies =
    { dependencies
        | taskDependencies = Dict.insert taskId (deduplicateAndSortDependencies items) dependencies.taskDependencies
        , taskDependencyHasMore = Dict.insert taskId False dependencies.taskDependencyHasMore
        , taskDependencyNextOffset = Dict.insert taskId (List.length items) dependencies.taskDependencyNextOffset
        , taskDependencyLoading = Dict.insert taskId False dependencies.taskDependencyLoading
        , taskDependencyRequests = Dict.remove taskId dependencies.taskDependencyRequests
        , taskDependencyRefreshItems = Dict.remove taskId dependencies.taskDependencyRefreshItems
    }


prepareDependencyEventRefresh : String -> String -> String -> Maybe String -> Model -> ( Model, Bool )
prepareDependencyEventRefresh taskId dependsOnId action maybeRequestId model =
    case maybeRequestId |> Maybe.andThen (\requestId -> List.filter (\correlation -> correlation.requestId == requestId && correlation.taskId == taskId && correlation.dependsOnId == dependsOnId && correlation.action == action && Just correlation.workspaceId == model.selectedWorkspaceId && correlation.sessionEpoch == model.sessionRequestEpoch) model.dependencies.taskDependencyMutations |> List.head) of
        Just correlation ->
            let
                dependencies =
                    model.dependencies

                updatedCorrelations =
                    dependencies.taskDependencyMutations
                        |> List.map
                            (\candidate ->
                                if candidate.requestId == correlation.requestId && candidate.taskId == taskId && candidate.dependsOnId == dependsOnId && candidate.action == action && candidate.workspaceId == correlation.workspaceId && candidate.sessionEpoch == correlation.sessionEpoch then
                                    { candidate | echoSeen = True }

                                else
                                    candidate
                            )
            in
            ( { model | dependencies = { dependencies | taskDependencyMutations = updatedCorrelations } }
            , not correlation.httpSucceeded && not correlation.echoSeen
            )

        Nothing ->
            ( model, True )


trackDependencyMutationRequest : String -> String -> String -> String -> Model -> Model
trackDependencyMutationRequest taskId dependsOnId action requestId model =
    case model.selectedWorkspaceId of
        Just workspaceId ->
            let
                dependencies =
                    model.dependencies
            in
            { model
                | dependencies =
                    { dependencies
                        | taskDependencyMutations =
                            List.take 128
                                ({ requestId = requestId
                                 , taskId = taskId
                                 , dependsOnId = dependsOnId
                                 , action = action
                                 , workspaceId = workspaceId
                                 , sessionEpoch = model.sessionRequestEpoch
                                 , httpSucceeded = False
                                 , echoSeen = False
                                 }
                                    :: List.filter (\correlation -> correlation.requestId /= requestId) dependencies.taskDependencyMutations
                                )
                    }
            }

        Nothing ->
            model


applyDependencyPage : String -> DependencyPageRequest -> Result Http.Error Api.TaskDependencyPage -> Model -> ( Model, Cmd Msg )
applyDependencyPage taskId request result model =
    case result of
        Ok page ->
            let
                dependenciesModel =
                    model.dependencies

                nextOffset =
                    request.offset + List.length page.items
            in
            case Dict.get taskId dependenciesModel.taskDependencyRefreshItems of
                Just accumulatedItems ->
                    let
                        authoritativeItems =
                            deduplicateAndSortDependencies (accumulatedItems ++ page.items)
                    in
                    if page.hasMore && not (List.isEmpty page.items) then
                        let
                            nextRequest =
                                { request
                                    | offset = nextOffset
                                    , generation = dependenciesModel.nextTaskDependencyRequestGeneration
                                }

                            updated =
                                { dependenciesModel
                                    | taskDependencyLoading = Dict.insert taskId True dependenciesModel.taskDependencyLoading
                                    , taskDependencyRequests = Dict.insert taskId nextRequest dependenciesModel.taskDependencyRequests
                                    , taskDependencyRefreshItems = Dict.insert taskId authoritativeItems dependenciesModel.taskDependencyRefreshItems
                                    , nextTaskDependencyRequestGeneration = nextRequest.generation + 1
                                }
                        in
                        ( { model | dependencies = updated }
                        , Api.fetchTaskDependencyPage model.flags.apiUrl taskId nextOffset (GotTaskDependencyPage taskId request.workspaceId request.sessionEpoch nextRequest.generation nextOffset)
                        )

                    else if page.hasMore then
                        addToast Error
                            "Failed to refresh task dependencies"
                            { model
                                | dependencies =
                                    { dependenciesModel
                                        | taskDependencyLoading = Dict.insert taskId False dependenciesModel.taskDependencyLoading
                                        , taskDependencyRequests = Dict.remove taskId dependenciesModel.taskDependencyRequests
                                        , taskDependencyRefreshItems = Dict.remove taskId dependenciesModel.taskDependencyRefreshItems
                                    }
                            }

                    else
                        ( { model
                            | dependencies =
                                { dependenciesModel
                                    | taskDependencies = Dict.insert taskId authoritativeItems dependenciesModel.taskDependencies
                                    , taskDependencyHasMore = Dict.insert taskId False dependenciesModel.taskDependencyHasMore
                                    , taskDependencyNextOffset = Dict.insert taskId nextOffset dependenciesModel.taskDependencyNextOffset
                                    , taskDependencyLoading = Dict.insert taskId False dependenciesModel.taskDependencyLoading
                                    , taskDependencyRequests = Dict.remove taskId dependenciesModel.taskDependencyRequests
                                    , taskDependencyRefreshItems = Dict.remove taskId dependenciesModel.taskDependencyRefreshItems
                                }
                          }
                        , Cmd.none
                        )

                Nothing ->
                    let
                        existing =
                            if request.offset == 0 && not page.hasMore then
                                []

                            else
                                Dict.get taskId dependenciesModel.taskDependencies |> Maybe.withDefault []

                        ordered =
                            deduplicateAndSortDependencies (existing ++ page.items)
                    in
                    ( { model
                        | dependencies =
                            { dependenciesModel
                                | taskDependencies = Dict.insert taskId ordered dependenciesModel.taskDependencies
                                , taskDependencyHasMore = Dict.insert taskId page.hasMore dependenciesModel.taskDependencyHasMore
                                , taskDependencyNextOffset = Dict.insert taskId nextOffset dependenciesModel.taskDependencyNextOffset
                                , taskDependencyLoading = Dict.insert taskId False dependenciesModel.taskDependencyLoading
                                , taskDependencyRequests = Dict.remove taskId dependenciesModel.taskDependencyRequests
                                , taskDependencyRefreshItems = Dict.remove taskId dependenciesModel.taskDependencyRefreshItems
                            }
                      }
                    , Cmd.none
                    )

        Err _ ->
            let
                dependenciesModel =
                    model.dependencies
            in
            addToast Error
                "Failed to load task dependencies"
                { model
                    | dependencies =
                        { dependenciesModel
                            | taskDependencyLoading = Dict.insert taskId False dependenciesModel.taskDependencyLoading
                            , taskDependencyRequests = Dict.remove taskId dependenciesModel.taskDependencyRequests
                            , taskDependencyRefreshItems = Dict.remove taskId dependenciesModel.taskDependencyRefreshItems
                        }
                }


beginDependencyRefresh : String -> Model -> ( Model, Cmd Msg )
beginDependencyRefresh taskId model =
    case model.selectedWorkspaceId of
        Just workspaceId ->
            let
                dependencies =
                    model.dependencies

                request =
                    { workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch, offset = 0, generation = dependencies.nextTaskDependencyRequestGeneration }

                cachedItems =
                    Dict.get taskId dependencies.taskDependencies |> Maybe.withDefault []

                refreshAll =
                    Dict.get taskId dependencies.taskDependencyHasMore
                        /= Just True
                        && List.length cachedItems
                        >= dependencyPageSize

                updated =
                    { dependencies
                        | taskDependencyLoading = Dict.insert taskId True dependencies.taskDependencyLoading
                        , taskDependencyRequests = Dict.insert taskId request dependencies.taskDependencyRequests
                        , taskDependencyRefreshItems =
                            if refreshAll then
                                Dict.insert taskId [] dependencies.taskDependencyRefreshItems

                            else
                                Dict.remove taskId dependencies.taskDependencyRefreshItems
                        , nextTaskDependencyRequestGeneration = request.generation + 1
                    }
            in
            ( { model | dependencies = updated }, Api.fetchTaskDependencyPage model.flags.apiUrl taskId 0 (GotTaskDependencyPage taskId workspaceId model.sessionRequestEpoch request.generation 0) )

        Nothing ->
            ( model, Cmd.none )


invalidateDependencyPage : String -> Model -> Model
invalidateDependencyPage taskId model =
    let
        dependencies =
            model.dependencies
    in
    { model
        | dependencies =
            { dependencies
                | taskDependencies = Dict.remove taskId dependencies.taskDependencies
                , taskDependencyHasMore = Dict.remove taskId dependencies.taskDependencyHasMore
                , taskDependencyNextOffset = Dict.remove taskId dependencies.taskDependencyNextOffset
                , taskDependencyLoading = Dict.remove taskId dependencies.taskDependencyLoading
                , taskDependencyRequests = Dict.remove taskId dependencies.taskDependencyRequests
                , taskDependencyRefreshItems = Dict.remove taskId dependencies.taskDependencyRefreshItems
                , nextTaskDependencyRequestGeneration = dependencies.nextTaskDependencyRequestGeneration + 1
            }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTaskDependencyPage taskId workspaceId sessionEpoch generation offset result ->
            case Dict.get taskId model.dependencies.taskDependencyRequests of
                Just request ->
                    if request.workspaceId /= workspaceId || request.sessionEpoch /= sessionEpoch || request.generation /= generation || request.offset /= offset || model.sessionRequestEpoch /= sessionEpoch || model.selectedWorkspaceId /= Just workspaceId then
                        ( model, Cmd.none )

                    else
                        applyDependencyPage taskId request result model

                Nothing ->
                    ( model, Cmd.none )

        LoadTaskDependencyPage taskId ->
            let
                dependenciesModel =
                    model.dependencies

                offset =
                    Dict.get taskId dependenciesModel.taskDependencyNextOffset
                        |> Maybe.withDefault 0

                canLoad =
                    (Dict.get taskId dependenciesModel.taskDependencyHasMore
                        |> Maybe.withDefault False
                    )
                        && not (Dict.get taskId dependenciesModel.taskDependencyLoading |> Maybe.withDefault False)
            in
            case model.selectedWorkspaceId of
                Just workspaceId ->
                    if canLoad then
                        let
                            request =
                                { workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch, offset = offset, generation = dependenciesModel.nextTaskDependencyRequestGeneration }

                            updated =
                                { dependenciesModel
                                    | taskDependencyLoading = Dict.insert taskId True dependenciesModel.taskDependencyLoading
                                    , taskDependencyRequests = Dict.insert taskId request dependenciesModel.taskDependencyRequests
                                    , taskDependencyRefreshItems = Dict.remove taskId dependenciesModel.taskDependencyRefreshItems
                                    , nextTaskDependencyRequestGeneration = request.generation + 1
                                }
                        in
                        ( { model | dependencies = updated }
                        , Api.fetchTaskDependencyPage model.flags.apiUrl taskId offset (GotTaskDependencyPage taskId workspaceId model.sessionRequestEpoch request.generation offset)
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GotTaskDependencies taskId workspaceId sessionEpoch dependencyGeneration result ->
            if model.auth.status /= AuthReady || model.sessionContext == Nothing || model.selectedWorkspaceId /= workspaceId || not (Permissions.canReadCurrentWorkspace model) || not (Dict.get taskId model.tasks |> Maybe.map (\task -> Just task.workspaceId == workspaceId) |> Maybe.withDefault False) || model.sessionRequestEpoch /= sessionEpoch || model.dependencies.nextTaskDependencyRequestGeneration /= dependencyGeneration || Dict.member taskId model.dependencies.taskDependencyRequests || Dict.member taskId model.dependencies.taskDependencyRefreshItems then
                ( model, Cmd.none )

            else
                case result of
                    Ok overview ->
                        if overview.task.id == taskId && Just overview.task.workspaceId == workspaceId then
                            let
                                dependenciesModel =
                                    model.dependencies
                            in
                            ( { model
                                | dependencies =
                                    cacheCompleteTaskDependencies taskId overview.dependencies dependenciesModel
                                        |> (\updated -> { updated | taskReadinessRollups = Dict.insert taskId overview.readinessRollup updated.taskReadinessRollups })
                              }
                            , Cmd.none
                            )

                        else
                            ( model, Cmd.none )

                    Err _ ->
                        addToast Error "Failed to load task dependencies" model

        GotProjectOverview projectId result ->
            case result of
                Ok overview ->
                    let
                        dependenciesModel =
                            model.dependencies
                    in
                    ( { model
                        | dependencies =
                            { dependenciesModel
                                | projectReadinessRollups = Dict.insert projectId overview.readinessRollup dependenciesModel.projectReadinessRollups
                            }
                      }
                    , Cmd.none
                    )

                Err _ ->
                    addToast Error "Failed to load project readiness" model

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

                correlatedModel =
                    trackDependencyMutationRequest taskId dependsOnId "add" requestId trackedModel
            in
            ( correlatedModel
            , Cmd.batch
                [ trackCmd
                , Api.addTaskDependency model.flags.apiUrl taskId dependsOnId requestId (DependencyMutationDone taskId requestId)
                ]
            )

        PerformRemoveDependency taskId dependsOnId ->
            let
                ( trackedModel, requestId, trackCmd ) =
                    beginTrackedMutation [ taskId, dependsOnId ] model

                correlatedModel =
                    trackDependencyMutationRequest taskId dependsOnId "remove" requestId trackedModel
            in
            ( correlatedModel
            , Cmd.batch
                [ trackCmd
                , Api.removeTaskDependency model.flags.apiUrl taskId dependsOnId requestId (DependencyMutationDone taskId requestId)
                ]
            )

        DependencyMutationDone taskId requestId result ->
            let
                correlation =
                    model.dependencies.taskDependencyMutations
                        |> List.filter
                            (\candidate ->
                                candidate.requestId
                                    == requestId
                                    && candidate.taskId
                                    == taskId
                                    && candidate.workspaceId
                                    == (model.selectedWorkspaceId |> Maybe.withDefault "")
                                    && candidate.sessionEpoch
                                    == model.sessionRequestEpoch
                            )
                        |> List.head
            in
            case ( correlation, result ) of
                ( Just currentCorrelation, Ok mutationResult ) ->
                    if mutationResult.taskId /= currentCorrelation.taskId || mutationResult.dependsOnId /= currentCorrelation.dependsOnId || mutationResult.action /= currentCorrelation.action then
                        ( model, Cmd.none )

                    else
                        let
                            dependencies =
                                model.dependencies

                            correlatedModel =
                                { model
                                    | dependencies =
                                        { dependencies
                                            | taskDependencyMutations =
                                                dependencies.taskDependencyMutations
                                                    |> List.map
                                                        (\candidate ->
                                                            if candidate.requestId == requestId && candidate.taskId == taskId then
                                                                { candidate | httpSucceeded = True }

                                                            else
                                                                candidate
                                                        )
                                        }
                                }

                            updatedModel =
                                applyDependencyMutationResult mutationResult correlatedModel

                            -- Revalidate only the mutated task. Complete caches use
                            -- the bounded authoritative crawl; request correlation
                            -- keeps the reconciled local state visible on failure.
                            ( revalidatedModel, revalidationCmd ) =
                                if currentCorrelation.echoSeen then
                                    ( updatedModel, Cmd.none )

                                else
                                    beginDependencyRefresh taskId updatedModel
                        in
                        ( revalidatedModel
                        , revalidationCmd
                        )

                ( Just _, Err _ ) ->
                    let
                        dependencies =
                            model.dependencies
                    in
                    addToast Error
                        "Failed to update dependency"
                        { model | dependencies = { dependencies | taskDependencyMutations = List.filter (\candidate -> candidate.requestId /= requestId || candidate.taskId /= taskId) dependencies.taskDependencyMutations } }

                ( Nothing, _ ) ->
                    ( model, Cmd.none )

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
        , if Dict.get taskId model.dependencies.taskDependencyHasMore |> Maybe.withDefault False then
            button
                [ class "btn-small btn-ghost"
                , disabled (Dict.get taskId model.dependencies.taskDependencyLoading |> Maybe.withDefault False)
                , onClick (LoadTaskDependencyPage taskId)
                ]
                [ text
                    (if Dict.get taskId model.dependencies.taskDependencyLoading |> Maybe.withDefault False then
                        "Loading dependencies…"

                     else
                        "Load more dependencies"
                    )
                ]

          else
            text ""
        , if Permissions.canEditCurrentWorkspace model then
            div [ class "card-inline-actions" ]
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

          else
            text ""
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
                                                [ span
                                                    [ class
                                                        ("entity-type-label "
                                                            ++ (if t.parentId /= Nothing then
                                                                    "entity-type-subtask"

                                                                else
                                                                    "entity-type-task"
                                                               )
                                                        )
                                                    ]
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
                                                [ span [ class (taskPopoverStatusClass t.status), title (taskStatusTitle t.status) ]
                                                    [ text (taskStatusDisplayText t.status) ]
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
                    [ span
                        [ class
                            ("entity-type-label "
                                ++ (if t.parentId /= Nothing then
                                        "entity-type-subtask"

                                    else
                                        "entity-type-task"
                                   )
                            )
                        ]
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
                        , if Permissions.canEditCurrentWorkspace model then
                            button
                                [ class "btn-icon btn-danger"
                                , onClick (PerformRemoveDependency taskId dep.id)
                                , title "Remove dependency"
                                ]
                                [ text "✕" ]

                          else
                            text ""
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
                    [ span [ class (taskPopoverStatusClass t.status), title (taskStatusTitle t.status) ]
                        [ text (taskStatusDisplayText t.status) ]
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
                        , if Permissions.canEditCurrentWorkspace model then
                            button
                                [ class "btn-icon btn-danger"
                                , onClick (PerformRemoveDependency taskId dep.id)
                                , title "Remove dependency"
                                ]
                                [ text "✕" ]

                          else
                            text ""
                        ]
                    ]
                ]
        )
