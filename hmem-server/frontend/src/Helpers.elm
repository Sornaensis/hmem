module Helpers exposing (..)

import Api
import Browser.Dom
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Ports exposing (saveToLocalStorage)
import Process
import String
import Task as ElmTask
import Types exposing (..)



-- FRAGMENT / URL


parseFragment : Maybe String -> { tab : WorkspaceTab, focus : Maybe ( String, String ) }
parseFragment fragment =
    case fragment of
        Nothing ->
            { tab = ProjectsTab, focus = Nothing }

        Just frag ->
            let
                pairs =
                    String.split "&" frag
                        |> List.filterMap
                            (\s ->
                                case String.split "=" s of
                                    [ k, v ] ->
                                        Just ( k, v )

                                    _ ->
                                        Nothing
                            )

                tabVal =
                    List.foldl
                        (\( k, v ) acc ->
                            if k == "tab" then
                                v

                            else
                                acc
                        )
                        "projects"
                        pairs

                tab =
                    case tabVal of
                        "memories" ->
                            MemoriesTab

                        "timeline" ->
                            TimelineTab

                        "audit" ->
                            AuditTab

                        _ ->
                            ProjectsTab

                focusVal =
                    List.foldl
                        (\( k, v ) acc ->
                            if k == "focus" then
                                Just v

                            else
                                acc
                        )
                        Nothing
                        pairs

                focus =
                    focusVal
                        |> Maybe.andThen
                            (\v ->
                                case String.split ":" v of
                                    [ t, id ] ->
                                        Just ( t, id )

                                    _ ->
                                        Nothing
                            )
            in
            { tab = tab, focus = focus }


buildFragment : WorkspaceTab -> Maybe ( String, String ) -> String
buildFragment tab focus =
    let
        tabPart =
            case tab of
                ProjectsTab ->
                    "tab=projects"

                MemoriesTab ->
                    "tab=memories"

                TimelineTab ->
                    "tab=timeline"

                AuditTab ->
                    "tab=audit"

        focusPart =
            case focus of
                Just ( t, id ) ->
                    "&focus=" ++ t ++ ":" ++ id

                Nothing ->
                    ""
    in
    tabPart ++ focusPart


replaceFragment : Model -> Cmd Msg
replaceFragment model =
    case model.selectedWorkspaceId of
        Just wsId ->
            Nav.replaceUrl model.key ("/workspace/" ++ wsId ++ "#" ++ buildFragment model.activeTab model.focus.focusedEntity)

        Nothing ->
            Cmd.none



-- FILTER PERSISTENCE


localStorageKey : String -> String
localStorageKey wsId =
    "hmem-ws-" ++ wsId


encodeFilterState : Model -> Encode.Value
encodeFilterState model =
    Encode.object
        [ ( "workspaceId"
          , case model.selectedWorkspaceId of
                Just wsId ->
                    Encode.string wsId

                Nothing ->
                    Encode.null
          )
        , ( "searchQuery", Encode.string model.search.query )
        , ( "filterShowOnly"
          , Encode.string
                (case model.search.filterShowOnly of
                    ShowAll ->
                        "all"

                    ShowProjectsOnly ->
                        "projects"

                    ShowTasksOnly ->
                        "tasks"
                )
          )
        , ( "filterPriority", encodeFilterPriority model.search.filterPriority )
        , ( "filterProjectStatuses", Encode.list Encode.string model.search.filterProjectStatuses )
        , ( "filterTaskStatuses", Encode.list Encode.string model.search.filterTaskStatuses )
        , ( "filterMemoryTypes", Encode.list Encode.string model.search.filterMemoryTypes )
        , ( "filterImportance", encodeFilterPriority model.search.filterImportance )
        , ( "filterMemoryPinned"
          , case model.search.filterMemoryPinned of
                Just True ->
                    Encode.string "true"

                Just False ->
                    Encode.string "false"

                Nothing ->
                    Encode.null
          )
        , ( "filterMemoryActiveLinked", Encode.bool model.search.filterMemoryActiveLinked )
        , ( "filterTags", Encode.list Encode.string model.search.filterTags )
        , ( "collapsedNodes"
          , model.cards.collapsedNodes
                |> Dict.toList
                |> List.filter (\( _, v ) -> v)
                |> List.map (\( k, _ ) -> k)
                |> Encode.list Encode.string
          )
        ]


encodeFilterPriority : FilterPriority -> Encode.Value
encodeFilterPriority fp =
    case fp of
        AnyPriority ->
            Encode.null

        ExactPriority n ->
            Encode.object [ ( "exact", Encode.int n ) ]

        AbovePriority n ->
            Encode.object [ ( "above", Encode.int n ) ]

        BelowPriority n ->
            Encode.object [ ( "below", Encode.int n ) ]


decodeFilterPriority : Decode.Decoder FilterPriority
decodeFilterPriority =
    Decode.oneOf
        [ Decode.null AnyPriority
        , Decode.map ExactPriority (Decode.field "exact" Decode.int)
        , Decode.map AbovePriority (Decode.field "above" Decode.int)
        , Decode.map BelowPriority (Decode.field "below" Decode.int)
        ]


applyStoredFilters : Encode.Value -> Model -> Model
applyStoredFilters json model =
    let
        decodeShowOnly =
            Decode.string
                |> Decode.andThen
                    (\s ->
                        case s of
                            "projects" ->
                                Decode.succeed ShowProjectsOnly

                            "tasks" ->
                                Decode.succeed ShowTasksOnly

                            _ ->
                                Decode.succeed ShowAll
                    )

        decodePinned =
            Decode.oneOf
                [ Decode.null Nothing
                , Decode.string
                    |> Decode.andThen
                        (\s ->
                            if s == "true" then
                                Decode.succeed (Just True)

                            else
                                Decode.succeed (Just False)
                        )
                ]

        decodeCollapsed =
            Decode.list Decode.string
                |> Decode.map (\ids -> Dict.fromList (List.map (\id -> ( id, True )) ids))

        currentSearch =
            model.search

        updatedSearch =
            { currentSearch
                | query = Decode.decodeValue (Decode.field "searchQuery" Decode.string) json |> Result.withDefault currentSearch.query
                , filterShowOnly = Decode.decodeValue (Decode.field "filterShowOnly" decodeShowOnly) json |> Result.withDefault currentSearch.filterShowOnly
                , filterPriority = Decode.decodeValue (Decode.field "filterPriority" decodeFilterPriority) json |> Result.withDefault currentSearch.filterPriority
                , filterProjectStatuses = Decode.decodeValue (Decode.field "filterProjectStatuses" (Decode.list Decode.string)) json |> Result.withDefault currentSearch.filterProjectStatuses
                , filterTaskStatuses = Decode.decodeValue (Decode.field "filterTaskStatuses" (Decode.list Decode.string)) json |> Result.withDefault currentSearch.filterTaskStatuses
                , filterMemoryTypes = Decode.decodeValue (Decode.field "filterMemoryTypes" (Decode.list Decode.string)) json |> Result.withDefault currentSearch.filterMemoryTypes
                , filterImportance = Decode.decodeValue (Decode.field "filterImportance" decodeFilterPriority) json |> Result.withDefault currentSearch.filterImportance
                , filterMemoryPinned = Decode.decodeValue (Decode.field "filterMemoryPinned" decodePinned) json |> Result.withDefault currentSearch.filterMemoryPinned
                , filterMemoryActiveLinked = Decode.decodeValue (Decode.field "filterMemoryActiveLinked" Decode.bool) json |> Result.withDefault currentSearch.filterMemoryActiveLinked
                , filterTags = Decode.decodeValue (Decode.field "filterTags" (Decode.list Decode.string)) json |> Result.withDefault currentSearch.filterTags
            }

        currentCards =
            model.cards

        updatedCards =
            { currentCards
                | collapsedNodes = Decode.decodeValue (Decode.field "collapsedNodes" decodeCollapsed) json |> Result.withDefault currentCards.collapsedNodes
            }
    in
    { model
        | search = updatedSearch
        , cards = updatedCards
    }


applyStoredFiltersIfCurrentWorkspace : Encode.Value -> Model -> Model
applyStoredFiltersIfCurrentWorkspace json model =
    let
        storedWorkspaceId =
            Decode.decodeValue (Decode.field "workspaceId" (Decode.nullable Decode.string)) json
                |> Result.withDefault Nothing
    in
    case ( model.selectedWorkspaceId, storedWorkspaceId ) of
        ( Just currentWsId, Just storedWsId ) ->
            if currentWsId == storedWsId then
                applyStoredFilters json model

            else
                model

        ( Just _, Nothing ) ->
            model

        _ ->
            applyStoredFilters json model


saveFiltersCmd : Model -> Cmd Msg
saveFiltersCmd model =
    case model.selectedWorkspaceId of
        Just wsId ->
            saveToLocalStorage
                (Encode.object
                    [ ( "key", Encode.string (localStorageKey wsId) )
                    , ( "value", encodeFilterState model )
                    ]
                )

        Nothing ->
            Cmd.none


beginWorkspaceDataReload : Bool -> Model -> ( Model, Cmd Msg )
beginWorkspaceDataReload showLoading model =
    case model.selectedWorkspaceId of
        Just wsId ->
            let
                currentDataLoading =
                    model.dataLoading

                token =
                    currentDataLoading.nextWorkspaceLoadToken

                updatedDataLoading =
                    { currentDataLoading
                        | activeWorkspaceLoadToken = Just token
                        , nextWorkspaceLoadToken = token + 1
                        , loadingWorkspaceData = if showLoading then True else currentDataLoading.loadingWorkspaceData
                        , pendingWorkspaceLoads = 4
                        , cardHydrationLoaded = False
                    }
            in
            ( { model | dataLoading = updatedDataLoading }
            , Cmd.batch
                [ Api.fetchProjects model.flags.apiUrl wsId (GotProjects wsId (Just token) 0)
                , Api.fetchTasks model.flags.apiUrl wsId (GotTasks wsId (Just token) 0)
                , Api.fetchMemories model.flags.apiUrl wsId (GotMemories wsId (Just token) 0)
                , Api.fetchWorkspaceCardHydration model.flags.apiUrl wsId (GotWorkspaceCardHydration wsId (Just token))
                ]
            )

        Nothing ->
            ( model, Cmd.none )



-- MUTATION TRACKING


{-| Mark an entity ID as recently mutated locally. Returns the updated model
and a Cmd that clears the flag after 3 seconds.
-}
trackLocalMutation : String -> Model -> ( Model, Cmd Msg )
trackLocalMutation entityId model =
    let
        ( updatedModel, _, clearCmd ) =
            beginTrackedMutation [ entityId ] model
    in
    ( updatedModel, clearCmd )


trackLocalMutations : List String -> Model -> ( Model, Cmd Msg )
trackLocalMutations entityIds model =
    let
        ( updatedModel, _, clearCmd ) =
            beginTrackedMutation entityIds model
    in
    ( updatedModel, clearCmd )


insertTasks : List Api.Task -> Model -> Model
insertTasks tasks model =
    { model
        | tasks =
            List.foldl
                (\task acc -> Dict.insert task.id task acc)
                model.tasks
                tasks
    }


applyDependencyStatusChanges : List Api.TaskDependencyStatusChange -> Model -> Model
applyDependencyStatusChanges changes model =
    insertTasks (List.map .task changes) model


applyDependencyMutationResult : Api.DependencyMutationResult -> Model -> Model
applyDependencyMutationResult result model =
    applyDependencyStatusChanges result.affectedTasks model


applyTaskMutationResult : Api.TaskMutationResult -> Model -> Model
applyTaskMutationResult result model =
    insertTasks (result.task :: List.map .task result.dependencyEffects) model


taskMutationResultIds : Api.TaskMutationResult -> List String
taskMutationResultIds result =
    result.task.id :: List.map (\change -> change.task.id) result.dependencyEffects


linkedMemoriesForEntity : Model -> String -> List Api.Memory
linkedMemoriesForEntity model entityId =
    case Dict.get entityId model.memory.entityMemories of
        Just memories ->
            memories

        Nothing ->
            model.memory.entityMemoryIds
                |> Dict.get entityId
                |> Maybe.withDefault []
                |> List.filterMap (\memoryId -> Dict.get memoryId model.memories)


hasLinkedMemoryData : Model -> String -> Bool
hasLinkedMemoryData model entityId =
    Dict.member entityId model.memory.entityMemories
        || model.dataLoading.cardHydrationLoaded


taskDependencySummariesForTask : Model -> String -> List Api.TaskDependencySummary
taskDependencySummariesForTask model taskId =
    case Dict.get taskId model.dependencies.taskDependencies of
        Just dependencies ->
            dependencies

        Nothing ->
            model.dependencies.taskDependencyLinks
                |> List.filter (\link -> link.taskId == taskId)
                |> List.filterMap
                    (\link ->
                        Dict.get link.dependsOnId model.tasks
                            |> Maybe.map (\task -> { id = task.id, name = task.title })
                    )


hasTaskDependencyData : Model -> String -> Bool
hasTaskDependencyData model taskId =
    Dict.member taskId model.dependencies.taskDependencies
        || model.dataLoading.cardHydrationLoaded


applyTaskDependencyLinkMutation : Api.DependencyMutationResult -> List Api.WorkspaceTaskDependencyLink -> List Api.WorkspaceTaskDependencyLink
applyTaskDependencyLinkMutation result links =
    let
        sameLink link =
            link.taskId == result.taskId && link.dependsOnId == result.dependsOnId
    in
    case result.action of
        "add" ->
            if List.any sameLink links then
                links

            else
                links ++ [ { taskId = result.taskId, dependsOnId = result.dependsOnId } ]

        "remove" ->
            List.filter (not << sameLink) links

        _ ->
            links


taskReadinessRollupForTask : Model -> String -> Maybe Api.TaskReadinessRollup
taskReadinessRollupForTask model taskId =
    case Dict.get taskId model.dependencies.taskReadinessRollups of
        Just rollup ->
            Just rollup

        Nothing ->
            if model.dataLoading.cardHydrationLoaded then
                Just (computeTaskReadinessRollup model taskId)

            else
                Nothing


projectReadinessRollupForProject : Model -> String -> Maybe Api.ProjectReadinessRollup
projectReadinessRollupForProject model projectId =
    case Dict.get projectId model.dependencies.projectReadinessRollups of
        Just rollup ->
            Just rollup

        Nothing ->
            if model.dataLoading.cardHydrationLoaded then
                Just (computeProjectReadinessRollup model projectId)

            else
                Nothing


computeTaskReadinessRollup : Model -> String -> Api.TaskReadinessRollup
computeTaskReadinessRollup model taskId =
    computeTaskReadinessRollupFrom (Dict.values model.tasks) model.dependencies.taskDependencyLinks taskId


computeTaskReadinessRollupFrom : List Api.Task -> List Api.WorkspaceTaskDependencyLink -> String -> Api.TaskReadinessRollup
computeTaskReadinessRollupFrom allTasks taskDependencyLinks taskId =
    let
        tasksById =
            indexBy .id allTasks

        treeIds =
            collectDescendantTaskIds allTasks taskId |> uniqueStringList

        descendantTasks =
            treeIds
                |> List.filter ((/=) taskId)
                |> List.filterMap (\id -> Dict.get id tasksById)

        openDependencyEdges =
            treeIds
                |> List.filterMap (\id -> Dict.get id tasksById)
                |> List.filter (\task -> isOpenTaskCardStatus task.status)
                |> List.concatMap
                    (\task ->
                        taskDependencyLinks
                            |> List.filter (\link -> link.taskId == task.id)
                            |> List.filterMap
                                (\link ->
                                    Dict.get link.dependsOnId tasksById
                                        |> Maybe.andThen
                                            (\dependency ->
                                                if isOpenTaskCardStatus dependency.status then
                                                    Just ( task.id, dependency.id )

                                                else
                                                    Nothing
                                            )
                                )
                    )
                |> uniquePairs

        dependencyBlockedTaskCount =
            openDependencyEdges
                |> List.map Tuple.first
                |> uniqueStringList
                |> List.length

        openDependencyCount =
            List.length openDependencyEdges
    in
    { openSubtaskCount = descendantTasks |> List.filter (\task -> isOpenTaskCardStatus task.status) |> List.length
    , doneSubtaskCount = descendantTasks |> List.filter (\task -> task.status == Api.Done) |> List.length
    , cancelledSubtaskCount = descendantTasks |> List.filter (\task -> task.status == Api.Cancelled) |> List.length
    , blockedSubtaskCount = descendantTasks |> List.filter (\task -> task.status == Api.Blocked) |> List.length
    , dependencyBlockedTaskCount = dependencyBlockedTaskCount
    , openDependencyCount = openDependencyCount
    , completionReady = not (List.any (\task -> isOpenTaskCardStatus task.status) descendantTasks)
    }


computeProjectReadinessRollup : Model -> String -> Api.ProjectReadinessRollup
computeProjectReadinessRollup model projectId =
    computeProjectReadinessRollupFrom (Dict.values model.projects) (Dict.values model.tasks) model.dependencies.taskDependencyLinks projectId


computeProjectReadinessRollupFrom : List Api.Project -> List Api.Task -> List Api.WorkspaceTaskDependencyLink -> String -> Api.ProjectReadinessRollup
computeProjectReadinessRollupFrom allProjects allTasks taskDependencyLinks projectId =
    let
        projectsById =
            indexBy .id allProjects

        tasksById =
            indexBy .id allTasks

        projectTreeIds =
            collectDescendantProjectIds allProjects projectId |> uniqueStringList

        descendantProjects =
            projectTreeIds
                |> List.filter ((/=) projectId)
                |> List.filterMap (\id -> Dict.get id projectsById)

        seededTaskIds =
            allTasks
                |> List.filter (\task -> task.projectId |> Maybe.map (\pid -> List.member pid projectTreeIds) |> Maybe.withDefault False)
                |> List.concatMap (\task -> collectDescendantTaskIds allTasks task.id)
                |> uniqueStringList

        projectTasks =
            seededTaskIds
                |> List.filterMap (\id -> Dict.get id tasksById)

        openDependencyEdges =
            projectTasks
                |> List.filter (\task -> isOpenTaskCardStatus task.status)
                |> List.concatMap
                    (\task ->
                        taskDependencyLinks
                            |> List.filter (\link -> link.taskId == task.id)
                            |> List.filterMap
                                (\link ->
                                    Dict.get link.dependsOnId tasksById
                                        |> Maybe.andThen
                                            (\dependency ->
                                                if isOpenTaskCardStatus dependency.status then
                                                    Just ( task.id, dependency.id )

                                                else
                                                    Nothing
                                            )
                                )
                    )
                |> uniquePairs
    in
    { openProjectCount = descendantProjects |> List.filter (\project -> isOpenProjectCardStatus project.status) |> List.length
    , closedProjectCount = descendantProjects |> List.filter (\project -> project.status == Api.ProjCompleted || project.status == Api.ProjArchived) |> List.length
    , openTaskCount = projectTasks |> List.filter (\task -> isOpenTaskCardStatus task.status) |> List.length
    , doneTaskCount = projectTasks |> List.filter (\task -> task.status == Api.Done) |> List.length
    , cancelledTaskCount = projectTasks |> List.filter (\task -> task.status == Api.Cancelled) |> List.length
    , blockedTaskCount = projectTasks |> List.filter (\task -> task.status == Api.Blocked) |> List.length
    , dependencyBlockedTaskCount = openDependencyEdges |> List.map Tuple.first |> uniqueStringList |> List.length
    , openDependencyCount = List.length openDependencyEdges
    , completionReady =
        not (List.any (\project -> isOpenProjectCardStatus project.status) descendantProjects)
            && not (List.any (\task -> isOpenTaskCardStatus task.status) projectTasks)
    }


isOpenTaskCardStatus : Api.TaskStatus -> Bool
isOpenTaskCardStatus status =
    status == Api.Todo || status == Api.InProgress || status == Api.Blocked


isOpenProjectCardStatus : Api.ProjectStatus -> Bool
isOpenProjectCardStatus status =
    status == Api.ProjActive || status == Api.ProjPaused


uniqueStringList : List String -> List String
uniqueStringList strings =
    List.foldl
        (\item acc ->
            if List.member item acc then
                acc

            else
                acc ++ [ item ]
        )
        []
        strings


uniquePairs : List ( String, String ) -> List ( String, String )
uniquePairs pairs =
    List.foldl
        (\pair acc ->
            if List.member pair acc then
                acc

            else
                acc ++ [ pair ]
        )
        []
        pairs


beginTrackedMutation : List String -> Model -> ( Model, String, Cmd Msg )
beginTrackedMutation entityIds model =
    let
        currentMutations =
            model.mutations

        requestId =
            model.flags.sessionId ++ "-req-" ++ String.fromInt currentMutations.nextRequestId

        updatedMutations =
            { currentMutations
                | pendingMutationIds =
                    List.foldl (\entityId acc -> Dict.insert entityId True acc) model.mutations.pendingMutationIds entityIds
                , pendingRequestIds = Dict.insert requestId True model.mutations.pendingRequestIds
                , nextRequestId = currentMutations.nextRequestId + 1
            }
    in
    ( { model | mutations = updatedMutations }
    , requestId
    , Cmd.batch
        ((Process.sleep 3000 |> ElmTask.perform (\_ -> ClearPendingRequest requestId))
            :: (entityIds
                    |> List.map (\entityId -> Process.sleep 3000 |> ElmTask.perform (\_ -> ClearPendingMutation entityId))
               )
        )
    )



-- UTILITIES


indexBy : (a -> comparable) -> List a -> Dict comparable a
indexBy key items =
    List.foldl (\item acc -> Dict.insert (key item) item acc) Dict.empty items


taskStatusDisplayText : Api.TaskStatus -> String
taskStatusDisplayText status =
    case status of
        Api.Blocked ->
            "blocked by dependencies"

        _ ->
            Api.taskStatusToString status |> String.replace "_" " "


taskStatusTitle : Api.TaskStatus -> String
taskStatusTitle status =
    case status of
        Api.Blocked ->
            "Blocked by incomplete dependencies"

        _ ->
            taskStatusDisplayText status


taskStatusBadgeClass : Api.TaskStatus -> String
taskStatusBadgeClass status =
    case status of
        Api.Blocked ->
            "task-status-text task-status-dependency-blocked"

        _ ->
            "badge badge-" ++ Api.taskStatusToString status


taskPopoverStatusClass : Api.TaskStatus -> String
taskPopoverStatusClass status =
    case status of
        Api.Blocked ->
            "popover-card-status task-status-text task-status-dependency-blocked"

        _ ->
            "popover-card-status card-status-" ++ Api.taskStatusToString status


taskCardStatusClass : Api.TaskStatus -> String
taskCardStatusClass status =
    case status of
        Api.Blocked ->
            ""

        _ ->
            " card-status-" ++ Api.taskStatusToString status


editElementId : String -> String -> String
editElementId entityId field =
    "edit-" ++ entityId ++ "-" ++ field


focusElement : String -> Cmd Msg
focusElement elemId =
    Browser.Dom.focus elemId
        |> ElmTask.attempt (\_ -> NoOp)


scrollToElement : String -> Cmd Msg
scrollToElement elemId =
    Browser.Dom.getElement elemId
        |> ElmTask.andThen
            (\info ->
                Browser.Dom.setViewportOf "main-content-scroll" 0 (info.element.y - 100)
            )
        |> ElmTask.attempt (\_ -> NoOp)


formatDate : String -> String
formatDate dateStr =
    String.left 10 dateStr


truncateText : Int -> String -> String
truncateText maxLen str =
    if String.length str > maxLen then
        String.left maxLen str ++ "…"

    else
        str


truncateId : String -> String
truncateId id =
    String.left 8 id ++ "..."


collectDescendantProjectIds : List Api.Project -> String -> List String
collectDescendantProjectIds allProjects parentId =
    let
        directChildren =
            List.filter (\p -> p.parentId == Just parentId) allProjects
    in
    parentId :: List.concatMap (\c -> collectDescendantProjectIds allProjects c.id) directChildren


collectDescendantTaskIds : List Api.Task -> String -> List String
collectDescendantTaskIds allTasks parentId =
    let
        directChildren =
            List.filter (\t -> t.parentId == Just parentId) allTasks
    in
    parentId :: List.concatMap (\child -> collectDescendantTaskIds allTasks child.id) directChildren


computeDropPriority : Maybe Int -> Maybe Int -> Int
computeDropPriority abovePri belowPri =
    case ( abovePri, belowPri ) of
        ( Just a, Just b ) ->
            (a + b) // 2

        ( Just a, Nothing ) ->
            Basics.max 0 (a - 1)

        ( Nothing, Just b ) ->
            Basics.min 10 (b + 1)

        ( Nothing, Nothing ) ->
            5


graphPositionsKey : String -> String
graphPositionsKey wsId =
    "hmem-graph-positions-" ++ wsId


isExpanded : Model -> String -> Bool
isExpanded model cardId =
    Dict.get cardId model.cards.expandedCards |> Maybe.withDefault False


isCollapsed : Model -> String -> Bool
isCollapsed model nodeId =
    Dict.get nodeId model.cards.collapsedNodes |> Maybe.withDefault False


editingValue : Model -> String -> String -> Maybe String
editingValue model entityId field =
    case model.editing.editState of
        Just (EditingField state) ->
            if state.entityId == entityId && state.field == field then
                Just state.value

            else
                Nothing

        Nothing ->
            Nothing


{-| Decode any scalar JSON value to a String for display in diffs.
Handles strings, ints, floats, bools, and nulls. Nested objects/arrays
will cause the dict decode to fail, falling back to a generic message.
-}
flexibleStringDecoder : Decode.Decoder String
flexibleStringDecoder =
    Decode.oneOf
        [ Decode.string
        , Decode.map String.fromInt Decode.int
        , Decode.map String.fromFloat Decode.float
        , Decode.map
            (\b ->
                if b then
                    "true"

                else
                    "false"
            )
            Decode.bool
        , Decode.null "null"
        ]



-- FILTER HELPERS


passesStatusFilter : List String -> String -> Bool
passesStatusFilter statuses status =
    List.isEmpty statuses || List.member status statuses


passesPriorityFilter : FilterPriority -> Int -> Bool
passesPriorityFilter filter priority =
    case filter of
        AnyPriority ->
            True

        ExactPriority v ->
            priority == v

        AbovePriority v ->
            priority >= v

        BelowPriority v ->
            priority <= v
