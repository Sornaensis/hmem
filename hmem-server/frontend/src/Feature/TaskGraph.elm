module Feature.TaskGraph exposing (Edge, Filters, Graph, GraphProject, GraphTask, Guard, State, Status(..), acceptSnapshot, begin, defaultFilters, fail, init, invalidate, markLive, project, recover, refresh, retire, visibleGraph)

{-| The graph owns a complete snapshot-derived read model, separate from the
ordinary hierarchy's sparse demand caches. Opaque transport bearers stay in JS.
-}

import Api
import Dict exposing (Dict)
import Feature.ChangeStream as ChangeStream
import Set exposing (Set)


type alias Guard =
    { workspaceId : String
    , projectId : String
    , audienceId : String
    , sessionEpoch : Int
    , targetGeneration : Int
    }


type Status
    = Idle
    | Loading
    | Ready
    | Refreshing
    | Recovering
    | Stale String
    | Failed String


type alias State =
    { guard : Maybe Guard
    , status : Status
    , graph : Maybe Graph
    , pending : Bool
    , live : Bool
    , revision : Int
    , runningChildren : Dict String Int
    }


type alias GraphProject =
    { id : String, name : String, parentId : Maybe String, external : Bool }


type alias GraphTask =
    { id : String, title : String, status : Api.TaskStatus, projectId : Maybe String, parentId : Maybe String, external : Bool }


type alias Edge =
    { prerequisiteId : String, dependentId : String }


type alias Graph =
    { selectedProjectId : String
    , projects : Dict String GraphProject
    , tasks : Dict String GraphTask
    , edges : List Edge
    , inScopeTaskIds : Set String
    , externalTaskIds : Set String
    }


type alias Filters =
    { showDone : Bool, showCancelled : Bool }


defaultFilters : Filters
defaultFilters =
    { showDone = True, showCancelled = True }


{-| Presentation only: the canonical graph stays complete. External membership
is recomputed from edges incident to admitted in-scope anchors, never expanded.
-}
visibleGraph : Filters -> Graph -> Graph
visibleGraph filters raw =
    let
        admitted = Dict.filter (\_ task -> (filters.showDone || task.status /= Api.Done) && (filters.showCancelled || task.status /= Api.Cancelled)) raw.tasks
        anchors = Set.intersect raw.inScopeTaskIds (Dict.keys admitted |> Set.fromList)
        edges = List.filter (\edge -> Dict.member edge.prerequisiteId admitted && Dict.member edge.dependentId admitted && (Set.member edge.prerequisiteId anchors || Set.member edge.dependentId anchors)) raw.edges
        included = List.foldl (\edge ids -> ids |> Set.insert edge.prerequisiteId |> Set.insert edge.dependentId) anchors edges
        external = Set.diff included anchors
        tasks = Dict.filter (\id _ -> Set.member id included) admitted
        owning = Dict.values tasks |> List.filterMap .projectId |> Set.fromList
    in
    { raw | tasks = tasks, edges = edges, inScopeTaskIds = anchors, externalTaskIds = external
        , projects = Dict.filter (\id project_ -> not project_.external || Set.member id owning) raw.projects }


init : State
init =
    { guard = Nothing, status = Idle, graph = Nothing, pending = False, live = False, revision = 0, runningChildren = Dict.empty }


begin : Guard -> State
begin guard =
    if List.any String.isEmpty [ guard.workspaceId, guard.projectId, guard.audienceId ] || guard.sessionEpoch < 0 || guard.targetGeneration < 0 then
        { init | status = Failed "invalid graph request" }

    else
        { init | guard = Just guard, status = Loading, pending = True }


refresh : Guard -> State -> State
refresh guard state =
    if state.guard == Just guard then
        { state | status = if state.graph == Nothing then Loading else Refreshing, pending = True, live = False }
    else state


recover : Guard -> State -> State
recover guard state =
    if state.guard == Just guard then
        { state | live = False, status =
            case state.status of
                Stale _ -> state.status
                Failed _ -> state.status
                _ -> if state.pending || state.graph == Nothing then state.status else Recovering
        }
    else state


markLive : Guard -> State -> State
markLive guard state =
    if state.guard == Just guard && not state.pending && state.graph /= Nothing && (state.status == Ready || state.status == Recovering) then
        { state | live = True, status = Ready }
    else state


invalidate : Guard -> String -> State -> State
invalidate guard message state =
    if state.guard == Just guard then
        { state | status = Failed message, pending = False, graph = Nothing, live = False, runningChildren = Dict.empty }
    else state


retire : State -> State
retire _ =
    init


fail : Guard -> String -> State -> State
fail guard message state =
    if state.guard == Just guard then
        { state | status = if state.graph == Nothing then Failed message else Stale message, pending = False, live = False }

    else
        state


acceptSnapshot : Guard -> List Api.SnapshotItem -> State -> State
acceptSnapshot guard items state =
    if state.guard /= Just guard || not state.pending then
        state

    else
        case ChangeStream.applySnapshotProfile (ChangeStream.Workspace guard.workspaceId) "full_v1" items of
            Err message -> fail guard message state
            Ok snapshot ->
                if not (Dict.member guard.projectId snapshot.projects) then invalidate guard "selected graph project is unavailable" state
                else case project guard.projectId snapshot of
                    Ok graph -> { state | status = Ready, graph = Just graph, pending = False, live = False, revision = state.revision + 1
                        , runningChildren = Dict.values snapshot.tasks |> List.filter (\task -> task.status == Api.InProgress) |> List.filterMap .parentId |> List.foldl (\id -> Dict.update id (\count -> Just (1+Maybe.withDefault 0 count))) Dict.empty }
                    Err message -> fail guard message state


project : String -> ChangeStream.Snapshot -> Result String Graph
project selectedProjectId snapshot =
    let
        children =
            Dict.values snapshot.projects
                |> List.foldl
                    (\value index ->
                        case value.parentId of
                            Just parent ->
                                Dict.update parent (\existing -> Just (value.id :: Maybe.withDefault [] existing)) index

                            Nothing ->
                                index
                    )
                    Dict.empty

        descendants pending visited =
            case pending of
                [] ->
                    Ok visited

                identity :: remaining ->
                    if Set.member identity visited then
                        Err "project hierarchy contains a cycle"

                    else
                        descendants (Maybe.withDefault [] (Dict.get identity children) ++ remaining) (Set.insert identity visited)

        build projectIds =
            let
                belongs task =
                    Maybe.map (\identity -> Set.member identity projectIds) task.projectId |> Maybe.withDefault False

                inScope =
                    Dict.filter (\_ task -> belongs task) snapshot.tasks |> Dict.keys |> Set.fromList

                incident =
                    List.filter (\edge -> Set.member edge.taskId inScope || Set.member edge.dependsOnId inScope) snapshot.dependencies

                included =
                    List.foldl (\edge ids -> ids |> Set.insert edge.taskId |> Set.insert edge.dependsOnId) inScope incident

                external =
                    Set.diff included inScope

                includedTasks =
                    Dict.filter (\identity _ -> Set.member identity included) snapshot.tasks

                requiredProjects =
                    Dict.values includedTasks |> List.filterMap .projectId |> Set.fromList |> Set.union projectIds

                taskValid task =
                    (Maybe.map (\identity -> Dict.member identity snapshot.projects) task.projectId |> Maybe.withDefault True)
                        && (Maybe.map (\identity -> Dict.member identity snapshot.tasks) task.parentId |> Maybe.withDefault True)

                graphTask task =
                    { id = task.id, title = task.title, status = task.status, projectId = task.projectId, parentId = task.parentId, external = Set.member task.id external }

                graphProject value =
                    { id = value.id, name = value.name, parentId = value.parentId, external = not (Set.member value.id projectIds) }
            in
            if Dict.size includedTasks /= Set.size included || not (List.all taskValid (Dict.values includedTasks)) || not (Set.foldl (\identity valid -> valid && Dict.member identity snapshot.projects) True requiredProjects) then
                Err "graph snapshot has an unresolved task, parent or owning project"

            else
                Ok
                    { selectedProjectId = selectedProjectId
                    , projects = Dict.filter (\identity _ -> Set.member identity requiredProjects) snapshot.projects |> Dict.map (\_ value -> graphProject value)
                    , tasks = Dict.map (\_ task -> graphTask task) includedTasks
                    , edges = List.map (\edge -> { prerequisiteId = edge.dependsOnId, dependentId = edge.taskId }) incident |> List.sortBy (\edge -> ( edge.prerequisiteId, edge.dependentId ))
                    , inScopeTaskIds = inScope
                    , externalTaskIds = external
                    }
    in
    if not (Dict.member selectedProjectId snapshot.projects) then
        Err "selected graph project is unavailable"

    else
        descendants [ selectedProjectId ] Set.empty |> Result.andThen build
