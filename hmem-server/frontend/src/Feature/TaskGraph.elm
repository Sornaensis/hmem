module Feature.TaskGraph exposing (Edge, Graph, GraphProject, GraphTask, Guard, State, Status(..), acceptSnapshot, begin, fail, init, project, retire)

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
    | Failed String


type alias State =
    { guard : Maybe Guard
    , status : Status
    , graph : Maybe Graph
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


init : State
init =
    { guard = Nothing, status = Idle, graph = Nothing }


begin : Guard -> State
begin guard =
    if List.any String.isEmpty [ guard.workspaceId, guard.projectId, guard.audienceId ] || guard.sessionEpoch < 0 || guard.targetGeneration < 0 then
        { init | status = Failed "invalid graph request" }

    else
        { guard = Just guard, status = Loading, graph = Nothing }


retire : State -> State
retire _ =
    init


fail : Guard -> String -> State -> State
fail guard message state =
    if state.guard == Just guard && state.status == Loading then
        { state | status = Failed message, graph = Nothing }

    else
        state


acceptSnapshot : Guard -> List Api.SnapshotItem -> State -> State
acceptSnapshot guard items state =
    if state.guard /= Just guard || state.status /= Loading then
        state

    else
        case ChangeStream.applySnapshotProfile (ChangeStream.Workspace guard.workspaceId) "full_v1" items |> Result.andThen (project guard.projectId) of
            Ok graph ->
                { state | status = Ready, graph = Just graph }

            Err message ->
                fail guard message state


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
