module Feature.SessionActivity exposing (init, initialVisibility, reconcile, update, targets, active)

{-| One physical boolean probe, independent of transcript/session delivery.
Logical retirement prunes cache/demand but retains occupancy until its callback.
-}
import Api
import Dict
import Http
import Json.Decode as Decode
import Permissions
import Set
import Task
import Time
import Types exposing (..)


init : SessionActivityModel
init =
    { cache=Dict.empty,order=[],pending=Nothing,nextId=1,version=0,sessionVersion=0,generation=0,now=0,lastProbe= -1000,visible=True,owner="" }


initialVisibility : Decode.Value -> Bool
initialVisibility flags = Decode.decodeValue (Decode.field "documentVisible" Decode.bool) flags |> Result.withDefault True


owner : Model -> String
owner model =
    if model.auth.status /= AuthReady || not model.sessionActivity.visible then ""
    else case model.page of
        HomePage -> model.groups.catalogueOwner |> Maybe.map (\context -> "catalogue:" ++ context.principal.actorId ++ ":" ++ String.fromInt model.groups.catalogueEpoch) |> Maybe.withDefault ""
        WorkspacePage workspace ->
            if Permissions.canReadCurrentWorkspace model then model.sessionContext |> Maybe.map (\context -> "workspace:" ++ workspace ++ ":" ++ context.principal.actorId ++ ":" ++ String.fromInt model.sessionRequestEpoch) |> Maybe.withDefault "" else ""
        _ -> model.groups.catalogueOwner |> Maybe.map (\context -> "catalogue:" ++ context.principal.actorId ++ ":" ++ String.fromInt model.groups.catalogueEpoch) |> Maybe.withDefault ""


targets : Model -> List String
targets model =
    if owner model == "" then [] else
    case model.page of
        HomePage ->
            let
                grouped=Dict.values model.groups.groupMembers |> List.concat |> Set.fromList
                inGroups=Dict.keys model.groups.workspaceGroups |> List.concatMap (\id -> Dict.get id model.groups.groupMembers |> Maybe.withDefault [])
                ungrouped=Dict.keys model.workspaces |> List.filter (\id -> not (Set.member id grouped))
            in
            if model.dataLoading.loadingWorkspaces then [] else Set.fromList (inGroups ++ ungrouped) |> Set.toList |> List.filter (\id -> Dict.member id model.workspaces && not (Set.member id model.groups.deletedWorkspaces)) |> List.map ((++) "workspace:")
        WorkspacePage _ ->
            sidebarTargets model ++ (if model.activeTab == ProjectsTab then model.dataLoading.visibleDetailDemand |> Maybe.map (Tuple.first >> Set.toList >> List.map ((++) "project:")) |> Maybe.withDefault [] else [])
        _ -> sidebarTargets model


sidebarTargets : Model -> List String
sidebarTargets model =
    let
        grouped = Dict.values model.groups.groupMembers |> List.concat |> Set.fromList
        expanded = Dict.keys model.groups.workspaceGroups |> List.filter (\id -> Dict.get id model.groups.collapsedGroups /= Just True)
        members = expanded |> List.concatMap (\id -> Dict.get id model.groups.groupMembers |> Maybe.withDefault [])
        ungrouped = Dict.keys model.workspaces |> List.filter (\id -> not (Set.member id grouped))
    in
    if model.dataLoading.loadingWorkspaces then [] else Set.fromList (members ++ ungrouped) |> Set.toList |> List.filter (\id -> Dict.member id model.workspaces && not (Set.member id model.groups.deletedWorkspaces)) |> List.map ((++) "workspace:")


active : String -> Model -> Bool
active key model =
    owner model /= "" && model.sessionActivity.owner == owner model && (Dict.get key model.sessionActivity.cache |> Maybe.andThen .active) == Just True


reconcile : Model -> (Model,Cmd Msg) -> (Model,Cmd Msg)
reconcile previous (model,command) =
    let
        state=model.sessionActivity
        authority=owner model
        keys=targets model
        changed=state.owner /= authority
        invalidated=state.sessionVersion /= model.sessionDelivery.listInvalidation || previous.webSocket.state /= model.webSocket.state
        version=state.version + (if invalidated then 1 else 0)
        generation=state.generation + (if changed || Set.fromList keys /= Set.fromList state.order then 1 else 0)
        cache=if changed then Dict.empty else Dict.filter (\key _ -> List.member key keys) state.cache
        order=List.filter (\key -> List.member key keys) state.order ++ List.filter (\key -> not (List.member key state.order)) keys
        due key=Dict.get key cache |> Maybe.map (\entry -> entry.version < version || state.now-entry.checked >= 10000) |> Maybe.withDefault True
        chosen=List.filter due order |> List.take 50
        ready={state | owner=authority,cache=cache,order=order,version=version,generation=generation,sessionVersion=model.sessionDelivery.listInvalidation}
        mayStart=List.any (\key -> not (Dict.member key cache)) chosen || state.now-state.lastProbe >= 1000
    in
    if authority == "" || state.pending /= Nothing || List.isEmpty chosen || not mayStart then ({model | sessionActivity=ready},command)
    else
        let
            scope=case model.page of
                WorkspacePage workspace -> Just workspace
                _ -> Nothing
            request={id=state.nextId,owner=authority,keys=chosen,workspace=scope,version=version,generation=generation}
            identifiers prefix=chosen |> List.filter (String.startsWith prefix) |> List.map (String.dropLeft (String.length prefix))
            next={ready | pending=Just request,nextId=state.nextId+1,lastProbe=state.now,order=List.filter (\key -> not (List.member key chosen)) order ++ chosen}
        in
        ({model | sessionActivity=next},Cmd.batch [command,Api.fetchSessionActivity model.flags.apiUrl (identifiers "workspace:") scope (identifiers "project:") (SessionActivityFetched request.id)])


update : Msg -> Model -> (Model,Cmd Msg)
update msg model =
    let state=model.sessionActivity in
    case msg of
        SessionActivityTick time -> ({model | sessionActivity={state | now=toFloat (Time.posixToMillis time)}},Cmd.none)
        SessionActivityVisibility visible -> ({model | sessionActivity={state | visible=visible,version=state.version+1}},Cmd.none)
        SessionActivityFetched id result ->
            case state.pending of
                Just request ->
                    if request.id /= id then (model,Cmd.none) else
                    let
                        current=request.owner == owner model && state.owner == request.owner && request.generation == state.generation
                        demanded=targets model
                        rows=result |> Result.map (\response -> List.map (\(key,value) -> ("workspace:" ++ key,value)) (Dict.toList response.workspaces) ++ List.map (\(key,value) -> ("project:" ++ key,value)) (Dict.toList response.projects))
                        valid=rows |> Result.toMaybe |> Maybe.map (\items -> List.length items <= List.length request.keys && List.all (Tuple.first >> (\key -> List.member key request.keys)) items) |> Maybe.withDefault False
                        values=if valid then rows |> Result.withDefault [] |> Dict.fromList else Dict.empty
                        cache=if current then List.foldl (\key all -> if List.member key demanded then Dict.insert key {active=Dict.get key values,checked=state.now,version=request.version} all else all) state.cache request.keys else state.cache
                        unauthorized=current && (case result of
                            Err (Http.BadStatus 401) -> True
                            _ -> False)
                        command=if unauthorized then Task.perform (always AuthUnauthorized) (Task.succeed ()) else Cmd.none
                    in ({model | sessionActivity={state | pending=Nothing,cache=cache}},command)
                Nothing -> (model,Cmd.none)
        _ -> (model,Cmd.none)
