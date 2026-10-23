module Feature.SessionDelivery exposing
    ( State, Target(..), Payload(..), Request, Resource, init, key, demand, removeDemand
    , invalidateSession, invalidateTask, invalidateLists, invalidateWorkspace, invalidateAll, retireWorkspace, retireAll
    , admit, settle, fetch, staleAt, consume
    )

{-| Bounded demand for canonical session HTTP resources. Logical retirement
never releases a physical admission; only its exact callback does that.
-}

import Api
import Dict exposing (Dict)
import Http
import Set exposing (Set)


type Target
    = WorkspaceSessions (Maybe Api.SessionCursor)
    | WorkspaceSessionsFiltered Api.SessionFilters (Maybe Api.SessionCursor)
    | TaskSessions String (Maybe Api.SessionCursor)
    | SessionMetadata String
    | SessionTasks String (Maybe String)
    | SessionBytes String Int Bool
    | SessionWindow String Int Int Bool
    | SessionSave String Int Int


type Payload
    = SessionsPage Api.SessionPage
    | Metadata Api.AgentSession
    | TasksPage Api.SessionTaskPage
    | Bytes Api.SessionRange


type alias Resource =
    { workspace : String, target : Target, dirty : Int, settled : Int
    , data : Maybe Payload, ranges : List Api.SessionRange, error : Maybe String }


type alias Request =
    { id : Int, key : String, workspace : String, target : Target, dirty : Int }


type alias State =
    { resources : Dict String Resource, physical : Dict Int Request, order : List String, implicitMetadata : Set String
    , nextId : Int, version : Int, retirement : Int, admissionError : Maybe String }


init : State
init =
    { resources = Dict.empty, physical = Dict.empty, order = [], implicitMetadata = Set.empty, nextId = 1, version = 0, retirement = 0, admissionError = Nothing }


key : String -> Target -> String
key workspace target =
    let
        cursor value = value |> Maybe.map (\item -> item.createdAt ++ ":" ++ item.id) |> Maybe.withDefault "first"
        suffix = case target of
            WorkspaceSessions before -> "list:" ++ cursor before
            WorkspaceSessionsFiltered filters before -> "list:" ++ filters.state ++ ":" ++ filters.freshness ++ ":" ++ cursor before
            TaskSessions task before -> "task:" ++ task ++ ":" ++ cursor before
            SessionMetadata id -> "detail:" ++ id
            SessionTasks id after -> "links:" ++ id ++ ":" ++ Maybe.withDefault "first" after
            SessionBytes id _ _ -> "range:" ++ id
            SessionWindow id _ _ _ -> "window:" ++ id
            SessionSave id _ _ -> "save:" ++ id
    in
    "workspace:" ++ workspace ++ "|sessions:" ++ suffix


demand : String -> Target -> State -> State
demand workspace target state =
    case target of
        SessionBytes id offset _ ->
            if offset < 0 then { state | admissionError = Just "invalid_range" }
            else if Dict.size state.resources + (List.filter (\identity -> not (Dict.member identity state.resources)) [key workspace target,key workspace (SessionMetadata id)] |> List.length) > 16 then
                { state | admissionError = Just "demand_limit" }
            else
                let
                    metadataKey = key workspace (SessionMetadata id)
                    withMetadata = demandOne workspace (SessionMetadata id) state
                    owned = if Dict.member metadataKey state.resources then withMetadata else { withMetadata | implicitMetadata = Set.insert metadataKey withMetadata.implicitMetadata }
                in
                demandOne workspace target owned
        SessionMetadata _ ->
            let explicit = demandOne workspace target state in
            { explicit | implicitMetadata = Set.remove (key workspace target) explicit.implicitMetadata }
        SessionWindow id offset end _ ->
            if offset < 0 || end <= offset || end - offset > 2097152 then { state | admissionError = Just "invalid_window" }
            else demandPaired workspace target id state
        SessionSave id offset count ->
            if offset < 0 || count < 1 || count > 65536 then { state | admissionError = Just "invalid_save_chunk" }
            else demandPaired workspace target id state
        _ -> demandOne workspace target state


demandPaired : String -> Target -> String -> State -> State
demandPaired workspace target id state =
    if Dict.size state.resources + (List.filter (\identity -> not (Dict.member identity state.resources)) [key workspace target,key workspace (SessionMetadata id)] |> List.length) > 16 then
        { state | admissionError = Just "demand_limit" }
    else
        let
            metadataKey = key workspace (SessionMetadata id)
            withMetadata = demandOne workspace (SessionMetadata id) state
            owned = if Dict.member metadataKey state.resources then withMetadata else { withMetadata | implicitMetadata = Set.insert metadataKey withMetadata.implicitMetadata }
        in demandOne workspace target owned


pairedId : Target -> Maybe String
pairedId target = case target of
    SessionBytes id _ _ -> Just id
    SessionWindow id _ _ _ -> Just id
    SessionSave id _ _ -> Just id
    _ -> Nothing


demandOne : String -> Target -> State -> State
demandOne workspace target state =
    let
        identity = key workspace target
        version = state.version + 1
    in
    case Dict.get identity state.resources of
        Just resource ->
            if resource.target == target && resource.error == Nothing then state
            else { state | version = version, admissionError = Nothing
                , resources = Dict.insert identity { resource | target = target, dirty = version, error = Nothing } state.resources }
        Nothing ->
            if Dict.size state.resources >= 16 then { state | admissionError = Just "demand_limit" }
            else { state | version = version, admissionError = Nothing, order = state.order ++ [identity]
                , resources = Dict.insert identity (Resource workspace target version 0 Nothing [] Nothing) state.resources }


removeDemand : String -> Target -> State -> State
removeDemand workspace target state =
    let
        remove identity source = { source | resources = Dict.remove identity source.resources, order = List.filter ((/=) identity) source.order, implicitMetadata = Set.remove identity source.implicitMetadata, admissionError = Nothing }
    in
    case target of
        SessionMetadata id ->
            if Dict.values state.resources |> List.any (\resource -> resource.workspace == workspace && pairedId resource.target == Just id) then
                { state | implicitMetadata = Set.insert (key workspace target) state.implicitMetadata }
            else remove (key workspace target) state
        _ ->
            let without = remove (key workspace target) state in
            case pairedId target of
                Just id ->
                    let metadataKey = key workspace (SessionMetadata id) in
                    if Set.member metadataKey state.implicitMetadata && not (Dict.values without.resources |> List.any (\resource -> resource.workspace == workspace && pairedId resource.target == Just id)) then remove metadataKey without else without
                Nothing -> without


invalidate : (Resource -> Bool) -> State -> State
invalidate matches state =
    let version = state.version + 1 in
    { state | version = version, resources = Dict.map (\_ resource ->
        if matches resource then { resource | dirty = version, error = Nothing } else resource) state.resources }


invalidateWorkspace : String -> State -> State
invalidateWorkspace workspace =
    invalidate (\resource -> resource.workspace == workspace)
        >> (\state -> { state | retirement = state.retirement + 1, resources = Dict.map (\_ resource ->
            if resource.workspace == workspace then { resource | data = Nothing } else resource) state.resources })


invalidateAll : State -> State
invalidateAll =
    invalidate (always True) >> (\state -> { state | retirement = state.retirement + 1, resources = Dict.map (\_ resource -> { resource | data = Nothing }) state.resources })


invalidateSession : String -> String -> State -> State
invalidateSession workspace id =
    invalidate (\resource -> resource.workspace == workspace && (case resource.target of
        SessionMetadata actual -> actual == id
        SessionTasks actual _ -> actual == id
        WorkspaceSessions _ -> True
        WorkspaceSessionsFiltered _ _ -> True
        TaskSessions _ _ -> True
        SessionBytes _ _ _ -> False
        SessionWindow _ _ _ _ -> False
        SessionSave _ _ _ -> False))


invalidateTask : String -> String -> State -> State
invalidateTask workspace task =
    invalidate (\resource -> resource.workspace == workspace && (case resource.target of
        TaskSessions actual _ -> actual == task
        _ -> False))


invalidateLists : String -> State -> State
invalidateLists workspace =
    invalidate (\resource -> resource.workspace == workspace && (case resource.target of
        WorkspaceSessions _ -> True
        WorkspaceSessionsFiltered _ _ -> True
        _ -> False))


retireWorkspace : String -> State -> State
retireWorkspace workspace state =
    let kept = Dict.filter (\_ resource -> resource.workspace /= workspace) state.resources in
    { state | retirement = state.retirement + 1, resources = kept, order = List.filter (\id -> Dict.member id kept) state.order, implicitMetadata = Set.filter (\id -> Dict.member id kept) state.implicitMetadata }


retireAll : State -> State
retireAll state =
    { state | retirement = state.retirement + 1, resources = Dict.empty, order = [], implicitMetadata = Set.empty, admissionError = Nothing }


admit : String -> State -> ( State, List Request )
admit workspace state =
    let
        occupied identity = Dict.values state.physical |> List.any (\request -> request.key == identity)
        metadataReady resource = case pairedId resource.target of
            Just id -> Dict.get (key workspace (SessionMetadata id)) state.resources
                |> Maybe.map (\detail -> detail.dirty == detail.settled && detail.data /= Nothing) |> Maybe.withDefault False
            Nothing -> True
        ready = state.order |> List.filterMap (\identity -> Dict.get identity state.resources |> Maybe.map (Tuple.pair identity)) |> List.filter (\( identity, resource ) ->
            resource.workspace == workspace && resource.dirty /= resource.settled && resource.error == Nothing && not (occupied identity) && metadataReady resource)
        selected = List.take (max 0 (2 - Dict.size state.physical)) ready
        add ( identity, resource ) ( current, requests ) =
            let request = Request current.nextId identity workspace resource.target resource.dirty in
            ( { current | nextId = current.nextId + 1, physical = Dict.insert request.id request current.physical, order = List.filter ((/=) identity) current.order ++ [identity] }, requests ++ [request] )
    in
    List.foldl add ( state, [] ) selected


settle : Int -> Bool -> Result Http.Error Payload -> State -> State
settle id authorized result state =
    case Dict.get id state.physical of
        Nothing -> state
        Just request ->
            let released = { state | physical = Dict.remove id state.physical } in
            case Dict.get request.key state.resources of
                Nothing -> released
                Just resource ->
                    if not authorized || resource.dirty /= request.dirty then released
                    else case result of
                        Err _ ->
                            { released | resources = Dict.insert request.key { resource | error = Just "refresh_failed" } released.resources }
                        Ok payload ->
                            if not (valid request payload) then
                                { released | resources = Dict.insert request.key { resource | error = Just "invalid_response" } released.resources }
                            else
                                apply request resource payload released


valid : Request -> Payload -> Bool
valid request payload =
    let sessionMatches id session = session.id == id && session.workspaceId == request.workspace in
    case ( request.target, payload ) of
        ( WorkspaceSessions _, SessionsPage page ) -> List.length page.items <= 50 && List.all (\session -> session.workspaceId == request.workspace) page.items
        ( WorkspaceSessionsFiltered _ _, SessionsPage page ) -> List.length page.items <= 50 && List.all (\session -> session.workspaceId == request.workspace) page.items
        ( TaskSessions _ _, SessionsPage page ) -> List.length page.items <= 50 && List.all (\session -> session.workspaceId == request.workspace) page.items
        ( SessionMetadata id, Metadata session ) -> sessionMatches id session
        ( SessionTasks _ _, TasksPage page ) -> List.length page.items <= 50
        ( SessionBytes id offset sealed, Bytes range ) ->
            validRange request.workspace id offset 65536 sealed range
        ( SessionWindow id offset end sealed, Bytes range ) -> validRange request.workspace id offset (min 65536 (end-offset)) sealed range
        ( SessionSave id offset count, Bytes range ) -> validRange request.workspace id offset count True range
        _ -> False


validRange : String -> String -> Int -> Int -> Bool -> Api.SessionRange -> Bool
validRange workspace id offset count sealed range =
    range.session.id == id && range.session.workspaceId == workspace && range.offset == offset && range.nextOffset >= offset
        && range.nextOffset <= offset + count && range.nextOffset <= range.session.committedBytes
        && (not range.hasMore || range.nextOffset > offset)
        && range.hasMore == (range.nextOffset < range.session.committedBytes)
        && (range.session.state == Api.SessionTerminated) == sealed
        && base64ByteLength range.dataBase64 == Just (range.nextOffset - offset)


consume : String -> Target -> State -> State
consume workspace target state =
    { state | resources = Dict.update (key workspace target) (Maybe.map (\resource -> { resource | data = Nothing, ranges = [] })) state.resources }


-- Validate standard padded base64, including zero pad bits, without decoding
-- transcript bytes as Unicode or allocating an unbounded second copy.
base64ByteLength : String -> Maybe Int
base64ByteLength encoded =
    let
        size = String.length encoded
        padding = if String.endsWith "==" encoded then 2 else if String.endsWith "=" encoded then 1 else 0
        body = String.dropRight padding encoded
        alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        index ch = String.indexes (String.fromChar ch) alphabet |> List.head
        finalIndex = String.right 1 body |> String.toList |> List.head |> Maybe.andThen index
        padBits = case padding of
            2 -> finalIndex |> Maybe.map (\value -> modBy 16 value == 0) |> Maybe.withDefault False
            1 -> finalIndex |> Maybe.map (\value -> modBy 4 value == 0) |> Maybe.withDefault False
            _ -> True
    in
    if size <= 87384 && modBy 4 size == 0 && String.all (\ch -> index ch /= Nothing) body && padBits then
        Just (size // 4 * 3 - padding)
    else Nothing


apply : Request -> Resource -> Payload -> State -> State
apply request resource payload state =
    let
        cached = { resource | data = Just payload, settled = request.dirty, error = Nothing }
        updated = { state | resources = Dict.insert request.key cached state.resources }
    in
    case payload of
        Metadata session ->
            let
                rangeKey = key request.workspace (SessionBytes session.id 0 False)
                updateRange old =
                    case old.target of
                        SessionBytes id offset sealed ->
                            if sealed /= (session.state == Api.SessionTerminated) || (session.committedBytes > offset && not (Dict.values updated.physical |> List.any (\pending -> pending.key == rangeKey))) then
                                { old | target = SessionBytes id offset (session.state == Api.SessionTerminated), dirty = updated.version + 1, error = Nothing }
                            else if session.committedBytes == offset then { old | settled = old.dirty }
                            else old
                        _ -> old
            in
            { updated | version = updated.version + 1, resources = Dict.update rangeKey (Maybe.map updateRange) updated.resources }
        Bytes range ->
            if pairedId request.target /= Nothing && (case request.target of
                SessionBytes _ _ _ -> False
                _ -> True) then updated
            else
            let
                nextVersion = updated.version + 1
                metadataAhead = Dict.get (key request.workspace (SessionMetadata range.session.id)) updated.resources
                    |> Maybe.andThen .data |> Maybe.map (\value -> case value of
                        Metadata latest -> latest.committedBytes > range.nextOffset
                        _ -> False) |> Maybe.withDefault False
                next = { cached | ranges = List.take 4 (range :: resource.ranges)
                    , target = SessionBytes range.session.id range.nextOffset (range.session.state == Api.SessionTerminated)
                    , dirty = if range.hasMore || metadataAhead then nextVersion else request.dirty }
            in
            { updated | version = nextVersion, resources = Dict.insert request.key next updated.resources }
        _ -> updated


fetch : String -> Request -> (Result Http.Error Payload -> msg) -> Cmd msg
fetch apiUrl request toMsg =
    case request.target of
        WorkspaceSessions cursor -> Api.fetchAgentSessions apiUrl request.workspace Nothing cursor 50 (Result.map SessionsPage >> toMsg)
        WorkspaceSessionsFiltered filters cursor -> Api.fetchFilteredAgentSessions apiUrl request.workspace Nothing filters cursor 50 (Result.map SessionsPage >> toMsg)
        TaskSessions task cursor -> Api.fetchAgentSessions apiUrl request.workspace (Just task) cursor 50 (Result.map SessionsPage >> toMsg)
        SessionMetadata id -> Api.fetchAgentSession apiUrl request.workspace id (Result.map Metadata >> toMsg)
        SessionTasks id cursor -> Api.fetchSessionTasks apiUrl request.workspace id cursor 50 (Result.map TasksPage >> toMsg)
        SessionBytes id offset sealed -> Api.fetchSessionRange apiUrl request.workspace id sealed offset 65536 (Result.map Bytes >> toMsg)
        SessionWindow id offset end sealed -> Api.fetchSessionRange apiUrl request.workspace id sealed offset (min 65536 (end-offset)) (Result.map Bytes >> toMsg)
        SessionSave id offset count -> Api.fetchSessionRange apiUrl request.workspace id True offset count (Result.map Bytes >> toMsg)


staleAt : Float -> Float -> Bool
staleAt now lastHeartbeat =
    now - lastHeartbeat >= 30000
