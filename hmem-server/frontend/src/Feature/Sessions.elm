module Feature.Sessions exposing (reconcile, update, view, viewTaskPanel, viewTaskActive, viewTaskEvidence, viewTaskIcon, taskActive, timestampSources, olderWindow, viewerReset)

import Api
import Dict
import Feature.SessionDelivery as Delivery
import Feature.WebSocket as WebSocket
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, tabindex, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import Permissions
import Ports
import Set
import Markdown.Parser
import Markdown.Renderer
import Time
import TimestampDisplay
import Types exposing (..)
import Url


urlId : String -> Model -> Maybe String
urlId key model =
    model.url.fragment |> Maybe.withDefault "" |> String.split "&"
        |> List.filterMap (\pair -> case String.split "=" pair of
            [ name, value ] -> if name == key then Url.percentDecode value else Nothing
            _ -> Nothing)
        |> List.head
        |> Maybe.andThen (\id -> if String.length id == 36 && List.map String.length (String.split "-" id) == [8,4,4,4,12] && String.all (\c -> String.contains (String.fromChar c) "0123456789abcdef-") id then Just id else Nothing)


selectedFromUrl : Model -> Maybe String
selectedFromUrl = urlId "session"


listTarget : SessionsModel -> Delivery.Target
listTarget state =
    case state.taskFilter of
        Just task -> Delivery.TaskSessionsFiltered task state.filters state.cursor
        Nothing -> Delivery.WorkspaceSessionsFiltered state.filters state.cursor


taskCursor : Model -> String -> Maybe Api.SessionCursor
taskCursor model task = Dict.get task model.sessions.taskCursors |> Maybe.withDefault Nothing


mountedTasks : Model -> List String
mountedTasks model =
    let
        focused = model.focus.focusedEntity |> Maybe.andThen (\(kind,id) -> if kind == "task" then Just id else Nothing) |> Maybe.map List.singleton |> Maybe.withDefault []
        visible = model.dataLoading.visibleDetailDemand |> Maybe.map (Tuple.second >> Set.toList) |> Maybe.withDefault []
        expanded id = Dict.get id model.cards.expandedCards == Just True
        candidates = focused ++ List.filter expanded visible ++ visible
    in
    List.foldl (\id ids -> if List.member id ids then ids else ids ++ [id]) [] candidates


reconcile : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
reconcile ( model, command ) =
    let
        old = model.sessions
        workspace = if model.auth.status == AuthReady && Permissions.canReadCurrentWorkspace model && (model.selectedWorkspaceId |> Maybe.map (\ws -> model.page == WorkspacePage ws) |> Maybe.withDefault False) then model.selectedWorkspaceId else Nothing
        selected = if workspace /= Nothing && model.activeTab == SessionsTab then selectedFromUrl model else Nothing
        taskPanel = if workspace /= Nothing && model.activeTab == ProjectsTab then model.focus.focusedEntity |> Maybe.andThen (\(kind,id) -> if kind == "task" && Dict.member id model.tasks then Just id else Nothing) else Nothing
        stamp = Encode.encode 0 (Encode.list Encode.string [Maybe.withDefault "" workspace, String.fromInt model.sessionRequestEpoch, model.sessionContext |> Maybe.map (.principal >> .actorId) |> Maybe.withDefault "", String.fromInt model.sessionDelivery.retirement, Maybe.withDefault "" selected])
        taskFilter = if workspace /= Nothing && model.activeTab == SessionsTab then urlId "task" model else Nothing
        taskIds = if workspace /= Nothing && model.activeTab == ProjectsTab then mountedTasks model else []
        reset = old.workspace /= workspace || old.epoch /= model.sessionRequestEpoch
        scoped = if reset then { old | workspace = workspace, epoch = model.sessionRequestEpoch, selected = selected, cursor = Nothing, previous = [], linksCursor = Nothing, filters = Api.defaultSessionFilters, taskFilter = taskFilter, taskCursors = Dict.empty, taskPreviousPages = Dict.empty, taskPages = Dict.empty, taskHints = Dict.empty, hintOrder = [], sectionOrder = [] }
            else if old.selected /= selected then { old | selected = selected, linksCursor = Nothing } else old
        filtered = if scoped.taskFilter /= taskFilter then { scoped | taskFilter = taskFilter, cursor = Nothing, previous = [], filters = Api.defaultSessionFilters } else scoped
        hintEpoch = model.sessionDelivery.listInvalidation
        retainedHints = Dict.filter (\id _ -> List.member id taskIds) filtered.taskHints
        consumeHint (ws,target) accumulated = case target of
            Delivery.TaskSessionsFiltered task filters cursor ->
                if cursor /= Nothing || filters.state /= "active" || not (List.member task taskIds) then accumulated else
                case resource ws target model of
                    Just entry ->
                        if entry.error /= Nothing then Dict.insert task {active=False,error=True,epoch=hintEpoch} accumulated else if entry.dirty /= entry.settled then accumulated else
                        case entry.data of
                            Just (Delivery.SessionsPage page) -> Dict.insert task {active=not (List.isEmpty page.items),error=False,epoch=hintEpoch} accumulated
                            _ -> if entry.error /= Nothing then Dict.insert task {active=False,error=True,epoch=hintEpoch} accumulated else accumulated
                    Nothing -> accumulated
            _ -> accumulated
        hints = if reset then Dict.empty else List.foldl consumeHint retainedHints old.owned
        rotation = List.filter (\id -> List.member id taskIds) filtered.hintOrder ++ List.filter (\id -> not (List.member id filtered.hintOrder)) taskIds
        unresolved id = Dict.get id hints |> Maybe.map (\hint -> hint.epoch /= hintEpoch) |> Maybe.withDefault True
        pendingHintTasks = old.owned |> List.filterMap (\(ws,target) -> case target of
            Delivery.TaskSessionsFiltered task filters _ ->
                if filters.state == "active" && List.member task taskIds && (resource ws target model |> Maybe.map (\entry -> entry.error == Nothing && entry.dirty /= entry.settled) |> Maybe.withDefault False) then Just task else Nothing
            _ -> Nothing)
        hintTasks = (pendingHintTasks ++ (List.filter (\id -> not (Dict.member id hints)) rotation ++ List.filter (\id -> Dict.member id hints && unresolved id) rotation)) |> List.foldl (\id ids -> if List.member id ids then ids else ids ++ [id]) [] |> List.take 2
        nextRotation = List.filter (\id -> not (List.member id hintTasks)) rotation ++ hintTasks
        expandedTasks = List.filter (\id -> Just id == taskPanel || Dict.get id model.cards.expandedCards == Just True) taskIds
        sectionTargets = expandedTasks |> List.concatMap (\task -> [sectionTarget False model task,sectionTarget True model task]) |> List.take 64
        sectionIdentity target = case target of
            Delivery.TaskSessionsFiltered task filters _ -> sectionKey (filters.state == "terminated") task
            _ -> ""
        sectionCursor target = case target of
            Delivery.TaskSessionsFiltered _ _ cursor -> cursor
            _ -> Nothing
        retainedPages = Dict.filter (\key _ -> List.any (sectionIdentity >> (==) key) sectionTargets) filtered.taskPages
        consumeSection (ws,target) accumulatedPages =
            if not (List.member target sectionTargets) then accumulatedPages else
            case resource ws target model of
                Just entry ->
                    if entry.error /= Nothing then Dict.insert (sectionIdentity target) {result=Err "Sessions could not be loaded.",epoch=hintEpoch,cursor=sectionCursor target} accumulatedPages
                    else if entry.dirty /= entry.settled then accumulatedPages else
                    case entry.data of
                        Just (Delivery.SessionsPage page) -> Dict.insert (sectionIdentity target) {result=Ok page,epoch=hintEpoch,cursor=sectionCursor target} accumulatedPages
                        _ -> accumulatedPages
                Nothing -> accumulatedPages
        pages = if reset then Dict.empty else List.foldl consumeSection retainedPages old.owned
        orderedSections = List.filterMap (\key -> List.filter (sectionIdentity >> (==) key) sectionTargets |> List.head) filtered.sectionOrder ++ List.filter (\target -> not (List.member (sectionIdentity target) filtered.sectionOrder)) sectionTargets
        sectionUnresolved target = Dict.get (sectionIdentity target) pages |> Maybe.map (\cached -> cached.epoch /= hintEpoch || cached.cursor /= sectionCursor target) |> Maybe.withDefault True
        sectionPending = old.owned |> List.filterMap (\(ws,target) ->
            if List.member target sectionTargets && (resource ws target model |> Maybe.map (\entry -> entry.error == Nothing && entry.dirty /= entry.settled) |> Maybe.withDefault False) then Just target else Nothing)
        sectionReads = (sectionPending ++ List.filter (\target -> not (Dict.member (sectionIdentity target) pages)) orderedSections ++ List.filter sectionUnresolved orderedSections)
            |> List.foldl (\target targets -> if List.member target targets then targets else targets ++ [target]) [] |> List.take 12
        withPanel = { filtered | taskPanel = taskPanel, taskHints = hints, hintOrder = nextRotation, taskPages = pages, sectionOrder = List.map sectionIdentity (List.filter (\target -> not (List.member target sectionReads)) orderedSections ++ sectionReads) }
        contextChanged = reset || old.selected /= selected
        retired =
            if old.stamp == stamp then withPanel
            else if contextChanged || old.following then
                { withPanel | stamp = stamp, generation = old.generation + 1, start = Nothing, offset = 0, end = 0, following = if contextChanged then True else old.following, loadWindow = True, pending = False, rows = [], notice = "", error = Nothing, save = Nothing, heartbeats = Dict.empty, saveMessage = if old.save /= Nothing then "Save cancelled because its session or authority changed." else old.saveMessage }
            else
                { withPanel | stamp = stamp, generation = old.generation + 1, pending = False, loadWindow = False, save = Nothing, heartbeats = Dict.empty, saveMessage = if old.save /= Nothing then "Save cancelled during canonical resynchronization." else old.saveMessage }
        metadata = Maybe.map2 (\ws id -> resource ws (Delivery.SessionMetadata id) model |> Maybe.andThen .data |> Maybe.andThen (\value -> case value of
            Delivery.Metadata session -> Just session
            _ -> Nothing)) workspace selected |> Maybe.withDefault Nothing
        current = case metadata of
            Just session ->
                if retired.start == Nothing then
                    let begin = max 0 (session.committedBytes - 2097152) in
                    { retired | start = Just begin, offset = begin, end = session.committedBytes, generation = retired.generation + 1 }
                else if retired.following && retired.offset >= retired.end && not retired.pending then
                    { retired | end = min session.committedBytes (retired.offset + 2097152) }
                else retired
            Nothing -> retired
        window = Maybe.map2 (\ws session -> (ws,Delivery.SessionWindow session.id current.offset current.end (session.state == Api.SessionTerminated))) workspace metadata
            |> Maybe.andThen (\target -> if current.loadWindow && current.error == Nothing && current.offset < current.end then Just target else Nothing)
        saveTarget = Maybe.map2 (\ws operation -> (ws,Delivery.SessionSave operation.session operation.offset (min 65536 (operation.size-operation.offset)))) workspace current.save
            |> Maybe.andThen (\target -> current.save |> Maybe.andThen (\operation -> if operation.phase == "writing" && operation.offset < operation.size then Just target else Nothing))
        desired = workspace |> Maybe.map (\ws ->
            (if model.activeTab == SessionsTab then [(ws,listTarget current)] else []) ++
            (selected |> Maybe.map (\id -> [(ws,Delivery.SessionMetadata id),(ws,Delivery.SessionTasks id current.linksCursor)]) |> Maybe.withDefault []) ++
            (List.map (\target -> (ws,target)) sectionReads) ++
            (List.map (\task -> (ws,Delivery.TaskSessionsFiltered task {state="active",freshness=""} Nothing)) hintTasks) ++
            (window |> Maybe.map List.singleton |> Maybe.withDefault []) ++ (saveTarget |> Maybe.map List.singleton |> Maybe.withDefault [])) |> Maybe.withDefault []
        identities = List.map (\(ws,target) -> Delivery.key ws target) desired
        kept = List.foldl (\(ws,target) state -> if List.member (Delivery.key ws target) identities then state else Delivery.removeDemand ws target state) model.sessionDelivery old.owned
        demanded = List.foldl (\(ws,target) state -> if Dict.get (Delivery.key ws target) state.resources |> Maybe.map (\existing -> existing.target == target) |> Maybe.withDefault False then state else Delivery.demand ws target state) kept desired
        chunk = window |> Maybe.andThen (\(ws,target) -> Dict.get (Delivery.key ws target) demanded.resources |> Maybe.andThen (\value -> if current.pending || value.dirty /= value.settled then Nothing else case value.data of
            Just (Delivery.Bytes range) -> if range.offset == current.offset then Just range else Nothing
            _ -> Nothing))
        consumed = case (window,chunk) of
            (Just (ws,target),Just _) -> Delivery.consume ws target demanded
            _ -> demanded
        saveRange = Maybe.map2 (\(ws,target) operation -> Dict.get (Delivery.key ws target) consumed.resources |> Maybe.andThen (\value -> if operation.pending || value.dirty /= value.settled then Nothing else case value.data of
            Just (Delivery.Bytes range) -> if range.offset == operation.offset then Just range else Nothing
            _ -> Nothing)) saveTarget current.save |> Maybe.withDefault Nothing
        savePayload = current.save |> Maybe.andThen (\operation ->
            if operation.phase /= "writing" || operation.pending then Nothing
            else if operation.size == 0 then workspace |> Maybe.map (\ws -> Encode.object
                [("session",Encode.object [("id",Encode.string operation.session),("workspace",Encode.string ws),("state",Encode.string "terminated"),("finalBytes",Encode.int operation.size),("finalSha256",Encode.string operation.hash),("revision",Encode.int operation.revision)]),("offset",Encode.int 0),("nextOffset",Encode.int 0),("dataBase64",Encode.string "")])
            else saveRange |> Maybe.map (\range -> encodeSaveChunk operation range.session range.nextOffset range.dataBase64))
        afterSave = case (saveTarget,saveRange) of
            (Just (ws,target),Just _) -> Delivery.consume ws target consumed
            _ -> consumed
        saveFailure = Maybe.map2 (\(ws,target) operation ->
            [target,Delivery.SessionMetadata operation.session] |> List.filterMap (\purpose -> Dict.get (Delivery.key ws purpose) afterSave.resources |> Maybe.andThen .error) |> List.head) saveTarget current.save |> Maybe.withDefault Nothing
        pending = { current | owned = desired, pending = current.pending || chunk /= Nothing, bridgedGeneration = current.generation
            , save = if saveFailure /= Nothing then Nothing else Maybe.map (\operation -> { operation | pending = operation.pending || savePayload /= Nothing }) current.save
            , saveMessage = if saveFailure /= Nothing then "Download could not finish. Please retry." else current.saveMessage }
        ( next, requests ) = WebSocket.pumpSessionResources { model | sessions = pending, sessionDelivery = afterSave }
        heartbeatSources = desired |> List.concatMap (\(ws,target) -> Dict.get (Delivery.key ws target) afterSave.resources |> Maybe.andThen .data |> Maybe.map (\payload -> case payload of
            Delivery.SessionsPage page -> page.items
            Delivery.Metadata session -> [session]
            _ -> []) |> Maybe.withDefault []) |> List.take 52
        bridge = Ports.sessionBridgeCommand (Encode.object
            [ ("stamp",Encode.string stamp)
            , ("generation",Encode.int current.generation)
            , ("reset",viewerReset current |> Maybe.map Encode.int |> Maybe.withDefault Encode.null)
            , ("follow",Encode.bool current.following)
            , ("chunk",chunk |> Maybe.map (\range -> Encode.object [("offset",Encode.int range.offset),("nextOffset",Encode.int range.nextOffset),("dataBase64",Encode.string range.dataBase64),("final",Encode.bool (range.nextOffset >= current.end))]) |> Maybe.withDefault Encode.null)
            , ("nextOperation",Encode.int current.nextOperation)
            , ("saveOperation",pending.save |> Maybe.map (.operation >> Encode.int) |> Maybe.withDefault Encode.null)
            , ("saveChunk",if saveFailure == Nothing then Maybe.withDefault Encode.null savePayload else Encode.null)
            , ("heartbeats",Encode.list (\session -> Encode.object [("id",Encode.string session.id),("lastSeenAt",Encode.string session.lastSeenAt)]) heartbeatSources)
            ])
    in
    ( next, Cmd.batch [ command, requests, bridge ] )


viewerReset : SessionsModel -> Maybe Int
viewerReset state =
    if state.generation /= state.bridgedGeneration then state.start else Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let state = model.sessions in
    case msg of
        SessionsNextPage cursor ->
            ( { model | sessions = { state | cursor = Just cursor, previous = List.take 16 (state.cursor :: state.previous) } }, Cmd.none )
        SessionsPreviousPage ->
            case state.previous of
                cursor :: rest -> ( { model | sessions = { state | cursor = cursor, previous = rest } }, Cmd.none )
                [] -> ( { model | sessions = { state | cursor = Nothing } }, Cmd.none )
        SessionsLinkedPage cursor -> ( { model | sessions = { state | linksCursor = cursor } }, Cmd.none )
        SessionsRetry ->
            let refresh ws delivery =
                    Delivery.invalidateLists ws delivery
                        |> (\current -> state.selected |> Maybe.map (\id -> Delivery.invalidateSession ws id current) |> Maybe.withDefault current)
                        |> (\current -> state.taskPanel |> Maybe.map (\id -> Delivery.invalidateTask ws id current) |> Maybe.withDefault current)
                        |> (\current -> List.foldl (\(ownedWorkspace,target) accumulated -> case target of
                            Delivery.SessionWindow _ _ _ _ -> Delivery.removeDemand ownedWorkspace target accumulated
                            _ -> accumulated) current state.owned)
            in
            ( {model | sessions = {state | error = Nothing,pending = False,loadWindow = True,offset = Maybe.withDefault 0 state.start,generation = state.generation + 1}
                ,sessionDelivery = state.workspace |> Maybe.map (\ws -> refresh ws model.sessionDelivery) |> Maybe.withDefault model.sessionDelivery},Cmd.none )
        SessionsStateFilter value ->
            if List.member value ["","active","terminated"] then
                let filters = state.filters in ( { model | sessions = { state | filters = { filters | state = value }, cursor = Nothing, previous = [] } }, Cmd.none )
            else (model,Cmd.none)
        SessionsFreshnessFilter value ->
            if List.member value ["","fresh","stale"] then
                let filters = state.filters in ( { model | sessions = { state | filters = { filters | freshness = value }, cursor = Nothing, previous = [] } }, Cmd.none )
            else (model,Cmd.none)
        SessionsFollow following ->
            ( { model | sessions = { state | following = following, loadWindow = following, pending = False, generation = state.generation + 1, start = if following then Nothing else state.start } }, Cmd.none )
        SessionsOlder ->
            let (begin,end) = olderWindow state in
            ( { model | sessions = { state | following = False, loadWindow = True, start = Just begin, offset = begin, end = end, pending = False, generation = state.generation + 1, rows = [], error = Nothing } }, Cmd.none )
        SessionsTick time ->
            let now = toFloat (Time.posixToMillis time)
                refresh = state.filters.freshness /= "" && model.activeTab == SessionsTab && now-state.lastFilterRefresh >= 10000
            in
            ( {model | sessions = {state | now = now, lastFilterRefresh = if refresh then now else state.lastFilterRefresh}
                , sessionDelivery = if refresh then state.workspace |> Maybe.map (\ws -> Delivery.invalidateLists ws model.sessionDelivery) |> Maybe.withDefault model.sessionDelivery else model.sessionDelivery }, Cmd.none )
        SessionsBridgeEvent value -> receiveBridge value model
        SessionsCancelSave -> ( {model | sessions = {state | save = Nothing, saveMessage = "Save cancellation requested."}}, Cmd.none )
        SessionsTaskNextPage task cursor ->
            ( {model | sessions = {state | taskCursors = Dict.insert task (Just cursor) state.taskCursors, taskPreviousPages = Dict.insert task (List.take 16 (taskCursor model task :: (Dict.get task state.taskPreviousPages |> Maybe.withDefault []))) state.taskPreviousPages}}, Cmd.none )
        SessionsTaskPreviousPage task ->
            case Dict.get task state.taskPreviousPages |> Maybe.withDefault [] of
                cursor::rest -> ( {model | sessions = {state | taskCursors = Dict.insert task cursor state.taskCursors, taskPreviousPages = Dict.insert task rest state.taskPreviousPages}}, Cmd.none )
                [] -> (model,Cmd.none)
        _ -> ( model, Cmd.none )


olderWindow : SessionsModel -> ( Int, Int )
olderWindow state =
    let
        -- A retained record's start is an exact JSONL boundary. End the older
        -- page there so the row cap cannot strand records between byte windows.
        end = List.head state.rows |> Maybe.map .offset |> Maybe.withDefault (min state.end (Maybe.withDefault 0 state.start + 1048578))
    in (max 0 (end-2097152),end)


receiveBridge : Decode.Value -> Model -> ( Model, Cmd Msg )
receiveBridge value model =
    let
        state = model.sessions
        field name decoder fallback = Decode.decodeValue (Decode.field name decoder) value |> Result.withDefault fallback
        stamp = field "stamp" Decode.string ""
        generation = field "generation" Decode.int -1
        through = field "through" Decode.int -1
        kind = field "kind" Decode.string ""
        row = Decode.map8 SessionRecord (Decode.field "offset" Decode.int) (Decode.field "end" Decode.int) (Decode.field "raw" Decode.string) (Decode.field "delimiter" Decode.string) (Decode.field "type" Decode.string) (Decode.field "role" Decode.string) (Decode.field "time" Decode.string) (Decode.field "text" Decode.string)
    in
    if kind == "heartbeat-status" && stamp == state.stamp then
        case Decode.decodeValue (Decode.field "statuses" (Decode.dict Decode.string)) value of
            Ok statuses -> if Dict.size statuses <= 52 then ( {model | sessions = {state | heartbeats = statuses}}, Cmd.none ) else (model,Cmd.none)
            Err _ -> (model,Cmd.none)
    else if kind == "save-capability" then
        ( {model | sessions = {state | saveCapability = Just (field "supported" Decode.bool False)}}, Cmd.none )
    else if String.startsWith "save-" kind then receiveSave value model
    else if stamp /= state.stamp || generation /= state.generation || not state.pending then (model,Cmd.none)
    else if field "kind" Decode.string "" == "viewer-error" then
        ( {model | sessions = {state | pending = False, error = Just "Record window could not be decoded. Retry or load another window."}}, Cmd.none )
    else if through <= state.offset || through > min state.end (state.offset + 65536) then (model,Cmd.none)
    else case Decode.decodeValue (Decode.field "rows" (Decode.list row)) value of
        Ok rows ->
            if List.length rows <= 100 && List.sum (List.map (.raw >> String.length) rows) <= 2097152 then
                ( {model | sessions = {state | offset = through, pending = False, rows = rows, notice = field "notice" Decode.string ""}}, Cmd.none )
            else ( {model | sessions = {state | pending = False,error = Just "Viewer row budget exceeded."}}, Cmd.none )
        Err _ -> ( {model | sessions = {state | pending = False,error = Just "Invalid viewer response."}}, Cmd.none )


encodeReceipt : Api.AgentSession -> Encode.Value
encodeReceipt session = Encode.object
    [("id",Encode.string session.id),("workspace",Encode.string session.workspaceId),("state",Encode.string (if session.state == Api.SessionTerminated then "terminated" else "active"))
    ,("finalBytes",session.finalBytes |> Maybe.map Encode.int |> Maybe.withDefault Encode.null),("finalSha256",session.finalSha256 |> Maybe.map Encode.string |> Maybe.withDefault Encode.null),("revision",Encode.int session.revision)]


encodeSaveChunk : SessionSaveOperation -> Api.AgentSession -> Int -> String -> Encode.Value
encodeSaveChunk operation session next bytes = Encode.object
    [("session",encodeReceipt session),("offset",Encode.int operation.offset),("nextOffset",Encode.int next),("dataBase64",Encode.string bytes)]


receiveSave : Decode.Value -> Model -> ( Model, Cmd Msg )
receiveSave value model =
    let
        state = model.sessions
        field name decoder fallback = Decode.decodeValue (Decode.field name decoder) value |> Result.withDefault fallback
        stamp = field "stamp" Decode.string ""
        operation = field "operation" Decode.int -1
        kind = field "kind" Decode.string ""
        requestedId = Decode.decodeValue (Decode.at ["session","id"] Decode.string) value |> Result.toMaybe
        metadata = availableSessions model |> List.filter (\session -> Just session.id == requestedId) |> List.head
    in
    if stamp /= state.stamp || model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) then (model,Cmd.none)
    else if kind == "save-picking" && operation == state.nextOperation && state.save == Nothing then
        case metadata of
            Just session ->
                case (session.finalBytes,session.finalSha256) of
                    (Just size,Just hash) ->
                        if session.state == Api.SessionTerminated then
                            ( {model | sessions = {state | save = Just (SessionSaveOperation operation session.id size hash session.revision 0 "picking" False), nextOperation = operation + 1, saveMessage = "Preparing download…"}}, Cmd.none )
                        else (model,Cmd.none)
                    _ -> (model,Cmd.none)
            Nothing -> (model,Cmd.none)
    else case state.save of
        Just save ->
            if save.operation /= operation then (model,Cmd.none)
            else case kind of
                "save-ready" -> ( {model | sessions = {state | save = Just {save | phase = "writing"}, saveMessage = "Saving verified evidence…"}}, Cmd.none )
                "save-error" ->
                    if field "code" Decode.string "" == "writer_cleanup_failed" then
                        ( {model | sessions = {state | save = Nothing, saveCapability = Just False, saveMessage = "Writer cleanup could not be confirmed. Reload this page before another save."}}, Cmd.none )
                    else ( {model | sessions = {state | save = Nothing, saveMessage = if field "code" Decode.string "" == "download_storage_unavailable" then "This browser cannot download sessions larger than 32 MiB without temporary file storage." else "Download could not finish. Please retry."}}, Cmd.none )
                _ ->
                    let offset = field "offset" Decode.int -1 in
                    if not save.pending || offset < save.offset || offset > min save.size (save.offset + 65536) || (offset == save.offset && save.size /= 0) then (model,Cmd.none)
                    else if kind == "save-complete" && offset == save.size then
                        ( {model | sessions = {state | save = Nothing, saveMessage = "Downloaded " ++ String.fromInt save.size ++ " bytes."}}, Cmd.none )
                    else if kind == "save-progress" && offset < save.size then
                        ( {model | sessions = {state | save = Just {save | offset = offset, pending = False}, saveMessage = "Saving " ++ String.fromInt offset ++ " / " ++ String.fromInt save.size ++ " bytes…"}}, Cmd.none )
                    else (model,Cmd.none)
        Nothing -> (model,Cmd.none)


resource : String -> Delivery.Target -> Model -> Maybe Delivery.Resource
resource workspace target model = Dict.get (Delivery.key workspace target) model.sessionDelivery.resources


sessionUrl : String -> String -> String
sessionUrl workspace id = "/workspace/" ++ Url.percentEncode workspace ++ "#tab=sessions&session=" ++ Url.percentEncode id


view : String -> Model -> Html Msg
view workspace model =
    let list = resource workspace (listTarget model.sessions) model in
    section [ class "sessions-page", attribute "aria-label" "Agent sessions" ]
        [ div [ class "sessions-toolbar" ]
            [ h2 [] [ text "Sessions" ]
            , span [ class "card-meta sessions-connection" ] [ text (case model.webSocket.state of
                Connected -> "Live"
                Connecting -> "Reconnecting…"
                _ -> "Offline") ]
            , button [ class "btn btn-sm", onClick SessionsRetry ] [ text "Refresh" ] ]
        , model.sessions.taskFilter |> Maybe.map (\task -> div [class "sessions-task-filter"] [text ("Sessions linked to " ++ (Dict.get task model.tasks |> Maybe.map .title |> Maybe.withDefault (String.left 8 task))),a [href ("/workspace/" ++ Url.percentEncode workspace ++ "#tab=sessions"),class "btn btn-sm"] [text "Clear"]]) |> Maybe.withDefault (text "")
        , div [class "sessions-filters"]
            [label [] [text "Status ",select [attribute "aria-label" "Status",onInput SessionsStateFilter]
                [option [value "",Html.Attributes.selected (model.sessions.filters.state == "")] [text "All"],option [value "active",Html.Attributes.selected (model.sessions.filters.state == "active")] [text "Active"],option [value "terminated",Html.Attributes.selected (model.sessions.filters.state == "terminated")] [text "Saved"]]]
            ,label [] [text "Activity ",select [attribute "aria-label" "Activity",onInput SessionsFreshnessFilter]
                [option [value "",Html.Attributes.selected (model.sessions.filters.freshness == "")] [text "All"],option [value "fresh",Html.Attributes.selected (model.sessions.filters.freshness == "fresh")] [text "Live"],option [value "stale",Html.Attributes.selected (model.sessions.filters.freshness == "stale")] [text "Quiet"]]]]
        , div [ class "sessions-layout" ]
            [ div [ class "sessions-list" ]
                [ case list |> Maybe.andThen .error of
                    Just _ -> div [ attribute "role" "alert" ] [ text "Sessions could not be loaded. ", button [ onClick SessionsRetry ] [text "Retry"] ]
                    Nothing -> case list |> Maybe.andThen .data of
                        Just (Delivery.SessionsPage page) -> div []
                            [ if List.isEmpty page.items then p [ class "empty-state" ] [text "No sessions match these filters."]
                              else ul [class "session-summaries"] (List.map (sessionSummary workspace model) page.items)
                            , div [class "sessions-pagination"]
                                [ button [class "btn btn-sm", disabled (model.sessions.cursor == Nothing), onClick SessionsPreviousPage] [text "Newer"]
                                , page.nextCursor |> Maybe.map (\cursor -> button [class "btn btn-sm",onClick (SessionsNextPage cursor)] [text "Older sessions"]) |> Maybe.withDefault (text "") ] ]
                        _ -> p [ attribute "role" "status" ] [text "Loading sessions…"] ]
            , model.sessions.selected |> Maybe.map (viewDetail workspace model) |> Maybe.withDefault (div [class "empty-state session-detail"] [h3 [] [text "Inspect an agent session"],p [] [text "Select a session to see its activity and linked tasks."]]) ] ]


sessionSummary : String -> Model -> Api.AgentSession -> Html Msg
sessionSummary workspace model session =
    li [ class ("session-summary" ++ (if model.sessions.selected == Just session.id then " selected" else "") ++ (if session.state == Api.SessionActive then " is-active" else "")) ]
        [ a [ href (sessionUrl workspace session.id ++ (model.sessions.taskFilter |> Maybe.map (\task -> "&task=" ++ Url.percentEncode task) |> Maybe.withDefault "")), class "session-summary-link" ]
            [ span [class "session-summary-heading"] [strong [] [text session.formatLabel], span [class "session-state-dot",attribute "aria-label" (if session.state == Api.SessionActive then "Active" else "Terminated")] []]
            , span [class "card-meta"] [text (String.fromInt session.committedRecords ++ " records · " ++ (if session.state == Api.SessionTerminated then "Saved" else Dict.get session.id model.sessions.heartbeats |> Maybe.map (\status -> if status == "Fresh" then "Live" else if status == "Stale (30s)" then "Quiet" else "Active") |> Maybe.withDefault "Active"))]
            , span [class "session-short-id"] [text (TimestampDisplay.lookup model.timestamps session.createdAt)] ] ]


viewDetail : String -> Model -> String -> Html Msg
viewDetail workspace model id =
    div [ class "session-detail" ]
        [ case resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .data of
            Just (Delivery.Metadata session) -> div [class "session-detail-header"]
                [ div [] [h3 [] [text session.formatLabel],span [class "card-meta"] [text (if session.state == Api.SessionTerminated then "Saved session" else "Active session")]]
                ,viewSave model session
                ,a [class "btn btn-sm",href ("/workspace/" ++ Url.percentEncode workspace ++ "#tab=sessions" ++ (model.sessions.taskFilter |> Maybe.map (\task -> "&task=" ++ Url.percentEncode task) |> Maybe.withDefault "")),attribute "aria-label" "Close session"] [text "✕"]]
            _ -> resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .error |> Maybe.map (\_ -> p [attribute "role" "alert"] [text "Session could not be loaded.",button [onClick SessionsRetry] [text "Retry"]]) |> Maybe.withDefault (p [attribute "role" "status"] [text "Loading session…"])
        , details [class "session-linked-tasks"]
            [ summary [] [text "Linked tasks"]
            , case resource workspace (Delivery.SessionTasks id model.sessions.linksCursor) model |> Maybe.andThen .data of
                Just (Delivery.TasksPage page) -> div []
                    [div [class "session-task-cards"] (List.map (\task -> a [class "session-task-card",href ("/workspace/" ++ Url.percentEncode workspace ++ "#tab=projects&focus=" ++ Url.percentEncode ("task:" ++ task.id))] [strong [] [text task.title],span [class "badge"] [text task.status]]) page.items)
                    ,if List.isEmpty page.items then p [class "card-meta"] [text "No linked tasks."] else text ""
                    ,page.nextTaskId |> Maybe.map (\cursor -> button [class "btn btn-sm",onClick (SessionsLinkedPage (Just cursor))] [text "More tasks"]) |> Maybe.withDefault (text "")
                    ,if model.sessions.linksCursor /= Nothing then button [class "btn btn-sm",onClick (SessionsLinkedPage Nothing)] [text "First tasks"] else text ""]
                _ -> p [class "card-meta"] [text "Loading linked tasks…"] ]
        , viewRecords workspace model id
        ,p [class "session-download-status",attribute "role" "status",attribute "aria-live" "polite"] [text model.sessions.saveMessage] ]


viewSave : Model -> Api.AgentSession -> Html Msg
viewSave model session =
    let
        state = model.sessions
        config = Encode.encode 0 (Encode.object [("stamp",Encode.string state.stamp),("operation",Encode.int state.nextOperation),("session",encodeReceipt session)])
    in
    if session.state /= Api.SessionTerminated then text ""
    else button [class "btn btn-sm session-download",attribute "data-session-download" config,disabled (state.save /= Nothing || session.finalBytes == Nothing || session.finalSha256 == Nothing),attribute "aria-label" ("Download session " ++ String.left 8 session.id)] [text "↓ Download"]


sectionKey : Bool -> String -> String
sectionKey evidence task = (if evidence then "terminated:" else "active:") ++ task


sectionTarget : Bool -> Model -> String -> Delivery.Target
sectionTarget evidence model task = Delivery.TaskSessionsFiltered task {state=if evidence then "terminated" else "active",freshness=""} (taskCursor model (sectionKey evidence task))


taskPage : Bool -> String -> Model -> String -> Maybe Api.SessionPage
taskPage evidence workspace model task =
    let
        target = sectionTarget evidence model task
        resident = resource workspace target model |> Maybe.andThen .data |> Maybe.andThen (\payload -> case payload of
            Delivery.SessionsPage page -> Just page
            _ -> Nothing)
        cached = Dict.get (sectionKey evidence task) model.sessions.taskPages |> Maybe.andThen (\page -> if page.cursor == taskCursor model (sectionKey evidence task) then Result.toMaybe page.result else Nothing)
    in
    case resident of
        Just page -> Just page
        Nothing -> cached


taskActive : String -> Model -> String -> Bool
taskActive workspace model task = Dict.get task model.sessions.taskHints |> Maybe.map (\hint -> hint.active && not hint.error) |> Maybe.withDefault False


viewTaskIcon : String -> Model -> String -> Html Msg
viewTaskIcon workspace model task =
    let
        hint = Dict.get task model.sessions.taskHints
        count = hint |> Maybe.andThen (\value -> if value.error then Nothing else Just (if value.active then 1 else 0))
        label = "Sessions for this task" ++ (case hint of
            Just current -> if current.error then " · activity unavailable" else if current.active then " · active sessions" ++ (if current.epoch /= model.sessionDelivery.listInvalidation then " · refreshing" else "") else if current.epoch /= model.sessionDelivery.listInvalidation then " · checking activity" else " · no active sessions"
            Nothing -> " · checking activity")
    in
    a [class ("btn-icon task-sessions-icon" ++ (if Maybe.withDefault 0 count > 0 then " has-active-sessions" else "")),href ("/workspace/" ++ Url.percentEncode workspace ++ "#tab=sessions&task=" ++ Url.percentEncode task),attribute "aria-label" label,Html.Attributes.title label] [text "◉",text ""]


viewTaskPanel : String -> Model -> String -> Html Msg
viewTaskPanel workspace model task = div [] [viewTaskActive workspace model task,viewTaskEvidence workspace model task]


viewTaskActive : String -> Model -> String -> Html Msg
viewTaskActive = viewTaskSection False


viewTaskEvidence : String -> Model -> String -> Html Msg
viewTaskEvidence = viewTaskSection True


viewTaskSection : Bool -> String -> Model -> String -> Html Msg
viewTaskSection evidence workspace model task =
    let
        page = taskPage evidence workspace model task
        key = sectionKey evidence task
        sessions = page |> Maybe.map (.items >> List.filter (\session -> (session.state == Api.SessionTerminated) == evidence)) |> Maybe.withDefault []
        heading = if evidence then "Evidence" else "Active sessions"
        entry = resource workspace (sectionTarget evidence model task) model
        cachedError = Dict.get key model.sessions.taskPages |> Maybe.andThen (\cached -> case cached.result of
            Err message -> Just message
            _ -> Nothing)
    in
    details [class ("task-session-section " ++ (if evidence then "task-session-evidence" else "task-session-active"))]
        [summary [] [text (heading ++ (if page /= Nothing then " · " ++ String.fromInt (List.length sessions) else ""))]
        ,if page == Nothing then p [class "card-meta"] [text (if cachedError /= Nothing then "Sessions could not be loaded." else "Loading sessions…"),if cachedError /= Nothing then button [class "btn btn-sm",onClick SessionsRetry] [text "Retry"] else text ""]
         else if List.isEmpty sessions then p [class "card-meta"] [text ("No " ++ String.toLower heading ++ " on this page.")]
         else div [class "task-session-cards"] (List.map (\session -> div [class "task-session-card"] [a [href (sessionUrl workspace session.id)] [strong [] [text session.formatLabel],span [class "card-meta"] [text (TimestampDisplay.lookup model.timestamps session.createdAt ++ " · " ++ String.fromInt session.committedRecords ++ " records")]],if evidence then viewSave model session else span [class "session-state-dot",attribute "aria-label" "Active"] []]) sessions)
        ,div [class "sessions-pagination"]
            [if taskCursor model key /= Nothing then button [class "btn btn-sm",onClick (SessionsTaskPreviousPage key)] [text "Newer sessions"] else text ""
            ,page |> Maybe.andThen .nextCursor |> Maybe.map (\cursor -> button [class "btn btn-sm",onClick (SessionsTaskNextPage key cursor)] [text "Older sessions"]) |> Maybe.withDefault (text "")]
        ,if evidence && model.sessions.saveMessage /= "" then p [class "card-meta",attribute "role" "status"] [text model.sessions.saveMessage] else text ""]


viewRecords : String -> Model -> String -> Html Msg
viewRecords workspace model id =
    let
        state = model.sessions
        session = resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .data |> Maybe.andThen (\payload -> case payload of
            Delivery.Metadata value -> Just value
            _ -> Nothing)
        newest = session |> Maybe.map .committedBytes |> Maybe.withDefault state.offset
        rangeError = resource workspace (Delivery.SessionWindow id state.offset state.end False) model |> Maybe.andThen .error
    in
    section [class "session-viewer",attribute "aria-label" "Session transcript"]
        [div [class "sessions-toolbar"]
            [h4 [] [text "Transcript"]
            ,button [class "btn btn-sm",onClick (SessionsFollow (not state.following))] [text (if state.following then "Pause" else "Follow tail")]
            ,button [class "btn btn-sm",disabled ((List.head state.rows |> Maybe.map .offset |> Maybe.withDefault (Maybe.withDefault 0 state.start)) <= 0),onClick SessionsOlder] [text "Older messages"]]
        ,p [class "card-meta",attribute "aria-live" "polite"]
            [text ((if state.following then "Following" else "Paused")
                ++ (if not state.following && newest > state.offset then " · " ++ String.fromInt (newest-state.offset) ++ " new bytes" else ""))]
        ,if state.notice /= "" then p [class "session-notice"] [text state.notice] else text ""
        ,if state.error /= Nothing || rangeError /= Nothing then p [attribute "role" "alert"] [text (Maybe.withDefault "Transcript request failed." state.error),button [onClick SessionsRetry] [text "Retry"]] else text ""
        ,Keyed.node "div" [Html.Attributes.id "session-records",class "session-records",tabindex 0,attribute "aria-label" "Bounded transcript records"]
            (List.map (\record -> (String.fromInt record.offset,viewRecord record)) state.rows)
        ,if List.isEmpty state.rows then text "" else details [class "session-raw-details"]
            [summary [] [text "Original JSON"],div [] (List.map (\record -> pre [class "session-record-raw"] [text (record.raw ++ record.delimiter)]) state.rows)]
        ,if List.isEmpty state.rows then p [attribute "role" "status"] [text (if state.start == Nothing then "Loading transcript metadata…" else if state.offset < state.end then "Reading bounded record window…" else if newest == 0 then "This session has no committed records." else "No complete records in this window. Load an older window to inspect its leading record.")] else text ""
        ,text "" ]



viewRecord : SessionRecord -> Html Msg
viewRecord record =
    let
        blocks = Decode.decodeString (Decode.list (Decode.map3 (\kind content label -> (kind,content,label)) (Decode.field "kind" Decode.string) (Decode.field "text" Decode.string) (Decode.field "label" Decode.string))) record.content |> Result.withDefault []
        activity = record.recordType == "conversation" && not (List.any (\(kind,_,_) -> kind == "text") blocks)
        markdown content =
            case Markdown.Parser.parse content |> Result.mapError (always "parse error") |> Result.andThen (Markdown.Renderer.render sessionMarkdownRenderer) of
                Ok rendered -> div [class "markdown-content session-record-text"] rendered
                Err _ -> pre [class "session-record-text"] [text content]
        viewBlock (kind,content,label) =
            if kind == "text" then markdown content
            else details [class ("session-content-" ++ kind)] [summary [] [text (if label /= "" then label else if kind == "thinking" then "Thinking" else "Tool activity")],if kind == "thinking" then markdown content else pre [class "session-tool-content"] [text content]]
    in
    article [class ("session-record session-role-" ++ record.role ++ (if activity then " session-record-activity" else "")),attribute "data-offset" (String.fromInt record.offset)]
        [if record.recordType == "unknown" || activity then text "" else div [class "session-record-meta"] [text (if record.role == "user" then "You" else if record.role == "tool" then "Tool" else if record.role == "assistant" then "Assistant" else "Message")]
        ,if record.recordType == "conversation" then div [] (List.map viewBlock blocks) else if record.recordType == "unknown" then text "" else markdown record.content
        ,if record.recordType == "unknown" then details [class "session-original"] [summary [] [text "Unrecognized record"],pre [class "session-record-raw"] [text (record.raw ++ record.delimiter)]] else text "" ]


sessionMarkdownRenderer : Markdown.Renderer.Renderer (Html Msg)
sessionMarkdownRenderer =
    let
        base = Markdown.Renderer.defaultHtmlRenderer
        safe destination =
            let normalized = String.toLower (String.trim destination) in
            String.startsWith "https://" normalized || String.startsWith "http://" normalized || String.startsWith "mailto:" normalized || String.startsWith "#" normalized
    in
    { base | link = \link children -> if safe link.destination then a [href link.destination,attribute "rel" "noopener noreferrer"] children else span [] children
        , image = \info -> text ("[Image: " ++ info.alt ++ "]") }


timestampSources : Model -> List String
timestampSources model =
    let
        list = model.sessions.workspace |> Maybe.andThen (\ws -> resource ws (listTarget model.sessions) model |> Maybe.andThen .data) |> Maybe.map (\payload -> case payload of
            Delivery.SessionsPage page -> List.map .createdAt page.items
            _ -> []) |> Maybe.withDefault []
        cards = Dict.values model.sessions.taskPages |> List.concatMap (\cached -> Result.toMaybe cached.result |> Maybe.map (.items >> List.map .createdAt) |> Maybe.withDefault [])
    in
    if model.activeTab == SessionsTab then list else if model.activeTab == ProjectsTab then cards else []


availableSessions : Model -> List Api.AgentSession
availableSessions model =
    (Dict.values model.sessionDelivery.resources |> List.filter (\entry -> Just entry.workspace == model.sessions.workspace) |> List.concatMap (\entry -> case entry.data of
        Just (Delivery.Metadata session) -> [session]
        Just (Delivery.SessionsPage page) -> page.items
        _ -> [])) ++ (Dict.values model.sessions.taskPages |> List.concatMap (\cached -> Result.toMaybe cached.result |> Maybe.map .items |> Maybe.withDefault []))
