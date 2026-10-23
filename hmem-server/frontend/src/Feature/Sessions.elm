module Feature.Sessions exposing (reconcile, update, view, viewTaskPanel, olderWindow, viewerReset)

import Api
import ActorAttribution
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
import Time
import Types exposing (..)
import Url


selectedFromUrl : Model -> Maybe String
selectedFromUrl model =
    model.url.fragment |> Maybe.withDefault "" |> String.split "&"
        |> List.filterMap (\pair -> case String.split "=" pair of
            [ "session", value ] -> Url.percentDecode value
            _ -> Nothing)
        |> List.head
        |> Maybe.andThen (\id -> if String.length id == 36 && List.map String.length (String.split "-" id) == [8,4,4,4,12] && String.all (\c -> String.contains (String.fromChar c) "0123456789abcdef-") id then Just id else Nothing)


reconcile : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
reconcile ( model, command ) =
    let
        old = model.sessions
        workspace = if model.auth.status == AuthReady && Permissions.canReadCurrentWorkspace model && (model.selectedWorkspaceId |> Maybe.map (\ws -> model.page == WorkspacePage ws) |> Maybe.withDefault False) then model.selectedWorkspaceId else Nothing
        selected = if workspace /= Nothing && model.activeTab == SessionsTab then selectedFromUrl model else Nothing
        taskPanel = if workspace /= Nothing && model.activeTab == ProjectsTab then model.focus.focusedEntity |> Maybe.andThen (\(kind,id) -> if kind == "task" && Dict.member id model.tasks then Just id else Nothing) else Nothing
        stamp = Encode.encode 0 (Encode.list Encode.string [Maybe.withDefault "" workspace, String.fromInt model.sessionRequestEpoch, model.sessionContext |> Maybe.map (.principal >> .actorId) |> Maybe.withDefault "", String.fromInt model.sessionDelivery.retirement, Maybe.withDefault "" selected])
        reset = old.workspace /= workspace || old.epoch /= model.sessionRequestEpoch
        scoped = if reset then { old | workspace = workspace, epoch = model.sessionRequestEpoch, selected = selected, cursor = Nothing, previous = [], linksCursor = Nothing, filters = Api.defaultSessionFilters }
            else if old.selected /= selected then { old | selected = selected, linksCursor = Nothing } else old
        withPanel = if scoped.taskPanel /= taskPanel then {scoped | taskPanel = taskPanel, taskCursor = Nothing, taskPrevious = []} else scoped
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
            (if model.activeTab == SessionsTab then [(ws,Delivery.WorkspaceSessionsFiltered current.filters current.cursor)] else []) ++
            (selected |> Maybe.map (\id -> [(ws,Delivery.SessionMetadata id),(ws,Delivery.SessionTasks id current.linksCursor)]) |> Maybe.withDefault []) ++
            (taskPanel |> Maybe.map (\task -> [(ws,Delivery.TaskSessions task current.taskCursor)]) |> Maybe.withDefault []) ++
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
            else if operation.size == 0 then metadata |> Maybe.map (\session -> encodeSaveChunk operation session 0 "")
            else saveRange |> Maybe.map (\range -> encodeSaveChunk operation range.session range.nextOffset range.dataBase64))
        afterSave = case (saveTarget,saveRange) of
            (Just (ws,target),Just _) -> Delivery.consume ws target consumed
            _ -> consumed
        saveFailure = saveTarget |> Maybe.andThen (\(ws,target) -> Dict.get (Delivery.key ws target) afterSave.resources |> Maybe.andThen .error)
        pending = { current | owned = desired, pending = current.pending || chunk /= Nothing, bridgedGeneration = current.generation
            , save = if saveFailure /= Nothing then Nothing else Maybe.map (\operation -> { operation | pending = operation.pending || savePayload /= Nothing }) current.save
            , saveMessage = if saveFailure /= Nothing then "Evidence request failed. The partial save was aborted." else current.saveMessage }
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
        SessionsTaskNextPage cursor -> ( {model | sessions = {state | taskCursor = Just cursor, taskPrevious = List.take 16 (state.taskCursor :: state.taskPrevious)}}, Cmd.none )
        SessionsTaskPreviousPage -> case state.taskPrevious of
            cursor::rest -> ( {model | sessions = {state | taskCursor = cursor, taskPrevious = rest}}, Cmd.none )
            [] -> ( {model | sessions = {state | taskCursor = Nothing}}, Cmd.none )
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
        metadata = Maybe.map2 (\ws id -> resource ws (Delivery.SessionMetadata id) model |> Maybe.andThen .data |> Maybe.andThen (\payload -> case payload of
            Delivery.Metadata session -> Just session
            _ -> Nothing)) state.workspace state.selected |> Maybe.withDefault Nothing
    in
    if stamp /= state.stamp || model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) then (model,Cmd.none)
    else if kind == "save-picking" && operation == state.nextOperation && state.save == Nothing then
        case metadata of
            Just session ->
                case (session.finalBytes,session.finalSha256) of
                    (Just size,Just hash) ->
                        if session.state == Api.SessionTerminated then
                            ( {model | sessions = {state | save = Just (SessionSaveOperation operation session.id size hash session.revision 0 "picking" False), nextOperation = operation + 1, saveMessage = "Choose where to save the original evidence."}}, Cmd.none )
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
                    else ( {model | sessions = {state | save = Nothing, saveMessage = "Save cancelled or failed; any opened partial writer was aborted."}}, Cmd.none )
                _ ->
                    let offset = field "offset" Decode.int -1 in
                    if not save.pending || offset < save.offset || offset > min save.size (save.offset + 65536) || (offset == save.offset && save.size /= 0) then (model,Cmd.none)
                    else if kind == "save-complete" && offset == save.size then
                        ( {model | sessions = {state | save = Nothing, saveMessage = "Saved " ++ String.fromInt save.size ++ " original bytes. SHA-256 verified."}}, Cmd.none )
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
    let list = resource workspace (Delivery.WorkspaceSessionsFiltered model.sessions.filters model.sessions.cursor) model in
    section [ class "sessions-page", attribute "aria-label" "Agent sessions" ]
        [ div [ class "sessions-toolbar" ]
            [ h2 [] [ text "Sessions" ]
            , span [ class "card-meta" ] [ text (case model.webSocket.state of
                Connected -> "Live updates connected"
                Connecting -> "Reconnecting…"
                _ -> "Live updates disconnected") ]
            , button [ class "btn btn-sm", onClick SessionsRetry ] [ text "Refresh" ] ]
        , div [class "sessions-filters"]
            [label [] [text "Lifecycle ",select [attribute "aria-label" "Lifecycle",onInput SessionsStateFilter]
                [option [value "",Html.Attributes.selected (model.sessions.filters.state == "")] [text "All"],option [value "active",Html.Attributes.selected (model.sessions.filters.state == "active")] [text "Active"],option [value "terminated",Html.Attributes.selected (model.sessions.filters.state == "terminated")] [text "Terminated"]]]
            ,label [] [text "Heartbeat ",select [attribute "aria-label" "Heartbeat",onInput SessionsFreshnessFilter]
                [option [value "",Html.Attributes.selected (model.sessions.filters.freshness == "")] [text "All"],option [value "fresh",Html.Attributes.selected (model.sessions.filters.freshness == "fresh")] [text "Fresh"],option [value "stale",Html.Attributes.selected (model.sessions.filters.freshness == "stale")] [text "Stale (30s)"]]]]
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
    li [ class ("session-summary" ++ if model.sessions.selected == Just session.id then " selected" else "") ]
        [ a [ href (sessionUrl workspace session.id) ] [ strong [] [text session.formatLabel], code [] [text session.id] ]
        , span [class "badge"] [text (if session.state == Api.SessionTerminated then "Terminated" else "Active")]
        , span [class "badge session-freshness"] [text (Dict.get session.id model.sessions.heartbeats |> Maybe.withDefault "Checking heartbeat…")]
        , p [class "card-meta"] [text (String.fromInt session.committedBytes ++ " bytes · " ++ String.fromInt session.committedRecords ++ " records")]
        , p [class "card-meta"] [text ("Last heartbeat: " ++ session.lastSeenAt)] ]


viewDetail : String -> Model -> String -> Html Msg
viewDetail workspace model id =
    div [ class "session-detail" ]
        [ a [href ("/workspace/" ++ Url.percentEncode workspace ++ "#tab=sessions")] [text "Close session"]
        , case resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .data of
            Just (Delivery.Metadata session) -> div []
                [h3 [] [text session.formatLabel],code [] [text session.id]
                ,p [] [text (if session.state == Api.SessionTerminated then "Terminated · immutable evidence" else "Active · committed server prefix")]
                ,ActorAttribution.viewInline session.creator
                ,p [class "card-meta"] [text ("Heartbeat: " ++ (Dict.get session.id model.sessions.heartbeats |> Maybe.withDefault "checking…") ++ " · " ++ session.lastSeenAt)]
                ,p [] [text (String.fromInt session.committedBytes ++ " bytes · revision " ++ String.fromInt session.revision)]
                ,session.finalSha256 |> Maybe.map (\hash -> p [class "session-hash"] [text "SHA-256 ",code [] [text hash]]) |> Maybe.withDefault (text "")
                ,viewSave model session]
            _ -> resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .error |> Maybe.map (\_ -> p [attribute "role" "alert"] [text "Session metadata could not be loaded.",button [onClick SessionsRetry] [text "Retry"]]) |> Maybe.withDefault (p [attribute "role" "status"] [text "Loading session metadata…"])
        , viewRecords workspace model id
        , h4 [] [text "Linked tasks"]
        , case resource workspace (Delivery.SessionTasks id model.sessions.linksCursor) model |> Maybe.andThen .data of
            Just (Delivery.TasksPage page) -> div []
                [ul [] (List.map (\task -> li [] [a [href ("/workspace/" ++ Url.percentEncode workspace ++ "#tab=projects&focus=" ++ Url.percentEncode ("task:" ++ task.id))] [text task.title],text (" · " ++ task.status)]) page.items)
                ,if List.isEmpty page.items then p [] [text "No linked tasks."] else text ""
                ,page.nextTaskId |> Maybe.map (\cursor -> button [onClick (SessionsLinkedPage (Just cursor))] [text "More linked tasks"]) |> Maybe.withDefault (text "")
                ,if model.sessions.linksCursor /= Nothing then button [onClick (SessionsLinkedPage Nothing)] [text "First linked tasks"] else text ""]
            _ -> p [] [text "Loading linked tasks…"] ]


viewSave : Model -> Api.AgentSession -> Html Msg
viewSave model session =
    let
        state = model.sessions
        config = Encode.encode 0 (Encode.object [("stamp",Encode.string state.stamp),("operation",Encode.int state.nextOperation),("session",encodeReceipt session)])
    in
    if session.state /= Api.SessionTerminated then text ""
    else div [class "session-save"]
        [button [class "btn btn-primary",attribute "data-session-save" config,disabled (state.saveCapability /= Just True || state.save /= Nothing || session.finalBytes == Nothing || session.finalSha256 == Nothing)] [text "Save original evidence"]
        ,if state.save /= Nothing then button [class "btn btn-sm",onClick SessionsCancelSave] [text "Cancel save"] else text ""
        ,if state.saveCapability == Just False then p [] [text "Streaming file save is unavailable in this browser. Use an authorized MCP session_evidence request and follow its bounded continuation."] else text ""
        ,p [attribute "role" "status",attribute "aria-live" "polite"] [text state.saveMessage]
        ,details [] [summary [] [text "Bounded MCP evidence retrieval"],p [] [text "Call session_evidence with this workspace_id and session_id. Preserve each data_base64 chunk and use next_request until complete; compare the final byte count and SHA-256 above."]]
        ]


viewTaskPanel : String -> Model -> String -> Html Msg
viewTaskPanel workspace model task =
    section [class "task-session-panel",attribute "aria-label" "Task sessions and evidence"]
        [h4 [] [text "Sessions and evidence"]
        ,case resource workspace (Delivery.TaskSessions task model.sessions.taskCursor) model of
            Just entry -> case entry.data of
                Just (Delivery.SessionsPage page) -> div []
                    [if List.isEmpty page.items then p [class "card-meta"] [text "No sessions are linked to this task."] else ul [class "session-summaries"] (List.map (sessionSummary workspace model) page.items)
                    ,div [class "sessions-pagination"]
                        [button [class "btn btn-sm",disabled (model.sessions.taskCursor == Nothing),onClick SessionsTaskPreviousPage] [text "Newer task sessions"]
                        ,page.nextCursor |> Maybe.map (\cursor -> button [class "btn btn-sm",onClick (SessionsTaskNextPage cursor)] [text "Older task sessions"]) |> Maybe.withDefault (text "")]]
                _ -> if entry.error /= Nothing then p [attribute "role" "alert"] [text "Task sessions could not be loaded.",button [onClick SessionsRetry] [text "Retry"]] else p [] [text "Loading task sessions…"]
            Nothing -> p [] [text "Loading task sessions…"]]


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
            ,button [class "btn btn-sm",disabled ((List.head state.rows |> Maybe.map .offset |> Maybe.withDefault (Maybe.withDefault 0 state.start)) <= 0),onClick SessionsOlder] [text "Load older window"]]
        ,p [class "card-meta",attribute "aria-live" "polite"]
            [text ((if state.following then "Following" else "Paused") ++ " · bytes " ++ String.fromInt (Maybe.withDefault 0 state.start) ++ "–" ++ String.fromInt state.offset
                ++ (if not state.following && newest > state.offset then " · " ++ String.fromInt (newest-state.offset) ++ " new bytes" else ""))]
        ,if state.notice /= "" then p [class "session-notice"] [text state.notice] else text ""
        ,if state.error /= Nothing || rangeError /= Nothing then p [attribute "role" "alert"] [text (Maybe.withDefault "Transcript request failed." state.error),button [onClick SessionsRetry] [text "Retry"]] else text ""
        ,Keyed.node "div" [Html.Attributes.id "session-records",class "session-records",tabindex 0,attribute "aria-label" "Bounded transcript records"]
            (List.map (\record -> (String.fromInt record.offset,
                article [class "session-record",attribute "data-offset" (String.fromInt record.offset)]
                    [div [class "session-record-meta"] [code [] [text (String.fromInt record.offset ++ "–" ++ String.fromInt record.end)],text (" " ++ record.recordType ++ " " ++ record.role ++ " " ++ record.time)]
                    ,pre [class "session-record-text"] [text record.content]
                    ,details [] [summary [] [text "Original JSON"],pre [class "session-record-raw"] [text (record.raw ++ record.delimiter)]] ])) state.rows)
        ,if List.isEmpty state.rows then p [attribute "role" "status"] [text (if state.start == Nothing then "Loading transcript metadata…" else if state.offset < state.end then "Reading bounded record window…" else if newest == 0 then "This session has no committed records." else "No complete records in this window. Load an older window to inspect its leading record.")] else text ""
        ,p [class "card-meta"] [text "At most 100 records and 2 MiB are shown. Original bytes remain available through bounded retrieval."] ]
