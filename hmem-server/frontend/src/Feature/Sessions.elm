module Feature.Sessions exposing (reconcile, update, view, viewTaskPanel, viewTaskActive, viewTaskActiveChoices, viewTaskActiveConversation, viewTaskEvidence, viewTaskIcon, viewWorkspaceInspector, taskActive, timestampSources, olderWindow, viewerReset)

import Api
import ActorAttribution
import Browser.Navigation as Nav
import SessionActivityLight
import SessionTranscript as Transcript
import Dict
import Feature.SessionDelivery as Delivery
import Feature.WebSocket as WebSocket
import Feature.DataLoading as DataLoading
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, tabindex, value)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Http
import Helpers
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
    if state.projectFilter /= Nothing then Delivery.ScopedSessions state.projectFilter state.taskFilter state.filters state.cursor
    else case state.taskFilter of
        Just task -> Delivery.TaskSessionsFiltered task state.filters state.cursor
        Nothing -> Delivery.WorkspaceSessionsFiltered state.filters state.cursor


facetTarget : String -> SessionsModel -> Delivery.Target
facetTarget kind state =
    if kind == "projects" then Delivery.SessionFacets kind Nothing state.projectFacetCursor state.projectFilter
    else Delivery.SessionFacets kind state.projectFilter state.taskFacetCursor state.taskFilter


filterSuffix : Maybe String -> Maybe String -> String
filterSuffix project task =
    (project |> Maybe.map (\id -> "&project=" ++ Url.percentEncode id) |> Maybe.withDefault "")
        ++ (task |> Maybe.map (\id -> "&task=" ++ Url.percentEncode id) |> Maybe.withDefault "")


filterUrl : Model -> Maybe String -> Maybe String -> String
filterUrl model project task =
    "/workspace/" ++ Maybe.withDefault "" model.selectedWorkspaceId ++ "#tab=sessions" ++ filterSuffix project task


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
        panelCandidate = if old.taskPanel /= Nothing then old.taskPanel else old.returnTaskPanel
        taskPanel = if workspace /= Nothing && old.workspace == workspace && old.epoch == model.sessionRequestEpoch && model.activeTab == ProjectsTab then panelCandidate |> Maybe.andThen (\task -> if Dict.member task model.tasks && List.member task (mountedTasks model) then Just task else Nothing) else Nothing
        dockSessions = taskPanel |> Maybe.andThen (taskPage False (Maybe.withDefault "" workspace) model) |> Maybe.map (.items >> List.filter (\session -> session.state == Api.SessionActive)) |> Maybe.withDefault []
        currentDockSessions = List.filter (effectiveActive (Maybe.withDefault "" workspace) model) dockSessions
        freshDockPage = taskPanel |> Maybe.andThen (freshTaskPage False (Maybe.withDefault "" workspace) model)
        freshDockSessions = freshDockPage |> Maybe.map (.items >> List.filter (effectiveActive (Maybe.withDefault "" workspace) model)) |> Maybe.withDefault []
        selected = if workspace /= Nothing && model.activeTab == SessionsTab then selectedFromUrl model else if taskPanel /= Nothing then
            if freshDockPage /= Nothing then (List.filter (\session -> Just session.id == old.selected) freshDockSessions ++ freshDockSessions) |> List.head |> Maybe.map .id
            else List.filter (\session -> Just session.id == old.selected) currentDockSessions |> List.head |> Maybe.map .id
            else Nothing
        stamp = Encode.encode 0 (Encode.list Encode.string [Maybe.withDefault "" workspace, String.fromInt model.sessionRequestEpoch, model.sessionContext |> Maybe.map (.principal >> .actorId) |> Maybe.withDefault "", Maybe.withDefault "" selected, Maybe.withDefault "" taskPanel])
        viewerScope = Encode.encode 0 (Encode.list Encode.string [Maybe.withDefault "" workspace, String.fromInt model.sessionRequestEpoch, model.sessionContext |> Maybe.map (.principal >> .actorId) |> Maybe.withDefault "", Maybe.withDefault "" selected, String.fromInt model.sessionDelivery.proofRetirement])
        continueViewer = old.viewerScope == viewerScope && selected /= Nothing && old.start /= Nothing && not old.resetPending && not old.pending && ((old.taskPanel /= Nothing && model.activeTab == SessionsTab) || (old.taskPanel == Nothing && taskPanel /= Nothing && old.returnTaskPanel == taskPanel))
        returnTaskPanel = if old.workspace /= workspace || old.epoch /= model.sessionRequestEpoch || old.selected /= selected then Nothing else if model.activeTab == SessionsTab && old.taskPanel /= Nothing then old.taskPanel else if model.activeTab == SessionsTab then old.returnTaskPanel else Nothing
        downloadStamp = Encode.encode 0 (Encode.list Encode.string [Maybe.withDefault "" workspace, String.fromInt model.sessionRequestEpoch, model.sessionContext |> Maybe.map (.principal >> .actorId) |> Maybe.withDefault "", String.fromInt model.sessionDelivery.retirement, if model.activeTab == SessionsTab then Maybe.withDefault "" selected else ""])
        downloadChanged = old.downloadStamp /= downloadStamp
        taskFilter = if workspace /= Nothing && model.activeTab == SessionsTab then urlId "task" model else Nothing
        projectFilter = if workspace /= Nothing && model.activeTab == SessionsTab then urlId "project" model else Nothing
        taskIds = if workspace /= Nothing && model.activeTab == ProjectsTab then mountedTasks model else []
        reset = old.workspace /= workspace || old.epoch /= model.sessionRequestEpoch
        scoped = if reset then { old | workspace = workspace, epoch = model.sessionRequestEpoch, selected = selected, cursor = Nothing, previous = [], linksCursor = Nothing, showAllLinkedTasks = False, filters = Api.defaultSessionFilters, taskFilter = taskFilter, projectFilter = projectFilter, projectFacetCursor = Nothing, taskFacetCursor = Nothing, activityDirty = False, taskCursors = Dict.empty, taskPreviousPages = Dict.empty, taskPages = Dict.empty, taskHints = Dict.empty, hintOrder = [], sectionOrder = [] }
            else if old.selected /= selected then { old | selected = selected, linksCursor = Nothing, showAllLinkedTasks = False } else old
        filtered = if scoped.taskFilter /= taskFilter || scoped.projectFilter /= projectFilter then { scoped | taskFilter = taskFilter, projectFilter = projectFilter, taskFacetCursor = Nothing, cursor = Nothing, previous = [] } else scoped
        hintEpoch = model.sessionDelivery.listInvalidation
        retainedHints = Dict.filter (\id _ -> List.member id taskIds) filtered.taskHints
        consumeHint (ws,target) accumulated = case target of
            Delivery.TaskSessionsFiltered task filters cursor ->
                if cursor /= Nothing || filters.state /= "active" || not (List.member task taskIds) then accumulated else
                case resource ws target model of
                    Just entry ->
                        if entry.error /= Nothing then Dict.insert task {active=False,error=True,epoch=hintEpoch} accumulated else if not entry.proven then accumulated else
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
        expandedTasks = List.filter (\id -> Dict.get id model.cards.expandedCards == Just True) taskIds
        sectionTargets = ((taskPanel |> Maybe.map (sectionTarget False model >> List.singleton) |> Maybe.withDefault []) ++ List.map (sectionTarget True model) expandedTasks) |> List.take 64
        sectionIdentity target = case target of
            Delivery.TaskSessionsFiltered task filters _ -> sectionKey (filters.state == "terminated") task
            _ -> ""
        sectionCursor target = case target of
            Delivery.TaskSessionsFiltered _ _ cursor -> cursor
            _ -> Nothing
        -- Keep only the already-read dock page as return context while its
        -- session is open full size. Freshness/authority checks still decide
        -- whether it can select a session when the card mounts again.
        retainedPages = Dict.filter (\key _ -> List.any (sectionIdentity >> (==) key) sectionTargets || (model.activeTab == SessionsTab && Just key == Maybe.map (sectionKey False) returnTaskPanel)) filtered.taskPages
        consumeSection (ws,target) accumulatedPages =
            if not (List.member target sectionTargets) then accumulatedPages else
            case resource ws target model of
                Just entry ->
                    if entry.error /= Nothing then Dict.insert (sectionIdentity target) {result=Err "Sessions could not be loaded.",epoch=hintEpoch,cursor=sectionCursor target} accumulatedPages
                    else if not entry.proven then accumulatedPages else
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
        withPanel = { filtered | taskPanel = taskPanel, returnTaskPanel = returnTaskPanel, viewerScope = viewerScope, downloadStamp = downloadStamp, taskHints = hints, hintOrder = nextRotation, taskPages = pages, sectionOrder = List.map sectionIdentity (List.filter (\target -> not (List.member target sectionReads)) orderedSections ++ sectionReads) }
        transportChanged = old.deliveryRetirement /= model.sessionDelivery.proofRetirement
        withDownloads = { withPanel | deliveryRetirement = model.sessionDelivery.proofRetirement
            , save = if downloadChanged then Nothing else old.save
            , archive = if downloadChanged then Nothing else old.archive
            , saveMessage = if downloadChanged && (old.save /= Nothing || old.archive /= Nothing) then "Save cancelled because its authority changed." else old.saveMessage }
        retired =
            if old.stamp /= stamp then
                if continueViewer then
                    { withDownloads | stamp = stamp, generation = old.generation + 1, resetPending = False, pending = False }
                else
                    { withDownloads | stamp = stamp, inspectedEvent = Nothing, generation = old.generation + 1, resetPending = True, start = Nothing, offset = 0, end = 0, following = True, loadWindow = True, pending = False, rows = [], notice = "", error = Nothing, heartbeats = Dict.empty }
            else if transportChanged then
                -- Retire response authority, not the presentation. Preserve the
                -- finite end so a framed but unacknowledged short tail retries
                -- exactly before newly committed bytes extend the next window.
                { withDownloads | generation = old.generation + 1, pending = False }
            else
                withDownloads
        metadata = Maybe.map2 (\ws id -> resource ws (Delivery.SessionMetadata id) model |> Maybe.andThen (\entry -> if entry.error == Nothing && entry.proven then entry.data else Nothing) |> Maybe.andThen (\value -> case value of
            Delivery.Metadata session -> Just session
            _ -> Nothing)) workspace selected |> Maybe.withDefault Nothing
        beforeArchive = case metadata of
            Just session ->
                if retired.start == Nothing then
                    let begin = max 0 (session.committedBytes - 2097152) in
                    { retired | start = Just begin, offset = begin, end = session.committedBytes, generation = retired.generation + 1 }
                else if retired.following && retired.offset >= retired.end && not retired.pending then
                    { retired | end = min session.committedBytes (retired.offset + 2097152) }
                else retired
            Nothing -> retired
        current = beforeArchive
        window = Maybe.map2 (\ws session -> (ws,Delivery.SessionWindow session.id current.offset current.end (session.state == Api.SessionTerminated))) workspace metadata
            |> Maybe.andThen (\target -> if current.loadWindow && current.error == Nothing && current.offset < current.end then Just target else Nothing)
        saveTarget = Maybe.map2 (\ws operation -> (ws,Delivery.SessionSave operation.session operation.offset (min 65536 (operation.size-operation.offset)))) workspace current.save
            |> Maybe.andThen (\target -> current.save |> Maybe.andThen (\operation -> if operation.phase == "writing" && not operation.pending && operation.offset < operation.size then Just target else Nothing))
        archiveTarget = Maybe.map2 (\ws archive -> (ws,Delivery.TaskEvidenceArchive archive.task archive.operation)) workspace current.archive |> Maybe.andThen (\target -> current.archive |> Maybe.andThen (\archive -> if archive.phase == "waiting" then Just target else Nothing))
        desired = workspace |> Maybe.map (\ws ->
            (if model.activeTab == SessionsTab then [(ws,listTarget current),(ws,facetTarget "projects" current),(ws,facetTarget "tasks" current)] else []) ++
            (selected |> Maybe.map (\id -> [(ws,Delivery.SessionMetadata id)] ++ (if model.activeTab == SessionsTab then [(ws,Delivery.SessionTasks id current.linksCursor)] else [])) |> Maybe.withDefault []) ++
            (archiveTarget |> Maybe.map List.singleton |> Maybe.withDefault []) ++
            (saveTarget |> Maybe.map List.singleton |> Maybe.withDefault []) ++
            (List.map (\target -> (ws,target)) (List.take (if current.archive /= Nothing || current.save /= Nothing then 10 else 12) sectionReads)) ++
            (List.map (\task -> (ws,Delivery.TaskSessionsFiltered task {state="active",freshness=""} Nothing)) hintTasks) ++
            (window |> Maybe.map List.singleton |> Maybe.withDefault [])) |> Maybe.withDefault []
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
        pending = { current | owned = desired, pending = current.pending || chunk /= Nothing, bridgedGeneration = current.generation, resetPending = current.resetPending && current.start == Nothing
            , archive = if saveFailure /= Nothing then Nothing else current.archive
            , save = if saveFailure /= Nothing then Nothing else Maybe.map (\operation -> { operation | pending = operation.pending || savePayload /= Nothing }) current.save
            , saveMessage = if saveFailure /= Nothing then "Download could not finish. Please retry." else current.saveMessage }
        ( next, requests ) = WebSocket.pumpSessionResources { model | sessions = pending, sessionDelivery = afterSave }
        heartbeatSources = desired |> List.concatMap (\(ws,target) -> Dict.get (Delivery.key ws target) afterSave.resources |> Maybe.andThen .data |> Maybe.map (\payload -> case payload of
            Delivery.SessionsPage page -> page.items
            Delivery.Metadata session -> [session]
            _ -> []) |> Maybe.withDefault []) |> List.take 52
        bridge = Ports.sessionBridgeCommand (Encode.object
            [ ("stamp",Encode.string stamp)
            , ("continueViewer",Encode.bool (continueViewer && old.stamp /= stamp))
            , ("viewerScope",Encode.string viewerScope)
            , ("downloadStamp",Encode.string downloadStamp)
            , ("generation",Encode.int current.generation)
            , ("reset",viewerReset current |> Maybe.map Encode.int |> Maybe.withDefault Encode.null)
            , ("follow",Encode.bool current.following)
            , ("chunk",chunk |> Maybe.map (\range -> Encode.object [("offset",Encode.int range.offset),("nextOffset",Encode.int range.nextOffset),("dataBase64",Encode.string range.dataBase64),("final",Encode.bool (range.nextOffset >= current.end))]) |> Maybe.withDefault Encode.null)
            , ("nextOperation",Encode.int current.nextOperation)
            , ("saveOperation",(pending.save |> Maybe.map .operation |> Maybe.withDefault (pending.archive |> Maybe.map .operation |> Maybe.withDefault -1)) |> Encode.int)
            , ("archiveRequest",archiveRequest next)
            , ("saveChunk",if saveFailure == Nothing then Maybe.withDefault Encode.null savePayload else Encode.null)
            , ("heartbeats",Encode.list (\session -> Encode.object [("id",Encode.string session.id),("lastSeenAt",Encode.string session.lastSeenAt)]) heartbeatSources)
            ])
        invalidTask = workspace |> Maybe.andThen (\ws -> resource ws (facetTarget "tasks" current) model)
            |> Maybe.map (\entry -> entry.error == Nothing && entry.dirty == entry.settled && current.taskFilter /= Nothing && (case entry.data of
                Just (Delivery.FacetsPage page) -> page.selected == Nothing
                _ -> False)) |> Maybe.withDefault False
        invalidProject = workspace |> Maybe.andThen (\ws -> resource ws (facetTarget "projects" current) model)
            |> Maybe.map (\entry -> entry.error == Nothing && entry.dirty == entry.settled && current.projectFilter /= Nothing && (case entry.data of
                Just (Delivery.FacetsPage page) -> page.selected == Nothing
                _ -> False)) |> Maybe.withDefault False
        retireInvalidTask = if invalidProject then (model.key |> Maybe.map (\key -> Nav.replaceUrl key (filterUrl model Nothing Nothing)) |> Maybe.withDefault Cmd.none) else if invalidTask then (model.key |> Maybe.map (\key -> Nav.replaceUrl key (filterUrl model current.projectFilter Nothing)) |> Maybe.withDefault Cmd.none) else Cmd.none
    in
    ( next, Cmd.batch [ command, requests, bridge, retireInvalidTask ] )


viewerReset : SessionsModel -> Maybe Int
viewerReset state =
    if state.resetPending then state.start else Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let state = model.sessions in
    case msg of
        SessionsInspect stamp key ->
            if stamp /= state.stamp || state.workspace == Nothing || model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) || not (List.any (Tuple.first >> (==) key) (inspectableEvents state.rows)) then (model,Cmd.none)
            else ({model | sessions = {state | inspectedEvent = Just key}},Cmd.none)
        SessionsToggleDock task ->
            if model.activeTab /= ProjectsTab || not (Dict.member task model.tasks) || not (List.member task (mountedTasks model)) || (state.taskPanel /= Just task && not (taskActive (Maybe.withDefault "" state.workspace) model task)) then (model,Cmd.none)
            else ({model | sessions = {state | taskPanel = if state.taskPanel == Just task then Nothing else Just task, returnTaskPanel = Nothing, selected = Nothing, following = True}},Cmd.none)
        SessionsSelectDock task id ->
            if state.taskPanel /= Just task || not (freshTaskPage False (Maybe.withDefault "" state.workspace) model task |> Maybe.map (.items >> List.any (\session -> session.id == id && effectiveActive (Maybe.withDefault "" state.workspace) model session)) |> Maybe.withDefault False) then (model,Cmd.none)
            else ({model | sessions = {state | selected = Just id, following = True}},Cmd.none)
        SessionsNextPage cursor ->
            ( { model | sessions = { state | cursor = Just cursor, previous = List.take 16 (state.cursor :: state.previous) } }, Cmd.none )
        SessionsPreviousPage ->
            case state.previous of
                cursor :: rest -> ( { model | sessions = { state | cursor = cursor, previous = rest } }, Cmd.none )
                [] -> ( { model | sessions = { state | cursor = Nothing } }, Cmd.none )
        SessionsLinkedPage cursor -> ( { model | sessions = { state | linksCursor = cursor } }, Cmd.none )
        SessionsShowAllLinkedTasks -> ( { model | sessions = { state | showAllLinkedTasks = True } }, Cmd.none )
        SessionsRetry ->
            let
                decodeRecovery = state.error /= Nothing
                begin = max 0 (state.end - 2097152)
                refresh ws delivery =
                    Delivery.invalidateFacets ws (Delivery.invalidateLists ws delivery)
                        |> (\current -> state.selected |> Maybe.map (\id -> Delivery.invalidateSession ws id current) |> Maybe.withDefault current)
                        |> (\current -> state.taskPanel |> Maybe.map (\id -> Delivery.invalidateTask ws id current) |> Maybe.withDefault current)
                        |> (\current -> List.foldl (\(ownedWorkspace,target) accumulated -> case target of
                            Delivery.SessionWindow _ _ _ _ -> Delivery.removeDemand ownedWorkspace target accumulated
                            _ -> accumulated) current state.owned)
            in
            ( {model | sessions = {state | error = Nothing,pending = False,loadWindow = True
                    , start = if decodeRecovery then Just begin else state.start
                    , offset = if decodeRecovery then begin else state.offset
                    , generation = state.generation + 1,resetPending = decodeRecovery || state.resetPending}
                ,sessionDelivery = state.workspace |> Maybe.map (\ws -> refresh ws model.sessionDelivery) |> Maybe.withDefault model.sessionDelivery},Cmd.none )
        SessionsStateFilter value ->
            if value == "active" then
                let filters = state.filters in ( { model | sessions = { state | filters = { filters | state = "active" }, cursor = Nothing, previous = [] } }, Cmd.none )
            else (model,Cmd.none)
        SessionsFreshnessFilter value ->
            if List.member value ["fresh","stale"] then
                let filters = state.filters in ( { model | sessions = { state | filters = { filters | freshness = value }, cursor = Nothing, previous = [] } }, Cmd.none )
            else (model,Cmd.none)
        SessionsProjectFilter value ->
            (model, Helpers.pushUrl model.key (filterUrl model (if value == "" then Nothing else Just value) Nothing))
        SessionsTaskFilter value ->
            (model, Helpers.pushUrl model.key (filterUrl model state.projectFilter (if value == "" then Nothing else Just value)))
        SessionsFacetPage kind cursor ->
            ({model | sessions = if kind == "projects" then {state | projectFacetCursor = cursor} else {state | taskFacetCursor = cursor}}, Cmd.none)
        SessionsFollow following ->
            ( { model | sessions = { state | following = following, loadWindow = following, pending = False, generation = state.generation + 1, resetPending = following, start = if following then Nothing else state.start } }, Cmd.none )
        SessionsOlder ->
            let (begin,end) = olderWindow state in
            ( { model | sessions = { state | following = False, loadWindow = True, start = Just begin, offset = begin, end = end, pending = False, generation = state.generation + 1, resetPending = True, rows = [], error = Nothing } }, Cmd.none )
        SessionsTick time ->
            let now = toFloat (Time.posixToMillis time)
                refresh = state.filters.freshness /= "" && model.activeTab == SessionsTab && now-state.lastFilterRefresh >= 10000
                refreshActivity = state.activityDirty && model.search.filterHasActiveSession && model.activeTab == ProjectsTab && now-state.lastActivityRefresh >= 1000
                updated = {model | sessions = {state | now = now, lastFilterRefresh = if refresh then now else state.lastFilterRefresh, activityDirty = if refreshActivity || not model.search.filterHasActiveSession then False else state.activityDirty, lastActivityRefresh = if refreshActivity then now else state.lastActivityRefresh}
                    , sessionDelivery = if refresh then state.workspace |> Maybe.map (\ws -> Delivery.invalidateLists ws model.sessionDelivery) |> Maybe.withDefault model.sessionDelivery else model.sessionDelivery}
            in
            if refreshActivity then DataLoading.revalidateNavigationForFilters updated else (updated, Cmd.none)
        SessionsBridgeEvent value -> receiveBridge value model
        SessionsCancelSave -> ( {model | sessions = {state | save = Nothing, archive = Nothing, saveMessage = "Save cancellation requested."}}, Cmd.none )
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
        row = Decode.map8 (\offset end raw delimiter recordType role time content -> {offset=offset,end=end,raw=raw,delimiter=delimiter,recordType=recordType,role=role,time=time,content=content,presentationOffset=offset,activityAnchors=Dict.empty}) (Decode.field "offset" Decode.int) (Decode.field "end" Decode.int) (Decode.field "raw" Decode.string) (Decode.field "delimiter" Decode.string) (Decode.field "type" Decode.string) (Decode.field "role" Decode.string) (Decode.field "time" Decode.string) (Decode.field "text" Decode.string)
            |> Decode.andThen (\record -> Decode.map (\anchor -> {record | presentationOffset=anchor}) (Decode.oneOf [Decode.field "presentationOffset" Decode.int,Decode.succeed record.offset]))
            |> Decode.andThen (\record -> Decode.map (\anchors -> {record | activityAnchors=anchors}) (Decode.oneOf [Decode.field "activityAnchors" (Decode.dict Decode.string),Decode.succeed Dict.empty]))
    in
    if kind == "archive-http" then receiveArchiveHttp value model
    else if kind == "heartbeat-status" && stamp == state.stamp then
        case Decode.decodeValue (Decode.field "statuses" (Decode.dict Decode.string)) value of
            Ok statuses -> if Dict.size statuses <= 52 then
                let crossed = Dict.toList statuses |> List.any (\(id,status) -> Dict.get id state.heartbeats |> Maybe.map ((/=) status) |> Maybe.withDefault False)
                in ( {model | sessions = {state | heartbeats = statuses, today = field "today" Decode.string state.today}
                    , sessionDelivery = if crossed && model.activeTab == SessionsTab && state.filters.freshness /= "" then state.workspace |> Maybe.map (\ws -> Delivery.invalidateLists ws model.sessionDelivery) |> Maybe.withDefault model.sessionDelivery else model.sessionDelivery}, Cmd.none )
                else (model,Cmd.none)
            Err _ -> (model,Cmd.none)
    else if kind == "save-capability" then
        ( {model | sessions = {state | saveCapability = Just (field "supported" Decode.bool False)}}, Cmd.none )
    else if String.startsWith "archive-" kind then receiveArchive value model
    else if String.startsWith "save-" kind then receiveSave value model
    else if stamp /= state.stamp || generation /= state.generation || not state.pending then (model,Cmd.none)
    else if field "kind" Decode.string "" == "viewer-error" then
        ( {model | sessions = {state | pending = False, error = Just "Record window could not be decoded. Retry or load another window."}}, Cmd.none )
    else if through <= state.offset || through > min state.end (state.offset + 65536) then (model,Cmd.none)
    else if kind == "viewer-progress" then
        ( {model | sessions = {state | offset = through,pending = False}}, Cmd.none )
    else case Decode.decodeValue (Decode.field "rows" (Decode.list row)) value of
        Ok rows ->
            if List.length rows <= 200 && List.sum (List.map (.raw >> String.length) rows) <= 4194304 then
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
    if stamp /= state.downloadStamp || model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) then (model,Cmd.none)
    else if kind == "save-error" && field "code" Decode.string "" == "writer_cleanup_failed" && operation > 0 && operation < state.nextOperation then
        ({model | sessions={state | archive=Nothing,save=Nothing,saveBlocked=True,saveMessage="Writer cleanup could not be confirmed. Reload this page before another download."}},Cmd.none)
    else if kind == "save-picking" && operation == state.nextOperation && state.save == Nothing && state.archive == Nothing then
        case metadata of
            Just session ->
                case (session.finalBytes,session.finalSha256) of
                    (Just size,Just hash) ->
                        if session.state == Api.SessionTerminated then
                            ( {model | sessions = {state | save = Just (SessionSaveOperation operation session.id size hash session.revision 0 "picking" False), archiveMessageTask = Nothing, nextOperation = operation + 1, saveMessage = "Preparing download…"}}, Cmd.none )
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
                        ( {model | sessions = {state | save = Nothing, archive = Nothing, saveCapability = Just False, saveBlocked = True, saveMessage = "Writer cleanup could not be confirmed. Reload this page before another save."}}, Cmd.none )
                    else ( {model | sessions = {state | save = Nothing, archive = Nothing, saveMessage = if field "code" Decode.string "" == "download_storage_unavailable" then "This browser cannot download sessions larger than 32 MiB without temporary file storage." else "Download could not finish. Please retry."}}, Cmd.none )
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
        [ div [ class "sessions-toolbar" ] [ h2 [] [ text "Sessions" ] ]
        , model.sessions.taskFilter |> Maybe.map (\task -> div [class "sessions-task-filter"] [text ("Sessions linked to " ++ (Dict.get task model.tasks |> Maybe.map .title |> Maybe.withDefault (String.left 8 task))),a [href ("/workspace/" ++ Url.percentEncode workspace ++ "#tab=sessions"),class "btn btn-sm"] [text "Clear"]]) |> Maybe.withDefault (text "")
        , div [class "sessions-filters",attribute "role" "group",attribute "aria-label" "Activity"]
            (List.map (\(filter,caption) -> button [class ("btn session-filter" ++ (if model.sessions.filters.freshness == filter then " selected" else "")),attribute "aria-pressed" (if model.sessions.filters.freshness == filter then "true" else "false"),onClick (SessionsFreshnessFilter filter)] [text caption]) [("fresh","Live"),("stale","Quiet")]
                ++ [viewFacet workspace model "projects",viewFacet workspace model "tasks"])
        , div [ class "sessions-layout" ]
            [ aside [ class "sessions-list", attribute "aria-label" "Session hierarchy" ]
                [ h3 [class "session-pane-label"] [text "Session hierarchy"]
                , case list |> Maybe.andThen .error of
                    Just _ -> div [ attribute "role" "alert" ] [ text "Sessions could not be loaded. ", button [ onClick SessionsRetry ] [text "Retry"] ]
                    Nothing -> case list |> Maybe.andThen .data of
                        Just (Delivery.SessionsPage page) -> div []
                            [ if List.isEmpty page.items then p [ class "empty-state" ] [text "No sessions match these filters."]
                              else viewSessionTree workspace model (List.filter (\session -> session.state == Api.SessionActive) page.items)
                            , div [class "sessions-pagination"]
                                [ button [class "btn btn-sm", disabled (model.sessions.cursor == Nothing), onClick SessionsPreviousPage] [text "Newer"]
                                , page.nextCursor |> Maybe.map (\cursor -> button [class "btn btn-sm",onClick (SessionsNextPage cursor)] [text "Older sessions"]) |> Maybe.withDefault (text "") ] ]
                        _ -> p [ attribute "role" "status" ] [text "Loading sessions…"] ]
            , model.sessions.selected |> Maybe.map (viewDetail workspace model) |> Maybe.withDefault (div [class "empty-state session-detail"] [h3 [] [text "Inspect an agent session"],p [] [text "Select a session to see its activity and linked tasks."]]) ] ]


viewFacet : String -> Model -> String -> Html Msg
viewFacet workspace model kind =
    let state = model.sessions
        caption = if kind == "projects" then "Project" else "Task"
        selected = if kind == "projects" then state.projectFilter else state.taskFilter
        currentCursor = if kind == "projects" then state.projectFacetCursor else state.taskFacetCursor
        entry = resource workspace (facetTarget kind state) model
        page = entry |> Maybe.andThen .data |> Maybe.andThen (\payload -> case payload of
            Delivery.FacetsPage choices -> Just choices
            _ -> Nothing)
        options = page |> Maybe.map (\items -> items.items ++ (items.selected |> Maybe.andThen (\item -> if List.any (.id >> (==) item.id) items.items then Nothing else Just item) |> Maybe.map List.singleton |> Maybe.withDefault [])) |> Maybe.withDefault []
    in
    div [class "session-facet"]
        [ label [] [span [class "filter-label"] [text caption],select [class "filter-select",attribute "aria-label" (caption ++ " sessions"),value (Maybe.withDefault "" selected),onInput (if kind == "projects" then SessionsProjectFilter else SessionsTaskFilter)]
            (option [value ""] [text ("All " ++ kind)] :: List.map (\item -> option [value item.id,Html.Attributes.selected (selected == Just item.id)] [text item.title]) options)]
        , if entry |> Maybe.andThen .error |> (/=) Nothing then button [class "btn btn-sm",onClick SessionsRetry] [text ("Retry " ++ kind)] else text ""
        , page |> Maybe.andThen .nextId |> Maybe.map (\cursor -> button [class "btn btn-sm",onClick (SessionsFacetPage kind (Just cursor))] [text ("More " ++ kind)]) |> Maybe.withDefault (text "")
        , if currentCursor /= Nothing then button [class "btn btn-sm",onClick (SessionsFacetPage kind Nothing)] [text ("First " ++ kind)] else text "" ]


viewSessionTree : String -> Model -> List Api.AgentSession -> Html Msg
viewSessionTree workspace model sessions =
    sessionTree (sessionSummary workspace model) sessions


sessionTree : (Api.AgentSession -> Html Msg) -> List Api.AgentSession -> Html Msg
sessionTree card sessions =
    let ids = List.map .id sessions
        render visited session =
            li [class "session-tree-node"]
                [ card session
                , let children = List.filter (\child -> child.parentSessionId == Just session.id && not (List.member child.id visited)) sessions in
                  if List.isEmpty children then text "" else ul [class ("session-tree-children" ++ (if List.length visited >= 2 then " session-tree-flat" else ""))] (List.map (render (session.id :: visited)) children) ]
        roots = List.filter (\session -> session.parentSessionId |> Maybe.map (\parent -> not (List.member parent ids)) |> Maybe.withDefault True) sessions
    in
    ul [class "session-summaries"] (List.map (render []) roots)


sessionTitle : Api.AgentSession -> String
sessionTitle session = Maybe.withDefault session.formatLabel session.title


sessionSummary : String -> Model -> Api.AgentSession -> Html Msg
sessionSummary workspace model session =
    compactSessionCard model session "session-summary" (a [href (sessionUrl workspace session.id ++ filterSuffix model.sessions.projectFilter model.sessions.taskFilter),class "session-summary-link"]) []


compactSessionCard : Model -> Api.AgentSession -> String -> (List (Html Msg) -> Html Msg) -> List (Html Msg) -> Html Msg
compactSessionCard model session extra link actions =
    let
        path = (if session.ancestorsTruncated then "Earlier ancestors omitted (16-level context limit) › " else "") ++ "Parent: " ++ String.join " › " (List.map (\ancestor -> Maybe.withDefault ancestor.formatLabel ancestor.title) session.ancestors)
        active = session.state == Api.SessionActive
    in
    div [class ("card clickable session-card " ++ extra ++ (if model.sessions.selected == Just session.id then " selected" else "") ++ (if active then " is-active" else ""))]
        (link
            [span [class "session-summary-heading"]
                [span [class "session-state-dot",attribute "aria-label" (if active then "Active" else "Terminated")] [],strong [Html.Attributes.title (sessionTitle session)] [text (sessionTitle session)]]
            ,if List.isEmpty session.ancestors then text "" else span [class "session-parent-context",Html.Attributes.title path] [text path]
            ,viewSessionIdentity session
            ,span [class "session-latest-activity"]
                [text "Last activity · ",time [attribute "datetime" (sessionActivity session)] [text (compactTimestamp model (sessionActivity session))]]
            ] :: actions)


viewSessionIdentity : Api.AgentSession -> Html Msg
viewSessionIdentity session =
    let
        runtime = session.runtimeIdentity |> Maybe.map (\identity -> String.join " · " (List.filterMap identityValue [Just identity.model,identity.provider,identity.reasoning])) |> Maybe.withDefault "Unknown"
        label = if session.state == Api.SessionActive then "Running agent" else "Session agent"
        role = session.runtimeIdentity |> Maybe.andThen .role |> identityValue |> Maybe.withDefault "Unknown role"
    in
    span [class "session-identity"]
        [span [class "session-runtime actor-attribution-kind",attribute "aria-label" (label ++ ": " ++ role ++ ". " ++ runtime),Html.Attributes.title (label ++ ": " ++ role ++ " · " ++ runtime)]
            [span [class "session-runtime-value"] [text role]]]


viewDetail : String -> Model -> String -> Html Msg
viewDetail workspace model id =
    div [ class "session-detail" ]
        [ case resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .data of
            Just (Delivery.Metadata session) -> div [class "session-detail-header"]
                [ div [class "session-detail-title"] [h3 [] [text (sessionTitle session)],span [class "card-meta session-lifecycle"] [text (if session.state == Api.SessionTerminated then "Saved session" else "Active session")],span [class "card-meta session-freshness"] [text (case Dict.get session.id model.sessions.heartbeats of
                    Just "Fresh" -> " · Live"
                    Just "Stale (30s)" -> " · Quiet"
                    _ -> "")]]
                ,viewSave model session
                ,a [class "btn btn-sm",href (filterUrl model model.sessions.projectFilter model.sessions.taskFilter),attribute "aria-label" "Close session"] [text "✕"]]
            _ -> resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .error |> Maybe.map (\_ -> p [attribute "role" "alert"] [text "Session could not be loaded.",button [onClick SessionsRetry] [text "Retry"]]) |> Maybe.withDefault (p [attribute "role" "status"] [text "Loading session…"])
        , resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .data |> Maybe.map (\payload -> case payload of
            Delivery.Metadata session -> viewAncestorPath session
            _ -> text "") |> Maybe.withDefault (text "")
        , div [class "session-conversation-grid"]
            [ div [class "session-conversation"]
              [ section [class "session-linked-tasks",attribute "aria-label" "Linked tasks"]
            [ case resource workspace (Delivery.SessionTasks id model.sessions.linksCursor) model |> Maybe.andThen .data of
                Just (Delivery.TasksPage page) -> div []
                    [div [class "session-task-cards"] (List.map (\task -> a [class "session-task-card",href ("/workspace/" ++ Url.percentEncode workspace ++ "#tab=projects&focus=" ++ Url.percentEncode ("task:" ++ task.id)),Html.Attributes.title task.title] [strong [] [text task.title],span [class "badge"] [text task.status]]) (if model.sessions.showAllLinkedTasks then page.items else List.take 2 page.items))
                    ,if List.isEmpty page.items then p [class "card-meta"] [text "No linked tasks."] else text ""
                    ,if not model.sessions.showAllLinkedTasks && (List.length page.items > 2 || page.nextTaskId /= Nothing) then button [class "btn btn-sm session-show-linked",onClick SessionsShowAllLinkedTasks] [text "Show All Linked Tasks"] else text ""
                    ,if model.sessions.showAllLinkedTasks then div [class "sessions-pagination"] [page.nextTaskId |> Maybe.map (\cursor -> button [class "btn btn-sm",onClick (SessionsLinkedPage (Just cursor))] [text "More tasks"]) |> Maybe.withDefault (text ""),if model.sessions.linksCursor /= Nothing then button [class "btn btn-sm",onClick (SessionsLinkedPage Nothing)] [text "First tasks"] else text ""] else text ""]
                _ -> p [class "card-meta"] [text "Loading linked tasks…"] ]
              , viewRecords workspace model id ]
            , viewEventInspector workspace model id ]
        ,p [class "session-download-status",attribute "role" "status",attribute "aria-live" "polite"] [text (if model.sessions.archiveMessageTask == Nothing || model.sessions.saveBlocked then model.sessions.saveMessage else "")] ]


viewAncestorPath : Api.AgentSession -> Html Msg
viewAncestorPath session =
    section [class "session-ancestor-path",attribute "aria-label" "Selected session lineage"]
        [ span [class "session-pane-label"] [text "Session lineage"]
        , if session.ancestorsTruncated then p [class "card-meta"] [text "Earlier ancestors omitted (16-level context limit)."] else text ""
        , if List.isEmpty session.ancestors && session.parentSessionId /= Nothing then p [class "card-meta"] [text "Parent context unavailable or inaccessible."] else text ""
        , ol []
            (List.map (\ancestor -> li [] [text (Maybe.withDefault ancestor.formatLabel ancestor.title),span [class "card-meta"] [text (" · ancestry context" ++ (if ancestor.state == "terminated" then " · terminated" else ""))]]) session.ancestors
                ++ [li [attribute "aria-current" "step"] [strong [] [text (sessionTitle session)]]]) ]


inspectableEvents : List SessionRecord -> List (String,Transcript.Block)
inspectableEvents rows =
    Transcript.project rows
        |> List.concatMap (\item -> case item of
            Transcript.Message piece -> [piece]
            Transcript.Activity pieces -> pieces)
        |> List.concatMap (\piece -> List.indexedMap (\index block -> (piece.key ++ ":" ++ String.fromInt index,block)) piece.blocks)
        |> List.filter (\(_,block) -> List.member block.kind ["tool","result","thinking"])


viewEventInspector : String -> Model -> String -> Html Msg
viewEventInspector workspace model id =
    let
        selected = if resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .error |> (/=) Nothing then Nothing else model.sessions.inspectedEvent |> Maybe.andThen (\key -> List.filter (Tuple.first >> (==) key) (inspectableEvents model.sessions.rows) |> List.head)
        metadata = resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .data |> Maybe.andThen (\payload -> case payload of
            Delivery.Metadata session -> Just session
            _ -> Nothing)
    in
    aside [class "session-event-inspector",attribute "aria-label" "Event inspector"]
        [ div [class "session-inspector-heading"] [h4 [] [text "Event inspector"]]
        , Keyed.node "div" [class "session-inspected-event"]
            (selected |> Maybe.map (\(key,block) -> [(model.sessions.stamp ++ ":" ++ key,div []
                [h5 [] [text (if block.label == "" then "Activity details" else block.label)]
                ,p [class "card-meta"] [text (if block.kind == "tool" then "Tool input" else if block.kind == "result" then "Tool result" else "Thinking summary")]
                ,viewBlockContent block])]) |> Maybe.withDefault [("empty",p [class "empty-state"] [text (if model.sessions.inspectedEvent == Nothing then "Select Inspect on an activity event to read its details here." else "The selected event is outside this transcript window.")])])
        , metadata |> Maybe.map (\session -> details [class "session-inspector-identity"]
            [summary [] [text "Session identity"]
            ,dl []
                [dt [] [text "Runtime declaration"],dd [] [text (session.runtimeIdentity |> Maybe.map (\identity -> String.join " · " (List.filterMap identityValue [identity.role,Just identity.model,identity.provider,identity.reasoning])) |> Maybe.withDefault "Unknown")]
                ,dt [] [text "Created by"],dd [] [text (if session.creator == ActorAttribution.empty then "Unknown" else ActorAttribution.label session.creator)]
                ,dt [] [text "Lifecycle"],dd [] [text (if session.state == Api.SessionActive then "Active" else "Terminated")]
                ,dt [] [text "Last activity"],dd [] [time [attribute "datetime" (sessionActivity session)] [text (compactTimestamp model (sessionActivity session))]]]]) |> Maybe.withDefault (text "") ]


identityValue : Maybe String -> Maybe String
identityValue = Maybe.andThen (\value -> if String.trim value == "" then Nothing else Just value)


viewSave : Model -> Api.AgentSession -> Html Msg
viewSave model session =
    let
        state = model.sessions
        config = Encode.encode 0 (Encode.object [("stamp",Encode.string state.downloadStamp),("operation",Encode.int state.nextOperation),("session",encodeReceipt session)])
    in
    if session.state /= Api.SessionTerminated then text ""
    else button [class "btn btn-sm session-download",attribute "data-session-download" config,disabled (state.saveBlocked || state.save /= Nothing || state.archive /= Nothing || session.finalBytes == Nothing || session.finalSha256 == Nothing),attribute "aria-label" ("Download session " ++ String.left 8 session.id)] [text "↓ Download"]


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


freshTaskPage : Bool -> String -> Model -> String -> Maybe Api.SessionPage
freshTaskPage evidence workspace model task =
    case resource workspace (sectionTarget evidence model task) model of
        Just entry ->
            if entry.error /= Nothing || not entry.proven then Nothing else
            case entry.data of
                Just (Delivery.SessionsPage page) -> Just page
                _ -> Nothing
        Nothing -> Dict.get (sectionKey evidence task) model.sessions.taskPages |> Maybe.andThen (\cached ->
            if cached.epoch == model.sessionDelivery.listInvalidation && cached.cursor == taskCursor model (sectionKey evidence task) then Result.toMaybe cached.result else Nothing)


effectiveActive : String -> Model -> Api.AgentSession -> Bool
effectiveActive workspace model session =
    session.state == Api.SessionActive && (case resource workspace (Delivery.SessionMetadata session.id) model |> Maybe.andThen .data of
        Just (Delivery.Metadata currentMetadata) -> currentMetadata.state == Api.SessionActive
        _ -> True)


viewTaskIcon : String -> Model -> String -> Html Msg
viewTaskIcon workspace model task =
    let
        hint = Dict.get task model.sessions.taskHints
        count = hint |> Maybe.andThen (\value -> if value.error then Nothing else Just (if value.active then 1 else 0))
        label = (if count == Just 1 then "Active Agent →" else "Agent activity") ++ (case hint of
            Just current -> if current.error then " · activity unavailable" else if current.active then " · active sessions" ++ (if current.epoch /= model.sessionDelivery.listInvalidation then " · refreshing" else "") else if current.epoch /= model.sessionDelivery.listInvalidation then " · checking activity" else " · no active sessions"
            Nothing -> " · checking activity")
    in
    button [class ("btn btn-secondary btn-sm task-sessions-icon" ++ (if Maybe.withDefault 0 count > 0 then " has-active-sessions" else "")),disabled (count /= Just 1 && model.sessions.taskPanel /= Just task),attribute "aria-label" label,attribute "aria-expanded" (if model.sessions.taskPanel == Just task then "true" else "false"),Html.Attributes.title label,stopPropagationOn "click" (Decode.succeed (SessionsToggleDock task,True)),stopPropagationOn "dblclick" (Decode.succeed (NoOp,True))]
        [if count == Just 1 then SessionActivityLight.view True label else span [class "session-state-dot session-state-inactive",attribute "aria-hidden" "true"] [],if count == Just 1 then text "Active Agent →" else text ""]


viewTaskPanel : String -> Model -> String -> Html Msg
viewTaskPanel workspace model task = div [] [viewTaskActive workspace model task,viewTaskEvidence workspace model task]


viewTaskActive : String -> Model -> String -> Html Msg
viewTaskActive workspace model task = viewTaskActiveWithChoices True workspace model task


viewTaskActiveConversation : String -> Model -> String -> Html Msg
viewTaskActiveConversation workspace model task = viewTaskActiveWithChoices False workspace model task


viewTaskActiveChoices : String -> Model -> String -> Html Msg
viewTaskActiveChoices workspace model task =
    if model.sessions.taskPanel /= Just task then text "" else
    let
        page = taskPage False workspace model task
        fresh = freshTaskPage False workspace model task /= Nothing
        sessions = page |> Maybe.map (.items >> List.filter (effectiveActive workspace model)) |> Maybe.withDefault []
        error = resource workspace (sectionTarget False model task) model |> Maybe.andThen .error
        failed = error /= Nothing || (Dict.get (sectionKey False task) model.sessions.taskPages |> Maybe.map (\cached -> case cached.result of
            Err _ -> True
            Ok _ -> False) |> Maybe.withDefault False)
        key = sectionKey False task
    in
    section [class "task-session-work-context task-session-context-choices",attribute "aria-label" "Task active agent hierarchy",attribute "aria-busy" (if fresh then "false" else "true"),stopPropagationOn "click" (Decode.succeed (NoOp,True)),stopPropagationOn "dblclick" (Decode.succeed (NoOp,True))] [h4 [class "session-pane-label"] [text "Active agents"],div [class "task-session-picker"] [sessionTree (\session -> compactSessionCard model session "task-session-choice" (button [class "session-summary-link",disabled (not fresh),attribute "aria-pressed" (if model.sessions.selected == Just session.id then "true" else "false"),onClick (SessionsSelectDock task session.id)]) []) sessions]
        ,if page == Nothing || failed then p [attribute "role" (if failed then "alert" else "status")] [text (if failed then "Active sessions could not be loaded." else "Loading active sessions…"),if failed then button [class "btn btn-secondary btn-sm",onClick SessionsRetry] [text "Retry"] else text ""] else if List.isEmpty sessions then p [class "card-meta"] [text "No active sessions for this task."] else text ""
        ,div [class "sessions-pagination"] [if taskCursor model key /= Nothing then button [class "btn btn-sm",onClick (SessionsTaskPreviousPage key)] [text "Newer sessions"] else text "",page |> Maybe.andThen .nextCursor |> Maybe.map (\cursor -> button [class "btn btn-sm",onClick (SessionsTaskNextPage key cursor)] [text "Older sessions"]) |> Maybe.withDefault (text "")]
            ]


viewTaskActiveWithChoices : Bool -> String -> Model -> String -> Html Msg
viewTaskActiveWithChoices includeChoices workspace model task =
    if model.sessions.taskPanel /= Just task then text "" else
    let
        page = taskPage False workspace model task
        fresh = freshTaskPage False workspace model task /= Nothing
        sessions = page |> Maybe.map (.items >> List.filter (effectiveActive workspace model)) |> Maybe.withDefault []
        error = resource workspace (sectionTarget False model task) model |> Maybe.andThen .error
        failed = error /= Nothing || (Dict.get (sectionKey False task) model.sessions.taskPages |> Maybe.map (\cached -> case cached.result of
            Err _ -> True
            Ok _ -> False) |> Maybe.withDefault False)
        key = sectionKey False task
    in
    aside [class "task-session-dock",attribute "aria-label" "Task active agents",attribute "aria-busy" (if fresh then "false" else "true"),stopPropagationOn "click" (Decode.succeed (NoOp,True)),stopPropagationOn "dblclick" (Decode.succeed (NoOp,True))]
        [div [class "task-session-dock-header"] [div [] [span [class "session-pane-label"] [text "Task context"],strong [class "task-session-context-title"] [text (Dict.get task model.tasks |> Maybe.map .title |> Maybe.withDefault "Task")]],button [class "btn btn-secondary btn-sm",attribute "aria-label" "Close active agents",onClick (SessionsToggleDock task)] [text "✕"]]
        ,div [class "task-session-context-grid"]
            [if includeChoices then viewTaskActiveChoices workspace model task else text ""
            ,model.sessions.selected |> Maybe.map (\id -> div [class "task-session-transcript"]
                [a [class "btn btn-sm task-session-open-full",href (sessionUrl workspace id ++ "&task=" ++ Url.percentEncode task)] [text "Open full session ↗"]
                ,resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .data |> Maybe.map (\payload -> case payload of
                    Delivery.Metadata session -> div [class "task-session-selected-context"] [h4 [] [text (sessionTitle session)],viewAncestorPath session]
                    _ -> text "") |> Maybe.withDefault (text "")
                ,viewRecords workspace model id]) |> Maybe.withDefault (div [class "empty-state"] [text "Select an active session to read its recent conversation."])
            ]]


viewWorkspaceInspector : String -> Model -> Html Msg
viewWorkspaceInspector workspace model =
    let
        task = model.sessions.taskPanel |> Maybe.andThen (\id -> Dict.get id model.tasks)
        focused = model.focus.focusedEntity
        returnTarget = task |> Maybe.map .id |> Maybe.withDefault (focused |> Maybe.map Tuple.second |> Maybe.withDefault "")
        session = model.sessions.selected |> Maybe.andThen (\id -> resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .data |> Maybe.andThen (\payload -> case payload of
            Delivery.Metadata value -> Just value
            _ -> Nothing))
        taskContext current =
            [span [class "session-pane-label"] [text "Task"],h3 [] [text current.title],p [class "card-meta"] [text (Api.taskStatusToString current.status ++ " · P" ++ String.fromInt current.priority)]]
    in
    aside [class "workspace-context-inspector",attribute "aria-label" "Selected task context"]
        ([h2 [class "session-pane-label"] [text "Inspector"]
         ,if returnTarget == "" then text "" else button [class "btn btn-secondary btn-sm workspace-inspector-return",onClick (ScrollToEntity returnTarget)] [text "Return to selected card"]]
            ++ (case task of
                Just current -> taskContext current ++ [p [class "card-meta"] [text "The active session preview is open in this task's card."],session |> Maybe.map (\value -> div [class "workspace-inspector-session"] [h4 [] [text (sessionTitle value)],viewSessionIdentity value,viewAncestorPath value]) |> Maybe.withDefault (text "")]
                Nothing -> case focused of
                    Just ("task",id) -> Dict.get id model.tasks |> Maybe.map taskContext |> Maybe.withDefault [p [class "card-meta"] [text "Task context is loading or unavailable."]]
                    Just ("project",id) -> Dict.get id model.projects |> Maybe.map (\project -> [span [class "session-pane-label"] [text "Project"],h3 [] [text project.name],p [class "card-meta"] [text (Api.projectStatusToString project.status ++ " · P" ++ String.fromInt project.priority)]]) |> Maybe.withDefault [p [class "card-meta"] [text "Project context is loading or unavailable."]]
                    _ -> [p [class "empty-state"] [text "Focus a project or task, or open its active agents, to inspect its context."]]))


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
        ,if evidence then viewArchive model task else text ""
        ,if page == Nothing then p [class "card-meta"] [text (if cachedError /= Nothing then "Sessions could not be loaded." else "Loading sessions…"),if cachedError /= Nothing then button [class "btn btn-sm",onClick SessionsRetry] [text "Retry"] else text ""]
         else if List.isEmpty sessions then p [class "card-meta"] [text ("No " ++ String.toLower heading ++ " on this page.")]
         else div [class "task-session-cards"] (List.map (\session -> compactSessionCard model session "task-session-card" (a [class "session-summary-link",href (sessionUrl workspace session.id)]) (if evidence then [viewSave model session] else [])) sessions)
        ,div [class "sessions-pagination"]
            [if taskCursor model key /= Nothing then button [class "btn btn-sm",onClick (SessionsTaskPreviousPage key)] [text "Newer sessions"] else text ""
            ,page |> Maybe.andThen .nextCursor |> Maybe.map (\cursor -> button [class "btn btn-sm",onClick (SessionsTaskNextPage key cursor)] [text "Older sessions"]) |> Maybe.withDefault (text "")]
        ,if evidence && model.sessions.archive == Nothing && model.sessions.saveMessage /= "" && (model.sessions.archiveMessageTask == Nothing || model.sessions.archiveMessageTask == Just task || model.sessions.saveBlocked) then p [class "card-meta",attribute "role" "status"] [text model.sessions.saveMessage] else text ""]


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
            [session |> Maybe.map viewSessionIdentity |> Maybe.withDefault (text "")]
        ,if state.error /= Nothing || rangeError /= Nothing then p [attribute "role" "alert"] [text (Maybe.withDefault "Transcript request failed." state.error),button [onClick SessionsRetry] [text "Retry"]] else text ""
        ,Keyed.node "div" [Html.Attributes.id "session-records",class "session-records",tabindex 0,attribute "data-viewer-owner" state.stamp,attribute "aria-label" "Bounded transcript records"]
            (List.map (\item -> let (key,node) = viewTranscriptItem model item in (state.stamp ++ ":" ++ key,node)) (Transcript.project state.rows))
        ,if List.isEmpty state.rows then p [attribute "role" "status"] [text (if state.start == Nothing then "Loading transcript metadata…" else if state.offset < state.end then "Reading transcript…" else if newest == 0 then "This session has no committed records." else "No complete messages in this window.")] else text ""
        ,text "" ]



viewTranscriptItem : Model -> Transcript.Item -> (String,Html Msg)
viewTranscriptItem model item =
    case item of
        Transcript.Message piece ->
            ("message:" ++ piece.key,viewTranscriptPiece model piece)

        Transcript.Activity pieces ->
            let
                firstKey = List.head pieces |> Maybe.map (\piece -> Maybe.withDefault piece.key piece.activityAnchor) |> Maybe.withDefault ""
                firstTime = List.filterMap (\piece -> if piece.record.time == "" then Nothing else Just piece.record.time) pieces |> List.head |> Maybe.withDefault ""
                count = List.map (.blocks >> List.length) pieces |> List.sum
            in
            ("activity:" ++ firstKey,details [class "session-activity-group",attribute "data-group-key" firstKey]
                [ summary [class "session-activity-summary"]
                    [if firstTime == "" then text "" else time [class "session-event-time",attribute "datetime" firstTime] [text (compactTimestamp model firstTime)]
                    ,span [] [text ("Activity · " ++ String.fromInt count ++ (if count == 1 then " event" else " events"))]]
                ,Keyed.node "div" [class "session-activity-records"] (List.map (\piece -> (piece.key,viewTranscriptPiece model piece)) pieces)])


viewTranscriptPiece : Model -> Transcript.Piece -> Html Msg
viewTranscriptPiece model piece =
    let record = piece.record in
    article [class ("session-record session-role-" ++ record.role ++ (if piece.activity then " session-record-activity" else "")),attribute "data-offset" (String.fromInt record.offset),attribute "data-block-key" piece.key]
        [div [class "session-record-meta"] [if record.time == "" then text "" else time [class "session-event-time",attribute "datetime" record.time] [text (compactTimestamp model record.time)],if piece.activity then text "" else span [] [text (if record.role == "user" then "You" else if record.role == "tool" then "Tool" else if record.role == "assistant" then "Assistant" else "Message")]]
        ,div [] (List.indexedMap (\index block -> viewTranscriptBlock model (piece.key ++ ":" ++ String.fromInt index) block) piece.blocks)]


viewTranscriptBlock : Model -> String -> Transcript.Block -> Html Msg
viewTranscriptBlock model key block =
    if block.kind == "text" then sessionMarkdown block.content
    else if block.kind == "encrypted" then em [class "session-thinking"] [text "Thinking..."]
    else div [class ("session-event" ++ (if model.sessions.inspectedEvent == Just key then " is-inspected" else ""))]
        [ button [class "btn btn-sm session-inspect-event",attribute "aria-label" ("Inspect event: " ++ (if block.label == "" then "Thinking" else block.label)),attribute "aria-pressed" (if model.sessions.inspectedEvent == Just key then "true" else "false"),onClick (SessionsInspect model.sessions.stamp key)] [text "Inspect ↗"]
        , details [class ("session-content-" ++ block.kind),attribute "data-event-key" key] [summary [Html.Attributes.title block.label] [text (if block.label /= "" then block.label else if block.kind == "thinking" then "Thinking" else "Tool activity")],viewBlockContent block] ]


viewBlockContent : Transcript.Block -> Html Msg
viewBlockContent block =
    if block.kind == "thinking" then sessionMarkdown block.content else pre [class "session-tool-content"] [code [class "language-json"] (if List.isEmpty block.tokens then [text block.content] else List.map (\(kind,content) -> span [class ("json-" ++ kind)] [text content]) block.tokens)]


sessionMarkdown : String -> Html Msg
sessionMarkdown content =
    case Markdown.Parser.parse content |> Result.mapError (always "parse error") |> Result.andThen (Markdown.Renderer.render sessionMarkdownRenderer) of
        Ok rendered -> div [class "markdown-content session-record-text"] rendered
        Err _ -> pre [class "session-record-text"] [text content]

compactTimestamp : Model -> String -> String
compactTimestamp model source =
    let formatted = TimestampDisplay.lookup model.timestamps source in
    if model.sessions.today /= "" && TimestampDisplay.dayKey model.timestamps source == model.sessions.today then String.right 5 formatted else formatted


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
            Delivery.SessionsPage page -> List.map sessionActivity page.items
            _ -> []) |> Maybe.withDefault []
        cards = Dict.values model.sessions.taskPages |> List.concatMap (\cached -> Result.toMaybe cached.result |> Maybe.map (.items >> List.map sessionActivity) |> Maybe.withDefault [])
    in
    if model.activeTab == SessionsTab then list ++ List.map .time model.sessions.rows else if model.activeTab == ProjectsTab then cards ++ (if model.sessions.taskPanel /= Nothing then List.map .time model.sessions.rows else []) else []


availableSessions : Model -> List Api.AgentSession
availableSessions model =
    (Dict.values model.sessionDelivery.resources |> List.filter (\entry -> Just entry.workspace == model.sessions.workspace) |> List.concatMap (\entry -> case entry.data of
        Just (Delivery.Metadata session) -> [session]
        Just (Delivery.SessionsPage page) -> page.items
        _ -> [])) ++ (Dict.values model.sessions.taskPages |> List.concatMap (\cached -> Result.toMaybe cached.result |> Maybe.map .items |> Maybe.withDefault []))


sessionActivity : Api.AgentSession -> String
sessionActivity session =
    if session.state == Api.SessionTerminated then Maybe.withDefault session.lastSeenAt session.terminatedAt else session.lastSeenAt


viewArchive : Model -> String -> Html Msg
viewArchive model task =
    let
        state = model.sessions
        title = Dict.get task model.tasks |> Maybe.map .title |> Maybe.withDefault "Task evidence"
        config = Encode.encode 0 (Encode.object [("stamp",Encode.string state.downloadStamp),("operation",Encode.int state.nextOperation),("task",Encode.string task),("title",Encode.string title)])
    in
    case state.archive of
        Just archive ->
            if archive.task == task then div [class "task-evidence-download",attribute "role" "status"]
                [span [] [text (state.saveMessage)],button [class "btn btn-sm",onClick SessionsCancelSave] [text "Cancel"]]
            else button [class "btn btn-sm",disabled True] [text "↓ Download all"]
        Nothing -> button [class "btn btn-sm task-evidence-download",attribute "data-session-archive" config,disabled (state.saveBlocked || state.save /= Nothing)] [text "↓ Download all"]


archiveRequest : Model -> Encode.Value
archiveRequest model =
    case model.sessions.archive of
        Just archive ->
            Dict.values model.sessionDelivery.physical |> List.filter (\request -> request.target == Delivery.TaskEvidenceArchive archive.task archive.operation && Dict.member request.id model.sessions.archiveRequests) |> List.head
                |> Maybe.map (\request -> Encode.object [("id",Encode.int request.id),("stamp",Encode.string model.sessions.downloadStamp),("operation",Encode.int archive.operation),("task",Encode.string archive.task),("url",Encode.string (model.flags.apiUrl ++ "/api/v1/workspaces/" ++ Url.percentEncode request.workspace ++ "/tasks/" ++ Url.percentEncode archive.task ++ "/evidence.zip"))]) |> Maybe.withDefault Encode.null
        Nothing -> Encode.null


receiveArchiveHttp : Decode.Value -> Model -> (Model,Cmd Msg)
receiveArchiveHttp value model =
    let
        field name decoder fallback = Decode.decodeValue (Decode.field name decoder) value |> Result.withDefault fallback
        id = field "request" Decode.int -1
        status = field "status" Decode.int 0
        state = model.sessions
    in
    case Dict.get id state.archiveRequests of
        Nothing -> (model,Cmd.none)
        Just guard ->
            let
                result = if status == 200 then Ok Delivery.ArchiveDownloaded else if status > 0 then Err (Http.BadStatus status) else Err (Http.BadBody "Archive transport failed")
                retired = Dict.get id model.sessionDelivery.physical |> Maybe.map (\request -> Delivery.removeDemand request.workspace request.target model.sessionDelivery) |> Maybe.withDefault model.sessionDelivery
                (updated,command) = WebSocket.update (SessionResourceFetched id guard result) {model | sessionDelivery=retired,sessions={state | archiveRequests=Dict.remove id state.archiveRequests}}
                current = updated.sessions
                matches = field "stamp" Decode.string "" == current.downloadStamp && (current.archive |> Maybe.map (\archive -> archive.operation == field "operation" Decode.int -1) |> Maybe.withDefault False)
            in
            if matches then ({updated | sessions={current | archive=Nothing,saveMessage=if status == 200 then "Downloaded task evidence." else "Archive could not finish. Please retry."}},command)
            else (updated,command)


receiveArchive : Decode.Value -> Model -> (Model,Cmd Msg)
receiveArchive value model =
    let
        state = model.sessions
        field name decoder fallback = Decode.decodeValue (Decode.field name decoder) value |> Result.withDefault fallback
        operation = field "operation" Decode.int -1
        kind = field "kind" Decode.string ""
        task = field "task" Decode.string ""
    in
    if field "stamp" Decode.string "" /= state.downloadStamp || model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) then (model,Cmd.none)
    else if kind == "archive-error" && field "code" Decode.string "" == "writer_cleanup_failed" && operation > 0 && operation < state.nextOperation then
        ({model | sessions={state | archive=Nothing,save=Nothing,saveBlocked=True,saveMessage="Writer cleanup could not be confirmed. Reload this page before another download."}},Cmd.none)
    else if kind == "archive-picking" && operation == state.nextOperation && state.save == Nothing && state.archive == Nothing && Dict.member task model.tasks then
        ({model | sessions={state | archiveMessageTask=Just task,archive=Just (SessionArchiveOperation operation task (field "title" Decode.string "Task evidence") "picking"),nextOperation=operation+1,saveMessage="Preparing ZIP…"}},Cmd.none)
    else case state.archive of
        Just archive ->
            if archive.operation /= operation then (model,Cmd.none)
            else if kind == "archive-ready" then ({model | sessions={state | archive=Just {archive | phase="waiting"},saveMessage="Downloading compressed evidence…"}},Cmd.none)
            else if kind == "archive-progress" then ({model | sessions={state | saveMessage="Downloading compressed evidence…"}},Cmd.none)
            else if kind == "archive-error" then
                let blocked = field "code" Decode.string "" == "writer_cleanup_failed" in
                ({model | sessions={state | archive=Nothing,save=Nothing,saveBlocked=state.saveBlocked || blocked,saveMessage=if blocked then "Writer cleanup could not be confirmed. Reload this page before another download." else if field "code" Decode.string "" == "download_storage_unavailable" then "This browser needs temporary file storage for archives larger than 32 MiB." else "Archive could not finish. Please retry."}},Cmd.none)
            else (model,Cmd.none)
        Nothing -> (model,Cmd.none)


