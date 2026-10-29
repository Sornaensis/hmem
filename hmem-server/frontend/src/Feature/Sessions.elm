module Feature.Sessions exposing (reconcile, update, view, viewTaskPanel, viewTaskActive, viewTaskActiveChoices, viewTaskActiveConversation, viewTaskEvidence, viewTaskIcon, viewWorkspaceInspector, taskActive, timestampSources, olderWindow, viewerReset)

import Api
import ActorAttribution
import Browser.Navigation as Nav
import CardSections
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
        scoped = if reset then { old | workspace = workspace, epoch = model.sessionRequestEpoch, selected = selected, cursor = Nothing, previous = [], linksCursor = Nothing, showAllLinkedTasks = False, filters = Api.defaultSessionFilters, taskFilter = taskFilter, projectFilter = projectFilter, projectFacetCursor = Nothing, taskFacetCursor = Nothing, activityDirty = False, taskCursors = Dict.empty, taskPreviousPages = Dict.empty, taskPages = Dict.empty, taskHints = Dict.empty, taskAssociations = Dict.empty, hintOrder = [], sectionOrder = [] }
            else if old.selected /= selected then { old | selected = selected, linksCursor = Nothing, showAllLinkedTasks = False } else old
        filtered = if scoped.taskFilter /= taskFilter || scoped.projectFilter /= projectFilter then { scoped | taskFilter = taskFilter, projectFilter = projectFilter, taskFacetCursor = Nothing, cursor = Nothing, previous = [] } else scoped
        hintEpoch = model.sessionDelivery.listInvalidation
        retainedHints = Dict.filter (\id _ -> List.member id taskIds) filtered.taskHints
        consumeHint (ws,target) accumulated = case target of
            Delivery.TaskSessionsFiltered task filters cursor ->
                if cursor /= Nothing || filters.state /= "active" || not (List.member task taskIds) then accumulated else
                case resource ws target model of
                    Just entry ->
                        if entry.error /= Nothing then Dict.insert task {active=False,error=True,epoch=hintEpoch} accumulated else if not entry.proven || entry.dirty /= entry.settled then accumulated else
                        case entry.data of
                            Just (Delivery.SessionsPage page) -> Dict.insert task {active=not (List.isEmpty page.items),error=False,epoch=hintEpoch} accumulated
                            _ -> if entry.error /= Nothing then Dict.insert task {active=False,error=True,epoch=hintEpoch} accumulated else accumulated
                    Nothing -> accumulated
            _ -> accumulated
        hints = if reset then Dict.empty else List.foldl consumeHint retainedHints old.owned
        rotation = List.filter (\id -> List.member id taskIds) filtered.hintOrder ++ List.filter (\id -> not (List.member id filtered.hintOrder)) taskIds
        unresolved id = Dict.get id hints |> Maybe.map (\hint -> hint.epoch /= hintEpoch) |> Maybe.withDefault True
        retainedAssociations = Dict.filter (\id _ -> List.member id taskIds) filtered.taskAssociations
        consumeAssociation (ws,target) accumulated = case target of
            Delivery.TaskSessionsFiltered task filters cursor ->
                if Just ws /= workspace || cursor /= Nothing || (filters.state /= "" && filters.state /= "active") || filters.freshness /= "" || not (List.member task taskIds) then accumulated else
                case resource ws target model of
                    Just entry ->
                        if entry.error /= Nothing then
                            if filters.state == "" then Dict.insert task {present=False,error=True,epoch=hintEpoch} accumulated else accumulated
                        else if not entry.proven || entry.dirty /= entry.settled then accumulated
                        else case entry.data of
                            Just (Delivery.SessionsPage page) ->
                                if filters.state == "" || not (List.isEmpty page.items) then Dict.insert task {present=not (List.isEmpty page.items),error=False,epoch=hintEpoch} accumulated else accumulated
                            _ -> accumulated
                    Nothing -> accumulated
            _ -> accumulated
        associations = if reset then Dict.empty else List.foldl consumeAssociation retainedAssociations old.owned
        needsAssociation id =
            (Dict.get id hints |> Maybe.map (\hint -> hint.epoch == hintEpoch && not hint.error && not hint.active) |> Maybe.withDefault False)
                && (Dict.get id associations |> Maybe.map (\hint -> hint.epoch /= hintEpoch) |> Maybe.withDefault True)
        hintTarget id =
            if unresolved id then Just (Delivery.TaskSessionsFiltered id {state="active",freshness=""} Nothing)
            else if needsAssociation id then Just (Delivery.TaskSessionsFiltered id {state="",freshness=""} Nothing)
            else Nothing
        pendingHintTargets = old.owned |> List.filterMap (\(ws,target) -> case target of
            Delivery.TaskSessionsFiltered task filters cursor ->
                if Just ws == workspace && cursor == Nothing && (filters.state == "active" || filters.state == "") && filters.freshness == "" && List.member task taskIds && (resource ws target model |> Maybe.map (\entry -> entry.error == Nothing && entry.dirty /= entry.settled) |> Maybe.withDefault False) then Just target else Nothing
            _ -> Nothing)
        hintTargets = (pendingHintTargets ++ List.filterMap hintTarget rotation) |> List.foldl (\target targets -> if List.member target targets then targets else targets ++ [target]) [] |> List.take 2
        hintTasks = List.filterMap (\target -> case target of
            Delivery.TaskSessionsFiltered task _ _ -> Just task
            _ -> Nothing) hintTargets
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
        withPanel = { filtered | taskPanel = taskPanel, returnTaskPanel = returnTaskPanel, viewerScope = viewerScope, downloadStamp = downloadStamp, taskHints = hints, taskAssociations = associations, hintOrder = nextRotation, taskPages = pages, sectionOrder = List.map sectionIdentity (List.filter (\target -> not (List.member target sectionReads)) orderedSections ++ sectionReads) }
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
                    { withDownloads | stamp = stamp, inspectedEvent = Nothing, generation = old.generation + 1, resetPending = True, start = Nothing, offset = 0, end = 0, following = True, loadWindow = True, pending = False, rows = [], notice = "", error = Nothing, heartbeats = Dict.empty, sealed = emptySealedSessionView }
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
                if session.state == Api.SessionTerminated then
                    { retired | start = Just 0, offset = 0, end = 0, resetPending = False, pending = False, rows = [] }
                else if retired.start == Nothing then
                    let begin = max 0 (session.committedBytes - 2097152) in
                    { retired | start = Just begin, offset = begin, end = session.committedBytes, generation = retired.generation + 1 }
                else if retired.following && retired.offset >= retired.end && not retired.pending then
                    { retired | end = min session.committedBytes (retired.offset + 2097152) }
                else retired
            Nothing -> retired
        current = beforeArchive
        isSealed = metadata |> Maybe.map (.state >> (==) Api.SessionTerminated) |> Maybe.withDefault False
        window = Maybe.map2 (\ws session -> (ws,Delivery.SessionWindow session.id current.offset current.end (session.state == Api.SessionTerminated))) workspace metadata
            |> Maybe.andThen (\target -> if not isSealed && current.loadWindow && current.error == Nothing && current.offset < current.end then Just target else Nothing)
        sealedTarget = Maybe.map2 (\ws request -> (ws,Delivery.SessionArchiveHttp (Maybe.withDefault "" selected) request.sequence)) workspace current.sealed.http
        saveTarget = Maybe.map2 (\ws operation -> (ws,Delivery.SessionSave operation.session operation.offset (min 65536 (operation.size-operation.offset)))) workspace current.save
            |> Maybe.andThen (\target -> current.save |> Maybe.andThen (\operation -> if operation.phase == "writing" && not operation.pending && operation.offset < operation.size then Just target else Nothing))
        archiveTarget = Maybe.map2 (\ws archive -> (ws,Delivery.TaskEvidenceArchive archive.task archive.operation)) workspace current.archive |> Maybe.andThen (\target -> current.archive |> Maybe.andThen (\archive -> if archive.phase == "waiting" then Just target else Nothing))
        desired = workspace |> Maybe.map (\ws ->
            (if model.activeTab == SessionsTab then [(ws,listTarget current),(ws,facetTarget "projects" current),(ws,facetTarget "tasks" current)] else []) ++
            (selected |> Maybe.map (\id -> [(ws,Delivery.SessionMetadata id)] ++ (if model.activeTab == SessionsTab then [(ws,Delivery.SessionTasks id current.linksCursor)] else [])) |> Maybe.withDefault []) ++
            (archiveTarget |> Maybe.map List.singleton |> Maybe.withDefault []) ++
            (sealedTarget |> Maybe.map List.singleton |> Maybe.withDefault []) ++
            (saveTarget |> Maybe.map List.singleton |> Maybe.withDefault []) ++
            (List.map (\target -> (ws,target)) (List.take (if current.archive /= Nothing || current.save /= Nothing then 10 else 12) sectionReads)) ++
            (List.map (\target -> (ws,target)) hintTargets) ++
            (window |> Maybe.map List.singleton |> Maybe.withDefault [])) |> Maybe.withDefault []
        identities = List.map (\(ws,target) -> Delivery.key ws target) desired
        selection = Maybe.map2 (\ws id -> {workspace=ws,session=id,lifetime=current.stamp ++ ":" ++ String.fromInt current.generation}) workspace selected
        selectedDelivery = Delivery.select selection model.sessionDelivery
        kept = List.foldl (\(ws,target) state -> if List.member (Delivery.key ws target) identities then state else Delivery.removeDemand ws target state) selectedDelivery old.owned
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
            , ("sealed",Encode.object
                [("active",Encode.bool (isSealed && selected /= Nothing)),("stamp",Encode.string stamp),("load",Encode.int current.generation)
                ,("workspace",Encode.string (Maybe.withDefault "" workspace)),("session",Encode.string (Maybe.withDefault "" selected)),("apiUrl",Encode.string model.flags.apiUrl)
                ,("authority",Encode.string (model.sessionContext |> Maybe.map (\context -> Encode.encode 0 (Encode.list Encode.string [context.principal.actorType,context.principal.actorId,context.principal.authority,Maybe.withDefault "" context.principal.grantUserId])) |> Maybe.withDefault ""))])
            , ("sealedRequest",sealedRequest next)
            , ("sealedMarkdownAck",Encode.object [("stamp",Encode.string stamp),("load",Encode.int current.generation),("request",Encode.int current.sealed.markdown.request)])
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
            if model.activeTab /= ProjectsTab || not (Dict.member task model.tasks) || not (List.member task (mountedTasks model)) || (state.taskPanel /= Just task && not (taskCanOpen (Maybe.withDefault "" state.workspace) model task)) then (model,Cmd.none)
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
                    Delivery.invalidateFacets ws (Delivery.invalidateLists ws (Delivery.retrySelected delivery))
                        |> (\current -> state.selected |> Maybe.map (\id -> Delivery.invalidateSession ws id current) |> Maybe.withDefault current)
                        |> (\current -> state.taskPanel |> Maybe.map (\id -> Delivery.invalidateTask ws id current) |> Maybe.withDefault current)
                        |> (\current -> List.foldl (\(ownedWorkspace,target) accumulated -> case target of
                            Delivery.SessionWindow _ _ _ _ -> Delivery.removeDemand ownedWorkspace target accumulated
                            _ -> accumulated) current state.owned)
            in
            ( {model | sessions = {state | error = Nothing,pending = False,loadWindow = True, sealed = emptySealedSessionView
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
    if kind == "sealed-http" then
        let sealed=state.sealed in
        receiveArchiveHttp value {model | sessions={state | sealed=if sealed.http |> Maybe.map (.sequence >> (==) (field "sequence" Decode.int -1)) |> Maybe.withDefault False then {sealed | http=Nothing} else sealed}}
    else if String.startsWith "sealed-" kind then receiveSealed value model
    else if kind == "archive-http" then receiveArchiveHttp value model
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


sealedRequest : Model -> Encode.Value
sealedRequest model =
    case model.sessions.sealed.http of
        Nothing -> Encode.null
        Just demand ->
            Dict.values model.sessionDelivery.physical
                |> List.filter (\request -> request.target == Delivery.SessionArchiveHttp (Maybe.withDefault "" model.sessions.selected) demand.sequence && Dict.member request.id model.sessions.archiveRequests)
                |> List.head
                |> Maybe.map (\request -> Encode.object [("id",Encode.int request.id),("sequence",Encode.int demand.sequence),("workspace",Encode.string request.workspace),("session",Encode.string (Maybe.withDefault "" model.sessions.selected)),("stamp",Encode.string model.sessions.stamp),("load",Encode.int model.sessions.generation),("archiveGeneration",Encode.string model.sessions.sealed.archiveGeneration)])
                |> Maybe.withDefault Encode.null


receiveSealed : Decode.Value -> Model -> ( Model, Cmd Msg )
receiveSealed value model =
    let
        state = model.sessions
        sealed = let previous = state.sealed in {previous | domEpoch=field "domEpoch" Decode.string previous.domEpoch}
        field name decoder fallback = Decode.decodeValue (Decode.field name decoder) value |> Result.withDefault fallback
        kind = field "kind" Decode.string ""
        piece = Decode.map8 (\key offset blockKind label role time group count -> {key=key,offset=offset,kind=blockKind,label=label,role=role,time=time,group=group,count=count,open=False,groupOpen=False,body=Nothing,ast=Err "not_loaded"})
            (Decode.field "key" Decode.string) (Decode.field "offset" Decode.string) (Decode.field "kind" Decode.string) (Decode.field "label" Decode.string)
            (Decode.field "role" Decode.string) (Decode.field "time" Decode.string) (Decode.field "group" Decode.string) (Decode.field "count" Decode.string)
            |> Decode.andThen (\row -> Decode.map3 (\open groupOpen body -> {row | open=open,groupOpen=groupOpen,body=body,ast=case body of
                    Just content -> if content.format == "markdown" && not content.more && content.part == 0 then Markdown.Parser.parse content.text |> Result.mapError (always "parse error") else Err "not_loaded"
                    Nothing -> Err "not_loaded"})
                (Decode.field "open" Decode.bool) (Decode.field "groupOpen" Decode.bool)
                (Decode.oneOf [Decode.field "body" (Decode.map7 (\text part more tokens format structured exception -> Just {text=text,part=part,more=more,tokens=tokens,format=format,structured=structured,exception=exception}) (Decode.field "text" Decode.string) (Decode.field "part" Decode.int) (Decode.field "more" Decode.bool) (Decode.oneOf [Decode.field "tokens" (Decode.list (Decode.map2 Tuple.pair (Decode.field "kind" Decode.string) (Decode.field "text" Decode.string))),Decode.succeed []]) (Decode.oneOf [Decode.field "format" Decode.string,Decode.succeed "markdown"]) (Decode.oneOf [Decode.field "structured" Decode.bool,Decode.succeed False]) (Decode.oneOf [Decode.field "exception" Decode.bool,Decode.succeed False])),Decode.succeed Nothing]))
    in
    if field "stamp" Decode.string "" /= state.stamp || field "generation" Decode.int -1 /= state.generation
        || Just (field "workspace" Decode.string "") /= state.workspace || Just (field "session" Decode.string "") /= state.selected
        || model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) then (model,Cmd.none)
    else if kind == "sealed-demand" then
        ({model | sessions={state | sealed={sealed | http=Just {sequence=field "sequence" Decode.int -1,path=field "path" Decode.string ""}}}},Cmd.none)
    else if kind == "sealed-clear" then
        ({model | sessions={state | inspectedEvent=Nothing,rows=[],sealed={sealed | phase="preparing",http=Nothing,rows=[],first="",last="",more=False,renderPart=0,inspectorPart=0,markdown=emptySealedSessionView.markdown}}},Cmd.none)
    else if kind == "sealed-error" then
        let code=field "code" Decode.string "archive_failed" in
        ({model | sessions={state | inspectedEvent=Nothing,sealed={sealed | phase="error",http=Nothing,rows=[],markdown=emptySealedSessionView.markdown
            ,error=Just (if code == "archive_cleanup_failed" then "Archive cleanup could not be confirmed. Reload this page before another archive." else if code == "archive_item_limit" then "One record exceeds the local view limit. The original transcript is still available to download." else if code == "archive_recovery_limit" then "Local archive recovery reached its directory limit. Close other archive tabs or clear this site's stored archive data before retrying." else if code == "archive_recovery_pending" then "Old local archives were removed. Retry to finish recovery." else code)}}},Cmd.none)
    else if kind == "sealed-cancelled" then
        ({model | sessions={state | inspectedEvent=Nothing,sealed={sealed | phase="cancelled",http=Nothing,rows=[],markdown=emptySealedSessionView.markdown,error=Nothing}}},Cmd.none)
    else if kind == "sealed-inspect" then
        ({model | sessions={state | inspectedEvent=Just (field "key" Decode.string ""),sealed={sealed | inspectorPart=0}}},Cmd.none)
    else if kind == "sealed-render" then
        let part=field "part" Decode.int -1 in
        if part < 0 || part > 100000 then (model,Cmd.none)
        else ({model | sessions={state | sealed=if field "inspector" Decode.bool False then {sealed | inspectorPart=part} else {sealed | renderPart=part}}},Cmd.none)
    else if kind == "sealed-disclosure" then
        let key=field "key" Decode.string ""
            open=field "open" Decode.bool False
            updateDisclosure row=if row.key == key then {row | open=open} else if "group:" ++ row.group == key then {row | groupOpen=open} else row
        in ({model | sessions={state | sealed={sealed | rows=List.map updateDisclosure sealed.rows}}},Cmd.none)
    else if kind == "sealed-markdown-start" then
        ({model | sessions={state | sealed={sealed | phase="reading-body",markdown={key=field "key" Decode.string "",source="",ast=Err "not_loaded",structured=False,complete=False,nextPart=0,request=0}}}},Cmd.none)
    else if kind == "sealed-markdown-chunk" then
        let
            previous=sealed.markdown
            textPart=field "text" Decode.string ""
            complete=field "complete" Decode.bool False
            source=previous.source++textPart
        in
        if field "key" Decode.string "" /= previous.key || field "part" Decode.int -1 /= previous.nextPart || String.length previous.source + String.length textPart > 1048576 then (model,Cmd.none)
        else ({model | sessions={state | sealed={sealed | phase=if complete then "ready" else "reading-body",renderPart=0,inspectorPart=0
            ,markdown={previous | source=source,ast=if complete then Markdown.Parser.parse source |> Result.mapError (always "parse error") else previous.ast,structured=field "structured" Decode.bool False,complete=complete,nextPart=previous.nextPart+1,request=field "request" Decode.int 0}}}},Cmd.none)
    else if kind == "sealed-progress" then
        ({model | sessions={state | sealed={sealed | phase=field "phase" Decode.string sealed.phase,rawBytes=field "rawBytes" Decode.string sealed.rawBytes,compressedBytes=field "compressedBytes" Decode.string sealed.compressedBytes,archiveGeneration=field "archiveGeneration" Decode.string sealed.archiveGeneration}}},Cmd.none)
    else if kind == "sealed-page" then
        case Decode.decodeValue (Decode.field "rows" (Decode.list piece)) value of
            Ok rows ->
                if List.length rows <= 200 && String.length (Encode.encode 0 value) <= 4194304 then
                    ({model | sessions={state | sealed={sealed | phase="ready",error=Nothing,http=Nothing,rows=rows,first=field "first" Decode.string "",last=field "last" Decode.string "",more=field "more" Decode.bool False,archiveGeneration=field "archiveGeneration" Decode.string "",renderPart=if field "fromEnd" Decode.bool False then -1 else 0,inspectorPart=0,markdown=emptySealedSessionView.markdown}}},Cmd.none)
                else ({model | sessions={state | sealed={sealed | phase="error",rows=[],error=Just "archive_page_limit"}}},Cmd.none)
            Err _ -> ({model | sessions={state | sealed={sealed | phase="error",rows=[],error=Just "archive_page_invalid"}}},Cmd.none)
    else (model,Cmd.none)


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
        archived = model.sessions.inspectedEvent |> Maybe.andThen (\key -> List.filter (.key >> (==) key) model.sessions.sealed.rows |> List.head
            |> Maybe.andThen (\piece -> piece.body |> Maybe.map (\body -> (key,{kind=piece.kind,content=body.text,label=piece.label,tokens=body.tokens}))))
        selected = if resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .error |> (/=) Nothing then Nothing else
            model.sessions.inspectedEvent |> Maybe.andThen (\key -> List.filter (Tuple.first >> (==) key) (inspectableEvents model.sessions.rows) |> List.head) |> (\live -> if live == Nothing then archived else live)
        metadata = resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .data |> Maybe.andThen (\payload -> case payload of
            Delivery.Metadata session -> Just session
            _ -> Nothing)
        ownerAttributes = if metadata |> Maybe.map (.state >> (==) Api.SessionTerminated) |> Maybe.withDefault False then
            [attribute "data-sealed-dom-owner" model.sessions.stamp,attribute "data-sealed-dom-load" (String.fromInt model.sessions.generation)] else []
        renderHost = if metadata |> Maybe.map (.state >> (==) Api.SessionTerminated) |> Maybe.withDefault False then
            \attributes children -> Keyed.node "aside" attributes (sealedHostNodes model children)
            else aside
    in
    renderHost [class "session-event-inspector",attribute "aria-label" "Event inspector"]
        [ div [class "session-inspector-heading"] [h4 [] [text "Event inspector"]]
        , Keyed.node "div" (class "session-inspected-event" :: ownerAttributes)
            (selected |> Maybe.map (\(key,block) -> [(model.sessions.stamp ++ ":" ++ key,div []
                [h5 [] [text (if block.label == "" then "Activity details" else block.label)]
                ,p [class "card-meta"] [text (if block.kind == "tool" then "Tool input" else if block.kind == "result" then "Tool result" else "Thinking summary")]
                ,if archived /= Nothing then viewSealedInspector model block else viewBlockContent block])]) |> Maybe.withDefault [("empty",p [class "empty-state"] [text (if model.sessions.inspectedEvent == Nothing then "Select Inspect on an activity event to read its details here." else "The selected event is outside this transcript window.")])])
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


taskAssociation : Model -> String -> Maybe Bool
taskAssociation model task =
    Dict.get task model.sessions.taskAssociations |> Maybe.andThen (\hint ->
        if hint.error || hint.epoch /= model.sessionDelivery.listInvalidation || model.sessions.epoch /= model.sessionRequestEpoch then Nothing else Just hint.present)


taskActivityCurrent : String -> Model -> String -> Bool
taskActivityCurrent workspace model task =
    let
        hintCurrent = Dict.get task model.sessions.taskHints |> Maybe.map (\hint -> not hint.error && hint.epoch == model.sessionDelivery.listInvalidation && model.sessions.epoch == model.sessionRequestEpoch) |> Maybe.withDefault False
        pageCurrent = resource workspace (Delivery.TaskSessionsFiltered task {state="active",freshness=""} Nothing) model |> Maybe.map (\entry -> entry.error == Nothing && entry.proven && entry.dirty == entry.settled) |> Maybe.withDefault True
    in
    hintCurrent && pageCurrent

taskCanOpen : String -> Model -> String -> Bool
taskCanOpen workspace model task = model.sessions.workspace == Just workspace && taskAssociation model task == Just True


viewTaskIcon : String -> Model -> String -> Html Msg
viewTaskIcon workspace model task =
    let
        hint = Dict.get task model.sessions.taskHints
        active = taskActive workspace model task
        association = taskAssociation model task
        open = model.sessions.taskPanel == Just task
        activityCurrent = taskActivityCurrent workspace model task
        inactive = association == Just True && activityCurrent && (hint |> Maybe.map (\current -> not current.error && not current.active && current.epoch == model.sessionDelivery.listInvalidation) |> Maybe.withDefault False)
        empty = association == Just False && not open
        label = if open then "Hide Agents" else if empty then "No sessions" else "View Agents"
        status = if empty then " · no sessions associated with this task" else if active then " · active sessions" ++ (if association == Nothing || not activityCurrent then " · refreshing" else "") else if inactive then " · no active sessions" else if (hint |> Maybe.map .error |> Maybe.withDefault False) || (Dict.get task model.sessions.taskAssociations |> Maybe.map .error |> Maybe.withDefault False) then " · associations or activity unavailable" else " · checking associations and activity"
    in
    button [class ("btn btn-secondary btn-sm task-sessions-icon" ++ (if active && not empty then " has-active-sessions" else "") ++ (if open then " selected" else "") ++ (if empty then " no-associated-sessions" else "")),disabled (not open && not (taskCanOpen workspace model task)),attribute "aria-label" label,attribute "aria-expanded" (if open then "true" else "false"),Html.Attributes.title (label ++ status),stopPropagationOn "click" (Decode.succeed (SessionsToggleDock task,True)),stopPropagationOn "dblclick" (Decode.succeed (NoOp,True))]
        ([if empty then text "" else if active then SessionActivityLight.view True status else if inactive then span [class "session-state-dot session-state-inactive",attribute "aria-hidden" "true"] [] else span [class "session-association-pending",attribute "aria-hidden" "true"] [text "…"]]
            ++ (if empty then [text label] else if open then [CardSections.horizontalDisclosureIcon True,text label] else [text label,CardSections.horizontalDisclosureIcon False]))


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
        fresh = freshTaskPage False workspace model task /= Nothing && taskActivityCurrent workspace model task
        sessions = page |> Maybe.map (.items >> List.filter (effectiveActive workspace model)) |> Maybe.withDefault []
        error = resource workspace (sectionTarget False model task) model |> Maybe.andThen .error
        failed = error /= Nothing || (Dict.get (sectionKey False task) model.sessions.taskPages |> Maybe.map (\cached -> case cached.result of
            Err _ -> True
            Ok _ -> False) |> Maybe.withDefault False)
        key = sectionKey False task
    in
    section [class "task-session-work-context task-session-context-choices",attribute "aria-label" "Task active agent hierarchy",attribute "aria-busy" (if fresh then "false" else "true"),stopPropagationOn "click" (Decode.succeed (NoOp,True)),stopPropagationOn "dblclick" (Decode.succeed (NoOp,True))] [h4 [class "session-pane-label"] [text "Active agents"],div [class "task-session-picker"] [sessionTree (\session -> compactSessionCard model session "task-session-choice" (button [class "session-summary-link",disabled (not fresh),attribute "aria-pressed" (if model.sessions.selected == Just session.id then "true" else "false"),onClick (SessionsSelectDock task session.id)]) []) sessions]
        ,if page == Nothing || failed || (List.isEmpty sessions && not fresh) then p [attribute "role" (if failed then "alert" else "status")] [text (if failed then "Active sessions could not be loaded." else "Loading active sessions…"),if failed then button [class "btn btn-secondary btn-sm",onClick SessionsRetry] [text "Retry"] else text ""] else if List.isEmpty sessions then p [class "card-meta"] [text "No active sessions for this task."] else text ""
        ,div [class "sessions-pagination"] [if taskCursor model key /= Nothing then button [class "btn btn-sm",onClick (SessionsTaskPreviousPage key)] [text "Newer sessions"] else text "",page |> Maybe.andThen .nextCursor |> Maybe.map (\cursor -> button [class "btn btn-sm",onClick (SessionsTaskNextPage key cursor)] [text "Older sessions"]) |> Maybe.withDefault (text "")]
            ]


viewTaskActiveWithChoices : Bool -> String -> Model -> String -> Html Msg
viewTaskActiveWithChoices includeChoices workspace model task =
    if model.sessions.taskPanel /= Just task then text "" else
    let
        page = taskPage False workspace model task
        fresh = freshTaskPage False workspace model task /= Nothing && taskActivityCurrent workspace model task
        sessions = page |> Maybe.map (.items >> List.filter (effectiveActive workspace model)) |> Maybe.withDefault []
        error = resource workspace (sectionTarget False model task) model |> Maybe.andThen .error
        failed = error /= Nothing || (Dict.get (sectionKey False task) model.sessions.taskPages |> Maybe.map (\cached -> case cached.result of
            Err _ -> True
            Ok _ -> False) |> Maybe.withDefault False)
        key = sectionKey False task
    in
    aside [class "task-session-dock",attribute "aria-label" "Task active agents",attribute "aria-busy" (if fresh then "false" else "true"),stopPropagationOn "click" (Decode.succeed (NoOp,True)),stopPropagationOn "dblclick" (Decode.succeed (NoOp,True))]
        [div [class "task-session-context-grid"]
            [model.sessions.selected |> Maybe.map (\id -> div [class "task-session-transcript"]
                [a [class "btn btn-sm task-session-open-full",href (sessionUrl workspace id ++ "&task=" ++ Url.percentEncode task)] [text "Open full session ↗"]
                ,resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .data |> Maybe.map (\payload -> case payload of
                    Delivery.Metadata session -> div [class "task-session-selected-context"] [h4 [] [text (sessionTitle session)],viewAncestorPath session]
                    _ -> text "") |> Maybe.withDefault (text "")
                ,viewRecords workspace model id]) |> Maybe.withDefault (div [class "empty-state"] [text "Select an active session to read its recent conversation."])
            ,if includeChoices then viewTaskActiveChoices workspace model task else text ""
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
    section [class ("task-session-section " ++ (if evidence then "task-session-evidence" else "task-session-active")),attribute "aria-label" heading]
        [h4 [class "card-section-heading"] [text (heading ++ (if page /= Nothing then " · " ++ String.fromInt (List.length sessions) else ""))]
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
    if session |> Maybe.map (.state >> (==) Api.SessionTerminated) |> Maybe.withDefault False then viewSealedRecords model session
    else section [class "session-viewer",attribute "aria-label" "Session transcript"]
        [div [class "sessions-toolbar"]
            [session |> Maybe.map viewSessionIdentity |> Maybe.withDefault (text "")]
        ,if state.error /= Nothing || rangeError /= Nothing then p [attribute "role" "alert"] [text (Maybe.withDefault "Transcript request failed." state.error),button [onClick SessionsRetry] [text "Retry"]] else text ""
        ,Keyed.node "div" [Html.Attributes.id "session-records",class "session-records",tabindex 0,attribute "data-viewer-owner" state.stamp,attribute "aria-label" "Bounded transcript records"]
            (List.map (\item -> let (key,node) = viewTranscriptItem model item in (state.stamp ++ ":" ++ key,node)) (Transcript.project state.rows))
        ,if List.isEmpty state.rows then p [attribute "role" "status"] [text (if state.start == Nothing then "Loading transcript metadata…" else if state.offset < state.end then "Reading transcript…" else if newest == 0 then "This session has no committed records." else "No complete messages in this window.")] else text ""
        ,text "" ]


viewSealedRecords : Model -> Maybe Api.AgentSession -> Html Msg
viewSealedRecords model session =
    let
        state=model.sessions
        sealed=state.sealed
        action name label = button [attribute "data-sealed-action" name,attribute "data-sealed-owner" state.stamp,disabled (sealed.phase /= "ready" && name /= "cancel")] [text label]
        openAttribute opened = if opened then [attribute "open" ""] else []
        body piece = case piece.body of
            Nothing -> Transcript.atom 0 (\_ -> text "")
            Just content ->
                let
                    complete=sealed.markdown.key == piece.key && sealed.markdown.complete
                    partial=content.format == "markdown" && (content.more || content.part>0)
                    source=if complete then sealed.markdown.source else content.text
                    renderedBody=if partial && not complete then Transcript.atom (1024+12*String.length state.stamp+12*String.length piece.key) (\_ -> button [attribute "data-sealed-action" "markdown",attribute "data-sealed-owner" state.stamp,attribute "data-sealed-key" piece.key] [text "Read complete message"])
                        else if complete then Transcript.formattedAst sessionMarkdownRenderer source sealed.markdown.ast
                        else if content.format == "markdown" then Transcript.formattedAst sessionMarkdownRenderer source piece.ast
                        else sealedBlockFragment (if content.format == "json" then "tool" else piece.kind) source content.tokens
                in Transcript.container 128 (div [])
                [renderedBody
                ,Transcript.atom (2048+12*String.length state.stamp+12*String.length piece.key) (\_ -> div []
                    [if content.format == "json" && content.part > 0 then button [attribute "data-sealed-action" "body",attribute "data-sealed-owner" state.stamp,attribute "data-sealed-key" piece.key,attribute "data-sealed-part" (String.fromInt (content.part-1))] [text "Previous body part"] else text ""
                    ,if content.format == "json" && content.more then button [attribute "data-sealed-action" "body",attribute "data-sealed-owner" state.stamp,attribute "data-sealed-key" piece.key,attribute "data-sealed-part" (String.fromInt (content.part+1))] [text "Continue body"] else text ""])]
        pieceView piece =
            Transcript.container (4096+24*String.length state.stamp+24*String.length piece.key+12*String.length piece.time+12*String.length piece.label+6*String.length piece.offset+6*String.length piece.role)
                (\children -> article [class ("session-record session-role-" ++ piece.role),attribute "data-offset" piece.offset,attribute "data-block-key" piece.key]
                    [div [class "session-record-meta"] [time [attribute "datetime" piece.time] [text (compactTimestamp model piece.time)],span [] [text (if piece.role == "user" then "You" else if piece.role == "assistant" then "Assistant" else if piece.role == "tool" then "Tool" else "Message")],if piece.body |> Maybe.map .exception |> Maybe.withDefault False then span [class "session-structured-json"] [text "Large structured reasoning · exact JSON"] else text ""]
                    ,if piece.kind == "text" then div [] children
                     else if piece.kind == "encrypted" then em [] [text "Thinking..."]
                     else div [] [button [class "btn btn-sm session-inspect-event",attribute "data-sealed-action" "inspect",attribute "data-sealed-key" piece.key,attribute "data-sealed-owner" state.stamp] [text "Inspect ↗"]
                        ,details ([class ("session-content-" ++ piece.kind),attribute "data-event-key" piece.key,attribute "data-sealed-key" piece.key,attribute "data-sealed-owner" state.stamp] ++ openAttribute piece.open)
                            (summary [] [text (if piece.label == "" then "Thinking" else piece.label)]::children)]]) [body piece]
        runs = List.foldr (\piece accumulated -> case accumulated of
            run::rest -> if piece.group /= "" && (List.head run |> Maybe.map (.group >> (==) piece.group) |> Maybe.withDefault False) then (piece::run)::rest else [piece]::accumulated
            [] -> [[piece]]) [] sealed.rows
        runView run = case run of
            [] -> Transcript.atom 0 (\_ -> text "")
            first::_ ->
                if first.group == "" then pieceView first
                else Transcript.container (2048+12*String.length first.group+12*String.length state.stamp)
                    (\children -> details ([class "session-activity-group",attribute "data-group-key" first.group,attribute "data-sealed-key" ("group:" ++ first.group),attribute "data-sealed-owner" state.stamp] ++ openAttribute first.groupOpen)
                        [summary [] [text ("Activity · " ++ first.count ++ " events · " ++ String.fromInt (List.length run) ++ " on this page")]
                        ,div [class "session-activity-records"] children]) (List.map pieceView run)
        rendered = Transcript.renderPage (if sealed.markdown.complete then (if sealed.markdown.structured then 524288 else 1048576) else 2097152) sealed.renderPart (List.map runView runs)
        message = case sealed.phase of
            "ready" -> if List.isEmpty sealed.rows then "No matching messages on this page." else "Verified complete archive."
            "cancelled" -> "Archive loading cancelled."
            "downloading" -> "Downloading complete archive… " ++ sealed.compressedBytes ++ " bytes"
            "decoding" -> "Verifying and indexing… " ++ sealed.rawBytes ++ " bytes"
            "navigating" -> "Reading local index…"
            "reading-body" -> "Reading the complete formatted message…"
            "error" -> Maybe.withDefault "Archive could not load." sealed.error
            _ -> "Preparing complete archive…"
    in
    Keyed.node "section" [class "session-viewer session-sealed-viewer",attribute "aria-label" "Session transcript"]
        (sealedHostNodes model [session |> Maybe.map viewSessionIdentity |> Maybe.withDefault (text "")
        ,p [attribute "role" (if sealed.error == Nothing then "status" else "alert"),attribute "aria-live" "polite"] [text message]
        ,div [class "sessions-toolbar"] [action "first" "First",action "previous" "Previous",action "next" "Next",action "last" "Last",action "cancel" "Cancel",button [onClick SessionsRetry] [text "Retry"]]
        ,div [class "sessions-toolbar"] [input [Html.Attributes.id "sealed-search",Html.Attributes.placeholder "Search complete archive",attribute "maxlength" "256"] [],action "search" "Search",action "search-next" "Continue search"]
        ,div [class "sessions-toolbar"] [input [Html.Attributes.id "sealed-position",Html.Attributes.placeholder "Original byte offset",attribute "inputmode" "numeric"] [],action "position" "Go to offset"]
        ,sealedRenderControls model False rendered.part rendered.more
        ,if rendered.error then p [attribute "role" "alert"] [text "This formatted piece exceeds the local presentation budget."] else text ""
        ,div [Html.Attributes.id "session-records",class "session-records",tabindex 0,attribute "data-viewer-owner" state.stamp,attribute "data-sealed-dom-owner" state.stamp,attribute "data-sealed-dom-load" (String.fromInt state.generation),attribute "aria-label" "Verified archive records"] rendered.nodes])


sealedHostKey : Model -> String
sealedHostKey model =
    Encode.encode 0 (Encode.list Encode.string [model.sessions.stamp,String.fromInt model.sessions.generation,model.sessions.sealed.domEpoch])


sealedHostNodes : Model -> List (Html Msg) -> List (String,Html Msg)
sealedHostNodes model =
    List.indexedMap (\index node -> (sealedHostKey model ++ ":" ++ String.fromInt index,node))


sealedBlockFragment : String -> String -> List (String,String) -> Transcript.Fragment Msg
sealedBlockFragment kind content tokens =
    if kind == "text" || kind == "thinking" then Transcript.formatted sessionMarkdownRenderer content
    else Transcript.atom (1024+6*String.length content+40*List.length tokens) (\_ -> pre [class "session-tool-content"]
        [code [class "language-json"] (if List.isEmpty tokens then [text content] else List.map (\(tokenKind,value) -> span [class ("json-" ++ tokenKind)] [text value]) tokens)])


sealedRenderControls : Model -> Bool -> Int -> Bool -> Html Msg
sealedRenderControls model inspector part more =
    let
        control next label=button [attribute "data-sealed-action" "render",attribute "data-sealed-owner" model.sessions.stamp,attribute "data-sealed-part" (String.fromInt next),attribute "data-sealed-inspector" (if inspector then "true" else "false")] [text label]
    in div [class "sessions-toolbar session-render-continuation",attribute "data-render-part" (String.fromInt part),attribute "data-render-inspector" (if inspector then "true" else "false")]
        [if part>0 then control (part-1) "Previous presentation" else text "",if more then control (part+1) "Continue presentation" else text ""]


viewSealedInspector : Model -> Transcript.Block -> Html Msg
viewSealedInspector model block =
    let
        selected=model.sessions.inspectedEvent |> Maybe.andThen (\targetKey -> List.filter (.key >> (==) targetKey) model.sessions.sealed.rows |> List.head)
        content=selected |> Maybe.andThen .body
        key=Maybe.withDefault "" model.sessions.inspectedEvent
        complete=model.sessions.sealed.markdown.key == key && model.sessions.sealed.markdown.complete
        partial=content |> Maybe.map (\body -> body.format == "markdown" && (body.more || body.part>0)) |> Maybe.withDefault False
        source=if complete then model.sessions.sealed.markdown.source else block.content
        kind=if content |> Maybe.map (.format >> (==) "json") |> Maybe.withDefault False then "tool" else block.kind
        fragment=if complete then Transcript.formattedAst sessionMarkdownRenderer source model.sessions.sealed.markdown.ast
            else if kind == "text" || kind == "thinking" then Transcript.formattedAst sessionMarkdownRenderer source (selected |> Maybe.map .ast |> Maybe.withDefault (Err "not_loaded"))
            else sealedBlockFragment kind source block.tokens
        rendered=Transcript.renderPage 262144 model.sessions.sealed.inspectorPart [fragment]
    in
    if partial && not complete then button [attribute "data-sealed-action" "markdown",attribute "data-sealed-owner" model.sessions.stamp,attribute "data-sealed-key" key] [text "Read complete message"]
    else div [] (rendered.nodes ++ [sealedRenderControls model True rendered.part rendered.more])



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


