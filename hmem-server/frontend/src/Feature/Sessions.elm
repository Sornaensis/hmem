module Feature.Sessions exposing (reconcile, update, view, viewTaskPanel, viewTaskActive, viewTaskActiveChoices, viewTaskActiveConversation, viewTaskEvidence, viewTaskIcon, viewWorkspaceInspector, taskActive, taskEvidenceAvailable, timestampSources, olderWindow, viewerReset)

import Api
import ActorAttribution
import Browser.Navigation as Nav
import CardSections
import SessionActivityLight
import SessionTranscript as Transcript
import SessionWindow
import Dict
import Char
import Feature.SessionDelivery as Delivery
import Feature.WebSocket as WebSocket
import Feature.DataLoading as DataLoading
import Feature.Dependencies as Dependencies
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
    if kind == "projects" then Delivery.SessionFacets kind Nothing state.projectFacetCursor state.projectFilter state.filters.activity
    else Delivery.SessionFacets kind state.projectFilter state.taskFacetCursor state.taskFilter state.filters.activity


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
        carryPresentation = old.viewerScope == viewerScope && selected /= Nothing && old.start /= Nothing && ((old.taskPanel /= Nothing && model.activeTab == SessionsTab) || (old.taskPanel == Nothing && taskPanel /= Nothing && old.returnTaskPanel == taskPanel))
        continueViewer = carryPresentation && not old.resetPending && not old.pending
        returnTaskPanel = if old.workspace /= workspace || old.epoch /= model.sessionRequestEpoch || old.selected /= selected then Nothing else if model.activeTab == SessionsTab && old.taskPanel /= Nothing then old.taskPanel else if model.activeTab == SessionsTab then old.returnTaskPanel else Nothing
        downloadStamp = Encode.encode 0 (Encode.list Encode.string [Maybe.withDefault "" workspace, String.fromInt model.sessionRequestEpoch, model.sessionContext |> Maybe.map (.principal >> .actorId) |> Maybe.withDefault "", String.fromInt model.sessionDelivery.retirement, if model.activeTab == SessionsTab then Maybe.withDefault "" selected else ""])
        downloadChanged = old.downloadStamp /= downloadStamp
        taskFilter = if workspace /= Nothing && model.activeTab == SessionsTab then urlId "task" model else Nothing
        projectFilter = if workspace /= Nothing && model.activeTab == SessionsTab then urlId "project" model else Nothing
        taskIds = if workspace /= Nothing && model.activeTab == ProjectsTab then mountedTasks model else []
        reset = old.workspace /= workspace || old.epoch /= model.sessionRequestEpoch
        scoped = if reset then { old | workspace = workspace, epoch = model.sessionRequestEpoch, selected = selected, cursor = Nothing, previous = [], linksCursor = Nothing, filters = Api.workspaceSessionFilters, taskFilter = taskFilter, projectFilter = projectFilter, projectFacetCursor = Nothing, taskFacetCursor = Nothing, activityDirty = False, taskCursors = Dict.empty, taskPreviousPages = Dict.empty, taskPages = Dict.empty, taskHints = Dict.empty, taskAssociations = Dict.empty, hintOrder = [], sectionOrder = [] }
            else if old.selected /= selected then { old | selected = selected, linksCursor = Nothing } else old
        filtered = if scoped.taskFilter /= taskFilter || scoped.projectFilter /= projectFilter then { scoped | taskFilter = taskFilter, projectFilter = projectFilter, taskFacetCursor = Nothing, cursor = Nothing, previous = [] } else scoped
        hintEpoch = model.sessionDelivery.listInvalidation
        retainedHints = Dict.filter (\id _ -> List.member id taskIds) filtered.taskHints
        consumeHint (ws,target) accumulated = case target of
            Delivery.TaskSessionsFiltered task filters cursor ->
                if cursor /= Nothing || filters.activity /= Nothing || filters.state /= "active" || not (List.member task taskIds) then accumulated else
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
                if Just ws /= workspace || cursor /= Nothing || filters.activity /= Nothing || (filters.state /= "" && filters.state /= "active") || filters.freshness /= "" || not (List.member task taskIds) then accumulated else
                case resource ws target model of
                    Just entry ->
                        if entry.error /= Nothing then
                            if filters.state == "" then Dict.insert task {present=False,error=True,epoch=hintEpoch} accumulated else accumulated
                        else if not entry.proven || entry.dirty /= entry.settled then accumulated
                        else case entry.data of
                            Just (Delivery.SessionsPage page) ->
                                if not (List.isEmpty page.items) then Dict.insert task {present=True,error=False,epoch=hintEpoch} accumulated
                                else if filters.state == "" then Dict.insert task {present=False,error=page.nextCursor /= Nothing,epoch=hintEpoch} accumulated
                                else accumulated
                            _ -> accumulated
                    Nothing -> accumulated
            _ -> accumulated
        associations = if reset then Dict.empty else List.foldl consumeAssociation retainedAssociations old.owned
        needsAssociation id =
            (Dict.get id hints |> Maybe.map (\hint -> hint.epoch == hintEpoch && not hint.error && not hint.active) |> Maybe.withDefault False)
                && (Dict.get id associations |> Maybe.map (\hint -> hint.epoch /= hintEpoch) |> Maybe.withDefault True)
        hintTarget id =
            if unresolved id then Just (Delivery.TaskSessionsFiltered id {state="active",freshness="",activity=Nothing} Nothing)
            else if needsAssociation id then Just (Delivery.TaskSessionsFiltered id {state="",freshness="",activity=Nothing} Nothing)
            else Nothing
        pendingHintTargets = old.owned |> List.filterMap (\(ws,target) -> case target of
            Delivery.TaskSessionsFiltered task filters cursor ->
                if Just ws == workspace && cursor == Nothing && filters.activity == Nothing && (filters.state == "active" || filters.state == "") && filters.freshness == "" && List.member task taskIds && (resource ws target model |> Maybe.map (\entry -> entry.error == Nothing && entry.dirty /= entry.settled) |> Maybe.withDefault False) then Just target else Nothing
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
                    else if not entry.proven || entry.dirty /= entry.settled then accumulatedPages else
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
                    { withDownloads | stamp = stamp, generation = old.generation + 1, resetPending = False, pending = False, positionPending = False }
                else if carryPresentation then
                    -- Accepted display rows keep their exact source authority.
                    -- Replay the captured window with a fresh framer only after
                    -- its reading point has moved to the new surface.
                    { withDownloads | stamp=stamp,generation=old.generation+1,resetPending=True,pending=False,positionPending=True,offset=Maybe.withDefault old.offset old.start }
                else
                    { withDownloads | stamp = stamp, generation = old.generation + 1, resetPending = True, start = Nothing, offset = 0, end = 0, following = True, loadWindow = True, windowDirection = "tail", windowAligned = False, historyBefore = 0, historyAfter = 0, historyScanBefore = 0, historyScanAfter = 0, historyScanEnd = 0, historyTop = 0, historyBottom = 0, historyTopCount = 0, historyBottomCount = 0, historyTopGroup = "", historyBottomGroup = "", historyAnchor = "", historyHeights = Dict.empty, historyGaps = [], historyActivity = Dict.empty, historyClosed = Set.empty, historyOpen = Set.empty, windowPublication = 0, positionPending = False, presentationPart = -1, presentationAnchor = "", windowPurpose = "latest", manualUp = False, latestPending = False, latestError = Nothing, pending = False, rows = [], notice = "", error = Nothing, heartbeats = Dict.empty, sealed = emptySealedSessionView }
            else if transportChanged then
                -- Retire response authority, not the presentation. Preserve the
                -- finite end so a framed but unacknowledged short tail retries
                -- exactly before newly committed bytes extend the next window.
                { withDownloads | generation = old.generation + 1, pending = False, positionPending = False }
            else
                withDownloads
        metadata = Maybe.map2 (\ws id -> resource ws (Delivery.SessionMetadata id) model |> Maybe.andThen (\entry -> if entry.error == Nothing && entry.proven then entry.data else Nothing) |> Maybe.andThen (\value -> case value of
            Delivery.Metadata session -> Just session
            _ -> Nothing)) workspace selected |> Maybe.withDefault Nothing
        beforeArchive = case metadata of
            Just session ->
                if session.state == Api.SessionTerminated then
                    { retired | start = Just 0, offset = 0, end = 0, resetPending = False, pending = False, positionPending = False, rows = [] }
                else if retired.start == Nothing then
                    let begin = max 0 (session.committedBytes - 65536) in
                    { retired | start = Just begin, offset = begin, end = session.committedBytes, historyBefore = begin, historyAfter = begin, windowDirection = "tail", windowAligned = False, generation = retired.generation + 1 }
                else if retired.following && not retired.pending && not retired.positionPending && (not retired.loadWindow || retired.offset >= retired.end) && retired.historyAfter < session.committedBytes then
                    let begin = List.drop (max 0 (List.length retired.rows-20)) retired.rows |> List.filter (\row -> retired.historyAfter-row.offset <= 65536) |> List.head |> Maybe.map .offset |> Maybe.withDefault (List.reverse retired.rows |> List.head |> Maybe.map .offset |> Maybe.withDefault (min retired.historyAfter retired.historyScanAfter)) in
                    { retired | start = Just begin, offset = begin, end = min session.committedBytes (begin + 2097152), generation = retired.generation + 1, resetPending = True, windowDirection = "forward", windowAligned = True, windowPurpose = "append", loadWindow = True }
                else retired
            Nothing -> retired
        current = beforeArchive
        isSealed = metadata |> Maybe.map (.state >> (==) Api.SessionTerminated) |> Maybe.withDefault False
        window = Maybe.map2 (\ws session -> (ws,Delivery.SessionWindow session.id current.offset current.end (session.state == Api.SessionTerminated))) workspace metadata
            |> Maybe.andThen (\target -> if not isSealed && not current.positionPending && current.loadWindow && current.error == Nothing && current.offset < current.end then Just target else Nothing)
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
            , ("preservePresentation",Encode.bool (carryPresentation && old.stamp /= stamp))
            , ("viewerScope",Encode.string viewerScope)
            , ("downloadStamp",Encode.string downloadStamp)
            , ("generation",Encode.int current.generation)
            , ("reset",viewerReset current |> Maybe.map Encode.int |> Maybe.withDefault Encode.null)
            , ("follow",Encode.bool current.following)
            , ("viewerError",Encode.bool (current.error/=Nothing))
            , ("direction",Encode.string current.windowDirection)
            , ("purpose",Encode.string current.windowPurpose)
            , ("knownBefore",Encode.int (if current.windowPurpose == "latest" then 0 else current.historyScanBefore))
            , ("knownAfter",Encode.int (if current.windowPurpose == "latest" then 0 else current.historyScanAfter))
            , ("runAnchor",Encode.string (if current.windowPurpose=="gap" then current.historyGaps |> List.filter (\gap -> gap.group/="" && (String.toInt gap.before |> Maybe.map (\begin -> begin <= Maybe.withDefault 0 current.start) |> Maybe.withDefault False) && (String.toInt gap.after |> Maybe.map (\finish -> finish > Maybe.withDefault 0 current.start) |> Maybe.withDefault False)) |> List.head |> Maybe.map .group |> Maybe.withDefault "" else ""))
            , ("runRanges",Encode.list (\gap -> Encode.object [("before",Encode.string gap.before),("after",Encode.string gap.after),("group",Encode.string gap.group)]) (if current.windowPurpose=="latest" then [] else List.filter (\gap -> gap.group/="") current.historyGaps |> List.take 201))
            , ("aligned",Encode.bool current.windowAligned)
            , ("windowEnd",Encode.int current.end)
            , ("latest",Encode.object [("request",Encode.int current.latestRequest),("pending",Encode.bool current.latestPending)])
            , ("publication",Encode.object [("request",Encode.int current.windowPublication),("pending",Encode.bool current.positionPending)])
            , ("historical",Encode.bool (not current.following && current.resetPending))
            , ("chunk",chunk |> Maybe.map (\range -> Encode.object [("offset",Encode.int range.offset),("nextOffset",Encode.int range.nextOffset),("dataBase64",Encode.string range.dataBase64),("final",Encode.bool (range.nextOffset >= current.end))]) |> Maybe.withDefault Encode.null)
            , ("nextOperation",Encode.int current.nextOperation)
            , ("saveOperation",(pending.save |> Maybe.map .operation |> Maybe.withDefault (pending.archive |> Maybe.map .operation |> Maybe.withDefault -1)) |> Encode.int)
            , ("archiveRequest",archiveRequest next)
            , ("sealed",Encode.object
                [("active",Encode.bool (isSealed && selected /= Nothing)),("stamp",Encode.string stamp),("load",Encode.int current.generation)
                ,("scope",Encode.string viewerScope),("historical",Encode.bool current.manualUp),("anchor",Encode.string current.historyAnchor)
                ,("latestRequest",Encode.int (if current.latestPending then current.latestRequest else -1))
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
    let cards = next.cards
        sections = List.foldl (\task currentSections -> if Dict.get task currentSections == Just "evidence" && taskEvidenceAvailable (Maybe.withDefault "" workspace) next task == Just False then Dict.insert task "work" currentSections else currentSections) cards.taskSections expandedTasks
    in
    ( { next | cards = { cards | taskSections = sections } }, Cmd.batch [ command, requests, bridge, retireInvalidTask ] )


viewerReset : SessionsModel -> Maybe Int
viewerReset state =
    if state.resetPending then state.start else Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let state = model.sessions in
    case msg of
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
        SessionsRetry ->
            let
                decodeRecovery = state.error /= Nothing
                begin = max 0 (state.end - 2097152)
                refresh ws delivery =
                    Delivery.invalidateFacets ws (Delivery.invalidateLists ws (Delivery.retrySelected delivery))
                        |> (\current -> state.selected |> Maybe.map (\id -> Delivery.invalidateSession ws id current) |> Maybe.withDefault current)
                        |> (\current -> state.selected |> Maybe.map (\id -> Delivery.removeDemand ws (Delivery.SessionTasks id state.linksCursor) current) |> Maybe.withDefault current)
                        |> (\current -> state.taskPanel |> Maybe.map (\id -> Delivery.invalidateTask ws id current) |> Maybe.withDefault current)
                        |> (\current -> List.foldl (\(ownedWorkspace,target) accumulated -> case target of
                            Delivery.SessionWindow _ _ _ _ -> Delivery.removeDemand ownedWorkspace target accumulated
                            _ -> accumulated) current state.owned)
            in
            ( {model | sessions = {state | error = Nothing,pending = False,positionPending=False,loadWindow = True, sealed = emptySealedSessionView
                    , start = if decodeRecovery then Just begin else state.start
                    , offset = if decodeRecovery then begin else state.offset
                    , generation = state.generation + 1,resetPending = decodeRecovery || state.resetPending}
                ,sessionDelivery = state.workspace |> Maybe.map (\ws -> refresh ws model.sessionDelivery) |> Maybe.withDefault model.sessionDelivery}
            ,Ports.sessionBridgeCommand (Encode.object [("stamp",Encode.string state.stamp),("viewerScope",Encode.string state.viewerScope),("generation",Encode.int (state.generation+1)),("disclosureRetry",Encode.bool True)]) )
        SessionsActivityFilter value ->
            if List.member value ["live","quiet","completed"] then
                let
                    selected = state.filters.activity |> Maybe.withDefault "live" |> String.split ","
                    toggled = if List.member value selected then List.filter ((/=) value) selected else value :: selected
                    canonical = ["live","quiet","completed"] |> List.filter (\item -> List.member item toggled) |> String.join ","
                in
                ({model | sessions = {state | filters = {state="",freshness="",activity=Just canonical},cursor=Nothing,previous=[],projectFacetCursor=Nothing,taskFacetCursor=Nothing}},Cmd.none)
            else (model,Cmd.none)
        SessionsStateFilter value ->
            if value == "active" then
                let filters = state.filters in ( { model | sessions = { state | filters = { filters | state = "active",activity=Nothing }, cursor = Nothing, previous = [] } }, Cmd.none )
            else (model,Cmd.none)
        SessionsFreshnessFilter value ->
            if List.member value ["fresh","stale"] then
                let filters = state.filters in ( { model | sessions = { state | filters = { filters | freshness = value,activity=Nothing }, cursor = Nothing, previous = [] } }, Cmd.none )
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
            let
                first = List.head state.rows |> Maybe.map .offset |> Maybe.withDefault state.historyBefore
                scanning = state.windowPurpose == "fill" && state.historyScanBefore < first
                boundary = if scanning then state.historyScanBefore else first
                end = if scanning then state.historyScanEnd else List.take 20 state.rows |> List.filter (\row -> row.end - first <= 65536) |> List.reverse |> List.head |> Maybe.map .end |> Maybe.withDefault first
                -- Reserve two older chunks separately from contextual overlap;
                -- larger leading fragments still use bounded lookback recovery.
                begin = max 0 (boundary - 131072)
            in
            ( { model | sessions = { state | loadWindow = True, latestPending=False,latestError=Nothing,windowPurpose=if state.windowPurpose == "fill" then "fill" else "older",windowDirection = "tail", windowAligned = False, start = Just begin, offset = begin, end = end, pending = False, generation = state.generation + 1, resetPending = True, error = Nothing } }, Cmd.none )
        SessionsNewer ->
            let
                last = List.reverse state.rows |> List.head
                begin = List.drop (max 0 (List.length state.rows - 20)) state.rows |> List.filter (\row -> state.historyAfter - row.offset <= 65536) |> List.head |> Maybe.map .offset |> Maybe.withDefault (last |> Maybe.map .end |> Maybe.withDefault state.historyAfter)
                newest = state.selected |> Maybe.andThen (\id -> state.workspace |> Maybe.andThen (\ws -> resource ws (Delivery.SessionMetadata id) model |> Maybe.andThen .data)) |> Maybe.andThen (\payload -> case payload of
                    Delivery.Metadata session -> Just session.committedBytes
                    _ -> Nothing) |> Maybe.withDefault state.end
            in
            ( { model | sessions = { state | loadWindow = True, latestPending=False,latestError=Nothing,windowPurpose="newer",windowDirection = "forward", windowAligned = True, start = Just begin, offset = begin, end = min newest (begin + 2097152), pending = False, generation = state.generation + 1, resetPending = True, error = Nothing } }, Cmd.none )
        SessionsJumpLatest ->
            let
                sealed = state.selected |> Maybe.andThen (\id -> state.workspace |> Maybe.andThen (\ws -> resource ws (Delivery.SessionMetadata id) model |> Maybe.andThen .data)) |> Maybe.map (\payload -> case payload of
                    Delivery.Metadata session -> session.state == Api.SessionTerminated
                    _ -> False) |> Maybe.withDefault False
                next = {state | latestRequest=state.latestRequest+1,latestPending=True,latestError=Nothing,positionPending=False,presentationPart=-1,presentationAnchor=""}
                delivery = if sealed then model.sessionDelivery else Maybe.map2 (\ws id -> Delivery.invalidateSession ws id model.sessionDelivery) state.workspace state.selected |> Maybe.withDefault model.sessionDelivery
                restartSealed = sealed && List.isEmpty state.sealed.rows && List.member state.sealed.phase ["error","cancelled"]
                sealedBlocked = sealed && state.sealed.phase == "error" && not state.sealed.canRetry
            in
            ( {model | sessions=if sealedBlocked then {next | latestPending=False,latestError=state.sealed.error} else if restartSealed then {next | generation=state.generation+1,sealed=emptySealedSessionView} else if sealed then next else {next | following=True,loadWindow=True,windowPurpose="latest",windowDirection="tail",windowAligned=False,start=Nothing,pending=False,generation=state.generation+1,resetPending=True,error=Nothing},sessionDelivery=delivery},Cmd.none )
        SessionsTick time ->
            let now = toFloat (Time.posixToMillis time)
                refresh = (state.filters.freshness /= "" || state.filters.activity /= Nothing) && model.activeTab == SessionsTab && now-state.lastFilterRefresh >= 10000
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


recordSource : SessionRecord -> String
recordSource row =
    String.padLeft 20 '0' (String.fromInt row.offset)


recordGroup : SessionRecord -> String
recordGroup row =
    case Transcript.project [row] of
        [Transcript.Activity pieces] -> List.head pieces |> Maybe.andThen .activityAnchor |> Maybe.withDefault ""
        _ -> ""


composeRecords : Decode.Value -> Int -> Int -> Int -> List SessionRecord -> SessionsModel -> SessionsModel
composeRecords packet before through completeThrough incoming state =
    let
        field name decoder fallback = Decode.decodeValue (Decode.field name decoder) packet |> Result.withDefault fallback
        seeking = state.windowPurpose == "latest"
        allPrevious = if seeking then [] else state.rows
        compact row (kept,dropped,seen) =
            let group=recordGroup row in
            if List.member state.windowPurpose ["fill","gap"] && state.following && group/="" && Set.member group state.historyClosed then
                if Set.member group seen then (kept,row::dropped,seen) else (row::kept,dropped,Set.insert group seen)
            else (row::kept,dropped,seen)
        (previous,compacted,_) = List.foldl compact ([],[],Set.empty) (List.reverse allPrevious)
        policy = { identity = .presentationOffset >> String.fromInt, source = recordSource, bytes = \row -> max 0 (row.end-row.offset) }
        anchor = if state.historyAnchor == "" then Nothing else Just state.historyAnchor
        result = SessionWindow.composeRange (Just (String.padLeft 20 '0' (String.fromInt before),String.padLeft 20 '0' (String.fromInt completeThrough))) policy 200 4194304 anchor state.following (List.member state.windowPurpose ["older","fill"]) previous incoming
        measured row = Dict.get (recordSource row) state.historyHeights |> Maybe.withDefault 64 |> max 32
        extent = List.map measured >> List.sum
        oldFirst = List.head previous |> Maybe.map .offset |> Maybe.withDefault before
        oldLast = List.reverse previous |> List.head |> Maybe.map .end |> Maybe.withDefault through
        refetchedBefore = List.filter (\row -> row.offset >= state.historyScanBefore && row.offset < oldFirst) result.addedBefore
        refetchedAfter = List.filter (\row -> row.end <= state.historyScanAfter && row.end > oldLast) result.addedAfter
        topCount = max 0 ((if seeking then 0 else state.historyTopCount) - List.length refetchedBefore + List.length result.removedBefore)
        bottomCount = max 0 ((if seeking then 0 else state.historyBottomCount) - List.length refetchedAfter + List.length result.removedAfter)
        top = clamp 0 10000000 ((if seeking then 0 else state.historyTop) - extent refetchedBefore + extent result.removedBefore)
        bottom = clamp 0 10000000 ((if seeking then 0 else state.historyBottom) - extent refetchedAfter + extent result.removedAfter)
        edgeGroup removed edge oldGroup =
            let group = edge |> Maybe.map recordGroup |> Maybe.withDefault "" in
            if group /= "" && List.all (recordGroup >> (==) group) removed && (oldGroup == "" || oldGroup == group) then group else ""
        first = List.head result.rows
        last = List.reverse result.rows |> List.head
        newestRow = List.maximum (List.map .offset (previous ++ incoming)) |> Maybe.withDefault -1
        after = last |> Maybe.map (\row -> if row.offset == newestRow then max through (if seeking then 0 else state.historyAfter) else row.end) |> Maybe.withDefault through
        parsedBefore = field "parsedBefore" Decode.int before
        scanBefore = if seeking || state.historyScanAfter == 0 then parsedBefore else min state.historyScanBefore parsedBefore
        scanEnd = if seeking || state.historyScanAfter == 0 || parsedBefore < state.historyScanBefore then field "parsedOverlapEnd" Decode.int parsedBefore else state.historyScanEnd
        gapForRows rows = case (List.head rows,List.reverse rows |> List.head) of
            (Just head,Just tail) ->
                let group=recordGroup head in
                [{before=recordSource head,after=String.padLeft 20 '0' (String.fromInt tail.end),pixels=min 10000000 (extent rows),group=if group/="" && List.all (recordGroup >> (==) group) rows then group else ""}]
            _ -> []
        compactedRuns = List.foldl (\row runs -> Dict.update (recordGroup row) (\rows -> Just (Maybe.withDefault [] rows ++ [row])) runs) Dict.empty compacted |> Dict.values |> List.concatMap gapForRows
        prefixDiscard = field "discarded" (Decode.map4 (\start finish count group -> [{before=String.padLeft 20 '0' (String.fromInt start),after=String.padLeft 20 '0' (String.fromInt finish),pixels=min 10000000 (64*toFloat count),group=group}]) (Decode.field "before" Decode.int) (Decode.field "after" Decode.int) (Decode.field "count" Decode.int) (Decode.field "group" Decode.string)) []
        represented = Just (String.padLeft 20 '0' (String.fromInt before),String.padLeft 20 '0' (String.fromInt completeThrough))
        gaps = SessionWindow.reconcileGaps represented (List.map (\row -> (recordSource row,String.padLeft 20 '0' (String.fromInt row.end))) result.rows)
            (compactedRuns ++ gapForRows result.removedBefore ++ gapForRows result.removedAfter ++ prefixDiscard) (if seeking then [] else state.historyGaps)
        deltas = field "groups" (Decode.dict Decode.int) Dict.empty |> Dict.toList |> List.take 200
        activity = List.foldl (\(key,count) counts -> Dict.update key (\old -> Just (max 0 (Maybe.withDefault 0 old+count))) counts) (if seeking then Dict.empty else state.historyActivity) deltas
        keptGroups = Set.fromList (List.map .group gaps ++ List.map recordGroup result.rows)
        boundedActivity = Dict.filter (\key _ -> key/="" && Set.member key keptGroups) activity |> Dict.toList |> List.take 400 |> Dict.fromList
        presentationAnchor =
            if state.windowPurpose=="gap" && state.presentationAnchor=="" then
                let target=String.toInt state.historyAnchor |> Maybe.withDefault before
                    selected=List.filter (\row -> row.offset<=target) result.rows |> List.reverse |> List.head
                in selected |> Maybe.andThen (\row -> Transcript.project [row] |> List.head) |> Maybe.andThen (\item -> case item of
                    Transcript.Message piece -> Just (piece.key ++ ":")
                    Transcript.Activity pieces -> List.head pieces |> Maybe.map (\piece -> piece.key ++ ":")) |> Maybe.withDefault ""
            else state.presentationAnchor
    in
    { state | rows = result.rows
        , historyBefore = first |> Maybe.map .offset |> Maybe.withDefault before
        , historyAfter = after
        , historyScanBefore = scanBefore
        , historyScanAfter = max completeThrough (if seeking then 0 else state.historyScanAfter)
        , historyScanEnd = scanEnd
        , historyTop = if topCount == 0 then 0 else top
        , historyBottom = if bottomCount == 0 then 0 else bottom
        , historyTopCount = topCount, historyBottomCount = bottomCount
        , historyTopGroup = if topCount == 0 then "" else edgeGroup result.removedBefore first (if seeking then "" else state.historyTopGroup)
        , historyBottomGroup = if bottomCount == 0 then "" else edgeGroup result.removedAfter last (if seeking then "" else state.historyBottomGroup)
        , historyGaps = gaps, historyActivity = boundedActivity, presentationAnchor=presentationAnchor
        , windowPurpose = if seeking then "append" else state.windowPurpose
    }


olderWindow : SessionsModel -> ( Int, Int )
olderWindow state =
    let
        -- A retained record's start is an exact JSONL boundary. End the older
        -- page there so the row cap cannot strand records between byte windows.
        end = List.head state.rows |> Maybe.map .offset |> Maybe.withDefault (if state.historyBefore > 0 then state.historyBefore else min state.end (Maybe.withDefault 0 state.start + 1048578))
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
        uiRequest=field "request" Decode.int -1
        uiKey=field "key" Decode.string ""
        uiOpen=field "open" Decode.bool False
        uiAncestor=field "ancestor" Decode.string ""
        uiNamespace=field "namespace" Decode.string ""
        uiValid=uiRequest<=0 || (field "scope" Decode.string ""==state.viewerScope && (uiNamespace=="group:"++uiKey || uiNamespace=="event:"++uiKey))
        uiError="Activity could not be updated. Retry."
        uiAck failed cancelled=if uiRequest>0 then Ports.sessionBridgeCommand (Encode.object [("stamp",Encode.string stamp),("generation",Encode.int generation),("disclosureAck",Encode.object [("scope",Encode.string state.viewerScope),("request",Encode.int uiRequest),("namespace",Encode.string uiNamespace),("key",Encode.string uiKey),("ancestor",Encode.string uiAncestor),("open",Encode.bool uiOpen),("error",Encode.bool failed),("cancelled",Encode.bool cancelled)])]) else Cmd.none
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
                    , sessionDelivery = if crossed && model.activeTab == SessionsTab && (state.filters.freshness /= "" || state.filters.activity /= Nothing) then state.workspace |> Maybe.map (\ws -> Delivery.invalidateLists ws model.sessionDelivery) |> Maybe.withDefault model.sessionDelivery else model.sessionDelivery}, Cmd.none )
                else (model,Cmd.none)
            Err _ -> (model,Cmd.none)
    else if kind == "save-capability" then
        ( {model | sessions = {state | saveCapability = Just (field "supported" Decode.bool False)}}, Cmd.none )
    else if String.startsWith "archive-" kind then receiveArchive value model
    else if String.startsWith "save-" kind then receiveSave value model
    else if kind == "latest-complete" && stamp == state.stamp && generation == state.generation && field "request" Decode.int -1 == state.latestRequest then
        ({model | sessions={state | latestPending=False,latestError=Nothing,following=True,manualUp=False,loadWindow=False}},Cmd.none)
    else if kind == "latest-error" && stamp == state.stamp && generation == state.generation && field "request" Decode.int -1 == state.latestRequest then
        ({model | sessions={state | latestPending=False,latestError=Just "Latest messages could not be reached."}},Cmd.none)
    else if kind == "latest-cancelled" && stamp == state.stamp && generation == state.generation && field "request" Decode.int -1 == state.latestRequest then
        ({model | sessions={state | latestPending=False,following=False,loadWindow=False}},Cmd.none)
    else if kind == "viewer-end" && stamp == state.stamp && generation == state.generation then update SessionsJumpLatest model
    else if kind == "viewer-handoff-cancelled" && stamp == state.stamp && generation == state.generation && field "publication" Decode.int -1 == state.windowPublication && state.positionPending then
        ({model | sessions={state | positionPending=False}},Cmd.none)
    else if kind == "viewer-handoff-error" && stamp == state.stamp && generation == state.generation && field "publication" Decode.int -1 == state.windowPublication then
        ({model | sessions={state | positionPending=False,error=Just "Conversation position could not be restored. Please retry."}},Cmd.none)
    else if kind == "viewer-positioned" && stamp == state.stamp && generation == state.generation && field "publication" Decode.int -1 == state.windowPublication && state.positionPending then
        ({model | sessions={state | positionPending=False,error=if field "failed" Decode.bool False then Just "Conversation position could not be restored. Please retry." else state.error}},Cmd.none)
    else if kind == "viewer-boundary" && stamp == state.stamp && generation == state.generation then
        let prepared={state | historyAnchor=field "anchor" Decode.string state.historyAnchor,presentationAnchor=field "fragmentAnchor" Decode.string state.presentationAnchor,historyHeights=field "heights" (Decode.dict Decode.float) state.historyHeights |> Dict.toList |> List.take 200 |> Dict.fromList,historyClosed=field "closedGroups" (Decode.list Decode.string) [] |> List.take 200 |> Set.fromList,windowPurpose=if field "fill" Decode.bool False then "fill" else state.windowPurpose}
            next={model | sessions=prepared}
        in
        if state.pending || state.positionPending || (state.loadWindow && state.offset < state.end) || state.error /= Nothing then (next,Cmd.none)
        else if field "edge" Decode.string "" == "gap" then
            let
                gapBefore = field "before" Decode.int -1
                gapAfter = field "after" Decode.int -1
                backward = field "direction" Decode.string "" == "tail"
                neighbor = List.filter (\candidate -> candidate.offset >= gapAfter) state.rows |> List.head
                target = gapBefore+round (toFloat (gapAfter-gapBefore)*clamp 0 1 (field "fraction" Decode.float 0))
                interior = target-gapBefore>32768 && gapAfter-target>32768
                finish = if interior then min gapAfter (target+16384) else if backward then neighbor |> Maybe.map (\candidate -> if candidate.end-gapAfter<=65536 then candidate.end else gapAfter) |> Maybe.withDefault gapAfter else min gapAfter (gapBefore+65536)
                begin = if interior || backward then max gapBefore (finish-65536) else gapBefore
                anchor = if prepared.historyAnchor/="" then prepared.historyAnchor else String.padLeft 20 '0' (String.fromInt (if interior then target else if backward then gapAfter else gapBefore))
            in
            if gapBefore<0 || gapAfter<=gapBefore || finish<=begin then (model,Cmd.none) else
            ({next | sessions={prepared | start=Just begin,offset=begin,end=finish,pending=False,loadWindow=True,generation=state.generation+1,resetPending=True,windowPurpose="gap",windowDirection=if backward || interior then "tail" else "forward",windowAligned=begin==gapBefore,historyAnchor=anchor,error=Nothing}},Cmd.none)
        else if field "edge" Decode.string "" == "older" && (state.historyBefore > 0 || state.historyScanBefore > 0) then update SessionsOlder next
        else if field "edge" Decode.string "" == "newer" then update SessionsNewer next
        else (next,Cmd.none)
    else if kind == "viewer-render" && stamp == state.stamp && generation == state.generation then
        ({model | sessions={state | presentationPart=clamp 0 100000 (field "part" Decode.int 0),presentationAnchor=""}},Cmd.none)
    else if kind == "viewer-disclosure" && stamp == state.stamp && generation == state.generation && uiValid then
        let ancestorOpen=not uiOpen || uiAncestor=="" || Set.member uiAncestor state.historyOpen
            protected=Set.fromList (uiKey::(if uiOpen && uiAncestor/="" then [uiAncestor] else []))
            retained=if not ancestorOpen then state.historyOpen else if uiOpen then Set.union protected (Set.diff state.historyOpen protected |> Set.toList |> List.take (400-Set.size protected) |> Set.fromList) else Set.remove uiKey state.historyOpen |> Set.toList |> List.take 400 |> Set.fromList
            applied=ancestorOpen && Set.member uiKey retained==uiOpen
        in ({model | sessions={state | historyOpen=retained}},if applied || not ancestorOpen then uiAck False (not ancestorOpen) else Cmd.none)
    else if kind == "viewer-disclosure-error" && stamp == state.stamp && generation == state.generation && uiValid then
        let ancestorOpen=not uiOpen || uiAncestor=="" || Set.member uiAncestor state.historyOpen
            applied=Set.member uiKey state.historyOpen==uiOpen && ancestorOpen
        in ({model | sessions={state | error=if applied || not ancestorOpen then state.error else Just uiError}},uiAck (not applied && ancestorOpen) (not ancestorOpen))
    else if kind == "viewer-follow" && stamp == state.stamp && generation == state.generation then
        if state.latestPending then (model,Cmd.none) else
        let manual=field "manualUp" Decode.bool state.manualUp
            following=not manual
        in
        ( {model | sessions={state | manualUp=manual,following=following,historyAnchor=field "anchor" Decode.string state.historyAnchor,presentationPart=if following then -1 else state.presentationPart,presentationAnchor=if following then "" else state.presentationAnchor,loadWindow=if state.following && not following then False else state.loadWindow}},Cmd.none )
    else if stamp /= state.stamp || generation /= state.generation || not state.pending then (model,Cmd.none)
    else if field "kind" Decode.string "" == "viewer-error" then
        ( {model | sessions = {state | pending = False, error = Just "Record window could not be decoded. Retry or load another window."}}, Cmd.none )
    else if through <= state.offset || through > min state.end (state.offset + 65536) then (model,Cmd.none)
    else if kind == "viewer-progress" then
        let
            newest = state.selected |> Maybe.andThen (\id -> state.workspace |> Maybe.andThen (\ws -> resource ws (Delivery.SessionMetadata id) model |> Maybe.andThen .data)) |> Maybe.andThen (\data -> case data of
                Delivery.Metadata session -> Just session.committedBytes
                _ -> Nothing) |> Maybe.withDefault state.end
            grow = state.windowDirection=="forward" && through>=state.end && field "pendingBytes" Decode.int 0>0 && state.end<newest && state.end-Maybe.withDefault state.offset state.start<2097152
            captured = through>=state.end && not grow && field "pendingBytes" Decode.int 0>0
        in
        -- A finite incomplete suffix is scanned, not authoritative complete
        -- records. Settle its captured end without admitting/deleting rows;
        -- subsequent growth restarts from the last complete boundary above.
        ( {model | sessions = {state | offset = through,pending = False,end=if grow then min newest (min (Maybe.withDefault state.offset state.start+2097152) (state.end+65536)) else state.end,historyAfter=if captured then max state.historyAfter through else state.historyAfter,loadWindow=if captured then False else state.loadWindow,notice = field "notice" Decode.string state.notice}}, Cmd.none )
    else case Decode.decodeValue (Decode.field "rows" (Decode.list row)) value of
        Ok rows ->
            if List.length rows <= 200 && List.sum (List.map (.raw >> String.length) rows) <= 4194304 then
                if List.isEmpty rows && field "pendingBytes" Decode.int 0 > 0 then
                    ( {model | sessions={state | offset=through,pending=False,notice=field "notice" Decode.string state.notice}},Cmd.none )
                else if ((List.isEmpty rows && (field "skipping" Decode.bool False || field "needsLookback" Decode.bool False)) || (List.member state.windowPurpose ["older","fill"] && field "parsedBefore" Decode.int (field "before" Decode.int 0) >= (if state.windowPurpose=="fill" then state.historyScanBefore else state.historyBefore))) && through >= state.end && Maybe.withDefault 0 state.start > max 0 (state.end - 2097152) then
                    let begin = max 0 (state.end - 2097152) in
                    ( {model | sessions={state | start=Just begin,offset=begin,pending=False,generation=state.generation+1,resetPending=True}},Cmd.none )
                else
                    let position = field "position" (Decode.map2 Tuple.pair (Decode.field "anchor" Decode.string) (Decode.field "fragmentAnchor" Decode.string)) (state.historyAnchor,state.presentationAnchor)
                        positioned = if state.following then state else {state | historyAnchor=Tuple.first position |> String.left 40,presentationAnchor=Tuple.second position |> String.left 4096}
                        composed=composeRecords value (field "before" Decode.int (Maybe.withDefault 0 state.start)) through (field "completeThrough" Decode.int through) rows positioned
                        full=field "pageFull" Decode.bool False
                    in
                    ( {model | sessions = {composed | offset=through,pending=False,positionPending=field "publication" Decode.int 0 > 0,windowPublication=field "publication" Decode.int 0,loadWindow=through<state.end,notice=field "notice" Decode.string ""
                        ,start=if full && through<state.end then Just through else state.start
                        ,generation=if full && through<state.end then state.generation+1 else state.generation
                        ,resetPending=full && through<state.end
                        ,windowDirection=if full && through<state.end then "forward" else state.windowDirection
                        ,windowAligned=if full && through<state.end then True else state.windowAligned}}, Cmd.none )
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


sealedPieceBytes : SealedSessionPiece -> Int
sealedPieceBytes piece =
    1024 + payloadBytes (Encode.list Encode.string [piece.key,piece.offset,piece.label,piece.role,piece.time,piece.group,piece.count,piece.endTime])
        + (piece.body |> Maybe.map sealedBodyBytes |> Maybe.withDefault 0)


payloadBytes : Encode.Value -> Int
payloadBytes value =
    Encode.encode 0 value |> String.foldl (\char count -> let code=Char.toCode char in count+(if code<128 then 1 else if code<2048 then 2 else if code<65536 then 3 else 4)) 0


sealedBodyBytes : SealedSessionBody -> Int
sealedBodyBytes body =
    payloadBytes (Encode.object [("text",Encode.string body.text),("part",Encode.int body.part),("more",Encode.bool body.more),("format",Encode.string body.format),("structured",Encode.bool body.structured),("exception",Encode.bool body.exception)
        ,("tokens",Encode.list (\(kind,token) -> Encode.object [("kind",Encode.string kind),("text",Encode.string token)]) body.tokens)])


boundSealedContent : SessionsModel -> SealedSessionView -> SessionsModel
boundSealedContent state sealed =
    let
        bodyCost key=Dict.get key sealed.bodies |> Maybe.map (List.map sealedBodyBytes >> List.sum) |> Maybe.withDefault 0
        policy={identity=.key,source=.key,bytes=\item -> sealedPieceBytes item+bodyCost item.key}
        anchor=if state.historyAnchor=="" then Nothing else Just state.historyAnchor
        result=SessionWindow.compose policy 200 (4194304-2*payloadBytes (Encode.string sealed.markdown.source)) anchor state.following False sealed.rows []
        retained=Set.fromList (List.map .key result.rows)
        gapFor items=case (List.head items,List.reverse items |> List.head) of
            (Just first,Just last) -> [{before=first.key,after=last.key ++ ":1",pixels=List.map (\item -> Dict.get item.key state.historyHeights |> Maybe.withDefault 64 |> max 32) items |> List.sum |> min 10000000,group=if first.group/="" && List.all (.group >> (==) first.group) items then first.group else ""}]
            _ -> []
        gaps=SessionWindow.reconcileGaps Nothing (List.map (\item -> (item.key,item.key ++ ":1")) result.rows) (gapFor result.removedBefore ++ gapFor result.removedAfter) state.historyGaps
        firstKey=List.head result.rows |> Maybe.map .key |> Maybe.withDefault ""
        lastKey=List.reverse result.rows |> List.head |> Maybe.map .key |> Maybe.withDefault ""
    in {state | historyGaps=gaps,sealed={sealed | rows=result.rows,bodies=Dict.filter (\key _ -> Set.member key retained) sealed.bodies,first=firstKey,last=lastKey,canPrevious=not sealed.atBeginning || firstKey>sealed.scanFirst,canNext=not sealed.atLatest || lastKey<sealed.scanLast}}


sealedBodyDecoder : Decode.Decoder SealedSessionBody
sealedBodyDecoder =
    Decode.map7 (\content part more tokens format structured exception -> {text=content,part=part,more=more,tokens=tokens,format=format,structured=structured,exception=exception})
        (Decode.field "text" Decode.string) (Decode.field "part" Decode.int) (Decode.field "more" Decode.bool)
        (Decode.oneOf [Decode.field "tokens" (Decode.list (Decode.map2 Tuple.pair (Decode.field "kind" Decode.string) (Decode.field "text" Decode.string))),Decode.succeed []])
        (Decode.oneOf [Decode.field "format" Decode.string,Decode.succeed "markdown"])
        (Decode.oneOf [Decode.field "structured" Decode.bool,Decode.succeed False]) (Decode.oneOf [Decode.field "exception" Decode.bool,Decode.succeed False])


composeSealed : List SealedSessionPiece -> String -> String -> Int -> Bool -> Bool -> Bool -> (String,String) -> SessionsModel -> SessionsModel
composeSealed incoming pageFirst pageLast request fromEnd canPrevious canNext represented state =
    let
        old = state.sealed
        starting = List.isEmpty old.rows
        seeking = state.latestPending
        compact item (kept,dropped,seen) =
            if state.windowPurpose=="fill" && state.following && item.group/="" && not item.groupOpen then
                if Set.member item.group seen then (kept,item::dropped,seen) else (item::kept,dropped,Set.insert item.group seen)
            else (item::kept,dropped,seen)
        (previous,compacted,_) = List.foldl compact ([],[],Set.empty) (List.reverse (if seeking then [] else old.rows))
        policy = { identity = .key, source = .key, bytes = sealedPieceBytes }
        anchor = if state.historyAnchor == "" then Nothing else Just state.historyAnchor
        preserveBody fresh = previous |> List.filter (.key >> (==) fresh.key) |> List.head |> Maybe.map (\cached -> case cached.body of
            Just content -> if content.format=="markdown" && not content.more && content.part==0 then {fresh | body=cached.body,ast=cached.ast} else fresh
            Nothing -> fresh) |> Maybe.withDefault fresh
        bodyBytes = Dict.values old.bodies |> List.concat |> List.map sealedBodyBytes |> List.sum
        result = SessionWindow.composeRange (Just represented) policy 200 (4194304-2*payloadBytes (Encode.string old.markdown.source)-bodyBytes) anchor state.following fromEnd previous (List.map preserveBody incoming)
        first = List.head result.rows |> Maybe.map .key |> Maybe.withDefault ""
        last = List.reverse result.rows |> List.head |> Maybe.map .key |> Maybe.withDefault ""
        scanFirst = if old.scanFirst == "" || seeking then pageFirst else min old.scanFirst pageFirst
        scanLast = if seeking then pageLast else max old.scanLast pageLast
        atBeginning = (old.atBeginning && not seeking) || not canPrevious
        atLatest = (old.atLatest && not seeking) || not canNext
        measured row = Dict.get row.key state.historyHeights |> Maybe.withDefault 64 |> max 32
        extent = List.map measured >> List.sum
        refetchedBefore = List.filter (\row -> old.scanFirst /= "" && row.key >= old.scanFirst) result.addedBefore
        refetchedAfter = List.filter (\row -> row.key <= old.scanLast) result.addedAfter
        topCount = max 0 ((if starting || seeking then 0 else state.historyTopCount)-List.length refetchedBefore+List.length result.removedBefore)
        bottomCount = max 0 ((if starting || seeking then 0 else state.historyBottomCount)-List.length refetchedAfter+List.length result.removedAfter)
        top = clamp 0 10000000 ((if starting || seeking then 0 else state.historyTop)-extent refetchedBefore+extent result.removedBefore)
        bottom = clamp 0 10000000 ((if starting || seeking then 0 else state.historyBottom)-extent refetchedAfter+extent result.removedAfter)
        gapFor rows = case (List.head rows,List.reverse rows |> List.head) of
            (Just head,Just tail) -> [{before=head.key,after=tail.key ++ ":1",pixels=min 10000000 (extent rows),group=if head.group/="" && List.all (.group >> (==) head.group) rows then head.group else ""}]
            _ -> []
        compactedRuns=List.foldl (\item runs -> Dict.update item.group (\items -> Just (Maybe.withDefault [] items ++ [item])) runs) Dict.empty compacted |> Dict.values |> List.concatMap gapFor
        gaps=SessionWindow.reconcileGaps (Just represented) (List.map (\item -> (item.key,item.key ++ ":1")) result.rows)
            (compactedRuns ++ gapFor result.removedBefore ++ gapFor result.removedAfter) (if starting || seeking then [] else state.historyGaps)
        keptKeys=Set.fromList (List.map .key result.rows)
        presentationAnchor=if state.windowPurpose=="gap" then result.rows |> List.filter (\item -> item.key>=state.historyAnchor) |> List.head |> Maybe.map (\item -> item.key ++ ":") |> Maybe.withDefault "" else state.presentationAnchor
    in
    {state | historyTop=if topCount==0 then 0 else top,historyBottom=if bottomCount==0 then 0 else bottom,historyTopCount=topCount,historyBottomCount=bottomCount,historyTopGroup="",historyBottomGroup="",historyGaps=gaps,presentationAnchor=presentationAnchor
        ,sealed={old | phase="ready",error=Nothing,http=Nothing,rows=result.rows,first=first,last=last,pageFirst=pageFirst,pageLast=pageLast,pageRequest=request,scanFirst=scanFirst,scanLast=scanLast,atBeginning=atBeginning,atLatest=atLatest
            ,canPrevious=not atBeginning || first>scanFirst,canNext=not atLatest || last<scanLast,more=not atLatest || last<scanLast
            ,renderPart=if state.following || seeking then -1 else if starting then (if fromEnd then -1 else 0) else old.renderPart
            ,bodies=Dict.filter (\key _ -> Set.member key keptKeys) old.bodies
            ,markdown=if List.any (\row -> row.key==old.markdown.key) result.rows then old.markdown else emptySealedSessionView.markdown}
    }


receiveSealed : Decode.Value -> Model -> ( Model, Cmd Msg )
receiveSealed value model =
    let
        state = model.sessions
        sealed = let previous = state.sealed in {previous | domEpoch=field "domEpoch" Decode.string previous.domEpoch}
        field name decoder fallback = Decode.decodeValue (Decode.field name decoder) value |> Result.withDefault fallback
        kind = field "kind" Decode.string ""
        piece = Decode.map8 (\key offset blockKind label role time group count -> {key=key,offset=offset,kind=blockKind,label=label,role=role,time=time,group=group,count=count,endTime="",open=False,groupOpen=False,body=Nothing,ast=Err "not_loaded"})
            (Decode.field "key" Decode.string) (Decode.field "offset" Decode.string) (Decode.field "kind" Decode.string) (Decode.field "label" Decode.string)
            (Decode.field "role" Decode.string) (Decode.field "time" Decode.string) (Decode.field "group" Decode.string) (Decode.field "count" Decode.string)
            |> Decode.andThen (\row -> Decode.map (\endTime -> {row | endTime=endTime}) (Decode.oneOf [Decode.field "endTime" Decode.string,Decode.succeed ""]))
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
        ({model | sessions={state | rows=[],historyGaps=[],historyTop=0,historyBottom=0,sealed={sealed | phase="preparing",http=Nothing,rows=[],first="",last="",more=False,canPrevious=False,canNext=False,renderPart=0,bodies=Dict.empty,markdown=emptySealedSessionView.markdown}}},Cmd.none)
    else if kind == "sealed-error" then
        let code=field "code" Decode.string "archive_failed" in
        ({model | sessions={state | sealed={sealed | phase="error",canRetry=code /= "archive_cleanup_failed",http=Nothing
            ,error=Just (if code == "archive_cleanup_failed" then "Archive cleanup could not be confirmed. Reload this page before another archive." else if code == "archive_item_limit" then "One record exceeds the local view limit. The original transcript is still available to download." else if code == "archive_recovery_limit" then "Local archive recovery reached its directory limit. Close other archive tabs or clear this site's stored archive data before retrying." else if code == "archive_recovery_pending" then "Old local archives were removed. Retry to finish recovery." else code)}}},Cmd.none)
    else if kind == "sealed-cancelled" then
        ({model | sessions={state | sealed={sealed | phase="cancelled",http=Nothing,rows=[],bodies=Dict.empty,markdown=emptySealedSessionView.markdown,error=Nothing}}},Cmd.none)
    else if kind == "sealed-render" then
        let part=field "part" Decode.int -1
            request=field "request" Decode.int 0
        in
        if part < 0 || part > 100000 || request < sealed.renderRequest then (model,Cmd.none)
        else ({model | sessions={state | presentationAnchor="",sealed={sealed | renderPart=part,renderRequest=request}}},Cmd.none)
    else if kind == "sealed-latest-render" && state.latestPending && field "latestRequest" Decode.int -1 == state.latestRequest then
        ({model | sessions={state | presentationAnchor="",sealed={sealed | renderPart=-1,renderRequest=field "request" Decode.int sealed.renderRequest}}},Cmd.none)
    else if kind == "sealed-latest-complete" && field "latestRequest" Decode.int -1 == state.latestRequest then
        ({model | sessions={state | latestPending=False,latestError=Nothing,manualUp=False,following=True}},Cmd.none)
    else if kind == "sealed-follow" then
        let manual=field "manualUp" Decode.bool state.manualUp in
        ({model | sessions={state | manualUp=manual,following=not manual,historyAnchor=field "anchor" Decode.string state.historyAnchor,presentationAnchor=field "fragmentAnchor" Decode.string state.presentationAnchor,historyHeights=field "heights" (Decode.dict Decode.float) state.historyHeights |> Dict.toList |> List.take 200 |> Dict.fromList}},Cmd.none)
    else if kind == "sealed-latest-error" && field "latestRequest" Decode.int -1 == state.latestRequest then
        ({model | sessions={state | latestPending=False,latestError=Just "Latest messages could not be reached."}},Cmd.none)
    else if kind == "sealed-latest-cancelled" && field "latestRequest" Decode.int -1 == state.latestRequest then
        ({model | sessions={state | latestPending=False}},Cmd.none)
    else if kind == "sealed-disclosure" then
        let key=field "key" Decode.string ""
            open=field "open" Decode.bool False
            updateDisclosure row=if row.key == key then {row | open=open} else if "group:" ++ row.group == key then {row | groupOpen=open} else row
        in ({model | sessions={state | sealed={sealed | rows=List.map updateDisclosure sealed.rows}}},Cmd.none)
    else if kind == "sealed-markdown-start" then
        ({model | sessions={state | sealed={sealed | phase="reading-body",markdown={key=field "key" Decode.string "",source="",ast=Err "not_loaded",structured=False,complete=False,nextPart=0,request=0}}}},Cmd.none)
    else if kind == "sealed-body" then
        case Decode.decodeValue (Decode.field "body" sealedBodyDecoder) value of
            Ok content ->
                let
                    key=field "key" Decode.string ""
                    cached=Dict.get key sealed.bodies |> Maybe.withDefault (sealed.rows |> List.filter (.key >> (==) key) |> List.head |> Maybe.andThen .body |> Maybe.map List.singleton |> Maybe.withDefault [])
                    previous=if List.any (\part -> abs (part.part-content.part)<=1) cached then cached else []
                    joined=content::List.filter (.part >> (/=) content.part) previous |> List.sortBy .part
                    anchorPart=field "anchorPart" Decode.int content.part
                    before=List.filter (\part -> part.part <= anchorPart) joined |> List.reverse |> List.take 2 |> List.reverse
                    after=List.filter (\part -> part.part > anchorPart) joined |> List.take (max 0 (3-List.length before))
                    parts=before++after
                    cost values=List.sum (List.map sealedBodyBytes values)
                    prune entries total kept=case entries of
                        [] -> Dict.fromList kept
                        (entryKey,values)::rest -> if total+cost values>4194304 then prune rest total kept else prune rest (total+cost values) ((entryKey,values)::kept)
                    bodies=prune ((key,parts)::(Dict.toList sealed.bodies |> List.filter (Tuple.first >> (/=) key))) 0 []
                    attach item=if item.key==key then {item | body=Just (if content.format=="json" then {content | text="",tokens=[],part=0,more=True} else content),ast=if content.format=="markdown" && not content.more && content.part==0 then Markdown.Parser.parse content.text |> Result.mapError (always "parse error") else Err "not_loaded"} else item
                in
                if String.length content.text>65536 || not (List.any (.key >> (==) key) sealed.rows) then (model,Cmd.none)
                else
                    let markdown=sealed.markdown in
                    ({model | sessions=boundSealedContent {state | presentationAnchor=if state.following || field "seekStart" Decode.bool False then "" else field "fragmentAnchor" Decode.string state.presentationAnchor} {sealed | rows=List.map attach sealed.rows,bodies=bodies,renderPart=if field "seekStart" Decode.bool False then 0 else sealed.renderPart,markdown={markdown | request=field "request" Decode.int markdown.request}}},Cmd.none)
            Err _ -> (model,Cmd.none)
    else if kind == "sealed-markdown-chunk" then
        let
            previous=sealed.markdown
            textPart=field "text" Decode.string ""
            complete=field "complete" Decode.bool False
            source=previous.source++textPart
        in
        if field "key" Decode.string "" /= previous.key || field "part" Decode.int -1 /= previous.nextPart || String.length previous.source + String.length textPart > 1048576 then (model,Cmd.none)
        else
            let
                parsed = if complete then Markdown.Parser.parse source |> Result.mapError (always "parse error") else previous.ast
                saveBody item = if complete && item.key==previous.key then {item | ast=parsed,body=item.body |> Maybe.map (\content -> {content | text=source,part=0,more=False,tokens=[]})} else item
            in
            ({model | sessions=boundSealedContent state {sealed | phase=if complete then "ready" else "reading-body"
                ,rows=List.map saveBody sealed.rows
                ,markdown={previous | key=if complete then "" else previous.key,source=if complete then "" else source,ast=Err "not_loaded",structured=field "structured" Decode.bool False,complete=False,nextPart=previous.nextPart+1,request=field "request" Decode.int 0}}},Cmd.none)
    else if kind == "sealed-progress" then
        ({model | sessions={state | sealed={sealed | phase=field "phase" Decode.string sealed.phase,rawBytes=field "rawBytes" Decode.string sealed.rawBytes,compressedBytes=field "compressedBytes" Decode.string sealed.compressedBytes,archiveGeneration=field "archiveGeneration" Decode.string sealed.archiveGeneration}}},Cmd.none)
    else if kind == "sealed-page" then
        case Decode.decodeValue (Decode.field "rows" (Decode.list piece)) value of
            Ok rows ->
                if List.length rows <= 200 && String.length (Encode.encode 0 value) <= 4194304 then
                    let composed=composeSealed rows (field "first" Decode.string "") (field "last" Decode.string "") (field "request" Decode.int 0) (field "fromEnd" Decode.bool False) (field "canPrevious" Decode.bool False) (field "canNext" Decode.bool False) (field "representedBefore" Decode.string (field "first" Decode.string ""),field "representedAfter" Decode.string (field "last" Decode.string "" ++ ":1")) {state | windowPurpose=field "purpose" Decode.string ""}
                        next=composed.sealed
                    in ({model | sessions={composed | sealed={next | archiveGeneration=field "archiveGeneration" Decode.string ""}}},Cmd.none)
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
            (List.map (\(filter,caption) -> button [class ("btn session-filter" ++ (if List.member filter (model.sessions.filters.activity |> Maybe.withDefault "live" |> String.split ",") then " selected" else "")),attribute "aria-pressed" (if List.member filter (model.sessions.filters.activity |> Maybe.withDefault "live" |> String.split ",") then "true" else "false"),onClick (SessionsActivityFilter filter)] [text caption]) [("live","Live"),("quiet","Quiet"),("completed","Completed")]
                ++ [viewFacet workspace model "projects",viewFacet workspace model "tasks"])
        , div [ class "sessions-layout" ]
            [ aside [ class "sessions-list", attribute "aria-label" "Session hierarchy" ]
                [ model.sessions.selected |> Maybe.map (viewLinkedTasks workspace model) |> Maybe.withDefault (text "")
                , h3 [class "session-pane-label"] [text "Session hierarchy"]
                , div [class "session-hierarchy"]
                    [ case list |> Maybe.andThen .error of
                    Just _ -> div [ attribute "role" "alert" ] [ text "Sessions could not be loaded. ", button [ onClick SessionsRetry ] [text "Retry"] ]
                    Nothing -> case list |> Maybe.andThen .data of
                        Just (Delivery.SessionsPage page) -> div []
                            [ if List.isEmpty page.items then p [ class "empty-state" ] [text "No sessions match these filters."]
                              else viewSessionTree workspace model page.items
                            , div [class "sessions-pagination"]
                                [ button [class "btn btn-sm", disabled (model.sessions.cursor == Nothing), onClick SessionsPreviousPage] [text "Newer"]
                                , page.nextCursor |> Maybe.map (\cursor -> button [class "btn btn-sm",onClick (SessionsNextPage cursor)] [text "Older sessions"]) |> Maybe.withDefault (text "") ] ]
                        _ -> p [ attribute "role" "status" ] [text "Loading sessions…"] ] ]
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
            ,viewSessionActivity model session
            ] :: actions)


viewSessionIdentity : Api.AgentSession -> Html Msg
viewSessionIdentity session =
    let
        component get = session.runtimeIdentity |> Maybe.andThen get |> identityValue |> Maybe.withDefault "unknown"
        runtime = component .harness ++ ":" ++ component (Just << .model) ++ ":" ++ component .reasoning
        label = if session.state == Api.SessionActive then "Running agent" else "Session agent"
        role = component .role
        visible = "[" ++ role ++ "] " ++ runtime
        provider = session.runtimeIdentity |> Maybe.andThen .provider |> identityValue |> Maybe.map (\value -> " · Provider: " ++ value) |> Maybe.withDefault ""
    in
    span [class "session-identity"]
        [span [class "session-role actor-attribution-kind",attribute "aria-label" (label ++ " role: " ++ role),Html.Attributes.title (label ++ " role: " ++ role)] [text role]
        ,span [class "session-runtime actor-attribution-kind",attribute "aria-label" (label ++ " model: " ++ runtime),Html.Attributes.title (label ++ ": " ++ visible ++ provider)]
            [span [class "session-runtime-value"] [text runtime]]]


viewDetail : String -> Model -> String -> Html Msg
viewDetail workspace model id =
    div [ class "session-detail" ]
        [ case resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .data of
            Just (Delivery.Metadata session) -> div [class "session-detail-header"]
                [ div [class "session-detail-title"] [h3 [] [text (sessionTitle session)],span [class "card-meta session-lifecycle"] [text (if session.state == Api.SessionTerminated then "Saved session" else "Active session")],span [class "card-meta session-freshness"] [text (case Dict.get session.id model.sessions.heartbeats of
                    Just "Fresh" -> " · Live"
                    Just "Stale (30s)" -> " · Quiet"
                    _ -> "")],text " · ",viewSessionActivity model session]
                ,viewSave model session
                ,a [class "btn btn-sm",href (filterUrl model model.sessions.projectFilter model.sessions.taskFilter),attribute "aria-label" "Close session"] [text "✕"]]
            _ -> resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .error |> Maybe.map (\_ -> p [attribute "role" "alert"] [text "Session could not be loaded.",button [onClick SessionsRetry] [text "Retry"]]) |> Maybe.withDefault (p [attribute "role" "status"] [text "Loading session…"])
        , div [class "session-conversation-grid"]
            [ div [class "session-conversation"]
              [ viewRecords WorkspaceReader workspace model id ] ]
        ,p [class "session-download-status",attribute "role" "status",attribute "aria-live" "polite"] [text (if model.sessions.archiveMessageTask == Nothing || model.sessions.saveBlocked then model.sessions.saveMessage else "")] ]


viewLinkedTasks : String -> Model -> String -> Html Msg
viewLinkedTasks workspace model id =
    let entry = resource workspace (Delivery.SessionTasks id model.sessions.linksCursor) model in
    details [class "session-linked-tasks",attribute "aria-label" "Linked tasks"]
        [summary [] [text "Linked Tasks"]
        ,div [class "session-linked-content"]
            [case entry |> Maybe.andThen .error of
                Just _ -> p [attribute "role" "alert"] [text "Linked tasks could not be loaded. ",button [class "btn btn-sm",onClick SessionsRetry] [text "Retry"]]
                Nothing -> case entry |> Maybe.andThen .data of
                    Just (Delivery.TasksPage page) -> div []
                        [div [class "session-task-cards"] (List.map (Dependencies.viewReadOnlyTask model) page.items)
                        ,if List.isEmpty page.items then p [class "card-meta"] [text "No linked tasks."] else text ""
                        ,div [class "sessions-pagination"]
                            [page.nextTaskId |> Maybe.map (\cursor -> button [class "btn btn-sm",onClick (SessionsLinkedPage (Just cursor))] [text "More tasks"]) |> Maybe.withDefault (text "")
                            ,if model.sessions.linksCursor /= Nothing then button [class "btn btn-sm",onClick (SessionsLinkedPage Nothing)] [text "First tasks"] else text ""]]
                    _ -> p [class "card-meta",attribute "role" "status"] [text "Loading linked tasks…"]]
        ]


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
sectionTarget evidence model task = Delivery.TaskSessionsFiltered task {state=if evidence then "terminated" else "active",freshness="",activity=Nothing} (taskCursor model (sectionKey evidence task))


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


taskAssociation : String -> Model -> String -> Maybe Bool
taskAssociation workspace model task =
    if model.sessions.workspace /= Just workspace || model.selectedWorkspaceId /= Just workspace || model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) then Nothing else
    if model.sessions.epoch == model.sessionRequestEpoch && taskActive workspace model task && taskActivityCurrent workspace model task then Just True else
    Dict.get task model.sessions.taskAssociations |> Maybe.andThen (\hint ->
        let negativePending = if hint.present then False else resource workspace (Delivery.TaskSessionsFiltered task {state="",freshness="",activity=Nothing} Nothing) model
                |> Maybe.map (\entry -> entry.error /= Nothing || not entry.proven || entry.dirty /= entry.settled || (case entry.data of
                    Just (Delivery.SessionsPage page) -> not (List.isEmpty page.items) || page.nextCursor /= Nothing
                    _ -> True)) |> Maybe.withDefault False
        in if hint.error || hint.epoch /= model.sessionDelivery.listInvalidation || model.sessions.epoch /= model.sessionRequestEpoch || negativePending then Nothing else Just hint.present)


taskEvidenceAvailable : String -> Model -> String -> Maybe Bool
taskEvidenceAvailable workspace model task =
    let
        presence page = if not (List.isEmpty page.items) then Just True else if page.nextCursor == Nothing then Just False else Nothing
        target = Delivery.TaskSessionsFiltered task {state="terminated",freshness="",activity=Nothing} Nothing
        cachedPresence = Dict.get (sectionKey True task) model.sessions.taskPages |> Maybe.andThen (\cached ->
            if cached.epoch /= model.sessionDelivery.listInvalidation then Nothing else Result.toMaybe cached.result |> Maybe.andThen (\page ->
                if not (List.isEmpty page.items) then Just True else if cached.cursor == Nothing then presence page else Nothing))
        confirmed = if cachedPresence == Just True then Just True else case resource workspace target model of
            Just entry -> if entry.error /= Nothing || not entry.proven || entry.dirty /= entry.settled then Nothing else case entry.data of
                Just (Delivery.SessionsPage page) -> presence page
                _ -> Nothing
            Nothing -> cachedPresence
    in
    if model.sessions.workspace /= Just workspace || model.selectedWorkspaceId /= Just workspace || model.sessions.epoch /= model.sessionRequestEpoch || model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) then Nothing
    else if confirmed /= Nothing then confirmed
    else if taskAssociation workspace model task == Just False then Just False
    else Nothing


taskActivityCurrent : String -> Model -> String -> Bool
taskActivityCurrent workspace model task =
    let
        hintCurrent = Dict.get task model.sessions.taskHints |> Maybe.map (\hint -> not hint.error && hint.epoch == model.sessionDelivery.listInvalidation && model.sessions.epoch == model.sessionRequestEpoch) |> Maybe.withDefault False
        pageCurrent = resource workspace (Delivery.TaskSessionsFiltered task {state="active",freshness="",activity=Nothing} Nothing) model |> Maybe.map (\entry -> entry.error == Nothing && entry.proven && entry.dirty == entry.settled) |> Maybe.withDefault True
    in
    hintCurrent && pageCurrent

taskCanOpen : String -> Model -> String -> Bool
taskCanOpen workspace model task = model.sessions.workspace == Just workspace && taskAssociation workspace model task == Just True


viewTaskIcon : String -> Model -> String -> Html Msg
viewTaskIcon workspace model task =
    let
        hint = Dict.get task model.sessions.taskHints
        active = taskActive workspace model task
        association = taskAssociation workspace model task
        open = model.sessions.taskPanel == Just task
        activityCurrent = taskActivityCurrent workspace model task
        inactive = association == Just True && activityCurrent && (hint |> Maybe.map (\current -> not current.error && not current.active && current.epoch == model.sessionDelivery.listInvalidation) |> Maybe.withDefault False)
        empty = association == Just False && not open
        label = if open then "Hide Agents" else if empty then "No sessions" else "View Agents"
        status = if empty then " · no sessions associated with this task" else if active then " · active sessions" ++ (if association == Nothing || not activityCurrent then " · refreshing" else "") else if inactive then " · no active sessions" else if (hint |> Maybe.map .error |> Maybe.withDefault False) || (Dict.get task model.sessions.taskAssociations |> Maybe.map .error |> Maybe.withDefault False) then " · associations or activity unavailable" else " · checking associations and activity"
    in
    if empty then text "" else button [class ("btn btn-secondary btn-sm task-sessions-icon" ++ (if active then " has-active-sessions" else "") ++ (if open then " selected" else "")),disabled (not open && not (taskCanOpen workspace model task)),attribute "aria-label" label,attribute "aria-expanded" (if open then "true" else "false"),Html.Attributes.title (label ++ status),stopPropagationOn "click" (Decode.succeed (SessionsToggleDock task,True)),stopPropagationOn "dblclick" (Decode.succeed (NoOp,True))]
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
                [resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .data |> Maybe.map (\payload -> case payload of
                    Delivery.Metadata session -> div [class "task-session-selected-context"] [h4 [] [text (sessionTitle session)],viewSessionIdentity session]
                    _ -> text "") |> Maybe.withDefault (text "")
                ,viewRecords EmbeddedReader workspace model id
                ,div [class "task-session-footer"]
                    [a [class "btn btn-sm task-session-open-full",href (sessionUrl workspace id ++ "&task=" ++ Url.percentEncode task)] [text "Jump To Session ↗"]]]) |> Maybe.withDefault (div [class "empty-state"] [text "Select an active session to read its recent conversation."])
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
                Just current -> taskContext current ++ [p [class "card-meta"] [text "The active session preview is open in this task's card."],session |> Maybe.map (\value -> div [class "workspace-inspector-session"] [h4 [] [text (sessionTitle value)],viewSessionIdentity value]) |> Maybe.withDefault (text "")]
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


type ReaderContext
    = WorkspaceReader
    | EmbeddedReader


viewLatestControl : SessionsModel -> Html Msg
viewLatestControl state =
    div [class "session-latest-overlay"]
        [button [class "session-jump-latest",onClick SessionsJumpLatest,attribute "data-latest-owner" state.stamp,attribute "aria-label" "Jump to latest",attribute "aria-busy" (if state.latestPending then "true" else "false")]
            [span [attribute "aria-hidden" "true"] [text "↓"],span [] [text "Jump to latest"],span [attribute "aria-hidden" "true"] [text "↓"]]]


viewLatestError : SessionsModel -> Html Msg
viewLatestError state =
    state.latestError |> Maybe.map (\_ -> p [class "session-latest-error",attribute "role" "alert"] [text "Latest messages could not be reached. ",button [class "btn btn-sm",onClick SessionsJumpLatest] [text "Retry"]]) |> Maybe.withDefault (text "")


historyGap : SessionWindow.Gap -> Html Msg
historyGap gap =
    div [class "session-history-spacer",attribute "data-history-gap" "source",attribute "data-gap-before" gap.before,attribute "data-gap-after" gap.after,attribute "data-gap-group" gap.group,attribute "aria-hidden" "true",Html.Attributes.style "height" (String.fromFloat (max 1 gap.pixels) ++ "px")] []


continuousTranscript : Model -> { nodes : List (String,Html Msg), part : Int, more : Bool, error : Bool }
continuousTranscript model =
    let
        items = Transcript.project model.sessions.rows
        group item = case item of
            Transcript.Activity pieces -> List.head pieces |> Maybe.map (\piece -> Maybe.withDefault piece.key piece.activityAnchor) |> Maybe.withDefault ""
            _ -> ""
        groups = List.map group items |> Set.fromList
        source item = case item of
            Transcript.Message piece -> recordSource piece.record
            Transcript.Activity pieces -> List.head pieces |> Maybe.map (.record >> recordSource) |> Maybe.withDefault ""
        content = List.map (\item -> let (key,node)=activeTranscriptFragment model item in (source item,(model.sessions.stamp ++ ":" ++ key,node))) items
        omitted = List.filter (\gap -> gap.group=="" || not (Set.member gap.group groups)) model.sessions.historyGaps
            |> List.map (\gap -> (gap.before,(model.sessions.stamp ++ ":gap:" ++ gap.before ++ ":" ++ gap.after,Transcript.atom 1024 (\_ -> historyGap gap))))
        entries=List.sortBy Tuple.first (content ++ omitted) |> List.map Tuple.second
        rendered=Transcript.renderWindow 1048576 (if model.sessions.following then -1 else model.sessions.presentationPart) (if model.sessions.following then "" else model.sessions.presentationAnchor) (List.map Tuple.second entries)
        renderKey sourceKey=String.toInt sourceKey |> Maybe.andThen (\index -> List.drop index entries |> List.head |> Maybe.map Tuple.first) |> Maybe.withDefault (model.sessions.stamp ++ ":" ++ sourceKey)
    in {nodes=List.map2 (\sourceKey node -> (renderKey sourceKey,node)) rendered.keys rendered.nodes,part=rendered.part,more=rendered.more,error=rendered.error}


activeTranscriptFragment : Model -> Transcript.Item -> (String,Transcript.Fragment Msg)
activeTranscriptFragment model item =
    let
        state=model.sessions
        opened key=Set.member key state.historyOpen
        attrs key=if opened key then [attribute "open" ""] else []
        blockFragment key source block =
            if block.kind=="text" then Transcript.scoped source (Transcript.formatted sessionMarkdownRenderer block.content)
            else if block.kind=="encrypted" then Transcript.atom 128 (\_ -> em [class "session-thinking"] [text "Thinking..."])
            else
                let body=if block.kind=="thinking" then Transcript.formatted sessionMarkdownRenderer block.content
                         else if block.kind=="event" then Transcript.atom 262144 (\_ -> Html.node "session-event-json" [Html.Attributes.property "payload" (Encode.string block.content)] [])
                         else sealedBlockFragment source "tool" block.content block.tokens
                in Transcript.keyedContainer (1024+12*String.length key+12*String.length block.label)
                    (\children -> div [class "session-event"] [Keyed.node "details" ([class ("session-content-" ++ block.kind),attribute "data-event-key" key] ++ attrs key) (("summary",summary [Html.Attributes.title block.label] [text (if block.label/="" then block.label else if block.kind=="thinking" then "Thinking" else "Tool activity")])::children)])
                    (if opened key then [Transcript.scoped source body] else [])
        pieceFragment piece =
            let record=piece.record in
            Transcript.scoped piece.key (Transcript.keyedContainer (4096+12*String.length piece.key+12*String.length record.time)
                (\children -> article [class ("session-record session-role-" ++ record.role ++ (if piece.activity then " session-record-activity" else "")),attribute "data-offset" (String.fromInt record.offset),attribute "data-block-key" piece.key]
                    [div [class "session-record-meta"] [if record.time=="" then text "" else time [class "session-event-time",attribute "datetime" record.time] [text (compactTimestamp model record.time)],if piece.activity then text "" else span [] [text (if record.role=="user" then "You" else if record.role=="tool" then "Tool" else if record.role=="assistant" then "Assistant" else "Message")]]
                    ,Keyed.node "div" [] children])
                (List.map2 (\index block -> blockFragment (piece.key ++ ":" ++ String.fromInt index) (String.padLeft 19 '0' (String.fromInt record.presentationOffset) ++ ":" ++ String.padLeft 7 '0' (String.fromInt index) ++ ":content") block) piece.blockIndices piece.blocks))
    in case item of
        Transcript.Message piece -> ("message:" ++ piece.key,pieceFragment piece)
        Transcript.Activity pieces ->
            let
                firstKey=List.head pieces |> Maybe.map (\piece -> Maybe.withDefault piece.key piece.activityAnchor) |> Maybe.withDefault ""
                finalTime=List.reverse pieces |> List.head |> Maybe.map (.record >> .time) |> Maybe.withDefault ""
                loaded=List.sum (List.map (.blocks >> List.length) pieces)
                count=max loaded (Dict.get firstKey state.historyActivity |> Maybe.withDefault loaded)
                content=List.map (\piece -> (recordSource piece.record,(piece.key,pieceFragment piece))) pieces
                gaps=List.filter (.group >> (==) firstKey) state.historyGaps |> List.map (\gap -> (gap.before,("gap:" ++ gap.before ++ ":" ++ gap.after,Transcript.atom 1024 (\_ -> historyGap gap))))
                entries=List.sortBy Tuple.first (content ++ gaps) |> List.map Tuple.second
                children rendered=List.map (\(key,node) -> (String.toInt key |> Maybe.andThen (\index -> List.drop index entries |> List.head |> Maybe.map Tuple.first) |> Maybe.withDefault key,node)) rendered
                render body=details ([class "session-activity-group",attribute "data-group-key" firstKey] ++ attrs firstKey)
                    [summary [class "session-activity-summary"] [activityTimestamp model finalTime,span [] [text ("Activity · " ++ String.fromInt count ++ (if count==1 then " event" else " events"))]]
                    ,Keyed.node "div" [class "session-activity-records"] (children body)]
            in ("activity:" ++ firstKey,Transcript.keyedContainer (2048+12*String.length firstKey) render (if opened firstKey then List.map Tuple.second entries else []))


viewRecords : ReaderContext -> String -> Model -> String -> Html Msg
viewRecords context workspace model id =
    let
        state = model.sessions
        session = resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .data |> Maybe.andThen (\payload -> case payload of
            Delivery.Metadata value -> Just value
            _ -> Nothing)
        newest = session |> Maybe.map .committedBytes |> Maybe.withDefault state.offset
        rangeError = resource workspace (Delivery.SessionWindow id state.offset state.end False) model |> Maybe.andThen .error
        metadataError = resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .error
        rendered = continuousTranscript model
    in
    if session |> Maybe.map (.state >> (==) Api.SessionTerminated) |> Maybe.withDefault False then viewSealedRecords context model session
    else section [class "session-viewer",attribute "aria-label" "Session transcript"]
        [div [class "sessions-toolbar"]
            [if context == WorkspaceReader then session |> Maybe.map viewSessionIdentity |> Maybe.withDefault (text "") else text ""]
        ,if state.error /= Nothing || rangeError /= Nothing || metadataError /= Nothing then p [attribute "role" "alert"] [text (Maybe.withDefault "Transcript request failed." state.error),button [onClick SessionsRetry] [text "Retry"]] else text ""
        ,p [attribute "role" "alert",attribute "data-disclosure-error-owner" state.stamp,attribute "data-disclosure-error-scope" state.viewerScope,Html.Attributes.hidden True] [text "Activity update could not be confirmed. ",button [onClick SessionsRetry] [text "Retry"]]
        ,viewLatestError state
        ,Keyed.node "div" [Html.Attributes.id "session-records",class "session-records",tabindex 0,attribute "data-viewer-owner" state.stamp,attribute "data-viewer-generation" (String.fromInt state.generation),attribute "data-latest-request" (String.fromInt state.latestRequest),attribute "data-latest-pending" (if state.latestPending then "true" else "false"),attribute "data-latest-error" (if state.latestError /= Nothing then "true" else "false"),attribute "data-window-publication" (String.fromInt state.windowPublication),attribute "data-position-pending" (if state.positionPending then "true" else "false"),attribute "data-history-mode" "active",attribute "data-render-part" (String.fromInt rendered.part),attribute "data-render-more" (if rendered.more then "true" else "false"),attribute "data-history-before" (String.fromInt state.historyBefore),attribute "data-history-scan-before" (String.fromInt state.historyScanBefore),attribute "data-history-after" (String.fromInt state.historyAfter),attribute "data-history-latest" (String.fromInt newest),attribute "data-history-loading" (if state.start == Nothing || state.pending || state.positionPending || (state.loadWindow && state.offset < state.end) || rangeError /= Nothing || state.error /= Nothing then "true" else "false"),attribute "data-history-error" (if rangeError /= Nothing || state.error /= Nothing || metadataError /= Nothing then "true" else "false"),attribute "aria-label" "Session conversation"]
            rendered.nodes
        ,if rendered.error then p [attribute "role" "alert"] [text "This formatted piece exceeds the local presentation budget."] else text ""
        ,if List.isEmpty state.rows && state.error == Nothing && rangeError == Nothing then p [attribute "role" "status"] [text (if state.start == Nothing then "Loading transcript metadata…" else if state.offset < state.end then "Reading transcript…" else if newest == 0 then "This session has no committed records." else "No complete messages in this window.")] else text ""
        ,if state.notice /= "" then p [attribute "role" "status"] [text state.notice] else text ""
        ,viewLatestControl state ]


viewSealedRecords : ReaderContext -> Model -> Maybe Api.AgentSession -> Html Msg
viewSealedRecords context model session =
    let
        state=model.sessions
        sealed=state.sealed
        workspaceReader = context == WorkspaceReader
        busy = List.member sealed.phase ["preparing","retrying","downloading","decoding","index-ready","navigating","reading-body"]
        footer = div [class "session-archive-footer",attribute "aria-label" "Transcript status"]
            [ if busy then button [class "btn btn-sm",attribute "data-sealed-action" "cancel",attribute "data-sealed-owner" state.stamp] [text "Cancel"] else text ""
            , if sealed.phase == "error" && sealed.canRetry && not (List.isEmpty sealed.rows) then button [class "btn btn-sm",attribute "data-sealed-action" "retry-page",attribute "data-sealed-owner" state.stamp] [text "Retry"] else if (sealed.phase == "error" && sealed.canRetry) || sealed.phase == "cancelled" then button [class "btn btn-sm",onClick SessionsRetry] [text "Retry"] else text ""
            ]
        openAttribute opened = if opened then [attribute "open" ""] else []
        body piece = case piece.body of
            Nothing -> Transcript.atom 1024 (\_ -> span [attribute "data-sealed-body-needed" piece.key,attribute "role" "status"] [text "Loading message…"])
            Just content ->
                let
                    complete=sealed.markdown.key == piece.key && sealed.markdown.complete
                    partial=content.format == "markdown" && (content.more || content.part>0)
                    source=if complete then sealed.markdown.source else content.text
                    renderedBody=Transcript.scoped (piece.key ++ ":content") (if partial && not complete then Transcript.atom (1024+12*String.length state.stamp+12*String.length piece.key) (\_ -> span [attribute "data-sealed-body-needed" piece.key,attribute "data-body-format" "markdown",attribute "role" "status"] [text "Loading message…"])
                        else if complete then Transcript.formattedAst sessionMarkdownRenderer source sealed.markdown.ast
                        else if content.format == "markdown" then Transcript.formattedAst sessionMarkdownRenderer source piece.ast
                        else sealedBlockFragment (piece.key ++ ":content") (if content.format == "json" then "tool" else piece.kind) source content.tokens)
                    parts = Dict.get piece.key sealed.bodies |> Maybe.withDefault [content]
                    firstPart = List.head parts |> Maybe.map .part |> Maybe.withDefault 0
                    lastPart = List.reverse parts |> List.head |> Maybe.withDefault content
                    omitted part anchor = Transcript.atom 1024 (\_ -> span [attribute "data-body-gap" piece.key,attribute "data-body-part" (String.fromInt part),attribute "data-body-anchor" (String.fromInt anchor),Html.Attributes.style "display" "block",Html.Attributes.style "height" "512px",attribute "aria-hidden" "true"] [])
                    entries = (if firstPart>0 then [("before",omitted (firstPart-1) firstPart)] else [])
                        ++ List.map (\part -> ("part:" ++ String.fromInt part.part,Transcript.scoped (piece.key ++ ":body:" ++ String.fromInt part.part) (Transcript.keyedContainer 128 (Keyed.node "div" [attribute "data-body-part-index" (String.fromInt part.part),attribute "data-body-key" piece.key]) [sealedBlockFragment (piece.key ++ ":body:" ++ String.fromInt part.part) "tool" part.text part.tokens]))) parts
                        ++ (if lastPart.more then [("after",omitted (lastPart.part+1) lastPart.part)] else [])
                    keyedChildren children = List.map (\(key,node) -> (String.toInt key |> Maybe.andThen (\index -> List.drop index entries |> List.head |> Maybe.map Tuple.first) |> Maybe.withDefault key,node)) children
                in if content.format=="json" && content.text=="" && not (Dict.member piece.key sealed.bodies) then Transcript.atom 1024 (\_ -> span [attribute "data-sealed-body-needed" piece.key,attribute "role" "status"] [text "Loading message…"])
                   else if content.format=="json" then Transcript.keyedContainer 128 (keyedChildren >> Keyed.node "div" [attribute "data-body-window" piece.key,attribute "data-body-parts" (List.map (.part >> String.fromInt) parts |> String.join ",")]) (List.map Tuple.second entries)
                   else Transcript.keyedContainer 128 (Keyed.node "div" []) [renderedBody]
        pieceView piece =
            Transcript.scoped piece.key (Transcript.keyedContainer (4096+24*String.length state.stamp+24*String.length piece.key+12*String.length piece.time+12*String.length piece.label+6*String.length piece.offset+6*String.length piece.role)
                (\children -> article [class ("session-record session-role-" ++ piece.role),attribute "data-offset" piece.offset,attribute "data-block-key" piece.key]
                    [div [class "session-record-meta"] [time [attribute "datetime" piece.time] [text (compactTimestamp model piece.time)],span [] [text (if piece.role == "user" then "You" else if piece.role == "assistant" then "Assistant" else if piece.role == "tool" then "Tool" else "Message")],if piece.body |> Maybe.map .exception |> Maybe.withDefault False then span [class "session-structured-json"] [text "Large structured reasoning · exact JSON"] else text ""]
                    ,if piece.kind == "text" then Keyed.node "div" [] children
                     else if piece.kind == "encrypted" then em [] [text "Thinking..."]
                     else div [] [Keyed.node "details" ([class ("session-content-" ++ piece.kind),attribute "data-event-key" piece.key,attribute "data-sealed-key" piece.key,attribute "data-sealed-owner" state.stamp] ++ openAttribute piece.open)
                            (("summary",summary [] [text (if piece.label == "" then "Thinking" else piece.label)])::children)]]) (if piece.kind=="text" || piece.open then [body piece] else []))
        runs = List.foldr (\piece accumulated -> case accumulated of
            run::rest -> if piece.group /= "" && (List.head run |> Maybe.map (.group >> (==) piece.group) |> Maybe.withDefault False) then (piece::run)::rest else [piece]::accumulated
            [] -> [[piece]]) [] sealed.rows
        runView run = case run of
            [] -> Transcript.atom 0 (\_ -> text "")
            first::_ ->
                if first.group == "" then pieceView first
                else
                    let
                        content=List.map (\piece -> (piece.key,(piece.key,pieceView piece))) run
                        gaps=List.filter (.group >> (==) first.group) state.historyGaps |> List.map (\gap -> (gap.before,("gap:" ++ gap.before ++ ":" ++ gap.after,Transcript.atom 1024 (\_ -> historyGap gap))))
                        entries=List.sortBy Tuple.first (content ++ gaps) |> List.map Tuple.second
                    in Transcript.keyedContainer (2048+12*String.length first.group+12*String.length state.stamp)
                    (\children -> details ([class "session-activity-group",attribute "data-group-key" first.group,attribute "data-sealed-key" ("group:" ++ first.group),attribute "data-sealed-owner" state.stamp] ++ openAttribute first.groupOpen)
                        [summary [class "session-activity-summary"] [activityTimestamp model first.endTime,text ("Activity · " ++ first.count ++ " events")]
                        ,Keyed.node "div" [class "session-activity-records"] (List.map (\(key,node) -> (String.toInt key |> Maybe.andThen (\index -> List.drop index entries |> List.head |> Maybe.map Tuple.first) |> Maybe.withDefault key,node)) children)]) (if first.groupOpen then List.map Tuple.second entries else [])
        groups=Set.fromList (List.map .group sealed.rows)
        rootContent=List.filterMap (\run -> List.head run |> Maybe.map (\piece -> (piece.key,(if piece.group=="" then "piece:" ++ piece.key else "group:" ++ piece.group,runView run)))) runs
        rootOmitted=List.filter (\gap -> gap.group=="" || not (Set.member gap.group groups)) state.historyGaps |> List.map (\gap -> (gap.before,("gap:" ++ gap.before ++ ":" ++ gap.after,Transcript.atom 1024 (\_ -> historyGap gap))))
        renderEntries=List.sortBy Tuple.first (rootContent ++ rootOmitted) |> List.map Tuple.second
        structured=List.any (\piece -> piece.body |> Maybe.map (\content -> content.structured || content.exception) |> Maybe.withDefault False) sealed.rows
        rendered = Transcript.renderWindow (if structured then 524288 else 1048576) sealed.renderPart (if state.following then "" else state.presentationAnchor) (List.map Tuple.second renderEntries)
        message = case sealed.phase of
            "ready" -> if List.isEmpty sealed.rows then "This session has no messages." else "Verified complete archive."
            "cancelled" -> "Archive loading cancelled."
            "downloading" -> "Downloading complete archive… " ++ sealed.compressedBytes ++ " bytes"
            "decoding" -> "Verifying and indexing… " ++ sealed.rawBytes ++ " bytes"
            "navigating" -> "Reading local index…"
            "reading-body" -> "Reading the complete formatted message…"
            "error" -> Maybe.withDefault "Archive could not load." sealed.error
            _ -> "Preparing complete archive…"
    in
    let
        identity = if workspaceReader then session |> Maybe.map viewSessionIdentity |> Maybe.withDefault (text "") else text ""
        status = p [class (if workspaceReader then "session-archive-status card-meta" else ""),attribute "role" (if sealed.error == Nothing then "status" else "alert"),attribute "aria-live" "polite"] [text message]
        lastProseMore = List.reverse sealed.rows |> List.head |> Maybe.map (\piece -> piece.kind == "text" && (piece.body |> Maybe.map .more |> Maybe.withDefault False) && not (sealed.markdown.complete && sealed.markdown.key == piece.key)) |> Maybe.withDefault False
        records = Keyed.node "div" [Html.Attributes.id "session-records",class "session-records",tabindex 0,attribute "data-viewer-owner" state.stamp,attribute "data-sealed-dom-owner" state.stamp,attribute "data-sealed-dom-load" (String.fromInt state.generation),attribute "data-sealed-first" sealed.first,attribute "data-sealed-last" sealed.last,attribute "data-sealed-page-request" (String.fromInt sealed.pageRequest),attribute "data-sealed-page-first" sealed.pageFirst,attribute "data-sealed-page-last" sealed.pageLast,attribute "data-sealed-scan-first" sealed.scanFirst,attribute "data-sealed-scan-last" sealed.scanLast,attribute "data-sealed-at-beginning" (if sealed.atBeginning then "true" else "false"),attribute "data-sealed-older-boundary" (List.take 20 sealed.rows |> List.reverse |> List.head |> Maybe.map .key |> Maybe.withDefault sealed.first),attribute "data-sealed-newer-boundary" (List.drop (max 0 (List.length sealed.rows-20)) sealed.rows |> List.head |> Maybe.map .key |> Maybe.withDefault sealed.last),attribute "data-render-request" (String.fromInt sealed.renderRequest),attribute "data-render-part" (String.fromInt rendered.part),attribute "data-render-more" (if rendered.more then "true" else "false"),attribute "data-archive-can-previous" (if sealed.canPrevious then "true" else "false"),attribute "data-archive-can-next" (if sealed.canNext then "true" else "false"),attribute "data-last-prose-more" (if lastProseMore then "true" else "false"),attribute "data-latest-request" (String.fromInt state.latestRequest),attribute "data-latest-pending" (if state.latestPending then "true" else "false"),attribute "data-latest-error" (if state.latestError /= Nothing then "true" else "false"),attribute "data-history-mode" "sealed",attribute "data-presentation-anchor" state.presentationAnchor,attribute "data-following" (if state.following then "true" else "false"),attribute "aria-label" "Verified archive records"] (List.map2 (\key node -> (state.stamp ++ ":" ++ (String.toInt key |> Maybe.andThen (\index -> List.drop index renderEntries |> List.head |> Maybe.map Tuple.first) |> Maybe.withDefault key),node)) rendered.keys rendered.nodes)
        budgetError = if rendered.error then p [attribute "role" "alert"] [text "This formatted piece exceeds the local presentation budget."] else text ""
        children = [identity,status,viewLatestError state,budgetError,records,footer]
    in
    Keyed.node "section" [class ("session-viewer session-sealed-viewer" ++ (if workspaceReader then " session-sealed-workspace" else "")),attribute "aria-label" "Session transcript"]
        (sealedHostNodes model (children ++ [viewLatestControl state]))


sealedHostKey : Model -> String
sealedHostKey model =
    Encode.encode 0 (Encode.list Encode.string [model.sessions.stamp,String.fromInt model.sessions.generation,model.sessions.sealed.domEpoch])


sealedHostNodes : Model -> List (Html Msg) -> List (String,Html Msg)
sealedHostNodes model =
    List.indexedMap (\index node -> (sealedHostKey model ++ ":" ++ String.fromInt index,node))


sealedBlockFragment : String -> String -> String -> List (String,String) -> Transcript.Fragment Msg
sealedBlockFragment scope kind content tokens =
    if kind == "text" || kind == "thinking" then Transcript.formatted sessionMarkdownRenderer content
    else
        let
            chunks tokenKind value start = if value=="" then [] else
                let prefix=String.left 8192 value
                    code=String.right 1 prefix |> String.uncons |> Maybe.map (Tuple.first >> Char.toCode) |> Maybe.withDefault 0
                    following=String.slice 8192 8193 value |> String.uncons |> Maybe.map (Tuple.first >> Char.toCode) |> Maybe.withDefault 0
                    length=String.length prefix-(if code>=55296 && code<=56319 && following>=56320 && following<=57343 then 1 else 0)
                    part=String.left length value
                in Transcript.scoped (scope ++ ":text:" ++ String.fromInt start) (Transcript.atom (256+6*String.length part+6*String.length tokenKind) (\_ -> span [class ("json-" ++ tokenKind),attribute "data-payload-start" (String.fromInt start)] [text part])) :: chunks tokenKind (String.dropLeft length value) (start+length)
            indexed = List.foldl (\(tokenKind,value) (start,entries) -> (start+String.length value,(tokenKind,value,start)::entries)) (0,[]) (if List.isEmpty tokens then [("plain",content)] else tokens) |> Tuple.second |> List.reverse
        in Transcript.keyedContainer 1024 (\children -> pre [class "session-tool-content"] [Keyed.node "code" [class "language-json"] children])
            (List.concatMap (\(tokenKind,value,start) -> chunks tokenKind value start) indexed)


activityTimestamp : Model -> String -> Html Msg
activityTimestamp model source =
    if source == "" then span [class "session-event-time",attribute "aria-label" "Activity time unavailable"] [text "Time unavailable"]
    else time [class "session-event-time",attribute "datetime" source] [text (compactTimestamp model source)]


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
        selected = model.sessions.workspace |> Maybe.andThen (\workspace -> model.sessions.selected |> Maybe.andThen (\id -> resource workspace (Delivery.SessionMetadata id) model |> Maybe.andThen .data)) |> Maybe.map (\value -> case value of
            Delivery.Metadata session -> [sessionActivity session]
            _ -> []) |> Maybe.withDefault []
        transcript = List.map .time model.sessions.rows ++ List.map .time model.sessions.sealed.rows ++ List.map .endTime model.sessions.sealed.rows
    in
    if model.activeTab == SessionsTab then list ++ selected ++ transcript else if model.activeTab == ProjectsTab then cards ++ (if model.sessions.taskPanel /= Nothing then selected ++ transcript else []) else []


availableSessions : Model -> List Api.AgentSession
availableSessions model =
    (Dict.values model.sessionDelivery.resources |> List.filter (\entry -> Just entry.workspace == model.sessions.workspace) |> List.concatMap (\entry -> case entry.data of
        Just (Delivery.Metadata session) -> [session]
        Just (Delivery.SessionsPage page) -> page.items
        _ -> [])) ++ (Dict.values model.sessions.taskPages |> List.concatMap (\cached -> Result.toMaybe cached.result |> Maybe.map .items |> Maybe.withDefault []))


sessionActivity : Api.AgentSession -> String
sessionActivity session =
    session.lastActivityAt |> identityValue |> Maybe.withDefault ""


viewSessionActivity : Model -> Api.AgentSession -> Html Msg
viewSessionActivity model session =
    let source = sessionActivity session in
    span [class "session-latest-activity"]
        [text "Last activity · ",if source == "" then span [attribute "aria-label" "Last activity unavailable"] [text "Unavailable"]
         else time [attribute "datetime" source] [text (compactTimestamp model source)]]


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


