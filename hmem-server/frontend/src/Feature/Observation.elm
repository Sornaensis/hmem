module Feature.Observation exposing
    ( ObservationPathGroup
    , ObservationSubjectGroup
    , applyCanonicalObservation
    , applyAuthoritativeObservation
    , canLoadMore
    , canLoadMoreFacets
    , clearSelection
    , deleteDialogFocusTarget
    , historyResponseMatches
    , refreshHistory
    , detailResponseMatches
    , failResultPage
    , facetKey
    , facetQuery
    , facetResponseMatches
    , groupPathMatches
    , init
    , pageSize
    , countQuery
    , hasAppliedCountFilter
    , syncCounts
    , invalidateCounts
    , validPage
    , retireSessionState
    , hasProtectedEdit
    , hasUnappliedFilters
    , filterValidationError
    , filtersPending
    , modeKey
    , treeQuery
    , loadTreeBranch
    , isLoadedOrSelected
    , markResultsStale
    , listQuery
    , matchGroupKey
    , matchQuery
    , matchResponseMatches
    , mergeFacetPage
    , mutationResponseMatches
    , normalizeMatchPaths
    , observationCardDomId
    , observationContentError
    , preferNewerObservation
    , queryFingerprint
    , reconcileCurationPermission
    , reconcileDeletedObservation
    , refreshActiveResults
    , acceptResultRefresh
    , continueAutomaticRefresh
    , completeSupersededRead
    , restoreRouteResults
    , resumeForegroundResults
    , restorePendingPreferences
    , bootstrapObservation
    , refuseContextExit
    , reload
    , removeObservation
    , selectObservation
    , selectRouteObservation
    , selectionForTab
    , startReload
    , startReloadForSession
    , update
    , viewSearchDetail
    , viewObservations
    , viewObservationsState
    , viewObservationsStateWithPermission
    , viewRetainedDraft
    , refreshViewport
    , rememberPreferences
    , updateViewport
    , projectResultRows
    , timestampSources
    )

import ActorAttribution
import Api
import CardSections
import Array
import Char
import Dict
import Helpers exposing (beginTrackedMutation, focusElement, plainTextExcerpt, replaceFragment)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (custom, onClick, onInput, onSubmit, stopPropagationOn)
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import HierarchyViewport
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Permissions
import StartupAdmission
import ObservationViewport
import ObservationPreferences as Preferences
import ObservationTree
import Process
import Task
import Ports exposing (copyToClipboard)
import Set
import Toast exposing (addToast)
import TimestampDisplay
import Types exposing (..)
import Url


init : ObservationModel
init =
    { items = Dict.empty
    , workspaceCounts = emptyCounts
    , counts = emptyCounts
    , resultRows = Array.empty
    , viewport = ObservationViewport.init
    , orderedIds = []
    , hasMore = False
    , loading = False
    , resultsStale = False
    , refreshPass = Nothing
    , refreshPending = False
    , refreshError = Nothing
    , expandedSubjects = Dict.empty
    , preferenceOwner = Nothing
    , preferenceValue = Preferences.empty
    , preferenceHydrated = False
    , preferencePendingDetail = False
    , preferenceEntryEcho = Nothing
    , preferenceEntryHistory = Nothing
    , preferenceTouch = 0
    , nextPreferenceRequest = 1
    , error = Nothing
    , query = ""
    , subjectQuery = ""
    , subjectKind = Nothing
    , subject = ""
    , selectedFacet = Nothing
    , gitSha = ""
    , currentGitSha = ""
    , historyGitSha = ""
    , requestMode = ObservationFacetMode
    , fileComposerOpen = False
    , advancedFiltersOpen = False
    , appliedQuery = Nothing
    , completedQuery = Nothing
    , modeQueries = Dict.empty
    , modeSubjectQueries = Dict.empty
    , filterComposing = False
    , clearViewportToken = 0
    , discovery = { sessionId = "", runtimeToken = 1, runtimePending = False, generation = 0, query = Nothing, owner = "", context = Nothing, bootstrap = Nothing, afterBootstrap = Nothing, physical = [], error = Nothing, failedCount = False }
    , modeScrolls = Dict.empty
    , preferenceScroll = Nothing
    , filterDebounce = Nothing
    , nextFilterToken = 1
    , tree = ObservationTree.init
    , linkNotice = Nothing
    , nextLinkToken = 1
    , pendingExcludedLink = Nothing
    , failedRequest = Nothing
    , matchPathsInput = ""
    , matchAppliedPaths = []
    , matchValidationError = Nothing
    , matchEvidence = Dict.empty
    , expandedMatchGroups = Dict.empty
    , browseReturn = Nothing
    , requestGeneration = 0
    , requestSessionEpoch = 0
    , queryFingerprint = ""
    , expectedOffset = Nothing
    , nextOffset = 0
    , facets = Dict.empty
    , facetKeys = []
    , facetHasMore = False
    , facetLoading = False
    , facetError = Nothing
    , facetRequestGeneration = 0
    , facetRequestSessionEpoch = 0
    , facetFingerprint = ""
    , facetExpectedOffset = Nothing
    , facetNextOffset = 0
    , selectedId = Nothing
    , inlineOwner = Nothing
    , selectedDetail = Nothing
    , history = Nothing
    , historyExpanded = False
    , nextHistoryRequestToken = 1
    , detailLoading = False
    , detailError = Nothing
    , activeDetailRequest = Nothing
    , nextDetailRequestToken = 1
    , treeSelectionPending = False
    , detailNavigationEpoch = 0
    , detailNavigationToken = 0
    , searchOriginDetail = False
    , detailReturnTarget = Nothing
    , pendingReturnNavigation = Nothing
    , edit = Nothing
    , deleteConfirmation = Nothing
    , nextCurationContextToken = 1
    , nextMutationRequestToken = 1
    }


pageSize : Int
pageSize =
    -- Files and legacy direct-page callers retain their historical bound.
    -- Discovery transport is independently capped at 50 in its encoder/decoder.
    200


-- A selected typed terminal chooses a view inside the accepted context; it is
-- not a new predicate. Inactive manual All fields likewise cannot filter Tree.
discoveryQuery : ObservationAppliedQuery -> ObservationAppliedQuery
discoveryQuery query =
    if treeMode query.requestMode then { query | requestMode = ObservationFacetMode, selectedFacet = Nothing, subject = "" }
    else query


discoveryFilters : ObservationAppliedQuery -> Api.ObservationDiscoveryFilters
discoveryFilters query =
    { subjectQuery = query.subjectQuery, contentQuery = query.query
    , subjectKind = query.subjectKind
    , subject = if query.requestMode == ObservationFlatMode then nonEmpty query.subject else Nothing
    , gitSha = nonEmpty query.gitSha, currentGitSha = nonEmpty query.currentGitSha, historyGitSha = nonEmpty query.historyGitSha }


discoveryWireQuery : ObservationDiscoveryRequest -> Api.ObservationDiscoveryQuery
discoveryWireQuery request =
    let
        selection = case request.destination of
            DiscoveryFlat _ _ _ value -> { view = "list", prefix = "", terminal = Nothing, offset = value }
            DiscoveryCount _ _ -> { view = "count", prefix = "", terminal = Nothing, offset = 0 }
            DiscoveryTree tree -> case tree.kind of
                Nothing -> { view = "tree", prefix = tree.prefix, terminal = Nothing, offset = tree.offset }
                Just kind -> { view = "list", prefix = "", terminal = Just { subjectKind = kind, subject = tree.prefix }, offset = tree.offset }
    in
    { workspaceId = request.workspaceId, searchSession = request.sessionId, generation = request.generation, contextToken = request.contextToken
    , filters = discoveryFilters request.query, view = selection.view, prefix = selection.prefix, terminal = selection.terminal, limit = 50, offset = selection.offset }


retireDiscoveryDraft : ObservationModel -> ObservationModel
retireDiscoveryDraft state =
    let discovery = state.discovery
        query = discoveryQuery (draftQuery state)
    in
    if discovery.query == Just query then state
    else { state | discovery = { discovery | query = Just query, generation = discovery.generation + 1, context = Nothing, bootstrap = Nothing, afterBootstrap = Nothing, error = Nothing } }


discoveryCountKey : ObservationDiscoveryRequest -> String
discoveryCountKey request =
    "discovery-count|" ++ Encode.encode 0 (Encode.list Encode.string [request.owner, request.sessionId, String.fromInt request.generation, String.fromInt request.countGeneration, String.fromInt request.workspaceCountGeneration])


sendDiscovery : ObservationDiscoveryRequest -> Model -> ( Model, Cmd Msg )
sendDiscovery request model =
    let state = model.observations
        discovery = state.discovery
        loading = model.dataLoading
        admitted = case request.destination of
            DiscoveryCount _ _ -> StartupAdmission.admit (discoveryCountKey request) (StartupAdmission.reconcile (Just {workspaceId = request.workspaceId, sessionEpoch = request.epoch, audience = countActor model}) loading.startupAdmission)
            _ -> Just loading.startupAdmission
    in
    if List.member request discovery.physical || List.length discovery.physical >= 4 then ( model, Cmd.none )
    else case admitted of
        Nothing -> ( model, Cmd.none )
        Just admission ->
            let activate guard counts = {counts | active = Just guard, nextToken = Basics.max counts.nextToken (guard.token + 1), pending = False}
                started = case request.destination of
                    DiscoveryCount scoped workspace -> {state | counts = activate scoped state.counts, workspaceCounts = activate workspace state.workspaceCounts}
                    _ -> state
            in
            ( { model | observations = { started | discovery = { discovery | physical = request :: discovery.physical } }, dataLoading = {loading | startupAdmission = admission} }
            , Api.fetchObservationDiscovery model.flags.apiUrl (discoveryWireQuery request) (GotObservationDiscovery request) )


pumpDiscovery : Model -> ( Model, Cmd Msg )
pumpDiscovery model =
    let discovery = model.observations.discovery in
    case discovery.bootstrap of
        Just request ->
            if model.auth.status == AuthReady && Permissions.canReadCurrentWorkspace model && model.workspaceAdmin.authorizationPending == Nothing && not (filtersPending model.observations) && discovery.sessionId /= "" && request.owner == treeOwner model && request.generation == discovery.generation then sendDiscovery request model else ( model, Cmd.none )
        Nothing -> ( model, Cmd.none )


requestDiscovery : ObservationDiscoveryDestination -> Model -> ( Model, Cmd Msg )
requestDiscovery destination model =
    let
        state = model.observations
        old = state.discovery
        query = discoveryQuery (querySnapshot state)
        owner = treeOwner model
        matching = old.owner == owner && old.query == Just query
        current = if matching then old else { old | owner = owner, query = Just query, generation = old.generation + 1, context = Nothing, bootstrap = Nothing, afterBootstrap = Nothing, error = Nothing }
        guarded = { model | observations = { state | discovery = current } }
        base = { workspaceId = Maybe.withDefault "" model.selectedWorkspaceId, epoch = model.sessionRequestEpoch, owner = owner, sessionId = current.sessionId
            , generation = current.generation, query = query, contextToken = Maybe.map .token current.context, destination = destination
            , countGeneration = state.counts.generation, workspaceCountGeneration = state.workspaceCounts.generation }
    in
    if model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) || filtersPending state || current.error /= Nothing then ( guarded, Cmd.none )
    else case current.context of
        Just _ -> sendDiscovery base guarded
        Nothing -> case current.bootstrap of
            Just bootstrapRequest ->
                case ( bootstrapRequest.destination, destination ) of
                    ( DiscoveryCount _ _, DiscoveryCount _ _ ) -> pumpDiscovery guarded
                    ( DiscoveryCount _ _, _ ) -> pumpDiscovery (updateObservation (\currentState -> let d = currentState.discovery in {currentState | discovery = {d | afterBootstrap = Just destination}}) guarded)
                    ( DiscoveryFlat _ _ _ offset, DiscoveryFlat _ _ _ nextOffset ) ->
                        if offset == nextOffset && bootstrapRequest.destination /= destination then
                            pumpDiscovery (updateObservation (\currentState -> let d = currentState.discovery in {currentState | discovery = {d | afterBootstrap = Just destination}}) guarded)
                        else pumpDiscovery guarded
                    _ -> pumpDiscovery guarded
            Nothing ->
                let
                    rollover = current.generation >= 2147483647
                    generation = if rollover then 1 else Basics.max 1 current.generation
                    session = if rollover then "" else current.sessionId
                    request = { base | generation = generation, sessionId = session, contextToken = Nothing }
                    discovery = { current | generation = generation, sessionId = session, bootstrap = Just request, runtimeToken = if rollover then current.runtimeToken + 1 else current.runtimeToken, runtimePending = if rollover then False else current.runtimePending }
                    prepareCount counts = if counts.current then counts else { counts | pending = True, error = Nothing }
                    prepared = { model | observations = { state | discovery = discovery, counts = prepareCount state.counts, workspaceCounts = prepareCount state.workspaceCounts } }
                in
                if session == "" then
                    if discovery.runtimePending then ( prepared, Cmd.none )
                    else
                        ( { prepared | observations = { state | discovery = { discovery | runtimePending = True } } }
                        , Ports.observationSearchRuntime (Encode.object [ ( "runtime", Encode.string model.flags.sessionId ), ( "token", Encode.int discovery.runtimeToken ) ]) )
                else sendDiscovery request prepared


acceptDiscoveryRuntime : Encode.Value -> Model -> ( Model, Cmd Msg )
acceptDiscoveryRuntime payload model =
    let discovery = model.observations.discovery
        decoder = Decode.map3 (\runtime token uuid -> ( runtime, token, uuid )) (Decode.field "runtime" Decode.string) (Decode.field "token" Decode.int) (Decode.field "uuid" Decode.string)
    in case Decode.decodeValue decoder payload of
        Ok ( runtime, token, uuid ) ->
            if runtime /= model.flags.sessionId || token /= discovery.runtimeToken || not discovery.runtimePending || not (Preferences.uuid uuid) then ( model, Cmd.none )
            else
                let state = model.observations
                    ready = { discovery | sessionId = uuid, runtimePending = False, bootstrap = Maybe.map (\request -> { request | sessionId = uuid }) discovery.bootstrap }
                in pumpDiscovery { model | observations = { state | discovery = ready } }
        Err _ -> ( model, Cmd.none )


settleDiscoveryPage : ObservationDiscoveryRequest -> Result Http.Error Api.ObservationDiscoveryResponse -> Model -> ( Model, Cmd Msg )
settleDiscoveryPage request result model =
    case request.destination of
        DiscoveryCount _ _ -> ( model, Cmd.none )
        DiscoveryTree tree ->
            if tree.kind == Nothing then updateRaw (GotObservationTree tree (result |> Result.andThen (\value -> value.page.tree |> Maybe.map Ok |> Maybe.withDefault (Err (Http.BadBody "Missing discovery tree"))))) model
            else updateRaw (GotObservationTreeTerminal tree (Result.map (\value -> { items = value.page.items, hasMore = value.page.hasMore }) result)) model
        DiscoveryFlat token generation fingerprint offset ->
            ( model, Task.perform identity (Task.succeed (GotObservations request.workspaceId token generation fingerprint offset (Result.map (\value -> { items = value.page.items, hasMore = value.page.hasMore }) result))) )


settleDiscoveryDestinations request latest result model =
    let (settled, command) = settleDiscoveryPage request result model in
    case (request.destination, latest) of
        (DiscoveryFlat _ _ _ offset, Just (DiscoveryFlat token generation fingerprint nextOffset)) ->
            if offset == nextOffset then
                let (_, currentCommand) = settleDiscoveryPage {request | destination = DiscoveryFlat token generation fingerprint nextOffset} result settled in
                (settled, Cmd.batch [command, currentCommand])
            else (settled, command)
        _ -> (settled, command)


acceptDiscovery : ObservationDiscoveryRequest -> Result Http.Error Api.ObservationDiscoveryResponse -> Model -> ( Model, Cmd Msg )
acceptDiscovery request result incoming =
    let
        state = incoming.observations
        current = state.discovery
        discovery = { current | physical = List.filter ((/=) request) current.physical }
        loading = incoming.dataLoading
        releaseCount guard counts = if counts.active == Just guard then {counts | active = Nothing} else counts
        releasedState = case request.destination of
            DiscoveryCount scoped workspace -> {state | counts = releaseCount scoped state.counts, workspaceCounts = releaseCount workspace state.workspaceCounts}
            _ -> state
        model = { incoming | observations = { releasedState | discovery = discovery }, dataLoading = case request.destination of
            DiscoveryCount _ _ -> {loading | startupAdmission = StartupAdmission.complete (discoveryCountKey request) loading.startupAdmission}
            _ -> loading }
        owned = incoming.auth.status == AuthReady && Permissions.canReadCurrentWorkspace incoming && incoming.selectedWorkspaceId == Just request.workspaceId && incoming.sessionRequestEpoch == request.epoch
            && request.owner == treeOwner incoming && discovery.owner == request.owner && discovery.sessionId == request.sessionId && discovery.generation == request.generation && discovery.query == Just request.query
            && (case request.contextToken of
                Nothing -> discovery.bootstrap == Just request
                Just token -> Maybe.map .token discovery.context == Just token)
            && (case request.destination of
                DiscoveryCount scoped workspace -> state.counts.active == Just scoped || state.workspaceCounts.active == Just workspace
                _ -> True)
    in
    if not owned then
        case request.destination of
            DiscoveryTree treeRequest ->
                let tree = model.observations.tree in
                ( updateObservation (\currentState -> { currentState | tree = { tree | physical = List.filter ((/=) treeRequest) tree.physical } }) model, Cmd.none )
            _ -> settleDiscoveryPage request (Err Http.NetworkError) model
    else case result of
        Err (Http.BadStatus 409) ->
            case request.destination of
                DiscoveryCount _ _ ->
                    let reset = {discovery | context = Nothing, bootstrap = Nothing, generation = discovery.generation + 1, error = Nothing}
                        pending counts = {counts | active = Nothing, current = False, pending = True, error = Nothing}
                    in syncDiscoveryCounts {model | observations = {releasedState | discovery = reset, counts = pending releasedState.counts, workspaceCounts = pending releasedState.workspaceCounts, resultsStale = True}}
                _ -> restartDiscoveryPage request discovery state model
        Err error ->
            let message = if error == Http.BadStatus 429 then "Search capacity is busy. Retry the current filters." else "Observation search failed. Retry the current filters."
                failed = { discovery | bootstrap = Nothing, afterBootstrap = case request.destination of
                    DiscoveryCount _ _ -> discovery.afterBootstrap
                    _ -> Nothing
                    , generation = if request.contextToken == Nothing then discovery.generation + 1 else discovery.generation, error = Just message, failedCount = case request.destination of
                    DiscoveryCount _ _ -> True
                    _ -> False }
                settleCount counts = if counts.current then counts else { counts | pending = False, active = Nothing, error = Just "Observation search did not complete. Retry the current filters." }
            in settleDiscoveryDestinations request discovery.afterBootstrap (Err error) { model | observations = { state | discovery = failed, counts = settleCount releasedState.counts, workspaceCounts = settleCount releasedState.workspaceCounts } }
        Ok response ->
            if List.length response.page.items > 50 || (response.page.tree |> Maybe.map (\tree -> List.length tree.items > 50) |> Maybe.withDefault False) || response.page.counts.workspaceId /= request.workspaceId || (request.contextToken /= Nothing && request.contextToken /= Just response.contextToken) then
                acceptDiscovery request (Err (Http.BadBody "Discovery response did not match this workspace or context")) incoming
            else
                let
                    settled = { discovery | bootstrap = Nothing, afterBootstrap = Nothing, error = Nothing, context = Just { token = response.contextToken, expiresAt = response.expiresAt, retrieval = response.retrieval } }
                    scoped = state.counts
                    workspace = state.workspaceCounts
                    countOwner = Just { workspaceId = request.workspaceId, sessionEpoch = request.epoch, actor = countActor incoming, token = 0, generation = 0, fingerprint = "" }
                    counts = if request.countGeneration /= scoped.generation then releasedState.counts else { scoped | owner = countOwner, value = Just response.page.counts, valueFingerprint = countFingerprint request.workspaceId state, settledFingerprint = countFingerprint request.workspaceId state, current = True, pending = False, error = Nothing, active = Nothing }
                    totals = response.page.counts
                    workspaceCounts = if request.workspaceCountGeneration /= workspace.generation then releasedState.workspaceCounts else { workspace | owner = countOwner, value = Just { totals | matchCount = totals.totalCount }, valueFingerprint = "workspace:" ++ request.workspaceId, settledFingerprint = "workspace:" ++ request.workspaceId, current = True, pending = False, error = Nothing, active = Nothing }
                    accepted = { model | observations = { releasedState | discovery = {settled | afterBootstrap = Nothing}, counts = counts, workspaceCounts = workspaceCounts } }
                in case (request.destination, discovery.afterBootstrap) of
                    (DiscoveryCount _ _, Just destination) -> requestDiscovery destination accepted
                    _ -> settleDiscoveryDestinations request discovery.afterBootstrap (Ok response) accepted


restartDiscoveryPage request discovery state model =
    let
        retired = { discovery | context = Nothing, bootstrap = Nothing, generation = discovery.generation + 1, error = Nothing }
        tree = state.tree
        reset = { state | discovery = retired, loading = False, facetLoading = False, refreshPass = Nothing, refreshPending = False, refreshError = Nothing, nextFilterToken = state.nextFilterToken + 1
            , requestGeneration = state.requestGeneration + 1, facetRequestGeneration = state.facetRequestGeneration + 1, tree = ObservationTree.retire ("discovery-restart-" ++ String.fromInt retired.generation) tree }
        ( released, releaseCmd ) = settleDiscoveryPage request (Err Http.NetworkError) { model | observations = reset }
        ( restarted, restartCmd ) = if treeMode reset.requestMode then startTree True released else refreshActiveResultsRaw released
    in ( restarted, Cmd.batch [ releaseCmd, restartCmd ] )


treeMode : ObservationRequestMode -> Bool
treeMode mode = List.member mode [ ObservationFacetMode, ObservationExactSubjectMode ]


treePathKey : String -> String
treePathKey path = "tree-path:" ++ path


treeCardContext : Api.ObservationSubject -> String
treeCardContext subject = "tree-terminal:" ++ ObservationTree.terminalKey subject


ownedTree : Model -> ObservationTree.State
ownedTree model =
    let tree = model.observations.tree
        lifetime = if model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) || model.workspaceAdmin.authorizationPending /= Nothing then "unavailable" else treeLifetime model
        owned = if tree.lifetime == lifetime then tree else ObservationTree.retire lifetime tree
    in if lifetime == "unavailable" then {owned | resolvedPath = Nothing} else owned


{-| Ordinary Tree demand belongs to its visible tab. A current explicit reveal
may still resolve its own ancestor chain and typed terminal; cached expansions
never turn that exception into an invisible backlog.
-}
treeDemandAllowed : String -> Maybe Api.SubjectKind -> Model -> Bool
treeDemandAllowed prefix kind model =
    if model.activeTab == ObservationsTab then True
    else
        let state = model.observations in
        state.tree.lifetime == treeLifetime model && (state.tree.revealTarget |> Maybe.map (\target ->
            target.focus
                && (case target.observationId of
                    Just identity -> state.selectedId == Just identity
                    Nothing -> state.selectedFacet == Just target.subject)
                && (case kind of
                    Nothing -> prefix == "" || List.member prefix (ObservationTree.ancestors target.subject.subject)
                    Just value -> prefix == target.subject.subject && value == target.subject.subjectKind)) |> Maybe.withDefault False)


loadTreeTerminal : Api.ObservationSubject -> Int -> Model -> (Model, Cmd Msg)
loadTreeTerminal subject offset model =
    case repositoryWorkspaceId model of
        Nothing -> (model, Cmd.none)
        Just workspaceId ->
            if model.observations.discovery.context == Nothing || List.length model.observations.discovery.physical >= 4 || not (treeDemandAllowed subject.subject (Just subject.subjectKind) model) || model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) || model.workspaceAdmin.authorizationPending /= Nothing || filtersPending model.observations || not (treeMode model.observations.requestMode) then (model, Cmd.none)
            else
                let state = model.observations
                    owned = if state.tree.lifetime == treeLifetime model then state.tree else ObservationTree.retire (treeLifetime model) state.tree
                    (tree, request) = ObservationTree.beginTerminal subject offset owned
                    query = listQuery workspaceId offset state
                    exact = {query | subjectKind = Just subject.subjectKind, subject = Just subject.subject, query = Nothing}
                in case request of
                    Just guard -> requestDiscovery (DiscoveryTree guard) { model | observations = { state | tree = tree } }
                    Nothing -> ( model, Cmd.none )


startTree : Bool -> Model -> (Model, Cmd Msg)
startTree refresh model =
    let state = (if refresh then ensureAppliedQuery else commitAppliedQuery) (retireFilterReads model.observations)
        prepared = {model | observations = {state | nextFilterToken = state.nextFilterToken + 1, loading = False, facetLoading = False, refreshPass = Nothing, refreshPending = False, refreshError = Nothing}}
        current = prepared.observations
        owned = if current.tree.resolvedPath |> Maybe.map (\resolved -> resolved.owner /= treeOwner prepared) |> Maybe.withDefault False then let previousTree = current.tree in {previousTree | resolvedPath = Nothing} else current.tree
        retired = (if refresh then ObservationTree.revalidate else ObservationTree.retire) (treeLifetime prepared) owned
        tree =
            if refresh then
                case retired.revealTarget of
                    Just _ -> retired
                    Nothing ->
                        if current.selectedId == Nothing then current.selectedFacet |> Maybe.map (\subject -> if (retired.resolvedPath |> Maybe.map .subject) == Just subject then retired else ObservationTree.revealPath subject retired) |> Maybe.withDefault retired
                        else
                        let subject = case current.selectedFacet of
                                Just selected -> Just selected
                                Nothing -> currentSelectedObservation current |> Maybe.map (\observation -> {subjectKind = observation.subjectKind, subject = observation.subject})
                        in case (subject, current.selectedId) of
                            (Just selected, Just identity) -> {retired | revealTarget = Just {subject = selected, observationId = Just identity, focus = False}}
                            _ -> retired
            else if current.selectedId /= Nothing then
                current.selectedDetail |> Maybe.map (\observation ->
                    let subject = current.selectedFacet |> Maybe.withDefault {subjectKind = observation.subjectKind, subject = observation.subject}
                    in ObservationTree.reveal subject observation.id retired) |> Maybe.withDefault retired
            else if current.selectedId == Nothing then current.selectedFacet |> Maybe.map (\subject -> if (retired.resolvedPath |> Maybe.map .subject) == Just subject then retired else ObservationTree.revealPath subject retired) |> Maybe.withDefault retired else retired
    in pumpTree {prepared | observations = {current | tree = tree}}


pumpTree : Model -> (Model, Cmd Msg)
pumpTree model =
    let state = model.observations in
    if not (treeMode state.requestMode) || model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) || model.workspaceAdmin.authorizationPending /= Nothing || filtersPending state || state.refreshPending then (model, Cmd.none)
    else
        let
            tree = if state.tree.lifetime == treeLifetime model then state.tree else ObservationTree.retire (treeLifetime model) state.tree
            root = Dict.get "" tree.branches
            current branch = branch.lifetime == tree.lifetime
            needs prefix = Dict.get prefix tree.branches |> Maybe.map (\branch -> branch.request == Nothing && branch.error == Nothing && (not (current branch) || (prefix /= "" && branch.hasMore))) |> Maybe.withDefault True
            offset prefix = Dict.get prefix tree.branches |> Maybe.map (\branch -> if current branch || branch.stagedPaths /= Nothing then branch.nextOffset else 0) |> Maybe.withDefault 0
            reveal = tree.revealTarget |> Maybe.map (.subject >> .subject)
            missingReveal = reveal |> Maybe.andThen (ObservationTree.ancestors >> List.head) |> Maybe.map (\path -> not (Dict.member path tree.nodes)) |> Maybe.withDefault False
            rootMore = missingReveal && (root |> Maybe.map (\branch -> current branch && branch.hasMore && branch.request == Nothing && branch.error == Nothing) |> Maybe.withDefault False)
            ready = root |> Maybe.map current |> Maybe.withDefault False
            expanded = if ready then ObservationTree.visiblePaths tree |> List.filter (\path -> Set.member path tree.expanded) else []
            rootWork = if needs "" || rootMore then [("", Nothing)] else []
            branchOwners = expanded |> List.filter (\path -> Dict.get path tree.nodes |> Maybe.map .hasChildren |> Maybe.withDefault False) |> List.map (\path -> (path, Nothing))
            branches = rootWork ++ List.filter (Tuple.first >> needs) branchOwners
            subjects = expanded |> List.concatMap (\path -> Dict.get path tree.nodes |> Maybe.map .terminals |> Maybe.withDefault []) |> List.map (\terminal -> {subjectKind = terminal.subjectKind, subject = terminal.subject})
            terminals = subjects
                |> List.filter (\subject -> Dict.get (ObservationTree.terminalKey subject) tree.terminals |> Maybe.map (\terminal -> terminal.request == Nothing && terminal.error == Nothing && (terminal.lifetime /= tree.lifetime || (terminal.hasMore && (tree.revealTarget |> Maybe.map (\target -> target.subject.subjectKind == subject.subjectKind && target.subject.subject == subject.subject && (target.observationId |> Maybe.map (\identity -> not (List.any (\item -> item.id == identity) terminal.items)) |> Maybe.withDefault False)) |> Maybe.withDefault False)))) |> Maybe.withDefault True)
                |> List.map (\subject -> (subject.subject, Just subject.subjectKind))
            owners = branchOwners ++ List.map (\subject -> (subject.subject, Just subject.subjectKind)) subjects
            after = tree.lastOwner |> Maybe.andThen (\owner -> owners |> List.indexedMap Tuple.pair |> List.filter (Tuple.second >> (==) owner) |> List.head |> Maybe.map (Tuple.first >> (+) 1)) |> Maybe.withDefault 0
            -- Rotate the existing owner order before eligibility filtering, so an
            -- in-flight last owner cannot send a queued terminal back to the tail.
            work = (rootWork ++ ((List.drop after owners ++ List.take after owners) |> List.filter (\owner -> List.member owner (branches ++ terminals))))
                |> List.filter (\(prefix, kind) -> treeDemandAllowed prefix kind model)
            admit (prefix, kind) (source, accumulated) =
                let (next, command) = case kind of
                        Nothing -> loadTreeBranch prefix (offset prefix) source
                        Just value ->
                            let subject = {subjectKind = value, subject = prefix}
                                at = Dict.get (ObservationTree.terminalKey subject) source.observations.tree.terminals |> Maybe.map (\terminal -> if terminal.lifetime == tree.lifetime || terminal.stagedItems /= Nothing then terminal.nextOffset else 0) |> Maybe.withDefault 0
                            in loadTreeTerminal subject at source
                in (next, command :: accumulated)
            converged = if ready && List.isEmpty branches && List.isEmpty terminals && not (List.any (\request -> request.lifetime == tree.lifetime) tree.physical) then {tree | refreshing = False} else tree
            (loaded, commands) = List.foldl admit ({model | observations = {state | tree = converged}}, []) work
        in (loaded, Cmd.batch commands)


focusTreePath : String -> Model -> (Model, Cmd Msg)
focusTreePath path model =
    let state = model.observations
        tree = state.tree
    in ({model | observations = {state | tree = {tree | focused = Just path, focusPending = Just path}}}, Cmd.none)


selectTreePath : String -> Model -> (Model, Cmd Msg)
selectTreePath path model =
    let state = model.observations
        subject = Dict.get path state.tree.nodes |> Maybe.andThen (\node -> case node.terminals of
            [terminal] -> Just {subjectKind = terminal.subjectKind, subject = terminal.subject}
            _ -> Nothing)
        selected = ObservationTree.select path subject state.tree
    in ({model | observations = {state | tree = {selected | focusPending = Just path}, treeSelectionPending = False}}, Cmd.none)


finishTreeReveal : Model -> (Model, Cmd Msg)
finishTreeReveal model =
    if model.observations.searchOriginDetail || (model.observations.selectedId |> Maybe.map (\observationId -> isAuditObservation observationId model) |> Maybe.withDefault False) then
        ( model, Cmd.none )
    else
    let state = model.observations
        tree = state.tree
        subject = case state.selectedFacet of
            Just selected -> Just selected
            Nothing -> currentSelectedObservation state |> Maybe.map (\observation -> {subjectKind = observation.subjectKind, subject = observation.subject})
        validation = subject |> Maybe.map (\selected -> {subject = selected, observationId = state.selectedId, focus = False})
        target = if not (treeMode state.requestMode) then Nothing else case tree.revealTarget of
            Just reveal -> Just reveal
            Nothing -> validation
    in case target of
        Nothing -> (model, Cmd.none)
        Just requested ->
            let
                terminal = Dict.get (ObservationTree.terminalKey requested.subject) tree.terminals
                loaded = terminal |> Maybe.map (\value -> value.lifetime == tree.lifetime && (requested.observationId |> Maybe.map (\identity -> List.any (\item -> item.id == identity) value.items) |> Maybe.withDefault True)) |> Maybe.withDefault False
                missing = ObservationTree.ancestors requested.subject.subject |> List.filter (\path -> not (Dict.member path tree.nodes)) |> List.head
                missingProof = missing |> Maybe.map (\path -> Dict.get (ObservationTree.parent path) tree.branches |> Maybe.map (\branch -> branch.lifetime == tree.lifetime && not branch.hasMore && branch.request == Nothing && branch.error == Nothing) |> Maybe.withDefault False) |> Maybe.withDefault False
                parentCurrent = Dict.get (ObservationTree.parent requested.subject.subject) tree.branches |> Maybe.map (\branch -> branch.lifetime == tree.lifetime && List.member requested.subject.subject branch.paths) |> Maybe.withDefault False
                typedProof = parentCurrent && (Dict.get requested.subject.subject tree.nodes |> Maybe.map (\node -> not (List.any (\value -> value.subjectKind == requested.subject.subjectKind) node.terminals)) |> Maybe.withDefault False)
                cardProof = terminal |> Maybe.map (\value -> value.lifetime == tree.lifetime && not value.hasMore && value.request == Nothing && value.error == Nothing && not loaded) |> Maybe.withDefault False
            in
            if missingProof || typedProof || cardProof then
                let reconciled = ObservationTree.recoverFocus tree {tree | revealTarget = Nothing, selected = Nothing, resolvedPath = Nothing}
                    cleared = commitAppliedQuery {state | selectedFacet = Nothing, requestMode = ObservationFacetMode, tree = reconciled} |> clearSelection
                in Helpers.writeObservationHistory False {model | observations = cleared}
            else if tree.revealTarget == Nothing || not loaded then (model, Cmd.none)
            else case requested.observationId of
                Nothing -> ({model | observations = {state | tree = {tree | revealTarget = Nothing, resolvedPath = Just {subject = requested.subject, owner = treeOwner model}, focusPending = if requested.focus then Just requested.subject.subject else Nothing}}}, Cmd.none)
                Just observationId ->
                    if state.selectedId /= Just observationId then (model, Cmd.none) else
                    let
                        origin = observationCardDomId (treeCardContext requested.subject) observationId
                        viewport = state.viewport
                        intent = {intent = "detail", selectedId = state.selectedId, workspaceId = Maybe.withDefault "" model.selectedWorkspaceId, sessionEpoch = model.sessionRequestEpoch
                            , queryGeneration = tree.lifetime, navigationToken = state.detailNavigationToken + 1, previousToken = state.detailNavigationToken, previousSelection = state.selectedId
                            , originKey = origin, readyRevision = Nothing, fallback = False}
                    in ({model | observations = {state | tree = {tree | revealTarget = Nothing, focusPending = Nothing}, detailReturnTarget = Just origin, detailNavigationEpoch = model.sessionRequestEpoch
                        , detailNavigationToken = if requested.focus then state.detailNavigationToken + 1 else state.detailNavigationToken, pendingReturnNavigation = if requested.focus then Just intent else Nothing, viewport = {viewport | returnPin = Just origin}}}, Cmd.none)


treeKey : String -> String -> Model -> (Model, Cmd Msg)
treeKey path key model =
    let tree = model.observations.tree
        paths = ObservationTree.visiblePaths tree
        position = paths |> List.indexedMap Tuple.pair |> List.filter (Tuple.second >> (==) path) |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault 0
        at index = List.drop (Basics.max 0 index) paths |> List.head |> Maybe.withDefault path
        expanded = Set.member path tree.expanded
        toggleNode = updateRaw (ToggleObservationTreePath path) model
    in case key of
        "ArrowDown" -> focusTreePath (at (Basics.min (List.length paths - 1) (position + 1))) model
        "ArrowUp" -> focusTreePath (at (position - 1)) model
        "Home" -> focusTreePath (at 0) model
        "End" -> focusTreePath (at (List.length paths - 1)) model
        "ArrowRight" -> if not expanded then toggleNode else focusTreePath (List.filter (ObservationTree.parent >> (==) path) paths |> List.head |> Maybe.withDefault path) model
        "ArrowLeft" -> if expanded then toggleNode else if ObservationTree.parent path /= "" then focusTreePath (ObservationTree.parent path) model else (model, Cmd.none)
        "Enter" -> toggleNode
        " " -> toggleNode
        _ -> (model, Cmd.none)


validPage : Int -> Bool -> List String -> List String -> Bool
validPage offset hasMore received existing =
    List.length received <= pageSize
        && (not hasMore || (List.member (List.length received) [ 50, pageSize ] && List.any (\key -> offset == 0 || not (List.member key existing)) received))


treeQuery : String -> String -> Int -> ObservationModel -> Api.ObservationSubjectTreeQuery
treeQuery workspaceId prefix offset input =
    let state = appliedState input in
    { workspaceId = workspaceId, prefix = prefix, subjectQuery = nonEmpty state.subjectQuery, subjectKind = state.subjectKind
    , gitSha = nonEmpty state.gitSha, currentGitSha = nonEmpty state.currentGitSha, historyGitSha = nonEmpty state.historyGitSha
    , limit = pageSize, offset = offset }


treeOwner : Model -> String
treeOwner model =
    Encode.encode 0 (Encode.list Encode.string [Maybe.withDefault "" model.selectedWorkspaceId, String.fromInt model.sessionRequestEpoch, countActor model])


treeLifetime : Model -> String
treeLifetime model =
    Encode.encode 0 (Encode.list Encode.string
        [ Maybe.withDefault "" model.selectedWorkspaceId, String.fromInt model.sessionRequestEpoch, countActor model
        , String.fromInt model.observations.nextFilterToken, modeKey model.observations.requestMode, Helpers.encodeObservationQuery (discoveryQuery (querySnapshot model.observations))
        , Api.observationSubjectTreeUrl "" (treeQuery (Maybe.withDefault "" model.selectedWorkspaceId) "" 0 model.observations) ])


loadTreeBranch : String -> Int -> Model -> ( Model, Cmd Msg )
loadTreeBranch prefix offset model =
    case repositoryWorkspaceId model of
        Nothing -> ( model, Cmd.none )
        Just workspaceId ->
            if (prefix /= "" && model.observations.discovery.context == Nothing) || List.length model.observations.discovery.physical >= 4 || not (treeDemandAllowed prefix Nothing model) || model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) || model.workspaceAdmin.authorizationPending /= Nothing || filtersPending model.observations || not (List.member model.observations.requestMode [ ObservationFacetMode, ObservationExactSubjectMode ]) then ( model, Cmd.none )
            else
                let
                    state = model.observations
                    owned = if state.tree.lifetime == treeLifetime model then state.tree else ObservationTree.retire (treeLifetime model) state.tree
                    ( tree, request ) = ObservationTree.begin prefix offset owned
                in
                case request of
                    Just guard -> requestDiscovery (DiscoveryTree guard) { model | observations = { state | tree = tree } }
                    Nothing -> ( model, Cmd.none )


revealTreeSelection : Api.Observation -> ObservationModel -> ObservationModel
revealTreeSelection observation state =
    if state.selectedId /= Just observation.id || state.searchOriginDetail || state.detailReturnTarget /= Nothing || not (List.member state.requestMode [ ObservationFacetMode, ObservationExactSubjectMode ]) then state
    else
        let subject = state.selectedFacet |> Maybe.withDefault { subjectKind = observation.subjectKind, subject = observation.subject } in
        -- Only the current guarded detail reply may consume deferred route selection.
        let revealed = ObservationTree.reveal subject observation.id state.tree in
        { state | tree = if state.treeSelectionPending then {revealed | selectedPath = Just subject.subject} else revealed, treeSelectionPending = False }


emptyCounts : ObservationCountState
emptyCounts =
    { owner = Nothing, active = Nothing, nextToken = 1, generation = 0, pending = False, current = False, value = Nothing, valueFingerprint = "", settledFingerprint = "", error = Nothing }


modeKey : ObservationRequestMode -> String
modeKey mode =
    case mode of
        ObservationFlatMode -> "flat"
        ObservationMatchMode -> "match"
        _ -> "tree"


filterValidationError : ObservationModel -> Maybe String
filterValidationError state =
    if List.any (\value -> value /= "" && (String.length value /= 40 || not (String.all (\c -> String.contains (String.fromChar c) "0123456789abcdef") value))) [ state.gitSha, state.currentGitSha, state.historyGitSha ] then
        Just "Enter a full lowercase 40-character Git SHA, or clear the field."
    else if state.requestMode /= ObservationMatchMode && List.any (Helpers.observationUrlBytes >> (\size -> size > 4096)) [ state.query, state.subjectQuery ] then
        Just "Each search field supports at most 4096 UTF-8 bytes."
    else
        Nothing


filtersPending : ObservationModel -> Bool
filtersPending state =
    state.filterComposing || state.filterDebounce /= Nothing || filterValidationError state /= Nothing || hasUnappliedFilters state


retireFilterReads : ObservationModel -> ObservationModel
retireFilterReads state =
    let counts = state.counts
        retired = retireDiscoveryDraft state
    in
    { retired | requestGeneration = state.requestGeneration + 1, facetRequestGeneration = state.facetRequestGeneration + 1
        , completedQuery = if state.completedQuery == Nothing && (not (List.isEmpty state.orderedIds) || not (List.isEmpty state.facetKeys)) then state.appliedQuery else state.completedQuery
        , loading = False, facetLoading = False, expectedOffset = Nothing, facetExpectedOffset = Nothing
        , refreshPass = Nothing, refreshPending = False, refreshError = Nothing, failedRequest = Nothing
        , filterDebounce = Nothing, nextFilterToken = state.nextFilterToken + 1
        , tree = ObservationTree.retire ("retired-" ++ String.fromInt state.nextFilterToken) state.tree
        , counts = { counts | generation = counts.generation + 1, pending = True, current = False, error = Nothing }
        , history = Maybe.map (\history -> { history | active = Nothing, loading = False }) state.history }


observationSearchResetStamp : String -> ObservationModel -> String
observationSearchResetStamp workspaceId state =
    Encode.encode 0 (Encode.list Encode.string [ workspaceId, state.discovery.owner, String.fromInt state.clearViewportToken, Helpers.encodeObservationQuery (draftQuery state) ])


clearSearchViewport : ObservationModel -> ObservationModel
clearSearchViewport state =
    let viewport = state.viewport
        tree = state.tree
    in
    { state | clearViewportToken = state.clearViewportToken + 1, preferenceScroll = Nothing, detailReturnTarget = Nothing, pendingReturnNavigation = Nothing, detailNavigationToken = state.detailNavigationToken + 1
        , treeSelectionPending = False, tree = { tree | revealTarget = Nothing, revealFocus = False, focusPending = Nothing }
        , modeScrolls = Dict.insert (modeKey state.requestMode) 0 state.modeScrolls
        , viewport = { viewport | top = 0, focus = Nothing, returnPin = Nothing, origin = Nothing, restoring = False, preserveScroll = False, preserveExtent = 0 } }


editSearchText : Bool -> Bool -> String -> Model -> ( Model, Cmd Msg )
editSearchText subjects force value model =
    let
        previous = if subjects then model.observations.subjectQuery else model.observations.query
        prepared = updateObservation preserveSearchViewport model
        edit state = if subjects then { state | subjectQuery = value, modeSubjectQueries = Dict.insert (modeKey state.requestMode) value state.modeSubjectQueries }
            else { state | query = value, modeQueries = Dict.insert (modeKey state.requestMode) value state.modeQueries }
        ( updated, command ) = if previous == value && not force then ( prepared, Cmd.none ) else editFilters (value /= "") edit prepared
    in
    ( updated, command )


preserveSearchViewport : ObservationModel -> ObservationModel
preserveSearchViewport state =
    let viewport = state.viewport
        tree = state.tree
    in
    { state | preferenceScroll = Nothing, pendingReturnNavigation = Nothing
        , tree = { tree | focusPending = Nothing, revealFocus = False, revealTarget = Maybe.map (\target -> { target | focus = False }) tree.revealTarget }
        , viewport = { viewport | preserveScroll = True, preserveExtent = Basics.max viewport.preserveExtent (HierarchyViewport.height viewport.index), origin = Nothing, restoring = False } }


clearSearchText : Bool -> Model -> ( Model, Cmd Msg )
clearSearchText subjects model =
    let
        ( updated, command ) = editSearchText subjects False "" model
        cleared = updateObservation clearSearchViewport updated
    in
    ( cleared, Cmd.batch [ command, Ports.resetObservationSearchViewport (Encode.object [ ( "stamp", Encode.string (observationSearchResetStamp (Maybe.withDefault "" cleared.selectedWorkspaceId) cleared.observations) ) ]) ] )


clearDiscoveryError : Model -> Model
clearDiscoveryError model =
    updateObservation (\state -> let discovery = state.discovery in { state | discovery = { discovery | error = Nothing, failedCount = False } }) model


retryDiscoveryCounts : Model -> ( Model, Cmd Msg )
retryDiscoveryCounts model =
    let
        retry counts = { counts | pending = True, current = False, error = Nothing }
        prepared = updateObservation (\state -> { state | counts = retry state.counts, workspaceCounts = retry state.workspaceCounts }) (clearDiscoveryError model)
        discovery = prepared.observations.discovery
    in
    if model.activeTab == ObservationsTab && discovery.context == Nothing && discovery.afterBootstrap == Nothing then
        if treeMode prepared.observations.requestMode then startTree True prepared else retryResults prepared
    else
        syncDiscoveryCounts prepared


editFilters : Bool -> (ObservationModel -> ObservationModel) -> Model -> ( Model, Cmd Msg )
editFilters debounce edit model =
    let
        state = edit model.observations |> retireFilterReads
        prepared = { model | observations = state }
    in
    if filterValidationError state /= Nothing then
        ( prepared, Cmd.none )
    else if state.filterComposing then
        ( prepared, Cmd.none )
    else if debounce then
        let stamp = { workspaceId = Maybe.withDefault "" model.selectedWorkspaceId, sessionEpoch = model.sessionRequestEpoch, actor = countActor model, token = state.nextFilterToken, mode = state.requestMode, query = querySnapshot { state | appliedQuery = Nothing } } in
        ( { prepared | observations = { state | filterDebounce = Just stamp } }
        , Process.sleep 250 |> Task.perform (\_ -> ObservationFiltersReady stamp) )
    else
        applyLiveFilters prepared


applyLiveFilters : Model -> ( Model, Cmd Msg )
applyLiveFilters model =
    if model.observations.filterComposing || filterValidationError model.observations /= Nothing then ( model, Cmd.none )
    else if model.observations.requestMode == ObservationMatchMode && List.isEmpty model.observations.matchAppliedPaths then
        ( updateObservation (\state -> commitAppliedQuery { state | filterDebounce = Nothing }) model, Cmd.none )
    else
        let
            prepared = updateObservation (\state -> commitAppliedQuery { state | filterDebounce = Nothing, nextOffset = 0, facetNextOffset = 0 }) model
        in
        if model.observations.completedQuery == Nothing && List.isEmpty model.observations.orderedIds && List.isEmpty model.observations.facetKeys then
            applyObservationFilters prepared
        else
            refreshActiveResultsRaw prepared


countQuery : String -> ObservationModel -> Api.ObservationCountQuery
countQuery workspaceId state =
    let value = listQuery workspaceId 0 state in
    { workspaceId = workspaceId, subjectKind = value.subjectKind, subject = value.subject, gitSha = value.gitSha, currentGitSha = value.currentGitSha, historyGitSha = value.historyGitSha, query = value.query
    , paths = if state.requestMode == ObservationMatchMode then Just (appliedState state).matchAppliedPaths else Nothing }


countFingerprint : String -> ObservationModel -> String
countFingerprint workspaceId state =
    let value = countQuery workspaceId state in
    Encode.encode 0 (Encode.list Encode.string [ modeKey state.requestMode, workspaceId, (appliedState state).subjectQuery, Maybe.withDefault "" (Maybe.map Api.subjectKindToString value.subjectKind), Maybe.withDefault "" value.subject, Maybe.withDefault "" value.gitSha, Maybe.withDefault "" value.currentGitSha, Maybe.withDefault "" value.historyGitSha, Maybe.withDefault "" value.query, Encode.encode 0 (Encode.list Encode.string (Maybe.withDefault [] value.paths)) ])


hasAppliedCountFilter : ObservationModel -> Bool
hasAppliedCountFilter state =
    let value = countQuery "" state in
    (appliedState state).subjectQuery /= "" || value.subjectKind /= Nothing || value.subject /= Nothing || value.gitSha /= Nothing || value.currentGitSha /= Nothing || value.historyGitSha /= Nothing || value.query /= Nothing || not (List.isEmpty (Maybe.withDefault [] value.paths))


countActor : Model -> String
countActor model =
    model.sessionContext |> Maybe.map (\session -> Encode.encode 0 (Encode.list Encode.string [ session.authMode, session.principal.actorType, session.principal.actorId, session.principal.authority, Maybe.withDefault "" session.principal.grantUserId, Permissions.currentWorkspaceRoleLabel model ])) |> Maybe.withDefault ""


invalidateCounts : ObservationModel -> ObservationModel
invalidateCounts state =
    let
        invalidate counts = { counts | generation = counts.generation + 1, pending = True, current = False, error = Nothing }
    in
    { state | counts = invalidate state.counts, workspaceCounts = invalidate state.workspaceCounts }


syncCounts : Model -> ( Model, Cmd Msg )
syncCounts model =
    if model.observations.requestMode /= ObservationMatchMode then syncDiscoveryCounts model else
    let
        ( workspace, workspaceCommand ) = syncCount True model
        ( scoped, scopedCommand ) = syncCount False workspace
    in
    ( scoped, Cmd.batch [ workspaceCommand, scopedCommand ] )


syncDiscoveryCounts : Model -> ( Model, Cmd Msg )
syncDiscoveryCounts incoming =
    let previous = incoming.observations
        old = previous.discovery
        changedOwner = old.owner /= "" && old.owner /= treeOwner incoming
        retire counts = { counts | owner = Nothing, active = Nothing, pending = True, current = False, error = Nothing }
        state = if changedOwner then
            { previous | discovery = {old | owner = treeOwner incoming, query = Just (discoveryQuery (querySnapshot previous)), context = Nothing, bootstrap = Nothing, afterBootstrap = Nothing, generation = old.generation + 1, error = Nothing, failedCount = False}
                , counts = retire previous.counts, workspaceCounts = retire previous.workspaceCounts }
            else previous
        model = { incoming | observations = state }
        d = state.discovery
        needs counts = not counts.current && counts.error == Nothing
        ownsRead request = request.owner == treeOwner model && request.generation == d.generation
        countInFlight request = case request.destination of
            DiscoveryCount _ _ -> True
            _ -> False
        available = d.bootstrap == Nothing && not (List.any (\request -> ownsRead request || countInFlight request) d.physical)
        settlingVisiblePage = model.activeTab == ObservationsTab && not (treeMode state.requestMode) && (state.loading || state.expectedOffset /= Nothing || state.refreshPass /= Nothing) && d.afterBootstrap == Nothing
    in
    if model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) || model.workspaceAdmin.authorizationPending /= Nothing || filtersPending state || not available || settlingVisiblePage || d.error /= Nothing || (model.activeTab == ObservationsTab && d.context == Nothing && d.afterBootstrap == Nothing && not state.resultsStale) || not (needs state.workspaceCounts || (state.counts.pending && needs state.counts)) then (model, Cmd.none)
    else case repositoryWorkspaceId model of
        Nothing -> (model, Cmd.none)
        Just workspaceId ->
            let guard counts fingerprint = {workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch, actor = countActor model, token = counts.nextToken, generation = counts.generation, fingerprint = fingerprint}
            in requestDiscovery (DiscoveryCount (guard state.counts (countFingerprint workspaceId state)) (guard state.workspaceCounts ("workspace:" ++ workspaceId))) model


countState : Bool -> ObservationModel -> ObservationCountState
countState workspace state =
    if workspace then state.workspaceCounts else state.counts


setCountState : Bool -> ObservationCountState -> ObservationModel -> ObservationModel
setCountState workspace counts state =
    if workspace then { state | workspaceCounts = counts } else { state | counts = counts }


countRequest : Bool -> String -> ObservationModel -> Api.ObservationCountQuery
countRequest workspace workspaceId state =
    if workspace then countQuery workspaceId init else countQuery workspaceId state


countRequestFingerprint : Bool -> String -> ObservationModel -> String
countRequestFingerprint workspace workspaceId state =
    if workspace then "workspace:" ++ workspaceId else countFingerprint workspaceId state


syncCount : Bool -> Model -> ( Model, Cmd Msg )
syncCount workspace model =
    let
        counts = countState workspace model.observations
        updateCounts next = updateObservation (setCountState workspace next) model
    in
    if model.auth.status /= AuthReady || model.sessionContext == Nothing || not (Permissions.canReadCurrentWorkspace model) || model.workspaceAdmin.authorizationPending /= Nothing then
        ( updateCounts { emptyCounts | nextToken = counts.nextToken, generation = counts.generation + 1 }, Cmd.none )
    else if not workspace && (treeMode model.observations.requestMode || model.observations.filterDebounce /= Nothing || filterValidationError model.observations /= Nothing || (model.observations.requestMode == ObservationMatchMode && List.isEmpty model.observations.matchAppliedPaths)) then
        ( model, Cmd.none )
    else
        case repositoryWorkspaceId model of
            Nothing -> ( updateCounts { emptyCounts | nextToken = counts.nextToken, generation = counts.generation + 1 }, Cmd.none )
            Just workspaceId ->
                let
                    fingerprint = countRequestFingerprint workspace workspaceId model.observations
                    owner = { workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch, actor = countActor model, token = 0, generation = 0, fingerprint = "" }
                    previousOwner = counts.owner
                    changedOwner = previousOwner /= Just owner
                    owned = if changedOwner then { emptyCounts | owner = Just owner, nextToken = counts.nextToken, generation = counts.generation + 1, pending = True } else counts
                    requestedFingerprint = Maybe.map .fingerprint owned.active |> Maybe.withDefault owned.settledFingerprint
                    changedFilter = requestedFingerprint /= fingerprint
                    queued = { owned | pending = owned.pending || changedFilter, error = if changedFilter then Nothing else owned.error }
                in
                if queued.active == Nothing && queued.pending && queued.error == Nothing then
                    let
                        guard = { owner | token = queued.nextToken, generation = queued.generation, fingerprint = fingerprint }
                        started = { queued | active = Just guard, nextToken = queued.nextToken + 1, pending = False }
                    in
                    let
                        loading = model.dataLoading
                        admission = StartupAdmission.reconcile
                            (if model.workspaceAdmin.authorizationPending == Nothing then Just { workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch, audience = countActor model } else Nothing)
                            loading.startupAdmission
                        occupied = Set.toList admission.physical |> List.any (String.startsWith (if workspace then "observation-workspace|" else "observation|"))
                    in
                    case if occupied then Nothing else StartupAdmission.admit (countPhysicalKey workspace guard) admission of
                        Just admitted ->
                            let next = updateCounts started in
                            ( { next | dataLoading = { loading | startupAdmission = admitted } }, Api.fetchScopedObservationCounts (not workspace && model.observations.requestMode == ObservationFlatMode) model.flags.apiUrl (countRequest workspace workspaceId model.observations) ((if workspace then GotWorkspaceObservationCounts else GotObservationCounts) guard) )
                        Nothing ->
                            let next = updateCounts queued in
                            ( { next | dataLoading = { loading | startupAdmission = admission } }, Cmd.none )
                else
                    ( updateCounts queued, Cmd.none )


acceptCounts : Bool -> ObservationCountGuard -> Result Http.Error Api.ObservationCounts -> Model -> ( Model, Cmd Msg )
acceptCounts workspace guard result source =
    let
        loading = source.dataLoading
        model = { source | dataLoading = { loading | startupAdmission = StartupAdmission.complete (countPhysicalKey workspace guard) loading.startupAdmission } }
        counts = countState workspace model.observations
    in
    if counts.active /= Just guard then ( model, Cmd.none )
    else
        let
            owned = guard.workspaceId == Maybe.withDefault "" (repositoryWorkspaceId model) && guard.sessionEpoch == model.sessionRequestEpoch && guard.actor == countActor model && model.auth.status == AuthReady && Permissions.canReadCurrentWorkspace model && model.workspaceAdmin.authorizationPending == Nothing
            current = owned && guard.generation == counts.generation && guard.fingerprint == countRequestFingerprint workspace guard.workspaceId model.observations && not counts.pending && (workspace || (model.observations.filterDebounce == Nothing && filterValidationError model.observations == Nothing))
            completed = { counts | active = Nothing, settledFingerprint = guard.fingerprint }
            settled = if not current then { completed | pending = owned, error = Nothing }
                else case result of
                    Ok value -> if value.workspaceId == guard.workspaceId then { completed | value = Just value, valueFingerprint = guard.fingerprint, current = True, error = Nothing }
                        else { completed | current = False, error = Just "Observation counts returned the wrong workspace." }
                    Err _ -> { completed | current = False, error = Just "Unable to load Observation counts." }
        in
        syncCounts (updateObservation (setCountState workspace settled) model)


countPhysicalKey : Bool -> ObservationCountGuard -> String
countPhysicalKey workspace guard =
    (if workspace then "observation-workspace|" else "observation|") ++ Encode.encode 0 (Encode.list Encode.string [ guard.workspaceId, String.fromInt guard.sessionEpoch, guard.actor, String.fromInt guard.token, String.fromInt guard.generation, guard.fingerprint ])


{-| Read permission may be revoked and regranted within the same session epoch.
Retire private state while keeping request/navigation identities monotonic.
-}
retireSessionState : ObservationModel -> ObservationModel
retireSessionState previous =
    { init
        | workspaceCounts = let counts = previous.workspaceCounts in { emptyCounts | nextToken = counts.nextToken, generation = counts.generation + 1 }
        , counts = let counts = previous.counts in { emptyCounts | nextToken = counts.nextToken, generation = counts.generation + 1 }
        , requestGeneration = previous.requestGeneration + 1
        , facetRequestGeneration = previous.facetRequestGeneration + 1
        , nextDetailRequestToken = previous.nextDetailRequestToken
        , nextCurationContextToken = previous.nextCurationContextToken
        , nextMutationRequestToken = previous.nextMutationRequestToken
        , nextHistoryRequestToken = previous.nextHistoryRequestToken
        , detailNavigationToken = previous.detailNavigationToken + 1
        , nextLinkToken = previous.nextLinkToken + 1
        , nextPreferenceRequest = previous.nextPreferenceRequest + 1
        , nextFilterToken = previous.nextFilterToken + 1
        , discovery = let discovery = previous.discovery in { discovery | context = Nothing, bootstrap = Nothing, owner = "", query = Nothing, generation = discovery.generation + 1, error = Nothing }
        , tree = let fresh = ObservationTree.init in { fresh | physical = previous.tree.physical, nextToken = previous.tree.nextToken + 1 }
    }


clearSelection : ObservationModel -> ObservationModel
clearSelection state =
    let viewport = state.viewport
        tree = state.tree
    in
    { state
        | selectedId = Nothing
        , tree = {tree | revealTarget = tree.revealTarget |> Maybe.andThen (\target -> if target.observationId == Nothing then Just target else Nothing)}
        , inlineOwner = Nothing
        , selectedDetail = Nothing
        , history = Nothing
        , historyExpanded = False
        , detailLoading = False
        , detailError = Nothing
        , activeDetailRequest = Nothing
        , treeSelectionPending = False
        , detailNavigationToken = state.detailNavigationToken + 1
        , searchOriginDetail = False
        , detailReturnTarget = Nothing
        , pendingReturnNavigation = Nothing
        , viewport = { viewport | origin = Nothing, restoring = False, returnPin = Nothing, focus = Nothing }
        , edit = retainedEdit state.edit
        , deleteConfirmation = Nothing
    }


retainedEdit : Maybe ObservationEditState -> Maybe ObservationEditState
retainedEdit maybeEdit =
    Maybe.andThen
        (\edit ->
            if edit.saving || (edit.draft /= edit.baseContent || edit.reviewedGitShaDraft /= edit.baseReviewedGitSha) then
                Just edit

            else
                Nothing
        )
        maybeEdit


hasProtectedEdit : Model -> Bool
hasProtectedEdit model =
    model.observations.edit
        |> retainedEdit
        |> Maybe.map (\edit -> editContextIsCurrent edit model)
        |> Maybe.withDefault False


refuseContextExit : Model -> ( Model, Cmd Msg )
refuseContextExit model =
    addToast Warning "Save or discard your Observation draft before leaving this workspace. An in-flight save must finish first." model


returnToDraft : Model -> ( Model, Cmd Msg )
returnToDraft model =
    case model.observations.edit of
        Just edit ->
            if editContextIsCurrent edit model then
                let
                    ( selected, selectionCmd ) =
                        selectObservation edit.observationId { model | activeTab = ObservationsTab }
                in
                ( selected, Cmd.batch [ selectionCmd, focusElement "observation-edit-content" ] )

            else
                ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


selectionForTab : WorkspaceTab -> ObservationModel -> ObservationModel
selectionForTab tab state =
    if tab == ObservationsTab then
        state

    else
        let
            cleared = clearSelection state
            tree = cleared.tree
        in
        { cleared | tree = { tree | revealTarget = Maybe.map (\target -> { target | focus = False }) tree.revealTarget } }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( rawUpdated, command ) =
            updateRaw msg model
        queryUpdated =
            if model.observations.tree.selectedPath /= Nothing && rawUpdated.observations.tree.selectedPath == Nothing then
                updateObservation (\state -> let prefs = state.preferenceValue in {state | preferenceValue = {prefs | selectedPath = Nothing}, treeSelectionPending = False}) rawUpdated
            else if not (treeMode rawUpdated.observations.requestMode) then updateObservation (\state -> {state | treeSelectionPending = False}) rawUpdated else rawUpdated

        intentional =
            case msg of
                SetObservationBrowseMode _ -> True
                OpenObservationFileComposer -> not (hasProtectedEdit model)
                SelectObservationFacet _ _ -> True
                ApplyObservationMatch -> True
                ClearObservationMatch -> True
                SelectObservation _ -> True
                SelectObservationFrom _ _ -> True
                ReturnObservationResults -> True
                ReturnToObservationDraft -> True
                _ -> False
        liveChange = case msg of
            ObservationFiltersReady _ -> rawUpdated.observations /= model.observations
            ApplyObservationFilters -> True
            SetObservationQuery _ -> True
            SetObservationSubjectQuery _ -> True
            ClearObservationSearchText _ -> True
            ObservationFilterCompositionEnd _ _ -> True
            SetObservationSubjectKind _ -> True
            SetObservationSubject _ -> True
            SetObservationGitSha _ -> True
            SetObservationCurrentGitSha _ -> True
            SetObservationHistoryGitSha _ -> True
            _ -> False
        touched = intentional || liveChange || (case msg of
            ToggleObservationSubjects _ -> True
            ToggleObservationMatchGroup _ -> True
            FocusObservationTreePath _ -> True
            SelectObservationTreePath _ -> True
            ToggleObservationTreePath _ -> True
            ObservationTreeKey _ _ -> True
            _ -> False)
        updated = if touched then rememberPreferences queryUpdated else queryUpdated
    in
    if (intentional || liveChange) && (Helpers.observationAppliedQuery updated.observations /= Helpers.observationAppliedQuery model.observations || updated.observations.selectedId /= model.observations.selectedId || updated.activeTab /= model.activeTab) then
        let
            ( linked, linkCmd ) = Helpers.writeObservationHistory intentional updated
        in
        ( linked, Cmd.batch [ command, linkCmd ] )
    else
        ( updated, command )


updateRaw : Msg -> Model -> ( Model, Cmd Msg )
updateRaw msg model =
    case msg of
        ObservationDiscoveryRuntimeReceived payload -> acceptDiscoveryRuntime payload model

        GotObservationDiscovery request result -> acceptDiscovery request result model

        GotWorkspaceObservationCounts guard result ->
            acceptCounts True guard result model

        GotObservationCounts guard result ->
            acceptCounts False guard result model

        GotObservationTree request result ->
            let state = model.observations
                owned = if state.refreshPending then ObservationTree.retire state.tree.lifetime (ownedTree model) else ownedTree model
                received = ObservationTree.receive request (Result.mapError (\_ -> "Unable to load Subject Tree.") result) owned
                recovered = if state.viewport.focus |> Maybe.map (String.startsWith "tree-path:") |> Maybe.withDefault False then received else {received | focusPending = Nothing}
                completed = request.prefix == "" && request.kind == Nothing && request.lifetime == owned.lifetime
                    && (Dict.get "" owned.branches |> Maybe.map .request) == Just (Just request)
                    && received.displayLifetime == request.lifetime
                    && (Dict.get "" received.branches |> Maybe.map (\branch -> branch.error == Nothing && branch.lifetime == request.lifetime) |> Maybe.withDefault False)
            in ( { model | observations = { state | tree = recovered, completedQuery = if completed then state.appliedQuery else state.completedQuery } }, Cmd.none )

        GotObservationTreeTerminal request result ->
            let state = model.observations
                owned = if state.refreshPending then ObservationTree.retire state.tree.lifetime (ownedTree model) else ownedTree model
                tree = ObservationTree.receiveTerminal request (Result.mapError (\_ -> "Unable to load subject observations.") result) owned
                items = Dict.values tree.terminals |> List.concatMap .items
                received = {state | tree = tree, items = List.foldl (\item accumulated -> Dict.update item.id (\old -> Just (old |> Maybe.map (preferNewerObservation item) |> Maybe.withDefault item)) accumulated) state.items items}
                reconciled = List.foldl applyAuthoritativeObservation received items
            in ({model | observations = reconciled}, Cmd.none)

        FocusObservationTreePath path ->
            let state = model.observations
                tree = state.tree
            in ({model | observations = {state | tree = {tree | focused = Just path}}}, Cmd.none)

        SelectObservationTreePath path -> selectTreePath path model

        ToggleObservationTreePath path ->
            let state = model.observations
                tree = state.tree
                selected = ObservationTree.toggle path {tree | focused = Just path}
            in ({model | observations = {state | tree = selected}}, Cmd.none)

        ObservationTreeKey path key -> treeKey path key model

        LoadObservationTreePage prefix kind ->
            let state = model.observations in
            case kind of
                Nothing ->
                    let branch = Dict.get prefix state.tree.branches
                        offset = branch |> Maybe.map (\value -> if value.lifetime == state.tree.lifetime then value.nextOffset else 0) |> Maybe.withDefault 0
                    in loadTreeBranch prefix offset (clearDiscoveryError model)
                Just value ->
                    let subject = {subjectKind = value, subject = prefix}
                        offset = Dict.get (ObservationTree.terminalKey subject) state.tree.terminals |> Maybe.map (\terminal -> if terminal.lifetime == state.tree.lifetime then terminal.nextOffset else 0) |> Maybe.withDefault 0
                    in loadTreeTerminal subject offset (clearDiscoveryError model)

        RetryWorkspaceObservationCounts ->
            let counts = model.observations.workspaceCounts in
            if model.observations.requestMode /= ObservationMatchMode then retryDiscoveryCounts model
            else syncCounts (updateObservation (setCountState True { counts | error = Nothing, pending = True, current = False }) (clearDiscoveryError model))

        RetryObservationCounts ->
            if model.observations.requestMode /= ObservationMatchMode then retryDiscoveryCounts model
            else syncCounts (updateObservation (\state -> let counts = state.counts in { state | counts = { counts | error = Nothing, pending = True, current = False } }) (clearDiscoveryError model))

        ObservationPreferencesReceived payload ->
            hydratePreferences payload model

        SetObservationQuery value ->
            editSearchText False False value model

        SetObservationSubjectQuery value ->
            editSearchText True False value model

        ClearObservationSearchText subjects ->
            clearSearchText subjects model

        ObservationFilterCompositionStart ->
            ( updateObservation (\state -> let preserved = preserveSearchViewport state in { preserved | filterComposing = True, filterDebounce = Nothing, nextFilterToken = state.nextFilterToken + 1 }) model, Cmd.none )

        ObservationFilterCompositionEnd subjects value ->
            let prepared = updateObservation (\state -> { state | filterComposing = False }) model in
            editSearchText subjects True value prepared

        ObservationFiltersReady stamp ->
            if model.observations.filterDebounce == Just stamp && model.selectedWorkspaceId == Just stamp.workspaceId && model.sessionRequestEpoch == stamp.sessionEpoch && countActor model == stamp.actor && model.observations.requestMode == stamp.mode && stamp.query == draftQuery model.observations then
                applyLiveFilters (updateObservation (\state -> { state | filterDebounce = Nothing }) model)
            else
                ( model, Cmd.none )

        SetObservationSubjectKind value ->
            editFilters False (\state -> { state | subjectKind = Api.subjectKindFromString value }) model

        SetObservationSubject value ->
            editFilters (not (String.isEmpty value)) (\state -> { state | subject = value }) model

        SetObservationGitSha value ->
            editFilters False (\state -> { state | gitSha = value }) model

        SetObservationCurrentGitSha value ->
            editFilters False (\state -> { state | currentGitSha = value }) model

        SetObservationHistoryGitSha value ->
            editFilters False (\state -> { state | historyGitSha = value }) model

        ApplyObservationFilters ->
            applyLiveFilters (updateObservation (\state -> { state | filterDebounce = Nothing }) model)

        RevertObservationFilters ->
            ( updateObservation revertFilters model, Cmd.none )

        RefreshObservationResults ->
            refreshActiveResults model

        RetryObservationResults ->
            if model.observations.discovery.failedCount then
                retryDiscoveryCounts model
            else if treeMode model.observations.requestMode then startTree True (clearDiscoveryError model) else retryResults (clearDiscoveryError model)

        RetryObservationDetail ->
            case model.observations.selectedId of
                Just observationId ->
                    if model.observations.detailError /= Nothing && not model.observations.detailLoading then
                        selectObservation observationId model

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetObservationBrowseMode mode ->
            switchBrowseMode mode model

        SelectObservationFacet subjectKind subject ->
            selectFacet subjectKind subject model

        SetObservationMatchPaths value ->
            ( updateObservation (\state -> { state | matchPathsInput = value, matchValidationError = Nothing }) model, Cmd.none )

        OpenObservationFileComposer ->
            let ( updated, command ) = if hasProtectedEdit model then ( model, Cmd.none ) else switchBrowseMode ObservationMatchMode model in
            ( updateObservation (\state -> { state | fileComposerOpen = True }) updated, Cmd.batch [ command, focusElement "observation-match-paths" ] )

        CloseObservationFileComposer ->
            let toolbarState = model.observations in
            ( updateObservation (\state -> { state | fileComposerOpen = False }) model
            , Ports.focusObservationToolbar (Encode.object
                [ ( "targetId", Encode.string "observation-for-files" )
                , ( "originId", Encode.string "observation-close-file-composer" )
                , ( "context", navigationStamp (Maybe.withDefault "" model.selectedWorkspaceId) { toolbarState | detailNavigationEpoch = model.sessionRequestEpoch } )
                , ( "viewport", ObservationViewport.stampValue model.observations.viewport.stamp )
                ]) )

        ToggleObservationAdvancedFilters ->
            ( updateObservation (\state -> { state | advancedFiltersOpen = not state.advancedFiltersOpen }) model, Cmd.none )

        ApplyObservationMatch ->
            matchObservations model

        ClearObservationMatch ->
            restoreBrowseAfterMatch model

        LoadMoreObservations ->
            case repositoryWorkspaceId model of
                Just workspaceId ->
                    if canLoadMore workspaceId model.observations then
                        case model.observations.requestMode of
                            ObservationFlatMode ->
                                fetchPage model.observations.nextOffset model

                            ObservationExactSubjectMode ->
                                fetchPage model.observations.nextOffset model

                            ObservationFacetMode ->
                                ( model, Cmd.none )

                            ObservationMatchMode ->
                                if List.isEmpty model.observations.matchAppliedPaths then
                                    ( model, Cmd.none )

                                else
                                    fetchMatchPage model.observations.nextOffset model.observations.matchAppliedPaths model

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        LoadMoreObservationFacets ->
            loadMoreFacets model

        ToggleObservationSubjects observationId ->
            ( updateObservation (\state -> { state | expandedSubjects = Dict.update observationId (\open -> Just (not (Maybe.withDefault False open))) state.expandedSubjects }) model, Cmd.none )

        ToggleObservationMatchGroup groupKey ->
            let
                state = model.observations
                ownsSelection = state.selectedId |> Maybe.map (\selected -> state.inlineOwner == Just (observationCardDomId groupKey selected)) |> Maybe.withDefault False
                ( retained, command ) = if ownsSelection && (Dict.get groupKey state.expandedMatchGroups |> Maybe.withDefault False) then returnToResults model else ( model, Cmd.none )
            in
            ( updateObservation (\current -> { current | expandedMatchGroups = Dict.update groupKey (\expanded -> Just (not (Maybe.withDefault False expanded))) current.expandedMatchGroups }) retained
            , command )

        SelectObservation observationId ->
            activateObservation observationId Nothing model

        SelectObservationFrom observationId originId ->
            let state = model.observations
                subject = if treeMode state.requestMode then Dict.values state.tree.terminals |> List.filter (\terminal -> observationCardDomId (treeCardContext terminal.subject) observationId == originId) |> List.head |> Maybe.map .subject else Nothing
                tree = state.tree
                prepared = case subject of
                    Nothing -> model
                    Just value -> {model | observations = {state | selectedFacet = Just value, tree = {tree | selected = Just value}, appliedQuery = Maybe.map (\query -> {query | selectedFacet = Just value}) state.appliedQuery}}
            in activateObservation observationId (Just originId) prepared

        ReturnObservationResults ->
            returnToResults model

        CopyObservationSubject subject ->
            ( model, copyToClipboard subject )

        CopyObservationGitSha gitSha ->
            ( model, copyToClipboard gitSha )

        CopyObservationContent observationId ->
            case currentSelectedObservation model.observations of
                Just observation ->
                    if observation.id == observationId then
                        ( model, copyToClipboard observation.content )
                    else
                        ( model, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

        StartObservationEdit ->
            if hasProtectedEdit model then
                returnToDraft model

            else
                startEdit model

        ReturnToObservationDraft ->
            returnToDraft model

        SetObservationDraft value ->
            ( updateObservation
                (\state ->
                    { state
                        | edit =
                            state.edit
                                |> Maybe.map
                                    (\edit ->
                                        if edit.saving then
                                            edit

                                        else
                                            { edit | draft = value, error = Nothing }
                                    )
                    }
                )
                model
            , Cmd.none
            )

        SetObservationReviewedGitSha value ->
            ( updateObservation (\state -> { state | edit = Maybe.map (\edit -> if edit.saving then edit else { edit | reviewedGitShaDraft = value, error = Nothing }) state.edit }) model, Cmd.none )

        LoadObservationHistory ->
            refreshHistory ( updateObservation (\state -> { state | historyExpanded = True, history = Maybe.map (\saved -> { saved | error = Nothing }) state.history }) model, Cmd.none )

        ToggleObservationHistory ->
            refreshHistory ( updateObservation (\state -> { state | historyExpanded = not state.historyExpanded }) model, Cmd.none )

        GotObservationHistory request result ->
            receiveHistory request result model

        SaveObservationEdit ->
            saveEdit model

        CancelObservationEdit ->
            if Maybe.map .saving model.observations.edit == Just True then
                ( model, Cmd.none )

            else
                ( updateObservation (\state -> { state | edit = Nothing }) model
                , focusElement "observation-review"
                )

        ReloadObservationEdit ->
            ( updateObservation reloadEdit model, Cmd.none )

        RebaseObservationEdit ->
            ( updateObservation rebaseEdit model, Cmd.none )

        ObservationUpdated request result ->
            handleUpdateResponse request result model

        ObservationConflictCanonicalFetched request knownVersion result ->
            handleConflictCanonicalResponse request knownVersion result model

        OpenObservationDelete ->
            openDeleteConfirmation model

        ConfirmObservationDelete ->
            confirmDelete model

        CancelObservationDelete ->
            cancelDelete model

        ObservationDeleteDialogKeyDown key shiftKey targetId ->
            handleDeleteDialogKeyDown key shiftKey targetId model

        ObservationDeleted request result ->
            handleDeleteResponse request result model

        GotObservationDetail workspaceId observationId sessionEpoch token result ->
            if detailResponseMatches workspaceId observationId sessionEpoch token model.selectedWorkspaceId model.sessionRequestEpoch model.observations then
                case result of
                    Ok observation ->
                        if observation.id == observationId && observation.workspaceId == workspaceId then
                            ( updateObservation
                                (acceptFetchedDetail (isAuditObservation observationId model) observation
                                    >> (if isAuditObservation observationId model then identity else revealTreeSelection observation)
                                    >> (\state -> { state | detailLoading = False, detailError = Nothing, activeDetailRequest = Nothing })
                                )
                                model
                            , Cmd.none
                            )

                        else
                            ( updateObservation (\state -> { state | detailLoading = False, detailError = Just "Observation detail did not match this workspace.", activeDetailRequest = Nothing, treeSelectionPending = False }) model, Cmd.none )

                    Err (Http.BadStatus 404) ->
                        if isAuditObservation observationId model then
                            ( updateObservation (\state -> { state | selectedDetail = Nothing, detailLoading = False, detailError = Just "Entity destination unavailable. It may no longer exist or be accessible.", activeDetailRequest = Nothing, treeSelectionPending = False }) model, Cmd.none )
                        else
                        let
                            ( cleaned, cleanupCmd ) =
                                reconcileDeletedObservation observationId model

                            ( refreshed, refreshCmd ) =
                                refreshActiveResults cleaned

                            ( toasted, toastCmd ) =
                                addToast Warning "This observation was deleted." refreshed
                        in
                        ( toasted, Cmd.batch [ cleanupCmd, refreshCmd, toastCmd ] )

                    Err _ ->
                        ( updateObservation (\state -> { state | detailLoading = False, detailError = Just "Failed to load observation detail.", activeDetailRequest = Nothing }) model, Cmd.none )

            else
                ( model, Cmd.none )

        GotObservationMatches workspaceId sessionEpoch generation fingerprint offset result ->
            if model.selectedWorkspaceId /= Just workspaceId || model.sessionRequestEpoch /= sessionEpoch || not (matchResponseMatches sessionEpoch generation fingerprint offset model.observations) then
                ( model, Cmd.none )

            else if model.observations.refreshPending && model.observations.refreshPass == Nothing then
                completeSupersededRead model
            else
                case result of
                    Ok paginated ->
                        if model.observations.refreshPass /= Nothing then
                            acceptRefreshChunk offset paginated.hasMore (List.length paginated.items)
                                (\pass -> { pass | items = List.foldl (\match -> Dict.insert match.observation.id match.observation) pass.items paginated.items, orderedIds = appendUnique pass.orderedIds (List.map (.observation >> .id) paginated.items), matchEvidence = List.foldl (\match -> Dict.insert match.observation.id match) pass.matchEvidence paginated.items }) model
                        else if validPage offset paginated.hasMore (List.map (\item -> item.observation.id) paginated.items) model.observations.orderedIds then
                            ( updateObservation (mergeMatchPage offset paginated) model, Cmd.none )
                        else ( updateObservation (failResultPage workspaceId offset "File matches are incomplete: the page made no valid progress.") model, Cmd.none )

                    Err _ ->
                        ( updateObservation (failResultPage workspaceId offset "Failed to match repository files.") model, Cmd.none )

        GotObservationSubjectFacets workspaceId sessionEpoch generation fingerprint offset result ->
            if model.selectedWorkspaceId /= Just workspaceId || model.sessionRequestEpoch /= sessionEpoch || not (facetResponseMatches sessionEpoch generation fingerprint offset model.observations) then
                ( model, Cmd.none )

            else if model.observations.refreshPending && model.observations.refreshPass == Nothing then
                completeSupersededRead model
            else
                case result of
                    Ok paginated ->
                        if model.observations.refreshPass /= Nothing then
                            acceptRefreshChunk offset paginated.hasMore (List.length paginated.items)
                                (\pass -> { pass | facets = List.foldl (\facet -> Dict.insert (facetKey facet.subjectKind facet.subject) facet) pass.facets paginated.items, facetKeys = appendUnique pass.facetKeys (List.map (\facet -> facetKey facet.subjectKind facet.subject) paginated.items) }) model
                        else if validPage offset paginated.hasMore (List.map (\item -> facetKey item.subjectKind item.subject) paginated.items) model.observations.facetKeys then
                            ( updateObservation (mergeFacetPage offset paginated) model, Cmd.none )
                        else ( updateObservation (failResultPage workspaceId offset "Shared subjects are incomplete: the page made no valid progress.") model, Cmd.none )

                    Err _ ->
                        ( updateObservation (failResultPage workspaceId offset "Failed to load shared subjects.") model, Cmd.none )

        _ ->
            ( model, Cmd.none )


startEdit : Model -> ( Model, Cmd Msg )
startEdit model =
    case currentSelectedObservation model.observations of
        Just observation ->
            if canMutateObservation observation model then
                let
                    state =
                        model.observations

                    contextToken =
                        state.nextCurationContextToken

                    edit =
                        { workspaceId = observation.workspaceId
                        , observationId = observation.id
                        , sessionEpoch = model.sessionRequestEpoch
                        , contextToken = contextToken
                        , baseContent = observation.content
                        , baseContentVersion = observation.contentVersion
                        , baseUpdatedAt = observation.updatedAt
                        , draft = observation.content
                        , baseReviewedGitSha = reviewedSha observation
                        , reviewedGitShaDraft = reviewedSha observation
                        , latestCanonical = observation
                        , conflict = False
                        , saving = False
                        , error = Nothing
                        , activeRequest = Nothing
                        , activeCanonicalRequest = Nothing
                        , canonicalProvisional = False
                        }
                in
                ( updateObservation
                    (\current ->
                        { current
                            | edit = Just edit
                            , deleteConfirmation = Nothing
                            , nextCurationContextToken = contextToken + 1
                        }
                    )
                    model
                , focusElement "observation-edit-content"
                )

            else
                ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


saveEdit : Model -> ( Model, Cmd Msg )
saveEdit model =
    case model.observations.edit of
        Just edit ->
            if not (editContextIsCurrent edit model) || edit.saving || edit.conflict || edit.activeCanonicalRequest /= Nothing then
                ( model, Cmd.none )

            else
                case editValidationError edit of
                    Just message ->
                        ( updateObservation
                            (\state -> { state | edit = Just { edit | error = Just message } })
                            model
                        , Cmd.none
                        )

                    Nothing ->
                        let
                            requestToken =
                                model.observations.nextMutationRequestToken

                            request =
                                { workspaceId = edit.workspaceId
                                , observationId = edit.observationId
                                , sessionEpoch = edit.sessionEpoch
                                , contextToken = edit.contextToken
                                , requestToken = requestToken
                                }

                            prepared =
                                updateObservation
                                    (\state ->
                                        { state
                                            | edit = Just { edit | saving = True, error = Nothing, activeRequest = Just request }
                                            , nextMutationRequestToken = requestToken + 1
                                        }
                                    )
                                    model

                            ( tracked, requestId, clearCmd ) =
                                beginTrackedMutation [ edit.observationId ] prepared
                        in
                        ( tracked
                        , Cmd.batch
                            [ clearCmd
                            , Api.updateObservation model.flags.apiUrl edit.observationId edit.draft edit.reviewedGitShaDraft edit.baseContentVersion requestId (ObservationUpdated request)
                            ]
                        )

        Nothing ->
            ( model, Cmd.none )


reloadEdit : ObservationModel -> ObservationModel
reloadEdit state =
    { state
        | edit =
            state.edit
                |> Maybe.map
                    (\edit ->
                        if edit.saving || edit.activeCanonicalRequest /= Nothing then
                            edit
                        else
                        { edit
                            | baseContent = edit.latestCanonical.content
                            , baseContentVersion = edit.latestCanonical.contentVersion
                            , baseUpdatedAt = edit.latestCanonical.updatedAt
                            , draft = edit.latestCanonical.content
                            , baseReviewedGitSha = reviewedSha edit.latestCanonical
                            , reviewedGitShaDraft = reviewedSha edit.latestCanonical
                            , conflict = False
                            , saving = False
                            , error = Nothing
                            , activeRequest = Nothing
                            , canonicalProvisional = False
                        }
                    )
    }


rebaseEdit : ObservationModel -> ObservationModel
rebaseEdit state =
    { state
        | edit =
            state.edit
                |> Maybe.map
                    (\edit ->
                        if edit.saving || edit.activeCanonicalRequest /= Nothing then
                            edit
                        else
                        { edit
                            | baseContent = edit.latestCanonical.content
                            , baseContentVersion = edit.latestCanonical.contentVersion
                            , baseUpdatedAt = edit.latestCanonical.updatedAt
                            , baseReviewedGitSha = reviewedSha edit.latestCanonical
                            , conflict = False
                            , saving = False
                            , error = Nothing
                            , activeRequest = Nothing
                            , canonicalProvisional = False
                        }
                    )
    }


handleUpdateResponse : ObservationMutationRequest -> Result Api.ObservationUpdateError Api.Observation -> Model -> ( Model, Cmd Msg )
handleUpdateResponse request result model =
    if not (mutationResponseMatches request model) then
        ( model, Cmd.none )

    else
        case ( model.observations.edit, result ) of
            ( Just edit, Ok observation ) ->
                if observation.id /= request.observationId || observation.workspaceId /= request.workspaceId || not (sameObservationProvenance edit.latestCanonical observation) then
                    ( finishEditFailure "The server returned mismatched immutable provenance. Retry after reloading." edit model, Cmd.none )

                else if edit.latestCanonical.contentVersion /= edit.baseContentVersion && edit.latestCanonical.contentVersion /= observation.contentVersion then
                    refreshPendingAppliedResults (updateObservation
                        (\state ->
                            { state
                                | edit =
                                    Just
                                        { edit
                                            | latestCanonical = edit.latestCanonical
                                            , conflict = True
                                            , saving = False
                                            , error = Just "Another canonical version arrived while this save was in flight. Choose how to continue."
                                            , activeRequest = Nothing
                                        }
                            }
                        )
                        (retireObservationReads request model)
                    )

                else
                    let
                        accepted =
                            updateObservation
                                (applyCanonicalObservationWithProof True observation
                                    >> (\state -> { state | edit = Nothing })
                                )
                                (retireObservationReads request model)

                        ( refreshing, refreshCmd ) =
                            refreshActiveResults accepted

                        ( toasted, toastCmd ) =
                            addToast Success "Observation content updated" refreshing
                    in
                    ( toasted, Cmd.batch [ refreshCmd, toastCmd ] )

            ( Just edit, Err (Api.ObservationContentConflict latest) ) ->
                if latest.id /= request.observationId || latest.workspaceId /= request.workspaceId || not (sameObservationProvenance edit.latestCanonical latest) || latest.contentVersion == edit.baseContentVersion then
                    ( finishEditFailure "The server returned an inconsistent content-version conflict. Your draft is preserved." edit model, Cmd.none )
                else
                    let
                        ambiguous =
                            edit.latestCanonical.contentVersion /= edit.baseContentVersion
                                && edit.latestCanonical.contentVersion /= latest.contentVersion
                                && compareTimestamps edit.latestCanonical.updatedAt latest.updatedAt /= Just LT

                        canonical =
                            if ambiguous then
                                edit.latestCanonical
                            else
                                latest

                        conflicted =
                            updateObservation
                                (applyCanonicalObservationWithProof True canonical
                                    >> (\state -> { state | edit = Just { edit | latestCanonical = canonical, conflict = True, saving = False, error = Nothing, activeRequest = Nothing, canonicalProvisional = ambiguous } })
                                ) (retireObservationReads request model)
                    in
                    if ambiguous then
                        let
                            ( checking, checkCmd ) =
                                checkConflictCanonical conflicted

                            ( refreshing, refreshCmd ) =
                                refreshPendingAppliedResults checking
                        in
                        ( refreshing, Cmd.batch [ checkCmd, refreshCmd ] )
                    else
                        refreshPendingAppliedResults conflicted

            ( Just edit, Err (Api.ObservationUpdateHttpError (Http.BadStatus 404)) ) ->
                deletedAfterMutation "This observation was already deleted." request.observationId model

            ( Just edit, Err _ ) ->
                ( finishEditFailure "Failed to update observation. Your draft is preserved; retry when ready." edit model, Cmd.none )

            _ ->
                ( model, Cmd.none )


{-| A matching conditional response retires reads admitted before that response.
An equal timestamp and content cannot establish the age of an opaque version.
-}
retireObservationReads : ObservationMutationRequest -> Model -> Model
retireObservationReads request model =
    let
        webSocket =
            model.webSocket

        target =
            "workspace:" ++ request.workspaceId ++ "|entity:observation:" ++ request.observationId

        generation =
            Dict.get target webSocket.targetGenerations |> Maybe.withDefault 0 |> (+) 1

        retireDetail state =
            if state.activeDetailRequest |> Maybe.map (\active -> active.workspaceId == request.workspaceId && active.observationId == request.observationId) |> Maybe.withDefault False then
                { state | activeDetailRequest = Nothing, detailLoading = False, detailError = Nothing }
            else
                state
    in
    { model | webSocket = { webSocket | targetGenerations = Dict.insert target generation webSocket.targetGenerations } }
        |> updateObservation retireDetail


checkConflictCanonical : Model -> ( Model, Cmd Msg )
checkConflictCanonical model =
    case model.observations.edit of
        Just edit ->
            let
                token =
                    model.observations.nextMutationRequestToken

                request =
                    { workspaceId = edit.workspaceId, observationId = edit.observationId, sessionEpoch = edit.sessionEpoch, contextToken = edit.contextToken, requestToken = token }

                checking =
                    updateObservation
                        (\state -> { state | edit = Just { edit | activeCanonicalRequest = Just request, error = Just "Checking the current version before choosing how to continue. Your draft is preserved." }, nextMutationRequestToken = token + 1 })
                        model
            in
            ( checking, Api.fetchObservation model.flags.apiUrl edit.observationId (ObservationConflictCanonicalFetched request edit.latestCanonical.contentVersion) )

        Nothing ->
            ( model, Cmd.none )


handleConflictCanonicalResponse : ObservationMutationRequest -> String -> Result Http.Error Api.Observation -> Model -> ( Model, Cmd Msg )
handleConflictCanonicalResponse request knownVersion result model =
    case model.observations.edit of
        Just edit ->
            if model.auth.status /= AuthReady || not (editContextIsCurrent edit model) || edit.activeCanonicalRequest /= Just request || request.workspaceId /= edit.workspaceId || request.observationId /= edit.observationId || request.sessionEpoch /= edit.sessionEpoch || request.contextToken /= edit.contextToken then
                ( model, Cmd.none )
            else
                case result of
                    Ok canonical ->
                        if canonical.id /= request.observationId || canonical.workspaceId /= request.workspaceId || not (sameObservationProvenance edit.latestCanonical canonical) then
                            ( finishConflictCheck "The server returned mismatched immutable provenance. The retained version is provisional; retrying it can conflict again." edit model, Cmd.none )
                        else if edit.latestCanonical.contentVersion /= knownVersion && edit.latestCanonical.contentVersion /= canonical.contentVersion then
                            ( finishConflictCheck "Another canonical version arrived during the check. The retained version is provisional; retrying it can conflict again." edit model, Cmd.none )
                        else
                            updateObservation
                                (applyCanonicalObservationWithProof True canonical
                                    >> (\state -> { state | edit = Just { edit | latestCanonical = canonical, activeCanonicalRequest = Nothing, canonicalProvisional = False, error = Nothing } })
                                )
                                (retireObservationReads request model)
                                |> refreshPendingAppliedResults

                    Err (Http.BadStatus 404) ->
                        deletedAfterMutation "This observation was already deleted." request.observationId model

                    Err _ ->
                        ( finishConflictCheck "Failed to check the current version. Your draft is preserved; the retained version is provisional and a conditional retry can conflict again." edit model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


finishConflictCheck : String -> ObservationEditState -> Model -> Model
finishConflictCheck message edit =
    updateObservation (\state -> { state | edit = Just { edit | activeCanonicalRequest = Nothing, error = Just message } })


refreshPendingAppliedResults : Model -> ( Model, Cmd Msg )
refreshPendingAppliedResults model =
    let
        state =
            model.observations

        pending =
            if state.requestMode == ObservationFacetMode then
                state.facetLoading && state.facetExpectedOffset /= Nothing
            else
                state.loading && state.expectedOffset /= Nothing
    in
    if pending then
        refreshActiveResults model
    else
        ( model, Cmd.none )


finishEditFailure : String -> ObservationEditState -> Model -> Model
finishEditFailure message edit model =
    updateObservation
        (\state -> { state | edit = Just { edit | saving = False, error = Just message, activeRequest = Nothing } })
        model


openDeleteConfirmation : Model -> ( Model, Cmd Msg )
openDeleteConfirmation model =
    if hasProtectedEdit model then
        returnToDraft model

    else
        openDeleteConfirmationWithoutDraft model


openDeleteConfirmationWithoutDraft : Model -> ( Model, Cmd Msg )
openDeleteConfirmationWithoutDraft model =
    case currentSelectedObservation model.observations of
        Just observation ->
            if canMutateObservation observation model then
                let
                    state =
                        model.observations

                    contextToken =
                        state.nextCurationContextToken

                    confirmation =
                        { workspaceId = observation.workspaceId
                        , observationId = observation.id
                        , sessionEpoch = model.sessionRequestEpoch
                        , contextToken = contextToken
                        , targetContent = observation.content
                        , deleting = False
                        , error = Nothing
                        , activeRequest = Nothing
                        }
                in
                ( updateObservation
                    (\current ->
                        { current
                            | deleteConfirmation = Just confirmation
                            , edit = Nothing
                            , nextCurationContextToken = contextToken + 1
                        }
                    )
                    model
                , focusElement "observation-delete-cancel"
                )

            else
                ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


confirmDelete : Model -> ( Model, Cmd Msg )
confirmDelete model =
    case model.observations.deleteConfirmation of
        Just confirmation ->
            if confirmation.deleting then
                ( model, Cmd.none )

            else if not (deleteContextIsCurrent confirmation model) then
                ( updateObservation
                    (\state -> { state | deleteConfirmation = Just { confirmation | error = Just "You no longer have permission to delete this observation." } })
                    model
                , Cmd.none
                )

            else
                let
                    requestToken =
                        model.observations.nextMutationRequestToken

                    request =
                        { workspaceId = confirmation.workspaceId
                        , observationId = confirmation.observationId
                        , sessionEpoch = confirmation.sessionEpoch
                        , contextToken = confirmation.contextToken
                        , requestToken = requestToken
                        }

                    prepared =
                        updateObservation
                            (\state ->
                                { state
                                    | deleteConfirmation = Just { confirmation | deleting = True, error = Nothing, activeRequest = Just request }
                                    , nextMutationRequestToken = requestToken + 1
                                }
                            )
                            model

                    ( tracked, requestId, clearCmd ) =
                        beginTrackedMutation [ confirmation.observationId ] prepared
                in
                ( tracked
                , Cmd.batch
                    [ clearCmd
                    , Api.deleteObservation model.flags.apiUrl confirmation.observationId requestId (ObservationDeleted request)
                    , focusElement "observation-delete-dialog"
                    ]
                )

        Nothing ->
            ( model, Cmd.none )


cancelDelete : Model -> ( Model, Cmd Msg )
cancelDelete model =
    case model.observations.deleteConfirmation of
        Just confirmation ->
            if confirmation.deleting then
                ( model, Cmd.none )

            else
                ( updateObservation (\state -> { state | deleteConfirmation = Nothing }) model
                , focusElement "observation-delete"
                )

        Nothing ->
            ( model, Cmd.none )


handleDeleteDialogKeyDown : String -> Bool -> String -> Model -> ( Model, Cmd Msg )
handleDeleteDialogKeyDown key shiftKey targetId model =
    case model.observations.deleteConfirmation of
        Just confirmation ->
            if not (Permissions.canEditCurrentWorkspace model) then
                ( reconcileCurationPermission model, Cmd.none )

            else if key == "Escape" then
                cancelDelete model

            else if key == "Tab" then
                ( model, focusElement (deleteDialogFocusTarget confirmation.deleting shiftKey targetId) )

            else
                ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


deleteDialogFocusTarget : Bool -> Bool -> String -> String
deleteDialogFocusTarget deleting shiftKey targetId =
    if deleting then
        "observation-delete-dialog"

    else if targetId == "observation-delete-cancel" then
        "observation-delete-confirm"

    else if targetId == "observation-delete-confirm" then
        "observation-delete-cancel"

    else if shiftKey then
        "observation-delete-confirm"

    else
        "observation-delete-cancel"


handleDeleteResponse : ObservationMutationRequest -> Result Http.Error () -> Model -> ( Model, Cmd Msg )
handleDeleteResponse request result model =
    if not (mutationResponseMatches request model) then
        ( model, Cmd.none )

    else
        case result of
            Ok _ ->
                deletedAfterMutation "Observation permanently deleted" request.observationId model

            Err (Http.BadStatus 404) ->
                deletedAfterMutation "Observation was already deleted" request.observationId model

            Err _ ->
                case model.observations.deleteConfirmation of
                    Just confirmation ->
                        ( updateObservation
                            (\state ->
                                { state
                                    | deleteConfirmation =
                                        Just
                                            { confirmation
                                                | deleting = False
                                                , error = Just "Failed to delete observation. Nothing was removed; retry or cancel."
                                                , activeRequest = Nothing
                                            }
                                }
                            )
                            model
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )


deletedAfterMutation : String -> String -> Model -> ( Model, Cmd Msg )
deletedAfterMutation message observationId model =
    let
        ( cleaned, cleanupCmd ) =
            reconcileDeletedObservation observationId model

        ( refreshing, refreshCmd ) =
            refreshActiveResults cleaned

        ( toasted, toastCmd ) =
            addToast Success message refreshing
    in
    ( toasted, Cmd.batch [ cleanupCmd, refreshCmd, toastCmd, focusElement "observation-results" ] )


mutationResponseMatches : ObservationMutationRequest -> Model -> Bool
mutationResponseMatches request model =
    let
        matches activeRequest workspaceId observationId sessionEpoch contextToken =
            model.selectedWorkspaceId
                == Just workspaceId
                && model.sessionRequestEpoch
                == sessionEpoch
                && request.workspaceId
                == workspaceId
                && request.observationId
                == observationId
                && request.sessionEpoch
                == sessionEpoch
                && request.contextToken
                == contextToken
                && activeRequest
                == Just request
    in
    case ( model.observations.edit, model.observations.deleteConfirmation ) of
        ( Just edit, _ ) ->
            matches edit.activeRequest edit.workspaceId edit.observationId edit.sessionEpoch edit.contextToken

        ( _, Just confirmation ) ->
            matches confirmation.activeRequest confirmation.workspaceId confirmation.observationId confirmation.sessionEpoch confirmation.contextToken

        _ ->
            False


editContextIsCurrent : ObservationEditState -> Model -> Bool
editContextIsCurrent edit model =
    model.selectedWorkspaceId
        == Just edit.workspaceId
        && model.sessionRequestEpoch
        == edit.sessionEpoch
        && Permissions.canEditCurrentWorkspace model
        && repositoryWorkspaceId model
        == Just edit.workspaceId


deleteContextIsCurrent : ObservationDeleteState -> Model -> Bool
deleteContextIsCurrent confirmation model =
    model.selectedWorkspaceId
        == Just confirmation.workspaceId
        && model.sessionRequestEpoch
        == confirmation.sessionEpoch
        && Permissions.canEditCurrentWorkspace model
        && repositoryWorkspaceId model
        == Just confirmation.workspaceId


canMutateObservation : Api.Observation -> Model -> Bool
canMutateObservation observation model =
    repositoryWorkspaceId model
        == Just observation.workspaceId
        && Permissions.canEditCurrentWorkspace model


currentSelectedObservation : ObservationModel -> Maybe Api.Observation
currentSelectedObservation state =
    state.selectedId
        |> Maybe.andThen
            (\observationId ->
                case ( state.selectedDetail, Dict.get observationId state.items ) of
                    ( Just detail, Just listed ) ->
                        Just (preferNewerObservation detail listed)

                    ( Just detail, Nothing ) ->
                        Just detail

                    ( Nothing, Just listed ) ->
                        Just listed

                    _ ->
                        Nothing
            )


observationContentError : String -> Maybe String
observationContentError content =
    if String.isEmpty (String.trim content) then
        Just "Observation content must not be blank."

    else if utf8Bytes content > 524288 then
        Just "Observation content must not exceed 512 KiB of UTF-8 text."

    else
        Nothing


{-| Compatibility entry point used by workspace bootstrap. Interactive requests
use `startReloadForSession` so their fingerprint includes the auth epoch.
-}
startReload : String -> ObservationModel -> ObservationModel
startReload workspaceId state =
    startReloadForSession state.requestSessionEpoch workspaceId state


startReloadForSession : Int -> String -> ObservationModel -> ObservationModel
startReloadForSession sessionEpoch workspaceId state =
    startResultReload ObservationFlatMode sessionEpoch workspaceId
        { state
            | matchPathsInput = ""
            , matchAppliedPaths = []
            , selectedFacet = Nothing
            , matchValidationError = Nothing
            , matchEvidence = Dict.empty
            , expandedMatchGroups = Dict.empty
            , browseReturn = Nothing
        }


startResultReload : ObservationRequestMode -> Int -> String -> ObservationModel -> ObservationModel
startResultReload mode sessionEpoch workspaceId draftState =
    let
        state =
            commitAppliedQuery { draftState | requestMode = mode }
    in
    { state
        | items = Dict.empty
        , refreshPass = Nothing
        , refreshPending = False
        , refreshError = Nothing
        , orderedIds = []
        , hasMore = False
        , loading = True
        , resultsStale = False
        , error = Nothing
        , requestMode = mode
        , requestGeneration = state.requestGeneration + 1
        , requestSessionEpoch = sessionEpoch
        , queryFingerprint = resultFingerprint mode sessionEpoch workspaceId state
        , expectedOffset = Just 0
        , nextOffset = 0
        , matchEvidence =
            if mode == ObservationMatchMode then
                state.matchEvidence

            else
                Dict.empty
    }


reload : Model -> ( Model, Cmd Msg )
reload model =
    reloadResultMode ObservationFlatMode model


applyObservationFilters : Model -> ( Model, Cmd Msg )
applyObservationFilters model =
    if filterValidationError model.observations /= Nothing then ( model, Cmd.none ) else
    case model.observations.requestMode of
        ObservationFacetMode ->
            reloadFacets model

        ObservationMatchMode ->
            reloadAppliedMatch model

        ObservationExactSubjectMode ->
            startTree False model

        ObservationFlatMode ->
            reloadResultMode ObservationFlatMode model


switchBrowseMode : ObservationRequestMode -> Model -> ( Model, Cmd Msg )
switchBrowseMode mode model =
    let
        previous = model.observations
        queries = Dict.insert (modeKey previous.requestMode) previous.query previous.modeQueries
        subjects = Dict.insert (modeKey previous.requestMode) previous.subjectQuery previous.modeSubjectQueries
        scrolls = Dict.insert (modeKey previous.requestMode) (Maybe.withDefault previous.viewport.top previous.preferenceScroll) previous.modeScrolls
        prepared = updateObservation (\state -> retireFilterReads
            { state | requestMode = mode, query = Dict.get (modeKey mode) queries |> Maybe.withDefault "", subjectQuery = Dict.get (modeKey mode) subjects |> Maybe.withDefault "", modeQueries = queries, modeSubjectQueries = subjects, modeScrolls = scrolls
                , selectedFacet = if mode == ObservationExactSubjectMode then state.selectedFacet else Nothing
                , matchValidationError = Nothing, browseReturn = Nothing }) model
        preserve ( updated, command ) =
            let state = updated.observations
                viewport = state.viewport
            in ( { updated | observations = { state | viewport = { viewport | top = Dict.get (modeKey mode) scrolls |> Maybe.withDefault previous.viewport.top, preserveScroll = True, preserveExtent = Basics.max viewport.preserveExtent (HierarchyViewport.height model.observations.viewport.index) } } }, command )
    in
    (if filterValidationError prepared.observations /= Nothing then ( prepared, Cmd.none )
    else
        case mode of
            ObservationFacetMode -> reloadFacets prepared
            ObservationExactSubjectMode -> startTree False prepared
            ObservationMatchMode -> applyLiveFilters prepared
            ObservationFlatMode -> reloadResultMode ObservationFlatMode prepared)
        |> preserve

selectFacet : Api.SubjectKind -> String -> Model -> ( Model, Cmd Msg )
selectFacet subjectKind subject model =
    if filterValidationError model.observations /= Nothing then ( model, Cmd.none ) else
    model
        |> updateObservation
            (\state ->
                retireFilterReads { state
                    | selectedFacet = Just { subjectKind = subjectKind, subject = subject }
                    , requestMode = ObservationExactSubjectMode
                    , tree = ObservationTree.revealPath {subjectKind = subjectKind, subject = subject} (ObservationTree.select subject (Just {subjectKind = subjectKind, subject = subject}) state.tree)
                    , treeSelectionPending = False
                    , matchValidationError = Nothing
                    , matchEvidence = Dict.empty
                    , expandedMatchGroups = Dict.empty
                    , browseReturn = Nothing
                }
            )
        |> startTree False
        |> (\( updated, command ) -> ( updated, Cmd.batch [ command, focusElement "observation-mode-heading" ] ))


restoreBrowseAfterMatch : Model -> ( Model, Cmd Msg )
restoreBrowseAfterMatch model =
    if List.isEmpty model.observations.matchAppliedPaths then
        ( updateObservation
            (\state ->
                { state
                    | matchPathsInput = ""
                    , matchValidationError = Nothing
                }
            )
            model
        , Cmd.none
        )

    else
        let
            restored =
                model.observations.browseReturn
                    |> Maybe.withDefault
                        { requestMode = ObservationFacetMode
                        , subjectKind = model.observations.subjectKind
                        , subject = model.observations.subject
                        , selectedFacet = model.observations.selectedFacet
                        , query = Dict.get "tree" model.observations.modeQueries |> Maybe.withDefault ""
                        , subjectQuery = Dict.get "tree" model.observations.modeSubjectQueries |> Maybe.withDefault ""
                        , gitSha = model.observations.gitSha
                        , currentGitSha = model.observations.currentGitSha
                        , historyGitSha = model.observations.historyGitSha
                        }

            prepared =
                updateObservation
                    (\state ->
                        { state
                            | requestMode = restored.requestMode
                            , subjectKind = restored.subjectKind
                            , subject = restored.subject
                            , selectedFacet = restored.selectedFacet
                            , query = restored.query
                            , subjectQuery = restored.subjectQuery
                            , gitSha = restored.gitSha, currentGitSha = restored.currentGitSha, historyGitSha = restored.historyGitSha
                            , matchPathsInput = ""
                            , matchAppliedPaths = []
                            , matchValidationError = Nothing
                            , matchEvidence = Dict.empty
                            , expandedMatchGroups = Dict.empty
                            , browseReturn = Nothing
                        }
                    )
                    model
        in
        case restored.requestMode of
            ObservationFacetMode ->
                reloadFacets prepared

            ObservationExactSubjectMode ->
                reloadResultMode ObservationExactSubjectMode prepared

            _ ->
                reloadResultMode ObservationFlatMode prepared


reloadResultMode : ObservationRequestMode -> Model -> ( Model, Cmd Msg )
reloadResultMode mode model =
    case repositoryWorkspaceId model of
        Just workspaceId ->
            let
                state =
                    startResultReload mode model.sessionRequestEpoch workspaceId model.observations

                updated =
                    updateObservation (always state) model
            in
            fetchPage 0 updated

        Nothing ->
            ( updateObservation (\state -> { state | loading = False, expectedOffset = Nothing }) model, Cmd.none )


reloadFacets : Model -> ( Model, Cmd Msg )
reloadFacets model =
    startTree False model


refreshActiveResults : Model -> ( Model, Cmd Msg )
refreshActiveResults model =
    let
        ( refreshed, resultsCmd ) =
            refreshActiveResultsRaw (updateObservation invalidateCounts model)

        state =
            refreshed.observations

        ( hydrated, detailCmd ) =
            if state.selectedDetail == Nothing && state.activeDetailRequest == Nothing && state.detailError == Nothing then
                case state.selectedId of
                    Just observationId -> selectObservation observationId refreshed
                    Nothing -> ( refreshed, Cmd.none )
            else
                ( refreshed, Cmd.none )

        ( repaired, linkCmd ) =
            if state.linkNotice /= Nothing && state.linkNotice /= Just Helpers.excludedObservationNotice then
                Helpers.writeObservationHistory False hydrated
            else
                ( hydrated, Cmd.none )
    in
    ( repaired, Cmd.batch [ resultsCmd, detailCmd, linkCmd ] )


refreshActiveResultsRaw : Model -> ( Model, Cmd Msg )
refreshActiveResultsRaw model =
    let
        state = model.observations
    in
    if model.activeTab /= ObservationsTab && state.requestMode == ObservationFlatMode then
        (updateObservation (\current -> {current | resultsStale = True, refreshPending = True, refreshPass = Maybe.map (\pass -> {pass | invalidated = True}) current.refreshPass}) model, Cmd.none)
    else if treeMode state.requestMode && state.filterDebounce == Nothing && filterValidationError state == Nothing then
        if List.any (\request -> request.lifetime == state.tree.lifetime) state.tree.physical then
            (updateObservation (\current -> {current | refreshPending = True}) model, Cmd.none)
        else startTree True model
    else if state.filterDebounce /= Nothing || filterValidationError state /= Nothing then
        ( updateObservation (\current -> { current | refreshPending = True }) model, Cmd.none )
    else if state.refreshPass /= Nothing then
        ( updateObservation (\current -> { current | refreshPass = Maybe.map (\pass -> { pass | invalidated = True }) current.refreshPass, refreshPending = True }) model, Cmd.none )
    else if state.loading || state.facetLoading then
        ( updateObservation (\current -> { current | refreshPending = True }) model, Cmd.none )
    else
        case repositoryWorkspaceId model of
            Just workspaceId ->
                if model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) then ( model, Cmd.none )
                else
                    let
                        facets = state.requestMode == ObservationFacetMode
                        pass = { query = querySnapshot state, targetOffset = Basics.max (if state.requestMode == ObservationMatchMode then pageSize else 50) (if facets then state.facetNextOffset else state.nextOffset), items = Dict.empty, orderedIds = [], matchEvidence = Dict.empty, facets = Dict.empty, facetKeys = [], invalidated = False }
                        started = if facets then startFacetRefresh model.sessionRequestEpoch workspaceId state
                            else if state.requestMode == ObservationMatchMode then startMatchRefresh model.sessionRequestEpoch workspaceId state
                            else startResultRefresh state.requestMode model.sessionRequestEpoch workspaceId state
                        prepared = updateObservation (always { started | refreshPass = Just pass, refreshPending = False, refreshError = Nothing }) model
                    in
                    fetchRefreshPage 0 prepared
            Nothing -> ( model, Cmd.none )


fetchRefreshPage : Int -> Model -> ( Model, Cmd Msg )
fetchRefreshPage offset model =
    case model.observations.requestMode of
        ObservationFacetMode -> fetchFacetPage offset model
        ObservationMatchMode -> fetchMatchPage offset model.observations.matchAppliedPaths model
        _ -> fetchPage offset model


continueAutomaticRefresh : Model -> ( Model, Cmd Msg )
continueAutomaticRefresh model =
    if model.observations.refreshPending && model.observations.refreshPass == Nothing && not model.observations.loading && not model.observations.facetLoading && model.observations.refreshError == Nothing then
        refreshActiveResultsRaw model
    else ( model, Cmd.none )


completeSupersededRead : Model -> ( Model, Cmd Msg )
completeSupersededRead model =
    continueAutomaticRefresh (updateObservation (\state -> { state | loading = False, facetLoading = False, expectedOffset = Nothing, facetExpectedOffset = Nothing }) model)


acceptResultRefresh : Int -> Api.PaginatedResult Api.Observation -> Model -> ( Model, Cmd Msg )
acceptResultRefresh offset page model =
    acceptRefreshChunk offset page.hasMore (List.length page.items)
        (\pass -> { pass | items = List.foldl (\item -> Dict.insert item.id item) pass.items page.items, orderedIds = appendUnique pass.orderedIds (List.map .id page.items) }) model


appendUnique : List String -> List String -> List String
appendUnique prior added =
    let
        ( reversed, _ ) = List.foldl (\key ( keys, seen ) -> if Set.member key seen then ( keys, seen ) else ( key :: keys, Set.insert key seen )) ( [], Set.fromList prior ) added
    in prior ++ List.reverse reversed


acceptRefreshChunk : Int -> Bool -> Int -> (ObservationRefreshPass -> ObservationRefreshPass) -> Model -> ( Model, Cmd Msg )
acceptRefreshChunk offset hasMore count merge model =
    let state = model.observations in
    case state.refreshPass of
        Nothing -> ( model, Cmd.none )
        Just old ->
            if old.query /= querySnapshot state then ( model, Cmd.none )
            else if old.invalidated then
                refreshActiveResultsRaw (updateObservation (\current -> { current | refreshPass = Nothing, loading = False, facetLoading = False, expectedOffset = Nothing, facetExpectedOffset = Nothing, refreshPending = False }) model)
            else
                let
                    pass = merge old
                    before = if state.requestMode == ObservationFacetMode then List.length old.facetKeys else List.length old.orderedIds
                    after = if state.requestMode == ObservationFacetMode then List.length pass.facetKeys else List.length pass.orderedIds
                    next = offset + count
                    fail message = ( updateObservation (\current -> { current | refreshPass = Nothing, refreshPending = False, refreshError = Just message, loading = False, facetLoading = False, expectedOffset = Nothing, facetExpectedOffset = Nothing }) model, Cmd.none )
                in
                if count > pageSize || (hasMore && (not (List.member count [ 50, pageSize ]) || after <= before)) then fail "Automatic refresh is incomplete: the page made no valid progress."
                else if hasMore && next >= 10000 && next < old.targetOffset then fail "Automatic refresh is incomplete: the bounded offset limit was reached."
                else if hasMore && next < old.targetOffset then
                    fetchRefreshPage next (updateObservation (\current -> { current | refreshPass = Just pass }) model)
                else
                    let
                        currentItems = Dict.map (\identity item -> Dict.get identity state.items |> Maybe.map (preferNewerObservation item) |> Maybe.withDefault item) pass.items
                        evidence = Dict.map (\identity match -> { match | observation = Dict.get identity currentItems |> Maybe.withDefault match.observation }) pass.matchEvidence
                        committed = if state.requestMode == ObservationFacetMode then
                            { state | facets = pass.facets, facetKeys = pass.facetKeys, facetHasMore = hasMore, facetNextOffset = next }
                            else { state | items = currentItems, orderedIds = pass.orderedIds, matchEvidence = evidence, hasMore = hasMore, nextOffset = next }
                        reconciled = List.foldl applyAuthoritativeObservation committed (Dict.values pass.items)
                        finished = { reconciled | completedQuery = Just (querySnapshot state), refreshPass = Nothing, refreshPending = False, refreshError = Nothing, resultsStale = False, loading = False, facetLoading = False, expectedOffset = Nothing, facetExpectedOffset = Nothing, failedRequest = Nothing }
                    in
                    ( updateObservation (always finished) model, Cmd.none )


startResultRefresh : ObservationRequestMode -> Int -> String -> ObservationModel -> ObservationModel
startResultRefresh mode sessionEpoch workspaceId input =
    let
        state =
            ensureAppliedQuery input
    in
    { state
        | loading = True
        , failedRequest = Nothing
        , resultsStale = False
        , error = Nothing
        , requestGeneration = state.requestGeneration + 1
        , requestSessionEpoch = sessionEpoch
        , queryFingerprint = resultFingerprint mode sessionEpoch workspaceId state
        , expectedOffset = Just 0
    }


startMatchRefresh : Int -> String -> ObservationModel -> ObservationModel
startMatchRefresh sessionEpoch workspaceId input =
    let
        state =
            ensureAppliedQuery input
    in
    { state
        | loading = True
        , failedRequest = Nothing
        , resultsStale = False
        , error = Nothing
        , requestGeneration = state.requestGeneration + 1
        , requestSessionEpoch = sessionEpoch
        , queryFingerprint = matchFingerprint sessionEpoch workspaceId state
        , expectedOffset = Just 0
    }


startFacetReload : Int -> String -> ObservationModel -> ObservationModel
startFacetReload sessionEpoch workspaceId draftState =
    let
        state =
            commitAppliedQuery { draftState | requestMode = ObservationFacetMode }
    in
    { state
        | requestMode = ObservationFacetMode
        , refreshPass = Nothing
        , refreshPending = False
        , refreshError = Nothing
        , facets = Dict.empty
        , facetKeys = []
        , facetHasMore = False
        , facetLoading = True
        , facetError = Nothing
        , facetRequestGeneration = state.facetRequestGeneration + 1
        , facetRequestSessionEpoch = sessionEpoch
        , facetFingerprint = facetFingerprintFor sessionEpoch (facetQuery workspaceId 0 state)
        , facetExpectedOffset = Just 0
        , facetNextOffset = 0
    }


startFacetRefresh : Int -> String -> ObservationModel -> ObservationModel
startFacetRefresh sessionEpoch workspaceId input =
    let
        state =
            ensureAppliedQuery input
    in
    { state
        | resultsStale = False
        , failedRequest = Nothing
        , facetLoading = True
        , facetError = Nothing
        , facetRequestGeneration = state.facetRequestGeneration + 1
        , facetRequestSessionEpoch = sessionEpoch
        , facetFingerprint = facetFingerprintFor sessionEpoch (facetQuery workspaceId 0 state)
        , facetExpectedOffset = Just 0
    }


fetchPage : Int -> Model -> ( Model, Cmd Msg )
fetchPage offset model =
    case repositoryWorkspaceId model of
        Just workspaceId ->
            let
                state =
                    model.observations

                updated =
                    updateObservation (\current -> { current | loading = True, error = Nothing, expectedOffset = Just offset }) model
            in
            requestDiscovery (DiscoveryFlat Nothing state.requestGeneration state.queryFingerprint offset) updated

        Nothing ->
            ( model, Cmd.none )


fetchFacetPage : Int -> Model -> ( Model, Cmd Msg )
fetchFacetPage offset model =
    case repositoryWorkspaceId model of
        Nothing ->
            ( model, Cmd.none )

        Just workspaceId ->
            let
                state =
                    model.observations

                updated =
                    updateObservation (\current -> { current | facetLoading = True, facetError = Nothing, facetExpectedOffset = Just offset }) model
            in
            ( updated
            , Api.fetchObservationSubjectFacets model.flags.apiUrl
                (facetQuery workspaceId offset state)
                (GotObservationSubjectFacets workspaceId model.sessionRequestEpoch state.facetRequestGeneration state.facetFingerprint offset)
            )


loadMoreFacets : Model -> ( Model, Cmd Msg )
loadMoreFacets model =
    case repositoryWorkspaceId model of
        Just workspaceId ->
            if canLoadMoreFacets workspaceId model.observations then
                fetchFacetPage model.observations.facetNextOffset model

            else
                ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


matchObservations : Model -> ( Model, Cmd Msg )
matchObservations model =
    if filterValidationError model.observations /= Nothing then ( model, Cmd.none ) else
    case repositoryWorkspaceId model of
        Nothing ->
            ( model, Cmd.none )

        Just workspaceId ->
            case normalizeMatchPaths model.observations.matchPathsInput of
                Err message ->
                    ( updateObservation (\state -> { state | matchValidationError = Just message }) model, Cmd.none )

                Ok paths ->
                    let
                        previous = model.observations
                        state =
                            startMatchReload model.sessionRequestEpoch workspaceId paths {previous | filterDebounce = Nothing, nextFilterToken = previous.nextFilterToken + 1}

                        updated =
                            updateObservation (always state) model
                    in
                    fetchMatchPage 0 paths updated


reloadAppliedMatch : Model -> ( Model, Cmd Msg )
reloadAppliedMatch model =
    case ( repositoryWorkspaceId model, model.observations.matchAppliedPaths ) of
        ( Just workspaceId, (_ :: _) as paths ) ->
            let
                state =
                    startMatchReload model.sessionRequestEpoch workspaceId paths model.observations

                updated =
                    updateObservation (always state) model
            in
            fetchMatchPage 0 paths updated

        _ ->
            ( model, Cmd.none )


fetchMatchPage : Int -> List String -> Model -> ( Model, Cmd Msg )
fetchMatchPage offset paths model =
    case repositoryWorkspaceId model of
        Nothing ->
            ( model, Cmd.none )

        Just workspaceId ->
            let
                state =
                    model.observations

                updated =
                    updateObservation (\current -> { current | loading = True, error = Nothing, expectedOffset = Just offset }) model
            in
            ( updated
            , Api.fetchObservationMatches model.flags.apiUrl
                (matchQuery workspaceId paths offset state)
                (GotObservationMatches workspaceId model.sessionRequestEpoch state.requestGeneration state.queryFingerprint offset)
            )


repositoryWorkspaceId : Model -> Maybe String
repositoryWorkspaceId model =
    model.selectedWorkspaceId
        |> Maybe.andThen
            (\workspaceId ->
                Dict.get workspaceId model.workspaces
                    |> Maybe.andThen
                        (\workspace ->
                            if workspace.workspaceType == Api.Repository then
                                Just workspaceId

                            else
                                Nothing
                        )
            )


listQuery : String -> Int -> ObservationModel -> Api.ObservationListQuery
listQuery workspaceId offset inputState =
    let
        state =
            appliedState inputState
    in
    { workspaceId = workspaceId
    , subjectKind =
        case ( state.requestMode, state.selectedFacet ) of
            ( ObservationExactSubjectMode, Just selectedFacet ) ->
                Just selectedFacet.subjectKind

            _ ->
                state.subjectKind
    , subject =
        case ( state.requestMode, state.selectedFacet ) of
            ( ObservationFlatMode, _ ) ->
                nonEmpty state.subject

            ( ObservationExactSubjectMode, Just selectedFacet ) ->
                Just selectedFacet.subject

            _ ->
                Nothing
    , gitSha = nonEmpty state.gitSha
    , currentGitSha = nonEmpty state.currentGitSha
    , historyGitSha = nonEmpty state.historyGitSha
    , query = if state.requestMode == ObservationExactSubjectMode then Nothing else nonEmpty state.query
    , limit = pageSize
    , offset = offset
    }


facetQuery : String -> Int -> ObservationModel -> Api.ObservationSubjectFacetQuery
facetQuery workspaceId offset inputState =
    let
        state =
            appliedState inputState
    in
    { workspaceId = workspaceId
    , subjectKind = state.subjectKind
    , gitSha = nonEmpty state.gitSha
    , currentGitSha = nonEmpty state.currentGitSha
    , historyGitSha = nonEmpty state.historyGitSha
    , query = nonEmpty state.query
    , limit = pageSize
    , offset = offset
    }


matchQuery : String -> List String -> Int -> ObservationModel -> Api.ObservationMatchQuery
matchQuery workspaceId paths offset inputState =
    let
        state =
            appliedState inputState
    in
    { workspaceId = workspaceId
    , paths =
        inputState.appliedQuery
            |> Maybe.map .matchAppliedPaths
            |> Maybe.withDefault paths
    , subjectKind = state.subjectKind
    , gitSha = nonEmpty state.gitSha
    , currentGitSha = nonEmpty state.currentGitSha
    , historyGitSha = nonEmpty state.historyGitSha
    , query = nonEmpty state.query
    , limit = pageSize
    , offset = offset
    }


startMatchReload : Int -> String -> List String -> ObservationModel -> ObservationModel
startMatchReload sessionEpoch workspaceId paths draftState =
    let
        previous =
            appliedState draftState

        state =
            commitAppliedQuery { draftState | requestMode = ObservationMatchMode, matchAppliedPaths = paths }

        browseReturn =
            if draftState.requestMode == ObservationMatchMode then
                state.browseReturn

            else
                Just
                    { requestMode = previous.requestMode
                    , subjectKind = previous.subjectKind
                    , subject = previous.subject
                    , selectedFacet = previous.selectedFacet
                    , query = previous.query
                    , subjectQuery = previous.subjectQuery
                    , gitSha = previous.gitSha, currentGitSha = previous.currentGitSha, historyGitSha = previous.historyGitSha
                    }
    in
    { state
        | items = Dict.empty
        , refreshPass = Nothing
        , refreshPending = False
        , refreshError = Nothing
        , orderedIds = []
        , hasMore = False
        , loading = True
        , error = Nothing
        , requestMode = ObservationMatchMode
        , matchAppliedPaths = paths
        , matchValidationError = Nothing
        , matchEvidence = Dict.empty
        , expandedMatchGroups = Dict.empty
        , browseReturn = browseReturn
        , requestGeneration = state.requestGeneration + 1
        , requestSessionEpoch = sessionEpoch
        , queryFingerprint = matchFingerprint sessionEpoch workspaceId { state | matchAppliedPaths = paths }
        , expectedOffset = Just 0
        , nextOffset = 0
    }


queryFingerprint : Api.ObservationListQuery -> String
queryFingerprint query =
    String.join "\u{001F}"
        [ query.workspaceId
        , query.subjectKind |> Maybe.map Api.subjectKindToString |> Maybe.withDefault ""
        , query.subject |> Maybe.withDefault ""
        , query.gitSha |> Maybe.withDefault ""
        , query.currentGitSha |> Maybe.withDefault ""
        , query.historyGitSha |> Maybe.withDefault ""
        , query.query |> Maybe.withDefault ""
        , String.fromInt query.limit
        ]


resultFingerprint : ObservationRequestMode -> Int -> String -> ObservationModel -> String
resultFingerprint mode sessionEpoch workspaceId state =
    String.join "\u{001F}"
        [ modeName mode
        , String.fromInt sessionEpoch
        , queryFingerprint (listQuery workspaceId 0 { state | requestMode = mode })
        , state.subjectQuery
        ]


facetFingerprintFor : Int -> Api.ObservationSubjectFacetQuery -> String
facetFingerprintFor sessionEpoch query =
    String.join "\u{001F}"
        [ "facets"
        , String.fromInt sessionEpoch
        , query.workspaceId
        , query.subjectKind |> Maybe.map Api.subjectKindToString |> Maybe.withDefault ""
        , query.gitSha |> Maybe.withDefault ""
        , query.currentGitSha |> Maybe.withDefault ""
        , query.historyGitSha |> Maybe.withDefault ""
        , query.query |> Maybe.withDefault ""
        , String.fromInt query.limit
        ]


matchFingerprint : Int -> String -> ObservationModel -> String
matchFingerprint sessionEpoch workspaceId inputState =
    let
        state =
            appliedState inputState
    in
    String.join "\u{001F}"
        [ "match"
        , String.fromInt sessionEpoch
        , workspaceId
        , state.subjectKind |> Maybe.map Api.subjectKindToString |> Maybe.withDefault ""
        , state.gitSha |> String.trim
        , state.currentGitSha |> String.trim
        , state.historyGitSha |> String.trim
        , state.query |> String.trim
        , state.matchAppliedPaths
            |> List.map (\path -> String.fromInt (String.length path) ++ ":" ++ path)
            |> String.join ""
        , String.fromInt pageSize
        ]


canLoadMore : String -> ObservationModel -> Bool
canLoadMore workspaceId state =
    not state.loading
        && state.failedRequest == Nothing
        && not (hasUnappliedFilters state)
        && state.hasMore
        && state.expectedOffset
        == Nothing
        && activeFingerprint workspaceId state
        == state.queryFingerprint


canLoadMoreFacets : String -> ObservationModel -> Bool
canLoadMoreFacets workspaceId state =
    state.requestMode == ObservationFacetMode
        && not state.facetLoading
        && state.failedRequest == Nothing
        && not (hasUnappliedFilters state)
        && state.facetHasMore
        && state.facetExpectedOffset == Nothing
        && facetFingerprintFor state.facetRequestSessionEpoch (facetQuery workspaceId 0 state) == state.facetFingerprint


commitAppliedQuery : ObservationModel -> ObservationModel
commitAppliedQuery state =
    let
        query = querySnapshot { state | appliedQuery = Nothing }
        clear observation = { observation | provenanceMatch = Nothing }
        scoped =
            if state.appliedQuery == Just query then state
            else
                { state
                    | selectedDetail = Maybe.map clear state.selectedDetail
                    , items = Dict.map (\_ -> clear) state.items
                    , edit = Maybe.map (\edit -> { edit | latestCanonical = clear edit.latestCanonical }) state.edit
                    , matchEvidence = Dict.map (\_ evidence -> { evidence | observation = clear evidence.observation }) state.matchEvidence
                }
    in
    { scoped | appliedQuery = Just query, failedRequest = Nothing }


draftQuery : ObservationModel -> ObservationAppliedQuery
draftQuery state = querySnapshot { state | appliedQuery = Nothing }


querySnapshot : ObservationModel -> ObservationAppliedQuery
querySnapshot input =
    let
        state =
            appliedState input
    in
    { requestMode = state.requestMode
    , query = state.query
    , subjectQuery = state.subjectQuery
    , subjectKind = state.subjectKind
    , subject = state.subject
    , selectedFacet = state.selectedFacet
    , gitSha = state.gitSha, currentGitSha = state.currentGitSha, historyGitSha = state.historyGitSha
    , matchAppliedPaths = if state.requestMode == ObservationMatchMode then state.matchAppliedPaths else []
    }


ensureAppliedQuery : ObservationModel -> ObservationModel
ensureAppliedQuery state =
    if state.appliedQuery == Nothing then
        commitAppliedQuery state
    else
        state


{-| Called only after the response's workspace/session/generation/offset guards
have accepted it. Keep the failed request independent of edited filter inputs.
-}
failResultPage : String -> Int -> String -> ObservationModel -> ObservationModel
failResultPage workspaceId offset message state =
    if state.refreshPass /= Nothing then
        let invalidated = state.refreshPass |> Maybe.map .invalidated |> Maybe.withDefault False in
        { state | refreshPass = Nothing, refreshPending = invalidated, refreshError = if invalidated then Nothing else Just ("Automatic refresh failed. " ++ message), loading = False, facetLoading = False, expectedOffset = Nothing, facetExpectedOffset = Nothing }
    else
    let
        facets =
            state.requestMode == ObservationFacetMode

        failed =
            { workspaceId = workspaceId
            , sessionEpoch = if facets then state.facetRequestSessionEpoch else state.requestSessionEpoch
            , generation = if facets then state.facetRequestGeneration else state.requestGeneration
            , fingerprint = if facets then state.facetFingerprint else state.queryFingerprint
            , offset = offset
            , query = querySnapshot state
            }

        hasCached =
            if facets then not (List.isEmpty state.facetKeys) else not (List.isEmpty state.orderedIds)
    in
    { state
        | failedRequest = Just failed
        , resultsStale = state.resultsStale || (offset == 0 && hasCached)
        , loading = if facets then state.loading else False
        , error = if facets then state.error else Just message
        , expectedOffset = if facets then state.expectedOffset else Nothing
        , facetLoading = if facets then False else state.facetLoading
        , facetError = if facets then Just message else state.facetError
        , facetExpectedOffset = if facets then Nothing else state.facetExpectedOffset
    }


retryResults : Model -> ( Model, Cmd Msg )
retryResults model =
    if model.observations.refreshError /= Nothing then refreshActiveResultsRaw model
    else
    case ( repositoryWorkspaceId model, model.observations.failedRequest ) of
        ( Just workspaceId, Just failed ) ->
            let
                state =
                    model.observations

                facets =
                    failed.query.requestMode == ObservationFacetMode

                valid =
                    failed.workspaceId == workspaceId
                        && failed.sessionEpoch == model.sessionRequestEpoch
                        && failed.query == querySnapshot state
                        && state.requestMode == failed.query.requestMode
                        && (if facets then
                                not state.facetLoading && state.facetExpectedOffset == Nothing
                                    && failed.generation == state.facetRequestGeneration && failed.fingerprint == state.facetFingerprint
                            else
                                not state.loading && state.expectedOffset == Nothing
                                    && failed.generation == state.requestGeneration && failed.fingerprint == state.queryFingerprint
                           )

                prepared =
                    updateObservation
                        (\current ->
                            if facets then
                                { current | failedRequest = Nothing, facetRequestGeneration = current.facetRequestGeneration + 1 }
                            else
                                { current | failedRequest = Nothing, requestGeneration = current.requestGeneration + 1 }
                        ) model
            in
            if not valid then
                ( model, Cmd.none )
            else
                case failed.query.requestMode of
                    ObservationFacetMode -> fetchFacetPage failed.offset prepared
                    ObservationMatchMode -> fetchMatchPage failed.offset failed.query.matchAppliedPaths prepared
                    _ -> fetchPage failed.offset prepared

        _ ->
            ( model, Cmd.none )


appliedState : ObservationModel -> ObservationModel
appliedState state =
    case state.appliedQuery of
        Just applied ->
            { state
                | requestMode = applied.requestMode
                , query = applied.query
                , subjectQuery = applied.subjectQuery
                , subjectKind = applied.subjectKind
                , subject = applied.subject
                , selectedFacet = applied.selectedFacet
                , gitSha = applied.gitSha, currentGitSha = applied.currentGitSha, historyGitSha = applied.historyGitSha
                , matchAppliedPaths = applied.matchAppliedPaths
            }

        Nothing ->
            state


hasUnappliedFilters : ObservationModel -> Bool
hasUnappliedFilters state =
    case state.appliedQuery of
        Just _ ->
            let
                fingerprint current =
                    case current.requestMode of
                        ObservationFacetMode ->
                            facetFingerprintFor 0 (facetQuery "" 0 current)

                        ObservationMatchMode ->
                            matchFingerprint 0 "" current

                        mode ->
                            resultFingerprint mode 0 "" current
            in
            fingerprint { state | appliedQuery = Nothing } /= fingerprint (appliedState state)

        Nothing ->
            False


revertFilters : ObservationModel -> ObservationModel
revertFilters state =
    let
        applied =
            appliedState state
    in
    { state | query = applied.query, subjectQuery = applied.subjectQuery, subjectKind = applied.subjectKind, subject = applied.subject, gitSha = applied.gitSha, currentGitSha = applied.currentGitSha, historyGitSha = applied.historyGitSha }


activeFingerprint : String -> ObservationModel -> String
activeFingerprint workspaceId state =
    case state.requestMode of
        ObservationFlatMode ->
            resultFingerprint ObservationFlatMode state.requestSessionEpoch workspaceId state

        ObservationExactSubjectMode ->
            resultFingerprint ObservationExactSubjectMode state.requestSessionEpoch workspaceId state

        ObservationFacetMode ->
            ""

        ObservationMatchMode ->
            matchFingerprint state.requestSessionEpoch workspaceId state


modeName : ObservationRequestMode -> String
modeName mode =
    case mode of
        ObservationFlatMode ->
            "flat"

        ObservationFacetMode ->
            "facets"

        ObservationExactSubjectMode ->
            "exact"

        ObservationMatchMode ->
            "match"


facetKey : Api.SubjectKind -> String -> String
facetKey subjectKind subject =
    let
        kind =
            Api.subjectKindToString subjectKind
    in
    String.fromInt (String.length kind) ++ ":" ++ kind ++ String.fromInt (String.length subject) ++ ":" ++ subject


nonEmpty : String -> Maybe String
nonEmpty value =
    let
        trimmed =
            String.trim value
    in
    if String.isEmpty trimmed then
        Nothing

    else
        Just trimmed


normalizeMatchPaths : String -> Result String (List String)
normalizeMatchPaths =
    Helpers.normalizeObservationPaths


utf8Bytes : String -> Int
utf8Bytes value =
    String.foldl
            (\character total ->
                let
                    code =
                        Char.toCode character
                in
                total + (if code <= 0x7F then
                    1

                else if code <= 0x07FF then
                    2

                else if code <= 0xFFFF then
                    3

                else
                    4)
            )
            0 value


{-| Select by id rather than list membership: unified-search hits may not occur
on the active observations page or under its current filters.
-}
selectRouteObservation : String -> Model -> ( Model, Cmd Msg )
selectRouteObservation observationId model =
    let state = model.observations
        tree = state.tree
        (selected, command) = selectObservation observationId {model | observations = {state | treeSelectionPending = False, tree = {tree | revealTarget = Nothing}}}
    in (if isAuditObservation observationId selected then { selected | observations = let current = selected.observations in { current | treeSelectionPending = False } } else armTreeRoute selected, command)


isAuditObservation : String -> Model -> Bool
isAuditObservation observationId model =
    model.focus.returnContext |> Maybe.map (\context -> context.source /= ReturnFromTimeline && context.entityType == "observation" && context.entityId == observationId && model.selectedWorkspaceId == Just context.workspaceId) |> Maybe.withDefault False


selectObservation : String -> Model -> ( Model, Cmd Msg )
selectObservation observationId model =
    if
        model.observations.selectedId
            == Just observationId
            && model.observations.detailError
            == Nothing
            && (model.observations.selectedDetail /= Nothing || model.observations.activeDetailRequest /= Nothing)
    then
        ( model, Cmd.none )

    else
        selectDifferentObservation observationId model


activateObservation : String -> Maybe String -> Model -> ( Model, Cmd Msg )
activateObservation observationId origin model =
    case repositoryWorkspaceId model of
        Nothing ->
            ( model, Cmd.none )

        Just workspaceId ->
            let
                priorState =
                    model.observations

                ( selected, selectionCmd ) =
                    selectObservation observationId model

                updated =
                    updateObservation
                        (\current -> { current | detailNavigationEpoch = model.sessionRequestEpoch, detailNavigationToken = current.detailNavigationToken + 1, detailReturnTarget = origin, pendingReturnNavigation = Nothing, treeSelectionPending = False
                            , viewport = if origin == Nothing then current.viewport else ObservationViewport.captureOrigin current.viewport })
                        selected
                state = updated.observations
                intent =
                    { intent = "detail", selectedId = state.selectedId, workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch
                    , queryGeneration = (viewportLifetime updated).generation, navigationToken = state.detailNavigationToken
                    , previousToken = priorState.detailNavigationToken, previousSelection = priorState.selectedId
                    , originKey = Maybe.withDefault "" origin, readyRevision = Nothing, fallback = False }
            in
            ( { updated | observations = { state | pendingReturnNavigation = Just intent } }, selectionCmd )


returnToResults : Model -> ( Model, Cmd Msg )
returnToResults model =
    case ( repositoryWorkspaceId model, model.observations.selectedId ) of
        ( Just workspaceId, Just _ ) ->
            let
                priorState =
                    model.observations

                previous =
                    { priorState | detailNavigationEpoch = model.sessionRequestEpoch }

                updated =
                    updateObservation
                        (clearSelection >> (\cleared ->
                            { cleared | detailNavigationEpoch = model.sessionRequestEpoch, viewport = ObservationViewport.returnToOrigin previous.detailReturnTarget previous.viewport }))
                        model
                state = updated.observations
                intent = Just
                    { intent = "return", selectedId = Nothing, workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch, queryGeneration = state.viewport.stamp.generation
                    , navigationToken = state.detailNavigationToken, previousToken = previous.detailNavigationToken, previousSelection = previous.selectedId
                    , originKey = Maybe.withDefault "" previous.detailReturnTarget, readyRevision = Nothing, fallback = previous.detailReturnTarget == Nothing }
                returned = { updated | observations = { state | pendingReturnNavigation = intent } }
            in
            ( returned, Cmd.none )

        _ ->
            ( model, Cmd.none )


navigationStamp : String -> ObservationModel -> Encode.Value
navigationStamp workspaceId state =
    Encode.object
        [ ( "workspaceId", Encode.string workspaceId )
        , ( "sessionEpoch", Encode.int state.detailNavigationEpoch )
        , ( "token", Encode.int state.detailNavigationToken )
        , ( "selectedId", state.selectedId |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
        ]


navigateDetail : String -> String -> Maybe String -> ObservationModel -> ObservationModel -> Cmd Msg
navigateDetail intent workspaceId origin previous destination =
    Ports.navigateObservationDetail
        (Encode.object
            [ ( "intent", Encode.string intent )
            , ( "originId", origin |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
            , ( "previous", navigationStamp workspaceId previous )
            , ( "destination", navigationStamp workspaceId destination )
            , ( "viewport", ObservationViewport.stampValue destination.viewport.stamp )
            ]
        )


selectDifferentObservation : String -> Model -> ( Model, Cmd Msg )
selectDifferentObservation observationId model =
    case model.selectedWorkspaceId of
        Just workspaceId ->
            let
                state =
                    model.observations

                token =
                    state.nextDetailRequestToken

                request =
                    { workspaceId = workspaceId
                    , observationId = observationId
                    , sessionEpoch = model.sessionRequestEpoch
                    , token = token
                    }

                updated =
                    updateObservation
                        (\current ->
                            { current
                                | selectedId = Just observationId
                                , searchOriginDetail = Helpers.isObservationSearchUrl model.url && (Helpers.observationUrlContext model.url).fragment.observationId == Just observationId
                                , tree = let tree = current.tree in {tree | revealFocus = True}
                                , treeSelectionPending = current.treeSelectionPending && current.selectedId == Just observationId
                                , history = Nothing
                                , historyExpanded = False
                                , selectedDetail =
                                    current.edit
                                        |> Maybe.andThen
                                            (\edit ->
                                                if edit.observationId == observationId then
                                                    Just edit.latestCanonical

                                                else
                                                    Nothing
                                            )
                                , detailLoading = True
                                , detailError = Nothing
                                , activeDetailRequest = Just request
                                , nextDetailRequestToken = token + 1
                                , edit =
                                    if current.selectedId == Just observationId then
                                        current.edit

                                    else
                                        retainedEdit current.edit
                                , deleteConfirmation = Nothing
                            }
                        )
                        model
            in
            ( updated
            , Cmd.batch
                [ Api.fetchObservation model.flags.apiUrl observationId (GotObservationDetail workspaceId observationId model.sessionRequestEpoch token)
                ]
            )

        Nothing ->
            ( model, Cmd.none )


detailResponseMatches : String -> String -> Int -> Int -> Maybe String -> Int -> ObservationModel -> Bool
detailResponseMatches workspaceId observationId sessionEpoch token selectedWorkspaceId currentSessionEpoch state =
    selectedWorkspaceId
        == Just workspaceId
        && currentSessionEpoch
        == sessionEpoch
        && state.selectedId
        == Just observationId
        && state.activeDetailRequest
        == Just { workspaceId = workspaceId, observationId = observationId, sessionEpoch = sessionEpoch, token = token }


preferNewerObservation : Api.Observation -> Api.Observation -> Api.Observation
preferNewerObservation candidate existing =
    let
        refreshed =
            if candidate.latestSequence == existing.latestSequence && candidate.contentVersion == existing.contentVersion && candidate.content == existing.content && candidate.provenanceMatch == Nothing then
                { candidate | provenanceMatch = existing.provenanceMatch }
            else candidate
    in
    if not (sameObservationProvenance candidate existing) then
        existing
    else if candidate.latestSequence > existing.latestSequence then
        candidate
    else if candidate.latestSequence < existing.latestSequence then
        existing
    else
        case compareTimestamps candidate.updatedAt existing.updatedAt of
            Just GT -> refreshed
            Just EQ -> if candidate.content == existing.content then refreshed else existing
            _ -> existing


type alias ParsedTimestamp =
    { second : Int
    , fraction : String
    }


compareTimestamps : String -> String -> Maybe Order
compareTimestamps left right =
    case ( parseTimestamp left, parseTimestamp right ) of
        ( Just parsedLeft, Just parsedRight ) ->
            case compare parsedLeft.second parsedRight.second of
                EQ ->
                    let
                        width =
                            Basics.max (String.length parsedLeft.fraction) (String.length parsedRight.fraction)
                    in
                    Just (compare (String.padRight width '0' parsedLeft.fraction) (String.padRight width '0' parsedRight.fraction))

                order ->
                    Just order

        _ ->
            if left == right then
                Just EQ

            else
                Nothing


timestampsEquivalent : String -> String -> Bool
timestampsEquivalent left right =
    compareTimestamps left right == Just EQ


parseTimestamp : String -> Maybe ParsedTimestamp
parseTimestamp value =
    if String.length value < 20 || String.slice 4 5 value /= "-" || String.slice 7 8 value /= "-" || String.slice 10 11 value /= "T" || String.slice 13 14 value /= ":" || String.slice 16 17 value /= ":" then
        Nothing

    else
        parseTimestampComponents value
            |> Maybe.andThen
                (\components ->
                    parseTimestampSuffix (String.dropLeft 19 value)
                        |> Maybe.andThen
                            (\suffix ->
                                if validDateTime components.year components.month components.day components.hour components.minute components.second then
                    Just
                        { second =
                                            (((daysBeforeYear components.year + daysBeforeMonth components.year components.month + components.day - 1) * 24 + components.hour) * 60 + components.minute) * 60
                                                + components.second
                                - suffix.offsetSeconds
                        , fraction = suffix.fraction
                        }

                                else
                                    Nothing
                            )
                )


parseTimestampComponents : String -> Maybe { year : Int, month : Int, day : Int, hour : Int, minute : Int, second : Int }
parseTimestampComponents value =
    String.toInt (String.slice 0 4 value)
        |> Maybe.andThen
            (\year ->
                String.toInt (String.slice 5 7 value)
                    |> Maybe.andThen
                        (\month ->
                            String.toInt (String.slice 8 10 value)
                                |> Maybe.andThen
                                    (\day ->
                                        String.toInt (String.slice 11 13 value)
                                            |> Maybe.andThen
                                                (\hour ->
                                                    String.toInt (String.slice 14 16 value)
                                                        |> Maybe.andThen
                                                            (\minute ->
                                                                String.toInt (String.slice 17 19 value)
                                                                    |> Maybe.map
                                                                        (\second ->
                                                                            { year = year
                                                                            , month = month
                                                                            , day = day
                                                                            , hour = hour
                                                                            , minute = minute
                                                                            , second = second
                                                                            }
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


parseTimestampSuffix : String -> Maybe { fraction : String, offsetSeconds : Int }
parseTimestampSuffix suffix =
    let
        ( fraction, zone ) =
            if String.startsWith "." suffix then
                let
                    afterDot =
                        String.dropLeft 1 suffix

                    digits =
                        takeLeadingDigits afterDot
                in
                ( digits, String.dropLeft (String.length digits) afterDot )

            else
                ( "", suffix )
    in
    if String.startsWith "." suffix && String.isEmpty fraction then
        Nothing

    else
        parseZoneOffset zone
            |> Maybe.map (\offsetSeconds -> { fraction = fraction, offsetSeconds = offsetSeconds })


takeLeadingDigits : String -> String
takeLeadingDigits value =
    value
        |> String.toList
        |> List.foldl
            (\character ( reversed, accepting ) ->
                if accepting && Char.isDigit character then
                    ( character :: reversed, True )

                else
                    ( reversed, False )
            )
            ( [], True )
        |> Tuple.first
        |> List.reverse
        |> String.fromList


parseZoneOffset : String -> Maybe Int
parseZoneOffset zone =
    if zone == "Z" then
        Just 0

    else if String.length zone == 6 && String.slice 3 4 zone == ":" && (String.startsWith "+" zone || String.startsWith "-" zone) then
        case ( String.toInt (String.slice 1 3 zone), String.toInt (String.slice 4 6 zone) ) of
            ( Just hour, Just minute ) ->
                if hour <= 23 && minute <= 59 then
                    let
                        magnitude =
                            (hour * 60 + minute) * 60
                    in
                    if String.startsWith "-" zone then
                        Just -magnitude

                    else
                        Just magnitude

                else
                    Nothing

            _ ->
                Nothing

    else
        Nothing


validDateTime : Int -> Int -> Int -> Int -> Int -> Int -> Bool
validDateTime year month day hour minute second =
    year >= 1
        && month >= 1
        && month <= 12
        && day >= 1
        && day <= daysInMonth year month
        && hour >= 0
        && hour <= 23
        && minute >= 0
        && minute <= 59
        && second >= 0
        && second <= 59


daysBeforeYear : Int -> Int
daysBeforeYear year =
    let
        completedYears =
            year - 1
    in
    365 * completedYears + completedYears // 4 - completedYears // 100 + completedYears // 400


daysBeforeMonth : Int -> Int -> Int
daysBeforeMonth year month =
    List.range 1 (month - 1)
        |> List.map (daysInMonth year)
        |> List.sum


daysInMonth : Int -> Int -> Int
daysInMonth year month =
    case month of
        2 ->
            if modBy 400 year == 0 || (modBy 4 year == 0 && modBy 100 year /= 0) then
                29

            else
                28

        4 ->
            30

        6 ->
            30

        9 ->
            30

        11 ->
            30

        _ ->
            31


sameObservationProvenance : Api.Observation -> Api.Observation -> Bool
sameObservationProvenance left right =
    left.id
        == right.id
        && left.workspaceId
        == right.workspaceId
        && left.subjects
        == right.subjects
        && left.subjectKind
        == right.subjectKind
        && left.subject
        == right.subject
        && left.gitSha
        == right.gitSha
        && timestampsEquivalent left.createdAt right.createdAt


withRetainedCanonical : String -> ObservationModel -> Maybe Api.Observation -> Maybe Api.Observation
withRetainedCanonical observationId state existing =
    case state.edit of
        Just edit ->
            if edit.observationId == observationId then
                existing
                    |> Maybe.map (preferNewerObservation edit.latestCanonical)
                    |> Maybe.withDefault edit.latestCanonical
                    |> Just

            else
                existing

        Nothing ->
            existing


{-| A detached search read proves this detail only, not membership changes in
saved tree/list results. Actual mutation and invalidation paths retain their
normal refresh behavior.
-}
acceptFetchedDetail : Bool -> Api.Observation -> ObservationModel -> ObservationModel
acceptFetchedDetail detached observation state =
    let accepted = applyCanonicalObservation observation state in
    if detached || state.searchOriginDetail then
        { accepted | items = state.items, matchEvidence = state.matchEvidence
            , resultsStale = state.resultsStale, workspaceCounts = state.workspaceCounts, counts = state.counts
            , refreshPending = state.refreshPending, refreshPass = state.refreshPass }
    else
        accepted


applyCanonicalObservation : Api.Observation -> ObservationModel -> ObservationModel
applyCanonicalObservation candidate state =
    applyCanonicalObservationWithProof False candidate state


{-| Only a matching, provenance-checked mutation or owned conflict revalidation
response can bypass cache freshness. Its request causality has already been fenced.
-}
applyCanonicalObservationWithProof : Bool -> Api.Observation -> ObservationModel -> ObservationModel
applyCanonicalObservationWithProof conditionalProof candidate state =
    let
        existing =
            case ( Dict.get candidate.id state.items, state.selectedDetail ) of
                ( Just listed, Just detail ) ->
                    if detail.id == candidate.id then
                        Just (preferNewerObservation detail listed)

                    else
                        Just listed

                ( Just listed, Nothing ) ->
                    Just listed

                ( Nothing, Just detail ) ->
                    if detail.id == candidate.id then
                        Just detail

                    else
                        Nothing

                _ ->
                    Nothing

        accepted =
            if conditionalProof then
                candidate
            else
                existing
                    |> withRetainedCanonical candidate.id state
                    |> Maybe.map (preferNewerObservation candidate)
                    |> Maybe.withDefault candidate

        acceptedCandidate =
            { accepted | provenanceMatch = candidate.provenanceMatch } == candidate

        selectedDetail =
            if state.selectedId == Just candidate.id then
                Just accepted

            else
                state.selectedDetail

        edit =
            if acceptedCandidate then
                reconcileEditWithCanonical accepted state.edit

            else
                state.edit

        matchEvidence =
            Dict.update accepted.id
                (Maybe.map (\evidence -> { evidence | observation = accepted }))
                state.matchEvidence

        resultsStale = state.resultsStale || (existing /= Just accepted)

    in
    { state
        | items =
            if Dict.member accepted.id state.items then
                Dict.insert accepted.id accepted state.items

            else
                state.items
        , selectedDetail = selectedDetail
        , history = currentHistory selectedDetail state.history
        , edit = edit
        , matchEvidence = matchEvidence
        , resultsStale = resultsStale
        , workspaceCounts = (if existing /= Just accepted then invalidateCounts state else state).workspaceCounts
        , counts = (if existing /= Just accepted then invalidateCounts state else state).counts
        , refreshPending = state.refreshPending || (existing /= Just accepted)
        , refreshPass = if existing /= Just accepted then Maybe.map (\pass -> { pass | invalidated = True }) state.refreshPass else state.refreshPass
    }


{-| A list, exact-subject, or match response is authoritative for its own
membership page.  It must still respect a newer local/detail version and edit
drafts, but unlike an entity invalidation it is evidence that the active
results are fresh.  Keeping this separate from `applyCanonicalObservation`
prevents a successful explicit refresh from immediately becoming stale again.
-}
applyAuthoritativeObservation : Api.Observation -> ObservationModel -> ObservationModel
applyAuthoritativeObservation candidate state =
    let
        existing =
            case ( Dict.get candidate.id state.items, state.selectedDetail ) of
                ( Just listed, Just detail ) ->
                    if detail.id == candidate.id then
                        Just (preferNewerObservation detail listed)

                    else
                        Just listed

                ( Just listed, Nothing ) ->
                    Just listed

                ( Nothing, Just detail ) ->
                    if detail.id == candidate.id then
                        Just detail

                    else
                        Nothing

                _ ->
                    Nothing

        freshest =
            existing
                |> withRetainedCanonical candidate.id state
                |> Maybe.map (preferNewerObservation candidate)
                |> Maybe.withDefault candidate

        -- Membership annotations come exclusively from this authoritative page.
        accepted =
            { freshest | provenanceMatch = if freshest.latestSequence == candidate.latestSequence && freshest.contentVersion == candidate.contentVersion && freshest.content == candidate.content && sameObservationProvenance freshest candidate then candidate.provenanceMatch else Nothing }

        acceptedCandidate =
            accepted == candidate

        selectedDetail =
            if state.selectedId == Just candidate.id then
                Just accepted

            else
                state.selectedDetail

        edit =
            if acceptedCandidate then
                reconcileEditWithCanonical accepted state.edit

            else
                state.edit

        matchEvidence =
            Dict.update accepted.id
                (Maybe.map (\evidence -> { evidence | observation = accepted }))
                state.matchEvidence
    in
    { state
        | items =
            if Dict.member accepted.id state.items then
                Dict.insert accepted.id accepted state.items

            else
                state.items
        , selectedDetail = selectedDetail
        , history = currentHistory selectedDetail state.history
        , edit = edit
        , matchEvidence = matchEvidence
    }


isLoadedOrSelected : String -> ObservationModel -> Bool
isLoadedOrSelected observationId state =
    Dict.member observationId state.items
        || state.selectedId == Just observationId
        || Maybe.map .id state.selectedDetail == Just observationId
        || Maybe.map .observationId state.edit == Just observationId


markResultsStale : ObservationModel -> ObservationModel
markResultsStale state =
    let invalidated = invalidateCounts state in
    { invalidated | resultsStale = True, refreshPending = True, refreshPass = Maybe.map (\pass -> { pass | invalidated = True }) state.refreshPass }


reconcileEditWithCanonical : Api.Observation -> Maybe ObservationEditState -> Maybe ObservationEditState
reconcileEditWithCanonical observation maybeEdit =
    maybeEdit
        |> Maybe.map
            (\edit ->
                if edit.workspaceId /= observation.workspaceId || edit.observationId /= observation.id then
                    edit

                else if observation.contentVersion == edit.latestCanonical.contentVersion && timestampsEquivalent observation.updatedAt edit.latestCanonical.updatedAt && observation.content == edit.latestCanonical.content then
                    edit

                else if edit.draft == edit.baseContent && edit.reviewedGitShaDraft == edit.baseReviewedGitSha && not edit.saving then
                    { edit
                        | baseContent = observation.content
                        , baseContentVersion = observation.contentVersion
                        , baseUpdatedAt = observation.updatedAt
                        , draft = observation.content
                        , baseReviewedGitSha = reviewedSha observation
                        , reviewedGitShaDraft = reviewedSha observation
                        , latestCanonical = observation
                        , conflict = False
                        , error = Nothing
                    }

                else
                    { edit
                        | latestCanonical = observation
                        , conflict = observation.contentVersion /= edit.baseContentVersion || not (timestampsEquivalent observation.updatedAt edit.baseUpdatedAt) || observation.content /= edit.baseContent
                        , error = Nothing
                    }
            )


removeObservation : String -> ObservationModel -> ObservationModel
removeObservation observationId state =
    let
        selected =
            state.selectedId == Just observationId

        removesEdit edit =
            edit.observationId == observationId

        removesConfirmation confirmation =
            confirmation.observationId == observationId
    in
    { state
        | workspaceCounts = (if isLoadedOrSelected observationId state then invalidateCounts state else state).workspaceCounts
        , counts = (if isLoadedOrSelected observationId state then invalidateCounts state else state).counts
        , items = Dict.remove observationId state.items
        , expandedSubjects = Dict.remove observationId state.expandedSubjects
        , preferenceValue = let prefs = state.preferenceValue in { prefs | detail = if Maybe.map .id prefs.detail == Just observationId then Nothing else prefs.detail, subjects = List.filter ((/=) observationId) prefs.subjects }
        , orderedIds = List.filter ((/=) observationId) state.orderedIds
        , matchEvidence = Dict.remove observationId state.matchEvidence
        , tree = let tree = state.tree in {tree | terminals = Dict.map (\_ terminal -> {terminal | items = List.filter (.id >> (/=) observationId) terminal.items, stagedItems = Maybe.map (List.filter (.id >> (/=) observationId)) terminal.stagedItems}) tree.terminals}
        , selectedId =
            if selected then
                Nothing

            else
                state.selectedId
        , selectedDetail =
            if selected then
                Nothing

            else
                state.selectedDetail
        , detailLoading =
            if selected then
                False

            else
                state.detailLoading
        , detailError =
            if selected then
                Nothing

            else
                state.detailError
        , activeDetailRequest =
            if selected then
                Nothing

            else
                state.activeDetailRequest
        , resultsStale = state.resultsStale || state.requestMode == ObservationFacetMode
        , refreshPending = state.refreshPending || isLoadedOrSelected observationId state
        , refreshPass = if isLoadedOrSelected observationId state then Maybe.map (\pass -> { pass | invalidated = True }) state.refreshPass else state.refreshPass
        , edit =
            state.edit
                |> Maybe.andThen
                    (\edit ->
                        if removesEdit edit then
                            Nothing

                        else
                            Just edit
                    )
        , deleteConfirmation =
            state.deleteConfirmation
                |> Maybe.andThen
                    (\confirmation ->
                        if removesConfirmation confirmation then
                            Nothing

                        else
                            Just confirmation
                    )
    }


reconcileDeletedObservation : String -> Model -> ( Model, Cmd Msg )
reconcileDeletedObservation observationId model =
    let
        wasSelected =
            model.observations.selectedId == Just observationId

        updated =
            updateObservation (removeObservation observationId >> invalidateCounts) model
    in
    ( updated
    , if wasSelected then
        replaceFragment updated

      else
        Cmd.none
    )


reconcileCurationPermission : Model -> Model
reconcileCurationPermission model =
    if Permissions.canEditCurrentWorkspace model then
        model

    else
        updateObservation
            (\state -> { state | edit = Nothing, deleteConfirmation = Nothing })
            model


updateObservation : (ObservationModel -> ObservationModel) -> Model -> Model
updateObservation fn model =
    { model | observations = fn model.observations }


observationResponseMatches : Int -> Int -> String -> Int -> ObservationModel -> Bool
observationResponseMatches sessionEpoch generation fingerprint offset state =
    state.requestGeneration
        == generation
        && state.requestSessionEpoch
        == sessionEpoch
        && state.queryFingerprint
        == fingerprint
        && state.expectedOffset
        == Just offset


matchResponseMatches : Int -> Int -> String -> Int -> ObservationModel -> Bool
matchResponseMatches sessionEpoch generation fingerprint offset state =
    state.requestMode
        == ObservationMatchMode
        && observationResponseMatches sessionEpoch generation fingerprint offset state


facetResponseMatches : Int -> Int -> String -> Int -> ObservationModel -> Bool
facetResponseMatches sessionEpoch generation fingerprint offset state =
    state.requestMode
        == ObservationFacetMode
        && state.facetRequestGeneration
        == generation
        && state.facetRequestSessionEpoch
        == sessionEpoch
        && state.facetFingerprint
        == fingerprint
        && state.facetExpectedOffset
        == Just offset


mergeFacetPage : Int -> Api.PaginatedResult Api.ObservationSubjectFacet -> ObservationModel -> ObservationModel
mergeFacetPage offset paginated state =
    let
        baseFacets =
            if offset == 0 then
                Dict.empty

            else
                state.facets

        baseKeys =
            if offset == 0 then
                []

            else
                state.facetKeys

        addFacet facet ( facets, keys ) =
            let
                key =
                    facetKey facet.subjectKind facet.subject
            in
            if Dict.member key facets then
                ( Dict.insert key facet facets, keys )

            else
                ( Dict.insert key facet facets, keys ++ [ key ] )

        ( mergedFacets, mergedKeys ) =
            List.foldl addFacet ( baseFacets, baseKeys ) paginated.items
    in
    { state
        | facets = mergedFacets
        , completedQuery = state.appliedQuery
        , facetKeys = mergedKeys
        , facetHasMore = paginated.hasMore
        , facetLoading = False
        , facetError = Nothing
        , facetExpectedOffset = Nothing
        , facetNextOffset = offset + List.length paginated.items
        , resultsStale = if offset == 0 then False else state.resultsStale
        , failedRequest = Nothing
    }


mergeMatchPage : Int -> Api.PaginatedResult Api.ObservationMatch -> ObservationModel -> ObservationModel
mergeMatchPage offset paginated state =
    let
        baseEvidence =
            if offset == 0 then
                Dict.empty

            else
                state.matchEvidence

        matchesById =
            List.foldl (\match evidence -> Dict.insert match.observation.id match evidence) baseEvidence paginated.items

        observations =
            List.map .observation paginated.items

        receivedIds =
            List.map .id observations

        orderedIds =
            if offset == 0 then
                receivedIds

            else
                state.orderedIds ++ List.filter (\observationId -> not (List.member observationId state.orderedIds)) receivedIds

        pageItems =
            List.foldl
                (\observation accumulatedItems ->
                    Dict.insert observation.id
                        (Dict.get observation.id state.items
                            |> Maybe.map (preferNewerObservation observation)
                            |> Maybe.withDefault observation
                        )
                        accumulatedItems
                )
                Dict.empty
                observations

        items =
            if offset == 0 then
                pageItems

            else
                Dict.union pageItems state.items
    in
    { state
        | items = items
        , completedQuery = state.appliedQuery
        , orderedIds = orderedIds
        , hasMore = paginated.hasMore
        , loading = False
        , error = Nothing
        , expectedOffset = Nothing
        , nextOffset = offset + List.length paginated.items
        , matchEvidence = matchesById
        , failedRequest = Nothing
        , resultsStale = if offset == 0 then False else state.resultsStale
    }
        |> (\merged -> List.foldl applyAuthoritativeObservation merged observations)


type alias ObservationSubjectGroup =
    { key : String
    , subjectKind : Api.SubjectKind
    , subject : String
    , observationIds : List String
    }


type alias ObservationPathGroup =
    { path : String
    , subjectGroups : List ObservationSubjectGroup
    }


groupPathMatches : List String -> List String -> Dict.Dict String Api.ObservationMatch -> List ObservationPathGroup
groupPathMatches paths orderedIds evidence =
    let
        groupsForPath path =
            let
                addObservation observationId groups =
                    case Dict.get observationId evidence of
                        Nothing ->
                            groups

                        Just match ->
                            match.pathMatches
                                |> List.filter (\pathMatch -> pathMatch.path == path)
                                |> List.concatMap .matchedSubjects
                                |> List.foldl (appendSubjectObservation path observationId) groups
            in
            { path = path
            , subjectGroups = List.foldl addObservation [] orderedIds
            }
    in
    List.map groupsForPath paths


appendSubjectObservation : String -> String -> Api.ObservationSubject -> List ObservationSubjectGroup -> List ObservationSubjectGroup
appendSubjectObservation path observationId matchedSubject groups =
    let
        key =
            matchGroupKey path matchedSubject.subjectKind matchedSubject.subject

        append remaining =
            case remaining of
                [] ->
                    [ { key = key
                      , subjectKind = matchedSubject.subjectKind
                      , subject = matchedSubject.subject
                      , observationIds = [ observationId ]
                      }
                    ]

                group :: rest ->
                    if group.key == key then
                        { group
                            | observationIds =
                                if List.member observationId group.observationIds then
                                    group.observationIds

                                else
                                    group.observationIds ++ [ observationId ]
                        }
                            :: rest

                    else
                        group :: append rest
    in
    append groups


matchGroupKey : String -> Api.SubjectKind -> String -> String
matchGroupKey path subjectKind subject =
    String.fromInt (String.length path) ++ ":" ++ path ++ facetKey subjectKind subject


observationCardDomId : String -> String -> String
observationCardDomId context observationId =
    "observation-card-" ++ domToken context ++ "-" ++ domToken observationId


domToken : String -> String
domToken value =
    String.fromInt (String.length value)
        ++ "-"
        ++ (value
                |> String.toList
                |> List.map (Char.toCode >> String.fromInt)
                |> String.join "-"
           )


resultRowKey : ObservationResultRow -> String
resultRowKey row =
    case row of
        ObservationCardRow context observation -> observationCardDomId context observation.id
        ObservationFacetRow facet -> "observation-facet-" ++ domToken (facetKey facet.subjectKind facet.subject)
        ObservationPathRow path _ -> "observation-path-" ++ domToken path
        ObservationSubjectRow _ key _ _ _ _ -> "observation-group-" ++ domToken key
        ObservationTreeRow _ content -> resultRowKey content
        ObservationTreePathRow node -> treePathKey node.path
        ObservationTreeTerminalRow subject -> "tree-terminal:" ++ ObservationTree.terminalKey subject
        ObservationTreePageRow path kind -> "tree-page:" ++ (kind |> Maybe.map Api.subjectKindToString |> Maybe.withDefault "branch") ++ ":" ++ path


type TreePresentation
    = TreePresentation ObservationResultRow (List TreePresentation)


rowContent : ObservationResultRow -> ObservationResultRow
rowContent row =
    case row of
        ObservationTreeRow _ content -> content
        _ -> row


{-| Topology is built with the logical preorder, before the viewport chooses
rows or pins. Rails belong to a parent and end at its own last loaded child or
continuation row, so a virtual gap cannot join unrelated root trees.
-}
treeResultRows : ObservationModel -> List ObservationResultRow
treeResultRows state =
    let
        tree = state.tree
        pageRow prefix kind pending = if pending then [TreePresentation (ObservationTreePageRow prefix kind) []] else []
        branchRows prefix =
            let branch = Dict.get prefix tree.branches
                paths = branch |> Maybe.map .paths |> Maybe.withDefault []
                pending = branch |> Maybe.map (\value -> value.request /= Nothing || value.error /= Nothing || value.hasMore) |> Maybe.withDefault True
            in List.filterMap nodeRows paths ++ pageRow prefix Nothing pending
        terminalRows terminal =
            let subject = {subjectKind = terminal.subjectKind, subject = terminal.subject}
                loaded = Dict.get (ObservationTree.terminalKey subject) tree.terminals
                items = loaded |> Maybe.map .items |> Maybe.withDefault []
                pending = loaded |> Maybe.map (\value -> value.request /= Nothing || value.error /= Nothing || value.hasMore) |> Maybe.withDefault True
            in TreePresentation (ObservationTreeTerminalRow subject)
                (List.map (\item -> TreePresentation (ObservationCardRow (treeCardContext subject) (Dict.get item.id state.items |> Maybe.withDefault item)) []) items ++ pageRow subject.subject (Just subject.subjectKind) pending)
        nodeRows path = Dict.get path tree.nodes |> Maybe.map (\node ->
            TreePresentation (ObservationTreePathRow node) (if Set.member path tree.expanded then
                List.map terminalRows node.terminals ++ (if node.hasChildren then branchRows path else []) else []))
        key (TreePresentation row _) = resultRowKey row
        unknown (TreePresentation row _) = case row of
            ObservationTreePageRow _ _ -> True
            _ -> False
        forest depth root parent rails entries =
            let walk remaining = case remaining of
                    [] -> []
                    entry :: rest ->
                        let following = case rest of
                                next :: _ -> if unknown next then TreeUnknown else TreeNext
                                [] -> if unknown entry then TreeUnknown else TreeEnd
                        in flatten depth root parent rails following entry ++ walk rest
            in walk entries
        flatten depth root parent rails following (TreePresentation row children) =
            let
                owner = resultRowKey row
                rootKey = if depth == 0 then owner else root
                childRail = children |> List.reverse |> List.head |> Maybe.map (\tail -> {owner = owner, tail = key tail, depth = depth + 1, unknown = unknown tail})
                inherited = rails ++ (if depth > 0 && following /= TreeEnd then parent |> Maybe.map List.singleton |> Maybe.withDefault [] else [])
                topology = {depth = depth, levels = 1, root = rootKey, following = following, parent = parent, ancestors = rails, children = childRail}
            in ObservationTreeRow topology row :: forest (depth + 1) rootKey childRail inherited children
        projected = forest 0 "" Nothing [] (branchRows "")
        levels = projected |> List.filterMap (\row -> case row of
            ObservationTreeRow topology _ -> Just topology.depth
            _ -> Nothing) |> List.maximum |> Maybe.withDefault 1 |> Basics.max 1
    in List.map (\row -> case row of
        ObservationTreeRow topology content -> ObservationTreeRow {topology | levels = levels} content
        _ -> row) projected


projectResultRows : ObservationModel -> Array.Array ObservationResultRow
projectResultRows state =
    let
        cards context ids = ids |> List.filterMap (\key -> Dict.get key state.items |> Maybe.map (ObservationCardRow context))
        subject path group =
            let expanded = Dict.get group.key state.expandedMatchGroups |> Maybe.withDefault False in
            ObservationSubjectRow path group.key group.subjectKind group.subject (List.length group.observationIds) expanded
                :: (if expanded then cards group.key group.observationIds else [])
        pathRows group = ObservationPathRow group.path (List.isEmpty group.subjectGroups)
            :: List.concatMap (subject group.path) group.subjectGroups
    in
    Array.fromList (case state.requestMode of
        ObservationFacetMode -> treeResultRows state
        ObservationMatchMode -> groupPathMatches state.matchAppliedPaths state.orderedIds state.matchEvidence |> List.concatMap pathRows
        ObservationExactSubjectMode -> treeResultRows state
        ObservationFlatMode -> cards "flat" state.orderedIds)


viewportLifetime : Model -> ObservationViewport.Stamp
viewportLifetime model =
    let state = model.observations in
    { workspace = if model.activeTab == ObservationsTab then repositoryWorkspaceId model |> Maybe.withDefault "" else ""
    , epoch = model.sessionRequestEpoch
    , generation = if treeMode state.requestMode then state.tree.lifetime else resultViewFingerprint model.sessionRequestEpoch (Maybe.withDefault "" model.selectedWorkspaceId) state
    , revision = state.viewport.stamp.revision }


treeReadsPending : ObservationModel -> Bool
treeReadsPending state =
    treeMode state.requestMode &&
        (not (Dict.member "" state.tree.branches)
            || (Dict.values state.tree.branches |> List.any (\branch -> branch.request /= Nothing))
            || (Dict.values state.tree.terminals |> List.any (\terminal -> terminal.request /= Nothing)))


{-| Build membership projection only after a structural/canonical change, never
for a draft keystroke. Transport caches and their counts remain untouched.
-}
refreshViewport : Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
refreshViewport previous ( incoming, command ) =
    let
        (discovered, discoveryCmd) = pumpDiscovery incoming
        (revealed, revealCmd) = finishTreeReveal discovered
        ( model, preferenceCmd ) = syncPreferences previous revealed
        old = previous.observations
        state = model.observations
        lifetime = viewportLifetime model
        sameContext = lifetime.workspace /= "" && lifetime.workspace == old.viewport.stamp.workspace && lifetime.epoch == old.viewport.stamp.epoch
        searchEdit = old.query /= state.query || old.subjectQuery /= state.subjectQuery
            || old.filterComposing || state.filterComposing || old.filterDebounce /= Nothing || state.filterDebounce /= Nothing
        preserveScroll = sameContext && state.viewport.preserveScroll
            && (old.requestMode /= state.requestMode || lifetime.generation == old.viewport.stamp.generation || searchEdit)
            && old.selectedId == state.selectedId && old.detailNavigationToken == state.detailNavigationToken
            && (old.requestMode /= state.requestMode || (old.expandedMatchGroups == state.expandedMatchGroups && old.expandedSubjects == state.expandedSubjects))
        currentViewport = state.viewport
        preparedViewport = { currentViewport | preserveScroll = preserveScroll, preserveExtent = if preserveScroll then currentViewport.preserveExtent else 0 }
        restoreScroll = state.preferenceScroll /= Nothing && state.selectedId == Nothing && model.activeTab == ObservationsTab
            && not state.loading && not state.facetLoading && not (treeReadsPending state) && state.refreshPass == Nothing
        changed = restoreScroll || old.tree /= state.tree || old.items /= state.items || old.orderedIds /= state.orderedIds || old.requestMode /= state.requestMode
            || old.selectedFacet /= state.selectedFacet || old.facets /= state.facets || old.facetKeys /= state.facetKeys
            || old.matchAppliedPaths /= state.matchAppliedPaths || old.matchEvidence /= state.matchEvidence
            || old.expandedMatchGroups /= state.expandedMatchGroups
            || lifetime.workspace /= state.viewport.stamp.workspace || lifetime.epoch /= state.viewport.stamp.epoch
            || lifetime.generation /= state.viewport.stamp.generation
        rows = if changed then (if lifetime.workspace == "" then Array.empty else projectResultRows state) else state.resultRows
        projectedViewport = if changed then
            let rebuilt = ObservationViewport.rebuild lifetime (Array.toList rows |> List.map resultRowKey) preparedViewport in
            if old.items /= state.items || old.expandedMatchGroups /= state.expandedMatchGroups then { rebuilt | origin = Nothing } else rebuilt
            else preparedViewport
        oldStamp = projectedViewport.stamp
        navigationChanged = projectedViewport.navigationToken /= state.detailNavigationToken
        navigationViewport = if navigationChanged then { projectedViewport | navigationToken = state.detailNavigationToken, stamp = { oldStamp | revision = oldStamp.revision + 1 } } else projectedViewport
        scrollViewport = if restoreScroll then { navigationViewport | top = Maybe.withDefault navigationViewport.top state.preferenceScroll, preserveScroll = False } else navigationViewport
        treeTarget = state.tree.focusPending |> Maybe.map treePathKey
        viewport = if treeTarget /= Nothing then {scrollViewport | focus = treeTarget} else scrollViewport
        invalidatedReturn = changed && (old.items /= state.items || old.expandedMatchGroups /= state.expandedMatchGroups || old.orderedIds /= state.orderedIds)
        ownedPending = state.pendingReturnNavigation |> Maybe.andThen (\intent ->
            if intent.workspaceId == lifetime.workspace && intent.sessionEpoch == lifetime.epoch && intent.queryGeneration == lifetime.generation && intent.navigationToken == state.detailNavigationToken then Just intent else Nothing)
        pending = if invalidatedReturn then ownedPending |> Maybe.map (\intent -> { intent | fallback = intent.intent == "return" && not (Dict.member intent.originKey viewport.index.positions), readyRevision = Nothing }) else ownedPending
        owner = if changed || old.selectedId /= state.selectedId || old.detailReturnTarget /= state.detailReturnTarget then resolveSelectedOwner rows viewport state else state.inlineOwner
        updated = { state | resultRows = rows, viewport = viewport, inlineOwner = owner, pendingReturnNavigation = pending, preferenceScroll = if restoreScroll then Nothing else state.preferenceScroll
            , tree = let tree = state.tree in {tree | focusPending = Nothing} }
        synchronize = changed || navigationChanged || old.selectedId /= state.selectedId || old.detailReturnTarget /= state.detailReturnTarget || old.viewport.focus /= viewport.focus || old.viewport.returnPin /= viewport.returnPin || old.viewport.stamp /= viewport.stamp || old.viewport.preserveScroll /= viewport.preserveScroll || (preserveScroll && (old.loading /= state.loading || old.facetLoading /= state.facetLoading))
    in
    let
        ( automatic, automaticCmd ) = continueAutomaticRefresh { model | observations = updated }
        ( pumped, treeCmd ) = pumpTree automatic
        ( counted, countsCmd ) = syncCounts pumped
    in
    ( counted, Cmd.batch [ command, discoveryCmd, revealCmd, preferenceCmd, automaticCmd, treeCmd, countsCmd, if synchronize || treeTarget /= Nothing then Ports.syncObservationViewport (ObservationViewport.sync changed 0 (treeTarget |> Maybe.map (\key -> (key, "first"))) viewport) else Cmd.none ] )


preferenceMode : ObservationRequestMode -> String
preferenceMode mode =
    case mode of
        ObservationFlatMode -> "flat"
        ObservationExactSubjectMode -> "exact"
        ObservationMatchMode -> "match"
        ObservationFacetMode -> "tree"


rememberPreferences : Model -> Model
rememberPreferences model =
    updateObservation (\state ->
        { state | preferenceTouch = state.preferenceTouch + 1
        , preferenceScroll = Nothing
        , preferencePendingDetail = False
        , preferenceEntryHistory = Nothing
        , preferenceValue = Preferences.normalize
            { modeIntent = True
            , detail = state.selectedId |> Maybe.map (\observationId -> { id = observationId, occurrence = state.detailReturnTarget, mode = preferenceMode state.requestMode })
            , subjects = Dict.toList state.expandedSubjects |> List.filter Tuple.second |> List.map Tuple.first
            , groups = Dict.toList state.expandedMatchGroups |> List.filter Tuple.second |> List.map Tuple.first
            , paths = Set.toList state.tree.expanded, focusedPath = state.tree.focused, selectedPath = state.tree.selectedPath
            , context = Just (Helpers.encodeObservationQuery (Helpers.observationAppliedQuery state))
            , queries = Dict.insert (modeKey state.requestMode) state.query state.modeQueries, subjectQueries = Dict.insert (modeKey state.requestMode) state.subjectQuery state.modeSubjectQueries, scrolls = Dict.insert (modeKey state.requestMode) state.viewport.top state.modeScrolls, fileInput = state.matchPathsInput }
        }) model


authorizedPreferenceOwner : Model -> Maybe Preferences.Owner
authorizedPreferenceOwner model =
    if model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) then Nothing else
    Maybe.map2 (\workspace context ->
        { workspaceId = workspace, actorId = context.principal.actorId, authority = context.principal.authority
        , runtimeId = model.flags.sessionId, epoch = model.sessionRequestEpoch, requestId = model.observations.nextPreferenceRequest, touch = model.observations.preferenceTouch })
        (repositoryWorkspaceId model) model.sessionContext


samePreferenceScope : Preferences.Owner -> Preferences.Owner -> Bool
samePreferenceScope a b =
    a.workspaceId == b.workspaceId && a.actorId == b.actorId && a.authority == b.authority && a.runtimeId == b.runtimeId && a.epoch == b.epoch


preferenceCommand : String -> Preferences.Owner -> Encode.Value -> Cmd Msg
preferenceCommand operation owner value =
    Ports.observationPreferenceCommand (Encode.object [ ( "operation", Encode.string operation ), ( "owner", Preferences.ownerValue owner ), ( "value", value ) ])


syncPreferences : Model -> Model -> ( Model, Cmd Msg )
syncPreferences previous model =
    let
        state = model.observations
        retire = Ports.observationPreferenceCommand (Encode.object [ ( "operation", Encode.string "retire" ) ])
    in
    case authorizedPreferenceOwner model of
        Nothing ->
            if state.preferenceOwner == Nothing && previous.observations.preferenceOwner == Nothing then ( model, Cmd.none ) else
            ( updateObservation (\current -> { current | preferenceOwner = Nothing, preferenceValue = Preferences.empty, preferenceHydrated = False, preferencePendingDetail = False, preferenceEntryHistory = Nothing, preferenceEntryEcho = Nothing
                , expandedSubjects = Dict.empty, expandedMatchGroups = Dict.empty, nextPreferenceRequest = current.nextPreferenceRequest + 1 }) model, retire )
        Just authorized ->
            case state.preferenceOwner of
                Just owner ->
                    if samePreferenceScope owner authorized then
                        if state.preferenceHydrated && state.preferencePendingDetail && model.activeTab == ObservationsTab then
                            restorePendingPreferences model
                        else if state.preferenceHydrated && (state.preferenceValue /= previous.observations.preferenceValue || state.preferenceTouch /= previous.observations.preferenceTouch || not previous.observations.preferenceHydrated) then
                            ( model, preferenceCommand "write" owner (Preferences.encode owner state.preferenceValue) )
                        else ( model, Cmd.none )
                    else
                        let clean = { state | preferenceOwner = Nothing, preferenceValue = Preferences.empty, preferenceHydrated = False, preferencePendingDetail = False, preferenceEntryHistory = Nothing, preferenceEntryEcho = Nothing, expandedSubjects = Dict.empty, expandedMatchGroups = Dict.empty } in
                        syncPreferences previous { model | observations = clean }
                Nothing ->
                    ( { model | observations = { state | preferenceOwner = Just authorized, preferenceHydrated = False, nextPreferenceRequest = state.nextPreferenceRequest + 1 } }
                    , preferenceCommand "read" authorized Encode.null )


hydratePreferences : Encode.Value -> Model -> ( Model, Cmd Msg )
hydratePreferences payload model =
    let state = model.observations in
    case ( state.preferenceOwner, Decode.decodeValue (Decode.field "owner" Preferences.ownerDecoder) payload, authorizedPreferenceOwner model ) of
        ( Just owner, Ok received, Just authorized ) ->
            if owner /= received || not (samePreferenceScope owner authorized) || state.preferenceHydrated then ( model, Cmd.none ) else
            let
                accepted = { state | preferenceHydrated = True }
                value = Decode.decodeValue (Decode.field "value" (Preferences.decoder owner)) payload
            in
            if state.preferenceTouch /= owner.touch then ( { model | observations = accepted }, Cmd.none ) else
            case value of
                Err _ ->
                    let
                        context = Helpers.observationUrlContext model.url
                        explicit = state.preferenceEntryHistory /= Just (Url.toString model.url) && context.notice == Nothing && (model.url.fragment |> Maybe.withDefault "" |> String.split "&" |> List.filterMap (String.split "=" >> List.head >> Maybe.andThen Url.percentDecode) |> List.any (\key -> List.member key ["ov","oq","observation"]))
                        ready = {model | observations = accepted}
                    in
                    (if explicit then rememberPreferences ready else ready, Cmd.none)
                Ok prefs ->
                    let
                        revealPath =
                            case accepted.tree.revealTarget of
                                Just target -> Just target.subject.subject
                                Nothing ->
                                    if explicit && not accepted.searchOriginDetail && context.fragment.observationId /= Nothing && context.fragment.observationId == accepted.selectedId then
                                        currentSelectedObservation accepted |> Maybe.map (\observation -> accepted.selectedFacet |> Maybe.map .subject |> Maybe.withDefault observation.subject)
                                    else Nothing
                        requiredPaths = revealPath |> Maybe.map (ObservationTree.ancestors >> Set.fromList) |> Maybe.withDefault Set.empty
                        restoredTree = let tree = accepted.tree in ObservationTree.reconcileSelection { tree | expanded = Set.union (Set.fromList prefs.paths) requiredPaths, focused = prefs.focusedPath
                            , selectedPath = if explicit || tree.revealTarget /= Nothing || tree.selectedPath /= Nothing then tree.selectedPath else prefs.selectedPath }
                        hydrated = { accepted | preferenceValue = {prefs | selectedPath = restoredTree.selectedPath, modeIntent = prefs.modeIntent || explicit, context = if explicit then Just (Helpers.encodeObservationQuery (Helpers.observationAppliedQuery accepted)) else prefs.context}, preferencePendingDetail = prefs.detail /= Nothing, preferenceEntryHistory = accepted.preferenceEntryHistory, expandedSubjects = Dict.fromList (List.map (\key -> ( key, True )) prefs.subjects)
                            , expandedMatchGroups = Dict.fromList (List.map (\key -> ( key, True )) prefs.groups)
                            , modeQueries = if explicit then Dict.union accepted.modeQueries prefs.queries else Dict.union prefs.queries accepted.modeQueries
                            , modeSubjectQueries = if explicit then Dict.union accepted.modeSubjectQueries prefs.subjectQueries else Dict.union prefs.subjectQueries accepted.modeSubjectQueries
                            , modeScrolls = Dict.union accepted.modeScrolls prefs.scrolls
                            , matchPathsInput = if accepted.matchPathsInput == "" then prefs.fileInput else accepted.matchPathsInput
                            , tree = restoredTree }
                        context = Helpers.observationUrlContext model.url
                        ownedEntry = state.preferenceEntryHistory == Just (Url.toString model.url)
                        explicit = not ownedEntry && (model.url.fragment |> Maybe.withDefault "" |> String.split "&" |> List.filterMap (String.split "=" >> List.head >> Maybe.andThen Url.percentDecode)
                            |> List.any (\key -> List.member key [ "ov", "oq", "ox", "observation" ]))
                        default = Helpers.defaultObservationQuery
                        restored = if explicit || context.notice /= Nothing then Nothing else prefs.context |> Maybe.andThen Url.percentDecode |> Maybe.andThen (Decode.decodeString Helpers.observationQueryDecoder >> Result.toMaybe) |> Maybe.map (\query -> if not prefs.modeIntent && prefs.detail == Nothing && query == { default | requestMode = ObservationFlatMode } then default else query)
                        queryState = restored |> Maybe.map (\query ->
                            let restoredState = Helpers.restoreObservationQuery query hydrated in
                            if query == Helpers.observationAppliedQuery hydrated then restoredState else retireFilterReads restoredState) |> Maybe.withDefault hydrated
                        savedTop = if queryState.selectedId /= Nothing || context.fragment.observationId /= Nothing || prefs.detail /= Nothing then Nothing else Dict.get (modeKey queryState.requestMode) prefs.scrolls
                        viewport = queryState.viewport
                        ready = { model | observations = { queryState | preferenceScroll = savedTop, viewport = { viewport | top = Maybe.withDefault viewport.top savedTop } } }
                        ( loaded, loadCmd ) = if restored /= Nothing then restoreRouteResults ready else ( ready, Cmd.none )
                        ( selected, selectCmd ) = restorePendingPreferences loaded
                    in
                    if ownedEntry && restored /= Nothing then
                        let (linked,linkCmd) = Helpers.writeObservationHistory False selected in
                        (linked,Cmd.batch [loadCmd,selectCmd,linkCmd])
                    else ( selected, Cmd.batch [ loadCmd, selectCmd ] )
        _ -> ( model, Cmd.none )


restorePendingPreferences : Model -> ( Model, Cmd Msg )
restorePendingPreferences model =
    let
        state = model.observations
        currentOwner = Maybe.map2 samePreferenceScope state.preferenceOwner (authorizedPreferenceOwner model) |> Maybe.withDefault False
        urlContext = Helpers.observationUrlContext model.url
        urlKeys = model.url.fragment |> Maybe.withDefault "" |> String.split "&" |> List.filterMap (String.split "=" >> List.head >> Maybe.andThen Url.percentDecode)
        ownedEntryHistory = state.preferenceEntryHistory == Just (Url.toString model.url)
        urlWins = not ownedEntryHistory && urlContext.fragment.tab == ObservationsTab && (urlContext.fragment.observationId /= Nothing || urlContext.notice /= Nothing || List.any (\key -> List.member key [ "observation", "ov", "oq", "ox" ]) urlKeys)
        consumed = { model | observations = { state | preferencePendingDetail = False, preferenceEntryHistory = Nothing } }
    in
    if not currentOwner || not state.preferenceHydrated || not state.preferencePendingDetail || model.activeTab /= ObservationsTab then
        ( model, Cmd.none )
    else if state.preferenceEntryHistory /= Nothing && urlContext.fragment.tab /= ObservationsTab then
        -- The early tab entry's history command has not reached UrlChanged yet.
        ( model, Cmd.none )
    else
        case state.preferenceValue.detail of
            Just detail ->
                if urlContext.notice == Nothing && urlContext.fragment.observationId == Just detail.id && state.selectedId == Just detail.id && detail.mode == preferenceMode state.requestMode then
                    ( { consumed | observations = { state | preferencePendingDetail = False, preferenceEntryHistory = Nothing, detailReturnTarget = detail.occurrence } }, Cmd.none )
                else if urlWins || state.selectedId /= Nothing then
                    ( consumed, Cmd.none )
                else
                    let
                        ( selected, command ) = selectObservation detail.id consumed
                        selectedState = selected.observations
                        occurrence = if detail.mode == preferenceMode selectedState.requestMode then detail.occurrence else Nothing
                        restored = { selected | observations = { selectedState | detailReturnTarget = occurrence } }
                        ( linked, linkCmd ) = if ownedEntryHistory then Helpers.writeObservationHistory False restored else ( restored, Cmd.none )
                    in
                    ( linked, Cmd.batch [ command, linkCmd ] )
            Nothing -> ( consumed, Cmd.none )


updateViewport : Encode.Value -> Model -> ( Model, Cmd Msg )
updateViewport payload model =
    if model.activeTab /= ObservationsTab || viewportLifetime model /= model.observations.viewport.stamp
        || Decode.decodeValue (Decode.field "navigationToken" Decode.int) payload /= Ok model.observations.detailNavigationToken then
        ( model, Cmd.none )
    else
        case ObservationViewport.updateWithOwner model.observations.inlineOwner model.observations.detailReturnTarget payload model.observations.viewport of
            Nothing -> ( model, Cmd.none )
            Just ( viewport, target, adjustment ) ->
                let
                    state = model.observations
                    valid intent = intent.workspaceId == viewport.stamp.workspace && intent.sessionEpoch == model.sessionRequestEpoch
                        && intent.queryGeneration == viewport.stamp.generation && intent.navigationToken == state.detailNavigationToken && state.selectedId == intent.selectedId
                    retained = state.pendingReturnNavigation |> Maybe.andThen (\intent ->
                        if not (valid intent) || target /= Nothing || Decode.decodeValue (Decode.field "focus" (Decode.nullable Decode.string)) payload == Ok (Just "@outside") then Nothing
                        else Just { intent | fallback = intent.fallback || (intent.intent == "return" && not (Dict.member intent.originKey viewport.index.positions)) })
                    ready = retained |> Maybe.andThen (\intent ->
                        if intent.readyRevision == Just state.viewport.stamp.revision && viewport.stamp == state.viewport.stamp && viewport.index.heights == state.viewport.index.heights
                            && not viewport.restoring && abs adjustment < 0.01
                            && (intent.intent /= "detail" || Decode.decodeValue (Decode.field "detailMounted" Decode.bool) payload == Ok True) then Just intent else Nothing)
                    pending = if ready /= Nothing then Nothing else retained |> Maybe.map (\intent -> { intent | readyRevision = if viewport.restoring then Nothing else Just viewport.stamp.revision })
                    previous intent = { state | detailNavigationEpoch = intent.sessionEpoch, detailNavigationToken = intent.previousToken, selectedId = intent.previousSelection }
                    destination = { state | viewport = viewport }
                    navigation = ready |> Maybe.map (\intent -> navigateDetail intent.intent intent.workspaceId (if intent.fallback || intent.originKey == "" then Nothing else Just intent.originKey) (previous intent) destination) |> Maybe.withDefault Cmd.none
                    settled = viewport.stamp == state.viewport.stamp && viewport.index.heights == state.viewport.index.heights
                        && not viewport.restoring && abs adjustment < 0.01 && target == Nothing
                        && (pending == Nothing || Maybe.map .readyRevision pending == Maybe.map .readyRevision state.pendingReturnNavigation)
                    releaseExtent = settled && viewport.preserveExtent > 0 && not state.loading && not state.facetLoading && not (treeReadsPending state)
                    stamp = viewport.stamp
                    finalViewport = if releaseExtent then { viewport | preserveExtent = 0, stamp = { stamp | revision = stamp.revision + 1 } } else viewport
                in
                ( { model | observations = { state | viewport = finalViewport, pendingReturnNavigation = pending
                    , tree = if Decode.decodeValue (Decode.field "focus" (Decode.nullable Decode.string)) payload == Ok (Just "@outside") then let tree = state.tree in {tree | revealFocus = False, revealTarget = Maybe.map (\intent -> {intent | focus = False}) tree.revealTarget} else state.tree } }
                , Cmd.batch [ Ports.syncObservationViewport (ObservationViewport.syncReceipt (settled && not releaseExtent) adjustment target finalViewport), navigation ] )


resultPresentation : ObservationModel -> ( Array.Array ObservationResultRow, ObservationViewport.State, Maybe String )
resultPresentation state =
    let
        -- Pure view callers (fixtures/tests) may not have passed through routing.
        uninitialized = state.viewport.stamp.workspace == "" && Array.isEmpty state.resultRows
        rows = if uninitialized then projectResultRows state else state.resultRows
        viewport = if uninitialized then ObservationViewport.rebuild state.viewport.stamp (Array.toList rows |> List.map resultRowKey) state.viewport else state.viewport
        owner = selectedOwner state
    in
    ( rows, viewport, owner )


timestampSources : ObservationModel -> List String
timestampSources state =
    let
        ( rows, viewport, owner ) = resultPresentation state
        rowSources piece =
            case piece of
                HierarchyViewport.Row at _ _ ->
                    case Array.get at rows |> Maybe.map rowContent of
                        Just (ObservationCardRow context observation) ->
                            if owner == Just (observationCardDomId context observation.id) then []
                            else [ observation.updatedAt, (revisionDisplay observation).recordedAt ]
                        Just (ObservationFacetRow facet) -> [ facet.latestUpdatedAt ]
                        _ -> []
                _ -> []
        detail =
            if state.selectedId == Nothing then []
            else state.selectedDetail |> Maybe.map (\observation ->
                [ observation.createdAt, observation.updatedAt, (revisionDisplay observation).recordedAt ]
                    ++ (if state.historyExpanded then
                            currentHistory (Just observation) state.history
                                |> Maybe.map (.items >> List.map (.provenance >> .recordedAt)) |> Maybe.withDefault []
                        else [])) |> Maybe.withDefault []
    in
    detail ++ (ObservationViewport.piecesWithOwner owner state.detailReturnTarget viewport |> List.concatMap rowSources)


viewResultRows : TimestampDisplay.State -> Bool -> ObservationModel -> Html Msg
viewResultRows display canEdit state =
    let
        ( rows, viewport, owner ) = resultPresentation state
        piece item =
            case item of
                HierarchyViewport.Gap at height ->
                    ( "gap:" ++ String.fromInt at, div [ class "observation-viewport-spacer", style "height" (String.fromFloat height ++ "px"), attribute "aria-hidden" "true" ] (viewTreeGap rows viewport at height) )
                HierarchyViewport.Row at key _ ->
                    ( key, Array.get at rows |> Maybe.map (\row ->
                        div [ class "observation-viewport-row", attribute "data-observation-key" key, attribute "data-observation-position" (String.fromInt at) ]
                            [ Lazy.lazy5 viewResultRow display canEdit owner state row ]) |> Maybe.withDefault (text "") )
    in
    Keyed.node "div"
        ([ id "observation-viewport", class "observation-list-rows observation-viewport"
        , style "min-height" (String.fromFloat viewport.preserveExtent ++ "px")
        , attribute "data-observation-preserve-scroll" (if viewport.preserveScroll then "true" else "false")
        , attribute "data-observation-viewport-context" (Encode.encode 0 (ObservationViewport.stampValue viewport.stamp))
        , attribute "data-observation-layout-ready" (if viewport.restoring then "false" else "true")
        , attribute "data-observation-focus-key" (Maybe.withDefault "" viewport.focus)
        , attribute "data-observation-logical-count" (String.fromInt (Array.length rows))
        , attribute "data-observation-loaded-count" (String.fromInt (if treeMode state.requestMode then Dict.values state.tree.terminals |> List.concatMap (.items >> List.map .id) |> Set.fromList |> Set.size else List.length state.orderedIds)) ]
            ++ (if treeMode state.requestMode then [attribute "role" "tree", attribute "aria-label" "Stored subject paths"] else []))
        (ObservationViewport.piecesWithOwner owner state.detailReturnTarget viewport |> List.map piece)


continuationLabel : TreeContinuation -> String
continuationLabel continuation =
    case continuation of
        TreeNext -> "next"
        TreeUnknown -> "unknown"
        TreeEnd -> "end"


treeRail : String -> ObservationTreeRail -> List ( String, String ) -> List (Html.Attribute Msg) -> Html Msg
treeRail kind rail styles extra =
    span ([ class ("observation-tree-rail " ++ kind)
        , attribute "style" (("--tree-rail-depth", String.fromInt rail.depth) :: styles |> List.map (\(key, value) -> key ++ ":" ++ value) |> String.join ";")
        , attribute "data-tree-owner" rail.owner
        , attribute "data-tree-rail-tail" rail.tail
        , attribute "data-tree-rail-state" (if rail.unknown then "unknown" else "complete") ] ++ extra) []


viewTreePresentation : ObservationTreeTopology -> Html Msg -> Html Msg
viewTreePresentation topology content =
    let
        parent = topology.parent |> Maybe.map (\rail ->
            [ treeRail "tree-rail-incoming" rail [] []
            , span [class "observation-tree-elbow", attribute "style" ("--tree-rail-depth:" ++ String.fromInt rail.depth)] [] ]
                ++ (if topology.following == TreeEnd then [] else [treeRail "tree-rail-following" rail [] [attribute "data-tree-following" (continuationLabel topology.following)]])) |> Maybe.withDefault []
        rails = List.map (\rail -> treeRail "tree-rail-ancestor" rail [] []) topology.ancestors
            ++ parent ++ (topology.children |> Maybe.map (\rail -> [treeRail "tree-rail-child" rail [] []]) |> Maybe.withDefault [])
    in
    div [class "observation-tree-presentation", attribute "style" ("--tree-depth:" ++ String.fromInt topology.depth ++ ";--tree-levels:" ++ String.fromInt topology.levels)
        , attribute "data-tree-depth" (String.fromInt topology.depth), attribute "data-tree-root" topology.root
        , attribute "data-tree-parent" (topology.parent |> Maybe.map .owner |> Maybe.withDefault "")
        , attribute "data-tree-following" (continuationLabel topology.following) ]
        [ div [class "observation-tree-connectors", attribute "aria-hidden" "true"] rails
        , div [class "observation-tree-row-content"] [content] ]


{-| Only rails at the two gap boundaries can meet a mounted window or pin.
Clip each parent-owned interval against the existing spacer geometry; omitted
middle trees add no DOM and cannot create a line through unrelated roots.
-}
viewTreeGap : Array.Array ObservationResultRow -> ObservationViewport.State -> Int -> Float -> List (Html Msg)
viewTreeGap rows viewport at height =
    let
        top = HierarchyViewport.offset at viewport.index
        end = Basics.min (Array.length rows - 1) (HierarchyViewport.positionAt (top + height - 0.001) viewport.index)
        topology position = Array.get position rows |> Maybe.andThen (\row -> case row of
            ObservationTreeRow value _ -> Just value
            _ -> Nothing)
        boundaries = List.filterMap topology [at, end]
        candidates = boundaries |> List.concatMap (\value -> value.ancestors ++ List.filterMap identity [value.parent, value.children])
            |> List.foldl (\rail found -> Dict.insert rail.owner rail found) Dict.empty |> Dict.values
        levels = boundaries |> List.head |> Maybe.map .levels |> Maybe.withDefault 1
        rowTop key = Dict.get key viewport.index.positions |> Maybe.map (\position -> HierarchyViewport.offset position viewport.index)
        rowHeight key = Dict.get key viewport.index.positions |> Maybe.map (\position -> HierarchyViewport.offset (position + 1) viewport.index - HierarchyViewport.offset position viewport.index)
        segment rail =
            case (rowTop rail.owner, rowTop rail.tail) of
                (Just start, Just tail) ->
                    let
                        from = Basics.max top (start + Maybe.withDefault 0 (rowHeight rail.owner))
                        to = Basics.min (top + height) (tail + (Maybe.withDefault 0 (rowHeight rail.tail) * (if rail.unknown then 1 else 0.5)))
                    in if to <= from then Nothing else Just (treeRail "tree-rail-gap" rail
                        [("top", String.fromFloat (from - top) ++ "px"), ("height", String.fromFloat (to - from) ++ "px"), ("--tree-levels", String.fromInt levels)] [])
                _ -> Nothing
    in List.filterMap segment candidates


viewResultRow : TimestampDisplay.State -> Bool -> Maybe String -> ObservationModel -> ObservationResultRow -> Html Msg
viewResultRow display canEdit owner state row =
    case row of
        ObservationTreeRow topology content ->
            viewTreePresentation topology (viewResultRow display canEdit owner state content)
        ObservationCardRow context observation ->
            if String.startsWith "tree-terminal:" context then
                div [class "observation-tree-observation", treeIndent (List.length (String.split "/" context))]
                    [viewObservationRow display canEdit owner state context observation]
            else viewObservationRow display canEdit owner state context observation
        ObservationFacetRow facet -> viewFacet display facet
        ObservationPathRow path empty ->
            section [ class "observation-path-group" ]
                [ h3 [ id ("observation-path-" ++ domToken path), class "observation-path-heading" ] [ Helpers.copyableValue "" "file path" path path ]
                , if empty then p [ class "observation-path-empty" ] [ text "No loaded matches for this path." ] else text "" ]
        ObservationSubjectRow path key kind subject count expanded ->
            section [ class "observation-subject-group", attribute "data-observation-group" key, attribute "aria-label" ("Subject " ++ subject ++ " for " ++ path) ]
                [ div [ class "observation-subject-group-header" ]
                    [ button [ class "observation-subject-group-toggle", type_ "button", onClick (ToggleObservationMatchGroup key)
                        , attribute "aria-expanded" (if expanded then "true" else "false"), attribute "aria-controls" "observation-viewport"
                        , attribute "aria-label" ((if expanded then "Collapse " else "Expand ") ++ subject ++ " for " ++ path) ]
                        [ span [ class "entity-type-label observation-kind" ] [ text (subjectKindLabel kind) ]
                        , span [ class "observation-loaded-count" ] [ text (String.fromInt count ++ " loaded") ] ]
                    , Helpers.copyableValue "observation-subject observation-subject-copy" "matching subject" subject subject
                    ] ]
        ObservationTreePathRow node -> viewTreeNode state node
        ObservationTreeTerminalRow subject ->
            div [ class "observation-tree-terminal", attribute "role" "region", attribute "aria-label" (subjectKindLabel subject.subjectKind ++ " observations for " ++ subject.subject)
                , treeIndent (List.length (String.split "/" subject.subject)) ]
                [ span [class "entity-type-label observation-kind"] [text (subjectKindLabel subject.subjectKind)]
                , span [] [text "Observations"] ]
        ObservationTreePageRow prefix kind -> viewTreePage state prefix kind


treeIndent : Int -> Html.Attribute Msg
treeIndent depth = attribute "style" ("--observation-tree-depth:" ++ String.fromInt depth)


viewTreeNode : ObservationModel -> Api.ObservationSubjectTreeNode -> Html Msg
viewTreeNode state node =
    let tree = state.tree
        expanded = Set.member node.path tree.expanded
        siblings = Dict.get (ObservationTree.parent node.path) tree.branches
        paths = siblings |> Maybe.map .paths |> Maybe.withDefault []
        setSize = if siblings |> Maybe.map .hasMore |> Maybe.withDefault True then -1 else List.length paths
        position = paths |> List.indexedMap Tuple.pair |> List.filter (Tuple.second >> (==) node.path) |> List.head |> Maybe.map (Tuple.first >> (+) 1) |> Maybe.withDefault 1
        focused = tree.focused |> Maybe.withDefault (ObservationTree.visiblePaths tree |> List.head |> Maybe.withDefault "")
        keyEvent = Decode.map2 Tuple.pair (Decode.field "key" Decode.string) (Decode.at ["target", "role"] (Decode.nullable Decode.string))
            |> Decode.andThen (\(key, role) -> if role == Just "treeitem" && List.member key ["ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight", "Home", "End", "Enter", " "] then Decode.succeed {message = ObservationTreeKey node.path key, preventDefault = True, stopPropagation = True} else Decode.fail "Not path navigation")
    in
    div [ id ("observation-tree-path-" ++ domToken node.path), class "observation-tree-path", attribute "role" "treeitem"
        , tabindex (if focused == node.path then 0 else -1), attribute "aria-level" (String.fromInt (List.length (String.split "/" node.path)))
        , attribute "aria-posinset" (String.fromInt position), attribute "aria-setsize" (String.fromInt setSize)
        , attribute "aria-expanded" (if expanded then "true" else "false"), attribute "aria-selected" (if tree.selectedPath == Just node.path then "true" else "false")
        , attribute "aria-label" node.path, attribute "data-tree-path" node.path
        , treeIndent (List.length (String.split "/" node.path) - 1)
        , Html.Events.onFocus (FocusObservationTreePath node.path), custom "keydown" keyEvent ]
        [ button [class "tree-toggle observation-tree-chevron", type_ "button", tabindex -1, onClick (ToggleObservationTreePath node.path)
            , attribute "aria-label" ((if expanded then "Collapse " else "Expand ") ++ node.path)] [text (if expanded then "▼" else "▶")]
        , span [class "observation-tree-label", onClick (SelectObservationTreePath node.path)] [text node.segment]
        , if List.any (\symbol -> String.contains symbol node.segment) ["*", "?", "["] then span [class "observation-tree-pattern"] [text "Literal pattern"] else text ""
        , span [class "observation-loaded-count"] [text (String.fromInt node.observationCount ++ (if node.observationCount == 1 then " unique observation" else " unique observations"))]
        , Helpers.copyableValue "observation-tree-copy" "canonical subject path" node.path node.path ]


viewTreePage : ObservationModel -> String -> Maybe Api.SubjectKind -> Html Msg
viewTreePage state prefix kind =
    let (loading, error, more) = case kind of
            Nothing -> Dict.get prefix state.tree.branches |> Maybe.map (\branch -> (branch.request /= Nothing, branch.error, branch.hasMore)) |> Maybe.withDefault (True, Nothing, False)
            Just value -> Dict.get (ObservationTree.terminalKey {subjectKind = value, subject = prefix}) state.tree.terminals |> Maybe.map (\terminal -> (terminal.request /= Nothing, terminal.error, terminal.hasMore)) |> Maybe.withDefault (True, Nothing, False)
        label = if kind == Nothing then "paths" else "observations"
        exhausted = case kind of
            Nothing -> Dict.get prefix state.tree.branches |> Maybe.map (\branch -> branch.nextOffset > 2147483647) |> Maybe.withDefault False
            Just value -> Dict.get (ObservationTree.terminalKey {subjectKind = value, subject = prefix}) state.tree.terminals |> Maybe.map (\terminal -> terminal.nextOffset > 100000) |> Maybe.withDefault False
    in div [class "observation-tree-page", treeIndent (if prefix == "" then 0 else List.length (String.split "/" prefix))]
        [ if loading then span [attribute "role" "status"] [text ("Loading " ++ label ++ "…")] else text ""
        , error |> Maybe.map (\message -> p [class "form-error", attribute "role" "alert"] [text message]) |> Maybe.withDefault (text "")
        , if not loading && (more || error /= Nothing) then button [type_ "button", class "btn btn-secondary observation-tree-more", disabled exhausted, onClick (LoadObservationTreePage prefix kind)] [text (if exhausted then "Offset limit reached" else (if error /= Nothing then "Retry " else "Load more ") ++ label)] else text "" ]


viewTree : TimestampDisplay.State -> Bool -> ObservationModel -> Html Msg
viewTree display canEdit state =
    div [id "observation-results", class "entity-list observation-list observation-subject-tree", tabindex -1]
        [ viewStaleResultsNotice state
        , p [class "form-help observation-tree-count-help"] [text "Counts are unique within each path. An observation can appear under multiple paths, so child counts may overlap."]
        , if state.tree.refreshing || state.refreshPending then p [class "form-help", attribute "role" "status"] [text "Refreshing Subject Tree. Showing the last completed paths."] else text ""
        , if state.tree.totals |> Maybe.map (Tuple.second >> (==) 0) |> Maybe.withDefault False then p [class "empty-state"] [text "No matching subjects."] else text ""
        , viewResultRows display canEdit state ]


viewObservations : Api.Workspace -> Model -> Html Msg
viewObservations workspace model =
    let
        state =
            model.observations

        noticeAttributes =
            [ class "form-help observation-url-notice", attribute "role" "status" ]
                ++ (if Permissions.canEditCurrentWorkspace model && state.deleteConfirmation /= Nothing then
                        [ attribute "inert" "", attribute "aria-hidden" "true" ]

                    else
                        []
                   )
    in
    div [ class "observation-workspace-view" ]
        [ if workspace.workspaceType == Api.Repository then
            case state.linkNotice of
                Just message -> p noticeAttributes [ text message ]
                Nothing -> if Result.toMaybe (Helpers.completeObservationUrl model) == Nothing then p noticeAttributes [ text Helpers.excludedObservationNotice ] else text ""
          else text ""
        , viewObservationsStateWithSelection (state.selectedId |> Maybe.map (\observationId -> isAuditObservation observationId model) |> Maybe.withDefault False) model.timestamps (Permissions.canEditCurrentWorkspace model) workspace { state | detailNavigationEpoch = model.sessionRequestEpoch }
        ]


viewObservationsState : TimestampDisplay.State -> Api.Workspace -> ObservationModel -> Html Msg
viewObservationsState display workspace state =
    viewObservationsStateWithPermission display False workspace state


viewObservationsStateWithPermission : TimestampDisplay.State -> Bool -> Api.Workspace -> ObservationModel -> Html Msg
viewObservationsStateWithPermission display canEdit workspace state =
    viewObservationsStateWithSelection False display canEdit workspace state


viewObservationsStateWithSelection : Bool -> TimestampDisplay.State -> Bool -> Api.Workspace -> ObservationModel -> Html Msg
viewObservationsStateWithSelection forceDetached display canEdit workspace state =
    if workspace.workspaceType /= Api.Repository then
        div [ class "empty-state observation-state observation-state-unavailable" ]
            [ h3 [] [ text "Observations unavailable" ]
            , p [] [ text "Observations are available only for repository workspaces." ]
            ]

    else
        div [ id "observation-panel", class "observations-panel", attribute "data-observation-context" (Encode.encode 0 (navigationStamp workspace.id state)), attribute "data-observation-search-reset" (observationSearchResetStamp workspace.id state) ]
            [ div
                ([ class "observation-curation-background" ]
                    ++ (if canEdit && state.deleteConfirmation /= Nothing then
                            [ attribute "inert" "", attribute "aria-hidden" "true" ]

                        else
                            []
                       )
                )
                [ div [class "observation-browser-context"] [viewModeNavigation state, viewFilters state]
                , div
                    [ classList
                        [ ( "observation-layout", True )
                        , ( "observation-layout-with-detail", state.selectedId /= Nothing )
                        ]
                    ]
                    [ if state.searchOriginDetail && state.selectedId /= Nothing then
                        viewSearchDetailState display state
                      else if state.selectedId /= Nothing && selectedOwner state == Nothing && (forceDetached || not (treeMode state.requestMode)) then
                        div [ class "card observation-result observation-detached", attribute "data-observation-detached" "true" ]
                            [ p [ class "form-help", attribute "role" "status" ] [ text "Selected observation is outside the displayed result rows. Loaded counts and file-match evidence are unchanged." ]
                            , case state.selectedDetail of
                                Just observation -> viewObservationHeader True "observation-detached-toggle" observation
                                Nothing -> div [ class "card-header observation-card-header" ]
                                    [ observationToggle True "observation-detached-toggle" "Selected observation" ReturnObservationResults
                                    , span [ class "observation-subject" ] [ text "Selected observation" ]
                                    ]
                            , viewDetail display canEdit state
                            ]
                      else text ""
                    , viewList display canEdit workspace.id state
                    ]
                ]
            , viewDeleteConfirmation canEdit state
            ]


viewRetainedDraft : Model -> Html Msg
viewRetainedDraft model =
    case model.observations.edit of
        Just edit ->
            if hasProtectedEdit model && (model.activeTab /= ObservationsTab || model.observations.selectedId /= Just edit.observationId) then
                div [ class "observation-retained-draft", attribute "role" "status" ]
                    [ p [] [ text "Your Observation draft is retained. Save or discard it before leaving this workspace or editing another observation." ]
                    , button [ class "btn btn-secondary", type_ "button", onClick ReturnToObservationDraft ] [ text "Return to draft" ]
                    , button [ class "btn btn-primary", type_ "button", onClick SaveObservationEdit, disabled (edit.saving || edit.conflict || editValidationError edit /= Nothing) ]
                        [ text (if edit.saving then "Saving..." else "Save draft") ]
                    , button [ class "btn btn-secondary", type_ "button", onClick CancelObservationEdit, disabled edit.saving ] [ text "Discard draft" ]
                    ]

            else
                text ""

        Nothing ->
            text ""


viewFilters : ObservationModel -> Html Msg
viewFilters state =
    div [] [ viewFilterInputs state, viewAppliedFilters state ]


viewFilterInputs : ObservationModel -> Html Msg
viewFilterInputs state =
    let
        searchLabel = case state.requestMode of
            ObservationFlatMode -> "Search observations"
            ObservationMatchMode -> "Search file matches"
            _ -> "Search observations"
        active = List.length (List.filter identity
            [ state.subjectKind /= Nothing, if state.requestMode == ObservationFlatMode then state.subject /= "" else state.requestMode == ObservationExactSubjectMode && state.selectedFacet /= Nothing
            , state.gitSha /= "", state.currentGitSha /= "", state.historyGitSha /= "" ])
        indication = if filterValidationError state /= Nothing then "Editing" else String.fromInt active ++ " active"
    in
    div [ class "observation-discovery-controls" ]
        [ Html.form [ class "filter-bar observation-filters observation-search", onSubmit ApplyObservationFilters ]
            [ if state.requestMode /= ObservationMatchMode then
                div [ class "filter-group observation-filter-group observation-paired-subjects" ]
                    [ label [ class "filter-label", for "observation-subject-query" ] [ text "Subjects" ]
                    , input [ id "observation-subject-query", class "form-input observation-filter-input", type_ "search", placeholder "Literal subject path", value state.subjectQuery
                        , onInput SetObservationSubjectQuery, Html.Events.on "compositionstart" (Decode.succeed ObservationFilterCompositionStart)
                        , Html.Events.on "compositionend" (Decode.map (ObservationFilterCompositionEnd True) (Decode.at [ "target", "value" ] Decode.string)) ] []
                    , if state.subjectQuery /= "" then button [ class "search-clear observation-query-clear", type_ "button", attribute "aria-label" "Clear Subjects", onClick (ClearObservationSearchText True) ] [text "×"] else text "" ]
              else text ""
            , div [ class "filter-group observation-filter-group observation-paired-content" ]
                [ label [ class "filter-label", for "observation-query" ] [ text searchLabel ]
                , input
                    [ id "observation-query"
                    , class "form-input observation-filter-input"
                    , type_ "search"
                    , placeholder "Search observation content"
                    , value state.query
                    , onInput SetObservationQuery
                    , Html.Events.on "compositionstart" (Decode.succeed ObservationFilterCompositionStart)
                    , Html.Events.on "compositionend" (Decode.map (ObservationFilterCompositionEnd False) (Decode.at [ "target", "value" ] Decode.string))
                    ]
                    []
                , if state.query /= "" then button [ class "search-clear observation-query-clear", type_ "button", attribute "aria-label" "Clear Search observations", onClick (ClearObservationSearchText False) ] [text "×"] else text ""
                ]
            , button
                [ id "observation-advanced-toggle"
                , classList [("btn", True), ("btn-secondary", True), ("observation-advanced-toggle", True), ("observation-advanced-active", active > 0)]
                , type_ "button"
                , attribute "aria-label" ("Advanced" ++ (if active > 0 then ", " ++ indication else ""))
                , onClick ToggleObservationAdvancedFilters
                , attribute "aria-expanded" (if state.advancedFiltersOpen then "true" else "false")
                , attribute "aria-controls" "observation-advanced-filters"
                ]
                [ text "Advanced", if active > 0 then span [class "observation-active-filter-count", attribute "aria-hidden" "true"] [text indication] else text "" ]
            ]
        , viewAdvancedFilters state
        , viewFileComposer state
        ]


viewAdvancedFilters : ObservationModel -> Html Msg
viewAdvancedFilters state =
    let
        shaField fieldId fieldLabel fieldValue setter =
            let invalid = filterValidationError {state | gitSha = fieldValue, currentGitSha = "", historyGitSha = ""} /= Nothing in
            div [class "filter-group observation-filter-group"]
                [ label [class "filter-label", for fieldId] [text fieldLabel]
                , input ([id fieldId, class "form-input observation-filter-input observation-filter-sha", value fieldValue, onInput setter, placeholder "40-character SHA", spellcheck False
                    , attribute "aria-invalid" (if invalid then "true" else "false")]
                    ++ (if invalid then [attribute "aria-describedby" "observation-filter-validation"] else [])) [] ]
    in
    div [ id "observation-advanced-filters", class "filter-bar observation-filters observation-advanced-filters", hidden (not state.advancedFiltersOpen) ]
        [ div [ class "filter-group observation-filter-group observation-filter-kind" ]
            [ label [ class "filter-label", for "observation-subject-kind" ] [text "Subject kind"]
            , select [ id "observation-subject-kind", class "filter-select observation-filter-select", onInput SetObservationSubjectKind ]
                [ option [ value "", selected (state.subjectKind == Nothing) ] [ text "Any kind" ]
                , option [ value "file", selected (state.subjectKind == Just Api.SubjectFile) ] [ text "Files" ]
                , option [ value "glob", selected (state.subjectKind == Just Api.SubjectGlob) ] [ text "Globs" ] ] ]
        , case state.requestMode of
            ObservationFlatMode ->
                div [ class "filter-group observation-filter-group" ]
                    [ label [ class "filter-label", for "observation-subject" ] [ text "Exact subject" ]
                    , input [ id "observation-subject", class "form-input observation-filter-input", placeholder "File or glob subject", value state.subject, onInput SetObservationSubject ] [] ]
            ObservationExactSubjectMode ->
                case state.selectedFacet of
                    Just selectedFacet ->
                        div [ class "filter-group observation-filter-group observation-selected-facet" ]
                            [ span [ class "filter-label" ] [ text "Locked terminal" ]
                            , strong [ class "observation-selected-facet-value" ]
                                [ text (subjectKindLabel selectedFacet.subjectKind ++ ": "), Helpers.copyableValue "" "repository subject" selectedFacet.subject selectedFacet.subject ] ]
                    Nothing -> p [ class "form-error", attribute "role" "alert" ] [ text "No terminal is selected." ]
            _ -> text ""
        , shaField "observation-git-sha" "Creation Git SHA" state.gitSha SetObservationGitSha
        , shaField "observation-current-git-sha" "Current reviewed Git SHA" state.currentGitSha SetObservationCurrentGitSha
        , shaField "observation-history-git-sha" "Recorded history Git SHA" state.historyGitSha SetObservationHistoryGitSha
        ]


viewFileComposer : ObservationModel -> Html Msg
viewFileComposer state =
    section [ id "observation-file-composer", class "filter-bar observation-filters observation-file-composer", hidden (not state.fileComposerOpen), attribute "aria-labelledby" "observation-file-composer-heading" ]
        [ div [ class "filter-group observation-filter-group observation-match-input" ]
            [ label [ id "observation-file-composer-heading", class "filter-label", for "observation-match-paths" ] [ text "Repository file paths" ]
            , textarea
                [ id "observation-match-paths"
                , class "form-input observation-filter-input"
                , placeholder "One concrete repository-relative path per line"
                , value state.matchPathsInput
                , onInput SetObservationMatchPaths
                , attribute "rows" "4"
                , attribute "aria-describedby" ("observation-match-help" ++ (if state.matchValidationError /= Nothing then " observation-match-error" else ""))
                , attribute "aria-invalid" (if state.matchValidationError /= Nothing then "true" else "false")
                ]
                []
            , p [ id "observation-match-help", class "form-help" ] [ text "One concrete repository-relative path per line, without wildcards. Select Match files to update results." ]
            , case state.matchValidationError of
                Just message ->
                    p [ id "observation-match-error", class "form-error", attribute "role" "alert" ] [ text message ]

                Nothing ->
                    text ""
            ]
        , button [ class "btn btn-secondary observation-match-apply", type_ "button", onClick ApplyObservationMatch, disabled state.loading ] [ text "Match files" ]
        , if not (List.isEmpty state.matchAppliedPaths) || not (String.isEmpty state.matchPathsInput) then
            button [ class "btn btn-secondary observation-match-clear", type_ "button", onClick ClearObservationMatch, disabled state.loading ] [ text "Clear match" ]

          else
            text ""
        , button [ id "observation-close-file-composer", class "btn btn-secondary", type_ "button", onClick CloseObservationFileComposer ] [ text "Close file composer" ]
        ]


retrievalFallbackLabel : Maybe String -> String
retrievalFallbackLabel reason =
    case reason of
        Just "disabled" -> "vector search disabled"
        Just "no_compatible_vectors" -> "no compatible vectors"
        Just "empty_query" -> "no content query"
        _ -> "vector search unavailable"


viewAppliedFilters : ObservationModel -> Html Msg
viewAppliedFilters state =
    div [ class "observation-applied-filters", attribute "role" "status" ]
        [ case filterValidationError state of
            Just message -> p [ id "observation-filter-validation", class "form-error", attribute "role" "alert" ] [ text message ]
            Nothing -> text ""
        , if state.requestMode /= ObservationMatchMode then
            case state.discovery.context of
                Just context -> p [class "form-help observation-retrieval-mode"] [text (if context.retrieval.mode == "hybrid" then "Hybrid content search" else if state.query == "" then "Showing matching subjects" else "Full-text search · " ++ retrievalFallbackLabel context.retrieval.reason)]
                Nothing -> text ""
          else text ""
        , case state.discovery.error of
            Just message -> div [class "observation-discovery-error", attribute "role" "alert"] [p [] [text message], button [class "btn btn-secondary", type_ "button", onClick RetryObservationResults] [text "Retry search"]]
            Nothing -> text ""
        , if filtersPending state then
            p [ class "form-help observation-filter-pending" ] [ text (if state.completedQuery == Nothing && List.isEmpty state.orderedIds && List.isEmpty state.facetKeys then "Editing filters." else "Editing filters. Showing the last completed results.") ]
          else if state.refreshPass /= Nothing then
            p [ class "form-help" ] [ text "Updating results. Showing the last completed results." ]
          else text ""
        , if state.refreshError /= Nothing then
            p [ class "form-help" ] [ text "Showing the last completed results." ]
          else text ""
        ]

viewModeNavigation : ObservationModel -> Html Msg
viewModeNavigation state =
    let
        modeButton mode active labelText =
            button
                [ classList
                    [ ( "filter-pill", True )
                    , ( "filter-pill-active", active )
                    , ( "observation-mode-button", True )
                    ]
                , type_ "button"
                , onClick (SetObservationBrowseMode mode)
                , attribute "aria-pressed"
                    (if active then
                        "true"

                     else
                        "false"
                    )
                ]
                [ text labelText ]

        headingText = "Observations"
    in
    section [ class "observation-mode-navigation", attribute "aria-labelledby" "observation-mode-heading" ]
        [ h2 [ id "observation-mode-heading", class "observation-mode-heading", tabindex -1 ] [ text headingText ]
        , div [ class "observation-mode-actions", attribute "role" "group", attribute "aria-label" "Observation browse mode" ]
            [ modeButton ObservationFacetMode (state.requestMode == ObservationFacetMode || state.requestMode == ObservationExactSubjectMode) "Subject Tree"
            , modeButton ObservationFlatMode (state.requestMode == ObservationFlatMode) "All Observations"
            , button
                [ id "observation-for-files", classList [ ( "filter-pill", True ), ( "filter-pill-active", state.requestMode == ObservationMatchMode ), ( "observation-mode-button", True ) ], type_ "button", onClick OpenObservationFileComposer
                , attribute "aria-pressed" (if state.requestMode == ObservationMatchMode then "true" else "false")
                , attribute "aria-expanded" (if state.fileComposerOpen then "true" else "false")
                , attribute "aria-controls" "observation-file-composer"
                ]
                [ text "Files" ]
            ]
        , if treeMode state.requestMode then
            p [class "observation-mode-announcement", attribute "aria-live" "polite"]
                [text (state.tree.totals |> Maybe.map (\(total, matches) -> if matches == total then String.fromInt total ++ (if total == 1 then " observation" else " observations") else String.fromInt matches ++ " matching observations · " ++ String.fromInt total ++ " in workspace") |> Maybe.withDefault (if Dict.get "" state.tree.branches |> Maybe.andThen .error |> (/=) Nothing then "Subject Tree unavailable" else "Loading Subject Tree…"))]
          else if hasAppliedCountFilter state then
            let counts = state.counts in
            p [ class "observation-mode-announcement", attribute "aria-live" "polite" ]
                [ text (if state.requestMode == ObservationMatchMode && List.isEmpty state.matchAppliedPaths then "Select Match files to load file results."
                    else if filtersPending state then
                    counts.value |> Maybe.map (\value -> "Last completed count: " ++ String.fromInt value.matchCount) |> Maybe.withDefault "Editing filters"
                    else if counts.current && counts.valueFingerprint == countFingerprint (Maybe.map .workspaceId counts.owner |> Maybe.withDefault "") state then
                    counts.value |> Maybe.map (\value -> String.fromInt value.matchCount ++ (if value.matchCount == 1 then " Observation matches" else " Observations match")) |> Maybe.withDefault "Observation match count unavailable"
                    else if counts.error /= Nothing then "Observation match count unavailable"
                    else "Counting matching Observations…")
                , if counts.error /= Nothing then button [ class "btn btn-sm", type_ "button", onClick RetryObservationCounts ] [ text "Retry counts" ] else text "" ]
          else text ""
        ]


viewList : TimestampDisplay.State -> Bool -> String -> ObservationModel -> Html Msg
viewList display canEdit workspaceId state =
    case state.requestMode of
        ObservationFacetMode ->
            viewTree display canEdit state

        ObservationMatchMode ->
            viewMatchResults display canEdit workspaceId state

        ObservationExactSubjectMode ->
            viewTree display canEdit state

        ObservationFlatMode ->
            viewObservationResults display canEdit workspaceId "Observations" "No observations found" state


viewObservationResults : TimestampDisplay.State -> Bool -> String -> String -> String -> ObservationModel -> Html Msg
viewObservationResults display canEdit workspaceId ariaLabel emptyHeading state =
    let
        observations =
            state.orderedIds |> List.filterMap (\observationId -> Dict.get observationId state.items)

        context =
            case state.requestMode of
                ObservationExactSubjectMode ->
                    state.selectedFacet
                        |> Maybe.map (\selectedFacet -> "exact:" ++ facetKey selectedFacet.subjectKind selectedFacet.subject)
                        |> Maybe.withDefault "exact:missing"

                _ ->
                    "flat"
    in
    div [ id "observation-results", class "entity-list observation-list", tabindex -1 ]
        [ viewStaleResultsNotice state
        , if state.loading && List.isEmpty observations then
            div [ class "loading-indicator observation-state observation-state-loading", attribute "role" "status", attribute "aria-live" "polite" ]
                [ text "Loading observations..." ]

          else
            text ""
        , case state.error of
            Just message ->
                viewRequestError "Unable to load observations" message (not (List.isEmpty observations)) state

            Nothing ->
                if not state.loading && state.refreshError == Nothing && List.isEmpty observations then
                    div [ class "empty-state observation-state observation-state-empty" ]
                        [ h3 [] [ text emptyHeading ]
                        , p [] [ text "Try clearing or changing the active provenance filters." ]
                        ]

                else
                    text ""
        , Lazy.lazy3 viewResultRows display canEdit state
        , if state.loading && not (List.isEmpty observations) then
            viewPageLoading state.expectedOffset "observations"
          else
            text ""
        , if state.hasMore then
            div [ class "observation-pagination" ]
                [ button [ class "btn btn-secondary observation-load-more", type_ "button", onClick LoadMoreObservations, disabled (not (canLoadMore workspaceId state)) ]
                    [ text
                        (if state.loading then
                            "Loading..."

                         else
                            "Load more"
                        )
                    ]
                ]

          else
            text ""
        ]


viewStaleResultsNotice : ObservationModel -> Html Msg
viewStaleResultsNotice state =
    case state.refreshError of
        Just message ->
            div [ class "observation-state observation-state-error", attribute "role" "status" ]
                [ text message, button [ class "btn btn-secondary observation-retry", type_ "button", onClick RetryObservationResults, disabled (state.loading || state.facetLoading) ] [ text "Retry refresh" ] ]
        Nothing -> text ""


viewRequestError : String -> String -> Bool -> ObservationModel -> Html Msg
viewRequestError heading message hasCached state =
    div
        [ classList [ ( "observation-state observation-state-error", True ), ( "empty-state", not hasCached ) ]
        , attribute "role" "alert"
        ]
        [ h3 [] [ text heading ]
        , p [] [ text message ]
        , if hasCached then
            p [] [ text "Previously loaded results remain available; the last request failed." ]
          else
            text ""
        , button
            [ class "btn btn-secondary observation-retry", type_ "button", onClick RetryObservationResults
            , disabled ((if state.requestMode == ObservationFacetMode then state.facetLoading else state.loading) || state.failedRequest == Nothing)
            ] [ text "Retry results" ]
        ]


viewPageLoading : Maybe Int -> String -> Html Msg
viewPageLoading offset labelText =
    p [ class "observation-loading-page", attribute "role" "status", attribute "aria-live" "polite" ]
        [ text
            (if offset == Just 0 then
                "Refreshing " ++ labelText ++ "..."
             else
                "Loading more " ++ labelText ++ "..."
            )
        ]


viewFacetCatalogue : TimestampDisplay.State -> Bool -> String -> ObservationModel -> Html Msg
viewFacetCatalogue display canEdit workspaceId state =
    let
        facets =
            state.facetKeys |> List.filterMap (\key -> Dict.get key state.facets)
    in
    div [ id "observation-results", class "entity-list observation-list observation-facet-list", tabindex -1 ]
        [ viewStaleResultsNotice state
        , if state.facetLoading && List.isEmpty facets then
            div [ class "loading-indicator observation-state observation-state-loading", attribute "role" "status", attribute "aria-live" "polite" ] [ text "Loading shared subjects..." ]

          else
            text ""
        , case state.facetError of
            Just message ->
                viewRequestError "Unable to load shared subjects" message (not (List.isEmpty facets)) state

            Nothing ->
                if not state.facetLoading && state.refreshError == Nothing && List.isEmpty facets then
                    div [ class "empty-state observation-state observation-state-empty" ]
                        [ h3 [] [ text "No subjects found" ]
                        , p [] [ text "Try clearing or changing the shared search, kind, or Git SHA filters." ]
                        ]

                else
                    text ""
        , Lazy.lazy3 viewResultRows display canEdit state
        , if state.facetLoading && not (List.isEmpty facets) then
            viewPageLoading state.facetExpectedOffset "shared subjects"
          else
            text ""
        , if state.facetHasMore then
            div [ class "observation-pagination" ]
                [ button [ class "btn btn-secondary observation-facet-load-more", type_ "button", onClick LoadMoreObservationFacets, disabled (not (canLoadMoreFacets workspaceId state)) ]
                    [ text
                        (if state.facetLoading then
                            "Loading..."

                         else
                            "Load more subjects"
                        )
                    ]
                ]

          else
            text ""
        ]


viewFacet : TimestampDisplay.State -> Api.ObservationSubjectFacet -> Html Msg
viewFacet display facet =
    div [ class "card observation-facet" ]
        [ div [ class "card-header observation-card-header" ]
            [ span [ class "entity-type-label observation-kind" ] [ text (subjectKindLabel facet.subjectKind) ]
            , Helpers.copyableValue "observation-subject" "repository subject" facet.subject facet.subject ]
        , button [ id ("observation-facet-" ++ domToken (facetKey facet.subjectKind facet.subject)), class "observation-facet-card", type_ "button"
            , onClick (SelectObservationFacet facet.subjectKind facet.subject)
            , attribute "aria-label" ("Open " ++ subjectKindLabel facet.subjectKind ++ " subject " ++ facet.subject ++ " with " ++ String.fromInt facet.observationCount ++ " observations") ]
            [ strong [] [ text (String.fromInt facet.observationCount ++ " observations") ]
            , span [ class "card-meta" ] [ text ("Latest update: " ++ TimestampDisplay.lookup display facet.latestUpdatedAt) ] ] ]


viewMatchResults : TimestampDisplay.State -> Bool -> String -> ObservationModel -> Html Msg
viewMatchResults display canEdit workspaceId state =
    let
        paths =
            state.matchAppliedPaths

        hasAnyGroups =
            not (List.isEmpty state.orderedIds)
    in
    div [ id "observation-results", class "entity-list observation-list observation-match-results", tabindex -1 ]
        [ viewStaleResultsNotice state
        , if state.loading && List.isEmpty state.orderedIds then
            div [ class "loading-indicator observation-state observation-state-loading", attribute "role" "status", attribute "aria-live" "polite" ] [ text "Matching repository files..." ]

          else
            text ""
        , case state.error of
            Just message ->
                viewRequestError "Unable to match repository files" message (not (List.isEmpty state.orderedIds)) state

            Nothing ->
                div []
                    ((if not state.loading && not hasAnyGroups then
                        [ div [ class "empty-state observation-state observation-state-empty" ]
                            [ h3 [] [ text "No matching observations" ]
                            , p [] [ text "Every supplied path is shown below. Try different paths or clear the match." ]
                            ]
                        ]

                      else
                        []
                     )
                    )
        , if state.error == Nothing || not (List.isEmpty state.orderedIds) then
            Lazy.lazy3 viewResultRows display canEdit state
          else
            text ""
        , if state.loading && not (List.isEmpty state.orderedIds) then
            viewPageLoading state.expectedOffset "path matches"

          else
            text ""
        , if state.hasMore then
            div [ class "observation-pagination" ]
                [ button [ class "btn btn-secondary observation-load-more", type_ "button", onClick LoadMoreObservations, disabled (not (canLoadMore workspaceId state)) ]
                    [ text
                        (if state.loading then
                            "Loading..."

                         else
                            "Load more observations"
                        )
                    ]
                , span [ class "form-help" ] [ text "Counts in path groups are loaded observations, not server totals." ]
                ]

          else
            text ""
        ]


viewObservationRow : TimestampDisplay.State -> Bool -> Maybe String -> ObservationModel -> String -> Api.Observation -> Html Msg
viewObservationRow display canEdit owner state context observation =
    let
        cardId = observationCardDomId context observation.id
        expanded = owner == Just cardId
    in
    div [ class "card observation-result", attribute "data-observation-id" observation.id, attribute "data-observation-context-key" context ]
        [ viewObservationHeader expanded cardId observation
        , if expanded then
            viewDetail display canEdit state
          else
            div [ id (cardId ++ "-body"), class "observation-folded-content" ]
                [ div [ class "card-body observation-summary" ] [ text (plainTextExcerpt 240 observation.content) ]
                , div [ class "card-meta-group observation-card-meta observation-card-footer" ]
                    [ dl [ class "observation-detail-meta" ] [ viewSubjects cardId observation.id state.expandedSubjects observation.subjects ]
                    , div [ class "card-meta-row" ]
                        [ viewRevisionSummary display observation
                        , span [ class "card-meta observation-updated" ] [ text ("Content updated: " ++ TimestampDisplay.lookup display observation.updatedAt) ]
                        ]
                    ]
                ]
        ]


viewObservationHeader : Bool -> String -> Api.Observation -> Html Msg
viewObservationHeader expanded cardId observation =
    div [ class "card-header observation-card-header tree-toggle-row" ]
        [ observationToggle expanded cardId
            (subjectKindLabel observation.subjectKind ++ " observation: " ++ plainTextExcerpt 96 observation.subject)
            (if expanded then ReturnObservationResults else SelectObservationFrom observation.id cardId)
        , span [ class "entity-type-label observation-kind" ] [ text (subjectKindLabel observation.subjectKind) ]
        , Helpers.copyableValue "observation-subject" "repository subject" observation.subject (plainTextExcerpt 96 observation.subject)
        , if List.length observation.subjects > 1 then
            span [ class "card-meta observation-subject-count" ] [ text ("+" ++ String.fromInt (List.length observation.subjects - 1)) ]
          else text ""
        ]


observationToggle : Bool -> String -> String -> Msg -> Html Msg
observationToggle expanded cardId name message =
    button
        ([ id cardId
         , classList [ ( "btn-extras-toggle", True ), ( "observation-disclosure", True ), ( "observation-card", cardId /= "observation-detached-toggle" ), ( "observation-card-selected", expanded ) ]
         , type_ "button"
         , attribute "aria-label" (plainTextExcerpt 180 ((if expanded then "Collapse " else "Open ") ++ name))
         , title (plainTextExcerpt 180 ((if expanded then "Collapse " else "Open ") ++ name))
         , attribute "aria-expanded" (if expanded then "true" else "false")
         , attribute "aria-current" (if expanded then "true" else "false")
         , attribute "aria-controls" (if expanded then "observation-detail" else cardId ++ "-body")
         , onClick message
         ] ++ (if expanded then [ attribute "data-observation-detail-anchor" "true" ] else []))
        [ CardSections.disclosureIcon expanded ]


{-| Only one exact repeated occurrence owns detail. Linked selection chooses the
first known occurrence; absent/collapsed membership gets a detached inline card.
-}
selectedOwner : ObservationModel -> Maybe String
selectedOwner state =
    if state.viewport.stamp.workspace == "" && Array.isEmpty state.resultRows then
        let rows = projectResultRows state in
        resolveSelectedOwner rows (ObservationViewport.rebuild state.viewport.stamp (Array.toList rows |> List.map resultRowKey) state.viewport) state
    else state.inlineOwner


resolveSelectedOwner : Array.Array ObservationResultRow -> ObservationViewport.State -> ObservationModel -> Maybe String
resolveSelectedOwner rows viewport state =
    case state.selectedId of
        Nothing -> Nothing
        Just selected ->
            case state.detailReturnTarget of
                Just origin ->
                    Dict.get origin viewport.index.positions |> Maybe.andThen (\position ->
                        Array.get position rows |> Maybe.andThen (\row -> case rowContent row of
                            ObservationCardRow _ observation -> if observation.id == selected then Just origin else Nothing
                            _ -> Nothing))
                Nothing ->
                    Array.toList rows |> List.filterMap (\row -> case rowContent row of
                        ObservationCardRow context observation -> if observation.id == selected then Just (observationCardDomId context observation.id) else Nothing
                        _ -> Nothing) |> List.head


viewSearchDetail : Model -> Html Msg
viewSearchDetail model =
    viewSearchDetailState model.timestamps model.observations


viewSearchDetailState : TimestampDisplay.State -> ObservationModel -> Html Msg
viewSearchDetailState display state =
    div [ id "search-observation-detail", class "card observation-detached search-observation-detail", tabindex -1, attribute "role" "region", attribute "aria-label" "Observation detail", attribute "data-observation-detached" "true" ]
        [ p [ class "form-help" ] [ text "Opened from entity search. Saved Observation filters and loaded counts are unchanged." ]
        , viewDetail display False state ]


viewDetail : TimestampDisplay.State -> Bool -> ObservationModel -> Html Msg
viewDetail display canEdit state =
    case state.selectedId of
        Nothing ->
            text ""

        Just _ ->
            section [ id "observation-detail", class "observation-detail" ]
                [ if state.detailLoading then
                    div [ class "loading-indicator observation-state observation-detail-state", attribute "role" "status", attribute "aria-live" "polite" ] [ text "Loading detail..." ]

                  else
                    text ""
                , case state.detailError of
                    Just message ->
                        div [ class "observation-state observation-state-error observation-detail-state", attribute "role" "alert" ]
                            [ text message
                            , button [ class "btn btn-secondary", type_ "button", onClick RetryObservationDetail, disabled state.detailLoading ] [ text "Retry detail" ]
                            ]

                    Nothing ->
                        text ""
                , case state.selectedDetail of
                    Just observation ->
                        article [ class "card observation-detail-card" ]
                            [ CardSections.section "Content" [viewFullObservationContent observation]
                            , if canEdit && Maybe.map .observationId state.edit == Just observation.id then
                                section [ class "observation-retained-editor", attribute "aria-label" "Retained observation draft" ]
                                    [ h4 [] [ text "Retained draft" ]
                                    , viewDetailContent canEdit observation state.edit
                                    ]
                              else text ""
                            , div [ class "observation-card-footer" ]
                                [ viewMatchContext observation.provenanceMatch
                                , CardSections.section "Provenance and metadata" [dl [ class "observation-detail-meta" ]
                                    [ viewDetailMeta "Workspace ID" observation.workspaceId "observation-detail-workspace"
                                    , viewSubjects (Maybe.withDefault "observation-detached" (selectedOwner state)) observation.id state.expandedSubjects observation.subjects
                                    , viewCurrentProvenance display observation state
                                    , if boundCurrentProvenance observation /= Nothing then viewProvenanceRevision observation.gitSha else text ""
                                    , viewDetailMeta "Created" (TimestampDisplay.lookup display observation.createdAt) ""
                                    , viewDetailMeta "Content updated" (TimestampDisplay.lookup display observation.updatedAt) ""
                                    ]]
                                , if canEdit && state.edit == Nothing then
                                    div [ class "observation-detail-actions" ]
                                        [ button [ id "observation-review", class "btn btn-secondary", type_ "button", onClick StartObservationEdit ] [ text "Review / re-audit" ]
                                        , button [ id "observation-delete", class "btn btn-danger", type_ "button", onClick OpenObservationDelete ] [ text "Delete" ] ]
                                  else text ""
                                ]
                            ]

                    Nothing ->
                        text ""
                ]


viewDetailContent : Bool -> Api.Observation -> Maybe ObservationEditState -> Html Msg
viewDetailContent canEdit observation maybeEdit =
    case maybeEdit of
        Just edit ->
            if canEdit && edit.observationId == observation.id then
                let
                    validationError =
                        editValidationError edit
                in
                div [ class "observation-edit-form" ]
                    [ label [ class "filter-label", for "observation-edit-content" ] [ text "Observation content" ]
                    , textarea
                        [ id "observation-edit-content"
                        , class "form-input observation-edit-content"
                        , value edit.draft
                        , onInput SetObservationDraft
                        , disabled edit.saving
                        , attribute "rows" "10"
                        , attribute "aria-describedby" "observation-edit-help observation-edit-status"
                        ]
                        []
                    , label [ class "filter-label", for "observation-reviewed-sha" ] [ text "Reviewed Git SHA" ]
                    , input [ id "observation-reviewed-sha", class "form-input", value edit.reviewedGitShaDraft, onInput SetObservationReviewedGitSha, disabled edit.saving, attribute "aria-describedby" "observation-edit-help observation-edit-status" ] []
                    , p [ id "observation-edit-help", class "form-help" ] [ text "Assert the full lowercase revision you reviewed. Every accepted review records a new assertion, including unchanged content. Creation revision, workspace and ordered subjects remain immutable." ]
                    , case validationError of
                        Just message ->
                            p [ id "observation-edit-status", class "form-error", attribute "role" "alert" ] [ text message ]

                        Nothing ->
                            if edit.conflict then
                                div [ id "observation-edit-status", class "observation-edit-conflict", attribute "role" "alert" ]
                                    [ p []
                                        [ text
                                            (if edit.activeCanonicalRequest /= Nothing then
                                                "Checking the current version before choosing how to continue. Your draft is preserved."
                                             else
                                                Maybe.withDefault
                                                    "This observation changed elsewhere. Your draft is preserved; choose how to continue before saving."
                                                    edit.error
                                            )
                                        ]
                                    , div [ class "observation-conflict-actions" ]
                                        [ button [ class "btn btn-secondary", type_ "button", onClick ReloadObservationEdit, disabled (edit.saving || edit.activeCanonicalRequest /= Nothing) ] [ text (if edit.canonicalProvisional then "Use retained version" else "Use latest version") ]
                                        , button [ class "btn btn-secondary", type_ "button", onClick RebaseObservationEdit, disabled (edit.saving || edit.activeCanonicalRequest /= Nothing) ] [ text "Keep my draft" ]
                                        ]
                                    ]

                            else
                                case edit.error of
                                    Just message ->
                                        p [ id "observation-edit-status", class "form-error", attribute "role" "alert" ] [ text message ]

                                    Nothing ->
                                        span [ id "observation-edit-status", attribute "aria-live" "polite" ]
                                            [ text
                                                (if edit.saving then
                                                    "Saving observation..."

                                                 else
                                                    ""
                                                )
                                            ]
                    , div [ class "observation-edit-actions" ]
                        [ button
                            [ class "btn btn-primary"
                            , type_ "button"
                            , onClick SaveObservationEdit
                            , disabled (edit.saving || edit.conflict || validationError /= Nothing)
                            ]
                            [ text
                                (if edit.saving then
                                    "Saving..."

                                 else
                                    (if edit.draft == edit.baseContent then "Record re-audit" else "Save reviewed content")
                                )
                            ]
                        , button [ class "btn btn-secondary", type_ "button", onClick CancelObservationEdit, disabled edit.saving ] [ text "Cancel" ]
                        ]
                    ]

            else
                viewFullObservationContent observation

        Nothing ->
            viewFullObservationContent observation


{-| Values above 16 KiB use a native read-only reader instead of a document-sized
paragraph. Its exact full value stays available for keyboard scrolling/selection.
-}
viewFullObservationContent : Api.Observation -> Html Msg
viewFullObservationContent observation =
    if utf8Bytes observation.content > 16384 then
        div [ class "observation-full-content" ]
            [ label [ class "filter-label", for "observation-content-reader" ] [ text "Full observation content (read only)" ]
            , textarea
                [ id "observation-content-reader", class "form-input observation-detail-content observation-detail-reader"
                , readonly True, value observation.content, attribute "rows" "10"
                , attribute "aria-describedby" "observation-content-reader-help"
                ] []
            , p [ id "observation-content-reader-help", class "form-help" ] [ text "Scroll within this read-only field to read the full content. Use Copy full content to preserve stored line endings." ]
            , button [ class "btn btn-secondary observation-content-copy", type_ "button", onClick (CopyObservationContent observation.id) ] [ text "Copy full content" ]
            ]

    else
        p [ class "observation-detail-content" ] [ text observation.content ]


viewDeleteConfirmation : Bool -> ObservationModel -> Html Msg
viewDeleteConfirmation canEdit state =
    case
        if canEdit then
            state.deleteConfirmation

        else
            Nothing
    of
        Nothing ->
            text ""

        Just confirmation ->
            div
                [ class "modal-overlay observation-delete-overlay"
                , onClick CancelObservationDelete
                ]
                [ div
                    [ id "observation-delete-dialog"
                    , class "modal delete-confirm-modal observation-delete-confirm"
                    , attribute "role" "dialog"
                    , attribute "aria-modal" "true"
                    , attribute "aria-labelledby" "observation-delete-title"
                    , attribute "aria-describedby" "observation-delete-description"
                    , tabindex -1
                    , deleteDialogKeyDown
                    , stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
                    ]
                    [ h3 [ id "observation-delete-title", class "modal-title" ] [ text "Delete observation permanently?" ]
                    , p [ id "observation-delete-description", class "delete-confirm-desc" ]
                        [ text "Delete “"
                        , strong [] [ text (contentPreview confirmation.targetContent) ]
                        , text "”? This permanently removes the observation and cannot be undone."
                        ]
                    , case confirmation.error of
                        Just message ->
                            p [ class "form-error observation-delete-error", attribute "role" "alert" ] [ text message ]

                        Nothing ->
                            span [ attribute "aria-live" "polite" ]
                                [ text
                                    (if confirmation.deleting then
                                        "Deleting observation..."

                                     else
                                        ""
                                    )
                                ]
                    , div [ class "modal-actions" ]
                        [ button [ id "observation-delete-confirm", class "btn btn-danger", type_ "button", onClick ConfirmObservationDelete, disabled confirmation.deleting ]
                            [ text
                                (if confirmation.deleting then
                                    "Deleting..."

                                 else
                                    "Delete permanently"
                                )
                            ]
                        , button [ id "observation-delete-cancel", class "btn btn-secondary", type_ "button", onClick CancelObservationDelete, disabled confirmation.deleting ] [ text "Cancel" ]
                        ]
                    ]
                ]


deleteDialogKeyDown : Attribute Msg
deleteDialogKeyDown =
    let
        targetIdDecoder =
            Decode.oneOf
                [ Decode.at [ "target", "id" ] Decode.string
                , Decode.succeed ""
                ]
    in
    custom "keydown"
        (Decode.map3
            (\key shiftKey targetId ->
                { message = ObservationDeleteDialogKeyDown key shiftKey targetId
                , stopPropagation = key == "Tab" || key == "Escape"
                , preventDefault = key == "Tab" || key == "Escape"
                }
            )
            (Decode.field "key" Decode.string)
            (Decode.oneOf [ Decode.field "shiftKey" Decode.bool, Decode.succeed False ])
            targetIdDecoder
        )


contentPreview : String -> String
contentPreview content =
    let
        singleLine =
            content |> String.words |> String.join " " |> String.left 80
    in
    if String.length singleLine < String.length (content |> String.words |> String.join " ") then
        singleLine ++ "…"

    else
        singleLine


viewDetailMeta : String -> String -> String -> Html Msg
viewDetailMeta labelText valueText valueClass =
    div [ class "observation-detail-meta-row" ]
        [ dt [ class "observation-detail-meta-label" ] [ text labelText ]
        , dd [ classList [ ( "observation-detail-meta-value", True ), ( valueClass, not (String.isEmpty valueClass) ) ] ] [ if valueClass == "observation-detail-workspace" then Helpers.copyableValue "" "workspace ID" valueText valueText else text valueText ]
        ]


viewProvenanceRevision : String -> Html Msg
viewProvenanceRevision gitSha =
    div [ class "observation-detail-meta-row" ]
        [ dt [ class "observation-detail-meta-label" ] [ text "Original creation revision (Git SHA)" ]
        , dd [ class "observation-detail-meta-value observation-detail-sha" ]
            [ Helpers.copyableValue "" "provenance revision" gitSha gitSha ]
        ]


viewSubjects : String -> String -> Dict.Dict String Bool -> List Api.ObservationSubject -> Html Msg
viewSubjects occurrence observationId expanded subjects =
    let
        open = Dict.get observationId expanded |> Maybe.withDefault False
        bodyId = "observation-subjects-" ++ occurrence ++ "-" ++ domToken observationId
    in
    div [ class "observation-detail-meta-row observation-detail-subjects" ]
        [ dt [ class "observation-detail-meta-label" ]
            [ button [ type_ "button", class "tree-toggle observation-subjects-toggle", onClick (ToggleObservationSubjects observationId)
                , attribute "aria-expanded" (if open then "true" else "false"), attribute "aria-controls" bodyId
                , attribute "aria-label" ((if open then "Hide" else "Show") ++ " ordered subjects") ] [ text (if open then "▼" else "▶") ]
            , text ("Subjects (" ++ String.fromInt (List.length subjects) ++ ")") ]
        , dd [ id bodyId, class "observation-detail-meta-value observation-detail-subject", hidden (not open) ]
            (if open then List.map viewSubject subjects else [])
        ]


viewSubject : Api.ObservationSubject -> Html Msg
viewSubject subject =
    div [ class "observation-subject-row" ]
        [ span [ class "entity-type-label observation-kind" ] [ text (subjectKindLabel subject.subjectKind) ]
        , Helpers.copyableValue "observation-subject-copy" "repository subject" subject.subject subject.subject
        ]


subjectKindLabel : Api.SubjectKind -> String
subjectKindLabel subjectKind =
    case subjectKind of
        Api.SubjectFile ->
            "File"

        Api.SubjectGlob ->
            "Glob"


armTreeRoute : Model -> Model
armTreeRoute model =
    let state = model.observations
        select subject = ObservationTree.select subject.subject (Just subject) state.tree
        known = case state.selectedFacet of
            Just subject -> Just subject
            Nothing -> currentSelectedObservation state |> Maybe.map (\observation -> {subjectKind = observation.subjectKind, subject = observation.subject})
    in
    if not (treeMode state.requestMode) then {model | observations = {state | treeSelectionPending = False}}
    else if state.selectedId == Nothing then
        case state.selectedFacet of
            Just subject -> {model | observations = {state | tree = ObservationTree.revealPath subject (select subject), treeSelectionPending = False}}
            Nothing -> {model | observations = {state | treeSelectionPending = False}}
    else
        case known of
            Just subject -> {model | observations = {state | tree = state.selectedId |> Maybe.map (\identity -> ObservationTree.reveal subject identity (select subject)) |> Maybe.withDefault (select subject), treeSelectionPending = False}}
            Nothing -> let tree = state.tree in {model | observations = {state | tree = {tree | selectedPath = Nothing}, treeSelectionPending = True}}


restoreRouteResults : Model -> ( Model, Cmd Msg )
restoreRouteResults incoming =
    let model = armTreeRoute (prepareForegroundResults incoming) in
    (if filterValidationError model.observations /= Nothing then ( model, Cmd.none ) else
        case model.observations.requestMode of
            ObservationFacetMode -> reloadFacets model
            ObservationExactSubjectMode -> startTree False model
            ObservationMatchMode -> reloadAppliedMatch model
            mode -> reloadResultMode mode model)
        |> migrateLegacySubjectLink


prepareForegroundResults : Model -> Model
prepareForegroundResults model =
    if model.observations.discovery.failedCount then clearDiscoveryError model else model


resumeForegroundResults : Model -> ( Model, Cmd Msg )
resumeForegroundResults model =
    if model.activeTab /= ObservationsTab || not model.observations.discovery.failedCount then
        ( model, Cmd.none )
    else if model.observations.discovery.afterBootstrap /= Nothing then
        retryDiscoveryCounts model
    else
        restoreRouteResults model


migrateLegacySubjectLink : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
migrateLegacySubjectLink ( model, command ) =
    let
        legacy = model.url.fragment |> Maybe.withDefault "" |> String.split "&"
            |> List.filter (String.startsWith "oq=") |> List.head |> Maybe.map (String.dropLeft 3)
            |> Maybe.andThen Url.percentDecode |> Maybe.andThen (Decode.decodeString (Decode.index 0 Decode.string) >> Result.toMaybe)
    in
    if legacy == Just "facets" && (Helpers.observationUrlContext model.url).notice == Nothing then
        let ( linked, linkCmd ) = Helpers.writeObservationHistory False model in
        ( linked, Cmd.batch [ command, linkCmd ] )
    else ( model, command )


bootstrapObservation : Int -> Model -> ( Model, Cmd Msg )
bootstrapObservation token model =
    bootstrapObservationRaw token (prepareForegroundResults model) |> migrateLegacySubjectLink


bootstrapObservationRaw : Int -> Model -> ( Model, Cmd Msg )
bootstrapObservationRaw token incoming =
    let model = armTreeRoute incoming in
    if filterValidationError model.observations /= Nothing then ( model, Cmd.none ) else
    case repositoryWorkspaceId model of
        Just workspaceId ->
            case model.observations.requestMode of
                ObservationFacetMode -> reloadFacets model
                ObservationExactSubjectMode -> startTree False model
                ObservationMatchMode -> reloadAppliedMatch model
                mode ->
                    let
                        state = startResultReload mode model.sessionRequestEpoch workspaceId model.observations
                    in
                    requestDiscovery (DiscoveryFlat (Just token) state.requestGeneration state.queryFingerprint 0) { model | observations = state }
        Nothing -> ( model, Cmd.none )


resultViewFingerprint : Int -> String -> ObservationModel -> String
resultViewFingerprint epoch workspaceId state =
    if state.requestMode == ObservationMatchMode then matchFingerprint epoch workspaceId state
    else resultFingerprint state.requestMode epoch workspaceId state


reviewedSha : Api.Observation -> String
reviewedSha observation =
    observation.currentProvenance |> Maybe.map .reviewedGitSha |> Maybe.withDefault observation.gitSha


editValidationError : ObservationEditState -> Maybe String
editValidationError edit =
    case observationContentError edit.draft of
        Just message -> Just message
        Nothing ->
            if String.length edit.reviewedGitShaDraft == 40 && String.all (\character -> Char.isDigit character || String.contains (String.fromChar character) "abcdef") edit.reviewedGitShaDraft then Nothing
            else Just "Reviewed Git SHA must be exactly 40 lowercase hexadecimal characters."


currentHistory : Maybe Api.Observation -> Maybe ObservationHistoryState -> Maybe ObservationHistoryState
currentHistory selected history =
    Maybe.andThen (\saved -> if Maybe.map (\observation -> observation.id == saved.observationId && observation.workspaceId == saved.workspaceId && observation.latestSequence == saved.head) selected == Just True then Just saved else Nothing) history


historyResponseMatches : ObservationHistoryRequest -> Model -> Bool
historyResponseMatches request model =
    Permissions.canReadCurrentWorkspace model
        && repositoryWorkspaceId model == Just request.workspaceId
        && model.sessionRequestEpoch == request.sessionEpoch
        && model.observations.selectedId == Just request.observationId
        && Maybe.map .latestSequence model.observations.selectedDetail == Just request.head
        && Maybe.andThen .active model.observations.history == Just request


loadHistory : Model -> ( Model, Cmd Msg )
loadHistory model =
    case currentSelectedObservation model.observations of
        Nothing -> ( model, Cmd.none )
        Just observation ->
            if not (Permissions.canReadCurrentWorkspace model) || repositoryWorkspaceId model /= Just observation.workspaceId then ( model, Cmd.none )
            else
                let
                    previous = currentHistory (Just observation) model.observations.history
                    saved = Maybe.withDefault { workspaceId = observation.workspaceId, observationId = observation.id, sessionEpoch = model.sessionRequestEpoch, head = observation.latestSequence, items = [], hasMore = True, nextOffset = 0, loading = False, error = Nothing, active = Nothing } previous
                    request = { workspaceId = observation.workspaceId, observationId = observation.id, sessionEpoch = model.sessionRequestEpoch, head = observation.latestSequence, token = model.observations.nextHistoryRequestToken, offset = saved.nextOffset }
                in
                if not model.observations.historyExpanded || saved.loading || saved.error /= Nothing || not saved.hasMore || saved.nextOffset > 100000 then ( model, Cmd.none )
                else
                    ( updateObservation (\state -> { state | history = Just { saved | loading = True, error = Nothing, active = Just request }, nextHistoryRequestToken = request.token + 1 }) model
                    , Api.fetchObservationHistory model.flags.apiUrl observation.id request.offset (GotObservationHistory request)
                    )


receiveHistory : ObservationHistoryRequest -> Result Http.Error (Api.PaginatedResult Api.ObservationRevision) -> Model -> ( Model, Cmd Msg )
receiveHistory request result model =
    if not (historyResponseMatches request model) then ( model, Cmd.none )
    else
        let
            finish transform = ( updateObservation (\state -> { state | history = Maybe.map (\saved -> transform { saved | loading = False, active = Nothing }) state.history }) model, Cmd.none )
            failure message = finish (\saved -> { saved | error = Just message })
        in
        case result of
            Err _ -> failure "Revision history could not be loaded. Retry this page."
            Ok page ->
                let
                    sequences = List.map (.provenance >> .sequence) page.items
                    expected = List.range (Basics.max 1 (request.head - request.offset - List.length page.items + 1)) (request.head - request.offset) |> List.reverse
                in
                if List.length page.items /= Basics.min 25 (Basics.max 0 (request.head - request.offset)) || page.hasMore /= (request.offset + List.length page.items < request.head) || (page.hasMore && List.length page.items /= 25) || sequences /= expected || List.any (\event -> event.observationId /= request.observationId) page.items then
                    failure "History changed or returned an inconsistent page. Refresh the Observation before retrying."
                else
                    finish (\saved -> { saved | items = saved.items ++ page.items, nextOffset = saved.nextOffset + List.length page.items, hasMore = page.hasMore, error = if page.hasMore && saved.nextOffset + List.length page.items > 100000 then Just "History is incomplete at the bounded page limit." else Nothing })
                        |> refreshHistory


refreshHistory : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
refreshHistory ( model, command ) =
    let
        ( updated, historyCommand ) = loadHistory model
    in
    ( updated, Cmd.batch [ command, historyCommand ] )


boundCurrentProvenance : Api.Observation -> Maybe Api.ObservationProvenance
boundCurrentProvenance observation =
    observation.currentProvenance |> Maybe.andThen (\provenance ->
        if provenance.eventKind /= "legacy_creation" && provenance.contentVersion /= Nothing && provenance.contentDigest /= Nothing then Just provenance else Nothing)


revisionDisplay : Api.Observation -> { label : String, sha : String, recordedAt : String }
revisionDisplay observation =
    case boundCurrentProvenance observation of
        Just provenance -> { label = "Current reviewed revision", sha = provenance.reviewedGitSha, recordedAt = provenance.recordedAt }
        Nothing -> { label = "Creation revision", sha = observation.gitSha, recordedAt = observation.createdAt }


viewRevisionSummary : TimestampDisplay.State -> Api.Observation -> Html Msg
viewRevisionSummary display observation =
    let revision = revisionDisplay observation in
    span [ class "card-meta observation-sha" ]
        [ span [] [ text (revision.label ++ ":") ]
        , Helpers.copyableValue "" (String.toLower revision.label) revision.sha (String.left 12 revision.sha ++ "…")
        , span [ class "observation-history-time" ] [ text (TimestampDisplay.lookup display revision.recordedAt) ]
        , span [ class "observation-actor" ] [ viewObservationActor observation ]
        ]


viewCurrentProvenance : TimestampDisplay.State -> Api.Observation -> ObservationModel -> Html Msg
viewCurrentProvenance display observation state =
    let revision = revisionDisplay observation in
    div [ class ("observation-detail-meta-row observation-current-provenance" ++ (if boundCurrentProvenance observation == Nothing then " observation-detail-sha" else "")) ]
        [ dt [ class "observation-detail-meta-label" ] [ text revision.label ]
        , dd [ class "observation-detail-meta-value" ]
            [ span [ class "observation-revision-value" ]
                [ Helpers.copyableValue "" (String.toLower revision.label) revision.sha revision.sha
                , span [ class "observation-history-time" ] [ text (TimestampDisplay.lookup display revision.recordedAt) ]
                ]
            , span [ class "observation-actor" ] [ viewObservationActor observation ]
            , viewHistory display observation state
            ]
        ]


viewMatchContext : Maybe Api.ObservationProvenanceMatch -> Html Msg
viewMatchContext matched =
    case matched of
        Nothing -> text ""
        Just context ->
            p [ class "form-help observation-provenance-match" ]
                ([ text "Matched current Observation by: " ]
                 ++ List.filterMap (\( labelText, sha ) -> Maybe.map (\value -> span [] [ text (labelText ++ " "), Helpers.copyableValue "" labelText value value, text " " ]) sha)
                    [ ( "original creation claim", context.originalGitSha ), ( "current reviewed assertion", context.currentGitSha ), ( "recorded history claim", context.historyGitSha ) ]
                 ++ [ text "History may include a migrated unbound creation claim; prior content is not reconstructed." ])


viewHistory : TimestampDisplay.State -> Api.Observation -> ObservationModel -> Html Msg
viewHistory display observation state =
    let
        saved = currentHistory (Just observation) state.history
        entry event =
            li [ class "observation-history-entry" ]
                [ span [ class "observation-revision-value" ]
                    [ Helpers.copyableValue "" "recorded revision" event.provenance.reviewedGitSha event.provenance.reviewedGitSha
                    , span [ class "observation-history-time" ] [ text (TimestampDisplay.lookup display event.provenance.recordedAt) ]
                    ]
                , span [ class "observation-actor" ] [ ActorAttribution.viewInline event.provenance ]
                ]
    in
    section [ class "observation-history", attribute "aria-label" "Revision history" ]
        [ button [ id "observation-history-toggle", type_ "button", class "observation-history-toggle", onClick ToggleObservationHistory, attribute "aria-expanded" (if state.historyExpanded then "true" else "false") ] [ text (if state.historyExpanded then "− History" else "+ History") ]
        , if state.historyExpanded then
            div [ class "observation-history-content" ]
                [ saved |> Maybe.map (\history -> ol [] (List.map entry history.items)) |> Maybe.withDefault (text "")
                , if saved |> Maybe.map .loading |> Maybe.withDefault False then span [ class "form-help", attribute "role" "status" ] [ text "Loading…" ] else text ""
                , saved |> Maybe.andThen .error |> Maybe.map (\message -> div [ class "form-error", attribute "role" "alert" ]
                    [ text message
                    , if saved |> Maybe.map (\history -> history.nextOffset <= 100000) |> Maybe.withDefault False then button [ id "observation-history-load", type_ "button", class "observation-history-toggle", onClick LoadObservationHistory ] [ text "Retry" ] else text ""
                    ]) |> Maybe.withDefault (text "")
                ]
          else text ""
        ]

viewObservationActor : Api.Observation -> Html Msg
viewObservationActor observation =
    observation.currentProvenance |> Maybe.map ActorAttribution.viewInline |> Maybe.withDefault (ActorAttribution.viewInline ActorAttribution.empty)
