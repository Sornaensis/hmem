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
    , restorePendingPreferences
    , bootstrapObservation
    , refuseContextExit
    , reload
    , removeObservation
    , selectObservation
    , selectionForTab
    , startReload
    , startReloadForSession
    , update
    , viewObservations
    , viewObservationsState
    , viewObservationsStateWithPermission
    , viewRetainedDraft
    , refreshViewport
    , updateViewport
    , projectResultRows
    )

import Api
import Array
import Char
import Dict
import Helpers exposing (beginTrackedMutation, focusElement, formatDate, formatObservationTimestamp, plainTextExcerpt, replaceFragment)
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
import ObservationViewport
import ObservationPreferences as Preferences
import Ports exposing (copyToClipboard)
import Set
import Toast exposing (addToast)
import Types exposing (..)
import Url


init : ObservationModel
init =
    { items = Dict.empty
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
    , preferenceEntryHistory = Nothing
    , preferenceTouch = 0
    , nextPreferenceRequest = 1
    , error = Nothing
    , query = ""
    , subjectKind = Nothing
    , subject = ""
    , selectedFacet = Nothing
    , gitSha = ""
    , currentGitSha = ""
    , historyGitSha = ""
    , requestMode = ObservationFlatMode
    , fileComposerOpen = False
    , advancedFiltersOpen = False
    , appliedQuery = Nothing
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
    , nextHistoryRequestToken = 1
    , detailLoading = False
    , detailError = Nothing
    , activeDetailRequest = Nothing
    , nextDetailRequestToken = 1
    , detailNavigationEpoch = 0
    , detailNavigationToken = 0
    , detailReturnTarget = Nothing
    , pendingReturnNavigation = Nothing
    , edit = Nothing
    , deleteConfirmation = Nothing
    , nextCurationContextToken = 1
    , nextMutationRequestToken = 1
    }


pageSize : Int
pageSize =
    200


validPage : Int -> Bool -> List String -> List String -> Bool
validPage offset hasMore received existing =
    List.length received <= pageSize
        && (not hasMore || (List.length received == pageSize && List.any (\key -> offset == 0 || not (List.member key existing)) received))


emptyCounts : ObservationCountState
emptyCounts =
    { owner = Nothing, active = Nothing, nextToken = 1, generation = 0, pending = False, current = False, value = Nothing, valueFingerprint = "", settledFingerprint = "", error = Nothing }


countQuery : String -> ObservationModel -> Api.ObservationCountQuery
countQuery workspaceId state =
    let value = listQuery workspaceId 0 state in
    { workspaceId = workspaceId, subjectKind = value.subjectKind, subject = value.subject, gitSha = value.gitSha, currentGitSha = value.currentGitSha, historyGitSha = value.historyGitSha, query = value.query
    , paths = if state.requestMode == ObservationMatchMode then Just (appliedState state).matchAppliedPaths else Nothing }


countFingerprint : String -> ObservationModel -> String
countFingerprint workspaceId state =
    let value = countQuery workspaceId state in
    Encode.encode 0 (Encode.list Encode.string [ workspaceId, Maybe.withDefault "" (Maybe.map Api.subjectKindToString value.subjectKind), Maybe.withDefault "" value.subject, Maybe.withDefault "" value.gitSha, Maybe.withDefault "" value.currentGitSha, Maybe.withDefault "" value.historyGitSha, Maybe.withDefault "" value.query, Encode.encode 0 (Encode.list Encode.string (Maybe.withDefault [] value.paths)) ])


hasAppliedCountFilter : ObservationModel -> Bool
hasAppliedCountFilter state =
    let value = countQuery "" state in
    value.subjectKind /= Nothing || value.subject /= Nothing || value.gitSha /= Nothing || value.currentGitSha /= Nothing || value.historyGitSha /= Nothing || value.query /= Nothing || not (List.isEmpty (Maybe.withDefault [] value.paths))


countActor : Model -> String
countActor model =
    model.sessionContext |> Maybe.map (\session -> Encode.encode 0 (Encode.list Encode.string [ session.authMode, session.principal.actorType, session.principal.actorId, session.principal.authority, Maybe.withDefault "" session.principal.grantUserId, Permissions.currentWorkspaceRoleLabel model ])) |> Maybe.withDefault ""


invalidateCounts : ObservationModel -> ObservationModel
invalidateCounts state =
    let counts = state.counts in
    { state | counts = { counts | generation = counts.generation + 1, pending = True, current = False, error = Nothing } }


syncCounts : Model -> ( Model, Cmd Msg )
syncCounts model =
    let counts = model.observations.counts in
    if model.auth.status /= AuthReady || model.sessionContext == Nothing || not (Permissions.canReadCurrentWorkspace model) then
        ( updateObservation (\state -> { state | counts = { emptyCounts | nextToken = counts.nextToken, generation = counts.generation + 1 } }) model, Cmd.none )
    else
        case repositoryWorkspaceId model of
            Nothing -> ( updateObservation (\state -> { state | counts = { emptyCounts | nextToken = counts.nextToken, generation = counts.generation + 1 } }) model, Cmd.none )
            Just workspaceId ->
                let
                    fingerprint = countFingerprint workspaceId model.observations
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
                    ( updateObservation (\state -> { state | counts = started }) model, Api.fetchObservationCounts model.flags.apiUrl (countQuery workspaceId model.observations) (GotObservationCounts guard) )
                else
                    ( updateObservation (\state -> { state | counts = queued }) model, Cmd.none )


acceptCounts : ObservationCountGuard -> Result Http.Error Api.ObservationCounts -> Model -> ( Model, Cmd Msg )
acceptCounts guard result model =
    let counts = model.observations.counts in
    if counts.active /= Just guard then ( model, Cmd.none )
    else
        let
            owned = guard.workspaceId == Maybe.withDefault "" (repositoryWorkspaceId model) && guard.sessionEpoch == model.sessionRequestEpoch && guard.actor == countActor model && model.auth.status == AuthReady && Permissions.canReadCurrentWorkspace model
            current = owned && guard.generation == counts.generation && guard.fingerprint == countFingerprint guard.workspaceId model.observations && not counts.pending
            completed = { counts | active = Nothing, settledFingerprint = guard.fingerprint }
            settled = if not current then { completed | pending = owned, error = Nothing }
                else case result of
                    Ok value -> if value.workspaceId == guard.workspaceId then { completed | value = Just value, valueFingerprint = guard.fingerprint, current = True, error = Nothing }
                        else { completed | current = False, error = Just "Observation counts returned the wrong workspace." }
                    Err _ -> { completed | current = False, error = Just "Unable to load Observation counts." }
        in
        syncCounts (updateObservation (\state -> { state | counts = settled }) model)


{-| Read permission may be revoked and regranted within the same session epoch.
Retire private state while keeping request/navigation identities monotonic.
-}
retireSessionState : ObservationModel -> ObservationModel
retireSessionState previous =
    { init
        | counts = let counts = previous.counts in { emptyCounts | nextToken = counts.nextToken, generation = counts.generation + 1 }
        , requestGeneration = previous.requestGeneration + 1
        , facetRequestGeneration = previous.facetRequestGeneration + 1
        , nextDetailRequestToken = previous.nextDetailRequestToken
        , nextCurationContextToken = previous.nextCurationContextToken
        , nextMutationRequestToken = previous.nextMutationRequestToken
        , nextHistoryRequestToken = previous.nextHistoryRequestToken
        , detailNavigationToken = previous.detailNavigationToken + 1
        , nextLinkToken = previous.nextLinkToken + 1
        , nextPreferenceRequest = previous.nextPreferenceRequest + 1
    }


clearSelection : ObservationModel -> ObservationModel
clearSelection state =
    let viewport = state.viewport in
    { state
        | selectedId = Nothing
        , inlineOwner = Nothing
        , selectedDetail = Nothing
        , history = Nothing
        , detailLoading = False
        , detailError = Nothing
        , activeDetailRequest = Nothing
        , detailNavigationToken = state.detailNavigationToken + 1
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
        clearSelection state


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( rawUpdated, command ) =
            updateRaw msg model
        queryUpdated = if querySnapshot rawUpdated.observations /= querySnapshot model.observations then
            updateObservation (\state -> { state | refreshPass = Nothing, refreshPending = False, refreshError = Nothing }) rawUpdated
            else rawUpdated

        intentional =
            case msg of
                ApplyObservationFilters -> True
                SetObservationBrowseMode _ -> True
                SelectObservationFacet _ _ -> True
                ApplyObservationMatch -> True
                ClearObservationMatch -> True
                SelectObservation _ -> True
                SelectObservationFrom _ _ -> True
                ReturnObservationResults -> True
                ReturnToObservationDraft -> True
                _ -> False
        touched = intentional || (case msg of
            ToggleObservationSubjects _ -> True
            ToggleObservationMatchGroup _ -> True
            _ -> False)
        updated = if touched then rememberPreferences queryUpdated else queryUpdated
    in
    if intentional && (Helpers.observationAppliedQuery updated.observations /= Helpers.observationAppliedQuery model.observations || updated.observations.selectedId /= model.observations.selectedId || updated.activeTab /= model.activeTab) then
        let
            ( linked, linkCmd ) = Helpers.writeObservationHistory True updated
        in
        ( linked, Cmd.batch [ command, linkCmd ] )
    else
        ( updated, command )


updateRaw : Msg -> Model -> ( Model, Cmd Msg )
updateRaw msg model =
    case msg of
        GotObservationCounts guard result ->
            acceptCounts guard result model

        RetryObservationCounts ->
            syncCounts (updateObservation (\state -> let counts = state.counts in { state | counts = { counts | error = Nothing, pending = True } }) model)

        ObservationPreferencesReceived payload ->
            hydratePreferences payload model

        SetObservationQuery value ->
            ( updateObservation (\state -> { state | query = value }) model, Cmd.none )

        SetObservationSubjectKind value ->
            ( updateObservation (\state -> { state | subjectKind = Api.subjectKindFromString value }) model, Cmd.none )

        SetObservationSubject value ->
            ( updateObservation (\state -> { state | subject = value }) model, Cmd.none )

        SetObservationGitSha value ->
            ( updateObservation (\state -> { state | gitSha = value }) model, Cmd.none )

        SetObservationCurrentGitSha value ->
            ( updateObservation (\state -> { state | currentGitSha = value }) model, Cmd.none )

        SetObservationHistoryGitSha value ->
            ( updateObservation (\state -> { state | historyGitSha = value }) model, Cmd.none )

        ApplyObservationFilters ->
            applyObservationFilters model

        RevertObservationFilters ->
            ( updateObservation revertFilters model, Cmd.none )

        RefreshObservationResults ->
            refreshActiveResults model

        RetryObservationResults ->
            retryResults model

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
            ( updateObservation (\state -> { state | fileComposerOpen = True }) model, focusElement "observation-match-paths" )

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
            activateObservation observationId (Just originId) model

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
            loadHistory model

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
                                (applyCanonicalObservation observation
                                    >> (\state -> { state | detailLoading = False, detailError = Nothing, activeDetailRequest = Nothing })
                                )
                                model
                            , Cmd.none
                            )

                        else
                            ( updateObservation (\state -> { state | detailLoading = False, detailError = Just "Observation detail did not match this workspace.", activeDetailRequest = Nothing }) model, Cmd.none )

                    Err (Http.BadStatus 404) ->
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
    case model.observations.requestMode of
        ObservationFacetMode ->
            reloadFacets model

        ObservationMatchMode ->
            reloadAppliedMatch model

        ObservationExactSubjectMode ->
            reloadResultMode ObservationExactSubjectMode model

        ObservationFlatMode ->
            reloadResultMode ObservationFlatMode model


switchBrowseMode : ObservationRequestMode -> Model -> ( Model, Cmd Msg )
switchBrowseMode mode model =
    let
        prepared =
            updateObservation
                (\state ->
                    { state
                        | requestMode = mode
                        , subject =
                            if mode == ObservationFacetMode then
                                ""

                            else
                                state.subject
                        , matchPathsInput =
                            if mode == ObservationMatchMode then
                                state.matchPathsInput

                            else
                                ""
                        , matchAppliedPaths =
                            if mode == ObservationMatchMode then
                                state.matchAppliedPaths

                            else
                                []
                        , selectedFacet =
                            if mode == ObservationExactSubjectMode || mode == ObservationMatchMode then
                                state.selectedFacet

                            else
                                Nothing
                        , matchValidationError = Nothing
                        , matchEvidence = Dict.empty
                        , expandedMatchGroups = Dict.empty
                        , browseReturn = Nothing
                    }
                )
                model

        withFocus ( updated, command ) =
            ( updated, Cmd.batch [ command, focusElement "observation-mode-heading" ] )
    in
    case mode of
        ObservationFacetMode ->
            withFocus (reloadFacets prepared)

        ObservationExactSubjectMode ->
            if prepared.observations.selectedFacet == Nothing then
                withFocus (reloadResultMode ObservationFlatMode prepared)

            else
                withFocus (reloadResultMode ObservationExactSubjectMode prepared)

        ObservationMatchMode ->
            withFocus (matchObservations model)

        ObservationFlatMode ->
            withFocus (reloadResultMode ObservationFlatMode prepared)


selectFacet : Api.SubjectKind -> String -> Model -> ( Model, Cmd Msg )
selectFacet subjectKind subject model =
    model
        |> updateObservation
            (\state ->
                { state
                    | selectedFacet = Just { subjectKind = subjectKind, subject = subject }
                    , requestMode = ObservationExactSubjectMode
                    , matchValidationError = Nothing
                    , matchEvidence = Dict.empty
                    , expandedMatchGroups = Dict.empty
                    , browseReturn = Nothing
                }
            )
        |> reloadResultMode ObservationExactSubjectMode
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
                        { requestMode = ObservationFlatMode
                        , subjectKind = model.observations.subjectKind
                        , subject = model.observations.subject
                        , selectedFacet = model.observations.selectedFacet
                        , query = model.observations.query
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
    case repositoryWorkspaceId model of
        Just workspaceId ->
            let
                state =
                    startFacetReload model.sessionRequestEpoch workspaceId model.observations

                updated =
                    updateObservation (always state) model
            in
            fetchFacetPage 0 updated

        Nothing ->
            ( updateObservation (\state -> { state | facetLoading = False, facetExpectedOffset = Nothing }) model, Cmd.none )


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
    if state.refreshPass /= Nothing then
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
                        pass = { query = querySnapshot state, targetOffset = Basics.max pageSize (if facets then state.facetNextOffset else state.nextOffset), items = Dict.empty, orderedIds = [], matchEvidence = Dict.empty, facets = Dict.empty, facetKeys = [], invalidated = False }
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
                if count > pageSize || (hasMore && (count /= pageSize || after <= before)) then fail "Automatic refresh is incomplete: the page made no valid progress."
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
                        finished = { reconciled | refreshPass = Nothing, refreshPending = False, refreshError = Nothing, resultsStale = False, loading = False, facetLoading = False, expectedOffset = Nothing, facetExpectedOffset = Nothing, failedRequest = Nothing }
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
            ( updated
            , Api.fetchObservations model.flags.apiUrl
                (listQuery workspaceId offset state)
                (GotObservations workspaceId Nothing state.requestGeneration state.queryFingerprint offset)
            )

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
    case repositoryWorkspaceId model of
        Nothing ->
            ( model, Cmd.none )

        Just workspaceId ->
            case normalizeMatchPaths model.observations.matchPathsInput of
                Err message ->
                    ( updateObservation (\state -> { state | matchValidationError = Just message }) model, Cmd.none )

                Ok paths ->
                    let
                        state =
                            startMatchReload model.sessionRequestEpoch workspaceId paths model.observations

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
    , query = nonEmpty state.query
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


querySnapshot : ObservationModel -> ObservationAppliedQuery
querySnapshot input =
    let
        state =
            appliedState input
    in
    { requestMode = state.requestMode
    , query = state.query
    , subjectKind = state.subjectKind
    , subject = state.subject
    , selectedFacet = state.selectedFacet
    , gitSha = state.gitSha, currentGitSha = state.currentGitSha, historyGitSha = state.historyGitSha
    , matchAppliedPaths = state.matchAppliedPaths
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
    { state | query = applied.query, subjectKind = applied.subjectKind, subject = applied.subject, gitSha = applied.gitSha, currentGitSha = applied.currentGitSha, historyGitSha = applied.historyGitSha }


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
                        (\current -> { current | detailNavigationEpoch = model.sessionRequestEpoch, detailNavigationToken = current.detailNavigationToken + 1, detailReturnTarget = origin, pendingReturnNavigation = Nothing
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
                                , history = Nothing
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
        | counts = (if isLoadedOrSelected observationId state then invalidateCounts state else state).counts
        , items = Dict.remove observationId state.items
        , expandedSubjects = Dict.remove observationId state.expandedSubjects
        , preferenceValue = let prefs = state.preferenceValue in { prefs | detail = if Maybe.map .id prefs.detail == Just observationId then Nothing else prefs.detail, subjects = List.filter ((/=) observationId) prefs.subjects }
        , orderedIds = List.filter ((/=) observationId) state.orderedIds
        , matchEvidence = Dict.remove observationId state.matchEvidence
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
        exact = state.selectedFacet |> Maybe.map (\facet -> "exact:" ++ facetKey facet.subjectKind facet.subject) |> Maybe.withDefault "exact:missing"
    in
    Array.fromList (case state.requestMode of
        ObservationFacetMode -> state.facetKeys |> List.filterMap (\key -> Dict.get key state.facets |> Maybe.map ObservationFacetRow)
        ObservationMatchMode -> groupPathMatches state.matchAppliedPaths state.orderedIds state.matchEvidence |> List.concatMap pathRows
        ObservationExactSubjectMode -> cards exact state.orderedIds
        ObservationFlatMode -> cards "flat" state.orderedIds)


viewportLifetime : Model -> ObservationViewport.Stamp
viewportLifetime model =
    let state = model.observations in
    { workspace = if model.activeTab == ObservationsTab then repositoryWorkspaceId model |> Maybe.withDefault "" else ""
    , epoch = model.sessionRequestEpoch
    , generation = if state.requestMode == ObservationFacetMode then facetFingerprintFor model.sessionRequestEpoch (facetQuery (Maybe.withDefault "" model.selectedWorkspaceId) 0 state) else resultViewFingerprint model.sessionRequestEpoch (Maybe.withDefault "" model.selectedWorkspaceId) state
    , revision = state.viewport.stamp.revision }


{-| Build membership projection only after a structural/canonical change, never
for a draft keystroke. Transport caches and their counts remain untouched.
-}
refreshViewport : Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
refreshViewport previous ( incoming, command ) =
    let
        ( model, preferenceCmd ) = syncPreferences previous incoming
        old = previous.observations
        state = model.observations
        lifetime = viewportLifetime model
        changed = old.items /= state.items || old.orderedIds /= state.orderedIds || old.requestMode /= state.requestMode
            || old.selectedFacet /= state.selectedFacet || old.facets /= state.facets || old.facetKeys /= state.facetKeys
            || old.matchAppliedPaths /= state.matchAppliedPaths || old.matchEvidence /= state.matchEvidence
            || old.expandedMatchGroups /= state.expandedMatchGroups
            || lifetime.workspace /= state.viewport.stamp.workspace || lifetime.epoch /= state.viewport.stamp.epoch
            || lifetime.generation /= state.viewport.stamp.generation
        rows = if changed then (if lifetime.workspace == "" then Array.empty else projectResultRows state) else state.resultRows
        projectedViewport = if changed then
            let rebuilt = ObservationViewport.rebuild lifetime (Array.toList rows |> List.map resultRowKey) state.viewport in
            if old.items /= state.items || old.expandedMatchGroups /= state.expandedMatchGroups then { rebuilt | origin = Nothing } else rebuilt
            else state.viewport
        oldStamp = projectedViewport.stamp
        navigationChanged = projectedViewport.navigationToken /= state.detailNavigationToken
        viewport = if navigationChanged then { projectedViewport | navigationToken = state.detailNavigationToken, stamp = { oldStamp | revision = oldStamp.revision + 1 } } else projectedViewport
        invalidatedReturn = changed && (old.items /= state.items || old.expandedMatchGroups /= state.expandedMatchGroups || old.orderedIds /= state.orderedIds)
        ownedPending = state.pendingReturnNavigation |> Maybe.andThen (\intent ->
            if intent.workspaceId == lifetime.workspace && intent.sessionEpoch == lifetime.epoch && intent.queryGeneration == lifetime.generation && intent.navigationToken == state.detailNavigationToken then Just intent else Nothing)
        pending = if invalidatedReturn then ownedPending |> Maybe.map (\intent -> { intent | fallback = intent.intent == "return" && not (Dict.member intent.originKey viewport.index.positions), readyRevision = Nothing }) else ownedPending
        owner = if changed || old.selectedId /= state.selectedId || old.detailReturnTarget /= state.detailReturnTarget then resolveSelectedOwner rows viewport state else state.inlineOwner
        updated = { state | resultRows = rows, viewport = viewport, inlineOwner = owner, pendingReturnNavigation = pending }
        synchronize = changed || navigationChanged || old.selectedId /= state.selectedId || old.detailReturnTarget /= state.detailReturnTarget || old.viewport.focus /= viewport.focus || old.viewport.returnPin /= viewport.returnPin || old.viewport.stamp /= viewport.stamp
    in
    let
        ( automatic, automaticCmd ) = continueAutomaticRefresh { model | observations = updated }
        ( counted, countsCmd ) = syncCounts automatic
    in
    ( counted, Cmd.batch [ command, preferenceCmd, automaticCmd, countsCmd, if synchronize then Ports.syncObservationViewport (ObservationViewport.sync changed 0 Nothing viewport) else Cmd.none ] )


preferenceMode : ObservationRequestMode -> String
preferenceMode mode =
    case mode of
        ObservationFlatMode -> "flat"
        ObservationExactSubjectMode -> "exact"
        ObservationMatchMode -> "match"
        ObservationFacetMode -> "facets"


rememberPreferences : Model -> Model
rememberPreferences model =
    updateObservation (\state ->
        { state | preferenceTouch = state.preferenceTouch + 1
        , preferencePendingDetail = False
        , preferenceEntryHistory = Nothing
        , preferenceValue = Preferences.normalize
            { detail = state.selectedId |> Maybe.map (\observationId -> { id = observationId, occurrence = state.detailReturnTarget, mode = preferenceMode state.requestMode })
            , subjects = Dict.toList state.expandedSubjects |> List.filter Tuple.second |> List.map Tuple.first
            , groups = Dict.toList state.expandedMatchGroups |> List.filter Tuple.second |> List.map Tuple.first }
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
            ( updateObservation (\current -> { current | preferenceOwner = Nothing, preferenceValue = Preferences.empty, preferenceHydrated = False, preferencePendingDetail = False, preferenceEntryHistory = Nothing
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
                        let clean = { state | preferenceOwner = Nothing, preferenceValue = Preferences.empty, preferenceHydrated = False, preferencePendingDetail = False, preferenceEntryHistory = Nothing, expandedSubjects = Dict.empty, expandedMatchGroups = Dict.empty } in
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
            if state.preferenceTouch /= owner.touch then ( { model | observations = { accepted | preferenceEntryHistory = Nothing } }, Cmd.none ) else
            case value of
                Err _ -> ( { model | observations = { accepted | preferenceEntryHistory = Nothing } }, Cmd.none )
                Ok prefs ->
                    let
                        hydrated = { accepted | preferenceValue = prefs, preferencePendingDetail = prefs.detail /= Nothing, preferenceEntryHistory = if prefs.detail == Nothing then Nothing else accepted.preferenceEntryHistory, expandedSubjects = Dict.fromList (List.map (\key -> ( key, True )) prefs.subjects)
                            , expandedMatchGroups = Dict.fromList (List.map (\key -> ( key, True )) prefs.groups) }
                        ready = { model | observations = hydrated }
                    in
                    restorePendingPreferences ready
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
                in
                ( { model | observations = { state | viewport = viewport, pendingReturnNavigation = pending } }
                , Cmd.batch [ Ports.syncObservationViewport (ObservationViewport.syncReceipt settled adjustment target viewport), navigation ] )


viewResultRows : Bool -> ObservationModel -> Html Msg
viewResultRows canEdit state =
    let
        -- Pure view callers (fixtures/tests) may not have passed through routing.
        uninitialized = state.viewport.stamp.workspace == "" && Array.isEmpty state.resultRows
        rows = if uninitialized then projectResultRows state else state.resultRows
        viewport = if uninitialized then ObservationViewport.rebuild state.viewport.stamp (Array.toList rows |> List.map resultRowKey) state.viewport else state.viewport
        owner = selectedOwner state
        piece item =
            case item of
                HierarchyViewport.Gap at height ->
                    ( "gap:" ++ String.fromInt at, div [ class "observation-viewport-spacer", style "height" (String.fromFloat height ++ "px"), attribute "aria-hidden" "true" ] [] )
                HierarchyViewport.Row at key _ ->
                    ( key, Array.get at rows |> Maybe.map (\row ->
                        div [ class "observation-viewport-row", attribute "data-observation-key" key, attribute "data-observation-position" (String.fromInt at) ]
                            [ Lazy.lazy4 viewResultRow canEdit owner state row ]) |> Maybe.withDefault (text "") )
    in
    Keyed.node "div"
        [ id "observation-viewport", class "observation-list-rows observation-viewport"
        , attribute "data-observation-viewport-context" (Encode.encode 0 (ObservationViewport.stampValue viewport.stamp))
        , attribute "data-observation-layout-ready" (if viewport.restoring then "false" else "true")
        , attribute "data-observation-focus-key" (Maybe.withDefault "" viewport.focus)
        , attribute "data-observation-logical-count" (String.fromInt (Array.length rows))
        , attribute "data-observation-loaded-count" (String.fromInt (List.length state.orderedIds)) ]
        (ObservationViewport.piecesWithOwner owner state.detailReturnTarget viewport |> List.map piece)


viewResultRow : Bool -> Maybe String -> ObservationModel -> ObservationResultRow -> Html Msg
viewResultRow canEdit owner state row =
    case row of
        ObservationCardRow context observation -> viewObservationRow canEdit owner state context observation
        ObservationFacetRow facet -> viewFacet facet
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
        , viewObservationsStateWithPermission (Permissions.canEditCurrentWorkspace model) workspace { state | detailNavigationEpoch = model.sessionRequestEpoch }
        ]


viewObservationsState : Api.Workspace -> ObservationModel -> Html Msg
viewObservationsState workspace state =
    viewObservationsStateWithPermission False workspace state


viewObservationsStateWithPermission : Bool -> Api.Workspace -> ObservationModel -> Html Msg
viewObservationsStateWithPermission canEdit workspace state =
    if workspace.workspaceType /= Api.Repository then
        div [ class "empty-state observation-state observation-state-unavailable" ]
            [ h3 [] [ text "Observations unavailable" ]
            , p [] [ text "Observations are available only for repository workspaces." ]
            ]

    else
        div [ id "observation-panel", class "observations-panel", attribute "data-observation-context" (Encode.encode 0 (navigationStamp workspace.id state)) ]
            [ div
                ([ class "observation-curation-background" ]
                    ++ (if canEdit && state.deleteConfirmation /= Nothing then
                            [ attribute "inert" "", attribute "aria-hidden" "true" ]

                        else
                            []
                       )
                )
                [ viewModeNavigation state
                , viewFilters state
                , div
                    [ classList
                        [ ( "observation-layout", True )
                        , ( "observation-layout-with-detail", state.selectedId /= Nothing )
                        ]
                    ]
                    [ if state.selectedId /= Nothing && selectedOwner state == Nothing then
                        div [ class "card observation-result observation-detached", attribute "data-observation-detached" "true" ]
                            [ p [ class "form-help", attribute "role" "status" ] [ text "Selected observation is outside the displayed result rows. Loaded counts and file-match evidence are unchanged." ]
                            , case state.selectedDetail of
                                Just observation -> viewObservationHeader True "observation-detached-toggle" observation
                                Nothing -> div [ class "card-header observation-card-header" ]
                                    [ observationToggle True "observation-detached-toggle" "Selected observation" ReturnObservationResults
                                    , span [ class "observation-subject" ] [ text "Selected observation" ]
                                    ]
                            , viewDetail canEdit state
                            ]
                      else text ""
                    , viewList canEdit workspace.id state
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
    div [] [ viewAppliedFilters state, viewFilterInputs state ]


viewFilterInputs : ObservationModel -> Html Msg
viewFilterInputs state =
    div [ class "observation-discovery-controls" ]
        [ Html.form [ class "filter-bar observation-filters observation-search", onSubmit ApplyObservationFilters ]
            [ div [ class "filter-group observation-filter-group" ]
                [ label [ class "filter-label", for "observation-query" ] [ text "Search observations" ]
                , input
                    [ id "observation-query"
                    , class "form-input observation-filter-input"
                    , type_ "search"
                    , placeholder "Search observation text"
                    , value state.query
                    , onInput SetObservationQuery
                    ]
                    []
                ]
            , button [ class "btn btn-primary observation-filter-apply", type_ "submit", disabled state.loading ] [ text "Apply filters" ]
            , button
                [ id "observation-advanced-toggle"
                , class "btn btn-secondary"
                , type_ "button"
                , onClick ToggleObservationAdvancedFilters
                , attribute "aria-expanded" (if state.advancedFiltersOpen then "true" else "false")
                , attribute "aria-controls" "observation-advanced-filters"
                ]
                [ text "Advanced filters" ]
            ]
        , viewAdvancedFilters state
        , viewFileComposer state
        ]


viewAdvancedFilters : ObservationModel -> Html Msg
viewAdvancedFilters state =
    div [ id "observation-advanced-filters", class "filter-bar observation-filters observation-advanced-filters", hidden (not state.advancedFiltersOpen) ]
        [ div [ class "filter-group observation-filter-group observation-filter-kind" ]
            [ label [ class "filter-label", for "observation-subject-kind" ]
                [ text
                    (if state.requestMode == ObservationExactSubjectMode then
                        "Browse/match subject kind"

                     else
                        "Subject kind"
                    )
                ]
            , select [ id "observation-subject-kind", class "filter-select observation-filter-select", onInput SetObservationSubjectKind ]
                [ option [ value "", selected (state.subjectKind == Nothing) ] [ text "All subjects" ]
                , option [ value "file", selected (state.subjectKind == Just Api.SubjectFile) ] [ text "Files" ]
                , option [ value "glob", selected (state.subjectKind == Just Api.SubjectGlob) ] [ text "Globs" ]
                ]
            ]
        , case state.requestMode of
            ObservationFlatMode ->
                div [ class "filter-group observation-filter-group" ]
                    [ label [ class "filter-label", for "observation-subject" ] [ text "Exact subject" ]
                    , input
                        [ id "observation-subject"
                        , class "form-input observation-filter-input"
                        , placeholder "Optional file or glob"
                        , value state.subject
                        , onInput SetObservationSubject
                        ]
                        []
                    ]

            ObservationExactSubjectMode ->
                case state.selectedFacet of
                    Just selectedFacet ->
                        div [ class "filter-group observation-filter-group observation-selected-facet" ]
                            [ span [ class "filter-label" ] [ text "Selected shared subject" ]
                            , strong [ class "observation-selected-facet-value" ]
                                [ text (subjectKindLabel selectedFacet.subjectKind ++ ": "), Helpers.copyableValue "" "repository subject" selectedFacet.subject selectedFacet.subject ]
                            , p [ class "form-help" ] [ text "Exact results stay locked to this subject tuple. Browse/match kind changes do not alter it." ]
                            ]

                    Nothing ->
                        p [ class "form-error", attribute "role" "alert" ] [ text "No exact shared subject is selected." ]

            ObservationFacetMode ->
                p [ class "form-help observation-filter-mode-help" ] [ text "Subject ignores the manual exact-subject filter and uses the shared search, kind, and Git SHA filters." ]

            ObservationMatchMode ->
                p [ class "form-help observation-filter-mode-help" ] [ text "Concrete path matching uses the shared search, kind, and Git SHA filters and ignores manual exact-subject filtering." ]
        , div [ class "filter-group observation-filter-group" ]
            [ label [ class "filter-label", for "observation-git-sha" ] [ text "Original Git SHA" ]
            , input
                [ id "observation-git-sha"
                , class "form-input observation-filter-input observation-filter-sha"
                , placeholder "Full Git SHA"
                , value state.gitSha
                , onInput SetObservationGitSha
                ]
                []
            ]
        , div [ class "filter-group observation-filter-group" ]
            [ label [ class "filter-label", for "observation-current-git-sha" ] [ text "Current reviewed Git SHA" ]
            , input [ id "observation-current-git-sha", class "form-input observation-filter-sha", value state.currentGitSha, onInput SetObservationCurrentGitSha, placeholder "Full Git SHA" ] []
            ]
        , div [ class "filter-group observation-filter-group" ]
            [ label [ class "filter-label", for "observation-history-git-sha" ] [ text "Recorded history Git SHA" ]
            , input [ id "observation-history-git-sha", class "form-input observation-filter-sha", value state.historyGitSha, onInput SetObservationHistoryGitSha, placeholder "Full Git SHA" ] []
            ]
        ]


viewFileComposer : ObservationModel -> Html Msg
viewFileComposer state =
    section [ id "observation-file-composer", class "filter-bar observation-filters observation-file-composer", hidden (not state.fileComposerOpen), attribute "aria-labelledby" "observation-file-composer-heading" ]
        [ div [ class "filter-group observation-filter-group observation-match-input" ]
            [ h3 [ id "observation-file-composer-heading" ] [ text "Find observations for files" ]
            , p [ class "form-help" ] [ text "The displayed results stay unchanged until you select Match files." ]
            , label [ class "filter-label", for "observation-match-paths" ] [ text "Repository file paths" ]
            , textarea
                [ id "observation-match-paths"
                , class "form-input observation-filter-input"
                , placeholder "One concrete repository-relative path per line"
                , value state.matchPathsInput
                , onInput SetObservationMatchPaths
                , attribute "rows" "4"
                , attribute "aria-describedby" "observation-match-help"
                ]
                []
            , p [ id "observation-match-help", class "form-help" ] [ text "For example: src/Main.elm. Enter one concrete repository-relative file path per line. Matches saved file and glob subjects; wildcards are not accepted as input and no checkout is scanned." ]
            , case state.matchValidationError of
                Just message ->
                    p [ class "form-error", attribute "role" "alert" ] [ text message ]

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


viewAppliedFilters : ObservationModel -> Html Msg
viewAppliedFilters state =
    let
        applied =
            appliedState state

        query =
            listQuery "" 0 state

        typed labelText value =
            span [ class "observation-applied-value" ] [ text (labelText ++ ": "), Helpers.copyableValue "" labelText value value ]
    in
    div [ class "observation-applied-filters" ]
        [ if state.appliedQuery == Nothing then p [] [ text "No query has been applied yet." ] else
            div [ class "observation-applied-summary" ]
                ([ span [] [ text ("Applied filters: Mode: " ++ modeName applied.requestMode ++ "; Search: " ++ Maybe.withDefault "all" query.query ++ "; Kind: " ++ (query.subjectKind |> Maybe.map subjectKindLabel |> Maybe.withDefault "all")) ]
                 , query.subject |> Maybe.map (typed "Subject") |> Maybe.withDefault (span [] [ text "Subject: all" ])
                 , query.gitSha |> Maybe.map (typed "Original Git SHA") |> Maybe.withDefault (span [] [ text "Original Git SHA: all" ])
                 , query.currentGitSha |> Maybe.map (typed "Current reviewed Git SHA") |> Maybe.withDefault (text "")
                 , query.historyGitSha |> Maybe.map (typed "Recorded history Git SHA") |> Maybe.withDefault (text "")
                 ] ++ (if applied.requestMode == ObservationMatchMode then List.map (typed "File") applied.matchAppliedPaths else []))
        , if hasUnappliedFilters state then
            div [ class "form-help", attribute "role" "status" ]
                [ text "Filters have unapplied changes. Apply filters or revert them before loading more results."
                , button [ class "btn btn-secondary", type_ "button", onClick RevertObservationFilters ] [ text "Revert filters" ]
                ]

          else
            text ""
        , if state.requestMode == ObservationMatchMode && normalizeMatchPaths state.matchPathsInput /= Ok state.matchAppliedPaths then
            p [ class "form-help", attribute "role" "status" ] [ text "File path changes are not applied. Select Match files to apply them." ]

          else
            text ""
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

        headingText =
            case state.requestMode of
                ObservationFlatMode ->
                    "All observations"

                ObservationFacetMode ->
                    "Stored subjects"

                ObservationExactSubjectMode ->
                    "Exact subject results"

                ObservationMatchMode ->
                    "File matches"
    in
    section [ class "observation-mode-navigation", attribute "aria-labelledby" "observation-mode-heading" ]
        [ h2 [ id "observation-mode-heading", class "observation-mode-heading", tabindex -1 ] [ text headingText ]
        , div [ class "observation-mode-actions", attribute "role" "group", attribute "aria-label" "Observation browse mode" ]
            [ modeButton ObservationFlatMode (state.requestMode == ObservationFlatMode) "All"
            , modeButton ObservationFacetMode (state.requestMode == ObservationFacetMode || state.requestMode == ObservationExactSubjectMode) "Subject"
            , button
                [ id "observation-for-files", classList [ ( "filter-pill", True ), ( "filter-pill-active", state.requestMode == ObservationMatchMode ), ( "observation-mode-button", True ) ], type_ "button", onClick OpenObservationFileComposer
                , attribute "aria-pressed" (if state.requestMode == ObservationMatchMode then "true" else "false")
                , attribute "aria-expanded" (if state.fileComposerOpen then "true" else "false")
                , attribute "aria-controls" "observation-file-composer"
                ]
                [ text "Files" ]
            ]
        , if hasAppliedCountFilter state then
            let counts = state.counts in
            p [ class "observation-mode-announcement", attribute "aria-live" "polite" ]
                [ text (if counts.current && counts.valueFingerprint == countFingerprint (Maybe.map .workspaceId counts.owner |> Maybe.withDefault "") state then
                    counts.value |> Maybe.map (\value -> String.fromInt value.matchCount ++ (if value.matchCount == 1 then " Observation matches" else " Observations match")) |> Maybe.withDefault "Observation match count unavailable"
                    else if counts.error /= Nothing then "Observation match count unavailable"
                    else "Counting matching Observations…")
                , if counts.error /= Nothing then button [ class "btn btn-sm", type_ "button", onClick RetryObservationCounts ] [ text "Retry counts" ] else text "" ]
          else text ""
        ]


viewList : Bool -> String -> ObservationModel -> Html Msg
viewList canEdit workspaceId state =
    case state.requestMode of
        ObservationFacetMode ->
            viewFacetCatalogue canEdit workspaceId state

        ObservationMatchMode ->
            viewMatchResults canEdit workspaceId state

        ObservationExactSubjectMode ->
            viewObservationResults canEdit workspaceId "Exact subject observations" "No observations share this exact subject" state

        ObservationFlatMode ->
            viewObservationResults canEdit workspaceId "Observations" "No observations found" state


viewObservationResults : Bool -> String -> String -> String -> ObservationModel -> Html Msg
viewObservationResults canEdit workspaceId ariaLabel emptyHeading state =
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
        , Lazy.lazy2 viewResultRows canEdit state
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


viewFacetCatalogue : Bool -> String -> ObservationModel -> Html Msg
viewFacetCatalogue canEdit workspaceId state =
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
        , Lazy.lazy2 viewResultRows canEdit state
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


viewFacet : Api.ObservationSubjectFacet -> Html Msg
viewFacet facet =
    div [ class "card observation-facet" ]
        [ div [ class "card-header observation-card-header" ]
            [ span [ class "entity-type-label observation-kind" ] [ text (subjectKindLabel facet.subjectKind) ]
            , Helpers.copyableValue "observation-subject" "repository subject" facet.subject facet.subject ]
        , button [ id ("observation-facet-" ++ domToken (facetKey facet.subjectKind facet.subject)), class "observation-facet-card", type_ "button"
            , onClick (SelectObservationFacet facet.subjectKind facet.subject)
            , attribute "aria-label" ("Open " ++ subjectKindLabel facet.subjectKind ++ " subject " ++ facet.subject ++ " with " ++ String.fromInt facet.observationCount ++ " observations") ]
            [ strong [] [ text (String.fromInt facet.observationCount ++ " observations") ]
            , span [ class "card-meta" ] [ text ("Latest update: " ++ formatDate facet.latestUpdatedAt) ] ] ]


viewMatchResults : Bool -> String -> ObservationModel -> Html Msg
viewMatchResults canEdit workspaceId state =
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
            Lazy.lazy2 viewResultRows canEdit state
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


viewObservationRow : Bool -> Maybe String -> ObservationModel -> String -> Api.Observation -> Html Msg
viewObservationRow canEdit owner state context observation =
    let
        cardId = observationCardDomId context observation.id
        expanded = owner == Just cardId
    in
    div [ class "card observation-result", attribute "data-observation-id" observation.id, attribute "data-observation-context-key" context ]
        [ viewObservationHeader expanded cardId observation
        , if expanded then
            viewDetail canEdit state
          else
            div [ id (cardId ++ "-body"), class "observation-folded-content" ]
                [ div [ class "card-body observation-summary" ] [ text (plainTextExcerpt 240 observation.content) ]
                , div [ class "card-meta-group observation-card-meta observation-card-footer" ]
                    [ dl [ class "observation-detail-meta" ] [ viewSubjects cardId observation.id state.expandedSubjects observation.subjects ]
                    , div [ class "card-meta-row" ]
                        [ span [ class "card-meta observation-sha" ] [ text "Current reviewed revision: ", (observation.currentProvenance |> Maybe.map (\provenance -> Helpers.copyableValue "" "current reviewed revision" provenance.reviewedGitSha (String.left 12 provenance.reviewedGitSha ++ "…")) |> Maybe.withDefault (text "Unknown legacy binding")) ]
                        , span [ class "card-meta observation-updated" ] [ text ("Content updated: " ++ formatObservationTimestamp observation.updatedAt) ]
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
         , classList [ ( "tree-toggle", True ), ( "observation-card", cardId /= "observation-detached-toggle" ), ( "observation-card-selected", expanded ) ]
         , type_ "button"
         , attribute "aria-label" (plainTextExcerpt 180 ((if expanded then "Collapse " else "Open ") ++ name))
         , attribute "aria-expanded" (if expanded then "true" else "false")
         , attribute "aria-current" (if expanded then "true" else "false")
         , attribute "aria-controls" (if expanded then "observation-detail" else cardId ++ "-body")
         , onClick message
         ] ++ (if expanded then [ attribute "data-observation-detail-anchor" "true" ] else []))
        [ text (if expanded then "▼" else "▶") ]


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
                        Array.get position rows |> Maybe.andThen (\row -> case row of
                            ObservationCardRow _ observation -> if observation.id == selected then Just origin else Nothing
                            _ -> Nothing))
                Nothing ->
                    Array.toList rows |> List.filterMap (\row -> case row of
                        ObservationCardRow context observation -> if observation.id == selected then Just (observationCardDomId context observation.id) else Nothing
                        _ -> Nothing) |> List.head


viewDetail : Bool -> ObservationModel -> Html Msg
viewDetail canEdit state =
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
                            [ viewFullObservationContent observation
                            , if canEdit && Maybe.map .observationId state.edit == Just observation.id then
                                section [ class "observation-retained-editor", attribute "aria-label" "Retained observation draft" ]
                                    [ h4 [] [ text "Retained draft" ]
                                    , viewDetailContent canEdit observation state.edit
                                    ]
                              else text ""
                            , div [ class "observation-card-footer" ]
                                [ viewMatchContext observation.provenanceMatch
                                , viewHistory observation state
                                , dl [ class "observation-detail-meta" ]
                                    [ viewDetailMeta "Workspace ID" observation.workspaceId "observation-detail-workspace"
                                    , viewSubjects (Maybe.withDefault "observation-detached" (selectedOwner state)) observation.id state.expandedSubjects observation.subjects
                                    , viewCurrentProvenance observation
                                    , viewProvenanceRevision observation.gitSha
                                    , viewDetailMeta "Created" (formatObservationTimestamp observation.createdAt) ""
                                    , viewDetailMeta "Content updated" (formatObservationTimestamp observation.updatedAt) ""
                                    ]
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


restoreRouteResults : Model -> ( Model, Cmd Msg )
restoreRouteResults model =
    case model.observations.requestMode of
        ObservationFacetMode -> reloadFacets model
        ObservationMatchMode -> reloadAppliedMatch model
        mode -> reloadResultMode mode model


bootstrapObservation : Int -> Model -> ( Model, Cmd Msg )
bootstrapObservation token model =
    case repositoryWorkspaceId model of
        Just workspaceId ->
            case model.observations.requestMode of
                ObservationFacetMode -> reloadFacets model
                ObservationMatchMode -> reloadAppliedMatch model
                mode ->
                    let
                        state = startResultReload mode model.sessionRequestEpoch workspaceId model.observations
                    in
                    ( { model | observations = state }, Api.fetchObservations model.flags.apiUrl (listQuery workspaceId 0 state) (GotObservations workspaceId (Just token) state.requestGeneration state.queryFingerprint 0) )
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
                if saved.loading || not saved.hasMore || saved.nextOffset > 10000 then ( model, Cmd.none )
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
                    finish (\saved -> { saved | items = saved.items ++ page.items, nextOffset = saved.nextOffset + List.length page.items, hasMore = page.hasMore, error = if page.hasMore && saved.nextOffset + List.length page.items > 10000 then Just "History is incomplete at the bounded page limit." else Nothing })


viewCurrentProvenance : Api.Observation -> Html Msg
viewCurrentProvenance observation =
    div [ class "observation-detail-meta-row observation-current-provenance" ]
        [ dt [ class "observation-detail-meta-label" ] [ text "Current reviewed revision" ]
        , dd [ class "observation-detail-meta-value" ]
            [ observation.currentProvenance
                |> Maybe.map (\provenance -> span [] [ Helpers.copyableValue "" "current reviewed revision" provenance.reviewedGitSha provenance.reviewedGitSha, text (" · assertion " ++ String.fromInt provenance.sequence ++ " · " ++ formatObservationTimestamp provenance.recordedAt) ])
                |> Maybe.withDefault (text "Unknown legacy content binding")
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


viewHistory : Api.Observation -> ObservationModel -> Html Msg
viewHistory observation state =
    let
        saved = currentHistory (Just observation) state.history
        entry event =
            li [ class "observation-history-entry" ]
                [ text ("Assertion " ++ String.fromInt event.provenance.sequence ++ " · ")
                , Helpers.copyableValue "" "recorded revision" event.provenance.reviewedGitSha event.provenance.reviewedGitSha
                , text (" · " ++ formatObservationTimestamp event.provenance.recordedAt ++ " · ")
                , text (if event.provenance.eventKind == "legacy_creation" then "Migrated creation claim; content binding unknown" else "Content-bound " ++ event.provenance.eventKind)
                , event.provenance.contentVersion |> Maybe.map (\version -> span [] [ text " · version ", Helpers.copyableValue "" "content version" version version ]) |> Maybe.withDefault (text "")
                , text (" · " ++ Maybe.withDefault "Unknown actor" event.provenance.actorLabel)
                , event.provenance.actorId |> Maybe.map (\actorId -> span [] [ text " · ", Helpers.copyableValue "" "actor ID" actorId actorId ]) |> Maybe.withDefault (text "")
                ]
        buttonLabel =
            saved |> Maybe.map (\history -> if history.loading then "Loading history..." else if history.error /= Nothing then "Retry history" else "Load more history") |> Maybe.withDefault "Load revision history"
    in
    section [ class "observation-history", attribute "aria-label" "Revision history" ]
        [ p [ class "form-help" ] [ text ("Revision history · head " ++ String.fromInt observation.latestSequence ++ ". Compact assertions contain no prior content.") ]
        , saved |> Maybe.map (\history -> ol [] (List.map entry history.items)) |> Maybe.withDefault (text "")
        , saved |> Maybe.andThen .error |> Maybe.map (\message -> p [ class "form-error", attribute "role" "alert" ] [ text message ]) |> Maybe.withDefault (text "")
        , if saved |> Maybe.map (\history -> history.hasMore && history.nextOffset <= 10000) |> Maybe.withDefault True then
            button [ id "observation-history-load", type_ "button", class "btn btn-secondary", onClick LoadObservationHistory, disabled (saved |> Maybe.map .loading |> Maybe.withDefault False) ] [ text buttonLabel ]
          else text ""
        ]
