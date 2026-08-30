module Feature.Observation exposing
    ( ObservationPathGroup
    , ObservationSubjectGroup
    , applyCanonicalObservation
    , canLoadMore
    , clearSelection
    , deleteDialogFocusTarget
    , detailResponseMatches
    , facetKey
    , facetQuery
    , facetResponseMatches
    , groupPathMatches
    , init
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
    )

import Api
import Char
import Dict
import Helpers exposing (beginTrackedMutation, focusElement, formatDate, replaceFragment)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (custom, onClick, onInput, stopPropagationOn)
import Http
import Json.Decode as Decode
import Permissions
import Ports exposing (copyToClipboard)
import Toast exposing (addToast)
import Types exposing (..)


init : ObservationModel
init =
    { items = Dict.empty
    , orderedIds = []
    , hasMore = False
    , loading = False
    , error = Nothing
    , query = ""
    , subjectKind = Nothing
    , subject = ""
    , selectedFacet = Nothing
    , gitSha = ""
    , requestMode = ObservationFlatMode
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
    , selectedDetail = Nothing
    , detailLoading = False
    , detailError = Nothing
    , activeDetailRequest = Nothing
    , nextDetailRequestToken = 1
    , edit = Nothing
    , deleteConfirmation = Nothing
    , nextCurationContextToken = 1
    , nextMutationRequestToken = 1
    }


clearSelection : ObservationModel -> ObservationModel
clearSelection state =
    { state
        | selectedId = Nothing
        , selectedDetail = Nothing
        , detailLoading = False
        , detailError = Nothing
        , activeDetailRequest = Nothing
        , edit = Nothing
        , deleteConfirmation = Nothing
    }


selectionForTab : WorkspaceTab -> ObservationModel -> ObservationModel
selectionForTab tab state =
    if tab == ObservationsTab then
        state

    else
        clearSelection state


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetObservationQuery value ->
            ( updateObservation (\state -> { state | query = value }) model, Cmd.none )

        SetObservationSubjectKind value ->
            ( updateObservation (\state -> { state | subjectKind = Api.subjectKindFromString value }) model, Cmd.none )

        SetObservationSubject value ->
            ( updateObservation (\state -> { state | subject = value }) model, Cmd.none )

        SetObservationGitSha value ->
            ( updateObservation (\state -> { state | gitSha = value }) model, Cmd.none )

        ApplyObservationFilters ->
            applyObservationFilters model

        SetObservationBrowseMode mode ->
            switchBrowseMode mode model

        SelectObservationFacet subjectKind subject ->
            selectFacet subjectKind subject model

        SetObservationMatchPaths value ->
            ( updateObservation (\state -> { state | matchPathsInput = value, matchValidationError = Nothing }) model, Cmd.none )

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

        ToggleObservationMatchGroup groupKey ->
            ( updateObservation
                (\state ->
                    { state
                        | expandedMatchGroups =
                            Dict.update groupKey
                                (\current -> Just (not (Maybe.withDefault False current)))
                                state.expandedMatchGroups
                    }
                )
                model
            , Cmd.none
            )

        SelectObservation observationId ->
            selectObservation observationId model

        CopyObservationSubject subject ->
            let
                ( updated, toastCmd ) =
                    addToast Success "Repository subject copied to clipboard" model
            in
            ( updated, Cmd.batch [ copyToClipboard subject, toastCmd ] )

        StartObservationEdit ->
            startEdit model

        SetObservationDraft value ->
            ( updateObservation
                (\state ->
                    { state
                        | edit =
                            state.edit
                                |> Maybe.map (\edit -> { edit | draft = value, error = Nothing })
                    }
                )
                model
            , Cmd.none
            )

        SaveObservationEdit ->
            saveEdit model

        CancelObservationEdit ->
            ( updateObservation (\state -> { state | edit = Nothing }) model
            , focusElement "observation-edit"
            )

        ReloadObservationEdit ->
            ( updateObservation reloadEdit model, Cmd.none )

        RebaseObservationEdit ->
            ( updateObservation rebaseEdit model, Cmd.none )

        ObservationUpdated request result ->
            handleUpdateResponse request result model

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

            else
                case result of
                    Ok paginated ->
                        ( updateObservation (mergeMatchPage offset paginated) model, Cmd.none )

                    Err _ ->
                        ( updateObservation (\state -> { state | loading = False, error = Just "Failed to match repository files.", expectedOffset = Nothing }) model, Cmd.none )

        GotObservationSubjectFacets workspaceId sessionEpoch generation fingerprint offset result ->
            if model.selectedWorkspaceId /= Just workspaceId || model.sessionRequestEpoch /= sessionEpoch || not (facetResponseMatches sessionEpoch generation fingerprint offset model.observations) then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        ( updateObservation (mergeFacetPage offset paginated) model, Cmd.none )

                    Err _ ->
                        ( updateObservation (\state -> { state | facetLoading = False, facetError = Just "Failed to load shared subjects.", facetExpectedOffset = Nothing }) model, Cmd.none )

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
                        , baseUpdatedAt = observation.updatedAt
                        , draft = observation.content
                        , latestCanonical = observation
                        , conflict = False
                        , saving = False
                        , error = Nothing
                        , activeRequest = Nothing
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
            if not (editContextIsCurrent edit model) || edit.saving || edit.conflict then
                ( model, Cmd.none )

            else
                case observationContentError edit.draft of
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
                            , Api.updateObservation model.flags.apiUrl edit.observationId edit.draft requestId (ObservationUpdated request)
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
                        { edit
                            | baseContent = edit.latestCanonical.content
                            , baseUpdatedAt = edit.latestCanonical.updatedAt
                            , draft = edit.latestCanonical.content
                            , conflict = False
                            , saving = False
                            , error = Nothing
                            , activeRequest = Nothing
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
                        { edit
                            | baseContent = edit.latestCanonical.content
                            , baseUpdatedAt = edit.latestCanonical.updatedAt
                            , conflict = False
                            , saving = False
                            , error = Nothing
                            , activeRequest = Nothing
                        }
                    )
    }


handleUpdateResponse : ObservationMutationRequest -> Result Http.Error Api.Observation -> Model -> ( Model, Cmd Msg )
handleUpdateResponse request result model =
    if not (mutationResponseMatches request model) then
        ( model, Cmd.none )

    else
        case ( model.observations.edit, result ) of
            ( Just edit, Ok observation ) ->
                if observation.id /= request.observationId || observation.workspaceId /= request.workspaceId || not (sameObservationProvenance edit.latestCanonical observation) then
                    ( finishEditFailure "The server returned mismatched immutable provenance. Retry after reloading." edit model, Cmd.none )

                else if observationIsOlderThan edit.latestCanonical observation then
                    ( updateObservation
                        (\state ->
                            { state
                                | edit =
                                    Just
                                        { edit
                                            | latestCanonical = edit.latestCanonical
                                            , conflict = True
                                            , saving = False
                                            , error = Just "A newer version arrived while this save was in flight. Choose how to continue."
                                            , activeRequest = Nothing
                                        }
                            }
                        )
                        model
                    , Cmd.none
                    )

                else
                    let
                        accepted =
                            updateObservation
                                (applyCanonicalObservation observation
                                    >> (\state -> { state | edit = Nothing })
                                )
                                model

                        ( refreshing, refreshCmd ) =
                            refreshActiveResults accepted

                        ( toasted, toastCmd ) =
                            addToast Success "Observation content updated" refreshing
                    in
                    ( toasted, Cmd.batch [ refreshCmd, toastCmd ] )

            ( Just edit, Err (Http.BadStatus 404) ) ->
                deletedAfterMutation "This observation was already deleted." request.observationId model

            ( Just edit, Err _ ) ->
                ( finishEditFailure "Failed to update observation. Your draft is preserved; retry when ready." edit model, Cmd.none )

            _ ->
                ( model, Cmd.none )


finishEditFailure : String -> ObservationEditState -> Model -> Model
finishEditFailure message edit model =
    updateObservation
        (\state -> { state | edit = Just { edit | saving = False, error = Just message, activeRequest = Nothing } })
        model


openDeleteConfirmation : Model -> ( Model, Cmd Msg )
openDeleteConfirmation model =
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
startResultReload mode sessionEpoch workspaceId state =
    { state
        | items = Dict.empty
        , orderedIds = []
        , hasMore = False
        , loading = True
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
                        }

            prepared =
                updateObservation
                    (\state ->
                        { state
                            | requestMode = restored.requestMode
                            , subjectKind = restored.subjectKind
                            , subject = restored.subject
                            , selectedFacet = restored.selectedFacet
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
    case repositoryWorkspaceId model of
        Just workspaceId ->
            case model.observations.requestMode of
                ObservationFacetMode ->
                    let
                        state =
                            startFacetRefresh model.sessionRequestEpoch workspaceId model.observations

                        updated =
                            updateObservation (always state) model
                    in
                    fetchFacetPage 0 updated

                ObservationMatchMode ->
                    case model.observations.matchAppliedPaths of
                        (_ :: _) as paths ->
                            let
                                state =
                                    startMatchRefresh model.sessionRequestEpoch workspaceId model.observations

                                updated =
                                    updateObservation (always state) model
                            in
                            fetchMatchPage 0 paths updated

                        [] ->
                            ( model, Cmd.none )

                mode ->
                    let
                        state =
                            startResultRefresh mode model.sessionRequestEpoch workspaceId model.observations

                        updated =
                            updateObservation (always state) model
                    in
                    fetchPage 0 updated

        Nothing ->
            ( model, Cmd.none )


startResultRefresh : ObservationRequestMode -> Int -> String -> ObservationModel -> ObservationModel
startResultRefresh mode sessionEpoch workspaceId state =
    { state
        | loading = True
        , error = Nothing
        , requestGeneration = state.requestGeneration + 1
        , requestSessionEpoch = sessionEpoch
        , queryFingerprint = resultFingerprint mode sessionEpoch workspaceId state
        , expectedOffset = Just 0
        , nextOffset = 0
    }


startMatchRefresh : Int -> String -> ObservationModel -> ObservationModel
startMatchRefresh sessionEpoch workspaceId state =
    { state
        | loading = True
        , error = Nothing
        , requestGeneration = state.requestGeneration + 1
        , requestSessionEpoch = sessionEpoch
        , queryFingerprint = matchFingerprint sessionEpoch workspaceId state
        , expectedOffset = Just 0
        , nextOffset = 0
    }


startFacetReload : Int -> String -> ObservationModel -> ObservationModel
startFacetReload sessionEpoch workspaceId state =
    { state
        | requestMode = ObservationFacetMode
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
startFacetRefresh sessionEpoch workspaceId state =
    { state
        | facetLoading = True
        , facetError = Nothing
        , facetRequestGeneration = state.facetRequestGeneration + 1
        , facetRequestSessionEpoch = sessionEpoch
        , facetFingerprint = facetFingerprintFor sessionEpoch (facetQuery workspaceId 0 state)
        , facetExpectedOffset = Just 0
        , facetNextOffset = 0
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
            let
                state =
                    model.observations

                currentFingerprint =
                    facetFingerprintFor state.facetRequestSessionEpoch (facetQuery workspaceId 0 state)
            in
            if state.requestMode == ObservationFacetMode && not state.facetLoading && state.facetHasMore && state.facetExpectedOffset == Nothing && currentFingerprint == state.facetFingerprint then
                fetchFacetPage state.facetNextOffset model

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
listQuery workspaceId offset state =
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
    , query = nonEmpty state.query
    , limit = 50
    , offset = offset
    }


facetQuery : String -> Int -> ObservationModel -> Api.ObservationSubjectFacetQuery
facetQuery workspaceId offset state =
    { workspaceId = workspaceId
    , subjectKind = state.subjectKind
    , gitSha = nonEmpty state.gitSha
    , query = nonEmpty state.query
    , limit = 50
    , offset = offset
    }


matchQuery : String -> List String -> Int -> ObservationModel -> Api.ObservationMatchQuery
matchQuery workspaceId paths offset state =
    { workspaceId = workspaceId
    , paths = paths
    , subjectKind = state.subjectKind
    , gitSha = nonEmpty state.gitSha
    , query = nonEmpty state.query
    , limit = 50
    , offset = offset
    }


startMatchReload : Int -> String -> List String -> ObservationModel -> ObservationModel
startMatchReload sessionEpoch workspaceId paths state =
    let
        browseReturn =
            if state.requestMode == ObservationMatchMode then
                state.browseReturn

            else
                Just
                    { requestMode = state.requestMode
                    , subjectKind = state.subjectKind
                    , subject = state.subject
                    , selectedFacet = state.selectedFacet
                    }
    in
    { state
        | items = Dict.empty
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
        , query.query |> Maybe.withDefault ""
        , String.fromInt query.limit
        ]


matchFingerprint : Int -> String -> ObservationModel -> String
matchFingerprint sessionEpoch workspaceId state =
    String.join "\u{001F}"
        [ "match"
        , String.fromInt sessionEpoch
        , workspaceId
        , state.subjectKind |> Maybe.map Api.subjectKindToString |> Maybe.withDefault ""
        , state.gitSha |> String.trim
        , state.query |> String.trim
        , state.matchAppliedPaths
            |> List.map (\path -> String.fromInt (String.length path) ++ ":" ++ path)
            |> String.join ""
        , "50"
        ]


canLoadMore : String -> ObservationModel -> Bool
canLoadMore workspaceId state =
    not state.loading
        && state.hasMore
        && state.expectedOffset
        == Nothing
        && activeFingerprint workspaceId state
        == state.queryFingerprint


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
normalizeMatchPaths rawInput =
    let
        paths =
            rawInput
                |> String.lines
                |> List.map String.trim
                |> List.filter (not << String.isEmpty)
                |> deduplicateFirst
    in
    if List.isEmpty paths then
        Err "Enter at least one repository-relative file path."

    else if List.length paths > 256 then
        Err "Match repository files accepts at most 256 paths."

    else if List.sum (List.map utf8Bytes paths) > 262144 then
        Err "Match repository files accepts at most 262144 UTF-8 bytes in total."

    else
        case List.filter (not << isConcreteRepositoryPath) paths |> List.head of
            Just invalidPath ->
                Err ("Invalid concrete repository path: " ++ invalidPath)

            Nothing ->
                Ok paths


deduplicateFirst : List String -> List String
deduplicateFirst paths =
    List.foldl
        (\path unique ->
            if List.member path unique then
                unique

            else
                unique ++ [ path ]
        )
        []
        paths


isConcreteRepositoryPath : String -> Bool
isConcreteRepositoryPath path =
    not (String.startsWith "/" path)
        && not (String.contains "\\" path)
        && not (String.contains "*" path)
        && not (String.contains "?" path)
        && not (windowsAbsolute path)
        && utf8Bytes path
        <= 4096
        && List.all (not << controlCharacter) (String.toList path)
        && List.all (\segment -> not (String.isEmpty segment) && segment /= "." && segment /= "..") (String.split "/" path)


windowsAbsolute : String -> Bool
windowsAbsolute path =
    case String.toList path of
        drive :: ':' :: '/' :: _ ->
            asciiLetter drive

        _ ->
            False


asciiLetter : Char -> Bool
asciiLetter character =
    let
        code =
            Char.toCode character
    in
    (code >= Char.toCode 'A' && code <= Char.toCode 'Z')
        || (code >= Char.toCode 'a' && code <= Char.toCode 'z')


controlCharacter : Char -> Bool
controlCharacter character =
    let
        code =
            Char.toCode character
    in
    code < 32 || code == 127


utf8Bytes : String -> Int
utf8Bytes value =
    String.toList value
        |> List.map
            (\character ->
                let
                    code =
                        Char.toCode character
                in
                if code <= 0x7F then
                    1

                else if code <= 0x07FF then
                    2

                else if code <= 0xFFFF then
                    3

                else
                    4
            )
        |> List.sum


{-| Select by id rather than list membership: unified-search hits may not occur
on the active observations page or under its current filters.
-}
selectObservation : String -> Model -> ( Model, Cmd Msg )
selectObservation observationId model =
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
                                , selectedDetail = Nothing
                                , detailLoading = True
                                , detailError = Nothing
                                , activeDetailRequest = Just request
                                , nextDetailRequestToken = token + 1
                                , edit = Nothing
                                , deleteConfirmation = Nothing
                            }
                        )
                        model
            in
            ( updated
            , Cmd.batch
                [ Api.fetchObservation model.flags.apiUrl observationId (GotObservationDetail workspaceId observationId model.sessionRequestEpoch token)
                , replaceFragment updated
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
    if not (sameObservationProvenance candidate existing) then
        existing

    else
        case compareTimestamps candidate.updatedAt existing.updatedAt of
            Just GT ->
                candidate

            Just EQ ->
                if candidate.content == existing.content then
                    candidate

                else
                    existing

            _ ->
                existing


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


observationIsOlderThan : Api.Observation -> Api.Observation -> Bool
observationIsOlderThan current candidate =
    preferNewerObservation candidate current
        == current
        && candidate
        /= current


applyCanonicalObservation : Api.Observation -> ObservationModel -> ObservationModel
applyCanonicalObservation candidate state =
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
            existing
                |> Maybe.map (preferNewerObservation candidate)
                |> Maybe.withDefault candidate

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
    in
    { state
        | items = Dict.insert accepted.id accepted state.items
        , selectedDetail = selectedDetail
        , edit = edit
    }


reconcileEditWithCanonical : Api.Observation -> Maybe ObservationEditState -> Maybe ObservationEditState
reconcileEditWithCanonical observation maybeEdit =
    maybeEdit
        |> Maybe.map
            (\edit ->
                if edit.workspaceId /= observation.workspaceId || edit.observationId /= observation.id then
                    edit

                else if timestampsEquivalent observation.updatedAt edit.latestCanonical.updatedAt && observation.content == edit.latestCanonical.content then
                    edit

                else if edit.draft == edit.baseContent && not edit.saving then
                    { edit
                        | baseContent = observation.content
                        , baseUpdatedAt = observation.updatedAt
                        , draft = observation.content
                        , latestCanonical = observation
                        , conflict = False
                        , error = Nothing
                    }

                else
                    { edit
                        | latestCanonical = observation
                        , conflict = not (timestampsEquivalent observation.updatedAt edit.baseUpdatedAt) || observation.content /= edit.baseContent
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
        | items = Dict.remove observationId state.items
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
            updateObservation (removeObservation observationId) model
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
    }
        |> (\merged -> List.foldl applyCanonicalObservation merged observations)


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


viewObservations : Api.Workspace -> Model -> Html Msg
viewObservations workspace model =
    viewObservationsStateWithPermission (Permissions.canEditCurrentWorkspace model) workspace model.observations


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
        div [ class "observations-panel" ]
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
                    [ viewList state
                    , viewDetail canEdit state
                    ]
                ]
            , viewDeleteConfirmation canEdit state
            ]


viewFilters : ObservationModel -> Html Msg
viewFilters state =
    div [ class "filter-bar observation-filters" ]
        [ div [ class "filter-group observation-filter-group" ]
            [ label [ class "filter-label", for "observation-query" ] [ text "Search" ]
            , input
                [ id "observation-query"
                , class "form-input observation-filter-input"
                , type_ "search"
                , placeholder "Full-text search"
                , value state.query
                , onInput SetObservationQuery
                ]
                []
            ]
        , div [ class "filter-group observation-filter-group observation-filter-kind" ]
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
                                [ text (subjectKindLabel selectedFacet.subjectKind ++ ": " ++ selectedFacet.subject) ]
                            , p [ class "form-help" ] [ text "Exact results stay locked to this subject tuple. Browse/match kind changes do not alter it." ]
                            ]

                    Nothing ->
                        p [ class "form-error", attribute "role" "alert" ] [ text "No exact shared subject is selected." ]

            ObservationFacetMode ->
                p [ class "form-help observation-filter-mode-help" ] [ text "Shared subjects ignores the manual exact-subject filter and uses the shared search, kind, and Git SHA filters." ]

            ObservationMatchMode ->
                p [ class "form-help observation-filter-mode-help" ] [ text "Concrete path matching uses the shared search, kind, and Git SHA filters and ignores manual exact-subject filtering." ]
        , div [ class "filter-group observation-filter-group" ]
            [ label [ class "filter-label", for "observation-git-sha" ] [ text "Git SHA" ]
            , input
                [ id "observation-git-sha"
                , class "form-input observation-filter-input observation-filter-sha"
                , placeholder "Full Git SHA"
                , value state.gitSha
                , onInput SetObservationGitSha
                ]
                []
            ]
        , button [ class "btn btn-primary observation-filter-apply", type_ "button", onClick ApplyObservationFilters, disabled state.loading ] [ text "Apply filters" ]
        , div [ class "filter-group observation-filter-group observation-match-input" ]
            [ label [ class "filter-label", for "observation-match-paths" ] [ text "Match repository files" ]
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
            , p [ id "observation-match-help", class "form-help" ] [ text "Matches stored file subjects and globs. Paths are not expanded from the repository." ]
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
        ]


viewModeNavigation : ObservationModel -> Html Msg
viewModeNavigation state =
    let
        modeButton mode labelText =
            button
                [ classList
                    [ ( "btn", True )
                    , ( "btn-secondary", state.requestMode /= mode )
                    , ( "btn-primary", state.requestMode == mode )
                    , ( "observation-mode-button", True )
                    ]
                , type_ "button"
                , onClick (SetObservationBrowseMode mode)
                , attribute "aria-pressed"
                    (if state.requestMode == mode then
                        "true"

                     else
                        "false"
                    )
                ]
                [ text labelText ]

        headingText =
            case state.requestMode of
                ObservationFlatMode ->
                    "Flat observations"

                ObservationFacetMode ->
                    "Shared subjects"

                ObservationExactSubjectMode ->
                    "Exact subject results"

                ObservationMatchMode ->
                    "Concrete path matches"
    in
    section [ class "observation-mode-navigation", attribute "aria-labelledby" "observation-mode-heading" ]
        [ div [ class "observation-mode-actions", attribute "role" "group", attribute "aria-label" "Observation browse mode" ]
            [ modeButton ObservationFlatMode "Flat observations"
            , modeButton ObservationFacetMode "Shared subjects"
            ]
        , h2 [ id "observation-mode-heading", class "observation-mode-heading", tabindex -1 ] [ text headingText ]
        , p [ class "observation-mode-announcement", attribute "aria-live" "polite" ]
            [ text
                (case state.requestMode of
                    ObservationFacetMode ->
                        String.fromInt (List.length state.facetKeys) ++ " shared subjects loaded"

                    ObservationMatchMode ->
                        String.fromInt (List.length state.orderedIds) ++ " matching observations loaded"

                    _ ->
                        String.fromInt (List.length state.orderedIds) ++ " observations loaded"
                )
            ]
        ]


viewList : ObservationModel -> Html Msg
viewList state =
    case state.requestMode of
        ObservationFacetMode ->
            viewFacetCatalogue state

        ObservationMatchMode ->
            viewMatchResults state

        ObservationExactSubjectMode ->
            viewObservationResults "Exact subject observations" "No observations share this exact subject" state

        ObservationFlatMode ->
            viewObservationResults "Observations" "No observations found" state


viewObservationResults : String -> String -> ObservationModel -> Html Msg
viewObservationResults ariaLabel emptyHeading state =
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
        [ if state.loading && List.isEmpty observations then
            div [ class "loading-indicator observation-state observation-state-loading", attribute "role" "status", attribute "aria-live" "polite" ]
                [ text "Loading observations..." ]

          else
            text ""
        , case state.error of
            Just message ->
                div [ class "empty-state observation-state observation-state-error", attribute "role" "alert" ]
                    [ h3 [] [ text "Unable to load observations" ]
                    , p [] [ text message ]
                    ]

            Nothing ->
                if not state.loading && List.isEmpty observations then
                    div [ class "empty-state observation-state observation-state-empty" ]
                        [ h3 [] [ text emptyHeading ]
                        , p [] [ text "Try clearing or changing the active provenance filters." ]
                        ]

                else
                    div [ class "observation-list-rows", attribute "aria-label" ariaLabel ]
                        (List.map (viewObservationRow state.selectedId context) observations)
        , if state.hasMore then
            div [ class "observation-pagination" ]
                [ button [ class "btn btn-secondary observation-load-more", type_ "button", onClick LoadMoreObservations, disabled state.loading ]
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


viewFacetCatalogue : ObservationModel -> Html Msg
viewFacetCatalogue state =
    let
        facets =
            state.facetKeys |> List.filterMap (\key -> Dict.get key state.facets)
    in
    div [ id "observation-results", class "entity-list observation-list observation-facet-list", tabindex -1 ]
        [ if state.facetLoading && List.isEmpty facets then
            div [ class "loading-indicator observation-state observation-state-loading", attribute "role" "status", attribute "aria-live" "polite" ] [ text "Loading shared subjects..." ]

          else
            text ""
        , case state.facetError of
            Just message ->
                div [ class "empty-state observation-state observation-state-error", attribute "role" "alert" ]
                    [ h3 [] [ text "Unable to load shared subjects" ]
                    , p [] [ text message ]
                    ]

            Nothing ->
                if not state.facetLoading && List.isEmpty facets then
                    div [ class "empty-state observation-state observation-state-empty" ]
                        [ h3 [] [ text "No shared subjects found" ]
                        , p [] [ text "Try clearing or changing the shared search, kind, or Git SHA filters." ]
                        ]

                else
                    div [ class "observation-facet-rows", attribute "aria-label" "Shared subjects" ]
                        (List.map viewFacet facets)
        , if state.facetHasMore then
            div [ class "observation-pagination" ]
                [ button [ class "btn btn-secondary observation-facet-load-more", type_ "button", onClick LoadMoreObservationFacets, disabled state.facetLoading ]
                    [ text
                        (if state.facetLoading then
                            "Loading..."

                         else
                            "Load more shared subjects"
                        )
                    ]
                ]

          else
            text ""
        ]


viewFacet : Api.ObservationSubjectFacet -> Html Msg
viewFacet facet =
    button
        [ id ("observation-facet-" ++ domToken (facetKey facet.subjectKind facet.subject))
        , class "card observation-facet-card"
        , type_ "button"
        , onClick (SelectObservationFacet facet.subjectKind facet.subject)
        , attribute "aria-label"
            ("Open " ++ subjectKindLabel facet.subjectKind ++ " subject " ++ facet.subject ++ " with " ++ String.fromInt facet.observationCount ++ " observations")
        ]
        [ div [ class "card-header observation-card-header" ]
            [ span [ class "entity-type-label observation-kind" ] [ text (subjectKindLabel facet.subjectKind) ]
            , span [ class "observation-subject" ] [ text facet.subject ]
            ]
        , div [ class "observation-facet-meta" ]
            [ strong [] [ text (String.fromInt facet.observationCount ++ " observations") ]
            , span [ class "card-meta" ] [ text ("Latest update: " ++ formatDate facet.latestUpdatedAt) ]
            ]
        ]


viewMatchResults : ObservationModel -> Html Msg
viewMatchResults state =
    let
        paths =
            state.matchAppliedPaths

        pathGroups =
            groupPathMatches paths state.orderedIds state.matchEvidence

        hasAnyGroups =
            List.any (not << List.isEmpty << .subjectGroups) pathGroups
    in
    div [ id "observation-results", class "entity-list observation-list observation-match-results", tabindex -1 ]
        [ if state.loading && List.isEmpty state.orderedIds then
            div [ class "loading-indicator observation-state observation-state-loading", attribute "role" "status", attribute "aria-live" "polite" ] [ text "Matching repository files..." ]

          else
            text ""
        , case state.error of
            Just message ->
                div [ class "empty-state observation-state observation-state-error", attribute "role" "alert" ]
                    [ h3 [] [ text "Unable to match repository files" ]
                    , p [] [ text message ]
                    ]

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
                        ++ List.map (viewPathGroup state) pathGroups
                    )
        , if state.loading && not (List.isEmpty state.orderedIds) then
            p [ class "observation-match-loading-more", attribute "role" "status", attribute "aria-live" "polite" ] [ text "Loading more path matches..." ]

          else
            text ""
        , if state.hasMore then
            div [ class "observation-pagination" ]
                [ button [ class "btn btn-secondary observation-load-more", type_ "button", onClick LoadMoreObservations, disabled state.loading ]
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


viewPathGroup : ObservationModel -> ObservationPathGroup -> Html Msg
viewPathGroup state pathGroup =
    section [ class "observation-path-group", attribute "aria-labelledby" ("observation-path-" ++ domToken pathGroup.path) ]
        [ h3 [ id ("observation-path-" ++ domToken pathGroup.path), class "observation-path-heading" ] [ text pathGroup.path ]
        , if List.isEmpty pathGroup.subjectGroups then
            p [ class "observation-path-empty" ] [ text "No loaded matches for this path." ]

          else
            div [ class "observation-subject-groups" ] (List.map (viewSubjectGroup state pathGroup.path) pathGroup.subjectGroups)
        ]


viewSubjectGroup : ObservationModel -> String -> ObservationSubjectGroup -> Html Msg
viewSubjectGroup state path group =
    let
        expanded =
            Dict.get group.key state.expandedMatchGroups |> Maybe.withDefault False

        panelId =
            "observation-group-panel-" ++ domToken group.key

        observations =
            group.observationIds |> List.filterMap (\observationId -> Dict.get observationId state.items)
    in
    section [ class "observation-subject-group" ]
        [ div [ class "observation-subject-group-header" ]
            [ button
                [ class "observation-subject-group-toggle"
                , type_ "button"
                , onClick (ToggleObservationMatchGroup group.key)
                , attribute "aria-expanded"
                    (if expanded then
                        "true"

                     else
                        "false"
                    )
                , attribute "aria-controls" panelId
                ]
                [ span [ class "entity-type-label observation-kind" ] [ text (subjectKindLabel group.subjectKind) ]
                , span [ class "observation-subject" ] [ text group.subject ]
                , span [ class "observation-loaded-count" ] [ text (String.fromInt (List.length group.observationIds) ++ " loaded") ]
                ]
            , button
                [ class "observation-subject-copy"
                , type_ "button"
                , onClick (CopyObservationSubject group.subject)
                , attribute "aria-label" ("Copy matching subject " ++ group.subject)
                ]
                [ text "Copy subject" ]
            ]
        , if expanded then
            div [ id panelId, class "observation-subject-group-cards", attribute "aria-label" ("Loaded observations for " ++ group.subject) ]
                (List.map (viewObservationRow state.selectedId group.key) observations)

          else
            text ""
        ]


viewObservationRow : Maybe String -> String -> Api.Observation -> Html Msg
viewObservationRow selectedId context observation =
    let
        isSelected =
            selectedId == Just observation.id
    in
    button
        [ id (observationCardDomId context observation.id)
        , classList
            [ ( "card", True )
            , ( "observation-card", True )
            , ( "observation-card-selected", isSelected )
            ]
        , type_ "button"
        , attribute "aria-current"
            (if isSelected then
                "true"

             else
                "false"
            )
        , onClick (SelectObservation observation.id)
        ]
        [ div [ class "card-header observation-card-header" ]
            [ span [ class "entity-type-label observation-kind" ] [ text (subjectKindLabel observation.subjectKind) ]
            , span [ class "observation-subject" ] [ text observation.subject ]
            , if List.length observation.subjects > 1 then
                span [ class "card-meta observation-subject-count", attribute "aria-label" (String.fromInt (List.length observation.subjects - 1) ++ " additional subjects") ] [ text ("+" ++ String.fromInt (List.length observation.subjects - 1)) ]

              else
                text ""
            ]
        , div [ class "card-body observation-summary" ] [ text observation.content ]
        , div [ class "card-meta-group observation-card-meta" ]
            [ div [ class "card-meta-row" ]
                [ span [ class "card-meta observation-sha" ] [ text ("Git SHA: " ++ observation.gitSha) ]
                , span [ class "card-meta" ] [ text ("Created: " ++ formatDate observation.createdAt) ]
                ]
            ]
        ]


viewDetail : Bool -> ObservationModel -> Html Msg
viewDetail canEdit state =
    case state.selectedId of
        Nothing ->
            text ""

        Just _ ->
            section [ id "observation-detail", class "observation-detail", attribute "aria-labelledby" "observation-detail-heading" ]
                [ h3 [ id "observation-detail-heading", class "observation-detail-heading" ] [ text "Observation detail" ]
                , if state.detailLoading then
                    div [ class "loading-indicator observation-state observation-detail-state", attribute "role" "status", attribute "aria-live" "polite" ] [ text "Loading detail..." ]

                  else
                    text ""
                , case state.detailError of
                    Just message ->
                        div [ class "empty-state observation-state observation-state-error observation-detail-state", attribute "role" "alert" ] [ text message ]

                    Nothing ->
                        case state.selectedDetail of
                            Just observation ->
                                article [ class "card observation-detail-card" ]
                                    [ viewDetailContent canEdit observation state.edit
                                    , dl [ class "observation-detail-meta" ]
                                        [ viewDetailMeta "Workspace ID" observation.workspaceId "observation-detail-workspace"
                                        , viewSubjects observation.subjects
                                        , viewDetailMeta "Git SHA" observation.gitSha "observation-detail-sha"
                                        , viewDetailMeta "Created" (formatDate observation.createdAt) ""
                                        , viewDetailMeta "Updated" (formatDate observation.updatedAt) ""
                                        ]
                                    , if canEdit && state.edit == Nothing then
                                        div [ class "observation-detail-actions" ]
                                            [ button [ id "observation-edit", class "btn btn-secondary", type_ "button", onClick StartObservationEdit ] [ text "Edit content" ]
                                            , button [ id "observation-delete", class "btn btn-danger", type_ "button", onClick OpenObservationDelete ] [ text "Delete observation" ]
                                            ]

                                      else
                                        text ""
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
                        observationContentError edit.draft
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
                    , p [ id "observation-edit-help", class "form-help" ] [ text "Only content can be edited. Workspace, subjects, Git SHA, subject order, and timestamps are immutable provenance." ]
                    , case validationError of
                        Just message ->
                            p [ id "observation-edit-status", class "form-error", attribute "role" "alert" ] [ text message ]

                        Nothing ->
                            if edit.conflict then
                                div [ id "observation-edit-status", class "observation-edit-conflict", attribute "role" "alert" ]
                                    [ p []
                                        [ text
                                            (Maybe.withDefault
                                                "This observation changed elsewhere. Your draft is preserved; choose how to continue before saving."
                                                edit.error
                                            )
                                        ]
                                    , div [ class "observation-conflict-actions" ]
                                        [ button [ class "btn btn-secondary", type_ "button", onClick ReloadObservationEdit, disabled edit.saving ] [ text "Use latest version" ]
                                        , button [ class "btn btn-secondary", type_ "button", onClick RebaseObservationEdit, disabled edit.saving ] [ text "Keep my draft" ]
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
                            , disabled (edit.saving || edit.conflict || validationError /= Nothing || edit.draft == edit.baseContent)
                            ]
                            [ text
                                (if edit.saving then
                                    "Saving..."

                                 else
                                    "Save content"
                                )
                            ]
                        , button [ class "btn btn-secondary", type_ "button", onClick CancelObservationEdit, disabled edit.saving ] [ text "Cancel" ]
                        ]
                    ]

            else
                p [ class "observation-detail-content" ] [ text observation.content ]

        Nothing ->
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
        , dd [ classList [ ( "observation-detail-meta-value", True ), ( valueClass, not (String.isEmpty valueClass) ) ] ] [ text valueText ]
        ]


viewSubjects : List Api.ObservationSubject -> Html Msg
viewSubjects subjects =
    div [ class "observation-detail-meta-row observation-detail-subjects" ]
        [ dt [ class "observation-detail-meta-label" ] [ text "Subjects" ]
        , dd [ class "observation-detail-meta-value observation-detail-subject" ]
            (List.map viewSubject subjects)
        ]


viewSubject : Api.ObservationSubject -> Html Msg
viewSubject subject =
    div [ class "observation-subject-row" ]
        [ span [ class "entity-type-label observation-kind" ] [ text (subjectKindLabel subject.subjectKind) ]
        , button
            [ class "observation-subject-copy"
            , type_ "button"
            , onClick (CopyObservationSubject subject.subject)
            , attribute "aria-label" ("Copy repository subject " ++ subject.subject)
            ]
            [ text subject.subject ]
        ]


subjectKindLabel : Api.SubjectKind -> String
subjectKindLabel subjectKind =
    case subjectKind of
        Api.SubjectFile ->
            "File"

        Api.SubjectGlob ->
            "Glob"
