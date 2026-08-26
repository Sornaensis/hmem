module Feature.Observation exposing (canLoadMore, clearSelection, detailResponseMatches, init, listQuery, queryFingerprint, reload, selectObservation, selectionForTab, startReload, update, viewObservations, viewObservationsState)

import Api
import Dict
import Helpers exposing (formatDate, replaceFragment)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
    , gitSha = ""
    , requestGeneration = 0
    , queryFingerprint = ""
    , expectedOffset = Nothing
    , nextOffset = 0
    , selectedId = Nothing
    , selectedDetail = Nothing
    , detailLoading = False
    , detailError = Nothing
    , activeDetailRequest = Nothing
    , nextDetailRequestToken = 1
    }


clearSelection : ObservationModel -> ObservationModel
clearSelection state =
    { state | selectedId = Nothing, selectedDetail = Nothing, detailLoading = False, detailError = Nothing, activeDetailRequest = Nothing }


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
            reload model

        LoadMoreObservations ->
            case repositoryWorkspaceId model of
                Just workspaceId ->
                    if canLoadMore workspaceId model.observations then
                        fetchPage model.observations.nextOffset model

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SelectObservation observationId ->
            selectObservation observationId model

        GotObservationDetail workspaceId observationId token result ->
            if detailResponseMatches workspaceId observationId token model.selectedWorkspaceId model.observations then
                case result of
                    Ok observation ->
                        ( updateObservation (\state -> { state | selectedDetail = Just observation, detailLoading = False, activeDetailRequest = Nothing }) model, Cmd.none )

                    Err _ ->
                        ( updateObservation (\state -> { state | detailLoading = False, detailError = Just "Failed to load observation detail.", activeDetailRequest = Nothing }) model, Cmd.none )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


{-| Reset a result set before issuing page zero. The generation and the complete
filter fingerprint make late filter responses harmless.
-}
startReload : String -> ObservationModel -> ObservationModel
startReload workspaceId state =
    let
        nextGeneration =
            state.requestGeneration + 1

        fingerprint =
            queryFingerprint (listQuery workspaceId 0 state)
    in
    { state
        | items = Dict.empty
        , orderedIds = []
        , hasMore = False
        , loading = True
        , error = Nothing
        , requestGeneration = nextGeneration
        , queryFingerprint = fingerprint
        , expectedOffset = Just 0
        , nextOffset = 0
    }


reload : Model -> ( Model, Cmd Msg )
reload model =
    case repositoryWorkspaceId model of
        Just workspaceId ->
            let
                state =
                    startReload workspaceId model.observations

                updated =
                    updateObservation (always state) model
            in
            fetchPage 0 updated

        Nothing ->
            ( updateObservation (\state -> { state | loading = False, expectedOffset = Nothing }) model, Cmd.none )


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
    , subjectKind = state.subjectKind
    , subject = nonEmpty state.subject
    , gitSha = nonEmpty state.gitSha
    , query = nonEmpty state.query
    , limit = 50
    , offset = offset
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


canLoadMore : String -> ObservationModel -> Bool
canLoadMore workspaceId state =
    not state.loading
        && state.hasMore
        && state.expectedOffset
        == Nothing
        && queryFingerprint (listQuery workspaceId 0 state)
        == state.queryFingerprint


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
                    { workspaceId = workspaceId, observationId = observationId, token = token }

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
                            }
                        )
                        model
            in
            ( updated
            , Cmd.batch
                [ Api.fetchObservation model.flags.apiUrl observationId (GotObservationDetail workspaceId observationId token)
                , replaceFragment updated
                ]
            )

        Nothing ->
            ( model, Cmd.none )


detailResponseMatches : String -> String -> Int -> Maybe String -> ObservationModel -> Bool
detailResponseMatches workspaceId observationId token selectedWorkspaceId state =
    selectedWorkspaceId
        == Just workspaceId
        && state.selectedId
        == Just observationId
        && state.activeDetailRequest
        == Just { workspaceId = workspaceId, observationId = observationId, token = token }


updateObservation : (ObservationModel -> ObservationModel) -> Model -> Model
updateObservation fn model =
    { model | observations = fn model.observations }


viewObservations : Api.Workspace -> Model -> Html Msg
viewObservations workspace model =
    viewObservationsState workspace model.observations


viewObservationsState : Api.Workspace -> ObservationModel -> Html Msg
viewObservationsState workspace state =
    if workspace.workspaceType /= Api.Repository then
        div [ class "empty-state observation-state observation-state-unavailable" ]
            [ h3 [] [ text "Observations unavailable" ]
            , p [] [ text "Observations are available only for repository workspaces." ]
            ]

    else
        div [ class "observations-panel" ]
            [ viewFilters state
            , div
                [ classList
                    [ ( "observation-layout", True )
                    , ( "observation-layout-with-detail", state.selectedId /= Nothing )
                    ]
                ]
                [ viewList state
                , viewDetail state
                ]
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
            [ label [ class "filter-label", for "observation-subject-kind" ] [ text "Subject kind" ]
            , select [ id "observation-subject-kind", class "filter-select observation-filter-select", onInput SetObservationSubjectKind ]
                [ option [ value "", selected (state.subjectKind == Nothing) ] [ text "All subjects" ]
                , option [ value "file", selected (state.subjectKind == Just Api.SubjectFile) ] [ text "Files" ]
                , option [ value "glob", selected (state.subjectKind == Just Api.SubjectGlob) ] [ text "Globs" ]
                ]
            ]
        , div [ class "filter-group observation-filter-group" ]
            [ label [ class "filter-label", for "observation-subject" ] [ text "Exact subject" ]
            , input
                [ id "observation-subject"
                , class "form-input observation-filter-input"
                , placeholder "Path or glob"
                , value state.subject
                , onInput SetObservationSubject
                ]
                []
            ]
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
        ]


viewList : ObservationModel -> Html Msg
viewList state =
    let
        observations =
            state.orderedIds |> List.filterMap (\observationId -> Dict.get observationId state.items)
    in
    div [ class "entity-list observation-list" ]
        [ if state.loading && List.isEmpty observations then
            div [ class "loading-indicator observation-state observation-state-loading", attribute "role" "status", attribute "aria-live" "polite" ] [ text "Loading observations..." ]

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
                        [ h3 [] [ text "No observations found" ]
                        , p [] [ text "Try clearing or changing the exact provenance filters." ]
                        ]

                else
                    div [ class "observation-list-rows", attribute "aria-label" "Observations" ]
                        (List.map (viewObservationRow state.selectedId) observations)
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


viewObservationRow : Maybe String -> Api.Observation -> Html Msg
viewObservationRow selectedId observation =
    let
        isSelected =
            selectedId == Just observation.id
    in
    button
        [ id ("entity-" ++ observation.id)
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
            ]
        , div [ class "card-body observation-summary" ] [ text observation.content ]
        , div [ class "card-meta-group observation-card-meta" ]
            [ div [ class "card-meta-row" ]
                [ span [ class "card-meta observation-sha" ] [ text ("Git SHA: " ++ observation.gitSha) ]
                , span [ class "card-meta" ] [ text ("Created: " ++ formatDate observation.createdAt) ]
                ]
            ]
        ]


viewDetail : ObservationModel -> Html Msg
viewDetail state =
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
                                    [ p [ class "observation-detail-content" ] [ text observation.content ]
                                    , dl [ class "observation-detail-meta" ]
                                        [ viewDetailMeta "Subject kind" (subjectKindLabel observation.subjectKind) ""
                                        , viewDetailMeta "Subject" observation.subject "observation-detail-subject"
                                        , viewDetailMeta "Git SHA" observation.gitSha "observation-detail-sha"
                                        , viewDetailMeta "Created" (formatDate observation.createdAt) ""
                                        , viewDetailMeta "Updated" (formatDate observation.updatedAt) ""
                                        ]
                                    ]

                            Nothing ->
                                text ""
                ]


viewDetailMeta : String -> String -> String -> Html Msg
viewDetailMeta labelText valueText valueClass =
    div [ class "observation-detail-meta-row" ]
        [ dt [ class "observation-detail-meta-label" ] [ text labelText ]
        , dd [ classList [ ( "observation-detail-meta-value", True ), ( valueClass, not (String.isEmpty valueClass) ) ] ] [ text valueText ]
        ]


subjectKindLabel : Api.SubjectKind -> String
subjectKindLabel subjectKind =
    case subjectKind of
        Api.SubjectFile ->
            "File"

        Api.SubjectGlob ->
            "Glob"
