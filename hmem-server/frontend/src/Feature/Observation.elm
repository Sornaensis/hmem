module Feature.Observation exposing (canLoadMore, clearSelection, detailResponseMatches, init, listQuery, matchQuery, matchResponseMatches, normalizeMatchPaths, queryFingerprint, reload, selectObservation, selectionForTab, startReload, update, viewObservations, viewObservationsState)

import Api
import Char
import Dict
import Helpers exposing (formatDate, replaceFragment)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
    , gitSha = ""
    , requestMode = ObservationListMode
    , matchPathsInput = ""
    , matchValidationError = Nothing
    , matchEvidence = Dict.empty
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

        SetObservationMatchPaths value ->
            ( updateObservation (\state -> { state | matchPathsInput = value, matchValidationError = Nothing }) model, Cmd.none )

        ApplyObservationMatch ->
            matchObservations model

        ClearObservationMatch ->
            reload
                (updateObservation
                    (\state ->
                        { state
                            | requestMode = ObservationListMode
                            , matchPathsInput = ""
                            , matchValidationError = Nothing
                            , matchEvidence = Dict.empty
                        }
                    )
                    model
                )

        LoadMoreObservations ->
            case repositoryWorkspaceId model of
                Just workspaceId ->
                    if canLoadMore workspaceId model.observations then
                        case model.observations.requestMode of
                            ObservationListMode ->
                                fetchPage model.observations.nextOffset model

                            ObservationMatchMode ->
                                case normalizeMatchPaths model.observations.matchPathsInput of
                                    Ok paths ->
                                        fetchMatchPage model.observations.nextOffset paths model

                                    Err _ ->
                                        ( model, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SelectObservation observationId ->
            selectObservation observationId model

        CopyObservationSubject subject ->
            let
                ( updated, toastCmd ) =
                    addToast Success "Repository subject copied to clipboard" model
            in
            ( updated, Cmd.batch [ copyToClipboard subject, toastCmd ] )

        GotObservationDetail workspaceId observationId token result ->
            if detailResponseMatches workspaceId observationId token model.selectedWorkspaceId model.observations then
                case result of
                    Ok observation ->
                        ( updateObservation (\state -> { state | selectedDetail = Just observation, detailLoading = False, activeDetailRequest = Nothing }) model, Cmd.none )

                    Err _ ->
                        ( updateObservation (\state -> { state | detailLoading = False, detailError = Just "Failed to load observation detail.", activeDetailRequest = Nothing }) model, Cmd.none )

            else
                ( model, Cmd.none )

        GotObservationMatches workspaceId generation fingerprint offset result ->
            if model.selectedWorkspaceId /= Just workspaceId || not (matchResponseMatches generation fingerprint offset model.observations) then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        ( updateObservation (mergeMatchPage offset paginated) model, Cmd.none )

                    Err _ ->
                        ( updateObservation (\state -> { state | loading = False, error = Just "Failed to match repository files.", expectedOffset = Nothing }) model, Cmd.none )

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
        , requestMode = ObservationListMode
        , matchPathsInput = ""
        , matchValidationError = Nothing
        , matchEvidence = Dict.empty
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
                            startMatchReload workspaceId paths model.observations

                        updated =
                            updateObservation (always state) model
                    in
                    fetchMatchPage 0 paths updated


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
                (GotObservationMatches workspaceId state.requestGeneration state.queryFingerprint offset)
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
    , subjectKind = state.subjectKind
    , subject = nonEmpty state.subject
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


startMatchReload : String -> List String -> ObservationModel -> ObservationModel
startMatchReload workspaceId paths state =
    let
        nextGeneration =
            state.requestGeneration + 1

        fingerprint =
            matchFingerprint workspaceId state
    in
    { state
        | items = Dict.empty
        , orderedIds = []
        , hasMore = False
        , loading = True
        , error = Nothing
        , requestMode = ObservationMatchMode
        , matchValidationError = Nothing
        , matchEvidence = Dict.empty
        , requestGeneration = nextGeneration
        , queryFingerprint = fingerprint
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


matchFingerprint : String -> ObservationModel -> String
matchFingerprint workspaceId state =
    String.join "\u{001F}"
        [ "match"
        , workspaceId
        , state.subjectKind |> Maybe.map Api.subjectKindToString |> Maybe.withDefault ""
        , state.gitSha |> String.trim
        , state.query |> String.trim
        , state.matchPathsInput
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
        ObservationListMode ->
            queryFingerprint (listQuery workspaceId 0 state)

        ObservationMatchMode ->
            matchFingerprint workspaceId state


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
        && utf8Bytes path <= 4096
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

                else if code <= 0x7FF then
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


observationResponseMatches : Int -> String -> Int -> ObservationModel -> Bool
observationResponseMatches generation fingerprint offset state =
    state.requestGeneration == generation
        && state.queryFingerprint == fingerprint
        && state.expectedOffset == Just offset


matchResponseMatches : Int -> String -> Int -> ObservationModel -> Bool
matchResponseMatches generation fingerprint offset state =
    state.requestMode == ObservationMatchMode
        && observationResponseMatches generation fingerprint offset state


mergeMatchPage : Int -> Api.PaginatedResult Api.ObservationMatch -> ObservationModel -> ObservationModel
mergeMatchPage offset paginated state =
    let
        matchesById =
            List.foldl (\match evidence -> Dict.insert match.observation.id match evidence) state.matchEvidence paginated.items

        observations =
            List.map .observation paginated.items

        receivedIds =
            List.map .id observations

        orderedIds =
            if offset == 0 then
                receivedIds

            else
                state.orderedIds ++ List.filter (\observationId -> not (List.member observationId state.orderedIds)) receivedIds
    in
    { state
        | items = List.foldl (\observation items -> Dict.insert observation.id observation items) state.items observations
        , orderedIds = orderedIds
        , hasMore = paginated.hasMore
        , loading = False
        , error = Nothing
        , expectedOffset = Nothing
        , nextOffset = offset + List.length paginated.items
        , matchEvidence = matchesById
    }


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
            [ label [ class "filter-label", for "observation-subject" ] [ text "Exact subject (list mode; matches any subject)" ]
            , input
                [ id "observation-subject"
                , class "form-input observation-filter-input"
                , placeholder "Path or glob"
                , value state.subject
                , onInput SetObservationSubject
                , disabled (state.requestMode == ObservationMatchMode)
                ]
                []
            , if state.requestMode == ObservationMatchMode then
                p [ class "form-help" ] [ text "Exact subject filtering is unavailable while matching files; the match endpoint accepts only kind, Git SHA, and text filters." ]

              else
                text ""
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
        , if state.requestMode == ObservationMatchMode || not (String.isEmpty state.matchPathsInput) then
            button [ class "btn btn-secondary observation-match-clear", type_ "button", onClick ClearObservationMatch, disabled state.loading ] [ text "Clear match" ]

          else
            text ""
        ]


viewList : ObservationModel -> Html Msg
viewList state =
    let
        observations =
            state.orderedIds |> List.filterMap (\observationId -> Dict.get observationId state.items)
    in
    div [ class "entity-list observation-list" ]
        [ if state.loading && List.isEmpty observations then
            div [ class "loading-indicator observation-state observation-state-loading", attribute "role" "status", attribute "aria-live" "polite" ]
                [ text
                    (if state.requestMode == ObservationMatchMode then
                        "Matching repository files..."

                     else
                        "Loading observations..."
                    )
                ]

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
                        [ h3 [] [ text (if state.requestMode == ObservationMatchMode then "No matching observations" else "No observations found") ]
                        , p [] [ text (if state.requestMode == ObservationMatchMode then "Try different concrete repository paths or clear the match." else "Try clearing or changing the exact provenance filters.") ]
                        ]

                else
                    div [ class "observation-list-rows", attribute "aria-label" "Observations" ]
                        (List.map (viewObservationRow state.selectedId state.matchEvidence) observations)
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


viewObservationRow : Maybe String -> Dict.Dict String Api.ObservationMatch -> Api.Observation -> Html Msg
viewObservationRow selectedId evidence observation =
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
            , if List.length observation.subjects > 1 then
                span [ class "card-meta observation-subject-count", attribute "aria-label" (String.fromInt (List.length observation.subjects - 1) ++ " additional subjects") ] [ text ("+" ++ String.fromInt (List.length observation.subjects - 1)) ]

              else
                text ""
            ]
        , div [ class "card-body observation-summary" ] [ text observation.content ]
        , case Dict.get observation.id evidence of
            Just match ->
                div [ class "observation-match-evidence" ]
                    [ p [] [ text ("Matched files: " ++ String.join ", " match.matchedPaths) ]
                    , p [] [ text ("Matching subjects: " ++ String.join ", " (List.map .subject match.matchedSubjects)) ]
                    ]

            Nothing ->
                text ""
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
                                        [ viewSubjects observation.subjects
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
