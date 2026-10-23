module Helpers exposing (..)

import Api
import Browser.Dom
import Browser.Navigation as Nav
import Char
import Dict exposing (Dict)
import Feature.ChangeStream
import Feature.FocusedFilters
import Html exposing (Html, button, text)
import Html.Attributes exposing (attribute, class, title, type_)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as Decode
import Json.Encode as Encode
import Ports exposing (requestLocalStorage, saveToLocalStorage)
import Permissions
import Process
import Set
import String
import Task as ElmTask
import Types exposing (..)
import Url


{-| A displayed identifier is its own copy control; the payload may be longer
than its abbreviated presentation. Native buttons provide Enter/Space support.
-}
copyableValue : String -> String -> String -> String -> Html Msg
copyableValue classes label value display =
    button
        [ class ("copyable-value " ++ (if String.contains " ID" label then "copyable-uuid " else "") ++ classes)
        , type_ "button"
        , title ("Copy " ++ label ++ ": " ++ value)
        , attribute "aria-label" ("Copy " ++ label ++ ": " ++ value)
        , stopPropagationOn "click" (Decode.succeed ( CopyId value, True ))
        , stopPropagationOn "keydown" (Decode.map (\key -> ( NoOp, key == "Enter" || key == " " )) (Decode.field "key" Decode.string))
        , stopPropagationOn "keyup" (Decode.map (\key -> ( NoOp, key == "Enter" || key == " " )) (Decode.field "key" Decode.string))
        ]
        [ text display ]


presentationWindowSize : Int
presentationWindowSize =
    25


presentationMaxPinned : Int
presentationMaxPinned =
    presentationWindowSize - 1


{-| A card presentation always reserves one ordinary row.  Focus/edit pins are
local to a branch, but can never consume the cursor's entire page. -}
presentationOrdinaryCapacity : Int -> Int
presentationOrdinaryCapacity pinCount =
    Basics.max 1 (presentationWindowSize - Basics.min presentationMaxPinned pinCount)


navigationPresentationWindow : (a -> String) -> Set.Set String -> Int -> List a -> List a
navigationPresentationWindow identify pinned offset values =
    let
        pinnedValues =
            values
                |> List.filter (identify >> (\entityId -> Set.member entityId pinned))

        ordinaryCapacity =
            presentationOrdinaryCapacity (List.length pinnedValues)

        ordinaryPageValues =
            values
                |> List.filter (\value -> not (Set.member (identify value) pinned))
                |> List.drop offset
                |> List.take ordinaryCapacity

        selectedIds =
            ordinaryPageValues
                |> List.map identify
                |> Set.fromList
                |> Set.union pinned
    in
    values
        |> List.filter (identify >> (\entityId -> Set.member entityId selectedIds))



-- FRAGMENT / URL


parseFragment : Maybe String -> { tab : WorkspaceTab, focus : Maybe ( String, String ), observationId : Maybe String }
parseFragment fragment =
    case fragment of
        Nothing ->
            { tab = ProjectsTab, focus = Nothing, observationId = Nothing }

        Just frag ->
            let
                pairs =
                    String.split "&" frag
                        |> List.filterMap
                            (\s ->
                                case String.split "=" s of
                                    [ k, v ] ->
                                        Maybe.map2 Tuple.pair (Url.percentDecode k) (Url.percentDecode v)

                                    _ ->
                                        Nothing
                            )

                tabVal =
                    List.foldl
                        (\( k, v ) acc ->
                            if k == "tab" then
                                v

                            else
                                acc
                        )
                        "projects"
                        pairs

                tab =
                    case tabVal of
                        "observations" ->
                            ObservationsTab

                        "timeline" ->
                            TimelineTab

                        "sessions" ->
                            SessionsTab

                        "audit" ->
                            AuditTab

                        "administration" ->
                            AdministrationTab

                        _ ->
                            ProjectsTab

                focusVal =
                    List.foldl
                        (\( k, v ) acc ->
                            if k == "focus" then
                                Just v

                            else
                                acc
                        )
                        Nothing
                        pairs

                focus =
                    focusVal
                        |> Maybe.andThen
                            (\v ->
                                case String.split ":" v of
                                    [ t, id ] ->
                                        Just ( t, id )

                                    _ ->
                                        Nothing
                            )

                observationId =
                    List.foldl
                        (\( key, value ) current ->
                            if key == "observation" && not (String.isEmpty value) then
                                Just value

                            else
                                current
                        )
                        Nothing
                        pairs
            in
            { tab = if observationId /= Nothing then ObservationsTab else tab, focus = focus, observationId = observationId }


buildFragment : WorkspaceTab -> Maybe ( String, String ) -> Maybe String -> String
buildFragment tab focus observationId =
    let
        tabPart =
            case tab of
                ProjectsTab ->
                    "tab=projects"

                ObservationsTab ->
                    "tab=observations"

                TimelineTab ->
                    "tab=timeline"

                SessionsTab ->
                    "tab=sessions"

                AuditTab ->
                    "tab=audit"

                AdministrationTab ->
                    "tab=administration"

        focusPart =
            case focus of
                Just ( t, id ) ->
                    "&focus=" ++ Url.percentEncode (t ++ ":" ++ id)

                Nothing ->
                    ""

        observationPart =
            case observationId of
                Just selectedId ->
                    "&observation=" ++ Url.percentEncode selectedId

                Nothing ->
                    ""
    in
    tabPart ++ focusPart ++ observationPart


pushUrl : Maybe Nav.Key -> String -> Cmd msg
pushUrl maybeKey url =
    case maybeKey of
        Just key ->
            Nav.pushUrl key url

        Nothing ->
            Cmd.none


replaceFragment : Model -> Cmd Msg
replaceFragment model =
    case ( model.selectedWorkspaceId, model.key ) of
        ( Just _, Just _ ) ->
            ElmTask.perform (\_ -> SynchronizeWorkspaceFragment) (ElmTask.succeed ())

        _ ->
            Cmd.none



-- FILTER PERSISTENCE


defaultObservationQuery : ObservationAppliedQuery
defaultObservationQuery =
    { requestMode = ObservationFlatMode, query = "", subjectQuery = "", subjectKind = Nothing, subject = "", selectedFacet = Nothing, gitSha = "", currentGitSha = "", historyGitSha = "", matchAppliedPaths = [] }


observationAppliedQuery : ObservationModel -> ObservationAppliedQuery
observationAppliedQuery state =
    Maybe.withDefault
        { requestMode = state.requestMode, query = state.query, subjectQuery = state.subjectQuery, subjectKind = state.subjectKind, subject = state.subject, selectedFacet = state.selectedFacet, gitSha = state.gitSha, currentGitSha = state.currentGitSha, historyGitSha = state.historyGitSha, matchAppliedPaths = if state.requestMode == ObservationMatchMode then state.matchAppliedPaths else [] }
        state.appliedQuery


restoreObservationQuery : ObservationAppliedQuery -> ObservationModel -> ObservationModel
restoreObservationQuery query state =
    { state | requestMode = query.requestMode, query = query.query, subjectQuery = query.subjectQuery, subjectKind = query.subjectKind, subject = query.subject, selectedFacet = query.selectedFacet, gitSha = query.gitSha, currentGitSha = query.currentGitSha, historyGitSha = query.historyGitSha, matchAppliedPaths = query.matchAppliedPaths, matchPathsInput = if query.requestMode == ObservationMatchMode then String.join "\n" query.matchAppliedPaths else state.matchPathsInput, appliedQuery = Just query, matchValidationError = Nothing, failedRequest = Nothing, pendingExcludedLink = Nothing
        , modeQueries = Dict.insert (case query.requestMode of
            ObservationFlatMode -> "flat"
            ObservationMatchMode -> "match"
            _ -> "tree") query.query state.modeQueries
        , modeSubjectQueries = Dict.insert (case query.requestMode of
            ObservationFlatMode -> "flat"
            ObservationMatchMode -> "match"
            _ -> "tree") query.subjectQuery state.modeSubjectQueries
        , filterDebounce = Nothing, nextFilterToken = state.nextFilterToken + 1, preferenceScroll = Nothing }


type alias ObservationUrlContext =
    { fragment : { tab : WorkspaceTab, focus : Maybe ( String, String ), observationId : Maybe String }
    , query : ObservationAppliedQuery
    , notice : Maybe String
    }


excludedObservationNotice : String
excludedObservationNotice =
    "This history entry excludes filters exceeding the 4096-byte link limit. Oversized filters stay active only in their original page state; they are not restored by Back, reload, or a shared link. Apply a smaller query to share its complete context."


observationUrlContext : Url.Url -> ObservationUrlContext
observationUrlContext url =
    let
        fallback message =
            { fragment = { tab = ObservationsTab, focus = Nothing, observationId = Nothing }, query = defaultObservationQuery, notice = Just message }

        parts =
            url.fragment |> Maybe.withDefault "" |> String.split "&"

        pair part =
            case String.split "=" part of
                [ key, value ] ->
                    Maybe.map2 Tuple.pair (Url.percentDecode key) (Url.percentDecode value)

                _ ->
                    Nothing

        pairs =
            List.filterMap pair parts

        keys =
            List.map Tuple.first pairs

        versioned =
            List.any (\key -> List.member key [ "ov", "ox", "oq", "search-jump" ]) keys
                || List.any (\part -> String.startsWith "ov=" part || String.startsWith "ox=" part || String.startsWith "oq=" part) parts

        lookup key =
            pairs |> List.filter (Tuple.first >> (==) key) |> List.head |> Maybe.map Tuple.second

        validFields =
            List.length pairs == List.length parts
                && List.length keys == Set.size (Set.fromList keys)
                && List.all (\key -> List.member key [ "tab", "focus", "observation", "ov", "oq", "ox", "search-jump", "session", "task" ]) keys
                && (lookup "tab" |> Maybe.map (\tab -> List.member tab [ "projects", "observations", "timeline", "sessions", "audit", "administration" ]) |> Maybe.withDefault True)
                && (lookup "focus" |> Maybe.map (\focus -> case String.split ":" focus of
                        [ kind, entityId ] -> List.member kind [ "project", "task", "memory" ] && not (String.isEmpty entityId)
                        _ -> False
                    ) |> Maybe.withDefault True)
                && (lookup "observation" |> Maybe.map (not << String.isEmpty) |> Maybe.withDefault True)
                && (lookup "search-jump" |> Maybe.map validObservationSearchJump |> Maybe.withDefault True)
    in
    if observationUrlBytes (Url.toString url) > 4096 then
        fallback "This Observation URL exceeds the 4096-byte limit. No filters or selection were restored."

    else if not versioned then
        { fragment = parseFragment url.fragment, query = defaultObservationQuery, notice = Nothing }

    else if not validFields then
        fallback "Invalid Observation link. No filters or selection were restored."

    else if (lookup "ox" |> Maybe.map (\nonce -> not (String.isEmpty nonce) && String.length nonce <= 160) |> Maybe.withDefault False) && lookup "ov" == Nothing && lookup "oq" == Nothing then
        { fragment = parseFragment url.fragment, query = defaultObservationQuery, notice = Just excludedObservationNotice }

    else if lookup "ov" /= Just "1" || lookup "ox" /= Nothing then
        fallback "Unsupported Observation link. No filters or selection were restored."

    else
        case lookup "oq" |> Maybe.map (Decode.decodeString observationQueryDecoder) of
            Just (Ok query) ->
                { fragment = parseFragment url.fragment, query = query, notice = Nothing }

            _ ->
                fallback "Invalid Observation link. No filters or selection were restored."


validObservationSearchJump : String -> Bool
validObservationSearchJump value =
    String.toInt value |> Maybe.map (\token -> token > 0 && token <= 2147483647 && String.fromInt token == value) |> Maybe.withDefault False


isObservationSearchUrl : Url.Url -> Bool
isObservationSearchUrl url =
    let
        context = observationUrlContext url
        marker = url.fragment |> Maybe.withDefault "" |> String.split "&" |> List.filterMap (\part -> case String.split "=" part of
            [ key, value ] -> if Url.percentDecode key == Just "search-jump" then Url.percentDecode value else Nothing
            _ -> Nothing)
    in
    (context.notice == Nothing || context.notice == Just excludedObservationNotice)
        && context.fragment.tab == ObservationsTab && context.fragment.observationId /= Nothing
        && (case marker of
            [ value ] -> validObservationSearchJump value
            _ -> False)


observationQueryDecoder : Decode.Decoder ObservationAppliedQuery
observationQueryDecoder =
    let
        kind =
            Decode.nullable Decode.string |> Decode.andThen (\value -> case value of
                Nothing -> Decode.succeed Nothing
                Just name -> case Api.subjectKindFromString name of
                    Just parsed -> Decode.succeed (Just parsed)
                    Nothing -> Decode.fail "Invalid subject kind"
                )

        mode =
            Decode.string |> Decode.andThen (\name -> case name of
                "flat" -> Decode.succeed ObservationFlatMode
                "facets" -> Decode.succeed ObservationFacetMode
                "tree" -> Decode.succeed ObservationFacetMode
                "exact" -> Decode.succeed ObservationExactSubjectMode
                "match" -> Decode.succeed ObservationMatchMode
                _ -> Decode.fail "Invalid mode"
                )

        tuple =
            Decode.map8 (\requestMode query subjectKind subject gitSha facetKind facetSubject paths ->
                ( { requestMode = requestMode, query = query, subjectQuery = "", subjectKind = subjectKind, subject = subject, gitSha = gitSha, currentGitSha = "", historyGitSha = "", selectedFacet = Maybe.map2 (\k s -> { subjectKind = k, subject = s }) facetKind facetSubject, matchAppliedPaths = paths }, (facetKind == Nothing) == (facetSubject == Nothing) ))
                (Decode.index 0 mode) (Decode.index 1 Decode.string) (Decode.index 2 kind) (Decode.index 3 Decode.string) (Decode.index 4 Decode.string) (Decode.index 5 kind) (Decode.index 6 (Decode.nullable Decode.string)) (Decode.index 7 (Decode.list Decode.string))
    in
    Decode.list Decode.value |> Decode.andThen (\values ->
        if not (List.member (List.length values) [ 8, 10, 11 ]) then Decode.fail "Invalid query tuple" else
        (if List.length values == 8 then tuple else
            Decode.map3 (\( query, paired ) current history -> ( { query | currentGitSha = current, historyGitSha = history }, paired )) tuple (Decode.index 8 Decode.string) (Decode.index 9 Decode.string))
        |> Decode.andThen (\( query, paired ) ->
            if List.length values == 11 then Decode.map (\subjects -> ( { query | subjectQuery = subjects }, paired )) (Decode.index 10 Decode.string)
            else Decode.succeed ( if List.member query.requestMode [ ObservationFacetMode, ObservationExactSubjectMode ] then { query | subjectQuery = query.query, query = "" } else query, paired ))
        |> Decode.andThen (\( query, pairedFacet ) ->
            if not pairedFacet || (query.requestMode == ObservationExactSubjectMode && query.selectedFacet == Nothing) then Decode.fail "Invalid exact facet" else
            if query.requestMode == ObservationMatchMode && not (List.isEmpty query.matchAppliedPaths) then
                case normalizeObservationPaths (String.join "\n" query.matchAppliedPaths) of
                    Ok paths -> Decode.succeed { query | matchAppliedPaths = paths }
                    Err message -> Decode.fail message
            else if query.requestMode /= ObservationMatchMode && not (List.isEmpty query.matchAppliedPaths) then Decode.fail "Paths require match mode" else Decode.succeed query
            )
        )


encodeObservationQuery : ObservationAppliedQuery -> String
encodeObservationQuery query =
    let
        kind value =
            value |> Maybe.map (Api.subjectKindToString >> Encode.string) |> Maybe.withDefault Encode.null

        mode =
            case query.requestMode of
                ObservationFlatMode -> "flat"
                ObservationFacetMode -> "tree"
                ObservationExactSubjectMode -> "exact"
                ObservationMatchMode -> "match"
    in
    Encode.list identity [ Encode.string mode, Encode.string query.query, kind query.subjectKind, Encode.string query.subject, Encode.string query.gitSha, kind (Maybe.map .subjectKind query.selectedFacet), query.selectedFacet |> Maybe.map (.subject >> Encode.string) |> Maybe.withDefault Encode.null, Encode.list Encode.string query.matchAppliedPaths, Encode.string query.currentGitSha, Encode.string query.historyGitSha, Encode.string query.subjectQuery ] |> Encode.encode 0 |> Url.percentEncode


observationUrl : Model -> String -> String
observationUrl model context =
    let
        url = model.url
    in
    Url.toString { url | path = "/workspace/" ++ Url.percentEncode (Maybe.withDefault "" model.selectedWorkspaceId), query = Nothing, fragment = Just (buildFragment model.activeTab model.focus.focusedEntity model.observations.selectedId ++ sessionUrlContext model ++ context) }


completeObservationUrl : Model -> Result String String
completeObservationUrl model =
    let
        url =
            observationUrl model ("&ov=1&oq=" ++ encodeObservationQuery (observationAppliedQuery model.observations))
    in
    if observationUrlBytes url <= 4096 then Ok url else Err excludedObservationNotice


observationHistoryUrl : Model -> String
observationHistoryUrl model =
    case completeObservationUrl model of
        Ok url -> url
        Err _ -> observationUrl model ("&ox=" ++ Url.percentEncode (model.flags.sessionId ++ "-" ++ String.fromInt model.sessionRequestEpoch ++ "-" ++ String.fromInt model.observations.nextLinkToken))


{-| Include the navigation marker before enforcing the public URL budget.
The bounded exclusion marker retains a truthful fallback for oversized filters.
-}
observationSearchHistoryUrl : Int -> Model -> String
observationSearchHistoryUrl token model =
    let
        marker = "&search-jump=" ++ String.fromInt (Basics.clamp 1 2147483647 token)
        complete = observationUrl model ("&ov=1&oq=" ++ encodeObservationQuery (observationAppliedQuery model.observations) ++ marker)
    in
    if observationUrlBytes complete <= 4096 then complete
    else observationUrl model ("&ox=search-" ++ String.fromInt (Basics.clamp 1 2147483647 token) ++ marker)


writeObservationHistory : Bool -> Model -> ( Model, Cmd Msg )
writeObservationHistory intentional model =
    let
        state = model.observations
        target = observationHistoryUrl model
        oversized = Result.toMaybe (completeObservationUrl model) == Nothing
        updated = { model | observations = { state | nextLinkToken = state.nextLinkToken + 1, pendingExcludedLink = if oversized then Just { url = target, workspaceId = Maybe.withDefault "" model.selectedWorkspaceId, sessionEpoch = model.sessionRequestEpoch, generation = state.requestGeneration, facetGeneration = state.facetRequestGeneration } else Nothing, linkNotice = if oversized then Just excludedObservationNotice else if intentional then Nothing else state.linkNotice } }
        command = case model.key of
            Just key -> if target == Url.toString model.url then Cmd.none else if intentional && not oversized then Nav.pushUrl key target else Nav.replaceUrl key target
            Nothing -> Cmd.none
    in
    ( updated, command )


localStorageKey : String -> String
localStorageKey wsId =
    "hmem-ws-" ++ wsId


globalSearchActor : Model -> String
globalSearchActor model =
    model.sessionContext |> Maybe.map (\session -> Encode.encode 0 (Encode.list Encode.string [ session.authMode, session.principal.actorType, session.principal.actorId, session.principal.authority, Maybe.withDefault "" session.principal.grantUserId ])) |> Maybe.withDefault ""


entitySearchPreferenceKey : String -> Model -> String
entitySearchPreferenceKey workspaceId model =
    "hmem-entity-search-v1:" ++ Url.percentEncode workspaceId ++ ":" ++ Url.percentEncode (globalSearchActor model)


loadEntitySearchPreferencesCmd : Model -> Cmd Msg
loadEntitySearchPreferencesCmd model =
    case model.selectedWorkspaceId of
        Just workspaceId ->
            if Permissions.canReadCurrentWorkspace model && not (String.isEmpty (globalSearchActor model)) then
                requestLocalStorage (entitySearchPreferenceKey workspaceId model)
            else Cmd.none
        Nothing -> Cmd.none


saveEntitySearchPreferencesCmd : Model -> Cmd Msg
saveEntitySearchPreferencesCmd model =
    case model.selectedWorkspaceId of
        Just workspaceId ->
            if Permissions.canReadCurrentWorkspace model && model.search.entityTypesActor == Just (globalSearchActor model) then
                saveToLocalStorage (Encode.object
                    [ ( "key", Encode.string (entitySearchPreferenceKey workspaceId model) )
                    , ( "value", Encode.object
                        [ ( "entitySearchPreference", Encode.bool True ), ( "workspaceId", Encode.string workspaceId )
                        , ( "actor", Encode.string (globalSearchActor model) )
                        , ( "types", Encode.list Encode.string (Maybe.withDefault [] model.search.entityTypes) ) ] ) ])
            else Cmd.none
        Nothing -> Cmd.none


restoreEntitySearchPreferences : Encode.Value -> Model -> Model
restoreEntitySearchPreferences json model =
    if not (Permissions.canReadCurrentWorkspace model) || model.search.globalActivated
        || (Decode.decodeValue (Decode.field "workspaceId" Decode.string) json |> Result.toMaybe) /= model.selectedWorkspaceId
        || Decode.decodeValue (Decode.field "actor" Decode.string) json /= Ok (globalSearchActor model) then model
    else
        case Decode.decodeValue (Decode.field "types" (Decode.list Decode.string)) json of
            Ok types ->
                let state = model.search in
                { model | search = { state | entityTypes = Just (List.filter (\kind -> List.member kind types) [ "project", "task", "observation" ]), entityTypesActor = Just (globalSearchActor model) } }
            Err _ -> model


encodeFilterState : Model -> Encode.Value
encodeFilterState original =
    let
        model = { original | search = Feature.FocusedFilters.normalSearch original }
    in
    Encode.object
        [ ( "workspaceId"
          , case model.selectedWorkspaceId of
                Just wsId ->
                    Encode.string wsId

                Nothing ->
                    Encode.null
          )
        , ( "timelineWindow", Encode.string model.timeline.histogramWindow )
        , ( "timelineBucket", Encode.string model.timeline.histogramBucket )
        , ( "searchQuery", Encode.string model.search.query )
        , ( "filterShowOnly"
          , Encode.string
                (case model.search.filterShowOnly of
                    ShowAll ->
                        "all"

                    ShowProjectsOnly ->
                        "projects"

                    ShowTasksOnly ->
                        "tasks"
                )
          )
        , ( "filterShowEmptyProjects", Encode.bool model.search.filterShowEmptyProjects )
        , ( "filterPriority", encodeFilterPriority model.search.filterPriority )
        , ( "filterProjectStatuses", Encode.list Encode.string model.search.filterProjectStatuses )
        , ( "filterTaskStatuses", Encode.list Encode.string model.search.filterTaskStatuses )
        , ( "filterMemoryTypes", Encode.list Encode.string model.search.filterMemoryTypes )
        , ( "filterImportance", encodeFilterPriority model.search.filterImportance )
        , ( "filterMemoryPinned"
          , case model.search.filterMemoryPinned of
                Just True ->
                    Encode.string "true"

                Just False ->
                    Encode.string "false"

                Nothing ->
                    Encode.null
          )
        , ( "filterMemoryActiveLinked", Encode.bool model.search.filterMemoryActiveLinked )
        , ( "filterTags", Encode.list Encode.string model.search.filterTags )
        , ( "collapsedNodes"
          , model.cards.collapsedNodes
                |> Dict.toList
                |> List.filter (\( k, v ) -> v && not (String.startsWith "group-" k))
                |> List.map (\( k, _ ) -> k)
                |> Encode.list Encode.string
          )
        ]


encodeFilterPriority : FilterPriority -> Encode.Value
encodeFilterPriority fp =
    case fp of
        AnyPriority ->
            Encode.null

        ExactPriority n ->
            Encode.object [ ( "exact", Encode.int n ) ]

        AbovePriority n ->
            Encode.object [ ( "above", Encode.int n ) ]

        BelowPriority n ->
            Encode.object [ ( "below", Encode.int n ) ]


decodeFilterPriority : Decode.Decoder FilterPriority
decodeFilterPriority =
    Decode.oneOf
        [ Decode.null AnyPriority
        , Decode.map ExactPriority (Decode.field "exact" Decode.int)
        , Decode.map AbovePriority (Decode.field "above" Decode.int)
        , Decode.map BelowPriority (Decode.field "below" Decode.int)
        ]


applyStoredFilters : Encode.Value -> Model -> Model
applyStoredFilters json model =
    let
        decodeShowOnly =
            Decode.string
                |> Decode.andThen
                    (\s ->
                        case s of
                            "projects" ->
                                Decode.succeed ShowProjectsOnly

                            "tasks" ->
                                Decode.succeed ShowTasksOnly

                            _ ->
                                Decode.succeed ShowAll
                    )

        decodePinned =
            Decode.oneOf
                [ Decode.null Nothing
                , Decode.string
                    |> Decode.andThen
                        (\s ->
                            if s == "true" then
                                Decode.succeed (Just True)

                            else
                                Decode.succeed (Just False)
                        )
                ]

        decodeCollapsed =
            Decode.list Decode.string
                |> Decode.map (\ids -> Dict.fromList (List.map (\id -> ( id, True )) (List.filter (not << String.startsWith "group-") ids)))

        currentSearch =
            Feature.FocusedFilters.normalSearch model

        updatedSearch =
            { currentSearch
                | query = Decode.decodeValue (Decode.field "searchQuery" Decode.string) json |> Result.withDefault currentSearch.query
                , filterShowOnly = Decode.decodeValue (Decode.field "filterShowOnly" decodeShowOnly) json |> Result.withDefault currentSearch.filterShowOnly
                , filterShowEmptyProjects = Decode.decodeValue (Decode.field "filterShowEmptyProjects" Decode.bool) json |> Result.withDefault currentSearch.filterShowEmptyProjects
                , filterPriority = Decode.decodeValue (Decode.field "filterPriority" decodeFilterPriority) json |> Result.withDefault currentSearch.filterPriority
                , filterProjectStatuses = Decode.decodeValue (Decode.field "filterProjectStatuses" (Decode.list Decode.string)) json |> Result.withDefault currentSearch.filterProjectStatuses
                , filterTaskStatuses = Decode.decodeValue (Decode.field "filterTaskStatuses" (Decode.list Decode.string)) json |> Result.withDefault currentSearch.filterTaskStatuses
                , filterMemoryTypes = Decode.decodeValue (Decode.field "filterMemoryTypes" (Decode.list Decode.string)) json |> Result.withDefault currentSearch.filterMemoryTypes
                , filterImportance = Decode.decodeValue (Decode.field "filterImportance" decodeFilterPriority) json |> Result.withDefault currentSearch.filterImportance
                , filterMemoryPinned = Decode.decodeValue (Decode.field "filterMemoryPinned" decodePinned) json |> Result.withDefault currentSearch.filterMemoryPinned
                , filterMemoryActiveLinked = Decode.decodeValue (Decode.field "filterMemoryActiveLinked" Decode.bool) json |> Result.withDefault currentSearch.filterMemoryActiveLinked
                , filterTags = Decode.decodeValue (Decode.field "filterTags" (Decode.list Decode.string)) json |> Result.withDefault currentSearch.filterTags
            }

        currentCards =
            model.cards

        updatedCards =
            { currentCards
                | collapsedNodes = Decode.decodeValue (Decode.field "collapsedNodes" decodeCollapsed) json |> Result.withDefault currentCards.collapsedNodes
            }
    in
    { model
        | cards = updatedCards
        , timeline = restoreTimelinePreferences json model.timeline
    }
        |> Feature.FocusedFilters.stored updatedSearch


restoreTimelinePreferences : Encode.Value -> TimelineModel -> TimelineModel
restoreTimelinePreferences json timeline =
    let
        valid field choices fallback =
            Decode.decodeValue (Decode.field field Decode.string) json
                |> Result.toMaybe
                |> Maybe.andThen (\value -> if List.member value choices then Just value else Nothing)
                |> Maybe.withDefault fallback
        window = valid "timelineWindow" [ "7", "14", "30", "6months", "year", "all" ] "30"
        bucket = valid "timelineBucket" [ "day", "week", "month", "quarter" ] "week"
    in
    if timeline.histogramWindow == window && timeline.histogramBucket == bucket then timeline else
        { timeline | histogramWindow = window, histogramBucket = bucket, histogramSince = "", histogramUntil = "", histogramLoadedRequest = Nothing, histogramActiveRequest = Nothing, histogramActiveIdentity = Nothing, histogramClockWorkspaceId = Nothing }


applyStoredFiltersIfCurrentWorkspace : Encode.Value -> Model -> Model
applyStoredFiltersIfCurrentWorkspace json original =
    if Decode.decodeValue (Decode.field "entitySearchPreference" Decode.bool) json == Ok True then
        restoreEntitySearchPreferences json original
    else
    let
        groups = original.groups
        model =
            { original | groups = { groups | collapsedGroups = Decode.decodeValue (Decode.field "collapsedGroups" (Decode.dict Decode.bool)) json |> Result.withDefault groups.collapsedGroups } }

        storedWorkspaceId =
            Decode.decodeValue (Decode.field "workspaceId" (Decode.nullable Decode.string)) json
                |> Result.withDefault Nothing
    in
    case ( model.selectedWorkspaceId, storedWorkspaceId ) of
        ( Just currentWsId, Just storedWsId ) ->
            if currentWsId == storedWsId then
                applyStoredFilters json model

            else
                model

        ( Just _, Nothing ) ->
            model

        _ ->
            applyStoredFilters json model


saveFiltersCmd : Model -> Cmd Msg
saveFiltersCmd model =
    case model.selectedWorkspaceId of
        Just wsId ->
            saveToLocalStorage
                (Encode.object
                    [ ( "key", Encode.string (localStorageKey wsId) )
                    , ( "value", encodeFilterState model )
                    ]
                )

        Nothing ->
            Cmd.none


beginWorkspaceDataReload : Bool -> Model -> ( Model, Cmd Msg )
beginWorkspaceDataReload showLoading model =
    case model.selectedWorkspaceId of
        Just wsId ->
            let
                currentDataLoading =
                    model.dataLoading

                token =
                    currentDataLoading.nextWorkspaceLoadToken

                repositoryWorkspace =
                    Dict.get wsId model.workspaces
                        |> Maybe.map (\workspace -> workspace.workspaceType == Api.Repository)
                        |> Maybe.withDefault False

                currentObservations =
                    model.observations

                observations =
                    if repositoryWorkspace then
                        beginObservationReload wsId currentObservations

                    else
                        { currentObservations
                            | items = Dict.empty
                            , orderedIds = []
                            , hasMore = False
                            , loading = False
                            , error = Nothing
                            , expectedOffset = Nothing
                            , nextOffset = 0
                        }

                updatedDataLoading =
                    { currentDataLoading
                        | activeWorkspaceLoadToken = Just token
                        , nextWorkspaceLoadToken = token + 1
                        , loadingWorkspaceData = if showLoading then True else currentDataLoading.loadingWorkspaceData
                        , pendingWorkspaceLoads = if repositoryWorkspace then 3 else 2
                        , cardHydrationLoaded = False
                    }

                observationCmds =
                    if repositoryWorkspace then
                        [ Api.fetchContentObservations model.flags.apiUrl (observationListQuery wsId 0 observations)
                            (GotObservations wsId (Just token) observations.requestGeneration observations.queryFingerprint 0)
                        ]

                    else
                        []
            in
            ( { model | dataLoading = updatedDataLoading, observations = observations }
            , Cmd.batch
                ([ Api.fetchProjects model.flags.apiUrl wsId (GotProjects wsId (Just token) 0)
                 , Api.fetchTasks model.flags.apiUrl wsId (GotTasks wsId (Just token) 0)
                 ]
                    ++ observationCmds
                )
            )

        Nothing ->
            ( model, Cmd.none )


beginObservationReload : String -> ObservationModel -> ObservationModel
beginObservationReload workspaceId observations =
    let
        fingerprint =
            observationQueryFingerprint (observationListQuery workspaceId 0 observations)
    in
    { observations
        | items = Dict.empty
        , orderedIds = []
        , hasMore = False
        , loading = True
        , error = Nothing
        , requestGeneration = observations.requestGeneration + 1
        , queryFingerprint = fingerprint
        , expectedOffset = Just 0
        , nextOffset = 0
    }


observationListQuery : String -> Int -> ObservationModel -> Api.ObservationListQuery
observationListQuery workspaceId offset observations =
    { workspaceId = workspaceId
    , subjectKind = observations.subjectKind
    , subject = observationFilterValue observations.subject
    , gitSha = observationFilterValue observations.gitSha
    , currentGitSha = observationFilterValue observations.currentGitSha
    , historyGitSha = observationFilterValue observations.historyGitSha
    , query = observationFilterValue observations.query
    , limit = 50
    , offset = offset
    }


observationQueryFingerprint : Api.ObservationListQuery -> String
observationQueryFingerprint query =
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


observationFilterValue : String -> Maybe String
observationFilterValue value =
    let
        trimmed = String.trim value
    in
    if String.isEmpty trimmed then Nothing else Just trimmed


-- MUTATION TRACKING


{-| Mark an entity ID as recently mutated locally. Returns the updated model
and a Cmd that clears the flag after 3 seconds.
-}
trackLocalMutation : String -> Model -> ( Model, Cmd Msg )
trackLocalMutation entityId model =
    let
        ( updatedModel, _, clearCmd ) =
            beginTrackedMutation [ entityId ] model
    in
    ( updatedModel, clearCmd )


trackLocalMutations : List String -> Model -> ( Model, Cmd Msg )
trackLocalMutations entityIds model =
    let
        ( updatedModel, _, clearCmd ) =
            beginTrackedMutation entityIds model
    in
    ( updatedModel, clearCmd )


insertTasks : List Api.Task -> Model -> Model
insertTasks tasks model =
    { model
        | tasks =
            List.foldl
                (\task acc -> Dict.insert task.id task acc)
                model.tasks
                tasks
    }


applyDependencyStatusChanges : List Api.TaskDependencyStatusChange -> Model -> Model
applyDependencyStatusChanges changes model =
    insertTasks (List.map .task changes) model


applyDependencyMutationResult : Api.DependencyMutationResult -> Model -> Model
applyDependencyMutationResult result model =
    let
        modelAfterStatusChanges =
            applyDependencyStatusChangesPreservingDependencyCount result.taskId result.affectedTasks model

        dependenciesBefore =
            modelAfterStatusChanges.dependencies

        dependencyKnownBefore =
            taskDependencyIsKnown result dependenciesBefore

        dependenciesAfter =
            { dependenciesBefore
                | taskDependencyLinks = applyTaskDependencyLinkMutation result dependenciesBefore.taskDependencyLinks
                , taskDependencies = applyTaskDependencySummaryMutation result modelAfterStatusChanges.tasks dependenciesBefore
            }

        dependencyKnownAfter =
            taskDependencyIsKnown result dependenciesAfter

        dependencyCountDelta =
            if dependencyKnownAfter == dependencyKnownBefore then
                0

            else if dependencyKnownAfter then
                1

            else
                -1

    in
    updateTaskDependencyCount result.taskId dependencyCountDelta
        { modelAfterStatusChanges | dependencies = dependenciesAfter }


applyDependencyStatusChangesPreservingDependencyCount : String -> List Api.TaskDependencyStatusChange -> Model -> Model
applyDependencyStatusChangesPreservingDependencyCount taskId changes model =
    let
        preserveDependencyCount task =
            if task.id == taskId then
                case Dict.get taskId model.tasks of
                    Just currentTask ->
                        { task | dependencyCount = currentTask.dependencyCount }

                    Nothing ->
                        task

            else
                task
    in
    insertTasks (List.map (.task >> preserveDependencyCount) changes) model


applyTaskMutationResult : Api.TaskMutationResult -> Model -> Model
applyTaskMutationResult result model =
    insertTasks (result.task :: List.map .task result.dependencyEffects) model


taskMutationResultIds : Api.TaskMutationResult -> List String
taskMutationResultIds result =
    result.task.id :: List.map (\change -> change.task.id) result.dependencyEffects


linkedMemoriesForEntity : Model -> String -> List Api.Memory
linkedMemoriesForEntity model entityId =
    case Dict.get entityId model.memory.entityMemories of
        Just memories ->
            memories

        Nothing ->
            model.memory.entityMemoryIds
                |> Dict.get entityId
                |> Maybe.withDefault []
                |> List.filterMap (\memoryId -> Dict.get memoryId model.memories)


hasLinkedMemoryData : Model -> String -> Bool
hasLinkedMemoryData model entityId =
    Dict.member entityId model.memory.entityMemories
        || model.dataLoading.cardHydrationLoaded


taskDependencySummariesForTask : Model -> String -> List Api.TaskDependencySummary
taskDependencySummariesForTask model taskId =
    case Dict.get taskId model.dependencies.taskDependencies of
        Just dependencies ->
            dependencies

        Nothing ->
            model.dependencies.taskDependencyLinks
                |> List.filter (\link -> link.taskId == taskId)
                |> List.filterMap
                    (\link ->
                        Dict.get link.dependsOnId model.tasks
                            |> Maybe.map (\task -> { id = task.id, name = task.title, status = task.status })
                    )


hasTaskDependencyData : Model -> String -> Bool
hasTaskDependencyData model taskId =
    Dict.member taskId model.dependencies.taskDependencies


applyTaskDependencyLinkMutation : Api.DependencyMutationResult -> List Api.WorkspaceTaskDependencyLink -> List Api.WorkspaceTaskDependencyLink
applyTaskDependencyLinkMutation result links =
    let
        sameLink link =
            link.taskId == result.taskId && link.dependsOnId == result.dependsOnId
    in
    case result.action of
        "add" ->
            if List.any sameLink links then
                links

            else
                links ++ [ { taskId = result.taskId, dependsOnId = result.dependsOnId } ]

        "remove" ->
            List.filter (not << sameLink) links

        _ ->
            links


applyTaskDependencySummaryMutation : Api.DependencyMutationResult -> Dict String Api.Task -> DependenciesModel -> Dict String (List Api.TaskDependencySummary)
applyTaskDependencySummaryMutation result tasks dependencies =
    case Dict.get result.taskId dependencies.taskDependencies of
        Nothing ->
            let
                existing =
                    dependencies.taskDependencyLinks
                        |> List.filter (\link -> link.taskId == result.taskId)
                        |> List.filterMap
                            (\link ->
                                Dict.get link.dependsOnId tasks
                                    |> Maybe.map (\task -> { id = task.id, name = task.title, status = task.status })
                            )
            in
            case ( result.action, Dict.get result.dependsOnId tasks ) of
                ( "add", Just task ) ->
                    Dict.insert result.taskId
                        ((existing ++ [ { id = task.id, name = task.title, status = task.status } ])
                            |> List.foldl (\summary values -> if List.any (\value -> value.id == summary.id) values then values else summary :: values) []
                            |> List.sortBy (\summary -> ( String.toLower summary.name, summary.id ))
                        )
                        dependencies.taskDependencies

                _ ->
                    dependencies.taskDependencies

        Just existing ->
            let
                sameSummary summary =
                    summary.id == result.dependsOnId

                updated =
                    case result.action of
                        "add" ->
                            case Dict.get result.dependsOnId tasks of
                                Just task ->
                                    if List.any sameSummary existing then
                                        existing

                                    else
                                        (existing ++ [ { id = task.id, name = task.title, status = task.status } ])
                                            |> List.sortBy (\summary -> ( String.toLower summary.name, summary.id ))

                                Nothing ->
                                    existing

                        "remove" ->
                            List.filter (not << sameSummary) existing

                        _ ->
                            existing
            in
            Dict.insert result.taskId updated dependencies.taskDependencies


taskDependencyIsKnown : Api.DependencyMutationResult -> DependenciesModel -> Bool
taskDependencyIsKnown result dependencies =
    let
        sameLink link =
            link.taskId == result.taskId && link.dependsOnId == result.dependsOnId

        sameSummary summary =
            summary.id == result.dependsOnId
    in
    List.any sameLink dependencies.taskDependencyLinks
        || (Dict.get result.taskId dependencies.taskDependencies
                |> Maybe.map (List.any sameSummary)
                |> Maybe.withDefault False
           )


updateTaskDependencyCount : String -> Int -> Model -> Model
updateTaskDependencyCount taskId delta model =
    if delta == 0 then
        model

    else
        case Dict.get taskId model.tasks of
            Just task ->
                { model
                    | tasks =
                        Dict.insert taskId
                            { task | dependencyCount = Basics.max 0 (task.dependencyCount + delta) }
                            model.tasks
                }

            Nothing ->
                model


{-| Only canonical own counts authorize status transitions. Sparse links, endpoint
rows and recursive descendant rollups cannot establish a zero count.
-}
taskOwnOpenDependencyCount : Model -> String -> Maybe Int
taskOwnOpenDependencyCount model taskId =
    Dict.get taskId model.dependencies.taskReadinessRollups
        |> Maybe.andThen .ownOpenDependencyCount


taskPrerequisiteBlockerReason : Model -> String -> Maybe String
taskPrerequisiteBlockerReason model taskId =
    case taskOwnOpenDependencyCount model taskId of
        Just 0 -> Nothing
        Just count -> Just ("Finish or cancel " ++ String.fromInt count ++ " open prerequisite(s) before changing this task to todo, in progress or completed.")
        Nothing -> Just "Prerequisite eligibility is unavailable. Refresh to retry."


taskParentEligibilityUnavailable : Model -> Api.Task -> Bool
taskParentEligibilityUnavailable model task =
    case task.parentId of
        Nothing -> False
        Just parentId ->
            not (Dict.member parentId model.tasks)
                || taskOwnOpenDependencyCount model parentId == Nothing


taskStatusEligibilityUnavailableReason : Model -> Api.Task -> Maybe String
taskStatusEligibilityUnavailableReason model task =
    if taskOwnOpenDependencyCount model task.id == Nothing then
        taskPrerequisiteBlockerReason model task.id
    else if taskParentEligibilityUnavailable model task then
        Just "Parent eligibility is unavailable. Refresh to retry."
    else Nothing


taskProjectPaused : Model -> Api.Task -> Bool
taskProjectPaused model task =
    let
        visit seen maybeId =
            case maybeId of
                Nothing -> False
                Just id ->
                    if Set.member id seen then False
                    else case Dict.get id model.projects of
                        Just project ->
                            case project.status of
                                Api.ProjPaused -> True
                                Api.ProjActive -> visit (Set.insert id seen) project.parentId
                                _ -> False
                        Nothing -> False
    in
    visit Set.empty task.projectId


taskStatusDisabledReason : Model -> Api.Task -> Api.TaskStatus -> Maybe String
taskStatusDisabledReason model task status =
    let
        rollup = taskReadinessRollupForTask model task.id
        openSubtasks = rollup |> Maybe.map .openSubtaskCount |> Maybe.withDefault 0
        parent = task.parentId |> Maybe.andThen (\id -> Dict.get id model.tasks)
        closedTaskAncestor seen maybeId =
            case maybeId of
                Nothing -> False
                Just id ->
                    if Set.member id seen then True
                    else case Dict.get id model.tasks of
                        Just ancestor -> List.member ancestor.status [ Api.Done, Api.Cancelled ] || closedTaskAncestor (Set.insert id seen) ancestor.parentId
                        Nothing -> True
        closedProjectAncestor seen maybeId =
            case maybeId of
                Nothing -> False
                Just id ->
                    if Set.member id seen then True
                    else case Dict.get id model.projects of
                        Just ancestor -> List.member ancestor.status [ Api.ProjCompleted, Api.ProjArchived ] || closedProjectAncestor (Set.insert id seen) ancestor.parentId
                        Nothing -> True
    in
    if status == task.status || status == Api.Cancelled then Nothing
    else if List.member status [ Api.Todo, Api.InProgress, Api.Done ] && taskProjectPaused model task then
        Just "Resume the paused project or ancestor before changing this task to todo, in progress or completed."
    else if isOpenTaskCardStatus status && (parent |> Maybe.map .status) == Just Api.Cancelled then
        Just "Reopen the cancelled parent task before reopening this subtask."
    else if isOpenTaskCardStatus status && (parent |> Maybe.map .status) == Just Api.Done then
        Just "Reopen the parent task before reopening this subtask."
    else if List.member status [ Api.Todo, Api.InProgress, Api.Done ] && taskPrerequisiteBlockerReason model task.id /= Nothing then
        taskPrerequisiteBlockerReason model task.id
    else if status == Api.InProgress && taskParentEligibilityUnavailable model task then
        Just "Parent eligibility is unavailable. Refresh to retry."
    else if isOpenTaskCardStatus status && (closedTaskAncestor Set.empty task.parentId || closedProjectAncestor Set.empty task.projectId) then
        Just "Reopen the closed parent task or project before reopening this task."
    else if status == Api.Done && openSubtasks > 0 then
        Just ("Finish or cancel " ++ String.fromInt openSubtasks ++ " subtasks before marking this task done.")
    else if status == Api.InProgress && openSubtasks > 0 then
        Just "Finish or cancel all incomplete subtasks before starting this task."
    else if status == Api.InProgress && task.parentId /= Nothing && not (parent |> Maybe.andThen (\value -> taskReadinessRollupForTask model value.id) |> Maybe.map .canStartSubtasks |> Maybe.withDefault False) then
        Just "Resolve parent prerequisites, manual blocking, or closed ancestors before starting subtasks."
    else Nothing


{-| Retire ready authority before a canonical refresh. The planning revision also
fences older overview/navigation replies, while preserving presentation counts.
-}
invalidateTaskPrerequisiteEligibility : List String -> Model -> Model
invalidateTaskPrerequisiteEligibility ids model =
    let
        ( revision, issued ) = allocatePlanningRead model
        invalidate id current =
            let cached = case Dict.get id current.dependencies.taskReadinessRollups of
                    Just rollup -> Just rollup
                    Nothing -> Dict.get id current.dataLoading.taskCardSummaries |> Maybe.map .readinessRollup
            in case cached of
                Just rollup -> applyTaskReadiness revision id { rollup | ownOpenDependencyCount = Nothing } current
                Nothing -> recordPlanningAuthority revision ("task:" ++ id) Nothing Nothing current
    in
    List.foldl invalidate issued ids


taskReadinessRollupForTask : Model -> String -> Maybe Api.TaskReadinessRollup
taskReadinessRollupForTask model taskId =
    case Dict.get taskId model.dependencies.taskReadinessRollups of
        Just rollup ->
            Just rollup

        Nothing ->
            if model.dataLoading.cardHydrationLoaded then
                Just (computeTaskReadinessRollup model taskId)

            else
                Nothing


projectReadinessRollupForProject : Model -> String -> Maybe Api.ProjectReadinessRollup
projectReadinessRollupForProject model projectId =
    case Dict.get projectId model.dependencies.projectReadinessRollups of
        Just rollup ->
            Just rollup

        Nothing ->
            if model.dataLoading.cardHydrationLoaded then
                Just (computeProjectReadinessRollup model projectId)

            else
                Nothing


computeTaskReadinessRollup : Model -> String -> Api.TaskReadinessRollup
computeTaskReadinessRollup model taskId =
    computeTaskReadinessRollupFrom (Dict.values model.tasks) model.dependencies.taskDependencyLinks taskId


computeTaskReadinessRollupFrom : List Api.Task -> List Api.WorkspaceTaskDependencyLink -> String -> Api.TaskReadinessRollup
computeTaskReadinessRollupFrom allTasks taskDependencyLinks taskId =
    let
        tasksById =
            indexBy .id allTasks

        treeIds =
            collectDescendantTaskIds allTasks taskId |> uniqueStringList

        descendantTasks =
            treeIds
                |> List.filter ((/=) taskId)
                |> List.filterMap (\id -> Dict.get id tasksById)

        openDependencyEdges =
            treeIds
                |> List.filterMap (\id -> Dict.get id tasksById)
                |> List.filter (\task -> isOpenTaskCardStatus task.status)
                |> List.concatMap
                    (\task ->
                        taskDependencyLinks
                            |> List.filter (\link -> link.taskId == task.id)
                            |> List.filterMap
                                (\link ->
                                    Dict.get link.dependsOnId tasksById
                                        |> Maybe.andThen
                                            (\dependency ->
                                                if isOpenTaskCardStatus dependency.status then
                                                    Just ( task.id, dependency.id )

                                                else
                                                    Nothing
                                            )
                                )
                    )
                |> uniquePairs

        dependencyBlockedTaskCount =
            openDependencyEdges
                |> List.map Tuple.first
                |> uniqueStringList
                |> List.length

        openDependencyCount =
            List.length openDependencyEdges
    in
    { openSubtaskCount = descendantTasks |> List.filter (\task -> isOpenTaskCardStatus task.status) |> List.length
    , doneSubtaskCount = descendantTasks |> List.filter (\task -> task.status == Api.Done) |> List.length
    , cancelledSubtaskCount = descendantTasks |> List.filter (\task -> task.status == Api.Cancelled) |> List.length
    , blockedSubtaskCount = descendantTasks |> List.filter (\task -> task.status == Api.Blocked) |> List.length
    , dependencyBlockedTaskCount = dependencyBlockedTaskCount
    , openDependencyCount = openDependencyCount
    , completionReady = not (List.any (\task -> isOpenTaskCardStatus task.status) descendantTasks)
    , inProgressSubtaskCount = allTasks |> List.filter (\task -> task.parentId == Just taskId && task.status == Api.InProgress) |> List.length
    -- Task rows cannot distinguish automatic blocking from manual intent.
    -- Eligibility comes from the authoritative readiness summary.
    , canStartSubtasks = False
    , ownOpenDependencyCount = Nothing
    }


computeProjectReadinessRollup : Model -> String -> Api.ProjectReadinessRollup
computeProjectReadinessRollup model projectId =
    computeProjectReadinessRollupFrom (Dict.values model.projects) (Dict.values model.tasks) model.dependencies.taskDependencyLinks projectId


computeProjectReadinessRollupFrom : List Api.Project -> List Api.Task -> List Api.WorkspaceTaskDependencyLink -> String -> Api.ProjectReadinessRollup
computeProjectReadinessRollupFrom allProjects allTasks taskDependencyLinks projectId =
    let
        projectsById =
            indexBy .id allProjects

        tasksById =
            indexBy .id allTasks

        projectTreeIds =
            collectDescendantProjectIds allProjects projectId |> uniqueStringList

        descendantProjects =
            projectTreeIds
                |> List.filter ((/=) projectId)
                |> List.filterMap (\id -> Dict.get id projectsById)

        seededTaskIds =
            allTasks
                |> List.filter (\task -> task.projectId |> Maybe.map (\pid -> List.member pid projectTreeIds) |> Maybe.withDefault False)
                |> List.concatMap (\task -> collectDescendantTaskIds allTasks task.id)
                |> uniqueStringList

        projectTasks =
            seededTaskIds
                |> List.filterMap (\id -> Dict.get id tasksById)

        openDependencyEdges =
            projectTasks
                |> List.filter (\task -> isOpenTaskCardStatus task.status)
                |> List.concatMap
                    (\task ->
                        taskDependencyLinks
                            |> List.filter (\link -> link.taskId == task.id)
                            |> List.filterMap
                                (\link ->
                                    Dict.get link.dependsOnId tasksById
                                        |> Maybe.andThen
                                            (\dependency ->
                                                if isOpenTaskCardStatus dependency.status then
                                                    Just ( task.id, dependency.id )

                                                else
                                                    Nothing
                                            )
                                )
                    )
                |> uniquePairs
    in
    { openProjectCount = descendantProjects |> List.filter (\project -> isOpenProjectCardStatus project.status) |> List.length
    , inProgressTaskCount = projectTasks |> List.filter (\task -> task.status == Api.InProgress) |> List.length
    , closedProjectCount = descendantProjects |> List.filter (\project -> project.status == Api.ProjCompleted || project.status == Api.ProjArchived) |> List.length
    , openTaskCount = projectTasks |> List.filter (\task -> isOpenTaskCardStatus task.status) |> List.length
    , doneTaskCount = projectTasks |> List.filter (\task -> task.status == Api.Done) |> List.length
    , cancelledTaskCount = projectTasks |> List.filter (\task -> task.status == Api.Cancelled) |> List.length
    , blockedTaskCount = projectTasks |> List.filter (\task -> task.status == Api.Blocked) |> List.length
    , dependencyBlockedTaskCount = openDependencyEdges |> List.map Tuple.first |> uniqueStringList |> List.length
    , openDependencyCount = List.length openDependencyEdges
    , completionReady =
        not (List.any (\project -> isOpenProjectCardStatus project.status) descendantProjects)
            && not (List.any (\task -> isOpenTaskCardStatus task.status) projectTasks)
    }


isOpenTaskCardStatus : Api.TaskStatus -> Bool
isOpenTaskCardStatus status =
    status == Api.Todo || status == Api.InProgress || status == Api.Blocked


isOpenProjectCardStatus : Api.ProjectStatus -> Bool
isOpenProjectCardStatus status =
    status == Api.ProjActive || status == Api.ProjPaused


uniqueStringList : List String -> List String
uniqueStringList strings =
    List.foldl
        (\item acc ->
            if List.member item acc then
                acc

            else
                acc ++ [ item ]
        )
        []
        strings


uniquePairs : List ( String, String ) -> List ( String, String )
uniquePairs pairs =
    List.foldl
        (\pair acc ->
            if List.member pair acc then
                acc

            else
                acc ++ [ pair ]
        )
        []
        pairs


beginTrackedMutation : List String -> Model -> ( Model, String, Cmd Msg )
beginTrackedMutation entityIds model =
    let
        currentMutations =
            model.mutations

        requestId =
            model.flags.sessionId ++ "-req-" ++ String.fromInt currentMutations.nextRequestId

        updatedMutations =
            { currentMutations
                | pendingMutationIds =
                    List.foldl (\entityId acc -> Dict.insert entityId True acc) model.mutations.pendingMutationIds entityIds
                , pendingRequestIds = Dict.insert requestId True model.mutations.pendingRequestIds
                , nextRequestId = currentMutations.nextRequestId + 1
            }
    in
    ( { model | mutations = updatedMutations }
    , requestId
    , Cmd.batch
        ((Process.sleep 3000 |> ElmTask.perform (\_ -> ClearPendingRequest requestId))
            :: (entityIds
                    |> List.map (\entityId -> Process.sleep 3000 |> ElmTask.perform (\_ -> ClearPendingMutation entityId))
               )
        )
    )



-- UTILITIES


indexBy : (a -> comparable) -> List a -> Dict comparable a
indexBy key items =
    List.foldl (\item acc -> Dict.insert (key item) item acc) Dict.empty items


taskStatusDisplayText : Api.TaskStatus -> String
taskStatusDisplayText status =
    case status of
        Api.Blocked ->
            "blocked"

        _ ->
            Api.taskStatusToString status |> String.replace "_" " "


taskStatusTitle : Api.TaskStatus -> String
taskStatusTitle status =
    case status of
        Api.Blocked ->
            "Blocked by incomplete subtasks, dependencies, or a manual pause"

        _ ->
            taskStatusDisplayText status


taskStatusBadgeClass : Api.TaskStatus -> String
taskStatusBadgeClass status =
    case status of
        Api.Blocked ->
            "task-status-text task-status-dependency-blocked"

        _ ->
            "badge badge-" ++ Api.taskStatusToString status


taskPopoverStatusClass : Api.TaskStatus -> String
taskPopoverStatusClass status =
    case status of
        Api.Blocked ->
            "popover-card-status task-status-text task-status-dependency-blocked"

        _ ->
            "popover-card-status card-status-" ++ Api.taskStatusToString status


taskCardAppearanceClass : Api.TaskStatus -> Maybe Api.TaskReadinessRollup -> String
taskCardAppearanceClass status maybeRollup =
    taskCardStatusClass (taskPresentationStatus status maybeRollup)


taskPresentationStatus : Api.TaskStatus -> Maybe Api.TaskReadinessRollup -> Api.TaskStatus
taskPresentationStatus status maybeRollup =
    if status == Api.Blocked && (maybeRollup |> Maybe.map .inProgressSubtaskCount |> Maybe.withDefault 0) > 0 then
        Api.InProgress

    else
        status


taskPresentationOrder : Model -> { a | id : String, status : Api.TaskStatus } -> Int
taskPresentationOrder model task =
    taskReadinessForPresentation model task.id
        |> taskPresentationStatus task.status
        |> Api.taskStatusOrder


taskReadinessForPresentation : Model -> String -> Maybe Api.TaskReadinessRollup
taskReadinessForPresentation model taskId =
    case Dict.get taskId model.dependencies.taskReadinessRollups of
        Just rollup ->
            Just rollup

        Nothing ->
            Dict.get taskId model.dataLoading.taskCardSummaries |> Maybe.map .readinessRollup


taskCardStatusClass : Api.TaskStatus -> String
taskCardStatusClass status =
    case status of
        Api.Blocked ->
            ""

        _ ->
            " card-status-" ++ Api.taskStatusToString status


editElementId : String -> String -> String
editElementId entityId field =
    "edit-" ++ entityId ++ "-" ++ field


focusElement : String -> Cmd Msg
focusElement elemId =
    Browser.Dom.focus elemId
        |> ElmTask.attempt (\_ -> NoOp)


scrollToElement : String -> Cmd Msg
scrollToElement elemId =
    Ports.scrollHierarchyTarget elemId


{-| Bound plain text by Unicode codepoints, including the ellipsis, without
splitting a surrogate pair. The full value remains in canonical detail.
-}
plainTextExcerpt : Int -> String -> String
plainTextExcerpt limit value =
    let
        characters =
            value |> String.words |> String.join " " |> String.toList
    in
    if limit <= 0 then
        ""

    else if List.length characters > limit then
        String.fromList (List.take (limit - 1) characters) ++ "…"

    else
        String.fromList characters


truncateText : Int -> String -> String
truncateText maxLen str =
    if String.length str > maxLen then
        String.left maxLen str ++ "…"

    else
        str


truncateId : String -> String
truncateId id =
    String.left 8 id ++ "..."


collectDescendantProjectIds : List Api.Project -> String -> List String
collectDescendantProjectIds allProjects parentId =
    let
        directChildren =
            List.filter (\p -> p.parentId == Just parentId) allProjects
    in
    parentId :: List.concatMap (\c -> collectDescendantProjectIds allProjects c.id) directChildren


collectDescendantTaskIds : List Api.Task -> String -> List String
collectDescendantTaskIds allTasks parentId =
    let
        directChildren =
            List.filter (\t -> t.parentId == Just parentId) allTasks
    in
    parentId :: List.concatMap (\child -> collectDescendantTaskIds allTasks child.id) directChildren


computeDropPriority : Maybe Int -> Maybe Int -> Int
computeDropPriority abovePri belowPri =
    case ( abovePri, belowPri ) of
        ( Just a, Just b ) ->
            (a + b) // 2

        ( Just a, Nothing ) ->
            Basics.max 0 (a - 1)

        ( Nothing, Just b ) ->
            Basics.min 10 (b + 1)

        ( Nothing, Nothing ) ->
            5


isExpanded : Model -> String -> Bool
isExpanded model cardId =
    Dict.get cardId model.cards.expandedCards |> Maybe.withDefault False


isCollapsed : Model -> String -> Bool
isCollapsed model nodeId =
    Dict.get nodeId model.cards.collapsedNodes |> Maybe.withDefault False


editingValue : Model -> String -> String -> Maybe String
editingValue model entityId field =
    case model.editing.editState of
        Just (EditingField state) ->
            if state.entityId == entityId && state.field == field then
                Just state.value

            else
                Nothing

        Nothing ->
            Nothing


{-| Decode any scalar JSON value to a String for display in diffs.
Handles strings, ints, floats, bools, and nulls. Nested objects/arrays
will cause the dict decode to fail, falling back to a generic message.
-}
flexibleStringDecoder : Decode.Decoder String
flexibleStringDecoder =
    Decode.oneOf
        [ Decode.string
        , Decode.map String.fromInt Decode.int
        , Decode.map String.fromFloat Decode.float
        , Decode.map
            (\b ->
                if b then
                    "true"

                else
                    "false"
            )
            Decode.bool
        , Decode.null "null"
        ]



-- FILTER HELPERS


passesStatusFilter : List String -> String -> Bool
passesStatusFilter statuses status =
    List.isEmpty statuses || List.member status statuses


passesPriorityFilter : FilterPriority -> Int -> Bool
passesPriorityFilter filter priority =
    case filter of
        AnyPriority ->
            True

        ExactPriority v ->
            priority == v

        AbovePriority v ->
            priority >= v

        BelowPriority v ->
            priority <= v


normalizeObservationPaths : String -> Result String (List String)
normalizeObservationPaths rawInput =
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

    else if List.sum (List.map observationUrlBytes paths) > 262144 then
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
        && observationUrlBytes path
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


observationUrlBytes : String -> Int
observationUrlBytes value =
    String.toList value
        |> List.map (\character -> let code = Char.toCode character in if code <= 0x7F then 1 else if code <= 0x07FF then 2 else if code <= 0xFFFF then 3 else 4)
        |> List.sum


-- Each dispatch shares the canonical target lifetime with live revalidation.
{-| One clock orders planning reads and accepted authority. Entity comparisons
remain local: a request for another entity cannot supersede this response.
-}
allocatePlanningRead : Model -> ( Int, Model )
allocatePlanningRead model =
    let
        loading = model.dataLoading
        revision = loading.nextNavigationSummaryRevision + 1
    in
    ( revision, { model | dataLoading = { loading | nextNavigationSummaryRevision = revision } } )


capturePlanningRead : String -> Int -> Model -> Model
capturePlanningRead key generation model =
    let
        ( revision, issued ) = allocatePlanningRead model
        loading = issued.dataLoading
    in
    { issued | dataLoading = { loading | planningReadRequests = Dict.insert key { generation = generation, sessionEpoch = model.sessionRequestEpoch, revision = revision } loading.planningReadRequests } }


planningReadRevision : CanonicalRequestGuard -> Model -> Maybe Int
planningReadRevision guard model =
    Dict.get (guard.scopeKey ++ "|" ++ guard.targetKey) model.dataLoading.planningReadRequests
        |> Maybe.andThen (\request -> if request.generation == guard.targetGeneration && request.sessionEpoch == guard.sessionEpoch then Just request.revision else Nothing)


planningReadIsCurrent : Int -> String -> Model -> Bool
planningReadIsCurrent revision key model =
    Dict.get key model.dataLoading.navigationSummaryAuthority
        |> Maybe.map (\authority -> authority.readRevision <= revision)
        |> Maybe.withDefault True


summaryReadiness : Api.NavigationSummary -> PlanningReadiness
summaryReadiness summary =
    case summary of
        Api.NavigationTaskSummary task -> TaskReadiness task.readinessRollup
        Api.NavigationProjectSummary project -> ProjectReadiness project.readinessRollup


recordPlanningAuthority : Int -> String -> Maybe Api.NavigationSummary -> Maybe PlanningReadiness -> Model -> Model
recordPlanningAuthority revision key summary readiness model =
    if not (planningReadIsCurrent revision key model) then model
    else
        let
            loading = model.dataLoading
            previous = Dict.get key loading.navigationSummaryAuthority
            unchanged = previous |> Maybe.map (\prior -> prior.summary == summary && prior.readiness == readiness) |> Maybe.withDefault False
            authority = { revision = if unchanged then previous |> Maybe.map .revision |> Maybe.withDefault revision else revision, readRevision = revision, summary = summary, readiness = readiness }
        in
        { model | dataLoading = { loading | navigationSummaryAuthority = Dict.insert key authority loading.navigationSummaryAuthority } }


recordPlanningSummaries : Int -> List ( String, Maybe Api.NavigationSummary ) -> Model -> Model
recordPlanningSummaries revision entries model =
    List.foldl (\( key, summary ) current -> recordPlanningAuthority revision key summary (Maybe.map summaryReadiness summary) current) model entries


applyTaskReadiness : Int -> String -> Api.TaskReadinessRollup -> Model -> Model
applyTaskReadiness revision id rollup model =
    let key = "task:" ++ id
    in
    if not (planningReadIsCurrent revision key model) then model
    else
        let
            dependencies = model.dependencies
            loading = model.dataLoading
            updateSummary value = case value of
                Api.NavigationTaskSummary task -> Api.NavigationTaskSummary { task | readinessRollup = rollup }
                other -> other
            summary = Dict.get key loading.navigationSummaryAuthority |> Maybe.andThen .summary |> Maybe.map updateSummary
            updated = { model | dependencies = { dependencies | taskReadinessRollups = Dict.insert id rollup dependencies.taskReadinessRollups }, dataLoading = { loading | taskCardSummaries = Dict.update id (Maybe.map (\task -> { task | readinessRollup = rollup })) loading.taskCardSummaries } }
        in
        recordPlanningAuthority revision key summary (Just (TaskReadiness rollup)) updated


applyProjectReadiness : Int -> String -> Api.ProjectReadinessRollup -> Model -> Model
applyProjectReadiness revision id rollup model =
    let key = "project:" ++ id
    in
    if not (planningReadIsCurrent revision key model) then model
    else
        let
            dependencies = model.dependencies
            loading = model.dataLoading
            updateSummary value = case value of
                Api.NavigationProjectSummary project -> Api.NavigationProjectSummary { project | readinessRollup = rollup }
                other -> other
            summary = Dict.get key loading.navigationSummaryAuthority |> Maybe.andThen .summary |> Maybe.map updateSummary
            updated = { model | dependencies = { dependencies | projectReadinessRollups = Dict.insert id rollup dependencies.projectReadinessRollups }, dataLoading = { loading | projectCardSummaries = Dict.update id (Maybe.map (\project -> { project | readinessRollup = rollup })) loading.projectCardSummaries } }
        in
        recordPlanningAuthority revision key summary (Just (ProjectReadiness rollup)) updated


taskOverviewCmd : Model -> String -> Cmd Msg
taskOverviewCmd model taskId =
    ElmTask.perform identity (ElmTask.succeed (RequestTaskOverview model.selectedWorkspaceId model.sessionRequestEpoch taskId))


applyMissingPlanningEntity : Int -> String -> String -> Model -> Model
applyMissingPlanningEntity revision kind id model =
    let key = kind ++ ":" ++ id
    in
    if not (planningReadIsCurrent revision key model) then model
    else
        let
            loading = model.dataLoading
            dependencies = model.dependencies
            removed = if kind == "task" then
                { model | tasks = Dict.remove id model.tasks, dataLoading = { loading | taskCardSummaries = Dict.remove id loading.taskCardSummaries, navigationVisibleTaskIds = Set.remove id loading.navigationVisibleTaskIds }, dependencies = { dependencies | taskReadinessRollups = Dict.remove id dependencies.taskReadinessRollups } }
                else
                { model | projects = Dict.remove id model.projects, dataLoading = { loading | projectCardSummaries = Dict.remove id loading.projectCardSummaries, navigationVisibleProjectIds = Set.remove id loading.navigationVisibleProjectIds }, dependencies = { dependencies | projectReadinessRollups = Dict.remove id dependencies.projectReadinessRollups } }
        in
        recordPlanningAuthority revision key Nothing Nothing removed


projectRequestIsCurrent : CanonicalRequestGuard -> Model -> Bool
projectRequestIsCurrent guard model =
    model.auth.status == AuthReady
        && (model.sessionContext
            |> Maybe.map (\session -> Feature.ChangeStream.requestGuardMatches guard model.sessionRequestEpoch model.selectedWorkspaceId session.principal.actorId model.webSocket.targetGenerations)
            |> Maybe.withDefault False)


prepareProjectRequest : Maybe String -> Int -> String -> Model -> Maybe ( CanonicalRequestGuard, Model )
prepareProjectRequest workspace epoch target model =
    if workspace /= model.selectedWorkspaceId || epoch /= model.sessionRequestEpoch || model.auth.status /= AuthReady then
        Nothing
    else
        case ( workspace, model.sessionContext ) of
            ( Just workspaceId, Just session ) ->
                let
                    scope = "workspace:" ++ workspaceId
                    key = scope ++ "|" ++ target
                    generation = Dict.get key model.webSocket.targetGenerations |> Maybe.withDefault 0 |> (+) 1
                    guard = { scopeKey = scope, targetKey = target, targetGeneration = generation, sessionEpoch = epoch, routeWorkspace = workspace, audienceId = session.principal.actorId }
                    webSocket = model.webSocket
                in
                Just ( guard, { model | webSocket = { webSocket | targetGenerations = Dict.insert key generation webSocket.targetGenerations } } )
            _ -> Nothing


beginProjectRequest : Maybe String -> Int -> String -> (CanonicalRequestGuard -> Cmd Msg) -> Model -> ( Model, Cmd Msg )
beginProjectRequest workspace epoch target makeCommand model =
    case prepareProjectRequest workspace epoch target model of
        Just ( guard, requested ) ->
            ( if String.startsWith "project-overview:" target then capturePlanningRead (guard.scopeKey ++ "|" ++ guard.targetKey) guard.targetGeneration requested else requested, makeCommand guard )
        Nothing -> ( model, Cmd.none )


beginProjectMutation : Maybe String -> Int -> String -> List ( String, Encode.Value ) -> Model -> ( Model, Cmd Msg )
beginProjectMutation workspace epoch projectId fields model =
    case prepareProjectRequest workspace epoch ("entity:project:" ++ projectId) model of
        Nothing -> ( model, Cmd.none )
        Just ( guard, requested ) ->
            let
                mutations = requested.mutations
                requestId = fields |> List.filter (Tuple.first >> (==) "request_id") |> List.head |> Maybe.andThen (Tuple.second >> Decode.decodeValue Decode.string >> Result.toMaybe)
                operation = { requestId = requestId, guard = guard }
                registered = { requested | mutations = { mutations | projectOperations = Dict.insert projectId operation mutations.projectOperations } }
            in
            ( registered, Api.updateProject model.flags.apiUrl projectId fields (ProjectUpdated guard) )


projectOperationMatches : String -> Maybe String -> Model -> Bool
projectOperationMatches projectId requestId model =
    Dict.get projectId model.mutations.projectOperations
        |> Maybe.map (\operation -> requestId /= Nothing && operation.requestId == requestId
            && operation.guard.targetKey == "entity:project:" ++ projectId
            && projectRequestIsCurrent operation.guard model)
        |> Maybe.withDefault False


finishProjectOperation : CanonicalRequestGuard -> Model -> Model
finishProjectOperation guard model =
    let
        mutations = model.mutations
        remaining = Dict.filter (\_ operation -> operation.guard /= guard) mutations.projectOperations
    in
    { model | mutations = { mutations | projectOperations = remaining } }


mutationScope : Model -> MutationScope
mutationScope model =
    { workspaceId = Maybe.withDefault "" model.selectedWorkspaceId
    , sessionEpoch = model.sessionRequestEpoch
    , audienceId = model.sessionContext |> Maybe.map (.principal >> .actorId) |> Maybe.withDefault ""
    }


mutationScopeIsCurrent : MutationScope -> Model -> Bool
mutationScopeIsCurrent scope model =
    model.auth.status == AuthReady && model.sessionContext /= Nothing
        && model.selectedWorkspaceId == Just scope.workspaceId
        && mutationScope model == scope
        && Permissions.canReadCurrentWorkspace model


registerTaskMutation : String -> String -> Model -> ( Model, TaskMutationOperation )
registerTaskMutation requestId taskId model =
    let
        ( revision, issued ) = allocatePlanningRead model
        operation = { request = { scope = mutationScope model, requestId = requestId, revision = revision }, taskId = taskId }
        mutations = issued.mutations
    in
    ( { issued | mutations = { mutations | taskOperations = Dict.insert taskId revision mutations.taskOperations, taskAcknowledgements = Dict.insert revision operation mutations.taskAcknowledgements } }, operation )


taskUpdateCmd : Model -> String -> String -> List ( String, Encode.Value ) -> Cmd Msg
taskUpdateCmd model requestId taskId fields =
    ElmTask.perform identity (ElmTask.succeed (RequestTaskUpdate (mutationScope model) taskId requestId fields))


beginTaskUpdate : MutationScope -> String -> String -> List ( String, Encode.Value ) -> Model -> ( Model, Cmd Msg )
beginTaskUpdate scope taskId requestId fields model =
    if not (mutationScopeIsCurrent scope model) || not (Permissions.canEditCurrentWorkspace model) || not (Dict.get taskId model.tasks |> Maybe.map (\task -> task.workspaceId == scope.workspaceId) |> Maybe.withDefault False) then ( model, Cmd.none )
    else
        let ( registered, operation ) = registerTaskMutation requestId taskId model
        in ( registered, Api.updateTask model.flags.apiUrl taskId fields (TaskUpdated operation) )


taskOperationIsLatest : TaskMutationOperation -> Model -> Bool
taskOperationIsLatest operation model =
    Dict.get operation.taskId model.mutations.taskOperations == Just operation.request.revision


finishTaskAcknowledgement : TaskMutationOperation -> Model -> Model
finishTaskAcknowledgement operation model =
    let mutations = model.mutations
    in
    if Dict.get operation.request.revision mutations.taskAcknowledgements == Just operation then
        { model | mutations = { mutations | taskAcknowledgements = Dict.remove operation.request.revision mutations.taskAcknowledgements } }
    else model


taskPayloadIsCurrent : PlanningMutationRequest -> Api.Task -> Model -> Bool
taskPayloadIsCurrent request task model =
    task.workspaceId == request.scope.workspaceId
        && planningReadIsCurrent request.revision ("task:" ++ task.id) model
        && (Dict.get task.id model.mutations.taskOperations |> Maybe.map (\latest -> latest <= request.revision) |> Maybe.withDefault True)


acceptTaskMutationPayload : PlanningMutationRequest -> Api.Task -> Model -> Model
acceptTaskMutationPayload request task model =
    let
        loading = model.dataLoading
        update previous = { previous | title = task.title, status = task.status, priority = task.priority, projectId = task.projectId, parentId = task.parentId, dependencyCount = task.dependencyCount, completedAt = task.completedAt, updatedAt = task.updatedAt }
        summaries = Dict.update task.id (Maybe.map update) loading.taskCardSummaries
        summary = Dict.get task.id summaries |> Maybe.map Api.NavigationTaskSummary
        readiness = Dict.get task.id model.dependencies.taskReadinessRollups |> Maybe.map TaskReadiness
    in
    recordPlanningAuthority request.revision ("task:" ++ task.id) summary readiness
        { model | tasks = Dict.insert task.id task model.tasks, dataLoading = { loading | taskCardSummaries = summaries } }


beginCreationMutation : CreationSlot -> Model -> ( Model, CreationOperation, Cmd Msg )
beginCreationMutation slot model =
    let
        ( tracked, requestId, command ) = beginTrackedMutation [] model
        ( revision, issued ) = allocatePlanningRead tracked
        operation = { request = { scope = mutationScope model, requestId = requestId, revision = revision }, slot = slot, formRevision = if slot == ModalCreation then model.editing.createFormRevision else model.editing.inlineCreateRevision }
        mutations = issued.mutations
    in
    ( { issued | mutations = { mutations | creationOperations = Dict.insert revision operation mutations.creationOperations, creationSlots = Dict.insert (creationSlotKey slot) revision mutations.creationSlots } }, operation, command )


creationSlotKey : CreationSlot -> String
creationSlotKey slot = if slot == ModalCreation then "modal" else "inline"


creationOwnsForm : CreationOperation -> Model -> Bool
creationOwnsForm operation model =
    Dict.get (creationSlotKey operation.slot) model.mutations.creationSlots == Just operation.request.revision
        && (if operation.slot == ModalCreation then model.editing.createForm /= Nothing && model.editing.createFormRevision == operation.formRevision
            else model.editing.inlineCreate /= Nothing && model.editing.inlineCreateRevision == operation.formRevision)


finishCreationForm : CreationOperation -> Model -> Model
finishCreationForm operation model =
    let editing = model.editing
    in
    if not (creationOwnsForm operation model) then model
    else if operation.slot == ModalCreation then
        { model | editing = { editing | createForm = Nothing, createFormRevision = editing.createFormRevision + 1 } }
    else
        { model | editing = { editing | inlineCreate = Nothing, inlineCreateRevision = editing.inlineCreateRevision + 1 } }


retireProjectOperations : Model -> Model
retireProjectOperations model =
    let
        mutations = model.mutations
        current = Dict.filter (\_ operation -> projectRequestIsCurrent operation.guard model) mutations.projectOperations
    in
    { model | mutations = { mutations | projectOperations = current
        , taskAcknowledgements = Dict.filter (\_ operation -> mutationScopeIsCurrent operation.request.scope model) mutations.taskAcknowledgements
        , creationOperations = Dict.filter (\_ operation -> mutationScopeIsCurrent operation.request.scope model) mutations.creationOperations
        } }


projectUpdateCmd : Model -> String -> List ( String, Encode.Value ) -> Cmd Msg
projectUpdateCmd model projectId fields =
    ElmTask.perform identity (ElmTask.succeed (RequestProjectUpdate model.selectedWorkspaceId model.sessionRequestEpoch projectId fields))


projectOverviewCmd : Model -> String -> Cmd Msg
projectOverviewCmd model projectId =
    ElmTask.perform identity (ElmTask.succeed (RequestProjectOverview model.selectedWorkspaceId model.sessionRequestEpoch projectId))


projectNextTasksCmd : Model -> String -> Bool -> Cmd Msg
projectNextTasksCmd model projectId diagnostics =
    ElmTask.perform identity (ElmTask.succeed (RequestProjectNextTasks model.selectedWorkspaceId model.sessionRequestEpoch projectId diagnostics))


projectNextDemandIsActive : String -> Model -> Bool
projectNextDemandIsActive projectId model =
    let
        project id = Dict.get id model.projects
        visible visited id =
            if Set.member id visited then False
            else case project id of
                Nothing -> False
                Just value ->
                    Just value.workspaceId == model.selectedWorkspaceId
                        && (case value.parentId of
                            Nothing -> True
                            Just parent ->
                                Dict.get ("proj-" ++ parent) model.cards.collapsedNodes /= Just True
                                    && visible (Set.insert id visited) parent)
    in
    Dict.get projectId model.cards.expandedCards == Just True && visible Set.empty projectId


sessionUrlContext : Model -> String
sessionUrlContext model =
    if model.activeTab /= SessionsTab then "" else
    model.url.fragment |> Maybe.withDefault "" |> String.split "&" |> List.filterMap (\pair -> case String.split "=" pair of
        [key,encoded] -> if List.member key ["task","session"] then Url.percentDecode encoded |> Maybe.map (\value -> "&" ++ key ++ "=" ++ Url.percentEncode value) else Nothing
        _ -> Nothing) |> String.join ""
