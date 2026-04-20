module Helpers exposing (..)

import Api
import Browser.Dom
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Ports exposing (saveToLocalStorage)
import Process
import String
import Task as ElmTask
import Types exposing (..)



-- FRAGMENT / URL


parseFragment : Maybe String -> { tab : WorkspaceTab, focus : Maybe ( String, String ) }
parseFragment fragment =
    case fragment of
        Nothing ->
            { tab = ProjectsTab, focus = Nothing }

        Just frag ->
            let
                pairs =
                    String.split "&" frag
                        |> List.filterMap
                            (\s ->
                                case String.split "=" s of
                                    [ k, v ] ->
                                        Just ( k, v )

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
                    if tabVal == "memories" then
                        MemoriesTab

                    else
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
            in
            { tab = tab, focus = focus }


buildFragment : WorkspaceTab -> Maybe ( String, String ) -> String
buildFragment tab focus =
    let
        tabPart =
            case tab of
                ProjectsTab ->
                    "tab=projects"

                MemoriesTab ->
                    "tab=memories"

        focusPart =
            case focus of
                Just ( t, id ) ->
                    "&focus=" ++ t ++ ":" ++ id

                Nothing ->
                    ""
    in
    tabPart ++ focusPart


replaceFragment : Model -> Cmd Msg
replaceFragment model =
    case model.selectedWorkspaceId of
        Just wsId ->
            Nav.replaceUrl model.key ("/workspace/" ++ wsId ++ "#" ++ buildFragment model.activeTab model.focus.focusedEntity)

        Nothing ->
            Cmd.none



-- FILTER PERSISTENCE


localStorageKey : String -> String
localStorageKey wsId =
    "hmem-ws-" ++ wsId


encodeFilterState : Model -> Encode.Value
encodeFilterState model =
    Encode.object
        [ ( "workspaceId"
          , case model.selectedWorkspaceId of
                Just wsId ->
                    Encode.string wsId

                Nothing ->
                    Encode.null
          )
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
        , ( "filterTags", Encode.list Encode.string model.search.filterTags )
        , ( "collapsedNodes"
          , model.cards.collapsedNodes
                |> Dict.toList
                |> List.filter (\( _, v ) -> v)
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
                |> Decode.map (\ids -> Dict.fromList (List.map (\id -> ( id, True )) ids))

        currentSearch =
            model.search

        updatedSearch =
            { currentSearch
                | query = Decode.decodeValue (Decode.field "searchQuery" Decode.string) json |> Result.withDefault currentSearch.query
                , filterShowOnly = Decode.decodeValue (Decode.field "filterShowOnly" decodeShowOnly) json |> Result.withDefault currentSearch.filterShowOnly
                , filterPriority = Decode.decodeValue (Decode.field "filterPriority" decodeFilterPriority) json |> Result.withDefault currentSearch.filterPriority
                , filterProjectStatuses = Decode.decodeValue (Decode.field "filterProjectStatuses" (Decode.list Decode.string)) json |> Result.withDefault currentSearch.filterProjectStatuses
                , filterTaskStatuses = Decode.decodeValue (Decode.field "filterTaskStatuses" (Decode.list Decode.string)) json |> Result.withDefault currentSearch.filterTaskStatuses
                , filterMemoryTypes = Decode.decodeValue (Decode.field "filterMemoryTypes" (Decode.list Decode.string)) json |> Result.withDefault currentSearch.filterMemoryTypes
                , filterImportance = Decode.decodeValue (Decode.field "filterImportance" decodeFilterPriority) json |> Result.withDefault currentSearch.filterImportance
                , filterMemoryPinned = Decode.decodeValue (Decode.field "filterMemoryPinned" decodePinned) json |> Result.withDefault currentSearch.filterMemoryPinned
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
        | search = updatedSearch
        , cards = updatedCards
    }


applyStoredFiltersIfCurrentWorkspace : Encode.Value -> Model -> Model
applyStoredFiltersIfCurrentWorkspace json model =
    let
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

                updatedDataLoading =
                    { currentDataLoading
                        | activeWorkspaceLoadToken = Just token
                        , nextWorkspaceLoadToken = token + 1
                        , loadingWorkspaceData = if showLoading then True else currentDataLoading.loadingWorkspaceData
                        , pendingWorkspaceLoads = 3
                    }
            in
            ( { model | dataLoading = updatedDataLoading }
            , Cmd.batch
                [ Api.fetchProjects model.flags.apiUrl wsId (GotProjects wsId (Just token))
                , Api.fetchTasks model.flags.apiUrl wsId (GotTasks wsId (Just token))
                , Api.fetchMemories model.flags.apiUrl wsId (GotMemories wsId (Just token))
                ]
            )

        Nothing ->
            ( model, Cmd.none )



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


editElementId : String -> String -> String
editElementId entityId field =
    "edit-" ++ entityId ++ "-" ++ field


focusElement : String -> Cmd Msg
focusElement elemId =
    Browser.Dom.focus elemId
        |> ElmTask.attempt (\_ -> NoOp)


scrollToElement : String -> Cmd Msg
scrollToElement elemId =
    Browser.Dom.getElement elemId
        |> ElmTask.andThen
            (\info ->
                Browser.Dom.setViewportOf "main-content-scroll" 0 (info.element.y - 100)
            )
        |> ElmTask.attempt (\_ -> NoOp)


formatDate : String -> String
formatDate dateStr =
    String.left 10 dateStr


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


graphPositionsKey : String -> String
graphPositionsKey wsId =
    "hmem-graph-positions-" ++ wsId


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
