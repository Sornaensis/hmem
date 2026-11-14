module Feature.TaskGraphNavigation exposing (attributes, breadcrumbs, current, guard, handleUrl, init, reconcile, restore, returnHeights, returnReady, update, view, viewReturnNotice)

{-| Graph URLs carry a graph-entry identity; their preceding hierarchy entry
carries graph-origin. The bounded registry owns presentation context per entry,
fenced by workspace, session epoch and authenticated audience. A missing receipt
uses the unfocused hierarchy fallback. Entity data and demand admissions are
always rebuilt from the current application state.

The renderer can consume `current` without replacing the graph history entry
when returning from task details; graph viewport/detail state is separate.
-}

import Api
import Browser.Navigation as Nav
import Dict
import Feature.DataLoading as Loading
import Feature.Focus as Focus
import Feature.FocusedFilters as Filters
import Helpers
import HierarchyViewport as Viewport
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Icon
import Json.Decode as Decode
import Json.Encode as Encode
import Permissions
import Ports
import Set
import TaskGraphNavigation as History
import Types exposing (..)
import Url


init : TaskGraphNavigationModel
init =
    { history = History.init, active = Nothing, pending = Nothing, returning = Nothing, nextReturnId = 1, returnNotice = Nothing
    , metadata = Dict.empty, metadataGeneration = 0, metadataRequested = Set.empty, metadataError = Nothing, scope = Nothing }


guard : Model -> Maybe History.Guard
guard model =
    if model.auth.status /= AuthReady || not (Permissions.canReadCurrentWorkspace model) then Nothing
    else Maybe.map2 (\workspace session -> { workspaceId = workspace, sessionEpoch = model.sessionRequestEpoch, audienceId = session.principal.actorId }) model.selectedWorkspaceId model.sessionContext


current : Model -> Maybe History.Route
current model =
    guard model |> Maybe.andThen (\scope -> model.taskGraphNavigation.active |> Maybe.andThen (\route -> if route.workspaceId == scope.workspaceId then Just route else Nothing))


capture : Model -> TaskGraphOrigin
capture model =
    { focus = { focusedEntity = model.focus.focusedEntity, breadcrumbAnchor = model.focus.breadcrumbAnchor, breadcrumbPreview = Nothing, history = model.focus.history, historyIndex = model.focus.historyIndex, returnContext = model.focus.returnContext }
    , filters = Filters.capture model.search, focusedFilters = model.focusedFilters
    , collapsedNodes = model.cards.collapsedNodes, nodesCollapsedByDefault = model.cards.nodesCollapsedByDefault, expandedCards = model.cards.expandedCards
    , viewportTop = model.cards.viewport.top, viewportIndex = model.cards.viewport.index, scrollTop = model.mainContentScrollY, anchorDelta = Nothing }


restore : String -> History.Entry TaskGraphOrigin -> Model -> ( Model, Cmd Msg )
restore id entry model =
    let
        origin = entry.origin
        cards = model.cards
        viewport = cards.viewport
        state = model.taskGraphNavigation
        restored = { model | activeTab = ProjectsTab, focus = origin.focus, search = Filters.apply origin.filters model.search
            , focusedFilters = origin.focusedFilters |> Maybe.map (\context -> { context | projectIds = Set.empty, taskIds = Set.empty })
            , cards = { cards | collapsedNodes = origin.collapsedNodes, nodesCollapsedByDefault = origin.nodesCollapsedByDefault, expandedCards = origin.expandedCards
                , viewport = { viewport | top = origin.viewportTop, index = origin.viewportIndex, projection = Nothing, nativePins = Set.singleton ("project:" ++ entry.projectId), preserveScroll = True } }
            , mainContentScrollY = origin.scrollTop
            , taskGraphNavigation = { state | active = Nothing, pending = Nothing, returnNotice = Nothing, nextReturnId = state.nextReturnId + 1, returning = Just { id = id, token = state.nextReturnId, projectId = entry.projectId, scrollTop = origin.scrollTop, anchorDelta = origin.anchorDelta } } }
    in
    if Filters.capture model.search /= origin.filters then Loading.revalidateNavigationForFilters restored
    else ( restored, Cmd.none )


handleUrl : Url.Url -> Model -> Maybe ( Model, Cmd Msg )
handleUrl url model =
    guard model |> Maybe.andThen (\scope ->
        let state = model.taskGraphNavigation in
        case History.parse url of
            Just route ->
                if route.workspaceId /= scope.workspaceId then Nothing
                else
                    let next = { model | url = url, activeTab = ProjectsTab, taskGraphNavigation = { state | active = Just route, pending = Nothing, returning = Nothing } } in
                    Just ( next, Cmd.none )
            Nothing ->
                case state.pending of
                    Just pending ->
                        if pending.entry.guard == scope && pending.entry.sourceUrl == Url.toString url then
                            Just ( { model | url = url, taskGraphNavigation = { state | pending = Nothing } }, Helpers.pushUrl model.key pending.entry.graphUrl )
                        else Nothing
                    Nothing ->
                        History.originKey url |> Maybe.andThen (\id -> History.lookup scope id state.history |> Maybe.andThen (\entry ->
                            if entry.sourceUrl /= Url.toString url then Nothing
                            else if state.returning |> Maybe.map (.id >> (==) id) |> Maybe.withDefault False then Just ( { model | url = url }, Cmd.none )
                            else Just (restore id entry { model | url = url }))) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let state = model.taskGraphNavigation in
    case msg of
        OpenTaskGraph projectId ->
            case guard model of
                Just scope ->
                    if current model /= Nothing || not (Dict.member projectId model.projects) then ( model, Cmd.none )
                    else
                        let
                            id = model.flags.sessionId ++ ":" ++ String.fromInt scope.sessionEpoch ++ ":" ++ String.fromInt state.history.nextId
                            entry = { guard = scope, projectId = projectId, sourceUrl = History.stampOrigin id model.url, graphUrl = History.graphUrl projectId id model.url, origin = capture model }
                        in
                        ( { model | taskGraphNavigation = { state | pending = Just { id = id, entry = entry }, history = let history = state.history in { history | nextId = history.nextId + 1 } } }
                        , Ports.taskGraphNavigationCommand (Encode.object [ ( "id", Encode.string id ), ( "project", Encode.string projectId ), ( "url", Encode.string (Url.toString model.url) ), ( "scope", encodeGuard scope ) ]) )
                Nothing -> ( model, Cmd.none )

        TaskGraphNavigationEvent raw ->
            let
                field key decoder = Decode.decodeValue (Decode.field key decoder) raw |> Result.toMaybe
                eventId = field "id" Decode.string
                receivedScope = field "scope" (Decode.map3 History.Guard (Decode.field "workspace" Decode.string) (Decode.field "epoch" Decode.int) (Decode.field "audience" Decode.string))
            in
            if receivedScope /= guard model || receivedScope == Nothing then ( model, Cmd.none )
            else case field "kind" Decode.string of
                Just "captured" ->
                    case state.pending of
                        Just pending ->
                            if eventId /= Just pending.id || History.stampOrigin pending.id model.url /= pending.entry.sourceUrl then ( model, Cmd.none )
                            else
                                let
                                    entry = pending.entry
                                    origin = entry.origin
                                    rows = field "rows" (Decode.list (Decode.map2 Tuple.pair (Decode.index 0 Decode.string) (Decode.index 1 Decode.float))) |> Maybe.withDefault [] |> List.take 64
                                    measured = List.foldl (\( key, height ) index -> if height > 0 && height < 100000 then Viewport.measure key height index else index) origin.viewportIndex rows
                                    completed = { entry | origin = { origin | scrollTop = field "top" Decode.float |> Maybe.withDefault origin.scrollTop, anchorDelta = field "anchor" Decode.float
                                        , viewportTop = field "viewportTop" Decode.float |> Maybe.withDefault origin.viewportTop, viewportIndex = measured } }
                                    next = { state | pending = Just { pending | entry = completed }, history = History.remember pending.id completed state.history }
                                in
                                ( { model | taskGraphNavigation = next }, model.key |> Maybe.map (\key -> Nav.replaceUrl key completed.sourceUrl) |> Maybe.withDefault Cmd.none )
                        Nothing -> ( model, Cmd.none )
                Just "restored" ->
                    if state.returning /= Nothing && Maybe.map .id state.returning == eventId && Maybe.map .token state.returning == field "token" Decode.int then
                        let
                            outcome = field "outcome" Decode.string |> Maybe.withDefault "settled"
                            cards = model.cards
                            viewport = cards.viewport
                            pins = if outcome == "settled" then viewport.nativePins else state.returning |> Maybe.map (\receipt -> Set.remove ("project:" ++ receipt.projectId) viewport.nativePins) |> Maybe.withDefault viewport.nativePins
                            notice = if outcome == "unrestored" then Just "The original project could not be restored in this view. Your hierarchy filters are unchanged." else Nothing
                        in ( { model | cards = { cards | viewport = { viewport | nativePins = pins } }, taskGraphNavigation = { state | returning = Nothing, returnNotice = notice } }, Cmd.none )
                    else ( model, Cmd.none )
                _ -> ( model, Cmd.none )

        TaskGraphBack ->
            case ( guard model, current model ) of
                ( Just scope, Just route ) ->
                    case route.entryId |> Maybe.andThen (\id -> History.lookup scope id state.history) |> Maybe.andThen (\entry -> if entry.projectId == route.projectId then Just entry else Nothing) of
                        Just _ -> ( model, model.key |> Maybe.map (\key -> Nav.back key 1) |> Maybe.withDefault Cmd.none )
                        Nothing -> fallback scope route model
                _ -> ( model, Cmd.none )

        TaskGraphDismissReturnNotice -> ( { model | taskGraphNavigation = { state | returnNotice = Nothing } }, Cmd.none )

        TaskGraphProjectFetched scope generation projectId result ->
            if guard model /= Just scope || generation /= state.metadataGeneration || not (Set.member projectId state.metadataRequested) then ( model, Cmd.none )
            else case result of
                Ok project ->
                    if project.workspaceId == scope.workspaceId && project.id == projectId then ( { model | taskGraphNavigation = { state | metadata = Dict.insert project.id project state.metadata } }, Cmd.none )
                    else ( { model | taskGraphNavigation = { state | metadataError = Just "Project unavailable." } }, Cmd.none )
                Err _ -> ( { model | taskGraphNavigation = { state | metadataError = Just "Project unavailable." } }, Cmd.none )
        _ -> ( model, Cmd.none )


fallback : History.Guard -> History.Route -> Model -> ( Model, Cmd Msg )
fallback scope route model =
    let
        state = model.taskGraphNavigation
        id = model.flags.sessionId ++ ":fallback:" ++ String.fromInt state.history.nextId
        base = capture model
        filters = base.filters
        origin = { base | focus = Focus.init Nothing, focusedFilters = Nothing, scrollTop = 0, viewportTop = 0, anchorDelta = Just 12
            , filters = { filters | query = "", showOnly = ShowAll, showEmptyProjects = True, hasActiveSession = False, priority = AnyPriority
                , projectStatuses = [ "active", "paused", "completed", "archived" ], taskStatuses = [ "todo", "in_progress", "blocked", "done", "cancelled" ] }
            , collapsedNodes = Dict.foldl (\projectId _ -> Dict.insert ("proj-" ++ projectId) False) base.collapsedNodes (Dict.union state.metadata model.projects) }
        currentUrl = model.url
        url = { currentUrl | fragment = Just "tab=projects" }
        entry = { guard = scope, projectId = route.projectId, sourceUrl = History.stampOrigin id url, graphUrl = Url.toString model.url, origin = origin }
        remembered = { model | taskGraphNavigation = { state | history = History.remember id entry state.history } }
        ( restored, command ) = restore id entry remembered
    in
    ( restored, Cmd.batch [ command, model.key |> Maybe.map (\key -> Nav.replaceUrl key entry.sourceUrl) |> Maybe.withDefault Cmd.none ] )


reconcile : Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
reconcile previous ( incoming, command ) =
    let
        state = incoming.taskGraphNavigation
        scope = guard incoming
        hierarchyVisible = incoming.activeTab == ProjectsTab && (case incoming.page of
            WorkspacePage workspace -> Just workspace == incoming.selectedWorkspaceId
            _ -> False)
        interrupted = state.scope == scope && state.returning /= Nothing && not hierarchyVisible
        retired = if state.scope /= scope then { state | history = History.retire state.history, pending = Nothing, returning = Nothing, returnNotice = Nothing, metadata = Dict.empty, metadataRequested = Set.empty, metadataError = Nothing, scope = scope }
            else if interrupted then {state|returning=Nothing,returnNotice=Nothing} else state
        active = History.parse incoming.url |> Maybe.andThen (\route -> if Just route.workspaceId == incoming.selectedWorkspaceId && scope /= Nothing then Just route else Nothing)
        changed = active /= previous.taskGraphNavigation.active
        next = if changed then { retired | active = active, metadata = Dict.empty, metadataGeneration = retired.metadataGeneration + 1, metadataRequested = Set.empty, metadataError = Nothing } else { retired | active = active }
        cards = incoming.cards
        viewport = cards.viewport
        pins = if interrupted then state.returning |> Maybe.map (\receipt -> Set.remove ("project:"++receipt.projectId) viewport.nativePins) |> Maybe.withDefault viewport.nativePins else viewport.nativePins
        model = { incoming | taskGraphNavigation = next, cards = {cards|viewport={viewport|nativePins=pins}} }
        missing id visited =
            if Set.member id visited || Set.size visited >= 32 then Nothing
            else case Dict.get id (Dict.union model.projects next.metadata) of
                Just project -> project.parentId |> Maybe.andThen (\parent -> missing parent (Set.insert id visited))
                Nothing -> if Set.member id next.metadataRequested then Nothing else Just id
    in
    case ( scope, active |> Maybe.andThen (\route -> missing route.projectId Set.empty) ) of
        ( Just owner, Just id ) ->
            ( { model | taskGraphNavigation = { next | metadataRequested = Set.insert id next.metadataRequested } }
            , Cmd.batch [ command, Api.fetchProject model.flags.apiUrl id (TaskGraphProjectFetched owner next.metadataGeneration id) ] )
        _ -> ( model, command )


encodeGuard : History.Guard -> Encode.Value
encodeGuard scope =
    Encode.object [ ( "workspace", Encode.string scope.workspaceId ), ( "epoch", Encode.int scope.sessionEpoch ), ( "audience", Encode.string scope.audienceId ) ]


returnHeights : Model -> Dict.Dict String Float
returnHeights model =
    Maybe.map2 Tuple.pair (guard model) model.taskGraphNavigation.returning
        |> Maybe.andThen (\( scope, receipt ) -> History.lookup scope receipt.id model.taskGraphNavigation.history)
        |> Maybe.map (.origin >> .viewportIndex >> .heights) |> Maybe.withDefault Dict.empty


returnReady : Model -> Bool
returnReady model =
    let
        viewport = model.cards.viewport
        focusedTarget = model.focus.focusedEntity /= Nothing && viewport.workspaceId == model.selectedWorkspaceId
            && viewport.sessionEpoch == model.sessionRequestEpoch && viewport.generation == model.dataLoading.navigationGeneration
            && (model.taskGraphNavigation.returning |> Maybe.map (\receipt -> Dict.member receipt.projectId model.projects && Dict.member ("project:" ++ receipt.projectId) viewport.rows) |> Maybe.withDefault False)
        rootsReady = model.dataLoading.rootNavigationRequest |> Maybe.map (\request -> request.succeeded && not request.inFlight && request.filterFingerprint == Loading.navigationFilterFingerprint model) |> Maybe.withDefault False
    in
    guard model /= Nothing && (focusedTarget || rootsReady)


attributes : Model -> List (Html.Attribute Msg)
attributes model =
    [ attribute "data-task-graph-navigation" (Encode.encode 0 (Encode.object
        [ ( "scope", guard model |> Maybe.map encodeGuard |> Maybe.withDefault Encode.null )
        , ( "url", Encode.string (Url.toString model.url) )
        , ( "ready", Encode.bool (returnReady model) )
        , ( "return", model.taskGraphNavigation.returning |> Maybe.map (\value -> Encode.object
            [ ( "id", Encode.string value.id ), ( "token", Encode.int value.token ), ( "project", Encode.string value.projectId ), ( "top", Encode.float value.scrollTop ), ( "anchor", value.anchorDelta |> Maybe.map Encode.float |> Maybe.withDefault Encode.null ) ]) |> Maybe.withDefault Encode.null )
        ])) ]


viewReturnNotice : Model -> Html Msg
viewReturnNotice model =
    case model.taskGraphNavigation.returnNotice of
        Nothing -> text ""
        Just message -> div [class "empty-state", attribute "role" "status", attribute "data-task-graph-return-notice" ""]
            [text message,button [class "btn-small btn-ghost",type_ "button",onClick TaskGraphDismissReturnNotice] [text "Dismiss"]]


breadcrumbs : Model -> Html Msg
breadcrumbs model =
    case current model of
        Nothing -> text ""
        Just route ->
            let
                combined = { model | projects = Dict.union model.projects model.taskGraphNavigation.metadata }
                crumbs = Dict.get route.projectId combined.projects |> Maybe.map (\project -> Focus.buildProjectBreadcrumb combined project []) |> Maybe.withDefault [ ( route.projectId, "Project", "project" ) ]
            in
            span [ class "titlebar-focus-trail" ] (List.concatMap (\( projectId, label, _ ) ->
                [ span [ class "focus-crumb-sep" ] [ text "›" ]
                , a [ class "focus-crumb", href ("/workspace/" ++ Url.percentEncode route.workspaceId ++ "#tab=projects&focus=" ++ Url.percentEncode ("project:" ++ projectId)) ] [ text label ]
                ]) crumbs ++ [ span [ class "focus-crumb-sep" ] [ text "›" ], span [ class "titlebar-section" ] [ text "Task Graph" ] ])


view : Model -> Html Msg
view model =
    div [ class "task-graph-page", id "task-graph-page" ]
        [ div [ class "page-header" ]
            [ h2 [] [ text "Task Graph" ], button [ class "btn-small btn-ghost", type_ "button", onClick TaskGraphBack, id "task-graph-back" ] [ Icon.view "arrow-left", text "Back" ] ]
        , model.taskGraphNavigation.metadataError |> Maybe.map (\message -> div [ class "empty-state", attribute "role" "status" ] [ text message ]) |> Maybe.withDefault (div [ id "task-graph-content" ] [])
        ]
