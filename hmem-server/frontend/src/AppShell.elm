module AppShell exposing (AppShellOwnedMsg(..), finalizeInit, handleOwned, initModel, sessionEpochMatches, subscriptions, viewDocument)

import Api
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Feature.AuditLog
import Feature.Cards
import Feature.ChangeStream
import Feature.DataLoading
import Feature.Dependencies
import Feature.DragDrop
import Feature.Editing
import Feature.Focus
import Feature.Groups
import Feature.Mutations
import Feature.Observation
import Feature.Search
import Feature.Timeline
import Feature.WebSocket
import Feature.WorkspaceAdmin
import Helpers exposing (applyStoredFiltersIfCurrentWorkspace, pushUrl, replaceFragment)
import Html exposing (..)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Page.Home
import Page.Workspace
import Permissions
import Ports exposing (authSessionError, authTokenChanged, authUnauthorized, clearChangeStreamScope, disconnectChangeStreamScope, disconnectWebSocket, localStorageReceived, loginAuth, logoutAuth, onMainContentScroll)
import Toast
import Types exposing (..)
import Url


type AppShellOwnedMsg
    = SelectWorkspaceMsg String
    | SwitchTabMsg WorkspaceTab
    | SessionContextLoadedMsg Int (Maybe String) (Result Http.Error Api.SessionContext)
    | AuthUnauthorizedMsg
    | AuthTokenChangedMsg Bool
    | AuthSessionErrorMsg String
    | LoginRequestedMsg
    | LogoutRequestedMsg
    | LocalStorageLoadedMsg Encode.Value
    | GlobalKeyDownMsg Int
    | MainContentScrolledMsg Float
    | NoOpMsg


initModel : Maybe Nav.Key -> Url.Url -> Page -> Flags -> Maybe Decode.Value -> { tab : WorkspaceTab, focus : Maybe ( String, String ), observationId : Maybe String } -> Model
initModel key url page flags storedFilters frag =
    let
        initialObservations =
            let
                observations =
                    Feature.Observation.init
            in
            { observations | selectedId = frag.observationId }

        baseModel =
            { key = key
            , url = url
            , page = page
            , flags = flags
            , auth = { status = AuthBooting, mode = Nothing }
            , sessionContext = Nothing
            , sessionRequestEpoch = 0
            , selectedWorkspaceId = Nothing
            , activeTab = frag.tab
            , mainContentScrollY = 0
            , workspaces = Dict.empty
            , projects = Dict.empty
            , tasks = Dict.empty
            , memories = Dict.empty
            , observations = initialObservations
            , toast = Toast.init
            , webSocket = Feature.WebSocket.init
            , dataLoading = Feature.DataLoading.init
            , search = Feature.Search.init
            , editing = Feature.Editing.init
            , memory = emptyMemoryModel
            , dependencies = Feature.Dependencies.init
            , cards = Feature.Cards.init
            , dragDrop = Feature.DragDrop.init
            , focus = Feature.Focus.init frag.focus
            , mutations = Feature.Mutations.init
            , groups = Feature.Groups.init
            , auditLog = Feature.AuditLog.init
            , timeline = Feature.Timeline.init
            , workspaceAdmin = Feature.WorkspaceAdmin.init
            }
    in
    case storedFilters of
        Just json ->
            applyStoredFiltersIfCurrentWorkspace json baseModel

        Nothing ->
            baseModel


finalizeInit : Page -> Model -> Model
finalizeInit page model =
    { model
        | selectedWorkspaceId =
            case page of
                WorkspacePage wsId ->
                    Just wsId

                _ ->
                    Nothing
        , sessionRequestEpoch = model.sessionRequestEpoch + 1
        , dataLoading = Feature.DataLoading.prepareForPageLoad page model.dataLoading
    }


handleOwned : AppShellOwnedMsg -> Model -> ( Model, Cmd Msg )
handleOwned ownedMsg model =
    Feature.Cards.refreshViewport model (handleOwnedRaw ownedMsg model)


handleOwnedRaw : AppShellOwnedMsg -> Model -> ( Model, Cmd Msg )
handleOwnedRaw ownedMsg model =
    case ownedMsg of
        SelectWorkspaceMsg wsId ->
            ( model, pushUrl model.key ("/workspace/" ++ wsId) )

        SwitchTabMsg tab ->
            let
                newModel =
                    model
                        |> Feature.Editing.clearForTabSwitch
                        |> (\currentModel ->
                                { currentModel
                                    | activeTab = tab
                                    , observations = Feature.Observation.selectionForTab tab currentModel.observations
                                    , search = Feature.Search.clearTransientSearchState currentModel.search
                                }
                           )

                ( auditLog, auditCmd ) =
                    case ( tab, newModel.selectedWorkspaceId ) of
                        ( AuditTab, Just wsId ) ->
                            if Permissions.canViewCurrentWorkspaceAudit newModel then
                                let
                                    filters =
                                        workspaceAuditFilters wsId
                                in
                                ( resetAuditLogWithFilters filters newModel.auditLog
                                , Api.fetchAuditLog newModel.flags.apiUrl filters (GotAuditLog filters)
                                )

                            else
                                ( newModel.auditLog, Cmd.none )

                        _ ->
                            ( newModel.auditLog, Cmd.none )

                ( timeline, timelineCmd ) =
                    case ( tab, newModel.selectedWorkspaceId ) of
                        ( TimelineTab, Just wsId ) ->
                            Feature.Timeline.ensureLoaded newModel.flags.apiUrl wsId newModel.sessionRequestEpoch newModel.timeline

                        _ ->
                            ( newModel.timeline, Cmd.none )

                finalModel =
                    { newModel | auditLog = auditLog, timeline = timeline }
            in
            ( finalModel, Cmd.batch [ replaceFragment finalModel, auditCmd, timelineCmd ] )

        SessionContextLoadedMsg epoch expectedWorkspace result ->
            if sessionEpochMatches epoch model.sessionRequestEpoch && sessionContextResponseMatches expectedWorkspace model then
                case result of
                    Ok sessionContext ->
                        let
                            sessionReadyModel =
                                { model | auth = { status = AuthReady, mode = Just sessionContext.authMode }, sessionContext = Just sessionContext, workspaceAdmin = nextWorkspaceAdmin, auditLog = nextAuditLog, timeline = nextTimeline }
                                    |> Feature.Observation.reconcileCurationPermission
                                    |> updateLoadingAfterSession model expectedWorkspace sessionContext
                                    |> prepareSessionNavigation model expectedWorkspace sessionContext

                            sessionBootstrapCmd =
                                bootstrapAfterSession model expectedWorkspace sessionContext sessionReadyModel

                            ( focusedModel, focusCmd ) =
                                case ( expectedWorkspace, sessionReadyModel.page, sessionReadyModel.focus.focusedEntity ) of
                                    ( Just workspaceId, WorkspacePage currentWorkspaceId, Just ( entityType, entityId ) ) ->
                                        if workspaceId == currentWorkspaceId && sessionCanReadWorkspace workspaceId sessionContext && not (sessionScopeRetained (Feature.ChangeStream.Workspace workspaceId) sessionContext model) then
                                            Feature.DataLoading.beginNavigationFocus workspaceId entityType entityId sessionReadyModel

                                        else
                                            ( sessionReadyModel, Cmd.none )

                                    _ ->
                                        ( sessionReadyModel, Cmd.none )

                            mMembershipWorkspaceId =
                                if Permissions.isImplicitLocalSuperadminSession sessionContext then
                                    Nothing

                                else
                                    case sessionContext.workspace of
                                        Just workspaceContext ->
                                            if sessionContext.globalPermissions.superadmin || workspaceContext.canAdmin then
                                                Just workspaceContext.workspaceId

                                            else
                                                Nothing

                                        Nothing ->
                                            Nothing

                            fetchMembershipsCmd =
                                case mMembershipWorkspaceId of
                                    Just wsId ->
                                        Api.fetchWorkspaceMemberships model.flags.apiUrl wsId (GotWorkspaceMemberships wsId)

                                    Nothing ->
                                        Cmd.none

                            nextWorkspaceAdmin =
                                case mMembershipWorkspaceId of
                                    Just wsId ->
                                        let
                                            admin =
                                                model.workspaceAdmin
                                        in
                                        { admin | loadingMemberships = Dict.insert wsId True admin.loadingMemberships }

                                    Nothing ->
                                        model.workspaceAdmin

                            ( nextAuditLog, fetchAuditCmd ) =
                                case ( expectedWorkspace, model.page ) of
                                    ( Nothing, AuditLogPage ) ->
                                        if sessionContext.globalPermissions.superadmin then
                                            let
                                                auditLog =
                                                    model.auditLog
                                            in
                                            ( { auditLog | loading = True, loadingFilters = Just model.auditLog.filters }, Api.fetchAuditLog model.flags.apiUrl model.auditLog.filters (GotAuditLog model.auditLog.filters) )

                                        else
                                            ( model.auditLog, Cmd.none )

                                    ( Just wsId, WorkspacePage currentWsId ) ->
                                        if wsId == currentWsId && model.activeTab == AuditTab && sessionCanAdminWorkspace wsId sessionContext then
                                            let
                                                filters =
                                                    workspaceAuditFilters wsId
                                            in
                                            ( resetAuditLogWithFilters filters model.auditLog
                                            , Api.fetchAuditLog model.flags.apiUrl filters (GotAuditLog filters)
                                            )

                                        else
                                            ( model.auditLog, Cmd.none )

                                    _ ->
                                        ( model.auditLog, Cmd.none )

                            ( nextTimeline, fetchTimelineCmd ) =
                                case ( expectedWorkspace, model.page ) of
                                    ( Just wsId, WorkspacePage currentWsId ) ->
                                        if wsId == currentWsId && model.activeTab == TimelineTab && sessionCanReadWorkspace wsId sessionContext then
                                            Feature.Timeline.ensureLoaded model.flags.apiUrl wsId model.sessionRequestEpoch model.timeline

                                        else
                                            ( model.timeline, Cmd.none )

                                    _ ->
                                        ( model.timeline, Cmd.none )
                        in
                        ( focusedModel
                        , Cmd.batch [ retireSessionScopes sessionContext model, sessionBootstrapCmd, focusCmd, fetchMembershipsCmd, fetchAuditCmd, fetchTimelineCmd ]
                        )

                    Err _ ->
                        ( clearSessionScopedState
                            { model
                                | auth = { status = authStatusFromSessionError result, mode = model.auth.mode }
                                , sessionContext = Nothing
                                , sessionRequestEpoch = model.sessionRequestEpoch + 1
                            }
                        , disconnectWebSocket ()
                        )

            else
                ( model, Cmd.none )

        AuthUnauthorizedMsg ->
            let
                currentWebSocket =
                    model.webSocket

                unauthorizedMessage =
                    if Permissions.isLocalMode model then
                        "Local session is unavailable or expired. Check local bootstrap/token settings, then retry."

                    else
                        "Authentication is required or has expired. Please sign in again, then retry."

                ( toastedModel, toastCmd ) =
                    Toast.addToast Warning
                        unauthorizedMessage
                        (clearSessionScopedState
                            { model
                                | auth = { status = AuthRequired, mode = model.auth.mode }
                                , sessionContext = Nothing
                                , sessionRequestEpoch = model.sessionRequestEpoch + 1
                                , webSocket = { currentWebSocket | state = Disconnected }
                            }
                        )
            in
            ( toastedModel, Cmd.batch [ toastCmd, disconnectWebSocket () ] )

        AuthTokenChangedMsg present ->
            let
                updatedFlags =
                    let
                        flags =
                            model.flags
                    in
                    { flags | authTokenPresent = present }

                expectedWorkspace =
                    currentSessionWorkspace model
            in
            if present then
                let
                    rebootModel =
                        clearSessionScopedState
                            { model
                                | flags = updatedFlags
                                , auth = { status = AuthBooting, mode = model.auth.mode }
                                , sessionContext = Nothing
                            }

                    preparedModel =
                        { rebootModel | dataLoading = Feature.DataLoading.prepareForPageLoad model.page rebootModel.dataLoading }
                in
                ( { preparedModel | sessionRequestEpoch = model.sessionRequestEpoch + 1 }
                , Cmd.batch [ disconnectWebSocket (), Api.fetchSessionContext model.flags.apiUrl expectedWorkspace (GotSessionContext (model.sessionRequestEpoch + 1) expectedWorkspace) ]
                )

            else
                let
                    ( toastedModel, toastCmd ) =
                        if Permissions.isLocalMode model then
                            Toast.addToast Warning
                                "Local auth token was removed; local session will be refreshed when credentials are restored."
                                (clearSessionScopedState { model | flags = updatedFlags, auth = { status = AuthRequired, mode = model.auth.mode }, sessionContext = Nothing, sessionRequestEpoch = model.sessionRequestEpoch + 1 })

                        else
                            Toast.addToast Warning
                                "Signed out. Sign in again to continue."
                                (clearSessionScopedState { model | flags = updatedFlags, auth = { status = AuthRequired, mode = model.auth.mode }, sessionContext = Nothing, sessionRequestEpoch = model.sessionRequestEpoch + 1 })
                in
                ( toastedModel, Cmd.batch [ toastCmd, disconnectWebSocket () ] )

        AuthSessionErrorMsg message ->
            Toast.addToast Warning message model

        LoginRequestedMsg ->
            ( model, loginAuth (Url.toString model.url) )

        LogoutRequestedMsg ->
            let
                flags =
                    model.flags

                updatedFlags =
                    { flags | authTokenPresent = False }
            in
            ( clearSessionScopedState { model | flags = updatedFlags, auth = { status = AuthRequired, mode = model.auth.mode }, sessionContext = Nothing, sessionRequestEpoch = model.sessionRequestEpoch + 1 }
            , Cmd.batch [ disconnectWebSocket (), logoutAuth () ]
            )

        LocalStorageLoadedMsg json ->
            let
                loaded =
                    applyStoredFiltersIfCurrentWorkspace json model

                navigationFiltersChanged =
                    loaded.search.query /= model.search.query
                        || loaded.search.filterShowOnly /= model.search.filterShowOnly
                        || loaded.search.filterPriority /= model.search.filterPriority
                        || loaded.search.filterProjectStatuses /= model.search.filterProjectStatuses
                        || loaded.search.filterTaskStatuses /= model.search.filterTaskStatuses

                collapsedNodesChanged =
                    loaded.cards.collapsedNodes /= model.cards.collapsedNodes

                canReloadNavigation =
                    loaded.auth.status == AuthReady
                        && Permissions.canReadCurrentWorkspace loaded
                        && loaded.selectedWorkspaceId /= Nothing
            in
            if canReloadNavigation && navigationFiltersChanged then
                Feature.DataLoading.reloadNavigationForFilters loaded

            else if canReloadNavigation && collapsedNodesChanged then
                Feature.DataLoading.ensureAllNavigationPresentations loaded

            else
                ( loaded, Cmd.none )

        GlobalKeyDownMsg keyCode ->
            if keyCode == 27 then
                case
                    List.filterMap identity
                        [ Feature.Cards.handleEscape model
                        , Feature.WorkspaceAdmin.handleEscape model
                        , Feature.DragDrop.handleEscape model
                        , Feature.Dependencies.handleEscape model
                        , Feature.Editing.handleEscape model
                        ]
                        |> List.head
                of
                    Just updatedModel ->
                        ( updatedModel, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        MainContentScrolledMsg scrollY ->
            ( { model | mainContentScrollY = scrollY }, Cmd.none )

        NoOpMsg ->
            ( model, Cmd.none )


sessionEpochMatches : Int -> Int -> Bool
sessionEpochMatches responseEpoch activeEpoch =
    responseEpoch == activeEpoch


sessionContextResponseMatches : Maybe String -> Model -> Bool
sessionContextResponseMatches expectedWorkspace model =
    case expectedWorkspace of
        Just wsId ->
            case model.page of
                WorkspacePage currentWsId ->
                    currentWsId == wsId

                _ ->
                    False

        Nothing ->
            case model.page of
                WorkspacePage _ ->
                    False

                _ ->
                    True


currentSessionWorkspace : Model -> Maybe String
currentSessionWorkspace model =
    case model.page of
        WorkspacePage wsId ->
            Just wsId

        _ ->
            Nothing


bootstrapAfterSession : Model -> Maybe String -> Api.SessionContext -> Model -> Cmd Msg
bootstrapAfterSession previous expectedWorkspace sessionContext model =
    let
        workspaceListLoadToken =
            model.dataLoading.activeWorkspaceListLoadToken |> Maybe.withDefault model.dataLoading.nextWorkspaceListLoadToken

        forceResync scope =
            previous.sessionContext /= Nothing && not (sessionScopeRetained scope sessionContext previous)

        globalCmds =
            if sessionContext.globalPermissions.superadmin then
                []

            else
                [ Api.fetchWorkspaces model.flags.apiUrl (GotWorkspaces workspaceListLoadToken) ]

        ( workspaceCmds, shouldKeepWorkspaceStream ) =
            case model.page of
                WorkspacePage currentWsId ->
                    if expectedWorkspace == Just currentWsId && sessionCanReadWorkspace currentWsId sessionContext then
                        ( [ Feature.WebSocket.connectCmd model.flags (Just sessionContext) (Feature.ChangeStream.Workspace currentWsId) (forceResync (Feature.ChangeStream.Workspace currentWsId))
                          ]
                        , True
                        )

                    else
                        ( [], False )

                _ ->
                    ( [], False )

        rootNavigationCmd =
            case model.page of
                WorkspacePage currentWsId ->
                    if expectedWorkspace == Just currentWsId && sessionCanReadWorkspace currentWsId sessionContext && not (sessionScopeRetained (Feature.ChangeStream.Workspace currentWsId) sessionContext previous) then
                        [ Feature.DataLoading.beginRootNavigation model ]

                    else
                        []

                _ ->
                    []

        globalStreamCmd =
            if sessionContext.globalPermissions.superadmin then
                [ Feature.WebSocket.connectCmd model.flags (Just sessionContext) Feature.ChangeStream.Global (forceResync Feature.ChangeStream.Global) ]

            else
                []

        websocketCmds =
            if shouldKeepWorkspaceStream || sessionContext.globalPermissions.superadmin then
                []

            else
                [ disconnectWebSocket () ]
    in
    Cmd.batch (globalCmds ++ globalStreamCmd ++ workspaceCmds ++ rootNavigationCmd ++ websocketCmds)


sessionCanReadWorkspace : String -> Api.SessionContext -> Bool
sessionCanReadWorkspace wsId sessionContext =
    sessionContext.globalPermissions.superadmin
        || (sessionContext.workspace
                |> Maybe.map (\workspaceContext -> workspaceContext.workspaceId == wsId && workspaceContext.canRead)
                |> Maybe.withDefault False
           )


sessionCanAdminWorkspace : String -> Api.SessionContext -> Bool
sessionCanAdminWorkspace wsId sessionContext =
    sessionContext.globalPermissions.superadmin
        || (sessionContext.workspace
                |> Maybe.map (\workspaceContext -> workspaceContext.workspaceId == wsId && workspaceContext.canAdmin)
                |> Maybe.withDefault False
           )


workspaceAuditFilters : String -> AuditLogFilters
workspaceAuditFilters wsId =
    { workspaceId = Just wsId
    , entityType = Nothing
    , entityId = Nothing
    , action = Nothing
    , since = Nothing
    , until = Nothing
    , limit = Just 50
    , offset = Nothing
    }


resetAuditLogWithFilters : AuditLogFilters -> AuditLogModel -> AuditLogModel
resetAuditLogWithFilters filters auditLog =
    { auditLog
        | entries = []
        , entryBaseOffset = filters.offset |> Maybe.withDefault 0
        , hasMore = False
        , loading = True
        , loadingFilters = Just filters
        , filters = filters
        , expandedEntries = Dict.empty
        , revertConfirmation = Nothing
        , revertInFlight = False
    }


{-| Compare with the trusted context before installing a refresh. A healthy
same-audience connect is idempotent, so retaining its canonical projection and
response generations is necessary. Authority changes must seed a fresh snapshot.
-}
sameSessionAuthority : Api.SessionContext -> Model -> Bool
sameSessionAuthority sessionContext previous =
    previous.auth.status == AuthReady
        && (previous.sessionContext
                |> Maybe.map
                    (\trusted ->
                        trusted.authMode == sessionContext.authMode
                            && trusted.principal.actorId == sessionContext.principal.actorId
                            && trusted.principal.actorType == sessionContext.principal.actorType
                            && trusted.principal.authority == sessionContext.principal.authority
                            && trusted.principal.grantUserId == sessionContext.principal.grantUserId
                    )
                |> Maybe.withDefault False
           )


sessionScopeAllowed : Feature.ChangeStream.Scope -> Api.SessionContext -> Model -> Bool
sessionScopeAllowed scope sessionContext model =
    case scope of
        Feature.ChangeStream.Global ->
            sessionContext.globalPermissions.superadmin

        Feature.ChangeStream.Workspace workspaceId ->
            model.page == WorkspacePage workspaceId
                && model.selectedWorkspaceId == Just workspaceId
                && sessionCanReadWorkspace workspaceId sessionContext


sessionScopeRetained : Feature.ChangeStream.Scope -> Api.SessionContext -> Model -> Bool
sessionScopeRetained scope sessionContext previous =
    sameSessionAuthority sessionContext previous
        && sessionScopeAllowed scope sessionContext previous
        && (previous.sessionContext |> Maybe.map (\trusted -> sessionScopeAllowed scope trusted previous) |> Maybe.withDefault False)


prepareSessionNavigation : Model -> Maybe String -> Api.SessionContext -> Model -> Model
prepareSessionNavigation previous expectedWorkspace sessionContext model =
    if expectedWorkspace |> Maybe.map (\workspaceId -> sessionScopeRetained (Feature.ChangeStream.Workspace workspaceId) sessionContext previous) |> Maybe.withDefault False then
        model

    else
        Feature.DataLoading.prepareRootNavigationRequest expectedWorkspace model


retireSessionScopes : Api.SessionContext -> Model -> Cmd Msg
retireSessionScopes sessionContext previous =
    case previous.sessionContext of
        Just trusted ->
            let
                oldScopes =
                    (Feature.ChangeStream.Global :: List.map Feature.ChangeStream.Workspace (Maybe.withDefault [] (Maybe.map List.singleton previous.selectedWorkspaceId)))
                        ++ List.map .scope (Dict.values previous.webSocket.streams)
                        |> List.map (\scope -> ( Feature.ChangeStream.scopeKey scope, scope ))
                        |> Dict.fromList
                        |> Dict.values

                retire scope =
                    let
                        value =
                            Encode.object
                                (( "audienceId", Encode.string trusted.principal.actorId )
                                    :: (case scope of
                                            Feature.ChangeStream.Global ->
                                                [ ( "scope", Encode.string "global" ) ]

                                            Feature.ChangeStream.Workspace workspaceId ->
                                                [ ( "scope", Encode.string "workspace" ), ( "workspaceId", Encode.string workspaceId ) ]
                                       )
                                )
                    in
                    Cmd.batch [ clearChangeStreamScope value, disconnectChangeStreamScope value ]
            in
            -- Newly authorized scopes are restarted atomically by forceResync
            -- in connectCmd; separate disconnect commands could close that replacement.
            oldScopes
                |> List.filter (\scope -> (sessionScopeAllowed scope trusted previous || Dict.member (Feature.ChangeStream.scopeKey scope) previous.webSocket.streams) && not (sessionScopeAllowed scope sessionContext previous))
                |> List.map retire
                |> Cmd.batch

        Nothing ->
            Cmd.none


updateLoadingAfterSession : Model -> Maybe String -> Api.SessionContext -> Model -> Model
updateLoadingAfterSession previous expectedWorkspace sessionContext model =
    let
        keepGlobal =
            sessionScopeRetained Feature.ChangeStream.Global sessionContext previous

        keepWorkspace =
            expectedWorkspace |> Maybe.map (\workspaceId -> sessionScopeRetained (Feature.ChangeStream.Workspace workspaceId) sessionContext previous) |> Maybe.withDefault False

        clearScopedData =
            previous.sessionContext /= Nothing
                && (not (sameSessionAuthority sessionContext previous)
                        || (expectedWorkspace /= Nothing && not keepWorkspace)
                        || (expectedWorkspace == Nothing && not keepGlobal)
                   )

        scopedModel =
            if clearScopedData then
                let
                    cleared =
                        clearSessionScopedState model
                in
                { cleared
                    | dataLoading = Feature.DataLoading.prepareForPageLoad model.page cleared.dataLoading
                    , sessionRequestEpoch =
                        if sameSessionAuthority sessionContext previous then
                            model.sessionRequestEpoch

                        else
                            model.sessionRequestEpoch + 1
                }

            else
                model

        currentLoading =
            scopedModel.dataLoading

        shouldLoadWorkspaceData =
            case model.page of
                WorkspacePage wsId ->
                    expectedWorkspace == Just wsId && sessionCanReadWorkspace wsId sessionContext

                _ ->
                    False

        retainedStreams =
            Dict.filter (\_ stream -> sessionScopeRetained stream.scope sessionContext previous) previous.webSocket.streams

        requiredScopes =
            (if sessionContext.globalPermissions.superadmin then [ Feature.ChangeStream.Global ] else [])
                ++ (if shouldLoadWorkspaceData then List.map Feature.ChangeStream.Workspace (Maybe.withDefault [] (Maybe.map List.singleton expectedWorkspace)) else [])

        retainedStates =
            List.map (\scope -> Dict.get (Feature.ChangeStream.scopeKey scope) retainedStreams) requiredScopes

        nextState =
            if List.isEmpty requiredScopes then
                Disconnected

            else if retainedStreams == previous.webSocket.streams && List.all (\scope -> sessionScopeRetained scope sessionContext previous) requiredScopes then
                previous.webSocket.state

            else
                case List.filterMap identity retainedStates |> List.filterMap (\stream -> Maybe.map (\reason -> "canonical:" ++ reason ++ ":" ++ Feature.ChangeStream.scopeKey stream.scope) stream.failure) |> List.head of
                    Just reason ->
                        ConnectionFailed reason

                    Nothing ->
                        if List.all (Maybe.map .live >> Maybe.withDefault False) retainedStates then
                            Connected

                        else
                            Connecting

        nextWebSocket =
            { state = nextState
            , streams = retainedStreams
            , targetGenerations = nextSessionTargetGenerations sessionContext previous
            }

        nextGroups =
            if keepGlobal then
                previous.groups

            else
                Feature.Groups.init

        updatedLoading =
            { currentLoading
                | loadingWorkspaces = if keepGlobal then previous.dataLoading.loadingWorkspaces else True
                , activeWorkspaceListLoadToken = if keepGlobal then previous.dataLoading.activeWorkspaceListLoadToken else Just currentLoading.nextWorkspaceListLoadToken
                , nextWorkspaceListLoadToken = if keepGlobal then previous.dataLoading.nextWorkspaceListLoadToken else currentLoading.nextWorkspaceListLoadToken + 1
                , loadingWorkspaceData = shouldLoadWorkspaceData && currentLoading.loadingWorkspaceData
                , pendingWorkspaceLoads =
                    if shouldLoadWorkspaceData then
                        currentLoading.pendingWorkspaceLoads

                    else
                        0
                , activeWorkspaceLoadToken =
                    if shouldLoadWorkspaceData then
                        currentLoading.activeWorkspaceLoadToken

                    else
                        Nothing
            }
    in
    { scopedModel | dataLoading = updatedLoading, webSocket = nextWebSocket, workspaces = if keepGlobal then previous.workspaces else Dict.empty, groups = nextGroups }


{-| Retired scope counters are tombstones within a session lifetime. Removing
them would let a revoke/regrant reuse generation 1 and accept a pre-revocation
response. A changed authority instead advances the session epoch.
-}
nextSessionTargetGenerations : Api.SessionContext -> Model -> Dict.Dict String Int
nextSessionTargetGenerations sessionContext previous =
    if sameSessionAuthority sessionContext previous then
        previous.webSocket.targetGenerations
            |> Dict.map
                (\target generation ->
                    let
                        scopeKey =
                            String.split "|" target |> List.head |> Maybe.withDefault ""

                        scope =
                            if scopeKey == "global" then
                                Feature.ChangeStream.Global

                            else
                                Feature.ChangeStream.Workspace (String.dropLeft (String.length "workspace:") scopeKey)

                        wasActive =
                            Dict.member scopeKey previous.webSocket.streams
                                || (previous.sessionContext |> Maybe.map (\trusted -> sessionScopeAllowed scope trusted previous) |> Maybe.withDefault False)
                    in
                    if wasActive && not (sessionScopeRetained scope sessionContext previous) then
                        generation + 1

                    else
                        generation
                )

    else
        Dict.empty


stopAllLoading : DataLoadingModel -> DataLoadingModel
stopAllLoading dataLoading =
    let
        empty =
            Feature.DataLoading.init
    in
    { empty
        | nextWorkspaceListLoadToken = dataLoading.nextWorkspaceListLoadToken
        , nextWorkspaceLoadToken = dataLoading.nextWorkspaceLoadToken
        , nextCardDetailRequestId = dataLoading.nextCardDetailRequestId
        , navigationGeneration = dataLoading.navigationGeneration + 1
        , navigationAdmissions = dataLoading.navigationAdmissions
        , cardDetailAdmissions = dataLoading.cardDetailAdmissions
    }


emptyMemoryModel : MemoryModel
emptyMemoryModel =
    { entityMemories = Dict.empty
    , entityMemoryIds = Dict.empty
    , linkingMemoryFor = Nothing
    , linkingEntityFor = Nothing
    }


clearSessionScopedState : Model -> Model
clearSessionScopedState model =
    { model
        | workspaces = Dict.empty
        , projects = Dict.empty
        , tasks = Dict.empty
        , memories = Dict.empty
        , observations = Feature.Observation.init
        , dataLoading = stopAllLoading model.dataLoading
        , search = Feature.Search.init
        , editing = Feature.Editing.init
        , memory = emptyMemoryModel
        , dependencies = Feature.Dependencies.resetCache model.dependencies
        , cards = Feature.Cards.init
        , dragDrop = Feature.DragDrop.init
        , focus = Feature.Focus.init Nothing
        , groups = Feature.Groups.init
        , auditLog = Feature.AuditLog.init
        , timeline = Feature.Timeline.init
        , workspaceAdmin = Feature.WorkspaceAdmin.init
        , webSocket = { state = Disconnected, streams = Dict.empty, targetGenerations = Dict.empty }
    }


authStatusFromSessionError : Result Http.Error Api.SessionContext -> AuthStatus
authStatusFromSessionError result =
    case result of
        Err (Http.BadStatus 401) ->
            AuthRequired

        Err (Http.BadStatus 403) ->
            AuthFailed "You do not have access to this workspace or session scope"

        Err _ ->
            AuthFailed "Unable to load session context"

        Ok _ ->
            AuthReady


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Feature.WebSocket.subscriptions
        , authUnauthorized (\_ -> AuthUnauthorized)
        , authTokenChanged AuthTokenChanged
        , authSessionError AuthSessionError
        , Browser.Events.onKeyDown (Decode.map GlobalKeyDown (Decode.field "keyCode" Decode.int))
        , localStorageReceived LocalStorageLoaded
        , Ports.onHierarchyViewport HierarchyViewportChanged
        , onMainContentScroll MainContentScrolled
        ]


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = "hmem"
    , body =
        [ div [ class "app" ]
            [ if model.auth.status == AuthReady then
                Feature.Groups.viewSidebar model

              else
                text ""
            , Keyed.node "div"
                [ class "main-content", id "main-content-scroll" ]
                [ ( pageKey model.page, viewPage model ) ]
            , Toast.view model.toast
            , viewConnectionStatus model.webSocket.state
            , if model.auth.status == AuthReady then
                div []
                    [ Feature.Editing.viewCreateFormModal model
                    , Feature.DragDrop.viewDropActionModal model
                    , Feature.Cards.viewDeleteConfirmModal model
                    , Feature.AuditLog.viewRevertConfirmModal model
                    , Feature.WorkspaceAdmin.viewPurgeConfirmModal model
                    ]

              else
                text ""
            ]
        ]
    }


viewConnectionStatus : WSState -> Html Msg
viewConnectionStatus state =
    let
        ( statusClass, statusText ) =
            case state of
                Connected ->
                    ( "connected", "Connected" )

                Disconnected ->
                    ( "disconnected", "Disconnected" )

                Connecting ->
                    ( "connecting", "Connecting" )

                ConnectionFailed _ ->
                    ( "disconnected", "Connection failed" )
    in
    div [ class ("connection-status " ++ statusClass) ]
        [ text statusText ]


pageKey : Page -> String
pageKey page =
    case page of
        HomePage ->
            "home"

        WorkspacePage wsId ->
            "workspace-" ++ wsId

        AuditLogPage ->
            "audit"

        NotFound ->
            "notfound"


viewPage : Model -> Html Msg
viewPage model =
    case model.auth.status of
        AuthBooting ->
            viewAuthBootstrapPage model

        AuthRequired ->
            viewAuthRequiredPage model

        AuthFailed message ->
            viewAuthFailedPage model message

        AuthReady ->
            case model.page of
                HomePage ->
                    Page.Home.viewHomePage model

                WorkspacePage wsId ->
                    Page.Workspace.viewWorkspacePage wsId model

                AuditLogPage ->
                    Feature.AuditLog.viewAuditLogPage model

                NotFound ->
                    div [ class "page" ]
                        [ h2 [] [ text "Not Found" ] ]


viewAuthBootstrapPage : Model -> Html Msg
viewAuthBootstrapPage model =
    let
        loadingText =
            if Permissions.isLocalMode model then
                "Starting configured local session..."

            else
                "Checking " ++ Permissions.authModeLabel model ++ " auth session..."
    in
    div [ class "page" ]
        [ h2 [] [ text "Loading session" ]
        , div [ class "loading-indicator" ] [ text loadingText ]
        ]


viewAuthRequiredPage : Model -> Html Msg
viewAuthRequiredPage model =
    if Permissions.isLocalMode model then
        div [ class "page auth-page" ]
            [ h2 [] [ text "Local session unavailable" ]
            , p [] [ text "This frontend is configured for local mode, which normally starts with a server-provided local session and does not require sign-in." ]
            , p [] [ text "The server did not return a local principal. Check local auth bootstrap settings or the configured local bot/static bearer token." ]
            , p [ class "help-text" ] [ text ("Auth token storage key: " ++ model.flags.authTokenStorageKey) ]
            , p [ class "help-text" ]
                [ text
                    (if model.flags.authTokenPresent then
                        "A local auth token is present in the frontend runtime."

                     else
                        "No local auth token is present in the frontend runtime."
                    )
                ]
            ]

    else
        div [ class "page auth-page" ]
            [ h2 [] [ text "Authentication required" ]
            , p [] [ text "The server did not return an authenticated session. Sign in, then retry this page." ]
            , p [] [ text ("Runtime mode: " ++ Permissions.authModeLabel model) ]
            , if model.flags.authTokenPresent then
                p [ class "help-text" ] [ text "A token is present in the frontend runtime, but the server did not accept it for this session." ]

              else
                p [ class "help-text" ] [ text "No token is present in the frontend runtime." ]
            , case model.flags.loginUrl of
                Just _ ->
                    div [ class "auth-actions" ]
                        [ button [ class "btn-primary", onClick LoginRequested ] [ text "Sign in" ]
                        , p [ class "help-text" ] [ text "A login provider is configured for this deployment." ]
                        ]

                Nothing ->
                    p [] [ text ("No login URL is configured. Auth token storage key: " ++ model.flags.authTokenStorageKey) ]
            ]


viewAuthFailedPage : Model -> String -> Html Msg
viewAuthFailedPage model message =
    div [ class "page" ]
        [ h2 [] [ text "Session unavailable" ]
        , p [] [ text message ]
        , p [] [ text ("Runtime mode: " ++ Permissions.authModeLabel model) ]
        , if Permissions.isLocalMode model then
            p [ class "help-text" ] [ text "This frontend is configured for local mode, which should resolve to a server-provided local principal. If this persists, verify local bootstrap and token settings on the server." ]

          else
            text ""
        ]
