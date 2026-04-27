module AppShell exposing (AppShellOwnedMsg(..), finalizeInit, handleOwned, initModel, subscriptions, viewDocument)

import Api
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Feature.AuditLog
import Feature.Cards
import Feature.DataLoading
import Feature.Dependencies
import Feature.DragDrop
import Feature.Editing
import Feature.Focus
import Feature.Graph
import Feature.Groups
import Feature.Memory
import Feature.Mutations
import Feature.Search
import Feature.WebSocket
import Feature.WorkspaceAdmin
import Helpers exposing (applyStoredFiltersIfCurrentWorkspace, replaceFragment)
import Html exposing (..)
import Html.Attributes exposing (class, href, id)
import Html.Keyed as Keyed
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Page.Home
import Page.Workspace
import Permissions
import Ports exposing (authSessionError, authTokenChanged, authUnauthorized, cytoscapeEdgeClicked, cytoscapeNodeClicked, disconnectWebSocket, localStorageReceived, loginAuth, logoutAuth, onMainContentScroll)
import Route exposing (loadWorkspaceData)
import Toast
import Types exposing (..)
import Url


type AppShellOwnedMsg
    = SelectWorkspaceMsg String
    | SwitchTabMsg WorkspaceTab
    | SessionContextLoadedMsg (Maybe String) (Result Http.Error Api.SessionContext)
    | AuthUnauthorizedMsg
    | AuthTokenChangedMsg Bool
    | AuthSessionErrorMsg String
    | LoginRequestedMsg
    | LogoutRequestedMsg
    | LocalStorageLoadedMsg Encode.Value
    | GlobalKeyDownMsg Int
    | MainContentScrolledMsg Float
    | NoOpMsg


initModel : Nav.Key -> Url.Url -> Page -> Flags -> Maybe Decode.Value -> { tab : WorkspaceTab, focus : Maybe ( String, String ) } -> Model
initModel key url page flags storedFilters frag =
    let
        baseModel =
            { key = key
            , url = url
            , page = page
            , flags = flags
            , auth = { status = AuthBooting, mode = Nothing }
            , sessionContext = Nothing
            , selectedWorkspaceId = Nothing
            , activeTab = frag.tab
            , mainContentScrollY = 0
            , workspaces = Dict.empty
            , projects = Dict.empty
            , tasks = Dict.empty
            , memories = Dict.empty
            , toast = Toast.init
            , webSocket = Feature.WebSocket.init
            , dataLoading = Feature.DataLoading.init
            , search = Feature.Search.init
            , editing = Feature.Editing.init
            , memory = Feature.Memory.init
            , dependencies = Feature.Dependencies.init
            , cards = Feature.Cards.init
            , graph = Feature.Graph.init
            , dragDrop = Feature.DragDrop.init
            , focus = Feature.Focus.init frag.focus
            , mutations = Feature.Mutations.init
            , groups = Feature.Groups.init
            , auditLog = Feature.AuditLog.init
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
        , dataLoading = Feature.DataLoading.prepareForPageLoad page model.dataLoading
    }


handleOwned : AppShellOwnedMsg -> Model -> ( Model, Cmd Msg )
handleOwned ownedMsg model =
    case ownedMsg of
        SelectWorkspaceMsg wsId ->
            ( model, Nav.pushUrl model.key ("/workspace/" ++ wsId) )

        SwitchTabMsg tab ->
            let
                newModel =
                    model
                        |> Feature.Editing.clearForTabSwitch
                        |> Feature.Memory.clearForTabSwitch
                        |> (\currentModel -> { currentModel | activeTab = tab })
            in
            ( newModel, replaceFragment newModel )

        SessionContextLoadedMsg expectedWorkspace result ->
            if sessionContextResponseMatches expectedWorkspace model then
                case result of
                    Ok sessionContext ->
                        let
                            sessionBootstrapCmd =
                                bootstrapAfterSession expectedWorkspace sessionContext model

                            mMembershipWorkspaceId =
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

                            fetchAuditCmd =
                                case ( expectedWorkspace, model.page ) of
                                    ( Nothing, AuditLogPage ) ->
                                        if sessionContext.globalPermissions.superadmin then
                                            Api.fetchAuditLog model.flags.apiUrl model.auditLog.filters GotAuditLog

                                        else
                                            Cmd.none

                                    _ ->
                                        Cmd.none
                        in
                        ( { model | auth = { status = AuthReady, mode = Just sessionContext.authMode }, sessionContext = Just sessionContext, workspaceAdmin = nextWorkspaceAdmin }
                            |> updateLoadingAfterSession expectedWorkspace sessionContext
                        , Cmd.batch [ sessionBootstrapCmd, fetchMembershipsCmd, fetchAuditCmd ]
                        )

                    Err _ ->
                        ( clearSessionScopedState
                            { model
                                | auth = { status = authStatusFromSessionError result, mode = model.auth.mode }
                                , sessionContext = Nothing
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
                    Toast.addToast Warning unauthorizedMessage
                        (clearSessionScopedState
                            { model
                                | auth = { status = AuthRequired, mode = model.auth.mode }
                                , sessionContext = Nothing
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
                ( preparedModel
                , Cmd.batch [ disconnectWebSocket (), Api.fetchSessionContext model.flags.apiUrl expectedWorkspace (GotSessionContext expectedWorkspace) ]
                )

            else
                let
                    ( toastedModel, toastCmd ) =
                        if Permissions.isLocalMode model then
                            Toast.addToast Warning "Local auth token was removed; local session will be refreshed when credentials are restored."
                                (clearSessionScopedState { model | flags = updatedFlags, auth = { status = AuthRequired, mode = model.auth.mode }, sessionContext = Nothing })

                        else
                            Toast.addToast Warning "Signed out. Sign in again to continue."
                                (clearSessionScopedState { model | flags = updatedFlags, auth = { status = AuthRequired, mode = model.auth.mode }, sessionContext = Nothing })
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
            ( clearSessionScopedState { model | flags = updatedFlags, auth = { status = AuthRequired, mode = model.auth.mode }, sessionContext = Nothing }
            , Cmd.batch [ disconnectWebSocket (), logoutAuth () ]
            )

        LocalStorageLoadedMsg json ->
            ( applyStoredFiltersIfCurrentWorkspace json model, Cmd.none )

        GlobalKeyDownMsg keyCode ->
            if keyCode == 27 then
                case List.filterMap identity
                    [ Feature.Cards.handleEscape model
                    , Feature.WorkspaceAdmin.handleEscape model
                    , Feature.DragDrop.handleEscape model
                    , Feature.Dependencies.handleEscape model
                    , Feature.Memory.handleEscape model
                    , Feature.Editing.handleEscape model
                    ]
                    |> List.head of
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


sessionContextResponseMatches : Maybe String -> Model -> Bool
sessionContextResponseMatches expectedWorkspace model =
    case expectedWorkspace of
        Just wsId ->
            case model.page of
                WorkspacePage currentWsId ->
                    currentWsId == wsId

                MemoryGraphPage ->
                    model.selectedWorkspaceId == Just wsId

                _ ->
                    False

        Nothing ->
            case model.page of
                WorkspacePage _ ->
                    False

                MemoryGraphPage ->
                    model.selectedWorkspaceId == Nothing

                _ ->
                    True


currentSessionWorkspace : Model -> Maybe String
currentSessionWorkspace model =
    case model.page of
        WorkspacePage wsId ->
            Just wsId

        MemoryGraphPage ->
            model.selectedWorkspaceId

        _ ->
            Nothing


bootstrapAfterSession : Maybe String -> Api.SessionContext -> Model -> Cmd Msg
bootstrapAfterSession expectedWorkspace sessionContext model =
    let
        canListGlobal =
            sessionContext.globalPermissions.superadmin

        globalCmds =
            if canListGlobal then
                [ Api.fetchWorkspaces model.flags.apiUrl GotWorkspaces
                , Api.fetchWorkspaceGroups model.flags.apiUrl GotWorkspaceGroups
                ]

            else
                []

        ( workspaceCmds, shouldKeepWebSocket ) =
            case model.page of
                WorkspacePage currentWsId ->
                    if expectedWorkspace == Just currentWsId && sessionCanReadWorkspace currentWsId sessionContext then
                        ( [ Api.fetchWorkspace model.flags.apiUrl currentWsId (GotWorkspace currentWsId)
                          , loadWorkspaceData model.flags.apiUrl currentWsId model.dataLoading.activeWorkspaceLoadToken
                          , Feature.WebSocket.connectCmd model.flags sessionContext currentWsId
                          ]
                        , True
                        )

                    else
                        ( [], False )

                MemoryGraphPage ->
                    case ( expectedWorkspace, model.selectedWorkspaceId ) of
                        ( Just expectedWsId, Just selectedWsId ) ->
                            if expectedWsId == selectedWsId && sessionCanReadWorkspace selectedWsId sessionContext then
                                ( [ Api.fetchVisualization model.flags.apiUrl selectedWsId (GotVisualization selectedWsId) ], False )

                            else
                                ( [], False )

                        _ ->
                            ( [], False )

                _ ->
                    ( [], False )

        websocketCmds =
            if shouldKeepWebSocket then
                []

            else
                [ disconnectWebSocket () ]
    in
    Cmd.batch (globalCmds ++ workspaceCmds ++ websocketCmds)


sessionCanReadWorkspace : String -> Api.SessionContext -> Bool
sessionCanReadWorkspace wsId sessionContext =
    sessionContext.globalPermissions.superadmin
        || (sessionContext.workspace
                |> Maybe.map (\workspaceContext -> workspaceContext.workspaceId == wsId && workspaceContext.canRead)
                |> Maybe.withDefault False
           )


updateLoadingAfterSession : Maybe String -> Api.SessionContext -> Model -> Model
updateLoadingAfterSession expectedWorkspace sessionContext model =
    let
        currentLoading =
            model.dataLoading

        shouldLoadWorkspaceData =
            case model.page of
                WorkspacePage wsId ->
                    expectedWorkspace == Just wsId && sessionCanReadWorkspace wsId sessionContext

                _ ->
                    False

        nextWebSocket =
            if shouldLoadWorkspaceData then
                model.webSocket

            else
                { state = Disconnected }

        nextWorkspaces =
            if sessionContext.globalPermissions.superadmin then
                model.workspaces

            else
                case expectedWorkspace of
                    Just wsId ->
                        Dict.filter (\id _ -> id == wsId) model.workspaces

                    Nothing ->
                        Dict.empty

        nextGroups =
            if sessionContext.globalPermissions.superadmin then
                model.groups

            else
                Feature.Groups.init

        updatedLoading =
            { currentLoading
                | loadingWorkspaces = sessionContext.globalPermissions.superadmin && currentLoading.loadingWorkspaces
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
    { model | dataLoading = updatedLoading, webSocket = nextWebSocket, workspaces = nextWorkspaces, groups = nextGroups }


stopAllLoading : DataLoadingModel -> DataLoadingModel
stopAllLoading dataLoading =
    { dataLoading
        | loadingWorkspaces = False
        , loadingWorkspaceData = False
        , pendingWorkspaceLoads = 0
        , activeWorkspaceLoadToken = Nothing
    }


clearSessionScopedState : Model -> Model
clearSessionScopedState model =
    { model
        | workspaces = Dict.empty
        , projects = Dict.empty
        , tasks = Dict.empty
        , memories = Dict.empty
        , dataLoading = stopAllLoading model.dataLoading
        , search = Feature.Search.init
        , editing = Feature.Editing.init
        , memory = Feature.Memory.init
        , dependencies = Feature.Dependencies.init
        , cards = Feature.Cards.init
        , graph = Feature.Graph.init
        , dragDrop = Feature.DragDrop.init
        , focus = Feature.Focus.init Nothing
        , groups = Feature.Groups.init
        , auditLog = Feature.AuditLog.init
        , workspaceAdmin = Feature.WorkspaceAdmin.init
        , webSocket = { state = Disconnected }
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
        , cytoscapeNodeClicked CytoscapeNodeClicked
        , cytoscapeEdgeClicked CytoscapeEdgeClicked
        , Browser.Events.onKeyDown (Decode.map GlobalKeyDown (Decode.field "keyCode" Decode.int))
        , localStorageReceived LocalStorageLoaded
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
            , Keyed.node "div" [ class "main-content", id "main-content-scroll" ]
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

        MemoryGraphPage ->
            "graph"

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

                MemoryGraphPage ->
                    Feature.Graph.viewGraphPage model

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
