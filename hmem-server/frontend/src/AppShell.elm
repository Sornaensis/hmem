module AppShell exposing (AppShellOwnedMsg(..), connectCmd, finalizeInit, handleOwned, initModel, subscriptions, viewDocument)

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
import Helpers exposing (applyStoredFiltersIfCurrentWorkspace, replaceFragment)
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import Page.Home
import Page.Workspace
import Ports exposing (cytoscapeEdgeClicked, cytoscapeNodeClicked, localStorageReceived, onMainContentScroll)
import Toast
import Types exposing (..)
import Url


type AppShellOwnedMsg
    = SelectWorkspaceMsg String
    | SwitchTabMsg WorkspaceTab
    | LocalStorageLoadedMsg Encode.Value
    | GlobalKeyDownMsg Int
    | MainContentScrolledMsg Float
    | NoOpMsg


connectCmd : String -> Cmd Msg
connectCmd =
    Feature.WebSocket.connectCmd


initModel : Nav.Key -> Url.Url -> Page -> Flags -> Maybe Decode.Value -> { tab : WorkspaceTab, focus : Maybe ( String, String ) } -> Model
initModel key url page flags storedFilters frag =
    let
        baseModel =
            { key = key
            , url = url
            , page = page
            , flags = flags
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

        LocalStorageLoadedMsg json ->
            ( applyStoredFiltersIfCurrentWorkspace json model, Cmd.none )

        GlobalKeyDownMsg keyCode ->
            if keyCode == 27 then
                case List.filterMap identity
                    [ Feature.Cards.handleEscape model
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


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Feature.WebSocket.subscriptions
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
            [ Feature.Groups.viewSidebar model
            , Keyed.node "div" [ class "main-content", id "main-content-scroll" ]
                [ ( pageKey model.page, viewPage model ) ]
            , Toast.view model.toast
            , viewConnectionStatus model.webSocket.state
            , Feature.Editing.viewCreateFormModal model
            , Feature.DragDrop.viewDropActionModal model
            , Feature.Cards.viewDeleteConfirmModal model
            , Feature.AuditLog.viewRevertConfirmModal model
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
