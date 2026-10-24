module Main exposing (main, update)

import Api exposing (..)
import AppShell
import Feature.Search
import Feature.AuditDiff
import Browser
import Browser.Events
import Task
import Browser.Navigation as Nav
import Feature.Cards as Cards
import Feature.Move
import Feature.DataLoading
import Feature.Observation
import Feature.TimestampDisplay
import Feature.SessionActivity
import Feature.Sessions
import Helpers exposing (parseFragment)
import Json.Decode as Decode
import Route exposing (handleUrlChange, handleUrlRequest, urlToPage)
import Types exposing (..)
import UpdateRouter
import Url
import Time



-- FLAGS


decodeFlags : Decode.Value -> { flags : Flags, storedFilters : Maybe Decode.Value }
decodeFlags raw =
    let
        flags =
            { apiUrl =
                Decode.decodeValue (Decode.field "apiUrl" Decode.string) raw
                    |> Result.withDefault ""
            , wsUrl =
                Decode.decodeValue (Decode.field "wsUrl" Decode.string) raw
                    |> Result.withDefault ""
            , sessionId =
                Decode.decodeValue (Decode.field "sessionId" Decode.string) raw
                    |> Result.withDefault "session"
            , runtimeMode =
                Decode.decodeValue (Decode.field "runtimeMode" Decode.string) raw
                    |> Result.withDefault "unknown"
            , authTokenStorageKey =
                Decode.decodeValue (Decode.field "authTokenStorageKey" Decode.string) raw
                    |> Result.withDefault "hmem-auth-token"
            , authTokenPresent =
                Decode.decodeValue (Decode.field "authTokenPresent" Decode.bool) raw
                    |> Result.withDefault False
            , loginUrl =
                decodeNullableStringField "loginUrl" raw
            , logoutUrl =
                decodeNullableStringField "logoutUrl" raw
            }

        storedFilters =
            Decode.decodeValue (Decode.field "storedFilters" Decode.value) raw
                |> Result.toMaybe
                |> Maybe.andThen
                    (\v ->
                        -- Decode.value passes null through; filter it out
                        case Decode.decodeValue (Decode.null Nothing) v of
                            Ok Nothing ->
                                Nothing

                            _ ->
                                Just v
                    )
    in
    { flags = flags, storedFilters = storedFilters }


decodeNullableStringField : String -> Decode.Value -> Maybe String
decodeNullableStringField fieldName raw =
    Decode.decodeValue (Decode.field fieldName (Decode.nullable Decode.string)) raw
        |> Result.withDefault Nothing



-- INIT


init : Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init rawFlags url key =
    let
        decoded =
            decodeFlags rawFlags

        flags =
            decoded.flags

        page =
            urlToPage url

        frag =
            parseFragment url.fragment

        initialModel =
            AppShell.initModel (Just key) url page flags decoded.storedFilters frag

        activity = initialModel.sessionActivity
        model = {initialModel | sessionActivity={activity | visible=Feature.SessionActivity.initialVisibility rawFlags}}

        cmds =
            [ Task.perform SessionActivityTick Time.now
            , Api.fetchSessionContext flags.apiUrl (sessionWorkspace page) (GotSessionContext 1 (sessionWorkspace page))
            ]

    in
    ( AppShell.finalizeInit page model
    , Cmd.batch cmds
    )


sessionWorkspace : Page -> Maybe String
sessionWorkspace page =
    case page of
        WorkspacePage wsId ->
            Just wsId

        _ ->
            Nothing



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updateCore msg model |> Feature.Search.reconcileNavigation msg model |> Feature.AuditDiff.reconcile model |> Feature.Sessions.reconcile |> Feature.SessionActivity.reconcile model |> Feature.TimestampDisplay.reconcile


updateCore : Msg -> Model -> ( Model, Cmd Msg )
updateCore msg model =
    case UpdateRouter.update msg model of
        Ok result ->
            Feature.Move.reconcile result

        Err mainOwnedMsg ->
            (case mainOwnedMsg of
                UpdateRouter.HandleUrlRequest urlRequest ->
                    handleUrlRequest urlRequest model

                UpdateRouter.HandleUrlChange url ->
                    Feature.Search.routeChanged url model |> Maybe.withDefault (handleUrlChange url model)

                UpdateRouter.HandleInAppShell ownedMsg ->
                    AppShell.prepareOwned ownedMsg model
            ) |> UpdateRouter.reconcileFocusedFilters model |> Feature.Observation.refreshViewport model |> Feature.DataLoading.syncPlanningCountsResult |> Cards.refreshViewport model |> Feature.Move.reconcile



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ AppShell.subscriptions, Browser.Events.onVisibilityChange (\visibility -> SessionActivityVisibility (visibility == Browser.Events.Visible)), if model.auth.status == AuthReady && model.sessionActivity.visible && not (List.isEmpty model.sessionActivity.order) then Time.every 1000 SessionActivityTick else Sub.none, if model.auth.status == AuthReady && not (List.isEmpty model.sessions.owned) then Time.every 1000 SessionsTick else Sub.none ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    AppShell.viewDocument model

-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
