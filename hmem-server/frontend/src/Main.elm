module Main exposing (main)

import Api exposing (..)
import AppShell
import Browser
import Browser.Navigation as Nav
import Helpers exposing (parseFragment)
import Json.Decode as Decode
import Route exposing (handleUrlChange, handleUrlRequest, loadWorkspaceData, urlToPage)
import Types exposing (..)
import UpdateRouter
import Url



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

        model =
            AppShell.initModel key url page flags decoded.storedFilters frag

        cmds =
            [ AppShell.connectCmd flags.wsUrl
            , Api.fetchWorkspaces flags.apiUrl GotWorkspaces
            , Api.fetchWorkspaceGroups flags.apiUrl GotWorkspaceGroups
            ]

        pageCmd =
            case page of
                WorkspacePage wsId ->
                    loadWorkspaceData flags.apiUrl wsId (Just model.dataLoading.nextWorkspaceLoadToken)

                _ ->
                    Cmd.none

    in
    ( AppShell.finalizeInit page model
    , Cmd.batch (cmds ++ [ pageCmd ])
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case UpdateRouter.update msg model of
        Ok result ->
            result

        Err mainOwnedMsg ->
            case mainOwnedMsg of
                UpdateRouter.HandleUrlRequest urlRequest ->
                    handleUrlRequest urlRequest model

                UpdateRouter.HandleUrlChange url ->
                    handleUrlChange url model

                UpdateRouter.HandleInAppShell ownedMsg ->
                    AppShell.handleOwned ownedMsg model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    AppShell.subscriptions



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
