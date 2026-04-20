module Toast exposing (addToast, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Process
import Task as ElmTask
import Types exposing (..)


init : ToastModel
init =
    { toasts = []
    , nextToastId = 0
    }



-- ADD


addToast : ToastLevel -> String -> Model -> ( Model, Cmd Msg )
addToast level message model =
    let
        tid =
            model.toast.nextToastId

        dismissDelay =
            case level of
                Error ->
                    8000

                Warning ->
                    6000

                _ ->
                    4000

        currentToast =
            model.toast

        updatedToast =
            { currentToast
                | toasts = model.toast.toasts ++ [ Toast tid message level ]
                , nextToastId = tid + 1
            }
    in
    ( { model
        | toast = updatedToast
      }
    , ElmTask.perform (\_ -> AutoDismissToast tid) (Process.sleep dismissDelay)
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DismissToast toastId ->
            let
                currentToast =
                    model.toast

                updatedToast =
                    { currentToast | toasts = List.filter (\t -> t.id /= toastId) model.toast.toasts }
            in
            ( { model | toast = updatedToast }, Cmd.none )

        AutoDismissToast toastId ->
            let
                currentToast =
                    model.toast

                updatedToast =
                    { currentToast | toasts = List.filter (\t -> t.id /= toastId) model.toast.toasts }
            in
            ( { model | toast = updatedToast }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : ToastModel -> Html Msg
view toastModel =
    div [ class "toast-container" ]
        (List.map viewToast toastModel.toasts)


viewToast : Toast -> Html Msg
viewToast toast =
    div
        [ class ("toast toast-" ++ toastLevelClass toast.level)
        , onClick (DismissToast toast.id)
        ]
        [ text toast.message ]


toastLevelClass : ToastLevel -> String
toastLevelClass level =
    case level of
        Info ->
            "info"

        Success ->
            "success"

        Warning ->
            "warning"

        Error ->
            "error"
