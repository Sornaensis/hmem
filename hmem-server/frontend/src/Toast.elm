module Toast exposing (addToast, update, view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Process
import Task as ElmTask
import Types exposing (..)



-- ADD


addToast : ToastLevel -> String -> Model -> ( Model, Cmd Msg )
addToast level message model =
    let
        tid =
            model.nextToastId

        dismissDelay =
            case level of
                Error ->
                    8000

                Warning ->
                    6000

                _ ->
                    4000
    in
    ( { model
        | toasts = model.toasts ++ [ Toast tid message level ]
        , nextToastId = tid + 1
      }
    , ElmTask.perform (\_ -> AutoDismissToast tid) (Process.sleep dismissDelay)
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DismissToast toastId ->
            ( { model | toasts = List.filter (\t -> t.id /= toastId) model.toasts }, Cmd.none )

        AutoDismissToast toastId ->
            ( { model | toasts = List.filter (\t -> t.id /= toastId) model.toasts }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : List Toast -> Html Msg
view toasts =
    div [ class "toast-container" ]
        (List.map viewToast toasts)


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
