module Page.Home exposing
    ( viewHomePage
    )

import Feature.Groups
import Html exposing (Html)
import Types exposing (Model, Msg)


viewHomePage : Model -> Html Msg
viewHomePage model =
    Feature.Groups.viewHomePage model
