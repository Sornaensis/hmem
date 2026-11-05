module Spinner exposing (block, inline, preserveFocus)

import Html exposing (Attribute, Html, div, span)
import Html.Attributes exposing (attribute, class)


inline : String -> Html msg
inline label =
    span [ class "app-spinner", attribute "role" "status", attribute "aria-label" label ] []


block : String -> Html msg
block label =
    div [ class "loading-indicator" ] [ inline label ]


preserveFocus : String -> Attribute msg
preserveFocus anchorId =
    attribute "data-pending-focus" anchorId
