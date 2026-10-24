module SessionActivityLight exposing (view)

import Html exposing (Html,span,text)
import Html.Attributes exposing (attribute,class,title)


view : Bool -> String -> Html msg
view active label =
    if active then span [class "session-activity-light",attribute "role" "img",attribute "aria-label" label,title label] [span [class "session-activity-ring",attribute "aria-hidden" "true"] [],span [class "session-activity-core",attribute "aria-hidden" "true"] []]
    else text ""
