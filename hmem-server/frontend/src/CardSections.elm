module CardSections exposing (metadata, section)

import Html exposing (Html, details, div, h4, summary, text)
import Html.Attributes exposing (attribute, class)


section : String -> List (Html msg) -> Html msg
section label children =
    Html.section [ class "card-content-section", attribute "aria-label" label ]
        (h4 [ class "card-section-heading" ] [ text label ] :: children)


metadata : List (Html msg) -> Html msg
metadata children =
    details [ class "card-metadata" ]
        [ summary [] [ text "Metadata" ]
        , div [ class "card-meta-group" ] children
        ]
