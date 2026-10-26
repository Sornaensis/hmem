module CardSections exposing (fact, facts, metadata, section)

import Html exposing (Html, details, div, footer, h4, label, span, summary, text)
import Html.Attributes exposing (attribute, class)


section : String -> List (Html msg) -> Html msg
section label children =
    Html.section [ class "card-content-section", attribute "aria-label" label ]
        (h4 [ class "card-section-heading" ] [ text label ] :: children)


facts : List (Html msg) -> Html msg
facts children =
    div [ class "card-actions card-facts" ] children


fact : String -> Html msg -> Html msg
fact name control =
    label [ class "card-fact" ]
        [ span [ class "card-fact-label" ] [ text name ], control ]


metadata : List (Html msg) -> Html msg
metadata children =
    footer [ class "card-footer" ]
        [ details [ class "card-metadata" ]
            [ summary [] [ text "Metadata" ]
            , div [ class "card-meta-group" ] children
            ]
        ]
