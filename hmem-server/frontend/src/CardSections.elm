module CardSections exposing (detailsId, disclosure, disclosureIcon, fact, facts, metadata, section)

import Html exposing (Html, div, footer, h4, label, span, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as SvgAttr


detailsId : String -> String -> String
detailsId kind entityId =
    kind ++ "-" ++ entityId ++ "-details"


disclosure : Bool -> String -> String -> msg -> Html msg
disclosure expanded regionId name message =
    let
        action = (if expanded then "Hide details for " else "Show details for ") ++ name
    in
    Html.button
        [ class "btn-extras-toggle"
        , Html.Attributes.type_ "button"
        , Html.Attributes.title action
        , attribute "aria-label" action
        , attribute "aria-expanded" (if expanded then "true" else "false")
        , attribute "aria-controls" regionId
        , onClick message
        ]
        [ disclosureIcon expanded ]


disclosureIcon : Bool -> Html msg
disclosureIcon expanded =
    Svg.svg
        [ SvgAttr.width "16", SvgAttr.height "16", SvgAttr.viewBox "0 0 16 16"
        , SvgAttr.fill "none", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5"
        , SvgAttr.strokeLinecap "round", SvgAttr.strokeLinejoin "round"
        , attribute "aria-hidden" "true", attribute "focusable" "false"
        ]
        [ Svg.path [ SvgAttr.d (if expanded then "M4 7 L8 3 L12 7 M4 12 L8 8 L12 12" else "M4 4 L8 8 L12 4 M4 9 L8 13 L12 9") ] [] ]


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
        [ div [ class "card-meta-group" ] children ]
