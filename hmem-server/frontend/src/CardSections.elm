module CardSections exposing (detailsId, disclosure, disclosureIcon, fact, facts, horizontalDisclosureIcon, metadata, panel, section, tabs)

import Html exposing (Html, div, footer, h4, label, span, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as SvgAttr


tabs : String -> String -> String -> (String -> msg) -> Html msg
tabs kind entityId current select =
    Html.nav [ class "card-local-sections", attribute "aria-label" (kind ++ " sections") ]
        (List.map
            (\( name, label ) ->
                Html.button
                    [ class ("card-section-button" ++ (if current == name then " selected" else ""))
                    , Html.Attributes.type_ "button"
                    , Html.Attributes.id (String.toLower kind ++ "-" ++ entityId ++ "-" ++ name ++ "-button")
                    , attribute "aria-controls" (String.toLower kind ++ "-" ++ entityId ++ "-" ++ name)
                    , attribute "aria-pressed" (if current == name then "true" else "false")
                    , onClick (select name)
                    ]
                    [ text label ]
            )
            [ ( "work", "Work" ), ( "evidence", "Evidence" ), ( "history", "History" ) ]
        )


panel : String -> String -> String -> String -> List (Html msg) -> Html msg
panel kind entityId current name content =
    div
        [ class "card-section-panel"
        , Html.Attributes.id (kind ++ "-" ++ entityId ++ "-" ++ name)
        , attribute "role" "region"
        , attribute "aria-labelledby" (kind ++ "-" ++ entityId ++ "-" ++ name ++ "-button")
        , Html.Attributes.hidden (current /= name)
        ]
        content


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
    chevronIcon (if expanded then "M4 7 L8 3 L12 7 M4 12 L8 8 L12 12" else "M4 4 L8 8 L12 4 M4 9 L8 13 L12 9")


horizontalDisclosureIcon : Bool -> Html msg
horizontalDisclosureIcon expanded =
    chevronIcon (if expanded then "M7 4 L3 8 L7 12 M12 4 L8 8 L12 12" else "M4 4 L8 8 L4 12 M9 4 L13 8 L9 12")


chevronIcon : String -> Html msg
chevronIcon path =
    Svg.svg
        [ SvgAttr.width "16", SvgAttr.height "16", SvgAttr.viewBox "0 0 16 16"
        , SvgAttr.fill "none", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5"
        , SvgAttr.strokeLinecap "round", SvgAttr.strokeLinejoin "round"
        , attribute "aria-hidden" "true", attribute "focusable" "false"
        ]
        [ Svg.path [ SvgAttr.d path ] [] ]


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
