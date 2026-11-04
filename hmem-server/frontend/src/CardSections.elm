module CardSections exposing (cardDisclosure, detailsId, disclosure, disclosureIcon, fact, facts, focusIcon, horizontalDisclosureIcon, metadata, panel, projectTabs, section, taskTabs)

import Html exposing (Html, div, footer, h4, label, span, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as SvgAttr


projectTabs : String -> String -> (String -> msg) -> Html msg
projectTabs entityId current select =
    sectionTabs True "Project" entityId current select [ ( "work", "Work" ), ( "history", "History" ) ]


taskTabs : Maybe Int -> Maybe Int -> String -> String -> (String -> msg) -> Html msg
taskTabs sourceTotal evidenceTotal entityId current select =
    let
        countedLabel label total =
            label ++ (total |> Maybe.map (String.fromInt >> (++) " · ") |> Maybe.withDefault "")
    in
    sectionTabs (evidenceTotal /= Just 0) "Task" entityId current select
        [ ( "work", "Work" ), ( "sources", countedLabel "Sources" sourceTotal )
        , ( "evidence", countedLabel "Evidence" evidenceTotal )
        , ( "history", "History" )
        ]


sectionTabs : Bool -> String -> String -> String -> (String -> msg) -> List ( String, String ) -> Html msg
sectionTabs evidenceAvailable kind entityId current select sections =
    Html.nav [ class "card-local-sections", attribute "aria-label" (kind ++ " sections") ]
        (List.map
            (\( name, label ) ->
                Html.button
                    ([ class ("card-section-button" ++ (if current == name then " selected" else ""))
                    , Html.Attributes.type_ "button"
                    , Html.Attributes.id (String.toLower kind ++ "-" ++ entityId ++ "-" ++ name ++ "-button")
                    , attribute "aria-controls" (String.toLower kind ++ "-" ++ entityId ++ "-" ++ name)
                    , attribute "aria-pressed" (if current == name then "true" else "false")
                    , Html.Attributes.disabled (name == "evidence" && not evidenceAvailable)
                    , onClick (select name)
                    ]
                    ++ (if kind == "Task" && List.member name [ "sources", "evidence" ] then
                            [ attribute "aria-label" (if name == "sources" then "Sources" else "Evidence")
                            , attribute "aria-description" label
                            ]
                        else
                            []
                       )
                    )
                    [ text label ]
            )
            sections
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


cardDisclosure : Bool -> String -> String -> msg -> Html msg
cardDisclosure expanded regionId name message =
    let action = (if expanded then "Hide details for " else "Show details for ") ++ name in
    Html.button
        [ class ("btn-extras-toggle card-details-control" ++ (if expanded then " is-expanded" else ""))
        , Html.Attributes.type_ "button"
        , Html.Attributes.id (regionId ++ "-toggle")
        , Html.Attributes.title action
        , attribute "aria-label" action
        , attribute "aria-expanded" (if expanded then "true" else "false")
        , attribute "aria-controls" regionId
        , onClick message
        ]
        [ disclosureIcon expanded
        , span [ class "card-details-label" ] [ text (if expanded then "Hide Details" else "Details") ]
        ]


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


focusIcon : Html msg
focusIcon =
    chevronIcon "M6 2 H2 V6 M10 2 H14 V6 M2 10 V14 H6 M14 10 V14 H10 M6 8 H10 M8 6 V10"


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
