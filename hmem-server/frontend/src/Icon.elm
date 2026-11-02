module Icon exposing (view)

{-| Local allowlist of Lucide 0.468.0 icons.
Primitives: https://github.com/lucide-icons/lucide/tree/0.468.0/icons
License and attribution: ../licenses/lucide-ISC.txt
No runtime icon library, network request, or caller-supplied SVG markup.
-}

import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Svg exposing (node, svg)
import Svg.Attributes exposing (fill, height, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox, width)


view : String -> Html msg
view name =
    svg
        [ attribute "class" "hm-icon"
        , attribute "data-lucide" name
        , attribute "aria-hidden" "true"
        , attribute "focusable" "false"
        , width "16", height "16", viewBox "0 0 24 24"
        , fill "none", stroke "currentColor", strokeWidth "1.6"
        , strokeLinecap "round", strokeLinejoin "round"
        ]
        (case name of
            "panels-top-left" ->
                [ node "rect" [ attribute "width" "18", attribute "height" "18", attribute "x" "3", attribute "y" "3", attribute "rx" "2" ] []
                , node "path" [ attribute "d" "M3 9h18" ] []
                , node "path" [ attribute "d" "M9 21V9" ] []
                ]

            "folder-git-2" ->
                [ node "path" [ attribute "d" "M9 20H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h3.9a2 2 0 0 1 1.69.9l.81 1.2a2 2 0 0 0 1.67.9H20a2 2 0 0 1 2 2v5" ] []
                , node "circle" [ attribute "cx" "13", attribute "cy" "12", attribute "r" "2" ] []
                , node "path" [ attribute "d" "M18 19c-2.8 0-5-2.2-5-5v8" ] []
                , node "circle" [ attribute "cx" "20", attribute "cy" "19", attribute "r" "2" ] []
                ]

            "folder-kanban" ->
                [ node "path" [ attribute "d" "M4 20h16a2 2 0 0 0 2-2V8a2 2 0 0 0-2-2h-7.93a2 2 0 0 1-1.66-.9l-.82-1.2A2 2 0 0 0 7.93 3H4a2 2 0 0 0-2 2v13c0 1.1.9 2 2 2Z" ] []
                , node "path" [ attribute "d" "M8 10v4" ] []
                , node "path" [ attribute "d" "M12 10v2" ] []
                , node "path" [ attribute "d" "M16 10v6" ] []
                ]

            "terminal" ->
                [ node "polyline" [ attribute "points" "4 17 10 11 4 5" ] []
                , node "line" [ attribute "x1" "12", attribute "x2" "20", attribute "y1" "19", attribute "y2" "19" ] []
                ]

            "scan-eye" ->
                [ node "path" [ attribute "d" "M3 7V5a2 2 0 0 1 2-2h2" ] []
                , node "path" [ attribute "d" "M17 3h2a2 2 0 0 1 2 2v2" ] []
                , node "path" [ attribute "d" "M21 17v2a2 2 0 0 1-2 2h-2" ] []
                , node "path" [ attribute "d" "M7 21H5a2 2 0 0 1-2-2v-2" ] []
                , node "circle" [ attribute "cx" "12", attribute "cy" "12", attribute "r" "1" ] []
                , node "path" [ attribute "d" "M18.944 12.33a1 1 0 0 0 0-.66 7.5 7.5 0 0 0-13.888 0 1 1 0 0 0 0 .66 7.5 7.5 0 0 0 13.888 0" ] []
                ]

            "history" ->
                [ node "path" [ attribute "d" "M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8" ] []
                , node "path" [ attribute "d" "M3 3v5h5" ] []
                , node "path" [ attribute "d" "M12 7v5l4 2" ] []
                ]

            "search" ->
                [ node "circle" [ attribute "cx" "11", attribute "cy" "11", attribute "r" "8" ] []
                , node "path" [ attribute "d" "m21 21-4.3-4.3" ] []
                ]

            "shield-check" ->
                [ node "path" [ attribute "d" "M20 13c0 5-3.5 7.5-7.66 8.95a1 1 0 0 1-.67-.01C7.5 20.5 4 18 4 13V6a1 1 0 0 1 1-1c2 0 4.5-1.2 6.24-2.72a1.17 1.17 0 0 1 1.52 0C14.51 3.81 17 5 19 5a1 1 0 0 1 1 1z" ] []
                , node "path" [ attribute "d" "m9 12 2 2 4-4" ] []
                ]

            "settings-2" ->
                [ node "path" [ attribute "d" "M20 7h-9" ] []
                , node "path" [ attribute "d" "M14 17H5" ] []
                , node "circle" [ attribute "cx" "17", attribute "cy" "17", attribute "r" "3" ] []
                , node "circle" [ attribute "cx" "7", attribute "cy" "7", attribute "r" "3" ] []
                ]

            "list-tree" ->
                [ node "path" [ attribute "d" "M21 12h-8" ] []
                , node "path" [ attribute "d" "M21 6H8" ] []
                , node "path" [ attribute "d" "M21 18h-8" ] []
                , node "path" [ attribute "d" "M3 6v4c0 1.1.9 2 2 2h3" ] []
                , node "path" [ attribute "d" "M3 10v6c0 1.1.9 2 2 2h3" ] []
                ]

            "unfold-vertical" ->
                [ node "path" [ attribute "d" "M12 22v-6" ] []
                , node "path" [ attribute "d" "M12 8V2" ] []
                , node "path" [ attribute "d" "M4 12H2" ] []
                , node "path" [ attribute "d" "M10 12H8" ] []
                , node "path" [ attribute "d" "M16 12h-2" ] []
                , node "path" [ attribute "d" "M22 12h-2" ] []
                , node "path" [ attribute "d" "m15 19-3 3-3-3" ] []
                , node "path" [ attribute "d" "m15 5-3-3-3 3" ] []
                ]

            "fold-vertical" ->
                [ node "path" [ attribute "d" "M12 22v-6" ] []
                , node "path" [ attribute "d" "M12 8V2" ] []
                , node "path" [ attribute "d" "M4 12H2" ] []
                , node "path" [ attribute "d" "M10 12H8" ] []
                , node "path" [ attribute "d" "M16 12h-2" ] []
                , node "path" [ attribute "d" "M22 12h-2" ] []
                , node "path" [ attribute "d" "m15 19-3-3-3 3" ] []
                , node "path" [ attribute "d" "m15 5-3 3-3-3" ] []
                ]

            "plus" ->
                [ node "path" [ attribute "d" "M5 12h14" ] []
                , node "path" [ attribute "d" "M12 5v14" ] []
                ]

            "chevron-down" ->
                [ node "path" [ attribute "d" "m6 9 6 6 6-6" ] []
                ]

            "chevron-right" ->
                [ node "path" [ attribute "d" "m9 18 6-6-6-6" ] []
                ]

            "layout-list" ->
                [ node "rect" [ attribute "width" "7", attribute "height" "7", attribute "x" "3", attribute "y" "3", attribute "rx" "1" ] []
                , node "rect" [ attribute "width" "7", attribute "height" "7", attribute "x" "3", attribute "y" "14", attribute "rx" "1" ] []
                , node "path" [ attribute "d" "M14 4h7" ] []
                , node "path" [ attribute "d" "M14 9h7" ] []
                , node "path" [ attribute "d" "M14 15h7" ] []
                , node "path" [ attribute "d" "M14 20h7" ] []
                ]

            "user" ->
                [ node "path" [ attribute "d" "M19 21v-2a4 4 0 0 0-4-4H9a4 4 0 0 0-4 4v2" ] []
                , node "circle" [ attribute "cx" "12", attribute "cy" "7", attribute "r" "4" ] []
                ]

            "building-2" ->
                [ node "path" [ attribute "d" "M6 22V4a2 2 0 0 1 2-2h8a2 2 0 0 1 2 2v18Z" ] []
                , node "path" [ attribute "d" "M6 12H4a2 2 0 0 0-2 2v6a2 2 0 0 0 2 2h2" ] []
                , node "path" [ attribute "d" "M18 9h2a2 2 0 0 1 2 2v9a2 2 0 0 1-2 2h-2" ] []
                , node "path" [ attribute "d" "M10 6h4" ] []
                , node "path" [ attribute "d" "M10 10h4" ] []
                , node "path" [ attribute "d" "M10 14h4" ] []
                , node "path" [ attribute "d" "M10 18h4" ] []
                ]

            "x" ->
                [ node "path" [ attribute "d" "M18 6 6 18" ] []
                , node "path" [ attribute "d" "m6 6 12 12" ] []
                ]

            _ ->
                []
        )
