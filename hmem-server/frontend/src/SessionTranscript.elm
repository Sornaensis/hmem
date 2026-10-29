module SessionTranscript exposing (Block, Item(..), Piece, project, Fragment, atom, container, formatted, formattedAst, renderPage)

{-| Presentation of the current bounded record window. Keys retain source block
positions; original records and byte offsets are unchanged.
-}

import Json.Decode as Decode
import Dict
import Types exposing (SessionRecord)
import Html exposing (Html)
import Html.Attributes as Attributes
import Markdown.Block as MarkdownBlock
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Char
import Array
import Set


{-| Deferred HTML carries a conservative serialized-DOM byte cost. In
particular, a resolved Markdown reference URL is counted at every occurrence.
Only the admitted tree window invokes HTML constructors.
-}
type Fragment msg
    = Atom Int (() -> Html msg)
    | Container Int (List (Fragment msg)) (List (Html msg) -> Html msg)
    | Table (List (Fragment msg))
    | TableSection Bool (List (Fragment msg))
    | TableRow (List (Fragment msg))
    | TableCell Bool (Maybe MarkdownBlock.Alignment) (List (Fragment msg))


cellHtml : Bool -> Int -> Maybe MarkdownBlock.Alignment -> List (Html msg) -> Html msg
cellHtml header index alignment children =
    let
        align = alignment |> Maybe.map (\value -> Attributes.align (case value of
            MarkdownBlock.AlignLeft -> "left"
            MarkdownBlock.AlignCenter -> "center"
            MarkdownBlock.AlignRight -> "right")) |> Maybe.map List.singleton |> Maybe.withDefault []
        attributes = [Attributes.attribute "aria-colindex" (String.fromInt (index+1))] ++ align ++ (if header then [Attributes.attribute "scope" "col"] else [])
    in (if header then Html.th else Html.td) attributes children


fillCells : Bool -> Int -> List (Int,Html msg) -> List (Html msg)
fillCells header count cells =
    let
        gap size = if size<=0 then [] else
            (if header then Html.th else Html.td) [Attributes.attribute "colspan" (String.fromInt (min 1000 size)),Attributes.attribute "aria-hidden" "true"] [] :: gap (size-1000)
        step (index,node) (previous,nodes) = (index+1,nodes ++ gap (index-previous) ++ [node])
        (last,completedCells)=List.foldl step (0,[]) cells
    in completedCells ++ gap (count-last)


costUpTo : Int -> Fragment msg -> Int
costUpTo limit fragment =
    let sum own children = List.foldl (\child total -> if total>=limit then total else total+costUpTo (limit-total) child) own children in
    case fragment of
        Atom cost _ -> cost
        Container cost children _ -> sum cost children
        Table children -> sum 128 children
        TableSection _ children -> sum 128 children
        TableRow children -> sum 128 children
        TableCell _ _ children -> sum 128 children


atom : Int -> (() -> Html msg) -> Fragment msg
atom = Atom


container : Int -> (List (Html msg) -> Html msg) -> List (Fragment msg) -> Fragment msg
container cost render children = Container cost children render


renderPage : Int -> Int -> List (Fragment msg) -> { nodes : List (Html msg), more : Bool, error : Bool, part : Int }
renderPage budget target fragments =
    let
        walk ancestors fragment state =
            case fragment of
                Atom cost render ->
                    let
                        total = ancestors + cost
                        next = if state.used > 0 && state.used + total > budget then {state | page=state.page+1,used=0} else state
                    in
                    if total > budget then (Nothing,{next | error=True})
                    else (if next.page == next.target then Just (render ()) else Nothing,{next | used=next.used+total})
                Container cost children render ->
                    if List.isEmpty children then walk ancestors (Atom cost (\_ -> render [])) state
                    else
                        let (nodes,next)=walkList (ancestors+cost) children state in
                        (if List.isEmpty nodes then Nothing else Just (render nodes),next)
                Table sections -> walkTable ancestors sections state
                TableSection header children -> walk ancestors (Container 128 children (if header then Html.thead [] else Html.tbody [])) state
                TableRow children -> walk ancestors (Container 128 children (Html.tr [])) state
                TableCell header alignment children -> walk ancestors (Container 128 children (cellHtml header 0 alignment)) state
        walkList ancestors values state =
            List.foldl (\fragment (nodes,current) -> let (node,next)=walk ancestors fragment current in (node |> Maybe.map (\value -> value::nodes) |> Maybe.withDefault nodes,next)) ([],state) values
                |> (\(nodes,next) -> (List.reverse nodes,next))
        walkTable ancestors sections state =
            let
                rows header = sections |> List.concatMap (\section -> case section of
                    TableSection isHeader children -> if header == isHeader then List.filterMap (\child -> case child of
                        TableRow cells -> Just cells
                        _ -> Nothing) children else []
                    _ -> [])
                headerRows=rows True
                bodyRows=rows False
                headers=List.head headerRows |> Maybe.withDefault [] |> Array.fromList
                columns=max (Array.length headers) (List.map List.length bodyRows |> List.maximum |> Maybe.withDefault 0)
                gapCost=2048+160*(columns//1000+3)
                walkCells header values current =
                    List.indexedMap Tuple.pair values |> List.foldl (\(index,fragment) (selected,previous) ->
                        let
                            context=if header then 0 else Array.get index headers |> Maybe.map (costUpTo 262144 >> (+) 512 >> min 262144) |> Maybe.withDefault 0
                            converted=case fragment of
                                TableCell _ alignment children -> Container (128+gapCost+context) children (cellHtml header index alignment)
                                _ -> Container (128+gapCost+context) [fragment] (cellHtml header index Nothing)
                            (node,next)=walk (ancestors+384) converted previous
                        in (node |> Maybe.map (\value -> (index,value)::selected) |> Maybe.withDefault selected,next)) ([],current)
                        |> (\(selected,next) -> (List.reverse selected,next))
                walkRows header values current = List.indexedMap Tuple.pair values |> List.foldl (\(index,row) (selected,previous) ->
                    let (nodes,next)=walkCells header row previous in
                    (if List.isEmpty nodes then selected else (index,nodes)::selected,next)) ([],current)
                    |> (\(selected,next) -> (List.reverse selected,next))
                (selectedHeaders,afterHeader)=walkRows True headerRows state
                (selectedBody,afterBody)=walkRows False bodyRows afterHeader
                original=List.concatMap Tuple.second selectedHeaders
                selectedColumns=(List.map Tuple.first original ++ (selectedBody |> List.concatMap (Tuple.second >> List.map Tuple.first))) |> Set.fromList |> Set.toList
                contextHeader index =
                    case List.filter (Tuple.first >> (==) index) original |> List.head of
                        Just existing -> Just existing
                        Nothing -> Array.get index headers |> Maybe.andThen (\fragment -> case fragment of
                            TableCell _ alignment children ->
                                let part=renderPage 261632 0 children in
                                Just (index,cellHtml True index alignment (part.nodes ++ (if part.more then [Html.span [Attributes.class "session-table-header-continuation"] [Html.text "Header continues in earlier presentation pages."]] else [])))
                            _ -> Nothing)
                renderedHeaders=List.filterMap contextHeader selectedColumns
                rowNode header (index,nodes)=Html.tr [Attributes.attribute "aria-rowindex" (String.fromInt (index+(if header then 1 else 2)))] (fillCells header columns nodes)
            in
            (if List.isEmpty original && List.isEmpty selectedBody then Nothing else Just (Html.table []
                [Html.thead [] [Html.tr [] (fillCells True columns renderedHeaders)],Html.tbody [] (List.map (rowNode False) selectedBody)]),afterBody)
        (initialNodes,initial)=walkList 0 fragments {used=0,page=0,error=False,target=target}
        effective=if target<0 then initial.page else target
        (renderedNodes,final)=if target<0 then walkList 0 fragments {used=0,page=0,error=False,target=effective} else (initialNodes,initial)
    in
    {nodes=renderedNodes,more=final.page>effective,error=final.error,part=effective}


formatted : Markdown.Renderer.Renderer (Html msg) -> String -> Fragment msg
formatted base content =
    formattedAst base content (Markdown.Parser.parse content |> Result.mapError (always "parse error"))


formattedAst : Markdown.Renderer.Renderer (Html msg) -> String -> Result String (List MarkdownBlock.Block) -> Fragment msg
formattedAst base content parsed =
    let
        bytes value = 6 * String.length value
        leaf cost render = Atom cost (\_ -> render ())
        branch render children = container 128 render children
        textFragments value =
            if String.isEmpty value then [] else
            let
                prefix=String.left 8192 value
                code=String.right 1 prefix |> String.uncons |> Maybe.map (Tuple.first >> Char.toCode) |> Maybe.withDefault 0
                following=String.slice 8192 8193 value |> String.uncons |> Maybe.map (Tuple.first >> Char.toCode) |> Maybe.withDefault 0
                length=String.length prefix - (if code >= 55296 && code <= 56319 && following >= 56320 && following <= 57343 then 1 else 0)
                part=String.left length value
            in leaf (32+bytes part) (\_ -> base.text part)::textFragments (String.dropLeft length value)
        renderer =
            { text = \value -> container 32 (Html.span []) (textFragments value)
            , codeSpan = \value -> branch (Html.code []) (textFragments value)
            , hardLineBreak = leaf 32 (\_ -> base.hardLineBreak)
            , thematicBreak = leaf 32 (\_ -> base.thematicBreak)
            , paragraph = branch base.paragraph
            , blockQuote = branch base.blockQuote
            , strong = branch base.strong
            , emphasis = branch base.emphasis
            , strikethrough = branch base.strikethrough
            , heading = \info -> branch (\children -> base.heading {level=info.level,rawText=info.rawText,children=children}) info.children
            , link = \info children -> container (256+bytes info.destination+bytes (Maybe.withDefault "" info.title)) (base.link info) children
            , image = \info -> leaf (256+bytes info.src+bytes info.alt+bytes (Maybe.withDefault "" info.title)) (\_ -> base.image info)
            , codeBlock = \info -> container (256+bytes (Maybe.withDefault "" info.language))
                (\children -> Html.pre [] [Html.code (info.language |> Maybe.map String.words |> Maybe.andThen List.head |> Maybe.map (\language -> [Attributes.class ("language-"++language)]) |> Maybe.withDefault []) children]) (textFragments info.body)
            , html = Markdown.Html.oneOf []
            , unorderedList = \items -> branch (Html.ul []) (List.map (\(MarkdownBlock.ListItem task children) ->
                let checkbox = case task of
                        MarkdownBlock.NoTask -> Html.text ""
                        MarkdownBlock.IncompleteTask -> Html.input [Attributes.disabled True,Attributes.checked False,Attributes.type_ "checkbox"] []
                        MarkdownBlock.CompletedTask -> Html.input [Attributes.disabled True,Attributes.checked True,Attributes.type_ "checkbox"] []
                in branch (Html.li []) (Atom 256 (\_ -> checkbox)::children)) items)
            , orderedList = \start items -> branch (Html.ol (if start == 1 then [Attributes.start start] else [])) (List.indexedMap (\index children -> branch (Html.li [Attributes.attribute "value" (String.fromInt (index+1))]) children) items)
            , table = Table
            , tableHeader = TableSection True
            , tableBody = TableSection False
            , tableRow = TableRow
            , tableCell = TableCell False
            , tableHeaderCell = TableCell True
            }
    in
    case parsed |> Result.andThen (Markdown.Renderer.render renderer) of
        Ok fragments -> container 128 (Html.div [Attributes.class "markdown-content session-record-text"]) fragments
        Err _ -> container 128 (Html.pre [Attributes.class "session-record-text"]) (textFragments content)


type alias Block =
    { kind : String, content : String, label : String, tokens : List ( String, String ) }


type alias Piece =
    { key : String, record : SessionRecord, blocks : List Block, activity : Bool, activityAnchor : Maybe String }


type Item
    = Message Piece
    | Activity (List Piece)


project : List SessionRecord -> List Item
project records =
    records
        |> List.concatMap pieces
        |> List.foldr
            (\piece rest ->
                if piece.activity then
                    case rest of
                        Activity run :: tail ->
                            if sameRun piece run then
                                Activity (piece :: run) :: tail

                            else
                                Activity [ piece ] :: rest

                        _ ->
                            Activity [ piece ] :: rest

                else
                    Message piece :: rest
            )
            []


sameRun : Piece -> List Piece -> Bool
sameRun piece run =
    case ( piece.activityAnchor, List.head run |> Maybe.andThen .activityAnchor ) of
        ( Just first, Just second ) ->
            first == second

        _ ->
            True


pieces : SessionRecord -> List Piece
pieces record =
    if record.recordType == "unknown" then
        []

    else if record.recordType /= "conversation" then
        if String.trim record.content == "" then
            []

        else
            [ { key = String.fromInt record.presentationOffset ++ ":0", record = record, blocks = [ { kind = "text", content = record.content, label = "", tokens = [] } ], activity = False, activityAnchor = Nothing } ]

    else
        Decode.decodeString (Decode.list blockDecoder) record.content
            |> Result.withDefault []
            |> List.indexedMap Tuple.pair
            |> List.filter (Tuple.second >> visible)
            |> List.foldr
                (\( index, block ) rest ->
                    let
                        piece = { key = String.fromInt record.presentationOffset ++ ":" ++ String.fromInt index, record = record, blocks = [ block ], activity = block.kind /= "text", activityAnchor = Dict.get (String.fromInt index) record.activityAnchors }
                    in
                    case rest of
                        next :: tail ->
                            if piece.activity == next.activity then
                                { piece | blocks = block :: next.blocks } :: tail

                            else
                                piece :: rest

                        [] ->
                            [ piece ]
                )
                []


visible : Block -> Bool
visible block =
    case block.kind of
        "encrypted" -> True
        "tool" -> String.trim block.content /= "" || String.trim block.label /= ""
        "result" -> String.trim block.content /= "" || String.trim block.label /= ""
        "thinking" -> String.trim block.content /= ""
        "text" -> String.trim block.content /= ""
        _ -> False


blockDecoder : Decode.Decoder Block
blockDecoder =
    Decode.map4 Block
        (Decode.field "kind" Decode.string)
        (Decode.field "text" Decode.string)
        (Decode.field "label" Decode.string)
        (Decode.oneOf [ Decode.field "tokens" (Decode.list (Decode.map2 Tuple.pair (Decode.field "kind" Decode.string) (Decode.field "text" Decode.string))), Decode.succeed [] ])
