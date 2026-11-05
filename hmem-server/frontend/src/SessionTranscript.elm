module SessionTranscript exposing (Block, Item(..), Piece, project, Fragment, atom, container, keyedContainer, scoped, formatted, formattedAst, renderPage, renderWindow)

{-| Presentation of the current bounded record window. Keys retain source block
positions; original records and byte offsets are unchanged.
-}

import Json.Decode as Decode
import Dict
import Types exposing (SessionRecord)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Keyed as Keyed
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
    | KeyedContainer Int (List (Fragment msg)) (List (String,Html msg) -> Html msg)
    | Scope String (Fragment msg)
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
        KeyedContainer cost children _ -> sum cost children
        Scope _ child -> costUpTo limit child
        Table children -> sum 128 children
        TableSection _ children -> sum 128 children
        TableRow children -> sum 128 children
        TableCell _ _ children -> sum 128 children


atom : Int -> (() -> Html msg) -> Fragment msg
atom = Atom


container : Int -> (List (Html msg) -> Html msg) -> List (Fragment msg) -> Fragment msg
container cost render children = Container cost children render


keyedContainer : Int -> (List (String,Html msg) -> Html msg) -> List (Fragment msg) -> Fragment msg
keyedContainer cost render children = KeyedContainer cost children render


scoped : String -> Fragment msg -> Fragment msg
scoped = Scope


renderPage : Int -> Int -> List (Fragment msg) -> { nodes : List (Html msg), indices : List Int, more : Bool, error : Bool, part : Int }
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
                KeyedContainer cost children render ->
                    walk ancestors (Container cost children (List.indexedMap (\index node -> (String.fromInt index,node)) >> render)) state
                Scope _ child -> walk ancestors child state
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
                        TableRow rowCells -> Just rowCells
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
        roots values initialState =
            List.indexedMap Tuple.pair values |> List.foldl (\(index,fragment) (nodes,indices,current) ->
                let (node,next)=walk 0 fragment current in
                case node of
                    Just value -> (value::nodes,index::indices,next)
                    Nothing -> (nodes,indices,next)) ([],[],initialState)
                |> (\(nodes,indices,current) -> (List.reverse nodes,List.reverse indices,current))
        (initialNodes,initialIndices,initial)=roots fragments {used=0,page=0,error=False,target=target}
        effective=if target<0 then initial.page else target
        (renderedNodes,renderedIndices,final)=if target<0 then roots fragments {used=0,page=0,error=False,target=effective} else (initialNodes,initialIndices,initial)
    in
    {nodes=renderedNodes,indices=renderedIndices,more=final.page>effective,error=final.error,part=effective}


{-| A single mounted-cost budget covers three overlapping slices. Omitted
siblings coalesce into source-independent presentation ranges inside the same
container; keyed children retain the actual leaf being read across a refill.
-}
renderWindow : Int -> Int -> String -> List (Fragment msg) -> { nodes : List (Html msg), keys : List String, more : Bool, error : Bool, part : Int }
renderWindow budget target anchor fragments =
    let
        slice = max 1 (budget // 3)
        gapNode tag first last weight =
            let attrs=[Attributes.class "session-fragment-gap",Attributes.attribute "data-fragment-from" (String.fromInt first),Attributes.attribute "data-fragment-to" (String.fromInt last),Attributes.attribute "aria-hidden" "true",Attributes.style "height" (String.fromFloat (min 10000000 (max 1 (toFloat weight/40))) ++ "px")]
            in if tag=="tr" then Html.tr attrs [Html.td [Attributes.colspan 1000] []]
               else if tag=="td" then Html.td attrs []
               else if tag=="tbody" then Html.tbody attrs [Html.tr [] [Html.td [Attributes.colspan 1000] []]]
               else Html.span (Attributes.style "display" "block"::attrs) []
        walkList ancestors path gapTag values state =
            let
                prependGap pending nodes =
                    case pending of
                        Just (begin,finish,cost) -> ("gap:" ++ String.fromInt begin ++ ":" ++ String.fromInt finish,gapNode gapTag begin finish cost) :: nodes
                        Nothing -> nodes
                add (index,fragment) (nodes,pending,(current,total)) =
                    let
                        (node,next,weight)=walk ancestors (path ++ ":" ++ String.fromInt index) fragment current
                    in case node of
                        Just html -> ((String.fromInt index,html) :: prependGap pending nodes,Nothing,(next,total+weight))
                        Nothing ->
                            let first=min current.page next.page in
                            (nodes,Just (case pending of
                                Just (begin,_,cost) -> (begin,next.page,cost+weight)
                                Nothing -> (first,next.page,weight)),(next,total+weight))
                (selected,tailPending,(listState,listWeight))=List.indexedMap Tuple.pair values |> List.foldl add ([],Nothing,(state,0))
            in (List.reverse (prependGap tailPending selected),listState,listWeight)
        branch ancestors path cost gapTag children render state =
            if List.isEmpty children then walk ancestors path (Atom cost (\_ -> render [])) state
            else
                let (nodes,next,weight)=walkList ((path,cost)::ancestors) path gapTag children state in
                (if next.selected==state.selected then Nothing else Just (render nodes),next,weight)
        walkTableWindow ancestors path sections state =
            let
                rows header = sections |> List.concatMap (\section -> case section of
                    TableSection isHeader children -> if header==isHeader then List.filterMap (\child -> case child of
                        TableRow cells -> Just cells
                        _ -> Nothing) children else []
                    _ -> [])
                headerRows=rows True
                bodyRows=rows False
                headers=List.head headerRows |> Maybe.withDefault [] |> Array.fromList
                columns=max (Array.length headers) (List.map List.length bodyRows |> List.maximum |> Maybe.withDefault 0)
                cellAttrs header index alignment =
                    [Attributes.attribute "aria-colindex" (String.fromInt (index+1))] ++ (if header then [Attributes.attribute "scope" "col"] else []) ++
                    (alignment |> Maybe.map (\value -> [Attributes.align (case value of
                        MarkdownBlock.AlignLeft -> "left"
                        MarkdownBlock.AlignCenter -> "center"
                        MarkdownBlock.AlignRight -> "right")]) |> Maybe.withDefault [])
                fill header selected omitted =
                    let
                        gaps begin finish =
                            if begin>=finish then [] else
                            let
                                end=min finish (begin+1000)
                                metadata=List.filter (\(index,_) -> index>=begin && index<end) omitted |> List.map Tuple.second
                                firstPart=List.map .first metadata |> List.minimum |> Maybe.withDefault 0
                                lastPart=List.map .last metadata |> List.maximum |> Maybe.withDefault firstPart
                                weight=List.sum (List.map .weight metadata)
                                attrs=[Attributes.colspan (end-begin),Attributes.attribute "aria-hidden" "true"] ++ (if List.isEmpty metadata then [] else
                                    [Attributes.class "session-fragment-gap",Attributes.attribute "data-fragment-from" (String.fromInt firstPart),Attributes.attribute "data-fragment-to" (String.fromInt lastPart),Attributes.style "height" (String.fromFloat (min 10000000 (max 1 (toFloat weight/40))) ++ "px")])
                            in ("columns:" ++ String.fromInt begin ++ ":" ++ String.fromInt end,(if header then Html.th else Html.td) attrs []) :: gaps end finish
                        add (index,node) (previous,nodes) = (index+1,nodes ++ gaps previous index ++ [("cell:" ++ String.fromInt index,node)])
                        (lastColumn,completed)=List.foldl add (0,[]) selected
                    in completed ++ gaps lastColumn columns
                windowCells header rowIndex values current =
                    let
                        add (index,fragment) (selected,omitted,(previous,weight)) =
                            let
                                context=if header then 0 else Array.get index headers |> Maybe.map (costUpTo 262144 >> (+) 512 >> min 262144) |> Maybe.withDefault 0
                                converted=case fragment of
                                    TableCell _ alignment children -> KeyedContainer (128+context+160*(columns//1000+3)) children (Keyed.node (if header then "th" else "td") (cellAttrs header index alignment))
                                    _ -> KeyedContainer (128+context+160*(columns//1000+3)) [fragment] (Keyed.node (if header then "th" else "td") (cellAttrs header index Nothing))
                                (node,next,cost)=walk ((path,384)::ancestors) (path ++ ":" ++ (if header then "header" else "body") ++ ":" ++ String.fromInt rowIndex ++ ":" ++ String.fromInt index) converted previous
                            in case node of
                                Just html -> (selected ++ [(index,html)],omitted,(next,weight+cost))
                                Nothing -> (selected,omitted ++ [(index,{first=previous.page,last=next.page,weight=cost})],(next,weight+cost))
                        (cellSelected,cellOmitted,(cellFinal,cellWeight))=List.indexedMap Tuple.pair values |> List.foldl add ([],[],(current,0))
                    in (cellSelected,cellOmitted,(cellFinal,cellWeight))
                tableRows header values current =
                    let
                        add (index,valuesInRow) (selected,(previous,weight)) =
                            let (nodes,omitted,(next,cost))=windowCells header index valuesInRow previous in
                            (if List.isEmpty nodes then selected else selected ++ [(index,nodes,omitted)],(next,weight+cost))
                    in List.indexedMap Tuple.pair values |> List.foldl add ([],(current,0))
                (selectedHeaders,(afterHeader,headerWeight))=tableRows True headerRows state
                (selectedBody,(afterBody,bodyWeight))=tableRows False bodyRows afterHeader
                original=List.concatMap (\(_,nodes,_) -> nodes) selectedHeaders
                selectedColumns=List.map Tuple.first original ++ List.concatMap (\(_,nodes,_) -> List.map Tuple.first nodes) selectedBody |> Set.fromList |> Set.toList
                contextHeader index = case List.filter (Tuple.first >> (==) index) original |> List.head of
                    Just existing -> Just existing
                    Nothing -> Array.get index headers |> Maybe.andThen (\fragment -> case fragment of
                        TableCell _ alignment children ->
                            let part=renderPage 261632 0 children in
                            Just (index,Keyed.node "th" (cellAttrs True index alignment) (List.indexedMap (\at node -> (String.fromInt at,node)) (part.nodes ++ (if part.more then [Html.span [Attributes.class "session-table-header-continuation"] [Html.text "Header continues in earlier content."]] else []))))
                        _ -> Nothing)
                headerNodes=List.filterMap contextHeader selectedColumns
                bodyNode (index,nodes,omitted) = (String.fromInt index,Keyed.node "tr" [Attributes.attribute "aria-rowindex" (String.fromInt (index+2))] (fill False nodes omitted))
            in
            (if List.isEmpty original && List.isEmpty selectedBody then Nothing else Just (Keyed.node "table" [Attributes.attribute "aria-colcount" (String.fromInt columns)]
                [("header",Html.thead [] [Keyed.node "tr" [Attributes.attribute "aria-rowindex" "1"] (fill True headerNodes [])]),("body",Keyed.node "tbody" [] (List.map bodyNode selectedBody))]),afterBody,headerWeight+bodyWeight)
        walk ancestors path fragment state =
            case fragment of
                Atom cost render ->
                    let
                        unpaid=List.filter (\(key,_) -> not (Set.member key state.charged)) ancestors |> List.map (Tuple.second >> (+) 1024) |> List.sum
                        -- The wrapper and two possible omitted sibling ranges at
                        -- each ancestor are charged to their adjacent leaf.
                        own=cost+512+12*String.length path+(if List.isEmpty ancestors then 1024 else 0)
                        crosses=state.used>0 && state.used+unpaid+own>slice
                        total=own+(if crosses then List.sum (List.map (Tuple.second >> (+) 1024) ancestors) else unpaid)
                        next=if crosses then {state | page=state.page+1,used=0,charged=Set.empty} else state
                        admitted=next.page>=next.first && next.page<=next.last && total<=budget
                        node=if admitted then Just (Html.span [Attributes.class "session-fragment",Attributes.attribute "data-fragment-path" path,Attributes.attribute "data-fragment-part" (String.fromInt next.page),Attributes.attribute "data-fragment-cost" (String.fromInt total)] [render ()]) else Nothing
                    in (node,{next | used=next.used+total,costs=Dict.update next.page (Maybe.withDefault 0 >> (+) total >> Just) next.costs,charged=List.foldl (Tuple.first >> Set.insert) next.charged ancestors,error=next.error || total>budget,selected=next.selected+(if admitted then 1 else 0),anchorPart=if next.anchorPart==Nothing && anchor/="" && String.startsWith anchor path then Just next.page else next.anchorPart},cost+unpaid)
                Container cost children render -> branch ancestors path cost "span" children (List.map Tuple.second >> render) state
                KeyedContainer cost children render -> branch ancestors path cost "span" children render state
                Scope name child -> walk ancestors name child state
                Table children -> walkTableWindow ancestors path children state
                TableSection header children -> branch ancestors path 128 "tr" children (Keyed.node (if header then "thead" else "tbody") []) state
                TableRow children -> branch ancestors path 128 "td" children (Keyed.node "tr" []) state
                TableCell header alignment children ->
                    branch ancestors path 128 "span" children (Keyed.node (if header then "th" else "td") (alignment |> Maybe.map (\value -> [Attributes.align (case value of
                        MarkdownBlock.AlignLeft -> "left"
                        MarkdownBlock.AlignCenter -> "center"
                        MarkdownBlock.AlignRight -> "right")]) |> Maybe.withDefault [])) state
        run first last = walkList [] "" "span" fragments {used=0,page=0,error=False,first=first,last=last,selected=0,charged=Set.empty,costs=Dict.empty,anchorPart=Nothing}
        (_,scan,_)=run -1 -1
        effective=clamp 0 scan.page (scan.anchorPart |> Maybe.withDefault (if target<0 then scan.page else target))
        center=Dict.get effective scan.costs |> Maybe.withDefault 0
        previousCost=Dict.get (effective-1) scan.costs |> Maybe.withDefault budget
        windowFirst=if effective>0 && center+previousCost<=budget then effective-1 else effective
        selectedCost=center+(if windowFirst<effective then previousCost else 0)
        following=Dict.get (effective+1) scan.costs |> Maybe.withDefault budget
        windowLast=if effective<scan.page && selectedCost+following<=budget then effective+1 else effective
        (rootNodes,finalState,_)=run windowFirst windowLast
    in {nodes=List.map Tuple.second rootNodes,keys=List.map Tuple.first rootNodes,more=finalState.page>windowLast,error=finalState.error,part=effective}


formatted : Markdown.Renderer.Renderer (Html msg) -> String -> Fragment msg
formatted base content =
    formattedAst base content (Markdown.Parser.parse content |> Result.mapError (always "parse error"))


formattedAst : Markdown.Renderer.Renderer (Html msg) -> String -> Result String (List MarkdownBlock.Block) -> Fragment msg
formattedAst base content parsed =
    let
        bytes value = 6 * String.length value
        leaf cost render = Atom cost (\_ -> render ())
        branch name attrs children = keyedContainer 128 (Keyed.node name attrs) children
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
            { text = \value -> keyedContainer 32 (Keyed.node "span" []) (textFragments value)
            , codeSpan = \value -> branch "code" [] (textFragments value)
            , hardLineBreak = leaf 32 (\_ -> base.hardLineBreak)
            , thematicBreak = leaf 32 (\_ -> base.thematicBreak)
            , paragraph = branch "p" []
            , blockQuote = branch "blockquote" []
            , strong = branch "strong" []
            , emphasis = branch "em" []
            , strikethrough = branch "del" []
            , heading = \info -> keyedContainer 128 (\nodes -> base.heading {level=info.level,rawText=info.rawText,children=[Keyed.node "span" [] nodes]}) info.children
            , link = \info children -> keyedContainer (256+bytes info.destination+bytes (Maybe.withDefault "" info.title)) (\nodes -> base.link info [Keyed.node "span" [] nodes]) children
            , image = \info -> leaf (256+bytes info.src+bytes info.alt+bytes (Maybe.withDefault "" info.title)) (\_ -> base.image info)
            , codeBlock = \info -> keyedContainer (256+bytes (Maybe.withDefault "" info.language))
                (\children -> Html.pre [] [Keyed.node "code" (info.language |> Maybe.map String.words |> Maybe.andThen List.head |> Maybe.map (\language -> [Attributes.class ("language-"++language)]) |> Maybe.withDefault []) children]) (textFragments info.body)
            , html = Markdown.Html.oneOf []
            , unorderedList = \items -> branch "ul" [] (List.map (\(MarkdownBlock.ListItem task children) ->
                let checkbox = case task of
                        MarkdownBlock.NoTask -> Html.text ""
                        MarkdownBlock.IncompleteTask -> Html.input [Attributes.disabled True,Attributes.checked False,Attributes.type_ "checkbox"] []
                        MarkdownBlock.CompletedTask -> Html.input [Attributes.disabled True,Attributes.checked True,Attributes.type_ "checkbox"] []
                in branch "li" [] (Atom 256 (\_ -> checkbox)::children)) items)
            , orderedList = \start items -> branch "ol" (if start == 1 then [Attributes.start start] else []) (List.indexedMap (\index children -> branch "li" [Attributes.attribute "value" (String.fromInt (index+1))] children) items)
            , table = Table
            , tableHeader = TableSection True
            , tableBody = TableSection False
            , tableRow = TableRow
            , tableCell = TableCell False
            , tableHeaderCell = TableCell True
            }
    in
    case parsed |> Result.andThen (Markdown.Renderer.render renderer) of
        Ok fragments -> keyedContainer 128 (Keyed.node "div" [Attributes.class "markdown-content session-record-text"]) fragments
        Err _ -> keyedContainer 128 (Keyed.node "pre" [Attributes.class "session-record-text"]) (textFragments content)


type alias Block =
    { kind : String, content : String, label : String, tokens : List ( String, String ) }


type alias Piece =
    { key : String, record : SessionRecord, blocks : List Block, blockIndices : List Int, activity : Bool, activityAnchor : Maybe String }


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
            [ { key = String.fromInt record.presentationOffset ++ ":0", record = record, blocks = [ { kind = "text", content = record.content, label = "", tokens = [] } ], blockIndices = [0], activity = False, activityAnchor = Nothing } ]

    else
        Decode.decodeString (Decode.list blockDecoder) record.content
            |> Result.withDefault []
            |> List.indexedMap Tuple.pair
            |> List.filter (Tuple.second >> visible)
            |> List.foldr
                (\( index, block ) rest ->
                    let
                        piece = { key = String.fromInt record.presentationOffset ++ ":" ++ String.fromInt index, record = record, blocks = [ block ], blockIndices = [index], activity = block.kind /= "text", activityAnchor = Dict.get (String.fromInt index) record.activityAnchors }
                    in
                    case rest of
                        next :: tail ->
                            if piece.activity == next.activity then
                                { piece | blocks = block :: next.blocks, blockIndices = index :: next.blockIndices } :: tail

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
        "event" -> String.trim block.content /= "" || String.trim block.label /= ""
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
