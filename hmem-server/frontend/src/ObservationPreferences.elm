module ObservationPreferences exposing (Owner, Preferences, Detail, empty, ownerDecoder, ownerValue, decoder, encode, normalize, uuid)

import Char
import Json.Decode as D
import Json.Encode as E
import Set
import Dict exposing (Dict)


type alias Owner =
    { workspaceId : String, actorId : String, authority : String, runtimeId : String, epoch : Int, requestId : Int, touch : Int }


type alias Detail =
    { id : String, occurrence : Maybe String, mode : String }


type alias Preferences =
    { modeIntent : Bool, detail : Maybe Detail, subjects : List String, groups : List String
    , paths : List String, focusedPath : Maybe String, selectedPath : Maybe String, context : Maybe String, queries : Dict String String, subjectQueries : Dict String String, scrolls : Dict String Float, fileInput : String }


empty : Preferences
empty =
    { modeIntent = False, detail = Nothing, subjects = [], groups = [], paths = [], focusedPath = Nothing, selectedPath = Nothing, context = Nothing, queries = Dict.empty, subjectQueries = Dict.empty, scrolls = Dict.empty, fileInput = "" }


uuid : String -> Bool
uuid value =
    List.map String.length (String.split "-" value) == [ 8, 4, 4, 4, 12 ]
        && (String.replace "-" "" value |> String.all (\c -> String.contains (String.fromChar (Char.toLower c)) "0123456789abcdef"))


ownerDecoder : D.Decoder Owner
ownerDecoder =
    D.map7 Owner (D.field "workspaceId" D.string) (D.field "actorId" D.string) (D.field "authority" D.string)
        (D.field "runtimeId" D.string) (D.field "epoch" D.int) (D.field "requestId" D.int) (D.field "touch" D.int)


ownerValue : Owner -> E.Value
ownerValue owner =
    E.object [ ( "workspaceId", E.string owner.workspaceId ), ( "actorId", E.string owner.actorId ), ( "authority", E.string owner.authority )
        , ( "runtimeId", E.string owner.runtimeId ), ( "epoch", E.int owner.epoch ), ( "requestId", E.int owner.requestId ), ( "touch", E.int owner.touch ) ]


detailDecoder : D.Decoder Detail
detailDecoder =
    D.map3 Detail (D.field "id" D.string) (D.field "occurrence" (D.nullable D.string)) (D.field "mode" D.string)
        |> D.andThen (\detail -> if validDetail detail then D.succeed detail else D.fail "Invalid detail preference")


validDetail : Detail -> Bool
validDetail detail =
    uuid detail.id && List.member detail.mode [ "flat", "exact", "match", "facets", "tree" ]
        && (detail.occurrence |> Maybe.map (\value -> String.startsWith "observation-card-" value && String.length value <= 8192
            && String.endsWith ("-" ++ String.fromInt (String.length detail.id) ++ "-" ++ (String.toList detail.id |> List.map (Char.toCode >> String.fromInt) |> String.join "-")) value
            && (String.dropLeft 17 value |> String.all (\c -> Char.isDigit c || c == '-'))) |> Maybe.withDefault True)


validGroup : String -> Bool
validGroup value =
    String.length value <= 8192 && not (String.any (\c -> Char.toCode c < 32) value)
        && (part value |> Maybe.andThen (\( path, remaining ) -> part remaining |> Maybe.andThen (\( kind, rest ) -> part rest |> Maybe.map (\( subject, suffix ) -> not (String.isEmpty path) && List.member kind [ "file", "glob" ] && not (String.isEmpty subject) && suffix == ""))) |> Maybe.withDefault False)


part : String -> Maybe ( String, String )
part value =
    String.indexes ":" value |> List.head |> Maybe.andThen (\at ->
        String.left at value |> String.toInt |> Maybe.andThen (\count ->
            let remaining = String.dropLeft (at + 1) value in
            if count < 0 || count > String.length remaining || count > 8192 then Nothing else Just ( String.left count remaining, String.dropLeft count remaining )))


decoder : Owner -> D.Decoder Preferences
decoder owner =
    D.value |> D.andThen (\value ->
        if (E.encode 0 value |> String.toList |> List.map (\c -> let code = Char.toCode c in if code < 128 then 1 else if code < 2048 then 2 else if code < 65536 then 3 else 4) |> List.sum) > 32768 then D.fail "Oversized preferences" else
        case D.decodeValue (scopedDecoder owner) value of
            Ok prefs -> D.succeed prefs
            Err _ -> D.fail "Invalid preferences")


scopedDecoder : Owner -> D.Decoder Preferences
scopedDecoder owner =
    D.map7 (\version workspace actor authority detail subjects groups -> ( version == 1 && workspace == owner.workspaceId && actor == owner.actorId && authority == owner.authority, { empty | detail = detail, subjects = subjects, groups = groups } ))
        (D.field "version" D.int) (D.field "workspaceId" D.string) (D.field "actorId" D.string) (D.field "authority" D.string)
        (D.field "detail" (D.nullable detailDecoder)) (D.field "subjects" (D.list D.string)) (D.field "groups" (D.list D.string))
        |> D.andThen (\( scoped, base ) -> D.map5 (\paths focused context queries fileInput -> (scoped, { base | paths = paths, focusedPath = focused, context = context, queries = queries, fileInput = fileInput }))
            (optional "paths" (D.list D.string) []) (optional "focusedPath" (D.nullable D.string) Nothing)
            (optional "context" (D.nullable D.string) Nothing) (optional "queries" (D.dict D.string) Dict.empty) (optional "fileInput" D.string ""))
        |> D.andThen (\( scoped, prefs ) -> D.map2 (\scrolls selectedPath -> (scoped, { prefs | scrolls = scrolls, selectedPath = selectedPath })) (optional "scrolls" (D.dict D.float) Dict.empty) (optional "selectedPath" (D.nullable D.string) Nothing))
        |> D.andThen (\( scoped, prefs ) ->
            D.oneOf
                [ D.field "subjectQueries" (D.dict D.string) |> D.map (\subjects -> ( scoped, { prefs | subjectQueries = subjects } ))
                , D.succeed ( scoped, { prefs | subjectQueries = Dict.filter (\key _ -> List.member key [ "tree", "facets" ]) prefs.queries, queries = Dict.filter (\key _ -> not (List.member key [ "tree", "facets" ])) prefs.queries } )
                ])
        |> D.andThen (\( scoped, prefs ) -> D.map (\intent -> (scoped, {prefs | modeIntent = intent})) (optional "modeIntent" D.bool False))
        |> D.andThen (\( scoped, prefs ) ->
            if scoped
                && List.length prefs.subjects <= 128 && List.length prefs.groups <= 128 && List.length prefs.paths <= 128
                && List.all uuid prefs.subjects && List.all validGroup prefs.groups && List.all validPath prefs.paths
                && (Dict.keys prefs.subjectQueries |> List.all (\key -> List.member key [ "flat", "tree", "facets" ]))
                && (Dict.keys prefs.queries |> List.all (\key -> List.member key [ "flat", "tree", "facets", "match" ])) then D.succeed (normalize prefs) else D.fail "Invalid preference scope or bounds")


optional : String -> D.Decoder a -> a -> D.Decoder a
optional name fieldDecoder fallback =
    D.oneOf [ D.field name fieldDecoder, D.succeed fallback ]


validPath : String -> Bool
validPath value =
    value /= "" && String.length value <= 4096 && not (String.startsWith "/" value)
        && String.slice 1 2 value /= ":" && not (String.contains "\\" value)
        && not (String.any (\c -> Char.toCode c < 32 || Char.toCode c == 127) value)
        && List.all (\partValue -> not (List.member partValue [ "", ".", ".." ])) (String.split "/" value)


normalize : Preferences -> Preferences
normalize prefs =
    let bounded valid values = values |> List.filter valid |> Set.fromList |> Set.toList |> List.take 128 in
    { modeIntent = prefs.modeIntent
    , detail = prefs.detail |> Maybe.andThen (\detail -> if validDetail detail then Just { detail | mode = if detail.mode == "facets" then "tree" else detail.mode } else Nothing)
    , subjects = bounded uuid prefs.subjects, groups = bounded validGroup prefs.groups
    , paths = bounded validPath prefs.paths, focusedPath = prefs.focusedPath |> Maybe.andThen (\path -> if validPath path then Just path else Nothing)
    , selectedPath = prefs.selectedPath |> Maybe.andThen (\path -> if validPath path then Just path else Nothing)
    , context = prefs.context |> Maybe.andThen (\value -> if String.length value <= 16384 then Just value else Nothing)
    , queries = (case Dict.get "facets" prefs.queries of
            Just value -> Dict.insert "tree" (Dict.get "tree" prefs.queries |> Maybe.withDefault value) (Dict.remove "facets" prefs.queries)
            Nothing -> prefs.queries) |> Dict.filter (\key value -> List.member key [ "flat", "tree", "match" ] && String.length value <= 4096)
    , subjectQueries = (case Dict.get "facets" prefs.subjectQueries of
            Just value -> Dict.insert "tree" (Dict.get "tree" prefs.subjectQueries |> Maybe.withDefault value) (Dict.remove "facets" prefs.subjectQueries)
            Nothing -> prefs.subjectQueries) |> Dict.filter (\key value -> List.member key [ "flat", "tree" ] && String.length value <= 4096)
    , scrolls = prefs.scrolls |> Dict.filter (\key top -> List.member key [ "flat", "tree", "match" ] && not (isNaN top) && not (isInfinite top) && top >= 0 && top <= 1000000000)
    , fileInput = if String.length prefs.fileInput <= 32768 then prefs.fileInput else "" }


encode : Owner -> Preferences -> E.Value
encode owner raw =
    let prefs = normalize raw in
    E.object [ ( "version", E.int 1 ), ( "workspaceId", E.string owner.workspaceId ), ( "actorId", E.string owner.actorId ), ( "authority", E.string owner.authority )
        , ( "modeIntent", E.bool prefs.modeIntent )
        , ( "detail", prefs.detail |> Maybe.map (\detail -> E.object [ ( "id", E.string detail.id ), ( "occurrence", detail.occurrence |> Maybe.map E.string |> Maybe.withDefault E.null ), ( "mode", E.string detail.mode ) ]) |> Maybe.withDefault E.null )
        , ( "subjects", E.list E.string prefs.subjects ), ( "groups", E.list E.string prefs.groups )
        , ( "paths", E.list E.string prefs.paths ), ( "focusedPath", Maybe.map E.string prefs.focusedPath |> Maybe.withDefault E.null ), ( "selectedPath", Maybe.map E.string prefs.selectedPath |> Maybe.withDefault E.null )
        , ( "context", Maybe.map E.string prefs.context |> Maybe.withDefault E.null ), ( "queries", E.dict identity E.string prefs.queries ), ( "subjectQueries", E.dict identity E.string prefs.subjectQueries ), ( "scrolls", E.dict identity E.float prefs.scrolls ), ( "fileInput", E.string prefs.fileInput ) ]
