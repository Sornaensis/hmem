module ActorAttribution exposing (Actor, BotIdentity, botIdentityDecoder, details, empty, label, viewDetails)

import Html exposing (Html, dd, dl, dt, summary, text)
import Html.Attributes exposing (class)
import Json.Decode as D exposing (Decoder)


type alias BotIdentity =
    { role : String, modelId : String }


type alias Actor =
    { actorType : Maybe String, actorId : Maybe String, actorLabel : Maybe String, botIdentity : Maybe BotIdentity }


empty : Actor
empty =
    Actor Nothing Nothing Nothing Nothing


botIdentityDecoder : Decoder BotIdentity
botIdentityDecoder =
    D.map2 BotIdentity (D.field "role" D.string) (D.field "model-id" D.string)


label : { a | actorType : Maybe String, actorId : Maybe String, actorLabel : Maybe String, botIdentity : Maybe BotIdentity } -> String
label actor =
    let
        known =
            actor.actorLabel |> nonblank

        fallback =
            case actor.actorType of
                Just "system" -> "System"
                Just "service" -> Maybe.withDefault "Service" (nonblank actor.actorId)
                Just "bot" -> Maybe.withDefault "Bot" (nonblank actor.actorId)
                Just "user" -> Maybe.withDefault "User" (nonblank actor.actorId)
                _ -> "Local User"
    in
    case actor.botIdentity of
        Just identity ->
            (if actor.actorType == Just "user" then Maybe.withDefault fallback known ++ " · " else "")
                ++ identity.role ++ " · " ++ identity.modelId

        Nothing ->
            Maybe.withDefault fallback known


nonblank : Maybe String -> Maybe String
nonblank =
    Maybe.andThen (\value -> if String.trim value == "" then Nothing else Just value)


details : { a | actorType : Maybe String, actorId : Maybe String, actorLabel : Maybe String, botIdentity : Maybe BotIdentity } -> List ( String, String )
details actor =
    List.filterMap identity
        [ Maybe.map (Tuple.pair "Actor type") actor.actorType
        , Maybe.map (Tuple.pair "Actor ID") actor.actorId
        , Maybe.map (Tuple.pair "Actor label") actor.actorLabel
        ]
        ++ (actor.botIdentity |> Maybe.map (\identity -> [ ( "Declared role", identity.role ), ( "Model", identity.modelId ) ]) |> Maybe.withDefault [])


viewDetails : { a | actorType : Maybe String, actorId : Maybe String, actorLabel : Maybe String, botIdentity : Maybe BotIdentity } -> Html msg
viewDetails actor =
    Html.details [ class "actor-details" ]
        [ summary [] [ text "Actor details" ]
        , dl [] (List.concatMap (\( name, value ) -> [ dt [] [ text name ], dd [] [ text value ] ]) (details actor))
        ]
