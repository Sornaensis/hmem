module ActorAttributionTest exposing (suite)

import ActorAttribution as Actor
import Api
import Expect
import Json.Decode as D
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


declared : Actor.BotIdentity
declared =
    { role = "Reviewer", modelId = "pi:gpt-6.1-sol:extra-high" }


suite : Test
suite =
    describe "shared actor attribution"
        [ test "shows full role/model while preserving known historical identities" <| \_ ->
            [ Actor.label { actorType = Just "bot", actorId = Just "credential-bot", actorLabel = Just "Authenticated bot", botIdentity = Just declared }
            , Actor.label { actorType = Just "user", actorId = Just "alice", actorLabel = Just "Alice", botIdentity = Nothing }
            , Actor.label { actorType = Just "bot", actorId = Just "old-bot", actorLabel = Nothing, botIdentity = Nothing }
            , Actor.label { actorType = Just "service", actorId = Just "worker", actorLabel = Nothing, botIdentity = Nothing }
            , Actor.label { actorType = Just "system", actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing }
            , Actor.label { actorType = Just "local", actorId = Nothing, actorLabel = Just "Local User", botIdentity = Nothing }
            , Actor.label Actor.empty
            ] |> Expect.equal [ "Reviewer · pi:gpt-6.1-sol:extra-high", "Alice", "old-bot", "worker", "System", "Local User", "Local User" ]
        , test "retains the authenticated human label when declaration metadata is present" <| \_ ->
            Actor.label { actorType = Just "user", actorId = Just "alice", actorLabel = Just "Alice", botIdentity = Just declared }
                |> Expect.equal "Alice · Reviewer · pi:gpt-6.1-sol:extra-high"
        , test "renders full actor ID and original label as inspectable detail content" <| \_ ->
            Actor.viewDetails { actorType = Just "bot", actorId = Just "credential-derived-bot-id", actorLabel = Just "Authenticated bot", botIdentity = Just declared }
                |> Query.fromHtml
                |> Query.has [ Selector.text "credential-derived-bot-id", Selector.text "Authenticated bot", Selector.text declared.modelId ]
        , test "old session data remains compatible and keeps effective actor separate from principal" <| \_ ->
            let
                wire = """{"auth_mode":"local","principal":{"actor_type":"user","actor_id":"local","actor_label":"Local User","authority":"local_superadmin"},"global_permissions":{"create_workspace":true,"superadmin":true}}"""
            in
            D.decodeString Api.sessionContextDecoder wire |> Result.map (\session -> ( session.principal.actorId, session.effectiveActor )) |> Expect.equal (Ok ( "local", Nothing ))
        , test "decodes the effective declared actor without replacing authority" <| \_ ->
            let
                wire = """{"auth_mode":"local","principal":{"actor_type":"user","actor_id":"local","actor_label":"Local User","authority":"local_superadmin"},"effective_actor":{"actor_type":"bot","actor_id":"local-declared-bot:v1:abc","actor_label":"Reviewer","bot_identity":{"role":"Reviewer","model-id":"pi:gpt-6.1-sol:extra-high"}},"global_permissions":{"create_workspace":true,"superadmin":true}}"""
            in
            D.decodeString Api.sessionContextDecoder wire |> Result.map (\session -> ( session.principal.actorId, Maybe.map Actor.label session.effectiveActor )) |> Expect.equal (Ok ( "local", Just "Reviewer · pi:gpt-6.1-sol:extra-high" ))
        , test "canonical service events retain declaration metadata and historical system events stay valid" <| \_ ->
            let
                frame actor = """{"schema_version":1,"type":"change","event":{"schema_version":1,"event_id":"e","scope":"workspace","workspace_id":"w","occurred_at":"now","transaction":{"id":"tx","cause":"mcp","request_id":null},"actor":""" ++ actor ++ ""","entity":{"type":"task","id":"t","action":"updated"},"invalidations":[{"kind":"entity","target":"task:t"}]}}"""
                decode actor = case Api.decodeCanonicalFrame (frame actor) of
                    Just (Api.CanonicalChange event) -> Just (Actor.label event.actor)
                    _ -> Nothing
            in
            [ decode """{"type":"service","id":"bot-id","label":"Bot","bot_identity":{"role":"Reviewer","model-id":"pi:gpt-6.1-sol:extra-high"}}"""
            , decode """{"type":"system","id":null}"""
            ] |> Expect.equal [ Just "Reviewer · pi:gpt-6.1-sol:extra-high", Just "System" ]
        , test "audit and Observation provenance decode optional declared snapshots and older absent metadata" <| \_ ->
            let
                declaration = """, "bot_identity":{"role":"Reviewer","model-id":"pi:gpt-6.1-sol:extra-high"}"""
                audit suffix = """{"id":"audit","entity_type":"task","entity_id":"t","action":"update","actor_type":"bot","actor_id":"credential-id","actor_label":"Authenticated bot","changed_at":"now" """ ++ suffix ++ "}"
                provenance suffix = """{"sequence":1,"event_kind":"legacy_creation","reviewed_git_sha":"abc","content_version":null,"content_digest":null,"recorded_at":"now","actor_type":null,"actor_id":null,"actor_label":null""" ++ suffix ++ "}"
            in
            Expect.all
                [ \_ -> D.decodeString Api.auditLogEntryDecoder (audit declaration) |> Result.map Actor.label |> Expect.equal (Ok "Reviewer · pi:gpt-6.1-sol:extra-high")
                , \_ -> D.decodeString Api.auditLogEntryDecoder (audit "") |> Result.map Actor.label |> Expect.equal (Ok "Authenticated bot")
                , \_ -> D.decodeString Api.observationProvenanceDecoder (provenance "") |> Result.map Actor.label |> Expect.equal (Ok "Local User")
                , \_ -> D.decodeString Api.observationProvenanceDecoder (provenance declaration) |> Result.map Actor.label |> Expect.equal (Ok "Reviewer · pi:gpt-6.1-sol:extra-high")
                ] ()
        ]
