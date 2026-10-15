module AuditDiffTest exposing (suite)

import Api
import AppShell
import AuditDiff exposing (Value(..), Comparison(..))
import Dict
import Expect
import Feature.AuditDiff as Dialog
import Http
import Json.Encode as Encode
import Main
import Set
import Test exposing (..)
import Time
import Types exposing (..)
import Url

suite : Test
suite = describe "literal audit text comparison and frozen-current lifetime"
    [ describe "exact text and hunks"
        (List.map (\( name, old, new ) -> test name <| \_ ->
            let edits = allLines (AuditDiff.compare (Text old) (Text new))
                reconstruct kind = edits |> List.filter (\line -> line.kind /= kind) |> List.map .content |> String.concat
            in Expect.equal ( old, new ) ( reconstruct "+", reconstruct "-" ))
            [ ( "short title", "old title", "new title" )
            , ( "past inline preview boundary", String.repeat 200 "a" ++ "old", String.repeat 200 "a" ++ "new" )
            , ( "repeated lines", "a\nx\na\nx\n", "a\na\nx\n" )
            , ( "insertions", "", "one\ntwo\n" )
            , ( "clearing", "one\ntwo\n", "" )
            , ( "whitespace and final newline", "one \n\ttwo\n", "one\n two" )
            , ( "Unicode and literal markup", "# Héj 🌍\n<script>alert(1)</script>", "# Hej 🌎\n<b>literal</b>" )
            ])
    , test "distant changes produce independent context hunks and correct old/new numbers" <| \_ ->
        let middle = List.range 1 30 |> List.map String.fromInt |> String.join "\n"
        in case AuditDiff.compare (Text ("old\n" ++ middle ++ "\nend")) (Text ("new\n" ++ middle ++ "\nEND")) of
            Unified [ first, last ] -> Expect.equal ( ( 1, 1 ), ( Just 32, Just 32 ) ) ( ( first.oldStart, first.newStart ), ( List.filterMap .old last.lines |> List.reverse |> List.head, List.filterMap .new last.lines |> List.reverse |> List.head ) )
            _ -> Expect.fail "Expected two separated unified hunks"
    , test "equal exact values show no differences" <| \_ -> Expect.equal Equal (AuditDiff.compare (Text "same\n") (Text "same\n"))
    , test "null and empty remain distinct" <| \_ -> Expect.notEqual Equal (AuditDiff.compare Null (Text ""))
    , test "typed extraction distinguishes literal null, explicit null, empty, absence and malformed" <| \_ ->
        let field value = AuditDiff.extract "description" (Just (Encode.object [ ( "description", value ) ]))
        in Expect.equal [ Text "null", Null, Text "", MissingSnapshot, MissingField, Malformed, Malformed ]
            [ field (Encode.string "null"), field Encode.null, field (Encode.string ""), AuditDiff.extract "description" Nothing, AuditDiff.extract "description" (Just (Encode.object [])), field (Encode.int 5), AuditDiff.extract "description" (Just (Encode.list identity [])) ]
    , test "unreadable baselines cannot invent an addition or removal" <| \_ -> Expect.equal [ Unavailable, Unavailable, Unavailable ] (List.map (\value -> AuditDiff.compare value (Text "now")) [ MissingSnapshot, MissingField, Malformed ])
    , test "large-input fallback preserves input and skips comparison" <| \_ -> Expect.equal Simplified (AuditDiff.compare (Text (String.repeat 80001 "x")) (Text "new"))
    , test "word emphasis preserves complete prose and marks changed words" <| \_ ->
        let ( before, after ) = AuditDiff.words "The old  café" "The new  café"
        in Expect.equal ( ( "The old  café", "The new  café" ), ( [ "old" ], [ "new" ] ) ) ( ( String.concat (List.map .content before), String.concat (List.map .content after) ), ( List.filter .changed before |> List.map .content, List.filter .changed after |> List.map .content ) )
    , test "frozen current is reused across fields and baselines" <| \_ ->
        let loaded = load opened
            changed = loaded |> step (AuditDiffField "description") |> step (AuditDiffBaseline True) |> step (AuditDiffMode False) |> step (AuditDiffMode True)
        in Expect.equal ( 0, Just "current", Just True ) ( Dict.size changed.auditDiff.admissions, changed.auditDiff.dialog |> Maybe.andThen .current |> Maybe.map .name, changed.auditDiff.dialog |> Maybe.map .beforeBaseline )
    , test "close/reopen rejects HTTP and loaded-at callbacks and retains physical occupancy" <| \_ ->
        let request = requestId opened
            token = dialogToken opened
            reopened = opened |> step CloseAuditDiff |> step (OpenAuditDiff entry "title")
            late = reopened |> step (AuditDiffTaskFetched token request (Ok currentTask)) |> step (AuditDiffLoaded token request (Ok snapshot) (Time.millisToPosix 1))
        in Expect.equal ( 1, 0, Nothing ) ( Dict.size reopened.auditDiff.admissions, Dict.size late.auditDiff.admissions, late.auditDiff.dialog |> Maybe.andThen .current )
    , test "refresh races accept only latest generation even after success clock is queued" <| \_ ->
        let first = opened |> step (AuditDiffTaskFetched (dialogToken opened) (requestId opened) (Ok currentTask))
            refreshed = first |> step RefreshAuditDiff
            oldClock = refreshed |> step (AuditDiffLoaded (dialogToken opened) (requestId opened) (Ok snapshot) (Time.millisToPosix 1))
        in Expect.equal ( Nothing, True ) ( oldClock.auditDiff.dialog |> Maybe.andThen .current, requestId refreshed > requestId opened )
    , test "invalidation during GET survives late snapshot" <| \_ ->
        let socket = opened.webSocket
            changed = { opened | webSocket = { socket | targetGenerations = Dict.singleton "workspace:ws|entity:task:task" 1 } }
            reconciled = Dialog.reconcile opened ( changed, Cmd.none ) |> Tuple.first |> load
        in Expect.equal (Just True) (reconciled.auditDiff.dialog |> Maybe.map .changed)
    , test "unrelated entity and workspace generations do not claim current changed" <| \_ ->
        let socket = opened.webSocket
            unrelated = { opened | webSocket = { socket | targetGenerations = Dict.fromList [ ( "workspace:ws|entity:task:other", 1 ), ( "workspace:other|entity:task:task", 1 ) ] } }
            reconciled = Dialog.reconcile opened ( unrelated, Cmd.none ) |> Tuple.first
        in Expect.equal (Just False) (reconciled.auditDiff.dialog |> Maybe.map .changed)
    , test "newer cache authority marks current changed without replacing frozen text" <| \_ ->
        let loaded = load { opened | tasks = Dict.singleton "task" currentTask }
            changed = { loaded | tasks = Dict.singleton "task" { currentTask | title = "local edit", updatedAt = "2026-10-15T14:00:00Z" } }
            reconciled = Dialog.reconcile loaded ( changed, Cmd.none ) |> Tuple.first
        in Expect.equal ( Just True, Just "current" ) ( reconciled.auditDiff.dialog |> Maybe.map .changed, reconciled.auditDiff.dialog |> Maybe.andThen .current |> Maybe.map .name )
    , test "same-version summary hydration is not evidence of a current change" <| \_ ->
        let loading = opened.dataLoading
            summary = { id = "task", workspaceId = "ws", projectId = Nothing, parentId = Nothing, title = "current", status = Api.Todo, priority = 5, completedAt = Nothing, dependencyCount = 0, createdAt = currentTask.createdAt, updatedAt = currentTask.updatedAt, directSubtaskCount = 0, hasChildren = False, readinessRollup = { openSubtaskCount = 0, doneSubtaskCount = 0, cancelledSubtaskCount = 0, blockedSubtaskCount = 0, dependencyBlockedTaskCount = 0, openDependencyCount = 0, completionReady = True, inProgressSubtaskCount = 0, canStartSubtasks = True } }
            before = { opened | tasks = Dict.singleton "task" { currentTask | description = Nothing }, dataLoading = { loading | taskCardSummaries = Dict.singleton "task" summary } }
            hydrated = { before | tasks = Dict.singleton "task" currentTask }
            reconciled = Dialog.reconcile before ( hydrated, Cmd.none ) |> Tuple.first
        in Expect.equal (Just False) (reconciled.auditDiff.dialog |> Maybe.map .changed)
    , test "authorization retirement preserves physical admissions and request identity" <| \_ ->
        let retired = step AuthUnauthorized opened
        in Expect.equal ( Nothing, 1, True ) ( retired.auditDiff.dialog, Dict.size retired.auditDiff.admissions, retired.auditDiff.nextToken >= opened.auditDiff.nextToken )
    , test "navigation retires dialog and stale responses" <| \_ ->
        let oldUrl = opened.url
            navigated = { opened | url = { oldUrl | fragment = Just "other" } }
            retired = Dialog.reconcile opened ( navigated, Cmd.none ) |> Tuple.first
            late = step (AuditDiffTaskFetched (dialogToken opened) (requestId opened) (Ok currentTask)) retired
        in Expect.equal Nothing late.auditDiff.dialog
    , test "workspace mismatch is a malformed current result" <| \_ ->
        let wrong = { currentTask | workspaceId = "other" }
            completed = step (AuditDiffTaskFetched (dialogToken opened) (requestId opened) (Ok wrong)) opened
        in Expect.equal (Just (requestId opened)) (completed.auditDiff.dialog |> Maybe.andThen .request)
    , test "six physical details queue current read until exact completion frees capacity" <| \_ ->
        let loading = base.dataLoading
            occupied = { base | dataLoading = { loading | cardDetailAdmissions = Set.fromList (List.range 1 6) } }
            waiting = occupied |> step (OpenAuditDiff entry "title") |> step (AuditDiffMode True)
            freeLoading = waiting.dataLoading
            freed = { waiting | dataLoading = { freeLoading | cardDetailAdmissions = Set.fromList (List.range 1 5) } }
            admitted = Dialog.reconcile waiting ( freed, Cmd.none ) |> Tuple.first
        in Expect.equal ( 0, 1 ) ( Dict.size waiting.auditDiff.admissions, Dict.size admitted.auditDiff.admissions )
    , test "errors keep historical comparison and no fabricated current" <| \_ ->
        let errored = step (AuditDiffLoaded (dialogToken opened) (requestId opened) (Err "Current unavailable (404)") (Time.millisToPosix 1)) opened
            historical = step (AuditDiffMode False) errored
        in Expect.equal ( Just Unavailable, True ) ( errored.auditDiff.dialog |> Maybe.map .comparison, historical.auditDiff.dialog |> Maybe.map (.comparison >> (/=) Unavailable) |> Maybe.withDefault False )
    ]

allLines : Comparison -> List AuditDiff.Line
allLines comparison = case comparison of
    Unified hunks -> List.concatMap .lines hunks
    _ -> []

step : Msg -> Model -> Model
step msg model = Main.update msg model |> Tuple.first

dialogToken : Model -> Int
dialogToken model = model.auditDiff.dialog |> Maybe.map .token |> Maybe.withDefault -1

requestId : Model -> Int
requestId model = model.auditDiff.dialog |> Maybe.andThen .request |> Maybe.withDefault -1

opened : Model
opened = base |> step (OpenAuditDiff entry "title") |> step (AuditDiffMode True)

load : Model -> Model
load model = model |> step (AuditDiffTaskFetched (dialogToken model) (requestId model) (Ok currentTask)) |> step (AuditDiffLoaded (dialogToken model) (requestId model) (Ok snapshot) (Time.millisToPosix 1792000000000))

snapshot : { name : String, description : Maybe String, updatedAt : String }
snapshot = { name = "current", description = Just "current description", updatedAt = "2026-10-15T13:00:00Z" }

currentTask : Api.Task
currentTask = { id = "task", workspaceId = "ws", projectId = Nothing, parentId = Nothing, title = "current", description = Just "current description", status = Api.Todo, priority = 5, completedAt = Nothing, dependencyCount = 0, memoryLinkCount = 0, createdAt = "2026-10-14T00:00:00Z", updatedAt = snapshot.updatedAt }

entry : Api.AuditLogEntry
entry = { id = "event", entityType = "task", entityId = "task", workspaceId = Just "ws", action = Api.AuditUpdate, oldValues = Just (Encode.object [ ( "title", Encode.string "old" ), ( "description", Encode.string "old description" ) ]), newValues = Just (Encode.object [ ( "title", Encode.string "after" ), ( "description", Encode.string "after description" ) ]), changedAt = "2026-10-15T12:00:00Z", actorType = Nothing, actorId = Nothing, actorLabel = Nothing, botIdentity = Nothing, requestId = Nothing }

base : Model
base =
    let initial = AppShell.initModel Nothing { protocol = Url.Https, host = "app.example", port_ = Nothing, path = "/workspace/ws", query = Nothing, fragment = Nothing } (WorkspacePage "ws") { apiUrl = "https://api.example", wsUrl = "wss://api.example", sessionId = "session", runtimeMode = "local", authTokenStorageKey = "token", authTokenPresent = False, loginUrl = Nothing, logoutUrl = Nothing } Nothing { tab = ProjectsTab, focus = Nothing, observationId = Nothing }
    in { initial | selectedWorkspaceId = Just "ws", auth = { status = AuthReady, mode = Just "local" }, sessionContext = Just { authMode = "local", effectiveActor = Nothing, principal = { actorType = "user", actorId = "local", actorLabel = "Local", authority = "local_superadmin", grantUserId = Nothing }, globalPermissions = { createWorkspace = True, superadmin = True }, workspace = Nothing } }
