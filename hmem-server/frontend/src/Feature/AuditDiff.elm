module Feature.AuditDiff exposing (init, update, reconcile, view, fields, recompute, timestampSources)

import Api
import AuditDiff exposing (Value(..), Comparison(..))
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Permissions
import Set
import Task
import Time
import TimestampDisplay
import Types exposing (..)
import Url

init : AuditDiffModel
init = { dialog = Nothing, nextToken = 1, admissions = Dict.empty }

fields : Api.AuditLogEntry -> List String
fields entry =
    (if entry.entityType == "project" then [ "name", "description" ] else if List.member entry.entityType [ "task", "subtask" ] then [ "title", "description" ] else [])
        |> List.filter (\field ->
            let
                old = AuditDiff.extract field entry.oldValues
                new = AuditDiff.extract field entry.newValues
            in old /= new && (AuditDiff.available old || AuditDiff.available new))

set : AuditDiffDialog -> Model -> Model
set dialog model =
    let state = model.auditDiff in { model | auditDiff = { state | dialog = Just (recompute dialog) } }

close : Model -> Model
close model =
    let state = model.auditDiff in { model | auditDiff = { state | dialog = Nothing } }

authorized : Model -> Bool
authorized model = model.auth.status == AuthReady && (Permissions.canViewGlobalAudit model || Permissions.canReadCurrentWorkspace model)

otherModal : Model -> Bool
otherModal model = model.move.dialog /= Nothing || model.editing.createForm /= Nothing || model.dragDrop.dropActionModal /= Nothing || model.cards.deleteConfirmation /= Nothing || model.auditLog.revertConfirmation /= Nothing || model.workspaceAdmin.purgeConfirmation /= Nothing || model.groups.workspaceDeletion /= Nothing || model.observations.deleteConfirmation /= Nothing

currentLifetime : AuditDiffDialog -> Model -> Bool
currentLifetime dialog model = authorized model && dialog.epoch == model.sessionRequestEpoch && dialog.audience == model.sessionContext && dialog.route == Url.toString model.url && dialog.workspace == model.selectedWorkspaceId && not (otherModal model)

valid : Int -> Int -> Model -> Maybe AuditDiffDialog
valid token request model = model.auditDiff.dialog |> Maybe.andThen (\dialog -> if dialog.token == token && dialog.request == Just request && currentLifetime dialog model then Just dialog else Nothing)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenAuditDiff entry field ->
            open (originId entry field) entry field model

        OpenAuditDiffFrom origin entry field ->
            open origin entry field model

        CloseAuditDiff -> ( close model, Cmd.none )

        AuditDiffProjectFetched token request result ->
            finish token request (Result.map (\p -> { id = p.id, workspaceId = p.workspaceId, name = p.name, description = p.description, updatedAt = p.updatedAt }) result) model

        AuditDiffTaskFetched token request result ->
            finish token request (Result.map (\t -> { id = t.id, workspaceId = t.workspaceId, name = t.title, description = t.description, updatedAt = t.updatedAt }) result) model

        AuditDiffLoaded token request result now ->
            case valid token request model of
                Nothing -> ( model, Cmd.none )
                Just dialog ->
                    case result of
                        Err error -> ( set { dialog | request = Nothing, error = Just error } model, Cmd.none )
                        Ok snapshot -> ( set { dialog | request = Nothing, current = Just { name = snapshot.name, description = snapshot.description, updatedAt = snapshot.updatedAt, loadedAt = now }, error = Nothing } model, Cmd.none )

        _ ->
            case model.auditDiff.dialog of
                Nothing -> ( model, Cmd.none )
                Just dialog ->
                    case msg of
                        AuditDiffMode current ->
                            ( set { dialog | againstCurrent = current, queued = dialog.queued || (current && dialog.current == Nothing && dialog.request == Nothing && dialog.error == Nothing) } model, Cmd.none )
                        AuditDiffBaseline before -> ( set { dialog | beforeBaseline = before } model, Cmd.none )
                        AuditDiffField field -> if List.member field (fields dialog.entry) then ( set { dialog | field = field } model, Cmd.none ) else ( model, Cmd.none )
                        RefreshAuditDiff ->
                            -- Retire logical request immediately; physical occupancy lasts until its callback.
                            ( set { dialog | queued = True, request = Nothing, error = Nothing, changed = False } model, Cmd.none )
                        _ -> ( model, Cmd.none )

open : String -> Api.AuditLogEntry -> String -> Model -> ( Model, Cmd Msg )
open origin entry field model =
    if not (authorized model) || otherModal model || not (List.member field (fields entry)) then ( model, Cmd.none ) else
    let
        state = model.auditDiff
        dialog = { entry = entry, field = field, origin = origin, againstCurrent = False, beforeBaseline = False, token = state.nextToken, request = Nothing, queued = False, current = Nothing, error = Nothing, changed = False, epoch = model.sessionRequestEpoch, audience = model.sessionContext, route = Url.toString model.url, workspace = model.selectedWorkspaceId, comparison = Equal }
    in ( set dialog { model | auditDiff = { state | nextToken = state.nextToken + 1 } }, Cmd.none )

finish : Int -> Int -> Result Http.Error { id : String, workspaceId : String, name : String, description : Maybe String, updatedAt : String } -> Model -> ( Model, Cmd Msg )
finish token request result model =
    let
        state = model.auditDiff
        released = { model | auditDiff = { state | admissions = Dict.remove request state.admissions } }
    in
    case valid token request released of
        Nothing -> ( released, Cmd.none )
        Just dialog ->
            let
                checked = result |> Result.mapError httpError |> Result.andThen (\record ->
                    if record.id /= dialog.entry.entityId || Just record.workspaceId /= expectedWorkspace dialog then Err "Malformed current response: entity or workspace does not match."
                    else Ok { name = record.name, description = record.description, updatedAt = record.updatedAt })
            in ( released, Task.perform (AuditDiffLoaded token request checked) Time.now )

expectedWorkspace : AuditDiffDialog -> Maybe String
expectedWorkspace dialog =
    case dialog.entry.workspaceId of
        Just workspace -> Just workspace
        Nothing ->
            [ dialog.entry.newValues, dialog.entry.oldValues ] |> List.filterMap (Maybe.andThen (Decode.decodeValue (Decode.field "workspace_id" Decode.string) >> Result.toMaybe)) |> List.head

httpError : Http.Error -> String
httpError error =
    case error of
        Http.BadStatus 403 -> "Current read denied (403)."
        Http.BadStatus 404 -> "Current entity unavailable (404). It may be deleted or inaccessible."
        Http.BadStatus status -> "Current request failed (HTTP " ++ String.fromInt status ++ ")."
        Http.BadBody _ -> "Malformed current response."
        Http.Timeout -> "Current request timed out. Refresh to retry."
        Http.NetworkError -> "Current network request failed. Refresh to retry."
        Http.BadUrl _ -> "Current request URL is invalid."

reconcile : Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
reconcile previous ( model, command ) =
    case model.auditDiff.dialog of
        Nothing -> ( model, command )
        Just dialog ->
            if not (currentLifetime dialog model) then ( close model, command ) else
            let
                entityType = if dialog.entry.entityType == "project" then "project" else "task"
                suffix = "|entity:" ++ entityType ++ ":" ++ dialog.entry.entityId
                generations source = Dict.filter (\key _ -> key == "global" ++ suffix || (expectedWorkspace dialog |> Maybe.map (\workspace -> key == "workspace:" ++ workspace ++ suffix) |> Maybe.withDefault False)) source.webSocket.targetGenerations
                cached source =
                    if entityType == "project" then Dict.get dialog.entry.entityId source.projects |> Maybe.map (\p -> ( p.name, p.description, p.updatedAt ))
                    else Dict.get dialog.entry.entityId source.tasks |> Maybe.map (\t -> ( t.title, t.description, t.updatedAt ))
                fullCache source =
                    if entityType == "project" then not (Dict.member dialog.entry.entityId source.dataLoading.projectCardSummaries) || (Dict.get dialog.entry.entityId source.dataLoading.projectCardDetailRequests |> Maybe.map .succeeded |> Maybe.withDefault False)
                    else not (Dict.member dialog.entry.entityId source.dataLoading.taskCardSummaries) || (Dict.get dialog.entry.entityId source.dataLoading.taskCardDetailRequests |> Maybe.map .succeeded |> Maybe.withDefault False)
                cacheChanged =
                    case ( cached previous, cached model ) of
                        ( Just ( oldName, oldDescription, oldUpdated ), Just ( newName, newDescription, newUpdated ) ) ->
                            newUpdated > oldUpdated || newName /= oldName || (fullCache previous && newDescription /= oldDescription)
                        ( Just _, Nothing ) -> True
                        _ -> False
                changed = (dialog.current /= Nothing || dialog.request /= Nothing) && (generations previous /= generations model || cacheChanged)
                updated = if changed && not dialog.changed then set { dialog | changed = True } model else model
                ( pumped, read ) = pump updated
            in ( pumped, Cmd.batch [ command, read ] )

pump : Model -> ( Model, Cmd Msg )
pump model =
    case model.auditDiff.dialog of
        Nothing -> ( model, Cmd.none )
        Just dialog ->
            let
                state = model.auditDiff
                occupied = Set.size model.dataLoading.cardDetailAdmissions + Dict.size state.admissions + (Dict.values model.move.admissions |> List.filter (.branch >> String.startsWith "@match:") |> List.length)
            in
            if not dialog.queued || occupied >= 6 then ( model, Cmd.none ) else
            let
                request = state.nextToken
                next = { dialog | queued = False, request = Just request, changed = False }
                admitted = set next { model | auditDiff = { state | nextToken = request + 1, admissions = Dict.insert request () state.admissions } }
                read = if dialog.entry.entityType == "project" then Api.fetchProjectForComparison model.flags.apiUrl dialog.entry.entityId (AuditDiffProjectFetched dialog.token request) else Api.fetchTaskForComparison model.flags.apiUrl dialog.entry.entityId (AuditDiffTaskFetched dialog.token request)
            in ( admitted, read )

endpoints : AuditDiffDialog -> ( Value, Value )
endpoints dialog =
    if dialog.againstCurrent then
        ( AuditDiff.extract dialog.field (if dialog.beforeBaseline then dialog.entry.oldValues else dialog.entry.newValues)
        , dialog.current |> Maybe.map (\snapshot -> if dialog.field == "description" then snapshot.description |> Maybe.map Text |> Maybe.withDefault Null else Text snapshot.name) |> Maybe.withDefault MissingSnapshot )
    else ( AuditDiff.extract dialog.field dialog.entry.oldValues, AuditDiff.extract dialog.field dialog.entry.newValues )

recompute : AuditDiffDialog -> AuditDiffDialog
recompute dialog =
    let ( before, after ) = endpoints dialog in { dialog | comparison = AuditDiff.compare before after }

historicalTime : Maybe Decode.Value -> String
historicalTime snapshot = snapshot |> Maybe.andThen (Decode.decodeValue (Decode.field "updated_at" Decode.string) >> Result.toMaybe) |> Maybe.withDefault ""

loadedTime : Time.Posix -> String
loadedTime instant =
    let
        pad number = String.padLeft 2 '0' (String.fromInt number)
        month = case Time.toMonth Time.utc instant of
            Time.Jan -> 1
            Time.Feb -> 2
            Time.Mar -> 3
            Time.Apr -> 4
            Time.May -> 5
            Time.Jun -> 6
            Time.Jul -> 7
            Time.Aug -> 8
            Time.Sep -> 9
            Time.Oct -> 10
            Time.Nov -> 11
            Time.Dec -> 12
    in String.fromInt (Time.toYear Time.utc instant) ++ "-" ++ pad month ++ "-" ++ pad (Time.toDay Time.utc instant) ++ "T" ++ pad (Time.toHour Time.utc instant) ++ ":" ++ pad (Time.toMinute Time.utc instant) ++ ":" ++ pad (Time.toSecond Time.utc instant) ++ "." ++ String.padLeft 3 '0' (String.fromInt (modBy 1000 (Time.posixToMillis instant))) ++ "Z"

timestampSources : Model -> List String
timestampSources model =
    case model.auditDiff.dialog of
        Nothing -> []
        Just dialog ->
            [ historicalTime dialog.entry.oldValues, historicalTime dialog.entry.newValues, dialog.entry.changedAt ]
                ++ (dialog.current |> Maybe.map (\snapshot -> [ snapshot.updatedAt, loadedTime snapshot.loadedAt ]) |> Maybe.withDefault [])

view : Model -> Html Msg
view model =
    case model.auditDiff.dialog of
        Nothing -> text ""
        Just dialog ->
            let
                ( before, after ) = endpoints dialog
                timestamp raw = if raw == "" then "timestamp unavailable" else TimestampDisplay.lookup model.timestamps raw
                oldLabel = if dialog.againstCurrent && not dialog.beforeBaseline then "After" else "Before"
                oldTime = historicalTime (if dialog.againstCurrent && not dialog.beforeBaseline then dialog.entry.newValues else dialog.entry.oldValues)
                newLabel = if dialog.againstCurrent then "Current" else "After"
                newTime = if dialog.againstCurrent then dialog.current |> Maybe.map .updatedAt |> Maybe.withDefault "" else historicalTime dialog.entry.newValues
                loading = dialog.request /= Nothing || dialog.queued
                notice = if dialog.changed then "Current has changed — Refresh." else ""
            in
            node "dialog" [ id "audit-diff-dialog", class "audit-diff-dialog", attribute "aria-labelledby" "audit-diff-title", attribute "data-dialog-token" (String.fromInt dialog.token), attribute "data-dialog-origin" dialog.origin, preventDefaultOn "cancel" (Decode.succeed ( CloseAuditDiff, True )) ]
                [ header [ class "audit-diff-header" ]
                    [ div [] [ h2 [ id "audit-diff-title" ] [ text "View diff" ], p [ class "audit-diff-event" ] [ text ("Audit event · " ++ timestamp dialog.entry.changedAt) ] ]
                    , button [ class "btn btn-secondary", attribute "autofocus" "", onClick CloseAuditDiff ] [ text "Close" ]
                    ]
                , div [ class "audit-diff-controls" ]
                    [ div [ class "audit-diff-modes", attribute "role" "group", attribute "aria-label" "Comparison mode" ]
                        [ button [ class ("btn btn-secondary" ++ (if dialog.againstCurrent then "" else " is-active")), attribute "aria-pressed" (if dialog.againstCurrent then "false" else "true"), onClick (AuditDiffMode False) ] [ text "This change" ]
                        , button [ class ("btn btn-secondary" ++ (if dialog.againstCurrent then " is-active" else "")), attribute "aria-pressed" (if dialog.againstCurrent then "true" else "false"), onClick (AuditDiffMode True) ] [ text "Against current" ]
                        ]
                    , label [] [ text "Field", select [ class "filter-select audit-diff-select", attribute "aria-label" "Field", onInput AuditDiffField ] (fields dialog.entry |> List.map (\field -> option [ value field, selected (field == dialog.field) ] [ text (fieldLabel field) ])) ]
                    , if dialog.againstCurrent then label [] [ text "Baseline", select [ class "filter-select audit-diff-select", attribute "aria-label" "Historical baseline", onInput ((==) "before" >> AuditDiffBaseline) ] [ option [ value "after", selected (not dialog.beforeBaseline) ] [ text "After" ], option [ value "before", selected dialog.beforeBaseline ] [ text "Before" ] ] ] else text ""
                    ]
                , div [ class "audit-diff-endpoints" ] [ endpoint "-" oldLabel (timestamp oldTime) before, endpoint "+" newLabel (timestamp newTime) after ]
                , div [ class "audit-diff-content", tabindex 0, attribute "aria-label" "Text comparison" ]
                    [ case dialog.comparison of
                        Equal -> p [] [ text "No differences" ]
                        Unavailable -> p [] [ text "Comparison unavailable. Both endpoints require readable snapshots; unavailable data is not an empty or removed value." ]
                        Simplified -> div [ class "audit-diff-simplified" ] [ p [] [ text "Large input: simplified full before/after presentation. Text is complete; line and word comparison was skipped." ], fullEndpoint oldLabel before, fullEndpoint newLabel after ]
                        Unified hunks -> div [] (List.map viewHunk hunks)
                    ]
                , if dialog.againstCurrent then div [ class "audit-diff-current", attribute "role" "status" ]
                    [ div [ class "audit-diff-current-heading" ]
                        [ p [] [ text (dialog.current |> Maybe.map (\snapshot -> "Current loaded at " ++ timestamp (loadedTime snapshot.loadedAt) ++ "; entity updated at " ++ timestamp snapshot.updatedAt) |> Maybe.withDefault "Current not loaded.") ]
                        , button [ class "btn btn-secondary", onClick RefreshAuditDiff ] [ text "Refresh" ]
                        ]
                    , if loading then p [] [ text "Loading current…" ] else text ""
                    , dialog.error |> Maybe.map (\error -> p [ class "audit-diff-error" ] [ text error ]) |> Maybe.withDefault (text "")
                    , if dialog.changed then p [ class "audit-diff-notice" ] [ text notice ] else text ""
                    , if model.webSocket.state /= Connected then p [] [ text "Freshness unknown: disconnected or resynchronizing." ] else text ""
                    , if model.page == AuditLogPage then p [] [ text "Global Audit does not subscribe to every workspace. Loaded-at and Refresh are the freshness guarantee; change notices reflect only observed evidence." ] else text ""
                    ] else text ""
                ]

fieldLabel : String -> String
fieldLabel field = String.toUpper (String.left 1 field) ++ String.dropLeft 1 field

originId : Api.AuditLogEntry -> String -> String
originId entry field = "audit-diff-origin-" ++ entry.id ++ "-" ++ field

endpoint : String -> String -> String -> Value -> Html Msg
endpoint sign labelText timestamp value_ = div [ class "audit-diff-endpoint" ] [ strong [] [ text (sign ++ " " ++ labelText) ], span [] [ text (timestamp ++ " · " ++ AuditDiff.describe value_) ] ]

fullEndpoint : String -> Value -> Html Msg
fullEndpoint labelText value_ = section [] [ h3 [] [ text labelText ], pre [] [ text (case value_ of
    Text content -> content
    _ -> AuditDiff.describe value_) ] ]

viewHunk : AuditDiff.Hunk -> Html Msg
viewHunk hunk =
    let
        numbered number = Maybe.map String.fromInt number |> Maybe.withDefault ""
        row segments line = div [ class ("audit-diff-line " ++ (if line.kind == "-" then "removed" else if line.kind == "+" then "added" else "context")) ]
            [ span [ class "audit-diff-number" ] [ text (numbered line.old) ], span [ class "audit-diff-number" ] [ text (numbered line.new) ], span [ class "audit-diff-sign", attribute "aria-label" (if line.kind == "-" then "Removed" else if line.kind == "+" then "Added" else "Unchanged") ] [ text line.kind ]
            , code [] (List.map (\segment -> if segment.changed then mark [] [ text segment.content ] else text segment.content) segments ++ (if String.endsWith "\n" line.content then [] else [ span [ class "audit-diff-no-newline" ] [ text " ␄ No newline at end" ] ])) ]
        plain line = [ { content = line.content, changed = False } ]
        takeKind kind remaining =
            case remaining of
                first :: rest ->
                    if first.kind == kind then let ( taken, tail ) = takeKind kind rest in ( first :: taken, tail )
                    else ( [], remaining )
                [] -> ( [], [] )
        paired removed added =
            case ( removed, added ) of
                ( old :: oldRest, new :: newRest ) ->
                    let ( left, right ) = AuditDiff.words old.content new.content in row left old :: row right new :: paired oldRest newRest
                _ -> List.map (\line -> row (plain line) line) (removed ++ added)
        rows remaining =
            case remaining of
                first :: rest ->
                    if first.kind == "-" then
                        let
                            ( removed, next ) = takeKind "-" remaining
                            ( added, tail ) = takeKind "+" next
                        in paired removed added ++ rows tail
                    else row (plain first) first :: rows rest
                [] -> []
    in section [ class "audit-diff-hunk" ] (div [ class "audit-diff-hunk-heading" ] [ text ("@@ -" ++ String.fromInt hunk.oldStart ++ "," ++ String.fromInt hunk.oldCount ++ " +" ++ String.fromInt hunk.newStart ++ "," ++ String.fromInt hunk.newCount ++ " @@") ] :: rows hunk.lines)
