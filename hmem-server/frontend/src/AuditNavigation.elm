module AuditNavigation exposing (destinationType, destinationUrl, related, resolve, snapshotId, validId)

import Api
import Helpers
import Json.Decode as Decode
import Types exposing (..)
import Url


{-| Relation keys are read from snapshots, never from composite relation IDs.
Unknown types have no implicit Projects destination.
-}
resolve : Api.AuditLogEntry -> Maybe AuditDestination
resolve entry =
    let
        workspace =
            if entry.entityType == "workspace" then checked entry.entityId
            else entry.workspaceId |> Maybe.andThen checked |> orElse (snapshotId "workspace_id" entry)
        direct kind = Maybe.map2 (\ws target -> { workspaceId = ws, kind = kind, id = target }) workspace (checked entry.entityId)
        relation kind field = Maybe.map2 (\ws target -> { workspaceId = ws, kind = kind, id = target }) workspace (snapshotId field entry)
    in
    case entry.entityType of
        "project" -> direct AuditProject
        "task" -> direct AuditTask
        "observation" -> direct AuditObservation
        "agent_session" -> direct AuditSession |> Maybe.andThen sessionDestination
        "task_session" -> relation AuditSession "session_id" |> Maybe.andThen sessionDestination
        "task_dependency" -> relation AuditTask "task_id"
        "workspace" -> direct AuditWorkspace
        _ -> Nothing


related : Api.AuditLogEntry -> List ( String, AuditDestination )
related entry =
    let
        field = if entry.entityType == "task_session" then "task_id" else "depends_on_id"
        label = if entry.entityType == "task_session" then "Task" else "Dependency task"
        workspace = entry.workspaceId |> Maybe.andThen checked |> orElse (snapshotId "workspace_id" entry)
    in
    if List.member entry.entityType [ "task_session", "task_dependency" ] then
        Maybe.map2 (\ws target -> [ ( label, { workspaceId = ws, kind = AuditTask, id = target } ) ]) workspace (snapshotId field entry) |> Maybe.withDefault []
    else []


snapshotId : String -> Api.AuditLogEntry -> Maybe String
snapshotId field entry =
    let
        read = Maybe.andThen (Decode.decodeValue (Decode.field field Decode.string) >> Result.toMaybe >> Maybe.andThen checked)
    in
    read entry.newValues |> orElse (read entry.oldValues)


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback primary = case primary of
    Just _ -> primary
    Nothing -> fallback


checked : String -> Maybe String
checked value = if validId value then Just value else Nothing


validId : String -> Bool
validId value =
    String.length value == 36 && List.map String.length (String.split "-" value) == [8,4,4,4,12]
        && String.all (\char -> String.contains (String.fromChar char) "0123456789abcdef-") value


sessionDestination : AuditDestination -> Maybe AuditDestination
sessionDestination destination =
    if String.length destination.id == 36 && List.map String.length (String.split "-" destination.id) == [8,4,4,4,12]
        && String.all (\char -> String.contains (String.fromChar char) "0123456789abcdef-") destination.id then Just destination else Nothing


destinationType : AuditDestination -> String
destinationType destination = case destination.kind of
    AuditProject -> "project"
    AuditTask -> "task"
    AuditObservation -> "observation"
    AuditSession -> "agent_session"
    AuditWorkspace -> "workspace"


destinationUrl : AuditDestination -> String
destinationUrl destination =
    "/workspace/" ++ Url.percentEncode destination.workspaceId ++ "#"
        ++ (case destination.kind of
                AuditProject -> Helpers.buildFragment ProjectsTab (Just ( "project", destination.id )) Nothing
                AuditTask -> Helpers.buildFragment ProjectsTab (Just ( "task", destination.id )) Nothing
                AuditObservation -> Helpers.buildFragment ObservationsTab Nothing (Just destination.id)
                AuditSession -> "tab=sessions&session=" ++ Url.percentEncode destination.id
                AuditWorkspace -> Helpers.buildFragment ProjectsTab Nothing Nothing
           )
