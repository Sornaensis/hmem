module Feature.ChangeStream exposing (Action(..), RequestGuard, Scope(..), Snapshot, State, acceptEvent, applySnapshot, applySnapshotProfile, coalesce, init, reduceFrame, reduceFrames, requestGuardMatches, scopeKey)

{-| Pure canonical-stream policy. Effects remain in `Feature.WebSocket`; this
module gives it one fail-closed place for scope matching, bounded de-duplication
and invalidation coalescing.
-}

import Api
import Dict exposing (Dict)
import Json.Decode
import Set exposing (Set)


type Scope
    = Workspace String
    | Global


type alias State =
    { scope : Scope
    , eventIds : List String
    , resumeToken : Maybe String
    , live : Bool
    }


type alias Snapshot =
    { workspaces : Dict String Api.Workspace
    , groups : Dict String Api.WorkspaceGroup
    , projects : Dict String Api.Project
    , tasks : Dict String Api.Task
    , dependencies : List Api.WorkspaceTaskDependencyLink
    , observations : Dict String Api.Observation
    }


type alias RequestGuard =
    { scopeKey : String
    , targetKey : String
    , targetGeneration : Int
    , sessionEpoch : Int
    , routeWorkspace : Maybe String
    , audienceId : String
    }


type Action
    = RefetchEntity String String
    | RemoveEntity String String
    | RefreshTaskOverview String
    | RefreshReadiness String String
    | RevalidateNavigationSummary String String
    | RefreshNextTasks String
    | RefreshSearch String
    | RefreshObservations
    | RefreshCatalogue
    | RefreshGroups
    | RefreshGroupMembers String
    | RefreshMemberships String
    | RefreshSessionAuthorization
    | AccessGranted String
    | ClearWorkspace String
    | RefreshTimeline
    | BeginResync
    | NoAction


init : Scope -> List String -> State
init scope eventIds =
    { scope = scope, eventIds = List.take 128 eventIds, resumeToken = Nothing, live = False }


scopeKey : Scope -> String
scopeKey scope =
    case scope of
        Workspace workspaceId ->
            "workspace:" ++ workspaceId

        Global ->
            "global"


requestGuardMatches : RequestGuard -> Int -> Maybe String -> String -> Dict String Int -> Bool
requestGuardMatches guard sessionEpoch routeWorkspace audienceId generations =
    guard.sessionEpoch
        == sessionEpoch
        && guard.routeWorkspace
        == routeWorkspace
        && guard.audienceId
        == audienceId
        && Dict.get (guard.scopeKey ++ "|" ++ guard.targetKey) generations
        == Just guard.targetGeneration


matchesScope : Scope -> Api.ChangeStreamScope -> Bool
matchesScope own received =
    case ( own, received ) of
        ( Workspace expected, Api.WorkspaceScope actual ) ->
            expected == actual

        ( Global, Api.GlobalScope ) ->
            True

        _ ->
            False


acceptEvent : String -> State -> Maybe State
acceptEvent eventId state =
    if List.member eventId state.eventIds then
        Nothing

    else
        Just { state | eventIds = List.take 128 (eventId :: state.eventIds) }


reduceFrame : Api.CanonicalFrame -> State -> ( State, List Action )
reduceFrame frame state =
    case frame of
        Api.CanonicalCheckpoint token ->
            ( { state | resumeToken = Just token, live = True }, [] )

        Api.CanonicalResyncRequired ->
            ( { state | live = False, resumeToken = Nothing }, [ BeginResync ] )

        Api.CanonicalAccessGranted workspaceId ->
            ( state, [ AccessGranted workspaceId, RefreshCatalogue, RefreshSessionAuthorization ] )

        Api.CanonicalAccessRevoked maybeWorkspace ->
            case ( state.scope, maybeWorkspace ) of
                ( Workspace current, Just revoked ) ->
                    if current == revoked then
                        ( { state | live = False, resumeToken = Nothing }, [ ClearWorkspace current, RefreshCatalogue, RefreshSessionAuthorization ] )

                    else
                        ( state, [ RefreshCatalogue, RefreshSessionAuthorization ] )

                ( Workspace current, Nothing ) ->
                    ( { state | live = False, resumeToken = Nothing }, [ ClearWorkspace current, RefreshCatalogue, RefreshSessionAuthorization ] )

                ( Global, _ ) ->
                    ( state, [ RefreshCatalogue, RefreshSessionAuthorization ] )

        Api.CanonicalChange envelope ->
            if not (matchesScope state.scope envelope.scope) then
                ( state, [] )

            else if not (validEnvelope envelope) then
                ( { state | live = False }, [ BeginResync ] )

            else
                case acceptEvent envelope.eventId state of
                    Nothing ->
                        ( state, [] )

                    Just next ->
                        let
                            invalidationActionsForEnvelope =
                                List.concatMap (invalidationActions envelope) envelope.invalidations

                            timelineActions =
                                if timelineEligible envelope && not (List.member BeginResync invalidationActionsForEnvelope) then
                                    [ RefreshTimeline ]

                                else
                                    []
                        in
                        ( next, coalesce (invalidationActionsForEnvelope ++ timelineActions) )

        Api.CanonicalScoped _ _ ->
            ( { state | live = False }, [ BeginResync ] )

        Api.CanonicalSnapshot _ _ _ _ ->
            ( { state | live = False }, [ BeginResync ] )

        Api.CanonicalBatch _ _ ->
            ( { state | live = False }, [ BeginResync ] )


reduceFrames : List Api.CanonicalFrame -> State -> ( State, List Action )
reduceFrames frames state =
    let
        step frame ( current, actions ) =
            let
                ( next, additions ) =
                    reduceFrame frame current
            in
            ( next, actions ++ additions )

        ( final, finalActions ) =
            List.foldl step ( state, [] ) frames
    in
    if List.member BeginResync finalActions then
        ( { final | live = False, resumeToken = Nothing }, [ BeginResync ] )

    else
        ( final, coalesce finalActions )


validEnvelope : Api.CanonicalEnvelope -> Bool
validEnvelope envelope =
    let
        scopeMatches =
            case ( envelope.scope, envelope.workspaceId ) of
                ( Api.WorkspaceScope expected, Just actual ) ->
                    expected == actual && not (String.isEmpty expected)

                ( Api.GlobalScope, Nothing ) ->
                    True

                _ ->
                    False

        entityMatches =
            case envelope.scope of
                Api.WorkspaceScope _ ->
                    List.member envelope.entityType [ "workspace", "project", "task", "observation", "task_dependency", "workspace_group_membership", "workspace_membership" ]

                Api.GlobalScope ->
                    List.member envelope.entityType [ "workspace", "workspace_group" ]

        invalidationsValid =
            List.all
                (\invalidation ->
                    not (String.isEmpty invalidation.kind)
                        && (String.split ":" invalidation.target |> List.all (not << String.isEmpty))
                )
                envelope.invalidations
    in
    scopeMatches && entityMatches && invalidationsValid && not (String.isEmpty envelope.eventId) && not (String.isEmpty envelope.entityId) && not (List.isEmpty envelope.invalidations)


timelineEligible : Api.CanonicalEnvelope -> Bool
timelineEligible envelope =
    case ( envelope.scope, envelope.entityType, envelope.entityAction ) of
        ( Api.WorkspaceScope _, "project", action ) ->
            List.member action [ "created", "updated", "deleted", "restored" ]

        ( Api.WorkspaceScope _, "task", action ) ->
            List.member action [ "created", "updated", "deleted", "restored" ]

        ( Api.WorkspaceScope _, "observation", action ) ->
            List.member action [ "created", "deleted" ]

        _ ->
            False


invalidationActions : Api.CanonicalEnvelope -> Api.CanonicalInvalidation -> List Action
invalidationActions envelope invalidation =
    let
        parts =
            String.split ":" invalidation.target

        primaryEntity =
            case envelope.entityType of
                "workspace_group" ->
                    "group"

                "workspace_group_membership" ->
                    "group_membership"

                other ->
                    other

        entityAction entity identity =
            if envelope.entityAction == "deleted" && entity == primaryEntity && identity == envelope.entityId then
                if entity == "workspace" then
                    [ RemoveEntity entity identity, ClearWorkspace identity, RefreshSessionAuthorization ]

                else if entity == "observation" then
                    [ RemoveEntity entity identity ]

                else
                    [ RemoveEntity entity identity ]

            else
                case entity of
                    "group" ->
                        [ RefreshGroups ]

                    "group_membership" ->
                        case String.split ":" identity of
                            groupId :: _ :: [] ->
                                [ RefreshGroupMembers groupId ]

                            _ ->
                                [ BeginResync ]

                    "observation" ->
                        [ RefetchEntity entity identity ]

                    "project" ->
                        [ RevalidateNavigationSummary entity identity ]

                    "task" ->
                        [ RevalidateNavigationSummary entity identity ]

                    _ ->
                        [ RefetchEntity entity identity ]
    in
    case ( envelope.scope, invalidation.kind, parts ) of
        ( Api.WorkspaceScope expected, "entity", [ "workspace", workspaceId ] ) ->
            if expected == workspaceId && not (String.isEmpty workspaceId) then
                entityAction "workspace" workspaceId

            else
                [ BeginResync ]

        ( Api.WorkspaceScope _, "entity", [ entity, identity ] ) ->
            if not (String.isEmpty identity) && List.member entity [ "project", "task", "observation" ] then
                entityAction entity identity

            else
                [ BeginResync ]

        ( Api.WorkspaceScope _, "entity", [ "task_dependency", taskId, dependsOnId ] ) ->
            if List.all (not << String.isEmpty) [ taskId, dependsOnId ] then
                let
                    identity =
                        taskId ++ ":" ++ dependsOnId
                in
                if envelope.entityAction == "deleted" && primaryEntity == "task_dependency" && envelope.entityId == identity then
                    [ RemoveEntity "task_dependency" identity, RevalidateNavigationSummary "task" taskId ]

                else
                    [ RevalidateNavigationSummary "task" taskId ]

            else
                [ BeginResync ]

        ( Api.WorkspaceScope expected, "entity", [ "group_membership", groupId, workspaceId ] ) ->
            if expected == workspaceId && not (String.isEmpty groupId) then
                entityAction "group_membership" (groupId ++ ":" ++ workspaceId)

            else
                [ BeginResync ]

        ( Api.GlobalScope, "entity", [ entity, identity ] ) ->
            if not (String.isEmpty identity) && List.member entity [ "workspace", "group" ] then
                entityAction entity identity

            else
                [ BeginResync ]

        ( Api.WorkspaceScope expected, "collection", [ collection, workspaceId ] ) ->
            if workspaceId == expected && List.member collection [ "projects", "tasks", "observations", "task_dependencies" ] then
                if collection == "observations" then
                    []

                else
                    [ NoAction ]

            else
                [ BeginResync ]

        ( Api.WorkspaceScope expected, "collection", [ "workspace", workspaceId, "memberships" ] ) ->
            if workspaceId == expected then
                [ RefreshMemberships workspaceId ]

            else
                [ BeginResync ]

        ( Api.WorkspaceScope _, "collection", [ "group", groupId, "members" ] ) ->
            if String.isEmpty groupId then
                [ BeginResync ]

            else
                [ RefreshGroupMembers groupId ]

        ( Api.WorkspaceScope expected, "collection", [ "workspace", workspaceId, "groups" ] ) ->
            if workspaceId == expected then
                [ NoAction ]

            else
                [ BeginResync ]

        ( Api.GlobalScope, "collection", [ "workspace-groups" ] ) ->
            [ RefreshGroups ]

        ( Api.WorkspaceScope expected, "tree", [ "workspace", workspaceId ] ) ->
            if workspaceId == expected then
                [ NoAction ]

            else
                [ BeginResync ]

        ( Api.WorkspaceScope _, "readiness", [ entity, identity ] ) ->
            case entity of
                "task" ->
                    [ RevalidateNavigationSummary entity identity ]

                "project" ->
                    [ RevalidateNavigationSummary entity identity ]

                _ ->
                    [ BeginResync ]

        ( Api.WorkspaceScope expected, "next_task", [ "workspace", workspaceId ] ) ->
            if workspaceId == expected then
                [ RefreshNextTasks workspaceId ]

            else
                [ BeginResync ]

        ( Api.WorkspaceScope expected, "search", [ "workspace", workspaceId ] ) ->
            if workspaceId == expected then
                [ RefreshSearch workspaceId ]

            else
                [ BeginResync ]

        ( Api.GlobalScope, "catalogue", [ "workspace-catalog" ] ) ->
            [ RefreshCatalogue ]

        ( Api.WorkspaceScope _, "catalogue", [ "workspace-catalog" ] ) ->
            [ RefreshCatalogue ]

        ( Api.WorkspaceScope _, "session_authorization", [ "session-authorization" ] ) ->
            [ RefreshSessionAuthorization ]

        ( Api.WorkspaceScope _, "permission_cache", [ "permission-cache" ] ) ->
            [ RefreshSessionAuthorization ]

        _ ->
            [ BeginResync ]


coalesce : List Action -> List Action
coalesce actions =
    let
        entityKey action =
            case action of
                RefetchEntity kind identity ->
                    Just ("entity:" ++ kind ++ ":" ++ identity)

                RemoveEntity kind identity ->
                    Just ("entity:" ++ kind ++ ":" ++ identity)

                _ ->
                    Nothing

        key action =
            case action of
                RefetchEntity "task_dependency" identity ->
                    case String.split ":" identity of
                        taskId :: _ :: [] ->
                            "task-overview:" ++ taskId

                        _ ->
                            "entity:task_dependency:" ++ identity

                RefetchEntity kind identity ->
                    "entity:" ++ kind ++ ":" ++ identity

                RemoveEntity kind identity ->
                    "entity:" ++ kind ++ ":" ++ identity

                RefreshTaskOverview taskId ->
                    "task-overview:" ++ taskId

                RefreshReadiness kind identity ->
                    if kind == "task" then
                        "task-overview:" ++ identity

                    else
                        "readiness:" ++ kind ++ ":" ++ identity

                RevalidateNavigationSummary kind identity ->
                    "navigation-summary:" ++ kind ++ ":" ++ identity

                RefreshNextTasks workspace ->
                    "next:" ++ workspace

                RefreshSearch workspace ->
                    "search:" ++ workspace

                RefreshObservations ->
                    "observations"

                RefreshCatalogue ->
                    "catalogue"

                RefreshGroups ->
                    "groups"

                RefreshGroupMembers groupId ->
                    "group-members:" ++ groupId

                RefreshMemberships workspace ->
                    "memberships:" ++ workspace

                RefreshSessionAuthorization ->
                    "session"

                AccessGranted workspace ->
                    "grant:" ++ workspace

                ClearWorkspace workspace ->
                    "clear:" ++ workspace

                RefreshTimeline ->
                    "timeline"

                BeginResync ->
                    "resync"

                NoAction ->
                    "none"

        finalEntities =
            List.foldl
                (\action transitions ->
                    case entityKey action of
                        Just target ->
                            Dict.insert target action transitions

                        Nothing ->
                            transitions
                )
                Dict.empty
                actions

        clearSuperseded action =
            case action of
                ClearWorkspace workspaceId ->
                    case Dict.get ("entity:workspace:" ++ workspaceId) finalEntities of
                        Just (RefetchEntity "workspace" _) ->
                            True

                        _ ->
                            False

                _ ->
                    False

        step action ( seen, kept ) =
            let
                finalAction =
                    entityKey action
                        |> Maybe.andThen (\entityTarget -> Dict.get entityTarget finalEntities)
                        |> Maybe.withDefault action

                target =
                    key finalAction
            in
            if clearSuperseded action || Set.member target seen then
                ( seen, kept )

            else
                ( Set.insert target seen, finalAction :: kept )

        placeWorkspaceClear workspaceId ordered =
            let
                clear =
                    ClearWorkspace workspaceId

                withoutClear =
                    List.filter ((/=) clear) ordered

                insert remaining =
                    case remaining of
                        [] ->
                            []

                        current :: rest ->
                            if current == RemoveEntity "workspace" workspaceId then
                                current :: clear :: rest

                            else
                                current :: insert rest
            in
            if List.member clear ordered then
                insert withoutClear

            else
                ordered

        workspaceDeletes =
            Dict.values finalEntities
                |> List.filterMap
                    (\action ->
                        case action of
                            RemoveEntity "workspace" workspaceId ->
                                Just workspaceId

                            _ ->
                                Nothing
                    )
    in
    if List.member BeginResync actions then
        [ BeginResync ]

    else
        actions
            |> List.foldl step ( Set.empty, [] )
            |> Tuple.second
            |> List.reverse
            |> (\ordered -> List.foldl placeWorkspaceClear ordered workspaceDeletes)


applySnapshot : Scope -> List Api.SnapshotItem -> Result String Snapshot
applySnapshot scope =
    applySnapshotProfile scope "full_v1"


applySnapshotProfile : Scope -> String -> List Api.SnapshotItem -> Result String Snapshot
applySnapshotProfile scope profile items =
    let
        permitted kind =
            case scope of
                Workspace _ ->
                    if profile == "workspace_shell_v1" then
                        kind == "workspace"

                    else if profile == "full_v1" then
                        List.member kind [ "workspace", "project", "task", "task_dependency", "observation" ]

                    else
                        False

                Global ->
                    profile == "full_v1" && List.member kind [ "workspace", "workspace_group" ]

        insertUnique identity value dictionary =
            if String.isEmpty identity || Dict.member identity dictionary then
                Err "snapshot identity is empty or duplicated"

            else
                Ok (Dict.insert identity value dictionary)

        nonEmpty value =
            not (String.isEmpty value)

        validOptional maybeValue =
            Maybe.map nonEmpty maybeValue |> Maybe.withDefault True

        add item result =
            if not (permitted item.kind) then
                Err "snapshot kind is invalid for scope"

            else
                case ( item.kind, result ) of
                    ( "workspace", Ok snapshot ) ->
                        Json.Decode.decodeValue Api.workspaceDecoder item.data
                            |> Result.mapError (\_ -> "malformed snapshot item")
                            |> Result.andThen
                                (\value ->
                                    if scopeOwnsWorkspace scope value.id && List.all nonEmpty [ value.id, value.name, value.createdAt, value.updatedAt ] then
                                        insertUnique value.id value snapshot.workspaces
                                            |> Result.map (\workspaces -> { snapshot | workspaces = workspaces })

                                    else
                                        Err "workspace snapshot identity is outside its scope"
                                )

                    ( "workspace_group", Ok snapshot ) ->
                        Json.Decode.decodeValue Api.workspaceGroupDecoder item.data
                            |> Result.mapError (\_ -> "malformed snapshot item")
                            |> Result.andThen
                                (\value ->
                                    if List.all nonEmpty [ value.id, value.name, value.createdAt, value.updatedAt ] then
                                        insertUnique value.id value snapshot.groups |> Result.map (\groups -> { snapshot | groups = groups })

                                    else
                                        Err "snapshot group has an empty required field"
                                )

                    ( "project", Ok snapshot ) ->
                        Json.Decode.decodeValue Api.projectDecoder item.data
                            |> Result.mapError (\_ -> "malformed snapshot item")
                            |> Result.andThen
                                (\value ->
                                    if scopeOwnsWorkspace scope value.workspaceId && List.all nonEmpty [ value.id, value.workspaceId, value.name, value.createdAt, value.updatedAt ] && validOptional value.parentId then
                                        insertUnique value.id value snapshot.projects
                                            |> Result.map (\projects -> { snapshot | projects = projects })

                                    else
                                        Err "project snapshot identity is outside its scope"
                                )

                    ( "task", Ok snapshot ) ->
                        Json.Decode.decodeValue Api.taskDecoder item.data
                            |> Result.mapError (\_ -> "malformed snapshot item")
                            |> Result.andThen
                                (\value ->
                                    if scopeOwnsWorkspace scope value.workspaceId && List.all nonEmpty [ value.id, value.workspaceId, value.title, value.createdAt, value.updatedAt ] && validOptional value.projectId && validOptional value.parentId then
                                        insertUnique value.id value snapshot.tasks
                                            |> Result.map (\tasks -> { snapshot | tasks = tasks })

                                    else
                                        Err "task snapshot identity is outside its scope"
                                )

                    ( "task_dependency", Ok snapshot ) ->
                        Json.Decode.decodeValue dependencyDecoder item.data
                            |> Result.mapError (\_ -> "malformed snapshot item")
                            |> Result.andThen
                                (\value ->
                                    if String.isEmpty value.taskId || String.isEmpty value.dependsOnId || List.any (\existing -> existing.taskId == value.taskId && existing.dependsOnId == value.dependsOnId) snapshot.dependencies then
                                        Err "snapshot dependency is empty or duplicated"

                                    else
                                        Ok { snapshot | dependencies = value :: snapshot.dependencies }
                                )

                    ( "observation", Ok snapshot ) ->
                        Json.Decode.decodeValue Api.observationDecoder item.data
                            |> Result.mapError (\_ -> "malformed snapshot item")
                            |> Result.andThen
                                (\value ->
                                    if scopeOwnsWorkspace scope value.workspaceId && List.all nonEmpty [ value.id, value.workspaceId, value.gitSha, value.content, value.createdAt, value.updatedAt ] && List.all (\subject -> nonEmpty subject.subject) value.subjects then
                                        insertUnique value.id value snapshot.observations
                                            |> Result.map (\observations -> { snapshot | observations = observations })

                                    else
                                        Err "observation snapshot identity is outside its scope"
                                )

                    ( _, Ok values ) ->
                        Ok values

                    ( _, Err err ) ->
                        Err err

        validate snapshot =
            let
                dependenciesValid =
                    List.all
                        (\link ->
                            link.taskId
                                /= link.dependsOnId
                                && Dict.member link.taskId snapshot.tasks
                                && Dict.member link.dependsOnId snapshot.tasks
                        )
                        snapshot.dependencies

                scopeRootValid =
                    case scope of
                        Workspace workspaceId ->
                            Dict.keys snapshot.workspaces == [ workspaceId ]

                        Global ->
                            True
            in
            if not dependenciesValid then
                Err "snapshot dependency references a task outside the completed workspace snapshot"

            else if not scopeRootValid then
                Err "workspace snapshot root is missing"

            else
                Ok snapshot
    in
    List.foldl add (Ok { workspaces = Dict.empty, groups = Dict.empty, projects = Dict.empty, tasks = Dict.empty, dependencies = [], observations = Dict.empty }) items
        |> Result.andThen validate


scopeOwnsWorkspace : Scope -> String -> Bool
scopeOwnsWorkspace scope workspaceId =
    case scope of
        Workspace expected ->
            workspaceId == expected

        Global ->
            True


dependencyDecoder : Json.Decode.Decoder Api.WorkspaceTaskDependencyLink
dependencyDecoder =
    Json.Decode.map2 Api.WorkspaceTaskDependencyLink
        (Json.Decode.field "task_id" nonEmptyStringDecoder)
        (Json.Decode.field "depends_on_id" nonEmptyStringDecoder)


nonEmptyStringDecoder : Json.Decode.Decoder String
nonEmptyStringDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\value ->
                if String.isEmpty value then
                    Json.Decode.fail "empty string"

                else
                    Json.Decode.succeed value
            )
