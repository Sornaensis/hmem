module Feature.AuditLog exposing (AuditFieldChange, auditActionDetailItems, auditChangedFieldItems, auditContextDetailItems, auditReturnFilters, init, nextAuditOffset, update, viewAuditLogPage, viewWorkspaceAuditPanel, viewEntityHistory, viewRevertConfirmModal)

import Api
import Browser.Navigation as Nav
import Dict
import Feature.Focus as Focus
import Helpers exposing (beginTrackedMutation, buildFragment, flexibleStringDecoder, formatDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Permissions
import Toast exposing (addToast)
import Types exposing (..)


init : AuditLogModel
init =
    { entityHistory = Dict.empty
    , entityHistoryHasMore = Dict.empty
    , historyExpanded = Dict.empty
    , entries = []
    , entryBaseOffset = 0
    , hasMore = False
    , loading = False
    , loadingFilters = Nothing
    , filters = { workspaceId = Nothing, entityType = Nothing, entityId = Nothing, action = Nothing, since = Nothing, until = Nothing, limit = Just 50, offset = Nothing }
    , expandedEntries = Dict.empty
    , revertConfirmation = Nothing
    , revertInFlight = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavigateToAuditEntity auditEntry ->
            let
                -- Extract workspace_id from the entry's values
                extractWsId val =
                    Decode.decodeValue (Decode.field "workspace_id" Decode.string) val |> Result.toMaybe

                orElseMaybe fallback primary =
                    case primary of
                        Just _ ->
                            primary

                        Nothing ->
                            fallback

                mWorkspaceId =
                    case auditEntry.workspaceId of
                        Just _ ->
                            auditEntry.workspaceId

                        Nothing ->
                            orElseMaybe
                                (Maybe.andThen extractWsId auditEntry.oldValues)
                                (Maybe.andThen extractWsId auditEntry.newValues)

                resolvedTarget =
                    resolveAuditNavigationTarget auditEntry

                returnSource =
                    auditReturnSource model

                entryExpanded =
                    Dict.get auditEntry.id model.auditLog.expandedEntries |> Maybe.withDefault False

                returnFilters =
                    auditReturnFilters auditEntry.id model.auditLog
            in
            case resolvedTarget of
                Nothing ->
                    addToast Warning "Cannot navigate to this entity type" model

                Just ( targetType, targetId ) ->
                    case mWorkspaceId of
                        Just wsId ->
                            let
                                targetTab =
                                    case targetType of
                                        "memory" ->
                                            MemoriesTab

                                        _ ->
                                            ProjectsTab

                                focusEntry =
                                    ( targetType, targetId )

                                focusModel =
                                    model.focus

                                currentSearch =
                                    model.search

                                updatedSearch =
                                    { currentSearch
                                        | unifiedResults = Nothing
                                        , isSearching = False
                                        , searchError = Nothing
                                        , activeRequestQuery = Nothing
                                    }

                                newHistory =
                                    List.take (focusModel.historyIndex + 1) focusModel.history ++ [ focusEntry ]

                                newIndex =
                                    List.length newHistory - 1
                            in
                            ( updateFocusModel
                                (\fc ->
                                    { fc
                                        | focusedEntity = Just focusEntry
                                        , breadcrumbAnchor = Just focusEntry
                                        , history = newHistory
                                        , historyIndex = newIndex
                                        , returnContext = Just (Focus.auditReturnContext returnSource wsId returnFilters entryExpanded auditEntry focusEntry)
                                    }
                                )
                                { model | selectedWorkspaceId = Just wsId, activeTab = targetTab, search = updatedSearch }
                            , Nav.pushUrl model.key ("/workspace/" ++ wsId ++ "#" ++ buildFragment targetTab (Just focusEntry))
                            )

                        Nothing ->
                            addToast Warning "Cannot navigate: entity workspace unknown" model

        GotAuditLog requestedFilters result ->
            if requestedFilters /= model.auditLog.filters || model.auditLog.loadingFilters /= Just requestedFilters then
                ( model, Cmd.none )

            else
                case result of
                    Ok paginated ->
                        ( updateAuditLogModel
                            (\al ->
                                { al
                                    | entries = al.entries ++ paginated.items
                                    , entryBaseOffset =
                                        if List.isEmpty al.entries then
                                            requestedFilters.offset |> Maybe.withDefault 0

                                        else
                                            al.entryBaseOffset
                                    , hasMore = paginated.hasMore
                                    , loading = False
                                    , loadingFilters = Nothing
                                }
                            )
                            model
                        , Cmd.none
                        )

                    Err _ ->
                        addToast Error "Failed to load audit log" (updateAuditLogModel (\al -> { al | loading = False, loadingFilters = Nothing }) model)

        GotEntityHistory entityId result ->
            case result of
                Ok paginated ->
                    ( updateAuditLogModel
                        (\al ->
                            { al
                                | entityHistory = Dict.insert entityId paginated.items al.entityHistory
                                , entityHistoryHasMore = Dict.insert entityId paginated.hasMore al.entityHistoryHasMore
                            }
                        )
                        model
                    , Cmd.none
                    )

                Err _ ->
                    let
                        ( m, cmd ) =
                            addToast Error "Failed to load entity history" model
                    in
                    ( updateAuditLogModel (\al -> { al | entityHistory = Dict.insert entityId [] al.entityHistory }) m, cmd )

        ToggleEntityHistory entityType entityId ->
            let
                current =
                    Dict.get entityId model.auditLog.historyExpanded |> Maybe.withDefault False

                newExpanded =
                    not current

                fetchCmd =
                    if newExpanded && not (Dict.member entityId model.auditLog.entityHistory) then
                        Api.fetchEntityHistory model.flags.apiUrl entityType entityId Nothing (GotEntityHistory entityId)

                    else
                        Cmd.none
            in
            ( updateAuditLogModel (\al -> { al | historyExpanded = Dict.insert entityId newExpanded al.historyExpanded }) model
            , fetchCmd
            )

        LoadMoreHistory entityType entityId ->
            let
                currentCount =
                    Dict.get entityId model.auditLog.entityHistory |> Maybe.map List.length |> Maybe.withDefault 0
            in
            ( model
            , Api.fetchEntityHistory model.flags.apiUrl entityType entityId (Just (currentCount + 20)) (GotEntityHistory entityId)
            )

        SetAuditFilter filterName filterValue ->
            let
                filters =
                    model.auditLog.filters

                updated =
                    case filterName of
                        "entityType" ->
                            { filters | entityType = if filterValue == "" then Nothing else Just filterValue }

                        "action" ->
                            { filters | action = if filterValue == "" then Nothing else Just filterValue }

                        "since" ->
                            { filters | since = if filterValue == "" then Nothing else Just filterValue }

                        "until" ->
                            { filters | until = if filterValue == "" then Nothing else Just filterValue }

                        _ ->
                            filters
            in
            ( updateAuditLogModel (\al -> { al | filters = updated, loading = False, loadingFilters = Nothing }) model, Cmd.none )

        ApplyAuditFilters ->
            let
                oldFilters =
                    model.auditLog.filters

                filters =
                    { oldFilters | offset = Nothing }
            in
            ( updateAuditLogModel (\al -> { al | entries = [], entryBaseOffset = filters.offset |> Maybe.withDefault 0, hasMore = False, loading = True, loadingFilters = Just filters, filters = filters }) model
            , Api.fetchAuditLog model.flags.apiUrl filters (GotAuditLog filters)
            )

        LoadMoreAuditLog ->
            if model.auditLog.loading then
                ( model, Cmd.none )

            else
                let
                    oldFilters =
                        model.auditLog.filters

                    filters =
                        { oldFilters | offset = Just (nextAuditOffset model.auditLog) }
                in
                ( updateAuditLogModel (\al -> { al | filters = filters, loading = True, loadingFilters = Just filters }) model
                , Api.fetchAuditLog model.flags.apiUrl filters (GotAuditLog filters)
                )

        ToggleAuditExpand entryId ->
            let
                current =
                    Dict.get entryId model.auditLog.expandedEntries |> Maybe.withDefault False
            in
            ( updateAuditLogModel (\al -> { al | expandedEntries = Dict.insert entryId (not current) al.expandedEntries }) model
            , Cmd.none
            )

        ConfirmRevert entry ->
            ( updateAuditLogModel (\al -> { al | revertConfirmation = Just entry }) model, Cmd.none )

        CancelRevert ->
            ( updateAuditLogModel (\al -> { al | revertConfirmation = Nothing }) model, Cmd.none )

        PerformRevert ->
            case model.auditLog.revertConfirmation of
                Just entry ->
                    let
                        ( trackedModel, requestId, clearCmd ) =
                            beginTrackedMutation [ entry.entityId ] (updateAuditLogModel (\al -> { al | revertInFlight = True }) model)
                    in
                    ( trackedModel
                    , Cmd.batch
                        [ clearCmd
                        , Api.revertAuditEntry model.flags.apiUrl entry.id requestId (GotRevertResult entry.entityType entry.entityId)
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotRevertResult entityType entityId result ->
            case result of
                Ok _ ->
                    let
                        refreshHistoryCmd =
                            Api.fetchEntityHistory model.flags.apiUrl entityType entityId Nothing (GotEntityHistory entityId)

                        refreshAuditFilters =
                            case model.page of
                                AuditLogPage ->
                                    let
                                        oldFilters =
                                            model.auditLog.filters

                                        filters =
                                            { oldFilters | offset = Nothing }
                                    in
                                    Just filters

                                _ ->
                                    Nothing

                        refreshAuditCmd =
                            case refreshAuditFilters of
                                Just filters ->
                                    Api.fetchAuditLog model.flags.apiUrl filters (GotAuditLog filters)

                                Nothing ->
                                    Cmd.none

                        clearAuditLog =
                            case model.page of
                                AuditLogPage ->
                                    True

                                _ ->
                                    False

                        ( toastModel, toastCmd ) =
                            addToast Success "Change reverted successfully"
                                (updateAuditLogModel
                                    (\al ->
                                        { al
                                            | revertConfirmation = Nothing
                                            , revertInFlight = False
                                            , entries =
                                                if clearAuditLog then
                                                    []

                                                else
                                                    al.entries
                                            , entryBaseOffset =
                                                if clearAuditLog then
                                                    refreshAuditFilters
                                                        |> Maybe.andThen .offset
                                                        |> Maybe.withDefault 0

                                                else
                                                    al.entryBaseOffset
                                            , loading =
                                                case refreshAuditFilters of
                                                    Just _ ->
                                                        True

                                                    Nothing ->
                                                        False
                                            , loadingFilters = refreshAuditFilters
                                            , filters =
                                                Maybe.withDefault al.filters refreshAuditFilters
                                        }
                                    )
                                    model
                                )
                    in
                    ( toastModel
                    , Cmd.batch [ toastCmd, refreshHistoryCmd, refreshAuditCmd ]
                    )

                Err _ ->
                    let
                        ( toastModel, toastCmd ) =
                            addToast Error "Failed to revert change" (updateAuditLogModel (\al -> { al | revertConfirmation = Nothing, revertInFlight = False }) model)
                    in
                    ( toastModel, toastCmd )

        _ ->
            ( model, Cmd.none )



-- AUDIT LOG PAGE


viewAuditLogPage : Model -> Html Msg
viewAuditLogPage model =
    if model.sessionContext == Nothing then
        div [ class "page audit-log-view" ]
            [ div [ class "page-header" ]
                [ h2 [] [ span [ class "page-header-icon icon-audit" ] [], text "Audit Log" ] ]
            , div [ class "loading-indicator" ] [ text "Loading audit permissions..." ]
            ]

    else if not (Permissions.canViewGlobalAudit model) then
        div [ class "page audit-log-view" ]
            [ div [ class "page-header" ]
                [ h2 [] [ span [ class "page-header-icon icon-audit" ] [], text "Audit Log" ] ]
            , div [ class "empty-state" ] [ text "Global audit log access requires superadmin permission." ]
            ]

    else
        div [ class "page audit-log-view" ]
            [ div [ class "page-header" ]
                [ h2 [] [ span [ class "page-header-icon icon-audit" ] [], text "Audit Log" ] ]
            , viewAuditLogFilters model
            , viewAuditEntries model "No audit log entries found."
            ]


viewWorkspaceAuditPanel : String -> Model -> Html Msg
viewWorkspaceAuditPanel wsId model =
    if model.sessionContext == Nothing then
        div [ class "audit-log-view" ]
            [ div [ class "loading-indicator" ] [ text "Loading audit permissions..." ] ]

    else if not (Permissions.canViewCurrentWorkspaceAudit model) then
        div [ class "audit-log-view" ]
            [ div [ class "empty-state" ]
                [ h3 [] [ text "Workspace audit unavailable" ]
                , p [] [ text "Workspace audit access requires workspace admin permission." ]
                ]
            ]

    else
        let
            filtersMatch =
                model.auditLog.filters.workspaceId == Just wsId
        in
        div [ class "audit-log-view workspace-audit-log" ]
            [ div [ class "section-header" ]
                [ h3 [] [ span [ class "section-header-icon icon-audit" ] [], text "Workspace audit" ]
                , p [ class "help-text" ] [ text "Recent changes recorded for this workspace." ]
                ]
            , viewAuditLogFilters model
            , if filtersMatch then
                viewAuditEntries model "No workspace audit entries found."

              else
                div [ class "loading-indicator" ] [ text "Loading workspace audit..." ]
            ]


viewAuditEntries : Model -> String -> Html Msg
viewAuditEntries model emptyMessage =
    if model.auditLog.loading && List.isEmpty model.auditLog.entries then
        div [ class "loading-indicator" ] [ text "Loading audit entries..." ]

    else if List.isEmpty model.auditLog.entries then
        div [ class "empty-state" ] [ text emptyMessage ]

    else
        div [ class "audit-log-list" ]
            (List.map (viewAuditLogEntry model) model.auditLog.entries
                ++ (if model.auditLog.hasMore && model.auditLog.loading then
                        [ div [ class "loading-indicator" ] [ text "Loading more audit entries..." ] ]

                    else if model.auditLog.hasMore then
                        [ button [ class "audit-log-load-more", onClick LoadMoreAuditLog ]
                            [ text "Load more..." ]
                        ]

                    else
                        []
                   )
            )


viewAuditLogFilters : Model -> Html Msg
viewAuditLogFilters model =
    let
        filters =
            model.auditLog.filters
    in
    div [ class "audit-log-filters" ]
        [ div [ class "audit-filter-group" ]
            [ label [] [ text "Entity type" ]
            , select [ onInput (SetAuditFilter "entityType") ]
                [ option [ value "", selected (filters.entityType == Nothing) ] [ text "All" ]
                , option [ value "workspace", selected (filters.entityType == Just "workspace") ] [ text "Workspace" ]
                , option [ value "project", selected (filters.entityType == Just "project") ] [ text "Project" ]
                , option [ value "task", selected (filters.entityType == Just "task") ] [ text "Task" ]
                , option [ value "memory", selected (filters.entityType == Just "memory") ] [ text "Memory" ]
                , option [ value "category", selected (filters.entityType == Just "category") ] [ text "Category" ]
                ]
            ]
        , div [ class "audit-filter-group" ]
            [ label [] [ text "Action" ]
            , select [ onInput (SetAuditFilter "action") ]
                [ option [ value "", selected (filters.action == Nothing) ] [ text "All" ]
                , option [ value "create", selected (filters.action == Just "create") ] [ text "Create" ]
                , option [ value "update", selected (filters.action == Just "update") ] [ text "Update" ]
                , option [ value "delete", selected (filters.action == Just "delete") ] [ text "Delete" ]
                ]
            ]
        , div [ class "audit-filter-group" ]
            [ label [] [ text "Since" ]
            , input [ type_ "date", value (Maybe.withDefault "" filters.since), onInput (SetAuditFilter "since") ] []
            ]
        , div [ class "audit-filter-group" ]
            [ label [] [ text "Until" ]
            , input [ type_ "date", value (Maybe.withDefault "" filters.until), onInput (SetAuditFilter "until") ] []
            ]
        , button [ class "btn btn-primary", onClick ApplyAuditFilters ] [ text "Apply" ]
        ]


resolveAuditNavigationTarget : Api.AuditLogEntry -> Maybe ( String, String )
resolveAuditNavigationTarget entry =
    let
        extractField field val =
            Decode.decodeValue (Decode.field field Decode.string) val |> Result.toMaybe

        fromValues field =
            let
                fromNew =
                    Maybe.andThen (extractField field) entry.newValues

                fromOld =
                    Maybe.andThen (extractField field) entry.oldValues
            in
            case fromNew of
                Just _ ->
                    fromNew

                Nothing ->
                    fromOld
    in
    case entry.entityType of
        "memory_link" ->
            fromValues "source_id" |> Maybe.map (\id -> ( "memory", id ))

        "memory_tag" ->
            fromValues "memory_id" |> Maybe.map (\id -> ( "memory", id ))

        "memory_category_link" ->
            fromValues "memory_id" |> Maybe.map (\id -> ( "memory", id ))

        "project_memory_link" ->
            fromValues "project_id" |> Maybe.map (\id -> ( "project", id ))

        "task_memory_link" ->
            fromValues "task_id" |> Maybe.map (\id -> ( "task", id ))

        "task_dependency" ->
            fromValues "task_id" |> Maybe.map (\id -> ( "task", id ))

        "workspace_group_member" ->
            Nothing

        _ ->
            Just ( entry.entityType, entry.entityId )


auditEntitySummary : Model -> Api.AuditLogEntry -> String
auditEntitySummary model entry =
    let
        decodeDict v =
            Decode.decodeValue (Decode.dict flexibleStringDecoder) v |> Result.toMaybe

        newDict =
            Maybe.andThen decodeDict entry.newValues

        oldDict =
            Maybe.andThen decodeDict entry.oldValues

        getField field =
            case Maybe.andThen (Dict.get field) newDict of
                Just v ->
                    Just v

                Nothing ->
                    Maybe.andThen (Dict.get field) oldDict

        short id =
            String.left 8 id

        lookupMemoryName id =
            Dict.get id model.memories |> Maybe.map (\m -> String.left 40 m.content) |> Maybe.withDefault (short id)

        lookupProjectName id =
            Dict.get id model.projects |> Maybe.map .name |> Maybe.withDefault (short id)

        lookupTaskName id =
            Dict.get id model.tasks |> Maybe.map .title |> Maybe.withDefault (short id)
    in
    case entry.entityType of
        "memory_link" ->
            let
                src =
                    getField "source_id" |> Maybe.map lookupMemoryName |> Maybe.withDefault "?"

                tgt =
                    getField "target_id" |> Maybe.map lookupMemoryName |> Maybe.withDefault "?"

                rel =
                    getField "relation_type" |> Maybe.withDefault "link"
            in
            src ++ " → " ++ tgt ++ " (" ++ rel ++ ")"

        "memory_tag" ->
            let
                tag =
                    getField "tag" |> Maybe.withDefault "?"

                mem =
                    getField "memory_id" |> Maybe.map lookupMemoryName |> Maybe.withDefault "?"
            in
            "Tag \"" ++ tag ++ "\" on " ++ mem

        "memory_category_link" ->
            let
                mem =
                    getField "memory_id" |> Maybe.map lookupMemoryName |> Maybe.withDefault "?"

                cat =
                    getField "category_id" |> Maybe.map short |> Maybe.withDefault "?"
            in
            mem ++ " ↔ Category " ++ cat

        "task_dependency" ->
            let
                task =
                    getField "task_id" |> Maybe.map lookupTaskName |> Maybe.withDefault "?"

                dep =
                    getField "depends_on_id" |> Maybe.map lookupTaskName |> Maybe.withDefault "?"
            in
            task ++ " → depends on " ++ dep

        "project_memory_link" ->
            let
                proj =
                    getField "project_id" |> Maybe.map lookupProjectName |> Maybe.withDefault "?"

                mem =
                    getField "memory_id" |> Maybe.map lookupMemoryName |> Maybe.withDefault "?"
            in
            proj ++ " ↔ " ++ mem

        "task_memory_link" ->
            let
                task =
                    getField "task_id" |> Maybe.map lookupTaskName |> Maybe.withDefault "?"

                mem =
                    getField "memory_id" |> Maybe.map lookupMemoryName |> Maybe.withDefault "?"
            in
            task ++ " ↔ " ++ mem

        _ ->
            getField "name"
                |> Maybe.withDefault
                    (getField "title"
                        |> Maybe.withDefault
                            (getField "content"
                                |> Maybe.map (String.left 60)
                                |> Maybe.withDefault (short entry.entityId)
                            )
                    )


type alias AuditFieldChange =
    { field : String
    , label : String
    , oldValue : Maybe String
    , newValue : Maybe String
    }


auditContextDetailItems : Api.AuditLogEntry -> List ( String, String )
auditContextDetailItems entry =
    [ ( "Actor", auditActorSummary entry )
    , ( "Action type", auditActionToDisplay entry.action )
    , ( "Target entity", auditEntityTypeLabel entry.entityType )
    , ( "Target ID", entry.entityId )
    , ( "Timestamp", entry.changedAt )
    , ( "Audit entry ID", entry.id )
    ]
        ++ maybeDetail "Actor type" entry.actorType
        ++ maybeDetail "Actor ID" entry.actorId
        ++ maybeDetail "Workspace ID" entry.workspaceId
        ++ maybeDetail "Request ID" entry.requestId


auditActionDetailItems : Api.AuditLogEntry -> List ( String, String )
auditActionDetailItems entry =
    ( "Operation", auditOperationLabel entry )
        :: (auditStatusChangeDetailItems entry
                ++ auditFieldUpdateDetailItems entry
                ++ auditRelationshipDetailItems entry
                ++ auditSnapshotDetailItems entry
           )


auditChangedFieldItems : Api.AuditLogEntry -> List AuditFieldChange
auditChangedFieldItems entry =
    auditChangedFieldItemsFromValues entry.oldValues entry.newValues


auditNonStatusFieldChanges : Api.AuditLogEntry -> List AuditFieldChange
auditNonStatusFieldChanges entry =
    let
        statusOnlyFields =
            case auditStatusChange entry of
                Just _ ->
                    [ "status", "completed_at" ]

                Nothing ->
                    [ "status" ]
    in
    auditChangedFieldItems entry
        |> List.filter (\change -> not (List.member change.field statusOnlyFields))


maybeDetail : String -> Maybe String -> List ( String, String )
maybeDetail label mValue =
    case mValue of
        Just value ->
            if String.isEmpty value then
                []

            else
                [ ( label, value ) ]

        Nothing ->
            []


auditActionToDisplay : Api.AuditAction -> String
auditActionToDisplay action =
    case action of
        Api.AuditCreate ->
            "Create"

        Api.AuditUpdate ->
            "Update"

        Api.AuditDelete ->
            "Delete"


pastTenseAuditAction : Api.AuditAction -> String
pastTenseAuditAction action =
    case action of
        Api.AuditCreate ->
            "Created"

        Api.AuditUpdate ->
            "Updated"

        Api.AuditDelete ->
            "Deleted"


auditEntityTypeLabel : String -> String
auditEntityTypeLabel entityType =
    case entityType of
        "memory_link" ->
            "memory link"

        "memory_tag" ->
            "memory tag"

        "memory_category" ->
            "memory category"

        "memory_category_link" ->
            "memory category link"

        "project_memory_link" ->
            "project memory link"

        "task_memory_link" ->
            "task memory link"

        "task_dependency" ->
            "task dependency"

        "workspace_group" ->
            "workspace group"

        "workspace_group_member" ->
            "workspace group member"

        "cleanup_policy" ->
            "cleanup policy"

        other ->
            String.replace "_" " " other


auditOperationLabel : Api.AuditLogEntry -> String
auditOperationLabel entry =
    case auditStatusChangeLabel entry of
        Just label ->
            label

        Nothing ->
            case auditFieldUpdateLabel entry of
                Just label ->
                    label

                Nothing ->
                    case ( entry.entityType, entry.action ) of
                        ( "task_dependency", Api.AuditCreate ) ->
                            "Added task dependency"

                        ( "task_dependency", Api.AuditDelete ) ->
                            "Removed task dependency"

                        ( "project_memory_link", Api.AuditCreate ) ->
                            "Linked memory to project"

                        ( "project_memory_link", Api.AuditDelete ) ->
                            "Unlinked memory from project"

                        ( "task_memory_link", Api.AuditCreate ) ->
                            "Linked memory to task"

                        ( "task_memory_link", Api.AuditDelete ) ->
                            "Unlinked memory from task"

                        ( "memory_link", Api.AuditCreate ) ->
                            "Linked memories"

                        ( "memory_link", Api.AuditDelete ) ->
                            "Unlinked memories"

                        ( "memory_tag", Api.AuditCreate ) ->
                            "Added memory tag"

                        ( "memory_tag", Api.AuditDelete ) ->
                            "Removed memory tag"

                        ( "memory_category_link", Api.AuditCreate ) ->
                            "Linked memory to category"

                        ( "memory_category_link", Api.AuditDelete ) ->
                            "Unlinked memory from category"

                        ( "workspace_group_member", Api.AuditCreate ) ->
                            "Added workspace to group"

                        ( "workspace_group_member", Api.AuditDelete ) ->
                            "Removed workspace from group"

                        _ ->
                            pastTenseAuditAction entry.action ++ " " ++ auditEntityTypeLabel entry.entityType


auditFieldUpdateLabel : Api.AuditLogEntry -> Maybe String
auditFieldUpdateLabel entry =
    case ( entry.action, auditNonStatusFieldChanges entry ) of
        ( Api.AuditUpdate, _ :: _ ) ->
            Just ("Updated " ++ auditUpdateEntityLabel entry ++ " fields")

        _ ->
            Nothing


auditStatusChangeLabel : Api.AuditLogEntry -> Maybe String
auditStatusChangeLabel entry =
    case auditStatusChange entry of
        Just _ ->
            let
                baseLabel =
                    case ( entry.entityType, entry.action, auditNewValueFor "status" entry ) of
                        ( "project", Api.AuditUpdate, Just "archived" ) ->
                            "Archived project"

                        ( "project", Api.AuditUpdate, Just "completed" ) ->
                            "Completed project"

                        ( "project", Api.AuditUpdate, _ ) ->
                            "Changed project status"

                        ( "task", Api.AuditUpdate, _ ) ->
                            "Changed " ++ auditUpdateEntityLabel entry ++ " status"

                        _ ->
                            "Changed " ++ auditEntityTypeLabel entry.entityType ++ " status"
            in
            if List.isEmpty (auditNonStatusFieldChanges entry) then
                Just baseLabel

            else
                Just (baseLabel ++ " and fields")

        Nothing ->
            Nothing


auditUpdateEntityLabel : Api.AuditLogEntry -> String
auditUpdateEntityLabel entry =
    if entry.entityType == "task" && auditEntryIsSubtask entry then
        "subtask"

    else
        auditEntityTypeLabel entry.entityType


auditEntryIsSubtask : Api.AuditLogEntry -> Bool
auditEntryIsSubtask entry =
    case auditValueFor "parent_id" entry of
        Just parentId ->
            not (List.member parentId [ "", "(unset)", "null" ])

        Nothing ->
            False


auditFieldUpdateDetailItems : Api.AuditLogEntry -> List ( String, String )
auditFieldUpdateDetailItems entry =
    let
        fieldLabels =
            auditNonStatusFieldChanges entry
                |> List.map .label
    in
    case ( entry.action, fieldLabels ) of
        ( Api.AuditUpdate, _ :: _ ) ->
            [ ( "Field updates", String.join ", " fieldLabels ) ]

        _ ->
            []


auditStatusChangeDetailItems : Api.AuditLogEntry -> List ( String, String )
auditStatusChangeDetailItems entry =
    case auditStatusChange entry of
        Just ( oldStatus, newStatus ) ->
            [ ( "Status change", oldStatus ++ " → " ++ newStatus ) ]

        Nothing ->
            []


auditStatusChange : Api.AuditLogEntry -> Maybe ( String, String )
auditStatusChange entry =
    case ( auditOldValueFor "status" entry, auditNewValueFor "status" entry ) of
        ( Just oldStatus, Just newStatus ) ->
            if oldStatus == newStatus then
                Nothing

            else
                Just ( oldStatus, newStatus )

        _ ->
            Nothing


auditRelationshipDetailItems : Api.AuditLogEntry -> List ( String, String )
auditRelationshipDetailItems entry =
    case entry.entityType of
        "memory_link" ->
            auditFieldsAsDetails entry [ "source_id", "target_id", "relation_type", "strength" ]

        "memory_tag" ->
            auditFieldsAsDetails entry [ "memory_id", "tag" ]

        "memory_category_link" ->
            auditFieldsAsDetails entry [ "memory_id", "category_id" ]

        "project_memory_link" ->
            auditFieldsAsDetails entry [ "project_id", "memory_id" ]

        "task_memory_link" ->
            auditFieldsAsDetails entry [ "task_id", "memory_id" ]

        "task_dependency" ->
            auditFieldsAsDetails entry [ "task_id", "depends_on_id" ]

        "workspace_group_member" ->
            auditFieldsAsDetails entry [ "group_id", "workspace_id" ]

        _ ->
            []


auditFieldsAsDetails : Api.AuditLogEntry -> List String -> List ( String, String )
auditFieldsAsDetails entry fields =
    fields
        |> List.filterMap (\field -> auditValueFor field entry |> Maybe.map (\value -> ( auditFieldLabel field, value )))


auditSnapshotDetailItems : Api.AuditLogEntry -> List ( String, String )
auditSnapshotDetailItems entry =
    let
        relationshipFields =
            auditRelationshipDetailItems entry |> List.map Tuple.first

        includeField field =
            not (List.member (auditFieldLabel field) relationshipFields)
    in
    case auditSnapshotValue entry of
        Just value ->
            auditSnapshotDetailItemsFromValue value
                |> List.filter (\( label, _ ) -> includeField (auditFieldFromLabel label))

        Nothing ->
            []


auditSnapshotValue : Api.AuditLogEntry -> Maybe Decode.Value
auditSnapshotValue entry =
    case entry.action of
        Api.AuditDelete ->
            entry.oldValues

        _ ->
            entry.newValues


auditOldValueFor : String -> Api.AuditLogEntry -> Maybe String
auditOldValueFor field entry =
    Maybe.andThen (auditDisplayField field) entry.oldValues


auditNewValueFor : String -> Api.AuditLogEntry -> Maybe String
auditNewValueFor field entry =
    Maybe.andThen (auditDisplayField field) entry.newValues


auditValueFor : String -> Api.AuditLogEntry -> Maybe String
auditValueFor field entry =
    case auditNewValueFor field entry of
        Just value ->
            Just value

        Nothing ->
            auditOldValueFor field entry


auditDisplayField : String -> Decode.Value -> Maybe String
auditDisplayField field value =
    if auditFieldHidden field then
        Nothing

    else
        case Decode.decodeValue (Decode.dict Decode.value) value of
            Ok dict ->
                Dict.get field dict |> Maybe.map auditDisplayValue

            Err _ ->
                Nothing


auditSnapshotDetailItemsFromValue : Decode.Value -> List ( String, String )
auditSnapshotDetailItemsFromValue value =
    case Decode.decodeValue (Decode.dict Decode.value) value of
        Ok dict ->
            orderedAuditFields dict
                |> List.filter (not << auditFieldHidden)
                |> List.filterMap (\field -> Dict.get field dict |> Maybe.map (\fieldValue -> ( auditFieldLabel field, auditDisplayValue fieldValue )))
                |> List.take 8

        Err _ ->
            []


orderedAuditFields : Dict.Dict String Decode.Value -> List String
orderedAuditFields dict =
    let
        keys =
            Dict.keys dict

        priorityFields =
            [ "name", "title", "status", "priority", "description", "content", "summary", "memory_type", "pinned", "tag", "relation_type", "strength", "project_id", "task_id", "memory_id", "category_id", "source_id", "target_id", "depends_on_id", "parent_id", "due_at", "completed_at" ]

        prioritized =
            List.filter (\field -> List.member field keys) priorityFields

        remaining =
            List.filter (\field -> not (List.member field prioritized)) keys
    in
    prioritized ++ remaining


auditChangedFieldItemsFromValues : Maybe Decode.Value -> Maybe Decode.Value -> List AuditFieldChange
auditChangedFieldItemsFromValues mOld mNew =
    case ( mOld, mNew ) of
        ( Just oldVal, Just newVal ) ->
            case ( Decode.decodeValue (Decode.dict Decode.value) oldVal, Decode.decodeValue (Decode.dict Decode.value) newVal ) of
                ( Ok oldDict, Ok newDict ) ->
                    let
                        changedKeys =
                            Dict.merge
                                (\k v acc -> auditChangeFromValues k (Just v) Nothing :: acc)
                                (\k ov nv acc ->
                                    if Encode.encode 0 ov /= Encode.encode 0 nv then
                                        auditChangeFromValues k (Just ov) (Just nv) :: acc

                                    else
                                        acc
                                )
                                (\k v acc -> auditChangeFromValues k Nothing (Just v) :: acc)
                                oldDict
                                newDict
                                []
                                |> List.filterMap identity
                                |> List.reverse
                    in
                    changedKeys

                _ ->
                    []

        _ ->
            []


auditChangeFromValues : String -> Maybe Decode.Value -> Maybe Decode.Value -> Maybe AuditFieldChange
auditChangeFromValues field mOld mNew =
    if auditFieldHidden field then
        Nothing

    else
        Just
            { field = field
            , label = auditFieldLabel field
            , oldValue = Maybe.map auditDisplayValue mOld
            , newValue = Maybe.map auditDisplayValue mNew
            }


auditDisplayValue : Decode.Value -> String
auditDisplayValue value =
    let
        rawValue =
            case Decode.decodeValue flexibleStringDecoder value of
                Ok raw ->
                    raw

                Err _ ->
                    Encode.encode 0 value
    in
    if rawValue == "null" then
        "(unset)"

    else if String.isEmpty rawValue then
        "(empty)"

    else
        truncateAuditValue 160 rawValue


truncateAuditValue : Int -> String -> String
truncateAuditValue maxLength value =
    if String.length value > maxLength then
        String.left maxLength value ++ "…"

    else
        value


auditFieldHidden : String -> Bool
auditFieldHidden field =
    let
        lower =
            String.toLower field
    in
    List.member lower
        [ "id"
        , "workspace_id"
        , "created_at"
        , "updated_at"
        , "deleted_at"
        , "search_vector"
        , "metadata"
        , "token_hash"
        , "last_used_at"
        , "session_hash"
        , "csrf_token_hash"
        , "password"
        , "client_secret"
        , "token_hash_secret"
        , "authorization_code"
        , "access_token"
        , "refresh_token"
        ]
        || String.contains "secret" lower
        || String.contains "token_hash" lower
        || String.contains "access_token" lower
        || String.contains "refresh_token" lower


auditFieldLabel : String -> String
auditFieldLabel field =
    case field of
        "actor_id" ->
            "Actor ID"

        "actor_label" ->
            "Actor label"

        "actor_type" ->
            "Actor type"

        "category_id" ->
            "Category ID"

        "completed_at" ->
            "Completed at"

        "depends_on_id" ->
            "Depends on task ID"

        "due_at" ->
            "Due at"

        "entity_id" ->
            "Entity ID"

        "group_id" ->
            "Group ID"

        "memory_id" ->
            "Memory ID"

        "memory_type" ->
            "Memory type"

        "name" ->
            "Name"

        "parent_id" ->
            "Parent ID"

        "project_id" ->
            "Project ID"

        "priority" ->
            "Priority"

        "relation_type" ->
            "Relationship"

        "request_id" ->
            "Request ID"

        "source_id" ->
            "Source memory ID"

        "target_id" ->
            "Target memory ID"

        "status" ->
            "Status"

        "summary" ->
            "Summary"

        "task_id" ->
            "Task ID"

        "title" ->
            "Title"

        "workspace_id" ->
            "Workspace ID"

        other ->
            String.replace "_" " " other


auditFieldFromLabel : String -> String
auditFieldFromLabel label =
    case label of
        "Actor ID" ->
            "actor_id"

        "Category ID" ->
            "category_id"

        "Completed at" ->
            "completed_at"

        "Depends on task ID" ->
            "depends_on_id"

        "Due at" ->
            "due_at"

        "Entity ID" ->
            "entity_id"

        "Group ID" ->
            "group_id"

        "Memory ID" ->
            "memory_id"

        "Memory type" ->
            "memory_type"

        "Parent ID" ->
            "parent_id"

        "Project ID" ->
            "project_id"

        "Relationship" ->
            "relation_type"

        "Request ID" ->
            "request_id"

        "Source memory ID" ->
            "source_id"

        "Target memory ID" ->
            "target_id"

        "Task ID" ->
            "task_id"

        "Workspace ID" ->
            "workspace_id"

        other ->
            String.replace " " "_" other


isRevertableEntityType : String -> Bool
isRevertableEntityType entityType =
    List.member entityType [ "memory", "project", "task", "memory_category" ]


viewAuditLogEntry : Model -> Api.AuditLogEntry -> Html Msg
viewAuditLogEntry model entry =
    let
        actionLabel =
            case entry.action of
                Api.AuditCreate ->
                    "Created"

                Api.AuditUpdate ->
                    "Updated"

                Api.AuditDelete ->
                    "Deleted"

        actionClass =
            "audit-action-" ++ Api.auditActionToString entry.action

        expanded =
            Dict.get entry.id model.auditLog.expandedEntries |> Maybe.withDefault False

        entitySummary =
            auditEntitySummary model entry

        navigable =
            resolveAuditNavigationTarget entry /= Nothing

        summaryAttrs =
            if navigable then
                [ class "audit-entity-summary"
                , stopPropagationOn "click" (Decode.succeed ( NavigateToAuditEntity entry, True ))
                , title ("Go to " ++ entry.entityType)
                ]

            else
                [ class "audit-entity-summary audit-entity-no-nav"
                , title entry.entityType
                ]
    in
    div [ class "audit-entry" ]
        [ div [ class "audit-entry-row", onClick (ToggleAuditExpand entry.id) ]
            [ span [ class "audit-expand-icon" ]
                [ text
                    (if expanded then
                        "▾"

                     else
                        "▸"
                    )
                ]
            , span [ class ("audit-action-badge " ++ actionClass) ] [ text actionLabel ]
            , span [ class "audit-entity-type" ] [ text entry.entityType ]
            , span summaryAttrs
                [ text entitySummary ]
            , span [ class "audit-timestamp" ] [ text (formatDate entry.changedAt) ]
            ]
        , if expanded then
            div [ class "audit-entry-detail" ]
                [ viewAuditDetailSection "Audit context" (auditContextDetailItemsForView model entry)
                , viewAuditDetailSection "Action details" (auditActionDetailItems entry)
                , viewAuditEntryChangeDetails entry
                , div [ class "audit-entry-meta" ]
                    [ span [ class "audit-entry-id" ] [ text ("Entry: " ++ String.left 8 entry.id) ]
                    , span [ class "audit-entity-id" ] [ text ("Entity: " ++ String.left 8 entry.entityId) ]
                    , span [ class "audit-actor" ] [ text ("Actor: " ++ auditActorSummary entry) ]
                    , if isRevertableEntityType entry.entityType && Permissions.canViewGlobalAudit model && model.page == AuditLogPage then
                        button [ class "btn-revert", onClick (ConfirmRevert entry), title "Revert this change" ] [ text "↩ Revert" ]

                      else
                        text ""
                    ]
                ]

          else
            text ""
        ]


viewAuditEntryChangeDetails : Api.AuditLogEntry -> Html Msg
viewAuditEntryChangeDetails entry =
    case entry.action of
        Api.AuditUpdate ->
            viewChangedFields entry.oldValues entry.newValues

        Api.AuditCreate ->
            text ""

        Api.AuditDelete ->
            text ""


viewAuditDetailSection : String -> List ( String, String ) -> Html Msg
viewAuditDetailSection titleText rows =
    if List.isEmpty rows then
        text ""

    else
        div [ class "audit-detail-section" ]
            [ h4 [ class "audit-detail-heading" ] [ text titleText ]
            , dl [ class "audit-detail-grid" ]
                (List.concatMap viewAuditDetailItem rows)
            ]


viewAuditDetailItem : ( String, String ) -> List (Html Msg)
viewAuditDetailItem ( labelText, valueText ) =
    [ dt [ class "audit-detail-label" ] [ text labelText ]
    , dd [ class "audit-detail-value" ] [ text valueText ]
    ]


auditContextDetailItemsForView : Model -> Api.AuditLogEntry -> List ( String, String )
auditContextDetailItemsForView model entry =
    auditContextDetailItems entry
        |> List.map
            (\( label, valueText ) ->
                if label == "Target entity" then
                    ( label, valueText ++ " · " ++ auditEntitySummary model entry )

                else
                    ( label, valueText )
            )


auditActorSummary : Api.AuditLogEntry -> String
auditActorSummary entry =
    case entry.actorLabel of
        Just label ->
            label

        Nothing ->
            case entry.actorId of
                Just actorId ->
                    String.left 12 actorId

                Nothing ->
                    "unknown"



-- ENTITY HISTORY


viewEntityHistory : Model -> String -> String -> Html Msg
viewEntityHistory model entityType entityId =
    let
        expanded =
            Dict.get entityId model.auditLog.historyExpanded |> Maybe.withDefault False
    in
    div [ class "entity-history" ]
        [ button [ class "entity-history-toggle", onClick (ToggleEntityHistory entityType entityId) ]
            [ text
                (if expanded then
                    "▾ History"

                 else
                    "▸ History"
                )
            ]
        , if expanded then
            case Dict.get entityId model.auditLog.entityHistory of
                Just entries ->
                    div [ class "entity-history-timeline" ]
                        (List.map (viewHistoryEntry model) entries
                            ++ (if Dict.get entityId model.auditLog.entityHistoryHasMore |> Maybe.withDefault False then
                                    [ button [ class "entity-history-load-more", onClick (LoadMoreHistory entityType entityId) ]
                                        [ text "Load more..." ]
                                    ]

                                else
                                    []
                               )
                            ++ (if List.isEmpty entries then
                                    [ div [ class "entity-history-empty" ] [ text "No history entries" ] ]

                                else
                                    []
                               )
                        )

                Nothing ->
                    div [ class "entity-history-timeline" ]
                        [ div [ class "entity-history-loading" ] [ text "Loading..." ] ]

          else
            text ""
        ]


viewHistoryEntry : Model -> Api.AuditLogEntry -> Html Msg
viewHistoryEntry model entry =
    let
        actionLabel =
            case entry.action of
                Api.AuditCreate ->
                    "Created"

                Api.AuditUpdate ->
                    "Updated"

                Api.AuditDelete ->
                    "Deleted"

        actionClass =
            "history-action-" ++ Api.auditActionToString entry.action

        changedFields =
            case entry.action of
                Api.AuditUpdate ->
                    viewChangedFields entry.oldValues entry.newValues

                Api.AuditCreate ->
                    case entry.newValues of
                        Just nv ->
                            viewJsonSummary "Initial" nv

                        Nothing ->
                            text ""

                Api.AuditDelete ->
                    text ""
    in
    div [ class "history-entry" ]
        [ div [ class "history-entry-header" ]
            [ span [ class ("history-action-badge " ++ actionClass) ] [ text actionLabel ]
            , span [ class "history-timestamp" ] [ text (formatDate entry.changedAt) ]
            , if isRevertableEntityType entry.entityType && Permissions.canViewGlobalAudit model && model.page == AuditLogPage then
                button [ class "btn-revert", onClick (ConfirmRevert entry), title "Revert this change" ] [ text "↩" ]

              else
                text ""
            ]
        , changedFields
        ]


viewChangedFields : Maybe Decode.Value -> Maybe Decode.Value -> Html Msg
viewChangedFields mOld mNew =
    let
        changes =
            auditChangedFieldItemsFromValues mOld mNew
    in
    if List.isEmpty changes then
        text ""

    else
        div [ class "history-diff audit-changed-fields" ]
            (h4 [ class "audit-detail-heading" ] [ text "Changed fields" ]
                :: List.map viewChangedField changes
            )


viewChangedField : AuditFieldChange -> Html Msg
viewChangedField change =
    div [ class "history-diff-field" ]
        [ span [ class "history-diff-field-name" ] [ text change.label ]
        , case ( change.oldValue, change.newValue ) of
            ( Just oldValue, Just newValue ) ->
                span []
                    [ span [ class "history-diff-old" ] [ text oldValue ]
                    , text " → "
                    , span [ class "history-diff-new" ] [ text newValue ]
                    ]

            ( Nothing, Just newValue ) ->
                span [ class "history-diff-new" ] [ text newValue ]

            ( Just oldValue, Nothing ) ->
                span [ class "history-diff-old" ] [ text oldValue ]

            ( Nothing, Nothing ) ->
                text ""
        ]


viewJsonSummary : String -> Decode.Value -> Html Msg
viewJsonSummary titleText val =
    viewAuditDetailSection titleText (auditSnapshotDetailItemsFromValue val)


viewRevertConfirmModal : Model -> Html Msg
viewRevertConfirmModal model =
    case model.auditLog.revertConfirmation of
        Nothing ->
            text ""

        Just entry ->
            let
                ( titleText, description ) =
                    case entry.action of
                        Api.AuditCreate ->
                            ( "Delete this " ++ entry.entityType ++ "?"
                            , "This will undo the creation by deleting the " ++ entry.entityType ++ "."
                            )

                        Api.AuditDelete ->
                            ( "Restore this " ++ entry.entityType ++ "?"
                            , "This will restore the previously deleted " ++ entry.entityType ++ "."
                            )

                        Api.AuditUpdate ->
                            let
                                fieldList =
                                    auditChangedFieldItems entry |> List.map .label

                                fieldStr =
                                    if List.isEmpty fieldList then
                                        "fields"

                                    else
                                        String.join ", " fieldList
                            in
                            ( "Revert this change?"
                            , "This will restore " ++ fieldStr ++ " to their previous values."
                            )
            in
            div [ class "modal-overlay", onClick CancelRevert ]
                [ div [ class "modal revert-confirm-modal", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                    [ h3 [ class "modal-title" ] [ text titleText ]
                    , p [ class "revert-confirm-desc" ] [ text description ]
                    , div [ class "modal-actions" ]
                        [ button
                            [ class "btn btn-primary"
                            , onClick PerformRevert
                            , disabled model.auditLog.revertInFlight
                            ]
                            [ text
                                (if model.auditLog.revertInFlight then
                                    "Reverting..."

                                 else
                                    "Revert"
                                 )
                            ]
                        , button [ class "btn btn-secondary", onClick CancelRevert ] [ text "Cancel" ]
                        ]
                    ]
                ]


auditReturnSource : Model -> FocusReturnSource
auditReturnSource model =
    case model.page of
        AuditLogPage ->
            ReturnFromGlobalAudit

        WorkspacePage _ ->
            ReturnFromWorkspaceAudit

        _ ->
            ReturnFromWorkspaceAudit


nextAuditOffset : AuditLogModel -> Int
nextAuditOffset auditLog =
    auditLog.entryBaseOffset + List.length auditLog.entries


auditReturnFilters : String -> AuditLogModel -> AuditLogFilters
auditReturnFilters entryId auditLog =
    let
        pageSize =
            50

        baseOffset =
            auditLog.entryBaseOffset

        entryIndex =
            auditLog.entries
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, entry ) -> entry.id == entryId)
                |> List.head
                |> Maybe.map Tuple.first

        pageOffset =
            entryIndex
                |> Maybe.map (\idx -> ((baseOffset + idx) // pageSize) * pageSize)
                |> Maybe.withDefault baseOffset

        currentFilters =
            auditLog.filters
    in
    { currentFilters
        | offset =
            if pageOffset <= 0 then
                Nothing

            else
                Just pageOffset
        , limit = Just pageSize
    }


updateAuditLogModel : (AuditLogModel -> AuditLogModel) -> Model -> Model
updateAuditLogModel fn model =
    { model | auditLog = fn model.auditLog }


updateFocusModel : (FocusModel -> FocusModel) -> Model -> Model
updateFocusModel fn model =
    { model | focus = fn model.focus }
