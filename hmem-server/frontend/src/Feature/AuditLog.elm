module Feature.AuditLog exposing (update, viewAuditLogPage, viewEntityHistory, viewRevertConfirmModal)

import Api
import Browser.Navigation as Nav
import Dict
import Helpers exposing (beginTrackedMutation, buildFragment, flexibleStringDecoder, formatDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Toast exposing (addToast)
import Types exposing (..)


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
                    orElseMaybe
                        (Maybe.andThen extractWsId auditEntry.oldValues)
                        (Maybe.andThen extractWsId auditEntry.newValues)

                resolvedTarget =
                    resolveAuditNavigationTarget auditEntry
            in
            case resolvedTarget of
                Nothing ->
                    addToast Warning "Cannot navigate to this entity type" model

                Just ( targetType, targetId ) ->
                    case mWorkspaceId of
                        Just wsId ->
                            let
                                focusEntry =
                                    ( targetType, targetId )

                                focusModel =
                                    model.focus

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
                                    }
                                )
                                { model | selectedWorkspaceId = Just wsId }
                            , Nav.pushUrl model.key ("/workspace/" ++ wsId ++ "#" ++ buildFragment model.activeTab (Just focusEntry))
                            )

                        Nothing ->
                            addToast Warning "Cannot navigate: entity workspace unknown" model

        GotAuditLog result ->
            case result of
                Ok paginated ->
                    ( updateAuditLogModel (\al -> { al | entries = al.entries ++ paginated.items, hasMore = paginated.hasMore }) model
                    , Cmd.none
                    )

                Err _ ->
                    addToast Error "Failed to load audit log" model

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
            ( updateAuditLogModel (\al -> { al | filters = updated }) model, Cmd.none )

        ApplyAuditFilters ->
            let
                oldFilters =
                    model.auditLog.filters

                filters =
                    { oldFilters | offset = Nothing }
            in
            ( updateAuditLogModel (\al -> { al | entries = [], hasMore = False, filters = filters }) model
            , Api.fetchAuditLog model.flags.apiUrl filters GotAuditLog
            )

        LoadMoreAuditLog ->
            let
                oldFilters =
                    model.auditLog.filters

                filters =
                    { oldFilters | offset = Just (List.length model.auditLog.entries) }
            in
            ( updateAuditLogModel (\al -> { al | filters = filters }) model
            , Api.fetchAuditLog model.flags.apiUrl filters GotAuditLog
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

                        refreshAuditCmd =
                            case model.page of
                                AuditLogPage ->
                                    let
                                        oldFilters =
                                            model.auditLog.filters

                                        filters =
                                            { oldFilters | offset = Nothing }
                                    in
                                    Api.fetchAuditLog model.flags.apiUrl filters GotAuditLog

                                _ ->
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
    div [ class "page audit-log-view" ]
        [ div [ class "page-header" ]
            [ h2 [] [ span [ class "page-header-icon icon-audit" ] [], text "Audit Log" ] ]
        , viewAuditLogFilters model
        , if List.isEmpty model.auditLog.entries then
            div [ class "empty-state" ] [ text "No audit log entries found." ]

          else
            div [ class "audit-log-list" ]
                (List.map (viewAuditLogEntry model) model.auditLog.entries
                    ++ (if model.auditLog.hasMore then
                            [ button [ class "audit-log-load-more", onClick LoadMoreAuditLog ]
                                [ text "Load more..." ]
                            ]

                        else
                            []
                       )
                )
        ]


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
                [ case entry.action of
                    Api.AuditUpdate ->
                        viewChangedFields entry.oldValues entry.newValues

                    Api.AuditCreate ->
                        case entry.newValues of
                            Just nv ->
                                viewJsonSummary "Initial" nv

                            Nothing ->
                                text ""

                    Api.AuditDelete ->
                        case entry.oldValues of
                            Just ov ->
                                viewJsonSummary "Deleted" ov

                            Nothing ->
                                text ""
                , div [ class "audit-entry-meta" ]
                    [ span [ class "audit-entry-id" ] [ text ("Entry: " ++ String.left 8 entry.id) ]
                    , span [ class "audit-entity-id" ] [ text ("Entity: " ++ String.left 8 entry.entityId) ]
                    , if isRevertableEntityType entry.entityType then
                        button [ class "btn-revert", onClick (ConfirmRevert entry), title "Revert this change" ] [ text "↩ Revert" ]

                      else
                        text ""
                    ]
                ]

          else
            text ""
        ]



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
                        (List.map viewHistoryEntry entries
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


viewHistoryEntry : Api.AuditLogEntry -> Html Msg
viewHistoryEntry entry =
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
            , button [ class "btn-revert", onClick (ConfirmRevert entry), title "Revert this change" ] [ text "↩" ]
            ]
        , changedFields
        ]


viewChangedFields : Maybe Decode.Value -> Maybe Decode.Value -> Html Msg
viewChangedFields mOld mNew =
    case ( mOld, mNew ) of
        ( Just oldVal, Just newVal ) ->
            case ( Decode.decodeValue (Decode.dict flexibleStringDecoder) oldVal, Decode.decodeValue (Decode.dict flexibleStringDecoder) newVal ) of
                ( Ok oldDict, Ok newDict ) ->
                    let
                        changedKeys =
                            Dict.merge
                                (\k v acc -> ( k, Just v, Nothing ) :: acc)
                                (\k ov nv acc ->
                                    if ov /= nv then
                                        ( k, Just ov, Just nv ) :: acc

                                    else
                                        acc
                                )
                                (\k v acc -> ( k, Nothing, Just v ) :: acc)
                                oldDict
                                newDict
                                []
                                |> List.reverse
                    in
                    if List.isEmpty changedKeys then
                        text ""

                    else
                        div [ class "history-diff" ]
                            (List.map
                                (\( field, mOldV, mNewV ) ->
                                    div [ class "history-diff-field" ]
                                        [ span [ class "history-diff-field-name" ] [ text field ]
                                        , case ( mOldV, mNewV ) of
                                            ( Just ov, Just nv ) ->
                                                span []
                                                    [ span [ class "history-diff-old" ] [ text ov ]
                                                    , text " → "
                                                    , span [ class "history-diff-new" ] [ text nv ]
                                                    ]

                                            ( Nothing, Just nv ) ->
                                                span [ class "history-diff-new" ] [ text nv ]

                                            ( Just ov, Nothing ) ->
                                                span [ class "history-diff-old" ] [ text ov ]

                                            ( Nothing, Nothing ) ->
                                                text ""
                                        ]
                                )
                                changedKeys
                            )

                _ ->
                    div [ class "history-diff" ]
                        [ div [ class "history-diff-field" ] [ text "Fields changed" ] ]

        _ ->
            text ""


viewJsonSummary : String -> Decode.Value -> Html Msg
viewJsonSummary _ val =
    case Decode.decodeValue (Decode.dict flexibleStringDecoder) val of
        Ok dict ->
            let
                items =
                    Dict.toList dict
                        |> List.filter (\( k, _ ) -> not (List.member k [ "id", "workspace_id", "created_at", "updated_at", "deleted_at" ]))
                        |> List.take 5
            in
            if List.isEmpty items then
                text ""

            else
                div [ class "history-diff" ]
                    (List.map
                        (\( field, v ) ->
                            div [ class "history-diff-field" ]
                                [ span [ class "history-diff-field-name" ] [ text field ]
                                , span [ class "history-diff-new" ] [ text v ]
                                ]
                        )
                        items
                    )

        _ ->
            text ""


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
                                    case ( entry.oldValues, entry.newValues ) of
                                        ( Just oldVal, Just newVal ) ->
                                            case ( Decode.decodeValue (Decode.dict flexibleStringDecoder) oldVal, Decode.decodeValue (Decode.dict flexibleStringDecoder) newVal ) of
                                                ( Ok oldDict, Ok newDict ) ->
                                                    Dict.merge
                                                        (\k _ acc -> k :: acc)
                                                        (\k ov nv acc ->
                                                            if ov /= nv then
                                                                k :: acc

                                                            else
                                                                acc
                                                        )
                                                        (\k _ acc -> k :: acc)
                                                        oldDict
                                                        newDict
                                                        []
                                                        |> List.reverse

                                                _ ->
                                                    []

                                        _ ->
                                            []

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


updateAuditLogModel : (AuditLogModel -> AuditLogModel) -> Model -> Model
updateAuditLogModel fn model =
    { model | auditLog = fn model.auditLog }


updateFocusModel : (FocusModel -> FocusModel) -> Model -> Model
updateFocusModel fn model =
    { model | focus = fn model.focus }
