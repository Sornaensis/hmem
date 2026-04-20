module Feature.Memory exposing
    ( clearForTabSwitch
    , handleEscape
    , init
    , update
    , viewLinkedMemories
    , viewMemoriesList
    , viewMemoryCard
    )

import Api
import Dict
import Feature.AuditLog
import Feature.DragDrop
import Feature.Editing
import Feature.Focus exposing (buildProjectBreadcrumb, buildTaskBreadcrumb)
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Toast exposing (addToast)
import Types exposing (..)


init : MemoryModel
init =
    { entityMemories = Dict.empty
    , linkingMemoryFor = Nothing
    , linkingEntityFor = Nothing
    }


clearForTabSwitch : Model -> Model
clearForTabSwitch =
    updateMemoryModel
        (\mm ->
            { mm
                | linkingMemoryFor = Nothing
                , linkingEntityFor = Nothing
            }
        )


handleEscape : Model -> Maybe Model
handleEscape model =
    if model.memory.linkingMemoryFor /= Nothing then
        Just (updateMemoryModel (\mm -> { mm | linkingMemoryFor = Nothing }) model)

    else if model.memory.linkingEntityFor /= Nothing then
        Just (updateMemoryModel (\mm -> { mm | linkingEntityFor = Nothing }) model)

    else
        Nothing



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Memory linking
        StartLinkMemory entityType entityId ->
            ( updateMemoryModel
                (\mm -> { mm | linkingMemoryFor = Just { entityType = entityType, entityId = entityId, search = "" } })
                model
            , Cmd.none
            )

        LinkMemorySearch query ->
            case model.memory.linkingMemoryFor of
                Just st ->
                    ( updateMemoryModel (\mm -> { mm | linkingMemoryFor = Just { st | search = query } }) model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CancelLinkMemory ->
            ( updateMemoryModel (\mm -> { mm | linkingMemoryFor = Nothing }) model, Cmd.none )

        PerformLinkMemory entityType entityId memoryId ->
            let
                ( trackedModel, requestId, trackCmd ) =
                    beginTrackedMutation [ entityId, memoryId ]
                        (updateMemoryModel (\mm -> { mm | linkingMemoryFor = Nothing }) model)

                cmd =
                    case entityType of
                        "project" ->
                            Api.linkProjectMemory model.flags.apiUrl entityId memoryId requestId (MemoryLinkDone entityId)

                        "task" ->
                            Api.linkTaskMemory model.flags.apiUrl entityId memoryId requestId (MemoryLinkDone entityId)

                        _ ->
                            Cmd.none
            in
            ( trackedModel, Cmd.batch [ trackCmd, cmd ] )

        PerformUnlinkMemory entityType entityId memoryId ->
            let
                ( trackedModel, requestId, trackCmd ) =
                    beginTrackedMutation [ entityId, memoryId ] model

                cmd =
                    case entityType of
                        "project" ->
                            Api.unlinkProjectMemory model.flags.apiUrl entityId memoryId requestId (MemoryLinkDone entityId)

                        "task" ->
                            Api.unlinkTaskMemory model.flags.apiUrl entityId memoryId requestId (MemoryLinkDone entityId)

                        _ ->
                            Cmd.none
            in
            ( trackedModel, Cmd.batch [ trackCmd, cmd ] )

        MemoryLinkDone entityId result ->
            case result of
                Ok () ->
                    let
                        fetchCmd =
                            if Dict.member entityId model.projects then
                                Api.fetchProjectMemories model.flags.apiUrl entityId (GotEntityMemories entityId)

                            else if Dict.member entityId model.tasks then
                                Api.fetchTaskMemories model.flags.apiUrl entityId (GotEntityMemories entityId)

                            else
                                Cmd.none
                    in
                    ( model, fetchCmd )

                Err _ ->
                    addToast Error "Failed to update memory link" model

        GotEntityMemories entityId result ->
            case result of
                Ok mems ->
                    ( updateMemoryModel (\mm -> { mm | entityMemories = Dict.insert entityId mems mm.entityMemories }) model, Cmd.none )

                Err _ ->
                    addToast Error "Failed to load linked memories" model

        -- Entity linking from memory cards
        StartLinkEntity memoryId ->
            ( updateMemoryModel
                (\mm -> { mm | linkingEntityFor = Just { entityType = "memory", entityId = memoryId, search = "" } })
                model
            , Cmd.none
            )

        LinkEntitySearch query ->
            case model.memory.linkingEntityFor of
                Just st ->
                    ( updateMemoryModel (\mm -> { mm | linkingEntityFor = Just { st | search = query } }) model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CancelLinkEntity ->
            ( updateMemoryModel (\mm -> { mm | linkingEntityFor = Nothing }) model, Cmd.none )

        PerformLinkEntity entityType entityId memoryId ->
            let
                ( trackedModel, requestId, trackCmd ) =
                    beginTrackedMutation [ entityId, memoryId ]
                        (updateMemoryModel (\mm -> { mm | linkingEntityFor = Nothing }) model)

                cmd =
                    case entityType of
                        "project" ->
                            Api.linkProjectMemory model.flags.apiUrl entityId memoryId requestId (MemoryLinkDone entityId)

                        "task" ->
                            Api.linkTaskMemory model.flags.apiUrl entityId memoryId requestId (MemoryLinkDone entityId)

                        _ ->
                            Cmd.none
            in
            ( trackedModel, Cmd.batch [ trackCmd, cmd ] )

        PerformUnlinkEntity entityType entityId memoryId ->
            let
                ( trackedModel, requestId, trackCmd ) =
                    beginTrackedMutation [ entityId, memoryId ] model

                cmd =
                    case entityType of
                        "project" ->
                            Api.unlinkProjectMemory model.flags.apiUrl entityId memoryId requestId (MemoryLinkDone entityId)

                        "task" ->
                            Api.unlinkTaskMemory model.flags.apiUrl entityId memoryId requestId (MemoryLinkDone entityId)

                        _ ->
                            Cmd.none
            in
            ( trackedModel, Cmd.batch [ trackCmd, cmd ] )

        _ ->
            ( model, Cmd.none )



-- VIEW: MEMORIES LIST


viewMemoriesList : String -> Model -> Html Msg
viewMemoriesList wsId model =
    let
        query =
            String.toLower (String.trim model.search.query)

        hasSearch =
            not (String.isEmpty query)

        memoryPassesTypeFilter m =
            if List.isEmpty model.search.filterMemoryTypes then
                True

            else
                List.member (Api.memoryTypeToString m.memoryType) model.search.filterMemoryTypes

        memoryPassesPinnedFilter m =
            case model.search.filterMemoryPinned of
                Nothing ->
                    True

                Just pinned ->
                    m.pinned == pinned

        memoryPassesImportanceFilter m =
            passesPriorityFilter model.search.filterImportance m.importance

        memoryPassesTagFilter m =
            if List.isEmpty model.search.filterTags then
                True

            else
                List.any (\t -> List.member t m.tags) model.search.filterTags

        allWsMemories =
            model.memories
                |> Dict.values
                |> List.filter (\m -> m.workspaceId == wsId)

        allTags =
            allWsMemories
                |> List.concatMap .tags
                |> List.sort
                |> uniqueStrings

        wsMemories =
            allWsMemories
                |> (if hasSearch then
                        List.filter
                            (\m ->
                                String.contains query (String.toLower m.content)
                                    || (m.summary |> Maybe.map (\s -> String.contains query (String.toLower s)) |> Maybe.withDefault False)
                                    || List.any (\t -> String.contains query (String.toLower t)) m.tags
                            )

                    else
                        identity
                   )
                |> List.filter memoryPassesTypeFilter
                |> List.filter memoryPassesPinnedFilter
                |> List.filter memoryPassesImportanceFilter
                |> List.filter memoryPassesTagFilter
                |> List.sortBy (\m -> ( negate m.importance, String.toLower (m.summary |> Maybe.withDefault m.content) ))

        inlineCreateView =
            Feature.Editing.viewInlineCreateMemory model
    in
    Keyed.node "div" [ class "entity-list" ]
        (( "tag-cloud", viewTagCloud model allTags )
            :: ( "inline-create-memory", inlineCreateView )
            :: (if List.isEmpty wsMemories then
                    [ ( "empty-state", div [ class "empty-state" ] [ text "No memories yet." ] ) ]

                else
                    List.map (\m -> ( m.id, viewMemoryCard model m )) wsMemories
               )
        )


viewTagCloud : Model -> List String -> Html Msg
viewTagCloud model allTags =
    if List.isEmpty allTags then
        text ""

    else
        div [ class "tag-cloud" ]
            [ div [ class "tag-cloud-tags" ]
                (List.map
                    (\tag ->
                        button
                            [ class
                                (if List.member tag model.search.filterTags then
                                    "tag-cloud-tag tag-cloud-tag-active"

                                 else
                                    "tag-cloud-tag"
                                )
                            , onClick (ToggleFilterTag tag)
                            ]
                            [ text tag ]
                    )
                    allTags
                )
            ]


updateMemoryModel : (MemoryModel -> MemoryModel) -> Model -> Model
updateMemoryModel fn model =
    { model | memory = fn model.memory }



-- VIEW: MEMORY CARD


viewMemoryCard : Model -> Api.Memory -> Html Msg
viewMemoryCard model memory =
    let
        extrasExpanded =
            isExpanded model memory.id

        linkedEntities =
            model.memory.entityMemories
                |> Dict.toList
                |> List.filterMap
                    (\( entityId, mems ) ->
                        if List.any (\m -> m.id == memory.id) mems then
                            Just entityId

                        else
                            Nothing
                    )

        linkedProjects =
            List.filterMap (\eid -> Dict.get eid model.projects) linkedEntities

        linkedTasks =
            List.filterMap (\eid -> Dict.get eid model.tasks) linkedEntities
    in
    div
        [ class ("card" ++ Feature.DragDrop.dragOverClass model memory.id)
        , draggable "true"
        , on "dragstart" (Decode.succeed (DragStartCard "memory" memory.id))
        , preventDefaultOn "dragover" (Decode.succeed ( DragOverCard memory.id, True ))
        , preventDefaultOn "drop" (Decode.succeed ( DropOnCard "memory" memory.id, True ))
        , on "dragend" (Decode.succeed DragEndCard)
        ]
        [ div [ class "card-header" ]
            [ case editingValue model memory.id "summary" of
                Just val ->
                    input
                        [ class "inline-edit-input"
                        , value val
                        , onInput EditInput
                        , onBlur (SaveEdit memory.id "summary")
                        , Feature.Editing.onKeyDown
                            (\keyCode ->
                                if keyCode == 13 then
                                    SaveEdit memory.id "summary"

                                else if keyCode == 27 then
                                    CancelEdit

                                else
                                    NoOp
                            )
                        , Html.Attributes.id (editElementId memory.id "summary")
                        ]
                        []

                Nothing ->
                    span
                        [ class "editable-text"
                        , onClick (StartEdit "memory" memory.id "summary" (Maybe.withDefault "" memory.summary))
                        , title "Click to edit summary"
                        ]
                        [ text
                            (case memory.summary of
                                Just s ->
                                    if String.isEmpty s then
                                        "<untitled>"

                                    else
                                        s

                                Nothing ->
                                    "<untitled>"
                            )
                        ]
            , div [ class "card-actions" ]
                [ Feature.Editing.viewMemoryTypeSelect memory.id memory.memoryType
                , Feature.Editing.viewImportanceSelect memory.id memory.importance
                , button
                    [ class
                        (if memory.pinned then
                            "btn-icon btn-pinned"

                         else
                            "btn-icon"
                        )
                    , onClick (ToggleMemoryPin memory.id (not memory.pinned))
                    , title "Pin"
                    ]
                    [ text "\u{1F4CC}" ]
                , button [ class "btn-icon btn-danger", onClick (ConfirmDelete "memory" memory.id), title "Delete" ] [ text "✕" ]
                ]
            ]
        , div [ class "card-body card-expanded" ]
            [ div [ class "card-desc-row" ]
                [ button [ class "btn-extras-toggle", onClick (ToggleCardExpand memory.id) ]
                    [ text
                        (if extrasExpanded then
                            "−"

                         else
                            "+"
                        )
                    ]
                , Feature.Editing.viewEditableTextarea model "memory" memory.id "content" memory.content
                ]
            , if extrasExpanded then
                div [ class "card-extras" ]
                    [ Feature.Editing.viewTagEditor model memory
                    , viewMemoryLinkedEntities model memory.id linkedProjects linkedTasks
                    , Feature.AuditLog.viewEntityHistory model "memory" memory.id
                    ]

              else if not (List.isEmpty linkedProjects) || not (List.isEmpty linkedTasks) then
                div [ class "card-collapsed-badges" ]
                    [ span [ class "linked-memories-badge" ]
                        [ text ("🔗 " ++ String.fromInt (List.length linkedProjects + List.length linkedTasks) ++ " linked") ]
                    ]

              else
                text ""
            ]
        , div [ class "card-meta-group" ]
            [ div [ class "card-meta-row" ]
                [ span [ class "card-meta" ] [ text ("Created: " ++ formatDate memory.createdAt) ]
                , span [ class "card-meta" ] [ text ("Updated: " ++ formatDate memory.updatedAt) ]
                , span [ class "card-meta card-id card-id-copy", onClick (CopyId memory.id) ] [ text memory.id ]
                ]
            ]
        ]



-- VIEW: MEMORY LINKED ENTITIES


viewMemoryLinkedEntities : Model -> String -> List Api.Project -> List Api.Task -> Html Msg
viewMemoryLinkedEntities model memoryId projects tasks =
    let
        items =
            List.map
                (\p ->
                    div [ class "linked-entity-item" ]
                        [ span [ class "entity-type-label entity-type-project" ] [ text "PRJ" ]
                        , span [ class "linked-entity-name", onClick (FocusEntity "project" p.id) ] [ text p.name ]
                        , button
                            [ class "btn-icon btn-danger"
                            , onClick (PerformUnlinkEntity "project" p.id memoryId)
                            , title "Unlink"
                            ]
                            [ text "✕" ]
                        ]
                )
                projects
                ++ List.map
                    (\t ->
                        div [ class "linked-entity-item" ]
                            [ span [ class "entity-type-label entity-type-task" ] [ text "TSK" ]
                            , span [ class "linked-entity-name", onClick (FocusEntity "task" t.id) ] [ text t.title ]
                            , button
                                [ class "btn-icon btn-danger"
                                , onClick (PerformUnlinkEntity "task" t.id memoryId)
                                , title "Unlink"
                                ]
                                [ text "✕" ]
                            ]
                    )
                    tasks

        selectorOpen =
            case model.memory.linkingEntityFor of
                Just st ->
                    st.entityId == memoryId

                Nothing ->
                    False
    in
    div [ class "linked-entities-section" ]
        [ div [ class "linked-entities-title" ]
            [ text ("Linked Entities (" ++ String.fromInt (List.length items) ++ ")") ]
        , if List.isEmpty items then
            div [ class "linked-memories-empty" ] [ text "No linked entities" ]

          else
            div [ class "linked-entities-list" ] items
        , div [ class "card-inline-actions" ]
            [ div [ class "popover-anchor" ]
                [ button
                    [ class
                        (if selectorOpen then
                            "btn-inline-create popover-trigger-active"

                         else
                            "btn-inline-create"
                        )
                    , onClick
                        (if selectorOpen then
                            CancelLinkEntity

                         else
                            StartLinkEntity memoryId
                        )
                    ]
                    [ text "+ Link" ]
                , viewLinkEntityPopover model memoryId projects tasks
                ]
            ]
        ]


viewLinkEntityPopover : Model -> String -> List Api.Project -> List Api.Task -> Html Msg
viewLinkEntityPopover model memoryId linkedProjects linkedTasks =
    case model.memory.linkingEntityFor of
        Just st ->
            if st.entityId == memoryId then
                let
                    linkedProjectIds =
                        List.map .id linkedProjects

                    linkedTaskIds =
                        List.map .id linkedTasks

                    query =
                        String.toLower st.search

                    availableProjects =
                        model.projects
                            |> Dict.values
                            |> List.filter (\p -> not (List.member p.id linkedProjectIds))
                            |> List.filter
                                (\p ->
                                    if String.isEmpty query then
                                        True

                                    else
                                        let
                                            crumbText =
                                                buildProjectBreadcrumb model p []
                                                    |> List.map (\( _, label, _ ) -> String.toLower label)
                                                    |> String.join " "
                                        in
                                        String.contains query (String.toLower p.name)
                                            || (p.description |> Maybe.map (\d -> String.contains query (String.toLower d)) |> Maybe.withDefault False)
                                            || String.contains query crumbText
                                )
                            |> List.sortBy (\p -> ( Api.projectStatusOrder p.status, negate p.priority, String.toLower p.name ))
                            |> List.take 10

                    availableTasks =
                        model.tasks
                            |> Dict.values
                            |> List.filter (\t -> not (List.member t.id linkedTaskIds))
                            |> List.filter
                                (\t ->
                                    if String.isEmpty query then
                                        True

                                    else
                                        let
                                            crumbText =
                                                buildTaskBreadcrumb model t []
                                                    |> List.map (\( _, label, _ ) -> String.toLower label)
                                                    |> String.join " "
                                        in
                                        String.contains query (String.toLower t.title)
                                            || (t.description |> Maybe.map (\d -> String.contains query (String.toLower d)) |> Maybe.withDefault False)
                                            || String.contains query crumbText
                                )
                            |> List.sortBy (\t -> ( Api.taskStatusOrder t.status, negate t.priority, String.toLower t.title ))
                            |> List.take 10

                    allItems =
                        List.map
                            (\p ->
                                let
                                    crumbs =
                                        buildProjectBreadcrumb model p []
                                            |> List.filter (\( eid, _, _ ) -> eid /= p.id)
                                in
                                div
                                    [ class "popover-card"
                                    , onClick (PerformLinkEntity "project" p.id memoryId)
                                    ]
                                    [ div [ class "popover-card-header" ]
                                        [ span [ class "entity-type-label entity-type-project" ] [ text "PRJ" ]
                                        , span [ class "popover-card-title" ] [ text p.name ]
                                        ]
                                    , if not (List.isEmpty crumbs) then
                                        div [ class "popover-card-breadcrumb" ]
                                            (List.intersperse (span [] [ text " › " ])
                                                (List.map (\( _, label, _ ) -> span [] [ text label ]) crumbs)
                                            )

                                      else
                                        text ""
                                    , div [ class "popover-card-meta" ]
                                        [ span [ class ("popover-card-status card-status-" ++ Api.projectStatusToString p.status) ]
                                            [ text (Api.projectStatusToString p.status) ]
                                        , case p.description of
                                            Just d ->
                                                span [ class "popover-card-desc" ] [ text d ]

                                            Nothing ->
                                                text ""
                                        ]
                                    ]
                            )
                            availableProjects
                            ++ List.map
                                (\t ->
                                    let
                                        crumbs =
                                            buildTaskBreadcrumb model t []
                                                |> List.filter (\( eid, _, _ ) -> eid /= t.id)
                                    in
                                    div
                                        [ class "popover-card"
                                        , onClick (PerformLinkEntity "task" t.id memoryId)
                                        ]
                                        [ div [ class "popover-card-header" ]
                                            [ span
                                                [ class
                                                    ("entity-type-label "
                                                        ++ (if t.parentId /= Nothing then
                                                                "entity-type-subtask"

                                                            else
                                                                "entity-type-task"
                                                           )
                                                    )
                                                ]
                                                [ text
                                                    (if t.parentId /= Nothing then
                                                        "SUB"

                                                     else
                                                        "TSK"
                                                    )
                                                ]
                                            , span [ class "popover-card-title" ] [ text t.title ]
                                            ]
                                        , if not (List.isEmpty crumbs) then
                                            div [ class "popover-card-breadcrumb" ]
                                                (List.intersperse (span [] [ text " › " ])
                                                    (List.map (\( _, label, _ ) -> span [] [ text label ]) crumbs)
                                                )

                                          else
                                            text ""
                                        , div [ class "popover-card-meta" ]
                                            [ span [ class ("popover-card-status card-status-" ++ Api.taskStatusToString t.status) ]
                                                [ text (Api.taskStatusToString t.status |> String.replace "_" " ") ]
                                            , span [ class "popover-card-priority" ] [ text ("P" ++ String.fromInt t.priority) ]
                                            , case t.description of
                                                Just d ->
                                                    span [ class "popover-card-desc" ] [ text d ]

                                                Nothing ->
                                                    text ""
                                            ]
                                        ]
                                )
                                availableTasks
                in
                div [ class "popover-container" ]
                    [ div [ class "popover-overlay", onClick CancelLinkEntity ] []
                    , div [ class "popover-menu", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                        [ input
                            [ class "popover-search"
                            , placeholder "Search projects & tasks..."
                            , value st.search
                            , onInput LinkEntitySearch
                            , autofocus True
                            ]
                            []
                        , div [ class "popover-results" ]
                            (if List.isEmpty allItems then
                                [ div [ class "popover-empty" ] [ text "No matching entities" ] ]

                             else
                                allItems
                            )
                        ]
                    ]

            else
                text ""

        Nothing ->
            text ""



-- VIEW: LINKED MEMORIES (used in project/task cards)


viewLinkedMemories : Model -> String -> String -> List Api.Memory -> Html Msg
viewLinkedMemories model entityType entityId linkedMems =
    let
        selectorOpen =
            case model.memory.linkingMemoryFor of
                Just st ->
                    st.entityType == entityType && st.entityId == entityId

                Nothing ->
                    False
    in
    div [ class "linked-memories-section" ]
        [ div [ class "linked-memories-header" ]
            [ span [ class "linked-memories-title" ] [ text ("Linked Memories (" ++ String.fromInt (List.length linkedMems) ++ ")") ]
            ]
        , if List.isEmpty linkedMems then
            div [ class "linked-memories-empty" ] [ text "No linked memories" ]

          else
            div [ class "linked-memories-list" ]
                (List.map (viewLinkedMemoryItem model entityType entityId) linkedMems)
        , div [ class "card-inline-actions" ]
            [ div [ class "popover-anchor" ]
                [ button
                    [ class
                        (if selectorOpen then
                            "btn-inline-create popover-trigger-active"

                         else
                            "btn-inline-create"
                        )
                    , onClick
                        (if selectorOpen then
                            CancelLinkMemory

                         else
                            StartLinkMemory entityType entityId
                        )
                    ]
                    [ text "+ Link" ]
                , viewLinkMemoryPopover model entityType entityId linkedMems
                ]
            ]
        ]


viewLinkMemoryPopover : Model -> String -> String -> List Api.Memory -> Html Msg
viewLinkMemoryPopover model entityType entityId linkedMems =
    case model.memory.linkingMemoryFor of
        Just st ->
            if st.entityType == entityType && st.entityId == entityId then
                let
                    linkedIds =
                        List.map .id linkedMems

                    query =
                        String.toLower st.search

                    availableMemories =
                        model.memories
                            |> Dict.values
                            |> List.filter (\m -> not (List.member m.id linkedIds))
                            |> List.filter
                                (\m ->
                                    if String.isEmpty query then
                                        True

                                    else
                                        String.contains query (String.toLower m.content)
                                            || (m.summary |> Maybe.map (\s -> String.contains query (String.toLower s)) |> Maybe.withDefault False)
                                            || List.any (\tag -> String.contains query (String.toLower tag)) m.tags
                                )
                            |> List.sortBy (\m -> ( negate m.importance, String.toLower (m.summary |> Maybe.withDefault m.content) ))
                            |> List.take 15
                in
                div [ class "popover-container" ]
                    [ div [ class "popover-overlay", onClick CancelLinkMemory ] []
                    , div [ class "popover-menu", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                        [ input
                            [ class "popover-search"
                            , placeholder "Search memories..."
                            , value st.search
                            , onInput LinkMemorySearch
                            , autofocus True
                            ]
                            []
                        , div [ class "popover-results" ]
                            (if List.isEmpty availableMemories then
                                [ div [ class "popover-empty" ] [ text "No matching memories" ] ]

                             else
                                List.map
                                    (\mem ->
                                        div
                                            [ class "popover-card"
                                            , onClick (PerformLinkMemory entityType entityId mem.id)
                                            ]
                                            [ div [ class "popover-card-header" ]
                                                [ span [ class "entity-type-label entity-type-memory" ]
                                                    [ text
                                                        (if mem.memoryType == Api.ShortTerm then
                                                            "STM"

                                                         else
                                                            "LTM"
                                                        )
                                                    ]
                                                , span [ class "popover-card-title" ]
                                                    [ text
                                                        (case mem.summary of
                                                            Just s ->
                                                                s

                                                            Nothing ->
                                                                mem.content
                                                        )
                                                    ]
                                                ]
                                            , div [ class "popover-card-meta" ]
                                                [ span [ class "popover-card-status" ]
                                                    [ text (Api.memoryTypeToString mem.memoryType |> String.replace "_" " ") ]
                                                , span [ class "popover-card-priority" ] [ text ("★ " ++ String.fromInt mem.importance) ]
                                                , if mem.pinned then
                                                    span [ class "popover-card-pinned" ] [ text "📌" ]

                                                  else
                                                    text ""
                                                ]
                                            , if not (List.isEmpty mem.tags) then
                                                div [ class "popover-card-tags" ]
                                                    (List.map (\tag -> span [ class "popover-card-tag" ] [ text tag ]) (List.take 4 mem.tags))

                                              else
                                                text ""
                                            , div [ class "popover-card-desc" ] [ text mem.content ]
                                            ]
                                    )
                                    availableMemories
                            )
                        ]
                    ]

            else
                text ""

        Nothing ->
            text ""


viewLinkedMemoryItem : Model -> String -> String -> Api.Memory -> Html Msg
viewLinkedMemoryItem model entityType entityId memory =
    div [ class "linked-memory-item popover-card" ]
        [ div [ class "popover-card-header" ]
            [ span [ class "entity-type-label entity-type-memory" ]
                [ text
                    (if memory.memoryType == Api.ShortTerm then
                        "STM"

                     else
                        "LTM"
                    )
                ]
            , span [ class "popover-card-title" ]
                [ text
                    (case memory.summary of
                        Just s ->
                            s

                        Nothing ->
                            memory.content
                    )
                ]
            , div [ class "dep-item-actions" ]
                [ button
                    [ class "btn-icon btn-danger"
                    , onClick (PerformUnlinkMemory entityType entityId memory.id)
                    , title "Unlink"
                    ]
                    [ text "✕" ]
                ]
            ]
        , div [ class "popover-card-meta" ]
            [ span [ class "popover-card-status" ]
                [ text (Api.memoryTypeToString memory.memoryType |> String.replace "_" " ") ]
            , span [ class "popover-card-priority" ] [ text ("★ " ++ String.fromInt memory.importance) ]
            , if memory.pinned then
                span [ class "popover-card-pinned" ] [ text "📌" ]

              else
                text ""
            ]
        , if not (List.isEmpty memory.tags) then
            div [ class "popover-card-tags" ]
                (List.map (\t -> span [ class "popover-card-tag" ] [ text t ]) (List.take 4 memory.tags))

          else
            text ""
        , div [ class "popover-card-desc" ] [ text memory.content ]
        ]



-- HELPERS


uniqueStrings : List String -> List String
uniqueStrings list =
    List.foldl
        (\item acc ->
            if List.member item acc then
                acc

            else
                acc ++ [ item ]
        )
        []
        list
