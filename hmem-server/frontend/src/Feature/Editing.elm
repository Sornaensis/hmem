module Feature.Editing exposing
    ( clearForTabSwitch
    , handleEscape
    , init
    , onKeyDown
    , update
    , viewCreateFormModal
    , viewEditableText
    , viewEditableTextarea
    , viewImportanceSelect
    , viewInlineCreateInput
    , viewInlineCreateInputForParent
    , viewInlineCreateMemory
    , viewMemoryTypeSelect
    , viewPrioritySelect
    , viewStatusSelect
    , viewTagEditor
    )

import Api
import Dict
import Helpers exposing (beginTrackedMutation, editElementId, editingValue, focusElement)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Markdown.Parser
import Markdown.Renderer
import Types exposing (..)


init : EditingModel
init =
    { editState = Nothing
    , createForm = Nothing
    , inlineCreate = Nothing
    }


clearForTabSwitch : Model -> Model
clearForTabSwitch =
    updateEditingModel
        (\ed ->
            { ed
                | createForm = Nothing
                , editState = Nothing
                , inlineCreate = Nothing
            }
        )


handleEscape : Model -> Maybe Model
handleEscape model =
    if model.editing.inlineCreate /= Nothing then
        Just (updateEditingModel (\ed -> { ed | inlineCreate = Nothing }) model)

    else if model.editing.editState /= Nothing then
        Just (updateEditingModel (\ed -> { ed | editState = Nothing }) model)

    else
        Nothing



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Inline editing
        StartEdit entityType entityId field currentValue ->
            let
                editingModel =
                    model.editing

                ( baseModel, saveCmd ) =
                    case editingModel.editState of
                        Just (EditingField state) ->
                            if state.value /= state.original then
                                let
                                    ( trackedModel, requestId, clearCmd ) =
                                        beginTrackedMutation [ state.entityId ] model
                                in
                                ( trackedModel, Cmd.batch [ clearCmd, saveEditCmd model.flags.apiUrl (Just requestId) state ] )

                            else
                                ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )
            in
            ( updateEditingModel
                (\ed ->
                    { ed
                        | editState =
                            Just
                                (EditingField
                                    { entityType = entityType
                                    , entityId = entityId
                                    , field = field
                                    , value = currentValue
                                    , original = currentValue
                                    }
                                )
                    }
                )
                baseModel
            , Cmd.batch [ saveCmd, focusElement (editElementId entityId field) ]
            )

        EditInput newValue ->
            case model.editing.editState of
                Just (EditingField state) ->
                    ( updateEditingModel (\ed -> { ed | editState = Just (EditingField { state | value = newValue }) }) model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SaveEdit saveEntityId saveField ->
            case model.editing.editState of
                Just (EditingField state) ->
                    if state.entityId == saveEntityId && state.field == saveField then
                        if state.value == state.original then
                            ( updateEditingModel (\ed -> { ed | editState = Nothing }) model, Cmd.none )

                        else
                            let
                                ( trackedModel, requestId, clearCmd ) =
                                    beginTrackedMutation [ state.entityId ] (updateEditingModel (\ed -> { ed | editState = Nothing }) model)
                            in
                            ( trackedModel, Cmd.batch [ clearCmd, saveEditCmd model.flags.apiUrl (Just requestId) state ] )

                    else
                        -- Editing state has moved to a different field; don't clear it
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CancelEdit ->
            ( updateEditingModel (\ed -> { ed | editState = Nothing }) model, Cmd.none )

        -- Quick-change handlers
        ChangeProjectStatus projId newStatus ->
            let
                ( trackedModel, requestId, clearCmd ) =
                    beginTrackedMutation [ projId ] model
            in
            ( trackedModel
            , Cmd.batch
                [ clearCmd
                , Api.updateProject model.flags.apiUrl
                    projId
                    [ ( "status", Encode.string (Api.projectStatusToString newStatus) ), ( "request_id", Encode.string requestId ) ]
                    ProjectUpdated
                ]
            )

        ChangeTaskStatus taskId newStatus ->
            let
                ( trackedModel, requestId, clearCmd ) =
                    beginTrackedMutation [ taskId ] model
            in
            ( trackedModel
            , Cmd.batch
                [ clearCmd
                , Api.updateTask model.flags.apiUrl
                    taskId
                    [ ( "status", Encode.string (Api.taskStatusToString newStatus) ), ( "request_id", Encode.string requestId ) ]
                    TaskUpdated
                ]
            )

        ChangeProjectPriority projId newPri ->
            let
                ( trackedModel, requestId, clearCmd ) =
                    beginTrackedMutation [ projId ] model
            in
            ( trackedModel
            , Cmd.batch
                [ clearCmd
                , Api.updateProject model.flags.apiUrl
                    projId
                    [ ( "priority", Encode.int newPri ), ( "request_id", Encode.string requestId ) ]
                    ProjectUpdated
                ]
            )

        ChangeTaskPriority taskId newPri ->
            let
                ( trackedModel, requestId, clearCmd ) =
                    beginTrackedMutation [ taskId ] model
            in
            ( trackedModel
            , Cmd.batch
                [ clearCmd
                , Api.updateTask model.flags.apiUrl
                    taskId
                    [ ( "priority", Encode.int newPri ), ( "request_id", Encode.string requestId ) ]
                    TaskUpdated
                ]
            )

        ChangeMemoryImportance memId newImp ->
            let
                ( trackedModel, requestId, clearCmd ) =
                    beginTrackedMutation [ memId ] model
            in
            ( trackedModel
            , Cmd.batch
                [ clearCmd
                , Api.updateMemory model.flags.apiUrl
                    memId
                    [ ( "importance", Encode.int newImp ), ( "request_id", Encode.string requestId ) ]
                    MemoryUpdated
                ]
            )

        ToggleMemoryPin memId newPinned ->
            let
                ( trackedModel, requestId, clearCmd ) =
                    beginTrackedMutation [ memId ] model
            in
            ( trackedModel
            , Cmd.batch
                [ clearCmd
                , Api.updateMemory model.flags.apiUrl
                    memId
                    [ ( "pinned", Encode.bool newPinned ), ( "request_id", Encode.string requestId ) ]
                    MemoryUpdated
                ]
            )

        ChangeMemoryType memId newType ->
            let
                ( trackedModel, requestId, clearCmd ) =
                    beginTrackedMutation [ memId ] model
            in
            ( trackedModel
            , Cmd.batch
                [ clearCmd
                , Api.updateMemory model.flags.apiUrl
                    memId
                    [ ( "memory_type", Encode.string (Api.memoryTypeToString newType) ), ( "request_id", Encode.string requestId ) ]
                    MemoryUpdated
                ]
            )

        -- Tags
        RemoveTag memId tagToRemove ->
            case Dict.get memId model.memories of
                Just mem ->
                    let
                        newTags =
                            List.filter (\t -> t /= tagToRemove) mem.tags

                        ( trackedModel, requestId, clearCmd ) =
                            beginTrackedMutation [ memId ] model
                    in
                    ( trackedModel, Cmd.batch [ clearCmd, Api.setTags model.flags.apiUrl memId newTags requestId (MutationDone "tags") ] )

                Nothing ->
                    ( model, Cmd.none )

        AddTag memId newTag ->
            case Dict.get memId model.memories of
                Just mem ->
                    if String.isEmpty (String.trim newTag) then
                        ( model, Cmd.none )

                    else
                        let
                            newTags =
                                mem.tags ++ [ String.trim newTag ]

                            trackedModel =
                                updateEditingModel (\ed -> { ed | editState = Nothing }) model

                            ( trackedModel2, requestId, clearCmd ) =
                                beginTrackedMutation [ memId ] trackedModel
                        in
                        ( trackedModel2
                        , Cmd.batch [ clearCmd, Api.setTags model.flags.apiUrl memId newTags requestId (MutationDone "tags") ]
                        )

                Nothing ->
                    ( model, Cmd.none )

        -- Create forms
        ShowCreateForm form ->
            ( updateEditingModel (\ed -> { ed | createForm = Just form }) model, Cmd.none )

        UpdateCreateForm form ->
            ( updateEditingModel (\ed -> { ed | createForm = Just form }) model, Cmd.none )

        SubmitCreateForm ->
            case model.editing.createForm of
                Just (CreateGroupForm f) ->
                    if String.isEmpty (String.trim f.name) then
                        ( model, Cmd.none )

                    else
                        let
                            desc =
                                if String.isEmpty (String.trim f.description) then
                                    Nothing

                                else
                                    Just (String.trim f.description)

                            ( trackedModel, requestId, clearCmd ) =
                                beginTrackedMutation [] (updateEditingModel (\ed -> { ed | createForm = Nothing }) model)
                        in
                        ( trackedModel
                        , Cmd.batch
                            [ clearCmd
                            , Api.createWorkspaceGroup model.flags.apiUrl (String.trim f.name) desc requestId WorkspaceGroupCreated
                            ]
                        )

                _ ->
                    case ( model.editing.createForm, model.selectedWorkspaceId ) of
                        ( Just (CreateProjectForm f), Just wsId ) ->
                            if String.isEmpty (String.trim f.name) then
                                ( model, Cmd.none )

                            else
                                let
                                    ( trackedModel, requestId, clearCmd ) =
                                        beginTrackedMutation [] model
                                in
                                ( trackedModel, Cmd.batch [ clearCmd, Api.createProject model.flags.apiUrl wsId f.name requestId ProjectCreated ] )

                        ( Just (CreateMemoryForm f), Just wsId ) ->
                            if String.isEmpty (String.trim f.content) then
                                ( model, Cmd.none )

                            else
                                let
                                    ( trackedModel, requestId, clearCmd ) =
                                        beginTrackedMutation [] model
                                in
                                ( trackedModel, Cmd.batch [ clearCmd, Api.createMemory model.flags.apiUrl wsId f.content f.memoryType requestId MemoryCreated ] )

                        _ ->
                            ( model, Cmd.none )

        CancelCreateForm ->
            ( updateEditingModel (\ed -> { ed | createForm = Nothing }) model, Cmd.none )

        -- Inline create
        ShowInlineCreate ic ->
            ( updateEditingModel (\ed -> { ed | inlineCreate = Just ic }) model, focusElement "inline-create-input" )

        UpdateInlineCreate ic ->
            ( updateEditingModel (\ed -> { ed | inlineCreate = Just ic }) model, Cmd.none )

        SubmitInlineCreate ->
            case ( model.editing.inlineCreate, model.selectedWorkspaceId ) of
                ( Just (InlineCreateProject { parentId, name }), Just wsId ) ->
                    if String.isEmpty (String.trim name) then
                        ( model, Cmd.none )

                    else
                        case parentId of
                            Just pid ->
                                let
                                    ( trackedModel, requestId, clearCmd ) =
                                        beginTrackedMutation [] model
                                in
                                ( trackedModel, Cmd.batch [ clearCmd, Api.createProjectWithParent model.flags.apiUrl wsId pid name requestId ProjectCreated ] )

                            Nothing ->
                                let
                                    ( trackedModel, requestId, clearCmd ) =
                                        beginTrackedMutation [] model
                                in
                                ( trackedModel, Cmd.batch [ clearCmd, Api.createProject model.flags.apiUrl wsId name requestId ProjectCreated ] )

                ( Just (InlineCreateTask { projectId, parentId, title }), Just wsId ) ->
                    if String.isEmpty (String.trim title) then
                        ( model, Cmd.none )

                    else
                        case parentId of
                            Just pid ->
                                let
                                    ( trackedModel, requestId, clearCmd ) =
                                        beginTrackedMutation [] model
                                in
                                ( trackedModel, Cmd.batch [ clearCmd, Api.createTaskWithParent model.flags.apiUrl wsId projectId pid title requestId TaskCreated ] )

                            Nothing ->
                                let
                                    ( trackedModel, requestId, clearCmd ) =
                                        beginTrackedMutation [] model
                                in
                                ( trackedModel, Cmd.batch [ clearCmd, Api.createTask model.flags.apiUrl wsId projectId title requestId TaskCreated ] )

                ( Just (InlineCreateMemory { content }), Just wsId ) ->
                    if String.isEmpty (String.trim content) then
                        ( model, Cmd.none )

                    else
                        let
                            ( trackedModel, requestId, clearCmd ) =
                                beginTrackedMutation [] model
                        in
                        ( trackedModel, Cmd.batch [ clearCmd, Api.createMemory model.flags.apiUrl wsId content Api.ShortTerm requestId MemoryCreated ] )

                _ ->
                    ( model, Cmd.none )

        CancelInlineCreate ->
            ( updateEditingModel (\ed -> { ed | inlineCreate = Nothing }) model, Cmd.none )

        -- Expand and edit (composite)
        ExpandAndEdit cardId entityType entityId field currentValue ->
            let
                ( baseModel, saveCmd ) =
                    case model.editing.editState of
                        Just (EditingField state) ->
                            if state.value /= state.original then
                                let
                                    ( trackedModel, requestId, clearCmd ) =
                                        beginTrackedMutation [ state.entityId ] model
                                in
                                ( trackedModel, Cmd.batch [ clearCmd, saveEditCmd model.flags.apiUrl (Just requestId) state ] )

                            else
                                ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                fetchMemCmd =
                    if not (Dict.member cardId model.memory.entityMemories) then
                        if Dict.member cardId model.projects then
                            Api.fetchProjectMemories model.flags.apiUrl cardId (GotEntityMemories cardId)

                        else if Dict.member cardId model.tasks then
                            Api.fetchTaskMemories model.flags.apiUrl cardId (GotEntityMemories cardId)

                        else
                            Cmd.none

                    else
                        Cmd.none

                fetchDepCmd =
                    if Dict.member cardId model.tasks && not (Dict.member cardId model.dependencies.taskDependencies) then
                        Api.fetchTaskOverview model.flags.apiUrl cardId (GotTaskDependencies cardId)

                    else
                        Cmd.none

                currentCards =
                    model.cards

                updatedCards =
                    { currentCards | expandedCards = Dict.insert cardId True model.cards.expandedCards }
            in
            ( { baseModel
                | cards = updatedCards
              }
                |> updateEditingModel
                    (\ed ->
                        { ed
                            | editState =
                                Just
                                    (EditingField
                                        { entityType = entityType
                                        , entityId = entityId
                                        , field = field
                                        , value = currentValue
                                        , original = currentValue
                                        }
                                    )
                        }
                    )
            , Cmd.batch [ saveCmd, focusElement (editElementId entityId field), fetchMemCmd, fetchDepCmd ]
            )

        _ ->
            ( model, Cmd.none )



-- COMMANDS


updateEditingModel : (EditingModel -> EditingModel) -> Model -> Model
updateEditingModel fn model =
    { model | editing = fn model.editing }


saveEditCmd : String -> Maybe String -> { entityType : String, entityId : String, field : String, value : String, original : String } -> Cmd Msg
saveEditCmd apiUrl maybeRequestId state =
    let
        fields =
            [ ( state.field, Encode.string state.value ) ]
                ++ (case maybeRequestId of
                        Just requestId ->
                            [ ( "request_id", Encode.string requestId ) ]

                        Nothing ->
                            []
                   )
    in
    case state.entityType of
        "workspace" ->
            Api.updateWorkspace apiUrl
                state.entityId
                fields
                WorkspaceUpdated

        "project" ->
            Api.updateProject apiUrl
                state.entityId
                fields
                ProjectUpdated

        "task" ->
            Api.updateTask apiUrl
                state.entityId
                fields
                TaskUpdated

        "memory" ->
            Api.updateMemory apiUrl
                state.entityId
                fields
                MemoryUpdated

        _ ->
            Cmd.none



-- VIEW HELPERS


onKeyDown : (Int -> Msg) -> Attribute Msg
onKeyDown toMsg =
    on "keydown" (Decode.map toMsg (Decode.field "keyCode" Decode.int))



-- INLINE EDITABLE FIELDS


viewEditableText : Model -> String -> String -> String -> String -> Html Msg
viewEditableText model entityType entityId field currentValue =
    case editingValue model entityId field of
        Just val ->
            input
                [ class "inline-edit-input"
                , value val
                , onInput EditInput
                , onBlur (SaveEdit entityId field)
                , onKeyDown
                    (\keyCode ->
                        if keyCode == 13 then
                            SaveEdit entityId field

                        else if keyCode == 27 then
                            CancelEdit

                        else
                            NoOp
                    )
                , Html.Attributes.id (editElementId entityId field)
                ]
                []

        Nothing ->
            span
                [ class "editable-text"
                , onClick (StartEdit entityType entityId field currentValue)
                , title "Click to edit"
                ]
                [ text
                    (if String.isEmpty currentValue then
                        "(empty)"

                     else
                        currentValue
                    )
                ]


viewMarkdownContent : String -> Html Msg
viewMarkdownContent raw =
    case
        raw
            |> Markdown.Parser.parse
            |> Result.mapError (\_ -> "parse error")
            |> Result.andThen (Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer)
    of
        Ok rendered ->
            div [ class "markdown-content" ] rendered

        Err _ ->
            div [ class "markdown-content", style "white-space" "pre-wrap" ] [ text raw ]


viewEditableTextarea : Model -> String -> String -> String -> String -> Html Msg
viewEditableTextarea model entityType entityId field currentValue =
    case editingValue model entityId field of
        Just val ->
            let
                lineCount =
                    val
                        |> String.split "\n"
                        |> List.length

                rowCount =
                    Basics.max 10 (lineCount + 1)
            in
            textarea
                [ class "inline-edit-textarea"
                , value val
                , onInput EditInput
                , onBlur (SaveEdit entityId field)
                , rows rowCount
                , Html.Attributes.id (editElementId entityId field)
                ]
                []

        Nothing ->
            div
                [ class "editable-textarea"
                , onClick (StartEdit entityType entityId field currentValue)
                , title "Click to edit"
                ]
                [ if String.isEmpty currentValue then
                    text "Click to add description..."

                  else
                    viewMarkdownContent currentValue
                ]



-- STATUS / PRIORITY SELECTORS


viewStatusSelect : String -> String -> String -> List a -> (a -> String) -> (String -> a -> Msg) -> Html Msg
viewStatusSelect entityType entityId currentStr allValues toString toMsg =
    select
        [ class ("status-select badge badge-" ++ currentStr)
        , onInput
            (\s ->
                let
                    matched =
                        allValues |> List.filter (\v -> toString v == s) |> List.head
                in
                case matched of
                    Just v ->
                        toMsg entityId v

                    Nothing ->
                        NoOp
            )
        ]
        (List.map
            (\v ->
                let
                    str =
                        toString v
                in
                option [ value str, selected (str == currentStr) ]
                    [ text (str |> String.replace "_" " ") ]
            )
            allValues
        )


viewPrioritySelect : String -> String -> Int -> (String -> Int -> Msg) -> Html Msg
viewPrioritySelect entityType entityId currentPri toMsg =
    select
        [ class "priority-select"
        , title "Priority"
        , onInput
            (\s ->
                case String.toInt s of
                    Just n ->
                        toMsg entityId n

                    Nothing ->
                        NoOp
            )
        ]
        (List.map
            (\n ->
                option [ value (String.fromInt n), selected (n == currentPri) ]
                    [ text ("P" ++ String.fromInt n) ]
            )
            (List.range 1 10)
        )


viewImportanceSelect : String -> Int -> Html Msg
viewImportanceSelect memId currentImp =
    select
        [ class "priority-select"
        , title "Importance"
        , onInput
            (\s ->
                case String.toInt s of
                    Just n ->
                        ChangeMemoryImportance memId n

                    Nothing ->
                        NoOp
            )
        ]
        (List.map
            (\n ->
                option [ value (String.fromInt n), selected (n == currentImp) ]
                    [ text ("★" ++ String.fromInt n) ]
            )
            (List.range 1 10)
        )


viewMemoryTypeSelect : String -> Api.MemoryType -> Html Msg
viewMemoryTypeSelect memId currentType =
    select
        [ class ("status-select badge badge-" ++ Api.memoryTypeToString currentType)
        , onInput
            (\s ->
                ChangeMemoryType memId (Api.memoryTypeFromString s)
            )
        ]
        (List.map
            (\mt ->
                let
                    str =
                        Api.memoryTypeToString mt
                in
                option [ value str, selected (mt == currentType) ]
                    [ text (str |> String.replace "_" " ") ]
            )
            Api.allMemoryTypes
        )



-- TAG EDITOR


viewTagEditor : Model -> Api.Memory -> Html Msg
viewTagEditor model memory =
    div [ class "tag-editor" ]
        [ div [ class "tag-list" ]
            (List.map
                (\t ->
                    span [ class "tag tag-removable" ]
                        [ text t
                        , button [ class "tag-remove", onClick (RemoveTag memory.id t) ] [ text "x" ]
                        ]
                )
                memory.tags
            )
        , case editingValue model memory.id "tags" of
            Just val ->
                input
                    [ class "tag-input"
                    , placeholder "New tag..."
                    , value val
                    , onInput EditInput
                    , onBlur (SaveEdit memory.id "tags")
                    , onKeyDown
                        (\keyCode ->
                            if keyCode == 13 then
                                AddTag memory.id val

                            else if keyCode == 27 then
                                CancelEdit

                            else
                                NoOp
                        )
                    , autofocus True
                    ]
                    []

            Nothing ->
                button [ class "btn-icon btn-add-tag", onClick (StartEdit "memory" memory.id "tags" "") ]
                    [ text "+ tag" ]
        ]



-- CREATE FORM MODAL


viewCreateFormModal : Model -> Html Msg
viewCreateFormModal model =
    case model.editing.createForm of
        Nothing ->
            text ""

        Just form ->
            div [ class "modal-overlay", onClick CancelCreateForm ]
                [ div [ class "modal", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                    [ viewCreateFormContent form ]
                ]


viewCreateFormContent : CreateForm -> Html Msg
viewCreateFormContent form =
    case form of
        CreateProjectForm f ->
            div []
                [ h3 [ class "modal-title" ] [ text "New Project" ]
                , div [ class "form-group" ]
                    [ label [ class "form-label" ] [ text "Name" ]
                    , input
                        [ class "form-input"
                        , value f.name
                        , onInput (\s -> UpdateCreateForm (CreateProjectForm { f | name = s }))
                        , placeholder "Project name"
                        , autofocus True
                        , onKeyDown
                            (\keyCode ->
                                if keyCode == 13 then
                                    SubmitCreateForm

                                else if keyCode == 27 then
                                    CancelCreateForm

                                else
                                    NoOp
                            )
                        ]
                        []
                    ]
                , div [ class "modal-actions" ]
                    [ button [ class "btn btn-secondary", onClick CancelCreateForm ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", onClick SubmitCreateForm ] [ text "Create" ]
                    ]
                ]

        CreateMemoryForm f ->
            div []
                [ h3 [ class "modal-title" ] [ text "New Memory" ]
                , div [ class "form-group" ]
                    [ label [ class "form-label" ] [ text "Content" ]
                    , textarea
                        [ class "form-input form-textarea"
                        , value f.content
                        , onInput (\s -> UpdateCreateForm (CreateMemoryForm { f | content = s }))
                        , placeholder "Memory content..."
                        , rows 6
                        , autofocus True
                        ]
                        []
                    ]
                , div [ class "form-group" ]
                    [ label [ class "form-label" ] [ text "Type" ]
                    , select
                        [ class "form-input"
                        , onInput (\s -> UpdateCreateForm (CreateMemoryForm { f | memoryType = Api.memoryTypeFromString s }))
                        ]
                        (List.map
                            (\mt ->
                                let
                                    str =
                                        Api.memoryTypeToString mt
                                in
                                option [ value str, selected (mt == f.memoryType) ]
                                    [ text (str |> String.replace "_" " ") ]
                            )
                            Api.allMemoryTypes
                        )
                    ]
                , div [ class "modal-actions" ]
                    [ button [ class "btn btn-secondary", onClick CancelCreateForm ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", onClick SubmitCreateForm ] [ text "Create" ]
                    ]
                ]

        CreateGroupForm f ->
            div []
                [ h3 [ class "modal-title" ] [ text "New Group" ]
                , div [ class "form-group" ]
                    [ label [ class "form-label" ] [ text "Name" ]
                    , input
                        [ class "form-input"
                        , value f.name
                        , onInput (\s -> UpdateCreateForm (CreateGroupForm { f | name = s }))
                        , placeholder "Group name"
                        , autofocus True
                        , onKeyDown
                            (\keyCode ->
                                if keyCode == 13 then
                                    SubmitCreateForm

                                else if keyCode == 27 then
                                    CancelCreateForm

                                else
                                    NoOp
                            )
                        ]
                        []
                    ]
                , div [ class "form-group" ]
                    [ label [ class "form-label" ] [ text "Description (optional)" ]
                    , input
                        [ class "form-input"
                        , value f.description
                        , onInput (\s -> UpdateCreateForm (CreateGroupForm { f | description = s }))
                        , placeholder "Brief description"
                        , onKeyDown
                            (\keyCode ->
                                if keyCode == 13 then
                                    SubmitCreateForm

                                else if keyCode == 27 then
                                    CancelCreateForm

                                else
                                    NoOp
                            )
                        ]
                        []
                    ]
                , div [ class "modal-actions" ]
                    [ button [ class "btn btn-secondary", onClick CancelCreateForm ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", onClick SubmitCreateForm ] [ text "Create" ]
                    ]
                ]



-- INLINE CREATE


viewInlineCreateMemory : Model -> Html Msg
viewInlineCreateMemory model =
    case model.editing.inlineCreate of
        Just (InlineCreateMemory f) ->
            div [ class "inline-create-row" ]
                [ input
                    [ class "inline-create-input"
                    , Html.Attributes.id "inline-create-input"
                    , placeholder "New memory content..."
                    , value f.content
                    , onInput (\s -> UpdateInlineCreate (InlineCreateMemory { f | content = s }))
                    , onBlur CancelInlineCreate
                    , onKeyDown
                        (\keyCode ->
                            if keyCode == 13 then
                                SubmitInlineCreate

                            else if keyCode == 27 then
                                CancelInlineCreate

                            else
                                NoOp
                        )
                    ]
                    []
                ]

        _ ->
            div [ class "inline-create-row" ]
                [ button
                    [ class "btn-inline-create-top"
                    , onClick (ShowInlineCreate (InlineCreateMemory { content = "" }))
                    ]
                    [ text "+ New Memory" ]
                ]


viewInlineCreateInput : Model -> Maybe String -> String -> Html Msg
viewInlineCreateInput model parentId inputType =
    let
        isActive =
            case model.editing.inlineCreate of
                Just (InlineCreateProject ic) ->
                    inputType == "project" && ic.parentId == parentId

                _ ->
                    False
    in
    if isActive then
        case model.editing.inlineCreate of
            Just (InlineCreateProject f) ->
                div [ class "inline-create-row" ]
                    [ input
                        [ class "inline-create-input"
                        , Html.Attributes.id "inline-create-input"
                        , placeholder "New project name..."
                        , value f.name
                        , onInput (\s -> UpdateInlineCreate (InlineCreateProject { f | name = s }))
                        , onBlur CancelInlineCreate
                        , onKeyDown
                            (\keyCode ->
                                if keyCode == 13 then
                                    SubmitInlineCreate

                                else if keyCode == 27 then
                                    CancelInlineCreate

                                else
                                    NoOp
                            )
                        ]
                        []
                    ]

            _ ->
                text ""

    else if inputType == "project" && parentId == Nothing then
        div [ class "inline-create-row" ]
            [ button
                [ class "btn-inline-create-top"
                , onClick (ShowInlineCreate (InlineCreateProject { parentId = Nothing, name = "" }))
                ]
                [ text "+ New Project" ]
            ]

    else
        text ""


viewInlineCreateInputForParent : Model -> Maybe String -> String -> Html Msg
viewInlineCreateInputForParent model parentId inputType =
    case model.editing.inlineCreate of
        Just (InlineCreateProject f) ->
            if inputType == "project" && f.parentId == parentId then
                div [ class "inline-create-row" ]
                    [ input
                        [ class "inline-create-input"
                        , Html.Attributes.id "inline-create-input"
                        , placeholder "Subproject name..."
                        , value f.name
                        , onInput (\s -> UpdateInlineCreate (InlineCreateProject { f | name = s }))
                        , onBlur CancelInlineCreate
                        , onKeyDown
                            (\keyCode ->
                                if keyCode == 13 then
                                    SubmitInlineCreate

                                else if keyCode == 27 then
                                    CancelInlineCreate

                                else
                                    NoOp
                            )
                        ]
                        []
                    ]

            else
                text ""

        Just (InlineCreateTask f) ->
            if (inputType == "task" && f.projectId == parentId && f.parentId == Nothing)
                || (inputType == "subtask" && f.parentId == parentId)
            then
                div [ class "inline-create-row" ]
                    [ input
                        [ class "inline-create-input"
                        , Html.Attributes.id "inline-create-input"
                        , placeholder
                            (if inputType == "subtask" then
                                "Subtask title..."

                             else
                                "Task title..."
                            )
                        , value f.title
                        , onInput (\s -> UpdateInlineCreate (InlineCreateTask { f | title = s }))
                        , onBlur CancelInlineCreate
                        , onKeyDown
                            (\keyCode ->
                                if keyCode == 13 then
                                    SubmitInlineCreate

                                else if keyCode == 27 then
                                    CancelInlineCreate

                                else
                                    NoOp
                            )
                        ]
                        []
                    ]

            else
                text ""

        Nothing ->
            text ""

        _ ->
            text ""
