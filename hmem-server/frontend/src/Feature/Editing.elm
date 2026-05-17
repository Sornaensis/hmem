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
    , viewStatusSelectWithDisabled
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
import Permissions
import Toast exposing (addToast)
import Types exposing (..)


init : EditingModel
init =
    { editState = Nothing
    , createForm = Nothing
    , inlineCreate = Nothing
    }


type alias MemoryTargetOption =
    { value : String
    , label : String
    }


memoryTargetOptions : Model -> List MemoryTargetOption
memoryTargetOptions model =
    case model.selectedWorkspaceId of
        Nothing ->
            []

        Just wsId ->
            let
                projectOptions =
                    model.projects
                        |> Dict.values
                        |> List.filter (\p -> p.workspaceId == wsId)
                        |> List.sortBy .name
                        |> List.map
                            (\p ->
                                { value = "project:" ++ p.id
                                , label = "Project: " ++ p.name
                                }
                            )

                taskOptions =
                    model.tasks
                        |> Dict.values
                        |> List.filter (\t -> t.workspaceId == wsId)
                        |> List.sortBy .title
                        |> List.map
                            (\t ->
                                { value = "task:" ++ t.id
                                , label = "Task: " ++ t.title
                                }
                            )
            in
            projectOptions ++ taskOptions


selectedMemoryTargetValue : Model -> String -> Maybe String
selectedMemoryTargetValue model target =
    let
        options =
            memoryTargetOptions model
    in
    if List.any (\option -> option.value == target) options then
        Just target

    else
        List.head options |> Maybe.map .value


memoryCreateTargetIds : Model -> String -> Maybe ( Maybe String, Maybe String )
memoryCreateTargetIds model target =
    selectedMemoryTargetValue model target
        |> Maybe.andThen decodeMemoryTargetValue


decodeMemoryTargetValue : String -> Maybe ( Maybe String, Maybe String )
decodeMemoryTargetValue target =
    if String.startsWith "project:" target then
        let
            projectId =
                String.dropLeft (String.length "project:") target
        in
        if String.isEmpty projectId then
            Nothing

        else
            Just ( Just projectId, Nothing )

    else if String.startsWith "task:" target then
        let
            taskId =
                String.dropLeft (String.length "task:") target
        in
        if String.isEmpty taskId then
            Nothing

        else
            Just ( Nothing, Just taskId )

    else
        Nothing


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
                Just (CreateWorkspaceForm f) ->
                    if not (Permissions.canCreateWorkspace model) then
                        addToast Warning "You do not have permission to create workspaces" model

                    else if String.isEmpty (String.trim f.name) then
                        ( model, Cmd.none )

                    else
                        let
                            ghOwner =
                                nonEmptyMaybe f.ghOwner

                            ghRepo =
                                nonEmptyMaybe f.ghRepo

                            ( trackedModel, requestId, clearCmd ) =
                                beginTrackedMutation [] model
                        in
                        ( trackedModel
                        , Cmd.batch
                            [ clearCmd
                            , Api.createWorkspace model.flags.apiUrl (String.trim f.name) f.workspaceType ghOwner ghRepo requestId WorkspaceCreated
                            ]
                        )

                Just (CreateGroupForm f) ->
                    if not (Permissions.isSuperadmin model) then
                        addToast Warning "Superadmin permission is required to manage workspace groups" model

                    else if String.isEmpty (String.trim f.name) then
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
                            if not (Permissions.canEditCurrentWorkspace model) then
                                addToast Warning "Workspace edit permission is required to create projects" model

                            else if String.isEmpty (String.trim f.name) then
                                ( model, Cmd.none )

                            else
                                let
                                    ( trackedModel, requestId, clearCmd ) =
                                        beginTrackedMutation [] model
                                in
                                ( trackedModel, Cmd.batch [ clearCmd, Api.createProject model.flags.apiUrl wsId f.name requestId ProjectCreated ] )

                        ( Just (CreateMemoryForm f), Just wsId ) ->
                            if not (Permissions.canEditCurrentWorkspace model) then
                                addToast Warning "Workspace edit permission is required to create memories" model

                            else if String.isEmpty (String.trim f.content) then
                                ( model, Cmd.none )

                            else
                                case memoryCreateTargetIds model f.target of
                                    Just ( projectId, taskId ) ->
                                        let
                                            ( trackedModel, requestId, clearCmd ) =
                                                beginTrackedMutation [] model
                                        in
                                        ( trackedModel, Cmd.batch [ clearCmd, Api.createMemory model.flags.apiUrl wsId projectId taskId f.content f.memoryType requestId MemoryCreated ] )

                                    Nothing ->
                                        addToast Warning "Select a project or task for this memory" model

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
            if not (Permissions.canEditCurrentWorkspace model) then
                addToast Warning "Workspace edit permission is required to create items" model

            else
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

                    ( Just (InlineCreateMemory { content, target }), Just wsId ) ->
                        if String.isEmpty (String.trim content) then
                            ( model, Cmd.none )

                        else
                            case memoryCreateTargetIds model target of
                                Just ( projectId, taskId ) ->
                                    let
                                        ( trackedModel, requestId, clearCmd ) =
                                            beginTrackedMutation [] model
                                    in
                                    ( trackedModel, Cmd.batch [ clearCmd, Api.createMemory model.flags.apiUrl wsId projectId taskId content Api.ShortTerm requestId MemoryCreated ] )

                                Nothing ->
                                    addToast Warning "Select a project or task for this memory" model

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


nonEmptyMaybe : String -> Maybe String
nonEmptyMaybe raw =
    let
        trimmed =
            String.trim raw
    in
    if String.isEmpty trimmed then
        Nothing

    else
        Just trimmed


workspaceTypeFromString : String -> Api.WorkspaceType
workspaceTypeFromString raw =
    case raw of
        "planning" ->
            Api.Planning

        "personal" ->
            Api.Personal

        "organization" ->
            Api.Organization

        _ ->
            Api.Repository


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
    if not (Permissions.canEditCurrentWorkspace model) then
        span [ class "editable-text readonly", title "Read-only" ]
            [ text
                (if String.isEmpty currentValue then
                    "(empty)"

                 else
                    currentValue
                )
            ]

    else
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
    if not (Permissions.canEditCurrentWorkspace model) then
        div [ class "editable-textarea readonly", title "Read-only" ]
            [ if String.isEmpty currentValue then
                text ""

              else
                viewMarkdownContent currentValue
            ]

    else
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


viewStatusSelect : Model -> String -> String -> String -> List a -> (a -> String) -> (String -> a -> Msg) -> Html Msg
viewStatusSelect model entityType entityId currentStr allValues toString toMsg =
    viewStatusSelectWithDisabled model entityType entityId currentStr allValues toString (\_ -> Nothing) toMsg


viewStatusSelectWithDisabled : Model -> String -> String -> String -> List a -> (a -> String) -> (a -> Maybe String) -> (String -> a -> Msg) -> Html Msg
viewStatusSelectWithDisabled model entityType entityId currentStr allValues toString disabledReason toMsg =
    select
        [ class ("status-select badge badge-" ++ currentStr)
        , disabled (not (Permissions.canEditCurrentWorkspace model))
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

                    reason =
                        disabledReason v
                in
                option
                    ([ value str, selected (str == currentStr) ]
                        ++ (case reason of
                                Just message ->
                                    [ disabled True, title message ]

                                Nothing ->
                                    []
                           )
                    )
                    [ text (str |> String.replace "_" " ") ]
            )
            allValues
        )


viewPrioritySelect : Model -> String -> String -> Int -> (String -> Int -> Msg) -> Html Msg
viewPrioritySelect model entityType entityId currentPri toMsg =
    select
        [ class "priority-select"
        , title "Priority"
        , disabled (not (Permissions.canEditCurrentWorkspace model))
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


viewImportanceSelect : Model -> String -> Int -> Html Msg
viewImportanceSelect model memId currentImp =
    select
        [ class "priority-select"
        , title "Importance"
        , disabled (not (Permissions.canEditCurrentWorkspace model))
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


viewMemoryTypeSelect : Model -> String -> Api.MemoryType -> Html Msg
viewMemoryTypeSelect model memId currentType =
    select
        [ class ("status-select badge badge-" ++ Api.memoryTypeToString currentType)
        , disabled (not (Permissions.canEditCurrentWorkspace model))
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
    if Permissions.canEditCurrentWorkspace model then
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

    else
        div [ class "tag-editor" ]
            [ div [ class "tag-list" ]
                (List.map (\t -> span [ class "tag" ] [ text t ]) memory.tags)
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
                    [ viewCreateFormContent model form ]
                ]


viewCreateFormContent : Model -> CreateForm -> Html Msg
viewCreateFormContent model form =
    case form of
        CreateWorkspaceForm f ->
            div []
                [ h3 [ class "modal-title" ] [ text "New Workspace" ]
                , if Permissions.canCreateWorkspace model then
                    text ""

                  else
                    div [ class "empty-state" ] [ text "You do not have permission to create workspaces." ]
                , div [ class "form-group" ]
                    [ label [ class "form-label" ] [ text "Name" ]
                    , input
                        [ class "form-input"
                        , value f.name
                        , onInput (\s -> UpdateCreateForm (CreateWorkspaceForm { f | name = s }))
                        , placeholder "Workspace name"
                        , autofocus True
                        , disabled (not (Permissions.canCreateWorkspace model))
                        ]
                        []
                    ]
                , div [ class "form-group" ]
                    [ label [ class "form-label" ] [ text "Type" ]
                    , select
                        [ class "form-input"
                        , disabled (not (Permissions.canCreateWorkspace model))
                        , onInput (\s -> UpdateCreateForm (CreateWorkspaceForm { f | workspaceType = workspaceTypeFromString s }))
                        ]
                        (List.map
                            (\wt ->
                                let
                                    str =
                                        Api.workspaceTypeToString wt
                                in
                                option [ value str, selected (wt == f.workspaceType) ] [ text str ]
                            )
                            Api.allWorkspaceTypes
                        )
                    ]
                , div [ class "form-group" ]
                    [ label [ class "form-label" ] [ text "GitHub owner (optional)" ]
                    , input [ class "form-input", value f.ghOwner, disabled (not (Permissions.canCreateWorkspace model)), onInput (\s -> UpdateCreateForm (CreateWorkspaceForm { f | ghOwner = s })) ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ class "form-label" ] [ text "GitHub repo (optional)" ]
                    , input [ class "form-input", value f.ghRepo, disabled (not (Permissions.canCreateWorkspace model)), onInput (\s -> UpdateCreateForm (CreateWorkspaceForm { f | ghRepo = s })) ] []
                    ]
                , div [ class "modal-actions" ]
                    [ button [ class "btn btn-secondary", onClick CancelCreateForm ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", disabled (not (Permissions.canCreateWorkspace model)), onClick SubmitCreateForm ] [ text "Create" ]
                    ]
                ]

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
            let
                targetOptions =
                    memoryTargetOptions model

                selectedTarget =
                    selectedMemoryTargetValue model f.target |> Maybe.withDefault ""
            in
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
                    [ label [ class "form-label" ] [ text "Link target" ]
                    , if List.isEmpty targetOptions then
                        div [ class "form-hint" ] [ text "Create a project or task before adding memories." ]

                      else
                        select
                            [ class "form-input"
                            , value selectedTarget
                            , onInput (\s -> UpdateCreateForm (CreateMemoryForm { f | target = s }))
                            ]
                            (List.map
                                (\optionItem ->
                                    option [ value optionItem.value, selected (optionItem.value == selectedTarget) ]
                                        [ text optionItem.label ]
                                )
                                targetOptions
                            )
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
                    , button [ class "btn btn-primary", disabled (List.isEmpty targetOptions), onClick SubmitCreateForm ] [ text "Create" ]
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
    if not (Permissions.canEditCurrentWorkspace model) then
        div [ class "inline-create-row empty-state" ] [ text "Read-only workspace" ]

    else
        let
            targetOptions =
                memoryTargetOptions model

            defaultTarget =
                selectedMemoryTargetValue model "" |> Maybe.withDefault ""
        in
        if List.isEmpty targetOptions then
            div [ class "inline-create-row empty-state" ] [ text "Create a project or task before adding memories." ]

        else
            case model.editing.inlineCreate of
                Just (InlineCreateMemory f) ->
                    let
                        selectedTarget =
                            selectedMemoryTargetValue model f.target |> Maybe.withDefault defaultTarget
                    in
                    div [ class "inline-create-row" ]
                        [ select
                            [ class "inline-create-input"
                            , value selectedTarget
                            , onInput (\s -> UpdateInlineCreate (InlineCreateMemory { f | target = s }))
                            ]
                            (List.map
                                (\optionItem ->
                                    option [ value optionItem.value, selected (optionItem.value == selectedTarget) ]
                                        [ text optionItem.label ]
                                )
                                targetOptions
                            )
                        , input
                            [ class "inline-create-input"
                            , Html.Attributes.id "inline-create-input"
                            , placeholder "New memory content..."
                            , value f.content
                            , onInput (\s -> UpdateInlineCreate (InlineCreateMemory { f | content = s }))
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
                            , onClick (ShowInlineCreate (InlineCreateMemory { content = "", target = defaultTarget }))
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
    if not (Permissions.canEditCurrentWorkspace model) then
        text ""

    else if isActive then
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
    if not (Permissions.canEditCurrentWorkspace model) then
        text ""

    else
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
