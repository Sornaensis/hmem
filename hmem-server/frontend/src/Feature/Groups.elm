module Feature.Groups exposing (update, viewHomePage, viewSidebar)

import Api
import Dict
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Toast exposing (addToast)
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWorkspaceGroups result ->
            case result of
                Ok paginated ->
                    let
                        groups =
                            indexBy .id paginated.items

                        fetchMembersCmds =
                            paginated.items
                                |> List.map (\g -> Api.fetchGroupMembers model.flags.apiUrl g.id (GotGroupMembers g.id))
                    in
                    ( { model | workspaceGroups = groups }
                    , Cmd.batch fetchMembersCmds
                    )

                Err _ ->
                    addToast Error "Failed to load workspace groups" model

        GotGroupMembers groupId result ->
            case result of
                Ok memberIds ->
                    ( { model | groupMembers = Dict.insert groupId memberIds model.groupMembers }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        CreateWorkspaceGroup name ->
            ( model
            , Api.createWorkspaceGroup model.flags.apiUrl name Nothing WorkspaceGroupCreated
            )

        WorkspaceGroupCreated result ->
            case result of
                Ok group ->
                    addToast Success ("Group \"" ++ group.name ++ "\" created")
                        { model | workspaceGroups = Dict.insert group.id group model.workspaceGroups }

                Err _ ->
                    addToast Error "Failed to create workspace group" model

        DeleteWorkspaceGroup groupId ->
            ( model
            , Api.deleteWorkspaceGroup model.flags.apiUrl groupId (WorkspaceGroupDeleted groupId)
            )

        WorkspaceGroupDeleted groupId result ->
            case result of
                Ok () ->
                    addToast Success "Group deleted"
                        { model
                            | workspaceGroups = Dict.remove groupId model.workspaceGroups
                            , groupMembers = Dict.remove groupId model.groupMembers
                            , managingGroup =
                                case model.managingGroup of
                                    Just st ->
                                        if st.groupId == groupId then
                                            Nothing

                                        else
                                            model.managingGroup

                                    Nothing ->
                                        Nothing
                        }

                Err _ ->
                    addToast Error "Failed to delete group" model

        ToggleManageGroup groupId ->
            case model.managingGroup of
                Just st ->
                    if st.groupId == groupId then
                        ( { model | managingGroup = Nothing }, Cmd.none )

                    else
                        ( { model | managingGroup = Just { groupId = groupId, addingWorkspace = False } }, Cmd.none )

                Nothing ->
                    ( { model | managingGroup = Just { groupId = groupId, addingWorkspace = False } }, Cmd.none )

        AddWorkspaceToGroup groupId workspaceId ->
            ( model
            , Api.addGroupMember model.flags.apiUrl groupId workspaceId (GroupMembershipDone groupId)
            )

        RemoveWorkspaceFromGroup groupId workspaceId ->
            ( model
            , Api.removeGroupMember model.flags.apiUrl groupId workspaceId (GroupMembershipDone groupId)
            )

        GroupMembershipDone groupId result ->
            case result of
                Ok () ->
                    ( model
                    , Api.fetchGroupMembers model.flags.apiUrl groupId (GotGroupMembers groupId)
                    )

                Err _ ->
                    addToast Error "Failed to update group membership" model

        _ ->
            ( model, Cmd.none )



-- SIDEBAR


viewSidebar : Model -> Html Msg
viewSidebar model =
    let
        allWs =
            Dict.values model.workspaces |> List.sortBy .name

        groupedWsIds =
            model.groupMembers
                |> Dict.values
                |> List.concat
                |> List.foldl (\wsId acc -> Dict.insert wsId True acc) Dict.empty

        ungroupedWs =
            allWs |> List.filter (\ws -> not (Dict.member ws.id groupedWsIds))

        groups =
            model.workspaceGroups |> Dict.values |> List.sortBy .name
    in
    nav [ class "sidebar" ]
        [ div [ class "sidebar-header" ]
            [ a [ href "/", class "sidebar-logo" ] [ text "hmem" ] ]
        , if not (List.isEmpty groups) then
            div []
                (List.map (viewSidebarGroup model) groups)

          else
            text ""
        , if not (List.isEmpty ungroupedWs) then
            div [ class "sidebar-section" ]
                [ div [ class "sidebar-section-title" ] [ text "Workspaces" ]
                , if model.loadingWorkspaces then
                    div [ class "sidebar-loading" ] [ text "Loading..." ]

                  else
                    ul [ class "sidebar-nav" ]
                        (ungroupedWs |> List.map (viewSidebarWorkspace model.selectedWorkspaceId))
                ]

          else if List.isEmpty groups then
            div [ class "sidebar-section" ]
                [ div [ class "sidebar-section-title" ] [ text "Workspaces" ]
                , if model.loadingWorkspaces then
                    div [ class "sidebar-loading" ] [ text "Loading..." ]

                  else
                    div [ class "sidebar-empty" ] [ text "No workspaces" ]
                ]

          else
            text ""
        , div [ class "sidebar-section" ]
            [ ul [ class "sidebar-nav" ]
                [ li []
                    [ a [ href "/memory-graph", class "sidebar-link" ]
                        [ span [ class "sidebar-icon" ] [ text "◉" ]
                        , text "Knowledge Graph"
                        ]
                    ]
                , li []
                    [ a [ href "/audit", class "sidebar-link" ]
                        [ span [ class "sidebar-icon icon-audit" ] []
                        , text "Audit Log"
                        ]
                    ]
                ]
            ]
        ]


viewSidebarGroup : Model -> Api.WorkspaceGroup -> Html Msg
viewSidebarGroup model group =
    let
        memberIds =
            Dict.get group.id model.groupMembers |> Maybe.withDefault []

        memberWs =
            memberIds
                |> List.filterMap (\wsId -> Dict.get wsId model.workspaces)
                |> List.sortBy .name

        collapsed =
            isCollapsed model ("group-" ++ group.id)
    in
    div [ class "sidebar-section" ]
        [ div
            [ class "sidebar-section-title sidebar-group-title"
            , onClick (ToggleTreeNode ("group-" ++ group.id))
            , style "cursor" "pointer"
            , style "display" "flex"
            , style "align-items" "center"
            , style "gap" "0.25rem"
            ]
            [ span [ class "collapse-toggle" ]
                [ text
                    (if collapsed then
                        "▸"

                     else
                        "▾"
                    )
                ]
            , text group.name
            , span [ style "margin-left" "auto", style "font-size" "0.75rem", style "opacity" "0.6" ]
                [ text (String.fromInt (List.length memberWs)) ]
            ]
        , if collapsed then
            text ""

          else
            ul [ class "sidebar-nav" ]
                (memberWs |> List.map (viewSidebarWorkspace model.selectedWorkspaceId))
        ]


viewSidebarWorkspace : Maybe String -> Api.Workspace -> Html Msg
viewSidebarWorkspace selectedId ws =
    let
        isSelected =
            selectedId == Just ws.id

        cls =
            if isSelected then
                "sidebar-link active"

            else
                "sidebar-link"

        icon =
            case ws.workspaceType of
                Api.Repository ->
                    "⌂"

                Api.Planning ->
                    "◇"

                Api.Personal ->
                    "●"

                Api.Organization ->
                    "◆"
    in
    li []
        [ a [ href ("/workspace/" ++ ws.id), class cls ]
            [ span [ class "sidebar-icon" ] [ text icon ]
            , text ws.name
            ]
        ]



-- HOME PAGE


viewHomePage : Model -> Html Msg
viewHomePage model =
    let
        allWs =
            model.workspaces |> Dict.values |> List.sortBy .name

        groups =
            model.workspaceGroups |> Dict.values |> List.sortBy .name

        groupedWsIds =
            model.groupMembers
                |> Dict.values
                |> List.concat
                |> List.foldl (\wsId acc -> Dict.insert wsId True acc) Dict.empty

        ungroupedWs =
            allWs |> List.filter (\ws -> not (Dict.member ws.id groupedWsIds))
    in
    div [ class "page" ]
        [ h2 [ style "display" "flex", style "align-items" "center", style "gap" "0.75rem" ]
            [ text "Workspaces"
            , button
                [ class "btn-small"
                , onClick (ShowCreateForm (CreateGroupForm { name = "", description = "" }))
                ]
                [ text "+ Group" ]
            ]
        , if model.loadingWorkspaces then
            div [ class "loading-indicator" ] [ text "Loading workspaces..." ]

          else
            div []
                (List.map (viewHomeGroup model) groups
                    ++ [ if not (List.isEmpty ungroupedWs) then
                            div []
                                [ if not (List.isEmpty groups) then
                                    h3 [ style "margin-top" "1.5rem", style "margin-bottom" "0.75rem" ] [ text "Ungrouped" ]

                                  else
                                    text ""
                                , div [ class "card-grid" ]
                                    (ungroupedWs |> List.map viewWorkspaceCard)
                                ]

                         else
                            text ""
                       ]
                )
        ]


viewHomeGroup : Model -> Api.WorkspaceGroup -> Html Msg
viewHomeGroup model group =
    let
        memberIds =
            Dict.get group.id model.groupMembers |> Maybe.withDefault []

        memberWs =
            memberIds
                |> List.filterMap (\wsId -> Dict.get wsId model.workspaces)
                |> List.sortBy .name

        isManaging =
            case model.managingGroup of
                Just st ->
                    st.groupId == group.id

                Nothing ->
                    False

        allGroupedIds =
            model.groupMembers
                |> Dict.values
                |> List.concat
                |> List.foldl (\wsId acc -> Dict.insert wsId True acc) Dict.empty

        ungroupedWs =
            model.workspaces
                |> Dict.values
                |> List.filter (\ws -> not (Dict.member ws.id allGroupedIds))
                |> List.sortBy .name
    in
    div [ class "group-section" ]
        [ div [ class "group-header" ]
            [ h3 [ class "group-title" ]
                [ text group.name
                , case group.description of
                    Just desc ->
                        span [ class "group-description" ] [ text ("— " ++ desc) ]

                    Nothing ->
                        text ""
                ]
            , div [ class "group-actions" ]
                [ button
                    [ class
                        (if isManaging then
                            "btn-small group-manage-active"

                         else
                            "btn-small"
                        )
                    , onClick (ToggleManageGroup group.id)
                    , title
                        (if isManaging then
                            "Done managing"

                         else
                            "Manage members"
                        )
                    ]
                    [ text
                        (if isManaging then
                            "Done"

                         else
                            "Manage"
                        )
                    ]
                , button
                    [ class "btn-small btn-danger-subtle"
                    , onClick (ConfirmDelete "group" group.id)
                    , title "Delete group"
                    ]
                    [ text "Delete" ]
                ]
            ]
        , if isManaging then
            div [ class "group-manage-panel" ]
                [ if not (List.isEmpty memberWs) then
                    div [ class "group-member-list" ]
                        (memberWs
                            |> List.map
                                (\ws ->
                                    span [ class "group-member-chip" ]
                                        [ text ws.name
                                        , button
                                            [ class "chip-remove"
                                            , onClick (RemoveWorkspaceFromGroup group.id ws.id)
                                            , title ("Remove " ++ ws.name)
                                            ]
                                            [ text "×" ]
                                        ]
                                )
                        )

                  else
                    text ""
                , if not (List.isEmpty ungroupedWs) then
                    div [ class "group-add-section" ]
                        [ span [ class "group-add-label" ] [ text "Add workspace:" ]
                        , div [ class "group-add-options" ]
                            (ungroupedWs
                                |> List.map
                                    (\ws ->
                                        button
                                            [ class "group-add-chip"
                                            , onClick (AddWorkspaceToGroup group.id ws.id)
                                            ]
                                            [ text ("+ " ++ ws.name) ]
                                    )
                            )
                        ]

                  else
                    div [ class "group-add-section" ]
                        [ span [ class "group-add-label", style "opacity" "0.5" ] [ text "All workspaces are grouped" ] ]
                ]

          else
            text ""
        , if List.isEmpty memberWs then
            div [ class "empty-state" ] [ text "No workspaces in this group." ]

          else
            div [ class "card-grid" ]
                (memberWs |> List.map viewWorkspaceCard)
        ]


viewWorkspaceCard : Api.Workspace -> Html Msg
viewWorkspaceCard ws =
    div [ class "card clickable", onClick (SelectWorkspace ws.id) ]
        [ div [ class "card-header" ]
            [ span [ class "card-title" ] [ text ws.name ]
            , span [ class ("badge badge-" ++ Api.workspaceTypeToString ws.workspaceType) ]
                [ text (Api.workspaceTypeToString ws.workspaceType) ]
            ]
        , div [ class "card-body" ]
            [ text ""
            ]
        ]
