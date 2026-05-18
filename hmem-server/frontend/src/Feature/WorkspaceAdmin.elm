module Feature.WorkspaceAdmin exposing
    ( handleEscape
    , init
    , update
    , viewPermissionSummary
    , viewPurgeConfirmModal
    , viewWorkspaceAdminPanel
    )

import Api
import Browser.Navigation as Nav
import Dict
import Helpers exposing (beginTrackedMutation, formatDate, trackLocalMutation)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Permissions
import Toast exposing (addToast)
import Types exposing (..)


init : WorkspaceAdminModel
init =
    { memberships = Dict.empty
    , loadingMemberships = Dict.empty
    , membershipUserId = ""
    , membershipRole = "read"
    , purgeConfirmation = Nothing
    }


handleEscape : Model -> Maybe Model
handleEscape model =
    if model.workspaceAdmin.purgeConfirmation /= Nothing then
        Just (updateWorkspaceAdmin (\admin -> { admin | purgeConfirmation = Nothing }) model)

    else
        Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWorkspaceMemberships wsId result ->
            case result of
                Ok paginated ->
                    ( updateWorkspaceAdmin
                        (\admin ->
                            { admin
                                | memberships = Dict.insert wsId paginated.items admin.memberships
                                , loadingMemberships = Dict.insert wsId False admin.loadingMemberships
                            }
                        )
                        model
                    , Cmd.none
                    )

                Err _ ->
                    addToast Error "Failed to load workspace memberships"
                        (updateWorkspaceAdmin (\admin -> { admin | loadingMemberships = Dict.insert wsId False admin.loadingMemberships }) model)

        UpdateMembershipUserId value ->
            ( updateWorkspaceAdmin (\admin -> { admin | membershipUserId = value }) model, Cmd.none )

        UpdateMembershipRole role ->
            ( updateWorkspaceAdmin (\admin -> { admin | membershipRole = role }) model, Cmd.none )

        SubmitWorkspaceMembership wsId ->
            if not (Permissions.canAdminCurrentWorkspace model) then
                addToast Warning "Workspace admin permission is required to manage memberships" model

            else
                let
                    userId =
                        String.trim model.workspaceAdmin.membershipUserId
                in
                if String.isEmpty userId then
                    addToast Warning "Enter a user ID before granting a role" model

                else
                    let
                        ( trackedModel, requestId, clearCmd ) =
                            beginTrackedMutation [ wsId, userId ] model
                    in
                    ( trackedModel
                    , Cmd.batch
                        [ clearCmd
                        , Api.upsertWorkspaceMembership model.flags.apiUrl wsId userId model.workspaceAdmin.membershipRole requestId (WorkspaceMembershipSaved wsId)
                        ]
                    )

        WorkspaceMembershipSaved wsId result ->
            case result of
                Ok membership ->
                    let
                        existing =
                            Dict.get wsId model.workspaceAdmin.memberships |> Maybe.withDefault []

                        withoutUser =
                            List.filter (\m -> m.userId /= membership.userId) existing

                        updatedAdmin admin =
                            { admin
                                | memberships = Dict.insert wsId (membership :: withoutUser) admin.memberships
                                , membershipUserId = ""
                            }

                        ( trackedModel, trackCmd ) =
                            trackLocalMutation membership.userId (updateWorkspaceAdmin updatedAdmin model)

                        ( toastedModel, toastCmd ) =
                            addToast Success "Workspace membership saved" trackedModel
                    in
                    ( toastedModel, Cmd.batch [ trackCmd, toastCmd, Api.fetchSessionContext model.flags.apiUrl (Just wsId) (GotSessionContext (Just wsId)) ] )

                Err _ ->
                    addToast Error "Failed to save workspace membership" model

        RemoveWorkspaceMembership wsId userId ->
            if not (Permissions.canAdminCurrentWorkspace model) then
                addToast Warning "Workspace admin permission is required to manage memberships" model

            else
                let
                    ( trackedModel, requestId, clearCmd ) =
                        beginTrackedMutation [ wsId, userId ] model
                in
                ( trackedModel
                , Cmd.batch
                    [ clearCmd
                    , Api.deleteWorkspaceMembership model.flags.apiUrl wsId userId requestId (WorkspaceMembershipDeleted wsId userId)
                    ]
                )

        WorkspaceMembershipDeleted wsId userId result ->
            case result of
                Ok () ->
                    let
                        existing =
                            Dict.get wsId model.workspaceAdmin.memberships |> Maybe.withDefault []

                        updatedAdmin admin =
                            { admin | memberships = Dict.insert wsId (List.filter (\m -> m.userId /= userId) existing) admin.memberships }

                        ( toastedModel, toastCmd ) =
                            addToast Success "Workspace membership removed" (updateWorkspaceAdmin updatedAdmin model)
                    in
                    ( toastedModel, Cmd.batch [ toastCmd, Api.fetchSessionContext model.flags.apiUrl (Just wsId) (GotSessionContext (Just wsId)) ] )

                Err _ ->
                    addToast Error "Failed to remove workspace membership" model

        ConfirmWorkspacePurge wsId ->
            if Permissions.canAdminCurrentWorkspace model then
                ( updateWorkspaceAdmin (\admin -> { admin | purgeConfirmation = Just wsId }) model, Cmd.none )

            else
                addToast Warning "Workspace admin permission is required to purge a workspace" model

        CancelWorkspacePurge ->
            ( updateWorkspaceAdmin (\admin -> { admin | purgeConfirmation = Nothing }) model, Cmd.none )

        PerformWorkspacePurge ->
            if not (Permissions.canAdminCurrentWorkspace model) then
                addToast Warning "Workspace admin permission is required to purge a workspace"
                    (updateWorkspaceAdmin (\admin -> { admin | purgeConfirmation = Nothing }) model)

            else
                case model.workspaceAdmin.purgeConfirmation of
                    Just wsId ->
                        let
                            ( trackedModel, requestId, clearCmd ) =
                                beginTrackedMutation [ wsId ] model
                        in
                        ( trackedModel
                        , Cmd.batch
                            [ clearCmd
                            , Api.deleteWorkspace model.flags.apiUrl wsId requestId (WorkspaceDeletedForPurge wsId)
                            ]
                        )

                    Nothing ->
                        ( model, Cmd.none )

        WorkspaceDeletedForPurge wsId result ->
            case result of
                Ok () ->
                    let
                        ( trackedModel, requestId, clearCmd ) =
                            beginTrackedMutation [ wsId ] model
                    in
                    ( trackedModel
                    , Cmd.batch
                        [ clearCmd
                        , Api.purgeWorkspace model.flags.apiUrl wsId requestId (WorkspacePurged wsId)
                        ]
                    )

                Err _ ->
                    addToast Error "Failed to delete workspace before purge"
                        (updateWorkspaceAdmin (\admin -> { admin | purgeConfirmation = Nothing }) model)

        WorkspaceDeleted wsId result ->
            case result of
                Ok () ->
                    let
                        ( toastedModel, toastCmd ) =
                            addToast Success "Workspace deleted" { model | workspaces = Dict.remove wsId model.workspaces, selectedWorkspaceId = Nothing }
                    in
                    ( toastedModel, Cmd.batch [ toastCmd, Nav.pushUrl model.key "/" ] )

                Err _ ->
                    addToast Error "Failed to delete workspace" model

        WorkspacePurged wsId result ->
            case result of
                Ok () ->
                    let
                        ( toastedModel, toastCmd ) =
                            addToast Success "Workspace purged" { model | workspaces = Dict.remove wsId model.workspaces, selectedWorkspaceId = Nothing }
                    in
                    ( updateWorkspaceAdmin (\admin -> { admin | purgeConfirmation = Nothing }) toastedModel
                    , Cmd.batch [ toastCmd, Nav.pushUrl model.key "/" ]
                    )

                Err _ ->
                    let
                        ( toastedModel, toastCmd ) =
                            addToast Error "Workspace was deleted, but purge failed. It has been removed from the active workspace list."
                                { model | workspaces = Dict.remove wsId model.workspaces, selectedWorkspaceId = Nothing }
                    in
                    ( updateWorkspaceAdmin (\admin -> { admin | purgeConfirmation = Nothing }) toastedModel
                    , Cmd.batch [ toastCmd, Nav.pushUrl model.key "/" ]
                    )

        _ ->
            ( model, Cmd.none )


viewPermissionSummary : Model -> Html Msg
viewPermissionSummary model =
    if not (Permissions.shouldShowPermissionSummary model) then
        text ""

    else
        case model.sessionContext of
            Nothing ->
                div [ class "permission-summary permission-summary-loading" ]
                    [ text "Loading permissions..." ]

            Just session ->
                div [ class "permission-summary" ]
                    [ span [] [ text ("Signed in as " ++ session.principal.actorLabel) ]
                    , span [] [ text ("Role: " ++ Permissions.currentWorkspaceRoleLabel model) ]
                    , if session.globalPermissions.createWorkspace then
                        span [ class "permission-pill" ] [ text "create workspace" ]

                      else
                        text ""
                    , if session.globalPermissions.superadmin then
                        span [ class "permission-pill permission-pill-superadmin" ] [ text "superadmin" ]

                      else
                        text ""
                    ]


viewWorkspaceAdminPanel : Api.Workspace -> Model -> Html Msg
viewWorkspaceAdminPanel ws model =
    let
        membershipLoaded =
            Dict.member ws.id model.workspaceAdmin.memberships

        workspaceSessionMatches =
            model.sessionContext
                |> Maybe.andThen .workspace
                |> Maybe.map (\workspaceContext -> workspaceContext.workspaceId == ws.id)
                |> Maybe.withDefault False
    in
    if Permissions.hasImplicitLocalSuperadmin model && Permissions.canAdminCurrentWorkspace model then
        div [ class "workspace-admin-panel workspace-admin-panel-local" ]
            [ viewDangerZone ws model ]

    else if Permissions.canAdminCurrentWorkspace model && (workspaceSessionMatches || membershipLoaded) then
        div [ class "workspace-admin-panel" ]
            [ h3 [] [ text "Workspace administration" ]
            , p [ class "help-text" ] [ text "Client affordances reflect server-provided permissions; the server remains authoritative for every action." ]
            , viewMembershipManager ws model
            , viewDangerZone ws model
            ]

    else if model.sessionContext == Nothing || (Permissions.isSuperadmin model && not workspaceSessionMatches && not membershipLoaded) then
        div [ class "workspace-admin-panel empty-state" ]
            [ text "Loading workspace permissions..." ]

    else
        div [ class "workspace-admin-panel empty-state" ]
            [ text "Workspace admin controls are hidden because your current role is "
            , strong [] [ text (Permissions.currentWorkspaceRoleLabel model) ]
            , text "."
            ]


viewMembershipManager : Api.Workspace -> Model -> Html Msg
viewMembershipManager ws model =
    if not (Permissions.shouldShowMembershipAdmin model) then
        text ""

    else
        let
            memberships =
                Dict.get ws.id model.workspaceAdmin.memberships |> Maybe.withDefault []

            loading =
                Dict.get ws.id model.workspaceAdmin.loadingMemberships |> Maybe.withDefault False
        in
        div [ class "workspace-memberships" ]
            [ h4 [] [ text "Memberships" ]
            , if loading && List.isEmpty memberships then
                div [ class "loading-indicator" ] [ text "Loading memberships..." ]

              else if List.isEmpty memberships then
                div [ class "empty-state" ] [ text "No explicit memberships found. Superadmins may still have access." ]

              else
                div [ class "membership-list" ] (List.map (viewMembershipRow ws.id) memberships)
            , div [ class "membership-form" ]
                [ input
                    [ class "form-input"
                    , placeholder "User UUID"
                    , value model.workspaceAdmin.membershipUserId
                    , onInput UpdateMembershipUserId
                    ]
                    []
                , select [ class "form-input", value model.workspaceAdmin.membershipRole, onInput UpdateMembershipRole ]
                    [ option [ value "read" ] [ text "read" ]
                    , option [ value "edit" ] [ text "edit" ]
                    , option [ value "admin" ] [ text "admin" ]
                    ]
                , button [ class "btn btn-primary", onClick (SubmitWorkspaceMembership ws.id) ]
                    [ text "Grant / update" ]
                ]
            ]


viewMembershipRow : String -> Api.WorkspaceMembership -> Html Msg
viewMembershipRow wsId membership =
    div [ class "membership-row" ]
        [ span [ class "membership-user" ] [ text membership.userId ]
        , span [ class ("badge badge-" ++ membership.role) ] [ text membership.role ]
        , span [ class "membership-updated" ] [ text ("Updated " ++ formatDate membership.updatedAt) ]
        , button [ class "btn-small btn-danger-subtle", onClick (RemoveWorkspaceMembership wsId membership.userId) ] [ text "Remove" ]
        ]


viewDangerZone : Api.Workspace -> Model -> Html Msg
viewDangerZone ws _ =
    div [ class "workspace-danger-zone" ]
        [ h4 [] [ text "Danger zone" ]
        , p [ class "help-text" ] [ text "Deleting is reversible from the API. Purging first deletes this active workspace, then permanently removes it." ]
        , button [ class "btn btn-danger", onClick (ConfirmDelete "workspace" ws.id) ] [ text "Delete workspace" ]
        , button [ class "btn btn-danger", onClick (ConfirmWorkspacePurge ws.id), title "Permanently delete and purge this workspace" ] [ text "Purge workspace" ]
        ]


viewPurgeConfirmModal : Model -> Html Msg
viewPurgeConfirmModal model =
    case model.workspaceAdmin.purgeConfirmation of
        Nothing ->
            text ""

        Just wsId ->
            div [ class "modal-overlay", onClick CancelWorkspacePurge ]
                [ div [ class "modal", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                    [ h3 [ class "modal-title" ] [ text "Permanently purge workspace?" ]
                    , p [] [ text "This will delete the workspace and then permanently purge it. This cannot be undone." ]
                    , p [ class "card-id" ] [ text wsId ]
                    , div [ class "modal-actions" ]
                        [ button [ class "btn btn-secondary", onClick CancelWorkspacePurge ] [ text "Cancel" ]
                        , button [ class "btn btn-danger", onClick PerformWorkspacePurge ] [ text "Purge" ]
                        ]
                    ]
                ]


updateWorkspaceAdmin : (WorkspaceAdminModel -> WorkspaceAdminModel) -> Model -> Model
updateWorkspaceAdmin fn model =
    { model | workspaceAdmin = fn model.workspaceAdmin }
