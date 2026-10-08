module Feature.WorkspaceAdmin exposing
    ( handleEscape
    , init
    , ensureMemberships
    , reconcileAuthority
    , reconcileSessionAuthority
    , retireSessionState
    , acceptCanonicalMemberships
    , update
    , viewPermissionSummary
    , viewDeleteConfirmModal
    , viewPurgeConfirmModal
    , viewWorkspaceAdminPanel
    )

import Api
import Dict
import Helpers exposing (beginTrackedMutation, formatDate, pushUrl, trackLocalMutation)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Set
import Permissions
import Toast exposing (addToast)
import Types exposing (..)


init : WorkspaceAdminModel
init =
    { memberships = Dict.empty
    , loadingMemberships = Dict.empty
    , membershipUserId = ""
    , membershipRole = "read"
    , owner = Nothing
    , nextRequestToken = 0
    , activeListRequest = Nothing
    , activeMutation = Nothing
    , authorizationPending = Nothing
    , authorizationFailure = Nothing
    , membershipErrors = Dict.empty
    , mutationError = Nothing
    , purgeConfirmation = Nothing
    }


sessionKey : Model -> String
sessionKey model =
    model.sessionContext
        |> Maybe.map (\session -> Encode.encode 0 (Encode.list Encode.string
            [ session.authMode, session.principal.actorType, session.principal.actorId, session.principal.authority
            , Maybe.withDefault "" session.principal.grantUserId
            , Permissions.currentWorkspaceRoleLabel model
            ]))
        |> Maybe.withDefault ""

canManageTarget : String -> Model -> Bool
canManageTarget workspaceId model =
    Permissions.canViewWorkspaceAdministration model
        && model.selectedWorkspaceId == Just workspaceId
        && model.page == WorkspacePage workspaceId

reconcileAuthority : Model -> Model
reconcileAuthority model =
    let
        admin = model.workspaceAdmin
        owner =
            if Permissions.canViewWorkspaceAdministration model then
                model.selectedWorkspaceId |> Maybe.map (\workspaceId -> { workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch, sessionKey = sessionKey model })
            else Nothing
        samePrincipal =
            case ( admin.owner, owner ) of
                ( Just previous, Just current ) -> previous.workspaceId == current.workspaceId && previous.sessionKey == current.sessionKey
                _ -> False
    in
    if admin.owner == owner then model
    else
        { model | workspaceAdmin =
            { admin | owner = owner
                , memberships = if samePrincipal then admin.memberships else Dict.empty
                , loadingMemberships = Dict.empty, membershipErrors = Dict.empty
                , activeListRequest = Nothing, activeMutation = Nothing, authorizationPending = Nothing, authorizationFailure = Nothing, mutationError = Nothing
                , membershipUserId = "", membershipRole = "read"
            }
        }

retireSessionState : WorkspaceAdminModel -> WorkspaceAdminModel
retireSessionState admin =
    { init | nextRequestToken = admin.nextRequestToken }

reconcileSessionAuthority : Model -> Model
reconcileSessionAuthority model =
    reconcileAuthority model
        |> updateWorkspaceAdmin (\admin -> { admin | authorizationPending = Nothing, authorizationFailure = Nothing })

membershipBusy : WorkspaceAdminModel -> Bool
membershipBusy admin =
    admin.activeMutation /= Nothing || admin.authorizationPending /= Nothing

newGuard : String -> Model -> MembershipRequestGuard
newGuard workspaceId model =
    { workspaceId = workspaceId, sessionEpoch = model.sessionRequestEpoch, sessionKey = sessionKey model, token = model.workspaceAdmin.nextRequestToken + 1 }

guardIsCurrent : MembershipRequestGuard -> Model -> Bool
guardIsCurrent guard model =
    canManageTarget guard.workspaceId model
        && guard.sessionEpoch == model.sessionRequestEpoch
        && guard.sessionKey == sessionKey model

ensureMemberships : Bool -> String -> Model -> ( Model, Cmd Msg )
ensureMemberships force workspaceId original =
    let
        model = reconcileAuthority original
        admin = model.workspaceAdmin
        guard = newGuard workspaceId model
    in
    if not (canManageTarget workspaceId model) || admin.activeListRequest /= Nothing || membershipBusy admin then
        ( model, Cmd.none )
    else if not force && Dict.member workspaceId admin.memberships then
        ( model, Cmd.none )
    else
        ( { model | workspaceAdmin = { admin | nextRequestToken = guard.token, activeListRequest = Just guard
            , loadingMemberships = Dict.insert workspaceId True admin.loadingMemberships
            , membershipErrors = Dict.remove workspaceId admin.membershipErrors } }
        , Api.fetchWorkspaceMemberships model.flags.apiUrl workspaceId (GotWorkspaceMemberships guard)
        )

acceptCanonicalMemberships : String -> List Api.WorkspaceMembership -> Model -> Model
acceptCanonicalMemberships workspaceId memberships model =
    if canManageTarget workspaceId model then
        updateWorkspaceAdmin (\admin -> { admin | memberships = Dict.insert workspaceId memberships admin.memberships
            , activeListRequest = Nothing
            , loadingMemberships = Dict.insert workspaceId False admin.loadingMemberships
            , membershipErrors = Dict.remove workspaceId admin.membershipErrors }) model
    else model

finishMembershipMutation : Model -> ( Model, Cmd Msg )
finishMembershipMutation model =
    let
        admitted = reconcileAuthority { model | sessionRequestEpoch = model.sessionRequestEpoch + 1 }
        updated = updateWorkspaceAdmin (\admin -> { admin | authorizationPending = admin.owner }) admitted
    in
    ( updated, Api.fetchSessionContext model.flags.apiUrl model.selectedWorkspaceId (GotSessionContext updated.sessionRequestEpoch model.selectedWorkspaceId) )


handleEscape : Model -> Maybe Model
handleEscape model =
    if model.groups.workspaceDeletion |> Maybe.map (\deletion -> not deletion.pending) |> Maybe.withDefault False then
        Just (setDeletion Nothing model)

    else if model.workspaceAdmin.purgeConfirmation /= Nothing then
        Just (updateWorkspaceAdmin (\admin -> { admin | purgeConfirmation = Nothing }) model)

    else
        Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        GotWorkspaceMemberships guard result ->
            if guardIsCurrent guard model && model.workspaceAdmin.activeListRequest == Just guard then
                case result of
                    Ok paginated ->
                        ( updateWorkspaceAdmin (\admin -> { admin | memberships = Dict.insert guard.workspaceId paginated.items admin.memberships
                            , loadingMemberships = Dict.insert guard.workspaceId False admin.loadingMemberships
                            , membershipErrors = Dict.remove guard.workspaceId admin.membershipErrors, activeListRequest = Nothing }) model
                        , Cmd.none )
                    Err _ ->
                        ( updateWorkspaceAdmin (\admin -> { admin | loadingMemberships = Dict.insert guard.workspaceId False admin.loadingMemberships
                            , membershipErrors = Dict.insert guard.workspaceId "Could not load memberships. Retry to refresh this list." admin.membershipErrors
                            , activeListRequest = Nothing }) model
                        , Cmd.none )
            else ( model, Cmd.none )

        RetryWorkspaceMemberships workspaceId ->
            ensureMemberships True workspaceId model

        UpdateMembershipUserId value ->
            if Permissions.canViewWorkspaceAdministration model && not (membershipBusy model.workspaceAdmin) then
                ( updateWorkspaceAdmin (\admin -> { admin | membershipUserId = value, mutationError = Nothing }) model, Cmd.none )
            else ( model, Cmd.none )

        UpdateMembershipRole role ->
            if Permissions.canViewWorkspaceAdministration model && not (membershipBusy model.workspaceAdmin) && List.member role [ "read", "edit", "admin" ] then
                ( updateWorkspaceAdmin (\admin -> { admin | membershipRole = role, mutationError = Nothing }) model, Cmd.none )
            else ( model, Cmd.none )

        SubmitWorkspaceMembership workspaceId ->
            let
                userId = String.trim model.workspaceAdmin.membershipUserId
            in
            if not (canManageTarget workspaceId model) || membershipBusy model.workspaceAdmin then
                ( model, Cmd.none )
            else if String.isEmpty userId then
                ( updateWorkspaceAdmin (\admin -> { admin | mutationError = Just "Enter a user UUID before granting a role." }) model, Cmd.none )
            else if not (List.member model.workspaceAdmin.membershipRole [ "read", "edit", "admin" ]) then
                ( updateWorkspaceAdmin (\admin -> { admin | mutationError = Just "Choose a valid role." }) model, Cmd.none )
            else
                let
                    guard = newGuard workspaceId model
                    ( trackedModel, requestId, clearCmd ) = beginTrackedMutation [ workspaceId, userId ] model
                    updated = updateWorkspaceAdmin (\admin -> { admin | nextRequestToken = guard.token
                        , activeMutation = Just { guard = guard, userId = userId, removing = False }, mutationError = Nothing
                        , activeListRequest = Nothing, loadingMemberships = Dict.insert workspaceId False admin.loadingMemberships }) trackedModel
                in
                ( updated, Cmd.batch [ clearCmd, Api.upsertWorkspaceMembership model.flags.apiUrl workspaceId userId model.workspaceAdmin.membershipRole requestId (WorkspaceMembershipSaved guard) ] )

        WorkspaceMembershipSaved guard result ->
            case model.workspaceAdmin.activeMutation of
                Just mutation ->
                    if guardIsCurrent guard model && mutation.guard == guard && not mutation.removing then
                        case result of
                            Ok membership ->
                                if membership.workspaceId == guard.workspaceId && membership.userId == mutation.userId then
                                    let
                                        existing = Dict.get guard.workspaceId model.workspaceAdmin.memberships |> Maybe.withDefault []
                                        updated = updateWorkspaceAdmin (\admin -> { admin | memberships = Dict.insert guard.workspaceId (membership :: List.filter (\item -> item.userId /= membership.userId) existing) admin.memberships
                                            , activeMutation = Nothing, membershipUserId = "", mutationError = Nothing }) model
                                        ( trackedModel, trackCmd ) = trackLocalMutation membership.userId updated
                                        ( toastedModel, toastCmd ) = addToast Success "Workspace membership saved" trackedModel
                                        ( finalModel, sessionCmd ) = finishMembershipMutation toastedModel
                                    in ( finalModel, Cmd.batch [ trackCmd, toastCmd, sessionCmd ] )
                                else
                                    ( updateWorkspaceAdmin (\admin -> { admin | activeMutation = Nothing, mutationError = Just "The membership response did not match this request. Retry the action." }) model, Cmd.none )
                            Err _ ->
                                ( updateWorkspaceAdmin (\admin -> { admin | activeMutation = Nothing, mutationError = Just "Could not save membership. Your form is retained; retry the action." }) model, Cmd.none )
                    else ( model, Cmd.none )
                Nothing -> ( model, Cmd.none )

        RemoveWorkspaceMembership workspaceId userId ->
            if not (canManageTarget workspaceId model) || membershipBusy model.workspaceAdmin then
                ( model, Cmd.none )
            else
                let
                    guard = newGuard workspaceId model
                    ( trackedModel, requestId, clearCmd ) = beginTrackedMutation [ workspaceId, userId ] model
                    updated = updateWorkspaceAdmin (\admin -> { admin | nextRequestToken = guard.token
                        , activeMutation = Just { guard = guard, userId = userId, removing = True }, mutationError = Nothing
                        , activeListRequest = Nothing, loadingMemberships = Dict.insert workspaceId False admin.loadingMemberships }) trackedModel
                in
                ( updated, Cmd.batch [ clearCmd, Api.deleteWorkspaceMembership model.flags.apiUrl workspaceId userId requestId (WorkspaceMembershipDeleted guard userId) ] )

        WorkspaceMembershipDeleted guard userId result ->
            case model.workspaceAdmin.activeMutation of
                Just mutation ->
                    if guardIsCurrent guard model && mutation.guard == guard && mutation.removing && mutation.userId == userId then
                        case result of
                            Ok () ->
                                let
                                    existing = Dict.get guard.workspaceId model.workspaceAdmin.memberships |> Maybe.withDefault []
                                    updated = updateWorkspaceAdmin (\admin -> { admin | memberships = Dict.insert guard.workspaceId (List.filter (\item -> item.userId /= userId) existing) admin.memberships
                                        , activeMutation = Nothing, mutationError = Nothing }) model
                                    ( toastedModel, toastCmd ) = addToast Success "Workspace membership removed" updated
                                    ( finalModel, sessionCmd ) = finishMembershipMutation toastedModel
                                in ( finalModel, Cmd.batch [ toastCmd, sessionCmd ] )
                            Err _ ->
                                ( updateWorkspaceAdmin (\admin -> { admin | activeMutation = Nothing, mutationError = Just "Could not remove membership. Retry the action." }) model, Cmd.none )
                    else ( model, Cmd.none )
                Nothing -> ( model, Cmd.none )

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

        ConfirmWorkspaceDelete workspaceId ->
            if canDeleteWorkspace workspaceId model && model.groups.workspaceDeletion == Nothing then
                let
                    groups = model.groups
                    deletion = { workspaceId = workspaceId, token = groups.nextDeletionToken + 1, sessionKey = deletionSessionKey model, pending = False, error = Nothing }
                in
                ( { model | groups = { groups | workspaceDeletion = Just deletion, nextDeletionToken = deletion.token } }, Helpers.focusElement "workspace-delete-cancel" )
            else ( model, Cmd.none )

        CancelWorkspaceDelete ->
            case model.groups.workspaceDeletion of
                Just deletion ->
                    if deletion.pending then ( model, Cmd.none )
                    else ( setDeletion Nothing model, Cmd.none )
                Nothing -> ( model, Cmd.none )

        PerformWorkspaceDelete ->
            case model.groups.workspaceDeletion of
                Just deletion ->
                    if deletion.pending || not (canDeleteWorkspace deletion.workspaceId model) || deletion.sessionKey /= deletionSessionKey model then
                        ( model, Cmd.none )
                    else
                        let
                            pending = { deletion | pending = True, error = Nothing }
                            ( tracked, requestId, clearCmd ) = beginTrackedMutation [ deletion.workspaceId ] (setDeletion (Just pending) model)
                        in
                        ( tracked, Cmd.batch [ clearCmd, Api.deleteWorkspace model.flags.apiUrl deletion.workspaceId requestId (WorkspaceDeleteCompleted pending) ] )
                Nothing -> ( model, Cmd.none )

        WorkspaceDeleteCompleted deletion result ->
            case model.groups.workspaceDeletion of
                Just active ->
                    if active.token /= deletion.token || active.workspaceId /= deletion.workspaceId || not active.pending || deletion.sessionKey /= deletionSessionKey model then
                        ( model, Cmd.none )
                    else
                        case result of
                            Ok () ->
                                let
                                    groups = model.groups
                                    cleaned = setDeletion Nothing { model | workspaces = Dict.remove deletion.workspaceId model.workspaces
                                        , selectedWorkspaceId = if model.selectedWorkspaceId == Just deletion.workspaceId then Nothing else model.selectedWorkspaceId
                                        , page = if model.selectedWorkspaceId == Just deletion.workspaceId then HomePage else model.page
                                        , dataLoading = let loading = model.dataLoading in { loading | activeWorkspaceListLoadToken = Nothing, loadingWorkspaces = False }
                                        , groups = { groups | deletedWorkspaces = Set.insert deletion.workspaceId groups.deletedWorkspaces, groupMembers = Dict.map (\_ members -> List.filter ((/=) deletion.workspaceId) members) groups.groupMembers } }
                                    ( toasted, toastCmd ) = addToast Success "Workspace deleted" cleaned
                                    routeCmd = if model.selectedWorkspaceId == Just deletion.workspaceId then pushUrl model.key "/" else Cmd.none
                                in
                                ( toasted, Cmd.batch [ toastCmd, routeCmd ] )
                            Err _ ->
                                ( setDeletion (Just { active | pending = False, error = Just "Could not delete workspace. Your workspace is unchanged; retry the action." }) model, Cmd.none )
                Nothing -> ( model, Cmd.none )

        WorkspaceDeleted wsId result ->
            case result of
                Ok () ->
                    let
                        ( toastedModel, toastCmd ) =
                            addToast Success "Workspace deleted" { model | workspaces = Dict.remove wsId model.workspaces, selectedWorkspaceId = Nothing }
                    in
                    ( toastedModel, Cmd.batch [ toastCmd, pushUrl model.key "/" ] )

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
                    , Cmd.batch [ toastCmd, pushUrl model.key "/" ]
                    )

                Err _ ->
                    let
                        ( toastedModel, toastCmd ) =
                            addToast Error "Workspace was deleted, but purge failed. It has been removed from the active workspace list."
                                { model | workspaces = Dict.remove wsId model.workspaces, selectedWorkspaceId = Nothing }
                    in
                    ( updateWorkspaceAdmin (\admin -> { admin | purgeConfirmation = Nothing }) toastedModel
                    , Cmd.batch [ toastCmd, pushUrl model.key "/" ]
                    )

        _ ->
            ( model, Cmd.none )


viewPermissionSummary : Model -> Html Msg
viewPermissionSummary model =
    case model.sessionContext of
        Nothing ->
            p [ class "form-help", attribute "role" "status" ] [ text "Loading permissions..." ]

        Just session ->
            div [ class "card workspace-admin-context" ]
                [ div [ class "workspace-admin-context-main" ]
                    [ span [ class "workspace-admin-label" ] [ text "Signed in as" ]
                    , strong [] [ text session.principal.actorLabel ]
                    , span [ class "badge workspace-admin-role" ] [ text (Permissions.currentWorkspaceRoleLabel model) ]
                    ]
                , if session.globalPermissions.createWorkspace then
                    span [ class "permission-pill" ] [ text "Can create workspaces" ]
                  else text ""
                , if session.globalPermissions.superadmin then
                    span [ class "permission-pill permission-pill-superadmin" ] [ text "Superadmin" ]
                  else text ""
                ]

viewWorkspaceAdminPanel : Api.Workspace -> Model -> Html Msg
viewWorkspaceAdminPanel ws model =
    section [ class "workspace-administration", attribute "aria-labelledby" "workspace-administration-heading" ]
        [ h2 [ id "workspace-administration-heading" ] [ text "Administration" ]
        , if model.auth.status /= AuthReady || model.sessionContext == Nothing then
            p [ class "form-help", attribute "role" "status" ] [ text "Loading workspace permissions..." ]
          else if not (canManageTarget ws.id model) then
            div [ class "card empty-state", attribute "role" "status" ]
                [ text "Administration is unavailable for your current workspace access." ]
          else
            div [ class "workspace-admin-content" ]
                [ viewPermissionSummary model
                , viewMembershipManager ws model
                ]
        ]

viewMembershipManager : Api.Workspace -> Model -> Html Msg
viewMembershipManager ws model =
    let
        admin = model.workspaceAdmin
        memberships = Dict.get ws.id admin.memberships |> Maybe.withDefault []
        loading = Dict.get ws.id admin.loadingMemberships |> Maybe.withDefault False
        busy = membershipBusy admin
        removing = admin.activeMutation |> Maybe.map .removing |> Maybe.withDefault False
    in
    div [ class "card workspace-memberships" ]
        [ h3 [] [ text "Memberships" ]
        , p [ class "form-help" ] [ text "Grant a user read, edit, or admin access to this workspace." ]
        , if loading then
            p [ class "form-help", attribute "role" "status" ] [ text "Loading memberships..." ]
          else text ""
        , case Dict.get ws.id admin.membershipErrors of
            Just message ->
                div [ class "workspace-admin-feedback", attribute "role" "alert" ]
                    [ p [] [ text message ]
                    , button [ class "btn btn-secondary", type_ "button", onClick (RetryWorkspaceMemberships ws.id), disabled (loading || busy) ] [ text "Retry memberships" ]
                    ]
            Nothing -> text ""
        , if List.isEmpty memberships then
            if Dict.member ws.id admin.memberships then
                p [ class "form-help", attribute "role" "status" ] [ text "No explicit memberships. Superadmins may still have access." ]
            else text ""
          else
            div [ class "membership-list" ] (List.map (viewMembershipRow ws.id busy) memberships)
        , Html.form [ class "membership-form", onSubmit (SubmitWorkspaceMembership ws.id) ]
            [ div [ class "filter-group membership-user-field" ]
                [ label [ class "filter-label", for "workspace-membership-user" ] [ text "User UUID" ]
                , input [ id "workspace-membership-user", class "form-input", placeholder "Enter user UUID"
                    , value admin.membershipUserId, onInput UpdateMembershipUserId, disabled busy ] []
                ]
            , div [ class "filter-group" ]
                [ label [ class "filter-label", for "workspace-membership-role" ] [ text "Role" ]
                , select [ id "workspace-membership-role", class "form-input", value admin.membershipRole, onInput UpdateMembershipRole, disabled busy ]
                    [ option [ value "read" ] [ text "Read" ]
                    , option [ value "edit" ] [ text "Edit" ]
                    , option [ value "admin" ] [ text "Admin" ]
                    ]
                ]
            , button [ id "workspace-membership-submit", class "btn btn-primary", type_ "submit", disabled busy ]
                [ text (if admin.authorizationPending /= Nothing then "Checking access..." else if busy && not removing then "Saving..." else "Grant / update") ]
            ]
        , if removing then p [ class "form-help", attribute "role" "status" ] [ text "Removing membership..." ] else text ""
        , if admin.authorizationPending /= Nothing then p [ class "form-help", attribute "role" "status" ] [ text "Checking current workspace access..." ] else text ""
        , case admin.mutationError of
            Just message -> p [ class "form-error", attribute "role" "alert" ] [ text message ]
            Nothing -> text ""
        ]

viewMembershipRow : String -> Bool -> Api.WorkspaceMembership -> Html Msg
viewMembershipRow wsId busy membership =
    div [ class "membership-row", attribute "data-member-user" membership.userId ]
        [ Helpers.copyableValue "membership-user" "user ID" membership.userId membership.userId
        , span [ class ("badge badge-" ++ membership.role) ] [ text membership.role ]
        , span [ class "membership-updated" ] [ text ("Updated " ++ formatDate membership.updatedAt) ]
        , button [ class "btn-small btn-danger-subtle", type_ "button", disabled busy, onClick (RemoveWorkspaceMembership wsId membership.userId) ] [ text "Remove" ]
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
                    , p [ class "card-id" ] [ Helpers.copyableValue "" "workspace ID" wsId wsId ]
                    , div [ class "modal-actions" ]
                        [ button [ class "btn btn-secondary", onClick CancelWorkspacePurge ] [ text "Cancel" ]
                        , button [ class "btn btn-danger", onClick PerformWorkspacePurge ] [ text "Purge" ]
                        ]
                    ]
                ]


updateWorkspaceAdmin : (WorkspaceAdminModel -> WorkspaceAdminModel) -> Model -> Model
updateWorkspaceAdmin fn model =
    { model | workspaceAdmin = fn model.workspaceAdmin }


deleteSessionPrincipal : Api.SessionContext -> String
deleteSessionPrincipal session =
    Encode.encode 0 (Encode.list Encode.string [ session.authMode, session.principal.actorType, session.principal.actorId, session.principal.authority, Maybe.withDefault "" session.principal.grantUserId ])

canDeleteWorkspace : String -> Model -> Bool
canDeleteWorkspace workspaceId model =
    model.auth.status == AuthReady && model.sessionContext /= Nothing && Dict.member workspaceId model.workspaces
        && (Permissions.isSuperadmin model || (model.selectedWorkspaceId == Just workspaceId && Permissions.canAdminCurrentWorkspace model))

deletionSessionKey : Model -> String
deletionSessionKey model =
    (case model.sessionContext of
        Just session -> Just session
        Nothing -> model.groups.catalogueOwner) |> Maybe.map deleteSessionPrincipal |> Maybe.withDefault ""

setDeletion : Maybe WorkspaceDeletion -> Model -> Model
setDeletion deletion model =
    let groups = model.groups in { model | groups = { groups | workspaceDeletion = deletion } }

viewDeleteConfirmModal : Model -> Html Msg
viewDeleteConfirmModal model =
    case model.groups.workspaceDeletion of
        Nothing -> text ""
        Just deletion ->
            div [ class "modal-overlay", onClick CancelWorkspaceDelete ]
                [ div [ class "modal", attribute "role" "dialog", attribute "aria-modal" "true", attribute "aria-labelledby" "workspace-delete-title", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
                    [ h3 [ id "workspace-delete-title", class "modal-title" ] [ text "Delete workspace?" ]
                    , p [] [ text ("Delete \"" ++ (Dict.get deletion.workspaceId model.workspaces |> Maybe.map .name |> Maybe.withDefault "this workspace") ++ "\" from the active workspace list? Its contents will be retained; this does not purge them.") ]
                    , case deletion.error of
                        Just message -> p [ class "form-error", attribute "role" "alert" ] [ text message ]
                        Nothing -> text ""
                    , div [ class "modal-actions" ]
                        [ button [ id "workspace-delete-cancel", class "btn btn-secondary", disabled deletion.pending, onClick CancelWorkspaceDelete ] [ text "Cancel" ]
                        , button [ class "btn btn-danger", disabled (deletion.pending || not (canDeleteWorkspace deletion.workspaceId model)), onClick PerformWorkspaceDelete ] [ text (if deletion.pending then "Deleting..." else "Delete workspace") ]
                        ]
                    ]
                ]
