module Permissions exposing
    ( canAdminCurrentWorkspace
    , canCreateWorkspace
    , canEditCurrentWorkspace
    , canReadCurrentWorkspace
    , canViewGlobalAudit
    , currentWorkspaceRoleLabel
    , hasSession
    , isSuperadmin
    )

import Api
import Types exposing (Model)


hasSession : Model -> Bool
hasSession model =
    model.sessionContext /= Nothing


isSuperadmin : Model -> Bool
isSuperadmin model =
    model.sessionContext
        |> Maybe.map (.globalPermissions >> .superadmin)
        |> Maybe.withDefault False


canCreateWorkspace : Model -> Bool
canCreateWorkspace model =
    model.sessionContext
        |> Maybe.map
            (\session ->
                session.globalPermissions.superadmin || session.globalPermissions.createWorkspace
            )
        |> Maybe.withDefault False


canReadCurrentWorkspace : Model -> Bool
canReadCurrentWorkspace model =
    currentWorkspacePermission .canRead model


canEditCurrentWorkspace : Model -> Bool
canEditCurrentWorkspace model =
    currentWorkspacePermission .canEdit model


canAdminCurrentWorkspace : Model -> Bool
canAdminCurrentWorkspace model =
    currentWorkspacePermission .canAdmin model


canViewGlobalAudit : Model -> Bool
canViewGlobalAudit =
    isSuperadmin


currentWorkspaceRoleLabel : Model -> String
currentWorkspaceRoleLabel model =
    if isSuperadmin model then
        "superadmin"

    else
        case ( model.selectedWorkspaceId, Maybe.andThen .workspace model.sessionContext ) of
            ( Just selectedWsId, Just workspaceContext ) ->
                if workspaceContext.workspaceId == selectedWsId then
                    Maybe.withDefault "no access" workspaceContext.role

                else
                    "loading"

            _ ->
                "no access"


currentWorkspacePermission : (Api.SessionWorkspaceContext -> Bool) -> Model -> Bool
currentWorkspacePermission selector model =
    if isSuperadmin model then
        True

    else
        case ( model.selectedWorkspaceId, Maybe.andThen .workspace model.sessionContext ) of
            ( Just selectedWsId, Just workspaceContext ) ->
                workspaceContext.workspaceId == selectedWsId && selector workspaceContext

            _ ->
                False
