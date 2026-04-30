module Permissions exposing
    ( authModeLabel
    , canAdminCurrentWorkspace
    , canCreateWorkspace
    , canEditCurrentWorkspace
    , canReadCurrentWorkspace
    , canViewGlobalAudit
    , canViewCurrentWorkspaceAudit
    , currentWorkspaceRoleLabel
    , hasImplicitLocalSuperadmin
    , hasSession
    , isLocalMode
    , isSuperadmin
    , principalAttributionLabel
    )

import Api
import Types exposing (Model)


localAuthMode : String
localAuthMode =
    "local"


localSuperadminAuthority : String
localSuperadminAuthority =
    "local_superadmin"


hasSession : Model -> Bool
hasSession model =
    model.sessionContext /= Nothing


authModeLabel : Model -> String
authModeLabel model =
    model.sessionContext
        |> Maybe.map .authMode
        |> Maybe.withDefault (Maybe.withDefault model.flags.runtimeMode model.auth.mode)


isLocalMode : Model -> Bool
isLocalMode model =
    authModeLabel model == localAuthMode


hasImplicitLocalSuperadmin : Model -> Bool
hasImplicitLocalSuperadmin model =
    model.sessionContext
        |> Maybe.map
            (\session ->
                session.authMode == localAuthMode
                    && session.globalPermissions.superadmin
                    && session.principal.authority == localSuperadminAuthority
            )
        |> Maybe.withDefault False


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


canViewCurrentWorkspaceAudit : Model -> Bool
canViewCurrentWorkspaceAudit =
    canAdminCurrentWorkspace


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


principalAttributionLabel : Model -> Maybe String
principalAttributionLabel model =
    model.sessionContext
        |> Maybe.map
            (\session ->
                let
                    principal =
                        session.principal

                    actorPrefix =
                        case ( session.authMode, principal.actorType ) of
                            ( mode, "bot" ) ->
                                if mode == localAuthMode then
                                    "Local bot"

                                else
                                    "Bot"

                            ( mode, _ ) ->
                                if mode == localAuthMode then
                                    "Local user"

                                else
                                    "User"

                    authoritySuffix =
                        case principal.authority of
                            authority ->
                                if authority == localSuperadminAuthority then
                                    " · implicit superadmin"

                                else if authority == "grant_user" then
                                    " · grant-backed"

                                else
                                    ""
                in
                actorPrefix ++ ": " ++ principal.actorLabel ++ authoritySuffix
            )

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
