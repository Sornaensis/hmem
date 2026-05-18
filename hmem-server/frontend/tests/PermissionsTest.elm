module PermissionsTest exposing (suite)

import Api
import Expect
import Permissions
import Test exposing (..)


suite : Test
suite =
    describe "permission presentation helpers"
        [ test "detects only the implicit local superadmin session" <|
            \_ ->
                [ Permissions.isImplicitLocalSuperadminSession implicitLocalSuperadmin
                , Permissions.isImplicitLocalSuperadminSession { implicitLocalSuperadmin | authMode = "deployed" }
                , Permissions.isImplicitLocalSuperadminSession { implicitLocalSuperadmin | principal = { localPrincipal | authority = "grant_user" } }
                , Permissions.isImplicitLocalSuperadminSession { implicitLocalSuperadmin | globalPermissions = { createWorkspace = True, superadmin = False } }
                ]
                    |> Expect.equal [ True, False, False, False ]
        ]


implicitLocalSuperadmin : Api.SessionContext
implicitLocalSuperadmin =
    { authMode = "local"
    , principal = localPrincipal
    , globalPermissions = { createWorkspace = True, superadmin = True }
    , workspace = Nothing
    }


localPrincipal : Api.SessionPrincipal
localPrincipal =
    { actorType = "user"
    , actorId = "local"
    , actorLabel = "Local User"
    , authority = "local_superadmin"
    , grantUserId = Nothing
    }
