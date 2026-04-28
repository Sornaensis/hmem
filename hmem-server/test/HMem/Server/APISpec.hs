{-# OPTIONS_GHC -Wno-x-partial -Wno-incomplete-uni-patterns #-}

module HMem.Server.APISpec (spec) where

import Data.Aeson (decode, encode, object, (.=), toJSON, Value(..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Control.Lens ((&), (?~))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Functor.Contravariant (contramap)
import Data.Maybe (isJust)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUIDv4
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Network.HTTP.Types
import Network.Wai (Application, Middleware, defaultRequest)
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse(..), runSession, srequest, SRequest(..))
import Servant (Proxy(..), serve)
import Test.Hspec

import Crypto.JWT qualified as JWT
import Crypto.JOSE.JWK qualified as JWK
import Crypto.JOSE.JWS qualified as JWS
import HMem.Config (CorsConfig(..))
import HMem.Config qualified as Config
import HMem.DB.Auth qualified as Auth
import HMem.DB.Category qualified as Cat
import HMem.DB.Memory qualified as Mem
import HMem.DB.Pool qualified as DBPool
import HMem.DB.Project qualified as Proj
import HMem.DB.SavedView qualified as SV
import HMem.DB.Task qualified as Task
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..), actorTypeToText, withPrincipalContext)
import HMem.DB.TestHarness
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.API (HMemAPI, server)
import HMem.Server.App (mkApp, requestIdMiddleware, resolveRequestPrincipal)
import HMem.Server.AuthBootstrap qualified as AuthBootstrap
import HMem.Server.AuthTokens qualified as AuthTokens
import HMem.Server.Event (ChangeEvent(..), ChangeType(..), EntityType(..))
import HMem.Server.WebSocket (WorkspaceSubscription(..), consumeTicket, createTicket, eventVisibleToSubscription, newWSState, resolveLocalWebSocketAccess)
import HMem.Types

------------------------------------------------------------------------
-- WAI test helpers
------------------------------------------------------------------------

withApp :: (Application -> IO a) -> IO a
withApp action = withAppConfig Config.defaultConfig action

withAppConfig :: Config.HMemConfig -> (Application -> IO a) -> IO a
withAppConfig cfg action = withAppEnvConfig cfg (\_ app -> action app)

withAppEnv :: (TestEnv -> Application -> IO a) -> IO a
withAppEnv = withAppEnvConfig Config.defaultConfig

withAppEnvConfig :: Config.HMemConfig -> (TestEnv -> Application -> IO a) -> IO a
withAppEnvConfig cfg = withAppEnvConfigAndMiddleware cfg id

withAppEnvConfigAndMiddleware :: Config.HMemConfig -> Middleware -> (TestEnv -> Application -> IO a) -> IO a
withAppEnvConfigAndMiddleware cfg middleware action = withTestEnv $ \env -> do
  tracker <- newAccessTracker env.pool 3600
  wsState <- newWSState
  let cfg' = cfg { Config.cors = CorsConfig { allowedOrigins = ["*"] } }
  app <- mkApp middleware cfg'.auth cfg'.cors cfg'.rateLimit env.pool tracker wsState Nothing True
  action env app

principalMiddleware :: Principal -> Middleware
principalMiddleware principal app req respond =
  withPrincipalContext (Just principal) (app req respond)

testAuthCfg :: Config.HMemConfig
testAuthCfg = Config.defaultConfig
  { Config.auth = Config.AuthConfig
      { Config.mode = Config.AuthModeLocal
      , Config.enabled = True
      , Config.apiKey = Just "test-secret"
      , Config.local = Config.LocalAuthConfig { Config.bootstrapEnabled = True, Config.allowRemoteBootstrap = False, Config.botTokens = [] }
      , Config.deployed = Config.DeployedAuthConfig
          { Config.issuer = Nothing
          , Config.audience = Nothing
          , Config.discoveryUrl = Nothing
          , Config.jwksUrl = Nothing
          , Config.jwks = Nothing
          , Config.tokenLookup = Config.TokenLookupDatabase
          }
      }
  }

testRateLimitCfg :: Config.HMemConfig
testRateLimitCfg = Config.defaultConfig
  { Config.rateLimit = Config.RateLimitConfig
      { Config.rlEnabled = True
      , Config.rlRequestsPerSecond = 1.0
      , Config.rlBurst = 1
      }
  }

deployedAuthCfg :: Config.HMemConfig
deployedAuthCfg = Config.defaultConfig
  { Config.auth = Config.defaultConfig.auth
      { Config.mode = Config.AuthModeDeployed
      , Config.enabled = False
      , Config.apiKey = Nothing
      , Config.local = Config.LocalAuthConfig { Config.bootstrapEnabled = False, Config.allowRemoteBootstrap = False, Config.botTokens = [] }
      }
  }

runReqWithHeaders :: Application -> Method -> BS.ByteString -> [(HeaderName, BS.ByteString)] -> LBS.ByteString -> IO SResponse
runReqWithHeaders app method fullPath headers body =
  runSession (srequest $ SRequest req body) app
  where
    (rawPath, rawQS) = BS.break (== 0x3F) fullPath  -- split on '?'
    req = defaultRequest
      { Wai.requestMethod  = method
      , Wai.rawPathInfo    = rawPath
      , Wai.pathInfo       = filter (not . T.null) $ T.split (== '/')
                               $ decodeUtf8 rawPath
      , Wai.rawQueryString = rawQS
      , Wai.queryString    = parseQuery rawQS
      , Wai.requestHeaders = [("Content-Type", "application/json")] <> headers
      }

runReq :: Application -> Method -> BS.ByteString -> LBS.ByteString -> IO SResponse
runReq app method fullPath body =
  runReqWithHeaders app method fullPath [] body

get_ :: Application -> BS.ByteString -> IO SResponse
get_ app path = runReq app methodGet path ""

postJSON :: Application -> BS.ByteString -> Value -> IO SResponse
postJSON app path body = runReq app methodPost path (encode body)

putJSON :: Application -> BS.ByteString -> Value -> IO SResponse
putJSON app path body = runReq app methodPut path (encode body)

del :: Application -> BS.ByteString -> IO SResponse
del app path = runReq app methodDelete path ""

respStatus :: SResponse -> Int
respStatus = statusCode . simpleStatus

respBody :: SResponse -> LBS.ByteString
respBody = simpleBody

principalActorType :: Principal -> ActorType
principalActorType Principal { actorType = value } = value

principalAuthority :: Principal -> PrincipalAuthority
principalAuthority Principal { authority = value } = value

containsObjectField :: T.Text -> Maybe Value -> Bool
containsObjectField fieldName = \case
  Just (Object obj) -> KM.member (Key.fromText fieldName) obj
  _ -> False

assertNoTokenHashAuditSnapshot :: AuditLogRow -> Expectation
assertNoTokenHashAuditSnapshot row = do
  row.oldValues `shouldSatisfy` (not . containsObjectField "token_hash")
  row.newValues `shouldSatisfy` (not . containsObjectField "token_hash")

uuidPath :: BS.ByteString -> UUID -> BS.ByteString
uuidPath prefix uid = prefix <> "/" <> encodeUtf8 (T.pack (show uid))

createTestUser :: TestEnv -> Bool -> Bool -> IO UUID
createTestUser env canCreateWorkspace isSuperadmin =
  DBPool.runSession env.pool $ Session.statement (canCreateWorkspace, isSuperadmin) createTestUserStatement

createTestUserStatement :: Statement.Statement (Bool, Bool) UUID
createTestUserStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO users (can_create_workspace, is_superadmin) VALUES ($1, $2) RETURNING id"
    encoder =
      contramap fst (Enc.param (Enc.nonNullable Enc.bool)) <>
      contramap snd (Enc.param (Enc.nonNullable Enc.bool))
    decoder = Dec.singleRow (Dec.column (Dec.nonNullable Dec.uuid))

createTestUserWithSubject :: TestEnv -> T.Text -> Bool -> Bool -> IO UUID
createTestUserWithSubject env authSubject canCreateWorkspace isSuperadmin =
  DBPool.runSession env.pool $ Session.statement (authSubject, canCreateWorkspace, isSuperadmin) createTestUserWithSubjectStatement

createTestUserWithSubjectStatement :: Statement.Statement (T.Text, Bool, Bool) UUID
createTestUserWithSubjectStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO users (auth_subject, display_name, can_create_workspace, is_superadmin) \
          \VALUES ($1, $1, $2, $3) RETURNING id"
    encoder =
      contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.text)) <>
      contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.bool)) <>
      contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.bool))
    decoder = Dec.singleRow (Dec.column (Dec.nonNullable Dec.uuid))

createAccessToken :: TestEnv -> UUID -> ActorType -> T.Text -> T.Text -> IO UUID
createAccessToken env grantUserId actor tokenLabel token =
  DBPool.runSession env.pool $ Session.statement
    (grantUserId, actorTypeToText actor, tokenLabel, Auth.accessTokenHash token)
    createAccessTokenStatement

createAccessTokenStatement :: Statement.Statement (UUID, T.Text, T.Text, T.Text) UUID
createAccessTokenStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO access_tokens (grant_user_id, actor_type, actor_label, token_hash) \
          \VALUES ($1, $2::actor_type_enum, $3, $4) RETURNING id"
    encoder =
      contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nonNullable Enc.text)) <>
      contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nonNullable Enc.text)) <>
      contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.text))
    decoder = Dec.singleRow (Dec.column (Dec.nonNullable Dec.uuid))

revokeAccessToken :: TestEnv -> UUID -> IO ()
revokeAccessToken env tokenId =
  DBPool.runSession env.pool $ Session.statement tokenId revokeAccessTokenStatement

revokeAccessTokenStatement :: Statement.Statement UUID ()
revokeAccessTokenStatement = Statement.Statement sql encoder decoder True
  where
    sql = "UPDATE access_tokens SET revoked_at = now() WHERE id = $1"
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.noResult

expireAccessToken :: TestEnv -> UUID -> IO ()
expireAccessToken env tokenId =
  DBPool.runSession env.pool $ Session.statement tokenId expireAccessTokenStatement

expireAccessTokenStatement :: Statement.Statement UUID ()
expireAccessTokenStatement = Statement.Statement sql encoder decoder True
  where
    sql = "UPDATE access_tokens \
          \SET created_at = now() - interval '2 seconds', expires_at = now() - interval '1 second' \
          \WHERE id = $1"
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.noResult

accessTokenLastUsedIsSet :: TestEnv -> UUID -> IO Bool
accessTokenLastUsedIsSet env tokenId =
  DBPool.runSession env.pool $ Session.statement tokenId accessTokenLastUsedIsSetStatement

accessTokenLastUsedIsSetStatement :: Statement.Statement UUID Bool
accessTokenLastUsedIsSetStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT last_used_at IS NOT NULL FROM access_tokens WHERE id = $1"
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))

deployedJwtCfg :: JWK.JWKSet -> Config.HMemConfig
deployedJwtCfg jwkSet = deployedAuthCfg
  { Config.auth = deployedAuthCfg.auth
      { Config.deployed = deployedAuthCfg.auth.deployed
          { Config.issuer = Just testJwtIssuer
          , Config.audience = Just testJwtAudience
          , Config.jwks = Just (toJSON jwkSet)
          }
      }
  }

testJwtIssuer :: T.Text
testJwtIssuer = "https://issuer.example"

testJwtAudience :: T.Text
testJwtAudience = "hmem-web"

testJwtSecret :: T.Text
testJwtSecret = "0123456789abcdef0123456789abcdef"

signTestJwt :: T.Text -> T.Text -> IO T.Text
signTestJwt secret subject = do
  let jwk = JWK.fromOctets (encodeUtf8 secret)
      asStringOrUri = fromString . T.unpack
      claims = JWT.emptyClaimsSet
        & JWT.claimIss ?~ asStringOrUri testJwtIssuer
        & JWT.claimAud ?~ JWT.Audience [asStringOrUri testJwtAudience]
        & JWT.claimSub ?~ asStringOrUri subject
  signedResult <- (JWT.runJOSE $ do
    alg <- JWK.bestJWSAlg jwk
    JWT.signClaims jwk (JWS.newJWSHeader ((), alg)) claims) :: IO (Either JWT.JWTError JWT.SignedJWT)
  case signedResult of
    Left err -> expectationFailure (show err) >> pure ""
    Right signed -> pure . decodeUtf8 . LBS.toStrict $ JWT.encodeCompact signed

grantPrincipal :: UUID -> Principal
grantPrincipal userId = Principal
  { actorType = ActorUser
  , actorId = T.pack (show userId)
  , actorLabel = "Grant User"
  , authority = PrincipalGrantUser userId
  }

withPrincipalApp :: TestEnv -> Config.HMemConfig -> Principal -> (Application -> IO a) -> IO a
withPrincipalApp env cfg principal action = do
  tracker <- newAccessTracker env.pool 3600
  wsState <- newWSState
  let cfg' = cfg { Config.cors = CorsConfig { allowedOrigins = ["*"] } }
  app <- mkApp (principalMiddleware principal) cfg'.auth cfg'.cors cfg'.rateLimit env.pool tracker wsState Nothing True
  action app

withCapturedBroadcastApp :: TestEnv -> Principal -> (Application -> IORef [ChangeEvent] -> IO a) -> IO a
withCapturedBroadcastApp env principal action = do
  tracker <- newAccessTracker env.pool 3600
  wsState <- newWSState
  eventsRef <- newIORef []
  let capture event = modifyIORef' eventsRef (<> [event])
      app = requestIdMiddleware
          $ principalMiddleware principal
          $ serve (Proxy @HMemAPI) (server Config.defaultConfig.auth env.pool tracker capture wsState True)
  action app eventsRef

grantWorkspaceRole :: TestEnv -> UUID -> UUID -> Auth.WorkspaceRole -> IO Auth.WorkspaceMembership
grantWorkspaceRole env workspaceId userId role =
  Auth.upsertWorkspaceMembership env.pool workspaceId
    Auth.UpsertWorkspaceMembership
      { Auth.membershipUserIdInput = userId
      , Auth.membershipRoleInput = role
      }
    Nothing

membershipUserRole :: Auth.WorkspaceMembership -> (UUID, Auth.WorkspaceRole)
membershipUserRole Auth.WorkspaceMembership { membershipUserId = userId, membershipRole = role } = (userId, role)

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = around withApp $ do

  describe "GET /api/v1/workspaces" $ do
    it "returns empty list initially" $ \app -> do
      resp <- get_ app "/api/v1/workspaces"
      respStatus resp `shouldBe` 200
      decode (respBody resp) `shouldBe` Just
        (object ["items" .= ([] :: [Value]), "has_more" .= False])

  describe "POST /api/v1/workspaces" $ do
    it "creates a workspace" $ \app -> do
      resp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("test-ws" :: T.Text)])
      respStatus resp `shouldBe` 200
      let Just ws = decode (respBody resp) :: Maybe Workspace
      ws.name `shouldBe` "test-ws"

    it "rejects oversized workspace names" $ \app -> do
      resp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= T.replicate (maxNameBytes + 1) "a"])
      respStatus resp `shouldBe` 400

  describe "workspace membership administration" $ do
    it "allows workspace admins to add, list, update, and remove memberships" $ \_ ->
      withAppEnv $ \env app -> do
        userId <- createTestUser env False False
        wsResp <- postJSON app "/api/v1/workspaces"
          (object ["name" .= ("membership-admin-ws" :: T.Text)])
        respStatus wsResp `shouldBe` 200
        let Just ws = decode (respBody wsResp) :: Maybe Workspace

        createResp <- postJSON app (uuidPath "/api/v1/workspaces" ws.id <> "/memberships")
          (object ["user_id" .= userId, "role" .= ("edit" :: T.Text)])
        respStatus createResp `shouldBe` 200
        let Just createdMembership = decode (respBody createResp) :: Maybe Auth.WorkspaceMembership
        membershipUserRole createdMembership `shouldBe` (userId, Auth.WorkspaceRoleEdit)

        updateResp <- postJSON app (uuidPath "/api/v1/workspaces" ws.id <> "/memberships")
          (object ["user_id" .= userId, "role" .= ("admin" :: T.Text)])
        respStatus updateResp `shouldBe` 200
        let Just updatedMembership = decode (respBody updateResp) :: Maybe Auth.WorkspaceMembership
        membershipUserRole updatedMembership `shouldBe` (userId, Auth.WorkspaceRoleAdmin)

        listResp <- get_ app (uuidPath "/api/v1/workspaces" ws.id <> "/memberships")
        respStatus listResp `shouldBe` 200
        let Just memberships = decode (respBody listResp) :: Maybe (PaginatedResult Auth.WorkspaceMembership)
        map membershipUserRole memberships.items `shouldContain` [(userId, Auth.WorkspaceRoleAdmin)]

        deleteResp <- del app (uuidPath "/api/v1/workspaces" ws.id <> "/memberships/" <> encodeUtf8 (T.pack (show userId)))
        respStatus deleteResp `shouldBe` 200

        listAfterDeleteResp <- get_ app (uuidPath "/api/v1/workspaces" ws.id <> "/memberships")
        let Just afterDelete = decode (respBody listAfterDeleteResp) :: Maybe (PaginatedResult Auth.WorkspaceMembership)
        map membershipUserRole afterDelete.items `shouldNotContain` [(userId, Auth.WorkspaceRoleAdmin)]

    it "auto-grants workspace admin to a grant-bearing creator" $ \_ ->
      withTestEnv $ \env -> do
        userId <- createTestUser env True False
        tracker <- newAccessTracker env.pool 3600
        wsState <- newWSState
        let cfg = deployedAuthCfg { Config.cors = CorsConfig { allowedOrigins = ["*"] } }
            principal = grantPrincipal userId
        app <- mkApp (principalMiddleware principal) cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState Nothing True

        createResp <- postJSON app "/api/v1/workspaces"
          (object ["name" .= ("auto-admin-ws" :: T.Text)])
        respStatus createResp `shouldBe` 200
        let Just ws = decode (respBody createResp) :: Maybe Workspace

        listResp <- get_ app (uuidPath "/api/v1/workspaces" ws.id <> "/memberships")
        respStatus listResp `shouldBe` 200
        let Just memberships = decode (respBody listResp) :: Maybe (PaginatedResult Auth.WorkspaceMembership)
        map membershipUserRole memberships.items `shouldContain` [(userId, Auth.WorkspaceRoleAdmin)]

    it "requires real workspace admin role for deployed membership administration" $ \_ ->
      withTestEnv $ \env -> do
        adminUserId <- createTestUser env False False
        readerUserId <- createTestUser env False False
        targetUserId <- createTestUser env False False
        ws <- createTestWorkspace env "deployed-membership-admin"
        _ <- Auth.upsertWorkspaceMembership env.pool ws.id
          Auth.UpsertWorkspaceMembership
            { Auth.membershipUserIdInput = adminUserId
            , Auth.membershipRoleInput = Auth.WorkspaceRoleAdmin
            }
          Nothing
        _ <- Auth.upsertWorkspaceMembership env.pool ws.id
          Auth.UpsertWorkspaceMembership
            { Auth.membershipUserIdInput = readerUserId
            , Auth.membershipRoleInput = Auth.WorkspaceRoleRead
            }
          Nothing

        tracker <- newAccessTracker env.pool 3600
        wsState <- newWSState
        let cfg = deployedAuthCfg { Config.cors = CorsConfig { allowedOrigins = ["*"] } }
        adminApp <- mkApp (principalMiddleware $ grantPrincipal adminUserId) cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState Nothing True
        readerApp <- mkApp (principalMiddleware $ grantPrincipal readerUserId) cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState Nothing True

        deniedListResp <- get_ readerApp (uuidPath "/api/v1/workspaces" ws.id <> "/memberships")
        respStatus deniedListResp `shouldBe` 403

        deniedUpsertResp <- postJSON readerApp (uuidPath "/api/v1/workspaces" ws.id <> "/memberships")
          (object ["user_id" .= targetUserId, "role" .= ("edit" :: T.Text)])
        respStatus deniedUpsertResp `shouldBe` 403

        allowedUpsertResp <- postJSON adminApp (uuidPath "/api/v1/workspaces" ws.id <> "/memberships")
          (object ["user_id" .= targetUserId, "role" .= ("edit" :: T.Text)])
        respStatus allowedUpsertResp `shouldBe` 200

    it "returns 404 for membership administration on deleted workspaces" $ \_ ->
      withAppEnv $ \_env app -> do
        wsResp <- postJSON app "/api/v1/workspaces"
          (object ["name" .= ("deleted-membership-ws" :: T.Text)])
        let Just ws = decode (respBody wsResp) :: Maybe Workspace

        deleteResp <- del app (uuidPath "/api/v1/workspaces" ws.id)
        respStatus deleteResp `shouldBe` 200

        listResp <- get_ app (uuidPath "/api/v1/workspaces" ws.id <> "/memberships")
        respStatus listResp `shouldBe` 404

  describe "workspace + memory flow" $ do
    it "creates workspace, memory, retrieves, updates, deletes" $ \app -> do
      -- Create workspace
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("flow-ws" :: T.Text)])
      respStatus wsResp `shouldBe` 200
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      -- Create memory
      memResp <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("Test memory content" :: T.Text)
          , "memory_type" .= ("short_term" :: T.Text)
          , "importance" .= (7 :: Int)
          , "tags" .= (["test", "flow"] :: [T.Text])
          ])
      respStatus memResp `shouldBe` 200
      let Just mem = decode (respBody memResp) :: Maybe Memory
      mem.content `shouldBe` "Test memory content"
      mem.importance `shouldBe` 7

      -- Get memory
      getResp <- get_ app (uuidPath "/api/v1/memories" mem.id)
      respStatus getResp `shouldBe` 200

      -- Update memory
      upResp <- putJSON app (uuidPath "/api/v1/memories" mem.id)
        (object ["content" .= ("Updated content" :: T.Text)])
      respStatus upResp `shouldBe` 200
      let Just updated = decode (respBody upResp) :: Maybe Memory
      updated.content `shouldBe` "Updated content"

      -- Delete memory
      delResp <- del app (uuidPath "/api/v1/memories" mem.id)
      respStatus delResp `shouldBe` 200

      -- Confirm gone
      getResp2 <- get_ app (uuidPath "/api/v1/memories" mem.id)
      respStatus getResp2 `shouldBe` 404

      purgeResp <- del app (uuidPath "/api/v1/memories" mem.id <> "/purge")
      respStatus purgeResp `shouldBe` 200

  describe "project flow" $ do
    it "creates workspace, project, lists, updates status" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("proj-flow-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      -- Create project
      projResp <- postJSON app "/api/v1/projects"
        (object
          [ "workspace_id" .= ws.id
          , "name" .= ("Test Project" :: T.Text)
          , "priority" .= (8 :: Int)
          ])
      respStatus projResp `shouldBe` 200
      let Just proj = decode (respBody projResp) :: Maybe Project
      proj.name `shouldBe` "Test Project"
      proj.status `shouldBe` ProjActive

      -- List projects
      listResp <- get_ app ("/api/v1/projects?workspace_id=" <> encodeUtf8 (T.pack (show ws.id)))
      respStatus listResp `shouldBe` 200
      let Just projs = decode (respBody listResp) :: Maybe (PaginatedResult Project)
      length projs.items `shouldBe` 1

      -- Update project
      upResp <- putJSON app (uuidPath "/api/v1/projects" proj.id)
        (object ["status" .= ("completed" :: T.Text)])
      respStatus upResp `shouldBe` 200
      let Just updated = decode (respBody upResp) :: Maybe Project
      updated.status `shouldBe` ProjCompleted

    it "reparents a project and clears its parent" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("proj-move-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      leftResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Left" :: T.Text)])
      let Just leftParent = decode (respBody leftResp) :: Maybe Project

      rightResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Right" :: T.Text)])
      let Just rightParent = decode (respBody rightResp) :: Maybe Project

      childResp <- postJSON app "/api/v1/projects"
        (object
          [ "workspace_id" .= ws.id
          , "name" .= ("Child" :: T.Text)
          , "parent_id" .= leftParent.id
          ])
      let Just child = decode (respBody childResp) :: Maybe Project

      moveResp <- putJSON app (uuidPath "/api/v1/projects" child.id)
        (object ["parent_id" .= rightParent.id])
      respStatus moveResp `shouldBe` 200
      let Just moved = decode (respBody moveResp) :: Maybe Project
      moved.parentId `shouldBe` Just rightParent.id

      clearResp <- putJSON app (uuidPath "/api/v1/projects" child.id)
        (object ["parent_id" .= Null])
      respStatus clearResp `shouldBe` 200
      let Just detached = decode (respBody clearResp) :: Maybe Project
      detached.parentId `shouldBe` Nothing

    it "rejects project hierarchy cycles" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("proj-cycle-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      rootResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Root" :: T.Text)])
      let Just root = decode (respBody rootResp) :: Maybe Project

      childResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Child" :: T.Text), "parent_id" .= root.id])
      let Just child = decode (respBody childResp) :: Maybe Project

      grandchildResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Grandchild" :: T.Text), "parent_id" .= child.id])
      let Just grandchild = decode (respBody grandchildResp) :: Maybe Project

      cycleResp <- putJSON app (uuidPath "/api/v1/projects" root.id)
        (object ["parent_id" .= grandchild.id])
      respStatus cycleResp `shouldBe` 409

    it "rejects cross-workspace project parents" $ \app -> do
      wsAResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("project-parent-ws-a" :: T.Text)])
      let Just wsA = decode (respBody wsAResp) :: Maybe Workspace

      wsBResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("project-parent-ws-b" :: T.Text)])
      let Just wsB = decode (respBody wsBResp) :: Maybe Workspace

      parentResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= wsB.id, "name" .= ("Parent B" :: T.Text)])
      let Just parentB = decode (respBody parentResp) :: Maybe Project

      createResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= wsA.id, "name" .= ("Child A" :: T.Text), "parent_id" .= parentB.id])
      respStatus createResp `shouldBe` 400

      childResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= wsA.id, "name" .= ("Existing Child A" :: T.Text)])
      let Just childA = decode (respBody childResp) :: Maybe Project

      updateResp <- putJSON app (uuidPath "/api/v1/projects" childA.id)
        (object ["parent_id" .= parentB.id])
      respStatus updateResp `shouldBe` 400

      batchResp <- postJSON app "/api/v1/projects/batch-update"
        (object ["items" .= [object ["id" .= childA.id, "parent_id" .= parentB.id]]])
      respStatus batchResp `shouldBe` 400

  describe "task flow" $ do
    it "creates project, task, marks done" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("task-flow-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      projResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("TProj" :: T.Text)])
      let Just proj = decode (respBody projResp) :: Maybe Project

      -- Create task
      taskResp <- postJSON app "/api/v1/tasks"
        (object
          [ "workspace_id" .= ws.id
          , "project_id" .= proj.id
          , "title" .= ("Do something" :: T.Text)
          , "priority" .= (6 :: Int)
          ])
      respStatus taskResp `shouldBe` 200
      let Just task = decode (respBody taskResp) :: Maybe Task
      task.title `shouldBe` "Do something"
      task.status `shouldBe` Todo

      -- Mark done
      doneResp <- putJSON app (uuidPath "/api/v1/tasks" task.id)
        (object ["status" .= ("done" :: T.Text)])
      respStatus doneResp `shouldBe` 200
      let Just done = decode (respBody doneResp) :: Maybe Task
      done.status `shouldBe` Done
      done.completedAt `shouldSatisfy` isJust

    it "moves and reparents a task while preserving status" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("task-move-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      leftProjResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Left" :: T.Text)])
      let Just leftProj = decode (respBody leftProjResp) :: Maybe Project

      rightProjResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Right" :: T.Text)])
      let Just rightProj = decode (respBody rightProjResp) :: Maybe Project

      leftParentResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= leftProj.id, "title" .= ("Left Parent" :: T.Text)])
      let Just leftParent = decode (respBody leftParentResp) :: Maybe Task

      rightParentResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= rightProj.id, "title" .= ("Right Parent" :: T.Text)])
      let Just rightParent = decode (respBody rightParentResp) :: Maybe Task

      childResp <- postJSON app "/api/v1/tasks"
        (object
          [ "workspace_id" .= ws.id
          , "project_id" .= leftProj.id
          , "parent_id" .= leftParent.id
          , "title" .= ("Child" :: T.Text)
          ])
      let Just child = decode (respBody childResp) :: Maybe Task

      progressResp <- putJSON app (uuidPath "/api/v1/tasks" child.id)
        (object ["status" .= ("in_progress" :: T.Text)])
      respStatus progressResp `shouldBe` 200

      moveResp <- putJSON app (uuidPath "/api/v1/tasks" child.id)
        (object ["project_id" .= rightProj.id, "parent_id" .= rightParent.id])
      respStatus moveResp `shouldBe` 200
      let Just moved = decode (respBody moveResp) :: Maybe Task
      moved.projectId `shouldBe` Just rightProj.id
      moved.parentId `shouldBe` Just rightParent.id
      moved.status `shouldBe` InProgress

      detachResp <- putJSON app (uuidPath "/api/v1/tasks" child.id)
        (object ["project_id" .= Null, "parent_id" .= Null])
      respStatus detachResp `shouldBe` 200
      let Just detached = decode (respBody detachResp) :: Maybe Task
      detached.projectId `shouldBe` Nothing
      detached.parentId `shouldBe` Nothing

    it "rejects task hierarchy cycles" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("task-cycle-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      projResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Tree" :: T.Text)])
      let Just proj = decode (respBody projResp) :: Maybe Project

      rootResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id, "title" .= ("Root" :: T.Text)])
      let Just root = decode (respBody rootResp) :: Maybe Task

      childResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id, "parent_id" .= root.id, "title" .= ("Child" :: T.Text)])
      let Just child = decode (respBody childResp) :: Maybe Task

      grandchildResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id, "parent_id" .= child.id, "title" .= ("Grandchild" :: T.Text)])
      let Just grandchild = decode (respBody grandchildResp) :: Maybe Task

      cycleResp <- putJSON app (uuidPath "/api/v1/tasks" root.id)
        (object ["parent_id" .= grandchild.id])
      respStatus cycleResp `shouldBe` 409

    it "rejects cross-workspace task project and parent moves" $ \app -> do
      wsAResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("task-parent-ws-a" :: T.Text)])
      let Just wsA = decode (respBody wsAResp) :: Maybe Workspace

      wsBResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("task-parent-ws-b" :: T.Text)])
      let Just wsB = decode (respBody wsBResp) :: Maybe Workspace

      projAResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= wsA.id, "name" .= ("Project A" :: T.Text)])
      let Just projA = decode (respBody projAResp) :: Maybe Project

      projBResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= wsB.id, "name" .= ("Project B" :: T.Text)])
      let Just projB = decode (respBody projBResp) :: Maybe Project

      parentBResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= wsB.id, "project_id" .= projB.id, "title" .= ("Parent B" :: T.Text)])
      let Just parentB = decode (respBody parentBResp) :: Maybe Task

      createWithProjectResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= wsA.id, "project_id" .= projB.id, "title" .= ("Bad Project" :: T.Text)])
      respStatus createWithProjectResp `shouldBe` 400

      createWithParentResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= wsA.id, "project_id" .= projA.id, "parent_id" .= parentB.id, "title" .= ("Bad Parent" :: T.Text)])
      respStatus createWithParentResp `shouldBe` 400

      taskAResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= wsA.id, "project_id" .= projA.id, "title" .= ("Task A" :: T.Text)])
      let Just taskA = decode (respBody taskAResp) :: Maybe Task

      updateProjectResp <- putJSON app (uuidPath "/api/v1/tasks" taskA.id)
        (object ["project_id" .= projB.id])
      respStatus updateProjectResp `shouldBe` 400

      updateParentResp <- putJSON app (uuidPath "/api/v1/tasks" taskA.id)
        (object ["parent_id" .= parentB.id])
      respStatus updateParentResp `shouldBe` 400

      batchUpdateResp <- postJSON app "/api/v1/tasks/batch-update"
        (object ["items" .= [object ["id" .= taskA.id, "project_id" .= projB.id]]])
      respStatus batchUpdateResp `shouldBe` 400

      batchMoveResp <- postJSON app "/api/v1/tasks/batch-move"
        (object ["task_ids" .= [taskA.id], "project_id" .= projB.id])
      respStatus batchMoveResp `shouldBe` 400

  describe "search" $ do
    it "full-text search finds matching memories" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("search-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      _ <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("Haskell is a purely functional programming language" :: T.Text)
          , "memory_type" .= ("short_term" :: T.Text)
          ])
      _ <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("Python is a dynamically typed scripting language" :: T.Text)
          , "memory_type" .= ("short_term" :: T.Text)
          ])
      searchResp <- postJSON app "/api/v1/memories/search"
        (object
          [ "workspace_id" .= ws.id
          , "query" .= ("haskell functional" :: T.Text)
          ])
      respStatus searchResp `shouldBe` 200
      let Just results = decode (respBody searchResp) :: Maybe [Memory]
      length results `shouldSatisfy` (>= 1)

  describe "error handling" $ do
    it "returns 404 for nonexistent memory" $ \app -> do
      resp <- get_ app "/api/v1/memories/00000000-0000-0000-0000-000000000099"
      respStatus resp `shouldBe` 404

    it "returns 404 for nonexistent project" $ \app -> do
      resp <- get_ app "/api/v1/projects/00000000-0000-0000-0000-000000000099"
      respStatus resp `shouldBe` 404

    it "returns 404 for nonexistent task" $ \app -> do
      resp <- get_ app "/api/v1/tasks/00000000-0000-0000-0000-000000000099"
      respStatus resp `shouldBe` 404

    it "returns all memories when workspace_id is omitted" $ \app -> do
      resp <- get_ app "/api/v1/memories"
      respStatus resp `shouldBe` 200

    it "sanitizes unique-violation details" $ \app -> do
      firstResp <- postJSON app "/api/v1/workspaces"
        (object
          [ "name" .= ("safe-errors-a" :: T.Text)
          , "gh_owner" .= ("test-owner" :: T.Text)
          , "gh_repo" .= ("test-repo" :: T.Text)
          ])
      respStatus firstResp `shouldBe` 200

      secondResp <- postJSON app "/api/v1/workspaces"
        (object
          [ "name" .= ("safe-errors-b" :: T.Text)
          , "gh_owner" .= ("test-owner" :: T.Text)
          , "gh_repo" .= ("test-repo" :: T.Text)
          ])
      respStatus secondResp `shouldBe` 409

      let bodyText = decodeUtf8 (LBS.toStrict (respBody secondResp))
      bodyText `shouldSatisfy` T.isInfixOf "Resource already exists"
      bodyText `shouldSatisfy` T.isInfixOf "\"error\":\"conflict\""
      bodyText `shouldSatisfy` (not . T.isInfixOf "duplicate key value violates unique constraint")
      bodyText `shouldSatisfy` (not . T.isInfixOf "uq_workspace")

  describe "GET /health" $ do
    it "returns 200 with status ok" $ \app -> do
      resp <- get_ app "/api/v1/health"
      respStatus resp `shouldBe` 200
      let Just body = decode (respBody resp) :: Maybe Value
      body `shouldSatisfy` \v -> case v of
        Object _ -> True
        _        -> False

  describe "optional bearer auth" $ do
    it "returns 401 when auth is enabled and the bearer token is missing" $ \_ ->
      withAppConfig testAuthCfg $ \app -> do
        resp <- get_ app "/api/v1/health"
        respStatus resp `shouldBe` 401

    it "allows requests when auth is enabled and the bearer token matches" $ \_ ->
      withAppConfig testAuthCfg $ \app -> do
        resp <- runReqWithHeaders app methodGet "/api/v1/health"
          [("Authorization", "Bearer test-secret")]
          ""
        respStatus resp `shouldBe` 200

  describe "session/principal context" $ do
    it "returns local superadmin context for local bootstrap sessions" $ \app -> do
      resp <- get_ app "/api/v1/session"
      respStatus resp `shouldBe` 200
      let Just session = decode (respBody resp) :: Maybe SessionContext
      session.authMode `shouldBe` "local"
      session.principal.actorType `shouldBe` "user"
      session.principal.actorId `shouldBe` "local-user"
      session.principal.authority `shouldBe` "local_superadmin"
      session.globalPermissions.createWorkspace `shouldBe` True
      session.globalPermissions.superadmin `shouldBe` True
      session.workspace `shouldBe` Nothing

    it "denies deployed session context without a principal" $ \_ ->
      withAppEnvConfig deployedAuthCfg $ \_env app -> do
        resp <- get_ app "/api/v1/session"
        respStatus resp `shouldBe` 401

    it "returns deployed grants and effective workspace role context" $ \_ ->
      withTestEnv $ \env -> do
        ws <- createTestWorkspace env "session-context-ws"
        userId <- createTestUser env True False
        _ <- grantWorkspaceRole env ws.id userId Auth.WorkspaceRoleEdit

        withPrincipalApp env deployedAuthCfg (grantPrincipal userId) $ \app -> do
          resp <- get_ app ("/api/v1/session?workspace_id=" <> encodeUtf8 (T.pack (show ws.id)))
          respStatus resp `shouldBe` 200
          let Just session = decode (respBody resp) :: Maybe SessionContext
          session.authMode `shouldBe` "deployed"
          session.principal.authority `shouldBe` "grant_user"
          session.principal.grantUserId `shouldBe` Just userId
          session.globalPermissions.createWorkspace `shouldBe` True
          session.globalPermissions.superadmin `shouldBe` False
          let Just workspaceContext = session.workspace
          workspaceContext.workspaceId `shouldBe` ws.id
          workspaceContext.role `shouldBe` Just "edit"
          workspaceContext.canRead `shouldBe` True
          workspaceContext.canEdit `shouldBe` True
          workspaceContext.canAdmin `shouldBe` False

    it "returns no workspace permissions for deployed users without membership" $ \_ ->
      withTestEnv $ \env -> do
        ws <- createTestWorkspace env "session-context-no-membership"
        userId <- createTestUser env False False

        withPrincipalApp env deployedAuthCfg (grantPrincipal userId) $ \app -> do
          resp <- get_ app ("/api/v1/session?workspace_id=" <> encodeUtf8 (T.pack (show ws.id)))
          respStatus resp `shouldBe` 200
          let Just session = decode (respBody resp) :: Maybe SessionContext
              Just workspaceContext = session.workspace
          workspaceContext.role `shouldBe` Nothing
          workspaceContext.canRead `shouldBe` False
          workspaceContext.canEdit `shouldBe` False
          workspaceContext.canAdmin `shouldBe` False

    it "returns effective workspace admin for deployed superadmins" $ \_ ->
      withTestEnv $ \env -> do
        ws <- createTestWorkspace env "session-context-superadmin"
        userId <- createTestUser env False True

        withPrincipalApp env deployedAuthCfg (grantPrincipal userId) $ \app -> do
          resp <- get_ app ("/api/v1/session?workspace_id=" <> encodeUtf8 (T.pack (show ws.id)))
          respStatus resp `shouldBe` 200
          let Just session = decode (respBody resp) :: Maybe SessionContext
              Just workspaceContext = session.workspace
          session.globalPermissions.superadmin `shouldBe` True
          workspaceContext.role `shouldBe` Just "admin"
          workspaceContext.canRead `shouldBe` True
          workspaceContext.canEdit `shouldBe` True
          workspaceContext.canAdmin `shouldBe` True

    it "returns 404 for session workspace context on deleted workspaces" $ \_ ->
      withTestEnv $ \env -> do
        ws <- createTestWorkspace env "session-context-deleted"
        userId <- createTestUser env False True

        withPrincipalApp env deployedAuthCfg (grantPrincipal userId) $ \app -> do
          deleteResp <- del app (uuidPath "/api/v1/workspaces" ws.id)
          respStatus deleteResp `shouldBe` 200

          resp <- get_ app ("/api/v1/session?workspace_id=" <> encodeUtf8 (T.pack (show ws.id)))
          respStatus resp `shouldBe` 404

  describe "WebSocket auth and event scoping" $ do
    it "denies deployed WebSocket ticket issuance without a principal" $ \_ ->
      withAppEnvConfig deployedAuthCfg $ \env app -> do
        ws <- createTestWorkspace env "ws-ticket-unauthenticated"
        resp <- postJSON app "/api/v1/ws-ticket"
          (object ["workspace_id" .= ws.id])
        respStatus resp `shouldBe` 401

    it "issues deployed WebSocket tickets to workspace readers only" $ \_ ->
      withTestEnv $ \env -> do
        ws <- createTestWorkspace env "ws-ticket-reader"
        readerUserId <- createTestUser env False False
        outsiderUserId <- createTestUser env False False
        _ <- grantWorkspaceRole env ws.id readerUserId Auth.WorkspaceRoleRead

        withPrincipalApp env deployedAuthCfg (grantPrincipal readerUserId) $ \readerApp -> do
          resp <- postJSON readerApp "/api/v1/ws-ticket"
            (object ["workspace_id" .= ws.id])
          respStatus resp `shouldBe` 200
          let Just ticketResp = decode (respBody resp) :: Maybe WebSocketTicketResponse
          ticketResp.ticket `shouldSatisfy` (not . T.null)

        withPrincipalApp env deployedAuthCfg (grantPrincipal outsiderUserId) $ \outsiderApp -> do
          deniedResp <- postJSON outsiderApp "/api/v1/ws-ticket"
            (object ["workspace_id" .= ws.id])
          respStatus deniedResp `shouldBe` 403

    it "matches events only to subscribed workspace connections" $ \_ -> do
      now <- getCurrentTime
      wsA <- UUIDv4.nextRandom
      wsB <- UUIDv4.nextRandom
      entityId <- UUIDv4.nextRandom
      let event = ChangeEvent
            { changeType = Updated
            , entityType = ETMemory
            , entityId = entityId
            , workspaceId = Just wsA
            , timestamp = now
            , requestId = Nothing
            , actorType = Nothing
            , actorId = Nothing
            , actorLabel = Nothing
            , payload = Nothing
            }

      eventVisibleToSubscription SubscribeAllWorkspaces event `shouldBe` True
      eventVisibleToSubscription (SubscribeWorkspace wsA) event `shouldBe` True
      eventVisibleToSubscription (SubscribeWorkspace wsB) event `shouldBe` False

    it "allows default local WebSocket all-workspace access only without remote-bootstrap override" $ \_ -> do
      let defaultLocal = Config.defaultConfig.auth
          exposedLocal = defaultLocal
            { Config.local = defaultLocal.local { Config.allowRemoteBootstrap = True }
            }

      case resolveLocalWebSocketAccess defaultLocal Nothing of
        Just (Just principal, SubscribeAllWorkspaces) -> do
          principalActorType principal `shouldBe` ActorUser
          principalAuthority principal `shouldBe` PrincipalSyntheticLocalSuperadmin
        other -> expectationFailure $ "expected implicit local WebSocket access, got " <> show other

      resolveLocalWebSocketAccess exposedLocal Nothing `shouldBe` Nothing

    it "requires an explicit local WebSocket token when remote bootstrap override is enabled" $ \_ -> do
      let exposedLocal = Config.defaultConfig.auth
            { Config.local = Config.defaultConfig.auth.local
                { Config.allowRemoteBootstrap = True
                , Config.botTokens = [Config.LocalBotTokenConfig { Config.label = "Agent", Config.token = "bot-secret" }]
                }
            }
          exposedStatic = exposedLocal { Config.enabled = True, Config.apiKey = Just "static-secret" }

      case resolveLocalWebSocketAccess exposedLocal (Just "bot-secret") of
        Just (Just principal, SubscribeAllWorkspaces) -> do
          principalActorType principal `shouldBe` ActorBot
          principalAuthority principal `shouldBe` PrincipalSyntheticLocalSuperadmin
        other -> expectationFailure $ "expected local bot WebSocket access, got " <> show other

      case resolveLocalWebSocketAccess exposedStatic (Just "static-secret") of
        Just (Just principal, SubscribeAllWorkspaces) -> do
          principalActorType principal `shouldBe` ActorBot
          principalAuthority principal `shouldBe` PrincipalSyntheticLocalSuperadmin
        other -> expectationFailure $ "expected legacy static WebSocket access, got " <> show other

      resolveLocalWebSocketAccess exposedLocal (Just "wrong-secret") `shouldBe` Nothing

    it "uses WebSocket tickets only once" $ \_ -> do
      wsState <- newWSState
      workspaceId <- UUIDv4.nextRandom
      userId <- UUIDv4.nextRandom
      ticketResp <- createTicket wsState (grantPrincipal userId) workspaceId

      firstUse <- consumeTicket wsState ticketResp.ticket
      secondUse <- consumeTicket wsState ticketResp.ticket

      isJust firstUse `shouldBe` True
      isJust secondUse `shouldBe` False

  describe "server authorization enforcement" $ do
    it "leaves health unauthenticated in deployed mode" $ \_ ->
      withAppConfig deployedAuthCfg $ \app -> do
        resp <- get_ app "/api/v1/health"
        respStatus resp `shouldBe` 200

    it "denies protected endpoints without a principal in deployed mode" $ \_ ->
      withAppConfig deployedAuthCfg $ \app -> do
        listResp <- get_ app "/api/v1/workspaces"
        respStatus listResp `shouldBe` 401

        createResp <- postJSON app "/api/v1/workspaces"
          (object ["name" .= ("denied-ws" :: T.Text)])
        respStatus createResp `shouldBe` 401

    it "authorizes protected endpoints before semantic validation" $ \_ ->
      withAppEnvConfig deployedAuthCfg $ \env app -> do
        ws <- createTestWorkspace env "auth-before-validation"
        resp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= T.replicate (maxMemoryContentBytes + 1) "x"
            , "memory_type" .= ("short_term" :: T.Text)
            ])
        respStatus resp `shouldBe` 401

        memorySearchResp <- postJSON app "/api/v1/memories/search"
          (object ["min_access_count" .= (-1 :: Int)])
        respStatus memorySearchResp `shouldBe` 401

        unifiedSearchResp <- postJSON app "/api/v1/search"
          (object ["query" .= ("" :: T.Text)])
        respStatus unifiedSearchResp `shouldBe` 401

        memoryBatchResp <- postJSON app "/api/v1/memories/batch"
          (toJSON ([] :: [Value]))
        respStatus memoryBatchResp `shouldBe` 401

        projectBatchResp <- postJSON app "/api/v1/projects/batch-delete"
          (object ["ids" .= ([] :: [UUID])])
        respStatus projectBatchResp `shouldBe` 401

        taskBatchResp <- postJSON app "/api/v1/tasks/batch-move"
          (object ["task_ids" .= ([] :: [UUID])])
        respStatus taskBatchResp `shouldBe` 401

        categoryBatchResp <- postJSON app "/api/v1/categories/batch-delete"
          (object ["ids" .= ([] :: [UUID])])
        respStatus categoryBatchResp `shouldBe` 401

    it "enforces deployed create_workspace and superadmin global permissions" $ \_ ->
      withTestEnv $ \env -> do
        plainUserId <- createTestUser env False False
        creatorUserId <- createTestUser env True False
        superadminUserId <- createTestUser env False True

        withPrincipalApp env deployedAuthCfg (grantPrincipal plainUserId) $ \plainApp -> do
          deniedCreateResp <- postJSON plainApp "/api/v1/workspaces"
            (object ["name" .= ("plain-create-denied" :: T.Text)])
          respStatus deniedCreateResp `shouldBe` 403

          deniedGlobalListResp <- get_ plainApp "/api/v1/workspaces"
          respStatus deniedGlobalListResp `shouldBe` 403

        withPrincipalApp env deployedAuthCfg (grantPrincipal creatorUserId) $ \creatorApp -> do
          allowedCreateResp <- postJSON creatorApp "/api/v1/workspaces"
            (object ["name" .= ("creator-create-allowed" :: T.Text)])
          respStatus allowedCreateResp `shouldBe` 200

          deniedGlobalListResp <- get_ creatorApp "/api/v1/workspaces"
          respStatus deniedGlobalListResp `shouldBe` 403

        ws <- createTestWorkspace env "superadmin-bypass-ws"
        withPrincipalApp env deployedAuthCfg (grantPrincipal superadminUserId) $ \superApp -> do
          superCreateResp <- postJSON superApp "/api/v1/workspaces"
            (object ["name" .= ("superadmin-create-allowed" :: T.Text)])
          respStatus superCreateResp `shouldBe` 200

          listMembershipsResp <- get_ superApp (uuidPath "/api/v1/workspaces" ws.id <> "/memberships")
          respStatus listMembershipsResp `shouldBe` 200

          globalWorkspacesResp <- get_ superApp "/api/v1/workspaces"
          respStatus globalWorkspacesResp `shouldBe` 200
          let Just globalWorkspaces = decode (respBody globalWorkspacesResp) :: Maybe (PaginatedResult Workspace)
          map (.id) globalWorkspaces.items `shouldContain` [ws.id]

    it "enforces deployed read edit and admin workspace roles" $ \_ ->
      withTestEnv $ \env -> do
        ws <- createTestWorkspace env "deployed-role-matrix"
        readUserId <- createTestUser env False False
        editUserId <- createTestUser env False False
        adminUserId <- createTestUser env False False
        outsiderUserId <- createTestUser env False False
        _ <- grantWorkspaceRole env ws.id readUserId Auth.WorkspaceRoleRead
        _ <- grantWorkspaceRole env ws.id editUserId Auth.WorkspaceRoleEdit
        _ <- grantWorkspaceRole env ws.id adminUserId Auth.WorkspaceRoleAdmin

        withPrincipalApp env deployedAuthCfg (grantPrincipal readUserId) $ \readApp -> do
          getWsResp <- get_ readApp (uuidPath "/api/v1/workspaces" ws.id)
          respStatus getWsResp `shouldBe` 200
          deniedCreateMemoryResp <- postJSON readApp "/api/v1/memories"
            (object ["workspace_id" .= ws.id, "content" .= ("read denied" :: T.Text), "memory_type" .= ("short_term" :: T.Text)])
          respStatus deniedCreateMemoryResp `shouldBe` 403

          deniedCreateProjectResp <- postJSON readApp "/api/v1/projects"
            (object ["workspace_id" .= ws.id, "name" .= ("read-project-denied" :: T.Text)])
          respStatus deniedCreateProjectResp `shouldBe` 403

        mem <- withPrincipalApp env deployedAuthCfg (grantPrincipal editUserId) $ \editApp -> do
          createMemoryResp <- postJSON editApp "/api/v1/memories"
            (object ["workspace_id" .= ws.id, "content" .= ("edit allowed" :: T.Text), "memory_type" .= ("short_term" :: T.Text)])
          respStatus createMemoryResp `shouldBe` 200
          let Just createdMem = decode (respBody createMemoryResp) :: Maybe Memory

          createProjectResp <- postJSON editApp "/api/v1/projects"
            (object ["workspace_id" .= ws.id, "name" .= ("edit-project-allowed" :: T.Text)])
          respStatus createProjectResp `shouldBe` 200
          let Just createdProject = decode (respBody createProjectResp) :: Maybe Project

          createTaskResp <- postJSON editApp "/api/v1/tasks"
            (object ["workspace_id" .= ws.id, "project_id" .= createdProject.id, "title" .= ("edit-task-allowed" :: T.Text)])
          respStatus createTaskResp `shouldBe` 200

          deleteMemoryResp <- del editApp (uuidPath "/api/v1/memories" createdMem.id)
          respStatus deleteMemoryResp `shouldBe` 200
          deniedPurgeResp <- del editApp (uuidPath "/api/v1/memories" createdMem.id <> "/purge")
          respStatus deniedPurgeResp `shouldBe` 403
          pure createdMem

        withPrincipalApp env deployedAuthCfg (grantPrincipal adminUserId) $ \adminApp -> do
          listMembershipsResp <- get_ adminApp (uuidPath "/api/v1/workspaces" ws.id <> "/memberships")
          respStatus listMembershipsResp `shouldBe` 200

          purgeResp <- del adminApp (uuidPath "/api/v1/memories" mem.id <> "/purge")
          respStatus purgeResp `shouldBe` 200

        withPrincipalApp env deployedAuthCfg (grantPrincipal outsiderUserId) $ \outsiderApp -> do
          deniedGetResp <- get_ outsiderApp (uuidPath "/api/v1/workspaces" ws.id)
          respStatus deniedGetResp `shouldBe` 403

    it "denies deployed entity-by-id reads across workspace boundaries" $ \_ ->
      withTestEnv $ \env -> do
        wsA <- createTestWorkspace env "entity-denial-a"
        wsB <- createTestWorkspace env "entity-denial-b"
        userId <- createTestUser env False False
        _ <- grantWorkspaceRole env wsA.id userId Auth.WorkspaceRoleRead
        memB <- Mem.createMemory env.pool CreateMemory
          { workspaceId = wsB.id
          , content = "workspace b secret"
          , summary = Nothing
          , memoryType = ShortTerm
          , importance = Nothing
          , metadata = Nothing
          , expiresAt = Nothing
          , source = Nothing
          , confidence = Nothing
          , pinned = Nothing
          , tags = Nothing
          , ftsLanguage = Nothing
          }
        projectB <- Proj.createProject env.pool CreateProject
          { workspaceId = wsB.id
          , name = "workspace b project"
          , description = Nothing
          , parentId = Nothing
          , priority = Nothing
          , metadata = Nothing
          }
        taskB <- Task.createTask env.pool CreateTask
          { workspaceId = wsB.id
          , projectId = Just projectB.id
          , parentId = Nothing
          , title = "workspace b task"
          , description = Nothing
          , priority = Nothing
          , metadata = Nothing
          , dueAt = Nothing
          }
        categoryB <- Cat.createCategory env.pool CreateMemoryCategory
          { workspaceId = Just wsB.id
          , name = "workspace-b-category"
          , description = Nothing
          , parentId = Nothing
          }
        viewB <- SV.createSavedView env.pool CreateSavedView
          { workspaceId = wsB.id
          , name = "workspace-b-view"
          , description = Nothing
          , entityType = "memory_list"
          , queryParams = object ["workspace_id" .= wsB.id]
          }

        withPrincipalApp env deployedAuthCfg (grantPrincipal userId) $ \app -> do
          deniedGetResp <- get_ app (uuidPath "/api/v1/memories" memB.id)
          respStatus deniedGetResp `shouldBe` 403

          deniedProjectResp <- get_ app (uuidPath "/api/v1/projects" projectB.id)
          respStatus deniedProjectResp `shouldBe` 403

          deniedTaskResp <- get_ app (uuidPath "/api/v1/tasks" taskB.id)
          respStatus deniedTaskResp `shouldBe` 403

          deniedCategoryResp <- get_ app (uuidPath "/api/v1/categories" categoryB.id)
          respStatus deniedCategoryResp `shouldBe` 403

          deniedSavedViewResp <- get_ app (uuidPath "/api/v1/saved-views" viewB.id)
          respStatus deniedSavedViewResp `shouldBe` 403

          deniedWorkspaceListResp <- get_ app ("/api/v1/memories?workspace_id=" <> encodeUtf8 (T.pack (show wsB.id)))
          respStatus deniedWorkspaceListResp `shouldBe` 403

          deniedProjectListResp <- get_ app ("/api/v1/projects?workspace_id=" <> encodeUtf8 (T.pack (show wsB.id)))
          respStatus deniedProjectListResp `shouldBe` 403

          deniedTaskListResp <- get_ app ("/api/v1/tasks?workspace_id=" <> encodeUtf8 (T.pack (show wsB.id)))
          respStatus deniedTaskListResp `shouldBe` 403

          deniedTaskProjectListResp <- get_ app ("/api/v1/tasks?project_id=" <> encodeUtf8 (T.pack (show projectB.id)))
          respStatus deniedTaskProjectListResp `shouldBe` 403

          deniedCategoryListResp <- get_ app ("/api/v1/categories?workspace_id=" <> encodeUtf8 (T.pack (show wsB.id)))
          respStatus deniedCategoryListResp `shouldBe` 403

          deniedSavedViewListResp <- get_ app ("/api/v1/saved-views?workspace_id=" <> encodeUtf8 (T.pack (show wsB.id)))
          respStatus deniedSavedViewListResp `shouldBe` 403

          deniedMemorySearchResp <- postJSON app "/api/v1/memories/search"
            (object ["workspace_id" .= wsB.id, "query" .= ("workspace" :: T.Text)])
          respStatus deniedMemorySearchResp `shouldBe` 403

          deniedCategorySearchResp <- postJSON app "/api/v1/memories/search"
            (object ["category_id" .= categoryB.id, "query" .= ("workspace" :: T.Text)])
          respStatus deniedCategorySearchResp `shouldBe` 403

          deniedUnifiedWorkspaceSearchResp <- postJSON app "/api/v1/search"
            (object ["workspace_id" .= wsB.id, "query" .= ("workspace" :: T.Text)])
          respStatus deniedUnifiedWorkspaceSearchResp `shouldBe` 403

          deniedUnifiedProjectSearchResp <- postJSON app "/api/v1/search"
            (object ["project_id" .= projectB.id, "query" .= ("workspace" :: T.Text)])
          respStatus deniedUnifiedProjectSearchResp `shouldBe` 403

          deniedUnifiedCategorySearchResp <- postJSON app "/api/v1/search"
            (object ["category_id" .= categoryB.id, "query" .= ("workspace" :: T.Text)])
          respStatus deniedUnifiedCategorySearchResp `shouldBe` 403

    it "rejects cross-workspace project-memory links" $ \app -> do
      wsAResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("link-ws-a" :: T.Text)])
      let Just wsA = decode (respBody wsAResp) :: Maybe Workspace

      wsBResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("link-ws-b" :: T.Text)])
      let Just wsB = decode (respBody wsBResp) :: Maybe Workspace

      projResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= wsA.id, "name" .= ("Project A" :: T.Text)])
      let Just proj = decode (respBody projResp) :: Maybe Project

      memResp <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= wsB.id
          , "content" .= ("Memory B" :: T.Text)
          , "memory_type" .= ("short_term" :: T.Text)
          ])
      let Just mem = decode (respBody memResp) :: Maybe Memory

      linkResp <- postJSON app (uuidPath "/api/v1/projects" proj.id <> "/memories")
        (object ["memory_id" .= mem.id])
      respStatus linkResp `shouldBe` 400

    it "rejects cross-workspace memory links and task dependencies" $ \app -> do
      wsAResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("relation-ws-a" :: T.Text)])
      let Just wsA = decode (respBody wsAResp) :: Maybe Workspace

      wsBResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("relation-ws-b" :: T.Text)])
      let Just wsB = decode (respBody wsBResp) :: Maybe Workspace

      memAResp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= wsA.id, "content" .= ("A" :: T.Text), "memory_type" .= ("short_term" :: T.Text)])
      let Just memA = decode (respBody memAResp) :: Maybe Memory

      memBResp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= wsB.id, "content" .= ("B" :: T.Text), "memory_type" .= ("short_term" :: T.Text)])
      let Just memB = decode (respBody memBResp) :: Maybe Memory

      memoryLinkResp <- postJSON app (uuidPath "/api/v1/memories" memA.id <> "/links")
        (object ["target_id" .= memB.id, "relation_type" .= ("related" :: T.Text)])
      respStatus memoryLinkResp `shouldBe` 400

      taskAResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= wsA.id, "title" .= ("Task A" :: T.Text)])
      let Just taskA = decode (respBody taskAResp) :: Maybe Task

      taskBResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= wsB.id, "title" .= ("Task B" :: T.Text)])
      let Just taskB = decode (respBody taskBResp) :: Maybe Task

      depResp <- postJSON app (uuidPath "/api/v1/tasks" taskA.id <> "/dependencies")
        (object ["depends_on_id" .= taskB.id])
      respStatus depResp `shouldBe` 400

  describe "request principal resolution" $ do
    it "keeps local bot synthetic authority local-mode only" $ \_ -> do
      let localCfg = Config.defaultConfig
            { Config.auth = Config.defaultConfig.auth
                { Config.mode = Config.AuthModeLocal
                , Config.local = Config.LocalAuthConfig
                    { Config.bootstrapEnabled = True
                    , Config.allowRemoteBootstrap = False
                    , Config.botTokens = [Config.LocalBotTokenConfig { Config.label = "Agent", Config.token = "bot-secret" }]
                    }
                }
            }
          deployedCfg = localCfg
            { Config.auth = localCfg.auth { Config.mode = Config.AuthModeDeployed }
            }
          req = defaultRequest
            { Wai.requestHeaders = [("Authorization", "Bearer bot-secret")]
            }

      fmap principalActorType (resolveRequestPrincipal localCfg.auth req) `shouldBe` Just ActorBot
      fmap principalAuthority (resolveRequestPrincipal localCfg.auth req) `shouldBe` Just PrincipalSyntheticLocalSuperadmin
      resolveRequestPrincipal deployedCfg.auth req `shouldBe` Nothing

    it "keeps legacy static bearer synthetic authority local-mode only" $ \_ -> do
      let deployedCfg = testAuthCfg
            { Config.auth = testAuthCfg.auth { Config.mode = Config.AuthModeDeployed }
            }
          req = defaultRequest
            { Wai.requestHeaders = [("Authorization", "Bearer test-secret")]
            }

      fmap principalAuthority (resolveRequestPrincipal testAuthCfg.auth req) `shouldBe` Just PrincipalSyntheticLocalSuperadmin
      resolveRequestPrincipal deployedCfg.auth req `shouldBe` Nothing

    it "resolves deployed PATs through persisted access_tokens" $ \_ ->
      withTestEnv $ \env -> do
        grantUserId <- createTestUser env True False
        tokenId <- createAccessToken env grantUserId ActorBot "Deploy Bot" "pat-secret"
        tracker <- newAccessTracker env.pool 3600
        wsState <- newWSState
        let cfg = deployedAuthCfg { Config.cors = CorsConfig { allowedOrigins = ["*"] } }
        app <- mkApp id cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState Nothing True

        wsResp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("Authorization", "Bearer pat-secret")]
          (encode (object ["name" .= ("pat-principal-ws" :: T.Text)]))
        respStatus wsResp `shouldBe` 200
        accessTokenLastUsedIsSet env tokenId >>= (`shouldBe` True)
        let Just ws = decode (respBody wsResp) :: Maybe Workspace

        tokenAuditRows <- getAuditLogRows env.pool "access_token" (T.pack $ show tokenId)
        mapM_ assertNoTokenHashAuditSnapshot tokenAuditRows
        let lastTokenAudit = last tokenAuditRows
        lastTokenAudit.actorType `shouldBe` Just "bot"
        lastTokenAudit.actorId `shouldBe` Just (T.pack $ show tokenId)
        lastTokenAudit.actorLabel `shouldBe` Just "Deploy Bot"

        rows <- getAuditLogRows env.pool "workspace" (T.pack (show ws.id))
        let [created] = rows
        created.actorType `shouldBe` Just "bot"
        created.actorId `shouldBe` Just (T.pack $ show tokenId)
        created.actorLabel `shouldBe` Just "Deploy Bot"

    it "accepts and revokes operator-issued deployed PATs" $ \_ ->
      withTestEnv $ \env -> do
        grantUserId <- createTestUser env True False
        Right issued <- AuthTokens.issueAccessToken env.pool AuthTokens.IssueAccessTokenInput
          { AuthTokens.grantUserId = grantUserId
          , AuthTokens.actorType = AuthTokens.AccessTokenActorBot
          , AuthTokens.actorLabel = "Issued MCP Bot"
          , AuthTokens.expiresAt = Nothing
          }
        tracker <- newAccessTracker env.pool 3600
        wsState <- newWSState
        let cfg = deployedAuthCfg { Config.cors = CorsConfig { allowedOrigins = ["*"] } }
        app <- mkApp id cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState Nothing True

        createResp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("Authorization", "Bearer " <> encodeUtf8 issued.rawToken)]
          (encode (object ["name" .= ("operator-issued-pat-ws" :: T.Text)]))
        respStatus createResp `shouldBe` 200

        revoked <- AuthTokens.revokeAccessToken env.pool issued.tokenId
        revoked `shouldBe` True
        deniedResp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("Authorization", "Bearer " <> encodeUtf8 issued.rawToken)]
          (encode (object ["name" .= ("operator-issued-pat-denied" :: T.Text)]))
        respStatus deniedResp `shouldBe` 401

    it "rejects revoked deployed PATs" $ \_ ->
      withTestEnv $ \env -> do
        grantUserId <- createTestUser env True False
        tokenId <- createAccessToken env grantUserId ActorBot "Revoked Bot" "revoked-pat-secret"
        revokeAccessToken env tokenId
        tracker <- newAccessTracker env.pool 3600
        wsState <- newWSState
        let cfg = deployedAuthCfg { Config.cors = CorsConfig { allowedOrigins = ["*"] } }
        app <- mkApp id cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState Nothing True

        wsResp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("Authorization", "Bearer revoked-pat-secret")]
          (encode (object ["name" .= ("revoked-pat-ws" :: T.Text)]))
        respStatus wsResp `shouldBe` 401

    it "rejects expired deployed PATs" $ \_ ->
      withTestEnv $ \env -> do
        grantUserId <- createTestUser env True False
        tokenId <- createAccessToken env grantUserId ActorBot "Expired Bot" "expired-pat-secret"
        expireAccessToken env tokenId
        tracker <- newAccessTracker env.pool 3600
        wsState <- newWSState
        let cfg = deployedAuthCfg { Config.cors = CorsConfig { allowedOrigins = ["*"] } }
        app <- mkApp id cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState Nothing True

        wsResp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("Authorization", "Bearer expired-pat-secret")]
          (encode (object ["name" .= ("expired-pat-ws" :: T.Text)]))
        respStatus wsResp `shouldBe` 401

    it "validates deployed JWTs and maps subject claims to users" $ \_ ->
      withTestEnv $ \env -> do
        Right bootstrapResult <- AuthBootstrap.bootstrapSuperadmin env.pool AuthBootstrap.BootstrapSuperadminInput
          { AuthBootstrap.authSubject = "provider-user-1"
          , AuthBootstrap.email = Nothing
          , AuthBootstrap.displayName = Just "provider-user-1"
          , AuthBootstrap.force = False
          }
        let userId = bootstrapResult.userId
        let jwk = JWK.fromOctets (encodeUtf8 testJwtSecret)
            cfg = (deployedJwtCfg (JWK.JWKSet [jwk])) { Config.cors = CorsConfig { allowedOrigins = ["*"] } }
        token <- signTestJwt testJwtSecret "provider-user-1"
        tracker <- newAccessTracker env.pool 3600
        wsState <- newWSState
        app <- mkApp id cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState Nothing True

        wsResp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("Authorization", "Bearer " <> encodeUtf8 token)]
          (encode (object ["name" .= ("jwt-principal-ws" :: T.Text)]))
        respStatus wsResp `shouldBe` 200
        let Just ws = decode (respBody wsResp) :: Maybe Workspace

        rows <- getAuditLogRows env.pool "workspace" (T.pack (show ws.id))
        let [created] = rows
        created.actorType `shouldBe` Just "user"
        created.actorId `shouldBe` Just (T.pack $ show userId)
        created.actorLabel `shouldBe` Just "provider-user-1"

        sessionResp <- runReqWithHeaders app methodGet "/api/v1/session"
          [("Authorization", "Bearer " <> encodeUtf8 token)]
          ""
        respStatus sessionResp `shouldBe` 200
        let Just session = decode (respBody sessionResp) :: Maybe SessionContext
        session.globalPermissions.superadmin `shouldBe` True

    it "rejects deployed JWTs that fail validation" $ \_ ->
      withTestEnv $ \env -> do
        _userId <- createTestUserWithSubject env "provider-user-2" True False
        let jwk = JWK.fromOctets (encodeUtf8 testJwtSecret)
            cfg = (deployedJwtCfg (JWK.JWKSet [jwk]))
              { Config.auth = (deployedJwtCfg (JWK.JWKSet [jwk])).auth
                  { Config.deployed = (deployedJwtCfg (JWK.JWKSet [jwk])).auth.deployed
                      { Config.audience = Just "different-audience" }
                  }
              , Config.cors = CorsConfig { allowedOrigins = ["*"] }
              }
        token <- signTestJwt testJwtSecret "provider-user-2"
        tracker <- newAccessTracker env.pool 3600
        wsState <- newWSState
        app <- mkApp id cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState Nothing True

        wsResp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("Authorization", "Bearer " <> encodeUtf8 token)]
          (encode (object ["name" .= ("jwt-denied-ws" :: T.Text)]))
        respStatus wsResp `shouldBe` 401

    it "fails closed for non-HTTPS deployed JWKS URLs" $ \_ ->
      withTestEnv $ \env -> do
        _userId <- createTestUserWithSubject env "provider-user-3" True False
        let cfg = deployedAuthCfg
              { Config.auth = deployedAuthCfg.auth
                  { Config.deployed = deployedAuthCfg.auth.deployed
                      { Config.issuer = Just testJwtIssuer
                      , Config.audience = Just testJwtAudience
                      , Config.jwksUrl = Just "http://issuer.example/jwks.json"
                      }
                  }
              , Config.cors = CorsConfig { allowedOrigins = ["*"] }
              }
        token <- signTestJwt testJwtSecret "provider-user-3"
        tracker <- newAccessTracker env.pool 3600
        wsState <- newWSState
        app <- mkApp id cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState Nothing True

        wsResp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("Authorization", "Bearer " <> encodeUtf8 token)]
          (encode (object ["name" .= ("jwt-http-jwks-denied-ws" :: T.Text)]))
        respStatus wsResp `shouldBe` 401

  describe "request context actor attribution" $ do
    it "records the synthetic local user for normal local HTTP requests" $ \_ ->
      withAppEnv $ \env app -> do
        wsResp <- postJSON app "/api/v1/workspaces"
          (object ["name" .= ("ctx-local-ws" :: T.Text)])
        respStatus wsResp `shouldBe` 200
        let Just ws = decode (respBody wsResp) :: Maybe Workspace
        rows <- getAuditLogRows env.pool "workspace" (T.pack (show ws.id))
        let [created] = rows
        created.actorType `shouldBe` Just "user"
        created.actorId `shouldBe` Just "local-user"
        created.actorLabel `shouldBe` Just "Local User"
        created.workspaceId `shouldBe` Just ws.id

    it "records the legacy static bearer actor for authenticated local token requests" $ \_ ->
      withAppEnvConfig testAuthCfg $ \env app -> do
        wsResp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("Authorization", "Bearer test-secret")]
          (encode (object ["name" .= ("ctx-bot-ws" :: T.Text)]))
        respStatus wsResp `shouldBe` 200
        let Just ws = decode (respBody wsResp) :: Maybe Workspace
        rows <- getAuditLogRows env.pool "workspace" (T.pack (show ws.id))
        let [created] = rows
        created.actorType `shouldBe` Just "bot"
        created.actorId `shouldBe` Just "legacy-static-bearer"
        created.actorLabel `shouldBe` Just "Legacy Static Bearer"
        created.workspaceId `shouldBe` Just ws.id

    it "records configured local bot actors while preserving local superadmin behavior" $ \_ -> do
      let localBotCfg = Config.defaultConfig
            { Config.auth = Config.defaultConfig.auth
                { Config.mode = Config.AuthModeLocal
                , Config.local = Config.LocalAuthConfig
                    { Config.bootstrapEnabled = True
                    , Config.allowRemoteBootstrap = False
                    , Config.botTokens = [Config.LocalBotTokenConfig { Config.label = "Codex Bot", Config.token = "codex-secret" }]
                    }
                }
            }
      withAppEnvConfig localBotCfg $ \env app -> do
        wsResp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("Authorization", "Bearer codex-secret")]
          (encode (object ["name" .= ("ctx-local-bot-ws" :: T.Text)]))
        respStatus wsResp `shouldBe` 200
        let Just ws = decode (respBody wsResp) :: Maybe Workspace
        purgeActiveResp <- del app (uuidPath "/api/v1/workspaces" ws.id <> "/purge")
        respStatus purgeActiveResp `shouldBe` 409

        rows <- getAuditLogRows env.pool "workspace" (T.pack (show ws.id))
        let created = head rows
        created.actorType `shouldBe` Just "bot"
        created.actorId `shouldBe` Just "local-bot:Codex Bot"
        created.actorLabel `shouldBe` Just "Codex Bot"
        created.workspaceId `shouldBe` Just ws.id

  describe "rate limiting" $ do
    it "returns 429 when the configured burst is exceeded" $ \_ ->
      withAppConfig testRateLimitCfg $ \app -> do
        firstResp <- get_ app "/api/v1/health"
        secondResp <- get_ app "/api/v1/health"
        respStatus firstResp `shouldBe` 200
        respStatus secondResp `shouldBe` 429

  describe "list filtering" $ do
    it "filters memories by created_after" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("memory-filter-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      firstMemResp <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("older memory" :: T.Text)
          , "memory_type" .= ("short_term" :: T.Text)
          ])
      secondMemResp <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("newer memory" :: T.Text)
          , "memory_type" .= ("short_term" :: T.Text)
          ])
      let Just firstMem = decode (respBody firstMemResp) :: Maybe Memory
      let Just secondMem = decode (respBody secondMemResp) :: Maybe Memory

      resp <- get_ app
        ( "/api/v1/memories?workspace_id=" <> encodeUtf8 (T.pack (show ws.id))
       <> "&created_after=" <> encodeUtf8 (T.pack (iso8601Show secondMem.createdAt))
        )
      respStatus resp `shouldBe` 200
      let Just page = decode (respBody resp) :: Maybe (PaginatedResult Memory)
      map (\item -> item.id) page.items `shouldBe` [secondMem.id]
      page.items `shouldNotSatisfy` any ((== firstMem.id) . (\item -> item.id))

    it "filters projects by updated_after" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("project-filter-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      oldProjResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Old Project" :: T.Text)])
      newProjResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Updated Project" :: T.Text)])
      let Just oldProj = decode (respBody oldProjResp) :: Maybe Project
      let Just newProj = decode (respBody newProjResp) :: Maybe Project
      updatedProjResp <- putJSON app (uuidPath "/api/v1/projects" newProj.id)
        (object ["status" .= ("paused" :: T.Text)])
      let Just updatedProj = decode (respBody updatedProjResp) :: Maybe Project

      resp <- get_ app
        ( "/api/v1/projects?workspace_id=" <> encodeUtf8 (T.pack (show ws.id))
       <> "&updated_after=" <> encodeUtf8 (T.pack (iso8601Show updatedProj.updatedAt))
        )
      respStatus resp `shouldBe` 200
      let Just page = decode (respBody resp) :: Maybe (PaginatedResult Project)
      map (\item -> item.id) page.items `shouldBe` [updatedProj.id]
      page.items `shouldNotSatisfy` any ((== oldProj.id) . (\item -> item.id))

    it "filters tasks by priority" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("task-filter-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      projResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Task Filter Project" :: T.Text)])
      let Just proj = decode (respBody projResp) :: Maybe Project

      lowTaskResp <- postJSON app "/api/v1/tasks"
        (object
          [ "workspace_id" .= ws.id
          , "project_id" .= proj.id
          , "title" .= ("Low priority" :: T.Text)
          , "priority" .= (2 :: Int)
          ])
      highTaskResp <- postJSON app "/api/v1/tasks"
        (object
          [ "workspace_id" .= ws.id
          , "project_id" .= proj.id
          , "title" .= ("High priority" :: T.Text)
          , "priority" .= (8 :: Int)
          ])
      let Just lowTask = decode (respBody lowTaskResp) :: Maybe Task
      let Just highTask = decode (respBody highTaskResp) :: Maybe Task

      resp <- get_ app
        ( "/api/v1/tasks?workspace_id=" <> encodeUtf8 (T.pack (show ws.id))
       <> "&priority=8"
        )
      respStatus resp `shouldBe` 200
      let Just page = decode (respBody resp) :: Maybe (PaginatedResult Task)
      map (\item -> item.id) page.items `shouldBe` [highTask.id]
      page.items `shouldNotSatisfy` any ((== lowTask.id) . (\item -> item.id))

    it "filters and sorts memories by access_count" $ \_ ->
      withAppEnv $ \env app -> do
        wsResp <- postJSON app "/api/v1/workspaces"
          (object ["name" .= ("memory-access-filter-ws" :: T.Text)])
        let Just ws = decode (respBody wsResp) :: Maybe Workspace

        lowMemResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("low access" :: T.Text)
            , "memory_type" .= ("short_term" :: T.Text)
            ])
        highMemResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("high access" :: T.Text)
            , "memory_type" .= ("short_term" :: T.Text)
            ])
        untouchedMemResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("untouched access" :: T.Text)
            , "memory_type" .= ("short_term" :: T.Text)
            ])
        let Just lowMem = decode (respBody lowMemResp) :: Maybe Memory
        let Just highMem = decode (respBody highMemResp) :: Maybe Memory
        let Just untouchedMem = decode (respBody untouchedMemResp) :: Maybe Memory
        Mem.touchMemory env.pool lowMem.id
        Mem.touchMemory env.pool highMem.id
        Mem.touchMemory env.pool highMem.id

        resp <- get_ app
          ( "/api/v1/memories?workspace_id=" <> encodeUtf8 (T.pack (show ws.id))
         <> "&min_access_count=1&sort_by=access_count"
          )
        respStatus resp `shouldBe` 200
        let Just page = decode (respBody resp) :: Maybe (PaginatedResult Memory)
        map (.id) page.items `shouldBe` [highMem.id, lowMem.id]
        page.items `shouldNotSatisfy` any ((== untouchedMem.id) . (.id))

  describe "linked memory list filters" $ do
    it "filters project-linked memories by query, tag, importance, type, and access_count" $ \_ ->
      withAppEnv $ \env app -> do
        wsResp <- postJSON app "/api/v1/workspaces"
          (object ["name" .= ("project-linked-filter-ws" :: T.Text)])
        let Just ws = decode (respBody wsResp) :: Maybe Workspace

        projResp <- postJSON app "/api/v1/projects"
          (object ["workspace_id" .= ws.id, "name" .= ("Filter Project" :: T.Text)])
        let Just proj = decode (respBody projResp) :: Maybe Project

        matchingResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("alpha haskell note" :: T.Text)
            , "memory_type" .= ("long_term" :: T.Text)
            , "importance" .= (8 :: Int)
            , "tags" .= (["keep"] :: [T.Text])
            ])
        wrongTagResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("alpha haskell note" :: T.Text)
            , "memory_type" .= ("long_term" :: T.Text)
            , "importance" .= (8 :: Int)
            , "tags" .= (["drop"] :: [T.Text])
            ])
        lowImportanceResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("alpha haskell note" :: T.Text)
            , "memory_type" .= ("long_term" :: T.Text)
            , "importance" .= (2 :: Int)
            , "tags" .= (["keep"] :: [T.Text])
            ])
        wrongTypeResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("alpha haskell note" :: T.Text)
            , "memory_type" .= ("short_term" :: T.Text)
            , "importance" .= (8 :: Int)
            , "tags" .= (["keep"] :: [T.Text])
            ])
        lowAccessResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("alpha haskell note" :: T.Text)
            , "memory_type" .= ("long_term" :: T.Text)
            , "importance" .= (8 :: Int)
            , "tags" .= (["keep"] :: [T.Text])
            ])
        wrongQueryResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("gamma note" :: T.Text)
            , "memory_type" .= ("long_term" :: T.Text)
            , "importance" .= (8 :: Int)
            , "tags" .= (["keep"] :: [T.Text])
            ])
        let Just matchingMem = decode (respBody matchingResp) :: Maybe Memory
        let Just wrongTagMem = decode (respBody wrongTagResp) :: Maybe Memory
        let Just lowImportanceMem = decode (respBody lowImportanceResp) :: Maybe Memory
        let Just wrongTypeMem = decode (respBody wrongTypeResp) :: Maybe Memory
        let Just lowAccessMem = decode (respBody lowAccessResp) :: Maybe Memory
        let Just wrongQueryMem = decode (respBody wrongQueryResp) :: Maybe Memory
        Mem.touchMemory env.pool matchingMem.id
        Mem.touchMemory env.pool wrongTagMem.id
        Mem.touchMemory env.pool lowImportanceMem.id
        Mem.touchMemory env.pool wrongTypeMem.id
        Mem.touchMemory env.pool wrongQueryMem.id

        linkProjectMemResp <- postJSON app (uuidPath "/api/v1/projects" proj.id <> "/memories")
          (object ["memory_id" .= matchingMem.id])
        respStatus linkProjectMemResp `shouldBe` 200
        mapM_ (\mid -> do
            linkResp <- postJSON app (uuidPath "/api/v1/projects" proj.id <> "/memories") (object ["memory_id" .= mid])
            respStatus linkResp `shouldBe` 200)
          [wrongTagMem.id, lowImportanceMem.id, wrongTypeMem.id, lowAccessMem.id, wrongQueryMem.id]

        resp <- get_ app
          ( uuidPath "/api/v1/projects" proj.id <> "/memories?query=alpha%20haskell&tag=keep&min_importance=5&memory_type=long_term&min_access_count=1" )
        respStatus resp `shouldBe` 200
        let Just mems = decode (respBody resp) :: Maybe [Memory]
        map (.id) mems `shouldBe` [matchingMem.id]

    it "filters task-linked memories by query, tag, importance, type, and access_count" $ \_ ->
      withAppEnv $ \env app -> do
        wsResp <- postJSON app "/api/v1/workspaces"
          (object ["name" .= ("task-linked-filter-ws" :: T.Text)])
        let Just ws = decode (respBody wsResp) :: Maybe Workspace

        projResp <- postJSON app "/api/v1/projects"
          (object ["workspace_id" .= ws.id, "name" .= ("Task Filter Project" :: T.Text)])
        let Just proj = decode (respBody projResp) :: Maybe Project
        taskResp <- postJSON app "/api/v1/tasks"
          (object ["workspace_id" .= ws.id, "project_id" .= proj.id, "title" .= ("Filter Task" :: T.Text)])
        let Just task = decode (respBody taskResp) :: Maybe Task

        matchingResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("beta haskell note" :: T.Text)
            , "memory_type" .= ("long_term" :: T.Text)
            , "importance" .= (7 :: Int)
            , "tags" .= (["keep"] :: [T.Text])
            ])
        wrongTagResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("beta haskell note" :: T.Text)
            , "memory_type" .= ("long_term" :: T.Text)
            , "importance" .= (7 :: Int)
            , "tags" .= (["drop"] :: [T.Text])
            ])
        lowImportanceResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("beta haskell note" :: T.Text)
            , "memory_type" .= ("long_term" :: T.Text)
            , "importance" .= (2 :: Int)
            , "tags" .= (["keep"] :: [T.Text])
            ])
        wrongTypeResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("beta haskell note" :: T.Text)
            , "memory_type" .= ("short_term" :: T.Text)
            , "importance" .= (7 :: Int)
            , "tags" .= (["keep"] :: [T.Text])
            ])
        lowAccessResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("beta haskell note" :: T.Text)
            , "memory_type" .= ("long_term" :: T.Text)
            , "importance" .= (7 :: Int)
            , "tags" .= (["keep"] :: [T.Text])
            ])
        wrongQueryResp <- postJSON app "/api/v1/memories"
          (object
            [ "workspace_id" .= ws.id
            , "content" .= ("gamma note" :: T.Text)
            , "memory_type" .= ("long_term" :: T.Text)
            , "importance" .= (7 :: Int)
            , "tags" .= (["keep"] :: [T.Text])
            ])
        let Just matchingMem = decode (respBody matchingResp) :: Maybe Memory
        let Just wrongTagMem = decode (respBody wrongTagResp) :: Maybe Memory
        let Just lowImportanceMem = decode (respBody lowImportanceResp) :: Maybe Memory
        let Just wrongTypeMem = decode (respBody wrongTypeResp) :: Maybe Memory
        let Just lowAccessMem = decode (respBody lowAccessResp) :: Maybe Memory
        let Just wrongQueryMem = decode (respBody wrongQueryResp) :: Maybe Memory
        Mem.touchMemory env.pool matchingMem.id
        Mem.touchMemory env.pool wrongTagMem.id
        Mem.touchMemory env.pool lowImportanceMem.id
        Mem.touchMemory env.pool wrongTypeMem.id
        Mem.touchMemory env.pool wrongQueryMem.id

        linkTaskMemResp <- postJSON app (uuidPath "/api/v1/tasks" task.id <> "/memories")
          (object ["memory_id" .= matchingMem.id])
        respStatus linkTaskMemResp `shouldBe` 200
        mapM_ (\mid -> do
            linkResp <- postJSON app (uuidPath "/api/v1/tasks" task.id <> "/memories") (object ["memory_id" .= mid])
            respStatus linkResp `shouldBe` 200)
          [wrongTagMem.id, lowImportanceMem.id, wrongTypeMem.id, lowAccessMem.id, wrongQueryMem.id]

        resp <- get_ app
          ( uuidPath "/api/v1/tasks" task.id <> "/memories?query=beta%20haskell&tag=keep&min_importance=5&memory_type=long_term&min_access_count=1" )
        respStatus resp `shouldBe` 200
        let Just mems = decode (respBody resp) :: Maybe [Memory]
        map (.id) mems `shouldBe` [matchingMem.id]

  describe "memory links" $ do
    it "creates and retrieves memory links" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("link-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      m1Resp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("mem A" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)])
      let Just m1 = decode (respBody m1Resp) :: Maybe Memory
      m2Resp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("mem B" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)])
      let Just m2 = decode (respBody m2Resp) :: Maybe Memory

      -- Link m1 -> m2
      linkResp <- postJSON app (uuidPath "/api/v1/memories" m1.id <> "/links")
        (object ["target_id" .= m2.id, "relation_type" .= ("related" :: T.Text)])
      respStatus linkResp `shouldBe` 200

      -- List links
      listResp <- get_ app (uuidPath "/api/v1/memories" m1.id <> "/links")
      respStatus listResp `shouldBe` 200

  describe "memory tags" $ do
    it "sets and retrieves tags" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("tag-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      memResp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("tagged" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)
                , "tags" .= (["alpha", "beta"] :: [T.Text])])
      let Just mem = decode (respBody memResp) :: Maybe Memory
      respStatus memResp `shouldBe` 200

      -- Get tags
      tagResp <- get_ app (uuidPath "/api/v1/memories" mem.id <> "/tags")
      respStatus tagResp `shouldBe` 200

  describe "batch create" $ do
    it "creates multiple memories in one request" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("batch-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      let items =
            [ object ["workspace_id" .= ws.id, "content" .= ("batch 1" :: T.Text)
                     , "memory_type" .= ("short_term" :: T.Text)]
            , object ["workspace_id" .= ws.id, "content" .= ("batch 2" :: T.Text)
                     , "memory_type" .= ("long_term" :: T.Text)]
            ]
      batchResp <- postJSON app "/api/v1/memories/batch" (toJSON items)
      respStatus batchResp `shouldBe` 200
      let Just mems = decode (respBody batchResp) :: Maybe [Memory]
      length mems `shouldBe` 2

    it "rejects batch items with empty content" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("batch-invalid-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      let items =
            [ object ["workspace_id" .= ws.id, "content" .= ("   " :: T.Text)
                     , "memory_type" .= ("short_term" :: T.Text)]
            ]
      batchResp <- postJSON app "/api/v1/memories/batch" (toJSON items)
      respStatus batchResp `shouldBe` 400

  describe "task dependencies via API" $ do
    it "adds dependency and rejects cycle" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("dep-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      projResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Dep" :: T.Text)])
      let Just proj = decode (respBody projResp) :: Maybe Project
      t1Resp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id, "title" .= ("A" :: T.Text)])
      let Just t1 = decode (respBody t1Resp) :: Maybe Task
      t2Resp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id, "title" .= ("B" :: T.Text)])
      let Just t2 = decode (respBody t2Resp) :: Maybe Task

      -- Add B depends on A (OK)
      depResp <- postJSON app (uuidPath "/api/v1/tasks" t2.id <> "/dependencies")
        (object ["depends_on_id" .= t1.id])
      respStatus depResp `shouldBe` 200

      -- Add A depends on B (cycle -- should be 409)
      cycleResp <- postJSON app (uuidPath "/api/v1/tasks" t1.id <> "/dependencies")
        (object ["depends_on_id" .= t2.id])
      respStatus cycleResp `shouldBe` 409

      -- Remove dependency
      delResp <- del app (uuidPath "/api/v1/tasks" t2.id <> "/dependencies/"
                          <> encodeUtf8 (T.pack (show t1.id)))
      respStatus delResp `shouldBe` 200

  describe "task overview endpoint" $ do
    it "returns dependency summaries and optional extra-context memories" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("task-overview-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      projResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Overview Project" :: T.Text)])
      let Just proj = decode (respBody projResp) :: Maybe Project

      depResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id, "title" .= ("Dependency" :: T.Text)])
      let Just depTask = decode (respBody depResp) :: Maybe Task

      taskResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id, "title" .= ("Target" :: T.Text)])
      let Just task = decode (respBody taskResp) :: Maybe Task

      addDepResp <- postJSON app (uuidPath "/api/v1/tasks" task.id <> "/dependencies")
        (object ["depends_on_id" .= depTask.id])
      respStatus addDepResp `shouldBe` 200

      taskMemResp <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("Task memory content" :: T.Text)
          , "summary" .= ("Task memory" :: T.Text)
          , "memory_type" .= ("short_term" :: T.Text)
          , "importance" .= (9 :: Int)
          ])
      let Just taskMem = decode (respBody taskMemResp) :: Maybe Memory

      projectMemResp <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("Project memory content" :: T.Text)
          , "summary" .= ("Project memory" :: T.Text)
          , "memory_type" .= ("long_term" :: T.Text)
          , "importance" .= (7 :: Int)
          ])
      let Just projectMem = decode (respBody projectMemResp) :: Maybe Memory

      workspaceMemResp <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("Workspace memory content" :: T.Text)
          , "summary" .= ("Workspace memory" :: T.Text)
          , "memory_type" .= ("short_term" :: T.Text)
          , "importance" .= (5 :: Int)
          ])
      let Just workspaceMem = decode (respBody workspaceMemResp) :: Maybe Memory

      linkTaskMemResp <- postJSON app (uuidPath "/api/v1/tasks" task.id <> "/memories")
        (object ["memory_id" .= taskMem.id])
      respStatus linkTaskMemResp `shouldBe` 200

      linkProjectMemResp <- postJSON app (uuidPath "/api/v1/projects" proj.id <> "/memories")
        (object ["memory_id" .= projectMem.id])
      respStatus linkProjectMemResp `shouldBe` 200

      overviewResp <- get_ app (uuidPath "/api/v1/tasks" task.id <> "/overview")
      respStatus overviewResp `shouldBe` 200
      let Just overview = decode (respBody overviewResp) :: Maybe TaskOverview
      map (.name) overview.dependencies `shouldBe` ["Dependency"]
      map (.scope) overview.connectedMemories `shouldBe` [ScopeTask]
      map (.id) overview.connectedMemories `shouldBe` [taskMem.id]

      extraResp <- get_ app (uuidPath "/api/v1/tasks" task.id <> "/overview?extra_context=true")
      respStatus extraResp `shouldBe` 200
      let Just extraOverview = decode (respBody extraResp) :: Maybe TaskOverview
      map (.scope) extraOverview.connectedMemories `shouldBe` [ScopeTask, ScopeProject, ScopeWorkspace]
      map (.id) extraOverview.connectedMemories `shouldBe` [taskMem.id, projectMem.id, workspaceMem.id]

    it "returns project overview with optional extra-context workspace memories" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("project-overview-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      projResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Project Overview" :: T.Text)])
      let Just proj = decode (respBody projResp) :: Maybe Project

      taskResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id, "title" .= ("Proj Task" :: T.Text)])
      let Just task = decode (respBody taskResp) :: Maybe Task

      projectMemResp <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("Project overview memory" :: T.Text)
          , "summary" .= ("Project overview mem" :: T.Text)
          , "memory_type" .= ("long_term" :: T.Text)
          , "importance" .= (8 :: Int)
          ])
      let Just projectMem = decode (respBody projectMemResp) :: Maybe Memory

      workspaceMemResp <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("Workspace context memory" :: T.Text)
          , "summary" .= ("Workspace context mem" :: T.Text)
          , "memory_type" .= ("short_term" :: T.Text)
          , "importance" .= (5 :: Int)
          ])
      let Just workspaceMem = decode (respBody workspaceMemResp) :: Maybe Memory

      linkProjectMemResp <- postJSON app (uuidPath "/api/v1/projects" proj.id <> "/memories")
        (object ["memory_id" .= projectMem.id])
      respStatus linkProjectMemResp `shouldBe` 200

      overviewResp <- get_ app (uuidPath "/api/v1/projects" proj.id <> "/overview")
      respStatus overviewResp `shouldBe` 200
      let Just overview = decode (respBody overviewResp) :: Maybe ProjectOverview
      map (.id) overview.tasks `shouldBe` [task.id]
      map (.id) overview.linkedMemories `shouldBe` [projectMem.id]
      map (.scope) overview.connectedMemories `shouldBe` [ScopeProject]
      map (.id) overview.connectedMemories `shouldBe` [projectMem.id]

      extraProjectResp <- get_ app (uuidPath "/api/v1/projects" proj.id <> "/overview?extra_context=true")
      respStatus extraProjectResp `shouldBe` 200
      let Just extraProjectOverview = decode (respBody extraProjectResp) :: Maybe ProjectOverview
      map (.scope) extraProjectOverview.connectedMemories `shouldBe` [ScopeProject, ScopeWorkspace]
      map (.id) extraProjectOverview.connectedMemories `shouldBe` [projectMem.id, workspaceMem.id]

  describe "workspace update and delete" $ do
    it "updates and deletes a workspace" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("upd-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      upResp <- putJSON app (uuidPath "/api/v1/workspaces" ws.id)
        (object ["name" .= ("renamed-ws" :: T.Text)])
      respStatus upResp `shouldBe` 200
      let Just updated = decode (respBody upResp) :: Maybe Workspace
      updated.name `shouldBe` "renamed-ws"

      delResp <- del app (uuidPath "/api/v1/workspaces" ws.id)
      respStatus delResp `shouldBe` 200

      getResp <- get_ app (uuidPath "/api/v1/workspaces" ws.id)
      respStatus getResp `shouldBe` 404

    it "soft-deletes workspace children and allows recreation with the same gh info" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object
          [ "name" .= ("cascade-ws" :: T.Text)
          , "gh_owner" .= ("cascade-owner" :: T.Text)
          , "gh_repo" .= ("cascade-repo" :: T.Text)
          ])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      memResp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("hidden memory" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)])
      let Just mem = decode (respBody memResp) :: Maybe Memory

      projResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Hidden Project" :: T.Text)])
      let Just proj = decode (respBody projResp) :: Maybe Project

      taskResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id, "title" .= ("Hidden Task" :: T.Text)])
      let Just task = decode (respBody taskResp) :: Maybe Task

      catResp <- postJSON app "/api/v1/categories"
        (object ["workspace_id" .= ws.id, "name" .= ("hidden-category" :: T.Text)])
      let Just cat = decode (respBody catResp) :: Maybe MemoryCategory

      viewResp <- postJSON app "/api/v1/saved-views"
        (object
          [ "workspace_id" .= ws.id
          , "name" .= ("hidden-view" :: T.Text)
          , "entity_type" .= ("memory_list" :: T.Text)
          , "query_params" .= object ["workspace_id" .= ws.id]
          ])
      let Just view = decode (respBody viewResp) :: Maybe SavedView

      delResp <- del app (uuidPath "/api/v1/workspaces" ws.id)
      respStatus delResp `shouldBe` 200

      getMemResp <- get_ app (uuidPath "/api/v1/memories" mem.id)
      respStatus getMemResp `shouldBe` 404
      getProjResp <- get_ app (uuidPath "/api/v1/projects" proj.id)
      respStatus getProjResp `shouldBe` 404
      getTaskResp <- get_ app (uuidPath "/api/v1/tasks" task.id)
      respStatus getTaskResp `shouldBe` 404
      getCatResp <- get_ app (uuidPath "/api/v1/categories" cat.id)
      respStatus getCatResp `shouldBe` 404
      getViewResp <- get_ app (uuidPath "/api/v1/saved-views" view.id)
      respStatus getViewResp `shouldBe` 404

      recreateResp <- postJSON app "/api/v1/workspaces"
        (object
          [ "name" .= ("cascade-ws-recreated" :: T.Text)
          , "gh_owner" .= ("cascade-owner" :: T.Text)
          , "gh_repo" .= ("cascade-repo" :: T.Text)
          ])
      respStatus recreateResp `shouldBe` 200

    it "requires soft-delete before purge" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("purge-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      viewResp <- postJSON app "/api/v1/saved-views"
        (object
          [ "workspace_id" .= ws.id
          , "name" .= ("purge-view" :: T.Text)
          , "entity_type" .= ("memory_list" :: T.Text)
          , "query_params" .= object ["workspace_id" .= ws.id]
          ])
      respStatus viewResp `shouldBe` 200

      purgeActiveResp <- del app (uuidPath "/api/v1/workspaces" ws.id <> "/purge")
      respStatus purgeActiveResp `shouldBe` 409

      delResp <- del app (uuidPath "/api/v1/workspaces" ws.id)
      respStatus delResp `shouldBe` 200

      purgeDeletedResp <- del app (uuidPath "/api/v1/workspaces" ws.id <> "/purge")
      respStatus purgeDeletedResp `shouldBe` 200

  describe "restore endpoints" $ do
    it "restores a soft-deleted memory" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("restore-memory-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      memResp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("restore me" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)])
      let Just mem = decode (respBody memResp) :: Maybe Memory

      delResp <- del app (uuidPath "/api/v1/memories" mem.id)
      respStatus delResp `shouldBe` 200
      getDeletedResp <- get_ app (uuidPath "/api/v1/memories" mem.id)
      respStatus getDeletedResp `shouldBe` 404

      restoreResp <- postJSON app (uuidPath "/api/v1/memories" mem.id <> "/restore") (object [])
      respStatus restoreResp `shouldBe` 200
      getRestoredResp <- get_ app (uuidPath "/api/v1/memories" mem.id)
      respStatus getRestoredResp `shouldBe` 200

    it "restores a soft-deleted project subtree" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("restore-project-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      parentResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Parent Project" :: T.Text)])
      let Just parent = decode (respBody parentResp) :: Maybe Project

      childResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Child Project" :: T.Text)
                , "parent_id" .= parent.id])
      let Just child = decode (respBody childResp) :: Maybe Project

      delResp <- del app (uuidPath "/api/v1/projects" parent.id)
      respStatus delResp `shouldBe` 200
      getParentResp <- get_ app (uuidPath "/api/v1/projects" parent.id)
      respStatus getParentResp `shouldBe` 404
      getChildResp <- get_ app (uuidPath "/api/v1/projects" child.id)
      respStatus getChildResp `shouldBe` 404

      restoreResp <- postJSON app (uuidPath "/api/v1/projects" parent.id <> "/restore") (object [])
      respStatus restoreResp `shouldBe` 200

      restoredParentResp <- get_ app (uuidPath "/api/v1/projects" parent.id)
      respStatus restoredParentResp `shouldBe` 200
      restoredChildResp <- get_ app (uuidPath "/api/v1/projects" child.id)
      respStatus restoredChildResp `shouldBe` 200
      let Just restoredChild = decode (respBody restoredChildResp) :: Maybe Project
      restoredChild.parentId `shouldBe` Just parent.id

    it "restores a soft-deleted task subtree" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("restore-task-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      projResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Task Restore Project" :: T.Text)])
      let Just proj = decode (respBody projResp) :: Maybe Project

      parentResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id
                , "title" .= ("Parent Task" :: T.Text)])
      let Just parent = decode (respBody parentResp) :: Maybe Task

      childResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id
                , "parent_id" .= parent.id
                , "title" .= ("Child Task" :: T.Text)])
      let Just child = decode (respBody childResp) :: Maybe Task

      delResp <- del app (uuidPath "/api/v1/tasks" parent.id)
      respStatus delResp `shouldBe` 200
      getParentResp <- get_ app (uuidPath "/api/v1/tasks" parent.id)
      respStatus getParentResp `shouldBe` 404
      getChildResp <- get_ app (uuidPath "/api/v1/tasks" child.id)
      respStatus getChildResp `shouldBe` 404

      restoreResp <- postJSON app (uuidPath "/api/v1/tasks" parent.id <> "/restore") (object [])
      respStatus restoreResp `shouldBe` 200

      restoredParentResp <- get_ app (uuidPath "/api/v1/tasks" parent.id)
      respStatus restoredParentResp `shouldBe` 200
      restoredChildResp <- get_ app (uuidPath "/api/v1/tasks" child.id)
      respStatus restoredChildResp `shouldBe` 200
      let Just restoredChild = decode (respBody restoredChildResp) :: Maybe Task
      restoredChild.parentId `shouldBe` Just parent.id

    it "restores a soft-deleted workspace and its children" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object
          [ "name" .= ("restore-workspace-ws" :: T.Text)
          ])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      memResp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("workspace child memory" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)])
      let Just mem = decode (respBody memResp) :: Maybe Memory

      projResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Workspace Child Project" :: T.Text)])
      let Just proj = decode (respBody projResp) :: Maybe Project

      taskResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id
                , "title" .= ("Workspace Child Task" :: T.Text)])
      let Just task = decode (respBody taskResp) :: Maybe Task

      catResp <- postJSON app "/api/v1/categories"
        (object ["workspace_id" .= ws.id, "name" .= ("workspace-child-category" :: T.Text)])
      let Just cat = decode (respBody catResp) :: Maybe MemoryCategory

      viewResp <- postJSON app "/api/v1/saved-views"
        (object
          [ "workspace_id" .= ws.id
          , "name" .= ("workspace-child-view" :: T.Text)
          , "entity_type" .= ("memory_list" :: T.Text)
          , "query_params" .= object ["workspace_id" .= ws.id]
          ])
      let Just view = decode (respBody viewResp) :: Maybe SavedView

      delResp <- del app (uuidPath "/api/v1/workspaces" ws.id)
      respStatus delResp `shouldBe` 200

      getDeletedViewResp <- get_ app (uuidPath "/api/v1/saved-views" view.id)
      respStatus getDeletedViewResp `shouldBe` 404

      restoreResp <- postJSON app (uuidPath "/api/v1/workspaces" ws.id <> "/restore") (object [])
      respStatus restoreResp `shouldBe` 200

      getWsResp <- get_ app (uuidPath "/api/v1/workspaces" ws.id)
      respStatus getWsResp `shouldBe` 200
      getMemResp <- get_ app (uuidPath "/api/v1/memories" mem.id)
      respStatus getMemResp `shouldBe` 200
      getProjResp <- get_ app (uuidPath "/api/v1/projects" proj.id)
      respStatus getProjResp `shouldBe` 200
      getTaskResp <- get_ app (uuidPath "/api/v1/tasks" task.id)
      respStatus getTaskResp `shouldBe` 200
      getCatResp <- get_ app (uuidPath "/api/v1/categories" cat.id)
      respStatus getCatResp `shouldBe` 200
      getViewResp <- get_ app (uuidPath "/api/v1/saved-views" view.id)
      respStatus getViewResp `shouldBe` 200

    it "restores a soft-deleted category" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("restore-category-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      catResp <- postJSON app "/api/v1/categories"
        (object ["workspace_id" .= ws.id, "name" .= ("restore-category" :: T.Text)])
      let Just cat = decode (respBody catResp) :: Maybe MemoryCategory

      delResp <- del app (uuidPath "/api/v1/categories" cat.id)
      respStatus delResp `shouldBe` 200
      getDeletedResp <- get_ app (uuidPath "/api/v1/categories" cat.id)
      respStatus getDeletedResp `shouldBe` 404

      restoreResp <- postJSON app (uuidPath "/api/v1/categories" cat.id <> "/restore") (object [])
      respStatus restoreResp `shouldBe` 200
      getRestoredResp <- get_ app (uuidPath "/api/v1/categories" cat.id)
      respStatus getRestoredResp `shouldBe` 200

    it "restores a soft-deleted saved view" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("restore-view-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      viewResp <- postJSON app "/api/v1/saved-views"
        (object
          [ "workspace_id" .= ws.id
          , "name" .= ("restore-view" :: T.Text)
          , "entity_type" .= ("memory_list" :: T.Text)
          , "query_params" .= object ["workspace_id" .= ws.id]
          ])
      let Just view = decode (respBody viewResp) :: Maybe SavedView

      delResp <- del app (uuidPath "/api/v1/saved-views" view.id)
      respStatus delResp `shouldBe` 200
      getDeletedResp <- get_ app (uuidPath "/api/v1/saved-views" view.id)
      respStatus getDeletedResp `shouldBe` 404

      restoreResp <- postJSON app (uuidPath "/api/v1/saved-views" view.id <> "/restore") (object [])
      respStatus restoreResp `shouldBe` 200
      getRestoredResp <- get_ app (uuidPath "/api/v1/saved-views" view.id)
      respStatus getRestoredResp `shouldBe` 200

  --------------------------------------------------------------------------
  -- Categories
  --------------------------------------------------------------------------

  describe "categories" $ do
    it "creates, lists, updates, and deletes a category" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("cat-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      -- Create category
      catResp <- postJSON app "/api/v1/categories"
        (object
          [ "workspace_id" .= ws.id
          , "name" .= ("test-category" :: T.Text)
          , "description" .= ("A test category" :: T.Text)
          ])
      respStatus catResp `shouldBe` 200
      let Just cat = decode (respBody catResp) :: Maybe MemoryCategory
      cat.name `shouldBe` "test-category"

      -- List categories for workspace
      listResp <- get_ app ("/api/v1/categories?workspace_id="
                            <> encodeUtf8 (T.pack (show ws.id)))
      respStatus listResp `shouldBe` 200
      let Just cats = decode (respBody listResp) :: Maybe (PaginatedResult MemoryCategory)
      length cats.items `shouldSatisfy` (>= 1)

      -- Get category
      getResp <- get_ app (uuidPath "/api/v1/categories" cat.id)
      respStatus getResp `shouldBe` 200

      -- Update category
      upResp <- putJSON app (uuidPath "/api/v1/categories" cat.id)
        (object ["name" .= ("renamed-category" :: T.Text)])
      respStatus upResp `shouldBe` 200
      let Just updated = decode (respBody upResp) :: Maybe MemoryCategory
      updated.name `shouldBe` "renamed-category"

      -- Delete category
      delResp <- del app (uuidPath "/api/v1/categories" cat.id)
      respStatus delResp `shouldBe` 200

      -- Confirm 404
      getResp2 <- get_ app (uuidPath "/api/v1/categories" cat.id)
      respStatus getResp2 `shouldBe` 404

    it "detaches active child categories when deleting a parent" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("cat-tree-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      parentResp <- postJSON app "/api/v1/categories"
        (object ["workspace_id" .= ws.id, "name" .= ("parent" :: T.Text)])
      let Just parent = decode (respBody parentResp) :: Maybe MemoryCategory

      childResp <- postJSON app "/api/v1/categories"
        (object
          [ "workspace_id" .= ws.id
          , "name" .= ("child" :: T.Text)
          , "parent_id" .= parent.id
          ])
      let Just child = decode (respBody childResp) :: Maybe MemoryCategory

      delResp <- del app (uuidPath "/api/v1/categories" parent.id)
      respStatus delResp `shouldBe` 200

      getChildResp <- get_ app (uuidPath "/api/v1/categories" child.id)
      respStatus getChildResp `shouldBe` 200
      let Just updatedChild = decode (respBody getChildResp) :: Maybe MemoryCategory
      updatedChild.parentId `shouldBe` Nothing

    it "lists global categories" $ \app -> do
      -- Create a global category (no workspace_id)
      catResp <- postJSON app "/api/v1/categories"
        (object ["name" .= ("global-cat" :: T.Text)])
      respStatus catResp `shouldBe` 200

      listResp <- get_ app "/api/v1/categories/global"
      respStatus listResp `shouldBe` 200

    it "links and unlinks a memory to a category" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("catlink-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      catResp <- postJSON app "/api/v1/categories"
        (object ["workspace_id" .= ws.id, "name" .= ("linkable" :: T.Text)])
      let Just cat = decode (respBody catResp) :: Maybe MemoryCategory

      memResp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("to categorize" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)])
      let Just mem = decode (respBody memResp) :: Maybe Memory

      -- Link
      linkResp <- postJSON app "/api/v1/categories/link"
        (object ["memory_id" .= mem.id, "category_id" .= cat.id])
      respStatus linkResp `shouldBe` 200

      -- Unlink
      unlinkResp <- postJSON app "/api/v1/categories/unlink"
        (object ["memory_id" .= mem.id, "category_id" .= cat.id])
      respStatus unlinkResp `shouldBe` 200

    it "rejects duplicate category name in same workspace under same parent" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("catdup-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      -- Create a parent category
      parentResp <- postJSON app "/api/v1/categories"
        (object ["workspace_id" .= ws.id, "name" .= ("parent-cat" :: T.Text)])
      let Just parent = decode (respBody parentResp) :: Maybe MemoryCategory

      -- Create first child category
      catResp1 <- postJSON app "/api/v1/categories"
        (object ["workspace_id" .= ws.id, "name" .= ("unique-name" :: T.Text)
                , "parent_id" .= parent.id])
      respStatus catResp1 `shouldBe` 200

      -- Attempt duplicate name under same parent in same workspace
      catResp2 <- postJSON app "/api/v1/categories"
        (object ["workspace_id" .= ws.id, "name" .= ("unique-name" :: T.Text)
                , "parent_id" .= parent.id])
      respStatus catResp2 `shouldBe` 409

  describe "project and category batch endpoints" $ do
    it "batch-updates and batch-deletes projects" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("project-batch-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      p1Resp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Batch Project One" :: T.Text)])
      let Just p1 = decode (respBody p1Resp) :: Maybe Project

      p2Resp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Batch Project Two" :: T.Text)])
      let Just p2 = decode (respBody p2Resp) :: Maybe Project

      updateResp <- postJSON app "/api/v1/projects/batch-update"
        (object
          [ "items" .=
              [ object ["id" .= p1.id, "name" .= ("Renamed One" :: T.Text), "status" .= ("completed" :: T.Text)]
              , object ["id" .= p2.id, "priority" .= (9 :: Int)]
              ]
          ])
      respStatus updateResp `shouldBe` 200
      let Just updateResult = decode (respBody updateResp) :: Maybe BatchResult
      updateResult.affected `shouldBe` 2

      getP1Resp <- get_ app (uuidPath "/api/v1/projects" p1.id)
      respStatus getP1Resp `shouldBe` 200
      let Just updatedP1 = decode (respBody getP1Resp) :: Maybe Project
      updatedP1.name `shouldBe` "Renamed One"
      updatedP1.status `shouldBe` ProjCompleted

      getP2Resp <- get_ app (uuidPath "/api/v1/projects" p2.id)
      respStatus getP2Resp `shouldBe` 200
      let Just updatedP2 = decode (respBody getP2Resp) :: Maybe Project
      updatedP2.priority `shouldBe` 9

      deleteResp <- postJSON app "/api/v1/projects/batch-delete"
        (object ["ids" .= [p1.id, p2.id]])
      respStatus deleteResp `shouldBe` 200
      let Just deleteResult = decode (respBody deleteResp) :: Maybe BatchResult
      deleteResult.affected `shouldBe` 2

      getDeletedP1Resp <- get_ app (uuidPath "/api/v1/projects" p1.id)
      respStatus getDeletedP1Resp `shouldBe` 404
      getDeletedP2Resp <- get_ app (uuidPath "/api/v1/projects" p2.id)
      respStatus getDeletedP2Resp `shouldBe` 404

    it "batch-links category memories, lists them, and batch-deletes categories" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("category-batch-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      cat1Resp <- postJSON app "/api/v1/categories"
        (object ["workspace_id" .= ws.id, "name" .= ("batch-category-one" :: T.Text)])
      let Just cat1 = decode (respBody cat1Resp) :: Maybe MemoryCategory

      cat2Resp <- postJSON app "/api/v1/categories"
        (object ["workspace_id" .= ws.id, "name" .= ("batch-category-two" :: T.Text)])
      let Just cat2 = decode (respBody cat2Resp) :: Maybe MemoryCategory

      mem1Resp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("batch category memory one" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)])
      let Just mem1 = decode (respBody mem1Resp) :: Maybe Memory

      mem2Resp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("batch category memory two" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)])
      let Just mem2 = decode (respBody mem2Resp) :: Maybe Memory

      linkResp <- postJSON app (uuidPath "/api/v1/categories" cat1.id <> "/memories/batch")
        (object ["memory_ids" .= [mem1.id, mem2.id]])
      respStatus linkResp `shouldBe` 200
      let Just linkResult = decode (respBody linkResp) :: Maybe BatchResult
      linkResult.affected `shouldBe` 2

      listResp <- get_ app (uuidPath "/api/v1/categories" cat1.id <> "/memories")
      respStatus listResp `shouldBe` 200
      let Just memories = decode (respBody listResp) :: Maybe [Memory]
      map (\memory -> memory.id) memories `shouldMatchList` [mem1.id, mem2.id]

      deleteResp <- postJSON app "/api/v1/categories/batch-delete"
        (object ["ids" .= [cat1.id, cat2.id]])
      respStatus deleteResp `shouldBe` 200
      let Just deleteResult = decode (respBody deleteResp) :: Maybe BatchResult
      deleteResult.affected `shouldBe` 2

      getDeletedCat1Resp <- get_ app (uuidPath "/api/v1/categories" cat1.id)
      respStatus getDeletedCat1Resp `shouldBe` 404
      getDeletedCat2Resp <- get_ app (uuidPath "/api/v1/categories" cat2.id)
      respStatus getDeletedCat2Resp `shouldBe` 404

  --------------------------------------------------------------------------
  -- Workspace groups
  --------------------------------------------------------------------------

  describe "workspace groups" $ do
    it "creates, gets, lists, and deletes a group" $ \app -> do
      grpResp <- postJSON app "/api/v1/groups"
        (object
          [ "name" .= ("test-group" :: T.Text)
          , "description" .= ("A test group" :: T.Text)
          ])
      respStatus grpResp `shouldBe` 200
      let Just grp = decode (respBody grpResp) :: Maybe WorkspaceGroup
      grp.name `shouldBe` "test-group"

      -- Get group
      getResp <- get_ app (uuidPath "/api/v1/groups" grp.id)
      respStatus getResp `shouldBe` 200

      -- List groups
      listResp <- get_ app "/api/v1/groups"
      respStatus listResp `shouldBe` 200
      let Just groups = decode (respBody listResp) :: Maybe (PaginatedResult WorkspaceGroup)
      length groups.items `shouldSatisfy` (>= 1)

      -- Delete group
      delResp <- del app (uuidPath "/api/v1/groups" grp.id)
      respStatus delResp `shouldBe` 200

      -- Confirm 404
      getResp2 <- get_ app (uuidPath "/api/v1/groups" grp.id)
      respStatus getResp2 `shouldBe` 404

    it "adds and removes group members" $ \app -> do
      grpResp <- postJSON app "/api/v1/groups"
        (object ["name" .= ("member-group" :: T.Text)])
      let Just grp = decode (respBody grpResp) :: Maybe WorkspaceGroup

      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("grp-member-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      -- Add member
      addResp <- postJSON app (uuidPath "/api/v1/groups" grp.id <> "/members")
        (object ["workspace_id" .= ws.id])
      respStatus addResp `shouldBe` 200

      -- List members
      memResp <- get_ app (uuidPath "/api/v1/groups" grp.id <> "/members")
      respStatus memResp `shouldBe` 200
      let Just members = decode (respBody memResp) :: Maybe [UUID]
      members `shouldBe` [ws.id]

      -- Remove member
      delResp <- del app (uuidPath "/api/v1/groups" grp.id <> "/members/"
                          <> encodeUtf8 (T.pack (show ws.id)))
      respStatus delResp `shouldBe` 200

  --------------------------------------------------------------------------
  -- Cleanup
  --------------------------------------------------------------------------

  describe "cleanup" $ do
    it "upserts a policy and lists policies" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("cleanup-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      -- Upsert a cleanup policy
      polResp <- postJSON app "/api/v1/cleanup/policies"
        (object
          [ "workspace_id" .= ws.id
          , "memory_type" .= ("short_term" :: T.Text)
          , "max_age_hours" .= (72 :: Int)
          , "min_importance" .= (3 :: Int)
          , "enabled" .= True
          ])
      respStatus polResp `shouldBe` 200
      let Just pol = decode (respBody polResp) :: Maybe CleanupPolicy
      pol.enabled `shouldBe` True

      -- List policies
      listResp <- get_ app ("/api/v1/cleanup/policies?workspace_id="
                            <> encodeUtf8 (T.pack (show ws.id)))
      respStatus listResp `shouldBe` 200
      let Just pols = decode (respBody listResp) :: Maybe (PaginatedResult CleanupPolicy)
      length pols.items `shouldSatisfy` (>= 1)

    it "runs cleanup on a workspace" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("cleanup-run-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      runResp <- postJSON app "/api/v1/cleanup/run"
        (object ["workspace_id" .= ws.id])
      respStatus runResp `shouldBe` 200
      let Just result = decode (respBody runResp) :: Maybe CleanupResult
      result.deletedCount `shouldBe` 0

  --------------------------------------------------------------------------
  -- Activity timeline
  --------------------------------------------------------------------------

  describe "activity timeline" $ do
    it "returns activity events after creating entities" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("activity-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      -- Create a memory to generate activity
      _ <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("activity test" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)])

      actResp <- get_ app ("/api/v1/activity?workspace_id="
                           <> encodeUtf8 (T.pack (show ws.id)))
      respStatus actResp `shouldBe` 200
      let Just events = decode (respBody actResp) :: Maybe (PaginatedResult ActivityEvent)
      length events.items `shouldSatisfy` (>= 1)

    it "returns all activity when workspace_id is omitted" $ \app -> do
      actResp <- get_ app "/api/v1/activity"
      respStatus actResp `shouldBe` 200

  --------------------------------------------------------------------------
  -- Error paths
  --------------------------------------------------------------------------

  describe "error paths" $ do
    it "returns 400 for invalid UUID in path" $ \app -> do
      resp <- get_ app "/api/v1/memories/not-a-uuid"
      respStatus resp `shouldBe` 400

    it "returns 400 for malformed JSON body" $ \app -> do
      resp <- runReq app methodPost "/api/v1/workspaces" "{invalid json"
      respStatus resp `shouldBe` 400

    it "returns 404 for nonexistent group" $ \app -> do
      resp <- get_ app "/api/v1/groups/00000000-0000-0000-0000-000000000099"
      respStatus resp `shouldBe` 404

    it "returns 404 for nonexistent category" $ \app -> do
      resp <- get_ app "/api/v1/categories/00000000-0000-0000-0000-000000000099"
      respStatus resp `shouldBe` 404

  describe "audit log" $ do
    it "persists the current request id for mutations" $ \_ -> do
      withAppEnv $ \env app -> do
        let requestIdHeader = "audit-req-1"
        resp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("X-Request-Id", requestIdHeader)]
          (encode (object ["name" .= ("audit-http-ws" :: T.Text)]))
        respStatus resp `shouldBe` 200
        lookup "X-Request-Id" resp.simpleHeaders `shouldBe` Just requestIdHeader

        let Just ws = decode (respBody resp) :: Maybe Workspace
        rows <- getAuditLogRows env.pool "workspace" (T.pack (show ws.id))
        length rows `shouldBe` 1
        let [created] = rows
        created.action `shouldBe` "create"
        created.requestId `shouldBe` Just "audit-req-1"

    it "exposes canonical actor attribution through audit API responses" $ \_ -> do
      withAppEnv $ \_env app -> do
        resp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("X-Request-Id", "audit-actor-req")]
          (encode (object ["name" .= ("audit-actor-api-ws" :: T.Text)]))
        respStatus resp `shouldBe` 200
        let Just ws = decode (respBody resp) :: Maybe Workspace

        listResp <- get_ app
          ( "/api/v1/audit?entity_type=workspace&entity_id="
         <> encodeUtf8 (T.pack (show ws.id))
          )
        respStatus listResp `shouldBe` 200
        let Just auditPage = decode (respBody listResp) :: Maybe (PaginatedResult AuditLogEntry)
        length auditPage.items `shouldBe` 1
        let [entry] = auditPage.items
        entry.requestId `shouldBe` Just "audit-actor-req"
        entry.actorType `shouldBe` Just "user"
        entry.actorId `shouldBe` Just "local-user"
        entry.actorLabel `shouldBe` Just "Local User"

        getResp <- get_ app (uuidPath "/api/v1/audit" entry.id)
        respStatus getResp `shouldBe` 200
        let Just fetchedEntry = decode (respBody getResp) :: Maybe AuditLogEntry
        fetchedEntry.actorType `shouldBe` Just "user"
        fetchedEntry.actorId `shouldBe` Just "local-user"
        fetchedEntry.actorLabel `shouldBe` Just "Local User"

    it "includes canonical actor hints in emitted change events" $ \_ -> do
      withTestEnv $ \env -> do
        let principal = Principal
              { actorType = ActorBot
              , actorId = "event-bot-1"
              , actorLabel = "Event Bot"
              , authority = PrincipalSyntheticLocalSuperadmin
              }
        withCapturedBroadcastApp env principal $ \app eventsRef -> do
          resp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
            [("X-Request-Id", "event-actor-req")]
            (encode (object ["name" .= ("event-actor-ws" :: T.Text)]))
          respStatus resp `shouldBe` 200

          events <- readIORef eventsRef
          length events `shouldBe` 1
          let [event@ChangeEvent { requestId = eventRequestId, actorType = eventActorType, actorId = eventActorId, actorLabel = eventActorLabel }] = events
          eventRequestId `shouldBe` Just "event-actor-req"
          eventActorType `shouldBe` Just "bot"
          eventActorId `shouldBe` Just "event-bot-1"
          eventActorLabel `shouldBe` Just "Event Bot"
          let eventJson = LBS8.unpack (encode event)
          eventJson `shouldContain` "\"actor_type\":\"bot\""
          eventJson `shouldContain` "\"actor_id\":\"event-bot-1\""
          eventJson `shouldContain` "\"actor_label\":\"Event Bot\""
