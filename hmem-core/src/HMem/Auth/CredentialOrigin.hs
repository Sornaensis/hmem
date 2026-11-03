-- | Authentication provenance for delegation; it never changes Principal authority.
module HMem.Auth.CredentialOrigin where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import HMem.DB.RequestContext

data CredentialOrigin = AccessTokenOrigin UUID | BrowserSessionOrigin UUID | LocalOrigin | JwtOrigin
  deriving stock (Show,Eq)

instance ToJSON CredentialOrigin where
  toJSON = \case
    AccessTokenOrigin identifier -> object ["kind" .= ("pat"::Text),"id" .= identifier]
    BrowserSessionOrigin identifier -> object ["kind" .= ("browser"::Text),"id" .= identifier]
    LocalOrigin -> object ["kind" .= ("local"::Text)]
    JwtOrigin -> object ["kind" .= ("jwt"::Text)]
instance FromJSON CredentialOrigin where
  parseJSON = withObject "CredentialOrigin" $ \o -> o .: "kind" >>= \case
    ("pat"::Text) -> AccessTokenOrigin <$> o .: "id"
    "browser" -> BrowserSessionOrigin <$> o .: "id"
    "local" -> pure LocalOrigin
    "jwt" -> pure JwtOrigin
    _ -> fail "Unsupported credential origin"

data ResolvedRequestAuth = ResolvedRequestAuth
  { principal :: !Principal, credentialOrigin :: !CredentialOrigin
  , credentialExpiresAt :: !(Maybe UTCTime)
  } deriving stock (Show,Eq)

-- The token actor and its grant-bearing user remain separate, including bot PATs.
data GrantIssuer = GrantIssuer { principal :: !Principal, credentialOrigin :: !CredentialOrigin }
  deriving stock (Show,Eq)
instance ToJSON GrantIssuer where
  toJSON issuer = object
    ["actor_type" .= actorTypeToText issuer.principal.actorType,"actor_id" .= issuer.principal.actorId
    ,"actor_label" .= issuer.principal.actorLabel,"credential_origin" .= issuer.credentialOrigin
    ,"authority" .= case issuer.principal.authority of
       PrincipalGrantUser identifier -> object ["kind" .= ("user"::Text),"id" .= identifier]
       PrincipalSyntheticLocalSuperadmin -> object ["kind" .= ("local"::Text)]
       PrincipalNoAuthority -> object ["kind" .= ("none"::Text)]]
instance FromJSON GrantIssuer where
  parseJSON = withObject "GrantIssuer" $ \o -> do
    actor <- o .: "actor_type" >>= \case ("user"::Text) -> pure ActorUser; "bot" -> pure ActorBot; _ -> fail "Invalid issuer actor"
    authorityValue <- o .: "authority"
    authority <- withObject "issuer authority" (\a -> a .: "kind" >>= \case
      ("user"::Text) -> PrincipalGrantUser <$> a .: "id"
      "local" -> pure PrincipalSyntheticLocalSuperadmin
      "none" -> pure PrincipalNoAuthority
      _ -> fail "Invalid issuer authority") authorityValue
    GrantIssuer <$> (Principal actor <$> o .: "actor_id" <*> o .: "actor_label" <*> pure authority)
                <*> o .: "credential_origin"

sameIssuer :: GrantIssuer -> ResolvedRequestAuth -> Bool
sameIssuer issuer current = issuer.credentialOrigin == current.credentialOrigin
  && issuer.principal.actorType == current.principal.actorType
  && issuer.principal.actorId == current.principal.actorId
  && issuer.principal.authority == current.principal.authority
