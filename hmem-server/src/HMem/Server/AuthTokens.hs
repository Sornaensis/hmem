module HMem.Server.AuthTokens
  ( AccessTokenActor(..)
  , accessTokenActorToText
  , accessTokenActorFromText
  , IssueAccessTokenInput(..)
  , RotateAccessTokenInput(..)
  , IssuedAccessToken(..)
  , AccessTokenError(..)
  , issueAccessToken
  , rotateAccessToken
  , revokeAccessToken
  , generateAccessToken
  , generatedAccessTokenPrefix
  , generatedAccessTokenRandomHexChars
  , generatedAccessTokenMinEntropyBits
  , isGeneratedAccessTokenFormat
  ) where

import Control.Applicative ((<|>))
import Data.Functor.Contravariant (contramap)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

import HMem.DB.Auth qualified as Auth
import HMem.DB.Pool (runSession, runTransaction)

data AccessTokenActor
  = AccessTokenActorUser
  | AccessTokenActorBot
  deriving stock (Show, Eq)

accessTokenActorToText :: AccessTokenActor -> Text
accessTokenActorToText = \case
  AccessTokenActorUser -> "user"
  AccessTokenActorBot  -> "bot"

accessTokenActorFromText :: Text -> Maybe AccessTokenActor
accessTokenActorFromText = \case
  "user" -> Just AccessTokenActorUser
  "bot"  -> Just AccessTokenActorBot
  _      -> Nothing

data IssueAccessTokenInput = IssueAccessTokenInput
  { grantUserId :: !UUID
  , actorType   :: !AccessTokenActor
  , actorLabel  :: !Text
  , expiresAt   :: !(Maybe UTCTime)
  } deriving stock (Show, Eq)

data RotateAccessTokenInput = RotateAccessTokenInput
  { sourceTokenId :: !UUID
  , expiresAt     :: !(Maybe UTCTime)
  , revokeOld     :: !Bool
  } deriving stock (Show, Eq)

data IssuedAccessToken = IssuedAccessToken
  { tokenId  :: !UUID
  , rawToken :: !Text
  } deriving stock (Eq)

data AccessTokenError
  = EmptyActorLabel
  | GrantUserNotFound !UUID
  | SourceTokenNotFound !UUID
  | GeneratedTokenFormatInvalid
  deriving stock (Show, Eq)

data TokenTemplate = TokenTemplate
  { templateGrantUserId :: !UUID
  , templateActorType   :: !AccessTokenActor
  , templateActorLabel  :: !Text
  , templateExpiresAt   :: !(Maybe UTCTime)
  } deriving stock (Show, Eq)

issueAccessToken
  :: Pool Hasql.Connection
  -> IssueAccessTokenInput
  -> IO (Either AccessTokenError IssuedAccessToken)
issueAccessToken pool input = do
  raw <- generateAccessToken
  runTransaction pool $ issueAccessTokenSession input raw

rotateAccessToken
  :: Pool Hasql.Connection
  -> RotateAccessTokenInput
  -> IO (Either AccessTokenError IssuedAccessToken)
rotateAccessToken pool input = do
  raw <- generateAccessToken
  runTransaction pool $ rotateAccessTokenSession input raw

revokeAccessToken :: Pool Hasql.Connection -> UUID -> IO Bool
revokeAccessToken pool tokenId = do
  affected <- runSession pool $ Session.statement tokenId revokeAccessTokenStatement
  pure (affected > 0)

generatedAccessTokenPrefix :: Text
generatedAccessTokenPrefix = "hmem_pat_v1_"

-- | Officially issued access tokens carry three UUIDv4 random identifiers.
-- UUIDv4 reserves version/variant bits, so this provides roughly 366 bits of
-- randomness while keeping a copy/paste-friendly lowercase hexadecimal format.
generatedAccessTokenRandomHexChars :: Int
generatedAccessTokenRandomHexChars = 96

generatedAccessTokenMinEntropyBits :: Int
generatedAccessTokenMinEntropyBits = 256

generateAccessToken :: IO Text
generateAccessToken = do
  parts <- traverse (const UUIDv4.nextRandom) [1 :: Int, 2, 3]
  pure $ generatedAccessTokenPrefix <> T.concat (map uuidTokenPart parts)
  where
    uuidTokenPart = T.filter (/= '-') . UUID.toText

isGeneratedAccessTokenFormat :: Text -> Bool
isGeneratedAccessTokenFormat token =
  T.isPrefixOf generatedAccessTokenPrefix token
    && T.length randomPart == generatedAccessTokenRandomHexChars
    && T.all isLowerHex randomPart
  where
    randomPart = T.drop (T.length generatedAccessTokenPrefix) token

    isLowerHex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

issueAccessTokenSession
  :: IssueAccessTokenInput
  -> Text
  -> Session.Session (Either AccessTokenError IssuedAccessToken)
issueAccessTokenSession input raw
  | T.null (T.strip input.actorLabel) = pure (Left EmptyActorLabel)
  | not (isGeneratedAccessTokenFormat raw) = pure (Left GeneratedTokenFormatInvalid)
  | otherwise = do
      userExists <- Session.statement input.grantUserId userExistsStatement
      if not userExists
        then pure (Left (GrantUserNotFound input.grantUserId))
        else do
          tokenId <- Session.statement
            ( input.grantUserId
            , accessTokenActorToText input.actorType
            , T.strip input.actorLabel
            , Auth.accessTokenHash raw
            , input.expiresAt
            )
            insertAccessTokenStatement
          pure (Right IssuedAccessToken { tokenId = tokenId, rawToken = raw })

rotateAccessTokenSession
  :: RotateAccessTokenInput
  -> Text
  -> Session.Session (Either AccessTokenError IssuedAccessToken)
rotateAccessTokenSession input raw = do
  if not (isGeneratedAccessTokenFormat raw)
    then pure (Left GeneratedTokenFormatInvalid)
    else do
      mTemplate <- Session.statement input.sourceTokenId tokenTemplateStatement
      case mTemplate of
        Nothing -> pure (Left (SourceTokenNotFound input.sourceTokenId))
        Just template -> do
          tokenId <- Session.statement
            ( template.templateGrantUserId
            , accessTokenActorToText template.templateActorType
            , template.templateActorLabel
            , Auth.accessTokenHash raw
            , input.expiresAt <|> template.templateExpiresAt
            )
            insertAccessTokenStatement
          if input.revokeOld
            then do
              _ <- Session.statement input.sourceTokenId revokeAccessTokenStatement
              pure ()
            else pure ()
          pure (Right IssuedAccessToken { tokenId = tokenId, rawToken = raw })

userExistsStatement :: Statement.Statement UUID Bool
userExistsStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT EXISTS (SELECT 1 FROM users WHERE id = $1)"
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))

insertAccessTokenStatement :: Statement.Statement (UUID, Text, Text, Text, Maybe UTCTime) UUID
insertAccessTokenStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO access_tokens (grant_user_id, actor_type, actor_label, token_hash, expires_at) \
          \VALUES ($1, $2::actor_type_enum, $3, $4, $5) \
          \RETURNING id"
    encoder =
      contramap (\(a,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,b,_,_,_) -> b) (Enc.param (Enc.nonNullable Enc.text)) <>
      contramap (\(_,_,c,_,_) -> c) (Enc.param (Enc.nonNullable Enc.text)) <>
      contramap (\(_,_,_,d,_) -> d) (Enc.param (Enc.nonNullable Enc.text)) <>
      contramap (\(_,_,_,_,e) -> e) (Enc.param (Enc.nullable Enc.timestamptz))
    decoder = Dec.singleRow (Dec.column (Dec.nonNullable Dec.uuid))

tokenTemplateStatement :: Statement.Statement UUID (Maybe TokenTemplate)
tokenTemplateStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT grant_user_id, actor_type::text, actor_label, expires_at \
          \FROM access_tokens \
          \WHERE id = $1 AND revoked_at IS NULL"
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowMaybe $ do
      grantUserId <- Dec.column (Dec.nonNullable Dec.uuid)
      actorText <- Dec.column (Dec.nonNullable Dec.text)
      actorType <- case accessTokenActorFromText actorText of
        Just parsed -> pure parsed
        Nothing -> fail $ "Unexpected actor_type_enum value: " <> show actorText
      label <- Dec.column (Dec.nonNullable Dec.text)
      expiry <- Dec.column (Dec.nullable Dec.timestamptz)
      pure TokenTemplate
        { templateGrantUserId = grantUserId
        , templateActorType = actorType
        , templateActorLabel = label
        , templateExpiresAt = expiry
        }

revokeAccessTokenStatement :: Statement.Statement UUID Int
revokeAccessTokenStatement = Statement.Statement sql encoder decoder True
  where
    sql = "UPDATE access_tokens SET revoked_at = now() \
          \WHERE id = $1 AND revoked_at IS NULL"
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = fromIntegral <$> Dec.rowsAffected
