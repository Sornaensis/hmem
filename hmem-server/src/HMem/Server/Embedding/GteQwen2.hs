-- | The pinned, pure semantic contract for the one embedding space hmem
-- supports.  Tokenization and pooling stay in the pinned TEI runtime; this
-- module records their required settings and validates its observable output.
module HMem.Server.Embedding.GteQwen2
  ( GteQwen2Requirements(..)
  , gteQwen2Requirements
  , gteQwen2QueryPrefix
  , gteQwen2MinimumSafeNorm
  , GteQwen2Error(..)
  , GteQwen2InvocationError(..)
  , PreparedEmbeddingRequest
  , prepareGteQwen2Request
  , finalizeGteQwen2Batch
  , embedGteQwen2
  ) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

import HMem.Config (managedTeiModelId, managedTeiSpaceFingerprint)
import HMem.Server.Embedding.Provider
  ( EmbeddingBatch(..)
  , EmbeddingFailure
  , EmbeddingInput(..)
  , EmbeddingInputKind(..)
  , EmbeddingProvider(..)
  , EmbeddingRequest(..)
  )

-- | Values that a managed TEI launch or an operator endpoint must honour.
-- @maxBatchTokens@ is deliberately a lower bound: TEI must reserve room for
-- the EOS token it appends after admission.
data GteQwen2Requirements = GteQwen2Requirements
  { modelId :: !Text
  , modelRevision :: !Text
  , spaceFingerprint :: !Text
  , dimensions :: !Int
  , tokenizerMaxLength :: !Int
  , eosPadToken :: !Text
  , eosPadTokenId :: !Int
  , addEosToken :: !Bool
  , isCausal :: !Bool
  , usesLastTokenPooling :: !Bool
  , autoTruncate :: !Bool
  , teiDefaultPrompt :: !(Maybe Text)
  , maxBatchTokens :: !Int
  } deriving stock (Show, Eq)

gteQwen2Requirements :: GteQwen2Requirements
gteQwen2Requirements = GteQwen2Requirements
  { modelId = managedTeiModelId
  , modelRevision = "1cad2ab3ff41c2671f34e135d29831368ee26b68"
  , spaceFingerprint = managedTeiSpaceFingerprint
  , dimensions = 1536
  , tokenizerMaxLength = 32768
  , eosPadToken = "<|endoftext|>"
  , eosPadTokenId = 151643
  , addEosToken = True
  , isCausal = False
  , usesLastTokenPooling = True
  , autoTruncate = False
  , teiDefaultPrompt = Nothing
  , maxBatchTokens = 32768
  }

gteQwen2QueryPrefix :: Text
gteQwen2QueryPrefix = "Instruct: Given a web search query, retrieve relevant passages that answer the query\nQuery: "

-- | Norms at or below this value are rejected.  It is fixed so every provider
-- has the same numerical admission rule; normalization itself uses a scaled
-- calculation to avoid overflow for otherwise finite coordinates.
gteQwen2MinimumSafeNorm :: Double
gteQwen2MinimumSafeNorm = 1.0e-12

data GteQwen2Error
  = InputExceedsTokenizerByteLimit !Int
  | ResponseCountMismatch !Int !Int
  | SpaceFingerprintMismatch
  | VectorDimensionMismatch !Int !Int
  | NonFiniteCoordinate !Int
  | UnsafeVectorNorm !Int
  deriving stock (Show, Eq)

-- | The provider and semantic-contract failure domains remain distinct so a
-- caller can make retry decisions without inspecting a raw endpoint or body.
data GteQwen2InvocationError
  = GteQwen2PreparationError !GteQwen2Error
  | GteQwen2ProviderError !EmbeddingFailure
  | GteQwen2FinalizationError !GteQwen2Error
  deriving stock (Show, Eq)

-- | The constructor is hidden so a completed formatting pass has a distinct
-- type from an ordinary provider request.
newtype PreparedEmbeddingRequest = PreparedEmbeddingRequest EmbeddingRequest
  deriving stock (Show, Eq)

-- | Format queries once and admit complete UTF-8 input bytes.  Documents are
-- carried through unchanged; local truncation would diverge from TEI's pinned
-- tokenizer and is therefore never attempted.
prepareGteQwen2Request :: EmbeddingRequest -> Either GteQwen2Error PreparedEmbeddingRequest
prepareGteQwen2Request request = do
  prepared <- traverse prepareInput request.inputs
  pure (PreparedEmbeddingRequest (EmbeddingRequest prepared))
  where
    byteLimit = gteQwen2Requirements.tokenizerMaxLength - 1
    prepareInput input =
      let text = case input.inputKind of
            EmbeddingQuery -> gteQwen2QueryPrefix <> input.inputText
            EmbeddingDocument -> input.inputText
       in if BS.length (Text.encodeUtf8 text) <= byteLimit
            then Right input { inputText = text }
            else Left (InputExceedsTokenizerByteLimit (BS.length (Text.encodeUtf8 text)))

-- | Validate the exact pinned response space and normalize every vector in
-- input order.  This deliberately does not infer a model or coerce a shape.
finalizeGteQwen2Batch
  :: PreparedEmbeddingRequest
  -> EmbeddingBatch
  -> Either GteQwen2Error EmbeddingBatch
finalizeGteQwen2Batch (PreparedEmbeddingRequest request) batch
  | batch.spaceFingerprint /= gteQwen2Requirements.spaceFingerprint = Left SpaceFingerprintMismatch
  | actualCount /= expectedCount = Left (ResponseCountMismatch expectedCount actualCount)
  | otherwise = EmbeddingBatch <$> traverse normalizeOne batch.vectors <*> pure batch.spaceFingerprint
  where
    expectedCount = length request.inputs
    actualCount = length batch.vectors
    normalizeOne vector
      | length vector /= gteQwen2Requirements.dimensions = Left (VectorDimensionMismatch gteQwen2Requirements.dimensions (length vector))
      | otherwise = normalizeFinite vector

-- | The only public provider invocation path consumes the opaque prepared
-- value internally.  A caller can provide raw inputs once, but cannot recover
-- the formatted request and feed it back through preparation.
embedGteQwen2
  :: EmbeddingProvider
  -> EmbeddingRequest
  -> IO (Either GteQwen2InvocationError EmbeddingBatch)
embedGteQwen2 provider request = case prepareGteQwen2Request request of
  Left errorValue -> pure (Left (GteQwen2PreparationError errorValue))
  Right prepared -> provider.embed (preparedRequest prepared) >>= \case
    Left failure -> pure (Left (GteQwen2ProviderError failure))
    Right batch -> pure $ case finalizeGteQwen2Batch prepared batch of
      Left errorValue -> Left (GteQwen2FinalizationError errorValue)
      Right normalized -> Right normalized
  where
    preparedRequest (PreparedEmbeddingRequest value) = value

normalizeFinite :: [Double] -> Either GteQwen2Error [Double]
normalizeFinite vector =
  case firstInvalid vector 0 of
    Just position -> Left (NonFiniteCoordinate position)
    Nothing -> case maximumAbs vector of
      0 -> Left (UnsafeVectorNorm 0)
      scale ->
        let unitNorm = sqrt (sum (map (\value -> let scaled = value / scale in scaled * scaled) vector))
         in if scale <= gteQwen2MinimumSafeNorm / unitNorm
              then Left (UnsafeVectorNorm 0)
              else Right (map (\value -> (value / scale) / unitNorm) vector)
  where
    firstInvalid [] _ = Nothing
    firstInvalid (value : values) position
      | not (isNaN value || isInfinite value) = firstInvalid values (position + 1)
      | otherwise = Just position
    maximumAbs = foldr (max . abs) 0
