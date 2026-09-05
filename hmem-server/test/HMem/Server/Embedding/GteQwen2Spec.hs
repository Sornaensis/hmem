module HMem.Server.Embedding.GteQwen2Spec (spec) where

import Data.Aeson (FromJSON, eitherDecodeStrict')
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)
import Test.Hspec

import HMem.Config (managedTeiSpaceFingerprint)
import HMem.Server.Embedding.GteQwen2
import HMem.Server.Embedding.Provider

spec :: Spec
spec = describe "GTE-Qwen2 contract" $ do
  it "uses the opaque production invocation flow exactly once across repeated calls" $ do
    seen <- newIORef []
    let provider = recordingProvider seen
        raw = EmbeddingRequest
          [ EmbeddingInput EmbeddingQuery "cafe\769\nneedle"
          , EmbeddingInput EmbeddingDocument "cafe\769\nneedle"
          ]
        expected = EmbeddingRequest
          [ EmbeddingInput EmbeddingQuery (gteQwen2QueryPrefix <> "cafe\769\nneedle")
          , EmbeddingInput EmbeddingDocument "cafe\769\nneedle"
          ]
    embedGteQwen2 provider raw `shouldReturn` Right (batch (replicate 2 unitVector))
    embedGteQwen2 provider raw `shouldReturn` Right (batch (replicate 2 unitVector))
    readIORef seen `shouldReturn` [expected, expected]

  it "preserves empty documents and formats empty queries through the production flow" $ do
    seen <- newIORef []
    let provider = recordingProvider seen
        raw = EmbeddingRequest [EmbeddingInput EmbeddingQuery "", EmbeddingInput EmbeddingDocument ""]
        expected = EmbeddingRequest [EmbeddingInput EmbeddingQuery gteQwen2QueryPrefix, EmbeddingInput EmbeddingDocument ""]
    embedGteQwen2 provider raw `shouldReturn` Right (batch (replicate 2 unitVector))
    readIORef seen `shouldReturn` [expected]

  it "admits the formatted 32767-byte boundary and rejects over-limit UTF-8 input without truncation" $ do
    seen <- newIORef []
    let provider = recordingProvider seen
        prefixBytes = BS.length (Text.encodeUtf8 gteQwen2QueryPrefix)
        queryAtLimit = Text.replicate (32767 - prefixBytes) "a"
        queryPastLimit = queryAtLimit <> "a"
        acceptedMultibyte = Text.replicate 10922 "界"
        rejectedMultibyte = Text.replicate 10923 "界"
    embedGteQwen2 provider (EmbeddingRequest [EmbeddingInput EmbeddingQuery queryAtLimit]) `shouldReturn` Right (batch [unitVector])
    embedGteQwen2 provider (EmbeddingRequest [EmbeddingInput EmbeddingDocument acceptedMultibyte]) `shouldReturn` Right (batch [unitVector])
    embedGteQwen2 provider (EmbeddingRequest [EmbeddingInput EmbeddingQuery queryPastLimit])
      `shouldReturn` Left (GteQwen2PreparationError (InputExceedsTokenizerByteLimit 32768))
    Text.length rejectedMultibyte `shouldSatisfy` (< 32768)
    BS.length (Text.encodeUtf8 rejectedMultibyte) `shouldBe` 32769
    embedGteQwen2 provider (EmbeddingRequest [EmbeddingInput EmbeddingDocument rejectedMultibyte])
      `shouldReturn` Left (GteQwen2PreparationError (InputExceedsTokenizerByteLimit 32769))
    readIORef seen `shouldReturn`
      [ EmbeddingRequest [EmbeddingInput EmbeddingQuery (gteQwen2QueryPrefix <> queryAtLimit)]
      , EmbeddingRequest [EmbeddingInput EmbeddingDocument acceptedMultibyte]
      ]

  it "decodes the attributed fixtures and matches every contract field" $ do
    fixture <- loadFixture
    let requirements = gteQwen2Requirements
        modelRevision = "1cad2ab3ff41c2671f34e135d29831368ee26b68"
        teiRevision = "06670157fb6c1523482219bdb2d1660277d38088"
    requirements.modelId `shouldBe` "Alibaba-NLP/gte-Qwen2-1.5B-instruct"
    requirements.modelRevision `shouldBe` fixture.model.source.sourceRevision
    requirements.spaceFingerprint `shouldBe` managedTeiSpaceFingerprint
    requirements.dimensions `shouldBe` fixture.model.hiddenSize
    requirements.tokenizerMaxLength `shouldBe` fixture.tokenizer.modelMaxLength
    requirements.eosPadToken `shouldBe` fixture.tokenizer.eosPadToken
    requirements.eosPadTokenId `shouldBe` fixture.tokenizer.eosPadTokenId
    requirements.addEosToken `shouldBe` fixture.tokenizer.addEosToken
    requirements.usesLastTokenPooling `shouldBe` fixture.pooling.lastTokenPooling
    requirements.autoTruncate `shouldBe` fixture.tei.hmemContractAutoTruncate
    requirements.teiDefaultPrompt `shouldBe` Nothing
    requirements.maxBatchTokens `shouldSatisfy` (>= requirements.tokenizerMaxLength)
    gteQwen2QueryPrefix `shouldBe` fixture.sentenceTransformers.queryPrefix
    fixture.readme.normalizesEmbeddings `shouldBe` True
    fixture.tei.upstreamRouterAutoTruncateDefault `shouldBe` True
    fixture.tei.hmemContractAutoTruncate `shouldBe` False
    fixture.tei.sourceArchive.sourceUrl `shouldBe` "https://api.github.com/repos/huggingface/text-embeddings-inference/tarball/06670157fb6c1523482219bdb2d1660277d38088"
    fixture.tei.sourceArchive.sourceTreeUrl `shouldBe` "https://github.com/huggingface/text-embeddings-inference/tree/06670157fb6c1523482219bdb2d1660277d38088"
    mapM_ (assertPinnedSource modelRevision)
      [ fixture.model.source, fixture.tokenizer.source, fixture.sentenceTransformers.source, fixture.pooling.source, fixture.readme.source ]
    assertPinnedSource teiRevision fixture.tei.sourceArchive
    assertTeiSource teiRevision fixture.tei.qwen2Postprocessor
    assertTeiSource teiRevision fixture.tei.lastTokenPoolingSource
    assertTeiSource teiRevision fixture.tei.routerAutoTruncateSource
    fixture.tei.qwen2Postprocessor.sourceLines `shouldContain`
      ["149-153: template appends <|endoftext|> with special-token id 151643"]
    fixture.tei.lastTokenPoolingSource.sourceLines `shouldContain`
      ["433-450: pooling_mode_lasttoken maps to Pool::LastToken"]
    fixture.tei.routerAutoTruncateSource.sourceLines `shouldContain`
      ["200: omitted --auto-truncate defaults to true"]

  it "normalizes valid 3-4 vectors deterministically and independently" $ do
    prepared <- expectPrepared (EmbeddingRequest [EmbeddingInput EmbeddingDocument "one", EmbeddingInput EmbeddingQuery "two"])
    finalizeGteQwen2Batch prepared (batch [[3, 4] <> replicate 1534 0, 4 : 3 : replicate 1534 0])
      `shouldBe` Right (batch [[0.6, 0.8] <> replicate 1534 0, 0.8 : 0.6 : replicate 1534 0])

  it "keeps each normalized vector tied to its batch position" $ do
    first <- expectPrepared (EmbeddingRequest [EmbeddingInput EmbeddingDocument "first", EmbeddingInput EmbeddingDocument "second"])
    second <- expectPrepared (EmbeddingRequest [EmbeddingInput EmbeddingDocument "second", EmbeddingInput EmbeddingDocument "first"])
    let a = [3, 4] <> replicate 1534 0
        b = 4 : 3 : replicate 1534 0
    finalizeGteQwen2Batch first (batch [a, b]) `shouldBe` Right (batch [[0.6, 0.8] <> replicate 1534 0, [0.8, 0.6] <> replicate 1534 0])
    finalizeGteQwen2Batch second (batch [b, a]) `shouldBe` Right (batch [[0.8, 0.6] <> replicate 1534 0, [0.6, 0.8] <> replicate 1534 0])

  it "rejects count, fingerprint, shape, non-finite, zero, and unsafe vectors" $ do
    prepared <- expectPrepared (EmbeddingRequest [EmbeddingInput EmbeddingDocument "one"])
    finalizeGteQwen2Batch prepared (batch []) `shouldBe` Left (ResponseCountMismatch 1 0)
    finalizeGteQwen2Batch prepared (EmbeddingBatch [replicate 1536 0.1] "other") `shouldBe` Left SpaceFingerprintMismatch
    finalizeGteQwen2Batch prepared (batch [replicate 1535 0]) `shouldBe` Left (VectorDimensionMismatch 1536 1535)
    finalizeGteQwen2Batch prepared (batch [replicate 1537 0]) `shouldBe` Left (VectorDimensionMismatch 1536 1537)
    finalizeGteQwen2Batch prepared (batch [0 / 0 : replicate 1535 0]) `shouldBe` Left (NonFiniteCoordinate 0)
    finalizeGteQwen2Batch prepared (batch [1 / 0 : replicate 1535 0]) `shouldBe` Left (NonFiniteCoordinate 0)
    finalizeGteQwen2Batch prepared (batch [replicate 1536 0]) `shouldBe` Left (UnsafeVectorNorm 0)
    finalizeGteQwen2Batch prepared (batch [1.0e-13 : replicate 1535 0]) `shouldBe` Left (UnsafeVectorNorm 0)

recordingProvider :: IORef [EmbeddingRequest] -> EmbeddingProvider
recordingProvider seen = EmbeddingProvider
  { availability = pure EmbeddingAvailable
  , embed = \request -> do
      modifyIORef' seen (<> [request])
      pure (Right (batch (replicate (length request.inputs) unitVector)))
  }

unitVector :: [Double]
unitVector = 1 : replicate 1535 0

batch :: [[Double]] -> EmbeddingBatch
batch values = EmbeddingBatch values managedTeiSpaceFingerprint

expectPrepared :: EmbeddingRequest -> IO PreparedEmbeddingRequest
expectPrepared request = case prepareGteQwen2Request request of
  Left errorValue -> expectationFailure (show errorValue) >> fail "unreachable"
  Right prepared -> pure prepared

data PinnedSource = PinnedSource
  { sourceUrl :: !Text.Text, sourceTreeUrl :: !Text.Text, sourceRevision :: !Text.Text
  , sourceSha256 :: !Text.Text, sourceFile :: !Text.Text, sourceLines :: ![Text.Text]
  } deriving stock (Show, Eq, Generic)
instance FromJSON PinnedSource

data ModelFixture = ModelFixture { source :: !PinnedSource, hiddenSize :: !Int } deriving stock (Show, Eq, Generic)
instance FromJSON ModelFixture

data TokenizerFixture = TokenizerFixture
  { source :: !PinnedSource, modelMaxLength :: !Int, addEosToken :: !Bool, eosPadToken :: !Text.Text, eosPadTokenId :: !Int
  } deriving stock (Show, Eq, Generic)
instance FromJSON TokenizerFixture

data PromptFixture = PromptFixture { source :: !PinnedSource, queryPrefix :: !Text.Text } deriving stock (Show, Eq, Generic)
instance FromJSON PromptFixture

data PoolingFixture = PoolingFixture { source :: !PinnedSource, lastTokenPooling :: !Bool } deriving stock (Show, Eq, Generic)
instance FromJSON PoolingFixture

data ReadmeFixture = ReadmeFixture { source :: !PinnedSource, normalizesEmbeddings :: !Bool } deriving stock (Show, Eq, Generic)
instance FromJSON ReadmeFixture

data TeiFixture = TeiFixture
  { sourceArchive :: !PinnedSource, qwen2Postprocessor :: !TeiSource, lastTokenPoolingSource :: !TeiSource
  , routerAutoTruncateSource :: !TeiSource, upstreamRouterAutoTruncateDefault :: !Bool, hmemContractAutoTruncate :: !Bool
  } deriving stock (Show, Eq, Generic)
instance FromJSON TeiFixture

data TeiSource = TeiSource
  { sourceUrl :: !Text.Text, sourceRevision :: !Text.Text, sourceFile :: !Text.Text, sourceLines :: ![Text.Text]
  } deriving stock (Show, Eq, Generic)
instance FromJSON TeiSource

data ContractFixture = ContractFixture
  { model :: !ModelFixture, tokenizer :: !TokenizerFixture, sentenceTransformers :: !PromptFixture
  , pooling :: !PoolingFixture, readme :: !ReadmeFixture, tei :: !TeiFixture
  } deriving stock (Show, Eq, Generic)
instance FromJSON ContractFixture

loadFixture :: IO ContractFixture
loadFixture = do
  bytes <- BS.readFile "test/fixtures/embedding-contract/gte-qwen2-contract.json"
  case eitherDecodeStrict' bytes of
    Left errorValue -> expectationFailure errorValue >> fail "unreachable"
    Right fixture -> pure fixture

assertPinnedSource :: Text.Text -> PinnedSource -> Expectation
assertPinnedSource revision sourceValue = do
  sourceValue.sourceRevision `shouldBe` revision
  sourceValue.sourceUrl `shouldSatisfy` Text.isInfixOf revision
  sourceValue.sourceTreeUrl `shouldSatisfy` Text.isInfixOf revision
  sourceValue.sourceSha256 `shouldSatisfy` (not . Text.null)
  sourceValue.sourceFile `shouldSatisfy` (not . Text.null)
  sourceValue.sourceLines `shouldSatisfy` (not . null)

assertTeiSource :: Text.Text -> TeiSource -> Expectation
assertTeiSource revision sourceValue = do
  sourceValue.sourceRevision `shouldBe` revision
  sourceValue.sourceUrl `shouldBe` "https://raw.githubusercontent.com/huggingface/text-embeddings-inference/06670157fb6c1523482219bdb2d1660277d38088/router/src/lib.rs"
  sourceValue.sourceFile `shouldBe` "router/src/lib.rs"
  sourceValue.sourceLines `shouldSatisfy` (not . null)
