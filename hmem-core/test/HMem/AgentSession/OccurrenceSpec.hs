module HMem.AgentSession.OccurrenceSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Aeson (FromJSON(..), eitherDecodeStrict', withObject, (.:))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import HMem.DB.TestHarness (resolveRepoRoot)
import System.FilePath ((</>))
import Test.Hspec
import HMem.AgentSession.Occurrence

spec :: Spec
spec = describe "Transcript occurrence extraction" $ do
  cases <- runIO $ do
    root <- resolveRepoRoot
    bytes <- BS.readFile (root </> "hmem-core" </> "test" </> "fixtures" </> "session-occurrence-parity.json")
    either fail pure (eitherDecodeStrict' bytes :: Either String [ParityCase])
  mapM_ (\(ParityCase label raw expected) -> it ("native viewer parity: "<>T.unpack label) $
    ((feedOccurrence True (TE.encodeUtf8 raw<>"\n") emptyOccurrence).lastActivityAt/=Nothing) `shouldBe` expected) cases
  it "validates explicit RFC3339 instants, calendars, offsets and microseconds" $ do
    parseOccurrenceTime "2026-10-30T12:00:00Z" `shouldBe` parseOccurrenceTime "2026-10-30t14:00:00+02:00"
    parseOccurrenceTime "2024-02-29T23:59:59.123456999z" `shouldBe` parseOccurrenceTime "2024-02-29T23:59:59.123456Z"
    mapM_ (\time -> parseOccurrenceTime time `shouldBe` Nothing)
      ["2026-02-29T12:00:00Z","2026-10-30T24:00:00Z","2026-10-30T12:00:60Z","2026-10-30T12:00:00+24:00","2026-10-30T12:00:00","1793352000","0000-01-01T00:00:00Z"]
  it "takes the maximum source occurrence with deterministic later-offset ties" $ do
    let a=event "2026-10-30T12:00:00Z" "agent_message" ",\"message\":\"hello\""
        b=event "2026-10-30T11:00:00Z" "unhandled_event" ",\"detail\":\"older\""
        c=event "2026-10-30T12:00:00Z" "unhandled_event" ",\"detail\":\"tie\""
        state=feedOccurrence True (a<>b<>c) emptyOccurrence
    state.lastActivityAt `shouldBe` parseOccurrenceTime "2026-10-30T12:00:00Z"
    state.sourceOffset `shouldBe` Just (fromIntegral (BS.length (a<>b)))
  it "excludes metadata, unknown shapes, empty messages and invalid outer timestamps" $ do
    let hidden=BS.concat [event "2099-01-01T00:00:00Z" kind ",\"message\":\"hidden\"" | kind<-["token_count","turn_aborted","task_started","task_complete"]]
        invalid="{\"type\":\"event_msg\",\"timestamp\":\"invalid\",\"time\":\"2099-01-01T00:00:00Z\",\"payload\":{\"type\":\"agent_message\",\"message\":\"bad outer\"}}\n"
        unknown="{\"timestamp\":\"2099-01-01T00:00:00Z\",\"type\":\"made_up\",\"content\":\"unsupported\"}\n"
    (feedOccurrence True (hidden<>invalid<>unknown<>event "2099-01-01T00:00:00Z" "marker" ",\"flag\":true") emptyOccurrence).lastActivityAt `shouldBe` Nothing
  it "handles every byte split, CRLF and finite EOF followed by a separator" $ do
    let body=event "2026-10-30T12:00:00Z" "agent_reasoning" ",\"text\":\"thinking\""
        expected=feedOccurrence True body emptyOccurrence
    mapM_ (\n -> feedOccurrence True (BS.drop n body) (feedOccurrence False (BS.take n body) emptyOccurrence) `shouldBe` expected) [0..BS.length body]
    let eof=feedOccurrence True (BS.init body) emptyOccurrence
        separated=feedOccurrence True "\r\n" eof
    separated.lastActivityAt `shouldBe` eof.lastActivityAt
    separated.sourceOffset `shouldBe` eof.sourceOffset
    separated.processedBytes `shouldBe` fromIntegral (BS.length body+1)
  it "bounds decoder recursion while retaining deep meaningful event activity" $ do
    let body="{\"timestamp\":\"2026-10-30T12:00:00Z\",\"type\":\"event_msg\",\"payload\":{\"type\":\"deep_event\",\"nested\":"<>BS.replicate 50000 91<>"true"<>BS.replicate 50000 93<>"}}\n"
    (feedOccurrence True body emptyOccurrence).lastActivityAt `shouldBe` parseOccurrenceTime "2026-10-30T12:00:00Z"
  it "supports native Codex, Claude, Copilot and pi envelopes without nested timestamp guessing" $ do
    let bodies=
          [ "{\"type\":\"response_item\",\"payload\":{\"type\":\"function_call\",\"arguments\":\"{}\"}}"
          , "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"hello\"}]}}"
          , "{\"type\":\"assistant.message\",\"data\":{\"reasoningText\":\"thinking\"}}"
          , "{\"type\":\"message\",\"id\":\"a\",\"parentId\":null,\"message\":{\"role\":\"bashExecution\",\"command\":\"pwd\"}}"
          ]
    mapM_ (\body -> (feedOccurrence True ("{\"timestamp\":\"2026-10-30T12:00:00Z\","<>BS.drop 1 body<>"\n") emptyOccurrence).lastActivityAt `shouldBe` parseOccurrenceTime "2026-10-30T12:00:00Z") bodies
    let unsupportedPi="{\"type\":\"session\",\"version\":4}\n"<>"{\"type\":\"message\",\"timestamp\":\"2099-01-01T00:00:00Z\",\"id\":\"a\",\"parentId\":null,\"message\":{\"role\":\"user\",\"content\":\"unsupported version\"}}\n"
    (feedOccurrence True unsupportedPi emptyOccurrence).lastActivityAt `shouldBe` Nothing

event :: String -> String -> String -> BS.ByteString
event time kind extra = B8.pack ("{\"type\":\"event_msg\",\"timestamp\":\""<>time<>"\",\"payload\":{\"type\":\""<>kind<>"\""<>extra<>"}}\n")

data ParityCase = ParityCase Text Text Bool
instance FromJSON ParityCase where
  parseJSON = withObject "ParityCase" $ \value -> ParityCase <$> value .: "name" <*> value .: "raw" <*> value .: "eligible"
