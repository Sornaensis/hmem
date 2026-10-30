{-# LANGUAGE BangPatterns #-}
-- | Versioned, bounded interpretation of native transcript occurrence times.
-- This projection never changes evidence or transport/lifecycle clocks.
module HMem.AgentSession.Occurrence
  ( Occurrence(..), emptyOccurrence, feedOccurrence, parseOccurrenceTime ) where

import Data.Aeson (Value(..), Result(..), eitherDecodeStrict', fromJSON)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isDigit, toLower)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Scientific (toRealFloat)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Text.Read (readMaybe)

data Occurrence = Occurrence
  { processedBytes :: !Int64, pending :: !ByteString
  , lastActivityAt :: !(Maybe UTCTime), sourceOffset :: !(Maybe Int64)
  , piVersion :: !(Maybe Int)
  } deriving stock (Show, Eq)

emptyOccurrence :: Occurrence
emptyOccurrence = Occurrence 0 BS.empty Nothing Nothing Nothing

-- At most one MiB arrives per turn; the only retained bytes are one incomplete
-- record (the canonical append validator limits individual records to one MiB).
-- A complete finite EOF record is consumed once; a later separator is empty.
feedOccurrence :: Bool -> ByteString -> Occurrence -> Occurrence
feedOccurrence final bytes previous = finish (walk start combined previous)
  where
    combined = previous.pending <> bytes
    start = previous.processedBytes - fromIntegral (BS.length previous.pending)
    end = previous.processedBytes + fromIntegral (BS.length bytes)
    walk offset input state = case BS.elemIndex 10 input of
      Just index -> let (line,rest) = BS.splitAt index input
                   in walk (offset + fromIntegral index + 1) (BS.drop 1 rest) (record offset line state)
      Nothing -> (offset,input,state)
    finish (offset,tailBytes,state)
      | final = (record offset tailBytes state) {processedBytes=end,pending=BS.empty}
      | otherwise = state {processedBytes=end,pending=tailBytes}
    record offset raw state = case eitherDecodeStrict' (boundedJSON raw) of
      Right value ->
        let version = if field "type" value == String "session"
              then Just (fromMaybe (-1) (integer (field "version" value))) else state.piVersion
            updated = state {piVersion=version}
        in case if eligible version raw value then occurrenceTime value else Nothing of
          Just time | maybe True (<= time) state.lastActivityAt -> updated {lastActivityAt=Just time,sourceOffset=Just offset}
          _ -> updated
      Left _ -> state

-- Keep the ordinary decoder's recursion finite even for canonical JSON with
-- hundreds of thousands of nested containers. Deep content is ambiguous to
-- the viewer's sixteen-level predicate already; retain that fact, never bytes
-- from the elided subtree. This is an interpretation buffer, not evidence.
boundedJSON :: ByteString -> ByteString
boundedJSON raw = scan 0 0 False False False 0 []
  where
    size=BS.length raw
    marker="{\"__hmem_ambiguous_content\":true}"
    scan !index !depth !quoted !escaped !skipping !start pieces
      | index==size = if null pieces then raw else BS.concat (reverse (BS.drop start raw:pieces))
      | quoted =
          let byte=BS.index raw index
          in scan (index+1) depth (escaped || byte/=34) (not escaped && byte==92) skipping start pieces
      | otherwise = case BS.index raw index of
          34 -> scan (index+1) depth True False skipping start pieces
          byte | byte==123 || byte==91 ->
            if not skipping && depth==32
            then scan (index+1) (depth+1) False False True index (marker:BS.take (index-start) (BS.drop start raw):pieces)
            else scan (index+1) (depth+1) False False skipping start pieces
          byte | byte==125 || byte==93 ->
            if skipping && depth==33
            then scan (index+1) (depth-1) False False False (index+1) pieces
            else scan (index+1) (depth-1) False False skipping start pieces
          _ -> scan (index+1) depth False False skipping start pieces

field :: Text -> Value -> Value
field name (Object value) = fromMaybe Null (KM.lookup (K.fromText name) value)
field _ _ = Null

text :: Value -> Text
text (String value) = value
text _ = ""

integer :: Value -> Maybe Int
integer value = case fromJSON value of Success number -> Just number; Error _ -> Nothing

visibleText :: Value -> Bool
visibleText (String value) = not (T.null (T.dropAround jsSpace value))
visibleText _ = False

-- ECMAScript WhiteSpace and LineTerminator, rather than Haskell isSpace:
-- U+FEFF is trimmed by String.trim; U+0085 and U+200B are visible.
jsSpace :: Char -> Bool
jsSpace c = c `elem` ['\x0009','\x000a','\x000b','\x000c','\x000d','\x0020','\x00a0','\x1680','\x2028','\x2029','\x202f','\x205f','\x3000','\xfeff']
  || (c>='\x2000' && c<='\x200a')

selected :: Value -> Value -> Value
selected first second = if truthy first then first else second

-- The legacy outer timestamp || time rule uses JSON/JavaScript truthiness:
-- null, false, numeric zero and an empty string fall through; a truthy invalid
-- outer value stays unknown. Numeric epochs/local/nested times are unsupported.
occurrenceTime :: Value -> Maybe UTCTime
occurrenceTime value = parseOccurrenceTime $ text $
  if truthy (field "timestamp" value) then field "timestamp" value else field "time" value

parseOccurrenceTime :: Text -> Maybe UTCTime
parseOccurrenceTime source = do
  let s=T.unpack source
  (date,rest) <- case splitAt 10 s of (a,'T':b)->Just(a,b); (a,'t':b)->Just(a,b); _->Nothing
  (year,month,day) <- case date of
    [a,b,c,d,'-',e,f,'-',g,h] | all isDigit [a,b,c,d,e,f,g,h] -> (,,) <$> readMaybe [a,b,c,d] <*> readMaybe [e,f] <*> readMaybe [g,h]
    _ -> Nothing
  calendar <- if year >= 1 then fromGregorianValid year month day else Nothing
  (hour,minute,second,zone) <- case rest of
    a:b:':':c:d:':':e:f:tailValue | all isDigit [a,b,c,d,e,f] -> (,,,) <$> (readMaybe [a,b] :: Maybe Int) <*> readMaybe [c,d] <*> readMaybe [e,f] <*> pure tailValue
    _ -> Nothing
  (fraction,offset) <- case zone of
    '.':xs -> let (digits,z)=span isDigit xs in if null digits then Nothing else Just (digits,z)
    _ -> Just ("",zone)
  shift <- case map toLower offset of
    "z" -> Just 0
    [sign,a,b,':',c,d] | sign `elem` ['+','-'] && all isDigit [a,b,c,d] -> do
      h <- readMaybe [a,b] :: Maybe Int; m <- readMaybe [c,d]
      if h<=23 && m<=59 then Just ((if sign=='-' then -1 else 1)*(h*60+m)) else Nothing
    _ -> Nothing
  if hour>23 || minute>59 || second>59 then Nothing else do
    -- PostgreSQL timestamptz stores microseconds. Truncate finer fractions
    -- explicitly rather than allowing rounding to change max/tie behavior.
    micros <- readMaybe (take 6 (fraction <> repeat '0')) :: Maybe Integer
    let seconds=fromIntegral (hour*3600+minute*60+second) + fromRational (toRational micros/1000000)
        instant=addUTCTime (fromIntegral (negate shift*60)) (UTCTime calendar seconds)
        (utcYear,_,_)=toGregorian (utctDay instant)
    if utcYear<1 || utcYear>9999 then Nothing else pure instant

eligible :: Maybe Int -> ByteString -> Value -> Bool
eligible version raw value
  | field "ephemeral" value == Bool True || kind `elem` ephemeral = False
  | isPi = piEligible
  | kind == "response_item" = case text (field "type" payload) of
      "message" -> visibleBlocks (field "content" payload)
      t | t `elem` ["function_call","custom_tool_call","function_call_output","custom_tool_call_output"] -> True
      "reasoning" -> truthy (field "encrypted_content" payload) || (has "summary" payload && case field "summary" payload of
        Array parts -> any (\part -> let value=field "text" part in truthy value && joinedText value) parts
        String t -> visibleText (String t)
        _ -> True)
      _ -> False
  | kind == "event_msg" = case text (field "type" payload) of
      t | t `elem` ["token_count","turn_aborted","task_started","task_complete"] -> False
      t | t `elem` ["agent_message","user_message","agent_reasoning"] -> visibleText (selected (field "message" payload) (field "text" payload))
      _ -> meaningful raw value
  | kind `elem` ["assistant","user"] && field "message" value /= Null = visibleBlocks (field "content" (field "message" value))
  | isObject dat = case kind of
      t | t `elem` ["hook.end","command.completed","permission.completed"] -> field "success" dat == Bool False
      "user.message" -> visibleText (field "content" dat)
      "assistant.message" -> visibleText (field "content" dat) || visibleText (field "reasoningText" dat) || requests (field "toolRequests" dat)
      "assistant.reasoning" -> visibleText (field "content" dat)
      t -> t `elem` ["tool.execution_start","tool.execution_complete"]
  | otherwise = False
  where
    kind=text (field "type" value); payload=field "payload" value; dat=field "data" value
    ephemeral=["assistant.message_delta","assistant.reasoning_delta","assistant.streaming_delta","tool.execution_partial_result","tool.execution_progress"]
    isPi = isString (field "id" value) && has "parentId" value && (kind `elem` ["message","custom_message","compaction","branch_summary","context_edit"])
    piEligible = maybe True (==3) version && (field "parentId" value == Null || isString (field "parentId" value)) && case kind of
      "message" -> let message=field "message" value in case text (field "role" message) of
        r | r `elem` ["user","assistant","system"] -> visibleBlocks (field "content" message)
        "custom" -> field "display" message == Bool True && visibleBlocks (field "content" message)
        r -> r `elem` ["toolResult","branchSummary","compactionSummary","bashExecution"]
      "custom_message" -> field "display" value == Bool True && visibleBlocks (field "content" value)
      t -> t `elem` ["compaction","branch_summary","context_edit"]
    requests (Array values) = any isObject values
    requests _ = False

-- Codex reasoning summary arrays use JavaScript Array.join, which coerces
-- truthy native text values before trimming the resulting summary.
joinedText :: Value -> Bool
joinedText (String value) = visibleText (String value)
joinedText Null = False
joinedText (Array values) = case foldr (:) [] values of
  [] -> False
  [value] -> joinedText value
  _ -> True -- the separating comma is visible, even for empty elements
joinedText _ = True

has :: Text -> Value -> Bool
has name (Object values) = KM.member (K.fromText name) values
has _ _ = False
isString, isObject :: Value -> Bool
isString (String _) = True
isString _ = False
isObject (Object _) = True
isObject _ = False

truthy :: Value -> Bool
truthy Null = False
truthy (Bool b) = b
truthy (String t) = not (T.null t)
truthy (Number n) = (toRealFloat n :: Double)/=0
truthy _ = True

visibleBlocks :: Value -> Bool
visibleBlocks (String t) = visibleText (String t)
visibleBlocks (Array values) = any visible values
  where
    visible v = case text (field "type" v) of
      t | t `elem` ["text","input_text","output_text"] -> visibleText (field "text" v)
      t | t `elem` ["redacted_thinking","encrypted_thinking","tool_use","toolCall","tool_result"] -> True
      t | t `elem` ["thinking","reasoning"] && (truthy (field "encrypted_content" v) || truthy (field "encrypted" v) || field "redacted" v==Bool True) -> True
      t | t `elem` ["thinking","reasoning","summary_text"] -> visibleText (selected (field "thinking" v) (field "text" v))
      _ -> False
visibleBlocks _ = False

-- Same finite meaningful-content predicate as the native viewer. Booleans and
-- discriminator-only objects are markers; ambiguous deep/large objects count.
meaningful :: ByteString -> Value -> Bool
meaningful raw value = inspect 256 [(0::Int,value,jsonSpace 0)]
  where
    inspect _ [] = False
    inspect budget _ | budget<=0 = True
    inspect budget ((depth,item,position):rest) = case item of
      String t -> visibleText (String t) || inspect (budget-1) rest
      Number _ -> True
      Array xs -> depth>=16 || not (null xs) || inspect (budget-1) rest
      Object object | depth>=16 -> True
        | otherwise -> case members (budget-1) position of
            Nothing -> True
            Just keys | any (meaningfulKey . fst) keys -> True
              | otherwise -> let children=[(depth+1,fromMaybe Null (KM.lookup (K.fromText key) object),start) | (key,start)<-keys, T.toLower key `notElem` discriminators]
                             in inspect (budget-1) (reverse children<>rest)
      _ -> inspect (budget-1) rest
    meaningfulKey key = let name=T.toLower key in
      any (`T.isSuffixOf` name) ["content","message","text","output","result","summary","error","warning","answer"]
      || name `elem` ["arguments","input","toolrequests","thinking","reasoningtext"]
    discriminators=["type","id","parentid","sessionid","messageid","toolcallid","timestamp","time","version"]
    jsonSpace !at | at<BS.length raw && BS.index raw at `elem` [9,10,13,32] = jsonSpace (at+1)
                  | otherwise = at
    stringEnd !at | BS.index raw at==92 = stringEnd (at+2)
                  | BS.index raw at==34 = at+1
                  | otherwise = stringEnd (at+1)
    valueEnd !at = case BS.index raw at of
      34 -> stringEnd (at+1)
      b | b `elem` [91,123] -> container (at+1) (1::Int)
      _ -> primitive at
    primitive !at | at==BS.length raw || BS.index raw at `elem` [9,10,13,32,44,93,125] = at
                  | otherwise = primitive (at+1)
    container !at !depth = case BS.index raw at of
      34 -> container (stringEnd (at+1)) depth
      b | b `elem` [91,123] -> container (at+1) (depth+1)
      b | b `elem` [93,125] -> if depth==1 then at+1 else container (at+1) (depth-1)
      _ -> container (at+1) depth
    -- Object.keys uses numeric indices first, then first insertion order;
    -- duplicate JSON keys keep their first position and final value. Retain at
    -- most budget+1 keys, while lexical skips never recursively decode values.
    members limit start = gather (jsonSpace (start+1)) Map.empty
      where
        gather !at found
          | BS.index raw at==125 = Just [(key,position) | (key,(_,position))<-sortOn order (Map.toList found)]
          | otherwise =
              let end=stringEnd (at+1)
                  key=case eitherDecodeStrict' (BS.take (end-at) (BS.drop at raw)) of Right (String t)->t; _->""
                  position=jsonSpace (jsonSpace end+1)
                  rank=maybe (Map.size found) fst (Map.lookup key found)
                  next=Map.insert key (rank,position) found
                  after=jsonSpace (valueEnd position)
              in if Map.size next>limit then Nothing else
                gather (if BS.index raw after==44 then jsonSpace (after+1) else after) next
        order (key,(rank,_)) = case if T.length key<=10 then readMaybe (T.unpack key) else Nothing :: Maybe Integer of
          Just number | number>=0 && number<4294967295 && T.pack (show number)==key -> (0::Int,number)
          _ -> (1,fromIntegral rank)
