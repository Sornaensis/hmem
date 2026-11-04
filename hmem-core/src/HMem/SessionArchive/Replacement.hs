{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
-- | The archive transform interprets only configured paths. It copies all
-- other source bytes from the input record, including its delimiter.
module HMem.SessionArchive.Replacement
  ( Policy, Rule, ReplacementError(..)
  , decodePolicy, freezePolicy, decodeSnapshot, replaceRecord, newReplacementSource
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (foldM, guard, unless, when)
import Data.Aeson (Value, eitherDecodeStrict', (.:), withObject)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding (encodingToLazyByteString, list, pairs, pair, text)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (inits, sortOn)
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word8)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr)

import HMem.AgentSession (maxSessionRecordBytes)
import HMem.SessionArchive.Codec (checkedAdd)

type Path = [Text]
data Rule = Rule !(Map Path Text) !Path !Text deriving stock (Show, Eq)
data Policy = Policy !UTCTime !(Map Text [Rule]) deriving stock (Show, Eq)
data ReplacementError = InvalidPolicy | InvalidRecord | AmbiguousRecord | RecordLimit
  deriving stock (Show, Eq)
instance Exception ReplacementError

decodePolicy :: ByteString -> Either ReplacementError Policy
decodePolicy = decodePolicyValue (withObject "archive replacement policy" $ \object -> do
  checkKeys ["version","eligible_from","formats"] object
  parseVersion object
  cutoff <- object .: "eligible_from"
  formats <- object .: "formats" >>= withObject "formats" (\labels -> do
    unless (KM.size labels >= 1 && KM.size labels <= 16) (fail "format count")
    Map.fromList <$> traverse (\(key,rules) -> do
      let label = Key.toText key
      unless (not (T.null label) && not (T.any (== '\0') label) && BS.length (TE.encodeUtf8 label) <= 128) (fail "format label")
      (label,) <$> parseRules rules) (KM.toList labels))
  pure (Policy cutoff formats))

-- The database records this effective cutoff at policy activation. Jobs copy
-- these exact bytes; a retry never interprets the current operator policy.
freezePolicy :: UTCTime -> Text -> Policy -> Either ReplacementError (Maybe (UTCTime,ByteString))
freezePolicy activated formatLabel (Policy cutoff formats) = traverse encode (Map.lookup formatLabel formats)
  where
    -- PostgreSQL timestamps have microsecond precision. Round eligibility up
    -- so storing the typed boundary can never make an older seal eligible.
    micros = ceiling (utcTimeToPOSIXSeconds (max activated cutoff) * 1000000) :: Integer
    effective = posixSecondsToUTCTime (fromIntegral micros / 1000000)
    encode rules = do
      let bytes = encodeSnapshot effective formatLabel rules
      unless (BS.length bytes <= 65536) (Left InvalidPolicy)
      pure (effective,bytes)

decodeSnapshot :: Text -> UTCTime -> ByteString -> Either ReplacementError [Rule]
decodeSnapshot expectedLabel expectedCutoff bytes = do
  (cutoff, formatLabel, rules) <- decodePolicyValue (withObject "archive policy snapshot" $ \object -> do
    checkKeys ["version","eligible_from","format_label","rules"] object
    parseVersion object
    (,,) <$> object .: "eligible_from" <*> object .: "format_label" <*> (object .: "rules" >>= parseRules)) bytes
  if cutoff == expectedCutoff && formatLabel == expectedLabel && bytes == encodeSnapshot cutoff formatLabel rules
    then Right rules else Left InvalidPolicy

encodeSnapshot :: UTCTime -> Text -> [Rule] -> ByteString
encodeSnapshot cutoff formatLabel rules = BL.toStrict $ encodingToLazyByteString $ pairs $
  pair "eligible_from" (Aeson.toEncoding cutoff) <>
  pair "format_label" (text formatLabel) <>
  pair "rules" (list encodeRule rules) <>
  pair "version" (Aeson.toEncoding (1 :: Int))
  where
    encodeRule (Rule discriminators target replacement) = pairs $
      pair "match" (pairs (foldMap (\(path,value) -> pair (Key.fromText path) (text value))
        (sortOn fst [(encodePath path,value) | (path,value) <- Map.toList discriminators]))) <>
      pair "path" (text (encodePath target)) <>
      pair "replacement" (text replacement)

decodePolicyValue :: (Value -> Parser a) -> ByteString -> Either ReplacementError a
decodePolicyValue parser bytes
  | BS.length bytes > 65536 = Left InvalidPolicy
  | otherwise = case scanRecord Set.empty bytes of
      Left _ -> Left InvalidPolicy
      Right _ -> either (const (Left InvalidPolicy)) Right (eitherDecodeStrict' bytes >>= parseEither parser)

parseVersion :: KM.KeyMap Value -> Parser ()
parseVersion object = do
  version <- object .: "version"
  unless (version == (1 :: Int)) (fail "unsupported version")

checkKeys :: [Key.Key] -> KM.KeyMap Value -> Parser ()
checkKeys allowed object = unless (all (`elem` allowed) (KM.keys object)) (fail "unknown key")

parseRules :: Value -> Parser [Rule]
parseRules value = do
  values <- Aeson.parseJSON value
  unless (not (null values) && length values <= 16) (fail "rule count")
  traverse parseRule values

parseRule :: Value -> Parser Rule
parseRule = withObject "replacement rule" $ \object -> do
  checkKeys ["match","path","replacement"] object
  discriminators <- object .: "match" >>= withObject "match" (\matches -> do
    unless (KM.size matches >= 1 && KM.size matches <= 8) (fail "discriminator count")
    entries <- traverse (\(key,value) -> (,) <$> parsePath (Key.toText key) <*> Aeson.parseJSON value) (KM.toList matches)
    let keyed = Map.fromList entries
    unless (Map.size keyed == length entries) (fail "duplicate discriminator")
    pure keyed)
  target <- object .: "path" >>= parsePath
  replacement <- if KM.member "replacement" object then object .: "replacement" else pure "Encrypted Payload"
  unless (BS.length (TE.encodeUtf8 replacement) <= 256) (fail "replacement length")
  pure (Rule discriminators target replacement)

parsePath :: Text -> Parser Path
parsePath pointer = case T.stripPrefix "/" pointer of
  Nothing -> fail "expected a non-root JSON pointer"
  Just rest -> do
    let segments = T.splitOn "/" rest
    unless (length segments <= 16) (fail "pointer depth")
    reverse <$> traverse (maybe (fail "invalid pointer escape") pure . unescape) segments
  where
    unescape = go []
      where
        go pieces segment = case T.breakOn "~" segment of
          (before, "") -> Just (T.concat (reverse (before:pieces)))
          (before, after) -> case T.uncons (T.drop 1 after) of
            Just ('0',suffix) -> go ("~":before:pieces) suffix
            Just ('1',suffix) -> go ("/":before:pieces) suffix
            _ -> Nothing

encodePath :: Path -> Text
encodePath = foldMap (("/" <>) . T.replace "/" "~1" . T.replace "~" "~0") . reverse

data Token = Token !Int !Int !Text

-- | Transform one record with or without its original LF or CRLF delimiter.
-- Return the number of selected string tokens, including identity replacements.
replaceRecord :: [Rule] -> ByteString -> Either ReplacementError (ByteString, Int)
replaceRecord rules bytes
  | recordLength > maxSessionRecordBytes = Left RecordLimit
  | otherwise = do
      captures <- scanRecord wanted bytes
      matches <- foldM (choose captures) Map.empty rules
      let pieces = sortOn (\(offset,_,_) -> offset) (Map.elems matches)
          growth = sum [BS.length value - (end-start) | (start,end,value) <- pieces]
      unless (recordLength + growth <= maxSessionRecordBytes) (Left RecordLimit)
      pure (if null pieces then bytes else BS.concat (splice 0 pieces), length pieces)
  where
    recordLength
      | BS.isSuffixOf "\r\n" bytes = BS.length bytes - 2
      | BS.isSuffixOf "\n" bytes = BS.length bytes - 1
      | otherwise = BS.length bytes
    paths = concatMap (\(Rule conditions target _) -> target : Map.keys conditions) rules
    wanted = Set.fromList (concatMap (map reverse . inits . reverse) paths)
    choose captures matches (Rule conditions target value) =
      case Map.lookup target captures of
        Just (Token start end original)
          | not (T.null original) && all (matchesValue captures) (Map.toList conditions) ->
              if Map.member target matches then Left AmbiguousRecord else
                Right (Map.insert target (start,end,BL.toStrict (Aeson.encode value)) matches)
        _ -> Right matches
    matchesValue captures (path, value) =
      case Map.lookup path captures of
        Just (Token _ _ actual) -> actual == value
        Nothing -> False
    splice offset [] = [BS.drop offset bytes]
    splice offset ((start,end,value):rest) =
      BS.take (start-offset) (BS.drop offset bytes) : value : splice end rest

-- | Bound the retained source to one current chunk and one record buffer.
-- Batching small records prevents one compressor call per short JSONL line.
newReplacementSource :: [Rule] -> IO (Maybe ByteString) -> IO (IO (Maybe ByteString), IO Int64)
newReplacementSource rules source = do
  buffer <- mallocForeignPtrBytes (maxSessionRecordBytes+2)
  used <- newIORef 0
  current <- newIORef BS.empty
  output <- newIORef Nothing
  count <- newIORef 0
  let append piece = do
        offset <- readIORef used
        let end = offset + BS.length piece
        when (end > maxSessionRecordBytes+2) (throwIO RecordLimit)
        withForeignPtr buffer $ \destination ->
          BS.useAsCStringLen piece $ \(pointer,n) ->
            copyBytes (destination `plusPtr` offset) pointer n
        writeIORef used end
      takeBuffer = do
        size <- readIORef used
        writeIORef used 0
        withForeignPtr buffer $ \pointer -> BS.packCStringLen (pointer,size)
      record bytes = do
        (replaced,n) <- either throwIO pure (replaceRecord rules bytes)
        total <- readIORef count >>= flip checkedAdd (fromIntegral n)
        writeIORef count total
        pure (Just replaced)
      nextRecord = do
        bytes <- readIORef current
        if BS.null bytes then source >>= \case
          Just more -> writeIORef current more >> nextRecord
          Nothing -> do
            size <- readIORef used
            if size == 0 then pure Nothing else takeBuffer >>= record
        else case BS.elemIndex 10 bytes of
          Nothing -> append bytes >> writeIORef current BS.empty >> nextRecord
          Just index -> do
            let (piece,rest) = BS.splitAt (index+1) bytes
            writeIORef current rest
            size <- readIORef used
            if size == 0 then record piece else append piece >> takeBuffer >>= record
      nextOutput = do
        pending <- readIORef output
        writeIORef output Nothing
        maybe nextRecord (pure . Just) pending
      collect !size !items pieces = nextOutput >>= \case
        Nothing -> pure (if null pieces then Nothing else Just (BS.concat (reverse pieces)))
        Just bytes
          | null pieces && BS.length bytes >= 65536 -> do
              let (piece,rest) = BS.splitAt maxSessionRecordBytes bytes
              unless (BS.null rest) (storeOwned rest)
              pure (Just piece)
          | size + BS.length bytes > 65536 || items >= (256 :: Int) ->
              storeOwned bytes >> pure (Just (BS.concat (reverse pieces)))
          -- An unchanged record can be a tiny slice of a full source chunk.
          -- Materialize the copy before advancing to the next source chunk.
          | otherwise -> let !owned = BS.copy bytes
                         in collect (size+BS.length bytes) (items+1) (owned:pieces)
      storeOwned bytes = let !owned = BS.copy bytes in writeIORef output (Just owned)
  pure (collect 0 0 [], readIORef count)

-- The scanner retains keys only for currently open objects and captures only
-- rule paths. Aeson validates a single string token at a time, including UTF-8,
-- escapes and surrogate pairs. Numbers are checked without numeric conversion.
scanRecord :: Set Path -> ByteString -> Either ReplacementError (Map Path Token)
scanRecord wanted bytes = do
  (end, captures) <- value 0 (Just []) (space 0) Map.empty
  unless (space end == size) (Left InvalidRecord)
  pure captures
  where
    size = BS.length bytes
    at i = if i < size then BS.index bytes i else 0
    space i | i < size && at i `elem` [9,10,13,32] = space (i+1)
            | otherwise = i
    child path key = do
      next <- (key :) <$> path
      guard (Set.member next wanted)
      pure next
    value :: Int -> Maybe Path -> Int -> Map Path Token -> Either ReplacementError (Int, Map Path Token)
    value !depth path i captures
      | i >= size = Left InvalidRecord
      | otherwise = case at i of
          123 -> container depth $ object (depth+1) path Set.empty (space (i+1)) captures
          91 -> container depth $ array (depth+1) path (0 :: Int) (space (i+1)) captures
          34 -> do
            (end, str) <- string i
            let next = case path of
                  Just key | Set.member key wanted -> Map.insert key (Token i end str) captures
                  _ -> captures
            pure (end, next)
          102 -> literal "false" i captures
          110 -> literal "null" i captures
          116 -> literal "true" i captures
          _ -> (,captures) <$> number i
    container depth action = if depth >= 64 then Left RecordLimit else action
    literal expected i captures
      | expected `BS.isPrefixOf` BS.drop i bytes = Right (i+BS.length expected,captures)
      | otherwise = Left InvalidRecord
    object depth path seen i captures
      | at i == 125 = Right (i+1,captures)
      | otherwise = member depth path seen i captures
    member depth path seen i captures = do
      unless (at i == 34) (Left InvalidRecord)
      (end, key) <- string i
      unless (Set.notMember key seen) (Left AmbiguousRecord)
      let colon = space end
      unless (at colon == 58) (Left InvalidRecord)
      (next,captured) <- value depth (child path key) (space (colon+1)) captures
      let sep = space next
      case at sep of
        44 -> member depth path (Set.insert key seen) (space (sep+1)) captured
        125 -> Right (sep+1,captured)
        _ -> Left InvalidRecord
    array depth path index i captures
      | at i == 93 = Right (i+1,captures)
      | otherwise = element depth path index i captures
    element depth path !index i captures = do
      (next,captured) <- value depth (path >>= \p -> child (Just p) (T.pack (show index))) i captures
      let sep = space next
      case at sep of
        44 -> element depth path (index+1) (space (sep+1)) captured
        93 -> Right (sep+1,captured)
        _ -> Left InvalidRecord
    string start = do
      end <- closing (start+1)
      str <- either (const (Left InvalidRecord)) Right (eitherDecodeStrict' (BS.take (end-start) (BS.drop start bytes)))
      pure (end,str)
    closing i
      | i >= size = Left InvalidRecord
      | at i == 34 = Right (i+1)
      | at i == 92 = closing (i+2)
      | at i < 32 = Left InvalidRecord
      | otherwise = closing (i+1)
    number start = do
      let first = if at start == 45 then start+1 else start
      integral <- case at first of
        48 -> Right (first+1)
        w | digit w -> Right (digits (first+1))
        _ -> Left InvalidRecord
      fraction <- if at integral == 46 then requiredDigits (integral+1) else Right integral
      if at fraction `elem` [69,101]
        then let sign = fraction+1
                 startDigits = if at sign `elem` [43,45] then sign+1 else sign
             in requiredDigits startDigits
        else Right fraction
    digit :: Word8 -> Bool
    digit w = w >= 48 && w <= 57
    digits i | digit (at i) = digits (i+1)
             | otherwise = i
    requiredDigits i = if digit (at i) then Right (digits (i+1)) else Left InvalidRecord
