module HMem.MCP.Config
  ( resolveServerUrl
  , resolveForwardedToken
  , normalizeBaseUrl
  , normalizeOptionalString
  , isLoopbackServerUrl
  , safeServerUrlForLog
  ) where

import Data.Char (isDigit, isSpace, toLower)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

import HMem.Config qualified as Config


resolveServerUrl :: Maybe String -> Maybe String -> Text -> String
resolveServerUrl cliUrl envUrl configUrl =
  normalizeBaseUrl $ firstNonEmpty [cliUrl, envUrl, Just (T.unpack configUrl)]


resolveForwardedToken :: Config.HMemConfig -> String -> Bool -> Maybe String -> Maybe String -> Maybe String -> Maybe Text
resolveForwardedToken cfg targetUrl noAuth cliToken envMcpToken envAuthToken
  | noAuth = Nothing
  | otherwise = T.pack <$> firstNonEmptyMaybe normalizedTokenSources
  where
    tokenSources = [cliToken, envMcpToken, envAuthToken, T.unpack <$> localLegacyToken]
    normalizedTokenSources = fmap normalizeBearerToken <$> tokenSources

    localLegacyToken
      | isLoopbackServerUrl targetUrl = Config.authStaticBearerToken cfg.auth
      | otherwise = Nothing


normalizeOptionalString :: Maybe String -> Maybe String
normalizeOptionalString = (nonEmpty =<<)


normalizeBaseUrl :: String -> String
normalizeBaseUrl raw =
  let trimmed = trim raw
      withoutTrailing = dropWhileEndSlash trimmed
  in if null withoutTrailing then trimmed else withoutTrailing


isLoopbackServerUrl :: String -> Bool
isLoopbackServerUrl raw =
  case hostFromUrl (normalizeBaseUrl raw) of
    Nothing -> False
    Just host ->
      host == "localhost"
        || host == "::1"
        || isIPv4LoopbackLiteral host


safeServerUrlForLog :: String -> String
safeServerUrlForLog raw =
  case splitScheme (normalizeBaseUrl raw) of
    Nothing -> "<configured server URL>"
    Just (scheme, afterScheme) ->
      let authority = takeWhile (`notElem` ['/', '?', '#']) afterScheme
          withoutUserInfo = stripUserInfo authority
      in if null withoutUserInfo
        then "<configured server URL>"
        else scheme <> "://" <> withoutUserInfo


firstNonEmpty :: [Maybe String] -> String
firstNonEmpty values =
  case firstNonEmptyMaybe values of
    Just value -> value
    Nothing -> ""


firstNonEmptyMaybe :: [Maybe String] -> Maybe String
firstNonEmptyMaybe [] = Nothing
firstNonEmptyMaybe (value : rest) =
  case normalizeOptionalString value of
    Just normalized -> Just normalized
    Nothing -> firstNonEmptyMaybe rest


nonEmpty :: String -> Maybe String
nonEmpty value =
  let trimmed = trim value
  in if null trimmed then Nothing else Just trimmed


normalizeBearerToken :: String -> String
normalizeBearerToken value =
  let trimmed = trim value
      lowered = map toLower trimmed
  in case stripAuthorizationPrefix trimmed of
    Just rest -> normalizeBearerToken rest
    Nothing
      | lowered == "bearer" -> ""
      | "bearer" `isPrefixOf` lowered
      , all isSpace (take 1 (drop 6 trimmed)) -> trim (drop 7 trimmed)
      | otherwise -> trimmed


stripAuthorizationPrefix :: String -> Maybe String
stripAuthorizationPrefix original
  | Just rest <- stripAuthorizationPrefixChars original = Just rest
  | otherwise = Nothing


stripAuthorizationPrefixChars :: String -> Maybe String
stripAuthorizationPrefixChars original =
  case splitAt 13 original of
    (prefix, rest)
      | map toLower prefix == "authorization" ->
          case dropWhile isSpace rest of
            ':' : afterColon -> Just afterColon
            _ -> Nothing
    _ -> Nothing


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse


dropWhileEndSlash :: String -> String
dropWhileEndSlash value
  | value == "http://" || value == "https://" = value
  | otherwise = dropWhileEnd (== '/') value


hostFromUrl :: String -> Maybe String
hostFromUrl raw = do
  (_, afterScheme) <- splitScheme raw
  let withoutUserInfo = stripUserInfo $ takeWhile (`notElem` ['/', '?', '#']) afterScheme
      hostPort = withoutUserInfo
      host = case hostPort of
        ('[' : rest) -> takeWhile (/= ']') rest
        _ -> takeWhile (/= ':') hostPort
      lowered = map toLower host
  if null lowered then Nothing else Just lowered


splitScheme :: String -> Maybe (String, String)
splitScheme raw
  | "http://" `isPrefixOf` lowered = Just ("http", drop 7 raw)
  | "https://" `isPrefixOf` lowered = Just ("https", drop 8 raw)
  | otherwise = Nothing
  where
    lowered = map toLower raw


stripUserInfo :: String -> String
stripUserInfo = reverse . takeWhile (/= '@') . reverse


isIPv4LoopbackLiteral :: String -> Bool
isIPv4LoopbackLiteral host = case splitDots host of
  [a, b, c, d]
    | all allDigits [a, b, c, d]
    , Just first <- readMaybe @Int a
    , all octetInRange [a, b, c, d] -> first == 127
  _ -> False
  where
    allDigits part = not (null part) && all isDigit part
    octetInRange part = case readMaybe @Int part of
      Just n -> n >= 0 && n <= 255
      Nothing -> False


splitDots :: String -> [String]
splitDots [] = [""]
splitDots input = case break (== '.') input of
  (part, []) -> [part]
  (part, _ : rest) -> part : splitDots rest
