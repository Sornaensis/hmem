module HMem.Server.Static
  ( staticMiddleware
  , resolveStaticDir
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Network.HTTP.Types (status200, hContentType)
import Network.Wai (Middleware)
import Network.Wai qualified as Wai
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), takeExtension)

import HMem.Config qualified as Config

-- | Resolve the static file directory.  Tries, in order:
--
--   1. An explicit path from config (if given and the directory exists)
--   2. @.\/static\/@ relative to the working directory
--   3. @~\/.hmem\/static\/@
--
-- Returns 'Nothing' if none of these exist.
resolveStaticDir :: Maybe FilePath -> IO (Maybe FilePath)
resolveStaticDir (Just dir) = do
  exists <- doesDirectoryExist dir
  if exists then Just <$> canonicalizePath dir else pure Nothing
resolveStaticDir Nothing = do
  localExists <- doesDirectoryExist "static"
  if localExists
    then Just <$> canonicalizePath "static"
    else do
      hmemDir <- Config.configDir
      let globalDir = hmemDir </> "static"
      globalExists <- doesDirectoryExist globalDir
      if globalExists then pure (Just globalDir) else pure Nothing

-- | WAI middleware that serves static files from the given directory.
-- Paths starting with @\/api\/@ are passed through to the inner
-- application.  For all other paths, if a matching file exists it is
-- served; otherwise @index.html@ is returned (SPA history-API
-- fallback).  When no static directory is configured the middleware
-- is a no-op.
staticMiddleware :: Maybe FilePath -> Middleware
staticMiddleware Nothing app req respond = app req respond
staticMiddleware (Just dir) app req respond
  | isApiPath req = app req respond
  | otherwise = do
      let segments = map T.unpack (Wai.pathInfo req)
          filePath = foldl (</>) dir segments
          indexPath = dir </> "index.html"
      exists <- doesFileExist filePath
      if exists
        then serveFile filePath respond
        else do
          indexExists <- doesFileExist indexPath
          if indexExists
            then serveFile indexPath respond
            else app req respond

isApiPath :: Wai.Request -> Bool
isApiPath req = case Wai.pathInfo req of
  ("api" : _) -> True
  _           -> False

serveFile :: FilePath -> (Wai.Response -> IO a) -> IO a
serveFile path respond = do
  contents <- BL.readFile path
  respond $ Wai.responseLBS status200
    [(hContentType, mimeForExt path)]
    contents

mimeForExt :: FilePath -> ByteString
mimeForExt path = case takeExtension path of
  ".html"  -> "text/html; charset=utf-8"
  ".js"    -> "application/javascript; charset=utf-8"
  ".mjs"   -> "application/javascript; charset=utf-8"
  ".css"   -> "text/css; charset=utf-8"
  ".json"  -> "application/json"
  ".svg"   -> "image/svg+xml"
  ".png"   -> "image/png"
  ".jpg"   -> "image/jpeg"
  ".jpeg"  -> "image/jpeg"
  ".gif"   -> "image/gif"
  ".ico"   -> "image/x-icon"
  ".woff"  -> "font/woff"
  ".woff2" -> "font/woff2"
  ".ttf"   -> "font/ttf"
  ".wasm"  -> "application/wasm"
  _        -> "application/octet-stream"
