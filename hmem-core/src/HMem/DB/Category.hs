module HMem.DB.Category
  ( createCategory
  , getCategory
  , updateCategory
  , deleteCategory
  , listCategories
  , listGlobalCategories
  , linkMemoryCategory
  , unlinkMemoryCategory
  ) where

import Control.Exception (throwIO)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Rel8

import HMem.DB.Pool (runSession, runTransaction, DBException(..))
import HMem.DB.Schema
import HMem.Types

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

rowToCategory :: MemoryCategoryT Result -> MemoryCategory
rowToCategory r = MemoryCategory
  { id          = r.mcId
  , workspaceId = r.mcWorkspaceId
  , name        = r.mcName
  , description = r.mcDescription
  , parentId    = r.mcParentId
  , createdAt   = r.mcCreatedAt
  }

------------------------------------------------------------------------
-- Create
------------------------------------------------------------------------

createCategory :: Pool Hasql.Connection -> CreateMemoryCategory -> IO MemoryCategory
createCategory pool cc = do
  rows <- runSession pool $ Session.statement () $ run $
    insert Insert
      { into = memoryCategorySchema
      , rows = values
          [ MemoryCategoryT
              { mcId          = unsafeDefault
              , mcWorkspaceId = lit cc.workspaceId
              , mcName        = lit cc.name
              , mcDescription = lit cc.description
              , mcParentId    = lit cc.parentId
              , mcDeletedAt   = unsafeDefault
              , mcCreatedAt   = unsafeDefault
              }
          ]
      , onConflict = Abort
      , returning  = Returning id
      }
  case rows of
    (r:_) -> pure $ rowToCategory r
    []    -> throwIO $ DBOtherError "createCategory: INSERT returned no rows"

------------------------------------------------------------------------
-- Read
------------------------------------------------------------------------

getCategory :: Pool Hasql.Connection -> UUID -> IO (Maybe MemoryCategory)
getCategory pool cid = do
  rows <- runSession pool $ Session.statement () $ run $ select $ do
    row <- each memoryCategorySchema
    where_ $ row.mcId ==. lit cid
    where_ $ activeCategory row
    pure row
  case rows of
    []    -> pure Nothing
    (r:_) -> pure . Just $ rowToCategory r

------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------

updateCategory :: Pool Hasql.Connection -> UUID -> UpdateMemoryCategory -> IO (Maybe MemoryCategory)
updateCategory pool cid uc = do
  rows <- runSession pool $ Session.statement () $ run $
    update Update
      { target = memoryCategorySchema
      , from = pure ()
      , set = \_ row -> row
          { mcName        = maybe row.mcName        lit uc.name
          , mcDescription = applyNullableUpdate row.mcDescription uc.description
          , mcParentId    = applyNullableUpdate row.mcParentId uc.parentId
          }
      , updateWhere = \_ row -> row.mcId ==. lit cid &&. activeCategory row
      , returning = Returning id
      }
  case rows of
    []    -> pure Nothing
    (r:_) -> pure . Just $ rowToCategory r

------------------------------------------------------------------------
-- Delete
------------------------------------------------------------------------

deleteCategory :: Pool Hasql.Connection -> UUID -> IO Bool
deleteCategory pool cid = do
  runTransaction pool $ do
    n <- Session.statement () $ runN $
      update Update
        { target = memoryCategorySchema
        , from = pure ()
        , set = \_ row -> row { mcDeletedAt = deletedNow }
        , updateWhere = \_ row -> row.mcId ==. lit cid &&. activeCategory row
        , returning = NoReturning
        }
    if n == 0
      then pure False
      else do
        Session.statement () $ run_ $
          update Update
            { target = memoryCategorySchema
            , from = pure ()
            , set = \_ row -> row { mcParentId = lit (Nothing :: Maybe UUID) }
            , updateWhere = \_ row -> row.mcParentId ==. lit (Just cid) &&. activeCategory row
            , returning = NoReturning
            }
        Session.statement () $ run_ $
          delete Delete
            { from = memoryCategoryLinkSchema
            , using = pure ()
            , deleteWhere = \_ row -> row.mclCategoryId ==. lit cid
            , returning = NoReturning
            }
        pure True

------------------------------------------------------------------------
-- List
------------------------------------------------------------------------

listCategories :: Pool Hasql.Connection -> UUID -> Maybe Int -> Maybe Int -> IO [MemoryCategory]
listCategories pool wsId mlimit moffset = do
  let lim = fromMaybe 50 mlimit
      off = fromMaybe 0  moffset
  rows <- runSession pool $ Session.statement () $ run $ select $
    limit (fromIntegral lim) $ offset (fromIntegral off) $ do
      row <- each memoryCategorySchema
      where_ $ row.mcWorkspaceId ==. lit (Just wsId)
      where_ $ activeCategory row
      pure row
  pure $ map rowToCategory rows

-- | List global categories (no workspace).
listGlobalCategories :: Pool Hasql.Connection -> Maybe Int -> Maybe Int -> IO [MemoryCategory]
listGlobalCategories pool mlimit moffset = do
  let lim = fromMaybe 50 mlimit
      off = fromMaybe 0  moffset
  rows <- runSession pool $ Session.statement () $ run $ select $
    limit (fromIntegral lim) $ offset (fromIntegral off) $ do
      row <- each memoryCategorySchema
      where_ $ isNull row.mcWorkspaceId
      where_ $ activeCategory row
      pure row
  pure $ map rowToCategory rows

------------------------------------------------------------------------
-- Memory ↔ category links
------------------------------------------------------------------------

linkMemoryCategory :: Pool Hasql.Connection -> UUID -> UUID -> IO ()
linkMemoryCategory pool memId catId =
  runSession pool $ Session.statement () $ run_ $
    insert Insert
      { into = memoryCategoryLinkSchema
      , rows = values
          [ MemoryCategoryLinkT
              { mclMemoryId   = lit memId
              , mclCategoryId = lit catId
              }
          ]
      , onConflict = DoNothing
      , returning = NoReturning
      }

unlinkMemoryCategory :: Pool Hasql.Connection -> UUID -> UUID -> IO ()
unlinkMemoryCategory pool memId catId =
  runSession pool $ Session.statement () $ run_ $
    delete Delete
      { from = memoryCategoryLinkSchema
      , using = pure ()
      , deleteWhere = \_ row -> row.mclMemoryId ==. lit memId &&. row.mclCategoryId ==. lit catId
      , returning = NoReturning
      }
