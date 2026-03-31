module HMem.DB.SavedView
  ( createSavedView
  , getSavedView
  , updateSavedView
  , deleteSavedView
  , purgeSavedView
  , listSavedViews
  ) where

import Control.Exception (throwIO)
import Data.Functor.Contravariant ((>$<))
import Data.Pool (Pool)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Rel8

import HMem.DB.Pool (runSession, DBException(..))
import HMem.DB.Schema
import HMem.Types

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

rowToSavedView :: SavedViewT Result -> SavedView
rowToSavedView r = SavedView
  { id          = r.svId
  , workspaceId = r.svWorkspaceId
  , name        = r.svName
  , description = r.svDescription
  , entityType  = r.svEntityType
  , queryParams = r.svQueryParams
  , createdAt   = r.svCreatedAt
  , updatedAt   = r.svUpdatedAt
  }

------------------------------------------------------------------------
-- Create
------------------------------------------------------------------------

createSavedView :: Pool Hasql.Connection -> CreateSavedView -> IO SavedView
createSavedView pool csv = do
  rows <- runSession pool $ Session.statement () $ run $
    insert Insert
      { into = savedViewSchema
      , rows = values
          [ SavedViewT
              { svId          = unsafeDefault
              , svWorkspaceId = lit csv.workspaceId
              , svName        = lit csv.name
              , svDescription = lit csv.description
              , svEntityType  = lit csv.entityType
              , svQueryParams = lit csv.queryParams
              , svDeletedAt   = unsafeDefault
              , svCreatedAt   = unsafeDefault
              , svUpdatedAt   = unsafeDefault
              }
          ]
      , onConflict = Abort
      , returning  = Returning id
      }
  case rows of
    (r:_) -> pure $ rowToSavedView r
    []    -> throwIO $ DBOtherError "createSavedView: INSERT returned no rows"

------------------------------------------------------------------------
-- Read
------------------------------------------------------------------------

getSavedView :: Pool Hasql.Connection -> UUID -> IO (Maybe SavedView)
getSavedView pool sid = do
  rows <- runSession pool $ Session.statement () $ run $ select $ do
    row <- each savedViewSchema
    where_ $ row.svId ==. lit sid
    where_ $ activeSavedView row
    pure row
  case rows of
    []    -> pure Nothing
    (r:_) -> pure . Just $ rowToSavedView r

------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------

updateSavedView :: Pool Hasql.Connection -> UUID -> UpdateSavedView -> IO (Maybe SavedView)
updateSavedView pool sid usv = do
  rows <- runSession pool $ Session.statement () $ run $
    update Update
      { target = savedViewSchema
      , from = pure ()
      , set = \_ row -> row
          { svName        = maybe row.svName lit usv.name
          , svDescription = applyNullableUpdate row.svDescription usv.description
          , svQueryParams = maybe row.svQueryParams lit usv.queryParams
          , svUpdatedAt   = unsafeLiteral "now()"
          }
      , updateWhere = \_ row -> row.svId ==. lit sid &&. activeSavedView row
      , returning = Returning id
      }
  case rows of
    []    -> pure Nothing
    (r:_) -> pure . Just $ rowToSavedView r

------------------------------------------------------------------------
-- Delete (soft)
------------------------------------------------------------------------

deleteSavedView :: Pool Hasql.Connection -> UUID -> IO Bool
deleteSavedView pool sid = do
  n <- runSession pool $ Session.statement () $ runN $
    update Update
      { target = savedViewSchema
      , from = pure ()
      , set = \_ row -> row { svDeletedAt = deletedNow }
      , updateWhere = \_ row -> row.svId ==. lit sid &&. activeSavedView row
      , returning = NoReturning
      }
  pure (n > 0)

------------------------------------------------------------------------
-- Purge (hard delete)
------------------------------------------------------------------------

purgeSavedView :: Pool Hasql.Connection -> UUID -> IO Bool
purgeSavedView pool sid = do
  -- Must be soft-deleted first
  rows <- runSession pool $ Session.statement () $ run $ select $ do
    row <- each savedViewSchema
    where_ $ row.svId ==. lit sid
    pure row
  case rows of
    []    -> pure False
    (r:_)
      | r.svDeletedAt == Nothing -> pure False
      | otherwise -> do
          runSession pool $ Session.statement () $ run_ $
            delete Delete
              { from = savedViewSchema
              , using = pure ()
              , deleteWhere = \_ row -> row.svId ==. lit sid
              , returning = NoReturning
              }
          pure True

------------------------------------------------------------------------
-- List
------------------------------------------------------------------------

listSavedViews :: Pool Hasql.Connection -> UUID -> Maybe Int -> Maybe Int -> IO [SavedView]
listSavedViews pool wsId mlimit moffset = do
  let (lim, off) = capPagination mlimit moffset
  rows <- runSession pool $ Session.statement () $ run $ select $
    limit (fromIntegral lim) $ offset (fromIntegral off) $
    orderBy ((\row -> row.svName) >$< asc) $ do
      row <- each savedViewSchema
      where_ $ row.svWorkspaceId ==. lit wsId
      where_ $ activeSavedView row
      pure row
  pure $ map rowToSavedView rows
