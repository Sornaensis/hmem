module HMem.DB.ObservationFixture (reviewed, writeObservation) where

import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import HMem.DB.Observation (getObservation, updateObservationReviewed)
import HMem.Types

reviewed :: Text -> ReviewedObservationUpdate
reviewed body = ReviewedObservationUpdate body (T.replicate 40 "b")

-- Setup writes consciously obtain the current token. Contention tests use the
-- production conditional function directly with a deliberately shared base.
writeObservation :: Pool Hasql.Connection -> UUID -> UUID -> ReviewedObservationUpdate -> IO (Maybe Observation)
writeObservation pool workspace observationId input = do
  base <- getObservation pool workspace observationId
  case base of
    Nothing -> pure Nothing
    Just row -> do
      result <- updateObservationReviewed pool workspace observationId row.contentVersion input
      case result of
        ObservationUpdated updated -> pure (Just updated)
        other -> fail (show other)
