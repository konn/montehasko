{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Math.Statistics.MonteCarlo.Pi (calculatePi) where

import Control.Lens
import Control.Monad (replicateM_, when)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.State.Strict (execStateT)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import System.Random.Stateful

data PiState = PiState {totalCount :: !Int, insideCount :: !Int}
  deriving (Show, Eq, Ord, Generic)

initialState :: PiState
initialState = PiState{totalCount = 0, insideCount = 0}

-- | Estimate the value of Pi using the Monte Carlo method.
calculatePi :: (StatefulGen g m) => Int -> g -> m Double
calculatePi n gen = estimate <$> execStateT (replicateM_ n go) initialState
 where
  estimate PiState{..} =
    4 * fromIntegral insideCount / fromIntegral totalCount
  go = do
    x <- lift $ uniformRM (0.0 :: Double, 1) gen
    y <- lift $ uniformRM (0.0 :: Double, 1) gen
    #totalCount += 1
    when (x * x + y * y <= 1) do
      #insideCount += 1

-- >>> runStateGenT (mkStdGen 42) $ calculatePi 10000
-- (3.1264,StdGen {unStdGen = SMGen 14787112805465166444 13679457532755275413})
-- >>> runStateGenT (mkStdGen 42) $ calculatePi 100000
-- (3.14628,StdGen {unStdGen = SMGen 8850561856670699532 13679457532755275413})
