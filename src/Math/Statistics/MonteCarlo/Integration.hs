{-# LANGUAGE RecordWildCards #-}

module Math.Statistics.MonteCarlo.Integration (integrate) where

import Control.Lens
import Control.Monad (replicateM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (execStateT)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import System.Random.Stateful

data IntegrationState = IntegrationState
  { totalCount :: !Int
  , sumValue :: !Double
  }
  deriving (Show, Eq, Generic)

integrate ::
  (StatefulGen g m) =>
  (Double -> Double) -> Double -> Double -> Int -> g -> m Double
integrate f a b n gen
  | a >= b = error "Lower bound must be strictly less than upper bound"
  | otherwise = estimate <$> execStateT (replicateM_ n go) initialState
 where
  initialState = IntegrationState{totalCount = 0, sumValue = 0}
  estimate IntegrationState{..} = sumValue / fromIntegral totalCount
  go = do
    x <- lift $ uniformRM (a, b) gen
    #totalCount += 1
    #sumValue += f x

-- >>> runStateGenT (mkStdGen 42) $ integrate (\x -> sqrt $ 1 - x * x) 0.0 1.0 1000
-- (0.7822914653537124,StdGen {unStdGen = SMGen 1271244949105037268 13679457532755275413})

-- >>> runStateGenT (mkStdGen 42) $ integrate (\x -> sqrt $ 1 - x * x) 0.0 1.0 10000
-- (0.7847234845769352,StdGen {unStdGen = SMGen 2819091809036269084 13679457532755275413})

-- >>> runStateGenT (mkStdGen 42) $ integrate (\x -> sqrt $ 1 - x * x) 0.0 1.0 100000
-- (0.7858276339200864,StdGen {unStdGen = SMGen 18297560408348587244 13679457532755275413})
