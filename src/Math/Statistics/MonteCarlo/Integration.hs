{-# LANGUAGE RecordWildCards #-}

module Math.Statistics.MonteCarlo.Integration (
  integrator,
  integrate,
) where

import Data.Generics.Labels ()
import Math.Statistics.MonteCarlo.Sampler
import Math.Statistics.RandomVar qualified as RVar
import System.Random.Stateful

integrator :: (Double, Double) -> (Double -> Double) -> MonteCarlo Double Double
integrator (lb, ub) f
  | lb > ub = error "Lower bound must be strictly less than upper bound"
  | lb == ub = MonteCarlo {space = pure 0, estimator = pure 0}
  | otherwise = MonteCarlo {space, estimator}
  where
    space = RVar.uniformR (lb, ub)
    estimator = estimateBy f

integrate :: (RandomGen g) => (Double, Double) -> (Double -> Double) -> Int -> g -> Double
integrate bounds f = evalMonteCarlo (integrator bounds f)

-- >>> integrate (0, 1) (\x -> sqrt $ 1 - x * x)  1000 (mkStdGen 42)
-- 0.7820929243459432

-- >>> integrate (0, 1) (\x -> sqrt $ 1 - x * x)  10000 (mkStdGen 42)
-- 0.7837574992199501

-- >>> integrate (0, 1) (\x -> sqrt $ 1 - x * x)  100000 (mkStdGen 42)
-- 0.7859682863878684
