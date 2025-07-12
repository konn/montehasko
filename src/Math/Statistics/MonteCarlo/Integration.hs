{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Math.Statistics.MonteCarlo.Integration (
  integrator,
  integrate,
  expectOnFan,
) where

import Linear
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
    estimator = ((ub - lb) *) <$> estimateBy f

{- |
Integrate a unary function over a specified interval using Monte Carlo method.

>>> integrate (0, 1) (\x -> sqrt $ 1 - x * x)  1000 (mkStdGen 42)
0.7820929243459432

>>> integrate (0, 1) (\x -> sqrt $ 1 - x * x)  10000 (mkStdGen 42)
0.7837574992199501

>>> integrate (0, 1) (\x -> sqrt $ 1 - x * x)  100000 (mkStdGen 42)
0.7859682863878684
-}
integrate :: (RandomGen g) => (Double, Double) -> (Double -> Double) -> Int -> g -> Double
integrate bounds f = evalMonteCarlo (integrator bounds f)

{- |
Expected value on the fan-region defined by \(x^2 + y^2 < 1, x,y > 0\).

>>> 2 * pi * evalMonteCarlo (expectOnFan (\(V2 x y) -> sqrt $ 1 - x * x - y * y)) 100000 (mkStdGen 42)
4.180798994170995
-}
expectOnFan :: (V2 Double -> Double) -> MonteCarlo (V2 Double) Double
expectOnFan f = MonteCarlo {space, estimator}
  where
    space = V2 <$> RVar.uniformR (0, 1) <*> RVar.uniformR (0, 1)
    estimator = filtered ((< 1) . quadrance) (estimateBy f)

-- Needed for doctest:

{- $setup
>>> import System.Random.Stateful (mkStdGen)
>>> import Linear
>>> import Math.Statistics.MonteCarlo.Sampler
-}
