module Math.Statistics.MonteCarlo.Pi (
  piMonteCarlo,
  calcPi,
  calcPi_,
  iteratePi,
  piSampleSpace,
) where

import Linear
import Math.Statistics.MonteCarlo.Sampler
import Math.Statistics.RandomVar (RVar)
import Math.Statistics.RandomVar qualified as RVar
import Streaming.Prelude (Of, Stream)
import System.Random.Stateful

piMonteCarlo :: MonteCarlo (V2 Double) Double
piMonteCarlo = MonteCarlo piSampleSpace piEstimator

piSampleSpace :: RVar (V2 Double)
piSampleSpace = V2 <$> RVar.uniformR (0, 1) <*> RVar.uniformR (0, 1)

piEstimator :: Estimator (V2 Double) Double
piEstimator = 4 * count ((<= 1) . quadrance)

calcPi :: (RandomGen g) => Int -> g -> (Double, g)
calcPi = runMonteCarlo piMonteCarlo

{- | Calculates π by Monte Carlo method.

>>> calcPi_ 10000 (mkStdGen 42)
3.125999999999995

>>> calcPi_ 1000000 (mkStdGen 42)
3.141816000000033
-}
calcPi_ :: (RandomGen g) => Int -> g -> Double
calcPi_ = evalMonteCarlo piMonteCarlo

iteratePi :: (RandomGen g, Monad m) => g -> Stream (Of Double) m r
iteratePi gen = iterateMonteCarlo piMonteCarlo gen

{- $setup
>>> import System.Random.Stateful (mkStdGen)
>>> import Linear
-}
