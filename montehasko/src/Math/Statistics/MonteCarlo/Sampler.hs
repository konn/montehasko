{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Math.Statistics.MonteCarlo.Sampler (
  MonteCarlo (..),
  SomeMonteCarlo (..),
  runMonteCarlo,
  evalMonteCarlo,
  iterateMonteCarlo,
  iterateMonteCarloN,
  Estimator (..),
  estimateBy,
  estimateMaybeBy,
  filtered,
  count,
  runEstimator,
  evalEstimator,
  iterateEstimator,
  iterateEstimatorN,
  estimate,

  -- * Statistics
  Statistics (..),
  statistics,
) where

import Control.Foldl qualified as L
import Control.Lens (_Just)
import Control.Monad (guard)
import Data.Functor.Identity (Identity (..))
import Data.List.Infinite (Infinite (..))
import Data.List.Infinite qualified as Inf
import Data.Monoid (Ap (..))
import Data.Profunctor (Profunctor (..))
import GHC.Generics (Generic, Generic1)
import Math.Statistics.RandomVar (RVar, samples, samplesN)
import Streaming (Stream)
import Streaming.Prelude (Of (..), lazily)
import Streaming.Prelude qualified as S
import System.Random (RandomGen, SplitGen)
import System.Random qualified as R

data SomeMonteCarlo a where
  MkSomeMonteCarlo :: MonteCarlo i a -> SomeMonteCarlo a

data MonteCarlo i a = MonteCarlo {space :: RVar i, estimator :: Estimator i a}
  deriving (Functor, Generic1)

data Estimator i a where
  Estimator ::
    forall i a b.
    (Fractional b) =>
    {evaluate :: i -> (Maybe b), extract :: b -> a} ->
    Estimator i a

deriving via Ap (Estimator i) a instance (Num a) => Num (Estimator i a)

instance (Fractional a) => Fractional (Estimator i a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational r = pure (fromRational r)

instance (Floating a) => Floating (Estimator i a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sqrt = fmap sqrt
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

estimateBy :: (Fractional a) => (i -> a) -> Estimator i a
{-# INLINE estimateBy #-}
estimateBy eval = Estimator (Just . eval) id

estimateMaybeBy :: (Fractional a) => (i -> Maybe a) -> Estimator i a
{-# INLINE estimateMaybeBy #-}
estimateMaybeBy eval = Estimator eval id

{- | Shrink the entire sampling space by a predicate.
'Applicative' instance takes intersection of all the predicates.
-}
filtered :: (i -> Bool) -> Estimator i a -> Estimator i a
filtered p (Estimator eval extract) =
  Estimator (\i -> guard (p i) *> eval i) extract

count :: (Fractional a) => (i -> Bool) -> Estimator i a
count p = estimateBy (\i -> if p i then 1 else 0)

runMonteCarlo :: (RandomGen g) => MonteCarlo i a -> Int -> g -> (a, g)
{-# INLINE runMonteCarlo #-}
runMonteCarlo (MonteCarlo rvar estim) = runEstimator estim rvar

evalMonteCarlo :: (RandomGen g) => MonteCarlo i a -> Int -> g -> a
{-# INLINE evalMonteCarlo #-}
evalMonteCarlo (MonteCarlo rvar estim) = evalEstimator estim rvar

iterateMonteCarlo ::
  (RandomGen g, Monad m) =>
  MonteCarlo i a ->
  g ->
  Stream (Of a) m x
{-# INLINE iterateMonteCarlo #-}
iterateMonteCarlo (MonteCarlo rvar estim) = iterateEstimator estim rvar

iterateMonteCarloN ::
  (RandomGen g, Monad m) =>
  Int ->
  MonteCarlo i a ->
  g ->
  Stream (Of a) m g
{-# INLINE iterateMonteCarloN #-}
iterateMonteCarloN n (MonteCarlo rvar estim) = iterateEstimatorN n estim rvar

runEstimator :: (RandomGen g) => Estimator i a -> RVar i -> Int -> g -> (a, g)
{-# INLINE runEstimator #-}
runEstimator mc rvar n =
  lazily
    . runIdentity
    . L.purely S.fold (estimate mc)
    . samplesN n rvar

evalEstimator :: (RandomGen g) => Estimator i a -> RVar i -> Int -> g -> a
{-# INLINE evalEstimator #-}
evalEstimator mc rvar n =
  runIdentity . L.purely S.fold_ (estimate mc) . samplesN n rvar

iterateEstimator ::
  (RandomGen g, Monad m) =>
  Estimator i a ->
  RVar i ->
  g ->
  Stream (Of a) m x
{-# INLINE iterateEstimator #-}
iterateEstimator mc rvar = L.purely S.scan (estimate mc) . samples rvar

iterateEstimatorN ::
  (RandomGen g, Monad m) =>
  Int ->
  Estimator i a ->
  RVar i ->
  g ->
  Stream (Of a) m g
{-# INLINE iterateEstimatorN #-}
iterateEstimatorN num mc rvar = L.purely S.scan (estimate mc) . samplesN num rvar

estimate :: Estimator i a -> L.Fold i a
{-# INLINE estimate #-}
estimate (Estimator eval extract) = extract <$> L.premap eval (L.handles _Just L.mean)

data Statistics a = Statistics
  { mean :: a
  , stddev :: a
  , values :: [a]
  }
  deriving (Show, Eq, Ord, Generic)

statistics ::
  (SplitGen g, Floating a) =>
  -- | # of random seeds
  Int ->
  -- | # of iterations
  Int ->
  MonteCarlo i a ->
  g ->
  Stream (Of (Statistics a)) Identity g
statistics numSeeds numSamples mc g =
  let gfin :< gs = Inf.unfoldr R.splitGen g
      seeds = Inf.take numSeeds gs
      streams =
        foldr
          (S.zipWith (:))
          (S.repeat [])
          $ map
            (iterateMonteCarloN numSamples mc)
            seeds
   in gfin <$ S.map calcStatistics streams

calcStatistics :: (Floating a) => [a] -> Statistics a
calcStatistics = L.fold do
  mean <- L.mean
  stddev <- L.std
  values <- L.list
  pure Statistics {..}

instance Functor (Estimator i) where
  fmap f (Estimator evaluate extract) =
    Estimator evaluate (f . extract)
  {-# INLINE fmap #-}

data P a b = P !a !b
  deriving (Show, Eq, Ord, Functor, Generic, Generic1)

instance (Num a, Num b) => Num (P a b) where
  P x1 x2 + P y1 y2 = P (x1 + y1) (x2 + y2)
  P x1 x2 * P y1 y2 = P (x1 * y1) (x2 * y2)
  negate (P x1 x2) = P (negate x1) (negate x2)
  abs (P x1 x2) = P (abs x1) (abs x2)
  signum (P x1 x2) = P (signum x1) (signum x2)
  fromInteger n = P (fromInteger n) (fromInteger n)

instance (Fractional a, Fractional b) => Fractional (P a b) where
  P x1 x2 / P y1 y2 = P (x1 / y1) (x2 / y2)
  recip (P x1 x2) = P (recip x1) (recip x2)
  fromRational r = P (fromRational r) (fromRational r)

instance (Floating a, Floating b) => Floating (P a b) where
  pi = P pi pi
  exp (P x1 x2) = P (exp x1) (exp x2)
  log (P x1 x2) = P (log x1) (log x2)
  sqrt (P x1 x2) = P (sqrt x1) (sqrt x2)
  P x1 x2 ** P y1 y2 = P (x1 ** y1) (x2 ** y2)
  logBase (P x1 x2) (P y1 y2) = P (logBase x1 y1) (logBase x2 y2)
  sin (P x1 x2) = P (sin x1) (sin x2)
  cos (P x1 x2) = P (cos x1) (cos x2)
  tan (P x1 x2) = P (tan x1) (tan x2)
  asin (P x1 x2) = P (asin x1) (asin x2)
  acos (P x1 x2) = P (acos x1) (acos x2)
  atan (P x1 x2) = P (atan x1) (atan x2)
  sinh (P x1 x2) = P (sinh x1) (sinh x2)
  cosh (P x1 x2) = P (cosh x1) (cosh x2)
  tanh (P x1 x2) = P (tanh x1) (tanh x2)
  asinh (P x1 x2) = P (asinh x1) (asinh x2)
  acosh (P x1 x2) = P (acosh x1) (acosh x2)
  atanh (P x1 x2) = P (atanh x1) (atanh x2)

data Zero = Zero
  deriving (Show, Eq, Ord, Generic)

instance Num Zero where
  _ + _ = Zero
  _ * _ = Zero
  negate = id
  abs = id
  signum = id
  fromInteger _ = Zero

instance Fractional Zero where
  _ / _ = Zero
  recip = id
  fromRational _ = Zero

instance Applicative (Estimator i) where
  pure = Estimator (\_ -> Just Zero) . const
  {-# INLINE pure #-}

  (Estimator eval1 extract1) <*> (Estimator eval2 extract2) =
    Estimator (\i -> P <$> eval1 i <*> eval2 i) (\(P l r) -> extract1 l (extract2 r))
  {-# INLINE (<*>) #-}

instance Profunctor Estimator where
  dimap f g (Estimator eval extract) =
    Estimator (eval . f) (g . extract)
  {-# INLINE dimap #-}
  rmap = fmap
  {-# INLINE rmap #-}
  lmap f (Estimator eval extract) =
    Estimator (eval . f) extract
  {-# INLINE lmap #-}
