{-# LANGUAGE DerivingVia #-}

module Math.Statistics.MonteCarlo.Sampler (
  Estimator (..),
  estimateBy,
  count,
  MonteCarlo (..),
  runMonteCarlo,
  evalMonteCarlo,
  iterateMonteCarlo,
  runEstimator,
  evalEstimator,
  iterateEstimator,
  estimate,
) where

import Control.Foldl qualified as L
import Data.Functor.Identity (Identity (..))
import Data.Profunctor (Profunctor (..))
import GHC.Generics (Generic, Generic1)
import Math.Statistics.RandomVar (RVar, samples, samplesN)
import Streaming (Stream)
import Streaming.Prelude (Of (..))
import Streaming.Prelude qualified as S
import System.Random (RandomGen)

data MonteCarlo i a = MonteCarlo {space :: RVar i, estimator :: Estimator i a}
  deriving (Functor, Generic1)

data Estimator i a where
  Estimator ::
    forall i a b.
    (Fractional b) =>
    {evaluate :: i -> b, extract :: b -> a} ->
    Estimator i a

instance (Num a) => Num (Estimator i a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger n = pure (fromInteger n)

instance (Fractional a) => Fractional (Estimator i a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational r = pure (fromRational r)

estimateBy :: (Fractional a) => (i -> a) -> Estimator i a
{-# INLINE estimateBy #-}
estimateBy eval = Estimator eval id

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

runEstimator :: (RandomGen g) => Estimator i a -> RVar i -> Int -> g -> (a, g)
{-# INLINE runEstimator #-}
runEstimator mc rvar n =
  (\(a :> b) -> (a, b))
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

estimate :: Estimator i a -> L.Fold i a
{-# INLINE estimate #-}
estimate (Estimator eval extract) = extract <$> L.premap eval L.mean

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
  pure = Estimator (\_ -> Zero) . const
  {-# INLINE pure #-}

  (Estimator eval1 extract1) <*> (Estimator eval2 extract2) =
    Estimator (\i -> P (eval1 i) $ eval2 i) (\(P l r) -> extract1 l (extract2 r))
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
