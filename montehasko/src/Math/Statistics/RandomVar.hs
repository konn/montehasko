{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Math.Statistics.RandomVar (
  RVar (),

  -- ** Distribution
  uniform,
  uniformR,
  normal,
  standardNormal,

  -- ** Sampling
  sample,
  sampleM,
  samples,
  samplesN,

  -- * Re-exports
  RandomGen,
  Uniform,
  UniformRange,
) where

import Control.Applicative.Free.Fast
import Data.Functor.Coyoneda
import Data.Monoid qualified as Mon
import Data.Strict.Tuple
import Streaming.Prelude (Of, Stream)
import Streaming.Prelude qualified as S
import System.Random (RandomGen, Uniform)
import System.Random.Stateful (StatefulGen, UniformRange (..), runStateGen, uniformM)

data RVarF a where
  Uniform :: (Uniform a) => RVarF a
  UniformR :: (UniformRange a) => (a, a) -> RVarF a

newtype RVar a = RVar {unwrap :: Ap (Coyoneda RVarF) a}
  deriving newtype (Functor, Applicative)

embed :: RVarF a -> RVar a
embed = RVar . liftAp . liftCoyoneda

uniform :: (Uniform a) => RVar a
{-# INLINE uniform #-}
uniform = embed Uniform

uniformR :: (UniformRange a) => (a, a) -> RVar a
{-# INLINE uniformR #-}
uniformR range = embed (UniformR range)

-- | A random variable that follows the standard normal distribution.
standardNormal :: (Floating a, UniformRange a) => RVar a
standardNormal = do
  x <- uniformR (0, 1)
  y <- uniformR (0, 1)
  pure $ sqrt (-2 * log x) * cos (2 * pi * y)

-- | A random variable @'normal' mu sigma@  follows the normal distribution with mean @mu@ and variance @sigma^2@.
normal :: (Floating a, UniformRange a) => a -> a -> RVar a
normal mu sigma = do
  z <- standardNormal
  pure $ mu + sigma * z

sample :: (RandomGen g) => RVar a -> g -> (a, g)
sample rv g = runStateGen g (sampleM rv)

sampleM :: (StatefulGen g m) => RVar a -> g -> m a
sampleM rv gen = runAp (lowerCoyoneda . hoistCoyoneda (go gen)) $ unwrap rv
  where
    go = flip \case
      Uniform -> uniformM
      UniformR range -> uniformRM range

samples ::
  (RandomGen g, Monad m) =>
  RVar a ->
  g ->
  Stream (Of a) m r
samples rv = S.unfoldr (\gen -> pure $ Right $ sample rv gen)

samplesN ::
  (RandomGen g, Monad m) =>
  Int ->
  RVar a ->
  g ->
  Stream (Of a) m g
samplesN n rv =
  S.unfoldr
    ( \(gen :!: k) ->
        if k <= 0
          then pure $ Left gen
          else pure $ Right $ (:!: k - 1) <$> sample rv gen
    )
    . (:!: n)

deriving via Mon.Ap RVar a instance (Num a) => Num (RVar a)

instance (Fractional a) => Fractional (RVar a) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance (Floating a) => Floating (RVar a) where
  pi = pure pi
  {-# INLINE pi #-}
  exp = fmap exp
  {-# INLINE exp #-}
  log = fmap log
  {-# INLINE log #-}
  sqrt = fmap sqrt
  {-# INLINE sqrt #-}
  (**) = liftA2 (**)
  {-# INLINE (**) #-}
  logBase = liftA2 logBase
  {-# INLINE logBase #-}
  sin = fmap sin
  {-# INLINE sin #-}
  cos = fmap cos
  {-# INLINE cos #-}
  tan = fmap tan
  {-# INLINE tan #-}
  asin = fmap asin
  {-# INLINE asin #-}
  acos = fmap acos
  {-# INLINE acos #-}
  atan = fmap atan
  {-# INLINE atan #-}
  sinh = fmap sinh
  {-# INLINE sinh #-}
  cosh = fmap cosh
  {-# INLINE cosh #-}
  tanh = fmap tanh
  {-# INLINE tanh #-}
  asinh = fmap asinh
  {-# INLINE asinh #-}
  acosh = fmap acosh
  {-# INLINE acosh #-}
  atanh = fmap atanh
  {-# INLINE atanh #-}
