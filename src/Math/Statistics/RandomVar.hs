{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Math.Statistics.RandomVar (
  RVar (),
  uniform,
  uniformR,
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

instance (Num a) => Num (RVar a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Fractional a) => Fractional (RVar a) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}
