{-# LANGUAGE BlockArguments #-}

module Math.Statistics.RandomVarSpec (module Math.Statistics.RandomVarSpec) where

import Data.Word
import Math.Statistics.RandomVar
import System.Random (mkStdGen64)
import Test.Falsify.Generator qualified as G
import Test.Falsify.Predicate ((.$))
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range qualified as R
import Test.Tasty (TestTree)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.Falsify qualified as F

test_constant :: TestTree
test_constant = testProperty "constant rvar doesn't depend on any seed" do
  seed <- F.gen $ G.inRange $ R.between (minBound, maxBound :: Word64)
  k <- F.gen $ G.inRange $ R.between (0, 100 :: Int)
  let gen = mkStdGen64 seed
  F.assert $
    P.expect k .$ ("fst (sample (pure k) gen)", fst $ sample (pure k) gen)

test_uniformR :: TestTree
test_uniformR = testProperty "uniformR rvar generates values in the specified range" do
  seed <- F.gen $ G.inRange $ R.between (minBound, maxBound :: Word64)
  (l, u) <- F.gen do
    l <- G.inRange $ R.between (-100, 100 :: Int)
    step <- G.inRange $ R.between (0, 100)
    pure (l, l + step)
  let gen = mkStdGen64 seed
  F.assert $
    P.satisfies ("a", \a -> l <= a && a <= u)
      .$ ("fst (sample (uniformR (l, u)) gen)", fst $ sample (uniformR (l, u)) gen)

test_ap_independent :: TestTree
test_ap_independent = testProperty "liftA2 on rvar is independent to each other" do
  seed <- F.gen $ G.inRange $ R.between (minBound, maxBound :: Word64)
  let gen = mkStdGen64 seed
      ((l, r), _) = sample ((,) <$> uniform <*> uniform) gen
  F.assert $
    P.ne .$ ("l", l :: Int) .$ ("r", r :: Int)
