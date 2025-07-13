module Math.Statistics.MonteCarlo.SamplerSpec (
  module Math.Statistics.MonteCarlo.SamplerSpec,
) where

import Control.Foldl qualified as L
import Math.Statistics.MonteCarlo.Sampler
import Test.Falsify.Generator qualified as G
import Test.Falsify.Predicate ((.$))
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range qualified as R
import Test.Tasty
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.Falsify qualified as F

test_filtered :: TestTree
test_filtered =
  testGroup
    "filtered"
    [ testProperty "filtered affects count of samples" do
        lst <- F.gen $ G.list (R.between (10, 100)) $ G.inRange $ R.between (0 :: Int, 100)
        let est = filtered even (estimateBy $ fromIntegral @_ @Double)
            expected = L.fold (L.prefilter even $ L.premap fromIntegral L.mean) lst
        F.label "parity" $ map (\e -> if even e then "even" else "odd") lst
        F.assert $
          P.expect expected
            .$ ("sample rvar gen", L.fold (estimate est) lst)
    ]
