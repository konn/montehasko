{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Math.Statistics.MonteCarlo.Chart (
  statisticsPlot,
  convergencePlot,
) where

import Data.Colour.SRGB (sRGB24)
import Data.List (transpose)
import Graphics.Rendering.Chart.Easy
import Math.Statistics.MonteCarlo.Sampler

statisticsPlot ::
  (Num a) =>
  -- | # of representatives
  Int ->
  [(Int, Statistics a)] ->
  EC (Layout Int a) ()
statisticsPlot n (filter ((/= 0) . fst) -> stats) = do
  layout_title .= "Monte Carlo Statistics"
  layout_x_axis . laxis_title .= "K, # of Steps"
  setColors $
    cycle $
      map
        opaque
        [ sRGB24 0xff 0x4b 0x00
        , sRGB24 0x00 0x5a 0xff
        , sRGB24 0x3d 0xcf 0xff
        , sRGB24 0xf6 0xaa 0x00
        , sRGB24 0xff 0xf1 0x00
        ]
  let xs = take n $ transpose $ map (mapM values) stats
  iforM_ xs \i ys -> do
    let name = "Sample " <> show i
    plot $ line name [ys]

  setColors $ repeat $ opaque green
  plot $ liftEC do
    plot_errbars_values .= [symErrPoint i stat.mean 0 stat.stddev | (i, stat) <- stats]
    plot_errbars_title .= "Errors"
    plot_errbars_line_style . line_color .= errorColor
  plot $ line "Mean" [[(i, stat.mean) | (i, stat) <- stats]]

errorColor :: AlphaColour Double
errorColor = opaque (sRGB24 0x03 0xaf 0x7a)

convergencePlot ::
  (Num a) =>
  [(Int, Statistics a)] ->
  EC (Layout Double a) ()
convergencePlot (filter ((/= 0) . fst) -> stats) = do
  let calcX = recip . sqrt . fromIntegral @Int
  layout_title .= "Convergence relative to sqrt(N)"
  layout_x_axis . laxis_title .= "1/sqrt(K), where K = # of Steps"
  layout_x_axis . laxis_reverse .= True
  plot $
    line
      "Mean"
      [ [ (calcX i, stat.mean)
        | (i, stat) <- stats
        ]
      ]

  plot $ liftEC do
    plot_errbars_values .= [symErrPoint (calcX i) stat.mean 0 stat.stddev | (i, stat) <- stats]
    plot_errbars_title .= "Errors"
    plot_errbars_line_style . line_color .= errorColor
