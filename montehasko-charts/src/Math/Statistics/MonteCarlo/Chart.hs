{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Math.Statistics.MonteCarlo.Chart (
  statisticsPlot,
  convergencePlot,
) where

import Graphics.Rendering.Chart.Easy
import Math.Statistics.MonteCarlo.Sampler

plotLineIteration ::
  String ->
  [a] ->
  EC (Layout Int a) (PlotLines Int a)
plotLineIteration name s = do
  let xs = zip [0 ..] s
  line name [xs]

statisticsPlot ::
  (Num a) =>
  -- | # of representatives
  Int ->
  [Statistics a] ->
  EC (Layout Int a) ()
statisticsPlot n stats = do
  layout_title .= "Monte Carlo Statistics"
  let xs = map (take n . values) stats
  iforM_ xs \i ys -> do
    let name = "Sample " ++ show i
    plot $ plotLineIteration name ys

  plot $ liftEC do
    c <- takeColor
    plot_errbars_values .= [symErrPoint i stat.mean 0 stat.stddev | i <- [0 ..] | stat <- stats]
    plot_errbars_title .= "Errors"
    plot_errbars_line_style . line_color .= c
  plot $ points "Mean" [(i, stat.mean) | i <- [0 ..] | stat <- stats]
  plot $ line "Mean" [[(i, stat.mean) | i <- [0 ..] | stat <- stats]]

convergencePlot ::
  (Num a) =>
  [Statistics a] ->
  EC (Layout Double a) ()
convergencePlot stats = do
  layout_title .= "Convergence relative to sqrt(N)"
  plot $
    line
      "Mean"
      [ [ (sqrt $ fromIntegral @Int i, stat.mean)
        | i <- [0 ..]
        | stat <- stats
        ]
      ]

  plot $ liftEC do
    c <- takeColor
    plot_errbars_values .= [symErrPoint (sqrt $ fromIntegral @Int i) stat.mean 0 stat.stddev | i <- [0 ..] | stat <- stats]
    plot_errbars_title .= "Errors"
    plot_errbars_line_style . line_color .= c
  plot $ points "Mean" [(sqrt $ fromIntegral @Int i, stat.mean) | i <- [0 ..] | stat <- stats]
