{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Math.Statistics.MonteCarlo.Chart.App (defaultMain, defaultMainWith) where

import Control.Monad.Trans.Class (lift)
import GHC.Generics (Generic)
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy (Default (def), Identity (..))
import Linear
import Math.Statistics.MonteCarlo.Chart (convergencePlot, statisticsPlot)
import Math.Statistics.MonteCarlo.Integration qualified as Int
import Math.Statistics.MonteCarlo.Pi qualified as Pi
import Math.Statistics.MonteCarlo.Sampler
import Options.Applicative
import Options.Applicative qualified as Opt
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Random (mkStdGen, newStdGen)
import Text.Printf (printf)

data Opts = Opts
  { target :: !Target
  , outdir :: !FilePath
  , seed :: !(Maybe Int)
  , numRepr :: !Int
  , numSeeds :: !Int
  , numIter :: !Int
  , width :: !Int
  , height :: !Int
  , every :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

data Target = Pi | Integration | FanIntegration | FanIntegrationPolar
  deriving (Show, Eq, Ord, Generic)

optsP :: Opt.ParserInfo Opts
optsP =
  Opt.info
    (p <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Run Monte Carlo simulations for various targets and visualises convergence."
        <> Opt.header "Monte Carlo Chart Application"
    )
  where
    p :: Opt.Parser Opts
    p = do
      target <-
        Opt.subparser $
          mconcat
            [ Opt.command "pi" (Opt.info (pure Pi) (Opt.progDesc "Simulates pi"))
            , Opt.command "integration" (Opt.info (pure Integration) (Opt.progDesc "Simulates integration"))
            , Opt.command "fan" (Opt.info (pure FanIntegration) (Opt.progDesc "Simulates integration on fan"))
            , Opt.command "fan-polar" (Opt.info (pure FanIntegrationPolar) (Opt.progDesc "Simulates integration on fan with polar coordinates"))
            ]
      outdir <-
        Opt.strOption $
          Opt.long "outdir"
            <> Opt.short 'o'
            <> Opt.metavar "DIR"
            <> Opt.showDefault
            <> Opt.value "workspace"
      seed <-
        Opt.optional $
          Opt.option Opt.auto $
            Opt.long "seed"
              <> Opt.short 'S'
              <> Opt.metavar "INT"
              <> Opt.help "Seed for the random number generator (default: use system seed)."
      numSeeds <-
        Opt.option Opt.auto $
          Opt.short 'n'
            <> Opt.long "num-seeds"
            <> Opt.metavar "INT"
            <> Opt.showDefault
            <> Opt.value 100
            <> Opt.help "Number of distinct seeds to use for the simulation."
      numIter <-
        Opt.option Opt.auto $
          Opt.short 'i'
            <> Opt.long "num-iter"
            <> Opt.metavar "INT"
            <> Opt.help "Number of iterations for each seed."
            <> Opt.showDefault
            <> Opt.value 20000
      numRepr <-
        Opt.option Opt.auto $
          Opt.short 'r'
            <> Opt.long "num-repr"
            <> Opt.metavar "INT"
            <> Opt.showDefault
            <> Opt.value 4
            <> Opt.help "Number of representative samples to plot."
      width <-
        Opt.option Opt.auto $
          Opt.short 'w'
            <> Opt.long "width"
            <> Opt.metavar "INT"
            <> Opt.showDefault
            <> Opt.value 512
            <> Opt.help "Width of the output image."
      height <-
        Opt.option Opt.auto $
          Opt.short 'w'
            <> Opt.long "width"
            <> Opt.metavar "INT"
            <> Opt.showDefault
            <> Opt.value 512
            <> Opt.help "Width of the output image."
      every <-
        Opt.option Opt.auto $
          Opt.short 'N'
            <> Opt.long "every"
            <> Opt.metavar "INT"
            <> Opt.value 1
            <> Opt.help "Plot every N-th sample"
            <> Opt.showDefault
      pure Opts {..}

defaultMain :: IO ()
defaultMain = defaultMainWith =<< Opt.execParser optsP

defaultMainWith :: Opts -> IO ()
defaultMainWith Opts {..} = do
  -- Here you would typically call the function that runs the Monte Carlo simulation
  -- and generates the chart based on the provided options.
  putStrLn $ "Running simulation for target: " <> show target
  putStrLn $ "Output directory: " <> outdir
  createDirectoryIfMissing True outdir
  g <- case seed of
    Just s -> do
      putStrLn $ "Using user-provided seed: " <> show s
      pure $ mkStdGen s
    Nothing -> newStdGen
  let targetName = getTargetName target
  let stats :: [(Int, Statistics Double)]
      stats = case getMC target of
        MkSomeMonteCarlo mc ->
          runIdentity
            $ S.toList_
            $ S.concats
            $ S.maps
              ( \s ->
                  case runIdentity $ S.next s of
                    Left r -> pure r
                    Right (hd, tl) -> do
                      S.yield hd
                      lift $ S.effects tl
              )
            $ S.chunksOf every
            $ statistics numSeeds numIter mc g
  let statDiag = statisticsPlot numRepr stats
      convDiag = convergencePlot stats
  let statDest = outdir </> printf "statistics-%s-%d-%d.svg" targetName numSeeds numIter
      convDest = outdir </> printf "convergence-%s-%d-%d.svg" targetName numSeeds numIter

  putStrLn $ "Saving statistics to " <> statDest <> "..."
  toFile def statDest statDiag
  putStrLn $ "Statistics chart saved to: " <> statDest

  putStrLn $ "Saving convergence to " <> convDest <> "..."
  toFile def convDest convDiag
  putStrLn $ "Convergence chart saved to: " <> convDest

getTargetName :: Target -> String
getTargetName Pi = "pi"
getTargetName Integration = "integration"
getTargetName FanIntegration = "fan"
getTargetName FanIntegrationPolar = "fan-polar"

getMC :: Target -> SomeMonteCarlo Double
getMC Pi = MkSomeMonteCarlo Pi.piMonteCarlo
getMC Integration = MkSomeMonteCarlo $ Int.integrator (0, 1) (\x -> sqrt $ 1 - x * x)
getMC FanIntegration = MkSomeMonteCarlo $ Int.expectOnFan (\(V2 x y) -> sqrt $ 1 - x * x - y * y)
getMC FanIntegrationPolar = MkSomeMonteCarlo $ Int.expectOnFanPolar (\(V2 x y) -> sqrt $ 1 - x * x - y * y)
