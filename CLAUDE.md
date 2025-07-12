# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Montehasko is a Haskell library for Monte Carlo Markov Chain (MCMC) computations. The library provides:

- Monte Carlo integration capabilities
- Random variable sampling with streaming support
- Estimators for statistical computations
- Example implementations (Pi calculation, numerical integration)

## Build System & Commands

This project uses Cabal as the build system:

```bash
# Build the project
cabal build

# Run tests (currently just a placeholder)
cabal test

# Clean build artifacts
cabal clean

# Start a REPL with the library loaded
cabal repl
```

## Architecture

### Core Modules Structure

The library is organized into hierarchical modules under `Math.Statistics`:

- **`RandomVar`**: Foundation for random variable generation using free applicatives and coyoneda for efficient composition
- **`MonteCarlo.Sampler`**: Core Monte Carlo framework with `Estimator` and `MonteCarlo` types
- **`MonteCarlo.Integration`**: Numerical integration using Monte Carlo methods
- **`MonteCarlo.Pi`**: Pi calculation as an example application

### Key Design Patterns

**Free Applicative for Random Variables**: The `RVar` type uses free applicatives (`Ap`) wrapped around coyoneda-lifted functors (`RVarF`) to efficiently compose random variable operations while maintaining purity.

**Profunctor-based Estimators**: The `Estimator` type is a profunctor that separates evaluation logic (`i -> Maybe b`) from result extraction (`b -> a`), enabling flexible composition and filtering.

**Streaming Integration**: Uses the `streaming` library for memory-efficient iteration over Monte Carlo samples, with `foldl` for aggregation.

### Type-Level Architecture

- Uses GHC2024 language edition with key extensions: `OverloadedLabels`, `OverloadedRecordDot`, `RecordWildCards`
- Extensive use of higher-kinded types and advanced type system features (GADTs, type families)
- Leverages lens ecosystem for data manipulation and optics

## Dependencies

Key dependencies include:
- `streaming` and `foldl` for efficient data processing
- `lens` family libraries for optics
- `linear` for vector operations
- `free` for free monad/applicative structures
- Advanced Haskell libraries: `comonad`, `kan-extensions`, `profunctors`, `semigroupoids`

## Development Notes

- The test suite is currently a placeholder and needs implementation
- The library uses strict evaluation in performance-critical paths (see `Data.Strict.Tuple`)
- Random number generation is abstracted through `System.Random.Stateful` for flexibility
- Code includes doctests embedded as comments (see `Integration.hs` examples)