+++
title = "Get Started"
description = "Install BHC and compile your first Haskell project"
template = "page.html"
+++

# Get Started

Install BHC and compile your first Haskell project.

## Installation

### Script (recommended)

```bash
curl -fsSL https://bhc.raskell.io/install.sh | sh
```

### Cargo

```bash
cargo install bhc
```

### Verify installation

```bash
bhc --version
```

## Hello World

Create a file `Main.hs`:

```haskell
module Main where

-- | A simple greeting program demonstrating BHC basics.
-- This compiles identically with BHC and GHC, showcasing
-- BHC's commitment to Haskell compatibility.

import System.Environment (getArgs)
import Data.List (intercalate)

main :: IO ()
main = do
    args <- getArgs
    case args of
        []    -> putStrLn "Hello from BHC!"
        names -> putStrLn $ "Hello, " ++ intercalate " and " names ++ "!"

-- | A pure function that transforms greetings.
-- BHC compiles this the same way GHC would in default profile.
formatGreeting :: String -> String -> String
formatGreeting timeOfDay name =
    "Good " ++ timeOfDay ++ ", " ++ name ++ "!"
```

Compile and run:

```bash
bhc Main.hs -o hello
./hello
./hello Alice Bob
```

## Compile with Compatibility Mode

BHC supports Haskell 2010 and selected GHC editions.

```bash
# Haskell 2010 mode
bhc --edition=Haskell2010 Main.hs

# GHC2021 compatibility (documented subset)
bhc --edition=GHC2021 Main.hs

# GHC2024 compatibility (documented subset)
bhc --edition=GHC2024 Main.hs
```

## Use a Profile

Profiles change compilation and runtime contracts without changing the language.

```bash
# Default profile (general-purpose)
bhc Main.hs

# Server profile (structured concurrency, observability)
bhc --profile=server Main.hs

# Numeric profile (strict hot paths, fusion guarantees)
bhc --profile=numeric Main.hs

# Edge profile (smaller runtime for WASI/WASM)
bhc --profile=edge Main.hs
```

## Enable BHC Extensions

BHC-specific extensions are opt-in and namespaced.

### Enable the BHC2026 bundle

```haskell
{-# LANGUAGE BHC2026 #-}
module Main where

-- | The BHC2026 bundle enables all recommended BHC extensions:
--   - Strict evaluation by default in numeric contexts
--   - Tensor IR for array/matrix operations
--   - Enhanced type inference for numeric literals
--
-- This is the recommended starting point for new BHC projects
-- that don't need GHC compatibility.

import BHC.Prelude  -- Enhanced prelude with strict variants

-- | With BHC2026, numeric operations default to strict evaluation.
-- This function won't build up thunks even without explicit bangs.
fibonacci :: Int -> Integer
fibonacci n = go n 0 1
  where
    go 0 a _ = a
    go k a b = go (k - 1) b (a + b)  -- Strict by default, no thunk buildup

main :: IO ()
main = print $ fibonacci 100
```

### Enable individual extensions

```haskell
{-# LANGUAGE BHC.StrictDefault #-}
{-# LANGUAGE BHC.TensorIR #-}
module NeuralLayer where

-- | Selective use of BHC extensions for performance-critical code.
--
-- BHC.StrictDefault: All let bindings and function arguments are strict.
-- No accidental thunk accumulation in tight loops.
--
-- BHC.TensorIR: Matrix and vector operations use a specialized
-- intermediate representation that enables:
--   - Shape verification at compile time
--   - Automatic loop fusion across operations
--   - SIMD vectorization on supported platforms

import BHC.Tensor (Matrix, Vector, Scalar)
import qualified BHC.Tensor as T

-- | A single layer of a neural network.
-- With TensorIR, matrix operations are optimized as a unit.
data Layer = Layer
    { weights :: Matrix Double   -- Shape: (outputSize, inputSize)
    , biases  :: Vector Double   -- Shape: (outputSize,)
    }

-- | Forward pass through a layer with ReLU activation.
-- TensorIR fuses: matmul -> add bias -> apply ReLU into one kernel.
forward :: Layer -> Vector Double -> Vector Double
forward (Layer w b) input = T.map relu (T.add (T.matvec w input) b)
  where
    relu x = max 0 x  -- Compiled to branchless SIMD operations

-- | Compute gradients for backpropagation.
-- StrictDefault ensures intermediate gradients don't accumulate as thunks.
backward :: Layer -> Vector Double -> Vector Double -> (Matrix Double, Vector Double)
backward (Layer w _) input gradOutput = (gradWeights, gradInput)
  where
    gradWeights = T.outer gradOutput input  -- Outer product for weight gradient
    gradInput   = T.matvec (T.transpose w) gradOutput  -- Propagate gradient back
```

If you avoid `BHC*` extensions, your code remains portable between BHC and GHC.

## Existing Projects

BHC works with existing `.cabal` files and Hackage packages.

```bash
cd my-haskell-project
bhc build
bhc run
```

You can also use [hx](https://hx.raskell.io), a unified Haskell toolchain that handles packages, dependencies, and builds. hx and BHC are designed to work together, but BHC also works standalone with traditional cabal workflows.

## Next Steps

- Read the [Compatibility Charter](/compatibility/) to understand what's supported
- Learn about [Runtime Profiles](/profiles/)
- Explore [Numeric Performance](/numeric/) for numeric workloads
- See [Target Backends](/targets/) for WASM and other targets
