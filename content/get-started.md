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

### Homebrew (macOS/Linux)

```bash
brew install raskell-io/tap/bhc
```

After installation, add the library path to your shell:

```bash
export LIBRARY_PATH="$(brew --prefix)/lib:$LIBRARY_PATH"
```

### hx (Haskell toolchain)

If you use [hx](https://hx.raskell.io), add BHC as a compiler backend:

```bash
hx toolchain add bhc
```

Then configure your project to use BHC in `hx.toml`:

```toml
[compiler]
backend = "bhc"
```

### Verify installation

```bash
bhc --version
```

## Hello World

Create a file `Main.hs`:

```haskell
module Main where

main :: IO ()
main = putStrLn "Hello from BHC!"
```

Compile and run:

```bash
bhc Main.hs -o hello
./hello
```

Output:
```
Hello from BHC!
```

## More Examples

BHC supports arithmetic and let bindings:

```haskell
module Main where

main :: IO ()
main = print (1 + 2 * 3)
```

```bash
bhc Main.hs -o calc
./calc
```

Output:
```
7
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

## Using hx with BHC

[hx](https://hx.raskell.io) is a unified Haskell toolchain that handles packages, dependencies, and builds. hx and BHC are designed to work together seamlessly.

### Basic Configuration

In your project's `hx.toml`:

```toml
[compiler]
backend = "bhc"
```

### BHC-Specific Options

Configure BHC features directly in `hx.toml`:

```toml
[compiler]
backend = "bhc"

[compiler.bhc]
profile = "numeric"           # default | server | numeric | edge
target = "aarch64-linux-gnu"  # Cross-compilation target
emit_kernel_report = true     # Performance reporting
tensor_fusion = true          # Enable tensor fusion optimizations
```

### CLI Override

Override the backend for a single build:

```bash
hx build --backend bhc
```

Priority order: CLI flag > hx.toml > default (GHC)

### Cross-Compilation Targets

hx with BHC supports additional targets beyond GHC:

```toml
[compiler.bhc]
target = "wasm32-wasi"      # WebAssembly
target = "riscv64-linux-gnu" # RISC-V
target = "aarch64-linux-gnu" # ARM64
```

### Build Commands

```bash
hx build    # Build with configured backend
hx check    # Type-check without full compilation
```

BHC also works standalone with traditional cabal workflows if you prefer not to use hx.

## Troubleshooting

### `bhc: command not found`

The BHC binary is not in your PATH. Add it:

```bash
# For bash
echo 'export PATH="$HOME/.bhc/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc

# For zsh
echo 'export PATH="$HOME/.bhc/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

### `library 'bhc_rts' not found` or linking errors

The RTS library path is not set. Add LIBRARY_PATH:

```bash
# For bash
echo 'export LIBRARY_PATH="$HOME/.bhc/lib:$LIBRARY_PATH"' >> ~/.bashrc
source ~/.bashrc

# For zsh
echo 'export LIBRARY_PATH="$HOME/.bhc/lib:$LIBRARY_PATH"' >> ~/.zshrc
source ~/.zshrc
```

### Unsupported platform

BHC v0.1.0-alpha supports:
- Linux x86_64
- macOS aarch64 (Apple Silicon)

Windows, Linux aarch64, and macOS x86_64 (Intel) support coming soon.

### Feature not working

BHC v0.1.0-alpha supports a subset of Haskell. Currently working:
- `putStrLn`, `print`
- Integer literals and arithmetic (`+`, `-`, `*`)
- Let bindings
- Simple function definitions

Not yet supported:
- `do` notation / monadic operations
- Pattern matching in function definitions
- Type classes
- Imports (except Prelude)

See the [Compatibility Charter](/compatibility/) for full details.

### Uninstalling BHC

To completely remove BHC:

```bash
curl -fsSL https://bhc.raskell.io/install.sh | sh -s -- --uninstall
```

Then remove these lines from your shell config (`~/.bashrc` or `~/.zshrc`):
```bash
export PATH="$HOME/.bhc/bin:$PATH"
export LIBRARY_PATH="$HOME/.bhc/lib:$LIBRARY_PATH"
```

## Next Steps

- Read the [Compatibility Charter](/compatibility/) to understand what's supported
- Learn about [Runtime Profiles](/profiles/)
- Explore [Numeric Performance](/numeric/) for numeric workloads
- See [Target Backends](/targets/) for WASM and other targets
- Browse the [API Reference](/docs/api/) for Prelude and standard library documentation
