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

main :: IO ()
main = putStrLn "Hello from BHC!"
```

Compile and run:

```bash
bhc Main.hs -o hello
./hello
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
-- ...
```

### Enable individual extensions

```haskell
{-# LANGUAGE BHC.StrictDefault #-}
{-# LANGUAGE BHC.TensorIR #-}
module Main where
-- ...
```

If you avoid `BHC*` extensions, your code remains portable between BHC and GHC.

## Existing Projects

BHC works with existing `.cabal` files and Hackage packages.

```bash
cd my-haskell-project
bhc build
bhc run
```

## Next Steps

- Read the [Compatibility Charter](/compatibility/) to understand what's supported
- Learn about [Runtime Profiles](/profiles/)
- Explore [Numeric Performance](/numeric/) for numeric workloads
- See [Target Backends](/targets/) for WASM and other targets
