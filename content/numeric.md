+++
title = "Numeric Performance"
description = "Predictable performance for numeric workloads"
template = "page.html"
+++

# Numeric Performance

BHC's numeric profile provides predictable performance for numeric workloads through a tensor-native optimization pipeline.

## What We Provide

- Predictable performance in numeric workloads
- A tensor-native optimization pipeline
- Guaranteed fusion patterns for standard containers in numeric profile
- Reduced heap activity in hot loops via arenas and pinned buffers

## How It Works

In Numeric profile, BHC prioritizes:

### Strict-by-default evaluation
Fewer thunks, fewer surprises. Hot paths evaluate strictly to avoid accumulating unevaluated computations.

### Unboxed numerics
`Int`, `Double`, and other numeric types use unboxed representations by default. No pointer chasing for arithmetic.

### Flat arrays
Arrays are flat, contiguous memory. No indirection through boxed elements.

### Tensor IR stage
An intermediate representation that understands:
- Shapes and dimensions
- Strides and layouts
- Contiguity requirements

### Fusion as a contract
Core patterns fuse reliably:
- `map . map` → single pass
- `zip` combinations → single traversal
- `fold` after `map` → no intermediate allocation

This is a contract, not a best-effort optimization. If fusion should happen and doesn't, that's a bug.

### SIMD and parallel lowering
When legal and profitable:
- Vectorized operations using CPU SIMD instructions
- Parallel execution of independent loop iterations
- Automatic tiling for cache efficiency

### Pinned buffers
Large arrays and FFI buffers use pinned, non-moving memory:
- No GC relocation during computation
- Safe to pass to C code
- Predictable memory layout

## Usage

### Enable numeric profile

```bash
bhc --profile=numeric Main.hs
```

### Per-module

```haskell
{-# OPTIONS_BHC -profile=numeric #-}
module HotPath where

-- This module compiles with numeric optimizations
compute :: Vector Double -> Double
compute = V.sum . V.map (*2)
```

### With BHC extensions

```haskell
{-# LANGUAGE BHC.TensorIR #-}
{-# LANGUAGE BHC.StrictDefault #-}
module Compute where

-- Tensor operations use the specialized IR
matmul :: Matrix Double -> Matrix Double -> Matrix Double
matmul = ...
```

## Example: Vector Operations

```haskell
{-# OPTIONS_BHC -profile=numeric #-}
module Example where

import qualified Data.Vector.Unboxed as V

-- These operations fuse into a single pass
pipeline :: V.Vector Double -> Double
pipeline = V.sum . V.map (*2) . V.filter (>0)

-- No intermediate vectors allocated
-- Single traversal of the input
```

## Example: Matrix Computation

```haskell
{-# LANGUAGE BHC.TensorIR #-}
module Matrix where

-- The compiler understands matrix shapes
-- and generates efficient blocked operations
gemm :: Matrix Double -> Matrix Double -> Matrix Double
gemm a b = ...
```

## What We Don't Promise

We avoid overpromising:

- We don't claim "faster than X across the board"
- We don't claim "GC-free Haskell"
- We don't claim "best-in-class for all workloads"

Numeric profile is optimized for numeric workloads. General-purpose code may perform the same or better with the default profile.

## Benchmarks

BHC publishes reproducible benchmarks for numeric workloads. See the [benchmarks repository](https://github.com/raskell-io/bhc/tree/main/benchmarks) for methodology and results.

We benchmark against:
- GHC with equivalent optimizations enabled
- Baseline implementations in other languages

All benchmarks include:
- Full source code
- Build instructions
- Hardware specifications
- Statistical methodology
