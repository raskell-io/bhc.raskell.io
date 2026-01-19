+++
title = "Runtime Profiles"
description = "Same language, different compilation and runtime contracts"
template = "page.html"
+++

# Runtime Profiles

BHC supports runtime profiles: the same source language, different compilation and runtime contracts. Profiles are explicit and localizable (package/module).

## How Profiles Work

- Profiles do not "rename the language"
- Profiles are explicit: you choose them; nothing silently changes
- Profile differences are documented and testable
- You can select profiles per package or per module

## Available Profiles

### default

General-purpose Haskell compilation. Focuses on correctness and broad compatibility.

```bash
bhc Main.hs
# or explicitly:
bhc --profile=default Main.hs
```

Best for:
- Library code
- Applications prioritizing compatibility
- Code that needs to work with both BHC and GHC

### server

Structured concurrency, predictable latency, observability hooks.

```bash
bhc --profile=server Main.hs
```

Features:
- Cancellation and deadlines for concurrent operations
- Scoped resource management
- Tracing and observability hooks
- Predictable scheduling behavior

Best for:
- Web services
- API servers
- Long-running daemons
- Applications needing observability

### numeric

Strict-by-default hot paths, unboxed-first numerics, tensor/array lowering, fusion guarantees, SIMD + parallel loops.

```bash
bhc --profile=numeric Main.hs
```

Features:
- Strict-by-default evaluation in hot paths (fewer thunks, fewer surprises)
- Unboxed numerics and flat arrays by default
- Array/tensor IR stage that understands shapes/strides/layouts
- Fusion as a contract for core patterns (map/zip/reduce)
- SIMD and parallel lowering when legal and profitable
- Pinned/non-moving buffers for FFI and large arrays

Best for:
- Scientific computing
- Data processing pipelines
- Machine learning workloads
- Performance-critical numeric code

See [Numeric Performance](/numeric/) for details.

### edge

Smaller runtime footprint and restricted environment assumptions.

```bash
bhc --profile=edge Main.hs
```

Features:
- Minimal runtime size
- Works within WASI constraints
- Reduced memory overhead
- Suitable for sandboxed execution

Best for:
- WebAssembly targets
- Serverless functions
- Embedded contexts
- Size-constrained deployments

## Selecting Profiles

### Command line

```bash
bhc --profile=numeric Main.hs
```

### Per-package (in cabal file)

```cabal
-- In your .cabal file
ghc-options: -profile=server
```

### Per-module

```haskell
{-# OPTIONS_BHC -profile=numeric #-}
module HotPath where
-- ...
```

## Profile Combinations

Profiles are mutually exclusive at the module level. A module compiles with exactly one profile. Different modules in the same package can use different profiles.

## Compatibility

All profiles compile the same Haskell source language. The differences are:
- Compilation strategy (strictness, unboxing decisions)
- Runtime behavior (scheduling, allocation)
- Available runtime features (tracing, concurrency primitives)

Code compiled with different profiles can interoperate, subject to documented constraints.
