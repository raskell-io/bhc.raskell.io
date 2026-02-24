+++
title = "Roadmap"
description = "BHC development roadmap and milestones"
template = "page.html"
+++

# Roadmap

BHC is under active development. This roadmap outlines our priorities and milestones.

## Current Status

**M0–M10 Complete.** The compiler has working native code generation, runtime profiles, Tensor IR with fusion guarantees, SIMD vectorization, GPU backends, WebAssembly support, dependent types preview, and Cargo-quality diagnostics.

## Completed Milestones

### M0 — Proof of Life ✅

Tree-walking interpreter foundation with lexer, parser, minimal type checker, and Core IR.

### M1 — Numeric Profile Skeleton ✅

Strict-by-default evaluation for numeric code:

- Unboxed primitive types (`I32`, `I64`, `F32`, `F64`)
- Unboxed `UArray` representation
- Hot Arena allocator in RTS
- `lazy { }` escape hatch syntax

### M2 — Tensor IR v1 ✅

Tensor intermediate representation with guaranteed fusion:

- `Tensor` type with shape/stride metadata
- View operations (`reshape`, `slice`, `transpose`)
- Fusion pass for guaranteed patterns
- Kernel report mode (`-fkernel-report`)

### M3 — Vectorization + Parallel Loops ✅

Auto-vectorization and parallel execution:

- SIMD primitive types (`Vec4F32`, `Vec8F32`, `Vec2F64`, `Vec4F64`)
- Auto-vectorization pass
- `parFor`, `parMap`, `parReduce` primitives
- Work-stealing scheduler in RTS

### M4 — Pinned Arrays + FFI ✅

Zero-copy FFI and external BLAS integration:

- Pinned heap region in RTS
- `PinnedUArray` type
- Safe FFI boundary with pinned buffer support
- Reference OpenBLAS integration

### M5 — Server Runtime Contract ✅

Production-ready concurrent runtime:

- Structured concurrency primitives
- Cancellation propagation (cooperative)
- Deadline/timeout support
- Incremental/concurrent GC
- Event tracing hooks

### M6 — Platform Standardization ✅

Complete H26 Platform and conformance suite:

- All H26 Platform modules implemented
- Conformance test suite
- Package manifest and lockfile formats
- Reproducible build verification

### M7 — GPU Backend ✅

GPU compute support for numeric workloads:

- CUDA/ROCm code generation
- Device memory management
- Kernel fusion across host/device boundary

### M8 — WASM Target ✅

WebAssembly compilation for edge deployment:

- WebAssembly code generation
- Browser runtime
- Edge profile optimization

### M9 — Dependent Types Preview ✅

Shape-indexed tensors with compile-time dimension checking:

- Type-level naturals and lists
- Promoted list syntax `'[1024, 768]`
- Type families for shape operations
- Dynamic escape hatch (`DynTensor`)

### M10 — Cargo-Quality Diagnostics ✅

World-class compiler error messages:

- Cargo-style rendering with colors and context
- "Did you mean?" suggestions
- Visual ASCII diagrams for tensor shape errors
- LSP integration with code actions

## Current Focus

### M11 — Real-World Haskell Compatibility 🔄

Enable BHC to compile real-world Haskell projects like xmonad, pandoc, and lens.

#### Phase 1: LANGUAGE Pragmas
- Parse `{-# LANGUAGE ExtensionName #-}` at module level
- Parse `{-# OPTIONS_GHC ... #-}` and `{-# INLINE/NOINLINE #-}` pragmas
- Common extensions: `OverloadedStrings`, `LambdaCase`, `BangPatterns`, etc.

#### Phase 2: Layout Rule
- Implement Haskell 2010 layout rule (Section 10.3)
- Implicit `{`, `}`, `;` insertion based on indentation
- Handle `where`, `let`, `do`, `of`, `case` layout contexts

#### Phase 3: Module System
- Full export list syntax
- Import declarations with all forms (qualified, hiding, as)
- Hierarchical module names

#### Phase 4: Declarations
- Type class declarations with methods and defaults
- Instance declarations
- `deriving` clauses and standalone deriving
- GADT syntax, pattern synonyms, foreign declarations

#### Phase 5: Patterns & Expressions
- Pattern guards, view patterns, as-patterns
- Record patterns, infix constructor patterns
- Multi-way if, lambda-case, typed holes

#### Phase 6: Types
- `forall` quantification, scoped type variables
- Type applications, kind signatures
- Type families and associated types

#### Exit Criteria
- `bhc check` succeeds on xmonad source files
- `bhc check` succeeds on pandoc source files
- All Haskell 2010 Report features supported

## Future Milestones

### v1.0 — Stable

- Stable API and CLI
- Documented compatibility guarantees
- Production-ready runtime
- Comprehensive documentation
- Full Hackage package compatibility testing

## Not Planned (v1)

Some features are explicitly out of scope for v1:

- Template Haskell
- GHC plugins
- Backpack
- Full GHC extension compatibility

These may be considered for future versions based on community needs.

## Contributing

We welcome contributions. See:

- [Contributing Guide](https://github.com/arcanist-sh/bhc/blob/main/CONTRIBUTING.md)
- [Good First Issues](https://github.com/arcanist-sh/bhc/labels/good%20first%20issue)
- [Discussions](https://github.com/arcanist-sh/bhc/discussions)

## Updates

Follow development:

- [GitHub Releases](https://github.com/arcanist-sh/bhc/releases)
- [Changelog](https://github.com/arcanist-sh/bhc/blob/main/CHANGELOG.md)
- [Twitter / X](https://x.com/raskelll)
