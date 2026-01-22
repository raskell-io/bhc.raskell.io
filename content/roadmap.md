+++
title = "Roadmap"
description = "BHC development roadmap and milestones"
template = "page.html"
+++

# Roadmap

BHC is under active development. This roadmap outlines our priorities and milestones.

## Current Status: v0.2.0 (Alpha)

**Native code generation is working!** The compiler can now build and run standalone executables.

## Completed Milestones

### M0 — Proof of Life ✅

Tree-walking interpreter foundation.

### M0.5 — Native Code Generation ✅

End-to-end native compilation pipeline:

- LLVM backend via inkwell
- Core IR to LLVM lowering
- Runtime system with mark-sweep GC
- Linker integration for standalone executables
- Basic IO primitives (`print`, `putStrLn`)

```bash
$ cat Main.hs
main = print 42

$ bhc run Main.hs
42
```

### M1 — Numeric Profile Skeleton ✅

Profile system with numeric-specific compilation paths.

### M2 — Tensor IR ✅

Tensor intermediate representation with guaranteed fusion patterns.

### M3 — Vectorization ✅

Loop IR with SIMD auto-vectorization and parallel loops.

### M4 — Pinned Arrays + FFI ✅

Foreign function interface with pinned memory for zero-copy interop.

### M5 — Server Runtime ✅

Structured concurrency with work-stealing scheduler, cancellation, and deadlines.

### M6 — Platform Standardization ✅

Cross-platform standard library foundation.

### M7 — GPU Backend ✅

CUDA and ROCm code generation from Tensor IR.

### M8 — WASM Target ✅

WebAssembly backend with WASI support.

### M9 — Dependent Types Preview ✅

Experimental dependent types support.

### M10 — Cargo-Quality Diagnostics ✅

Structured error messages with suggestions and context.

## Current Focus

### M11 — Real-World Haskell Compatibility 🔄

- LANGUAGE pragma support
- Full Haskell 2010 layout rule
- Module system with qualified imports
- Type classes and instances
- Pattern guards and view patterns
- Type families and advanced type features
- Improved Prelude coverage

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

- [Contributing Guide](https://github.com/raskell-io/bhc/blob/main/CONTRIBUTING.md)
- [Good First Issues](https://github.com/raskell-io/bhc/labels/good%20first%20issue)
- [Discussions](https://github.com/raskell-io/bhc/discussions)

## Updates

Follow development:

- [GitHub Releases](https://github.com/raskell-io/bhc/releases)
- [Changelog](https://github.com/raskell-io/bhc/blob/main/CHANGELOG.md)
- [Twitter / X](https://x.com/raskelll)
