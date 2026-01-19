+++
title = "Roadmap"
description = "BHC development roadmap and milestones"
template = "page.html"
+++

# Roadmap

BHC is under active development. This roadmap outlines our priorities and milestones.

## Current Status: Alpha

Core language compilation works. Edge cases and advanced features are under development.

## Milestones

### v0.1 — Foundation (Current)

- ✅ Haskell 2010 core language support
- ✅ Basic GHC2021 extension subset
- ✅ Native target backend
- ✅ Default and numeric profiles
- 🔄 WASI/WASM target
- 🔄 Server profile

### v0.2 — Compatibility

- GHC2024 extension subset
- Improved Hackage package compatibility
- Conformance test suite expansion
- FFI improvements

### v0.3 — Profiles

- Server profile completion
- Edge profile optimization
- Profile-specific runtime tuning
- Observability hooks

### v0.4 — Numeric

- Tensor IR stabilization
- SIMD lowering improvements
- Fusion guarantees formalization
- Benchmark suite

### v1.0 — Stable

- Stable API and CLI
- Documented compatibility guarantees
- Production-ready runtime
- Comprehensive documentation

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
- [Twitter / X](https://x.com/raskell_io)
