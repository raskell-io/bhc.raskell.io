+++
title = "Target Backends"
description = "Compile Haskell to multiple targets"
template = "page.html"
+++

# Target Backends

BHC is designed for multiple targets. "Haskell" doesn't have to mean "one runtime on one OS."

## Available Targets

### Native

Server deployments and desktop applications.

```bash
bhc --target=native Main.hs
# or just:
bhc Main.hs
```

Status: **Supported**

- Full runtime features
- All profiles available
- Best performance for server workloads

### WASI/WASM

Sandboxed compute and edge deployments.

```bash
bhc --target=wasi Main.hs
```

Status: **Supported**

- WebAssembly output
- WASI system interface
- Works with edge profile
- Suitable for serverless and browser environments

Limitations:
- Some runtime features restricted by WASI
- Edge profile recommended for size

### Experimental

Additional backends under development. Check the [roadmap](/roadmap/) for status.

| Target | Status | Notes |
|--------|--------|-------|
| JavaScript | Planned | Browser-native execution |
| Bare metal | Experimental | No OS dependencies |

## Target Selection

### Command line

```bash
# Native (default)
bhc Main.hs

# WASI/WASM
bhc --target=wasi Main.hs

# Specify output
bhc --target=wasi -o app.wasm Main.hs
```

### Cross-compilation

BHC supports cross-compilation from your development machine to different targets.

```bash
# Compile WASM from macOS
bhc --target=wasi Main.hs -o app.wasm

# Run with wasmtime
wasmtime app.wasm
```

## Target-Specific Considerations

### Native Target

Full feature set:
- All runtime profiles
- Full FFI support
- Native threads and concurrency
- System calls unrestricted

### WASI Target

Restricted environment:
- WASI system interface only
- No native threads (cooperative concurrency)
- File system access through WASI capabilities
- Network access through WASI sockets (when available)

Recommended setup:
```bash
bhc --target=wasi --profile=edge Main.hs
```

### FFI Considerations

Foreign function calls differ by target:

| Target | FFI Support |
|--------|-------------|
| Native | Full C FFI |
| WASI | WASI imports only |

For portable code, abstract FFI behind a platform layer.

## Runtime Size

Different targets have different runtime sizes:

| Target | Profile | Approximate Size |
|--------|---------|------------------|
| Native | default | ~2 MB |
| Native | server | ~2.5 MB |
| Native | numeric | ~2.2 MB |
| WASI | edge | ~500 KB |

Sizes are approximate and depend on what features your code uses.

## Combining Targets and Profiles

| Profile | Native | WASI |
|---------|--------|------|
| default | ✅ | ✅ |
| server | ✅ | 🟡 (limited) |
| numeric | ✅ | ✅ |
| edge | ✅ | ✅ (recommended) |

The edge profile is designed for WASI and other constrained targets.
