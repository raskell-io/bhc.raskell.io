+++
title = "About BHC"
description = "About the Basel Haskell Compiler project"
template = "page.html"
+++

# About BHC

BHC (Basel Haskell Compiler) is an alternative compiler for Haskell, built around a simple premise: keep compatibility as the baseline, and make performance and deployment feel modern again.

## What BHC Is

BHC targets Haskell 2010 and selected GHC editions (for a documented subset of extensions), while introducing explicit runtime profiles for server latency, numeric workloads, and constrained targets. Innovations are opt-in and clearly namespaced (`BHC*`), so you can choose portability or specialization explicitly.

## Why Basel?

Glasgow gave Haskell its industrial-strength compiler. For over three decades, GHC has been the reference implementation, the research vehicle, and the production workhorse. That's a remarkable legacy.

Basel is a deliberate successor name — not a replacement, but a continuation. Like Glasgow, Basel is a European city with a history of precision engineering and scientific rigor. It's home to pharmaceutical research, chemical engineering, and a tradition of getting complex things right.

The name signals what this project values:

- **Precision over magic** — If performance matters, the compiler tells you what happened. No folklore, no "try adding bang patterns and see."
- **Engineering over research** — BHC isn't a research vehicle. It's built to ship production software to real targets.
- **Respect for lineage** — We're not here to "fix" Haskell or declare GHC obsolete. We're building on what works and making different tradeoffs for different use cases.

The Basel name is also a reminder: this is one compiler among potentially many. Haskell is bigger than any single implementation. A healthy ecosystem has room for compilers optimized for different goals.

## Project Goals

BHC exists to strengthen the Haskell ecosystem:

- We prioritize compatibility and portability
- We publish conformance tests and benchmarks
- We upstream proposals when ideas mature
- We collaborate rather than compete for identity

## Design Decisions

Every compiler makes tradeoffs. Here's why BHC made the choices it did.

### Why Rust?

GHC is written in Haskell (bootstrapped). That's elegant, but it also means the compiler inherits Haskell's runtime characteristics — lazy evaluation, garbage collection, unpredictable memory usage during compilation.

Rust gives us:

- **Predictable compilation performance** — No GC pauses during compilation, predictable memory usage, fast incremental builds.
- **Memory safety without runtime cost** — The compiler itself won't segfault or leak. We get safety guarantees at compile time.
- **Modern tooling** — Cargo, rust-analyzer, excellent IDE support. Contributors can be productive quickly.
- **No bootstrap problem** — You don't need a working Haskell compiler to build BHC.

This choice has no effect on the Haskell code you write. It's purely about compiler engineering.

### Why LLVM?

We didn't want to write a code generator from scratch. LLVM gives us:

- **Battle-tested optimizations** — Decades of work on register allocation, instruction selection, loop optimization.
- **Multi-target support** — x86-64, ARM64, RISC-V, WebAssembly (via separate backend), and more.
- **SIMD auto-vectorization** — Critical for the Numeric profile's performance guarantees.
- **Industry standard** — GHC uses LLVM (optionally). So do Rust, Swift, Julia, and Clang. The tooling and knowledge base are mature.

The GPU backends (CUDA/ROCm) generate PTX and AMDGCN directly — LLVM doesn't help there. The WASM backend also bypasses LLVM for tighter control over output size.

### Why Profiles Instead of Flags?

GHC has hundreds of flags. You can enable strictness analysis, change the GC, tune the RTS, enable LLVM, and more. This is powerful but creates a combinatorial explosion of configurations.

Profiles are opinionated bundles:

- **Fewer decisions** — Pick `server`, `numeric`, or `edge`. The compiler knows what that means.
- **Tested combinations** — Each profile is a tested, documented configuration. We don't ship flag combinations we haven't validated.
- **Clear contracts** — "Numeric profile guarantees fusion for these patterns" is a contract. "Try `-fstrictness` and see" is not.
- **Per-module granularity** — Different modules can use different profiles. Your hot numeric loop gets `numeric`; your CLI parsing gets `default`.

You can still tune within a profile, but the profile gives you a sane starting point with documented behavior.

### Why a New Runtime?

Haskell's semantics don't mandate a specific runtime. GHC's RTS is excellent but optimized for GHC's compilation model.

BHC's RTS is designed around profiles:

- **Server profile** — Work-stealing scheduler, structured concurrency primitives, cancellation propagation, tracing hooks.
- **Numeric profile** — Hot arena allocation, pinned buffers for FFI, minimal GC interaction in hot paths.
- **Realtime profile** — Incremental GC with bounded pauses, per-frame arenas.
- **Embedded profile** — No GC at all. Static allocation only.

A single RTS can't optimize for all of these. Profile-specific runtime code lets us make different tradeoffs for different deployment targets.

### Why Clean-Slate?

We could have forked GHC and modified it. We chose not to because:

- **Different implementation language** — Rust vs Haskell means a fork isn't practical anyway.
- **Architectural freedom** — We wanted to design the IR pipeline, the query system, and the driver from scratch for modern incremental compilation.
- **No legacy constraints** — GHC supports platforms and features we don't need. A clean slate lets us focus.
- **Learning opportunity** — Building a compiler from scratch forces you to understand every decision. That understanding shows up in the design.

The downside is real: we're reimplementing decades of work. But we're also able to make choices GHC can't make without breaking existing users.

## What BHC Is Not

BHC is not:

- A new language
- A fork of Haskell
- A replacement for GHC
- An attempt to define a new Haskell standard

## The Runtime

BHC includes its own runtime system (BHC RTS). Runtime systems can differ between compilers even when the surface language is the same. BHC RTS focuses on:

- Structured concurrency (cancellation, deadlines, scoping)
- Observability (events, tracing hooks)
- Profile-driven allocation strategies (arena/pinned where appropriate)
- Predictable scheduling behavior for server and numeric workloads

In compatibility modes, BHC aims to preserve Haskell semantics. Where runtime behavior is observably different, we document it and test it.

## Open Source

BHC is open source under the MIT license.

- [GitHub Repository](https://github.com/raskell-io/bhc)
- [Issue Tracker](https://github.com/raskell-io/bhc/issues)
- [Discussions](https://github.com/raskell-io/bhc/discussions)

## Community

Join the conversation:

- [Twitter / X](https://x.com/raskelll)
- [GitHub Discussions](https://github.com/raskell-io/bhc/discussions)

## Acknowledgments

BHC builds on decades of work by the Haskell community, the GHC team, and researchers in functional programming and compiler design. We are grateful for this foundation.

---

*BHC is an independent compiler implementation for Haskell. "Haskell" is a community language; BHC does not define the Haskell standard.*
