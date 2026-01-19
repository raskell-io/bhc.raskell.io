+++
title = "About BHC"
description = "About the Basel Haskell Compiler project"
template = "page.html"
+++

# About BHC

BHC (Basel Haskell Compiler) is an alternative compiler for Haskell, built around a simple premise: keep compatibility as the baseline, and make performance and deployment feel modern again.

## What BHC Is

BHC targets Haskell 2010 and selected GHC editions (for a documented subset of extensions), while introducing explicit runtime profiles for server latency, numeric workloads, and constrained targets. Innovations are opt-in and clearly namespaced (`BHC*`), so you can choose portability or specialization explicitly.

## The Basel Name

BHC is named after Basel, Switzerland — a respectful nod to the Glasgow lineage, and a signal that the project is focused on practical engineering outcomes: predictability, observability, and serious numeric performance.

## Project Goals

BHC exists to strengthen the Haskell ecosystem:

- We prioritize compatibility and portability
- We publish conformance tests and benchmarks
- We upstream proposals when ideas mature
- We collaborate rather than compete for identity

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

- [Discord](https://discord.gg/raskell)
- [Twitter / X](https://x.com/raskell_io)

## Acknowledgments

BHC builds on decades of work by the Haskell community, the GHC team, and researchers in functional programming and compiler design. We are grateful for this foundation.

---

*BHC is an independent compiler implementation for Haskell. "Haskell" is a community language; BHC does not define the Haskell standard.*
