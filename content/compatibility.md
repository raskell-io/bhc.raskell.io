+++
title = "Compatibility Charter"
description = "BHC's commitment to Haskell compatibility"
template = "page.html"
+++

# Compatibility Charter

BHC is a compiler for Haskell. Our default goal is source compatibility.

## The Charter

- **Baseline:** Haskell 2010 compatibility mode (where supported by the current release)
- **Pragmatic compatibility:** selected GHC edition compatibility (GHC2021, GHC2024) for a documented subset of extensions
- **BHC innovations:** are opt-in via `BHC*` extensions or profile selection

If you avoid `BHC*` extensions, you should be able to move your code between BHC and GHC with minimal change (subject to documented implementation gaps).

## Important Statements

BHC does not define the Haskell standard. Haskell is a community language.

If an idea is generally useful, we'll try to upstream it to the broader Haskell ecosystem.

## Compatibility Levels

| Level | Meaning |
|-------|---------|
| ✅ | Compiles and matches semantics |
| 🟡 | Compiles with minor changes |
| 🟠 | Compiles but performance/RTS differs |
| ❌ | Not yet supported |

## Supported Haskell Baseline

| Feature | Status | Notes |
|---------|--------|-------|
| Haskell 2010 | ✅ | Core language support |
| Haskell 98 | ✅ | Via Haskell2010 mode |

## GHC Edition Compatibility

BHC supports a documented subset of GHC editions. Not all GHC extensions are supported.

### GHC2021 Subset

| Extension | Status |
|-----------|--------|
| BangPatterns | ✅ |
| BinaryLiterals | ✅ |
| ConstrainedClassMethods | ✅ |
| ConstraintKinds | ✅ |
| DeriveDataTypeable | ✅ |
| DeriveFoldable | ✅ |
| DeriveFunctor | ✅ |
| DeriveGeneric | ✅ |
| DeriveLift | 🟡 |
| DeriveTraversable | ✅ |
| EmptyCase | ✅ |
| EmptyDataDecls | ✅ |
| EmptyDataDeriving | ✅ |
| ExistentialQuantification | ✅ |
| ExplicitForAll | ✅ |
| FlexibleContexts | ✅ |
| FlexibleInstances | ✅ |
| GADTSyntax | ✅ |
| GADTs | ✅ |
| GeneralizedNewtypeDeriving | ✅ |
| InstanceSigs | ✅ |
| KindSignatures | ✅ |
| LambdaCase | ✅ |
| MultiParamTypeClasses | ✅ |
| NamedFieldPuns | ✅ |
| NumericUnderscores | ✅ |
| PolyKinds | 🟡 |
| RankNTypes | ✅ |
| RecordWildCards | ✅ |
| ScopedTypeVariables | ✅ |
| StandaloneDeriving | ✅ |
| TupleSections | ✅ |
| TypeApplications | ✅ |
| TypeOperators | ✅ |
| TypeSynonymInstances | ✅ |

### GHC2024 Subset

Includes GHC2021, plus:

| Extension | Status |
|-----------|--------|
| DataKinds | ✅ |
| DerivingStrategies | ✅ |
| DisambiguateRecordFields | ✅ |
| ExplicitNamespaces | ✅ |
| MonoLocalBinds | ✅ |
| RoleAnnotations | 🟡 |

## Known Gaps

The following are not yet supported or have significant differences:

| Feature | Status | Notes |
|---------|--------|-------|
| Template Haskell | ❌ | Planned for future release |
| Foreign Function Interface (advanced) | 🟠 | Basic FFI works, some edge cases differ |
| GHC Plugins | ❌ | Not planned for v1 |
| Backpack | ❌ | Not planned |
| Unsafe operations | 🟠 | Supported but may differ in RTS behavior |

## Conformance Testing

BHC maintains a conformance test suite tracking compatibility with Haskell 2010 and GHC editions.

Current status: **Alpha** — core language works, edge cases under development.

## Not a Fork

BHC is not a fork of the Haskell language. It's a new compiler implementation that:

- Targets compatibility with existing Haskell standards and common GHC editions
- Innovates in runtime and targets (which can differ between implementations)
- Provides opt-in extensions with clear namespacing (`BHC.*`)

If you never enable `BHC*` features, your code remains "ordinary Haskell" to the extent supported by the current BHC release.
