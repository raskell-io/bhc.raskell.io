+++
title = "BHC - Basel Haskell Compiler"
template = "landing.html"
+++

## Quick Start

Install BHC and compile your existing Haskell project.

```bash
# Install BHC
curl -fsSL https://bhc.raskell.io/install.sh | sh

# Compile in Haskell 2010 mode
bhc --edition=Haskell2010 Main.hs

# Or use GHC2024 compatibility
bhc --edition=GHC2024 Main.hs

# Use numeric profile for performance-critical code
bhc --profile=numeric Main.hs
```

BHC aims to compile existing Haskell code with minimal friction. Innovations are opt-in.

---

## FAQ

**Is BHC a new language?**

No. BHC is a compiler for Haskell. It targets Haskell compatibility by default, and offers opt-in BHC extensions and runtime profiles.

**Will this split the community?**

The goal is the opposite. BHC tries to broaden deployment options and improve performance while staying compatible and upstreaming mature ideas.

**What is BHC2026?**

An opt-in bundle of BHC-specific extensions and profiles. It's not an official Haskell standard; it's an implementation-defined bundle, like a curated mode.

**Can I write portable code?**

Yes. Avoid `BHC*` extensions and stick to supported compatibility modes.

**Why a new runtime?**

Runtime systems can differ between implementations even when the surface language is the same. BHC RTS focuses on predictability, observability, and profile-driven performance.
