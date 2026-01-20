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
