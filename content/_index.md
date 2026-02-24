+++
title = "BHC - Basel Haskell Compiler"
template = "landing.html"
+++

## Try It Now

Install BHC and compile your first program in 30 seconds.

```bash
# 1. Install BHC (or: brew install arcanist-sh/tap/bhc)
curl -fsSL https://bhc.arcanist.sh/install.sh | sh

# 2. Add to your PATH (or restart your terminal)
export PATH="$HOME/.bhc/bin:$PATH"
export LIBRARY_PATH="$HOME/.bhc/lib:$LIBRARY_PATH"

# 3. Create a Haskell file
echo 'main = putStrLn "Hello from BHC!"' > hello.hs

# 4. Compile and run
bhc hello.hs -o hello
./hello
```

Output: `Hello from BHC!`

BHC produces native executables via LLVM. No runtime interpreter, just fast native code.
