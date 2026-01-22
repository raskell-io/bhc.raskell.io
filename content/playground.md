+++
title = "Playground"
description = "Try BHC in your browser"
template = "playground.html"
+++

## About the Playground

This playground runs a real BHC interpreter compiled to WebAssembly. Your code is parsed, type-checked, and evaluated entirely in your browser — no server round-trips.

### What's Supported

The playground supports a subset of Haskell:

- **Literals**: integers, floats, characters, strings
- **Arithmetic**: `+`, `-`, `*`, `/`, `mod`
- **Comparison**: `==`, `/=`, `<`, `>`, `<=`, `>=`
- **Boolean**: `&&`, `||`, `not`
- **Conditionals**: `if ... then ... else ...`
- **Let expressions**: `let x = ... in ...`
- **Lists**: `[1, 2, 3]`, list operations
- **Functions**: lambda expressions, recursion

### Profiles

- **Default (Lazy)**: Standard Haskell lazy evaluation
- **Numeric (Strict)**: Strict evaluation for numeric workloads

### Keyboard Shortcuts

- **Ctrl/Cmd + Enter**: Run code

### Limitations

This is an interpreter, not a full compiler. Some features are not yet supported:

- Type classes (no `Show`, `Eq` instances)
- Pattern matching (limited)
- Custom data types
- IO operations (except `main` result display)
- Imports and modules

For full BHC capabilities, [install the compiler](/get-started/).
