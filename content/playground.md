+++
title = "Playground"
description = "Try BHC in your browser"
template = "playground.html"
+++

## About the Playground

This playground runs a real BHC interpreter compiled to WebAssembly. Your code is parsed, type-checked, and evaluated entirely in your browser — no server round-trips.

### What's Supported

The playground supports a substantial subset of Haskell:

#### Expressions
- **Literals**: integers, floats, characters, strings
- **Arithmetic**: `+`, `-`, `*`, `/`, `mod`
- **Comparison**: `==`, `/=`, `<`, `>`, `<=`, `>=`
- **Boolean**: `&&`, `||`, `not`
- **Conditionals**: `if ... then ... else ...`
- **Let expressions**: `let x = ... in ...`
- **Lists**: `[1, 2, 3]`, `[1..10]`, list operations
- **Functions**: lambda expressions, recursion, composition

#### Module System
- **Module declarations**: `module Foo where`
- **Export lists**: `module Foo (bar, baz, (.)) where`
- **Import statements**: `import Data.List (sort, (++))`
- **Operator exports/imports**: `(.)`, `(!)`, `($)`, `(&)`, etc.
- **Haddock comments**: `-- | Documentation` in export lists

#### Syntax Features
- **Function definitions**: pattern matching, guards, where clauses
- **Type signatures**: `foo :: Int -> Int`
- **Operators as functions**: `(+)`, `(.)`, `(!)`
- **Sections**: `(+1)`, `(1+)`

### Examples

```haskell
-- Function composition
let double = (*2)
    addOne = (+1)
    doubleThenAdd = addOne . double
in doubleThenAdd 5
-- Result: 11
```

```haskell
-- List operations
let xs = [1..10]
in sum (filter even xs)
-- Result: 30
```

### Profiles

- **Default (Lazy)**: Standard Haskell lazy evaluation
- **Numeric (Strict)**: Strict evaluation for numeric workloads

### Keyboard Shortcuts

- **Ctrl/Cmd + Enter**: Run code

### Limitations

This is an interpreter, not a full compiler. Some features are not yet supported:

- Type classes (no `Show`, `Eq` instances)
- Custom data types (limited `data` support)
- IO operations (except `main` result display)
- Multi-module projects

For full BHC capabilities, [install the compiler](@/get-started.md).
