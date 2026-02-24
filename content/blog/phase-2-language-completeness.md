+++
title = "Phase 2: Language Completeness is Complete"
description = "BHC now compiles real Haskell programs with pattern matching, closures, higher-order functions, recursion, type classes, and lazy evaluation. Here's what Phase 2 delivers."
date = 2026-01-30
template = "blog-post.html"

[extra]
tag = "milestone"
+++

Phase 2 of BHC development is complete. The Basel Haskell Compiler now compiles and runs real Haskell programs — not just hello world, but programs with closures, higher-order functions, recursive pattern matching, type classes, and lazy evaluation.

Phase 1 proved the pipeline worked. Phase 2 proves the language works.

## What's New

Phase 2 adds the language features that separate a toy compiler from one that can run actual Haskell:

| Feature | What It Means |
|---------|---------------|
| Pattern matching | Full ADT matching, nested patterns, guards, wildcards, literal patterns |
| Closures | Lambda expressions capture free variables, allocated and invoked correctly |
| Thunks & laziness | Lazy evaluation via thunk creation, forcing, and indirection handling |
| Type classes | Instance resolution, dictionary passing, superclass constraints |
| Let/where bindings | Recursive and non-recursive local definitions with proper scoping |
| Recursion | Mutual recursion and tail call optimization |
| Prelude bootstrap | 26 type classes, 30+ list functions, 100+ FFI primitives |

## A Compiler Example

Here's a program that exercises most of Phase 2 in a single file — higher-order functions, lambdas, recursion, pattern matching, and let bindings:

```haskell
-- functions.hs
double x = x + x
square x = x * x
apply f x = f x

result = apply double (square 3)

main = print result
```

Compile and run:

```
$ bhc functions.hs -o functions
$ ./functions
18
```

`apply` takes a function as an argument. `square 3` evaluates to `9`. `double 9` evaluates to `18`. Closures, application, and higher-order dispatch all work.

Here's a recursive fibonacci that exercises pattern matching and recursive calls through the full LLVM pipeline:

```haskell
-- fibonacci.hs
fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)

main = print (fib 15)
```

```
$ bhc fibonacci.hs -o fib
$ ./fib
610
```

And lambda expressions work as first-class values:

```haskell
main = print ((\x -> x * 2) 21)
```

```
$ bhc run lambda.hs
42
```

## The Full Test Matrix

Every example compiles to a native executable via LLVM and produces the correct output:

| Program | Expression | Expected | Actual |
|---------|-----------|----------|--------|
| Hello World | `putStrLn "Hello, World!"` | `Hello, World!` | `Hello, World!` |
| Arithmetic | `1 + 2 * 3 + 10 - 4` | `13` | `13` |
| Let bindings | `let x=10; y=20; z=x+y in z` | `30` | `30` |
| Functions | `apply double (square 3)` | `18` | `18` |
| Factorial | `factorial 10` | `3628800` | `3628800` |
| Fibonacci | `fib 15` | `610` | `610` |
| Lambda | `(\x -> x * 2) 21` | `42` | `42` |
| Higher-order | `apply double 21` | `42` | `42` |
| Let + recursion | `fib 10 + fib 15` | `665` | `665` |
| Branching | `classify 42 + classify (-5) + classify 0` | `0` | `0` |

## What This Took

Phase 2 required changes across the compiler:

- **Pattern matching codegen** — Constructor dispatch, nested pattern decision trees, literal patterns, guards, and wildcards in `bhc-codegen` (~900 lines)
- **Closure implementation** — Free variable analysis, closure object layout (`fn_ptr + env`), allocation and invocation (~950 lines)
- **Thunk and laziness support** — Thunk creation, forcing via `bhc_force()`, tag checking, and indirection handling in both the codegen and RTS
- **Type class infrastructure** — Instance resolution, dictionary construction, superclass propagation, and `$sel_N` field selectors for dictionary passing
- **Let binding codegen** — Recursive let (letrec lifted to top-level functions) and non-recursive let with proper scoping
- **Prelude bootstrap** — Eq, Ord, Num, Functor, Monad, Show, and 20 more type classes, plus 100+ FFI primitives for numeric operations

The exit criterion for Phase 2 was straightforward: compile and run recursive fibonacci correctly. That's been working since commit `745bbac`.

## What's Next

Phase 3 targets the numeric profile — BHC's differentiator. This means:

- **Tensor IR** with shape and stride tracking
- **Guaranteed fusion** for `map`/`map`, `zipWith`/`map`, `sum`/`map`, and `foldl'`/`map` compositions
- **SIMD vectorization** for numeric hot loops
- **Hot arena allocation** for kernel temporaries
- **Kernel fusion reports** so you can verify what the compiler did

The goal: `sum (map (*2) [1..1000000])` fuses to a single loop with no intermediate allocation.

## Try It

Build from source:

```bash
git clone https://github.com/arcanist-sh/bhc
cd bhc
cargo build --release
```

Write a program and compile it:

```haskell
-- demo.hs
double x = x + x
apply f x = f x
main = print (apply double 21)
```

```bash
bhc demo.hs -o demo
./demo
```

Or use `bhc run` to compile and execute in one step:

```bash
bhc run demo.hs
```

---

*The BHC Team*
