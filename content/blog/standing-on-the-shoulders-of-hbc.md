+++
title = "Standing on the Shoulders of HBC"
description = "An email from Anthony Travers introduced us to HCT, his Haskell 98 translation of Lennart Augustsson's 1982 compiler. It gave BHC exactly the optimization roadmap we needed."
date = 2026-02-22
template = "blog-post.html"

[extra]
tag = "engineering"
+++

A few weeks ago, an email arrived from Anthony Travers. He'd found BHC in his search results and thought we might be interested in something he'd been working on: a translation of the Haskell B. Compiler's sources from Lazy ML to Haskell 98.

> Despite its "vintage", perhaps there's something in it which could still be of use to BHC.

The "something" turned out to be a 40,000-line compiler that Lennart Augustsson and Thomas Johnsson built at Chalmers University between 1982 and 1999. Anthony had spent years translating it from its original Lazy ML into readable Haskell 98, producing what he calls the Haskell Compiler Test, or HCT.

We downloaded it, read through it, and realized it contained exactly what BHC needed next.

## What HCT Is

HBC was the first Haskell compiler to generate native code. Before GHC existed, before the Haskell standard was even finalized, Augustsson and Johnsson were compiling a lazy functional language to machine code at Chalmers. The compiler was written in Lazy ML (LML), a strict ML dialect that Augustsson had also implemented.

Anthony's HCT translation preserves the full compiler in 558 Haskell source files. It's organized into modules that read like a textbook on functional language implementation:

```
src/
├── Simpl/          # Core simplifier
│   ├── Simpl.hs    # Reference-counting simplifier
│   └── Casetr.hs   # Case-of-case transformation
├── Strict/         # Strictness analysis
│   ├── Calcstrict.hs  # Boolean-tree demand analysis
│   └── Strict.hs      # Strictness data types
├── Transform/      # Program transformations
│   ├── Match.hs    # Pattern match compilation
│   └── Derived.hs  # Derived instances
└── ExprE/          # Expression transformations
    └── Classtrans.hs  # Dictionary method inlining
```

Each file carries the original copyright: "Copyright (c) 1982-1999 Lennart Augustsson, Thomas Johnsson." The license ends with: "Share and enjoy!"

## What We Found

BHC, at the time of Anthony's email, had 41 feature milestones completed. We could compile user-defined typeclasses, Data.Map, deriving, record syntax, monad transformers, and a JSON parser. But we had no optimizer. Every binding, every beta-redex, every `case Just 42 of { Nothing -> 0; Just x -> x }` was passed through to LLVM exactly as written.

This matters because LLVM is excellent at optimizing machine-level operations (register allocation, instruction scheduling, loop unrolling) but knows nothing about algebraic data types, closures, or thunks. When BHC generates `case (Just 42) of { Nothing -> 0; Just x -> x + 1 }`, LLVM sees a heap allocation, a tag check, and a branch. It cannot see that the scrutinee is a known constructor and the entire case expression should reduce to `43`.

HCT's source code showed us exactly what passes a functional language compiler needs, with working implementations that have been tested against real programs for over a decade.

Here's what we extracted.

### The Core Simplifier

HBC's simplifier (`Simpl/Simpl.hs`) is a single recursive traversal that applies multiple transformations in one pass, then iterates until nothing changes. It uses reference counting to make inlining decisions: if a binding is used exactly once, always substitute; if used multiple times, only substitute if the body is small.

The constant folding is charmingly direct:

```haskell
constfold d fno [e1, e2]
    | isoToFname fno Fadd = zlnEmkint (e1 + e2)
constfold d fno [e1, e2]
    | isoToFname fno Fsub = zlnEmkint (e1 - e2)
```

The case-of-case transformation in `Casetr.hs` is a single line of code that pushes an outer case expression into each alternative of an inner case:

```haskell
casetr (Ecase (Ecase e1 cl1 d1) cl2 d2) =
    let f' x = Ecase x cl2 d2
    in Ecase e1 (mapthd (pushcase f') cl1) (pushcase f' d1)
```

One line. It eliminates intermediate data constructors and collapses nested pattern matches. This is the kind of transformation that turns quadratic-looking code into linear traversals.

### Pattern Match Compilation

`Transform/Match.hs` implements the Augustsson/Sestoft algorithm for compiling pattern matches into decision trees. Rather than checking each equation top-to-bottom (which can examine the same constructor multiple times), it selects the most informative column, groups equations by constructor, and generates a single case dispatch.

BHC currently uses an equation-by-equation approach. The Augustsson algorithm would give us efficient decision trees, exhaustiveness warnings, and overlap detection. All from a proven implementation.

### Strictness Analysis

`Strict/Calcstrict.hs` implements boolean-tree demand analysis. For each function, it computes which arguments are always evaluated (strict) versus potentially unused (lazy). The result feeds into worker/wrapper transformations that unbox strict arguments, avoiding thunk allocation entirely.

This is critical for the Default profile, where BHC preserves Haskell's lazy semantics. Without strictness analysis, every function argument is wrapped in a thunk. With it, the compiler can prove that most arguments in practice are strict and skip the thunk.

### Dictionary Specialization

`ExprE/Classtrans.hs` inlines typeclass dictionary selections when the dictionary is statically known. Instead of allocating a dictionary tuple, extracting a method pointer, and calling through it, the compiler replaces the whole thing with a direct call to the concrete method.

BHC just landed user-defined typeclasses with dictionary passing in milestone E.39. The next step is exactly this: when we know at compile time which instance is being used, skip the dictionary entirely.

## What We Did With It

We didn't copy HCT's code into BHC. The languages are different (Haskell 98 vs Rust), the IR representations are different, and the overall architectures diverge significantly. What we did was study HCT's approach to each problem and write a comprehensive optimization specification for BHC, documenting the algorithms, the pass ordering, and the expected file structure.

The result is a roadmap that maps HCT's proven modules directly to planned BHC implementation files:

| HCT Module | BHC Equivalent | What It Does |
|------------|----------------|--------------|
| `Simpl/Simpl.hs` | `bhc-core/src/simplify.rs` | Reference-counting simplifier |
| `Simpl/Casetr.hs` | `bhc-core/src/simplify/case.rs` | Case transformations |
| `Transform/Match.hs` | `bhc-hir-to-core/src/pattern.rs` | Pattern match compilation |
| `Strict/Calcstrict.hs` | `bhc-core/src/demand.rs` | Boolean-tree demand analysis |
| `ExprE/Classtrans.hs` | `bhc-core/src/specialize.rs` | Dictionary method inlining |

The full specification covers the simplifier architecture (iterate-to-fixpoint with a cap of 10 iterations), inlining heuristics (size threshold of 20 AST nodes), the complete pass pipeline (simplify, demand analysis, worker/wrapper, specialize, simplify again), and dump flags for debugging each stage.

None of this is implemented yet. It's the roadmap for the next phase of BHC development.

## Why This Matters

There's a tendency in software to start from scratch and figure things out as you go. Sometimes that's the right call. But compiler optimization is a domain where forty years of research has produced proven techniques, and reimagining them from first principles would be both slow and likely inferior.

Augustsson's pattern match algorithm is from 1985. The boolean-tree approach to strictness analysis is from the same era. These techniques survived because they work. GHC's own optimizer, while significantly more sophisticated today, grew from similar roots.

What Anthony did with HCT was preserve these techniques in a form that's readable and studyable. The original HBC was written in Lazy ML, a language that essentially no one uses today. By translating to Haskell 98, Anthony made forty years of compiler engineering accessible to anyone who can read Haskell. That's a genuine contribution to the field.

And for BHC specifically, the timing was perfect. We'd spent a month building language features (from hello world through typeclasses and Data.Map), and we'd reached the point where the next bottleneck wasn't "what can we compile" but "how well does it run." Anthony's email arrived exactly when we needed to think about optimization, and HCT gave us a concrete, proven starting point.

## Thank You

Anthony's email was modest. He wasn't sure if his work would be useful. It was.

BHC's optimization pipeline will be built on the foundations that Augustsson and Johnsson laid at Chalmers in the 1980s, preserved and translated by Anthony Travers in the 2010s, and adapted for a Rust-based LLVM compiler in 2026. That's forty-four years of knowledge transfer across three programming languages and four decades of hardware evolution, connected by a BSD license and an email.

Open source works like this more often than people realize.

---

*The HCT source is available at [gitlab.haskell.org](https://gitlab.haskell.org/-/project/1/uploads/2cc126c8b6a5b51fb18dd56fec129f2f/hct-2018-05-02-PRE-ALPHA.src.tar.gz) under the BSD license. BHC is open source at [github.com/arcanist-sh/bhc](https://github.com/arcanist-sh/bhc).*
