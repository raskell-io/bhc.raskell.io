+++
title = "Compiler Architecture"
description = "Deep dive into BHC's internal architecture and compilation pipeline"
template = "page.html"
+++

# Compiler Architecture

BHC is a multi-stage optimizing compiler written in Rust. This page documents the internal architecture, intermediate representations, and compilation pipeline.

## High-Level Pipeline

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         BHC COMPILATION PIPELINE                            │
└─────────────────────────────────────────────────────────────────────────────┘

  Source Files (.hs)
        │
        ▼
┌───────────────────┐
│     FRONTEND      │  Parsing, name resolution, type inference
└───────────────────┘
        │
        ▼
┌───────────────────┐
│     CORE IR       │  Desugared, typed lambda calculus
└───────────────────┘
        │
        ▼
┌───────────────────┐
│   OPTIMIZATION    │  Inlining, fusion, strictness analysis
└───────────────────┘
        │
        ├──────────────────┬──────────────────┐
        ▼                  ▼                  ▼
┌───────────────┐  ┌───────────────┐  ┌───────────────┐
│   TENSOR IR   │  │    STG IR     │  │   DIRECT IR   │
│  (numeric)    │  │  (default)    │  │   (edge)      │
└───────────────┘  └───────────────┘  └───────────────┘
        │                  │                  │
        ▼                  ▼                  ▼
┌───────────────┐  ┌───────────────┐  ┌───────────────┐
│ LLVM / CUDA   │  │  LLVM / C     │  │  WASM / LLVM  │
│   CODEGEN     │  │   CODEGEN     │  │   CODEGEN     │
└───────────────┘  └───────────────┘  └───────────────┘
        │                  │                  │
        └──────────────────┴──────────────────┘
                           │
                           ▼
                    ┌─────────────┐
                    │   LINKER    │
                    └─────────────┘
                           │
                           ▼
                      Executable
```

## Frontend Architecture

The frontend transforms source code into typed Core IR through several phases.

### Lexer and Parser

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              FRONTEND DETAIL                                │
└─────────────────────────────────────────────────────────────────────────────┘

   Source Text
        │
        ▼
┌───────────────────────────────────────────────────────────────────────────┐
│                               LEXER                                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐      │
│  │   Layout    │  │  Keywords   │  │  Operators  │  │  Literals   │      │
│  │   Engine    │──│  & Idents   │──│  & Symbols  │──│  & Strings  │      │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘      │
│         │                                                                  │
│         ▼                                                                  │
│  ┌─────────────────────────────────────────────────────────────────┐      │
│  │  Token Stream with Layout-Inserted Braces and Semicolons        │      │
│  └─────────────────────────────────────────────────────────────────┘      │
└───────────────────────────────────────────────────────────────────────────┘
        │
        ▼
┌───────────────────────────────────────────────────────────────────────────┐
│                               PARSER                                       │
│                                                                            │
│   ┌─────────────────────────────────────────────────────────────────┐     │
│   │                    Recursive Descent Parser                      │     │
│   │  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐        │     │
│   │  │  Module  │  │  Decls   │  │  Exprs   │  │ Patterns │        │     │
│   │  │  Header  │──│  Parser  │──│  Parser  │──│  Parser  │        │     │
│   │  └──────────┘  └──────────┘  └──────────┘  └──────────┘        │     │
│   └─────────────────────────────────────────────────────────────────┘     │
│         │                                                                  │
│         ▼                                                                  │
│   ┌─────────────────────────────────────────────────────────────────┐     │
│   │  Concrete Syntax Tree (CST) - preserves all source structure     │     │
│   └─────────────────────────────────────────────────────────────────┘     │
└───────────────────────────────────────────────────────────────────────────┘
        │
        ▼
   Abstract Syntax Tree (AST)
```

### Name Resolution

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           NAME RESOLUTION                                   │
└─────────────────────────────────────────────────────────────────────────────┘

                              AST (unresolved names)
                                       │
          ┌────────────────────────────┼────────────────────────────┐
          ▼                            ▼                            ▼
   ┌─────────────┐              ┌─────────────┐              ┌─────────────┐
   │   Import    │              │   Scope     │              │  Fixity     │
   │  Resolver   │              │  Builder    │              │  Resolver   │
   │             │              │             │              │             │
   │ - Find      │              │ - Build     │              │ - Resolve   │
   │   modules   │              │   lexical   │              │   operator  │
   │ - Check     │              │   scopes    │              │   prece-    │
   │   exports   │              │ - Track     │              │   dence     │
   │ - Resolve   │              │   bindings  │              │ - Rewrite   │
   │   qualif.   │              │ - Check     │              │   infix     │
   │   names     │              │   shadows   │              │   exprs     │
   └─────────────┘              └─────────────┘              └─────────────┘
          │                            │                            │
          └────────────────────────────┼────────────────────────────┘
                                       ▼
                           ┌───────────────────┐
                           │  Resolved AST     │
                           │  (all names have  │
                           │  unique IDs)      │
                           └───────────────────┘
```

### Type Inference

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                            TYPE INFERENCE                                   │
└─────────────────────────────────────────────────────────────────────────────┘

                           Resolved AST
                                │
                                ▼
┌───────────────────────────────────────────────────────────────────────────┐
│                        CONSTRAINT GENERATION                               │
│                                                                            │
│    For each expression, generate typing constraints:                       │
│                                                                            │
│    Expression          Generated Constraint                                │
│    ──────────          ─────────────────────                               │
│    x                   x : τ_x  (lookup)                                   │
│    \x -> e             fresh α, check e : β, result α → β                  │
│    f x                 f : α → β, x : α, result β                          │
│    let x = e1 in e2    generalize(check e1), check e2 with x bound         │
│    if c then t else f  c : Bool, t : τ, f : τ, result τ                    │
│                                                                            │
└───────────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
┌───────────────────────────────────────────────────────────────────────────┐
│                         CONSTRAINT SOLVING                                 │
│                                                                            │
│   ┌────────────────────────────────────────────────────────────────┐      │
│   │                    Unification Engine                           │      │
│   │                                                                 │      │
│   │   τ₁ ~ τ₂                                                       │      │
│   │    │                                                            │      │
│   │    ├── Both type vars? ──────────────► Union-find merge         │      │
│   │    │                                                            │      │
│   │    ├── One type var? ────────────────► Occurs check, then bind  │      │
│   │    │                                                            │      │
│   │    ├── Both constructors? ───────────► Recurse on arguments     │      │
│   │    │                                                            │      │
│   │    └── Mismatch? ────────────────────► Type error               │      │
│   │                                                                 │      │
│   └────────────────────────────────────────────────────────────────┘      │
│                                                                            │
│   ┌────────────────────────────────────────────────────────────────┐      │
│   │                   Instance Resolution                           │      │
│   │                                                                 │      │
│   │   For constraint (C τ):                                         │      │
│   │    1. Find instances matching τ                                 │      │
│   │    2. Check instance context satisfied                          │      │
│   │    3. Generate evidence (dictionary passing)                    │      │
│   │                                                                 │      │
│   └────────────────────────────────────────────────────────────────┘      │
└───────────────────────────────────────────────────────────────────────────┘
                                │
                                ▼
                    Typed AST + Evidence
```

## Core IR

Core is BHC's primary intermediate representation: a small, explicitly-typed functional language.

### Core Language

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              CORE IR GRAMMAR                                │
└─────────────────────────────────────────────────────────────────────────────┘

  Expression e ::=
      │
      ├── x                         Variable reference
      │
      ├── K                         Data constructor
      │
      ├── lit                       Literal (Int#, Float#, String, ...)
      │
      ├── e₁ e₂                     Application
      │
      ├── e @τ                      Type application
      │
      ├── λx:τ. e                   Lambda abstraction
      │
      ├── Λα:κ. e                   Type abstraction
      │
      ├── let x:τ = e₁ in e₂        Non-recursive let
      │
      ├── letrec binds in e         Recursive let (mutually recursive)
      │
      ├── case e of alts            Case expression (pattern matching)
      │
      └── e ▷ τ                     Type cast (for newtypes, coercions)


  Alternatives alt ::=
      │
      ├── K x₁ ... xₙ → e           Constructor pattern
      │
      ├── lit → e                   Literal pattern
      │
      └── _ → e                     Default pattern


  Type τ ::=
      │
      ├── α                         Type variable
      │
      ├── T                         Type constructor
      │
      ├── τ₁ τ₂                     Type application
      │
      ├── τ₁ → τ₂                   Function type
      │
      └── ∀α:κ. τ                   Universal quantification
```

### Desugaring Examples

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          DESUGARING TO CORE                                 │
└─────────────────────────────────────────────────────────────────────────────┘

  Source Haskell                      Core IR
  ──────────────                      ───────

  do { x <- m; e }          ═══►      m >>= \x -> e

  [x | x <- xs, p x]        ═══►      concatMap (\x -> if p x
                                                       then [x]
                                                       else []) xs

  f x y = e                 ═══►      f = \x -> \y -> e
  where g = ...                       where g = ...

  data Maybe a =            ═══►      data Maybe a = Nothing | Just a
    Nothing | Just a                  -- generates:
                                      --   Nothing : ∀a. Maybe a
                                      --   Just    : ∀a. a → Maybe a

  class Eq a where          ═══►      data Eq a = MkEq {
    (==) :: a -> a -> Bool              eq :: a -> a -> Bool
                                      }
                                      -- (==) becomes dictionary selector

  instance Eq Int where     ═══►      eqInt :: Eq Int
    x == y = primEqInt x y            eqInt = MkEq { eq = primEqInt }
```

## Optimization Pipeline

### Core-to-Core Transformations

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        OPTIMIZATION PIPELINE                                │
└─────────────────────────────────────────────────────────────────────────────┘

                              Core IR (from frontend)
                                       │
    ┌──────────────────────────────────┼──────────────────────────────────┐
    │                                  │                                   │
    │  ┌───────────────────────────────┴───────────────────────────────┐  │
    │  │                        SIMPLIFIER                              │  │
    │  │                                                                │  │
    │  │   Iterates until fixed point:                                  │  │
    │  │                                                                │  │
    │  │   ┌──────────────┐  ┌──────────────┐  ┌──────────────┐        │  │
    │  │   │   Inlining   │  │    Beta      │  │    Dead      │        │  │
    │  │   │              │──│  Reduction   │──│    Code      │        │  │
    │  │   │  - Small     │  │              │  │  Elimination │        │  │
    │  │   │    functions │  │  (λx.e) v    │  │              │        │  │
    │  │   │  - Once-used │  │    ═► e[v/x] │  │  Remove      │        │  │
    │  │   │  - CONLIKE   │  │              │  │  unused      │        │  │
    │  │   └──────────────┘  └──────────────┘  └──────────────┘        │  │
    │  │          │                  │                  │               │  │
    │  │          ▼                  ▼                  ▼               │  │
    │  │   ┌──────────────┐  ┌──────────────┐  ┌──────────────┐        │  │
    │  │   │    Case      │  │   Constant   │  │    Let       │        │  │
    │  │   │  of Known    │──│   Folding    │──│   Floating   │        │  │
    │  │   │ Constructor  │  │              │  │              │        │  │
    │  │   │              │  │  2 + 3 ═► 5  │  │  Float lets  │        │  │
    │  │   │ case K x of  │  │  not True    │  │  out of      │        │  │
    │  │   │   K y -> e   │  │    ═► False  │  │  lambdas     │        │  │
    │  │   │   ═► e[x/y]  │  │              │  │              │        │  │
    │  │   └──────────────┘  └──────────────┘  └──────────────┘        │  │
    │  │                                                                │  │
    │  └────────────────────────────────────────────────────────────────┘  │
    │                                  │                                   │
    │                                  ▼                                   │
    │  ┌────────────────────────────────────────────────────────────────┐  │
    │  │                    SPECIALIZATION                               │  │
    │  │                                                                 │  │
    │  │   Polymorphic function           Specialized versions           │  │
    │  │   ─────────────────────           ────────────────────          │  │
    │  │   map :: (a→b) → [a] → [b]  ═══►  map_Int_Bool :: ...           │  │
    │  │                                   map_Char_Char :: ...          │  │
    │  │                                                                 │  │
    │  │   Creates monomorphic copies at known call sites                │  │
    │  │   Enables further optimization (unboxing, fusion)               │  │
    │  │                                                                 │  │
    │  └────────────────────────────────────────────────────────────────┘  │
    │                                  │                                   │
    │                                  ▼                                   │
    │  ┌────────────────────────────────────────────────────────────────┐  │
    │  │                   STRICTNESS ANALYSIS                           │  │
    │  │                                                                 │  │
    │  │   Determines which arguments are always evaluated:              │  │
    │  │                                                                 │  │
    │  │   f x y = case x of           f is strict in x (always eval'd) │  │
    │  │             0 -> y            f is lazy in y (not always)      │  │
    │  │             _ -> x + y                                          │  │
    │  │                                                                 │  │
    │  │   Strict arguments can be:                                      │  │
    │  │   - Evaluated before call (worker/wrapper)                      │  │
    │  │   - Passed unboxed                                              │  │
    │  │   - Allocated on stack instead of heap                          │  │
    │  │                                                                 │  │
    │  └────────────────────────────────────────────────────────────────┘  │
    │                                  │                                   │
    │                                  ▼                                   │
    │  ┌────────────────────────────────────────────────────────────────┐  │
    │  │                   WORKER/WRAPPER                                │  │
    │  │                                                                 │  │
    │  │   Original:                    After W/W:                       │  │
    │  │   ─────────                    ──────────                       │  │
    │  │   f :: Int → Int → Int         f :: Int → Int → Int             │  │
    │  │   f x y = x + y                f x y = case x of                │  │
    │  │                                          I# x' → case y of      │  │
    │  │                                                   I# y' →       │  │
    │  │                                                     I# (f' x' y')│  │
    │  │                                                                 │  │
    │  │                                f' :: Int# → Int# → Int#         │  │
    │  │                                f' x y = x +# y  -- unboxed!     │  │
    │  │                                                                 │  │
    │  └────────────────────────────────────────────────────────────────┘  │
    │                                  │                                   │
    └──────────────────────────────────┼──────────────────────────────────┘
                                       │
                                       ▼
                              Optimized Core IR
```

### Fusion System

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                             FUSION SYSTEM                                   │
└─────────────────────────────────────────────────────────────────────────────┘

  BHC implements stream fusion with guaranteed fusion for standard patterns.

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                        STREAM REPRESENTATION                             │
  │                                                                          │
  │   data Stream a = ∃s. Stream (s → Step a s) s                           │
  │                                                                          │
  │   data Step a s = Yield a s    -- produce element, continue             │
  │                 | Skip s       -- no element, continue                  │
  │                 | Done         -- finished                              │
  │                                                                          │
  │   toStream   :: [a] → Stream a      -- list to stream                   │
  │   fromStream :: Stream a → [a]      -- stream to list                   │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                         FUSION RULES                                     │
  │                                                                          │
  │   Rule: stream/unstream                                                  │
  │   ─────────────────────                                                  │
  │   fromStream (toStream xs)  ═══►  xs                                    │
  │                                                                          │
  │   This rule eliminates intermediate structures:                          │
  │                                                                          │
  │   map f (map g xs)                                                       │
  │     = fromStream (mapS f (toStream (fromStream (mapS g (toStream xs))))) │
  │     ═══► [stream/unstream]                                               │
  │     = fromStream (mapS f (mapS g (toStream xs)))                         │
  │     ═══► [mapS/mapS fusion]                                              │
  │     = fromStream (mapS (f . g) (toStream xs))                            │
  │     = map (f . g) xs                                                     │
  │                                                                          │
  │   Result: single traversal, no intermediate list!                        │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                    GUARANTEED FUSION PATTERNS                            │
  │                                                                          │
  │   BHC guarantees fusion for these patterns (failure = compiler bug):    │
  │                                                                          │
  │   ✓  map f . map g           ═══►  map (f . g)                          │
  │   ✓  filter p . filter q     ═══►  filter (\x → p x && q x)             │
  │   ✓  map f . filter p        ═══►  single pass                          │
  │   ✓  sum . map f             ═══►  foldl' (\a x → a + f x) 0            │
  │   ✓  foldr f z . map g       ═══►  foldr (\x a → f (g x) a) z           │
  │   ✓  zip xs . map f          ═══►  single pass over xs                  │
  │                                                                          │
  │   The numeric profile extends this to tensor operations.                 │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘
```

## Backend Architecture

### STG Machine (Default Profile)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        STG MACHINE (DEFAULT PROFILE)                        │
└─────────────────────────────────────────────────────────────────────────────┘

  The Spineless Tagless G-machine is BHC's evaluation model for lazy code.

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                          STG SYNTAX                                      │
  │                                                                          │
  │   Binding    b ::= f = \π x₁...xₙ → e                                   │
  │                    where π ∈ {\n, \u}  (updatable / non-updatable)      │
  │                                                                          │
  │   Expression e ::= let b in e                                           │
  │                  | letrec b₁...bₙ in e                                   │
  │                  | case e of alts                                        │
  │                  | f a₁...aₙ           (saturated application)          │
  │                  | C a₁...aₙ           (saturated constructor)          │
  │                  | prim a₁...aₙ        (primitive operation)            │
  │                                                                          │
  │   Atom       a ::= x | lit                                              │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                       HEAP OBJECT LAYOUT                                 │
  │                                                                          │
  │   Every heap object has this structure:                                  │
  │                                                                          │
  │   ┌────────────────┬────────────────────────────────────────┐           │
  │   │  Info Pointer  │  Payload (free variables / fields)     │           │
  │   └────────────────┴────────────────────────────────────────┘           │
  │          │                                                               │
  │          ▼                                                               │
  │   ┌──────────────────────────────────────────────────────────┐          │
  │   │                    Info Table                             │          │
  │   │  ┌──────────┬──────────┬──────────┬──────────┐           │          │
  │   │  │  Entry   │  Layout  │  Closure │   SRT    │           │          │
  │   │  │  Code    │  Info    │   Type   │  (GC)    │           │          │
  │   │  └──────────┴──────────┴──────────┴──────────┘           │          │
  │   └──────────────────────────────────────────────────────────┘          │
  │                                                                          │
  │   Closure Types:                                                         │
  │   ─────────────────                                                      │
  │   THUNK     - Unevaluated expression (updatable)                        │
  │   FUN       - Function closure                                          │
  │   PAP       - Partial application                                       │
  │   CONSTR    - Data constructor                                          │
  │   BLACKHOLE - Thunk being evaluated (cycle detection)                   │
  │   IND       - Indirection (after update)                                │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                        EVALUATION MODEL                                  │
  │                                                                          │
  │   Registers:                                                             │
  │   ──────────                                                             │
  │   Sp  - Stack pointer                                                    │
  │   Hp  - Heap pointer                                                     │
  │   R1  - Node register (current closure)                                  │
  │   R2+ - Argument registers                                               │
  │                                                                          │
  │   Evaluation of (case e of alts):                                        │
  │   ──────────────────────────────────                                     │
  │                                                                          │
  │   1. Push continuation (return address + saved registers)                │
  │   2. Enter e (jump to its entry code)                                    │
  │   3. If e is:                                                            │
  │      - THUNK: evaluate, update with result, return                       │
  │      - FUN:   error (case on function)                                   │
  │      - CONSTR: return immediately, match alts                            │
  │   4. Pop continuation, select matching alternative                       │
  │   5. Bind pattern variables, continue                                    │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘
```

### Tensor IR (Numeric Profile)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                       TENSOR IR (NUMERIC PROFILE)                           │
└─────────────────────────────────────────────────────────────────────────────┘

  The numeric profile lowers array operations to Tensor IR for optimization.

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                       TENSOR IR OPERATIONS                               │
  │                                                                          │
  │   Tensor τ[d₁,...,dₙ]  -- n-dimensional array of element type τ         │
  │                                                                          │
  │   Operations:                                                            │
  │   ───────────                                                            │
  │   alloc    : dims → Tensor τ dims                                       │
  │   index    : Tensor τ dims → indices → τ                                │
  │   store    : Tensor τ dims → indices → τ → ()                           │
  │   map      : (τ → σ) → Tensor τ d → Tensor σ d                          │
  │   reduce   : (τ → τ → τ) → τ → Tensor τ d → Tensor τ d'                 │
  │   contract : Tensor τ d₁ → Tensor τ d₂ → axes → Tensor τ d₃             │
  │   reshape  : Tensor τ d₁ → shape → Tensor τ d₂                          │
  │   slice    : Tensor τ d → range → Tensor τ d'                           │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                    TENSOR FUSION EXAMPLE                                 │
  │                                                                          │
  │   Source:                                                                │
  │   ───────                                                                │
  │   normalize :: Tensor Float [n] → Tensor Float [n]                      │
  │   normalize t = map (/ norm) t                                          │
  │     where norm = sqrt (reduce (+) 0 (map (^2) t))                       │
  │                                                                          │
  │   Naive execution: 3 passes over data                                    │
  │                                                                          │
  │   After fusion:                                                          │
  │   ─────────────                                                          │
  │   normalize t =                                                          │
  │     let sumSq = 0.0                                                      │
  │     for i in 0..n:                                                       │
  │       sumSq += t[i] * t[i]        -- fused: square and sum              │
  │     let norm = sqrt sumSq                                                │
  │     for i in 0..n:                                                       │
  │       result[i] = t[i] / norm     -- single output pass                 │
  │                                                                          │
  │   Result: 2 passes (theoretical minimum: need sum before divide)         │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                      SIMD LOWERING                                       │
  │                                                                          │
  │   Tensor IR lowers to SIMD instructions where profitable:               │
  │                                                                          │
  │   map (+1) (xs :: Tensor Float [1024])                                  │
  │                                                                          │
  │   ═══► (on x86-64 with AVX2)                                            │
  │                                                                          │
  │   for i in 0..1024 step 8:                                               │
  │     v = vload256 xs[i:i+8]        -- load 8 floats                      │
  │     v = vaddps v, ones            -- parallel add                       │
  │     vstore256 result[i:i+8], v    -- store 8 floats                     │
  │                                                                          │
  │   Speedup: ~8x for pure compute (memory-bound less)                      │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘
```

### Code Generation

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          CODE GENERATION                                    │
└─────────────────────────────────────────────────────────────────────────────┘

                              Optimized IR
                                   │
           ┌───────────────────────┼───────────────────────┐
           │                       │                       │
           ▼                       ▼                       ▼
    ┌─────────────┐         ┌─────────────┐         ┌─────────────┐
    │    LLVM     │         │    WASM     │         │    CUDA     │
    │   Backend   │         │   Backend   │         │   Backend   │
    └─────────────┘         └─────────────┘         └─────────────┘
           │                       │                       │
           ▼                       ▼                       ▼
    ┌─────────────┐         ┌─────────────┐         ┌─────────────┐
    │  LLVM IR    │         │   WASM      │         │    PTX      │
    │             │         │   Binary    │         │  (NVIDIA)   │
    │  - SSA form │         │             │         │             │
    │  - Typed    │         │  - Stack    │         │  - GPU      │
    │  - Portable │         │    machine  │         │    threads  │
    │             │         │  - Sandboxed│         │  - SIMT     │
    └─────────────┘         └─────────────┘         └─────────────┘
           │                       │                       │
           ▼                       ▼                       │
    ┌─────────────┐         ┌─────────────┐               │
    │   LLVM      │         │   WASM      │               │
    │  Optimizer  │         │  Optimizer  │               │
    │             │         │   (wasm-opt)│               │
    │  - DCE      │         │             │               │
    │  - Inlining │         │  - Dead     │               │
    │  - Loop opt │         │    code     │               │
    │  - Vectorize│         │  - Inline   │               │
    └─────────────┘         └─────────────┘               │
           │                       │                       │
           ▼                       │                       │
    ┌─────────────┐                │                       │
    │   Native    │                │                       │
    │   Codegen   │                │                       │
    │             │                │                       │
    │  - x86-64   │                │                       │
    │  - ARM64    │                │                       │
    │  - RISC-V   │                │                       │
    └─────────────┘                │                       │
           │                       │                       │
           └───────────────────────┴───────────────────────┘
                                   │
                                   ▼
                            ┌─────────────┐
                            │   Linker    │
                            │             │
                            │  - RTS      │
                            │  - GC       │
                            │  - Stdlib   │
                            └─────────────┘
                                   │
                                   ▼
                             Executable
```

## Runtime System

### Memory Management

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         RUNTIME SYSTEM                                      │
└─────────────────────────────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                      HEAP LAYOUT                                         │
  │                                                                          │
  │   ┌─────────────────────────────────────────────────────────────────┐   │
  │   │                        HEAP                                      │   │
  │   │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐           │   │
  │   │  │  Nursery     │  │   Old Gen    │  │   Large      │           │   │
  │   │  │  (Gen 0)     │  │   (Gen 1+)   │  │   Object     │           │   │
  │   │  │              │  │              │  │   Space      │           │   │
  │   │  │  - Small     │  │  - Survived  │  │              │           │   │
  │   │  │    allocs    │  │    objects   │  │  - Arrays    │           │   │
  │   │  │  - Bump      │  │  - Compacted │  │  - Pinned    │           │   │
  │   │  │    pointer   │  │    or aging  │  │  - FFI       │           │   │
  │   │  │  - Frequent  │  │  - Less      │  │              │           │   │
  │   │  │    GC        │  │    frequent  │  │              │           │   │
  │   │  │              │  │    GC        │  │              │           │   │
  │   │  └──────────────┘  └──────────────┘  └──────────────┘           │   │
  │   └─────────────────────────────────────────────────────────────────┘   │
  │                                                                          │
  │   Allocation (nursery):                                                  │
  │   ─────────────────────                                                  │
  │   alloc n =                                                              │
  │     if Hp + n > HpLim:                                                   │
  │       gc()              -- trigger garbage collection                    │
  │     ptr = Hp                                                             │
  │     Hp += n             -- bump allocation (very fast)                   │
  │     return ptr                                                           │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                    GARBAGE COLLECTOR                                     │
  │                                                                          │
  │   BHC uses a generational, copying collector with optional compaction.   │
  │                                                                          │
  │   Minor GC (nursery only):                                               │
  │   ────────────────────────                                               │
  │   1. Stop mutator threads                                                │
  │   2. Scan roots (stack, registers, globals)                              │
  │   3. Copy live nursery objects to old gen                                │
  │   4. Update pointers                                                     │
  │   5. Reset nursery                                                       │
  │   6. Resume                                                              │
  │                                                                          │
  │   Major GC (all generations):                                            │
  │   ───────────────────────────                                            │
  │   1. Stop all threads                                                    │
  │   2. Mark all reachable objects                                          │
  │   3. Compact (slide) live objects                                        │
  │   4. Update all pointers                                                 │
  │   5. Resume                                                              │
  │                                                                          │
  │   Server profile: concurrent marking, incremental compaction             │
  │   Edge profile: simple two-space copying (smaller runtime)               │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘
```

### Thread Scheduler

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                       THREAD SCHEDULER                                      │
└─────────────────────────────────────────────────────────────────────────────┘

  BHC uses M:N threading (many Haskell threads on few OS threads).

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                                                                          │
  │     Haskell Threads (lightweight, millions possible)                     │
  │                                                                          │
  │        T₁    T₂    T₃    T₄    T₅    T₆    T₇    T₈    ...              │
  │         │     │     │     │     │     │     │     │                      │
  │         └──┬──┴──┬──┴──┬──┴──┬──┴──┬──┴──┬──┴──┬──┘                      │
  │            │     │     │     │     │     │                               │
  │            ▼     ▼     ▼     ▼     ▼     ▼                               │
  │     ┌──────────────────────────────────────────────────┐                 │
  │     │              SCHEDULER                            │                 │
  │     │                                                   │                 │
  │     │   ┌─────────────────────────────────────────┐    │                 │
  │     │   │           Run Queues                     │    │                 │
  │     │   │   ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐       │    │                 │
  │     │   │   │ CPU │ │ CPU │ │ CPU │ │ CPU │ ...   │    │                 │
  │     │   │   │  0  │ │  1  │ │  2  │ │  3  │       │    │                 │
  │     │   │   └─────┘ └─────┘ └─────┘ └─────┘       │    │                 │
  │     │   └─────────────────────────────────────────┘    │                 │
  │     │                                                   │                 │
  │     │   Work stealing: idle CPUs steal from busy ones   │                 │
  │     │                                                   │                 │
  │     └──────────────────────────────────────────────────┘                 │
  │                        │     │     │     │                               │
  │                        ▼     ▼     ▼     ▼                               │
  │                                                                          │
  │     OS Threads (Capabilities) - one per CPU core                         │
  │                                                                          │
  │                       C₀    C₁    C₂    C₃                               │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                   THREAD STATE MACHINE                                   │
  │                                                                          │
  │                        ┌─────────┐                                       │
  │           forkIO ────► │ RUNNABLE│ ◄──── wakeup                         │
  │                        └────┬────┘                                       │
  │                             │                                            │
  │                             │ scheduled                                  │
  │                             ▼                                            │
  │                        ┌─────────┐                                       │
  │                        │ RUNNING │                                       │
  │                        └────┬────┘                                       │
  │                             │                                            │
  │           ┌─────────────────┼─────────────────┐                          │
  │           │                 │                 │                          │
  │           ▼                 ▼                 ▼                          │
  │      ┌─────────┐       ┌─────────┐       ┌─────────┐                     │
  │      │ BLOCKED │       │ FINISHED│       │  KILLED │                     │
  │      │ (MVar,  │       │         │       │         │                     │
  │      │  STM,   │       └─────────┘       └─────────┘                     │
  │      │  I/O)   │                                                         │
  │      └─────────┘                                                         │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘
```

## Module System

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          MODULE SYSTEM                                      │
└─────────────────────────────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                    COMPILATION UNIT STRUCTURE                            │
  │                                                                          │
  │   Source Module (.hs)                                                    │
  │         │                                                                │
  │         ▼                                                                │
  │   ┌─────────────────────────────────────────────────────────────────┐   │
  │   │                     Interface File (.bhi)                        │   │
  │   │                                                                  │   │
  │   │   - Module name and dependencies                                 │   │
  │   │   - Exported declarations (types, values, classes)               │   │
  │   │   - Inlinings for cross-module optimization                      │   │
  │   │   - Instance declarations                                        │   │
  │   │   - Rewrite rules                                                │   │
  │   │                                                                  │   │
  │   └─────────────────────────────────────────────────────────────────┘   │
  │         │                                                                │
  │         ▼                                                                │
  │   ┌─────────────────────────────────────────────────────────────────┐   │
  │   │                     Object File (.o)                             │   │
  │   │                                                                  │   │
  │   │   - Compiled code                                                │   │
  │   │   - Info tables                                                  │   │
  │   │   - Static closures                                              │   │
  │   │   - Relocations                                                  │   │
  │   │                                                                  │   │
  │   └─────────────────────────────────────────────────────────────────┘   │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                    DEPENDENCY RESOLUTION                                 │
  │                                                                          │
  │   Package Database                                                       │
  │   ────────────────                                                       │
  │   ┌──────────────────────────────────────────────────────────┐          │
  │   │  base-4.18.0                                              │          │
  │   │    ├── modules: Prelude, Data.List, Control.Monad, ...   │          │
  │   │    ├── depends: ghc-prim, ghc-bignum                     │          │
  │   │    └── abi-hash: abc123...                               │          │
  │   ├──────────────────────────────────────────────────────────┤          │
  │   │  text-2.0                                                 │          │
  │   │    ├── modules: Data.Text, Data.Text.IO, ...             │          │
  │   │    ├── depends: base, bytestring                         │          │
  │   │    └── abi-hash: def456...                               │          │
  │   └──────────────────────────────────────────────────────────┘          │
  │                                                                          │
  │   Import Resolution:                                                     │
  │   ──────────────────                                                     │
  │   import Data.Text (Text)                                                │
  │     1. Search package DB for module "Data.Text"                          │
  │     2. Find package "text-2.0"                                           │
  │     3. Load interface file                                               │
  │     4. Resolve "Text" in exports                                         │
  │     5. Record dependency                                                 │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘
```

## Compilation Modes

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         COMPILATION MODES                                   │
└─────────────────────────────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                                                                          │
  │   bhc Main.hs                          One-shot compilation              │
  │        │                                                                 │
  │        ├── Parse Main.hs                                                 │
  │        ├── Load dependencies (from .bhi files)                           │
  │        ├── Type check                                                    │
  │        ├── Optimize                                                      │
  │        ├── Generate code                                                 │
  │        └── Link with RTS → executable                                    │
  │                                                                          │
  │   bhc --make Main.hs                   Dependency-aware compilation      │
  │        │                                                                 │
  │        ├── Build dependency graph                                        │
  │        ├── Check timestamps (.hs vs .bhi vs .o)                          │
  │        ├── Compile changed modules (parallel where possible)             │
  │        └── Link all objects → executable                                 │
  │                                                                          │
  │   bhc build                            Cabal integration                 │
  │        │                                                                 │
  │        ├── Read .cabal file                                              │
  │        ├── Resolve Hackage dependencies                                  │
  │        ├── Download/build dependencies                                   │
  │        ├── Compile project modules                                       │
  │        └── Link → executable/library                                     │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘
```

## Diagnostic System

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        DIAGNOSTIC SYSTEM                                    │
└─────────────────────────────────────────────────────────────────────────────┘

  BHC provides rich error messages with source locations and suggestions.

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                      ERROR STRUCTURE                                     │
  │                                                                          │
  │   ┌───────────────────────────────────────────────────────────────┐     │
  │   │  Diagnostic                                                    │     │
  │   │    ├── severity: Error | Warning | Note                        │     │
  │   │    ├── code: E0001, W0042, ...                                │     │
  │   │    ├── message: "Type mismatch..."                            │     │
  │   │    ├── primary_span: file:line:col                            │     │
  │   │    ├── secondary_spans: [related locations]                   │     │
  │   │    ├── labels: [span annotations]                             │     │
  │   │    ├── notes: [additional context]                            │     │
  │   │    └── suggestions: [fix-it hints]                            │     │
  │   └───────────────────────────────────────────────────────────────┘     │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘

  ┌─────────────────────────────────────────────────────────────────────────┐
  │                      EXAMPLE OUTPUT                                      │
  │                                                                          │
  │   error[E0308]: type mismatch                                            │
  │     --> src/Main.hs:15:10                                                │
  │      |                                                                   │
  │   14 |   let x :: Int                                                    │
  │   15 |       x = "hello"                                                 │
  │      |           ^^^^^^^ expected `Int`, found `String`                  │
  │      |                                                                   │
  │      = note: expected type `Int`                                         │
  │                 found type `String`                                      │
  │      = help: consider using `read` to parse the string:                  │
  │      |                                                                   │
  │   15 |       x = read "hello"                                            │
  │      |           ++++                                                    │
  │                                                                          │
  └─────────────────────────────────────────────────────────────────────────┘
```

## Further Reading

- [Get Started](/get-started/) - Install BHC and write your first program
- [Compatibility](/compatibility/) - Supported Haskell features
- [Profiles](/profiles/) - Runtime profile details
- [Numeric](/numeric/) - Tensor IR and fusion guarantees
- [Targets](/targets/) - Backend targets (native, WASM, GPU)
