+++
title = "One Developer, One Claude, One Month: Building a Haskell Compiler from Scratch"
description = "Anthropic built a C compiler with sixteen parallel Claude instances. I built a Haskell compiler with one Claude and a Max subscription. We arrived at the same conclusions about the future of software development."
date = 2026-02-15
template = "blog-post.html"

[extra]
tag = "engineering"
+++

Last week, Anthropic published [a fascinating engineering post](https://www.anthropic.com/engineering/building-c-compiler) about building a C compiler using sixteen parallel Claude instances. Over two weeks, they produced a 100,000-line Rust-based C compiler capable of compiling the Linux kernel. They spent roughly $20,000 in API credits and consumed 2 billion input tokens.

I read the post and smiled. Not because it was surprising, but because I'd been living a smaller-scale version of the same experiment for a month already.

## The BHC Experiment

On January 16, 2026, I started building BHC (Basel Haskell Compiler): a clean-slate Haskell compiler written in Rust. Not a toy. Not a weekend project. A real compiler with a lexer, parser, type checker, HIR, Core IR, LLVM codegen, and a runtime system, targeting native executables on macOS and Linux.

My setup is considerably more modest than Anthropic's: one person, one Claude Pro subscription (the 20x Max plan), and whatever time I can carve out between everything else. No sixteen parallel agents in Docker containers. No $20,000 API budget. No team. Just me, talking to Claude in a terminal.

Thirty days later, the numbers look like this:

| Metric | Value |
|--------|-------|
| Lines of Rust | **184,621** |
| Crates | **33** (compiler) + **9** (stdlib) |
| Commits | **316** |
| Feature commits | **64** |
| End-to-end tests | **121**, all passing |
| Test fixtures | **123** across 5 tiers |
| Development time | **30 days** |

BHC compiles real Haskell programs to native machine code via LLVM. Here's a program that exercises user-defined typeclasses with dictionary passing, a feature we landed on day 28:

```haskell
module Main where

class Describable a where
    describe :: a -> String

data Color = Red | Green | Blue

instance Describable Color where
    describe Red   = "color:red"
    describe Green = "color:green"
    describe Blue  = "color:blue"

greet :: Describable a => a -> String
greet x = "Hello, " ++ describe x ++ "!"

main :: IO ()
main = do
    putStrLn (greet Red)
    putStrLn (greet Blue)
    putStrLn (describe Green)
```

```
$ bhc run typeclass.hs
Hello, color:red!
Hello, color:blue!
color:green
```

That `Describable a =>` constraint compiles to a dictionary argument passed at runtime, exactly like GHC does. The `greet` function receives a tuple containing a pointer to the `describe` implementation, extracts it with a selector, and calls it. All generated as LLVM IR, linked against a Rust runtime, producing a native binary.

We've also compiled a multi-module Markdown-to-HTML converter:

```haskell
main = do
  let blocks = parseMarkdown "# Hello\n\nA paragraph.\n"
  putStrLn (renderDocument blocks)
```

```
$ bhc run markdown.hs
<html>
<body>
<h1>Hello</h1>
<p>A paragraph.</p>
</body>
</html>
```

And a 307-line JSON parser, a CSV parser, and a word-count utility. These aren't demonstrations of Claude's ability to write Haskell. They're demonstrations of BHC's ability to compile it.

## What Actually Happened

I want to be precise about how the collaboration works, because I think the details matter more than the headline.

Claude doesn't write BHC unsupervised. I'm not feeding it a spec and walking away. What happens is closer to pair programming with an extraordinarily knowledgeable partner who never gets tired and never forgets a calling convention.

A typical session looks like this: I decide what feature to build next, say, `Data.Map` support or `deriving Ord`. I explain the goal in natural language. Claude and I explore the existing codebase together, identify the touch points, and work through the implementation. When something segfaults (and it does, frequently, because we're generating native code) we debug together.

The key insight I've arrived at, and that Anthropic's post confirms from a very different angle, is this: **the bottleneck is no longer writing code. The bottleneck is knowing what to build and being able to verify that it works.**

Anthropic's team solved this with GCC as a reference oracle and an existing test suite. I solved it with a growing set of end-to-end tests, each one a small Haskell program with its expected output. Every feature we add, we write a test first, and we don't move on until the compiled binary produces the right output. That discipline is what makes the AI collaboration work. Without it, you're generating plausible-looking code that might do anything.

## The Anatomy of a Milestone

To make this concrete, here's what adding a new builtin function to BHC actually involves. This is the kind of task where AI assistance shines, and the kind that would be tedious and error-prone by hand.

Every new function (say, `even`) requires changes in four files that must stay perfectly synchronized:

1. **Lowering context** (`bhc-lower/src/context.rs`): Register a fixed DefId, like `DefId(10500)`, mapped to the name `"even"`. This is how the rest of the compiler refers to this function.

2. **Type checker** (`bhc-typeck/src/builtins.rs`): Register the type signature `Int -> Bool` at the same DefId. If this doesn't match, type inference will accept programs that segfault at runtime.

3. **Type context** (`bhc-typeck/src/context.rs`): Wire the DefId into the type environment so constraint solving can find it.

4. **LLVM codegen** (`bhc-codegen/src/llvm/lower.rs`): Implement the actual code generation: emit `srem(n, 2)`, compare to zero, allocate a Bool ADT with the right tag. Register the arity so the compiler knows when a call is saturated.

Miss any one of these four, and you get a different failure mode: missing DefId gives "unknown function," wrong type signature gives silent miscompilation, missing arity entry gives a closure allocation where you wanted a direct call. Claude is remarkably good at holding all four in working memory and keeping them consistent. This is, I think, exactly the kind of "holding many constraints simultaneously" task where large context windows pay off.

A typical milestone session takes two to four hours and touches 200 to 800 lines across these four files. We landed 41 milestones in 30 days. Each one adds a testable capability, and each one's test must pass before we move on.

## Debugging Example: Two Kinds of True

Here's a debugging story that illustrates what working at this level actually feels like.

Around milestone E.30, we started getting mysterious failures: `filter even [1,2,3,4]` would return `[1,2,3,4]` instead of `[2,4]`. Every element passed the predicate. The `even` function worked correctly when called directly, `show (even 4)` printed `True`. But when passed as an argument to `filter`, every number was "even."

The problem turned out to be that BHC had two representations of boolean values, created at different times by different milestones, and nobody (neither Claude nor I) had noticed the inconsistency.

**Comparison operators** (`==`, `<`, `>`) returned a *tagged integer as a pointer*: the literal number 0 or 1, cast to a pointer type. Checking this is simple: is the pointer value zero or non-zero?

**Functions like `even` and `isAlpha`** returned a proper *Bool ADT*: a heap-allocated struct with a tag field (0 for False, 1 for True). Checking this requires loading the tag from the struct.

The `filter` function was written to check booleans using `ptr_to_int`, which works for tagged integers. But when `even` returns a Bool ADT, `ptr_to_int` gives you the *heap address*, which is always non-zero. So every value looks True.

The fix was `extract_bool_tag()`, a unified function that checks whether it's looking at a tagged integer (value is 0 or 1) or a heap pointer (value is larger, so dereference and read the tag). Three new basic blocks, a comparison, a conditional branch, and a phi node to merge the results. We applied it to `filter`, `takeWhile`, `dropWhile`, `span`, `break`, `find`, and `partition`.

This is the kind of bug that's invisible in isolation. Each representation works fine on its own. It only breaks when two subsystems interact in a way that neither was designed for. Finding and fixing it took about forty minutes of collaborative debugging, reading LLVM IR, and tracing pointer values. Claude spotted the dual representation; I decided the architectural fix.

## Maintaining Context Across Sessions

One challenge unique to AI-assisted development at this scale: Claude is stateless. Each conversation starts fresh. But BHC has 185,000 lines of code with intricate invariants. How do you maintain continuity?

Two mechanisms turned out to be essential.

**CLAUDE.md** is a project instructions file that Claude reads at the start of every session. Ours contains the full project spec, repository structure, coding guidelines, and the development roadmap. It's the "who we are and what we're building" document. This means Claude never needs to rediscover the project's architecture.

**Auto-memory files** are where Claude records hard-won lessons during development. When we discover a pitfall (like the Bool representation duality, or the fact that BHC closures use flat calling conventions rather than curried ones), Claude writes it down. Next session, when we're working on a feature that might hit the same issue, the note is right there in context.

The memory file for BHC is currently about 300 lines of dense technical notes. It reads like a changelog of every non-obvious decision and every trap we've fallen into. Entries like:

> **Container Bool ADT Pitfall (E.21)**: Container predicate functions (Map.member, Map.null, Set.member, etc.) RTS returns `i64` (0/1). Must wrap with `allocate_bool_adt()` not `int_to_ptr()`, otherwise `show` prints raw int instead of True/False.

> **Flat vs Curried Calling Convention (E.27)**: BHC compiles named multi-arg functions as flat: `fn(env, x, y) -> result`. Cannot use curried 2-step calls on flat functions, causes segfault.

These aren't documentation for humans. They're institutional memory for an AI that would otherwise repeat mistakes. It's a strange artifact, but it works.

## Convergent Evolution

What strikes me most about Anthropic's post is how independently we arrived at similar conclusions:

**Test suites are the real product.** Anthropic writes: "The key enabler was having high quality test suites." Absolutely. My 121 E2E tests are the backbone of BHC. They're what let me refactor with confidence, what catch regressions when a new feature accidentally changes how Bool ADTs are represented, and what tell me when a feature actually works versus when it merely compiles.

**Decomposition is everything.** Anthropic decomposed C compilation into independently testable subtasks using GCC as an oracle. I decomposed Haskell compilation into milestones (E.1 through E.41 so far) each one adding a specific capability (ByteStrings, IORef, Data.Map, deriving, typeclasses) with its own test suite. Each milestone is small enough to fit in a single Claude session.

**The AI is better at some things than you'd expect.** Anthropic found their agents could handle complex code generation tasks autonomously. I've found Claude surprisingly effective at LLVM IR generation, at reasoning about memory layouts and calling conventions, and at tracking the four-way registration dance that BHC requires for every new builtin function. These are tasks that involve holding many constraints in your head simultaneously, exactly where an AI with a large context window shines.

**The AI is worse at some things than you'd hope.** Both projects hit the same wall: novel architectural decisions. When I need to decide *how* to implement a feature (should typeclasses use dictionary passing or inline specialization? should derived instances generate Core IR or go straight to LLVM?) that's still a human decision. Claude can lay out the trade-offs, but the judgment call is mine.

## What This Means

I'm not trying to claim equivalence with Anthropic's project. They had a team, serious infrastructure, and a much larger budget. Their compiler builds Linux. Mine compiles Fibonacci and JSON parsers.

But that's precisely why I think this is worth writing about. If a *single developer* with a *consumer subscription* can produce 185,000 lines of working compiler code in 30 days (code that does type inference, generates LLVM IR, and produces native binaries) then something fundamental has shifted in what's possible.

I've been writing software professionally for a long time. I have never been this productive. Not by an order of magnitude.

And I don't think this is about compilers specifically. Compilers happen to be a good test case because they're complex, well-specified, and testable. But the same pattern (human sets direction, AI generates code, tests verify correctness) applies to any domain where you can write good tests.

Anthropic titled their post "Building a C Compiler with a Team of Parallel Claudes." I'd title mine more modestly: one developer, one Claude, one month. But I think we're both pointing at the same future.

## What's Next for BHC

BHC is far from done. We're compiling a meaningful subset of Haskell, but there's a long road to full language coverage. The Numeric profile (BHC's differentiator, with guaranteed fusion, SIMD, and GPU offload) exists as infrastructure but hasn't been battle-tested on real workloads. The WASM backend emits binaries that don't validate yet. The REPL is stubbed.

But the foundation is solid, and the pace hasn't slowed. Forty-one feature milestones in thirty days. If the next thirty are anything like the first, BHC will be compiling real-world Haskell packages by spring.

The tools are here. The question is what we build with them.

---

*BHC is open source at [github.com/raskell-io/bhc](https://github.com/raskell-io/bhc). If you're interested in compilers, Haskell, or AI-assisted development, come take a look.*
