# Locque: State-of-the-Project Evaluation

**Date:** 2026-03-25
**Evaluator:** polecat obsidian (Gas Town autonomous audit)
**Scope:** Interpreter, type system, stdlib, tests, tooling, open questions, compiler branch

---

## 1. Interpreter Health

### What Works Solidly

**Parser (Parser.hs, 1,454 lines)**
Both M-expression (human-readable) and S-expression (canonical/storage) parsers are production-grade. The 1:1 bidirectional conversion (`emit-lqs`/`emit-lq`) is reliable. The `checkParens` paren-checker correctly reports line/column positions.

**Evaluator (Eval.hs, 1,387 lines)**
The evaluator handles all in-use constructs:
- Values: literals, closures, constructors, lists, pairs, dictionaries, unit, booleans
- Computations: `bind`/`return`/`perform` chain
- Recursion: structural `recur` at value level
- ~60 primitives covering arithmetic, string, file I/O, TCP networking, process execution, shell, time, dictionary operations, signal handling, validation

All 638 assertions across 33 suites pass with zero failures (26 are *expected* failures via `error-tests` in Smythfile.lq).

**Import resolver and caching (ImportResolver.hs, RunCache.hs)**
Module loading with `::` → file path mapping is solid. The run cache (`.locque-cache/`) uses content-based digests (FNV-1a variant) to skip redundant type checks; version is at `cacheVersionCurrent = 5`, suggesting it has been invalidated correctly across breaking changes.

### What Is Fragile or Incomplete

**DictPass / typeclass evaluation split (DictPass.hs, 966 lines — the largest non-typechecker file)**
The type checker natively resolves typeclass constraints (it finds instances and checks them). However, the evaluator still requires a separate dictionary-insertion pass (`transformModuleWithEnvs`). This is called in six places: `Main.hs`, `SmythRun.hs`, `SmythTest.hs`, `SmythDump.hs`, and inline in `Eval.hs` for imported modules. The pass re-traverses the entire module AST to insert explicit dictionary arguments at call sites. This is the project's most significant structural debt: two parallel systems tracking the same typeclass information. A regression in either can silently diverge.

**Effect tracking is nominal, not enforced**
`ECompType eff t` carries an effect tag, but the type checker applies a blanket rule: if either side is `Effects::any`, the comparison passes (TypeChecker.hs:1429–1436). In practice, essentially all computations currently inferred as `computation T` use `Effects::any`. The `effects.lq` library defines `Effects` as a simple ADT (`any`, `pure`, `io`, `ffi`), but subsumption between these is not checked — `computation Effects::pure Nat` and `computation Effects::io Nat` are considered equal whenever `any` is involved. Effect-indexed computation remains aspirational.

**Structural recursion is value-only**
`recur` is only valid in `value` function bodies (TypeChecker.hs:1912–1919). Recursive computation functions require a workaround: define a recursive value function and wrap it with `compute ... end`. There is no `recur` for computations, no well-founded recursion on measures, and no coinduction.

**Termination: positivity check absent**
User-defined data types use `data ... in TypeN ... end` syntax, and constructors are type-checked, but there is no strict positivity check for constructor argument types. Negative occurrences (e.g., `data Bad where case Bad::mk of-type Function (Function Bad Bad) Bad`) would be accepted.

**`match` scrutinee restriction**
TypeChecker.hs lines 2270–2276 have two paths that hit `"unsupported scrutinee type"` for certain scrutinee shapes. These are fallbacks for non-constructor-headed types; the known supported scrutinee types are data constructors, `Boolean`, `List`, `Pair`, `Dictionary`, and `Option`/`Either`/`Result` (via their user-defined `data` encodings). The restriction is documented behavior, not a silent bug, but it means matching on a term whose head is a type variable is rejected.

**`unsupportedScrutineeValue` in Eval**
Eval.hs:1243 panics with `"match: unsupported scrutinee value"` for values that don't match any known constructor shape at runtime. This is a hard crash rather than a user-visible error message.

**LSP server is minimal (LocqueLsp.hs, 737 lines)**
Implements: `textDocument/didOpen`, `didChange`, `didClose`, `definition` (go-to-definition), `documentSymbol`, and `publishDiagnostics` (on save/open). Absent: hover types, completion, find references, rename, formatting, semantic tokens. The server does re-run the type checker on every open/change to publish diagnostics, which is correct but potentially slow for large modules with transitive imports.

**Division by zero panics at runtime**
Eval.hs:981: `error "div-nat-prim: division by zero"` — this is an unhandled Haskell exception, not a Locque-level error. Same for mod-by-zero (line 990). These should be wrapped as `Result`-returning operations or at minimum produce structured Locque errors.

---

## 2. Type System Status

### Implemented and Working

| Feature | Status | Notes |
|---------|--------|-------|
| Dependent function types (`for-all x as A to B`) | ✓ Full | Pi types with full reduction |
| Sigma types (`there-exists x as A in B`) | ✓ Full | Pack/unpack with witness checking |
| Non-cumulative universe tower (`Type0`, `Type1`, ...) | ✓ Full | `lift`/`up`/`down` coercions implemented |
| User-defined inductive types (`data ... in TypeN`) | ✓ Full | Multi-param, includes recursors |
| Typed match with dependent motive | ✓ Full | `match e of-type T as x returns R` |
| Propositional equality (`equal T a b`) | ✓ Full | `reflexive`, `rewrite`, `transport` all work |
| Typeclass checking (`requires C A`) | ✓ Full | Multi-constraint via `and`; instance resolution native |
| Recursive definitions with structural check | ✓ Value-level | Computation recursion not yet supported |
| Opacity markers (`transparent`/`opaque`) | ✓ Full | Per-definition δ-unfolding control |
| βδι normalization (WHNF + full) | ✓ Full | `normalize`, `whnf` both available |
| Type-level computation (`match` returning `TypeN`) | ✓ Full | Type families via functions (see `test/typecheck/type_family.lq`) |
| Exhaustiveness checking for `match` | ✓ Full | Missing/duplicate case errors with names |
| `rewrite` for equality types | ✓ Full | Checks proof is of `equal T a b` shape |
| Higher-kinded type parameters (`F of-kind TypeFunction Type0`) | ✓ Full | Used in Functor/Applicative/Monad |
| Import cycle detection | ✓ Full | Error at TypeChecker.hs:3247 |
| Fuzzy "Did you mean?" in scope errors | ✓ Full | Levenshtein ≤ 2 on all in-scope names |
| Source location in errors | ✓ Partial | Present in parser-originated errors; typechecker uses `NoLoc` for most internal cases |

### Stubbed or Not Implemented

**Effect polymorphism:** The `ECompType eff t` form exists in the AST and is parsed, but effects are opaque during checking — any `any` side wins. No effect rows, no algebraic effects, no effect inference.

**Universe polymorphism / level variables:** Levels are concrete integers. There are no level variables, no universe polymorphism (`∀ l. ...`), and no `lift` inference. Authors must explicitly annotate all universe levels.

**Eta rules:** The comment in philosophy.md ("opt-in definitional η") is not yet implemented. Functions are not eta-expanded for conversion: `f` and `λ x → f x` are *not* definitionally equal.

**Coinduction / corecursion:** Not present. `forever-prim` in the evaluator is an infinite loop primitive, not a typed corecursion construct.

**Type holes:** Not implemented. Partial programs cannot leave `?` holes for the type checker to fill.

**Implication / propositional logic proofs involving equality rewriting at computation level:** `rewrite` works at the value level to produce a term of a rewritten type, but there is no dependent elimination that bridges value-level proofs to computation branches.

**Strict positivity:** No check. Negative recursive types are accepted silently.

**Totality for computations:** Only value-level `recur` is checked for structural decrease. Computation-level recursion has no totality guarantee.

---

## 3. Stdlib Completeness

The stdlib totals **5,662 lines across 49 files**. Here is a module-by-module status:

### Solid Modules

| Module | Lines | Notes |
|--------|-------|-------|
| `prelude` | 238 | Core re-exports; covers Option/Either/Result data, list ops, pair ops, computation helpers; recently added `map-comp`, `bind-comp`, `and-then`, `tap`, `when`, `unless`, `guard`, `sequence-when` |
| `string` | 850 | Very complete: 108 functions covering concat, equality, split-on, join-with, trim, lines/unlines, substring, index-of, URL decode, character predicates, char-code, hex helpers |
| `list` | 315 | 26 functions: drop-until, nth, take, drop, last, init, NonEmptyList, member, any, all, count, slice, reverse, concat, sort-by; missing: `traverse`, `map-compute`, `filter-compute`, `find-map` (listed in TODO.md) |
| `arithmetic` | 60 | add, sub, mul, div, mod, remainder, is-zero, is-even, is-odd, max, min |
| `option` | 95 | map, and-then, unwrap-or, or-else, filter, zip, from-either, to-either, is-none, is-some |
| `either` | 81 | map, map-left, and-then, bimap, fold, is-left, is-right, left-value, right-value, with-context |
| `result` | 109 | ok, err, map, map-error, and-then, fold, unwrap-or, is-ok, is-err, with-context |
| `logic` | 324 | Proof library: `Not`, `And`, `Or`, `Decidable`, `Refine` (refinement types), `transport`, `symmetry`, `transitivity`, `congruence`/`congruence2`/`congruence3`; decide-eq primitives for Nat/String/Bool/Pair/List |
| `path` | 143 | join, dirname, basename, ext, is-absolute, is-relative, normalize-slashes |
| `dictionary` | 97 | empty, insert, lookup, remove, size, to-list, from-list, map-values |
| `typeclass/equality` | ~60 | Equality typeclass + instances for Natural, String, Boolean, Pair, List |
| `typeclass/order` | 129 | LessThan, LessOrEqual, GreaterThan, GreaterOrEqual, min/max + instances |
| `typeclass/semigroup` | 28 | Semigroup + instances for String, List, NonEmptyList |
| `typeclass/monoid` | 42 | Monoid (extends Semigroup) + instances |
| `typeclass/functor` | ~60 | Functor + instances for Option, Either, Result |
| `typeclass/applicative` | ~90 | Applicative + instances for Option, Either, Result, List |
| `typeclass/monad` | 79 | Monad + instances for Option, Either, Result, List |
| `typeclass/hash` | ~30 | Hash typeclass; used by Dictionary |
| `control` | 37 | `when`, `unless`, `guard`, `tap` |
| `file` | 112 | read, write, append, copy, copy-tree, rename, list-dir, walk, stat, lines helpers |
| `natural` | 107 | to-string, from-string, is-digit, Peano helpers, basic lemmas |
| `cli` | 119 | args, options, flag parsing |
| `shell` | 5 | Thin wrapper over shell-prim; minimal |
| `signal` | 18 | SIGINT/SIGTERM handlers via on-signal-prim |
| `net/tcp` | ~50 | TCP listen/accept/recv/send/close with timeout-select |
| `http/request` | ~60 | Request parser (line-by-line HTTP/1.1) |
| `http/response` | ~50 | Response builder (ok, bad-request, not-found, etc.) |
| `http/server` | ~80 | Simple TCP request/response loop |
| `time` | 36 | now, measure (wall clock micros) |
| `effects` | 8 | Just the `Effects` ADT; no helpers |
| `io` | 6 | `print` wrapper only |
| `process` | 3 | Stub — just `process-run-prim` exposed |
| `type_aliases` | 52 | `Function`, `TypeFunction`, `BinaryTypeFunction`, `Predicate`, `Comparator`, `FoldStep`, `Thunk`, `Exists` |

### Gaps and Inconsistencies

**List traversal helpers missing:** `traverse`, `map-compute`, `filter-compute`, `fold-compute`, `find-map` are listed in TODO.md but not implemented. Current workaround is manually inline `P::fold` with a computation accumulator.

**`split-once` missing from string:** `S::split-on` returns a full list; there is no single-split-at-first-occurrence helper. TODO.md notes this.

**`process` is a stub (3 lines):** Only exposes the raw `process-run-prim`. No structured result type, no stderr capture, no exit code distinction. The TODO mentions `run-result`/`run-ok` wrappers.

**`effects` provides no helpers:** The `Effects` ADT exists but there are no `lift-io`, `lift-pure`, or subsumption helpers. Authors writing effect-parameterized code have no library support.

**CLI option parsing is incomplete:** The `cli` module handles basic flags but TODO.md notes structured option parsing (returning flags/options/positionals) as a near-term item.

**`typeclass/display` and `typeclass/default`** are listed in AGENTS.md but `display.lq` is present (it provides a `Display` typeclass with `show`), while `default.lq` provides a `Default` typeclass. However, neither has tests in the standard rollup test suite — they're present in `lib/typeclass/` but no `test/typeclass/` files cover them.

**No JSON library:** Parked in open_questions.md. The HTTP server builds/parses HTTP line-by-line with no structured body parsing.

**No HTTP client:** `lib/http/` has a server. There is no client-side `GET`/`POST` abstraction.

**`testimport.lq` is a test artifact in `lib/`:** `lib/testimport.lq` appears to exist solely to test import mechanics; it exports a single test definition. It should probably be in `test/` only.

---

## 4. Test Suite

### Coverage and Reliability

- **638 total assertions** across 33 suites in `test/main.lq`
- **26 expected failures** registered in `Smythfile.lq` (error-tests), all verified to produce the correct error substring
- **Zero actual failures**: all 638 assertions pass cleanly
- **159 test files** totaling 7,369 lines — tests are substantive, not trivial

### Suite Coverage Breakdown (by assertion count)

| Suite | Assertions | Coverage quality |
|-------|-----------|-----------------|
| `test_prelude` | 156 | High — covers all re-exported helpers |
| `test_typeclass` | 121 | High — Functor/Applicative/Monad/Equality/Order |
| `test_string` | 50 | Good — split, join, index, starts/ends-with |
| `test_list` | 43 | Good — standard list operations |
| `test_shell` | 24 | Integration — runs actual subprocesses |
| `test_file` | 32 | Integration — reads/writes real files |
| `test_dictionary` | 21 | Covers insert/lookup/remove/size/map-values |
| `test_http` | 17 | Request parsing, response building, server roundtrip |
| `test_path` | 18 | Path join, dirname, basename, ext |
| `test_logic` | 18 | Proof construction, decidable equality |
| `test_result` / `test_either` / `test_option` | 12/13/11 | Core combinators covered |
| `test_arithmetic` | 27 | All arithmetic operations |
| `test_control` | 10 | when/unless/guard/tap |

### Weaknesses

**No property-based tests.** Every test is a concrete assertion. Edge cases (empty list, boundary Natural values, Unicode strings, concurrent TCP) are only tested if manually enumerated. TODO.md acknowledges this; QA fuzzers are in the "parked backlog."

**test_signal has 1 assertion** (a trivial "smoke test"). Signal handling under concurrent load is untested.

**test_net has 1 assertion.** TCP integration tests are essentially absent. The net tests only verify that the module loads.

**test_process has 2 assertions.** Process execution (stdin/stdout capture, non-zero exit codes) is undertested.

**test_typeclass/display and test_typeclass/default are absent** from the main rollup.

**No performance regression tests.** `smyth bench` exists and `test/bench.lq` exists, but benchmark coverage is sparse and there are no tracked baselines. The `smyth test --slow` mode reports the top 5 slowest suites but doesn't fail on regressions.

**test_io has 3 assertions** — just smoke-tests `print`. There is no test for `capture-output-prim` behavior.

### Known Flaky Behavior

No systematically flaky tests identified. The test runner uses `assert-no-output` as the harness, which re-runs each suite inside output-capture to verify no extraneous printing. This provides a useful side-channel check.

---

## 5. Tooling (smyth)

### Working Subcommands

| Subcommand | Status | Notes |
|-----------|--------|-------|
| `smyth run <file>` | ✓ Full | Type-checks then runs; supports `--pid-file`, `--timeout` |
| `smyth test` | ✓ Full | Runs `test/main.lq`; supports `--slow`, `--verbose` |
| `smyth test <file>` | ✓ Full | Runs a single test file |
| `smyth bench` | ✓ Full | Runs `test/bench.lq` |
| `smyth bench <file>` | ✓ Full | Runs a specific benchmark |
| `smyth dump core` | ✓ Full | Prints S-expr AST |
| `smyth dump normalized` | ✓ Full | After βδι normalization |
| `smyth dump elaborated` | ✓ Full | After DictPass transformation |
| `smyth dump typed` | ✓ Full | With inferred type annotations |
| `smyth dump typed-normalized` | ✓ Full | Normalized + typed |
| `smyth dump types` | ✓ Full | Per-definition type signatures |
| `smyth dump types-normalized` | ✓ Full | Normalized type signatures |
| `smyth dump --multi` | ✓ Full | Multiple dump requests in one invocation |
| `smyth count` | ✓ Full | Lines/files for lib/ and test/ |
| `smyth dependencies` | ✓ Full | Module dependency tree |
| `smyth format` | ✓ Functional | Delegates to `lib/tools/formatter.lq`; works but formatter itself is Locque code (tested via `test_tools` suite) |
| `smyth --help` | ✓ Full | |

### Missing or Incomplete

**`smyth new` (project scaffolding):** Listed as near-term in TODO.md. Currently, starting a new project requires manually creating `Smythfile.lq`, `lib/`, and `test/` directories by hand.

**LSP server is a separate binary** (`locque-lsp` / `LocqueLsp.hs`) that isn't surfaced through `smyth`. IDE integration requires manual configuration. The LSP itself is minimal (no completion, no hover types).

**`smyth typecheck`:** Not a top-level smyth subcommand; only available via `locque-interpreter typecheck <file>`. Users who reach for `smyth typecheck` will be confused.

**No `smyth format --fix`:** The formatter subcommand checks formatting but there's no auto-fix mode surfaced through smyth (the underlying `formatter.lq` may support it via args).

**Build caching does not invalidate on transitive changes:** If `lib/foo.lq` changes, cached type-check results for `lib/bar.lq` (which imports `foo`) are not automatically invalidated. This can produce stale diagnostics after library edits. The cache keys off per-file digests, not transitive closure.

---

## 6. Open Questions Status

From `open_questions.md`:

### Open (Unresolved)

**1. Universes — level polymorphism:** Decision is explicit non-cumulative tower with `lift`/`up`/`down`. The open part: whether to add level solving/inference later. Currently `lift Natural from Type0 to Type1` must be written explicitly everywhere. This is a significant ergonomics burden for any universe-polymorphic library code. **Status: open, no work in progress.**

**2. Inductive types — strict positivity, explicit recursors, totality for computations:** The kernel accepts `data` declarations but does not check strict positivity. Recursors (`Type::recursor`) are automatically generated and work at value level. Computation recursion totality is explicitly deferred. **Status: partially resolved (basic inductives work), core kernel guarantee not met.**

**3. Parked backlog (JSON, HTTP):** JSON is not implemented. HTTP client is not implemented. **Status: open.**

### Resolved (from `## Resolved Decisions` section)

1. **Normalization:** WHNF for conversion, CBPV split — implemented and solid.
2. **Definitional equality:** βδι with opacity — implemented and tested.
3. **Totality vs partiality:** Total core with explicit effects — partially implemented (value level only).
4. **Namespaces:** One module per file, `::` qualification — implemented and working.
5. **Surface syntax mapping:** 1:1 M-exp ↔ S-exp — implemented.
6. **Ergonomics for LLMs:** No implicit coercions, explicit effects — implemented.
7. **Entrypoints:** `smyth run/test/bench/dump/format/count` — all working.

### From `philosophy.md` (Live Design Constraints)

- "Words over symbols" posture — consistently applied throughout the grammar.
- "No warnings, only errors" — correctly enforced in both type checker and validator.
- `define X as Y` extensibility pattern — consistently applied; new constructs (`typeclass`, `instance`, `data`) all follow this.
- Effects via `computation T` / `perform` — implemented structurally but not semantically enforced.

---

## 7. The `compiler/` Branch

**Branch:** `origin/debug_output` (the most recent compiler-active branch)
**Last commit:** "WIP debug output from compiler stages"
**History:** ~20 commits; substantial work completed

### What Was Attempted

A Haskell-to-Haskell compilation pipeline: Locque source → Core IR → Erased Core IR → Haskell source → GHC. The compiler lives in a separate `compiler/` directory with its own Cabal project.

**Key files (in `compiler/src/`):**
- `Locque/Compiler/Core.hs` — Core IR (`CoreValue`, `CoreComp`, `CoreDecl`, etc.); clean separation of value/computation worlds
- `Locque/Compiler/Convert.hs` — Converts from interpreter AST to Core IR
- `Locque/Compiler/Erase.hs` — Type-level erasure (removes type arguments, universe levels)
- `Locque/Compiler/CoreErased.hs` — Erased IR (no types, just values and computations)
- `Locque/Compiler/Codegen.hs` — Emits Haskell with `unsafeCoerce` bridges; targets a `LocqueRuntime.hs` runtime
- `Locque/Compiler/Emit.hs` — Top-level module printer

**Milestone history suggests:** Compiled `lhttp` (the HTTP server example), wired compile tests, achieved multi-argument lowering, and added debug output for compiler stages. The `reduce-unsafeCoerce-usage` commit suggests a known correctness hole.

### What Should Be Done

The branch is in "WIP debug output" state — it compiles but is not considered production. Key issues to address before promotion:

1. **`unsafeCoerce` usage is a red flag.** The erased IR loses type information, and the Haskell codegen bridges gaps with `unsafeCoerce`. This bypasses GHC's safety and any GHC-level type errors become runtime panics. The remaining occurrences need to be eliminated via a typed intermediate representation or by threading enough type information into the erased IR.

2. **The branch diverged from the current `main` surface.** It was forked before recent additions (Functor/Applicative/Monad typeclasses, new list/prelude helpers, the `with-context` result helpers, `map-values` for Dictionary). Rebasing onto current `main` would require updating the Core IR conversion layer for these new constructs.

3. **The separate `compiler/` Cabal project is not integrated into `smyth`.** There's no `smyth compile` subcommand. Integration would require either merging the compiler into the main `locque-interpreter.cabal` or making `smyth` aware of the separate project.

4. **No test suite for the compiler.** The `compiler/test/` directory exists but it's unclear (from history) whether it has comprehensive coverage. Compiled output should be diffed against interpreted output for a regression suite.

**Recommendation:** Do not merge as-is. The branch is valuable proof-of-concept work. It should be treated as a research branch until `unsafeCoerce` is eliminated and it can pass the full `smyth test` suite against compiled output. A `smyth compile --emit-haskell <file>` subcommand (no execution, just emission) would be a safe first integration step.

---

## 8. Production-Quality vs. Prototype-Quality Assessment

### Production-Quality

- **Parser** (both M-expr and S-expr): Well-tested, handles real programs, correct error reporting.
- **Type checker core** (βδι normalization, Pi/Sigma, dependent match, typeclasses): Handles all tested constructs correctly. The 3,451-line TypeChecker.hs is the most mature component.
- **Prelude and standard data structures** (Option, Either, Result, List, Pair, Dictionary): Stable API, comprehensive tests.
- **String library** (850 lines): Feature-complete for practical use.
- **`smyth test`/`smyth run`**: Reliable, used daily in Gas Town CI.
- **File and path libraries**: Well-tested with integration assertions.
- **Typeclass hierarchy** (Equality, Order, Functor, Applicative, Monad, Semigroup, Monoid): Correctly implemented with appropriate instances.

### Prototype-Quality

- **Effect tracking**: Structurally present, semantically unenforced. `computation Effects::pure` and `computation Effects::io` unify freely.
- **DictPass**: Works, but is architectural debt that needs to be eliminated.
- **LSP server**: Feature-minimal; hover and completion are absent.
- **`logic.lq`** (proof library): Technically works, but lacks automation (no `omega`, no tactic language). Proofs must be spelled out term-by-term.
- **TCP and HTTP libraries**: Functional for simple demos but untested under load, no TLS, no HTTP/2.
- **`smyth format`**: Delegates to a Locque-written formatter; correct but not fast (type-checks the formatter before running it).

### Not Production-Ready

- **Compiler branch**: WIP, `unsafeCoerce` present, not integrated.
- **List traversal helpers** (`traverse`, etc.): Not yet implemented.
- **Effect-polymorphic code**: No library support; no effect subtyping.
- **Computation-level recursion**: No structural check; must be structured manually.

---

## 9. Recommended Priorities for the Next 3–5 Work Items

### Priority 1: Eliminate DictPass — Unify Typeclass Resolution into the Evaluator

**Why first:** DictPass is the most dangerous structural debt. Two systems tracking typeclass information (TypeChecker + DictPass) diverge silently. The TypeChecker already resolves instances natively; the evaluator should receive already-elaborated dictionary arguments from the type checker's output rather than running a second AST-traversal pass. This is a moderate-to-large refactor but removes ~966 lines of duplication and reduces failure surface area significantly.

**Files:** `DictPass.hs`, `Eval.hs`, `Main.hs`, `SmythRun.hs`, `SmythTest.hs`, `SmythDump.hs`

### Priority 2: List Traversal Helpers (`traverse`, `map-compute`, `filter-compute`, `fold-compute`, `find-map`)

**Why second:** This is the most frequent stdlib gap encountered when writing real Locque programs. The current workaround (manual `P::fold` with a computation accumulator) is verbose and error-prone. These are straightforward additions with clear semantics from the existing pattern in `prelude.lq`.

**Files:** `lib/list.lq`, `test/list/` (new test files)

### Priority 3: Fix Division-by-Zero and Mod-by-Zero to Return Results

**Why third:** `div-nat-prim` and `mod-nat-prim` currently throw Haskell exceptions on zero denominators (Eval.hs:981, 990). These should return `Result Natural String` (or be wrapped by `lib/arithmetic.lq` as safe-div/safe-mod). This is a small change with a correctness impact on any program doing arithmetic without pre-checking for zero.

**Files:** `Eval.hs`, `lib/arithmetic.lq`, `TypeChecker.hs` (type signature for primitives), `test/arithmetic/`

### Priority 4: `smyth new` Project Scaffolding

**Why fourth:** New project setup is currently manual. A `smyth new <name>` command that creates `Smythfile.lq`, `lib/`, `test/`, and a hello-world entry point would dramatically reduce friction for new work. The implementation is simple (file generation) and the payoff is high for the Gas Town agent workflow where polecats frequently start new sub-projects.

**Files:** `interpreter/src/Smyth.hs` (new subcommand), new `SmythNew.hs`

### Priority 5: Strict Positivity Check for `data` Declarations

**Why fifth:** The type system's guarantee of totality at value level is weakened by the absence of positivity checking. A negative recursive type creates a Turing-complete fixed-point combinator, breaking the "total core" invariant that makes type-level computation safe. Implementing a standard polarity check (syntactic traversal of constructor argument types, marking occurrences as positive/negative based on function argument position) is a bounded-scope addition with high theoretical payoff.

**Files:** `TypeChecker.hs` (new `checkPositivity` function in the `checkDataDef` path), `test/typecheck/` (new expected-failure cases)

---

## Summary Table

| Dimension | Grade | Key Evidence |
|-----------|-------|--------------|
| Interpreter correctness | B+ | 638/638 tests pass; division-by-zero panics; DictPass debt |
| Type system completeness | B | Pi/Sigma/inductive/typeclass solid; no positivity check, no eta, effects nominal |
| Stdlib quality | B | String/logic/prelude strong; list traversal helpers missing; process/effects stubs |
| Test coverage | B- | 638 assertions but no property tests; net/signal/process undertested |
| Tooling | B+ | smyth subcommands complete; `smyth new` missing; LSP minimal |
| Open questions resolution | C+ | Core design resolved; kernel guarantees (positivity, effect safety) open |
| Compiler branch | C | Working proof-of-concept; `unsafeCoerce` present; not integrated |

**Overall:** Locque is a credible prototype of a dependently typed, LLM-friendly language. The interpreter is stable enough for real use (the Gas Town system uses it to run all agent tooling). The main risks are architectural (DictPass debt, nominal effect tracking) rather than correctness bugs. The five priorities above address the highest-leverage improvements given the current state.
