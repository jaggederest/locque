# Locque COMPLETE

## Grammar Alignment (text-first, no arrows/symbols)

### Parser (M/S)
- [x] Removed legacy forms (`lambda`, legacy `let … in`, `inspect`/`case`, `do … then …`, `perform io`).
- [x] Added `let value … be … in … end`, `bind name from … then … end`, `perform` (single expr).
- [x] Implemented `function … returns … value|compute … end`, unified `match … of-type … as … returns …` cases, `open … exposing … end`.
- [x] Switched `define … as <Value>` (no `as value|computation`) and added `compute … end` as explicit computation-value wrapper.
- [x] Switched type grammar to `for-all`/`there-exists`, `computation T`, explicit universes `Type0/1/2`, `::` qualifiers only.
- [x] Require parenthesized non-atomic types in function params and match binders to avoid ambiguity.
- [x] Add `data … in TypeN … end` definitions and `case <Type::Ctor>` match syntax (M/S), remove legacy `*-case`.

### Types/TypeChecker
- [x] Replace `TFun` with Pi (`for-all`); add Sigma (`there-exists`) and explicit universes.
- [x] Allow `function ... returns TypeN ... end` as a value-level type (used in type arguments via parentheses).
- [x] Enforce compute-wrapper semantics: computations are values (`compute <Comp> end`) and run only via `perform`.
- [x] Drop legacy `lambda`/`inspect` paths; remove computation identifiers; adjust primitive types.
- [x] Update import/open handling to `::`-only qualifiers and explicit-name `open`.
- [x] Add dependent substitution/elaboration for `for-all`/`there-exists` (currently structural only).
  - [x] Binder-aware substitution/free-vars (params/constraints/typeclass binders).
  - [x] Binder-aware normalization (for-all/there-exists/functions/match).
  - [x] Preserve dependent types on partial application in normalization.
  - [x] Add typecheck-only shadowing test (global vs bound names).

### Evaluator
- [x] Execute computation values only when performed; remove computation identifiers.
- [x] Remove `match-prim`; keep `perform` as single bridge.
- [x] Align primitive env with constructor names; drop `nil-prim`/`cons-prim`/`pair-prim` aliases; add `Boolean::true/false` and `Unit::tt` runtime bindings.

### Converter/rendering
- [x] Update S-expr/M-expr pretty-printers to new forms; keep full `define` keyword; ban infix separators in S-expr.

### Docs/tests
- [x] Update core libs used by tests to new syntax (prelude/assert/io/string/comparison).

## Type Classes (after alignment)
- [x] Design class/instance/constraint syntax that fits the new grammar.
- [x] Parser support for `typeclass`, `instance`, `requires`.
- [x] Dictionary pass: resolve constraints, insert method params, and resolve instances.
- [x] Update stdlib/tests to use `Equality` + `assert-eq` (explicit type args).
- [x] Typechecker native typeclass support (classes/instances/constraints); DictPass still used for dictionary insertion.
- [x] Use stdlib wrappers in `typeclass::equality` instances (no raw prims).
- [x] Higher-kinded type params (`TypeFunction`) and Functor class with Option/Either/Result instances + tests.
- [x] Applicative/Monad typeclasses with Option/Either/Result/List instances + tests.

## Error Message Improvements
- [x] Suggestions, SourceLoc/ErrorMsg infra, TypeError locations, runtime fuzzy matching, source threading.

## Core Tooling (smyth)
- [x] `smyth test`, `smyth run`; Smythfile.lq; standalone binary (`~/.local/bin/smyth`).
- [x] `smyth bench` with benchmark rollup (`test/bench.lq`) and thresholds.
- [x] `locque-lsp`: diagnostics, go-to-definition, document symbols (Haskell).
- [x] Run cache in `tmp/locque/cache` for annotated modules + ctor arity; keyed by module digest; used by `smyth test/run`.
- [x] Reuse import env from digest to avoid double import loading on cache-miss typecheck/normalize.
- [x] Cache per-module normalized/transform results for imports via RunCache in evaluator.

## Compiler (Haskell backend bootstrap)
- [x] M0: Core IR AST + pretty-printer + parser round-trips + conversion stubs (typed CBPV Core).
- [x] M1: Erasure pass (types/proofs/refinements → runtime data only), preserving match/binders and module name.
- [x] M2: LocqueRuntime shim (Comp/run/bind/return/perform), builtin type aliases, minimal primitive wrappers, tests.
- [x] M3: Core → Haskell codegen (single module/entrypoint; imports LocqueRuntime; maps builtins).
- [x] M4: `smyth emit-hs` CLI (outputs .hs via codegen; supports `--out-dir`).
- [x] M5: Golden tests (compile/run hello + sequence; compare output vs interpreter).
- [x] M6: Reuse run cache for `emit-hs` (skip typecheck/transform when cached).
- [x] M7: `smyth compile` wrapper (ghc build, binary output, optional arg passthrough with `--`).
- [x] Compiled test suite runs via `smyth compile test/main.lq` and `tmp/locque/bin/main`.
- [x] M9: Dictionary lowering (EDict/EDictAccess) into runtime representation for codegen.

## Language Features
- [x] Dependent types in checker (universes, Pi/Sigma) per new grammar.
- [x] Equality types + transport (`equal`, `reflexive`, `rewrite`).
- [x] Induction/recursor generation for `data` with dependent motives.
- [x] Decidable equality / refinement tooling (e.g., Character length 1).
- [x] Explicit universe lifting (`lift`/`up`/`down`) for strict universes.
- [x] there-exists intro/elim via `pack`/`unpack`.
- [x] Definitional equality: iota reduction for `match` and `unpack`.
- [x] Exhaustiveness checking for pattern matching.
- [x] Effect-annotated computation types (`computation E T`) with `Effects::any` default for legacy `computation T`.

## Pattern Matching
- [x] Type-specific primitives (current impl).
- [x] Unified `match` over constructors (data + built-ins); exhaustiveness by constructor set; guards/nested patterns later.

## Standard Library
- [x] Core: Prelude, List, String, IO, Assert; List.slice.
- [x] File primitives + file module (helpers, walk, stat).
- [x] CLI helpers (args/options).
- [x] Path utilities (join/dirname/basename/ext/is-absolute).
- [x] Result/Option/Either data types in prelude.
- [x] Simplify `assert` to use `assert-eq` without per-type wrappers.
- [x] Generalize `list::drop-until` via `Equality`.
- [x] Typeclass instances: `Equality`/`Hash`/`Display` for `List`/`Pair`, `Semigroup`/`Monoid` for `List`/`String`.
- [x] Add `NonEmptyList`/`NonEmptyString` refinements with total accessors; re-export `Refine` via prelude; update tokenizer/formatter indent checks to use nonempty helpers.
- [x] Proof toolkit: `Decider`, `decidable-*` combinators (and/or/not/map/flag), `congruence3`, `rewrite-to`/`rewrite-from`, `assert-decidable`.
- [x] Proof-oriented conversions: `Option::to-decidable`, `Result::to-decidable`, `Either::to-decidable`, list/pair equality helpers (`decide-eq-flag`/`decide-eq-proof`).
- [x] Refinements: `Natural::NonZero`/`Positive` and `Path::NonEmptySegments` helpers.
- [x] String refinements: `AsciiChar`/`LowerChar` with constructors and accessors.
- [x] Refactor stdlib/tests to use typeclass equality for string comparisons (formatter/webapp/http/shell).
- [x] Move `get-line` to `io` and update call sites; move decidable equality wrappers into `logic`.

## Documentation
- [x] Document `lift`/`up`/`down` in grammar and reference docs.

## Milestones (rolled up)
- Test runner (`smyth test`) and modular tests.
- `smyth run`; Smythfile.lq; standalone binary.
- Comment syntax.
- Error message improvements (phases 1-3).
- Assertion counting across suite.
