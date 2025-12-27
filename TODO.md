# Locque TODO

## Immediate Priorities

### 1. Grammar Alignment (text-first, no arrows/symbols)
- Parser (M/S):
  - [x] Removed legacy forms (`lambda`, legacy `let … in`, `inspect`/`case`, `do … then …`, `perform io`).
  - [x] Added `let value … be … in … end`, `bind name from … then … [end]`, `perform` (single expr).
  - [ ] Add `function … returns … as|do … end` fully (no reliance on ELam), unified `match … of-type …` cases, `open … exposing … end`.
  - [ ] Switch type grammar to `for-all`/`there-exists`, `computation T`, explicit universes `Type0/1/2`, `::` qualifiers only.
- Types/TypeChecker:
  - [ ] Replace `TFun`/`TComp` with Pi (`for-all`) and computation wrapper; add Sigma (`there-exists`); explicit universes.
  - [ ] Enforce value vs computation split at def/body level; drop `lambda`/`inspect`/`match-prim` paths; adjust primitive types.
  - [ ] Update import/open handling to `::`-only qualifiers and explicit-name `open`.
- Evaluator:
  - [ ] Remove `match-prim` and `perform io`; keep `perform` as single bridge.
  - [ ] Align primitives/env with new types; ensure `match` dispatch uses typed eliminators only.
- Converter/rendering:
  - [ ] Update S-expr/M-expr pretty-printers to new forms; keep full `define` keyword.
- Docs/tests:
  - [ ] Regenerate `.lqs` from `.lq`; refresh examples/tests to new surface once parser/typechecker are aligned.

### 2. Type Classes (after alignment)
- [ ] Design class/instance/constraint syntax that fits the new grammar.
- [ ] Implement parser/typechecker/instance resolution + dictionary pass.
- [ ] Refactor stdlib/tests to use type classes (e.g., unified `match` sugar).

### 3. Error Message Improvements
- [x] Suggestions, SourceLoc/ErrorMsg infra, TypeError locations, runtime fuzzy matching, source threading
- [ ] Show code context once parser locations updated
- [ ] Inline fix hints; color; error codes

## Core Tooling (smyth)
- [x] `smyth test`, `smyth run`; Smythfile.lq; standalone binary (`~/.local/bin/smyth`)
- [ ] `smyth check <file>` (type check only)
- [ ] `smyth repl` (aligned with new grammar)
- [ ] `smyth validate <file>`; `smyth convert <file>`; `smyth fmt <file>`; `smyth doc`

## Language Features
- [ ] Dependent types in checker (universes, Pi/Sigma) per new grammar
- [ ] Exhaustiveness checking for pattern matching
- [ ] Type holes; better inference; rows/records (future)

## Pattern Matching
- [x] Type-specific primitives (current impl)
- [ ] Unified `match` sugar over typed eliminators; exhaustiveness; guards; nested patterns; as-patterns

## Standard Library
- [x] Core: Prelude, List, String, IO, Assert; List.slice
- [ ] Result/Option/Either
- [ ] More string/list ops; filesystem; JSON; HTTP (future)

## Self-Hosting (Dogfooding)
- [ ] M-expr ↔ S-expr converter in Locque
- [ ] Parser/type checker/validator in Locque
- [ ] smyth in Locque (long term)

## Documentation
- [ ] Update reference/tutorials/migration guides to the new grammar (no arrows/lambdas; `function … as|do … end`; `bind … from …`; `perform` without `io`; `::`; `Type0/1/2`)
- [ ] Add type class migration guide once implemented

## Infrastructure
- [ ] CI: run tests/typecheck; releases
- [ ] QA: fuzzers (parser/typechecker), benchmarks, coverage, property-based tests
- [ ] Package ecosystem (future): manager, lockfile, registry, semver, resolution

## Completed ✓
- Test runner (`smyth test`) and modular tests
- `smyth run`; Smythfile.lq; standalone binary
- Comment syntax
- Error message improvements (phases 1-3)
- Assertion counting across suite
