# Locque Test Specification (current)

This document describes what the test suite is expected to validate, not every individual assertion.

## Execution model

- `smyth test` runs `test/main.lq` plus the `error-tests` list in `Smythfile.lq`.
- `smyth test <file>` runs a single test module.
- `smyth run <file>` typechecks and runs a program file.

## Coverage goals

### Core language
- CBPV split (values vs computations): `compute`, `perform`, `bind`, `sequence`.
- Dependent types: `for-all`, `there-exists`, `pack`/`unpack`, explicit universes.
- Typed match with mandatory binder + returns, plus exhaustiveness enforcement.
- Explicit effects and no implicit coercions.

### Standard library
- Prelude primitives (list, pair, bool, string, arithmetic).
- Arithmetic and comparison modules.
- List utilities (including `drop/take/slice`, `nth`, `drop-until`).
- String utilities.
- IO and shell (where applicable).
- Typeclass-backed `assert-eq` behavior.

### Tooling & errors
- Typechecker negative tests registered in `Smythfile.lq`.
- Error message smoke tests (fuzzy matching).
- Syntax and tooling basics (`smyth run`, assertion counting).

## Structure

- `test/main.lq`: rollup entry (currently prelude + arithmetic + list).
- `test/prelude/`: language/CBPV/dependent tests.
- `test/typecheck/`: negative typechecker cases.
- `test/<module>/`: stdlib module tests.
- `test/errors/`, `test/syntax/`, `test/tools/`, `test/smyth/`: diagnostics and tooling.

## Invariants

- Every `lib/**/*.lq` must have a matching `test/**/*.lq` path.
- `lib/**/*.lq` and `test/**/*.lq` paths are lowercase.
