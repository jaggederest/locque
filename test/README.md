Test harness overview

- Runner: `smyth test` runs `test/main.lq` and the `error-tests` list from `Smythfile.lq`.
- Single test: `smyth test <file>` runs that module as a test.
- Program run: `smyth run <file>` typechecks and runs any `.lq/.lqs`.
- Assertions: prefer `assert::assert-eq` with an explicit type argument; primitive asserts are still available for low-level checks.
- Entrypoint: tests are ordinary modules; `main` must evaluate to a computation value (use `compute` and `sequence`).

## Organization

- `test/main.lq`: rollup entry; currently runs prelude + arithmetic + list suites.
- `test/prelude/`: core language and CBPV tests.
- `test/typecheck/`: negative typechecker cases (registered in `Smythfile.lq`).
- `test/arithmetic/`, `test/list/`, `test/string/`, `test/comparison/`, `test/io/`, `test/shell/`, `test/typeclass/`: stdlib/module tests.
- `test/errors/`, `test/syntax/`, `test/tools/`, `test/smyth/`: tooling and diagnostics.

## Policies

- Every `lib/**/*.lq` must have a matching `test/**/*.lq` path (tooling enforces this).
- File paths for `lib/**/*.lq` and `test/**/*.lq` must be lowercase (tooling enforces this).
- `.lqs` files are generated mirrors; author `.lq` and use the converters.

## Running tests

```bash
# Run the full suite (test/main.lq + error-tests)
smyth test

# Run a single test module
smyth test test/prelude/basics.lq

# Typecheck only
locque-interpreter typecheck test/list.lq
```
