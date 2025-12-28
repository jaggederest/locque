# Test Coverage Summary (current)

The test suite focuses on correctness of the canonical surface grammar and the dependent typechecker, plus stdlib sanity checks.

## What runs by default

- `smyth test` executes `test/main.lq` (prelude + arithmetic + list) and the `error-tests` list from `Smythfile.lq`.

## Additional suites (manual runs)

- `test/typecheck/`: negative typechecker cases.
- `test/typeclass/`: typeclass behavior and dictionary pass.
- `test/errors/`, `test/syntax/`, `test/tools/`, `test/smyth/`: tooling and diagnostics.
- `test/comparison/`, `test/string/`, `test/io/`, `test/shell/`: module-level checks.

## Notes

- Exact assertion counts are reported by `smyth test`.
- `.lqs` files are generated mirrors; author `.lq`.
