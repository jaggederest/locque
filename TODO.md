# Locque TODO

## Priority (next up)
- Align primitives/env with new types; ensure `match` dispatch uses typed eliminators only.
- Result/Option/Either data types + tests.
- `smyth check <file>` (type check only).

## Tooling
- LSP for Locque (create + enable), ideally implemented in Locque.

## Standard library & typeclasses
- Refactor stdlib/tests to use type classes more broadly (e.g., unified `match` sugar).

## Diagnostics
- Show code context once parser locations updated.

## Documentation
- Update reference/tutorials/migration guides to the new grammar (no arrows/lambdas; `function … value|compute … end`; `bind … from …`; `perform` without `io`; `::`; `Type0/1/2`).

## Language features (future)
- Definitional equality: eta reduction for functions.
- Type holes; better inference; rows/records.

## Parked backlog

### Self-hosting
- M-expr ↔ S-expr converter in Locque
- Parser/type checker/validator in Locque
- smyth in Locque (long term)

### Docs
- Add type class migration guide once implemented

### Infrastructure
- QA: fuzzers (parser/typechecker), benchmarks, coverage, property-based tests
- Package ecosystem (future): manager, lockfile, registry, semver, resolution
