# Locque TODO

## Priority (next up)
- `smyth check <file>` (type check only).
- Refactor stdlib/tests to use type classes more broadly (e.g., unified `match` sugar).
- Update reference/tutorials/migration guides to the new grammar (no arrows/lambdas; `function … value|compute … end`; `bind … from …`; `perform` without `io`; `::`; `Type0/1/2`).
- Make `assert-eq` the core implementation and keep `assert-eq-*` as thin wrappers (`lib/assert.lq`).
- Move `typeclass::equality` instances to stdlib wrappers instead of raw prims (`lib/typeclass/equality.lq`).
- Generalize `drop-until` to a typeclass-based list helper (e.g., `member`/`drop-until` via `Equality`) (`lib/list.lq`).

## Tooling
- LSP for Locque (create + enable), ideally implemented in Locque.

## Diagnostics
- Show code context once parser locations updated.

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
