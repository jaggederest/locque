# Locque TODO

## Priority (next up)
- `smyth check <file>` (type check only).
- Refactor stdlib/tests to use type classes more broadly (e.g., unified `match` sugar).

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

### Infrastructure
- QA: fuzzers (parser/typechecker), benchmarks (expand coverage), coverage, property-based tests
- Package ecosystem (future): manager, lockfile, registry, semver, resolution
