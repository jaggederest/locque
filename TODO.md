# Locque TODO

## Priority (next up)
- Refactor stdlib/tests to use type classes more broadly (e.g., unified `match` sugar).
- Add tests for logic primitives (`logic::False`, `logic::Not`, `logic::Decidable`).

## Diagnostics
- Show code context once parser locations updated.

## Language features (future)
- Definitional equality: eta reduction for functions.
- Type holes; better inference; rows/records.
- Higher-kinded type parameters (plan):
  - Accept non-`TypeN` type params (e.g., `F (for-all x as Type0 to Type0)`).
  - Update typeclass/classType handling to support higher-kinded params.
  - Extend DictPass type-parameter detection (`isTypeParam`) and constraint resolution.
  - Add initial Functor-style class and instance tests (Option/Either/Result).

## Parked backlog

### Self-hosting
- M-expr ↔ S-expr converter in Locque
- Parser/type checker/validator in Locque
- smyth in Locque (long term)

### Infrastructure
- QA: fuzzers (parser/typechecker), benchmarks (expand coverage), coverage, property-based tests
- Package ecosystem (future): manager, lockfile, registry, semver, resolution
