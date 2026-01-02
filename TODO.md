# Locque TODO

## Priority (next up)
- Webapp: consider refined todo IDs (e.g., `Natural` with `is-positive`) when predicate helpers land.
- HTTP request/response: add refinement hooks for valid requests and response status (future).
- Default instances: consider `Default (Refine A P)` once proof automation exists.
- Hash instances: add `Refine` once higher-kinded params land (`List`/`Pair` done).
- Semigroup: add `NonEmptyList` instance (`List`/`String` done).
- Arithmetic refinements: add instances for `Positive`/`NonZero` and refine div/mod inputs/outputs.
- Display: add `Display (Refine A P)` once higher-kinded params land (`List`/`Pair` done).
- Order: add instances for refined types via `refine-value` (e.g., Positive, NonZero) once higher-kinded params land.
- Dictionary: consider `NonEmptyDictionary` refinement, plus `keys`/`values` helpers.
- List: consider a `Sorted` refinement for `sort-by`.
- Natural: consider digit list conversions for proof-friendly properties.
- Arithmetic: add proof lemmas (add-zero, add-comm, etc.) and refine div/mod inputs/outputs (Positive/NonZero).
- Shell: consider refined `ExistingPath` for file operations.
- Net/TCP: consider `ValidPort` refinement for safe APIs.

## Diagnostics
- Show code context once parser locations updated.

## Performance
- Consider caching normalization/transform outputs per module to speed repeated runs beyond the run cache.

## Language features (future)
- Definitional equality: eta reduction for functions.
- Type holes; better inference; rows/records.
- Higher-kinded type parameters (plan):
  - Accept non-`TypeN` type params (e.g., `F (for-all x as Type0 to Type0)`).
  - Update typeclass/classType handling to support higher-kinded params.
  - Extend DictPass type-parameter detection (`isTypeParam`) and constraint resolution.
  - Add initial Functor-style class and instance tests (Option/Either/Result).

## Theorem proving
- Induction/recursor generation for `data` (with dependent motives).
- Definitional equality: iota reduction for match (match computation in types).
- Refinement helpers: expand common predicate library beyond NonEmpty/Positive/NonZero.
- Decidable predicates: add string helpers beyond equality.

## Parked backlog

### Self-hosting
- M-expr ↔ S-expr converter in Locque
- Parser/type checker/validator in Locque
- smyth in Locque (long term)

### Infrastructure
- QA: fuzzers (parser/typechecker), benchmarks (expand coverage), coverage, property-based tests
- Package ecosystem (future): manager, lockfile, registry, semver, resolution
