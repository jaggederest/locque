# Locque TODO

## Priority (next up)
- Refactor stdlib/tests to use type classes more broadly.
- Move `get-line` from `prelude` to `io` and update call sites.
- Pair/List equality: add proof-friendly wrappers in `pair::eq` and `list::eq` (Decidable form) and consider enabling Equality instances.
- Result/logic bridge: consider `Result -> Decidable` helpers for proof-oriented workflows.
- Path refinements: add `NonEmptySegments` (Refine over `List String`) and helpers like `segments-nonempty`.
- List eq: add explicit `Decidable (equal (List A) x y)` wrapper in `list::eq`.
- Webapp: consider refined todo IDs (e.g., `Natural` with `is-positive`) when predicate helpers land.
- HTTP request/response: add refinement hooks for valid requests and response status (future).
- String refinements: consider `AsciiChar`/`LowerChar` refinements.
- Default instances: consider `Default (Refine A P)` once proof automation exists.
- Equality instances: enable `Equality-List` and `Equality-Pair` using existing decidable helpers.
- Hash instances: allow `Refine` via `refine-value`, and consider `Hash` for `List`/`Pair`.
- Semigroup/Monoid: add `List` and `String` instances; add `Semigroup` for `NonEmptyList` later.
- Arithmetic refinements: add `Positive` predicate and instances once predicate helpers exist.
- Display: add `Display (Refine A P)` and consider `Display` for `List`/`Pair` formatting.
- Order: add instances for refined types via `refine-value` (e.g., Positive, NonZero).
- Dictionary: consider `NonEmptyDictionary` refinement, plus `keys`/`values` helpers.
- List: consider a `Sorted` refinement for `sort-by`.
- Natural: add `Positive`/`NonZero` refinements; consider digit list conversions for proof-friendly properties.
- Option: add `to-decidable` and proof-style combinators once Decidable conventions solidify.
- Arithmetic: add proof lemmas (add-zero, add-comm, etc.) and refine div/mod inputs/outputs (Positive/NonZero).
- Either: add proof helpers (Left -> Not P, Right -> P) for `Decidable` conversions.
- Shell: consider refined `ExistingPath` for file operations.
- Logic: add `Decidable` combinators (and/or/not), `congruence3`, and clearer `rewrite` helpers.
- Net/TCP: consider `ValidPort` refinement for safe APIs.
- Assert: add `assert-decidable` (or `assert-result`) for proof‑driven tests.

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

## Theorem proving
- Induction/recursor generation for `data` (with dependent motives).
- Proof combinators: expand beyond `transport` to include multi-arg congruence and rewrite helpers.
- Definitional equality: iota reduction for match (match computation in types).
- Refinement helpers: `Refine A P` alias, projection helpers, and common predicates.
- Decidable predicates: combinators (and/or/not), list/pair/string helpers beyond equality.

## Parked backlog

### Self-hosting
- M-expr ↔ S-expr converter in Locque
- Parser/type checker/validator in Locque
- smyth in Locque (long term)

### Infrastructure
- QA: fuzzers (parser/typechecker), benchmarks (expand coverage), coverage, property-based tests
- Package ecosystem (future): manager, lockfile, registry, semver, resolution
