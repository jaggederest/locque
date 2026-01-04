# Locque TODO

## Strategic assessment
- Compiler backend now builds and runs the full test suite; next risk is semantics drift (dictionary values, erased types).
- Short term focus should stay on compiler correctness/observability before deeper type system features.
- Stdlib is stable; refinements/proofs and effect typing remain the main growth areas.

## Priority (next up)
- Compiler: debug build mode (optional) with source-map + selected type annotations.
- Compiler: simplify constant matches for nullary constructors (Boolean/Unit/List/etc.).
- Smyth: add `smyth new` to scaffold a project (Smythfile.lq, lib/, test/).
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

## Stdlib follow-ups (Applicative/Monad)
- Add `traverse`/`sequence` for `List` using `Applicative` (beyond the existing computation sequence helpers).
- Add `Option`/`Either`/`Result`/`List` convenience wrappers (`pure`, `apply`, `and-then`) aligned with new typeclasses.
- Consider `Dictionary` functorial map (value mapping) and possible `Functor` instance.
- Consider `computation` monad helpers (effect-indexed) once effect polymorphism is formalized.

## Compiler (Haskell backend bootstrap)
### Milestones (incremental)
- M8: Debug build mode (optional): keep source map/selected type annotations.
- Lowering cleanup: simplify constant matches for nullary constructors (Boolean/Unit/List/etc.).

### Project decisions (locked)
- Typed CBPV Core IR; no ANF for now.
- Full erasure in normal builds; debug flag later.
- Dictionary-passing preserved; monomorphization later.
- Builtins mapping: Option→Maybe, Either→Either, Result→Either, List→[], Pair→(,), Natural→Integer, String/Character→Text (upgrade later).
- Computations: `newtype Comp a = Comp (IO a)` in runtime.
- Haskell backend emits a single module per entrypoint; `LocqueRuntime` is shared.

## Diagnostics
- Show code context once parser locations updated.

## Performance
- Typecheck profiling + targeted caching or parallelism once we have a stable baseline.

## Language features (future)
- Definitional equality: eta reduction for functions.
- Type holes; better inference; rows/records.

## Theorem proving
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
