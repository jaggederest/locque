# Locque TODO

## Strategic assessment
- Short term focus should stay on interpreter correctness/observability before deeper type system features.
- Stdlib is stable; refinements/proofs and effect typing remain the main growth areas.

## Roadmap (prioritized)
### Near term
- Smyth: add `smyth new` to scaffold a project (Smythfile.lq, lib/, test/).
- Stdlib ergonomics: List traversal helpers (`traverse`, `map-compute`, `filter-compute`, `find-map`); `fold-compute` is already in prelude.
- Stdlib ergonomics: Option/Either/Result helpers (`with-context`, `map-error` for Option/Either, additional conversions); `and-then` and `unwrap-or` are already implemented, and Result already has `map-error`.
- Stdlib ergonomics: String `split-once` with consistent trailing-empty behavior (`lines`/`unlines` are already implemented).
- Stdlib ergonomics: File/Process Result wrappers (`read-result`, `run-result`, `run-ok`) with consistent error formatting.
- Stdlib ergonomics: CLI structured option parsing that returns flags/options/positionals (current parse returns positionals + key/value pairs).
- Diagnostics: contextual error helper combinator for structured breadcrumbs.
- Tests: helper for per-suite assertion counts in verbose output.

### Mid term
- Stdlib: Applicative/Monad convenience wrappers (`pure`, `apply`, `and-then`) aligned with new typeclasses.
- Dictionary: functorial map (value mapping) + possible `Functor` instance.
- Computation: effect-indexed monad helpers once effect polymorphism is formalized.
- Diagnostics: show code context once parser locations updated.
- Performance: typecheck profiling + caching/parallelism once a stable baseline exists.
- Language ergonomics: computation `let`/`do`-style sugar to reduce `perform (let ...)`.
- Language ergonomics: allow `match` to return computations directly (reduce extra `compute` nesting).

### Later
- Refinement/typeclass expansion: Default/Display/Order/Hash for `Refine` once proof automation + higher-kinded params land.
- Semigroup: `NonEmptyList` instance (`List`/`String` done).
- Arithmetic refinements + lemmas: `Positive`/`NonZero` instances; div/mod refinements; add-zero/add-comm proofs.
- Dictionary: `NonEmptyDictionary` refinement + `keys`/`values` helpers.
- List: `Sorted` refinement for `sort-by`.
- Natural: digit list conversions for proof-friendly properties.
- Shell: `ExistingPath` refinement for file operations.
- Net/TCP: `ValidPort` refinement for safe APIs.
- HTTP request/response: refinement hooks for valid requests and response status.
- Webapp: refined todo IDs (e.g., `Natural` with `is-positive`) once predicate helpers land.
- Theorem proving: expand predicate library; add decidable predicate helpers beyond equality.
- Language features: definitional equality (eta), type holes, better inference, rows/records.

## Parked backlog

### Self-hosting
- M-expr ↔ S-expr converter in Locque
- Parser/type checker/validator in Locque
- smyth in Locque (long term)

### Infrastructure
- QA: fuzzers (parser/typechecker), benchmarks (expand coverage), coverage, property-based tests
- Package ecosystem (future): manager, lockfile, registry, semver, resolution
