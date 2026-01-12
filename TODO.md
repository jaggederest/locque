# Locque TODO

## Strategic assessment
- Compiler backend now builds and runs the full test suite; next risk is semantics drift (dictionary values, erased types).
- Short term focus should stay on interpreter correctness/observability before deeper type system features.
- Stdlib is stable; refinements/proofs and effect typing remain the main growth areas.

## Roadmap (prioritized)
### Near term
- Compiler: debug build mode (optional) with source-map + selected type annotations.
- Compiler: simplify constant matches for nullary constructors (Boolean/Unit/List/etc.).
- Smyth: add `smyth new` to scaffold a project (Smythfile.lq, lib/, test/).
- Stdlib ergonomics: computation helpers (`when`, `unless`, `guard`, `tap`, `map-comp`, `bind-comp`, `and-then`, `sequence-when`).
- Stdlib ergonomics: List traversal helpers (`traverse`, `map-compute`, `filter-compute`, `fold-compute`, `find-map`).
- Stdlib ergonomics: Option/Either/Result helpers (`unwrap-or`, `and-then`, `map-error`, `with-context`, conversions).
- Stdlib ergonomics: String `lines`/`unlines`/`split-once` with consistent trailing-empty behavior.
- Stdlib ergonomics: File/Process Result wrappers (`read-result`, `run-result`, `run-ok`) with consistent error formatting.
- Stdlib ergonomics: CLI structured option parsing that returns flags/options/positionals.
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

## Compiler backend (context)
### Project decisions (locked)
- Typed CBPV Core IR; no ANF for now.
- Full erasure in normal builds; debug flag later.
- Dictionary-passing preserved; monomorphization later.
- Builtins mapping: Option→Maybe, Either→Either, Result→Either, List→[], Pair→(,), Natural→Integer, String/Character→Text (upgrade later).
- Computations: `newtype Comp a = Comp (IO a)` in runtime.
- Haskell backend emits a single module per entrypoint; `LocqueRuntime` is shared.

## Parked backlog

### Self-hosting
- M-expr ↔ S-expr converter in Locque
- Parser/type checker/validator in Locque
- smyth in Locque (long term)

### Infrastructure
- QA: fuzzers (parser/typechecker), benchmarks (expand coverage), coverage, property-based tests
- Package ecosystem (future): manager, lockfile, registry, semver, resolution
