# Locque TODO

## Priority (next up)
- Compiler bootstrap: Haskell backend via typed CBPV Core IR (emit-hs first, compile later).
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
- Add `traverse`/`sequence` for `List` using `Applicative` (plus `sequence-unit` helpers).
- Add `Option`/`Either`/`Result`/`List` convenience wrappers (`pure`, `apply`, `and-then`) aligned with new typeclasses.
- Consider `Dictionary` functorial map (value mapping) and possible `Functor` instance.
- Consider `computation` monad helpers (effect-indexed) once effect polymorphism is formalized.

## Compiler (Haskell backend bootstrap)
### Milestones (incremental)
- M2: Runtime shim (`LocqueRuntime`): `newtype Comp a = Comp (IO a)`, bind/perform/return, primitive wrappers.
 - M4: CLI: `smyth emit-hs <file>` (writes .hs next to input or in tmp/locque/gen).
- M5: Golden tests: compile/run `hello` + small stdlib sample; compare output vs interpreter.
- M6: Cache-friendly build: reuse existing run cache for compiled artifacts (optional).
- M7: `smyth compile` wrapper (ghc build, binary output, args passthrough).
- M8: Debug build mode (optional): keep source map/selected type annotations.

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
