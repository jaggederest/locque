Test harness sketch

- Runner: `smith test` (future) discovers test modules under `test/` and runs a designated entry (default `main`) expecting success to be signaled by returning `tt` (unit) with no assertion failures.
- Assertions: use explicit computation-level assertions (e.g., `assert-eq-nat`, `assert-eq-string`, `assert-true`). These live in the computation world and fail the test by raising an effect (to be defined).
- No implicit magic: tests are ordinary modules; `main` is a `computation`.
- File forms: M-exp tests `.lq`; S-exp mirrors `.lqs`.

## Test Organization

### Current Structure (Modular)
- `features/`: Language feature tests (basics, IO, lambda, fold, match, primitives, etc.)
- `typecheck/`: Type checker tests (monomorphic, polymorphic, bidirectional, etc.)
- `smyth/`: Smyth tool integration tests (run, test commands, assertion counting)
- `errors/`: Error message tests (fuzzy matching, suggestions)
- `syntax/`: Syntax tests (comments, validation)
- `conversion/`: M-expr ↔ S-expr conversion tests

### Legacy Structure (Numbered)
- `00-39`: Feature tests (basics, IO, lambda, fold, match, primitives, etc.)
- `40-49`: Validator/negative tests (malformed code, error cases)
- `50-59`: Conversion/metaprogramming tests (roundtrip, AST manipulation)
- `99-XX`: Type checker tests (monomorphic, polymorphic, etc.)

**Note:** Legacy numbered tests at root level will be gradually migrated to the modular structure.

## Test Expectations

- All tests must pass at all times (no known failures)
- New features require new tests before merge
- Both `.lq` and `.lqs` versions should exist where appropriate
- Negative tests (40s) should test error handling explicitly
- **New tests should include type annotations** (use `function x of-type T produce ...`)
- Legacy tests may require `--skip-typecheck` flag

## Running Tests

```bash
# Run single test (with type checking)
cabal run locque-interpreter -- --run-lq test/99_typecheck_mono.lq

# Run legacy test (without type checking)
cabal run locque-interpreter -- --skip-typecheck --run-lq test/00_basics.lq

# Run validator test
cabal run locque-interpreter -- --run-lqs test/40_small_bad_code.lqs

# Type check only (don't run)
cabal run locque-interpreter -- --typecheck test/99_typecheck_mono.lq
```

Initial smoke tests (spec intent)
- Equality (nat): `assert-eq-nat (add-nat 2 3) 5`
- Equality (string): `assert-eq-string "hi" "hi"`
- Output: `perform io (print "Hello, test!")`
- Composition: run multiple assertions, ensure sequencing via `bind`.

See `test/00_basics.lq` and `test/00_basics.lqs` for concrete shapes.
