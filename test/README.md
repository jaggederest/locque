Test harness sketch

- Runner: `smith test` (future) discovers test modules under `test/` and runs a designated entry (default `main`) expecting success to be signaled by returning `tt` (unit) with no assertion failures.
- Assertions: use explicit computation-level assertions (e.g., `assert-eq-nat`, `assert-eq-string`, `assert-true`). These live in the computation world and fail the test by raising an effect (to be defined).
- No implicit magic: tests are ordinary modules; `main` is a `computation`.
- File forms: M-exp tests `.lq`; S-exp mirrors `.lqs`.

Initial smoke tests (spec intent)
- Equality (nat): `assert-eq-nat (add-nat 2 3) 5`
- Equality (string): `assert-eq-string "hi" "hi"`
- Output: `perform io (print "Hello, test!")`
- Composition: run multiple assertions, ensure sequencing via `bind`.

See `test/00_basics.lq` and `test/00_basics.lqs` for concrete shapes.
