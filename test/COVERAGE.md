# Test Coverage Audit (current)

This is a high-level audit of what is covered, not a per-assertion inventory.

## Covered

- Core syntax: `function`, `let value`, `compute`, `perform`, `bind`, `sequence`.
- Dependent types: `for-all`, `there-exists`, `pack`/`unpack`, explicit universes, `lift`.
- Typed `match` with mandatory binder/returns and exhaustiveness.
- Prelude primitives: list, pair, bool, string, arithmetic, comparison.
- Typeclass usage at the surface via DictPass (Equality + `assert-eq`).
- Smyth runner basics and assertion counting.

## Gaps / future work

- Full typeclass instance resolution without DictPass (typechecker now checks classes/instances/constraints).
- Eta conversion in definitional equality.
- Deeper tooling coverage: conversion roundtrips, validator diagnostics, CLI flags.
- Error message coverage beyond fuzzy matching smoke tests.
