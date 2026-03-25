---
name: locque
description: Orient to the Locque repo, understand how to run and test programs, and follow key project conventions. Use this when starting work in the locque repo.
---

# Locque Repo Orientation

Use this skill when starting work in the locque repo or when you need a reference for project conventions, tooling, and the "definition of done."

## What Locque is

Locque is a dependently typed, keyword-led language with a strict CBPV split (values vs computations), a 1:1 M-expr ↔ S-expr mapping, explicit effects, and no implicit coercions. It is built almost entirely by LLMs.

## Project layout

| Path | Purpose |
|------|---------|
| `grammar.md` | Canonical surface syntax and S-expr mapping — **source of truth** |
| `AGENTS.md` | Project conventions, tooling, working agreements |
| `lib/` | Standard library (`.lq` M-expr files) |
| `test/` | Tests — one rollup per `lib/**` module |
| `interpreter/` | Haskell interpreter — **polecats normally do not touch this** |
| `Smythfile.lq` | Project config (error-tests list) |
| `skills/` | Skill files for agents (including this one) |

## Tooling

The primary tool is `smyth` (installed at `~/.local/bin/smyth`):

```bash
smyth test              # Run full test suite (entry: test/main.lq)
smyth test <file>       # Run one test file
smyth test --slow       # Show slowest test suites
smyth bench             # Run benchmarks (test/bench.lq)
smyth run <file>        # Type-check and run a .lq file
smyth dump core <file>  # Inspect elaborated output
```

**Always run `smyth test` after any change** — it is the gate for "done."

## File conventions

- Source: `.lq` (M-expr, human-authored). Do NOT handwrite `.lqs`.
- Paths: **lowercase only** — `lib/some/module.lq`, `test/some/module.lq`
- Every `lib/**/*.lq` must have a matching `test/**/*.lq`
- Module name `Some::Module` maps to file `lib/some/module.lq`
- Test module `Test::Some::Module` maps to `test/some/module.lq`

## Syntax quick-reference

```locque
# Definition
define transparent name as <Value>
define opaque       name as <Value>

# Function
define transparent inc as
  function x Natural returns Natural value
    Ar::add x 1
  end

# Computation value (effect-bearing)
define transparent main as compute
  perform (IO::print "hello")
end

# Data type
define transparent Option as data A Type0 in Type0
  case Option::none of-type Option A
  case Option::some of-type for-all x as A to Option A
end

# Match
match opt of-type (Option A) as ignored returns A
  case Option::none as fallback
  case Option::some with x A as x
end

# Import and qualify
import string as S   # Use S::concat, S::eq, etc.
open S exposing concat end  # Then use concat directly
```

Key rules:
- Qualification uses `::` only (never `.`)
- Application is prefix, left-associative
- `end` closes every block (`function`, `compute`, `match`, `data`, `module`, `open`, `bind`, `typeclass`, `instance`, `pack`, `unpack`)
- List literals: `[]` and `[a, b]` (commas required); empty lists need `of-type [] (List A)` when no type is in scope
- Use `ignored` instead of `_` for unused binders
- Effects are explicit: `computation T` type, bridged by `perform`; sequences via `bind name from comp then ... end`

## Writing tests

```locque
import assert as A

module test::my_module contains
  define transparent run-tests as compute
    bind ignored from
      A::assert-eq Natural (Ar::add 1 2) 3
    then
      perform (IO::print "tests passed")
    end
  end
end
```

- Use `A::assert-eq` with an explicit type argument
- Add your test module to `test/main.lq` imports and the runner

## What polecats do (and do not) change

**Typical polecat work (library `.lq` files):**
- New library functions in `lib/`
- New or updated tests in `test/`
- Syntax fixes, refactors within `.lq` files
- Adding/fixing data types, typeclasses, instances

**Off-limits for polecats (interpreter):**
- `interpreter/` — Haskell source. Do NOT modify unless the bead explicitly asks for interpreter changes. The interpreter is a separate build (GHC required) and is rarely the right place to fix library-level issues.
- Do not add new primitives without explicit instruction from the bead.

## Definition of done (checklist)

```
[ ] smyth test — all tests pass (no deletions to make tests pass)
[ ] New lib file has a matching test file
[ ] Paths are lowercase
[ ] No handwritten .lqs files
[ ] No changes to interpreter/ (unless bead explicitly requires it)
[ ] git status clean
[ ] gt done
```

## Related skills

- `locque-authoring` — detailed authoring rules: data/match forms, module syntax, CBPV rules, templates
