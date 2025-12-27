Project overview for agents

- Goal: locque is an LLM-friendly, keyword-led, dependently typed language with a strict CBPV split (values vs computations), 1:1 S-exp ↔ M-exp mapping, explicit opacity, explicit effects, no implicit coercions.
- Core constructs: `define (transparent|opaque) name as <value|computation>`, functions only via `function ... returns ... as|do ... end`, dependent types with `for-all x as A to B` and `there-exists x as A in B`, typed `match ... of-type ...` with fixed cases, application is prefix/left-assoc, modules/imports/opens explicit, qualification uses `::` only, comments `#`/`/* */` (M) and `;`/`#| |#` (S).

Interpreter (Haskell, `interpreter/`)
- AST (current impl): literals (Natural, String, Boolean, Unit), vars, apps, lambdas/functions, defs (transparency/kind), values vs computations.
- Evaluator: values include closures, primitives, lists, pairs, unit, bool. Primitives: add/sub nat; eq nat/string (Boolean); concat/length/split-on/join/trim strings; print/read-file/write-file; assert eq nat/string/bool; match (list/bool/pair); filter; fold (left); map (temp); append; pair/fst/snd/pair-to-list; drop-until; not; if-bool; many string/list utilities; shell; error. (To be aligned with new surface: `perform` without `io`, `match-prim`/`inspect` to be removed.)
- Import resolution:
  - Module names use `::` separator (e.g., `Some::Module::Name`)
  - Map to lowercase file paths: `Some::Module::Name` → `lib/some/module/name.lq` (tests: `Test::X::Y` → `test/x/y.lq`)
  - Qualifies names with module/alias and can open explicit names
- CLI: `locque-interpreter run <file>` (type check + run .lq/.lqs), `typecheck <file>`, `emit-lqs <in.lq> <out.lqs>`, `emit-lq <in.lqs> <out.lq>`, `validate <file.lqs>`. Smyth tool: `smyth run <file>` (type check + run), `smyth test` (all via test/main.lq), `smyth test <file>` (one). Installed at `~/.local/bin/smyth`.
- Smyth tool: `smyth run <file>`, `smyth test` (all), `smyth test <file>` (one). Installed at `~/.local/bin/smyth`.
- Type checker: currently Hindley-Milner with CBPV-aware `Comp` wrapper and type classes/families scaffolding; needs upgrade to the new dependent/universe grammar (explicit `Type0/1/2`, `for-all`/`there-exists`, `computation T`).
- Validator: checks nonempty names and kind/body match; paren checker with line/col reporting; `validate-prim` returns Boolean for a string (adds trailing newline automatically).

Libraries (`lib/`)
- Prelude: arithmetic; list ops (nil/cons/head/tail/length/append/map/filter/fold/reverse/drop-until); bool ops (not/if-bool/match); pair ops; id/is-falsy; tt.
- Assert: assert-eq-nat/string/bool, assert-false.
- IO: print/read-file/write-file/shell.
- String: concat/length/eq/split-on/join-with/trim/split-spaces.
- Tools: tokenizer; validator (validate-string via validate-prim).
- Note: primitives are Haskell scaffolding; aim to replace with locque implementations over time.

Notes and working agreements
- Single canonical surface (no arrows/lambdas/inspect/match-prim in new grammar); S-expr mirrors AST (keep using converter; do not handcraft `.lqs`).
- Qualification uses `::` only; `open Alias exposing ... end` with explicit names only.
- Effects are explicit: computations typed as `computation T`, bridged by `perform`; sequencing via `bind name from comp then ... end`.
- Work is test-driven; do not delete tests to make them pass. No new primitives without explicit instruction. Do not suppress warnings unless told.
- Parentheses are brittle; author `.lq` and convert; keep `.lqs` as generated mirrors when needed.
- Use `gsed` (GNU sed) on macOS for `\b`.

Examples/tests
- `examples/00_hello_world.{lq,lqs}`.
- Tests under `test/features/` and `test/typecheck/`; run all from `interpreter/` with `smyth test` (entry `test/main.lq`), or `smyth test <file>`.
- All tests run with type checking enabled by default (legacy `--skip-typecheck` still available).

Outstanding alignment work
- Update parser/typechecker/evaluator to the new grammar: `function ... returns ... as|do ... end`, `let value ... end`, `bind name from ... then ... end`, unified `match ... of-type ...`, `perform` without `io`, `computation T`, `for-all`/`there-exists`, explicit universes `Type0/1/2`, `::` qualifiers only, drop `lambda`/`inspect`/`match-prim`.
- Refresh docs/examples/tests to the new surface once the interpreter matches.
