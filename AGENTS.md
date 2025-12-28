Project overview for agents

- Goal: locque is an LLM-friendly, keyword-led, dependently typed language with a strict CBPV split (values vs computations), 1:1 S-exp ↔ M-exp mapping, explicit opacity, explicit effects, no implicit coercions.
- Core constructs: `define (transparent|opaque) name as <Value>`, computation values via `compute ... end`, functions only via `function ... returns ... value|compute ... end`, dependent types with `for-all x as A to B` and `there-exists x as A in B` plus `pack`/`unpack`, typed `match ... of-type ... as ... returns ...` with fixed cases, application is prefix/left-assoc, modules/imports/opens explicit, qualification uses `::` only, comments `#`/`/* */` (M) and `;`/`#| |#` (S).

Interpreter (Haskell, `interpreter/`)
- AST (current impl): literals (Natural, String, Boolean, Unit), vars, apps, functions, defs (transparency/kind), values vs computations, dependent match with binder/returns, typeclasses/instances (DictPass).
- Evaluator: values include closures, primitives, lists, pairs, unit, bool. Primitives: add/sub/mul/div/mod nat; eq/lt/le/gt/ge nat; eq/concat string; string-to-list; nat-to-string; nil/cons/pair/natural-to-peano; decide-eq-(nat/string/bool/pair/list); print/read-file/write-file/append-file/copy-file/copy-tree/rename-path/remove-file/remove-directory/shell/get-line/cli-args/current-directory/list-dir/path-exists/is-directory/is-file/make-directory/walk/stat; validate; error; assert-hit.
- Import resolution:
  - Module names use `::` separator (e.g., `Some::Module::Name`)
  - Map to lowercase file paths: `Some::Module::Name` → `lib/some/module/name.lq` (tests: `Test::X::Y` → `test/x/y.lq`)
  - Qualifies names with module/alias and can open explicit names
- CLI: `locque-interpreter run <file>` (type check + run .lq/.lqs), `typecheck <file>`, `emit-lqs <in.lq> <out.lqs>`, `emit-lq <in.lqs> <out.lq>`, `validate <file.lqs>`. Smyth tool: `smyth run <file>` (type check + run), `smyth test` (all via test/main.lq), `smyth test <file>` (one). Installed at `~/.local/bin/smyth`.
- Smyth tool: `smyth run <file>`, `smyth test` (all), `smyth test <file>` (one). Installed at `~/.local/bin/smyth`.
- Smythfile: `Smythfile.lq` lives at repo root; the only config currently read is `error-tests`, a list of `(file, substring)` expected-failure cases consumed by `smyth test`.
- Type checker: dependent (Pi/Sigma), explicit universes, strict CBPV; iota for `match`/`unpack`, exhaustiveness checking; native typeclass checking for classes/instances/constraints (DictPass still used for dictionary insertion during evaluation).
- Validator: checks nonempty names and kind/body match; paren checker with line/col reporting; `validate-prim` returns Boolean for a string (adds trailing newline automatically).

Libraries (`lib/`)
- Prelude: arithmetic; list ops (nil/cons/head/tail/length/append/map/filter/fold/reverse/drop-until); bool ops (not); pair ops; id/is-falsy; tt.
- Assert: assert-eq-nat/string/bool, assert-false.
- IO: print/read-file/write-file/shell.
- String: concat/eq/length/split-on/join-with/trim/substring/index-of/contains/starts/ends/reverse/chars.
- FS: read/write/append/copy/copy-tree/rename/list-dir/current-directory/path-exists/is-directory/is-file/make-directory/remove-file/remove-directory/walk/stat/lines helpers.
- Path: join/dirname/basename/ext/is-absolute.
- Tools: tokenizer; validator (validate-string via validate-prim).
- Note: primitives are Haskell scaffolding; aim to replace with locque implementations over time.

Notes and working agreements
- Canonical syntax and surface rules live in `grammar.md`.
- Single canonical surface (no arrows/lambdas/inspect/match-prim in new grammar); S-expr mirrors AST (keep using converter; do not handcraft `.lqs`).
- Qualification uses `::` only; `open Alias exposing ... end` with explicit names only.
- Effects are explicit: computations typed as `computation T`, bridged by `perform`; sequencing via `bind name from comp then ... end` or `sequence ... end`.
- Work is test-driven; do not delete tests to make them pass. No new primitives without explicit instruction. Do not suppress warnings unless told.
- Parentheses are brittle; author `.lq` and convert; keep `.lqs` as generated mirrors when needed.
- Use `gsed` (GNU sed) on macOS for `\b`.
- One test rollup per library module: every `lib/**/*.lq` must have a matching `test/**/*.lq` path; enforced by tooling.
- Lowercase file paths required for `lib/**/*.lq` and `test/**/*.lq` (module/file names map to lowercase paths); enforced by tooling.

Examples/tests
- `examples/00_hello_world.{lq,lqs}`.
- Tests under `test/`; run all from `interpreter/` with `smyth test` (entry `test/main.lq`), or `smyth test <file>`.
- All tests run with type checking enabled by default (legacy `--skip-typecheck` still available).

Outstanding alignment work
- Native typeclass support in the type checker (beyond DictPass).
- Refresh remaining docs/examples/tests to the new surface as needed.
