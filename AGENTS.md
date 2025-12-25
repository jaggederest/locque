Project overview for agents

- Goal: locque is an LLM-friendly, keyword-led, dependently typed language with a strict CBPV split (values vs computations), 1:1 S-exp ↔ M-exp mapping, explicit opacity, explicit effects, no implicit coercions.
- Core constructs (surface intent): `define (transparent|opaque) name as (value|computation) …`, `function x of-type A produce …`, `for-all`, `there-exists`, `inspect … with … end` (matching to be added), application is prefix/left-assoc, modules/imports are explicit.

Interpreter (Haskell, `interpreter/`)
- AST: literals (Nat, String, Bool), vars, apps, lambdas, defs (transparency/kind), values vs computations.
- Evaluator: values include closures, primitives, lists, pairs, unit, bool. Primitives: add/sub nat; eq nat/string (Bool); concat/length/split-on/join/trim strings; print/read-file/write-file; assert eq nat/string; match (lists/bools/pairs); filter; fold (left); map (temp); append; pair/fst/snd/pair-to-list; drop-until; not; if-bool.
- Import resolution: loads `lib/<Module>.lqs`, qualifies names with module/alias, also inserts unqualified names.
- CLI: `--run-lqs <file>`, `--validate <file>` (parens check + parse + structural validate); `--run-lq/--emit-lqs` unimplemented.
- Validator module: checks nonempty names and kind/body match; paren checker with line/col reporting; `validate-prim` returns Bool for a string.

Libraries (`lib/`)
- Prelude: add/sub; nil/cons/head/tail/length/append/map/filter/fold/reverse (string-list flavor); not/if-bool/match; pair/fst/snd/pair-to-list; id/is-falsy; tt.
- Assert: assert-eq-nat/string, assert-false.
- IO: print/read-file/write-file.
- String: concat/length/eq/split-on/join-with/trim/split-spaces (locque-defined).
- List: drop-until.
- Tools:
  - Tokenizer (tokenize by spaces).
  - DefParser (parse a simple `define … as value …`), current S-expr needs validation.
  - MexprToSexpr (naive converter, currently failing logical tests).
  - Validator (validate-string via validate-prim).
- Notes:
  - We’re aiming for minimal syntax surfaced in locque, but some primitives are currently defined in Haskell as scaffolding; these should be replaced with locque implementations over time.
  - Work follows a test-driven loop: make a test fail, fix the code to pass, then refactor (no deleting tests to make them pass).
  - Do not add new primitives without explicit permission/instruction. Keep existing tests intact; do not suppress warnings with annotations unless explicitly told to.
  - Existing code/tests are good examples for extending functionality; prefer building on them rather than bypassing them.
  - Parentheses are brittle: NEVER hand-balance or manually rewrite `.lqs` S-expressions; author canonical `.lq` M-expr sources and use the converter to produce `.lqs` only when needed for debugging/fixtures. Keep `.lqs` tests as generated mirrors of `.lq` where required.

Examples/tests
- `examples/00_hello_world.{lq,lqs}`.
- Tests: basics, file IO, lambda, fold/map, match, validator (`test/40_small_bad_code.lqs`), converter (`test/50_mexpr_to_sexpr.{lq,lqs}`).
- Match test passes; converter test re-enabled (expected real S-expr) but converter is naive; validator test module currently malformed (missing paren issues).

## Match Primitive Semantics

The `match` primitive (`match-prim` in Haskell) performs pattern matching on values.

### Syntax

```locque
P.match <value>
  <empty-case-handler>
  <non-empty-case-handler>
```

### Handler Arities by Type

| Value Type | Empty Case Args | Non-Empty Case Args | Example |
|------------|----------------|---------------------|---------|
| `List` | 0 (empty list) | 2 (head, tail) | `(lambda () ...) (lambda h -> lambda t -> ...)` |
| `Bool` | 0 (false) | 0 (true) | `(lambda () ...) (lambda () ...)` |
| `Pair` | N/A | 2 (first, second) | `(lambda () ...) (lambda a -> lambda b -> ...)` |

### Critical Syntax Rule: Zero-Parameter Lambdas

**Zero-parameter lambdas MUST use paren form WITHOUT arrows**:
- ✅ Correct: `lambda () body` (paren form, no arrow)
- ❌ Wrong: `lambda _ -> body` (arrow form with one parameter)
- ❌ Wrong: `lambda () -> body` (syntax error - paren form doesn't use arrows)

**The distinction**:
- Paren form: `lambda () body` parses `()` as zero identifiers
- Arrow form: `lambda x -> body` parses `x` as one identifier
- Arrow form: `lambda -> body` would parse as zero identifiers, but is not valid syntax

### Examples

**List matching**:
```locque
P.match my-list
  (lambda () "list is empty")
  (lambda h -> lambda t -> h)
```

**Bool matching**:
```locque
P.match my-bool
  (lambda () "was false")
  (lambda () "was true")
```

**Pair matching**:
```locque
P.match my-pair
  (lambda () "unreachable")
  (lambda a -> lambda b -> a)
```

### S-expr Equivalent

```scheme
; M-expr: lambda () body
; S-expr: (lambda () body)

; M-expr: lambda x -> body
; S-expr: (lambda ((x Type)) body)

; Example:
(P.match xs
  (lambda () true)                          ; Zero params
  (lambda ((h a)) (lambda ((t (List a))) false)))  ; Two params nested
```

Outstanding issues / next steps
- Fix S-expr structure of `lib/tools/DefParser.lqs` and `MexprToSexpr.lqs` (ensure single balanced `(module …)`); then rerun tests.
- Implement proper `match/inspect` syntax in parser/evaluator (currently only `match-prim`).
- Improve converter logic to emit `(module Name (def transparent … (value …)))` for simple inputs.
- Add M-exp parser in Haskell for `.lq` files.
- Clean up Haskell warnings (unused imports/binds, cabal license path) when convenient.
