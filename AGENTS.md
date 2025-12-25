Project overview for agents

- Goal: locque is an LLM-friendly, keyword-led, dependently typed language with a strict CBPV split (values vs computations), 1:1 S-exp ↔ M-exp mapping, explicit opacity, explicit effects, no implicit coercions.
- Core constructs (surface intent): `define (transparent|opaque) name as (value|computation) …`, `function x of-type A produce …`, `for-all`, `there-exists`, `inspect … with … end` (matching to be added), application is prefix/left-assoc, modules/imports are explicit.

Interpreter (Haskell, `interpreter/`)
- AST: literals (Nat, String, Bool), vars, apps, lambdas, defs (transparency/kind), values vs computations.
- Evaluator: values include closures, primitives, lists, pairs, unit, bool. Primitives: add/sub nat; eq nat/string (Bool); concat/length/split-on/join/trim strings; print/read-file/write-file; assert eq nat/string; match (lists/bools/pairs); filter; fold (left); map (temp); append; pair/fst/snd/pair-to-list; drop-until; not; if-bool.
- Import resolution: loads `lib/<Module>.lq` or `.lqs`, qualifies names with module/alias, also inserts unqualified names.
- CLI: `--run-lq <file>` (run M-expr with type checking), `--run-lqs <file>` (run S-expr with type checking), `--skip-typecheck` flag to bypass type checking, `--typecheck <file>` (type check only), `--validate <file>` (parens + parse + structural validation), `--emit-lqs <file> <out>` (M-expr → S-expr), `--emit-lq <file> <out>` (S-expr → M-expr).
- Type checker: Bidirectional type checking with Hindley-Milner polymorphism, enforces CBPV split at type level, integrated into interpreter by default (use `--skip-typecheck` to disable for legacy code).
- Validator module: checks nonempty names and kind/body match; paren checker with line/col reporting; `validate-prim` returns Bool for a string (adds trailing newline automatically).

Libraries (`lib/`)
- Prelude: add/sub; nil/cons/head/tail/length/append/map/filter/fold/reverse (string-list flavor); not/if-bool/match; pair/fst/snd/pair-to-list; id/is-falsy; tt.
- Assert: assert-eq-nat/string, assert-false.
- IO: print/read-file/write-file.
- String: concat/length/eq/split-on/join-with/trim/split-spaces (locque-defined).
- List: drop-until.
- Tools:
  - Tokenizer (tokenize by spaces).
  - Validator (validate-string via validate-prim).
- Notes:
  - We’re aiming for minimal syntax surfaced in locque, but some primitives are currently defined in Haskell as scaffolding; these should be replaced with locque implementations over time.
  - Work follows a test-driven loop: make a test fail, fix the code to pass, then refactor (no deleting tests to make them pass).
  - Do not add new primitives without explicit permission/instruction. Keep existing tests intact; do not suppress warnings with annotations unless explicitly told to.
  - Existing code/tests are good examples for extending functionality; prefer building on them rather than bypassing them.
  - Parentheses are brittle: NEVER hand-balance or manually rewrite `.lqs` S-expressions; author canonical `.lq` M-expr sources and use the converter to produce `.lqs` only when needed for debugging/fixtures. Keep `.lqs` tests as generated mirrors of `.lq` where required.

Examples/tests
- `examples/00_hello_world.{lq,lqs}`.
- Tests: basics, file IO, lambda, fold/map, match, validator (`test/40_small_bad_code.lqs`), roundtrip (`test/51_roundtrip.{lq,lqs}`), type checker (`test/99_typecheck_*.lq`).
- Organization: `00-39` feature tests, `40-49` validator/negative tests, `50-59` conversion tests, `99-XX` type checker tests.
- Most legacy tests require `--skip-typecheck` flag; new tests should include type annotations.

## Match Primitive Semantics

Locque provides three type-specific match primitives for pattern matching on different value types. Each primitive has a precise type signature that correctly represents its semantics in the Hindley-Milner type system.

### Type-Specific Match Primitives

**match-list**: Match on lists
```
∀a b. List a -> (() -> b) -> (a -> List a -> b) -> b
```

**match-bool**: Match on booleans
```
∀b. Bool -> (() -> b) -> (() -> b) -> b
```

**match-pair**: Match on pairs
```
∀a b c. Pair a b -> (() -> c) -> (a -> b -> c) -> c
```

### Handler Arities by Type

| Primitive | Value Type | First Handler | Second Handler |
|-----------|------------|---------------|----------------|
| `match-list` | `List a` | `() -> b` (empty list) | `a -> List a -> b` (head, tail) |
| `match-bool` | `Bool` | `() -> b` (false) | `() -> b` (true) |
| `match-pair` | `Pair a b` | `() -> c` (unreachable) | `a -> b -> c` (first, second) |

### Critical Syntax Rule: Zero-Parameter Lambdas

**Zero-parameter lambdas MUST use paren form WITHOUT arrows**:
- ✅ Correct: `lambda () body` (paren form, no arrow)
- ❌ Wrong: `lambda _ -> body` (arrow form with one parameter)
- ❌ Wrong: `lambda () -> body` (syntax error - paren form doesn't use arrows)

**The distinction**:
- Paren form: `lambda () body` parses `()` as zero identifiers (type: `() -> a`)
- Arrow form: `lambda x -> body` parses `x` as one identifier (type: `a -> b`)

All match handlers that take zero parameters must use the paren form because their type is `Unit -> b` (in Locque syntax: `() -> b`).

### Examples

**List matching**:
```locque
P.match-list my-list
  (lambda () "list is empty")
  (lambda h -> lambda t -> h)
```

**Bool matching**:
```locque
P.match-bool my-bool
  (lambda () "was false")
  (lambda () "was true")
```

**Pair matching**:
```locque
P.match-pair my-pair
  (lambda () "unreachable")
  (lambda a -> lambda b -> a)
```

### Migration from Old `match`

The old unified `match` primitive (`match-prim`) is kept for backward compatibility but has a broken type signature that cannot correctly represent all three matching semantics in Hindley-Milner. It will cause "occurs check" failures when type checking code that matches on lists.

**Migration guide**:
- Replace `P.match <list> ...` with `P.match-list <list> ...`
- Replace `P.match <bool> ...` with `P.match-bool <bool> ...`
- Replace `P.match <pair> ...` with `P.match-pair <pair> ...`

**Note on `inspect` syntax**: The `inspect ... with ... end` syntax desugars to the old `match-prim` and will not type-check correctly for lists. Use explicit `P.match-list` calls instead. Future versions may add type-based dispatch or type classes to unify these back into a single polymorphic `match` function.

### S-expr Equivalent

```scheme
; M-expr: lambda () body
; S-expr: (lambda () body)

; M-expr: lambda x -> body
; S-expr: (lambda ((x Type)) body)

; Example with match-list:
(P.match-list xs
  (lambda () "empty")
  (lambda ((h a)) (lambda ((t (List a))) h)))
```

## M-expr ↔ S-expr Conversion

**Haskell Reference Implementation:**
- Canonical converter in `interpreter/src/Parser.hs`
- Bidirectional: `.lq` (M-expr) ↔ `.lqs` (S-expr)
- CLI: `--emit-lqs <in.lq> <out.lqs>` and `--emit-lq <in.lqs> <out.lq>`
- Both formats parse to identical AST
- Roundtrip conversions preserve semantics

**Philosophy:**
- NEVER hand-write `.lqs` S-expressions (except for debugging)
- Author canonical `.lq` M-expr sources with type annotations
- Use `--emit-lqs` to generate `.lqs` for debugging/fixtures
- Haskell implementation is the reference; native Locque converter is future dogfooding

**Type System Integration:**
- Type checker runs by default on `--run-lq` and `--run-lqs`
- Use `--skip-typecheck` flag for legacy code without type annotations
- All new code should include `function x of-type T produce ...` annotations
- Type errors caught before execution (fail fast)

Outstanding issues / next steps
- Add type annotations to all legacy tests (currently require `--skip-typecheck`)
- Clean up Haskell warnings (unused imports/binds) when convenient
- Consider native Locque converter as dogfooding exercise (future)
