Examples: CBPV surface, opacity markers, and extensionality posture

Value vs computation (CBPV-flavored)

- Transparent value definition (pure, unfolds in conversion):
  - M:
    ```
    define transparent add as
      function x Natural y Natural returns Natural value
        add-nat x y
      end
    ```
  - S: `(define transparent add (function ((x Natural) (y Natural)) Natural (value (add-nat x y))))`

- Computation definition (can perform effects/diverge; does not unfold under value-only contexts):
  - M:
    ```
    define transparent read-two as compute
      bind a from perform read-number then
        bind b from perform read-number then
          return Pair::pair a b
        end
      end
    end
    ```
  - S: `(define transparent read-two (compute (bind (a (perform read-number)) (bind (b (perform read-number)) (return (Pair::pair a b))))))`

Opacity and unfolding

- Opaque value definition (does not unfold in conversion):
  - M:
    ```
    define opaque secret-step as
      function x Natural returns Natural value
        add-nat x 1
      end
    ```
  - S: `(define opaque secret-step (function ((x Natural)) Natural (value (add-nat x 1))))`
  - Equality: `secret-step 2` is not definitionally equal to `3` without a lemma or explicit rewrite.

- Transparent counterpart (will unfold during conversion):
  - M:
    ```
    define transparent step as
      function x Natural returns Natural value
        add-nat x 1
      end
    ```
  - S: `(define transparent step (function ((x Natural)) Natural (value (add-nat x 1))))`
  - Equality: `step 2` is definitionally equal to `3` under βδι conversion.

Extensionality posture (propositional, not definitional)

- Function extensionality as a lemma:
  - M (schema): `define transparent fun-ext as for-all f as (for-all ignored as A to B) to for-all g as (for-all ignored as A to B) to for-all pf as (for-all x as A to equal B (f x) (g x)) to equal (for-all ignored as A to B) f g`
  - S: `(define transparent fun-ext (for-all (f (for-all (ignored A) B)) (for-all (g (for-all (ignored A) B)) (for-all (pf (for-all (x A) (equal B (f x) (g x)))) (equal (for-all (ignored A) B) f g)))))`
  - Use: proofs apply `fun-ext` explicitly; conversion does not rely on eta by default.

Notes on surface cues
- `define transparent|opaque name as <Value> ...` declares a value; opacity controls δ-unfolding in conversion.
- Computation values are explicit: `define ... as compute <Computation> end` (M-expr) or `(compute <Computation>)` (S-expr).
- `value` and `compute` mark function bodies.
- `perform ...`, `return ...`, `bind name from ... then ... end`, and `sequence ... end` are the computation sequencing vocabulary.
- `end` closes every multiline construct (function, compute, bind, match, module, open).

Universe choices and examples

- Strict tower (explicit levels, non-cumulative; `Type0 : Type1 : Type2 ...`)
  - Example 1 (Type0 inhabitant, lives in Type1):
    - M: `define transparent id0 as for-all A as Type0 to for-all ignored as A to A`
    - S: `(define transparent id0 (for-all (A Type0) (for-all (ignored A) A)))`
    - Type of `id0` is in `Type1` because it quantifies over `Type0`.
  - Example 2 (Type1 inhabitant using Type0 data):
    - M: `define transparent pair0 as for-all A as Type0 to for-all B as Type0 to Type0`
    - S: `(define transparent pair0 (for-all (A Type0) (for-all (B Type0) Type0)))`
    - The type of `pair0` itself sits in `Type1`.
  - Lifting is explicit: `lift A from Type0 to Type1`, with term-level `up`/`down`.

- Type-in-type (rejected, but illustrative of the shape we avoid)
  - Example 1: `Type : Type` collapses levels; would be written as `define transparent Type as Type` in a type-in-type world.
  - Example 2: self-application of type transformers becomes possible, leading toward inconsistency (Girard-style); we avoid this by keeping an explicit tower.

- Deferred (not in current grammar): level polymorphism and typical ambiguity; all levels are explicit `Type0/Type1/Type2`.

Pattern matching (current surface)

- List:
  - M:
    ```
    match xs of-type List A as ignored returns List A
      case List::empty as List::empty
      case List::cons with h A t (List A) as List::cons h t
    end
    ```
  - S: `(match (of-type xs (List A)) ignored (List A) (case List::empty List::empty) (case List::cons (h A) (t (List A)) (List::cons h t)))`

- Boolean:
  - M:
    ```
    match flag of-type Boolean as ignored returns Natural
      case Boolean::false as 0
      case Boolean::true as 1
    end
    ```
  - S: `(match (of-type flag Boolean) ignored Natural (case Boolean::false 0) (case Boolean::true 1))`

- Pair:
  - M:
    ```
    match p of-type Pair A B as ignored returns A
      case Pair::pair with a A b B as a
    end
    ```
  - S: `(match (of-type p (Pair A B)) ignored A (case Pair::pair (a A) (b B) a))`

Inductives and recursors (current + planned)

- Current: `data ... in TypeN ... end` introduces user-defined inductives; constructors are `Type::Ctor`; pattern matching is the eliminator.
- Planned: explicit recursors (and/or syntax sugar) and strict positivity checks; match remains deterministic sugar over recursors.
- Candidate core: Natural, Boolean, Unit, Empty, equal, List, Pair, Either/Option, Vec/Fin.
- Termination: structural recursion is enforced for `recur`; effects stay out of eliminators.

Total values vs. effectful computations

- Total, pure value function:
  - M:
    ```
    define transparent add2 as
      function x Natural returns Natural value
        add-nat x 2
      end
    ```
  - S: `(define transparent add2 (function ((x Natural)) Natural (value (add-nat x 2))))`
  - This lives in the value world, normalizes, and can appear in types.

- Effectful computation:
  - M:
    ```
    define transparent read-and-increment as compute
      bind n from perform read-number then
        return add-nat n 1
      end
    end
    ```
  - S: `(define transparent read-and-increment (compute (bind (n (perform read-number)) (return (add-nat n 1)))))`
  - It is a computation value; it does not run until `perform`.

Operators as ordinary identifiers (no infix)

- Addition as prefix application:
  - M: `define transparent add4 as add-nat 2 2 2 4`
  - S: `(define transparent add4 (add-nat 2 2 2 4))`
  - Meaning: left-associated application; no precedence rules beyond application.

Termination and coverage illustrations

- Accepted structural recursion:
  - M:
    ```
    define transparent sum-list as
      function xs (List Natural) returns Natural value
        match xs of-type List Natural as ignored returns Natural
          case List::empty as 0
          case List::cons with h Natural t (List Natural) as add-nat h (sum-list t)
        end
      end
    ```
  - Structural decrease on `t` (tail) is clear.

- Rejected non-structural recursion (would loop):
  - M:
    ```
    define transparent bad as
      function n Natural returns Natural value
        bad n
      end
    ```
  - Fails termination check; no decreasing argument.

- Coverage: exhaustive match required:
  - Missing case example (reject):
    ```
    match b of-type Boolean as ignored returns Natural
      case Boolean::true as 0
    end
    ```
  - Must include `case Boolean::false` for `Boolean`.

CBPV small-step semantics (informal rules)

- Values vs computations:
  - Values: variables, functions, constructors, literals. Do not step on their own.
  - Computations: `return v`, `bind x from c then d end`, `perform v`, matches/eliminators applied to scrutinee, applications where head reduces.

- Core rules (→ is one small step):
  - Application β (value world): `(function x A returns B value body end) v → body[x := v]`
  - Return/bind: `bind x from (return v) then d end → d[x := v]`
  - Bind step: if `c → c'` then `bind x from c then d end → bind x from c' then d end`
  - Match ι rules: `match ... case Type::Ctor ...` steps to the selected branch with pattern vars bound.
  - Perform: `perform v → effect-step(v)` (abstract; handled by runtime/host).

- Evaluation contexts (computations):
  - Left-to-right inside computation forms: e.g., in `bind x from C then d end`, evaluate `C`; in application `(f a)` evaluate `f` to WHNF, then `a`, etc.
  - Values are not reduced under function bodies (consistent with WHNF strategy).

- Opacity at runtime:
  - Opacity affects conversion, not runtime stepping: a transparent computation definition may be inlined at compile time, but runtime semantics is uniform once definitions are expanded by the compiler.

Namespaces and modules (current)

- Module names use `::` and map to lowercase file paths: `Some::Module::Name` → `lib/some/module/name.lq` (tests: `Test::X::Y` → `test/x/y.lq`).
- Imports precede opens; opens precede the module.
- `open Alias exposing name1 name2 ... end` is explicit only; no wildcard opens.

- Example:
  - M:
    ```
    import Tools::String as S
    open Prelude exposing map List::empty List::cons end

    module My::Module contains
      define transparent add2 as
        function x Natural returns Natural value
          add-nat x 2
        end
    end
    ```
  - S: `(module My::Module (define transparent add2 (function ((x Natural)) Natural (value (add-nat x 2)))))`

Entrypoints and tooling (current)

- Interpreter CLI: `locque-interpreter run <file>`, `typecheck <file>`, `emit-lqs <in.lq> <out.lqs>`, `emit-lq <in.lqs> <out.lq>`, `validate <file.lqs>`.
- Smyth tool: `smyth run <file>`, `smyth test`, `smyth test <file>`.
