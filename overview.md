Examples: CBPV surface, opacity markers, and extensionality posture

Value vs computation (CBPV-flavored)

- Transparent value definition (pure, unfolds in conversion):
  - M: `define transparent add as value function x of-type Nat produce function y of-type Nat produce add-nat x y`
  - S: `(def transparent add (value (lambda ((x Nat)) (lambda ((y Nat)) (add-nat x y)))))`

- Computation definition (can perform effects/diverge; does not unfold under value-only contexts):
  - M: `define transparent read-two as computation do a <- perform io read-number then b <- perform io read-number then return (pair a b)`
  - S: `(def transparent read-two (computation (do (bind a (perform io read-number)) (bind b (perform io read-number)) (return (pair a b)))))`

- Thunking a computation to make it a value (explicit delay/force):
  - M: `define transparent delayed-read as value thunk compute read-two`
  - S: `(def transparent delayed-read (value (thunk (compute read-two))))`
  - Forcing it later:
    - M: `define transparent consume-delayed as computation do pair <- force delayed-read then return pair`
    - S: `(def transparent consume-delayed (computation (do (bind pair (force delayed-read)) (return pair))))`

Opacity and unfolding

- Opaque value definition (does not unfold in conversion unless explicitly opened):
  - M: `define opaque secret-step as value function x of-type Nat produce add-nat x 1`
  - S: `(def opaque secret-step (value (lambda ((x Nat)) (add-nat x 1))))`
  - Equality: `secret-step 2` is not definitionally equal to `3` unless you locally `open secret-step` or rewrite using a lemma.

- Transparent counterpart (will unfold during conversion):
  - M: `define transparent step as value function x of-type Nat produce add-nat x 1`
  - S: `(def transparent step (value (lambda ((x Nat)) (add-nat x 1))))`
  - Equality: `step 2` is definitionally equal to `3` under βδι conversion.

Extensionality posture (propositional, not definitional)

- Function extensionality as a lemma:
  - M (schema): `define transparent fun-ext as value for-all f of-type (value A -> B) for-all g of-type (value A -> B) for-all pf of-type (for-all x of-type A -> f x == g x) -> (f == g)`
  - S: `(def transparent fun-ext (value (pi ((f (value (-> A B))) (g (value (-> A B))) (pf (pi ((x A)) (== (f x) (g x))))) (== f g))))`
  - Use: proofs apply `fun-ext` explicitly; conversion does not rely on η by default.

Notes on surface cues
- `define transparent|opaque name as value …` declares a pure value; opacity controls δ-unfolding in conversion.
- `define … as computation …` declares a computation; effects live here. Thunks/force bridge values and computations explicitly.
- `perform io …`, `return …`, `bind x <- … then …` are candidates for the computation sequencing vocabulary; these remain explicit to avoid implicit precedence. 

Universe choices and examples

- Cumulative tower (explicit levels, `Type0 : Type1 : Type2 …`)
  - Example 1 (Type0 inhabitant, lives in Type1):
    - M: `define transparent id0 as value for-all A of-type Type0 -> A -> A`
    - S: `(def transparent id0 (value (pi ((A Type0)) (-> A A))))`
    - Type of `id0` is in `Type1` because it quantifies over `Type0`.
  - Example 2 (Type1 inhabitant using Type0 data):
    - M: `define transparent pair0 as value for-all A of-type Type0 -> for-all B of-type Type0 -> Type0`
    - S: `(def transparent pair0 (value (pi ((A Type0) (B Type0)) Type0)))`
    - The type of `pair0` itself sits in `Type1`.

- Universe polymorphism (level variables with solving)
  - Example 1 (polymorphic identity over any level `l`):
    - M: `define transparent id as value for-all l of-type Level -> for-all A of-type Type l -> A -> A`
    - S: `(def transparent id (value (pi ((l Level) (A (Type l))) (-> A A))))`
  - Example 2 (polymorphic dependent pair):
    - M: `define transparent sigma as value for-all l of-type Level -> for-all A of-type Type l -> for-all B of-type (A -> Type l) -> Type l`
    - S: `(def transparent sigma (value (pi ((l Level) (A (Type l)) (B (-> A (Type l)))) (Type l))))`

- Type-in-type (rejected, but illustrative of the shape we avoid)
  - Example 1: `Type : Type` collapses levels; would be written as `define transparent Type as value Type` in a type-in-type world.
  - Example 2: self-application of type transformers becomes possible, leading toward inconsistency (Girard-style); we avoid this by keeping an explicit tower.

- Typical ambiguity (level inference, eliding explicit levels)
  - Example 1 (level elided, inferred as some `Type k`):
    - M: `define transparent idT as value for-all A of-type Type -> A -> A`
    - S: `(def transparent idT (value (pi ((A Type)) (-> A A))))`
    - Checker infers a fresh level variable for `Type` and solves constraints.
  - Example 2 (dependent pair with implicit levels):
    - M: `define transparent sigmaT as value for-all A of-type Type -> for-all B of-type (A -> Type) -> Type`
    - S: `(def transparent sigmaT (value (pi ((A Type) (B (-> A Type))) Type)))`
    - Inference assigns levels to each `Type` and enforces maxima; authoring stays terse at the cost of implicitness.

Inductives and eliminators: primer and examples

- Base inductives (suggested core): `Bool`, `Nat`, dependent pair `Sigma` (Σ), dependent function `Pi` (Π), sums `Either`/`+`, lists. Each inductive comes with its recursor/eliminator; pattern matching is sugar over these recursors.

- Booleans (non-dependent eliminator as an example):
  - M: `define transparent if-bool as value for-all A of-type Type0 -> Bool -> A -> A -> A`
  - S: `(def transparent if-bool (value (pi ((A Type0)) (-> Bool A A A))))`
  - Reduction: `if-bool A true t f` → `t`, `if-bool A false t f` → `f`.

- Naturals with explicit recursor:
  - Recursor type:
    - M: `define transparent nat-rec as value for-all P of-type (Nat -> Type0) -> P zero -> (for-all n of-type Nat -> P n -> P (succ n)) -> for-all n of-type Nat -> P n`
    - S: `(def transparent nat-rec (value (pi ((P (-> Nat Type0)) (pz (P zero)) (ps (pi ((n Nat)) (-> (P n) (P (succ n))))) (n Nat)) (P n))))`
  - Example use (pattern-match sugar expanded):
    - Pattern sugar: `inspect n with case zero -> zcase; case succ m -> scase m end`
    - Desugars to: `nat-rec (\n. Type_of_result) zcase (\m rec. scase m rec) n`

- Sums with eliminator:
  - Eliminator type:
    - M: `define transparent either-rec as value for-all A of-type Type0 -> for-all B of-type Type0 -> for-all P of-type (Either A B -> Type0) -> (for-all a of-type A -> P (left a)) -> (for-all b of-type B -> P (right b)) -> for-all e of-type Either A B -> P e`
    - S: `(def transparent either-rec (value (pi ((A Type0) (B Type0) (P (-> (Either A B) Type0)) (pl (pi ((a A)) (P (left a)))) (pr (pi ((b B)) (P (right b)))) (e (Either A B))) (P e))))`

- Lists with structurally recursive eliminator:
  - M: `define transparent list-rec as value for-all A of-type Type0 -> for-all P of-type (List A -> Type0) -> P (nil A) -> (for-all x of-type A -> for-all xs of-type List A -> P xs -> P (cons x xs)) -> for-all xs of-type List A -> P xs`
  - S: `(def transparent list-rec (value (pi ((A Type0) (P (-> (List A) Type0)) (pnil (P (nil A))) (pcons (pi ((x A) (xs (List A))) (-> (P xs) (P (cons x xs))))) (xs (List A))) (P xs))))`

- Pattern matching as sugar:
  - Surface: `inspect e with case left a -> L | case right b -> R end`
  - Desugars to: `either-rec A B (\_ . Type_of_result) (\a. L) (\b. R) e`
  - Keeping this sugar deterministic preserves the 1:1 M-exp ↔ S-exp mapping; the core stays on recursors for clarity and positivity checks.

- Positivity and termination:
  - Inductive definitions must be strictly positive; recursion/recursors must be structurally decreasing (or use a well-founded measure) to maintain totality. Effects stay out of eliminators to keep normalization properties intact. 

Additional small inductives

- Unit (⊤):
  - M: `define transparent unit as value Type0`
  - Constructor: `define transparent tt as value unit`
  - Eliminator: trivial; any function out of `unit` is constant.

- Empty (⊥):
  - M: `define transparent empty as value Type0`
  - No constructors; eliminator expresses ex falso:
    - M: `define transparent empty-rec as value for-all P of-type (empty -> Type0) -> for-all e of-type empty -> P e`
    - S: `(def transparent empty-rec (value (pi ((P (-> empty Type0)) (e empty)) (P e))))`

- Identity/equality type:
  - M: `define transparent Eq as value for-all A of-type Type0 -> A -> A -> Type0`
  - Constructor: `refl : for-all A of-type Type0 -> for-all x of-type A -> Eq A x x`
  - Eliminator (J):
    - M: `define transparent eq-rec as value for-all A of-type Type0 -> for-all x of-type A -> for-all P of-type (for-all y of-type A -> Eq A x y -> Type0) -> P x (refl A x) -> for-all y of-type A -> for-all p of-type Eq A x y -> P y p`
    - S: `(def transparent eq-rec (value (pi ((A Type0) (x A) (P (pi ((y A) (p (Eq A x y))) Type0)) (pr (P x (refl A x))) (y A) (p (Eq A x y))) (P y p))))`

Bootstrapping common types in a Prelude (derived from core)

- Maybe/Option from sum + Unit:
  - M: `define transparent Option as value for-all A of-type Type0 -> Type0`
  - S: `(def transparent Option (value (pi ((A Type0)) Type0)))`
  - Constructors (sugar over Either):
    - M: `define transparent none as value for-all A of-type Type0 -> Option A`
    - M: `define transparent some as value for-all A of-type Type0 -> A -> Option A`
    - S (using Either + Unit): `(def transparent none (value (lambda ((A Type0)) (left unit tt))))` and `(def transparent some (value (lambda ((A Type0) (a A)) (right A Unit a))))`
  - Eliminator via `either-rec` with a constant branch for `none`.

- Finite sets `Fin n` indexed by a Nat bound:
  - Constructors:
    - M: `define transparent fin-zero as value for-all n of-type Nat -> Fin (succ n)`
    - M: `define transparent fin-succ as value for-all n of-type Nat -> Fin n -> Fin (succ n)`
  - Eliminator ensures recursion is structurally decreasing on the Fin argument.

- Length-indexed vectors `Vec A n`:
  - Constructors:
    - M: `define transparent vnil as value for-all A of-type Type0 -> Vec A zero`
    - M: `define transparent vcons as value for-all A of-type Type0 -> for-all n of-type Nat -> A -> Vec A n -> Vec A (succ n)`
  - Eliminator/recursor enforces structural recursion on the vector; indexing by length gives stronger guarantees for functions like `head`/`tail`.

Total values vs. effectful computations

- Total, pure value function:
  - M: `define transparent add2 as value function x of-type Nat produce add-nat x 2`
  - S: `(def transparent add2 (value (lambda ((x Nat)) (add-nat x 2))))`
  - This lives in the value world, normalizes, and can appear in types.

- Effectful computation (IO):
  - M: `define transparent read-and-increment as computation do n <- perform io read-number then return (add-nat n 1)`
  - S: `(def transparent read-and-increment (computation (do (bind n (perform io read-number)) (return (add-nat n 1)))))`
  - Lives in computation world; cannot appear in types unless thunked.

- Thunking an effect to pass as a value:
  - M: `define transparent delayed-read as value thunk compute read-and-increment`
  - S: `(def transparent delayed-read (value (thunk (compute read-and-increment))))`

- Forcing a thunk inside a computation:
  - M: `define transparent run-delayed as computation do m <- force delayed-read then return m`
  - S: `(def transparent run-delayed (computation (do (bind m (force delayed-read)) (return m))))`

Operators as ordinary identifiers (no infix)

- Addition as prefix application:
  - M: `define transparent add4 as value add-nat 2 2 2 4`
  - S: `(def transparent add4 (value (add-nat 2 2 2 4)))`
  - Meaning: left-associated application; no precedence rules beyond application.

Termination and coverage illustrations

- Accepted structural recursion:
  - M: 
    ```
    define transparent sum-list as value
      function xs of-type List Nat produce
        inspect xs with
          case nil -> zero
          case cons h t -> add-nat h (sum-list t)
        end
    ```
  - Structural decrease on `t` (tail) is clear.

- Rejected non-structural recursion (would loop):
  - M:
    ```
    define transparent bad as value
      function n of-type Nat produce bad n
    ```
  - Fails termination check; no decreasing argument.

- Coverage: exhaustive match required:
  - Missing case example (reject):
    ```
    inspect b with
      case true -> zero
    end
    ```
  - Must include `case false -> …` for `Bool`.

CBPV small-step semantics (informal rules)

- Values vs computations:
  - Values: variables, lambdas, constructors, thunks, etc. Do not step on their own.
  - Computations: `return v`, `bind x <- c then d`, `perform io e`, `force v`, `thunk compute c`, matches/eliminators applied to scrutinee, applications where head reduces.

- Core rules (→ is one small step):
  - Application β (value world): `(lambda (x. b)) v → b[x := v]`
  - Thunk/force: `force (thunk c) → c`
  - Return/bind: `bind x <- (return v) then d → d[x := v]`
  - Bind step: if `c → c'` then `bind x <- c then d → bind x <- c' then d`
  - Force step: if `e → e'` then `force e → force e'`
  - Match/recursor ι rules: `inspect ctor args with …` steps to the selected branch with pattern vars bound.
  - Perform io: primitive step `perform io e` → `io-step(e)` (abstract; handled by runtime/host).

- Evaluation contexts (computations):
  - Left-to-right inside computation forms: e.g., in `bind x <- C then d`, evaluate `C`; in application `(f a)` evaluate `f` to WHNF, then `a`, etc.
  - Values are not reduced under lambdas (consistent with WHNF strategy).

- Opacity at runtime:
  - Opacity affects conversion, not runtime stepping: a transparent computation definition may be inlined at compile time, but runtime semantics is uniform once definitions are expanded by the compiler.

Namespaces and modules: options and examples

- Files-as-modules (one module per file, no nested modules):
  - File `math.lc` declares its module implicitly as `math`; exports definitions unless marked `private`.
  - Import with qualification:
    - M: `import math as m`
    - Use: `m.add2 3`
  - S: `(import math m)`
  - Keeps surface simple; collision avoidance via prefixes.

- Explicit module blocks (nested allowed):
  - M:
    ```
    module Math contains
      define transparent add2 as value function x of-type Nat produce add-nat x 2
    end
    ```
  - S: `(module Math (def transparent add2 (value (lambda ((x Nat)) (add-nat x 2)))))`
  - Import:
    - M: `import Math` or `import Math as M`
    - Use: `Math.add2` or `M.add2`

- Qualified-only, no unqualified opens:
  - No `open` keyword; every external reference uses its module qualifier (`Math.add2`), keeping resolution deterministic and avoiding accidental shadowing. Shorthand via `import Math as M`.

- Optional `open` with explicitness:
  - If allowed, it should be local and explicit:
    - M: `open Math exposing (add2, vcons)`
    - S: `(open Math (add2 vcons))`
  - Explicit `exposing` set limits collisions; no wildcard import to preserve predictability.

- Flat, single-file scope:
  - Within a file/module, no nested scopes; names are unique per module. Local bindings inside expressions (`bind x <- … then …`) are term-level and do not affect module namespace.

Entrypoints and tooling (proposed)

- Compiler: `locquec`.
- Build/package tool: `smith` (deps, build, run). Name is generic; watch for collisions if a user already has a `smith` in PATH.
- Default entry: `Main.main` if present; otherwise require explicit target.
- Explicit run: `smith run Hello.main` (module-qualified), entry must be a `computation` returning `unit/tt`.
- Module/file: one module per file; M-exp examples in `examples/*.lq`, S-exp in `examples/*.lqs`.
