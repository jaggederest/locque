# Locque Grammar (consensus spec)

One canonical surface, no arrows or symbolic sugar. Qualification uses `::` only. Definitions are single-assignment; opacity lives on `define`. Computations are first-class values via the `compute` wrapper.

## Lexical and tokens

- Identifiers: `[A-Za-z_][A-Za-z0-9_-]*`
- Qualified names: `Alias::name` (only `::`; `.` reserved for future records)
- Module names: `[A-Za-z0-9_:/-]+` with `::` separators; `Group::Module` → `group/module.lq` (lowercased). Tests live under `Test::…` → `test/…`.
- Literals:
  - Naturals: digits with optional `_` separators (ignored): `0`, `42`, `1_000`
  - Strings: `"..."` with escapes
  - Booleans: `true`, `false`
  - Unit: `tt`
- Comments: M-expr `#` / `/* */`; S-expr `;` / `#| |#`
- Application: left-associative; minimal parentheses for grouping only
- Keywords are reserved (including `to` for `for-all`).

## Module, import, open

```
import Prelude
import Tools::String as S
open Prelude exposing map List::empty List::cons end

module my::module contains
  ...
end
```

- Imports precede opens; opens precede the module.
- `open Alias exposing name1 name2 ... end` (explicit list only; no wildcard).
- `open` names may include `::` (e.g., `Option::some`) to drop only the module qualifier while keeping the type-qualified constructor name. `open` never introduces bare constructor names.
- `end` is mandatory for `open` and `module` blocks in M-expr.

## Definitions

```
define transparent <name> as <Value>
define opaque       <name> as <Value>
define transparent <TypeName> as data p1 TypeParam p2 TypeParam ... in TypeN
  case <TypeName::Constructor> of-type <Type>
  ...
end
```

- Opacity: `transparent` or `opaque` on the binding.
- Definitions are values by default.
- Computation values are explicit: `compute <Computation> end` (M-expr) or `(compute <Computation>)` (S-expr); they run only when performed.
- Functions are values; effectful functions return `computation T`.
- `data` defines a type constructor and its constructors (values) as `TypeName::Constructor`.
- Data parameters use the same `name Type` binder rules as functions; they may live in any universe (`TypeN`) and are in scope in constructor types.
- Data parameters may be empty.
- The `data` result must be a universe (`Type0`, `Type1`, ...). The type constructor has the corresponding Pi type.
- Each constructor type must return the data type with the same arity as the header, but arguments may be refined (indices). The checker enforces this.
- Constructor applications include only the data parameters that appear free in the constructor type, in header order; unused data parameters are implicit. Example:
  `define transparent Tagged as data n Natural in Type0 ... case Tagged::zero of-type Tagged 0 ... end` makes `Tagged::zero` nullary.
- Constructor field types may include `computation T` (since computations are values), but constructor results must be pure data (not `computation (Type ...)`).
- A `data` definition may declare zero constructors (an empty type).

## Typeclasses and constraints

```
define transparent Equality as typeclass A where
  eq of-type for-all x as A to for-all y as A to Boolean
end

define transparent Equality-Natural as instance Equality Natural where
  eq as <Value>
end
```

- `typeclass` introduces a class with a single type parameter (currently assumed `Type0`) and a list of method types.
- `instance` provides implementations for all class methods; missing/extra methods are errors.
- Instance constraints are not supported yet.

```
function A Type0 x A y A requires Equality A and Display A returns Boolean value
  ...
end
```

- `requires` introduces class constraints for a function. Constraints bring method names into scope in the body.
- Method names must be unique across constraints; local bindings can shadow them.

S-expr sketches:
- `(typeclass A (eq of-type (for-all (x A) (for-all (y A) Boolean))))`
- `(instance Equality Natural (eq <expr>))`
- `(function (A Type0) (x A) (requires (Equality A)) Boolean (value <expr>))`

## Block termination (M-expr)

`end` is mandatory for any construct that can be multiline: `function`, `compute`,
`let value`, `bind`, `match`, `sequence`, `pack`, `unpack`, `typeclass`,
`instance`, `data`, `module`, `open`. `end` is never optional in M-expr, even
for single-line bodies.

## Values

```
Value ::=
    Identifier
  | Literal
  | Value Value                        -- application (left-assoc)
  | Function                           -- pure or effectful intro
  | compute Computation end            -- explicit computation value
  | let value x be Value in Value end  -- pure local
  | match ...                          -- typed match (see below)
  | of-type ValueAtom Type             -- explicit annotation
  | up Type from TypeN to TypeM Value  -- term lift
  | down Type from TypeN to TypeM Value -- term unlift
  | reflexive Type ValueAtom           -- equality intro
  | rewrite ValueAtom ValueAtom as Value -- equality transport
  | pack x as Type in Type with ValueAtom Value end
  | unpack Value as x y in Value end

ValueAtom ::=
    Identifier
  | Literal
  | ( Value )
```

### Function (only form)

```
function p1 TypeParam p2 TypeParam ... returns Type value ValueBody end              -- pure
function p1 TypeParam p2 TypeParam ... returns computation Type compute CompBody end -- effectful
function returns Type value|compute Body end                          -- zero-arg
```

- Parameters are `name Type`, space-separated.
- `returns` separates params from result type.
- Body keyword: `value` (pure value body) or `compute` (computation body). This also signals purity.
- Non-atomic parameter types must be parenthesized: `xs (List String)`, `f (for-all x as A to B)`, `c (computation T)`.

## Computations

```
Computation ::=
    return Value
  | bind x from Computation then Computation end
  | perform Value                 -- value of type computation T
  | sequence ValueAtom+ end       -- sugar for `perform` + `bind` sequencing

- Computations are not identifiers; to run a named computation value, use `perform`.
- `sequence` performs each value in order and returns `tt` (unit).
```

## Pattern matching (typed, unified `match`)

Scrutinee type and return type must be known (annotate inline if ambiguous). A
scrutinee binder is mandatory; use `ignored` when it is unused. The return type
may depend on the binder.

```
-- List
match xs of-type List A as ignored returns List A
  case List::empty as <Value>
  case List::cons with h A t (List A) as <Value>
end

-- Boolean
match flag of-type Boolean as ignored returns Natural
  case Boolean::false as <Value>
  case Boolean::true  as <Value>
end

-- Pair
match p of-type Pair A B as ignored returns A
  case Pair::pair with a A b B as <Value>
end
```

Handlers use value bodies (pure); no lambdas.
Case binders use the same `TypeParam` rule as function params; parenthesize non-atomic types.
Matches must be exhaustive over the constructors for the scrutinee type; no duplicates or mixed-constructor sets.
Dependent motives refine the return type per case by unifying constructor results with the scrutinee type; the case body is checked under the resulting index substitution.
If a constructor result cannot unify with the scrutinee type (e.g., index mismatch), that case is treated as unreachable and its body is not checked.
Built-in constructor names:
- `List::empty`, `List::cons`
- `Boolean::false`, `Boolean::true`
- `Pair::pair`
- `Unit::tt`
Case labels always use the `Type::Constructor` form, even if the corresponding literal can appear elsewhere (e.g., `tt`).
For empty types, a match with zero cases is exhaustive.

### Built-in data sketches (spec)

These show the surface syntax clients should expect; built-ins may still be implemented primitively.

```
define transparent Boolean as data in TypeN
  case Boolean::false of-type Boolean
  case Boolean::true of-type Boolean
end

define transparent Unit as data in TypeN
  case Unit::tt of-type Unit
end

define transparent List as data A TypeN in TypeN
  case List::empty of-type List A
  case List::cons of-type for-all h as A to for-all t as List A to List A
end

define transparent Pair as data A TypeN B TypeN in TypeN
  case Pair::pair of-type for-all a as A to for-all b as B to Pair A B
end
```

## Types

```
Type ::=
    TypeAtom
  | for-all x as Type to Type          -- Pi (function type, dependent)
  | there-exists x as Type in Type     -- Sigma (dependent pair)
  | computation Type                   -- effectful result type
  | lift Type from TypeN to TypeM      -- universe lift (strict)
  | equal Type ValueAtom ValueAtom     -- equality type

TypeParam ::= TypeSimple | ( Type )    -- M-expr binders only

TypeAtom ::=
    Type0 | Type1 | Type2 | ...        -- universes (explicit levels)
  | Natural | String | Boolean | Unit
  | List Type
  | Pair Type Type                     -- non-dependent pair sugar
  | TypeVar                            -- lower-case identifier
  | ( Type )                           -- grouping

TypeSimple ::=
    Type0 | Type1 | Type2 | ...
  | Natural | String | Boolean | Unit
  | TypeVar
  | TypeConstructor                    -- uppercase identifier
```

- No arrow syntax; always the `for-all ... to ...` form (even non-dependent; use a throwaway name if unused).
- Sigma is `there-exists x as A in B`; non-dependent pairs may use `Pair A B`.
- Universes are explicit (`Type0`, `Type1`, `Type2`, …) and **not** cumulative; no bare `Type`.
- `TypeN : Type(N+1)`. Use `lift` to move a type across universes.
- Type application is word-based: `List Natural`, `Vector Natural n`, `Result Error a`.
- In M-expr binder positions, parenthesize non-atomic types: `x (List Natural)`, `x (Vector Natural n)`.
- `equal A x y` is a type (in the same universe as `A`) asserting `x` and `y` are definitionally equal at `A`.
- Type formation is not restricted: any value expression that checks as `Typek` is a valid type (e.g., `match` or `let` in types).

## Universe lifting (strict)

```
lift A from TypeN to TypeM          -- type-level lift
up   A from TypeN to TypeM x        -- term-level lift
down A from TypeN to TypeM x        -- term-level unlift
```

- `A` must be a type in `TypeN`; `N <= M` is required (same-universe lifts are allowed).
- `lift A from TypeN to TypeM` has type `TypeM`.
- `up A from TypeN to TypeM x` requires `x : A` and produces `lift A from TypeN to TypeM`.
- `down A from TypeN to TypeM x` requires `x : lift A from TypeN to TypeM` and produces `A`.

## there-exists introduction/elimination

```
pack x as A in B with w v end
unpack p as x y in body end
```

- `pack`: `w : A`, `v : B[w/x]`, result is `there-exists x as A in B`.
- `unpack`: `p : there-exists x as A in B`, binds `x : A`, `y : B`,
  then returns `body`.
- `w` is a value atom; parenthesize if it is an application.

## Equality and transport

```
reflexive A x
rewrite P p as t
```

- `reflexive A x : equal A x x`.
- `rewrite` transports along equality:
  - `P : for-all z as A to Typek`
  - `p : equal A x y`
  - `t : P x`
  - result type is `P y`.
- In M-expr, `P` and `p` are value atoms; parenthesize applications.

## Effects

- `perform <value>` is the only value→computation bridge; effect kind comes from the callee’s type (no `io` token).
- Computation values are formed with `compute <Computation> end` or `(compute <Computation>)`; they run only when performed.
- Effectful functions return `computation T` and use `compute` bodies; pure functions return `T` with `value` bodies.

## Removed / forbidden

- No `lambda`, no arrows (`->`), no `<-`.
- No `inspect … with … end`, no `match-prim`.
- No `*-case` variants (`empty-case`, `cons-case`, etc.); use `case <Constructor>`.
- No multi-binding `let` or tuple binding sugar.
- No wildcard `open`; no qualification with `.`.

## M-expr and S-expr mapping

- M-expr is the human-facing surface; it uses infix keywords and `end` to make structure explicit.
- S-expr uses the same keyword vocabulary but **bans** M-expr infix separators (`as`, `returns`, `from`, `then`, `with`, `in`, `be`). Structure is purely positional via parentheses.
- S-expr uses `value` and `compute` nodes to mark function body kind.
- `define` binds a value directly in S-expr (no outer `value` wrapper).
- S-expr `data` form is `(data <params...> <TypeN> <cases...>)` where each case is `(case <Constructor> <Type>)`.
- S-expr represents computation values as `(compute <Computation>)`.
- S-expr uses `(of-type <expr> <Type>)` for explicit type annotations.
- S-expr match shape is `(match (of-type <expr> <Type>) <binder> <Type> <cases...>)`.
- S-expr case form is `(case <Constructor> <binders...> <body>)`, where binders are `(x Type)` and omitted for nullary constructors.
- S-expr sequence form is `(sequence <expr> <expr> ...)`.
- S-expr equality/transport: `(equal <Type> <expr> <expr>)`, `(reflexive <Type> <expr>)`, `(rewrite <expr> <expr> <expr>)`.
- S-expr binders are always parenthesized: `(x Type)`; M-expr requires parentheses only for non-atomic types.
- S-expr may introduce additional keywords for desugared AST forms when the M-expr is ambiguous.
- M-expr `of-type` accepts only a value atom; parenthesize applications: `of-type (f x) T`.
- S-expr lift forms are `(lift <Type> TypeN TypeM)`, `(up <Type> TypeN TypeM <expr>)`, `(down <Type> TypeN TypeM <expr>)`.
- S-expr pack/unpack forms are `(pack (x <Type>) <Type> <witness> <value>)`
  and `(unpack <expr> (x y) <body>)`.
- M-expr `rewrite` is `rewrite P p as t`; S-expr is `(rewrite <expr> <expr> <expr>)`.

## Determinism guarantees

1. One canonical spelling per construct (no alternate sugars).
2. Explicit universes and function binders; no implicit arrows.
3. Strict CBPV split: values are pure; computations sequence effects; computation values are explicit and inert until `perform`.
4. Qualification only via `::`; imports/opens explicit.
5. 1:1 mapping to S-expressions; S-expr uses the same keywords, with parentheses instead of `end`, and no infix separators.
