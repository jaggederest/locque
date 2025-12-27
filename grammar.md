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
open Prelude exposing map nil cons end

module my::module contains
  ...
end
```

- Imports precede opens; opens precede the module.
- `open Alias exposing name1 name2 ... end` (explicit list only; no wildcard).
- `end` is mandatory for `open` and `module` blocks in M-expr.

## Definitions

```
define transparent <name> as <Value>
define opaque       <name> as <Value>
```

- Opacity: `transparent` or `opaque` on the binding.
- Definitions are values by default.
- Computation values are explicit: `compute <Computation> end` (M-expr) or `(compute <Computation>)` (S-expr); they run only when performed.
- Functions are values; effectful functions return `computation T`.

## Block termination (M-expr)

`end` is mandatory for any construct that can be multiline: `function`, `compute`, `let value`, `bind`, `match`, `module`, `open`. `end` is never optional in M-expr, even for single-line bodies.

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

- Computations are not identifiers; to run a named computation value, use `perform`.
```

## Pattern matching (typed, unified `match`)

Scrutinee type must be known (annotate inline if ambiguous).

```
-- List
match xs of-type List A
  empty-case as <Value>
  cons-case with h A t (List A) as <Value>
end

-- Boolean
match flag of-type Boolean
  false-case as <Value>
  true-case  as <Value>
end

-- Pair
match p of-type Pair A B
  pair-case with a A b B as <Value>
end
```

Handlers use value bodies (pure); no lambdas.
Case binders use the same `TypeParam` rule as function params; parenthesize non-atomic types.

## Types

```
Type ::=
    TypeAtom
  | for-all x as Type to Type          -- Pi (function type, dependent)
  | there-exists x as Type in Type     -- Sigma (dependent pair)
  | computation Type                   -- effectful result type

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
- Universes are explicit (`Type0`, `Type1`, `Type2`, …); cumulative; no bare `Type`.
- Type application is word-based: `List Natural`, `Vector Natural n`, `Result Error a`.
- In M-expr binder positions, parenthesize non-atomic types: `x (List Natural)`, `x (Vector Natural n)`.

## Effects

- `perform <value>` is the only value→computation bridge; effect kind comes from the callee’s type (no `io` token).
- Computation values are formed with `compute <Computation> end` or `(compute <Computation>)`; they run only when performed.
- Effectful functions return `computation T` and use `compute` bodies; pure functions return `T` with `value` bodies.

## Removed / forbidden

- No `lambda`, no arrows (`->`), no `<-`.
- No `inspect … with … end`, no `match-prim`.
- No multi-binding `let` or tuple binding sugar.
- No wildcard `open`; no qualification with `.`.

## M-expr and S-expr mapping

- M-expr is the human-facing surface; it uses infix keywords and `end` to make structure explicit.
- S-expr uses the same keyword vocabulary but **bans** M-expr infix separators (`as`, `returns`, `from`, `then`, `with`, `in`, `be`). Structure is purely positional via parentheses.
- S-expr uses `value` and `compute` nodes to mark function body kind.
- `define` binds a value directly in S-expr (no outer `value` wrapper).
- S-expr represents computation values as `(compute <Computation>)`.
- S-expr uses `(of-type <expr> <Type>)` for explicit type annotations.
- S-expr binders are always parenthesized: `(x Type)`; M-expr requires parentheses only for non-atomic types.
- S-expr may introduce additional keywords for desugared AST forms when the M-expr is ambiguous.

## Determinism guarantees

1. One canonical spelling per construct (no alternate sugars).
2. Explicit universes and function binders; no implicit arrows.
3. Strict CBPV split: values are pure; computations sequence effects; computation values are explicit and inert until `perform`.
4. Qualification only via `::`; imports/opens explicit.
5. 1:1 mapping to S-expressions; S-expr uses the same keywords, with parentheses instead of `end`, and no infix separators.
