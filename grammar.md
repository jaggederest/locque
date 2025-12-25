# Locque Grammar (Current Implementation)

This document describes the **current** implementation of Locque's M-expression syntax, not future/aspirational features.

## Lexical Conventions

### Identifiers
- **Pattern**: `[A-Za-z_][A-Za-z0-9_-]*`
- Examples: `add-nat`, `my-function`, `foo_bar`, `x`
- Qualified names use `.` separator: `P.add-nat`, `String.concat`

### Module Names
- **Pattern**: `[A-Za-z0-9_:/-]+` starting with letter or underscore
- Support `::` separator for namespacing: `Some::Module::Name`
- Map to lowercase filenames with `/`: `Some::Module::Name` → `lib/some/module/name.lq`
- Examples: `prelude`, `String`, `Tools::Validator`, `test::features::basics`

### Reserved Keywords
```
module, contains, import, as, define, transparent, opaque,
value, computation, function, of-type, produce,
lambda, let, in, return, bind, then, perform, io, end, where
```

### Comments
- **M-expr**: `#` (line comment), `/* */` (block comment)
- **S-expr**: `;` (line comment), `#| |#` (block comment)

### Application
- **Left-associative**: `f a b c` means `(((f a) b) c)`
- **No precedence rules** beyond left-to-right application
- Every construct starts with a keyword (no ambiguity)

---

## Module Structure

```ebnf
File ::= Import* Module

Import ::= "import" ModuleName ("as" ModuleName)?

Module ::= "module" ModuleName "contains" Definition* "end"
```

**Examples:**
```locque
import prelude as P
import string as S

module my-module contains
  define transparent main as computation
    perform io (P.print "Hello")
end
```

**Module resolution:**
- `import prelude` → loads `lib/prelude.lq`
- `import Some::Module` → loads `lib/some/module.lq`
- `import test::features::basics` → loads `test/features/basics.lq`

---

## Definitions

```ebnf
Definition ::= "define" Transparency Identifier "as" DefKind Body

Transparency ::= "transparent" | "opaque"

DefKind ::= "value" | "computation"
          | "typeclass" | "instance"  # Planned for type classes

Body ::= Expr  # for value/computation
       | TypeClassDef  # for typeclass (planned)
       | InstanceDef   # for instance (planned)
```

**Current:**
```locque
# Value (pure, no effects)
define transparent add-nat as value add-nat-prim

# Computation (effectful)
define transparent read-line as computation
  perform io read-line-prim
```

**Planned (type classes):**
```locque
# Type class declaration
define transparent Match as typeclass where
  match of-type (a -> (() -> b) -> (a -> a -> b) -> b)

# Instance declaration
define transparent Match-List as instance Match (List a) where
  match produce lambda xs -> ...
```

**Transparency:**
- `transparent` - can unfold during type checking (for optimization/reasoning)
- `opaque` - abstract, never unfolds (encapsulation)

**DefKind:**
- `value` - pure term-level value
- `computation` - effectful computation (CBPV split)
- `typeclass` - overloading mechanism (planned)
- `instance` - typeclass implementation (planned)

---

## Expressions

```ebnf
Expr ::= Identifier                    # Variable
       | Literal                       # Nat, String, Bool
       | "(" Expr ")"                  # Grouping
       | Expr Expr                     # Application (left-assoc)
       | Lambda                        # Function
       | "let" Identifier "=" Expr "in" Expr    # Let binding
       | MatchExpr                     # Pattern matching

Lambda ::= "lambda" LambdaParams "->" Expr
         | "lambda" "()" Expr          # Zero-parameter (no arrow!)

LambdaParams ::= Identifier            # Single param
               | Identifier "->" LambdaParams  # Curried multi-param

# Annotated lambda (for type checking)
AnnotatedLambda ::= "function" Identifier+ "of-type" Type "produce" Expr

Literal ::= Number | String | "true" | "false"

MatchExpr ::= MatchPrimitive Expr Expr Expr

MatchPrimitive ::= "match-list" | "match-bool" | "match-pair"
```

**Lambda examples:**
```locque
# Zero parameters (Unit -> a)
lambda () "constant"

# One parameter
lambda x -> x

# Curried (two parameters)
lambda x -> lambda y -> x

# Annotated (type-checked)
function x of-type Nat produce x
function x y of-type (-> Nat (-> Nat Nat)) produce add-nat x y
```

**CRITICAL: Zero-parameter lambdas use paren form WITHOUT arrow:**
```locque
✅ lambda () body         # Correct
❌ lambda _ -> body      # Wrong (one parameter)
❌ lambda () -> body     # Syntax error
```

**Let bindings:**
```locque
let x = 5 in
let y = 10 in
add-nat x y
```

**Pattern matching:**
```locque
# Match on list
P.match-list my-list
  (lambda () "empty")
  (lambda h -> lambda t -> h)

# Match on bool
P.match-bool condition
  (lambda () "false-branch")
  (lambda () "true-branch")

# Match on pair
P.match-pair my-pair
  (lambda () "unreachable")
  (lambda a -> lambda b -> a)
```

---

## Computations

```ebnf
Comp ::= "return" Expr                                    # Pure value
       | "bind" Identifier "<-" Comp "then" Comp         # Sequencing
       | "perform" "io" Expr                             # IO effect
```

**Examples:**
```locque
# Return pure value
return 42

# Sequence computations
bind x <- perform io (read-file "input.txt") then
bind _ <- perform io (print x) then
return tt

# IO effect
perform io (print "Hello, world!")
```

---

## Types

```ebnf
Type ::= TypeAtom
       | "(" Type ")"                              # Grouping
       | Type "->" Type                            # Function (right-assoc)
       | "(List" Type ")"                          # List
       | "(Pair" Type Type ")"                     # Pair
       | "(effect" Identifier ")" Type             # Effect annotation (planned)

TypeAtom ::= "Nat" | "String" | "Bool" | "Unit"
           | Identifier                            # Type variable (lowercase)

# Planned: constraint syntax for type classes
ConstrainedType ::= Constraint "=>" Type          # e.g., "Show a => a -> String"
Constraint ::= Identifier Identifier              # e.g., "Show a"
```

**Current types:**
```locque
Nat
String
Bool
Unit
(List Nat)
(Pair String Bool)
(-> Nat Nat)                    # Nat -> Nat
(-> Nat (-> Nat Nat))           # Nat -> Nat -> Nat
```

**Type variables** (in polymorphic signatures):
```locque
# Implicit forall
a                               # Type variable
(List a)                        # Polymorphic list

# Explicit in type schemes (internal)
∀a. List a -> Nat               # Represented internally
```

---

## S-expression Mapping

M-expressions have a **1:1 mapping** to S-expressions for serialization.

### Definitions
```
M: define transparent foo as value <expr>
S: (def transparent foo (value <expr>))
```

### Lambdas
```
M: lambda x -> body
S: (lambda ((x Type)) body)     # Type inferred or annotated

M: lambda () body
S: (lambda () body)             # Zero parameters
```

### Application
```
M: f a b c
S: (f a b c)                    # Left-associative application
```

### Let
```
M: let x = e1 in e2
S: (let ((x e1)) e2)
```

### Computation
```
M: bind x <- c1 then c2
S: (bind x c1 c2)

M: perform io expr
S: (perform io expr)

M: return expr
S: (return expr)
```

### Pattern Matching
```
M: P.match-list xs
     (lambda () empty-case)
     (lambda h -> lambda t -> cons-case)

S: (P.match-list xs
     (lambda () empty-case)
     (lambda ((h a)) (lambda ((t (List a))) cons-case)))
```

---

## Symbol Philosophy

**Structural symbols** (permitted):
- `->` function type arrow
- `()` grouping and zero-parameter lambdas
- `.` qualified names (module.name)

**Semantic operations use words**, not symbols:
- `equals` not `=` (for equality)
- `where` not `=` (for definitions in type classes)
- `once` not `!` (for linear types, planned)
- `(effect IO)` not `{IO}` (for effects, planned)

**One symbol, one meaning** - no context-dependent overloading.

---

## Determinism Guarantees

1. **No implicit coercions** - all conversions explicit
2. **No overloading** - single definition per name in scope (until type classes)
3. **Explicit imports** - all names qualified unless imported
4. **Left-associative application only** - no precedence ambiguity
5. **Keyword-led constructs** - every form starts with a keyword
6. **1:1 M-exp ↔ S-exp** - bijective mapping for all syntax

---

## Extensibility Pattern

All top-level introductions follow: `define <name> as <kind> <body>`

**Current kinds:**
- `value` - pure values
- `computation` - effectful computations

**Planned kinds:**
- `typeclass` - overloading mechanism
- `instance` - typeclass implementation
- `data` - algebraic data types (future)
- `family` - type-level functions (future)
- `refinement` - subset types (future)

Adding new language features = add new `<kind>` discriminator. No grammar restructuring needed.

**Self-hosted parser** will be simple pattern matching on keyword after `as`.
