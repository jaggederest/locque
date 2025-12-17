Draft M-expression grammar (EBNF-style)

Lexical notes
- `Ident` matches `[A-Za-z_][A-Za-z0-9_-]*`.
- `ModuleName` matches `[A-Z][A-Za-z0-9_]*`.
- Keywords are reserved: `define`, `transparent`, `opaque`, `as`, `value`, `computation`, `function`, `for-all`, `of-type`, `produce`, `has-type`, `do`, `then`, `bind`, `perform`, `io`, `return`, `thunk`, `force`, `inspect`, `with`, `case`, `end`, `module`, `contains`, `import`, `open`, `exposing`, `Type`, `Type0`, `Type1`, `Level`, `true`, `false`, `zero`, `succ`, `left`, `right`, `nil`, `cons`.
- Application is left-associative. No other precedence; every construct starts with a keyword.

File/module
```
File ::= {ImportDecl | OpenDecl} ModuleDecl?
ModuleDecl ::= "module" ModuleName "contains" {Definition} "end"
ImportDecl ::= "import" ModuleName ("as" Ident)?
OpenDecl ::= "open" ModuleName "exposing" "(" Ident {"," Ident} ")"
```
# Imports/opens precede the module declaration (one module per file). Definitions live inside the module block.

Definitions
```
Definition ::= "define" Transparency Ident "as" DefKind Expr
Transparency ::= "transparent" | "opaque"
DefKind ::= "value" | "computation"

# Transparency and DefKind are orthogonal:
# - transparent value (unfolds in conversion)
# - opaque value (no unfolding)
# - transparent computation (can inline the computation definition)
# - opaque computation (kept abstract)
# Keywords stay explicit to avoid hidden defaults.
```

Expressions (Expr)
```
Expr ::= Annotation | Lam | Pi | Sigma | Match | LetDo | Thunk | Force | Perform | Return | Bind | Application

Annotation ::= Application "has-type" Expr
Lam ::= "function" Ident "of-type" Expr "produce" Expr
Pi ::= "for-all" Ident "of-type" Expr "->" Expr
Sigma ::= "there-exists" Ident "of-type" Expr "->" Expr

Match ::= "inspect" Expr "with" Case+ "end"
Case ::= "case" Ident PatternArgs? "->" Expr
PatternArgs ::= Ident*

LetDo ::= "do" Expr "then" Expr
Bind ::= "bind" Ident "<-" Expr "then" Expr
Perform ::= "perform" "io" Expr
Return ::= "return" Expr
Thunk ::= "thunk" "compute" Expr
Force ::= "force" Expr

Application ::= Atom {Atom}
Atom ::= Ident | Literal | ParenExpr
ParenExpr ::= "(" Expr ")"
Literal ::= Number | String
```

Notes on mapping to S-expressions
- `define transparent foo as value <e>` ↔ `(def transparent foo (value <e>))`
- `define opaque bar as computation <e>` ↔ `(def opaque bar (computation <e>))`
- Application is `(<f> <a1> … <an>)` in S.
- `function x of-type A produce B` ↔ `(lambda ((x A)) B)`
- `for-all x of-type A -> B` ↔ `(pi ((x A)) B)`
- `there-exists x of-type A -> B` ↔ `(sigma ((x A)) B)`
- `inspect e with case K x y -> b … end` ↔ `(match e (K x y b) …)`
- `bind x <- e1 then e2` ↔ `(bind x e1 e2)`
- `perform io e` ↔ `(perform io e)`
- `return e` ↔ `(return e)`
- `thunk compute e` ↔ `(thunk (compute e))`
- `force e` ↔ `(force e)`

Operators and application
- No infix: operators are identifiers. Addition is a normal function name (e.g., `add-nat`); `add-nat 2 2 2 4` means left-associated application and maps to `(add-nat 2 2 2 4)`. If infix sugar is ever added, it must desugar deterministically to prefix application.

Determinism rules
- No implicit coercions or overloading.
- No implicit opens; all imports are qualified unless `open … exposing (…)` is present.
- Pattern matching desugars deterministically to recursors; case order is literal.
- Only left-associative application; all other constructs are keyword-led, so no precedence ambiguity.
