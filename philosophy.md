LLM-first dependently typed language (working name TBD) guiding notes

- Purpose: a language meant to be written and read by LLMs/agents, with English-like, verbose keywords and a predictable, canonical surface form. S-expressions are the canonical serialized/AST form; a matching M-expression surface provides readability without precedence traps.
- Type discipline: pursue a strong, dependent type system (Π/Σ, universes, inductive types, normalization) to give maximal guarantees and make iterative generation/checking by LLMs easier.
- Syntax posture: minimize punctuation; every construct starts with a keyword to avoid implicit precedence. Application stays left-associative; other relationships are explicit words (e.g., `define`, `function … returns … value|compute … end`, `for-all`, `there-exists`, `bind … from … then … end`, `perform …`, `match … of-type … as … returns …`).
- Assignment/definition: single, universal word with explicit transparency; definitions are values by default, and computation values use `compute ... end`. Namespaces are explicit modules with `::` qualification and file mapping.
- S-exp ↔ M-exp: require a 1:1 mapping. S-exp used for storage/parse tree and debugging; M-exp used for human-facing editing. Avoid ad-hoc sugars that break determinism; one canonical spelling per construct.
- Ergonomics for LLMs: keep whitespace/indentation cosmetic; ban implicit coercions; prefer explicit casts and effect boundaries. Provide canonical templates/patterns for completion and avoid ambiguous grammar.
- Effects/partiality: total core is preferred for predictability; any partiality/effects must be isolated with explicit keywords (e.g., `perform …`) to keep type checking and normalization clear.
- Opacity and extensionality: allow explicit per-definition opacity markers to control unfolding during conversion; keep definitional equality lean (βδι) and provide extensionality as propositional lemmas, with opt-in definitional η only where safe for performance/purity.
- Value semantics: defaults are immutable; variables are single-assignment; strings and collections are persistent/immutable. Any mutation lives in explicit computation constructs (e.g., a future `ref`/`set` effect), preserving predictability for LLMs and type checking.
- Reference implementation: Core language features (parser, type checker, evaluator) implemented in Haskell for correctness, performance, and bootstrapping. These define canonical behavior.
- Native implementation: Selected tools/libraries reimplemented in Locque itself as dogfooding exercises to test language expressiveness. Examples: future M-expr parser, future S-expr emitter, advanced string utilities.
- Implementation philosophy: Start with robust Haskell reference; migrate to native Locque only when language is mature enough and native version offers clear value (dogfooding, verification, metaprogramming).
- Compilation errors only: Locque has no concept of "warnings" - all issues are either errors (stop compilation) or not issues at all.
- Rationale for no-warnings policy: LLMs frequently ignore warnings during code generation. By making every problem an error, we force LLMs to address issues immediately rather than accumulating technical debt.
- Exception: Deprecation notices for future breaking changes may be warnings, but these should be rare and temporary.
- Implementation note: The Haskell interpreter may emit warnings (unused variables, incomplete patterns), but these are implementation details - Locque itself has no warning system.

## Symbol Philosophy: Words Over Symbols

**Guiding principle**: Prefer words to symbols. Each symbol should have exactly one meaning, never overloaded by context.

**Structural symbols** (kept minimal):
- Parentheses for grouping type/term expressions
- `::` for qualification (module/name)

**Semantic symbols** (avoid - use words instead):
- NO `!` for linearity - use `once` keyword
- NO `=` for equality/assignment/identity - use `equals`, `where`, `assign` as appropriate
- NO `&` for conjunction/sharing - use `and`, `shared` as appropriate
- NO symbolic arrows - use `for-all x as A to B`
- NO `{...}` for effects - use `computation T` and `perform`
- NO symbolic operators for type constraints - use words

**Examples of word-based design**:
```locque
# Linear types: use 'once' keyword
define send as
  function socket (once Socket) returns computation Unit compute
    ...
  end

# Effects: use explicit computation types and perform
define read-file as
  function path String returns computation String compute
    perform read-file-prim path
  end

# Refinement types: explicit 'refinement' and 'constraint' keywords
define Positive as refinement Natural where
  constraint (function x Natural returns Boolean value
    greater-than-nat x 0
  end)
end
```

**Rationale**:
- Words have semantic meaning → better LLM token embeddings
- Reduces ambiguity and hallucination
- Grep-able and searchable (can find all uses of `once`, can't search `!`)
- Self-documenting code
- No "symbol budget" constraints
- Future macros can use symbols without conflicts with core language

**One symbol, one meaning**: We never overload symbols. If `=` means assignment in one context, we don't reuse it for equality testing or pattern matching. Each construct gets its own keyword.

## Extensibility: The "define X as Y" Pattern

**Core pattern**: All top-level introductions follow `define <name> as <Value>|<discriminator>` with transparency on the binding (`transparent|opaque`). Term-level definitions are values by default; computation values use `compute ... end`. Function introduction is single-form `function … returns … value|compute … end`.

**Current discriminators**:
- `typeclass` - overloading mechanism (planned)
- `instance` - typeclass implementation (planned)

**Future discriminators** (as language evolves):
- `data` - algebraic data types (for when we add user-defined types)
- `family` - type-level functions (type families)
- `refinement` - subset types with constraints
- `effect` - effect definitions
- `protocol` - session types (if we add them)

**Why this pattern scales**:
1. Self-hosted parser becomes simple pattern matching on keyword after `as`
2. Adding new features = add new discriminator, no grammar restructuring
3. LLMs learn one pattern, apply it to all definition forms
4. Consistent left-to-right reading: "define X as <what-kind-of-thing>"

**Type system features vs definition forms**:
- **Structural features** need `define X as Y`: data types, typeclasses, type families
- **Parametric features** extend type syntax: higher-kinded types, existentials, rank-N polymorphism
- Example: Higher-kinded typeclass still uses `define X as typeclass`, just with kind annotations

**Examples of future extensions**:
```locque
# Type class (overloading)
define Match as typeclass where
  match of-type (for-all a as Type0 to
    for-all b as Type0 to
      for-all _ as Unit to
        for-all _ as a to
          for-all _ as a to
            b)
end

# Instance (typeclass implementation)
define Match-List as instance Match (List a) where
  match returns for-all _ as List a to b value
    ...
  end
end

# GADT (when we add user-defined types)
define Expr as data (a of-type Type0) where
  LitNat of-type (for-all _ as Natural to Expr Natural)
  Add of-type (for-all _ as Expr Natural to
    for-all _ as Expr Natural to
      Expr Natural)
end

# Type family (type-level computation)
define Length as family (for-all _ as List a to Natural) where
  Length Nil equals Zero
  Length (Cons x xs) equals Succ (Length xs)
end

# Refinement type (constrained subset)
define NonZero as refinement Natural where
  constraint (function n Natural returns Boolean value
    greater-than-nat n 0
  end)
end
```

This pattern remains consistent regardless of how sophisticated the type system becomes.
