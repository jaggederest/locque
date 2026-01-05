Locque guiding notes

- Purpose: a language meant to be written and read by LLMs/agents, with English-like, verbose keywords and a predictable, canonical surface form. S-expressions are the canonical serialized/AST form; a matching M-expression surface provides readability without precedence traps.
- Type discipline: pursue a strong, dependent type system (Π/Σ, universes, inductive types, normalization) to give maximal guarantees and make iterative generation/checking by LLMs easier.
- Syntax posture: minimize punctuation; every construct starts with a keyword to avoid implicit precedence, except list literals (`[]`, `[a, b]`) which are canonical and 1:1 with S-expr. Application stays left-associative; other relationships are explicit words (e.g., `define`, `function … returns … value|compute … end`, `for-all`, `there-exists`, `bind … from … then … end`, `perform …`, `match … of-type … as … returns …`).
- Assignment/definition: single, universal word with explicit transparency; definitions are values by default, and computation values use `compute ... end`. Namespaces are explicit modules with `::` qualification and file mapping; standard library imports use a `standard-library::` prefix to avoid local collisions.
- S-exp ↔ M-exp: require a 1:1 mapping. S-exp used for storage/parse tree and debugging; M-exp used for human-facing editing. Avoid ad-hoc sugars that break determinism; one canonical spelling per construct (list literals map to S-expr `list`).
- Ergonomics for LLMs: keep whitespace/indentation cosmetic; ban implicit coercions; prefer explicit casts and effect boundaries. Provide canonical templates/patterns for completion and avoid ambiguous grammar (e.g., empty list literals require `of-type [] (List A)`).
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
- `[]` and commas for list literals (canonical, not optional)

List literal brackets are the only punctuation-level sugar; they desugar deterministically to `List::empty`/`List::cons`.

**Semantic symbols** (avoid - use words instead):
- NO `!` for linearity - use `once` keyword
- NO `=` for equality/assignment/identity - use `equal`, `where`, `assign` as appropriate
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

# Refinement types via dependent pairs and equality
define Character as
  there-exists c as String in
    equal Natural (length c) 1
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
- `typeclass` - overloading mechanism
- `instance` - typeclass implementation
- `data` - algebraic data types

**Future discriminators** (syntax TBD, not in current grammar):
- `family` - type-level functions (type families)
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

**Examples of current extensions**:
```locque
# Type class (overloading)
define transparent Equality as typeclass A of-kind Type0 where
  eq of-type (for-all x as A to for-all y as A to Boolean)
end

# Instance (typeclass implementation)
define transparent Equality-Natural as instance Equality Natural where
  eq as function x Natural y Natural returns Boolean value
    eq-nat-prim x y
  end
end
```

This pattern remains consistent regardless of how sophisticated the type system becomes.
