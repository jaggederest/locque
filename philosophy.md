LLM-first dependently typed language (working name TBD) guiding notes

- Purpose: a language meant to be written and read by LLMs/agents, with English-like, verbose keywords and a predictable, canonical surface form. S-expressions are the canonical serialized/AST form; a matching M-expression surface provides readability without precedence traps.
- Type discipline: pursue a strong, dependent type system (Π/Σ, universes, inductive types, normalization) to give maximal guarantees and make iterative generation/checking by LLMs easier.
- Syntax posture: minimize punctuation; every construct starts with a keyword to avoid implicit precedence. Application stays left-associative; other relationships are explicit words (e.g., `define`, `has-type`, `for-all`, `do … then …`, `inspect … with`).
- Assignment/definition: single, universal word (e.g., `define … as …`), with no hidden scoping rules or implicit capture. Flat namespace is the initial stance, but we may introduce modules/namespaces if collisions or reasoning issues emerge.
- S-exp ↔ M-exp: require a 1:1 mapping. S-exp used for storage/parse tree and debugging; M-exp used for human-facing editing. Avoid ad-hoc sugars that break determinism.
- Ergonomics for LLMs: keep whitespace/indentation cosmetic; ban implicit coercions; prefer explicit casts and effect boundaries. Provide canonical templates/patterns for completion and avoid ambiguous grammar.
- Effects/partiality: total core is preferred for predictability; any partiality/effects must be isolated with explicit keywords (e.g., `perform io …`, `promise …`) to keep type checking and normalization clear.
- Opacity and extensionality: allow explicit per-definition opacity markers to control unfolding during conversion; keep definitional equality lean (βδι) and provide extensionality as propositional lemmas, with opt-in definitional η only where safe for performance/purity.
- Value semantics: defaults are immutable; variables are single-assignment; strings and collections are persistent/immutable. Any mutation lives in explicit computation constructs (e.g., a future `ref`/`set` effect), preserving predictability for LLMs and type checking.
- Reference implementation: Core language features (parser, type checker, evaluator) implemented in Haskell for correctness, performance, and bootstrapping. These define canonical behavior.
- Native implementation: Selected tools/libraries reimplemented in Locque itself as dogfooding exercises to test language expressiveness. Examples: future M-expr parser, future S-expr emitter, advanced string utilities.
- Implementation philosophy: Start with robust Haskell reference; migrate to native Locque only when language is mature enough and native version offers clear value (dogfooding, verification, metaprogramming).
- Compilation errors only: Locque has no concept of "warnings" - all issues are either errors (stop compilation) or not issues at all.
- Rationale for no-warnings policy: LLMs frequently ignore warnings during code generation. By making every problem an error, we force LLMs to address issues immediately rather than accumulating technical debt.
- Exception: Deprecation notices for future breaking changes may be warnings, but these should be rare and temporary.
- Implementation note: The Haskell interpreter may emit warnings (unused variables, incomplete patterns), but these are implementation details - Locque itself has no warning system.
