# Open Questions

Future design decisions for dependent types and advanced features.

## 1) Universes

- Decision: explicit cumulative tower `Type0`, `Type1`, `Type2`, … (no type-in-type). Surface tokens stay numeric; no sugar.
- Open: whether to add level polymorphism/solving later vs. keep levels explicit; max rules for Π/Σ/inductives remain to be spelled out; no typical ambiguity.

## 2) Inductive Types and Eliminators

- What is the base set of inductives? Are eliminators exposed as explicit recursors or via pattern matching sugar? How do we encode strictly positive checks and recursion/termination?
- Notes: minimal core (Boolean, Natural, Sigma/Π, sum, List) with explicit recursors; pattern matching as deterministic sugar over recursors to preserve 1:1 M/S mapping. Require strict positivity and structural (or well-founded) recursion to keep totality. Effects stay out of eliminators.
- Consider adding early: `Unit` (⊤), `Empty` (⊥), identity/equality type, `Maybe`/`Option`, and size-indexed finites (`Fin n`) or vectors (`Vec A n`) once we pin down recursion/termination checks. Coinductives/streams can be deferred.
- Bootstrap stance: prefer a minimal trusted core; define `Option/Maybe` via `Either+Unit`, `Fin` and `Vec` as indexed inductives, and other utilities in a Prelude, to keep the kernel small and LLM-facing sugar deterministic.

---

## Resolved Decisions

The following decisions have been resolved and are documented in grammar.md and philosophy.md:

1. **Normalization**: WHNF for conversion, CBPV split (values vs computations) - see grammar.md
2. **Definitional equality**: β/δ/ι with per-definition opacity markers - see philosophy.md and grammar.md
3. **Totality vs partiality**: Total core with explicit effects in computations - see grammar.md Computations section
4. **Namespaces**: One module per file, explicit imports, qualified names - see grammar.md Module Structure
5. **Surface syntax mapping**: Complete 1:1 M-exp ↔ S-exp mapping - see grammar.md
6. **Ergonomics for LLMs**: No implicit coercions, explicit casts/effects - see philosophy.md
7. **Entrypoints**: `smyth run <file>` / `smyth test` CLI (see AGENTS.md for current implementation)
