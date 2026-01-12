# Open Questions

Future design decisions for dependent types and advanced features.

## 1) Universes

- Decision: explicit **non-cumulative** tower `Type0`, `Type1`, `Type2`, … (no type-in-type). Use `lift`/`up`/`down` to move types/terms across levels.
- Open: whether to add level polymorphism/solving later vs. keep levels explicit; max rules for Π/Σ/inductives remain to be spelled out; no typical ambiguity.

## 2) Inductive Types and Eliminators

- Current: user-defined inductives via `data ... in TypeN ... end`; constructors are `Type::Ctor`; elimination is typed `match` with mandatory binder/returns (dependent motives supported).
- Open: explicit recursors (or a desugaring target) and strict positivity checks in the kernel; clarify any restrictions on eliminators beyond totality; decide if eliminators ever permit effects.
- Termination: structural recursion is enforced for value `recur`; computation recursion remains future.
- Candidates: `Unit`, `Empty`, `Natural`, `List`, `Pair`, `Either`/`Option`, and indexed families like `Vec`/`Fin` (some are already built-in; the rest could be `data` in the stdlib).
- Bootstrap stance: keep the kernel small; prefer library definitions when possible; defer coinductives/streams.

## Parked backlog

- Standard library expansion: more string/list ops; JSON; HTTP (future).

---

## Resolved Decisions

The following decisions have been resolved and are documented in grammar.md and philosophy.md:

1. **Normalization**: WHNF for conversion, CBPV split (values vs computations) - see grammar.md
2. **Definitional equality**: β/δ/ι with per-definition opacity markers - see philosophy.md and grammar.md
3. **Totality vs partiality**: Total core with explicit effects in computations - see grammar.md Computations section
4. **Namespaces**: One module per file, explicit imports, qualified names - see grammar.md Module Structure
5. **Surface syntax mapping**: Complete 1:1 M-exp ↔ S-exp mapping - see grammar.md
6. **Ergonomics for LLMs**: No implicit coercions, explicit casts/effects - see philosophy.md
7. **Entrypoints**: `smyth run`, `smyth test`, `smyth bench`, `smyth dump`, `smyth format`, `smyth count` (see AGENTS.md for current implementation)
