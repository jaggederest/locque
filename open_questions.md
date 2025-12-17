Early design decisions to resolve

1) Normalization and evaluation strategy
- Do we normalize to weak-head or strong? Call-by-push-value (values vs computations) seems preferred for explicit effect/purity boundaries; how do we balance predictability with performance for LLM-oriented workflows?
- Notes: adopt WHNF for conversion; expose a user-facing normalizer on-demand. In CBPV, values stay pure and inert; computations are where effects/divergence live. We can still surface CBV/CNB fragments via translation, but keep the surface explicit about value vs computation.
- Definition header shape: `define (transparent|opaque) <name> as (value|computation) <body>` where the first flag controls δ-unfolding and the second declares purity/effectfulness. This keeps conversion predictable and makes value vs computation explicit in the surface/S-exp.

2) Definitional equality
- What is the notion of definitional equality (β, η, δ, ι)? Any extensionality principles? How aggressive should unfolding be for type checking vs. user-facing reduction?
- Notes: β/δ/ι as default; make unfolding of defs opt-in opaque/transparent per name to control blow-ups. Treat η/extensionality as optional knobs (per type or per symbol) to keep conversion predictable; consider extensionality lemmas rather than baking into definitional equality by default.
- Current leaning: per-definition opacity marker (explicitly mark opaque vs transparent), default conversion uses βδι with WHNF. Extensionality provided propositionally (lemmas), with any definitional η as an opt-in for pure types only if performance permits.
- Conversion defaults (provisional): run conversion only on values (no forcing computations in types); include β/δ/ι, no η. Unfold transparent defs lazily/on-demand with recursion guard; opaque defs never unfold. Strategy: WHNF, compare heads/args; for lambdas/Pis/Sigmas compare components under binder extension. Universes cumulative with explicit levels; Π/Σ levels use max. Subject to revision as understanding deepens.

3) Universes
- Do we use explicit `Type0`, `Type1`, … with cumulativity? How do we prevent universe inconsistencies while keeping authoring straightforward?
- Options: cumulative tower `Type0 : Type1 : Type2 …` (practical, safe) vs. type-in-type (simpler but inconsistent, probably avoid). Universe polymorphism with automatic level solving vs. manual annotations. Need a clear max rule for Π/Σ and inductives; decide whether to allow typical ambiguity or require explicit levels for clarity to LLMs.
- Current leaning: cumulative tower with explicit levels (no type-in-type), allow universe polymorphism with explicit `Level` parameters, avoid typical ambiguity to keep surface unambiguous for LLMs.

4) Inductive types and eliminators
- What is the base set of inductives? Are eliminators exposed as explicit recursors or via pattern matching sugar? How do we encode strictly positive checks and recursion/termination?
- Notes: minimal core (Bool, Nat, Sigma/Π, sum, List) with explicit recursors; pattern matching as deterministic sugar over recursors to preserve 1:1 M/S mapping. Require strict positivity and structural (or well-founded) recursion to keep totality. Effects stay out of eliminators.
- Consider adding early: `Unit` (⊤), `Empty` (⊥), identity/equality type, `Maybe`/`Option`, and size-indexed finites (`Fin n`) or vectors (`Vec A n`) once we pin down recursion/termination checks. Coinductives/streams can be deferred. 
- Bootstrap stance: prefer a minimal trusted core; define `Option/Maybe` via `Either+Unit`, `Fin` and `Vec` as indexed inductives, and other utilities in a Prelude, to keep the kernel small and LLM-facing sugar deterministic.

5) Totality vs. partiality and effects
- Is the core total? If partiality/effects are allowed, how are they isolated (keywords, monads/effect rows, capabilities)? How do we keep normalization and type checking decidable?
- Notes: prefer a total core—values are pure and normalizing. Computations may carry effects/divergence and are typed distinctly (`define … as computation …`). Effects are explicit (`perform io`, `bind … then …`, `return …`), and do not appear in types unless explicitly thunked/forced. Structural recursion is the default check; well-founded recursion can be an opt-in later. Conversion operates in the value world; computations do not reduce during type checking except where explicitly forced.
- Termination: enforce structural recursion on an inductive argument (or on decreasing indices for indexed types). Reject general recursion in values; allow well-founded recursion later with explicit measures. Computations may diverge but must be marked `computation`.
- Coverage: pattern matches must be exhaustive; reject missing cases. Prefer non-overlapping/orthogonal patterns; if overlap is allowed, it is ordered (first match wins) and deterministic.
- CBPV small-step (informal): values do not step; computations step via β for application, ι for matches, force/thunk, bind/return. Evaluation is left-to-right in computation contexts. `perform io` steps via host/runtime. Opacity is a compile-time conversion concern, not a runtime stepping change.

6) Namespaces and scoping
- If we start with a flat namespace, how do we avoid collisions and enable modularity? Do we add lightweight modules or qualified names without introducing implicit scope rules?
- Notes: flat per-file with explicit imports keeps parsing simple and predictable for LLMs. Options: (a) files-as-modules with explicit `import Foo` / `import Foo as F`; (b) explicit module blocks `module Foo … end` possibly nested; (c) qualified names only (no open/import), requiring long names but zero implicit scope. Need to decide whether `open`/unqualified import exists (risk of collisions) or only explicit qualification. Favor deterministic resolution: no implicit search paths; one module = one file for simplicity.
- Current leaning: one module per file; qualified imports with optional `as` alias; no wildcard opens. If `open` exists, it must list names (`open M exposing (…)`) and unused imports are errors to keep modules tidy and predictable for LLMs.

7) Surface syntax ↔ S-expression mapping
- What is the exact 1:1 mapping between M-exp and S-exp? How do we ensure no precedence-driven ambiguities and keep serialization canonical?
- Notes: all constructs are keyword-led; application is left-associative; no implicit precedence. Pattern match sugar desugars deterministically to recursors. Definitions use `define (transparent|opaque) name as (value|computation) …` with explicit thunk/force. Modules/imports have fixed shapes. Need to codify a complete EBNF to freeze the mapping.

8) Ergonomics for LLMs
- What canonical templates/snippets do we commit to? Do we forbid implicit coercions/coalescing? How explicit are casts and effect boundaries?
- Notes: ban implicit coercions; require explicit casts and effect boundaries. Favor higher-order functions over templating. Provide a small set of canonical forms/patterns (definitions, for-all/function forms, inspect/case, bind/return) as guidance rather than heavy templating.

9) Entrypoints and execution
- How is an executable bootstrapped? Options: (a) convention `module Main` with `define transparent main as computation …`; (b) CLI-targeted entry `locque run <Module.name>` to select a computation of type `Computation Unit`. Effects run only inside computations. Decide default search path and whether multiple entrypoints can be declared.
- Current leaning: CLI `locquec run <Module.entry>` with a default of `Main.main` if unspecified. Entry must be a `computation` returning `Unit`/`tt`. One module per file; search path is project root plus declared deps.
- Tooling names: compiler `locquec`; build/package tool `smith` (handles deps/build/run). `smith` is generic—potential PATH collisions should be noted.
