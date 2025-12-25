# Locque TODO

## Immediate Priorities

### 1. Test Runner (`smyth test`) - ✓ **COMPLETED**
- [x] Modular test organization (test/features/, test/typecheck/)
- [x] test/main.lq entry point for running all tests
- [x] Type checking always runs before execution
- [x] Clear pass/fail reporting
- [x] Exit codes for CI/CD integration (0 = pass, 1 = fail)
- [x] Project root discovery via Smythfile.lq
- [ ] Golden file testing (snapshot comparison) - future enhancement

### 2. Type Classes
**Goal**: Unify match-list/bool/pair into polymorphic `match` via type classes

- [ ] Design type class syntax and semantics
- [ ] Implement type class declaration in parser
- [ ] Add type class constraint solving to type checker
- [ ] Implement instance resolution
- [ ] Create `Match` type class with instances for List, Bool, Pair
- [ ] Refactor standard library to use type classes
- [ ] Update tests and documentation

**Benefits**: Will stress-test interpreter and grow stdlib organically

### 3. Error Message Improvements
**Philosophy**: Self-prompting for LLMs, concise for humans, optimize for token efficiency

- [ ] Add "Did you mean?" suggestions (fuzzy matching on identifiers)
- [ ] Show code context (line with error highlighted)
- [ ] Suggest fixes inline: "Try: `function x of-type String produce`"
- [ ] Migration hints: "Use `match-list` instead of `match` for lists"
- [ ] Keep suggestions under 100 tokens per error
- [ ] Color coding in terminal (red/yellow/green)
- [ ] Error codes for programmatic handling

## Core Tooling (smyth)

### Build Tool CLI
- [x] `locque-interpreter --run-lq` (bootstrap)
- [x] `smyth test` - Run all tests (test/main.lq)
- [x] `smyth test <file>` - Run specific test file
- [x] `smyth run <file>` - Type check + execute
- [x] Standalone binary generation (`cabal install smyth` → `~/.local/bin/smyth`)
- [ ] `smyth check <file>` - Type check only
- [ ] `smyth repl` - Interactive REPL
- [ ] `smyth validate <file>` - Syntax + structural validation
- [ ] `smyth convert <file>` - M-expr ↔ S-expr bidirectional
- [ ] `smyth fmt <file>` - Auto-formatter (canonical style)
- [ ] `smyth doc` - Generate HTML documentation

### REPL Improvements
- [ ] Multiline input support
- [ ] `:type <expr>` command to show inferred types
- [ ] `:load <file>` to import definitions
- [ ] `:reload` to refresh loaded files
- [ ] `:help` for command reference
- [ ] Persistent history across sessions
- [ ] Tab completion for identifiers

### LSP Server (Language Server Protocol)
**HIGH VALUE for LLMs and editors**

- [ ] Go to definition
- [ ] Hover for type information
- [ ] Inline type errors while editing
- [ ] Rename refactoring (update all references)
- [ ] Auto-completion (imports, functions, keywords)
- [ ] Code actions (auto-fix common errors)
- [ ] Document symbols (outline view)

## Language Features

### Type System
- [x] Bidirectional type checking for annotated lambdas
- [x] Type-specific match primitives (match-list/bool/pair)
- [x] Fresh variable generation (StateT FreshCounter)
- [ ] Type classes and instances
- [ ] Exhaustiveness checking for pattern matching
- [ ] Type holes (`?` for "fill this in later")
- [ ] Better type inference (less annotation required)
- [ ] Row polymorphism for records (future)
- [ ] Dependent types (long-term endgame)

### Pattern Matching
- [x] match-list, match-bool, match-pair primitives
- [ ] Type class-based polymorphic match
- [ ] Exhaustiveness warnings
- [ ] Guard clauses in patterns
- [ ] Nested pattern destructuring
- [ ] As-patterns (`x@(cons h t)`)

### Standard Library
- [x] Core: Prelude, List, String, IO, Assert
- [x] List.slice
- [ ] Result type for error handling (`Result e a`)
- [ ] Option type for nullable values (`Option a`)
- [ ] Either type for choice
- [ ] More string operations (substring, replace, index-of)
- [ ] More list operations (zip, partition, group-by)
- [ ] File system (read-dir, walk-tree, file-info)
- [ ] JSON parsing/serialization
- [ ] Basic HTTP client (future)

## Self-Hosting (Dogfooding)

**Hold until tooling is more polished**

- [ ] M-expr → S-expr converter in Locque
- [ ] Parser written in Locque
- [ ] Type checker written in Locque
- [ ] Validator written in Locque
- [ ] smyth tool itself in Locque (ultimate goal)

## Documentation

### Language Reference
- [ ] Syntax specification (M-expr and S-expr)
- [ ] Type system formal semantics
- [ ] Pattern matching semantics
- [ ] CBPV (values vs computations) explanation
- [ ] Module system and imports
- [ ] Primitive operations reference

### Tutorials
- [ ] "Hello World" - basics of values and computations
- [ ] "Type Annotations" - navigating the type system
- [ ] "Pattern Matching" - lists, bools, pairs
- [ ] "Higher-Order Functions" - fold, map, filter
- [ ] "Building Abstractions" - defining your own functions
- [ ] "Type Classes" - polymorphism and interfaces (after implementation)
- [ ] "Dependent Types" - the final frontier (long-term)

### Migration Guides
- [x] match → match-list/bool/pair (in AGENTS.md)
- [ ] Adding type annotations to legacy code
- [ ] Migrating to type classes (once implemented)

## Infrastructure

### CI/CD
- [ ] GitHub Actions workflow
- [ ] Run all tests on every commit
- [ ] Type check all library code
- [ ] Build and cache dependencies
- [ ] Automated releases (versioning)

### Quality Assurance
- [ ] Benchmark suite for performance tracking
- [ ] Regression detection (performance and correctness)
- [ ] Fuzzer for parser robustness
- [ ] Fuzzer for type checker soundness
- [ ] Code coverage tracking
- [ ] Property-based testing for type checker

### Package Ecosystem (Future)
- [ ] Package manager design (`smyth add <package>`)
- [ ] Lock files for reproducible builds
- [ ] Central registry (like crates.io)
- [ ] Semantic versioning enforcement
- [ ] Transitive dependency resolution

## Code Quality & Refactoring

### Haskell Interpreter
- [ ] Clean up unused imports/bindings (warnings)
- [ ] Better separation of concerns (Parser/TypeChecker/Eval)
- [ ] Add property-based tests (QuickCheck)
- [ ] Performance profiling and optimization
- [ ] Better error handling (fewer `error` calls, more Either/Maybe)

### Technical Debt
- [ ] Standardize file path handling (absolute vs relative)
- [ ] Unify M-expr and S-expr parsing (shared AST)
- [ ] Simplify primitive environment building
- [ ] Better abstraction for import resolution
- [ ] Document internal architecture decisions

## Completed ✓

- [x] Fix #1: Inline annotated lambda type checking
- [x] Fix #2: Type-specific match primitives
- [x] Fix #3: Fresh variable generation (module scope pollution)
- [x] Three new type checker tests (fold_inline, match, opaque_import)
- [x] Updated AGENTS.md with match migration guide
- [x] List.slice function
- [x] Comment syntax (M-expr: `#` and `/* */`, S-expr: `;` and `#| |#`)
- [x] Smythfile.lq project configuration
- [x] smyth test runner (modular test organization, type checking, pass/fail reporting)
- [x] Migrated all 19 tests to test/features/ and test/typecheck/
- [x] test/main.lq unified test suite
- [x] `smyth run <file>` command (type check + execute)
- [x] Assertion counting (115 assertions across full test suite)
- [x] Standalone binary (`~/.local/bin/smyth`)

---

## Next Session Goals

Based on completed test runner, three main paths forward:

### Option A: Type Classes (Major Feature)
**Goal**: Unify match-list/bool/pair into polymorphic `match` via type classes

**Why now**:
- Would immediately improve ergonomics (single `match` instead of three)
- Stress-tests type system with real-world use case
- Foundation for future polymorphism needs

**Effort**: High (touches parser, type checker, evaluator)

### Option B: Error Message Improvements (Developer Experience)
**Goal**: Self-prompting for LLMs, concise for humans, token-efficient

**Why now**:
- Immediate payoff for development velocity
- LLM-friendly error messages are core philosophy
- Lower effort than type classes

**Effort**: Medium (mostly type checker and parser error reporting)

### Option C: More `smyth` Commands (Tooling Expansion)
**Goal**: Add `smyth check`, `smyth run`, `smyth repl`

**Why now**:
- Natural extension of test runner work
- Immediate utility for daily workflow
- `smyth check` particularly useful for fast feedback

**Effort**: Low-Medium (infrastructure already exists)
