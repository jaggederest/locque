# Locque

Locque is a dependently typed language designed to be easy for both humans and LLMs to read, write, and refactor. It uses explicit keywords, a strict value/computation split, and a 1:1 mapping between M-expressions (human-friendly) and S-expressions (AST-exact).

Note: Locque is built almost entirely by LLMs; no human coding has been used.

## For a broad audience

If you want a language that is:
- explicit and predictable (no hidden coercions),
- precise about effects (computations are marked and performed explicitly),
- friendly to tools and automation,
then Locque is aimed at you.

Here is a small example:

```locque
import io as IO
import string as S

module examples::hello contains
  define transparent main as compute
    perform (IO::print (S::concat "hello, " "world"))
  end
end
```

You can also define your own data types and pattern match explicitly:

```locque
module examples::maybe contains
  define transparent Option as data A Type0 in Type0
    case Option::none of-type Option A
    case Option::some of-type for-all x as A to Option A
  end

  define transparent get-or as
    function A Type0 fallback A opt (Option A) returns A value
      match opt of-type (Option A) as ignored returns A
        case Option::none as fallback
        case Option::some with x A as x
      end
    end
end
```

## For vibe coders

Locque is intentionally verbose. The structure is always visible, so refactors are safe and repeatable. You do not need to guess operator precedence or implicit conversions. The language is built to help LLMs stay consistent and to help humans audit what was generated.

If you enjoy quick iterations, the workflow is simple:
- edit `.lq` files,
- run `smyth test`,
- repeat.

## For mathematicians

Locque is dependent and explicit:
- universes are strict (`Type0`, `Type1`, ...),
- Pi and Sigma are core (`for-all`, `there-exists`),
- equality is first-class (`equal`, `reflexive`, `rewrite`),
- definitional equality is intentionally small (beta/delta/iota).

Values are total and normalize; computations are explicitly marked and only run via `perform`. This makes the value world suitable for proofs and types, while still supporting effectful programs in the computation world.

## Contributing

- License: MIT. See `LICENSE`.
- Repo: https://github.com/jaggederest/locque/
- PRs: fork, create a branch, make focused changes, and open a PR.
- Tests: run `smyth test` from the repo root to check the full suite and expected failures.

Reference grammar and canonical syntax live in `grammar.md`.

Copyright 2025-2026 Justin George.
