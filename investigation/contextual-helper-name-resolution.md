# Contextual helper name resolution

Investigated: 2026-08-15
Revised: 2026-08-15 — investigation/contextual-helper-execution-mechanism.md

Evidence gathered for #172, which asks whether an unqualified contextual
helper spelling inside a Margin summary is reserved syntax or an ordinary
lexical lookup. The ticket records the question as a SPEC CONFLICT and does not
answer it. This note records what the package and dplyr were measured to do, so
that the decision — which belongs in an ADR, not here — rests on behaviour
rather than on the historical safety rationale the ticket says is insufficient.

Measured against `7f73d5e` with `pkgload::load_all(".")`, dplyr 1.1.4,
tidyselect 1.2.1, R 4.6.1, aarch64-apple-darwin23. Every row below was produced
by running the call, not by reading the source.

## What was measured

Each spelling was called three ways inside `summarize_with_margins()`: bare,
namespace-qualified, and with a caller binding of the same name in scope
(`f <- function(...) "CALLER"`). The same three were run against plain
`dplyr::summarise()` for comparison.

| spelling | plain dplyr under a shadow | marginplyr under a shadow |
| --- | --- | --- |
| `n()` | caller binding won | caller binding won |
| `where()` | package won | package won |
| `cur_group_id()` | **caller binding won** | **package won** — refused |
| `cur_group()`, `cur_group_rows()`, `cur_data()`, `cur_data_all()` | not measured individually | refused, same message |
| `pick()` | **caller binding won** | **package won** — dplyr semantics, argument rewritten |
| `across()` | **package won** | **caller binding won** — `"CALLER"` returned |
| `grouping_id()`, `grouping_bit()` | n/a | package won |
| `share_of_parent()`, `share_of_total()` | n/a | package won |
| `rollup()`, `cube()`, and the other constructors | n/a | caller binding was evaluated; refused on the returned object's class |

Two of these rows disagree with dplyr, and they disagree in opposite
directions: marginplyr refused a shadowed `cur_group_id()` and a shadowed
`pick()` that dplyr honoured, and honoured a shadowed `across()` that dplyr
refused.

## The `across()` row is an analysis/execution disagreement

`find_summary_context_helpers()` and the `across` branch of
`rewrite_summary_selection()` (`R/summary-selections.R:249`, `:364`) recognize
the spelling statically and check the grouping-column exclusion rule against
it. The measured result showed the check running and the caller's function
executing anyway:

```r
across <- function(...) "CALLER"
summarize_with_margins(d, k = across(units, sum), .grouping = rollup(region))
#>   region      k
#> 1      E CALLER
```

So the grouping-column exclusion rule was evaluated against an expression that
did not run, and a caller binding was sufficient to reach execution without it.

The mechanism behind the asymmetry with `pick()` was **not** established.
`rewrite_pick_selection()` (`R/summary-selections.R:461`) ends in
`rebuild_static_call()`, which preserves the original call head, so head
qualification does not on its own explain why the shadowed `pick()` reached
dplyr's semantics while the shadowed `across()` did not. Whatever the cause, it
was not read out of the source during this investigation, and an implementation
should establish it rather than assume the two paths differ only in the branch
they take at `:364` and `:373`.

## Revisions (2026-08-15)

`investigation/contextual-helper-execution-mechanism.md` establishes the
mechanism the section above records as "**not** established", and revisits one
row of the plain-dplyr table.

The asymmetry is decided inside dplyr, not by anything marginplyr writes: a
`pick()` spelling is expanded syntactically wherever it appears except under a
`~` or a `function`, while an `across()` spelling is expanded only in an
unnamed top-level dot — and every summary a Margin verb stages is a named dot.
The successor note also finds that this note's plain-dplyr `pick()` row holds
at dplyr 1.2.1 only inside a `~` or a `function`, and that nine functions read
a spelling rather than the four listed below, carrying eleven namespace tests
between them.

## Nested constructor positions refuse a genuine specification

A caller function that returns a real `margin_grouping_spec` was accepted at
the top level of `.grouping` and refused when nested:

```r
my_spec <- function(...) rollup(...)

summarize_with_margins(d, t = sum(units), .grouping = my_spec(region))
#> works

summarize_with_margins(d, t = sum(units),
                       .grouping = grouping_sets(my_spec(region), grade))
#> Error: Can't select columns with `my_spec(region)`.
#> x `my_spec(region)` must be numeric or character, not a <margin_grouping_spec> object.
```

`grouping_arg_spec()` (`R/grouping-plan.R:701-735`) evaluates a nested argument
only when its call head is a known constructor spelling, when it is a symbol,
or when it is not language. Anything else falls through to tidyselect, which
then reports the specification as the wrong kind of object for a position where
a specification is exactly what belongs.

The name gate is load-bearing rather than incidental: a nested argument is
ambiguous between a tidyselect selection and a nested specification, and
evaluating every nested call would run selections such as `starts_with("re")`
outside a selection context. A pre-built specification passed by symbol was
measured to work, which is the workaround the diagnostic does not mention.

## The marginplyr helpers have no runtime semantics to fall back to

`grouping_id()`, `grouping_bit()` (`R/grouping-context.R:100`, `:108`),
`share_of_parent()` and `share_of_total()` (`R/share.R:561`, `:573`) are error
stubs whose bodies are a single `abort_marginplyr()` call. Outside a Margin
verb they raise; inside one they are recognized statically and rewritten, and
the function object is never called. A caller binding of one of these names was
therefore the only way to obtain a callable of that name, and it was measured
to be ignored.

## Where the recognized spellings were written down

Four sites carried a list of names, each with its own namespace test, and no
site derived from another:

- `grouping_helper_name()` — `R/grouping-context.R:188`; `grouping_bit`,
  `grouping_id`; namespace `NULL` or `marginplyr`.
- `find_summary_context_helpers()` — `R/summary-selections.R:249`; the five
  refused `cur_*` spellings; namespace `NULL` or `dplyr`.
- `rewrite_summary_selection()` — `R/summary-selections.R:357-377`; `across`,
  `if_any`, `if_all`, `pick`; namespace `NULL` or `dplyr`.
- `share_helper_call_kind()` — `R/share.R:2548`; derived from
  `share_kind_rules()` through `share_named_kind()`; namespace `NULL` or
  `marginplyr`.

`grouping_constructor_names()` (`R/grouping-plan.R:683`) derives from
`grouping_kind_rules()`, and `share_named_kind()` from `share_kind_rules()`, so
the derived-table pattern the repository prefers already existed in two of the
five places a spelling was read.

The namespace test was uniform across all four sites and was measured to hold:
`marginplyr::grouping_id()` and `dplyr::cur_group_id()` were recognized, while
`stats::grouping_id()`, `stats::pick()`, `stats::share_of_total()` and
`stats::where()` all fell through to ordinary evaluation and failed with R's
own "not an exported object" error.

## What was searched for and not found

- **No snapshot asserts the refusal message.** `grep` over
  `tests/testthat/_snaps/` for `cur_group` and for `does not support` matched
  nothing, so changing that message breaks no snapshot.
- **Six assertions anchor on the phrase `does not support`**, by regular
  expression rather than by condition class:
  `test-grouping-interface.R:957`, `:965`, `:973`, `:981`,
  `test-static-expression-analysis.R:484`, `:3317`, and
  `test-summarize-operation.R:286`. A rewording that drops that phrase changes
  those assertions; one that keeps it as the opening does not.
- **A data column named `grouping_id` was not affected.** It is a symbol rather
  than a call, and summing it worked, so the reserved spellings were measured
  to reserve the call position only.
- **A computed head was refused, and so was a redundantly parenthesized one.**
  `get("grouping_id")(region)` and `(grouping_id)(region)` both reached the
  stub's error. The first is the conservative #130 policy behaving as designed;
  the second is #178.

## Reproduction

The four probe scripts this note was written from are not committed. Each was a
short `pkgload::load_all(".")` followed by `tryCatch()` around the calls in the
tables above, printing the result or the condition's class and message. Every
result quoted here can be reproduced by running any single call from this note
in a session with the working tree loaded.
