# Which dplyr path a contextual helper spelling takes

Investigated: 2026-08-15

`investigation/contextual-helper-name-resolution.md` established that a
shadowed `across()` inside a Margin summary ran the caller's function while a
shadowed `pick()` reached dplyr's semantics, and recorded that the mechanism
behind that asymmetry "was **not** established", ruling out head qualification
because both rewrites end in `rebuild_static_call()`, which preserves the call
head. It asked that an implementation establish the mechanism rather than
assume it. This note is that work.

Measured against dplyr **1.2.1** and dbplyr **2.6.0**, on R 4.6.1,
aarch64-apple-darwin23. The predecessor note's findings were taken on dplyr
1.1.4, so one of its rows is revisited below. Every result was produced by
running the call or by reading the installed source, not by inference.

## The mechanism is in dplyr, and it is not head qualification

The predecessor's reasoning about `rebuild_static_call()` was right, and the
cause is that marginplyr does not distinguish the two paths at all — dplyr
does. `dplyr:::summarise_cols()` processes each dot as `expand_pick(dot, mask)`
and then `expand_across(dot)`, and the two have different reach:

- `dplyr:::expand_pick_call()` matches
  `is_call(expr, name = "pick", ns = c("", "dplyr"))` and descends through every
  argument, so a `pick()` anywhere in a dot is expanded before evaluation and no
  binding can capture it. It returns early on
  `is_call(expr, name = c("~", "function"))`.
- `dplyr:::expand_across()` returns the quosure unchanged unless the dot's
  top-level expression is a call to `across` **and**
  `attr(quo, "dplyr:::data")$is_named` is `FALSE`. Nothing in it looks at
  `if_any` or `if_all`; `dplyr:::expand_if_across()` is reached from
  `filter_expand()` and not from a summary.

Every summary a Margin verb stages is a named dot, so `across()` always took
the second path and reached the data mask with its head resolved by ordinary
lexical lookup.

Four runs pin the boundary, all under `across <- function(...) "CALLER"` and
`pick <- function(...) "CALLER"` with plain `dplyr::summarise()`:

| call | result |
| --- | --- |
| `summarise(g, across(c(units, qty), sum))` — unnamed | package won |
| `summarise(g, k = across(units, sum))` — named | caller binding won |
| `summarise(g, k = ncol(pick(units)))` | package won |
| `summarise(g, k = (function() pick(units))())` | caller binding won |

The last row is the `function` early return, and it is the one position where
plain dplyr lets a caller's `pick` run.

## One predecessor row is revisited

The predecessor's table reports that plain dplyr under a shadow let the caller
binding win for `pick()`. At 1.2.1 that holds only inside a `~` or a
`function`; a `pick()` anywhere else is expanded syntactically and the package
wins, as the third row above shows. The form the predecessor measured is not
recorded there, so what changed — the dplyr version, or the shape of the probe
— was not determined. Whether 1.1.4 behaved differently was not tested, and no
1.1.4 library was available to test it against.

Nothing else in the predecessor's tables was contradicted.

## marginplyr's walk reaches further than dplyr's expansion

`captured_call_parts()` excludes only plainly written `quote()`-family
captures, so marginplyr's own walk descends into a `function` or a `~` written
as an argument. It therefore analysed helpers in a position where dplyr's
expansion does not reach, which is a second place the analysis and the
execution disagreed, and one the predecessor did not look at.

The reach stops short of a function literal used as a *call head*. Measured:
`summarize_with_margins(d, k = (function(z) dplyr::cur_group_id())(1),
.grouping = rollup(region))` returned values rather than the refusal, because
`static_call_name()` answers a call whose head is itself a call as no name and
the walk maps over arguments alone, so the body is never visited. Nothing
inside such a literal is analysed or rewritten, which makes the blind spot
uniform rather than a disagreement.

## Head qualification was measured to close every position

Under the same shadows, `dplyr::across`, `dplyr::if_any`, `dplyr::if_all`, and
`dplyr::pick` each ran dplyr's function in the named, nested, and lambda
positions, and the unnamed top-level `dplyr::across()` was still expanded
statically — both dplyr entry points accept `ns = c("", "dplyr")`.

`dbplyr:::partial_eval()` matches all four names with `is_call(call, "across")`
and no `ns` argument, and `rlang::is_call()` documents that a `NULL` namespace
does not participate in the match; `is_call(quote(dplyr::across(x)), "across")`
was confirmed `TRUE`. Runs on local, dtplyr, and DuckDB inputs then agreed
value for value between the shadowed and unshadowed calls for `across()`,
`if_any()`, `if_all()`, and `pick()`.

## Nine functions read a spelling, not four

The predecessor lists four, and adds `grouping_arg_spec()` in passing as a
fifth. Counted at `7f73d5e` by grepping `R/` for each recognized spelling as a
string literal and following every match to the function containing it, then
reading each of those for how it tested the namespace. Starting from the
namespace test instead would have missed the one function that has none, which
is the row the count exists to surface. Nine functions across four files read a
spelling:

| function | file | spellings | namespace test |
| --- | --- | --- | --- |
| `grouping_helper_name()` | `R/grouping-context.R` | `grouping_bit`, `grouping_id` | `marginplyr` |
| `find_summary_context_helpers()` | `R/summary-selections.R` | the five `cur_*` | `dplyr` |
| `rewrite_summary_selections()` | `R/summary-selections.R` | `across`, `if_any`, `if_all`, `pick` | `dplyr` |
| `known_data_frame_output_names()` | `R/summary-selections.R` | `tibble`, `data_frame`, `data.frame`, `pick`, `across` | four separate tests |
| `share_helper_call_kind()` | `R/share.R` | derived from `share_kind_rules()` | `marginplyr` |
| `share_helper_function_kind()` | `R/share.R` | the same, in reference position | `marginplyr`, written as `is_symbol(expr[[2L]], "marginplyr")` |
| `is_across_call()` | `R/share.R` | `across` | `dplyr` |
| `contains_selection_predicate()` | `R/share.R` | `where` | **none** |
| `grouping_arg_spec()` | `R/grouping-plan.R` | derived from `grouping_kind_rules()` | `marginplyr` |

Eleven namespace tests among them, four of those inside
`known_data_frame_output_names()` alone, and one function with none. Two
derived their spellings from a rules table; the rest wrote them out.

The `tibble()` and `data.frame()` rows are read statically for their output
names and are never rewritten, which is why they are the one group here whose
recognition changes nothing about what runs.

## Reproduction

The probe scripts are not committed. Each was a `pkgload::load_all(".")` or a
bare `library(dplyr)` followed by `tryCatch()` around the calls in the tables
above, printing the result or the condition's message. The source readings were
`print(dplyr:::summarise_cols)`, `print(dplyr:::expand_across)`,
`print(dplyr:::expand_pick_call)`, and `print(dbplyr:::partial_eval)`.
