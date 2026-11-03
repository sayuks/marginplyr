# Grouping specification lexical and dots selection observations

Investigated: 2026-11-03
Baseline: `4b34fdecb2d8a41034b3d9b5d5af8e51592555e1` (`origin/main`)

This track established no new Grouping specification defect. It compared Grouping
specification selections and fixed keys with independent typed tidyselect
answers, concentrating on lexical defaults, aliases, argument matching and
forwarded dots. No package implementation or permanent regression test was
changed.

## Readings and previous boundaries

The readings included `CLAUDE.md`, its `AGENTS.md` closure, `CONTEXT.md`,
ADRs 0007, 0008, 0013 and 0026, and the Grouping specification constructors,
compiler, selection classifier and inspection implementation. The November 2
[expansion specification record](../../2026-11-02-expand-grouping-greedy/spec/README.md),
[selection-effect and empty-dots record](../../2026-11-02-nest-grouping-hunt/spec/README.md),
October 31 [closure and identity record](../../2026-10-31-nest-spec-greedy/spec/README.md),
and October 30 [lifecycle record](../../2026-10-30-nest-spec-greedy/spec/README.md)
supplied prior coverage. Their measurements were not counted as this track's
execution. Relevant helper-argument, retained-selection and qualified-helper
tests were inspected after forming the differential hypotheses.

## Lexical/default selection differential

Hypothesis: classification of a closure's body or defaults as independent of
column types could disagree with its actual evaluation, causing a false
failure or selection from the names-only proxy. The distinguishing observation
was disagreement with direct typed `tidyselect::eval_select()` using the same
expression and lexical environment.

`closure-selection-differential.R` exercised 251 expressions over five local
frames and six public positions: `.by`, `grouping_set()`, `grouping_sets()`,
`rollup()`, `cube()` and `grouping_spec()`. Helpers were bare, qualified,
parenthesized, or wrapped in `unname()`/`c()`. Forty argument expressions
included type-dependent functions, name-only functions, aliases, chained
forwarding defaults, partial argument matching, default arguments referring to
other formals, an independently captured lexical environment, anonymous
closures, recursive helpers, explicit and empty dots, and local assignments.
The five frames changed column types, row count and column order.

All 7,530 comparisons were accounted for:

- 7,330 selected-column matches;
- 200 required empty rollup/cube refusals; and
- zero unexplained differences.

The direct oracle did not call the package compiler. For constructor
positions, the assertion compared the union of selected dimensions, not the
entire expanded family or its order. The latter contract was already probed
in the previous records and was not claimed by this experiment. The final log
retained the environment's `sessionInfo()`.

## Dots and dot-dot lookup differential

Hypothesis: the special lookup of `..1` and its neighbors could evade the
ordinary lexical-binding test and force a type-dependent dots promise against
the names-only proxy. Empty `...` had been a reported boundary, and its repaired
helper classifier did not by itself establish the dot-dot forms.

`dots-selection-differential.R` exercised eleven helper expressions, eight
argument shapes, three frames and the same six public positions: 1,584
comparisons. The expressions used `...`, `..1`, `..2`, `..3`, `..4` and `..0`,
including repeated, parenthesized, `base::c()` and list/unlist spellings. The
arguments included no arguments, literal names, deferred calls selecting
character columns from `peek_data()`, multiple deferred arguments, an explicit
missing marker, and a missing first argument followed by a supplied one.

Both paths created the selection quosure inside their own actual dots frame.
Fresh wrappers supplied independent promises and a calibrated force counter
for each native/package pair. Expected type-dependent selection was therefore
not obtained by forcing a promise before the public call.

All observations were accounted for:

- 588 selected-column matches;
- 954 matching External-condition leading classes;
- 42 required empty rollup/cube refusals; and
- matching caller force counts for every pair.

The dot-dot helper argument was deferred to typed selection rather than forced
by the classifier. A separate exploratory bare `..1` spelling was excluded:
direct tidyselect itself interpreted it as a nonexistent column, so it did not
supply a supported forwarding contract. The exploratory script was replaced
by the asserted matrix rather than retained as another observation count.

## Replay, environment and limits

From the investigated checkout root:

```sh
Rscript investigation/2026-11-03-expand-grouping-greedy/spec/closure-selection-differential.R
Rscript investigation/2026-11-03-expand-grouping-greedy/spec/dots-selection-differential.R
```

Both final commands exited zero. Each script also accepts
`MARGINPLYR_HUNT_SOURCE` to load a separate source checkout; output is written
relative to the working directory. The adjacent CSVs retain every comparison;
logs retain the aggregate dispositions. Testthat parallel execution was
explicitly disabled, and every owned execution was collected through its
terminal result. No suite, package build, installation or site render ran in
this track.

The environment was R 4.6.1 on aarch64 macOS Tahoe 26.6.2 with dplyr 1.2.1,
tidyselect 1.2.1, rlang 1.3.0, tibble 3.3.1, vctrs 0.7.3 and pkgload 1.5.3.
These finite negative observations did not establish arbitrary caller-code
correctness, all condition attributes or backtrace equality, every active
binding/reentrant handler, forged specification field behavior, unbounded
Grouping families, lazy backend metadata, other operating systems or dependency
versions. No specification candidate required a new issue. The SQL findings below were
owned and searched for publication by the SQL track.

## Independent challenge of the SQL findings

After the specification probes, this track independently challenged the SQL
track's collapse/public-schema and direct Boolean-word CTE findings. It read
both minimized reproductions, their causal controls, ADR 0031 in full, the
saved complete Issue bodies/comments for #976, #1136 and #826, and the
implementation PR #989/#1150 bodies/comments. The installed dbplyr
`collect.tbl_sql()`, `collapse.tbl_sql()` and exported `sql_options()` functions
provided the local primary source for the supported options and operation.

`sql-boundary-witness.R` used different values and public spellings from the
primary reproductions:

- An included-only expansion over integer key values 2/1, payload 29/19, a
  public occurrence identifier and first Margin order reported three public
  columns. Direct collection returned those columns. `collapse()` still
  reported the same three names, while collecting the collapsed table returned
  two additional private order columns. Ordinary source collapse and collapse
  after explicit materialization retained exactly the public schema. Public
  values were compared after an explicit key sort, without claiming that
  collapse preserved the earlier Margin order.
- An unsorted rollup with mixed-case `FaLsE` payload 29/19 collected directly.
  Rendering with `sql_options(cte = TRUE)` produced `group` and `column2`;
  direct collection with that option raised `simpleError`, `undefined columns
  selected`. An ordinary identity-filtered source and identity-filtered
  explicit materialization both collected with the expected public schema and
  values under the same option.

The witness's observation mode exited zero. Its `--assert-collapse` and
`--assert-cte` modes each exited one on the pinned baseline, after successful
ordinary controls. Both SQL defects were independently reproduced; the
specification comparison counts above do not include these witnesses.

An initial witness compared direct and collapsed data-frame/tibble objects in
physical order, overclaiming an order/class promise; it was corrected to compare
ordinary tibbles sorted explicitly by the public key. A later control tried CTE
collection directly from a newly materialized bare table. That failed inside
ordinary dbplyr's CTE renderer with `query_list[[n]]`; the final control added
an identity filter, matching the primary reproduction's independently
successful source control. Neither initial fixture failure was counted as a
package finding or a successful final control.

ADR 0031 delegated derived-result types and Margin order to dbplyr, while
promising that a later verb saw only public columns. Its explicit exposure of
private columns through raw `sql_render()` did not license their exposure from
ordinary `collect(collapse(result))`. The #976/#989 repair concerned a Margin
verb consuming the result for re-expansion, rather than ordinary collapse.
The #1136/#1150 CTE regression exercised derived query layers; it did not
exercise direct companion-query collection under CTE rendering. The direct
CTE finding was a residual alias boundary under the public contract already
established by #826, rather than a new type/order guarantee. Separate follow-up
tickets for the two independently verifiable behaviors were justified; neither
repair was established as a prerequisite for the other.
