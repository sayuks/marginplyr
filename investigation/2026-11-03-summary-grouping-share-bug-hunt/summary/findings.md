# Local summary and across-naming investigation

Investigated: 2026-11-03
Repository snapshot: `4b34fdecb2d8a41034b3d9b5d5af8e51592555e1`
Scope: investigation only; no package implementation or regression-test edits

Two nonduplicate defects were confirmed at the snapshot above. The scripts
loaded that checkout with `pkgload::load_all()`. `environment.log` recorded
R 4.6.1, aarch64 macOS Tahoe 26.6.2, dplyr 1.2.1, dbplyr 2.6.0,
dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1, glue 1.8.1,
rlang 1.3.0, tidyselect 1.2.1, vctrs 0.7.3 and pkgload 1.5.3.
No suite, build, render or package check was run in this lane. Every launched
reproduction reached a collected terminal result before another started.

## F1: local pick first-dots references lost promise capture

Suggested priority: P2. This was a remaining helper boundary after closed
[#1138](https://github.com/sayuks/marginplyr/issues/1138), rather than an
unrepaired across/if_any/if_all reproduction.

```r
data <- data.frame(x = 1:3)
dplyr::summarise(
  data, value = (function(...) sum(dplyr::pick(..1)))(x)
)
# value = 6L
marginplyr::summarize_with_margins(
  data, value = (function(...) sum(dplyr::pick(..1)))(x)
)
# Can't select columns that don't exist.
# Column `..1` doesn't exist.
```

The `.by`-free call above needed neither shares nor a multi-set plan. Fixed-key
and rollup calls failed in the same way. An ordinary successful data-mask call
became an error in the Margin verb. The owning public contract was
*Relationship to dplyr summaries* in `R/summarize_with_margins.R`, together with
ADR 0007 and ADR 0019's *The rule* and evaluated-head amendment: the recognized
pick helper retains ordinary evaluation of other names and function arguments.
The `..1` token here was an R dots-argument reference, not an input column name.

`pick-forward-matrix.R` completed 504 comparisons:

| Investigated boundary | Count | Outcome |
| --- | --- | --- |
| First/second/two dots references, named forwarding, qualified/head-parenthesized/whole-parenthesized/qualified-parenthesized pick | 288 | Ordinary success; Margin error |
| All-dots pick, literal selection, c-dots wrapper, all_of-dots wrapper, across first-dots and across all-dots controls | 216 | Complete ordinary/Margin agreement |

The factors were three eager input classes (data.frame, tibble and data.table),
unpartitioned/fixed/rollup calls, named/unnamed output and first/preceded
ordinary position. `pick-forward-results.csv` retained each case. The ordinary
rollup oracle summarized each grain and combined its rows; no backend equality
assumption was needed.

The causal alternatives were loss of the inline function's complete lexical
scope, confusion between dots-column names and dots promises, and a generic
inline-selector failure. The working all-dots and wrapper controls refuted the
first alternative; the working across `..1` control refuted the third.
`rewrite_local_pick_selection()` combined written arguments as `c(..1)` and
passed that quoted language to `local_summary_selection()`. Resolving that raw
symbol with tidyselect treated it as a column selection. By contrast,
`rewrite_local_across_selection()` recognized raw dots references and used
`local_forwarded_selection()` to capture the runtime forwarded promise first.
No analogous pick capture occurred.

The tests in `test-local-selector-forwarding.R` covered across/if_any/if_all,
matching #1138's explicit acceptance criteria, and omitted pick. The full body
and comments of #1138 and [PR #1149](https://github.com/sayuks/marginplyr/pull/1149)
were read; the PR's body, commits and discussion stated the three-helper scope
and contained no intentional pick exclusion or separate pick contract.
#1070 repaired general runtime lexical scope, #953 addressed a genuinely empty
pick, #1151 covered unnamed top-level across expansion, and #1154 concerned an
omitted separately written ordinary formal. Their retained full bodies and
comments did not describe this populated pick dots-reference failure.

## F2: across names were expanded one scalar pair at a time

Suggested priority: P2. The smallest silent result difference used a pure
vector-sensitive naming template and no share:

```r
data <- data.frame(x = 1:3, y = 5:7)
source <- dtplyr::lazy_dt(data)
dplyr::collect(dplyr::summarise(
  source, dplyr::across(c(x, y), sum,
    .names = "{.col}_{seq_along(.col)}")
))
# x_1 = 6L, y_2 = 18L

dplyr::collect(marginplyr::summarize_with_margins(
  source, dplyr::across(c(x, y), sum,
    .names = "{.col}_{seq_along(.col)}")
))
# x_1 = 6L, y_1 = 18L
```

The returned measure values agreed, but their public names did not. Replacing
`seq_along(.col)` with `length(.col)` gave ordinary x_2/y_2 and Margin x_1/y_1.
Replacing it with `rev(.col)` gave ordinary x_y/y_x and Margin x_x/y_y.
Neither example used mutable external state or unsupported input-column names.
The owning ordinary-summary contract was *Relationship to dplyr summaries* in
`R/summarize_with_margins.R`. Margin naming analysis did not establish a second
public meaning for ordinary across's `.names` glue context.

A local no-share call kept dplyr's correct x_1/y_2 names. Adding
`fraction = share_of_total(y_2)` instead refused an unknown preceding summary,
although the successful ordinary/local control defined that scalar y_2.
The same scalar-pair expansion named outputs of an across-style contextual
share incorrectly: for two preceding sources x and y, the template above
produced x_1/y_1 instead of x_1/y_2. `R/share.R`'s *Column-wise shares* discussion
required a nonempty unique `.names` result, which the intended vector names
satisfied.

`vector-names-matrix.R` completed 228 cases:

| Lane | Scope | Outcome |
| --- | --- | --- |
| Ordinary summaries | Six templates, one function or two-function list, fixed/unfixed, local/dtplyr/Arrow | 64 agreements; eight wrong dtplyr public-name sets |
| Local share source | Six templates, one/two functions, three eager input classes, fixed/unfixed rollups, Parent/Total | 96 false source refusals; 48 standard/elementwise-template controls succeeded |
| Share output names | Six templates, Parent/Total | Eight wrong output-name sets; four standard/elementwise controls agreed |

The four investigated templates used seq_along, length, rev and match on the
complete glue `.col` vector. Standard `{.col}_{.fn}` and an elementwise
`toupper(.col)` template were controls. `vector-names-results.csv` held every
case. Two initial harness runs incorrectly tried a Parent share without a
rollup and then a forbidden empty rollup; their failed harness assertions were
not package findings. The retained successful run used ordinary and fixed-key
rollups and had terminal exit zero.

Additional controls in `vector-name-literals.R` established a lawful
vector-valued glue template: two selected columns and
`.names = "{c('first','second')}"`. Ordinary dplyr returned first/second on
local, dtplyr, Arrow and a simulated SQL relation. The no-share local Margin
control agreed. dtplyr, Arrow and simulated SQL Margin calls instead refused
that two-name vector as if the caller had requested two names for one column.
The SQL control established construction and SQL rendering only, not live
execution.

`known_across_output_names()` called `expand_across_name()` for each individual
column/function pair. `expand_across_name()` gave glue a scalar `.col` and
`.fn`. Dplyr's expansion gave glue the full column/function naming vectors.
A direct glue control in `reproduce-vector-names.R` reproduced the correct
whole-vector result and the incorrect scalar-pair result without running a
Margin verb. This excluded aggregation, collection and source-value validation
as the cause. `rewrite_across_selection()` used those predicted names for
single-function dtplyr normalization; source planning used them to register
eligible summaries; `plan_across_share()` used the same scalar-pair helper to
name share outputs.

`names-effects.R` exposed the same boundary's evaluation consequence. A glue
interpolation that counted its invocations but always returned the same prefix
ran once in ordinary/no-share local summaries and three times beside a direct
share source. Returning a different prefix after the first call turned the
valid source into a scalar-cardinality refusal. This was supplementary evidence;
the primary pure reproduction did not depend on an exact callback count.
The false source-cardinality message was another consequence of names differing
between prediction and evaluation, not evidence that the ordinary sum had
ceased to be scalar.

The existing `test-across-naming-conditions.R` counted evaluation of a callback
used as the `.names` argument, not an interpolation within the resulting glue
text. `test-first-local-across-conditions.R` likewise covered selector/template
conditions rather than vector-sensitive glue contexts.

Full bodies/comments were read for related reports: #100, #630, #700, #701 and
#870, plus PR #1087. #100's invalid example selected one column and returned two
names; the pure reproductions here produced exactly the required complete
name count. Its per-column guard did not establish that `.col` should lose the
vector context dplyr supplies. #630 concerned actual protected-name collisions
on native DuckDB, with stateful names; this finding needed neither a collision
nor native SQL. #700/#701 concerned unpack/function or packed-selector context.
#870 concerned catching a naming callback's failure and retrying; it did not
cover successful vectorized interpolation. PR #1087 handled first-selector
timing, templated names and escaped glue braces, without this scalar/vector
context difference.

## Duplicate searches and replay

`duplicate-searches.R` performed 18 read-only all-state issue/PR searches with
limit 100. The exact queries and results were retained in
`duplicate-searches.json`. Pick queries were `"pick" "forward"`,
`"pick" "forwarded"`, `"pick" "..1"` and `"populated" "selector"`.
Naming queries were `"across" "glue"`, `"glue" "vector"`,
`".col" "names"`, `"seq_along" "across"` and
`"template" "evaluation"`. No existing report matched either minimal
reproduction. Related full issue/PR bodies and comments were retained in the
adjacent issue-N.json and pr-N.json files; the comparisons above state why the
closest matches were distinct. No ticket was written by this investigator.

Run from the checkout root:

```sh
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/summary/reproduce-pick-forwarding.R
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/summary/reproduce-vector-names.R
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/summary/pick-forward-matrix.R
Rscript investigation/2026-11-03-summary-grouping-share-bug-hunt/summary/vector-names-matrix.R
```

All four report commands completed with exit zero on the investigated snapshot.
Adding `--assert` to either minimized reproduction required desired behavior
and deliberately exited one; the adjacent assertion logs retained those red
signals. These were research assertions rather than package regression tests.

The two exploration files `explore.R` and `planning-context.R` retained rejected
or paired-error probes. The former exposed F1 while inspecting populated dots,
wrapped selectors and ordinary function forwarding. The latter tested first
across factories capturing input values or n(); input-value captures also
failed under ordinary dplyr, and unknown dynamic source provenance was already
refused. It established no additional defect. Error-message differences whose
outer caller argument was already correctly restated were not promoted: the
cause diagnostic belonged to dplyr and its preservation was separately promised.

The evidence was limited to the finite fixtures, expression forms, classes and
installed versions above. No empty-input, other dependency-version, arbitrary
extension-class, live SQL, concurrency or interruption claim followed from
these two findings. Arrow emitted sandbox CPU-cache discovery messages during
initialization in the matrix; every Arrow comparison completed, and none was
skipped.
