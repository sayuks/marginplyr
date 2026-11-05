# Contextual-share accessor and deferred namespace investigation

Investigated: 2026-11-05
Repository snapshot: `0403c9150394cbae21b59f34c75791d0873b8451`

This lane examined Parent shares and Total shares without changing package
implementation or package tests. The source was loaded with pkgload from the
recorded main snapshot. [Environment](environment.txt) records R 4.6.1 on
`aarch64-apple-darwin23`, dplyr 1.2.1, dtplyr 1.3.3, data.table 1.18.6.1,
rlang 1.3.0 and pkgload 1.5.3. Testthat parallel execution was disabled.

The recent November 2–5 summary/share investigations supplied repaired controls,
particularly #1212, #1311 and #1318. Their recorded defects were not counted as
new findings. Two residual boundaries remained reproducible on the snapshot.
They were independent: one concerned static dependency classification before
summary execution, the other generated function lookup during explicit lazy
execution. Neither repair logically required the other.

## S1. Literal mask access through equivalent accessor forms lost dependencies

ADRs 0010 and 0017 require an ordinary summary to refuse a read of an earlier
share and require a share source to be self-contained rather than reading an
earlier ordinary summary alias. Both rules were bypassed by literal accessors
that reached the same mask as the repaired accessors.

The fixture had groups a/b, x = 1/3, and input prior = 100/300. This call
returned observed = 100/300/400 alongside prior = .25/.75/1:

```r
summarize_with_margins(
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300)),
  value = sum(x), prior = share_of_total(value),
  observed = sum(parent.env(environment())[["prior", exact = TRUE]]),
  .grouping = rollup(g), .sort = "last"
)
```

Removing only the explicit `exact = TRUE` produced the intended Package
condition. The documented reverse rule also failed: define prior = sum(x),
use that same accessor as observed, then request a share of observed. It
returned observed = 1/3/4 and shares .25/.75/1 instead of refusing the alias
dependency. Parent shares agreed with Total shares on this one-dimensional
rollup.

Equivalent bypasses used `exact = FALSE`, base `getElement()` or `.subset2()`
on that literal mask environment, or `rlang::env_get()` with the base current
`environment()` spelling. The repaired `rlang::current_env()` spelling also
bypassed validation when an explicit `last` was `emptyenv()`, `baseenv()` or
`globalenv()`; those stop frames did not cut the fixture's path to the mask.

The ranked hypotheses were: (1) accessor/arity recognition lost the literal
field name, (2) summary staging used an unexpected grouping result, or (3)
the accessors themselves had incompatible evaluation semantics. The standard
bracket, dollar, base get and current-env accessor controls correctly refused
both dependencies, refuting (2). Successful ordinary evaluation and equivalent
values from each accessor refuted (3). Source inspection isolated (1): the
explicit mask bracket branch required a three-part call, other base accessor
names reached ordinary field traversal, and the inheriting rlang reader
recognized only current_env while declining every supplied last frame. No
caller code needed to run to distinguish the reproduced literal forms.

[The matrix](accessor-matrix.R) completed 192 public calls across data.frame,
tibble and data.table inputs, both helpers and both dependency directions:
96 missing refusals, 48 correct repaired refusals and 48 valid independent
external-environment reads. Original input bytes were unchanged.
[Results](accessor-results.csv) retain each case. `--assert` intentionally
exited 1 at the first missing Package condition.

## S2. Deferred dtplyr share namespaces remained caller-resolved

ADRs 0010, 0014 and 0017 require shares to use the displayed ordinary values.
The caller's namespace operators could replace package-selected conversion or
denominator functions after the query was constructed:

```r
root <- new.env(parent = globalenv())
root$source <- data.frame(g = c("a", "b"), x = c(1, 3))
input <- evalq(dtplyr::lazy_dt(source), root)
query <- summarize_with_margins(
  input, value = sum(x), ratio = share_of_total(value),
  .grouping = rollup(g), .sort = "last"
)
root$`::` <- function(pkg, name) {
  package <- as.character(substitute(pkg))
  member <- as.character(substitute(name))
  if (identical(package, "base") && identical(member, "as.double")) {
    return(function(x) rep(10, length(x)))
  }
  getExportedValue(package, member)
}
dplyr::collect(query)
```

Observed value was 1/3/4 but ratio was 1/1/1, rather than .25/.75/1.
A selective `:::` dispatcher replacing only the private dtplyr denominator
function with a constant ten yielded detail ratios .1/.3 in the same fixture.
The no-share Margin query stayed correct and never invoked either selective
callback. Removing the callback restored the correct shares. A separate
exploratory dispatcher replacing only the private source validator returned
ordinary values 0/0/0 and missing/missing/1 shares; this supported the same
generated-namespace boundary rather than a third ticket.

The ranked hypotheses were: (1) the namespace operator in generated deferred
heads remained ordinary root lookup, (2) ordinary caller summary evaluation
was corrupt, or (3) denominator selection was intrinsically wrong. Correct
no-share values and preserved displayed values refuted (2). Restoring only
the root binding restored ratios, refuting (3). The generated base-qualified
ratio expression and private-qualified denominator call isolated (1).
Construction invoked no selective callback; explicit collection did. The
analysis did not require early execution of the caller's lazy input.

[The matrix](namespace-matrix.R) completed 96 cases: both helpers, fixed/no
fixed partitions, direct/across sources, early/late root binding, both
namespace operators and counting/raising/corrupting callbacks. There were
32 unwanted callback cases, 32 callback collection errors and 32 wrong-ratio
cases. Every construction and no-share control invoked zero callbacks,
restored results agreed with the no-share displayed values, and source bytes
were unchanged. [Results](namespace-results.csv) retain each case. `--assert`
intentionally exited 1 on the first intercepted operation.

## Duplicate comparison and ticket boundaries

All-state GitHub issue searches used `"getElement"`, `".subset2"`,
`"env_get"`, `"namespace operator"`, `"namespace" "dtplyr"`, and
`"exact" "share"`; the latter was a broad search rather than evidence of
semantic equality. All-state PR searches used `"mask" "share"` and
`"namespace" "share"`. Selected result JSONs were retained beside this note.
Full bodies and comments were read for #1318, #1316, #1311, #1212 and #491,
and for merged PRs #1332 and #1219. PR #1338's body was read too.

- [#1318](https://github.com/sayuks/marginplyr/issues/1318) and
  [#1332](https://github.com/sayuks/marginplyr/pull/1332) required the
  demonstrated literal dollar/bracket and current_env accessor forms; all
  passed as refusal controls here. S1 extended those repaired forms with an
  explicit bracket formal, equivalent base accessor names and equivalent
  rlang environment/stop-frame spellings. These were grouped into one bounded
  accessor follow-up, rather than separate issues per spelling.
- [#1212](https://github.com/sayuks/marginplyr/issues/1212) and
  [#1219](https://github.com/sayuks/marginplyr/pull/1219) repaired same-named
  arithmetic/helper bindings by qualifying generated ratio heads. S2 replaced
  the namespace dispatch those qualified heads still evaluated, including a
  private denominator head outside that ratio repair.
- [#1311](https://github.com/sayuks/marginplyr/issues/1311) and
  [#1338](https://github.com/sayuks/marginplyr/pull/1338) isolated generated
  selection and nested-mask operations. Their bounded reproduced operations
  did not cover dtplyr's root-environment share execution. S2 concerned that
  separate deferred boundary. #491 introduced private qualification to make
  internal names readable to ordinary dtplyr users; it did not establish that
  a qualified call bypasses a caller namespace operator.

No matching open issue or merged repair for these residual forms was found in
those searches. Both proposed issues were independently verifiable without
blockers. All namespace variants in this lane belonged to one generated
execution follow-up. Local naming/glue variants were owned by the summary
lane and were not counted here.

## Replay and limits

Run from the recorded source root, using an absolute path to these scripts if
the evidence resides outside an archive of that snapshot:

```sh
Rscript investigation/2026-11-05-summary-grouping-share-greedy/share/accessor-matrix.R
Rscript investigation/2026-11-05-summary-grouping-share-greedy/share/accessor-matrix.R --assert
Rscript investigation/2026-11-05-summary-grouping-share-greedy/share/namespace-matrix.R
Rscript investigation/2026-11-05-summary-grouping-share-greedy/share/namespace-matrix.R --assert
```

Observation modes exited 0 and desired-contract modes exited 1. Scripts
retained case CSVs from the observation runs. Replays print totals and write
a CSV only when `MP_SHARE_HUNT_OUTPUT` names an existing output directory;
they therefore run from an archive without creating an evidence directory.
There was no package fix, full-suite run, build or implicit source collection
in this lane. Hosted databases and arbitrary dynamically computed reflection
were outside these bounded results. The source environment and exact SHA
limit the conclusions.
