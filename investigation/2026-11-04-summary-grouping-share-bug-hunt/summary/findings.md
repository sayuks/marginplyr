# Noncanonical numbered dots in ordinary summaries

Investigated: 2026-11-04
Repository snapshot: `78ebbc36200bdc5cef70838dd4e772c5c4a1401c`
Scope: investigation only; no package implementation or regression-test edits

One defect was confirmed across two ordinary-summary boundaries. R accepted
numbered dots references beyond the canonical spelling `..1`; local Margin
rewriting recognized only the canonical positive-decimal spelling. The resulting
selection failures and altered function-factory evaluation belonged in one
ticket because they shared this incomplete syntax predicate. No implementation
was changed.

The scripts loaded this checkout with `pkgload::load_all()`. `environment.txt`
recorded R 4.6.1 on aarch64 macOS, dplyr 1.2.1, rlang 1.3.0, tidyselect 1.2.1,
vctrs 0.7.3, pkgload 1.5.3, tibble 3.3.1 and data.table 1.18.6.1. No full suite,
build, rendering, or package check was used as an investigation loop. Each
launched reproduction was collected before the next started.

## Contract

The *Relationship to dplyr summaries* section of
`R/summarize_with_margins.R` promised dplyr data-masking semantics for ordinary
summary expressions. ADR 0007 retained the captured expression's environment.
ADR 0019 distinguished static Contextual helper recognition from ordinary
lookup and evaluation of other names, and preserved written across evaluation
boundaries. A numbered dots argument denoted an R promise, rather than an input
column whose name happened to equal its spelling.

Closed [#1138](https://github.com/sayuks/marginplyr/issues/1138),
[#1182](https://github.com/sayuks/marginplyr/issues/1182), and
[#1170](https://github.com/sayuks/marginplyr/issues/1170) supplied adjacent
repaired boundaries. Their canonical and all-dots reproductions succeeded on
this snapshot. The November 2 and 3 summary investigations guided the new
syntax dimension; their dated findings were left intact.

## S1a: selector forwarding became a missing-column error

With a data frame holding x = 1:3:

```r
dplyr::summarise(data, value = (function(...) sum(pick(..01)))(x))
# value = 6L
marginplyr::summarize_with_margins(
  data, value = (function(...) sum(pick(..01)))(x)
)
# Can't select columns that don't exist.
# Column `..01` doesn't exist.
```

Changing only `..01` to `..1` made the Margin call return six. Neither a
Grouping plan with several sets nor a share request was needed. The same
boundary affected `across()`, `if_any()`, and `if_all()` selectors, including
qualified helpers. The smallest replay was `reproduce-noncanonical-dots.R`.
Its report mode exited zero; its `--assert` mode required agreement with
dplyr and exited one.

`noncanonical-dots-matrix.R` completed 576 ordinary/Margin comparisons:

| Dimension | Values |
| --- | --- |
| Eager input | data.frame, tibble, data.table |
| Grain | unpartitioned, fixed key, rollup |
| Helper | pick, across, if_any, if_all |
| Qualification | bare, dplyr-qualified |
| Canonical controls | `..1`, `..2` |
| Additional R dots spellings | `..01`, `..001`, `..02`, `..+1`, `.. 1`, `..\t1` |

The 432 additional-spelling cases all failed only in the Margin call. All
144 canonical controls agreed. Every ordinary call succeeded, and the loop
found no successful Margin result with a different value in this selector
matrix. `noncanonical-dots-results.csv` retained all outcomes and the final
missing-column diagnostic, without repeating generated backtraces.

## S1b: dynamic unpacking changed a forwarded factory's values

A local summary using a function factory exposed the same incomplete dots
recognition at the function argument. The factory counted its calls, captured
that count as an offset, and returned a function producing a one-column data
frame. A wrapper forwarded the factory through its dots to an unnamed
`across(x, ..01, .unpack = unpack)`, where unpack was bound to TRUE. With
g = a,a,b and x = 1,2,3:

| Call | Factory calls | x_value |
| --- | --- | --- |
| Ordinary dplyr, `..01` | 2 | 5, 4 |
| Margin, `..01` | 1 | 4, 4 |
| Margin, canonical `..1` | 2 | 5, 4 |

The offset made the lost evaluation observable as a wrong successful summary,
rather than only a callback-count difference. `reproduce-forwarded-factory.R`
retained the direct written wrapper. Its report mode exited zero; its
desired-contract `--assert` mode exited one. Dynamic FALSE was an agreement
control: its ordinary evaluation path legitimately used one factory call.
There was no universal required factory-call count.

`forwarded-factory-matrix.R` completed 72 comparisons across the same three
eager classes, eight dots spellings, and unpack values TRUE, a string template,
and FALSE. The 36 noncanonical TRUE/string cases returned different values
and counts; the 36 canonical or FALSE controls agreed. The retained CSV named
both the values and counts. No condition or skip replaced a comparison.

## Causal alternatives and observations

The ranked alternatives were:

1. An incomplete numbered-dots predicate skipped promise capture. It predicted
   canonical success and failure for every helper using the same predicate,
   together with a function-forwarding boundary using that predicate.
2. R or dplyr itself refused the additional spellings. It predicted that the
   ordinary calls would fail as well.
3. Pick alone lost the inline function's lexical scope. It predicted agreement
   for the other helper families and no independent function-argument symptom.

The successful ordinary controls refuted alternative 2. The four-helper
matrix and dynamic factory reproduction refuted alternative 3. The source
read supported alternative 1: `rewrite_local_across_selection()` and
`rewrite_local_pick_selection()` recognized a symbol using
`^\\.\\.[1-9][0-9]*$`. `wrap_local_frame_summaries()` used that same narrow
test before preserving a forwarded function argument. A canonical argument
took the promise-capturing path; `..01` instead remained raw quoted selection
language or was wrapped for function-kind validation. R itself supplied the
actual accepted-spelling oracle; this investigation did not propose a new
hand-maintained grammar.

The existing local forwarding tests covered all-dots and canonical first/later
numbered references, nested forwarding, output names, missing columns and
grouping exclusions. The factory tests covered dynamic unpacking, ordinary
function lists, and parenthesized calls, but no noncanonical numbered
reference. Those tests therefore did not establish the newly varied syntax
boundary.

## Duplicate comparison

`duplicate-searches.R` issued 14 all-state issue/PR searches with a per-query
limit of 100. The queries were `"noncanonical"`, `"..01"`,
`"dots" "signed"`, `"dots" "leading"`, `"dots" "whitespace"`,
`"numbered" "forward"`, and `"unpack" "factory"`. Each query returned
fewer than 100 results. `duplicate-searches.json` retained exact queries,
identifiers, states, titles and links. No existing report described the
additional-spelling reproductions. Relevant full issue bodies and comments
were read and retained for #1138, #1182, #1170 and #1002; relevant PR bodies
were retained for #1174 and #1192.

The first three reports and their merged repairs established the canonical
boundaries used as successful controls here. This was a remaining syntax
boundary after those repairs, rather than a replay of their broken canonical
examples. [#1002](https://github.com/sayuks/marginplyr/issues/1002) concerned
generated column reads for signed or whitespace *column names*, not an actual
numbered promise forwarded by an R dots function. It supported keeping those
two meanings distinct and did not repair this direction.

## Additional bounded probes and limitations

Parenthesized or c-wrapped numbered selections were paired ordinary/Margin
errors, so they were not findings. Trying to add an input column with the same
noncanonical name also produced paired ordinary/Margin errors and established
no silent selector misread. A dynamic FALSE factory with a scalar result
agreed for canonical and noncanonical references. A pure factory reading n()
also agreed in the tested form; it was not promoted as another discrepancy.

Evidence was limited to the finite fixtures, installed versions, expression
forms and eager input classes above. No lazy-backend, hosted-service, arbitrary
extension-class, other dependency-version, concurrency, interruption, or
unbounded-grammar guarantee followed. Suggested priority was P2. The proposed
ticket had no blocker and called for ordinary dplyr values, names and evaluation
to survive the accepted dots spellings while retaining existing collision,
grouping-exclusion, missing-column and share-dependency protections.

## Replay

From the investigated checkout root:

```sh
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/summary/reproduce-noncanonical-dots.R
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/summary/reproduce-forwarded-factory.R
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/summary/noncanonical-dots-matrix.R
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/summary/forwarded-factory-matrix.R
```

Both minimized scripts accepted `--assert` for a deliberately red desired-
contract signal. The adjacent logs retained terminal outputs. These were
standalone research harnesses, not installed regression tests.
