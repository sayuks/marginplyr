# Follow-up: maintenance cost of zero-row output types

Investigated: 2026-09-27
Baseline: 9af58d87ca8c49ff267df66d368e1059885f5b38
Status: comparison evidence; no public contract change adopted

## Question

The user remained undecided after the initial comparison. This follow-up asked
whether test consolidation made withdrawing zero-row guarantees more valuable,
and whether extending Grouping helper guarantees remained small when ordinary
`across()` forms were included. Nonempty types, including all-missing shares,
remained required. Helper integer/double differences across backends remained
acceptable, as clarified by the user.

## Revised cost picture

| Item | Withdraw zero-row promises | Extend helper promises |
| --- | --- | --- |
| Measured implementation draft | Net -14 lines, from the initial experiment | Net +50 lines for direct and literal across forms |
| Measured test consolidation | Net -119 lines in five files | Not implemented |
| Additional tests | Existing nonempty and empty-shape coverage retained | 100-160 lines estimated before dynamic-name repair; not a measured diff |
| Remaining shared infrastructure | SQLite result collector/materializer | The same infrastructure |
| Unresolved implementation issue | Full checks and contract edits not performed | Dynamic names, class behavior, and ordinary empty-expression side effects |

The implementation-plus-test reduction was 133 net lines in the concrete
drafts. This did not include documentation changes and was not a claim that
the entire package could shrink by that amount without further review.

### Tests could shrink more than isolated type assertions

The minimal edit would remove 23 zero-row `.id`/share type assertion sites,
occupying 25 lines. That underestimated the practical consolidation.

`relaxed-tests.patch` replaced two empty-result matrices with one 61-line test
covering five query shapes and direct/finite/computed results. It retained
empty row counts, public/physical column names, representative source types,
unchanged input, and the empty-input case that produced a real grand-total row.
Existing nonempty label/type/order matrices supplied the interaction coverage
that remained relevant after withdrawing the empty-type promise.

Across the five edited test files, the draft removed 119 physical lines,
four `test_that()` blocks, and 38 static `expect_*()` call sites. Counts were
not loop-expanded expectation counts. Nonempty all-missing share tests stayed.
Both the consolidated empty test and the retained 144-line declared-boundary
file passed against the unmodified baseline; all five files parsed.

`counts.json` and `minimal-assertion-sites.json` record the counting basis.
`docs-sites.json` identifies eight maintained documentation sources needing
contract edits. Most would need rewriting rather than wholesale deletion;
generated Rd files were not counted as additional maintenance decisions.

### Across required actual output provenance

`extended-across.patch` added a 49-line function to find helper output names
using existing expression, naming, and provenance utilities. With integration
and a collector-class adjustment, the net implementation change was +50 lines.
Normal formula/function lambdas, single-expression blocks, redundant
parentheses, lists of functions, and stable `.names` templates worked.
Enclosing expressions such as `as.character(grouping_id())` appropriately
remained outside the numeric-output declaration.

The original 114-query/570-observation probe found no column-name/row-count
differences and no nonempty value/type differences in 352 observations. Its
saved `across.rds` preceded the final parenthesized-function normalization;
`probe-across.R` separately checked that normalization.

This was not a complete extension. Predicting output names separately from
dbplyr's actual expansion failed for this accepted baseline expression:

```r
k <- 0L
bump <- function() { k <<- k + 1L; k }
across(v, ~ grouping_id(), .names = "{.col}_{bump()}")
```

The baseline returned valid empty and nonempty results. The prototype declared
`v_2` while the query produced `v_3`; nonempty collection failed, and empty
collection added a spurious column. `base-across.txt` and `variant-across.txt`
preserve the observations. This was a defect in the prototype, not proof that
the desired guarantee was impossible or that this naming style was common.

Intersecting declared names with actual columns would prevent corruption but
would omit the promised repair for this case. The complete solution would
share actual name expansion with declarations or preserve output provenance
after expansion. Contextual shares already owned their resolved output names;
ordinary across expressions were delegated to dbplyr and had no equivalent
carrier. An existing single-function name normalizer offered a partial reuse,
but did not cover mixed function lists. `test-across-uncertain-output-names.R`
was further evidence that uncertain ordinary output names were accepted.

The new output-provenance analysis served zero-row helper types: in the
measured SQLite paths nonempty direct helpers already produced numeric 0/1 or
mask values and did not produce all-missing helper columns. This differed from
the shared share restoration that was needed for nonempty missing values.

The collector's one-line conversion to tibble also changed 112 pre-existing
nonempty sorted results from data.frame to tibble. Without it, four newly
covered observations changed in the opposite direction. Resolving that
pre-existing inconsistency was additional work before adopting the extension.

## How much guarantee callers would gain

The baseline documented a direct-result guarantee. Actual empty-result probes
confirmed that inserting lazy `filter()`, effective `select()`, `rename()`, or
`mutate()` before collection could lose `.id` and share types as well. Direct
compute first created a physical typed table, after which ordinary projections
could retain those types. This followed ADR 0031's existing scope.

Extending helpers at the same point would improve direct empty results, but
would not provide stable schemas for arbitrary lazy pipelines. Ordinary
summaries such as `n()` also remained outside the proposed repair.

`downstream.R` and its output showed successful `bind_rows(empty, populated)`
with numeric common types. The concrete downstream difference was type-based
selection and generated columns via `where(is.numeric)`. An all-empty batch
also retained the unstable empty schema. These were real costs to users who
needed schema processing, but not universal failures of empty-result pipelines.

## Execution cost

For a helper-only, unsorted one-set query without `.id` or shares, direct
compute used one CREATE TABLE AS SELECT at baseline. The initial extension
used SAVEPOINT, an empty typed CREATE, INSERT, and RELEASE. Thus the extension
could change nonempty execution paths as well as empty results.

On one local in-memory SQLite workload (100,000 input rows, 1,000 groups,
seven repetitions, analyze = FALSE), median compute elapsed time was 0.019 s
at baseline and 0.023 s with the extension. These tiny local measurements
established no general performance regression. Queries already using the
typed materializer would not incur this entire incremental change.
`cost.R` and the two cost outputs preserve the statement trace and timings.

## Primary-source context

[DBI's dbFetch specification](https://dbi.r-dbi.org/reference/dbFetch.html)
expected fully typed columns even for zero-row fetches.
[SQLite's column-declaration API](https://sqlite.org/c3ref/column_decltype.html)
did not provide a declared type for general result expressions. Observing an
empty logical `n()` was therefore not evidence that stable empty schemas were
undesirable; it demonstrated a backend limitation that a package could choose
to repair where it owned enough information.

[dplyr's across reference](https://dplyr.tidyverse.org/reference/across.html)
described `.names` as a glue specification, and
[glue's reference](https://glue.tidyverse.org/reference/glue.html) documented
evaluation of embedded expressions. The dynamic-name probe was consistent
with that interface rather than a new spelling introduced by the prototype.

## Recommendation from the follow-up

The recommendation remained to exclude zero-row result types consistently for
`.id`, shares, and Grouping helper outputs, retain nonempty guarantees, and
keep useful empty typing supplied incidentally by shared code. There was no
fatal flaw in stable empty types. The choice followed the user's priority:
nonempty reliability mattered more than extending empty-schema guarantees.

The strongest new evidence was the concrete test reduction and the need to
track actual helper output names, beyond the initial three-line direct-call
extension. A schema-stability requirement would justify that work, but the
conversation had not established one. Future positive demand could justify a
deliberate extension; this note did not adopt a decision or change the package.

## Limits and replay

The artifacts were disposable probes, not reviewed production patches. The
full package suite, strict coverage, and review-ready check were not run.
The implementation and test patches still required contract edits and those
checks before adoption.

Apply each patch to a disposable archive of the baseline using `git apply`.
Run `probe-across.R <checkout>` and `cost.R <checkout>` from the repository
root. Run `downstream.R` from the baseline checkout. The initial
`../zero-row-type-policy-2026-09-27/probe.R` supplied the 114-query matrix.
