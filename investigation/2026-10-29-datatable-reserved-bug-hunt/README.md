# data.table special-symbol conflict bug hunt

Investigated: 2026-10-29
Repository snapshot: `4c63bb917ed7ed31ff6aadf060a5eae7a32cd004`

The investigation started from local `main` in a new managed worktree on
`investigate/datatable-reserved-symbols`. It used R 4.6.1, data.table 1.18.6.1,
dtplyr 1.3.3, and dplyr 1.2.1. Four investigators divided summaries/shares,
expansion/nesting, derived-input metadata, and source inventory/label checks.
No package implementation, package tests, generated documentation, or package
metadata was changed. The diagnosis stopped at confirmed causes, reproductions,
and repair assertions, as requested.

Six findings were published through `to-tickets`, each labelled `bug` and
`ready-for-agent`. The user delegated ticket granularity and publication, so
the skill's interactive approval checkpoint was omitted. Every ticket had no
blocker: each repair could be verified independently without another ticket.
The tickets owned repair scope; this note recorded the dated evidence.

| Finding | Ticket | Observed effect |
| --- | --- | --- |
| F1 | [#1046](https://github.com/sayuks/marginplyr/issues/1046) | Observed label checking generated a forbidden `.SD` count-output name |
| F2 | [#1047](https://github.com/sayuks/marginplyr/issues/1047) | Share validation broke six otherwise working special-name input measures |
| F3 | [#1048](https://github.com/sayuks/marginplyr/issues/1048) | Share or identifier writes interpreted `.ROW` as row deletion syntax |
| F4 | [#1049](https://github.com/sayuks/marginplyr/issues/1049) | String/indirect lexical `.BY` assignments silently became input-column reads |
| F5 | [#1050](https://github.com/sayuks/marginplyr/issues/1050) | Explicit evaluation masks lost their `.BY` binding to an outer input alias |
| F6 | [#1051](https://github.com/sayuks/marginplyr/issues/1051) | Discarded dot-dot root names invalidated a safe visible input schema |

## Official inventory and version boundary

The official [Special symbols reference][symbols] enumerated nine spellings:
`.SD`, `.BY`, `.N`, `.I`, `.GRP`, `.NGRP`, `.EACHI`, `.NATURAL`, and `.ROW`.
That page displayed development version 1.18.99. The audit therefore also read
the [distributed 1.18.6.1 help][distributed-symbols] and its R/C source, rather
than attributing development behavior to the installed version.

[`source-inventory.R`](source-inventory.R) extracted all nine aliases from the
installed help and scanned all 420 namespace functions for reservation,
locking, evaluation-environment, and assignment evidence. The retained
[`namespace-scan.csv`](namespace-scan.csv) and
[`source-inventory.txt`](source-inventory.txt) recorded the matches and runtime.
[`upstream-sources.tsv`](upstream-sources.tsv) recorded SHA-256 content hashes
and version-specific URLs for the six downloaded upstream files. The scan
included `special meaning` and `may not be called`, since a diagnostic need not
contain the word `reserved` to establish a restriction.

| Official spelling | Documented context | Outcome in the audited Margin fixtures |
| --- | --- | --- |
| `.SD` | Group subset in `j` | Expansion/nesting passed; generated collision-count output failed (F1); optimized input measures failed when shares added validation (F2) |
| `.BY` | Grouping-key list in `j` | Protected input measure and row verbs passed; lexical and explicit-mask ownership failed (F4/F5); ordinary selected-column name drift was isolated upstream |
| `.N` | Group size in `j`, row count in `i` | No additional Margin-created defect was isolated; upstream input-measure refusals were retained separately |
| `.I` | Row positions in `j`, rowwise marker in `by` | No additional Margin-created defect was isolated; upstream input-measure refusals were retained separately |
| `.GRP` | Group counter in `j` | Optimized input measures failed when share validation was added (F2); row-verb comparisons passed |
| `.NGRP` | Group count in `j` | Optimized input measures failed when share validation was added (F2); row-verb comparisons passed |
| `.EACHI` | Group-by-each-joined-row marker in `by`/`keyby` | Named-column roles passed, including singleton fixed-key/dimension controls |
| `.NATURAL` | Natural-join marker in `on` | Named-column roles passed, including singleton fixed-key/dimension controls |
| `.ROW` | Row deletion target with `:= NULL` | Row verbs and no-share identifiers passed; generated share/identifier writes failed (F3) |

This was a context inventory, not a claim that each spelling was prohibited as
a column everywhere. The [bracket reference][bracket] distinguished argument
names, join qualifiers, and double-dot lookup; the [assignment reference][assign]
described the separate row-deletion operation.

## Additional source-derived candidates

The distributed source identified the following surrounding mechanisms. Each
candidate was exercised in the matrices linked below; passing a Margin fixture
did not establish support inside an unrelated upstream reserved context.

| Mechanism | Candidate spellings and evidence |
| --- | --- |
| Locked internal group bindings | `.SDall`, `.xSD`, `.iSD` in [`[.data.table`][dt-r]; all three also reproduced F2 |
| Constructor restriction | `.SD` was explicitly refused by [`as.data.table.list()`][as-dt]; the generated F1 count reached this restriction |
| Double-dot lookup/optimizer functions | `..value`, `..FUN1`, `..FUN2` in [`[.data.table`][dt-r]; positive `..j`/`...` controls separately exposed R/tibble validation in F6 |
| Join qualifiers | `i.value`, `x.value` in [the bracket reference][bracket] |
| Non-equi temporary column | `_nqgrp_` was refused in the relevant [`bmerge()`][bmerge] branch |
| Split temporaries | `.ll.tech.split`, `.nm.tech.split`, `.ll.tech.split.names` in [`split.data.table()`][dt-r]; the first two had explicit refusals in different input roles |
| Constructor/CJ arguments | `key`, `keep.rownames`, `check.names`, `stringsAsFactors`, `.named`, `sorted`, `unique` in [`data.table()`][dt-r], [`as.data.table.list()`][as-dt], and [`CJ()`][setkey] |
| Bracket arguments | `.SDcols`, `x`, `i`, `j`, `by`, `keyby`, `with`, `nomatch`, `mult`, `roll`, `rollends`, `which`, `verbose`, `allow.cartesian`, `drop`, `on`, `env`, `showProgress` in [the bracket reference][bracket] |
| Evaluation helpers | `print`, `order`, `Cfastmean`, `-.POSIXt` from [`[.data.table`][dt-r], plus ordinary `get`, `mget`, `eval`, `list`, `T`, and `F` controls |
| Namespace state/attributes | `.Last.updated`, `.internal.selfref`, `.data.table.locked`; these were not extra globally reserved column names |

The C [`dogroups()`][groups] source supplied the `.BY` and `.I` binding locks
not established by the R locking statements alone. The namespace scan also
found locking in `.onLoad()` and the locked-table refusal in `setkeyv()`;
neither added a new column spelling. `.iGRP`/`.xGRP` probes were not treated as
established special bindings.

The previous [special-name audit](../2026-10-28-dtplyr-special-name-followup/README.md)
had omitted the explicit `.SD` constructor diagnostic. Before adding its dated
revision, an exact repository-path citation search found no citing maintained
artifact. Its original measurements were preserved; the revision corrected
the inventory without reversing its nesting results.

## F1: the observed-collision query generated an illegal output name

One character `.SD = "a"` row and `.check_margin_label = TRUE` sufficed. All
four Margin verbs failed with `A column may not be called .SD. That has special
meaning.` The unchecked operation, local checked operation, and renamed lazy
control succeeded. Changing the observed value to `Total` produced the intended
collision Package condition only in the local/renamed controls.

The ranked explanations were an unsafe generated count-output name, inability
to read the source `.SD` column, and a generic unsupported collision query.
Successful source collection and unchecked execution ruled out the second.
The renamed query ruled out the third. Holding source, count expression, and
values constant while changing only the count-output name from `count` to
`.SD` reproduced the constructor diagnostic. The query protected its input
read but named its count with the caller's original special spelling.

The 61-name matrix recorded 1,952 collision decisions: 1,944 matched the local
control and eight failed, all character `.SD` dimensions with checking enabled.
Factor-level decisions and the other sixty names added no failure. Both
colliding and non-colliding values reproduced across all four verbs. Every
caller table remained unchanged.

The minimal [`labels/reproduce.R`](labels/reproduce.R) exited zero asserting
those observed failures and all successful controls. Its `--assert-correct`
mode exited one on the first non-colliding call, as recorded in
[`labels/repair-assertion.txt`](labels/repair-assertion.txt). The broader
[`labels/matrix.R`](labels/matrix.R) intentionally exited one after retaining
its eight discrepancies in [`labels/results.csv`](labels/results.csv).

## F2–F5: summaries and contextual shares

The [summary evidence note](summary/README.md) owned the minimized expressions,
full controls, generated queries, and replay commands for these four findings.

F2 required a fixed key so every no-share branch remained eligible for the
working grouped aggregate. With optimization levels 2/Inf, ordinary and
no-share sums over six special-name measures succeeded; adding either share
helper failed in 24 direct/across cases. At 0/1 those controls already refused
upstream, so the finding did not claim support there. The protected `.BY`
measure passed its four corresponding controls.

F3 comprised eight failures: share output or requested identifier named `.ROW`,
both helpers, and direct/across spelling. Ordinary output construction,
no-share identifiers, and renamed/local controls established that the generated
reference update introduced the unsupported row-deletion interpretation.

F4 and F5 silently substituted `1, 3, 4` from an unused input `.BY` for the
independent lexical result `12, 12, 12`. Renaming only that unused input column
restored the lazy result. F4 covered six assignment forms in 24 cases; F5
covered three explicit evaluation-mask forms in twelve cases. The source
probes distinguished assignment recognition from evaluation-mask ownership,
which justified separate tickets. Existing #1033 controls were retained.

The default targeted replays asserted the observed defects and passed. Their
`--assert-correct` modes required correct results, or appropriate refusals for
unsupported lexical shapes, and failed on the investigated snapshot. This
retained a red-capable repair contract without implementing a fix.

## F6 and the derived-input boundary

The [metadata evidence note](metadata/README.md) owned the full matrix,
source-isolation checks, upstream exclusions, and F6 replay.

An ordinary dtplyr selection discarded a root `..1`, `..2`, or `...` column
and collected successfully with only safe visible names. Name-only inspection
also succeeded. Typed inspection and all four Margin verbs nevertheless
revalidated the discarded root name during zero-row metadata acquisition.
There were 54 failures across three names, three row counts, and six affected
operations; nine name-only inspections and 63 rename-away controls passed.
This concerned safe visible inputs, not expanded admission of visible R dot
argument names.

The metadata audit also isolated ordinary `.BY` selection changing the physical
output name to `BY` before any Margin operation. That discrepancy was recorded
as an upstream lead, not another package issue. Value-dependent or otherwise
unsupported metadata graphs remained refusals rather than inferred bugs.

## Coverage and negative evidence

| Evidence family | Recorded comparisons | Scope |
| --- | ---: | --- |
| [Row verbs](rows/README.md) | 9,360 + 4,032 + 336 + 408 | Candidate roles, empty/populated inputs, sorting, `.keep`, typed labels, simultaneous specials, occupied aliases, result reuse |
| [Summary matrix](summary/matrix.csv) | 1,088 | 32 names; input, key, dimension, summary/share output, identifier; direct/across; no share/Parent/Total |
| [Targeted summary evidence](summary/README.md) | 28 + 28 + 8 + 48 | Measures, optimization controls, `.ROW`, lexical ownership and negative controls |
| [Typed inspection](metadata/inspection-results.csv) | 21,588 records | 67 names, three types, eighteen derivations; 24 upstream construction failures were single records |
| [Inspection isolation](metadata/isolation-results.csv) | 832 | Zero/populated, immutable/Mutable, cached/shared graphs; no row predicate execution or source mutation |
| [Discarded names](metadata/hidden-source-results.csv) | 126 | Drop versus rename-away; typed/name-only inspection and four Margin verbs |
| [Label checks](labels/results.csv) | 1,952 | 61 names, character/factor, colliding/non-colliding, checks on/off, four Margin verbs |

The investigation used deterministic fixtures and local or safely renamed
controls, checking actual values, types, row/cell membership, and source
preservation where appropriate. It did not call every raw discrepancy a
package bug. Upstream refusals, local-control errors, and the documented
metadata-safety boundary were retained separately. The row evidence established
no additional name-specific defect after its ordinary-name controls.

Recent starting points included the October 28 special-name/nesting audits,
the October 29 summary/share and expansion audits, and the closed issues named
in the subnotes. Existing `.ROW` row-verb and locked-internal-binding fixes
were replayed rather than assumed. No existing issue was reopened or changed
into a parent ticket.

## Validation and limits

Before publication, both repository linters were run: `jarl check .` and
package-aware `lintr::lint_package()` after `pkgload::load_all()`.
The always-loaded-context verifier passed at 20,008 of 22,005 bytes.
`git diff --check` checked the final notes and evidence sources.

The changes were Repository-only under the local-check policy: all new
scripts/results were investigation evidence, excluded from the source package
and used by no package check or generation input. The package Review-ready
check was therefore not invoked. Expected failing repair assertions were
recorded as failures, not presented as green package tests. CI was not watched.

The audit established findings on one package snapshot and one runtime stack.
It did not establish support for arbitrary caller code, every derived graph,
non-equi joins or raw split operations, every optimization option, or future
backend versions. The official nine were all covered; the contextual inventory
and matrices defined the additional finite scope.

[symbols]: https://rdatatable.gitlab.io/data.table/reference/special-symbols.html
[distributed-symbols]: https://github.com/cran/data.table/blob/1.18.6.1/man/special-symbols.Rd
[bracket]: https://rdatatable.gitlab.io/data.table/reference/data.table.html
[assign]: https://rdatatable.gitlab.io/data.table/reference/assign.html
[dt-r]: https://github.com/cran/data.table/blob/1.18.6.1/R/data.table.R
[as-dt]: https://github.com/cran/data.table/blob/1.18.6.1/R/as.data.table.R
[bmerge]: https://github.com/cran/data.table/blob/1.18.6.1/R/bmerge.R
[groups]: https://github.com/cran/data.table/blob/1.18.6.1/src/dogroups.c
[setkey]: https://github.com/cran/data.table/blob/1.18.6.1/R/setkey.R
