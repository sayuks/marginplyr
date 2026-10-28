# data.table special column spellings after PR #961

Investigated: 2026-10-28

This follow-up investigated other data.table-specific spellings after the
`.SDall`, `.xSD`, and `.iSD` fix in PR #961. The package snapshot was
`9a7558ac483fe80c2f26daa04ef51360b369c74d`; the runtime used data.table
1.18.6.1 and dtplyr 1.3.3. The investigation distinguished names reserved in a
particular expression or API from spellings that were ordinary column names
outside that context.

## Primary-source inventory

The installed data.table namespace was searched by deparsing every function
and inspecting `reserved`, `lockBinding`, `assign`, and `SDenv` occurrences.
The relevant bodies were inspected alongside the distributed source in the
[CRAN data.table 1.18.6.1 mirror][dt-r]. The online
reference pages displayed development version 1.18.99, so the distributed
source, rather than an assumption about that development version, supplied
the version-specific evidence.

| Spellings | Context established by the source |
| --- | --- |
| `.SD`, `.BY`, `.N`, `.I`, `.GRP`, `.NGRP` | The documented read-only evaluation symbols. `[.data.table` locked the bindings of `.SD`, `.N`, `.I`, `.GRP`, and `.NGRP`; `dogroups()` also locked `.BY` and `.I` in C. These bindings could collide when data.table installed same-named input columns in the evaluation environment. [Distributed help][symbols], [`[.data.table`][dt-r], [`dogroups()`][groups] |
| `.SDall`, `.xSD`, `.iSD` | Additional internal bindings locked by `[.data.table`. They were the three spellings addressed by PR #961. [`[.data.table`][dt-r] |
| `.EACHI`, `.NATURAL` | Syntactic markers recognized in `by`/`keyby` and `on`, respectively. Their documented meaning did not make every same-named column invalid. [Distributed help][symbols], [`[.data.table`][dt-r] |
| `.ROW` | A special left-hand side of `:=`. The parser required the right-hand side to be `NULL` and an `i` argument, and rejected grouping. This restriction also matched a character left-hand side equal to `.ROW`. [`[.data.table`][dt-r] |
| `..value`, `..FUN1`, `..FUN2` | The `..` prefix could select a value from the calling environment in `j`. In a different branch, `[.data.table` generated and locked `..FUNn` bindings while optimizing anonymous functions supplied to `lapply(.SD, ...)`. These were context-dependent mechanisms, rather than one global prohibition on `..` column names. [`[.data.table`][dt-r] |
| `i.value`, `x.value` | Join-expression prefixes selecting the `i` and `x` input columns. Their use as join qualifiers was documented separately from ordinary column names. [data.table reference, `j` argument][reference], [`[.data.table`][dt-r] |
| `_nqgrp_` | `bmerge()` rejected this column in the non-equi branch where `nqmaxgrp > 1L`, before adding its own temporary column. [`bmerge()`][bmerge] |
| `.ll.tech.split`, `.nm.tech.split`, `.ll.tech.split.names` | `split.data.table()` rejected `.ll.tech.split` anywhere in its input and `.nm.tech.split` among `by` columns. It generated `.ll.tech.split.names` for its intermediate result; that third name had no matching explicit refusal. [`split.data.table()`][dt-r] |
| `.Last.updated` | A namespace integer holding the number of rows affected by the last update. C retained a pointer to that namespace integer; it was not an extra locked column binding created in the grouping environment. [Reference][last-updated], [`initLastUpdated()`][init], [`assign()`][assign] |
| `.internal.selfref`, `.data.table.locked` | Internal attribute names used for self-reference and locking. They were not declarations that identically spelled columns were reserved. [`R_init_data_table()`][init], [`setselfref()`][assign], [`lock()`][utils] |
| `.SDcols` and the other `[.data.table` formals | Arguments of the bracket method. The spelling alone did not prevent a column from having the same name. [`[.data.table`][dt-r] |
| `key`, `keep.rownames`, `check.names`, `stringsAsFactors`, `.named` | Formal-argument collisions were possible when constructing a table by passing named columns directly to `data.table()` or `as.data.table.list()`. In contrast, passing one named list kept the column names inside that list. [`data.table()`][dt-r], [`as.data.table.list()`][as-dt] |
| `sorted`, `unique` | Formal arguments of `CJ()`. data.table's own `.prepareFastSubset()` removed the prospective column names before calling `CJ()` and restored them afterwards to avoid interpreting these names as arguments. [`CJ()`][setkey], [`.prepareFastSubset()`][dt-r] |
| `print`, `-.POSIXt`, `Cfastmean`, `order` | Function bindings inserted into evaluation environments by `[.data.table`, without the column-binding locks listed above. These were included as additional collision probes. [`[.data.table`][dt-r] |

The namespace search found no additional explicit reserved-column diagnostic
beyond the non-equi-join and split cases listed above. It found no `.iGRP` or
`.xGRP` evaluation binding; those spellings remained probes rather than claims
about data.table internals.

## Measurement scope

[`check-nesting-names.R`](check-nesting-names.R) exercised the public
`nest_with_margins()` and `nest_by_with_margins()` methods on an immutable
`dtplyr::lazy_dt()` and a local tibble control. Each candidate appeared as a
payload column, fixed `.by` column, Grouping dimension, `.id`, and `.key`.
Each role was combined with zero, one, and three input rows, both `.keep`
values, and both verbs: 60 calls per candidate. Three-row inputs included a
missing grouping value and duplicate rows. Existing columns resembling the
allocated safe names tested collision avoidance.

The comparison checked output names, typed outer-column values, nested-cell
names and complete row multisets, source preservation, the lazy result class
of `nest_with_margins()`, and the rowwise result class of
`nest_by_with_margins()`. A separate source `collect()` control recorded whether
the name was accepted before any Margin operation ran. Errors from the local
control were retained separately from dtplyr-only failures.

The audited path already built nested cells through
`as.data.table(list(...))` in `nest_cell_expr()`. It did not expose the multiple
output or `.names_sep` variants of `tidyr::nest()`. For comparison, the
[dtplyr 1.3.3 `nest.dtplyr_step()` source][dtplyr-nest] generated `.SD` for one
output without `.names_sep` and `data.table(...)` in its other branch; the
latter branch was not the implementation exercised here.

## Reproduction commands

The commands were run from the repository root:

```sh
Rscript investigation/2026-10-28-dtplyr-special-name-followup/check-nesting-names.R
Rscript investigation/2026-10-28-dtplyr-special-name-followup/check-nesting-names.R print '-.POSIXt' Cfastmean order
Rscript investigation/2026-10-28-dtplyr-special-name-followup/reproduce-row.R
```

The first two commands recorded comparison outcomes even for failures; their
successful exit meant the audit completed, rather than that all candidate
names worked. The third command asserted the expected failure and successful
controls.

## Measured results

Both completed matrix runs exited successfully. The main run covered 60
spellings and 3,600 calls; the supplemental run covered `print`, `-.POSIXt`,
`Cfastmean`, and `order` for another 240 calls. Their results were retained in
[`nesting-results.csv`](nesting-results.csv) and
[`supplemental-results.csv`](supplemental-results.csv), with console summaries
in [`nesting-console.txt`](nesting-console.txt) and
[`supplemental-console.txt`](supplemental-console.txt).

| Result | Calls | Interpretation |
| --- | ---: | --- |
| Matched the local control | 3,696 | No unexpected warning or source mutation was observed. |
| `.ROW` failed only on the dtplyr path | 24 | Both verbs failed for the Grouping-dimension and `.id` roles across all three sizes and both `.keep` values. |
| `..1` or `...` failed only on the dtplyr path | 24 | The `.id` cases passed their local control but failed through tibble name validation during dtplyr collection. |
| `..1` or `...` local-control errors | 96 | The local path, including comparison normalization, raised an error; this result did not isolate the stage or establish a Margin input refusal. |

Excluding the two R/tibble dot-dot-name controls, the audit covered 62
data.table-related spellings and 3,720 calls. Of these, 3,696 matched their
local controls and the remaining 24 reproduced the `.ROW` failure. The locked
symbols, constructor/formal names, join prefixes, non-equi and split internal
names, and evaluation-function probes produced no additional failing spelling
in those nesting roles. The dot-dot list-column cases overlapped the subject
of [issue #950](https://github.com/sayuks/marginplyr/issues/950); the audit did
not establish that every dot-dot input or output case fell within that issue.

### Additional `.ROW` defect

[`reproduce-row.R`](reproduce-row.R) and its successful
[`row-console.txt`](row-console.txt) run isolated the newly observed failure:

```r
input <- tibble::tibble(.ROW = "a")
source <- dtplyr::lazy_dt(input, immutable = TRUE)
result <- nest_with_margins(source, .grouping = rollup(.ROW))
dplyr::collect(result)
# Error: .ROW can only be used with := NULL to delete rows
```

The ordinary source collected successfully, as did the local nesting control,
the fixed `.by` control, and a control renaming `.ROW` before nesting. The
payload, fixed-key, and list-column-name roles passed the matrix. Both nesting
verbs failed when `.ROW` was a Grouping dimension or `.id`. Lazy
`nest_with_margins()` construction returned a step and collection exposed the
failure. An ordinary `dplyr::mutate(source, .ROW = "Total")` reproduced the
same diagnostic, consistent with data.table's special `:=` left-hand side.
The source remained unchanged. This follow-up recorded the reproduction and
did not change package behavior.

## Limits

This was a dated audit of the two Margin nesting verbs at the recorded
snapshot. It did not claim compatibility for arbitrary upstream data.table
expressions, joins, `split()`, other Margin verbs, or future package versions.
In particular, passing a name through a nesting operation did not establish
that the same name would be accepted in the separate reserved context listed
in the inventory.

[dt-r]: https://github.com/cran/data.table/blob/1.18.6.1/R/data.table.R
[symbols]: https://github.com/cran/data.table/blob/1.18.6.1/man/special-symbols.Rd
[groups]: https://github.com/cran/data.table/blob/1.18.6.1/src/dogroups.c
[reference]: https://rdatatable.gitlab.io/data.table/reference/data.table.html
[bmerge]: https://github.com/cran/data.table/blob/1.18.6.1/R/bmerge.R
[last-updated]: https://rdatatable.gitlab.io/data.table/reference/last.updated.html
[init]: https://github.com/cran/data.table/blob/1.18.6.1/src/init.c
[assign]: https://github.com/cran/data.table/blob/1.18.6.1/src/assign.c
[utils]: https://github.com/cran/data.table/blob/1.18.6.1/src/utils.c
[as-dt]: https://github.com/cran/data.table/blob/1.18.6.1/R/as.data.table.R
[setkey]: https://github.com/cran/data.table/blob/1.18.6.1/R/setkey.R
[dtplyr-nest]: https://github.com/tidyverse/dtplyr/blob/v1.3.3/R/step-nest.R
