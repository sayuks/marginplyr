# Nesting backend boundaries

Investigated: 2026-11-04
Baseline: `7a05a1e42d53af62c057a30ad1892ffb35d4eef8` (fetched `origin/main`)

## A dtplyr predicate selected different rows for detail and total

[The replay](dtplyr-source-reuse.R) built an immutable two-row dtplyr source
with a filter that selected source row 1 on its first invocation and row 2 on
its second. Ordinary collection ran the filter once and returned row 1.
Nesting that materialized row with `rollup(g)` produced the detail cell
`a` and the Grand total cell `Total`, both containing row 1. Passing the
uncollected filter to either nesting verb produced the same visible keys and
Grouping set identifiers, but the detail contained row 1 and the total
contained row 2. The recorded callback input sizes were `2, 2`. Constructing
and displaying the deferred result invoked the filter zero times. The
row-wise verb invoked it during its documented collection. [Baseline
output and generated queries](dtplyr-source-reuse.txt).

The nesting reference's *Relationship to tidyr and dplyr* promises the
original rows in each cell and original grouping values under `.keep`.
The two cells above did not describe one evaluated input. The generated
query showed the filter expression repeated in each `funion()` operand.
Ordinary collection and a caller-cached source, built with
`dplyr::compute(name = "cached_source")`, ran the same callback once. The
cached Nest query used the cache reference in both operands. These controls
separated branch replay from mutation of the source, an effectful metadata
probe, or an operation performed while constructing the result. ADR 0020
supplied the construction and collection boundary. The collision read
remained at its lazy default throughout.

[The matrix](dtplyr-source-reuse-matrix.R) compared three public consumers,
three Grouping plans, three predicates, an uncached graph and two
caller-cached graphs, both Nest keep choices, and two sorting choices. It
compared Grouping set identifiers and row IDs against the equivalent
operation on a one-time native collection. Of 270 comparisons, 40 differed
and 230 agreed. The wrong comparisons comprised 16 for each nesting verb
and eight for expansion. All wrong cases had an uncached alternating or
first-only predicate and a rollup or cube. Every uncached rollup ran the
predicate twice and every uncached cube ran it four times, including
stable-predicate controls whose rows agreed. All single-set and cached
cases ran it once. Every deferred case ran it zero times during
construction and query display. The caller's plain source still collected
to its original rows. [Per-call data](dtplyr-source-reuse-matrix.csv) and
[totals](dtplyr-source-reuse-matrix.txt).

[Issue #1271](https://github.com/sayuks/marginplyr/issues/1271) was opened
during the investigation for expansion's use of the same dtplyr branch
composition. Its witness dropped a row copy. The two Nest witnesses add a
different visible consequence and coverage of the two return shapes. This
investigation linked that issue as the shared repair unit. The earlier
[SQL-specific issue #1243](https://github.com/sayuks/marginplyr/issues/1243)
and [PR #1262](https://github.com/sayuks/marginplyr/pull/1262) shared
evaluated rows on supported SQL engines. [#1235](https://github.com/sayuks/marginplyr/issues/1235)
concerned reexecuting a single-set source when creating a typed empty
row-wise cell. [#1003](https://github.com/sayuks/marginplyr/issues/1003)
and [#1241](https://github.com/sayuks/marginplyr/issues/1241) addressed
isolation and lookup of cached branch copies.

## A rollup failed for an sf input with two geometry columns

[The replay](sf-two-geometry.R) made a one-row `sf` input with an ordinary
character key and two point-geometry columns, with the first geometry
active. Both nesting verbs failed on `rollup(g)` with
`'names' attribute [4] must be the same length as the vector [3]`. They
produced the detail and Grand total cells on the same columns in a tibble
and on the original `sf` input with `.id = "..1"`. Both geometry columns,
their values and their CRS matched the source in each cell. Native
`tidyr::nest()` and `dplyr::nest_by()`, an included-only Grouping
specification, and an `sf` source with one geometry column also succeeded.
The serialized original source stayed identical. [Baseline
output](sf-two-geometry.txt).

A native union of two branches with an added scalar source-identity
column and set identifier succeeded and preserved both geometries. An
explicit full-column `dplyr::rename()` of one augmented branch reproduced
the same error from `sf`'s `rename.sf()`, while a single grouping set
needed no branch union. In the failing Margin calls, the
`bind_margin_branches()` name-protection step performed the full rename.
The supported dot-dot identifier reached that helper's existing column
frame path, which bypassed `rename.sf()` and preserved both geometry
payloads. Those controls separated the internal rename from unsupported
geometry payloads and from a failure of native union. ADR 0016 supplied
the class and attribute boundary. The finding concerned the two documented
data-frame cells and did not require preservation of an outer `sf` class.

[The matrix](sf-two-geometry-matrix.R) completed 648 comparisons against
the same columns in a tibble. It crossed both verbs, one or two geometry
columns and their placement, zero or three rows, three plans, three
identifier choices, both keep choices, and every Margin order. All 192
two-geometry, two-set cases with an absent or ordinary identifier raised
the `names` length error. All other 456 agreed on outer keys, order where
specified, identifiers, geometry values, and payload rows. The caller's
source remained unchanged in every case. [Per-call
data](sf-two-geometry-matrix.csv) and [totals](sf-two-geometry-matrix.txt).

The [all-state Issue and PR searches](duplicate-search.txt) found related
[#949](https://github.com/sayuks/marginplyr/issues/949) and
[#951](https://github.com/sayuks/marginplyr/issues/951), which concerned
stale active geometry metadata and an empty single-geometry rollup.
Their repairs [#962](https://github.com/sayuks/marginplyr/pull/962) and
[#966](https://github.com/sayuks/marginplyr/pull/966) addressed those
boundaries. [#993](https://github.com/sayuks/marginplyr/issues/993) and
[#1000](https://github.com/sayuks/marginplyr/issues/1000) addressed
expansion column order and summary key restoration, respectively. No
earlier issue in those searches reproduced the full-branch rename on two
geometry columns.

## Remote refusal and inspection controls

The [previous remote boundary probe](../../2026-11-04-nesting-grouping-followup/remote-boundaries.R)
was rerun on the pinned source. Six SQL graphs per engine covered both
nesting refusals, invalid inspection, and name-only and typed inspection.
SQLite completed 36 boundary calls and sent no statements. DuckDB
completed 36 and sent twelve statements, all `LIMIT 0`. Neither nesting
verb forced its supplied specification or sent a statement. Twenty-four
Arrow calls left all 18 one-shot readers available with their 36 original
rows. These were refusal and inspection measurements.
[Replay output](remote-boundary-replay.txt).

## Replay

The November 3 and November 4 nesting investigations supplied prior
helper-binding, cache isolation, factor payload, and remote admission
coverage. The measurements above came from the pinned source loaded
with `pkgload::load_all()` from a separate `git archive` copy. They ran
with testthat parallelism disabled on R 4.6.1 and macOS Tahoe 26.6.2 arm64.
The observation logs give the dependency versions. Both minimal
`--assert` runs returned 1 after the controls passed and their
observation runs returned 0. Both matrix runs and the remote replay
returned 0. `git ls-remote origin refs/heads/main` still reported the
pinned revision at duplicate assessment.

Set `MARGINPLYR_HUNT_SOURCE` to an archive of the baseline, then run from
the repository holding this note:

```sh
probe=investigation/2026-11-04-nesting-grouping-boundaries/backend
Rscript "$probe/dtplyr-source-reuse.R" --assert
Rscript "$probe/sf-two-geometry.R" --assert
Rscript "$probe/dtplyr-source-reuse-matrix.R" /tmp/dtplyr-source-reuse.csv
Rscript "$probe/sf-two-geometry-matrix.R" /tmp/sf-two-geometry.csv
Rscript investigation/2026-11-04-nesting-grouping-followup/remote-boundaries.R
```

The finite checks did not cover hosted databases, arbitrary custom R
classes or methods, explicit observed label checks on a volatile source,
other R and dependency versions, or resource exhaustion.
