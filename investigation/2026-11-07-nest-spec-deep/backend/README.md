# Lazy Grouping metadata and inspection boundary investigation

Investigated: 2026-11-07
Tested source: `0b5143133c023ccb4efb6fe27f1ef551e9f49186` (main at lane start).

This lane investigated typed Grouping-plan inspection, preparation without
source-row work, source ownership and unsupported Nest admission. It added no
package implementation or permanent test. No new defect was confirmed by the
finite comparisons below, so this lane supplied no new repair ticket.

The distinguishing hypotheses were: an admitted derived dtplyr proxy might
select a different grain from its native empty result; metadata preparation
might execute a removed row predicate or mutate its source; direct Arrow
reader normalization might consume a batch; and the earlier registered
bracket/converter failures might survive the merged repair. Respectively,
the name differential, source-element/serialization assertions, reader
consumption controls and desired-contract registry replays refuted those
hypotheses within the executed fixtures. Arbitrary dispatch remained outside
that conclusion.

## dtplyr metadata differential

The [new differential](metadata-differential.R) exercised 16 source-column
fixtures, source sizes zero/one/four, 17 graph shapes, ten metadata predicates
and both Grouping-dimension and fixed-key selection positions. Graphs included
root, row filtering, selection/deletion/reordering, renaming, grouping, caches,
safe arithmetic, new scalar columns/aliases, unsafe scalar overwrites,
ordering, renamed caches, joins and unions.

The source fixtures covered ordinary integer/double/logical/character/raw/
complex values, factors including unused and declared-NA levels, ordered
factors, Date, zoned POSIXct, difftime, an unclassed attribute, element names,
a matrix supplied through data.table assignment, a list and a packed data
frame. The matrix fixture followed data.table's native assignment semantics;
its assignment warned that a two-column matrix RHS would be treated as one
vector. It did not establish a guarantee of preserved matrix-column shape.

The oracle explicitly collected each input, then selected columns from its
empty native result. It never used the package's compiled Grouping plan to
derive the expected names. Value-dependent predicates were excluded: the
documented lazy-selection contract uses metadata rather than source values.
For every executed inspection the harness asserted that the row predicate
processed zero source elements and that the original source's serialized
bytes were unchanged.

The [completed table](metadata-differential.csv) recorded:

| Classification | Count |
| --- | ---: |
| Exact selected-name match | 13,624 |
| Established unsafe-metadata Package refusal | 2,000 |
| Correct grouped-key/Grouping-dimension overlap refusal | 96 |
| Native raw/complex union refusal before inspection | 6 |

There were 15,720 inspection calls and six unsupported native graph
constructions/executions. No admitted metadata mismatch or unexpected
inspection error was observed. This comparison asserted selected names,
source ownership and source-row nonexecution; it did not assert Nest cell
membership, physical result order or a universal backend class policy.

The first harness version classified the 96 grouped-key overlaps as generic
errors although they were the documented structural refusal. Its
[initial output](metadata-initial-oracle.log) was retained. After that oracle
classification was corrected, the complete run was repeated with a strict
assertion rejecting every mismatch or unexpected error and exited zero.

## Arrow schema and one-shot readers

The [new Arrow comparison](arrow-schema-readers.R) exercised 15 physical/R
type fixtures: boolean, int8, int64, float32, decimal128, utf8, binary, date32,
zoned nanosecond timestamp, nanosecond duration, factor, ordered factor,
variable/fixed-size list and struct. Each fixture was inspected as a Table,
RecordBatch, query, direct reader and query over a reader, across eight type
predicates and dimension/fixed-key positions.

All [1,200 comparisons](arrow-schema-readers.csv) agreed with the empty native
R representation. After inspection, each of 480 newly created readers still
yielded both original source rows, retaining 960 source rows in total. The
[run](arrow-schema-readers.log) exited zero. These checks established metadata
selection and reader preservation, not SQL/Arrow Nest support.

Two earlier setup attempts used unsupported Table-to-RecordBatch spellings.
Their [constructor error](arrow-initial-setup.log) and
[missing-method error](arrow-initial-method.log) arose in the harness before
the relevant package path and were not package findings. The retained run
used Arrow's installed `as_record_batch.Table` method.

## Prior boundary controls and duplicate context

The November 7 [remote frontier harness][remote] was rerun on this lane's SHA.
Its [fresh output](remote-frontier-controls.log) recorded 36 SQLite, 36 DuckDB
and 24 Arrow calls. SQL Nest admission did not force the supplied Grouping
specification; invalid local arguments sent no statement. SQLite sent zero
statements, DuckDB sent six zero-row metadata statements, and name-only
inspection sent none. All 18 Arrow readers in the join/union graphs retained
their 36 source rows.

The earlier registered-root bracket and registered-converter desired-contract
reproductions were rerun separately with `--assert`. Their
[bracket](root-bracket-replay.log) and [converter](converter-replay.log) outputs
recorded safe Package refusals before callback dispatch and terminal exit zero.
The former findings were not counted as new defects.

Recent November 6/7 backend notes supplied the root-subclass, registered head,
bracket, converter and source-sharing boundaries. All-state CLI searches used
`"inspection" "Arrow"`, `"metadata" "setnames"` and
`"metadata" "inspection"`, alongside a 1,000-limit all-state issue-title
inventory filtered for dtplyr/metadata/binding/proxy/inspection. Relevant PR
searches used the latter two metadata queries. Full bodies/comments of #1281,
#892 and #1422 were read. They concerned effective join lookup, ordinary
metadata-preserving ordering and generated deferred-copy namespace lookup.
The earlier observation that a caller `setnames` function did not replace
dtplyr's own generated wrapper was treated as historical evidence, not a new
execution or finding. No novel candidate in this lane required publication.

## Replay and limits

Run the new scripts from a source root of the tested SHA; use
`MARGINPLYR_HUNT_SOURCE` for a separate archive and `MARGINPLYR_HUNT_OUTPUT`
for a separate evidence directory:

```sh
Rscript investigation/2026-11-07-nest-spec-deep/backend/metadata-differential.R
Rscript investigation/2026-11-07-nest-spec-deep/backend/arrow-schema-readers.R
```

All executions set testthat parallelism false and were collected to terminal
completion. No suite, build, rendering or Review-ready invocation ran in this
lane. The [environment](environment.txt) recorded R 4.6.1 on arm64 macOS,
dplyr 1.2.1, dtplyr 1.3.3, data.table 1.18.6.1, tidyselect 1.2.1, rlang 1.3.0
and vctrs 0.7.3. Remote output also recorded DBI 1.3.0, dbplyr 2.6.0,
RSQLite 3.53.3, DuckDB 1.5.5 and Arrow 25.0.1.

Retained terminal sessions were `43773` (initial differential, exit zero),
`57324` (corrected differential, exit zero), `87688` (Arrow comparison, exit
zero), `23128` (remote replay, exit zero) and `41842` (sequential bracket and
converter replays, exit zero). Both Arrow setup attempts returned immediate
exit one. No execution handle or owned child remained live.

These finite fixtures did not establish arbitrary extension dispatch,
unbounded query graphs, hosted services, other dependency/OS versions,
interruption behavior or resource limits. Safe refusals were not evidence
that a refused transformation could never support faithful metadata.

[remote]: ../../2026-11-07-nest-spec-bughunt/lazy/remote-frontier-controls.R
