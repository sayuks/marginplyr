# Lazy expansion and Grouping specification boundary evidence

Investigated: 2026-11-02
Baseline: `978d7bef4247bf1348af5b90bfab47924db25500`

The track used a disposable `git archive` source copy of the baseline at
`/private/tmp/marginplyr-lazy-source.Xwb7ig/source`; every package load named
that copy through `MARGINPLYR_HUNT_SOURCE`. R executions were sequential within
this track, each retained until its terminal outcome. No package source, test,
generated file, or build rule was edited. No full suite or build ran.

The October 30 exhaustive and October 31 greedy expansion investigations were
read as prior coverage. The new measurements below were independent of those
runs. No new package defect was confirmed by this track.

## Identifier roles and source integrity

`name-role-matrix.R` compared actual public expansion with an independent local
branch oracle. The oracle copied each source row into included and omitted
occurrences, wrote the label and identifier itself, placed public columns in
fixed/dimension/identifier/payload order, and separately constructed the
requested key order. It did not call local `expand_with_margins()`.

The 4,800 comparisons covered 25 identifiers in dimension, fixed, payload and
identifier roles; zero/three rows; Arrow/dtplyr; root/identity-filtered sources;
NULL/text labels; and all three sorts. Names included `.SDcols`,
`.Last.updated`, `.env`, `.data`, `.`, `..`, `..g`, join-like names, function-like
names, Boolean/NA/NaN/Inf words, backticks, embedded tab/newline, Unicode and a
private-prefix collision. All 2,400 Arrow and 2,400 dtplyr comparisons passed.
The caller-owned data.table source was identical to its pre-call copy after
every expansion; Arrow source fixtures were immutable Tables.

The result established exact scalar values/types, public names/order, row bags,
and requested key sequences for these fixtures. Payload order within equal keys
was not asserted. This was a finite role matrix, not an enumeration of arbitrary
identifiers or classes.

## Nested Arrow types and contributing source graphs

`nested-arrow-graph.R` compared expansion against ordinary Arrow
`mutate()`/`union_all()` with an Arrow-native typed-null scalar. It checked
canonical occurrence/source-row values and the physical Arrow type of `g`.
Eight families covered nested lists, list-of-struct, struct-of-large-list,
list-of-map, a vctrs extension, struct-of-extension, chunked dictionaries with
opposite chunk dictionaries, and chunked structs. Each used zero/three rows,
Table/RecordBatch/IPC Dataset and table/Dataset join/union graphs where those
sources could execute, dimension/payload/fixed roles, and none/last sorts.

The corrected 672 observations were:

| Outcome | Count | Interpretation |
| --- | ---: | --- |
| Matching values and physical key type | 445 | Measured package/ordinary agreement |
| Shared sort failure | 64 | Ordinary Arrow also refused the type |
| Ordinary source failure | 144 | Joins rejected nested non-key fields before expansion |
| Ordinary-control-only sort failure | 14 | Native dictionary sorting refused; package decoding admitted it |
| Factor-level order only | 5 | Values/schema agreed; ordinary dictionary unification chose another level order |

The initial RecordBatch fixture called a nonexistent Table method. Its 96
observations were excluded and rerun using exported `arrow::as_record_batch()`;
`nested-arrow-batch-results.csv` replaced that entire population in the final
CSV. The initial CSV and log remain explicitly named as initial evidence.
No initial fixture failure was counted as an ordinary backend limitation.
The final script accepts `MARGINPLYR_HUNT_SHAPES=batch` to repeat that selected
population without repeating the complete sweep.

The five apparent dictionary differences suggested three explanations: a
partition boundary changed the decoded values, ordinary Arrow dictionary
unification chose different level metadata, or the row alignment compared
unrelated duplicate copies. `dictionary-controls.R` aligned by occurrence and
source-row identifiers, printed both level orders, and compared decoded values
for all six graph/role combinations. All six decoded comparisons passed.
Repeated ordinary source graphs chose either level order. The difference was
therefore excluded under ADR 0016's attribute boundary and ADR 0020's Arrow
factor amendment. No value-corruption claim or new factor-restoration promise
was inferred.

The sorted ordinary oracle directly sorted dictionaries and therefore could
not verify the package's intentional dictionary-decoding sort path. Physical
type agreement was checked only after both paths completed. The probe did not
compare every nested child field's metadata, extension serialization policy,
or complete requested key order on these structured values.

## dtplyr join/set metadata and row shape

`dtplyr-join-set-metadata.R` compared typed fixed-column selection from
`inspect_grouping()` with type predicates over the ordinarily collected input.
It covered logical/integer/double/character keys; both-empty, left-empty,
right-empty, matched, unmatched and partial/NA-match relations; inner/left/right/
full/semi/anti joins and union/union_all/intersect/setdiff; plain and explicitly
cached inputs; and logical/integer/double/character/numeric predicates.

All 2,400 admitted typed inspections matched the independently collected
schema. Both caller-owned data.table roots remained identical after each query.
Another 240 cross-join observations were excluded: dtplyr had no ordinary
`cross_join()` method, so they never reached package inspection. No metadata
refusal, package error or type-selection difference was observed among the
admitted cases. These comparisons checked metadata fidelity and source
integrity, not every expanded value over each graph or arbitrary callbacks.

## Replay and environment

Run from the investigation worktree root, or supply another baseline source
copy through `MARGINPLYR_HUNT_SOURCE`:

```sh
Rscript investigation/2026-11-02-expand-grouping-greedy/lazy/name-role-matrix.R
Rscript investigation/2026-11-02-expand-grouping-greedy/lazy/nested-arrow-graph.R
Rscript investigation/2026-11-02-expand-grouping-greedy/lazy/dictionary-controls.R
Rscript investigation/2026-11-02-expand-grouping-greedy/lazy/dtplyr-join-set-metadata.R
```

All four completed executions and the selected RecordBatch replay exited zero.
Their classified CSVs carry the scope of their evidence; runner success alone
does not turn shared/backend or excluded observations into passing assertions.
`environment.txt` records R 4.6.1 on arm64 macOS, Arrow 25.0.1, dtplyr 1.3.3,
data.table 1.18.6.1, dplyr 1.2.1 and the remaining package versions.
The Arrow log included three sandbox CPU-cache metadata diagnostics while its
executions completed; none was a marginplyr condition. No live network service,
other OS, dependency-version matrix, arbitrary source graph or reader-
consumption instrumentation was exercised. Successful comparisons establish
only their recorded inputs. No new Issue was proposed from this track, so no
nonduplicate-publication claim was made.
