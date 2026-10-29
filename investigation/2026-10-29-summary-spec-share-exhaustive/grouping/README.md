# Grouping specification and inspection: bounded exhaustive probes

Investigated: 2026-10-29
Repository snapshot: `4c63bb917ed7ed31ff6aadf060a5eae7a32cd004`

This lane investigated public Grouping specification construction, expansion,
inspection, captured environments, and typed-selection metadata. No package
implementation or existing package test was changed. The retained harnesses
loaded the checkout with `pkgload::load_all()` and were run from its root.

No new confirmed defect was established by these four completed loops. This is
an observation about their stated input spaces, not a claim that Grouping
specifications or their metadata adapters had no remaining defects. The known
concurrent tickets listed under **Limits and existing tickets** describe
important boundaries these loops did not exercise.

## Prior evidence and contract sources

The earlier dated notes
[`summary-spec-share-bughunt-2026-10-29.md`](../../summary-spec-share-bughunt-2026-10-29.md)
and
[`summary-grouping-share-followup-bughunt-2026-10-29.md`](../../summary-grouping-share-followup-bughunt-2026-10-29.md)
had already reported broad random trees, unusual names, selection forms,
identifier widths, and Mutable-step inspection. This lane added exhaustive
small composite-dimension sequences and pair families, together with a matrix
crossing nested binding reads, redundant parentheses, and injected-quosure
environments. It did not count the earlier notes' runs as new measurements.

The sources read included `CLAUDE.md`, `AGENTS.md`, `CONTEXT.md`,
`investigation/README.md`, and the relevant constructor/inspection/metadata
implementation and tests. ADRs 0005, 0008, 0009, 0013, 0015, 0018, 0020, and
0026 supplied the early-refusal, one-reading, grouping-identity, inspection,
condition, ordering, query, and ambiguity boundaries. The full bodies and
comments of #975, #995, #1017, and #1024 were read to distinguish the repaired
qualified/pronoun, typed-dispatch, repeated-update, and cached-graph defects.

## Completed loops

| Harness | Executed input space | Result |
| --- | --- | --- |
| `algebra-probes.R` | 3,534 plans; 2,463 produced duplicate occurrences | 20,133 assertions passed |
| `capture-probes.R` | 600 nested-name cases, 240 ordinary retained-value cases, 10 delayed-promise cases | 1,690 assertions passed |
| `arrow-types.R` | 22 physical fields; 320 typed-selection comparisons across five input shapes, two positions, and two int64 conversion options | 366 assertions passed |
| `metadata-probes.R` | 2,016 dtplyr inspections: 1,728 successful typed comparisons and 288 expected metadata refusals | 6,049 assertions passed |

The four scripts produced 28,238 assertions in total. Each script exited 0 on
the investigated snapshot and exits nonzero when one of its stated assertions
fails. These are investigation harnesses, not package regression tests.

### Independent ordered-family oracle

The algebra loop represented a grouping-set family independently as an ordered
list of ordinary character vectors. It compared exact set membership, global
dimension order, named Grouping bits, Grouping identifiers, and one-based
Grouping set identifiers through public list-format inspection. It then
checked first-occurrence duplicate dropping and the duplicate-error refusal.

Its cases consisted of:

- All dimension sequences of lengths one through three over ten units on
  three column names: three ordinary scalar selections and seven nonempty
  composite grouping sets. Both rollup and cube were checked: 2,220 plans.
- All pairs of the sixteen subsets of four column names, combined by
  grouping-set union: 256 plans.
- All pairs of twenty-three leaf families, combined by both grouping-set
  union and Cartesian product: 1,058 plans. These included empty sets, the
  identity product, overlapping cubes, and overlapping rollups.

The expected cube order was descending subset size and then combination order.
The oracle normalized each set to the family's first-encounter dimension order,
which is distinct from grouping-set occurrence order. It did not assume that
commuting two product arguments preserves plan order.

### Captured bindings and quosure environments

The capture loop crossed all five parent kinds with all five nested kinds,
zero through three redundant-parenthesis layers, the presence or absence of a
colliding input column, and zero through two injected-quosure layers. An inner
quosure's environment held an active binding named `s`; outer environments held
a conflicting value. The expected reading followed the innermost captured
environment and the parent kind's admitted nested kinds.

The loop checked both the resulting acceptance/refusal and the binding-read
count. In a grouping set's colliding selection-only position no binding was
read; the other recognized or ambiguity-checked positions read once. Ordinary
character, integer, predicate, and language bindings alternating between two
answers retained the first answer per occurrence. Delayed-promise controls
checked that irrelevant colliding selection-only positions did not force the
promise. None of these probes changed the caller's namespace operators or
arithmetic operator bindings.

### Arrow physical metadata and reusable readers

The Arrow loop included integer widths, unsigned integers, floating point,
strings, decimals, dates, timestamps, durations, time-of-day, binary vectors,
variable/fixed-size lists, structs, string dictionaries, and null fields.
Sixteen predicates exercised ordinary type tests and class tests. The five
input shapes were Table, RecordBatch, InMemoryDataset, a selection query, and
RecordBatchReader. Fixed `.by` selection and a grouping-set selection were
compared with direct typed tidyselect over the native conversion of the same
fixture. The small int64 values were representable as R integers; both values
of `arrow.int64_downcast` were exercised.

After the inspection loop, each reader's complete two-row fixture was still
available for ordinary conversion. This established that these inspection
calls had not consumed its batches. Physical float16, large int64 values,
non-string dictionaries, and file-backed Dataset partitioning were outside
this loop.

### dtplyr typed metadata and source isolation

The dtplyr loop crossed twenty-one graph forms with zero, one, and five source
rows; immutable and Mutable roots; eight predicates; and fixed-key versus
Grouping specification selection. Graph forms covered projection, renaming,
ordering, filtering, numeric arithmetic, sequential aliases, new constants,
cached derivations, differently typed cached join branches, and cached union
and semi-join reuse. Explicit collection on a separately rooted immutable
control supplied the ordinary typed-selection oracle.

For each public inspection, the script checked the selected names or the
expected actionable refusal, counted row-filter/type-callback invocations,
and compared every caller source table with its initial copy. The row callbacks
were not executed during inspection, and caller tables stayed unchanged. No
inspection warning was observed.

Three unsafe update forms were expected refusals: an existing-column scalar
overwrite, a value-dependent callback overwrite, and a repeated update ending
in that callback. They produced 288 refusals without evaluating the callback
for metadata. Twelve warnings arose only while the ordinary reference
collections attempted their scalar character replacements of integer columns;
the log tagged every such warning with its `oracle` phase. They were
upstream coercion controls, not Margin-operation warnings.

An initial scanner mistakenly included `flag = TRUE` among new constants even
though `flag` was already a source column. Its 96 metadata refusals were the
same documented existing-column scalar boundary. The retained loop used a new
`added_flag` column and retained the three explicit unsafe controls above. No
source-column read or type-fidelity defect was inferred from that refusal.

## Limits and existing tickets

The full bodies and comments of these concurrently available tickets were also
read. No duplicate ticket was proposed:

- [#1038](https://github.com/sayuks/marginplyr/issues/1038) covered retaining
  constructor-shaped selections when a caller-defined namespace operator
  resolved a qualified head. The capture matrix used ordinary namespace
  resolution and did not exercise that head-binding boundary.
- [#1039](https://github.com/sayuks/marginplyr/issues/1039) covered typed
  selection through caller-defined namespace operators. Familiar qualified
  helper spellings in these loops used their ordinary implementations.
- [#1043](https://github.com/sayuks/marginplyr/issues/1043) covered dtplyr
  arithmetic operators supplied by active bindings. This metadata loop used
  stable base arithmetic; its active bindings belonged only to nested names.
- [#1040](https://github.com/sayuks/marginplyr/issues/1040) covered omitted
  typed-missing factor arrays during local expansion. Factor-array label
  validation and expansion were outside the inspection/algebra loop.

No live SQL execution, alternative dependency versions, unbounded specification
nesting, interruption stress, arbitrary class extension, or general
value-dependent selection contract was established. The finite algebra spaces
were exhaustive only over the units and depths stated above.

## Replay

Run from the repository root:

```sh
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/grouping/algebra-probes.R
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/grouping/capture-probes.R
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/grouping/arrow-types.R
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/grouping/metadata-probes.R
```

Adjacent `.log` files retain stdout/stderr from a fresh replay of the retained
scripts. `replay-status.json` records the terminal exit outcomes. Jarl was run
on this artifact directory after the capture-loop braces were made explicit.

The environment was R 4.6.1 (2026-06-24) on aarch64 macOS; dplyr 1.2.1,
dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1, tidyselect 1.2.1,
rlang 1.3.0, and pkgload 1.5.3.
