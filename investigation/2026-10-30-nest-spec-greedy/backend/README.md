# Backend nesting and metadata boundary investigation

Investigated: 2026-10-30
Baseline: `28c449aa03fd5289916f0673e41c848cc126999e` (`main`)

## Outcome

This track found no additional independently actionable defect. It investigated
immutable dtplyr row-selection graphs, repeated mutations, Mutable-step
inspection isolation, and SQL/Arrow inspection and nesting-admission boundaries.
No package implementation or permanent package test was changed.

The prior investigations consulted were
`investigation/nesting-grouping-deep-hunt-2026-10-29.md`,
`investigation/2026-10-29-nest-spec-exhaustive/backend/README.md`, and
`investigation/2026-10-29-nest-spec-dispatch-followup/README.md`.
Their measurements were not counted as runs on this baseline. The local issue
index showed #995, #996, #1003, #1017, #1024 and #1043 closed; their established
failure boundaries informed the additional probes rather than becoming new
reports.

The contracts read were the nesting and inspection help, `CONTEXT.md`, and ADRs
0002, 0016, 0020 and 0029. The source predicates and rewrites inspected were
`dtplyr_metadata_safe_step()`, `dtplyr_selection_proxy()`,
`zero_row_dtplyr_proxy_input()`, `dtplyr_step_input_fields()`,
`isolate_dtplyr_cached_inputs()` and `dtplyr_margin_input()`.

## Completed measurements

Each script loaded the investigated working tree with `pkgload::load_all()`.
Its adjacent log retained the final completed invocation. All four final
invocations returned terminal exit zero. They were dated investigation probes,
not package regression tests or evidence that untested inputs were safe.

| Script | Scope and oracle | Observations |
| --- | --- | --- |
| `row-selection-graphs.R` | Three row counts; ordinary, `.SD`-named and factor inputs; 13 distinct/slice/rank/duplicate/lag row operations; eight cache/rename/self-join/self-union compositions. Explicit source collection followed by local inspection/nesting supplied the oracle. | 819 constructed graphs; 450 typed-plan matches and 369 Package-condition refusals. Both verbs and both keep policies supplied 3,276 nesting comparisons: 1,800 matched and 1,476 were refused. No admitted mismatch, warning or caller-table mutation. |
| `mutable-metadata-boundary.R` | Empty/populated inputs, mutable permission on left/right/both roots, join/semi/set/filter/cache/unsafe-transform graphs, cached and uncached. Inspections were checked against the declared schema, while both Margin verbs had to refuse before callbacks or writes. | 96 graphs; 192 inspections, comprising 180 matches and 12 metadata refusals. Both verbs gave 192 Mutable-step refusals. Row predicates and unsafe callbacks ran zero times; all caller-table snapshots stayed identical. |
| `repeated-mutations.R` | Seed 30102917; 600 generated sequences of two to eight scalar/copy/arithmetic/NULL updates with repeated destination names; zero/one/five rows. Explicit collection's column types supplied the oracle. | 542 sequences constructed; 252 ordinary source collections failed and were excluded. The other 290 produced 176 matching typed selections and 114 metadata refusals. No admitted type mismatch or caller-table mutation. Eight metadata warnings matched ordinary source warnings about deleting nonexistent columns; no additional warning was observed. |
| `remote-schema-boundary.R` | Arrow readers behind distinct, limit/order, grouped summary and join/union graphs; live SQLite/DuckDB distinct/grouped/windowed/nested-limit/summary/union relations; both inspection formats, typed/name-only specifications, invalid local arguments and both nesting verbs. | 24 Arrow calls preserved all four readers and their 12 original rows. Each SQL engine supplied 60 boundary calls. SQLite sent zero statements; DuckDB sent 24, all containing `LIMIT 0`. Nesting refusals and locally decidable invalid calls sent no statement. |

The first matrix attempted 936 graph constructions. Its 117 excluded
constructions were the three `aux`-referencing post-operations applied to the
special-name fixtures, whose `aux` column had deliberately been renamed `.SD`.
They did not count as metadata or nesting refusals. Its completed cases did not
establish optimal admission for every conservative metadata refusal.

The repeated-mutation generator likewise excluded construction or ordinary
collection errors before comparing metadata. A source operation that already
failed was not presented as a package defect. Its eight matching deletion
warnings were recorded separately from artificial metadata-only warnings.

## Candidate exclusions and harness corrections

An exploratory root with a multi-column matrix forcibly inserted through
`data.table`'s `[[<-` retained that matrix through ordinary dtplyr collection,
but ordinary data.table row selection also rejected it as a malformed table:
the column's length exceeded the row count. The usual tibble-to-data.table
conversion expanded the matrix at entry instead. Neither observation
established a defect in marginplyr's supported nesting pipeline.

Additional small controls for non-scalar numeric replacement of character
columns, column copies, and a repeated new-column mutation agreed with the
explicitly collected schema. No finding was inferred from lexical operator
lookups or conservative refusal alone.

During probe construction, the mutable cache-chain fixture was corrected to
include both roots; its original left-only form could legitimately be
immutable when only the right root was mutable. The remote probe was restricted
to the two documented inspection formats, `list` and `text`. The mutation probe
initially demanded zero metadata warnings, then used its ordinary-source
control to distinguish the eight genuine source warnings from additional
warnings. The retained scripts and counts described the corrected probes.

## Replay and limits

From the investigated checkout root:

```sh
Rscript investigation/2026-10-30-nest-spec-greedy/backend/row-selection-graphs.R
Rscript investigation/2026-10-30-nest-spec-greedy/backend/mutable-metadata-boundary.R
Rscript investigation/2026-10-30-nest-spec-greedy/backend/repeated-mutations.R
Rscript investigation/2026-10-30-nest-spec-greedy/backend/remote-schema-boundary.R
```

The first script also wrote the per-comparison CSV. `environment.txt` recorded
the dependency versions and R/macOS environment. No dependency was installed or
changed for these experiments.

The finite probes did not establish behavior on a live PostgreSQL or hosted
database, other dependency versions or operating systems, arbitrary extension
methods, nondeterministic source expressions, unbounded graph depth, or large
performance loads. SQL and Arrow measurements concerned inspection and
refusal, not support for nested list columns on those backends. Metadata
refusals were not correctness passes for the refused operation.
