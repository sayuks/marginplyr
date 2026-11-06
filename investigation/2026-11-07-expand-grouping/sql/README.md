# SQLite declared-source type boundaries

Investigated: 2026-11-07
Baseline: `e1dabaabb392a3791177e5c48e983f780c7c93ec`

The hypothesis was that wrapping a source, placing an omitted set first, or
asking for Margin order could lose source values or collected types. The
November 6 SQL graph and Boolean-alias investigations supplied previous
coverage; their measurements were not counted here.

`types.R` ran against a disposable archive of the baseline with private
in-memory SQLite connections. Its 2,400 final comparisons all matched an
independent local row-copy oracle constructed with ordinary mutation and
`union_all()`. The matrix crossed both RSQLite `extended_types` settings,
ten declared source types, root/window/collapse/limited/inline source graphs,
nonempty/empty sources, four grouping families and three sort choices. Families
included one included set, a rollup, an omitted-first union and repeated
omitted occurrences. Each expected source was collected independently from
the exact input query, and omitted values used that column's vctrs prototype.
Values, public column order, column classes and row multiplicities matched.
Canonicalization by occurrence and source-row identifiers meant that these
comparisons did not independently establish physical Margin order.

An initial 2,400-case draft reported 528 differences. Its empty oracle sliced
a nonempty collection instead of collecting the empty query; ordinary SQLite
gave different types for empty computed/inline columns. Its BLOB fixture also
mixed integer and binary storage in one column. RSQLite's result inference
could then change after ordinary sorting. Those differences were not counted
as package findings. The final run collected the exact empty source and used
binary storage consistently for the BLOB fixture. The original draft supplied
no passing observations to the final count.

The final original execution handle was `80038`, collected at exit zero;
the initial execution `34456` also ended at zero. `types.log` and `types.csv`
retained the final classifications, and `environment.txt` recorded R 4.6.1,
RSQLite 3.53.3, dplyr 1.2.1 and dbplyr 2.6.0 on arm64 macOS.

Replay from an archive or checkout of the investigated package:

```sh
MARGINPLYR_HUNT_OUTPUT=/private/tmp/sql-types-replay Rscript /absolute/path/to/types.R
```

No package tests, builds, rendering or implementation edits ran in this lane.
Other drivers, collations, platforms, dependency versions, arbitrary mixed
SQLite storage and resource limits were outside the measured matrix. No new
SQL defect was confirmed by this lane.
