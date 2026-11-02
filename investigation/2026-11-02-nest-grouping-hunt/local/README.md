# Local nesting composition and empty-result audit

Investigated: 2026-11-02
Baseline: `978d7bef4247bf1348af5b90bfab47924db25500` (`main`)

## Outcome

This track found no new confirmed defect in the finite local boundaries below.
It changed no package implementation or permanent package test, and published
no Issue. The October 30/31 nesting investigations supplied earlier failure
patterns; their earlier measurements were not counted as runs on this baseline.

The investigated contracts were the nesting reference's one cell per source
group, original source values under `.keep`, source-row cardinality and empty
no-key cardinality, and the ungrouped/row-wise return shapes. ADR 0016 supplied
the boundary against promising arbitrary attribute restoration. ADRs 0012 and
0018 supplied the factor identity and Margin order context. The probe varied
sort choices to detect membership/projection interference; it did not claim an
independent complete Margin-order oracle.

## Hypotheses and separating evidence

1. Multiple typed keys could lose source equality when dimension identities,
   fixed keys, branch union and nesting composed. An independently enumerated
   source-group oracle would disagree with cell memberships. This was refuted
   in all 2,160 completed composed-key calls.
2. Expansion or sorting could duplicate, discard or cross-associate payloads
   and `.keep` copies, or mutate a caller source. The source-row identifiers,
   original source projections and serialized input would then disagree.
   There were 174,516 matching cell-column value projections and no serialized
   source change in that same matrix.
3. Scalar output-name metadata could leak into empty no-key reconstruction,
   particularly when both `.id` and `.key` carried dimensions or R names and
   the list column had a special spelling. The documented outer cardinality,
   output names, integer identifiers or row-wise identifiers would differ.
   All 500 calls matched the required shapes and source cells.

## Completed matrices

`composed-key-oracle.R` used seed 26110231 and generated 180 sources/plans.
It independently enumerated rollup, cube and grouping-set-union sets rather
than reading the package's inspected plan. It crossed two dimensions and
zero/two fixed keys with both nesting verbs, both `.keep` values and all three
sort choices. Sources included zero rows, all six rows, reordered subsets and
duplicate sampled source rows; new row identifiers distinguished sampled
occurrences. The ten candidate key classes were character, a factor with an
NA level and actual missing codes, a valid custom vctrs vector with a
bucketed equality proxy, package versions with equal values and different
renderings, packed data frames, AsIs matrices, reference-bearing lists,
complex vectors, raw vectors and integer64 values above double precision.
Every input retained the other key classes as payload when not selected.
Raw dimensions used a non-missing string label, because a typed-missing raw
Margin value was intentionally refused; other dimensions used typed missing
labels. All 2,160 calls were admitted and matched the source oracle. The CSV
retained every case, including the chosen key columns and plan family.

For each explicit set, memberships were compared as unordered collections of
ordered source-row identifier sequences. Outer row order was not used in that
comparison. Cell column order was checked against the documented key-first
`.keep` layout. Values were first compared with `identical()`; an
attribute-only difference could pass `all.equal(check.attributes = FALSE)`
in accordance with ADR 0016. Arbitrary class/attribute restoration was not
part of this probe's verdict.

`scalar-empty-boundaries.R --assert` made 500 calls crossing zero/three source
rows, five wrappers for each scalar name (plain, named, one-by-one matrix,
three-dimensional singleton array and AsIs), five list-column spellings
(`cell`, `.rows`, `..1`, `...` and an embedded newline), and both nesting
verbs. The output names, expected outer row count, integer identifier, rowwise
group names and entire cell values matched. It also independently confirmed
both verbs' package-version equality result, and replayed the October 31
NA-level-factor desired-behavior assertion successfully. The prior factor
reproducer's printed sentence about removing the NA level describes its
historical control; on this baseline the original NA-level case also kept one
cell, as its displayed result and desired-behavior assertion showed.

The scalar-name history was checked with:

```sh
gh issue list --state all --search 'nest' --limit 200 --json number,title,state,url
gh issue view 997 --comments --json number,title,body,comments,labels
gh issue view 831 --comments --json number,title,body,comments,labels
```

The complete bodies and empty comment lists of closed
[#997](https://github.com/sayuks/marginplyr/issues/997) and
[#831](https://github.com/sayuks/marginplyr/issues/831) were read and retained
in the adjacent JSON files. The tested cases supplied no remaining scalar-name
failure to publish or deduplicate. No existing Issue was modified.

## Replay and harness corrections

Run from the investigated checkout root:

```sh
Rscript investigation/2026-11-02-nest-grouping-hunt/local/composed-key-oracle.R
Rscript investigation/2026-11-02-nest-grouping-hunt/local/scalar-empty-boundaries.R --assert
```

The final composed-key run was retained through terminal exit 0 under shell
session 29714; the scalar run was retained through terminal exit 0 under
session 75402. Adjacent logs recorded both results and the composed-key log
recorded the R/platform and package versions.

The first fixture construction used an invalid bare major-version string and
failed before target calls. A subsequent completed development run (session
70264, terminal exit 0) exposed two harness choices: anonymous function heads
inside `grouping_sets()` were not recognized constructor spellings, and raw
keys could not use typed-missing Margin values. The final run used ordinary
constructor spellings and per-dimension non-missing raw labels. Earlier run
counts were excluded from the completed matrix above. These were corrected
probe inputs, not package findings.

These finite measurements covered one R/package/OS environment and local
tibbles. They established neither arbitrary S3/S4 behavior, all possible source
classes, remote nesting support, a complete sort oracle, nor behavior under
unbounded plans or resource exhaustion. No full test suite or Review-ready
invocation ran in this track.
