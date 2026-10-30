# Lazy expansion and metadata boundary probes

Investigated: 2026-10-30
Baseline: `28c449aa03fd5289916f0673e41c848cc126999e` (`main`)

This track found no new confirmed defect. It extended the October 29 Arrow
physical-type differential to FileSystemDataset inputs, after the Directory
partition repair introduced a deferred ordering boundary. It also challenged
the dtplyr metadata-safety predicate with repeated mutation destinations.
No package implementation or regression tests were changed.

## Arrow file types

`arrow-file-types.R` adapted the fixture and ordinary-union comparison from
`2026-10-29-expand-grouping-followup/arrow-types.R`. The final execution made
1,656 comparisons: 46 successfully constructed physical type families,
Parquet/IPC sources, empty/populated inputs, NULL/NA/text labels, and three
sorting options. The families included half floats, decimals, temporal widths
and units, binary forms, nested lists/structs, maps, an extension type, and
ordered/unordered integer/double/text dictionaries. Dictionary fixtures used
native dictionary encoding followed by a cast to the requested index type.

The final statuses were 1,510 value/schema matches, 130 cases where both paths
raised, and 16 where only the ordinary control raised. No package-only error,
value difference, or physical dimension-type difference was reported. The 16
control-only failures were text labels on numeric IPC dictionaries: ordinary
union tried to parse `Total` as the dictionary value type, while expansion
returned a character dimension. Both-error cases included unsupported nested
casts/sorts and fixed-size-list Parquet limitations; they were not evidence of
successful expansion.

The oracle read the type from the opened source schema. An earlier pass used
the pre-write Array type for its NULL control, misclassifying Parquet's native
dictionary and temporal normalization as control failures. Those intermediate
counts were discarded. Canonicalization compared row identifiers, occurrence
identifiers, dimension values, and dimension physical types while ignoring
unspecified order; this was not a complete Margin-order oracle. Warnings were
not compared or counted. The output CSV retained both error diagnostics.

## dtplyr repeated mutations

`dtplyr-mutate-matrix.R` used seed 301030 and attempted 1,600 mutation queries.
It crossed 0/1/3-row roots, six families for a source column (character,
double, integer, factor, Date, logical), and one to five updates drawn from
copies, scalar constants, NULL removal, and simple arithmetic. Destinations
could repeat. Each collected source was compared with typed inspection under
six `typeof` predicates.

There were 695 admitted matching cases, 665 documented metadata refusals,
39 ordinary query-construction failures, and 201 ordinary collection failures.
No typed-plan mismatch or unexpected inspection error was reported. The matrix
was a metadata-selection oracle, not a full expansion value/order oracle. It
did not compare warnings or exhaust arbitrary R expressions and custom classes.

## Focused successful controls

`boundary-controls.R` passed eight checks: four Arrow virtual scanner names
used as occurrence identifiers (`__filename`, `__fragment_index`,
`__batch_index`, `__last_in_fragment`), one dtplyr caller `copy` binding that
was not invoked by generated isolation calls, and three dtplyr aliases copied
from columns named `..x`, `.env`, and `.data`.

## Replay

Run the three commands recorded in `replay-status.json` from the repository
root. The matrices wrote CSVs under `MARGINPLYR_HUNT_OUTPUT`; the recorded runs
set it to `/tmp`. Their adjacent retained logs and CSVs were the terminal
outputs of successful runs. The matrix runners were diagnostic reports whose
exit zero did not assert that every attempted input was supported. The focused
controls used assertions and exited zero. No testthat suite was run by this
track. The final directory passed `jarl check`.
