# Expansion and nesting spelling audit

Investigated: 2026-10-29

The audited package snapshot was `4c63bb917ed7ed31ff6aadf060a5eae7a32cd004`.
The runtime was R 4.6.1, data.table 1.18.6.1, dtplyr 1.3.3, dplyr 1.2.1,
and tidyr 1.3.2. This part of the investigation covered
`expand_with_margins()`, `nest_with_margins()`, and
`nest_by_with_margins()`. It established no additional spelling-specific defect
in these row-producing paths.

## Method

[`check-row-verbs.R`](check-row-verbs.R) compared each immutable lazy dtplyr
call with the same public call on a local tibble. A candidate spelling occupied
the payload, fixed `.by` key, Grouping dimension, requested `.id`, and nesting
`.key` roles; expansion omitted the last role. The baseline crossed zero, one,
and four rows with `.sort = "none"` and `"last"`, and both nesting `.keep`
values. Inputs included duplicated rows, a missing key, two Grouping dimensions,
and existing columns resembling generated aliases.

The comparison checked public names, typed outer values, complete nested-cell
row multisets, original nested key values under `.keep`, source integrity, lazy
result class, and the rowwise result class. Result rows were canonicalized for
comparison, so exercising a `.sort` branch did not independently assert every
Margin-order relation. Cell classes were normalized as permitted by ADR 0016.
The script captured warnings and errors separately. Its successful exit meant
that the audit completed, not that every probe was accepted.

The 65 baseline spellings comprised the preceding follow-up's 62
data.table-related spellings, the dplyr `.rows` control, and the R/tibble `..1`
and `...` controls. This included all nine documented special-symbol spellings
and the additional locked bindings `.SDall`, `.xSD`, and `.iSD`. The candidate
list also covered constructor and bracket formals, `CJ()` formals, join prefixes,
non-equi and split temporary names, the `..` mechanism, and installed evaluation
function bindings. `.iGRP` and `.xGRP` remained negative probes, not claims that
data.table installed those symbols.

The typed phase crossed the nine documented symbols and three additional locked
bindings with factor, ordered-factor, integer, Date, and list grouping values,
zero and four rows, typed-missing labels, and text labels for factors. A named
`label` value of `"named"` in the CSV denoted the scalar `"Total"` label, not a
per-column label map. Every typed phase used `.sort = "last"`. An ordinary-name
control repeated the same 336 case shapes.

[`check-composition.R`](check-composition.R) rotated twelve simultaneous special
names through fixed keys, two dimensions, identifier, and nesting list-column
roles. Other special names remained payloads, alongside alias-looking input
names. It crossed all three `.sort` modes and both nesting `.keep` settings. It
also tested a single special key leaving zero-column cells, and reused expansion
and nesting results as inputs to all three verbs and typed inspection. The
reused-input comparisons included an ordinary-name control and used the same
backend-produced cell classes on both inputs.

## Results

The 9,360 baseline comparisons produced 9,144 matched results and 216
local-control errors. All 8,928 calls involving the 62 data.table-related
spellings matched. The remaining errors were 36 documented `.rows` rowwise-key
refusals and 180 R/tibble source-name refusals for `..1` and `...`. Every
non-dot-dot source control collected identically; no captured warning or source
mutation was recorded. Results and their console summary are in
[`baseline-results.csv`](baseline-results.csv) and
[`baseline-console.txt`](baseline-console.txt).

The 4,032 typed comparisons produced 3,504 matched results and 528 dtplyr errors.
Every error was a list-key case; all factor, ordered-factor, integer, and Date
cases matched. The ordinary-name run produced 292 matched results and 44 errors.
Matching each of the 4,032 special-name records to its ordinary-name case shape
found zero status differences. The list errors reported unsupported list grouping,
unsupported list ordering, or `Invalid input`; the measurement did not establish
a special-name cause. Results are retained in
[`typed-results.csv`](typed-results.csv) and
[`typed-control-results.csv`](typed-control-results.csv), with their respective
console summaries. Every typed source control collected identically, and no
captured warning or source mutation was recorded.

The composition run recorded 408 comparisons:

| Shape | Matched | Refused |
| --- | ---: | ---: |
| Simultaneous special-name roles | 180 | 0 |
| Special keys leaving zero-column cells | 72 | 0 |
| Reused Margin result passed to a row verb | 0 | 130 |
| Reused Margin result passed to typed inspection | 0 | 26 |

The 156 reused-result refusals all reported that column metadata could not be
safely determined without evaluating upstream operations. Each special spelling
and the ordinary-name control produced the same twelve refusals. This audit did
not establish a spelling-specific failure at that derived-input admission
boundary. Individual outcomes and the summary are retained in
[`composition-results.csv`](composition-results.csv) and
[`composition-console.txt`](composition-console.txt).

## Existing reports and conversion boundaries

The old `.ROW` nesting and expansion reproductions collected successfully at the
audited snapshot. PR [#959](https://github.com/sayuks/marginplyr/pull/959)
contained that protection. The additional locked-binding defect from
[#948](https://github.com/sayuks/marginplyr/issues/948) had also been repaired.
These previously reported findings were not reopened.

The baseline's `.rows` rowwise-identifier refusal was documented behavior.
The `..1` and `...` source-name probes encountered the R/tibble input-name
boundary. For supported generated dot-dot output names, the audit used
`tibble::as_tibble(result, .name_repair = "minimal")`, matching the established
controls for [#968](https://github.com/sayuks/marginplyr/issues/968) and
[#950](https://github.com/sayuks/marginplyr/issues/950). Ordinary dtplyr
`collect()` did not forward minimal name repair. That conversion difference was
not treated as a new Margin implementation defect.

## Reproduction

Run from the repository root:

```sh
Rscript investigation/2026-10-29-datatable-reserved-bug-hunt/rows/check-row-verbs.R baseline
Rscript investigation/2026-10-29-datatable-reserved-bug-hunt/rows/check-row-verbs.R typed
Rscript investigation/2026-10-29-datatable-reserved-bug-hunt/rows/check-row-verbs.R typed-control
Rscript investigation/2026-10-29-datatable-reserved-bug-hunt/rows/check-composition.R
```

The typed and baseline result files were checkpointed after each candidate. The
console's final candidate/call totals distinguish a completed run from a partial
checkpoint. No package implementation or regression test was changed in this
investigation.
