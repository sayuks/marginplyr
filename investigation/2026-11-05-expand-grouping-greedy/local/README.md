# Local expansion row, type and order comparison

Investigated: 2026-11-05
Baseline: `0403c9150394cbae21b59f34c75791d0873b8451` (`main`)

## Outcome

No new local expansion defect was established by these probes. The independent
[local comparison](local-oracle.R) completed 4,455 matches with no ordinary
control error, expansion-only error or value/type/order difference. Each case
also required the serialized input to remain unchanged. The retained
[execution log](local-oracle.log) records the summary and environment.

The comparison crossed 19 families, zero/one/four source rows, grouping
dimension/fixed-key/payload roles, tibble/data.frame/data.table inputs, three
Grouping families and all three Margin order options. The family fixtures
covered integer, double with NaN and NA, character, logical, complex, factor,
ordered factor, factor NA levels, Date, POSIXct with a time zone, difftime,
hms, integer64, units, haven labelled, plain list, list-of, packed data-frame
and matrix columns. It used NULL Margin labels throughout. The three plans
were a rollup, a grouping-set union whose second occurrence was the Grand
total set, and a union retaining a repeated grouping set.

The expected grouping sets were written independently rather than read from
inspection. Each expected branch used source values or a row-sized typed
missing value, and ordinary dplyr binding combined the branches. A separate
ordering function derived Grouping bits from the explicit occurrence lists,
then ordered each fixed key and dimension by the documented missingness and
value terms. It compared public names, column values and attributes, physical
row order and the occurrence identifier. These finite comparisons therefore
covered more than unordered value bags, without treating any earlier hunt as
a new execution.

The 4,455 comparisons comprised 1,539 each for tibble and data.frame and 1,377
for data.table. Native data.table conversion unpacked or reshaped packed and
matrix columns. Eighteen source-construction combinations were excluded
before expansion, accounting for the 162 absent comparisons; they were not
counted as successful expansions of those original column shapes.

The [group-state and re-input probe](group-state-reinput.R) made 54 cases.
Of these, 27 confirmed either the row-wise-input refusal or the `.drop = FALSE`
grouped-input refusal. The other 27 compared plain or `.drop = TRUE` grouped
inputs against explicit fixed keys, checked the ungrouped result, expanded the
result a second time, and verified the copies of source row identifiers,
previous occurrence identifiers and fixed keys. This probe did not make a
second independent claim about the full re-input Margin order.

## Candidate classification

An earlier exploratory sweep attempted 810 combinations of 30 native or
extension families, three source row counts, three input containers and
NULL/NA/text labels. It separated expansion errors from errors in a comparison
oracle before any claim was made. No finding was inferred from its raw counts.

- Raw vectors and sfc keys could not supply the requested typed-missing value;
  structured keys rejected a text label. These matched the documented admission
  boundaries, rather than demonstrating row-copy corruption.
- Native data.table conversion changed structured column names/shapes or
  POSIXlt storage. Those were source-construction effects and did not establish
  a defect in expansion of the original tibble shape.
- Surv and two-dimensional table equality proxies recursed or exposed their
  auxiliary dimension labels when the exploratory oracle tried to compare
  them. Actual tibble/data.frame NULL-label expansion completed. Those oracle
  signals were not counted as package failures.
- An AsIs POSIXlt key became a packed row containing its time components on
  included and omitted branches. The result kept the original components and
  a wholly missing omitted row. Ordinary union of the original key and its
  initialized packed prototype refused their different types. The package's
  explicit packing did not establish a new defect; arbitrary class restoration
  would also contradict ADR 0016's promise boundary.

The recent adversarial and invariant hunts, the October 30 structured-type
hunt, and the October 31 local follow-up supplied known witnesses and
exclusions. In particular #1069/#1071 concerned missingness and prototypes,
#1242 concerned factor NA-level codes, and #993 concerned sf column order.
None of those historical measurements was included in the 4,455 matches.

## Replay and limits

Run from a checkout containing the investigated package source:

```sh
Rscript investigation/2026-11-05-expand-grouping-greedy/local/local-oracle.R
Rscript investigation/2026-11-05-expand-grouping-greedy/local/group-state-reinput.R
```

Both commands exited zero. The first writes its per-case CSV to a temporary
directory by default; set `MARGINPLYR_HUNT_OUTPUT` to retain it. The environment
was arm64 macOS Tahoe 26.6.2, R 4.6.1, dplyr 1.2.1, vctrs 0.7.3, tibble 3.3.1,
data.table 1.18.6.1, haven 2.5.5, bit64 4.8.6, units 1.0-1 and hms 1.1.4.
The fixture coverage did not establish arbitrary extension-class support,
other package versions or operating systems, unrestricted plan sizes, or
lazy-backend behavior. All changes in this directory were dated evidence and
standalone investigation commands; no implementation or package test changed.
