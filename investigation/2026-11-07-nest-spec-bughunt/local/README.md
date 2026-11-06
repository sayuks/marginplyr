# Local nesting source membership and boundary probes

Investigated: 2026-11-07
Tested package SHA: `e1dabaabb392a3791177e5c48e983f780c7c93ec`

## Outcome

This lane established no new package defect. It made 1,888 public Nest calls:
1,800 semantic matches and 88 documented or independently reproduced native
refusals. Package implementation and permanent tests were not changed.
`TESTTHAT_PARALLEL=false` was set in each script. Every retained execution was
collected to terminal completion, with no subprocess or backend connection left.

## Independent membership oracle

`property.R` made 1,536 calls across sixteen source types, empty and populated
inputs, four known Grouping families, both Nest verbs, both keep choices and
all three sort requests. Each family had explicit expected sets: the script
did not call package Grouping-plan expansion to construct its expectation.
It independently grouped original fixed keys and present dimensions with
`vctrs::vec_group_id()` and checked source-row partitioning, one cell per source
group, separation of distinct groups, and each cell's exact source projection.

The 1,464 successful results matched. The remaining 72 calls attempted a
raw dimension with an omitted grouping set and a typed-missing Margin label;
all produced the documented `marginplyr_error` refusal. The summary CSV and
empty dput failure list recorded zero unexpected mismatches. Deliberately
wrong synthetic memberships calibrated omission, duplication and extra-row
detection. Sort variants checked that ordering did not damage membership or
payloads; they did not independently establish every Margin-order term.

The sixteen types were character, integer, double (NA and NaN), logical,
complex, ordinary factor, ordered factor, factor with an NA level, Date,
POSIXct, difftime, plain list, a list with typed missing and NULL elements,
matrix, packed data frame and raw. A typed column was both a dimension and
payload in each fixture; character fixed keys supplied two independent
partitions. This was a finite corpus, not an arbitrary-class proof.

The first oracle draft incorrectly excluded an ordinary non-dimension column
from one payload projection and compared empty `NULL` concatenation with
`integer()`. These were harness errors, not package failures. Both were
corrected and all 1,536 calls rerun. The retained portable script integrated
the classification and calibration rather than requiring postprocessing.
Its final execution handle was `56156`, terminal exit 0.

## Names, empty shapes and generated helper lookup

`names-empty.R` made 272 comparisons. Sixteen spellings were crossed as list
column names or payload names, both Nest verbs, both keep choices, and empty
or populated sources. The spellings included dots, numbered dots, pronouns,
`.rows`, `.SD`, converter/helper names, braces, a newline, Unicode, and occupied
private-name prefixes. Source serialization remained unchanged for every call.
A further sixteen calls checked zero-column source row counts and the verbs'
distinct keyless-empty behavior. There were 256 semantic matches and sixteen
paired native refusals: payload columns named `...` or `..1` were also refused
by native tidyr/dplyr nesting. Those were not promoted to package defects.

The first names harness used tibble subsetting for empty/projection preparation,
which itself refused a reserved name. Preparation was changed to vctrs slicing
and list projection. The subsequent actual package refusals were checked against
native nesting and explicitly classified. The complete retained run used
handle `63386`, terminal exit 0.

`local-bindings.R` made eighty successful public comparisons across ten
caller-local helper bindings, empty/populated inputs, both keep choices and
both Nest verbs. No masked helper was invoked, and expected visible groups and
cell payloads matched. The generated helper names were `list`, `as_tibble`,
`seq_len`, `min`, `vec_slice`, `n`, `summarize`, `rowwise`, `is.na`, and
`if_else`. Its execution handle `50410` reached terminal exit 0.

## Prior evidence and excluded candidate

The lane read the recent November 6 greedy, exhaustive and dispatch-boundary
nesting investigation READMEs from their retained worktrees, and the last
lane's local report. Those supplied prior type, name, empty/grouped and
independent Margin-order coverage; their historical counts were not counted
as executions on this SHA. The Nest sources, source-equality/display-helper
and empty-row tests, CONTEXT, AGENTS and ADRs 0012, 0016 and 0018 supplied the
contracts and comparisons.

A classed stored-FALSE logical with a constant-false negation method was
considered as an admission-refusal candidate. Full retrieval of closed
[#1360](https://github.com/sayuks/marginplyr/issues/1360), including its comment,
showed that this exact reverse-negation refusal was already recorded and
explicitly outside that ticket's accepted-flag boundary. No new accepted-value
contract was inferred and no duplicate ticket was proposed. A stored-TRUE
`is.na` method variation likewise did not establish that admission contract.

## Replay and limits

Run each command separately from any directory, passing the tested source root.
The property output directory is optional; its default is R's temporary directory.
The other scripts report to standard output.

```sh
Rscript property.R /path/to/tested-source /path/to/output
Rscript names-empty.R /path/to/tested-source
Rscript local-bindings.R /path/to/tested-source
```

`property.log` retained sessionInfo: R 4.6.1, aarch64-apple-darwin23,
macOS Tahoe 26.6.2, dplyr 1.2.1, vctrs 0.7.3, tibble 3.3.1 and rlang 1.3.0.
These scripts did not run the package suite or Review-ready. They established
no result for another OS/dependency version, arbitrary user classes,
interruptions, unbounded resources, remote backends or all expression trees.
Object-level attribute restoration was not asserted, consistently with ADR 0016.
