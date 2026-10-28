# Nesting and Grouping specification bug hunt

Investigated: 2026-10-29
Baseline: `58356a67a79d1d2961eac6b8865d30afdb751d5c` (`main`)

## Outcome and ticket boundary

The investigation found one new, reproducible defect: dtplyr Margin results
rejected valid dot-dot Grouping set identifier names. It was published as
[#968](https://github.com/sayuks/marginplyr/issues/968), labelled `bug` and
`ready-for-agent`, with no blockers. The ticket covered one public contract
from identifier creation through nesting and finalization. Separate tickets
for those internal stages would have left the same supported call broken
after either partial repair.

The primary targets were `nest_with_margins()`, `nest_by_with_margins()`, the
five Grouping specification constructors, and `inspect_grouping()`. Related
Grouping bit and Grouping identifier results were compared across backends.
Summary and expansion calls served as controls and established the additional
finalization path reached by the same identifier defect.

This commit recorded evidence only. No package implementation, package tests,
generated documentation, dependency metadata, or CI configuration was changed.
A lack of further findings meant that the recorded probes found no additional
contract violation on this snapshot, not that all possible inputs were correct.

## F1: dtplyr revalidated its own generated identifier as an input name

The minimal input had one integer column and one row. No unusual source name,
Grouping specification, fixed key, or sort request was needed:

```r
input <- tibble::tibble(value = 1L)
nest_with_margins(dtplyr::lazy_dt(input), .id = "..1")
nest_by_with_margins(dtplyr::lazy_dt(input), .id = "..1")
```

Both calls raised `vctrs_error_names_cannot_be_dot_dot`:

```text
Names can't be of the form `...` or `..j`.
✖ These names are invalid:
  * "..1" at location 1.
```

Both local counterparts succeeded, and a safely named `.id = "set"` succeeded
on the same dtplyr input. An ordinary dtplyr mutation could create a `..1`
column and materialize it with minimal name repair. Those controls separated
a generated-output handling defect from an unsupported source column name or
a backend inability to store the output name.

The public `.id` contract in `summarize_with_margins()` admitted a scalar string
and prohibited collisions; the nesting verbs inherited that contract. Its
*Grouping set identifiers* section made the identifier an outer key and, for
the rowwise counterpart, a rowwise grouping key. The failing calls violated
neither a collision rule nor a scalar-string rule.

The identifier matrix in `identifier-matrix.R` exercised five names × two
input sizes × four plans (no grouping, fixed keys, rollup, and duplicate-set
dropping) × two `.keep` values × three sort values × both nesting verbs. All
480 cases failed; their local exotic-name and dtplyr ordinary-name controls
matched, and source tables remained unchanged. The supplementary `reproduce.R`
failed all 20 combinations of those names and four Margin verbs, while every
ordinary dtplyr mutation/collection control succeeded. Its `--assert` mode
exited one on the baseline, providing a regression signal for the ticket.

### Mechanism and discriminating controls

For nesting, the captured trace reached `nest_expanded_margins()` after the
identifier had been staged. Its `get_col_names()` call enumerated payload
columns through `dplyr::select()`, which reached dtplyr's tidyselect interface
and rejected the generated identifier. The preceding
`with_margin_column_aliases()` protected local data frames, but did not alias
dtplyr steps. The failure occurred during construction, before collection or
the final rowwise conversion; those later stages could not explain it.

Identifiers `..0` and `..01` exposed a further evaluation boundary: a lazy
nesting result could be constructed, but its generated bare key references
were interpreted as R dot arguments during collection. The rowwise verb
performed that collection before returning; the replay labelled that outer
phase `construction`, and its coarse stage classifier printed `unknown` for
those two rowwise cases. The observed errors
were `indexing '...' with non-positive index 0` and
`..1 used in an incorrect context, no ... to look in`.

Related `summarize_with_margins()` and `expand_with_margins()` calls reached a
second refusal: finalization's `reorder_margin_columns()` sent the generated
name through `relocate.dtplyr_step()`. Repairing only the nesting enumeration
would therefore have left the shared output-order path unproven. Ticket #968
required the complete output-name behavior while retaining laziness, rowwise
identity, source-row membership, and collision checks.

The issue search read closed #827, #950, and #964, including their comments.
#827 concerned summary outputs and identifiers on earlier paths. #950 and
#964 concerned nesting list-column names; #964's additional dot-dot identifier
verification used local data frames and tibbles. None supplied the missing
dtplyr identifier coverage. `expect_dot_dot_nesting()` in the existing tests
varied dot-dot `.key` values while its public identifier was absent or named
`set`; it did not exercise this identifier-name boundary. The three open issues at the start of this hunt
were unrelated release or upstream-report work.

## Search coverage and negative results

All counts below described the recorded invocations on the baseline. A case
could include a public call and a separate oracle/control call, so counts of
cases were not a single total of public API calls.

| Area | Probe | Observed result |
|---|---|---|
| Grouping-plan algebra | `plan-algebra.R`: 1,200 seeded trees with non-syntactic names, composite and overlapping dimensions, all five kinds | Included columns, Grouping-plan order, Grouping bits, set IDs, and `keep`/`drop` behavior matched an independent oracle |
| Independent constructor algebra | `specification-algebra.R`: 1,500 seeded legal trees, up to five columns and deeper nesting | Expansion and deduplication matched a separately written oracle |
| Capture and forwarding | `capture-boundaries.R`: 510 cases across five consumers | Constructor masking, parentheses, computed-head refusal, forwarding, `inject()`, `!!!`, `{{}}`, delayed bindings, and quosure environments matched the contract |
| Binding and conditions | `binding-context.R`: 135 cases | Colliding bindings, active binding changes between operations, evaluation counts, and caller warning/error identity matched the contract |
| Local column types | `local-types.R`: 128 calls | Sixteen types, fixed/dimension roles, `.keep`, both verbs: source-row membership and original inner values matched independent grouping |
| Local output names | `local-names.R`: 968 calls; `output-names.R`: 64 further cases | Special `.key`/`.id` spellings, empty input, and distinct output names survived |
| Random local nesting | `local-membership.R`: 300 generated datasets, 556 admitted calls, seed `492763` | Independent `vec_group_loc()` membership, fixed partitions, up to eight sets, duplicate values, missing values, factor keys, original `.keep` values, output columns, and all sort choices matched |
| sf follow-up | `local-sf.R`: 4,176 sf/tibble pairs, 8,352 calls | Values and refusals matched; serialization checks found no source mutation |
| dtplyr name roles | `backend-names.R`: 100 pairs | 83 matched, 14 were refused by both inputs, and three identifier cases exposed F1 |
| dtplyr column types | `backend-types.R`: 456 pairs over 19 types | 298 matched, 22 were refused by both, 124 were dtplyr refusals, and 12 value comparisons differed; the conversion/order limits below prevented treating these as additional findings |
| dtplyr metadata | `backend-metadata.R`: 100 column-copy comparisons | Selection-proxy prototypes matched explicitly collected inputs |
| Unsupported nesting backends | `backend-refusals.R`: five input shapes × both verbs | SQLite/Arrow nesting refused before the traced SQL read, and an Arrow reader remained consumable |
| Cross-backend inspection | `inspection.R`: 12 selections × six input forms = 72 cases | Plans matched local results, or SQLite gave the documented predicate refusal |
| Grouping identity | `grouping-identity.R`: 80 generated plans × six input forms = 480 results | Permuted and fixed columns, default Grouping identifiers, and Grouping bits matched independent masks and inspection |

The six input forms were local, dtplyr, SQLite, DuckDB, Arrow Table, and Arrow
RecordBatch. The sf matrix varied active geometry names (including pronouns,
`.rows`, `data`, and an internal-name prefix), multiple geometry columns and
active-column switching, geometry as fixed keys or variable/composite
dimensions, empty/populated inputs, `.keep`, every sort choice, and a nesting
list-column name equal to a source geometry column.

### Excluded leads and scope limits

- Dot-dot source names failed at the ordinary input selection boundary. They
  were distinct from F1, where an ordinary source acquired the name inside the
  Margin operation.
- Raw and complex dtplyr payloads failed in ordinary dtplyr set unions too.
  Prior dated investigations had recorded that external limitation. No new
  package defect was inferred from the same `funion()` refusal.
- Eight of the twelve differing type comparisons came from matrix/packed
  source conversion; four involved ordering `NA` and `NaN` fixed keys. Margin
  order did not specify an order between those missing-value ties, and no
  source-row membership defect was established from those four comparisons.
- Matrix, packed, array, and record-class differences caused by dtplyr source
  conversion or its own zero-row operation were checked against those upstream
  controls before exclusion.
- Malformed manually forged specification arguments reached internal errors,
  but the constructors could not produce those objects. ADR 0015's
  documented-interface boundary excluded that observation from tickets.
- Forty-four randomized calls whose outer rowwise identifier was `.rows` were
  excluded by the documented refusal. The sf comparison retained matching
  Package conditions for geometry dimensions whose typed-missing label was
  unrepresentable; it did not count those calls as successful nesting.
- SQL and Arrow nesting were outside the documented nesting backend set.
  Inspection and identity checks on those backends did not establish nesting
  support. No live PostgreSQL, cloud backend, alternate R version, or alternate
  operating system was tested.

## Replay and verification

Run from the repository root. These scripts load the working tree; source
behavior on a later commit may differ. The scripts and captured outputs live
beside this note. Trailing spaces were removed from captured text outputs
for repository whitespace checks. `MANIFEST.sha256` identifies the preserved evidence.

```sh
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/reproduce.R
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/reproduce.R --assert
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/identifier-matrix.R
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/plan-algebra.R .
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/specification-algebra.R
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/capture-boundaries.R
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/binding-context.R
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/local-types.R
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/local-names.R
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/local-membership.R
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/local-sf.R
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/inspection.R .
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/grouping-identity.R .
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/output-names.R .
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/backend-names.R
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/backend-types.R
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/backend-metadata.R
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/backend-refusals.R
Rscript investigation/2026-10-29-nesting-grouping-bug-hunt/unsupported-payloads.R
```

`reproduce.R --assert` was intentionally red (exit one) at the investigated
snapshot; ordinary observation mode exited zero after reporting the failures.
The remaining PASS outputs denoted their stated comparisons, not a claim that
the expected-red identifier defect was fixed.

Existing tests were run with `testthat::test_local()` and the summary reporter:

```r
pkgload::load_all()
testthat::test_local(
  filter = "grouping|nest", stop_on_failure = FALSE, reporter = "summary"
)
testthat::test_local(
  filter = "selection-arithmetic|selection-helper-arguments|selection-operators",
  stop_on_failure = FALSE, reporter = "summary"
)
```

Both executions exited zero: 18 nesting/grouping test files and three further
selection test files. No package-test failure or snapshot content change was
observed. The first restricted invocation stopped when testthat's snapshot
writer could not open an unchanged snapshot in the new worktree; the complete
rerun with worktree access passed. That filesystem failure was environmental.

The investigation-only files were excluded from the source package by the
existing `.Rbuildignore` rule for `investigation/`. Under
`design/agents/local-checks.md`, this was a Repository-only change; the
Package-affecting Review-ready check was not required. Package-aware lintr,
Jarl, the context-budget verifier, and the documentation-reference verifier
were run before publication; their captured outcomes accompanied the evidence.
CI monitoring was outside the requested work.

## Environment

The probes used macOS arm64 (Darwin 25.6.0), R 4.6.1, dplyr 1.2.1, tidyr
1.3.2, rlang 1.3.0, tidyselect 1.2.1, dtplyr 1.3.3, data.table 1.18.6.1,
SQLite via RSQLite 3.53.3, DuckDB 1.5.5, Arrow 25.0.1, and sf 1.1.2.
`environment.txt` recorded the remaining direct tool versions. Arrow emitted
sandbox `sysctlbyname` cache-size diagnostics during some runs; the recorded
identity probes nevertheless completed successfully. Those diagnostics were
not package findings.
