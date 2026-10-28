# Nesting and Grouping specification bug hunt

Investigated: 2026-10-28
Snapshot: main `ac6ad79ff6026abe5fc45538d6dfefadf03a8ca3`

## Outcome and scope

The audit started from [this main commit](https://github.com/sayuks/marginplyr/commit/ac6ad79ff6026abe5fc45538d6dfefadf03a8ca3)
in a new worktree on `codex/nest-grouping-bug-hunt`. It examined
`nest_with_margins()`, `nest_by_with_margins()`, the five Grouping specification
constructors, compilation, `inspect_grouping()`, and associated Grouping identity
helpers. Local, dtplyr, Arrow, DuckDB, and SQLite were exercised where the
respective public interfaces admitted them.

Two independently reproducible P2 defects became tickets:

| Finding | Ticket | Blocking edges |
| --- | --- | --- |
| Valid dot-dot list-column names failed during nesting cleanup or rowwise conversion | [#950](https://github.com/sayuks/marginplyr/issues/950) | None |
| Temporary column aliases invalidated local sf geometry metadata before nesting | [#951](https://github.com/sayuks/marginplyr/issues/951) | None |

Each ticket covered its public behavior, regression boundaries, and controls as
one independently verifiable slice. Neither depended on the other. Both received
`bug` and `ready-for-agent`. The user delegated granularity and publication, so
the `to-tickets` approval interview was unnecessary. Open issues and relevant
closed tickets were checked before publication; closed #827 concerned summary
outputs and identifiers, #831 named scalar output arguments, and #857 `.rows`
outer keys, rather than either defect above.

No package implementation, tracked package test, generated documentation,
dependency declaration, or public contract was changed. The scripts here were
investigation evidence, excluded from the source package by the existing
`^investigation$` rule. They were not added to the package test suite. Diagnosis
stopped at minimized reproduction and cause isolation; implementing a fix and
making its regression test green were outside the requested scope.

The contracts read were `CONTEXT.md`, the nesting/inspection/Grouping identity
help, the relevant parts of `design/architecture.md`, and ADRs 0002, 0005,
0007–0009, 0013, 0015, 0016, 0018–0020, 0026, 0029, 0032, and 0033. Those
artifacts owned decisions; this note recorded observations at the pinned commit.

## F1: output-name support depended on internal cleanup

The smallest failing local call was:

```r
x <- tibble::tibble(g = c("a", "b"), value = 1:2)
nest_with_margins(x, .by = g, .key = "..1")
```

It raised `vctrs_error_names_cannot_be_dot_dot`:

```text
Names can't be of the form `...` or `..j`.
```

The blamed operation was the private set-column removal in
`nest_expanded_margins()`. Adding `.id = "set"` to the very same public call
succeeded with names `g`, `set`, `..1` and two one-row cells. With no grouping,
`.key = "..1"` and `.sort = "last"` also succeeded. These controls ruled out an
intrinsic inability to construct the requested local list column.

A rollup with a public identifier failed instead while removing its private
identity columns. The rowwise counterpart with fixed keys and `.id = "set"`
passed the fold but raised `tibble_error_column_names_cannot_be_dot_dot` in the
final `as_tibble(data)` reached by `dplyr::rowwise()`.

Three hypotheses were distinguished: an invalid public `.key`, the fold's
private-column projections, and final rowwise name validation. The successful
controls rejected the first; the condition calls isolated the latter two.
The fold used selection-based deletion even though `drop_margin_columns()`
already avoided that validation for local public outputs. The final rowwise
alias scope covered outer identifiers but omitted the list-column name.

The `.key` help described a nonempty scalar column-name string without a
dot-dot restriction. Its NULL semantics differed between the verbs; the
observed restriction instead depended on unrelated identifier and sorting
options. Input columns that tidyselect itself refused were excluded from this
finding. No promise to admit those source names was inferred.

[reproduce-key.R](reproduce-key.R) compared both names (`...`, `..1`) against a
safely named control across both verbs, no/fixed/rollup grouping, 0/2 rows,
both `.keep` choices, absent/public `.id`, and all `.sort` values. Of 288 calls,
208 failed and 80 matched the control. Its `--assert` mode exited 1 at the audited snapshot; ordinary evidence mode exited
0. [Captured output](reproduce-key-output.txt) preserved the stage-specific
conditions and successful controls. Ticket #950 held the repair criteria.

## F2: temporary aliases left sf metadata naming a removed column

The minimal populated fixture needed no Grouping specification:

```r
x <- sf::st_as_sf(
  data.frame(g = c("a", "b"), x = 1:2, y = 3:4),
  coords = c("x", "y")
)
nest_with_margins(x)
```

Both nesting verbs failed; `.by = g` and a rollup of `g` also reproduced it.
The smallest calls raised `simpleError` from `select.sf()`:

```text
internal error: can't find sf column
```

On the same source, `tidyr::nest(x, .by = g)` and `dplyr::nest_by(x, g)` returned
two one-row cells. Their empty counterparts also returned their expected
zero-row or ungrouped one-cell result. Converting the source explicitly to a
tibble allowed Margin nesting with the geometry payload retained. These
controls separated input capability from package-induced metadata damage.

The competing explanations were unsupported upstream nesting, broken source
geometry, and a temporary package rename. A direct probe of the branch reorder
isolated the third. `reorder_margin_columns()` selected every local column
under numeric aliases using dplyr, then restored the public names using
`rlang::set_names()`. `select.sf()` correctly changed the active geometry name
to `"2"`; the raw name restoration did not restore that dependent metadata.
The result had names `g`, `geometry` and `sf_column == "2"`, and its geometry
accessor failed. `label_margin_branch()` called that reorder before the fold,
even when no dimension needed a label.

[reproduce-sf.R](reproduce-sf.R) explored 72 combinations of both verbs,
no/fixed/rollup grouping, empty/two-row input, both `.keep` values, and all sort
modes. All failed: 48 with the missing-column diagnostic, 12 with an explicit
geometry-metadata diagnostic, and 12 empty cases with a replacement-row-count
error. The last 12 were not individually cause-isolated; they were not counted
as another defect. The populated minimal case and directly invalid metadata
established F2 independently. Evidence mode exited 0 and `--assert` exited 1
at the audited snapshot. [Captured output](reproduce-sf-output.txt)
recorded the controls and inconsistent metadata.

ADR 0016 did not promise arbitrary subclass or attribute restoration. This
finding required successful nesting from an admitted local data frame and
valid intermediate objects, with source membership and geometry payload. It
did not require an outer sf result or new package dependency. Expansion also
reached sf invariant errors at the shared seam. Summary calls could meet sf's
separate aggregate restrictions, so no blanket guarantee about every summary
expression was inferred. Ticket #951 kept that distinction.

## Coverage and excluded leads

The saved scripts ran from the repository root against the working-tree
namespace. The two reproduction scripts offered `--assert`; the exploratory
sweeps printed observations, including intentional refusals and comparison
candidates that needed the interpretation below.

| Probe | Observed scope and result |
| --- | --- |
| `local-names.R` | 1,800 name-role attempts across fixed keys, dimensions, payloads, `.id`, `.key`, both verbs/keep modes/all sorts. Documented `.rows` rowwise identifiers were skipped. The 96 failures involved dot-dot input or list-column names; F1 separated those roles. |
| `local-types.R` | 1,056 combinations over 22 types and empty/populated inputs. Accepted cell membership matched independent `vctrs::vec_group_loc()` partitions. Forty-eight raw typed-missing refusals were expected. Twelve strict `.keep` type comparisons differed because POSIXlt became POSIXct through delegated branch combination; time values agreed. |
| `local-structured.R` | Partial/full missing packed, matrix, and array keys under both ordered modes, plus 12 zero-width structural cases. Source cardinalities and kept shapes agreed. |
| `dtplyr-names.R` | 667 role/name/key/id comparisons with local controls. No discrepancy among successful local calls. |
| `dtplyr-types.R` | 2,430 type/size/role/keep/order/label comparisons after checking root conversion. Candidates were separated from unsupported backend types and unspecified order. |
| `dtplyr-classed.R` | 864 integer64/hms/units/AsIs-Date comparisons, including both verbs. No discrepancy. |
| `dtplyr-transforms.R` | 242 safe mutate/prototype comparisons. No discrepancy. This inspected the private metadata seam to distinguish proxy fidelity from nesting. |
| `dtplyr-mutation.R` | 96 immutable/mutable root/filter/select/rename/arrange/group/mutate/join/set shapes. Immutable calls succeeded; mutable roots were refused; source tables stayed unchanged. |
| `plan-algebra.R` | Seed 4306; 1,000 generated trees, each under keep/drop, five kinds, depth up to three, composite dimensions, bindings and parentheses, 11 unusual column names. Independent set/order/mask/identifier oracle agreed in all 2,000 comparisons. |
| `plan-boundaries.R` | 0/1/2/30/31/32/64/128 dimensions. Set counts, complete bits, integer-mask boundaries, helper/inspection identity, and the over-31 helper refusal agreed. |
| `backend-inspection.R` | 192 dtplyr/Arrow/DuckDB typed-selection comparisons over eight derived shapes and eight selectors, using explicit collection as the type oracle. Expected selection errors matched; no discrepancy. This was a fidelity check, not a no-read measurement. |
| `backend-identity.R` | Seed 20261028; 220 local/dtplyr/Arrow/DuckDB/SQLite reports over 11 specifications, fixed-key choices, and keep/drop. Inspector IDs matched helper outputs; every set accounted for all source rows. |
| `dtplyr-inspection-copies.R` | 216 typed-inspection comparisons across destination/source column-copy pairs. No discrepancy. |

`classify-dtplyr-types.R` replayed the type-sweep candidates. The 254
successful-but-order-different candidates in the dtplyr type sweep had
identical typed outer-row/cell multisets. They arose from unspecified
`.sort = "none"` order or NA/NaN ties in backend arrange semantics, rather than
lost membership. ADR 0018 delegated such tie ordering to arrange. Complex/raw/
list grouping failures that data.table itself produced were not attributed to
marginplyr; accepted payloads survived. Matrix shapes lost at `lazy_dt()` root
conversion were likewise outside the Margin operation.

Parenthesized selector heads were checked against direct tidyselect evaluation;
the mask semantics agreed, rejecting a suspected names-only classification
hole. A forged specification with non-quosure private arguments could raise an
internal rlang error, but no supported constructor produced that representation;
it was excluded rather than expanding the private representation contract.

No new Grouping specification defect was confirmed. These finite sweeps did not
prove arbitrary S3/S4 support, unbounded expression depth, huge exponential
cubes, concurrency, future dependency versions, or a live PostgreSQL result.
No remote PostgreSQL service was exercised.

## Replay and verification

From the repository root:

```sh
Rscript investigation/2026-10-28-nesting-grouping-bug-hunt/reproduce-key.R
Rscript investigation/2026-10-28-nesting-grouping-bug-hunt/reproduce-key.R --assert
Rscript investigation/2026-10-28-nesting-grouping-bug-hunt/reproduce-sf.R
Rscript investigation/2026-10-28-nesting-grouping-bug-hunt/reproduce-sf.R --assert
Rscript investigation/2026-10-28-nesting-grouping-bug-hunt/plan-algebra.R
Rscript investigation/2026-10-28-nesting-grouping-bug-hunt/backend-identity.R
```

Captured output files had trailing whitespace trimmed.

The two `--assert` commands were intentionally red on the pinned commit. They
asserted the desired public behavior, so a complete repair could make them
green. Other saved sweeps were runnable individually by the same convention;
`dtplyr-common.R` supplied helpers to the type/classed sweeps. Their diagnostic
RDS files went to `/tmp`, rather than into the package or this evidence record.

The existing package tests matching `nest|grouping|inspect` passed. An initial
sandboxed attempt could not rewrite an existing testthat snapshot; the same
command completed after granting access to the new worktree. That filesystem
failure was environmental, not a package finding. Boundary tests matching
`query-policy|margin-order|factor|verb-argument-admission|data-frame-subclass`
were also run. The final verification outcomes were recorded in
[verification.txt](verification.txt).

Both required linters used the repository commands: `jarl check .` and
`LINTR_ERROR_ON_LINT=true Rscript -e 'pkgload::load_all(".", quiet=TRUE);
lintr::lint_package()'`. The context-budget and verifier-invocation checks
passed. This repository-only evidence addition did not trigger the source
package Review-ready boundary in `design/agents/local-checks.md`.

The environment was R 4.6.1, dplyr 1.2.1, tidyr 1.3.2, dtplyr 1.3.3,
data.table 1.18.6.1, vctrs 0.7.3, dbplyr 2.6.0, Arrow 25.0.1, DuckDB 1.5.5,
DBI 1.3.0, RSQLite 3.53.3, sf 1.1.2, and testthat 3.3.2. Extra class probes
used installed bit64, hms, and units. Dependency absence was not treated as a
passing experiment. CI monitoring was outside the requested work.
