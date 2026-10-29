# Independent SQL signed/whitespace output validation

Investigated: 2026-10-29
Baseline: `4c63bb917ed7ed31ff6aadf060a5eae7a32cd004`
Issue: [#1044](https://github.com/sayuks/marginplyr/issues/1044)

This independent replay confirmed one remaining generated-reference defect.
No package implementation or package test was changed. The scripts in this
folder were investigation harnesses, not additions to the package suite.

## Public symptom and expected behavior

With input `g = c("a", "b")`, `v = c(1, 3)`, an ordinary grouped dplyr sum
accepted the names `..+0`, `..+1`, and `.. 1`. Both live SQLite and DuckDB
collected and materialized those ordinary summaries with the original names
and values one and three. A Margin summary over `rollup(g)` failed while
rendering generated SQL, before the caller could collect the result.

The first name reported `indexing '...' with non-positive index 0`; the
other two reported `..1 used in an incorrect context, no ... to look in`.
A successful rollup would have retained each public name and returned the two
detail sums and grand total four. A Total share of that source would have
returned doubles `0.25, 0.75, 1` under last Margin order. A requested identifier
would have retained its spelling and integer Grouping set identifiers.

The same defect affected an ordinary summary output, the source of a Total
share, the output of a Total share, and a requested Grouping set identifier.
The separate Parent-share probe also failed for all three spellings on SQLite,
native DuckDB, and portable DuckDB; its protected `..-1` control returned the
expected Parent shares on every path.

## Coverage and measured result

The main matrix crossed five names, four roles, three Margin-order choices,
three public Grouping specifications, two input contents, and three SQL paths:

- Names: `..+0`, `..+1`, `.. 1`; protected controls `..-1`, `.env`.
- Roles: summary output, Total-share source, Total-share output, identifier.
- Margin order: `none`, `first`, `last`.
- Specifications: `NULL`, `grouping_set()`, `rollup(g)`.
- Contents: the two populated rows and an empty typed input.
- Paths: SQLite, native DuckDB, portable DuckDB. A requested identifier with
  `.duplicates = "keep"` selected the portable DuckDB path; the separate
  adapter probe verified `UNION ALL` versus `GROUPING SETS` SQL.

Of 1,080 public cases, 612 failed, all at SQL rendering. The 648 cases for the
three investigated names included 612 failures and 36 successes. Each target
name failed 204 of its 216 cases. SQLite and portable DuckDB failed every
investigated-name case; native DuckDB succeeded only for a share output in
`NULL` and single-empty-set plans. These exceptions included both input contents
and all three Margin-order choices.

| Slice among investigated names | Failed | Passed |
| --- | ---: | ---: |
| SQLite | 216 | 0 |
| Native DuckDB | 180 | 36 |
| Portable DuckDB | 216 | 0 |
| Rollup plans | 216 | 0 |
| `NULL` plans | 198 | 18 |
| `grouping_set()` plans | 198 | 18 |
| Summary-output role | 162 | 0 |
| Share-source role | 162 | 0 |
| Share-output role | 126 | 36 |
| Identifier role | 162 | 0 |

All 432 protected-name cases passed collection and direct materialization,
including public column order, values, and requested Margin order. The matrix's
93 ordinary-output and explicit lexical `.env$scale` controls passed. The scalar
probe independently confirmed ordinary collection and materialization of all
three investigated names on both database connections. Ordinary `.env`
materialization was excluded from that separate upstream control because
ordinary dbplyr has its own pronoun-name restriction there; the Margin `.env`
materialization cases remained covered and passed.

The expected result was assembled from ordinary backend summaries for each
grain, rather than assuming that an empty SQL `SUM` had the local R empty-sum
value. The empty grouped-output controls asserted zero rows and exact names,
without inventing a type guarantee for an ordinary empty aggregate column.

## Supported cause and relationship to earlier findings

`sql_name_needs_quoting()` in the investigated snapshot recognized `.env` and
an optional minus sign followed by digits after `..`. It returned false for
all three investigated names. The scalar probe showed the same R diagnostic
when dbplyr translated a bare symbol carrying each name, and successful quoted
SQL when it translated `dbplyr::ident()` carrying the identical name. Thus the
accepted output alias and aggregate were not the failure: a generated read
crossed the SQL translation boundary with an R dot-argument meaning.

The successful `..-1` matrix control distinguished this residual SQL path from
the signed-negative SQL repair in closed #971. Closed #1002 covered generated
local and dtplyr reads for signed/whitespace names. Its broader local boundary
did not establish the SQL generated-reference behavior measured here. Full
bodies and comments for both issues were read on the investigation date.

The evidence supported one repair ticket for the shared SQL reference boundary,
not separate tickets for each spelling or each downstream role. Input-key names
that ordinary lazy grouping refused, including ordinary `...`/dot-argument
restrictions, were not positive fixtures. No new source-type-eligibility,
Grouping-plan-expansion, or SQL-engine-name restriction was inferred from these
rendering errors.

## Replay and artifacts

Run from the repository root:

```sh
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/backend/reproduce-sql-signed-outputs.R
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/backend/reproduce-sql-signed-outputs.R --assert
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/backend/probe-sql-signed-reference.R
```

The report command exited zero and saved the full matrix. The fast `--assert`
command exercised one SQLite rollup output named `..+1` without a requested
identifier. Its ordinary-output and lexical controls passed, and it exited one
because the desired public behavior failed. That mode did not overwrite the
full-matrix CSVs. The reference probe exited zero while recording the expected
bare-symbol diagnostics and successful controls.

- `sql-signed-output-results.csv`: one row per public matrix case; the stage
  identifies the first failed boundary. A passed case reached direct compute.
- `sql-signed-output-controls.csv`: the 93 ordinary/lexical controls.
- `sql-signed-output-report.log`: matrix counts, representative diagnostics,
  baseline SHA, and package versions.
- `sql-signed-output-assert.log`: the minimized desired-behavior failure.
- `sql-signed-reference-probe.log`: scalar translation, ordinary collect/compute,
  adapter selection, and Parent-share controls.
- `sql-signed-output-coverage.log`: independently summarized CSV coverage.

## Acceptance evidence for a repair

A repair would let all three admitted names survive construction, SQL rendering,
collection, and supported direct materialization in all four measured roles.
It would retain names, column order, integer identifiers, double shares, and the
backend's documented empty-aggregate behavior. It would preserve explicit
caller lexical reads and existing protected-name behavior, leave genuine
upstream input-name restrictions intact, and preserve ADR 0020's construction
query policy. Public backend regressions and the package-affecting Review-ready
check would establish that repair; this investigation did not implement it.
