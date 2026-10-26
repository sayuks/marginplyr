# Nesting and Grouping specification bug hunt

Investigated: 2026-10-26

## Snapshot and outcome

This audit examined main at
[`6b91254016dbcce60cc2cc69071f01f69eb8084e`](https://github.com/sayuks/marginplyr/commit/6b91254016dbcce60cc2cc69071f01f69eb8084e)
in a new worktree on `investigate/nest-grouping-bug-hunt`. The requested scope
was `nest_with_margins()`, `nest_by_with_margins()`, Grouping specification
constructors, compilation and inspection, and the associated Grouping identity
helpers. No package implementation, package tests, generated documentation, or
public contract was changed by the audit.

One confirmed defect was published as
[#857 — Preserve .rows outer columns through the nesting fold](https://github.com/sayuks/marginplyr/issues/857),
labelled `bug` and `ready-for-agent`. It had no blockers. Identifier, fixed-key,
and dimension reproductions were grouped into one ticket because they reached
the same fold and could be repaired and verified together. The user delegated
ticket granularity and publication, so the `to-tickets` approval interview was
not needed. No new defect was confirmed in the Grouping specification algebra,
nested-specification grammar, or backend query boundaries exercised below.

This is dated evidence, not a new authority for supported behavior. The
contracts read were `CONTEXT.md`, the public nesting and inspection help, the
*Lifecycle*, *Grouping plan*, and *Answering a review* sections of
`design/architecture.md`, and ADRs 0002, 0005, 0008, 0009, 0013, 0015, 0016,
0018–0020, 0026, 0029, 0032, and 0033. The relevant existing tests and recent
closed issues were also checked to distinguish new defects from intentional
refusals and previously repaired cases.

## Confirmed defect: an outer `.rows` column disappears in local nesting

The nesting help promised no reserved input name for bookkeeping and accepted
a nonmissing, nonempty `.id` name when it did not collide with public columns.
The following valid local call failed:

```r
pkgload::load_all(".", quiet = TRUE)
x <- tibble::tibble(g = c("a", "b"), value = 1:2)
nest_with_margins(x, .grouping = rollup(g), .id = ".rows")
```

Observed diagnostic:

```text
In argument: `dplyr::all_of(unname(identity_cols))`.
Caused by error in `dplyr::all_of()`:
! Can't subset elements that don't exist.
Element `..marginplyr_nest_1` doesn't exist.
```

The safely named control, `.id = "set"`, returned three outer rows with `g`
values `a`, `b`, `Total`, integer identifiers `1L, 1L, 2L`, and nested cell
sizes 1, 1, 2. Renaming its finished identifier column to `.rows` also worked.
The defect therefore preceded result presentation; an ordinary ungrouped data
frame could represent the requested result.

An even smaller reproduction needed no Grouping specification:

```r
nest_with_margins(tibble::tibble(value = 1:2), .id = ".rows")
```

It failed because the finalizer could not select `.rows`. The expected result
had one outer row, identifier `1L`, and a two-row nested cell. For input with a
column named `.rows`, selecting it as `.by` or using `rollup(.rows)` reproduced
the loss as well.

### Mechanism and boundary

`nest_expanded_margins()` grouped its expanded input by the public outer
columns before removing identity columns and finalizing the result. In the
local `dplyr::summarize(.by = ...)` path, the `.rows` grouping key disappeared
when dplyr assembled its grouping-key tibble. A direct control confirmed the
loss without marginplyr:

```r
y <- tibble::tibble(g = c("a", "b"), value = 1:2)
names(y)[1L] <- ".rows"
names(dplyr::summarize(y, data = list(value), .by = .rows))
# [1] "data"
```

The later identity-column removal or final column placement assumed the
original outer schema remained intact. The resulting error exposed a private
column name or reported the missing public identifier instead of fulfilling
the ungrouped nesting contract. The dtplyr ungrouped route succeeded with the
same names and values.

The rowwise counterpart had a separate representational boundary. Both
`dplyr::nest_by(y, .rows)` and `dplyr::rowwise(y, .rows)` failed in dplyr's own
grouping metadata; the latter reported that `group_data` must be a tibble
without a `.rows` column. A repair of the shared fold would not, by itself,
establish support for that rowwise identifier. Ticket #857 asks for the
unsupported rowwise boundary to be explicit and actionable while preserving
the valid ungrouped result.

### Observed name-role matrix

The saved `rows-boundaries.R` covered both `.keep` values, 0/2 source rows,
and payload-present/payload-free inputs: eight calls per cell below.

| Role of `.rows` | Local `nest_with_margins()` | Local `nest_by_with_margins()` | dtplyr `nest_with_margins()` | dtplyr `nest_by_with_margins()` |
| --- | --- | --- | --- | --- |
| Requested `.id` | failed | failed | succeeded | failed at rowwise boundary |
| Fixed `.by` key | failed | failed | succeeded | failed at rowwise boundary |
| Grouping dimension | failed | failed | succeeded | failed at rowwise boundary |
| `.key` list-column name | succeeded | succeeded | succeeded | succeeded |
| Payload column | succeeded | succeeded | succeeded | succeeded |

The matrix made 160 calls. The two successful roles were controls that a fix
must preserve; a blanket prohibition on the spelling would reject working
calls. The no-specification identifier reproduction was measured separately.

## Search method and negative results

Three independent tracks examined local nesting, Grouping specifications, and
backend behavior. Exploration used public calls and independent oracles rather
than treating test coverage as proof of correctness. Root-agent checks replayed
the preserved harnesses from the worktree. The harnesses are dated evidence
under `investigation/2026-10-26-nesting-grouping-bug-hunt/`; they do not run as
part of the package suite.

| Preserved harness | Coverage and oracle | Observed result |
| --- | --- | --- |
| `grouping-algebra.R` | Seed `20261026`; 1,500 random ASTs, five columns, nesting depth at most three; independently expanded sets, unions, hierarchical prefixes, subsets, and Cartesian products; `keep` and `drop` each inspected | 3,000 inspections matched membership, plan order, and retained-occurrence renumbering |
| `grouping-grammar.R` | Five parent constructors × five nested constructors × colliding/noncolliding name × 11 spellings, including parentheses, qualified calls, and injected objects | All 550 grammar and ambiguity outcomes matched |
| `local-membership.R` | Seed `61`; 560 random configurations, 0–8 rows, fixed keys, `.keep`, arbitrary sets/rollup/cube/product; character, factor, NA-level factor, double with NA/NaN, list, matrix, packed data-frame dimensions | All 1,422 grouping-set membership comparisons matched `vctrs::vec_group_loc()`; list payload values were preserved |
| `backend-differential.R` | Seed `6351`; nine dimension types × nine types × `.keep` × empty/nonempty; fixed-key missing values, NA-level factor payload, cube, `.id`, `.sort = "last"` | All 324 local/dtplyr comparisons matched outer values and inner row multisets |
| `backend-contracts.R` | Live SQLite/DuckDB query counters, Arrow reader graphs, mutable dtplyr graphs, and source-expression counters | All boundary assertions passed |
| `rows-boundaries.R` | Five `.rows` roles × both verbs × local/dtplyr × `.keep` × empty/nonempty × payload present/absent | The 160-call matrix above; every payload and `.key` control passed |
| `reproduce-rows.R` | Safely named control followed by the otherwise identical `.rows` identifier call | Exited 1 at the confirmed defect; the control passed |

The backend differential oracle preserved NA and NaN as distinct values but
added explicit tie keys when comparing row order. Its first version reported
nine differences caused by permuting NA and NaN within their missing-value
slot. ADR 0018 did not order those equal sorting keys relative to each other;
canonicalizing those ties removed the false alarms without changing values.
The retained harness also treats either side raising an error as a failure,
rather than accepting two errors as agreement.

### Additional targeted exploration

Local probes exercised 13 column types with `NULL` and `"Total"` labels and
both `.keep` settings; 29 special names in list-column, identifier, fixed-key,
dimension, and payload roles; and factor NA levels nested inside packed
payloads and keys. A further local membership sweep combined eight structured
and scalar dimension shapes across both verbs and all three `.sort` options.
Those probes confirmed no additional package defect. Names `...` and `..1`
were excluded as findings after ordinary tidyselect/tidyr controls rejected
the same source names. Matrix and packed dimensions with nonmissing display
labels were refused as documented.

Grouping identity probes covered 0, 1, 2, 30, 31, 32, and 33 dimensions,
explicit reversed argument order, and fixed keys at different positions. The
31-bit maximum was `2147483647`; inspection returned an `NA_integer_` mask
beyond 31 dimensions while `grouping_id()` refused more than 31 requested
columns. Identity values agreed across local, SQLite, dtplyr, DuckDB native
and portable paths, and Arrow. Twelve capture/evaluation/injection patterns
and 20 empty-input backend/fixed-key/duplicate-policy combinations passed.
These additional targeted scripts were exploratory scratch artifacts, rather
than part of the preserved replay set.

The backend boundary harness established the following:

- Both nesting verbs refused live SQLite and DuckDB inputs before any observed
  `DBI::dbSendQuery()` call. SQLite name-only inspection sent none; its typed
  predicate refusal sent none. DuckDB's name-only and typed inspections each
  sent one query, and both statements contained `LIMIT 0`.
- Inspection of an Arrow reader directly, wrapped in a query, or on the right
  side of a join or union left its complete original rows available afterward.
  Both nesting verbs refused those graphs without consuming the reader.
- Typed inspection of mutable dtplyr root/filter/select/join/union graphs left
  both caller data tables unchanged. Both nesting verbs refused them.
- A dtplyr row predicate ran zero times during construction and twice only
  after explicit collection of a two-set result. An unsafe value-derived
  schema was refused by inspection and nesting with zero source side effects.

Arithmetic-overwrite metadata probes (`x / 2`, `x + 0.5`, `x * 0.5`, `x / 0`,
`x + x`, and `x + 1L`) agreed between the zero-row proxy and collected dtplyr
input. The proposed stale-type defect was not reproduced. Raw and complex
payload refusals in multi-set dtplyr unions were attributed to the upstream
set-operation limitation and retained as External conditions, rather than
filed as a second package defect. A shadowed bare `everything()` hypothesis
was also rejected: tidyselect itself supplied its standard helper in that
selection context, so the ordinary dplyr control had the same selection.

## Reproduction and validation

From the audited repository root, each preserved harness can be run with
`Rscript`, for example:

```sh
Rscript investigation/2026-10-26-nesting-grouping-bug-hunt/grouping-algebra.R
Rscript investigation/2026-10-26-nesting-grouping-bug-hunt/local-membership.R
Rscript investigation/2026-10-26-nesting-grouping-bug-hunt/backend-differential.R
Rscript investigation/2026-10-26-nesting-grouping-bug-hunt/reproduce-rows.R
```

All except `reproduce-rows.R` exited 0 on the audited snapshot. That script
intentionally exited 1 after its safely named control succeeded and the
`.rows` call failed; it compares the actual result with the control if the call
succeeds. The other scripts can print intermediate diagnostic output; their
final assertions determine success. The scripts accept a repository root as
an optional first argument and require the relevant installed packages.

The focused existing suite passed with this command:

```r
testthat::test_local(
  ".",
  filter = paste(
    "nest-operation|grouping-plan|inspect-grouping|selection-helper-arguments",
    "grouping-interface|cyclic-grouping|dtplyr-special-nesting",
    sep = "|"
  ),
  reporter = "summary"
)
```

The audit commit contained only investigation material, excluded from the
source package by the existing `.Rbuildignore`. It was Repository-only under
`design/agents/local-checks.md`; a full Review-ready check was not required.
Before publication, `jarl check .`, package-aware `lintr::lint_package()` after
`pkgload::load_all()`, and the context-budget and document-reference verifiers
were run. Their results are recorded in the pull request.

## Environment and limits

The replay environment measured R 4.6.1 (2026-06-24), dplyr 1.2.1, tibble
3.3.1, vctrs 0.7.3, rlang 1.3.0, tidyselect 1.2.1, data.table 1.18.6.1,
dtplyr 1.3.3, dbplyr 2.6.0, Arrow 25.0.1, RSQLite 3.53.3, DuckDB 1.5.5,
DBI 1.3.0, pkgload 1.5.3, and testthat 3.3.2 on macOS.

This was a finite bug hunt, not proof that every input was correct. It did not
exercise a live PostgreSQL server, other operating systems, older dependency
versions, unlimited Grouping specification depth or dimension counts, or large
performance stress cases. SQL refusal probes covered two live dialects, and
the query counter observed the stated DBI entry point rather than proving the
absence of every possible backend execution mechanism. No CI monitoring was
performed.
