# Expansion and Grouping specification bug hunt

Investigated: 2026-10-25
Baseline: `b6f9faba946521aeb92fcf2c5655a2c3bf608ec8` (local `main`)

This investigation exercised `expand_with_margins()`, the five Grouping
specification constructors, `inspect_grouping()`, and Grouping identity helpers.
It added investigation evidence only. Package implementation, package tests,
installed files, and generated documentation were unchanged.

Five new independent repair tickets were published. The type-dependent
selection finding was already covered by #825 when the tracker was rechecked;
two additional name-boundary observations were covered by #826 and #827.
None of the five new tickets required another ticket to start. Related completed
work supplied context rather than blocking dependencies.

## Environment and method

The measured environment was macOS arm64, R 4.6.1, marginplyr 0.1.0 loaded from
the checkout, dplyr 1.2.1, dbplyr 2.6.0, rlang 1.3.0, tidyselect 1.2.1, vctrs
0.7.3, tibble 3.3.1, dtplyr 1.3.3, data.table 1.18.6.1, RSQLite/SQLite 3.53.3,
DuckDB 1.5.5, and Arrow 25.0.1. No live PostgreSQL server or external warehouse
was exercised. Simulated SQL was supplementary; the SQL findings below were
reproduced against live in-process databases.

The hunt combined independently generated grouping-set families, direct
row-copy and Margin-order oracles, local/lazy differential runs, typed schema
and special-name matrices, and minimized public-call reproductions. Ordinary
R, dplyr, backend operations, and explicitly materialized inputs supplied
controls. Hypotheses were separated by changing the label, grouping-set order,
backend, or one input boundary at a time. No production instrumentation or fix
was introduced.

Contract sources included `CONTEXT.md`, the public Grouping specification,
inspection, expansion and label references, and ADRs 0002, 0005, 0008, 0009,
0012, 0015, 0016, 0018, 0019, 0020, 0026, 0031 and 0033. Relevant completed
Issues were read with their comments, particularly #179, #342, #346, #427,
#688 and #769. The note recorded measured behavior; those maintained artifacts
remained authoritative for decisions.

## Findings and tickets

| Finding | Priority | Ticket | Blocking edges |
| --- | --- | --- | --- |
| Type-dependent helper arguments were resolved from the name proxy | P2 | [#825](https://github.com/sayuks/marginplyr/issues/825), existing | None |
| Captured Grouping-helper syntax was silently rewritten | P2 | [#832](https://github.com/sayuks/marginplyr/issues/832), new | None |
| Missing list dimensions failed in dtplyr's generated-expression parser | P2 | [#833](https://github.com/sayuks/marginplyr/issues/833), new | None |
| Higher-dimensional arrays escaped scalar-label validation | P3 | [#834](https://github.com/sayuks/marginplyr/issues/834), new | None |
| SQL input limits escaped an expansion branch | P2 | [#835](https://github.com/sayuks/marginplyr/issues/835), new | None |
| DuckDB scalar missing prototypes were not SQL-translatable | P2 | [#836](https://github.com/sayuks/marginplyr/issues/836), new | None |

### Type-dependent helper arguments

An input with character `label = c("a", "b")` and numeric `amount = c(1, 1)`
was selected through
`all_of(names(Filter(is.numeric, tidyselect::peek_data())))`. Ordinary
tidyselect selected only `amount`, and ordinary dplyr produced one count-two
summary. The same `.by` selection under the Margin summary fixed both columns
and produced two count-one rows. Inspection exposed the extra fixed key.

The character-selection equivalent inside `rollup()` was falsely refused with
`` `rollup()` requires at least one dimension. `` A dtplyr inspection using the
numeric selection inside `grouping_set()` also included both columns.

`is_name_only_expr()` classified the outer leaf helper without accounting for
its argument's type dependence. The name proxy represented every column as an
integer. `resolve_fixed_keys()` retained that answer, while the name-only
dtplyr inspection path also used it for its canonical plan. This explained
both wrong selection and premature rejection. The existing #825 covered this
root cause, so no duplicate ticket was created. The additional dtplyr and
character-selection observations were retained in `grouping-findings.R`.

### Captured Grouping-helper syntax

A rollup summary deparsing `base::bquote(grouping_bit(label))` returned `"0L"`
for detail and `"1L"` for the Grand total set. Deparsing the qualified
`rlang::expr(grouping_id(label))` produced the same substitutions. Ordinary R
and dplyr returned the literal helper syntax; the existing `base::quote()`
control preserved it under the Margin summary too. Independent local and
dtplyr reproductions agreed.

`base::bquote(f(.(grouping_bit(label))))` correctly evaluated its explicit
substitution and returned `"f(0L)"` / `"f(1L)"`. Protecting the entire capture
would therefore have been wrong as well. Executing recovered language was a
separate evaluated path that a repair would need to preserve.

The shared language traversal excluded these captures and actually rewrote
their contents. #179 had deliberately protected only selected primitives; the
new finding did not claim that it had promised arbitrary quasiquotation.
Instead, the measured fallback silently changed a caller's value, exceeding
the conservative false-refusal behavior used to justify that fallback.
#832 required safe handling of statically identifiable capture boundaries,
with explicit refusal for unsupported shapes rather than silent rewriting.

### Omitted dtplyr list dimensions

For a two-row list dimension `list(1L, 2L)`, a rollup expansion with `NULL` or
`NA_character_` raised `Invalid input` from `dt_squash()` during construction.
The condition was not a Package condition. The same local expansion, an
included-only dtplyr plan, a string-labelled dtplyr rollup, and a list payload
all succeeded.

Ordinary lazy mutation assigning `list(NULL)` followed by union returned
`list(1L, 2L, NULL, NULL)` and left the source unchanged. The distinction was
between that language expression and the bare R list prototype injected by
`label_margin_branch()` after `margin_column_info()` had constructed it.
The parser refused the generated object before any lazy result was returned.

This established #833 for an ordinary list-column operation the backend could
already express. It did not rely on extending ADR 0016's local packed-column
support to a new lazy backend.

### Higher-dimensional array labels

Ordinary dplyr grouped a two-row, 2-by-2-by-2 array dimension. Missing-labelled
expansion retained a 4-by-2-by-2 array. With a string label, expansion instead
failed in a generated `as.character()` mutation with
`g must be size 2 or 1, not 8`. A zero-row input with the same schema was
accepted and converted to character. Disabling observed collision checks did
not change the populated-input failure.

The prototype path preserved shape for matrices and packed data frames but
used scalar indexing for higher-dimensional arrays. The scalar-label guard
then tested only for matrices on those prototypes. The generated conversion
flattened the array instead of providing one value per source row.

#834 requested the existing actionable scalar-label refusal at that boundary,
consistently for empty and populated inputs. It introduced no array display
encoding. #688 had addressed fixed-key ordering and explicitly left omitted
array display labels outside its scope. Fixed-key and typed-missing controls
were therefore retained separately.

### SQL input LIMIT boundaries

The source `(g, v) = (c, 1), (b, 2), (a, 3)` was ordered by unique `v` and
limited to two rows. Expansion across the empty set followed by the dimension
set should have copied `(c, 1), (b, 2)` twice, producing four rows. With a
missing label, SQLite without `.id` and DuckDB with `.id = "set"` instead
returned two rows: both copies of the first input row. Both copies of the
second row were lost. The explicitly ordered input made this deterministic.

Changing the occurrence order to `rollup(g)` produced syntax errors instead:
SQLite reported `ORDER BY clause should come after UNION ALL not before`;
DuckDB reported a parser error near `UNION`. Explicitly computing the selected
input first produced all four expected rows.

The unchanged detail branch exposed the input's trailing `ORDER BY ... LIMIT`
at the compound-query boundary. In the empty-first case the limit truncated
the union as a whole; in the other order it occupied an illegal operand
position. The existing `sqlite_margin_input()` wrapper did not keep an
unchanged branch nested. #835 grouped the invalid syntax and silent truncation
as one boundary repair spanning SQLite and DuckDB.

Ordinary dbplyr unions also exposed the underlying compound-query restriction.
That did not establish a safe result for the accepted Margin expansion, whose
cardinality contract was violated. Completed #769 had repaired related SQLite
construction failures; these shapes demonstrated an incomplete boundary after
that repair. They did not establish that its covered summary cases regressed.
Any repair still needed to preserve laziness and the query policy in ADR 0020.

### DuckDB scalar missing values

Single-row TIME and INTERVAL dimensions collected as `difftime`; their
missing-labelled expansion failed with
`Cannot translate a <difftime> object to SQL`. A scalar BLOB collected as a
list of raw vectors and failed with `Unknown input type: list`. The broader
physical-type sweep also observed the temporal error for TIMETZ.

For TIME, INTERVAL and BLOB, the source collected, inspection returned the
expected plan, ordinary union with a SQL-NULL mutation returned the expected
two rows, and a native Margin summary succeeded. Text-labelled expansion
also succeeded. The package-created omitted R prototype, rather than the
caller's expression or SQL type, was what failed translation.

`margin_column_info()` took a missing R element from the zero-row proxy and
`label_margin_branch()` injected it into a generated mutation. #836 covered
backend-compatible missing scalar generation for the temporal and binary
cases. SQL LIST/MAP/STRUCT exploratory failures were not used to add a remote
structured-key promise. #833 had a related prototype symptom on dtplyr, but
its parser and list-expression repair could be verified independently, so the
tickets had no blocking edge.

## Replaying the evidence

All companion scripts were saved under
`investigation/2026-10-25-expand-grouping-bug-hunt/` and loaded the checkout
from the repository root. Optional backends used by a script needed to be
installed. The grouping reproduction's optional dtplyr branch used the
repository's version-aware Suggest guard.

```sh
Rscript investigation/2026-10-25-expand-grouping-bug-hunt/grouping-findings.R
Rscript investigation/2026-10-25-expand-grouping-bug-hunt/local-findings.R
Rscript investigation/2026-10-25-expand-grouping-bug-hunt/sql-limit.R
Rscript investigation/2026-10-25-expand-grouping-bug-hunt/duckdb-scalars.R
```

These four runners printed desired-contract failures while asserting their
successful controls. Their `--assert` mode required the desired behavior and
exited 1 on the baseline: six failed checks in the grouping runner, four in
the local runner, four SQL boundary failures, and three DuckDB scalar failures.
Those exits were expected evidence, not passing package tests. A refusal
allowed by #832 could require adjusting the preferred-preservation check; the
runner did not prove arbitrary quotation support.

The finite model-based and differential campaigns were:

| Companion script | Measured scope and result |
| --- | --- |
| `grouping-oracle.R` | Seed 20261025, 1,000 trees up to constructor depth three, all five constructors and three duplicate policies: 3,000 plan comparisons agreed on membership, order, fixed keys and both identifier forms. |
| `expansion-oracle.R` | 100 random inputs, arbitrary explicit sets, keep/drop and all sort modes, local/data.table/dtplyr: 1,800 row-copy and Margin-order comparisons passed. |
| `local-names.R` | 32 names, dimension/fixed/payload/id roles, three sort modes: 384 local/dtplyr comparisons found no admission asymmetry or value discrepancy. Shared restrictions were not proof of validity. |
| `factor-matrix.R` | 216 comparisons covered ordered factors, NA levels and missing codes together, unused/sentinel levels, empty inputs, label positions, sorting and column roles; all agreed. |
| `lazy-matrix.R` | 1,440 cases across normal/empty/factor/temporal shapes, three backends and direct/derived inputs. Differences were reviewed against backend type and ordering contracts. |
| `lazy-names.R` | 528 cases across 22 names and three backends; the 48 name-repair failures involved dot-dot names. Another temporary reserved-name sweep passed 60 DuckDB/Arrow cases. |
| `lazy-derived.R` | 840 cases covered ordered limits, physical order, groups, distinct, summaries, union, joins, rename and window filtering; minimized SQL boundary findings above. |
| `arrow-physical.R` | 240 cases covered 15 physical types and Table/empty-query/derived-query/Dataset shapes; 19 structured-cast/list-sort refusals were reviewed as unsupported backend operations. Null, temporal, dictionary and empty scalar cases passed. |
| `sqlite-names.R` | 468 comparisons varied a dimension name, id name and sort mode; 72 failed on Boolean-word aliases and corresponded to the already tracked SQLite interaction. |
| `dtplyr-metadata.R` | 32 derived arithmetic/copy/constant candidates; 30 admitted inspections selected integer columns consistently with collected input metadata. |

The exploratory lazy matrices deliberately printed raw differences and saved
scratch results under `/tmp`; they were not all-green regression tests.
In particular, Arrow's documented factor-to-character behavior, backend
numeric/date-to-text rendering, and payload ordering within Margin-key ties
were not classified as bugs. The independently asserted oracles covered a
finite grammar and ordinary scalar keys, not all R expressions or SQL types.

## Tracked observations and excluded leads

`tracked-names.R` independently reproduced accepted `.id` values `...` and
`..1` failing after ordinary local mutation had accepted them. #827 already
covered that identifier boundary. It also reproduced SQLite expansion ids
`TRUE` and `FALSE` failing despite successful top-level dplyr mutation;
#826 already covered the generated Boolean-alias interaction. Raw SQL
`SELECT` from a compound subquery with those aliases failed too, locating
an upstream component. That observation alone was not treated as proof of
another package root cause. Neither existing ticket was duplicated.

Additional probes checked the 31-bit Grouping identifier limit, the documented
32-column refusal, fixed-key Grouping bits, and injected bare-column quosures
with unrelated environments; they agreed with the contracts. Named helper
arguments were excluded because no applicable prohibition was established.

Complex/raw dtplyr union failures, list-key sorting failures and reordered
set-operand failures reproduced in ordinary data.table/dtplyr. Zero-column
row-count loss already occurred during input conversion, before marginplyr.
Arrow structured-to-text and list-order refusals also occurred in ordinary
backend operations. An unordered SQL limit selecting different rows after
later ordering was excluded because the ordinary dplyr control behaved alike;
the confirmed ordered-limit finding did not depend on that ambiguity.

## Local verification

The focused existing package tests for expansion, Grouping plan/interface,
inspection, matrices and packed columns passed at the baseline. The ordinary
and `--assert` reproduction modes and the independently asserted plan,
expansion and factor oracles were replayed from the saved companions. The
expected failing assertions were kept outside the package test suite.

Both repository linters were run before publication, including package-aware
lintr after `pkgload::load_all()`. The context-budget and document-reference
verifiers were run. Results were recorded on the pull request.

The diff contained only investigation notes and companion probes excluded
from the source package. It did not drive generation or change package-check
tooling, so the Package-affecting Review-ready boundary in
`design/agents/local-checks.md` did not apply. No CI monitoring was performed.
