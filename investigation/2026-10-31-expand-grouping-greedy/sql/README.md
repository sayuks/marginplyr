# SQL composition and independent local replays

Investigated: 2026-10-31
Baseline: `339315c387f31deace77566f5d3294b15b323865` (`main`)

## SQLite Boolean-word composition finding

`boolean-composition.R` established a new derived-query boundary beyond the
direct collection/materialization repair in closed #826. A one-row SQLite
source held `g = "a"` and integer payload `TRUE = 7`. Default expansion of
`rollup(g)` collected both payload copies under the correct public name.
Collecting `head(result, 1)` or `filter(result, TRUE)` instead raised
`no such column: TRUE`. No public identifier, Margin order, NULL label,
special grouping column, or source-row ambiguity was required.

The ranked hypotheses were companion alias recovery being discarded by a
derived query, an ordinary SQLite/dbplyr limitation, and an identifier/order-only
problem. Direct companion detection was true for the result and false after
`head()`. The emitted derived SQL used the unrecovered Boolean aliases in the
stored ordinary lazy query. The source table and an explicitly computed
expansion both supported `head()` with the public `TRUE` name intact.
Ordinary manual-union controls supported limiting/filtering and retained the
values, but their Boolean-word column was named `column2`; those controls did
not prove public-name preservation. These observations supported the first
hypothesis and excluded a failure requiring identifiers or Margin order.

`boolean-matrix.R` tested 384 downstream cases: four Boolean-word spellings,
four public roles, empty/populated sources, NULL/text labels, three sorts, and
head/identity-filter derivations. Every direct result matched local expansion
in public names, typed values, and row bags. All 384 ordinary union derivations
executed; all 384 package derivations raised the missing Boolean-word column
error. Ordinary-union names were not compared as if they retained the public
spelling. An independent investigator reproduced the minimal failure with
integer payload 19 and both absent/present identifiers in the adjacent lazy
track.

The expected behavior required usable ordinary queries over the public schema,
not Margin order or a direct-result type guarantee on a derived table. ADR
0031 explicitly delegated those tables to ordinary dbplyr semantics; ADR 0020
excluded implicit materialization as a construction-time workaround.

## Re-expansion and materialization differential

`composition.R` retained 912 classified observations over SQLite/DuckDB,
seven name sets, empty/populated inputs, three sorts, nine derivations, and
collection plus selected computation paths. It expanded a source, derived
another query, collected that query as the source control, then expanded it
again against local expansion of that collected control.

There were 752 matching public schemas and character-normalized row bags,
18 expansion/materialization errors, and 142 failures before the second
expansion. Six of the 18 errors were ordinary prohibited dot-dot-name
limitations; twelve were the documented sorted SQLite compute refusal when
all rowid aliases were shadowed. The earlier seven DuckDB window differences
disappeared after the oracle specified occurrence plus source-row ordering,
eliminating ties between expanded copies. The final retained run contained
no value difference. This oracle did not prove physical SQL types or complete
Margin order.

`composition-controls.R` distinguished Boolean derived-query failures from
other source failures. Ordinary SQLite ordered unions reproduced the invalid
ORDER BY placement. Ordinary select/rename over the `.env` name reproduced
the pronoun translation refusal. Ordinary joining of Boolean-word union
columns also failed. The matrix's before-expansion failures were therefore
not all classified as backend-only: some supplied the new Boolean finding,
which the separate focused matrix established without those confounders.

## Other controls

`index-shapes.R` compared ten malformed or unusual index requests across
ordinary DuckDB computation and the package's `.env` expansion method, for
both index kinds: 40 executions. Ordinary controls omitted `.env`, because
ordinary dbplyr could not materialize that spelling. Null/empty/nested index
requests and duplicate unique-index values failed after destination replacement
in both paths. Missing columns were rejected before writes in both paths.
These were excluded as a new package-only validation finding; no external
database or caller table was used.

`independent-local.R` independently reproduced both local-track findings with
a three-distance fixture and the minimal summary payload. Its ordinary
controls and included-only calls passed, and both expected-behavior predicates
were false. The callback and prototype diagnoses belonged to the local track.

## Replay

Run from the repository root:

```sh
Rscript investigation/2026-10-31-expand-grouping-greedy/sql/boolean-composition.R
Rscript investigation/2026-10-31-expand-grouping-greedy/sql/boolean-composition.R --assert
Rscript investigation/2026-10-31-expand-grouping-greedy/sql/boolean-matrix.R
Rscript investigation/2026-10-31-expand-grouping-greedy/sql/independent-local.R --assert
```

Observation mode required successful controls and exited zero. Both assertion
commands required the intended behavior and exited 1 on the pinned baseline.
The differential and matrix scripts wrote their CSVs under `/private/tmp`;
the final outputs were copied into this directory. Their exit zero recorded
completed measurements, not repaired package behavior.

The environment used R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0, DuckDB 1.5.5,
RSQLite 3.53.3, rlang 1.3.0, and vctrs 0.7.3 on macOS arm64. SQL fixtures
were private in-memory databases and disposable tables. No live PostgreSQL,
SQL Server, hosted service, dependency-version matrix, or general physical-type
proof was included in this track.
