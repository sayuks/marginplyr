# Live SQL expansion composition oracle

Investigated: 2026-11-06
Baseline: `952a0d9b18fd3ff4631f153432f6024ad62fcac2`

[composition-oracle.R](composition-oracle.R) completed 2,016 observations:
SQLite and DuckDB, fourteen source representations, three explicit grouping
families, NULL/text labels, three Margin orders, absent/present identifiers,
and default/CTE SQL rendering. The sources were a table, filter, arrangement,
collapse, projection, rename, arithmetic mutation, window mutation, ordered
limit, distinct, union, join, named CTE and an SQL expression. A fixed key,
two dimensions, source missing values and payload row identity were retained.

The oracle enumerated rollup/cube/explicit-union sets independently, copied
the explicitly collected source rows, replaced omitted dimensions, and built
the documented Margin-order key. It compared exact public column order,
row-value multiplicities and the physical sequence of visible order keys.
Payload order within identical keys was not promised. It did not derive its
expected result from another Margin operation or inspection.

All 1,008 SQLite observations matched. Of 1,008 DuckDB observations, 924
matched and 84 reached an ordinary dbplyr bare-query CTE limitation. Those
84 were exactly the unsorted, identifier-free CTE collections, across the
fourteen source representations, three families and two labels. They shared
the diagnostic `attempt to select less than one element in integerOneIndex`.
The [CSV](composition-results.csv) retained those as `ok = FALSE`; they were
excluded from package-only findings after the separate control below.
The RDS retained case details, actual errors and rendered SQL/error strings.

[delegated-controls.R](delegated-controls.R) collected an ordinary bare SQL
table and a bare expansion query with CTE options: both produced identical
errors. Adding an ordinary identity filter made both collect successfully.
The November 3 hunt had already excluded the ordinary bare-table CTE boundary;
these observations established no new package-owned CTE promise. The same
control script compared included-branch ordering of NaN, missing, infinite
and signed-zero DuckDB values under both Margin orders with ordinary backend
ordering. Both matched, and all seven omitted copies were missing.

Initial harness runs needed corrections: the oracle originally put the
identifier after payload instead of after dimensions; a CTE fixture omitted a
namespace qualifier; and a failure-capture branch itself tried to render the
failing query without a handler. The final complete run corrected all three.
Only its 2,016 observations supplied the counts above. Original handles
`26419` and `40400` terminated at exit 1; final `53659` terminated at exit 0.
The delegated-control handle `35134` also terminated at exit 0. These were
development probes, not Review-ready results.

Run both scripts from the investigated package root. The oracle writes its
CSV/RDS to this evidence directory and logs can be captured alongside it.
They explicitly disabled testthat parallel execution. Each used private
in-memory databases, disconnected its successful run's connections, and
started no owned child process or service.

The environment was arm64 macOS, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
vctrs 0.7.3, rlang 1.3.0, RSQLite 3.53.3 and DuckDB 1.5.5. These comparisons
did not establish other dialects, hosted stores, alternate versions, arbitrary
collations or performance/resource bounds. No source changes, full suite,
package build or rendering were used for exploration.
