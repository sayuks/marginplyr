# Live SQL window and composition boundaries

Investigated: 2026-11-04
Snapshot: `78ebbc36200bdc5cef70838dd4e772c5c4a1401c`

The coordinator tested live RSQLite 3.53.3 and DuckDB 1.5.5 with dplyr 1.2.1,
dbplyr 2.6.0 and R 4.6.1 on aarch64 macOS. Each harness used disposable
in-memory databases and disconnected every connection at completion. No new
SQL defect was confirmed by these finite loops.

## Hypotheses and independent oracles

The ranked hypotheses were (1) downstream projection/materialization could
lose the window calculation's public columns or values, (2) fixed or variable
key equivalence could choose an incorrect denominator, and (3) requesting both
share kinds could disturb their staged values. ADRs 0010, 0014 and 0017 owned
the displayed-denominator and fixed-partition contracts; ADR 0031 owned the
narrow direct SQLite type boundary. Source inspection covered the share
adapters, the ratio expression and private name allocation.

`live-window-matrix.R` ran 320 cases: two live backends, finite/zero/missing/
empty fixtures, with/without fixed keys, native-policy/portable-policy
aggregation, unsorted/sorted results, and direct/select/mutate/collapse/compute
execution. SQLite used portable aggregation under both duplicate policies;
DuckDB selected native or portable aggregation. Both share kinds were requested
together. The oracle located each denominator in the collected visible result
using its Grouping set identifier and missing-safe fixed/included keys, then
computed the ratio independently in R. All 320 final value comparisons passed.
Direct compute was exercised with temporary destinations removed after use.
This loop did not independently assert Margin order.

An initial version of that oracle also demanded double types after ordinary
`mutate()` and `collapse()` over an empty SQLite result with fixed keys. Eight
such comparisons differed only because derived empty share columns were
logical. ADR 0031 explicitly left subsequent ordinary dplyr verbs to dbplyr's
semantics and did not extend the direct-result type guarantee to them. Those
observations were excluded as unsupported type expectations; the final oracle
compared numeric values at that boundary. The initial harness also used an
invalid sort vocabulary and a conditional bare-name selection; these setup
errors were corrected before the matrix completed and were not findings.

`collation-matrix.R` ran 16 cases: SQLite/DuckDB, both aggregation policies,
both sort requests and typed-missing/text Margin labels. The source declared
NOCASE collation on both a fixed key and a variable dimension and contained
case-equivalent strings and missing keys. An independent local fixture
normalized those strings before summarization. Normalized key identities,
visible summaries and both shares agreed in every case. This tested the
backend's equivalence relation rather than assuming distinct stored spellings
were distinct groups. It did not test every collation or dialect.

## Replay

From the checkout root:

```sh
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/backend/live-window-matrix.R
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/backend/collation-matrix.R
```

Adjacent CSVs and logs retained each final matrix. The final window-matrix
completed through handle `47535`, exit zero; the collation
matrix completed through handle `54662`, exit zero. Earlier discovery
executions `81361` and `29286` stopped on setup errors; `4215` completed the
initial matrix and exited one on the unsupported derived-type expectations.
No run supplied Review-ready evidence.

These loops established no guarantee for hosted services, other dependency
versions, arbitrary callbacks, every SQL type, all collations, interruption,
concurrency, or all later query compositions. PostgreSQL's prior long-alias
finding and its merged repair were read as historical evidence; this lane did
not rerun that earlier PostgreSQL matrix. No source or package test was edited.
