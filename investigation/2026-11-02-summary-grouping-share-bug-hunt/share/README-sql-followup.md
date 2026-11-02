# Reading the volatile SQL share follow-up boundary

Investigated: 2026-11-02
Repository snapshot: `978d7bef4247bf1348af5b90bfab47924db25500`

This was a bounded independent reading of the coordinator's volatile-share
failure, the full body and comments of closed
[#919](https://github.com/sayuks/marginplyr/issues/919), the full body and
comments of its merged implementation
[#939](https://github.com/sayuks/marginplyr/pull/939), ADR 0014's
*lazy shares read one staged relation* amendment, the current
`apply_windowed_shares()` implementation, and the entire
`test-share-visible-denominators.R` file. A small independent hand-written SQL
control supplemented the coordinator's public-interface runtime evidence.

## What the old report established

#919 established the public visible-denominator contract for eligible numeric
scalar sources, including stateful and nondeterministic ones. Its original
failure reevaluated the staged summary in lazy denominator self-joins. The
reported ratios used values from a different evaluation of that relation than
the displayed numerator and denominator rows.

#939 replaced those self-joins with grouped scalar selection for dtplyr and
conditional aggregate windows for SQL. ADR 0014 recorded that implementation
boundary and explicitly declined a purity restriction. Thus a scalar random
source was not outside the supported contract merely because it lacked an
ordinary aggregate in its expression.

## What the investigated implementation and tests held

The examined SQL adapter looped over distinct target occurrences. For each
Parent target, it grouped by fixed keys plus typed keys retained by that
target and added a conditional MAX window. Later targets updated the same
denominator alias conditionally. A two-dimension rollup therefore produced
two nested window stages; it did not reintroduce the original denominator
self-join.

The existing regression file was not confined to one-dimension rollups.
It included DuckDB fixed partitions, a missing fixed key, repeated grouping
levels, both helpers and a two-dimension parent chain. However, that case
requested `.duplicates = "keep"` and `.id = "set"`. The examined adapter
selection explicitly disabled native aggregation for that combination, so it
covered portable UNION aggregation. The simple default-policy DuckDB cases
covered native aggregation with only one Parent target. Thus those tests did
not cover native aggregation together with multiple Parent windows.

The coordinator's retained `volatile-shares.log` displayed a pure scalar
`random() + 0.1` source on a native DuckDB rollup(g, h). Its SQL carried one
staged relation and two nested conditional MAX windows. The subsequent
`sequence-share-probe.log` established the same mismatch with nextval alone
and with sum(x) plus nextval. One-dimension Parent, two-dimension Total and
portable two-dimension Parent controls agreed with their displayed values.
In the failing native cases, detail shares disagreed with the displayed
immediate-parent values while subtotal-to-root ratios agreed. The failure
therefore did not require a pure scalar expression without an aggregate.

## Independent ordinary SQL control

`ordinary-window-control.R` used no Margin operation. It issued hand-written
native GROUPING SETS SQL over four rows, a nextval source, and the same two
nested conditional MAX windows. The seven-row result displayed values eight
through fourteen, while its first Parent window had read the earlier sequence
values: the detail ratio using displayed value eight was 8/6 instead of the
visible parent's 8/12. The sequence ended at fourteen. This reproduced the
coordinator's numerical mismatch through the ordinary SQL engine alone.

A second ordinary SQL statement placed only the seven-row grouping result in
an explicitly MATERIALIZED CTE before applying those windows. All ratios
matched the displayed rows and the sequence ended at seven. The control made
no construction-time read or materialization request through marginplyr; it
was an independently executed SQL statement. It established backend-native
reevaluation and a contrasting value-binding boundary, not a proposed package
implementation. The replay asserted both outcomes and exited 0. The final
driver used `duckdb(shared_home = FALSE)` and closed the connection on exit.

## Classification and next evidence

The public contract was the same one #919 already established. The examined
failure was a distinct residual implementation boundary after that repair:
native DuckDB grouping through multiple Parent windows, rather than reuse of
the staged relation in hidden self-joins. A narrowly scoped follow-up could
reference #919/#939 and preserve their portable multi-level and native
one-level passing controls. A report claiming that multi-level volatile
regression coverage was entirely absent would be false.

The ordinary-engine reproduction did not withdraw marginplyr's stronger
visible-denominator contract. It showed why one textual staged relation was
insufficient evidence for that contract on the tested native query shape.
These experiments established no other SQL engine, DuckDB version, arbitrary
volatile-expression shape, or optimizer setting. They did not recommend a
particular implementation or treat a source purity restriction as decided.

The coordinator's later 96-call matrix expanded this boundary: 128 individual
share comparisons included 32 mismatches. Fixed-key native cases also exposed
one-dimension Parent-only and Total-only failures; requesting both helpers
could change which share failed. All tested portable comparisons agreed.
Thus multiple Parent windows were one demonstrated trigger, not a complete
classification or necessary condition. Fixed-key private-alias stages supplied
another boundary, and the exact planner transformation remained unproved.
The [backend investigation](../backend/README.md) and its
`native-parent-matrix.csv`/log owned the expanded cases and final scope.
