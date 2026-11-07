# Coordinator comparisons

Investigated: 2026-11-07
Tested SHA: `0b5143133c023ccb4efb6fe27f1ef551e9f49186`

The coordinator used a separate disposable Git archive and sequential R processes, with testthat parallel execution disabled. No package source was changed.

## Nested input ownership

Hypothesis: exact, partial or reordered input arguments of a nested data-mask operation might hide an enclosing dependency on an earlier contextual share. ADR 0019's independent data-mask ownership amendment requires the nested input expression to remain in the outer operation's scope.

`nested-input-controls.R` required the existing Package condition for seven spellings: positional, exact input name, partial input name, reordered arguments, rowwise, count and a two-operation nested chain. All seven refused the prohibited dependency. No new defect was established.

## Typed fixed-key denominator identity

Hypothesis: retained typed fixed keys, including missingness and structured local columns, might merge partitions or match an incorrect displayed Grand total set row.

`typed-fixed-key-controls.R` covered seven fixed-key kinds, two backend entrances and three sort requests (42 requests). For each successful result, an independent vctrs partition identity located its displayed Grand total set row; both shares had to equal the displayed numerator/denominator ratio and every Grand total share had to be exactly one.

All 33 accepted requests matched. Nine dtplyr requests failed at an upstream input boundary: list keys are unsupported by data.table grouping, while conversion to a lazy data.table did not retain a single matrix/packed input column under the original name. Direct dtplyr summaries with an explicit literal-name selection confirmed those restrictions. These nine were neither passing comparisons nor marginplyr findings. This oracle did not assert Margin order.

The numeric fixed-key fixture included signed zero, NA, NaN and infinities; those were keys, not cross-backend nonfinite share values. No live SQL collection was performed in this coordinator lane.

## Reading and duplicate discovery

`required-reading.csv` records the immutable blob, extent and completed coverage of the required documents. `environment.txt` records R and dependency versions. Package source digests were retained for the tested snapshot.

Duplicate discovery exported all 721 issue bodies, labels and comments and 703 pull requests through REST pagination. The CLI pull-request listing alone was not used as the complete archive. `duplicate-queries.txt` records conjunctive local searches and candidate titles. Full issue bodies/comments were read for the relevant candidate boundaries and the lane records identify the separating witnesses.

These finite comparisons established no guarantee for arbitrary R programs, every extension class, every backend or another dependency version.
