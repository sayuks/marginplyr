# Preserve dtplyr special grouping dimensions while calculating contextual shares

## What to build

**Priority:** P2

Allow Parent and Total shares when a dtplyr Margin dimension is named .N, .I, .SD, .GRP, or .NGRP. Internal copies of original grouping values must preserve those columns instead of resolving a data.table special variable.

With .N = a,b and value = 2,3, a rollup with total = sum(value) succeeds both as ordinary dtplyr grouping and as a Margin summary without a share. Adding a Total share fails at collection with a locked-binding error. The intended result has groups a,b,Total, totals 2,3,5, and double shares .4,.6,1. Parent shares fail through the same original-key retention path, which is also used by Total-share cardinality validation.

This differs from #696's special summary-source names and #695's ordinary grouping-key preparation. The summary source here is the ordinary name total. ADRs 0010 and 0017 govern the shares; ADRs 0020 and 0029 preserve lazy construction and caller-data safety.

Evidence: finding 4 and the `dtplyr_share` probe in the companion investigation. All 240 comparisons across the five names, both shares, text/missing labels, fixed keys, empty/populated inputs, and character/factor dimensions failed; the source remained unchanged in the minimized probes.

## Acceptance criteria

- [ ] Both shares over all five special dimension names match the equivalent local result's public names, grouping values, totals, and double ratios.
- [ ] Preserve original typed grouping identity through denominator matching and cardinality checks; a displayed Margin label must not become a join key.
- [ ] Cover zero/one/multiple rows, character/factor dimensions, text/NULL/NA labels, fixed .by partitions, composite rollups, and retained duplicate grouping sets.
- [ ] Cover all Margin orders and requested identifiers, and assert source-table immutability after both successful construction/collection and failure controls.
- [ ] Preserve numeric scalar eligibility, zero/missing-denominator behavior, Grand total semantics, and existing special summary-source protections from #696.
- [ ] Return a lazy dtplyr query and introduce no source materialization during construction. Keep existing Mutable-step refusals.
- [ ] Add public-API regressions and pass the Review-ready check.

## Blocked by

None (can start immediately).
