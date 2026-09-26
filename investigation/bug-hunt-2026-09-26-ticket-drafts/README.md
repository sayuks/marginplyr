# Proposed tickets for the edge-combination bug hunt

Investigated: 2026-09-26
Status at preparation: six unpublished drafts awaiting breakdown approval.

This snapshot preserves the issue bodies prepared from the
[bug-hunt evidence](../bug-hunt-2026-09-26-edge-combinations.md).
Each draft includes the intended behavior, independently verified evidence,
acceptance criteria, and blocking edges. The proposed GitHub labels for every
ticket were `bug` and `ready-for-agent`; no issue was created by saving these
files. These drafts are a preparation record, not the issue tracker.

The six repairs had no semantic blocking edges. The SQL identifier ticket
kept public-name validation and internal-name allocation together because
both need the same backend equivalence rule; it explicitly distinguished
refusing a caller conflict from avoiding a package-generated one.

| Draft | Proposed title | Priority | Blocked by |
| --- | --- | --- | --- |
| 01 | [Preserve ordinary .fns evaluation with dynamically false across unpacking](01-dynamic-unpack.md) | P2 | None |
| 02 | [Avoid planning-time selection of ordinary packed across outputs beside shares](02-packed-selection.md) | P2 | None |
| 03 | [Preserve dtplyr grouping keys named like data.table special variables](03-dtplyr-dimensions.md) | P2 | None |
| 04 | [Support eligible dtplyr share sources named .I, .GRP, or .NGRP](04-dtplyr-share-sources.md) | P2 | None |
| 05 | [Respect SQL identifier equivalence when validating outputs and allocating internal names](05-sql-identifiers.md) | P1 | None |
| 06 | [Preserve dynamically named .data summary outputs through portable evaluation](06-dot-data-output.md) | P2 | None |
