# Proposed tickets after #699

Investigated: 2026-09-26
Status at preparation: five unpublished drafts awaiting breakdown approval.

The [investigation](../bug-hunt-2026-09-26-post-699.md) and its executable
probes establish five independently verifiable repair units. These are proposed
English GitHub issue bodies, with `bug` and `ready-for-agent` labels. Saving
these drafts does not publish an issue.

| Draft | Priority | Blocked by |
| --- | --- | --- |
| [Preserve ordinary across argument evaluation for dynamic unpack values](01-unpack-evaluation.md) | P2 | None |
| [Evaluate a first packed across selection in its local summary mask beside shares](02-first-packed-selection.md) | P2 | None |
| [Preserve .data and .env grouping dimensions during Margin label conversion](03-pronoun-dimensions.md) | P1 | None |
| [Preserve dtplyr special grouping dimensions while calculating contextual shares](04-dtplyr-share-dimensions.md) | P2 | None |
| [Preserve dtplyr special columns when expansion puts grouping keys first](05-dtplyr-projection.md) | P2 | None |

The unpack cases share one ordinary-evaluation boundary, and the two pronoun
names share one label conversion. The dtplyr findings remain separate because
one repairs original share-key copying and one repairs expansion projection;
shared files alone create no blocking edge.

## Publication (2026-09-26)

The user approved these five independent repair units. The following GitHub
Issues were published and read back to verify their full bodies, open states,
`bug` / `ready-for-agent` labels, and empty native blocking dependencies:

- [#700](https://github.com/sayuks/marginplyr/issues/700): ordinary across argument evaluation.
- [#701](https://github.com/sayuks/marginplyr/issues/701): first packed selection beside shares.
- [#702](https://github.com/sayuks/marginplyr/issues/702): pronoun-named dimensions (P1).
- [#703](https://github.com/sayuks/marginplyr/issues/703): dtplyr original share keys.
- [#704](https://github.com/sayuks/marginplyr/issues/704): dtplyr expansion projection.

Each published body links to the investigation and executable probes at the
fixed evidence commit `2b2b9e73e6d7381c260595e3c472597a7c92160e`.
[PR #705](https://github.com/sayuks/marginplyr/pull/705) preserves the evidence
and this preparation/publication record; it does not implement the repairs.
