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
