# Evaluate a first packed across selection in its local summary mask beside shares

## What to build

**Priority:** P2

A caller-named ordinary across must select columns in its actual local summary mask even when it is the first ordinary summary and a later, unrelated summary supplies a Contextual share.

For one input row x = 1, a first packed across uses ordinary n() in its selector to choose x, followed by total = sum(x). Both ordinary dplyr and the Margin call without a share return packed x = 1. Adding a Total share of total raises the data-masking-only n() error during planning. Adding an otherwise irrelevant preceding ordinary summary avoids the failure, demonstrating that the first-summary deferral boundary changes the behavior.

#694 repaired packed selection after a preceding summary; the first ordinary selection follows a different planning branch. A named packed output remains ineligible as a share source. This repair does not relax eligibility or dependency restrictions on actual share sources. ADRs 0010, 0019, and 0028 govern those distinctions.

Evidence: finding 2 and the `first_packed` probe in the companion investigation. Thirty-six comparisons cover three local frame classes, Parent/Total shares, one/multiple detail groups, and direct/callback/active-binding selectors.

## Acceptance criteria

- [ ] The one-row reproduction returns packed x = 1, total = 1, and a double Total share of 1, matching the ordinary control's packed output.
- [ ] For selectors unrelated to the share output, adding an unrelated share preserves selected names, packed values, selector counts, and access to the actual summary context. A dummy preceding summary is unnecessary.
- [ ] Cover a packed across as the first ordinary dot and after preceding summaries; include direct selector expressions, callbacks, and active bindings that read ordinary n().
- [ ] Cover Parent and Total shares, one and multiple detail groups, rollups, and all three supported local frame classes. Preserve preceding-summary visibility where such summaries exist.
- [ ] Keep Contextual share outputs excluded from ordinary selection as documented. A constant substituted for a share output must not be used as an oracle if the selector selects that constant column.
- [ ] Packed share sources, duplicate source definitions, ineligible numeric sources, and disallowed earlier-summary dependencies remain refused with their existing public contract. Preserve #694 and unrelated naming-callback protections.
- [ ] Add focused public-API regressions and pass the Review-ready check. No additional source read is needed.

## Blocked by

None (can start immediately).
