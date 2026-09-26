# Avoid planning-time selection of ordinary packed across outputs beside shares

Investigated: 2026-09-26
Status at preparation: proposed; publication awaited breakdown approval.

## What to build

**Priority:** P2

A caller-named ordinary `across()` after a preceding summary must select columns in its actual local summary mask. Adding an unrelated Parent share or Total share must not execute its selector during planning.

The callback reproduction changes packed x = 1 into packed y = 10 because the selector runs twice instead of once. A second reproduction needs no side effect: input x = 1, y = -10 with `where(~ all(.x > 0))` works without the share but is rejected with an NA-predicate error when an unrelated share is added. The NA belongs to the analysis proxy, not the user's input. The intended packed columns are x and the preceding total.

A named `across()` stays packed and remains ineligible as a share source. Only its unrelated ordinary evaluation is being repaired. This path predates the latest dynamic-unpack change and is separate from the closed .names callback issue #684.

Evidence: [Investigation](https://github.com/sayuks/marginplyr/blob/81ca34a72e060ea7d5619e15849c52321d664319/investigation/bug-hunt-2026-09-26-edge-combinations.md), finding 2; probes `selection` and `predicate` in [executable probes](https://github.com/sayuks/marginplyr/blob/81ca34a72e060ea7d5619e15849c52321d664319/investigation/bug-hunt-2026-09-26-edge-combinations.R). ADR 0010 scopes dependency restrictions to actual share sources; ADR 0028 preserves caller-named packing.

## Acceptance criteria

- [ ] For a selector that does not select or reference the replaced share output, adding an unrelated share preserves the selected names, packed values, and selector count of the same Margin call with a constant in place of that share. Contextual share outputs remain excluded from ordinary local selection as documented.
- [ ] The callback reproduction evaluates once and retains packed x = 1.
- [ ] The nonmissing predicate reproduction succeeds, retaining packed x = 1 and total = 1, with share p = 1.
- [ ] Cover Parent and Total shares, named ordinary across before and after the share expression, more than one local group, selector calls, and active bindings.
- [ ] The selection still sees preceding ordinary summaries in the actual local summary mask, consistent with the documented local-selection contract.
- [ ] Trying to use the packed output itself as a share source is still refused. Source uniqueness, eligible-source restrictions, and earlier-summary alias restrictions are preserved.
- [ ] No extra source read or change to valid share denominators is introduced; existing callback protections remain effective.

## Blocked by

None (can start immediately).
