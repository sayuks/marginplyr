# Preserve dtplyr grouping keys named like data.table special variables

Investigated: 2026-09-26
Status at preparation: proposed; publication awaited breakdown approval.

## What to build

**Priority:** P2

Make supported dtplyr Margin summaries and expansions work when a grouping dimension is named `.N`, `.I`, `.SD`, `.GRP`, or `.NGRP`. Summaries must also accept these names as fixed keys. Package-generated key copies, conversions, and label expressions must address the input column safely while preserving its public name.

The minimal expansion has .N values b and a, payload values 1 and 2, and rollup(.N). A nonmissing Margin label makes collection fail with a locked-binding error. Ordinary dtplyr union of the source and a scalar-labelled copy succeeds, as does the typed-missing Margin expansion. Margin summaries also fail with a typed-missing label because their internal grouping-key copy introduces another special-variable read.

The already repaired #689 case carries an unrelated .N payload and passes. This ticket covers the dimension role. Ordinary backend nesting restrictions are outside its scope.

Evidence: [Investigation](https://github.com/sayuks/marginplyr/blob/81ca34a72e060ea7d5619e15849c52321d664319/investigation/bug-hunt-2026-09-26-edge-combinations.md), finding 3; probe `dimension` in [executable probes](https://github.com/sayuks/marginplyr/blob/81ca34a72e060ea7d5619e15849c52321d664319/investigation/bug-hunt-2026-09-26-edge-combinations.R). The supported ordinary dtplyr equivalents distinguish package-added failures from backend input restrictions.

## Acceptance criteria

- [ ] The minimal expansion collects four correct rows, retaining the .N spelling and the original payload in detail and Margin rows.
- [ ] For all five affected names, supported summaries and expansions agree with corresponding local results in public names, key values, counts, and payload values.
- [ ] Cover empty, one-row, and multiple-row input; a one-set plan and a rollup; text and typed-missing labels; all three Margin-order choices.
- [ ] Summaries with typed-missing labels are explicitly covered, so repairing only the text-label expression cannot satisfy the ticket.
- [ ] These names also work as fixed summary keys, selected with explicit .by or inherited from an ordinary group_by input. Include passing direct dtplyr grouping controls.
- [ ] Retain passing controls using equivalent ordinary dtplyr operations and the .BY spelling; retain the unrelated .N-payload regression from #689.
- [ ] Result construction stays lazy, and both construction and collection leave the caller's data and lazy source unchanged.
- [ ] Existing factor-label semantics and internal-name collision safeguards continue to pass. Do not impose a new public prohibition on these otherwise supported names.

## Blocked by

None (can start immediately).
