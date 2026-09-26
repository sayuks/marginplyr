# Support eligible dtplyr share sources named .I, .GRP, or .NGRP

Investigated: 2026-09-26
Status at preparation: proposed; publication awaited breakdown approval.

## What to build

**Priority:** P2

Calculate Parent shares and Total shares from otherwise eligible numeric scalar summaries named `.I`, `.GRP`, or `.NGRP`. The internal denominator-copy and ratio expressions must safely read those summary columns.

A single value 2 summarized as .I is accepted by ordinary dtplyr and by the Margin verb. Adding a Total share of .I fails at collection with a locked-binding error, even without a grouping specification, text labels, or Margin order. Renaming only the source to total succeeds; the local equivalent returns .I = 2 and p = 1. These names also work as share-output names in passing controls.

This is independent of the grouping-dimension repair: the minimal reproduction has no dimension at all.

Evidence: [Investigation](https://github.com/sayuks/marginplyr/blob/81ca34a72e060ea7d5619e15849c52321d664319/investigation/bug-hunt-2026-09-26-edge-combinations.md), finding 4; probe `share` in [executable probes](https://github.com/sayuks/marginplyr/blob/81ca34a72e060ea7d5619e15849c52321d664319/investigation/bug-hunt-2026-09-26-edge-combinations.R). Preserve the eligibility and denominator contracts of ADRs 0010 and 0017.

## Acceptance criteria

- [ ] The minimal Total-share call collects .I = 2 and p = 1 under the original public names.
- [ ] All three source names work for Parent and Total shares where their corresponding ordinary-name calls are supported.
- [ ] Direct helper calls and the supported contextual-across spelling both calculate the share from the intended source column.
- [ ] Cover empty and nonempty input, fixed partitions, multiple grouping sets, both text and missing labels, and all Margin-order choices.
- [ ] Numeric eligibility, uniquely defined sources, missing or zero denominators, and the Grand total share rule remain unchanged.
- [ ] Distinct ordinary source columns and share-output names are preserved; existing successful use of these spellings as share outputs is covered.
- [ ] Construction stays lazy and never mutates the caller's source; collection produces the same values as the local and ordinary-name controls.
- [ ] Regression tests reach the public Margin interface and do not depend on the separate grouping-dimension fix.

## Blocked by

None (can start immediately).
