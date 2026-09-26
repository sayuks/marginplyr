# Preserve dtplyr special columns when expansion puts grouping keys first

## What to build

**Priority:** P2

A dtplyr expansion must preserve special-named columns when its required output projection moves fixed keys and Margin dimensions ahead of payload columns, including branches using typed-missing labels.

Input columns .N,g with .N = 1,2 and g = a,b should expand a rollup of g with a missing label into columns g,.N, values g = a,b,NA,NA and .N = 1,2,1,2. Collection instead raises a locked-binding error. The ordinary dtplyr union of the source and its missing-g copy succeeds, as does the Margin expansion with a text label. The special name may be an unrelated payload, fixed key, or dimension.

The package-generated projection can become a data.table expression that reads the special name rather than simply reordering columns. .BY takes the same boundary but produces a union column-name mismatch. This is independent of share calculation and needs no special-named summary source. ADRs 0016, 0020, and 0029 govern delegated results, lazy construction, and caller-input safety.

Evidence: finding 5 and the `dtplyr_projection` probe in the companion investigation. A 378-case matrix found 216 failures for .N, .I, .SD, .GRP, .NGRP, and .BY under typed-missing labels. All text-label cases and the .EACHI controls passed.

## Acceptance criteria

- [ ] The reproduction returns the expected column order and all four rows, preserving the original .N payload values without error.
- [ ] Preserve names and values for .N, .I, .SD, .GRP, .NGRP, and .BY in dimension, fixed-key, and carried-payload roles when the input column order requires projection.
- [ ] Cover NULL and NA_character_ labels, empty and populated inputs, all Margin orders, identifiers present/absent, and one-set/multiple-set plans. Compare complete results rather than merely successful collection.
- [ ] Keep default text-label and .EACHI controls correct; fixes must not depend on an otherwise unnecessary label mutation occurring first.
- [ ] Assert that caller input stays unchanged. Keep the result lazy, introduce no unrequested collection/materialization, and preserve Mutable-step refusal.
- [ ] Preserve previous special-column repairs for Margin order, labelling, ordinary summaries, and shares; do not forbid supported names globally.
- [ ] Add focused public-API regressions and pass the Review-ready check.

## Blocked by

None (can start immediately).
