# Preserve .data and .env grouping dimensions during Margin label conversion

## What to build

**Priority:** P1

Preserve actual local grouping-column values when a Margin dimension is named .data or .env and has a non-missing text Margin label. The package-generated conversion must read the column rather than the data-mask pronoun.

A single row with .data = "a" and value = 1, summarized under one Grouping set retaining .data, returns the group "<environment>" and total 1. Ordinary dplyr grouping and the same Margin call with a missing label retain "a". A .env dimension instead raises an environment-to-character coercion error. All four Margin verbs are affected; neither an omitted grouping set nor sorting is required.

This is an integration defect in package-generated conversion. Ordinary across shares the upstream lookup quirk, while explicit column lookup reads the intended value; a caller asking for grouping or nesting never supplied across. #698 repaired a summary output named .data, and the existing fixed-key tests bypass this conversion. Do not reserve or reject these valid source names.

Evidence: finding 3 and the `pronoun_data` / `pronoun_env` probes in the companion investigation. The 2,304-case scope matrix produced 96 silently corrupted results, 192 errors, and 2,016 passing controls. ADRs 0003 and 0016 govern label validation and delegated output behavior.

## Acceptance criteria

- [ ] The one-set .data reproduction retains the actual group "a" and total 1. A rollup over a,b retains a,b and labels only the omitted dimension, with its correct totals.
- [ ] Both .data and .env work as Margin dimensions in summarize, expand, nest, and nest-by, preserving public names and actual keys. Check keys explicitly rather than relying on row counts or aggregate values.
- [ ] Cover character and numeric dimensions, empty and populated input, one-set and rollup plans, all Margin orders, observed-label checking on/off, and requested identifiers present/absent.
- [ ] Empty .env dimensions follow each verb's normal empty-result semantics without a generated coercion error.
- [ ] Preserve factor and ordered-factor reconstruction, including NA levels, missing values, and synthetic-level placement. NULL and NA_character_ labels and ordinary-named dimensions remain correct.
- [ ] Fixed keys, payload columns, and summary outputs with the same spellings retain their supported behavior. No collision check is weakened and no new source read is introduced.
- [ ] Add public-API regression tests for the shared conversion path and pass the Review-ready check.

## Blocked by

None (can start immediately).
