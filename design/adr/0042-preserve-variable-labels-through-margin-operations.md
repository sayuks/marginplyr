---
status: proposed
---

# Preserve variable labels through Margin operations

## Agreed principle

On 2026-11-10, the maintainer agreed that preserving a column's variable label
(`label`) should be a default marginplyr guarantee when the output continues
to represent the same variable. This includes a grouping dimension whose
display values contain a Margin label. This draft awaits confirmation of the
complete design; it does not establish an implemented public contract.

Source-derived columns include fixed `.by` keys, grouping dimensions, columns
carried by expansion, and original columns inside nested cells, including keys
retained by `.keep = TRUE`. A calculated summary is governed by the upstream
baseline below, including when its output reuses an input column name.

## Agreed upstream baseline

For calculated summary columns, the maintainer selected the corresponding
upstream `summarise()` behavior as the baseline: dplyr for local data frames,
and the native Arrow or dtplyr method for those inputs. Backend-specific
attribute behavior is the reference rather than one normalized behavior across
backends. marginplyr does not infer a source variable label from a summary's
expression or output name. Labels that survive native summary evaluation follow
the combination rule below through marginplyr's additional processing.
The [native-summary measurements](../../investigation/upstream-summary-labels-2026-11-10.md)
showed why source-name reuse alone cannot define one rule: local dplyr dropped
the label for both `x = mean(x)` and `avg = mean(x)`, grouped dtplyr retained
it for both, and Arrow retained it only for the same-named output.

## Agreed output boundary

The variable-label guarantee covers the returned result and, for a lazy result,
its direct `collect()`. Persistence through `compute()` and subsequent ordinary
dplyr operations follows the backend's own rules. Metadata already absent
before a Margin operation reaches its input is outside this guarantee.
Obtaining labels introduces no additional source-row read.

## Agreed empty-result and metadata lifetime rules

Source-derived labels are preserved even when the returned column has zero
rows. A lazy result retains source-derived labels captured when the Margin
operation is constructed. Changing input labels afterwards requires constructing
a new Margin result to reflect the change, consistent with ADR 0002's
construction-time metadata boundary. Calculated-summary metadata continues to
follow native evaluation rather than being inferred from the input label.

## Rationale

Margin operations introduce display conversions and branch combination. Those
internal steps can discard variable descriptions that remain meaningful in the
output, even when implemented through upstream functions. marginplyr owns the
retention of these descriptions across its own processing. The
[dated investigation](../../investigation/attribute-label-preservation-2026-11-09.md)
records the observed losses separately from class-owned value-label behavior.

Variable labels describe columns. Value labels map stored codes to descriptions,
and missing-value declarations affect interpretation of stored values. The
agreed principle covers `label`; the rules for the other metadata require
their upstream owners.

## Agreed metadata scope

The additional preservation guarantee is limited to the variable label
`label`. Value-label dictionaries, missing-value declarations and arbitrary
other attributes follow the corresponding upstream backend and vector-class
rules. This adds no blanket attribute-copying or class-restoration policy.
Existing factor type, ordered status and level guarantees remain in force.

## Label representation and combination

A variable label is a single character value, including `NA_character_` or an
empty string, as accepted by `labelled::var_label()`. `NULL` denotes an absent
attribute. Other representations receive no additional preservation guarantee
and introduce no new input-validation error.

For source-derived columns, retain the captured source label. For summary
columns, inspect labels on the corresponding native grouping-set results:

- If the non-missing labels agree, preserve that label; absent attributes and
  `NA_character_` do not override an available description. An empty string is
  a description, not an absent attribute.
- With only absent or missing labels, retain `NA_character_` if present;
  otherwise leave the attribute absent.
- If different non-missing labels occur, leave their resolution to the native
  backend/vector combination rules. Add no marginplyr label-conflict error.

These are column attributes: intermediate grouping-set results can each carry
one label for their own column; the final column still has one label. The
policy adds no per-cell metadata representation.

## Pending dataset-level boundary

The agreed `label` guarantee concerns columns. An attribute on the outer data
frame, including a dataset-level `label`, is a separate property. Its explicit
treatment, including attributes on the outer nesting result and on each nested
data frame, remains to be agreed before confirming the complete design.
The [table-attribute investigation](../../investigation/upstream-table-attribute-contracts-2026-11-10.md)
found no single upstream preservation rule covering the complete Margin
composition. The final boundary must explicitly decide whether original frame
attributes receive any independent restoration guarantee.

## Relationship to ADR 0016

The proposed variable-label guarantee changes
[ADR 0016](0016-delegate-result-class-and-attributes-to-dplyr.md)'s refusal to
restore arbitrary column attributes by introducing one explicit responsibility.
Its general delegation of result classes and other attributes remains a
separate boundary. Upon confirmation, the accepted decision and ADR 0016's
affected clauses must be reconciled together. Public help must distinguish the
new variable-label guarantee from the remaining delegated attributes, and
behavioral verification must cover native-summary comparison, source-derived
columns, grouping-set combination, nesting, zero rows and direct collection.
