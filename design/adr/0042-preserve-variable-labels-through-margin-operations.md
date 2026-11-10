---
status: accepted
---

# Preserve column labels and follow native table attributes

Accepted and implemented on 2026-11-10 for
[#1559](https://github.com/sayuks/marginplyr/issues/1559). The original
research/documentation change recorded the decision before implementation.

## Principle

On 2026-11-10, the maintainer agreed that preserving a column's variable label
(`label`) should be a default marginplyr guarantee when the output continues
to represent the same variable. This includes a grouping dimension whose
display values contain a Margin label.

Source-derived columns include fixed `.by` keys, grouping dimensions, columns
carried by expansion, and original columns inside nested cells, including keys
retained by `.keep = TRUE`. A calculated summary is governed by the upstream
baseline below, including when its output reuses an input column name.

## Upstream summary baseline

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

## Output boundary

The variable-label and dataset-reference guarantees cover the returned result
and, for a lazy result, its direct `collect()`. Persistence through `compute()`
and subsequent ordinary dplyr operations follows the backend's own rules.
Metadata already absent before a Margin operation reaches its input is
outside this guarantee.
Obtaining labels introduces no additional source-row read.

## Empty results and metadata lifetime

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
column-label principle covers `label`; the remaining column metadata follows
its upstream owners.

## Metadata scope

The additional column-attribute preservation guarantee is limited to the
variable label `label`. Column value-label dictionaries, missing-value
declarations and arbitrary other column attributes follow the corresponding
upstream backend and vector-class rules. This adds no blanket attribute-copying
or class-restoration policy. Existing factor type, ordered status and level
guarantees remain in force. Dataset-wide attributes have the separate native
reference policy below.

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

## Dataset-level reference policy

On 2026-11-10, the maintainer selected explicit native reference operations as
the basis for dataset-wide attribute behavior. A frame-level `label` belongs
to this policy, separately from the column-label guarantee above.

| Margin API | Native reference for dataset-wide attributes |
| --- | --- |
| `summarize_with_margins()` and its alias | The corresponding backend's `dplyr::summarise()` |
| `expand_with_margins()` | Native `dplyr::mutate()` for each grouping set, followed by the backend's `dplyr::union_all()` combination |
| `nest_with_margins()` | The corresponding supported `tidyr::nest()` method; outer and inner frames are separate metadata locations |
| `nest_by_with_margins()` | Local `dplyr::nest_by()`; for dtplyr, the reference first collects the input and then applies local `nest_by()` |

Expansion's eager combination must honor that native reference even when its
implementation uses a more efficient binding strategy. The reference does not
require replacing the optimized implementation with repeated binary unions.
It also does not normalize dataset attributes across backends: a native union
that drops an attribute is a different reference from one that retains it.

For nesting, apply the reference to the original input separately for each
grouping set. The fixed keys and included dimensions define that set's groups;
the Margin verb's payload selection and `.keep` determine the inner columns.
Inner keys retain original values. Margin display values are assigned to the
outer result, and the partial results are combined. Compare inner frame
attributes against the corresponding native cells, rather than copying all
source attributes onto freshly constructed cells by default.

The dtplyr rowwise-nesting reference defines the expected metadata, not a
required implementation order. Efficient deferred nesting followed by
collection may remain if it produces the reference metadata without extra
source reads.

The native-reference guarantee covers plain data frames, tibbles and direct
data.table inputs, and the existing supported lazy backends. Arbitrary extra
classes on the data frame itself do not gain a guarantee of complete metadata
equivalence to their native methods. Their existing S3/vctrs behavior is
respected; supporting additional frame classes is a separate extension. This
limit concerns frame classes, not the factor, Date or labelled classes of
individual columns.

Existing Margin contracts for public names, row counts, grouping and result
shape take precedence over a literal copy of structural frame attributes.
Input `names`, `row.names`, class or grouping metadata must not be reattached
blindly to a differently shaped result. Attribute comparison concerns the
corresponding reference's metadata for the intended result.

The [table-attribute investigation](../../investigation/upstream-table-attribute-contracts-2026-11-10.md)
found no single upstream preservation rule covering the complete Margin
composition, so the reference operations are explicit. Observed native
behavior is not a promise that every preserved attribute describes the
resulting rows correctly.

Grouping-plan inspection, specification constructors and SQL diagnostics
construct their own outputs; this reference policy does not make those outputs
inherit source-frame metadata. Package-created identifiers and share outputs
gain no automatic descriptive label from an input column.

## Relationship to ADR 0016

The column-label guarantee and dataset-reference policy supersede
[ADR 0016](0016-delegate-result-class-and-attributes-to-dplyr.md)'s refusal to
add independent attribute restoration within these explicit scopes.
Its delegation of result classes and other column attributes remains a
separate boundary, while dataset-wide metadata follows the reference policy
above. The implementation issue owns the corresponding public-help, test and
architecture updates. Public help must describe implemented behavior until
that work lands. Verification must cover native-summary comparison,
source-derived columns, grouping-set combination, nesting, zero rows and
direct collection.
