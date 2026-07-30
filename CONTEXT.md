# Margin Grouping

marginplyr describes SQL-style multi-grain grouping in terms that apply
consistently to local and remote data.

## Language

**Grouping specification**:
A user-declared description of the grouping sets, rollups, cubes, or products
to be applied.
_Avoid_: Grouping expression

**Grouping specification kind**:
The semantic form of one node in a Grouping specification: grouping set,
grouping-set union, rollup, cube, or Cartesian product.
_Avoid_: Grouping specification type

**Grouping plan**:
The backend-independent grouping semantics obtained by fully expanding a
grouping specification.
_Avoid_: Execution plan, backend plan

**Grouping set identifier**:
The one-based position of a grouping-set occurrence in a Grouping plan. It
distinguishes repeated identical sets and changes when the plan order changes.
_Avoid_: Grouping identifier, grouping mask

**Grouping identifier**:
A bit mask encoding which chosen grouping dimensions are absent from a
grouping set. Identical absence patterns have the same identifier regardless
of their position in a Grouping plan.
_Avoid_: Grouping set identifier, set number

**Grouping bit**:
A zero-or-one flag indicating whether one chosen grouping dimension is absent
from a grouping set.
_Avoid_: Missing-value flag, grouping set identifier

**Margin operation**:
One request to summarize, expand, or nest data across the grouping sets in a
grouping plan, from preparation through finalization.
_Avoid_: Grouping plan, operation context

**Margin label**:
A display value inserted into a grouping dimension that is absent from a
grouping set. For factor dimensions, it is a synthetic level placed last by
default and may be placed first explicitly. `NA_character_` and an absent
label represented by `NULL` use a typed missing value instead of a synthetic
factor level.
_Avoid_: Total value, missing-value replacement

**Parent share**:
For a row in a rollup result, the ratio of one named scalar summary value to
the corresponding value in the immediately less detailed grouping set. The
root row has a parent share of one. A missing numerator, zero denominator, or
missing denominator produces a missing double value. The source is a
previously defined numeric scalar summary and the result is always a double;
finite ratios are not clamped. Parent shares are defined for a rollup,
including composite dimensions, and are calculated independently within each
fixed `.by` group. Duplicate occurrences remain in the result but are skipped
while finding the next strictly less detailed parent set.
_Avoid_: Subtotal share, percent of grand total
