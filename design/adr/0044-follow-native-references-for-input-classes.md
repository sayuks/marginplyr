---
status: accepted
---

# Follow native references for input classes

Accepted on 2026-11-10. This decision records the target contract; package
implementation is separate. Class behavior follows explicit official reference
operations within the scope below, rather than whatever class loss an internal
implementation happens to introduce.

## Reference operations

| Margin API | Reference operation |
| --- | --- |
| `summarize_with_margins()` and its alias | The corresponding backend's `dplyr::summarise()` |
| `expand_with_margins()` | Native `dplyr::mutate()` for each grouping set, followed by native `dplyr::union_all()` |
| `nest_with_margins()` | The corresponding supported `tidyr::nest()` method, subject to the generated-container boundary below |
| `nest_by_with_margins()` | Local `dplyr::nest_by()`; for dtplyr, collect the input before applying local `nest_by()` as the reference, subject to the generated-container boundary below |

Use the same input, grouping-set keys and payload selection, with native
combination in Grouping-plan order. These are the references used by
[ADR 0042](0042-preserve-variable-labels-through-margin-operations.md) and
[ADR 0043](0043-follow-native-column-element-names.md). They define expected
results, not an execution algorithm or identical method-invocation counts.
Package-owned processing must not introduce additional class loss within the
selected guarantee; do not restore an input class the reference transforms or
drops. Existing contracts for grouping, row counts, result shape, display
conversion, factors and package-declared output types take precedence.

## Supported inputs and uses

The frame-level guarantee covers plain data frames, tibbles, direct data.table
inputs and existing supported lazy backends. Arbitrary custom classes on the
input frame require individual support decisions. Such an extra class is not
itself a new reason for refusal, but acceptance does not promise complete class
equivalence to that frame class's own methods. This retains ADR 0042 and
ADR 0043's frame-scope boundary.

Within supported inputs, custom column vectors and objects stored in list-column
cells receive the reference guarantee for uses the corresponding official
operation can handle. There is no fixed list of supported column or cell
classes and no blanket exclusion based on S3, S4 or S7 membership. This includes
an original custom frame object carried inside a cell; it is distinct from the
input frame passed as `.data` and from a newly generated nesting container.
R6-specific guarantees are outside this decision; existing backend support is
unchanged.

Carrying an object, grouping on it, ordering it and evaluating a summary over it
are separate capabilities. A reference that can carry a fitted model in a cell
does not establish that the model supports grouping or aggregation. An ordinary
atomic column does not contain independently classed objects per cell.

## Class-owned state and column roles

Class-name equality alone is insufficient. State needed to use the result as
that class follows the reference through the class's and backend's own
operations. A money class without its currency metadata is not equivalent
merely because its class attribute matches. Do not satisfy the guarantee by
blindly reattaching input class names or copying every attribute. This adds no
independent validation or repair of defects in a class's own methods.
Descriptive attributes, variable labels and column-vector element names retain
ADR 0042 and ADR 0043's separate policies.

| Role | Class comparison |
| --- | --- |
| Fixed keys, expanded payloads and original nested columns | Follow the reference for that use; `.keep = TRUE` retains original inner key values rather than outer display replacements |
| Variable dimensions | Apply the existing Margin-label display conversion; a string may change an ordinary dimension to character, while typed-missing labels follow supported prototype rules |
| Factors processed by package reconstruction | Retain existing backend-specific level and ordered-status guarantees; these do not add restoration of arbitrary factor subclasses |
| Calculated summaries | Follow native evaluation and common-type combination, without inferring class from a reused input name |
| Package-created identifiers, helpers and shares | Retain separately declared type contracts, including empty-result responsibilities |

Unsupported operations retain existing Package condition and External condition
responsibilities under [ADR 0015](0015-separate-package-conditions-from-internal-invariants.md).
This decision authorizes no automatic conversion of unsupported inputs into data
frames, implicit local evaluation of lazy inputs, or additional source-row reads;
[ADR 0020](0020-ask-before-reading-a-lazy-input.md) remains in force.

## Generated nesting containers

Retain the existing container contract: the generated nesting column is a list,
and its elements are data frames with the promised payload and row counts.
Neither the exact subclass of that list nor the exact frame class of those
newly generated elements receives native-reference equivalence. This includes
empty and zero-column payloads. Do not normalize all generated elements to one
class across backends.

This exception concerns newly created containers. Classes of original columns
and original objects transported inside them retain the scoped reference
guarantee above. A generated inner frame can be a data.table while its original
Date column still follows the Date reference behavior.

Exact container matching would add conversions without strengthening the input
class guarantee and can conflict with existing row-count promises. The dated
[nesting-class investigation](../../investigation/native-nesting-classes-2026-11-10.md)
measured the native and Margin representations, including the dtplyr rowwise
reference and zero-column cells.

## Output boundary and consequences

The new guarantee covers returned results and direct `collect()`. It adds no
class guarantee to subsequent ordinary operations, `compute()`, persistence or
re-reading. Existing explicit materialization and declared-type guarantees,
including [ADR 0033](0033-preserve-declared-types-in-empty-margin-results.md),
remain in force. Classes lost before the input reaches marginplyr are outside
the guarantee. Native metadata lifetime and the construction-time schema
precondition continue to apply. Empty results follow native class behavior
alongside separately declared package output types.

This supersedes [ADR 0016](0016-delegate-result-class-and-attributes-to-dplyr.md)'s
class delegation within the selected scope: an internal implementation choice
alone does not justify extra class loss. Its generated-container boundary and
separate package-owned guarantees remain. Public help must describe implemented
behavior until implementation lands. Verification compares native and Margin
results by role, including class-owned state, compatible custom columns and
cell objects, empty results and direct collection, while retaining the scoped
container exclusions. This decision does not implement those comparisons.
