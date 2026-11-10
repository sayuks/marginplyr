---
status: accepted
---

# Follow native references for column-vector element names

Accepted on 2026-11-10. This decision records the target contract for
`names(data[[column]])`; implementation is pending. Column names and data-frame
row names retain their existing contracts.

## Native reference operations

| Margin API | Native reference for column-vector element names |
| --- | --- |
| `summarize_with_margins()` and its alias | The corresponding backend's `dplyr::summarise()` |
| `expand_with_margins()` | Native `dplyr::mutate()` for each grouping set, followed by native `dplyr::union_all()` |
| `nest_with_margins()` | The corresponding supported `tidyr::nest()` method; outer keys and inner columns are compared separately |
| `nest_by_with_margins()` | Local `dplyr::nest_by()`; for dtplyr, collect the input before applying local `nest_by()` as the reference |

A reference uses the same input, grouping-set keys and payload selection.
Combine partial results in Grouping-plan order using native element-name rules.
Backend-specific behavior is the reference, rather than one normalized rule
for every input class. These operations specify expected metadata, not a
required execution algorithm; efficient multiway binding and deferred nesting
may remain.

Element names belong to individual elements. Expansion can repeat them,
nesting can partition them, and aggregation can combine several named
elements into one value. Copying an original names vector onto every output
column cannot express all three operations.

## Display conversion and preservation

For a dimension omitted from a grouping set, compare against native mutation
with an unnamed Margin-label value of the required type, including a typed
missing value. Original element names are not restored onto that replacement.
Dimensions that are not replaced retain the native reference behavior.

Expanded payloads and original columns inside nested cells, including keys
retained by `.keep = TRUE`, follow the native reference. Names the reference
retains must survive package-owned processing; names the reference drops
receive no independent restoration guarantee. Calculated summaries likewise
follow their native reference rather than receiving inferred source names.

Repeated, empty-string and missing element names introduce no new validation
error. Their acceptance and transformation follow the native reference. This
also covers absent names and empty results, including native distinctions
between `NULL` and `character()`. Ordering or selecting result rows uses the
backend's native element-name behavior alongside the existing Margin contracts
for values, row counts and Margin order.

## Supported inputs and output boundary

The reference guarantee covers plain data frames, tibbles, direct data.table
inputs and the existing supported lazy inputs. Arbitrary additional classes on
the frame itself gain no guarantee of complete equivalence to their native
methods. Their existing S3/vctrs behavior is respected. Existing column-type
and factor guarantees remain in force.

The guarantee covers the returned result and its direct `collect()`. Names
already absent before the Margin operation reaches its input are outside the
guarantee. Persistence through `compute()` and subsequent ordinary dplyr
operations follows the backend's own rules without an additional element-name
guarantee. Element-name metadata lifetime also follows the native backend;
this decision adds no independent snapshot of source elements or monitoring of
their names. ADR 0002's construction-time input-schema precondition remains.

[ADR 0020](0020-ask-before-reading-a-lazy-input.md) continues to govern queries.
This decision permits no additional source-row read, implicit fallback
collection or duplicate metadata acquisition to obtain element names.

## Arrow metadata is not row identity

Arrow's native R metadata behavior is the reference even when restored names
do not track the corresponding source elements. marginplyr adds no independent
tracking, correction or verification of that correspondence. Element names are
not promised to act as row identifiers.

The [dated investigation](../../investigation/column-element-names-2026-11-10.md)
observed Arrow 25.0.1 restoring original names after reordering values, losing
names with a warning after filtering, and extending original names with missing
names after a union. These are versioned upstream observations, not fixed
spellings or outcomes that marginplyr must perpetuate if Arrow changes.
Native warnings and errors retain their existing External condition treatment.

## Relationship to existing attribute decisions

This decision specializes [ADR 0016](0016-delegate-result-class-and-attributes-to-dplyr.md)'s
attribute delegation for column-vector element names: compare the complete
Margin composition with the explicit native references, rather than accepting
additional losses caused by package-owned processing. Other attributes retain
their existing owners. [ADR 0042](0042-preserve-variable-labels-through-margin-operations.md)'s
variable-label and dataset-attribute policies remain separate.

Public help must describe implemented behavior until implementation lands.
Acceptance verification compares native and Margin results on the same
backend, covering grouping keys, calculated summaries, converted dimensions,
expanded payloads, nested original columns, name representations, empty
results and direct collection. It checks native-relative behavior rather than
pinning the observed upstream name vectors across future dependency versions.
