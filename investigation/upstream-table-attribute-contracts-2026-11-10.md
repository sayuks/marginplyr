# Upstream contracts for table-level attributes

Investigated: 2026-11-10

## Question and boundary

This investigation asked what concrete rules could replace an unspecified
reference to "upstream rules" for dataset-wide metadata. It distinguished an
attribute of the outer table, such as `attr(df, "label")`, from a column's
`label`, and from an attribute of an individual nested data frame. Identical
attribute names did not make those three locations one metadata property.

The findings below separated public documentation from version-pinned source
behavior. They did not establish a new marginplyr contract. The inspected live
documentation identified dplyr 1.2.1, tidyr 1.3.2, dtplyr 1.3.3, vctrs 0.7.3
and Arrow 26.0.1. Arrow source inspection used 25.0.1, the version installed
for the accompanying measurements, rather than assuming that the live guide
described that installation exactly.

## Explicit public contracts for local operations

| Operation | Documented rule relevant to table attributes | What this did not establish |
| --- | --- | --- |
| `dplyr::mutate()` | The result has the input type, and data-frame attributes are preserved. | Preservation through a later row combination, nesting operation, or every third-party method. |
| `dplyr::summarise()` | It creates a new data frame and does not preserve data-frame attributes. | A rule that every column attribute must disappear, or a rule for a separately implemented backend method. |
| `dplyr::bind_rows()` | The result has the type of the first input. | A documented conflict-resolution policy for arbitrary attributes from different input tables. |
| `dplyr::union_all()` | Compatible frames' rows are combined, including duplicates. | A public table-attribute conflict-resolution policy. |
| `tidyr::nest()` | Non-nested columns define the outer groups; a grouped input's grouping is preserved. | A blanket guarantee about arbitrary attributes on the outer result or on each inner frame. |
| `dplyr::nest_by()` | A rowwise frame is built from group keys and a list of data frames; input frame attributes are not preserved. | A separate blanket preservation rule for attributes of every inner frame. |

Sources: the Value and Methods sections of
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
the Value section of
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
the Value section of
[`bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html),
the description and compatibility requirements of
[set operations](https://dplyr.tidyverse.org/reference/setops.html),
the description and Grouped data frames section of
[`nest()`](https://tidyr.tidyverse.org/reference/nest.html), and the Value
section of [`nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.html).
The absences above describe those inspected references, not proof that no
other upstream document could ever discuss the subject.

`mutate()` and `summarise()` were S3 generics; their references directed readers
to individual methods for behavioral differences. Applying a local frame
method's attribute rule to Arrow or dtplyr solely because the generic had the
same name would therefore have been an inference, not a documented guarantee.
Sources: Methods sections of
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) and
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).

## Reconstruction belonged to the class and the particular operation

dplyr's extension guide distinguished independent frame attributes from
attributes depending on rows or columns. It advised class authors to
recompute row-dependent scalar attributes in `dplyr_reconstruct()`, and to
provide additional column/subsetting methods where required. A subclass of a
plain data frame needed an attribute-preserving `[` method because base
subsetting did not preserve its attributes. The guide also warned that each
verb needed verification. Source: Basic advice in
[Extending dplyr](https://dplyr.tidyverse.org/reference/dplyr_extending.html).

That did not mean that every aggregation consulted the original input's
reconstruction method. The same guide explicitly said `summarise()` and
`reframe()` modified a new frame made from grouping data or `.by`, so their
results did not retain custom subclasses or attributes. Its join and
`nest_join()` descriptions separately named reconstruction against original
inputs. Source: Current usage in
[Extending dplyr](https://dplyr.tidyverse.org/reference/dplyr_extending.html).

The source-level distinction also mattered for combination. In dplyr 1.2.1,
`union_all.data.frame()` called `vec_rbind(x, y)` and then
`dplyr_reconstruct(out, x)`. Thus calling `vec_rbind()` alone was not the same
attribute operation as calling the public `union_all()` method. Source:
[`union_all.data.frame()` in dplyr 1.2.1](https://github.com/tidyverse/dplyr/blob/v1.2.1/R/sets.R).
The vctrs reference described a common container/column type for row binding,
including `vec_ptype2()` and `vec_restore()` dependencies; it did not promise
to copy every arbitrary frame attribute from the first argument. Source:
[Value, invariants and dependencies of `vec_bind()`](https://vctrs.r-lib.org/reference/vec_bind.html).

For nesting, tidyr 1.3.2's `nest.tbl_df()` selected inner and outer columns,
packed the inner columns, combined them with `vec_cbind()`, reconstructed the
outer tibble, and chopped rows into list elements. `nest.data.frame()` first
converted an exactly plain data frame to a tibble. This was a particular
composition of operations, rather than an alias for `mutate()`. Source:
[`nest.data.frame()` and `nest.tbl_df()` in tidyr 1.3.2](https://github.com/tidyverse/tidyr/blob/v1.3.2/R/nest.R).

## dtplyr exposed translations, not one local metadata rule

dtplyr documented `lazy_dt()` as a deferred data.table translation and
`collect()` as producing a tibble, grouped when needed. Its native
`summarise()` method translated to data.table's `j` argument; its `nest()`
method used non-nested variables in `by` and `.SD` in `j`. Sources:
[`lazy_dt()`](https://dtplyr.tidyverse.org/reference/lazy_dt.html),
[`collect.dtplyr_step()`](https://dtplyr.tidyverse.org/reference/collect.dtplyr_step.html),
[`summarise.dtplyr_step()`](https://dtplyr.tidyverse.org/reference/summarise.dtplyr_step.html),
and [`nest.dtplyr_step()`](https://dtplyr.tidyverse.org/reference/nest.dtplyr_step.html).

The inspected dtplyr references did not state a general guarantee for arbitrary
dataset-wide attributes. In dtplyr 1.3.3's source, collection converted the
evaluated data.table to a tibble and explicitly removed `.internal.selfref`
and `sorted`; this was not an arbitrary attribute-restoration pass. Source:
[`collect.dtplyr_step()` and `as_tibble.dtplyr_step()` in dtplyr 1.3.3](https://github.com/tidyverse/dtplyr/blob/v1.3.3/R/step.R).
Consequently, preservation needed to be measured on the translated operation
and its collection, not inferred from the local dplyr method's name.

## Arrow had a metadata representation and operation-specific implementation

Arrow's Schema reference explicitly described storing top-level, column and
list-element R attributes in `schema$metadata[["r"]]` for reconstruction in R.
This representation was distinct from ordinary R attributes attached directly
to an Arrow R6 wrapper and from other schema metadata keys. The guide's
example stored a frame's `df_meta` in `$r$attributes`. Sources:
[Schema, R Metadata](https://arrow.apache.org/docs/r/reference/Schema-class.html)
and [Metadata, R object attributes](https://arrow.apache.org/docs/r/articles/metadata.html).

Those documents described conversion and file round trips; they did not supply
a comprehensive attribute contract for all dplyr query compositions. The
native query reference described deferred execution, `compute()` producing an
Arrow Table and `collect()` producing an R tibble. Its supported verbs
included `mutate()`, `summarise()` and `union_all()`, but did not list
`tidyr::nest()` or `dplyr::nest_by()`. Source:
[Arrow's dplyr query reference](https://arrow.apache.org/docs/r/reference/acero.html).

Version-pinned Arrow 25.0.1 source gave a more precise explanation, without
turning it into a cross-version public promise:

- `ExecNode$Aggregate()` explicitly cleared the source schema's R top-level
  attributes. `Project()`, `Filter()` and `Union()` carried the left node's
  source-schema information forward. `final_metadata()` copied that schema's
  metadata and filtered R column metadata to the output schema. Source:
  [`ExecNode` in Arrow 25.0.1](https://github.com/apache/arrow/blob/apache-arrow-25.0.1/r/R/query-engine.R).
- `get_r_metadata_from_old_schema()` kept column metadata only where output
  name and Arrow type matched. `apply_arrow_r_metadata()` applied stored
  top-level attributes when collecting a frame. These mechanisms were
  different from evaluating an R expression that itself retained attributes.
  Source: [Arrow 25.0.1 metadata helpers](https://github.com/apache/arrow/blob/apache-arrow-25.0.1/r/R/metadata.R).
- `collect.arrow_dplyr_query()` computed a Table and the ArrowTabular
  collection method applied its R metadata. Source:
  [Arrow 25.0.1 collection methods](https://github.com/apache/arrow/blob/apache-arrow-25.0.1/r/R/dplyr-collect.R).

## Consequence for specifying Margin operations

The public references did not provide a single native verb that simultaneously
represented generating several grouping sets, inserting Margin values,
combining the branches, and finalizing a Margin result. In particular,
`mutate()` did not duplicate rows or nest them, while `nest()` was itself a
summarising operation. This was an inference from the documented operation
shapes above, not a claim that upstream guaranteed every composition's
metadata behavior.

Therefore "the same rules as mutate" alone could not determine table-wide
attributes after extra combination or nesting. A concrete delegation statement
needed to identify the actual backend and operations, including combination
and collection; alternatively marginplyr could choose and document an explicit
new dataset-metadata guarantee. The evidence did not select that product
decision. Arbitrary provenance or row-count attributes also could not be
assumed valid merely because an upstream primitive happened to copy them.

## Versioned marginplyr and native-operation measurements

The [companion R probe](upstream-table-attributes-2026-11-10.R) measured
marginplyr at worktree HEAD `4b65d6ce6460d887d6502be6e042fa5cb081073f`, whose
package source was unchanged from `cadf57fdf7b1c0b9f149736a49e6edf829ded317`.
It used local plain data frames, tibbles and a plain-data-frame subclass with
no custom reconstruction method; dtplyr and Arrow probes started from the same
tibble. Each input had `label = "Source dataset"`, `source = "Study A"`, and
`row_count = 3`, with the last attribute defined to describe the current number
of rows. The three attributes had the same retention pattern in these probes.

The environment was macOS arm64, R 4.6.1, dplyr 1.2.1, vctrs 0.7.3,
tibble 3.3.1, tidyr 1.3.2, dtplyr 1.3.3, data.table 1.18.6.1 and Arrow 25.0.1.
The run returned exit 0 with 120 recorded table objects and no errors:

```sh
Rscript investigation/upstream-table-attributes-2026-11-10.R /private/tmp/marginplyr-upstream-table-attributes-20261110-final
```

`table-attributes.csv`, `errors.csv` and `session-info.txt` were saved in that
output directory. Lazy results were observed after direct `collect()`; already
local results were inspected without an additional conversion. Every nested
frame was recorded separately from its outer result.

### Native tibble controls

| Operation | Outer attributes | Inner attributes |
| --- | --- | --- |
| `dplyr::mutate()` | Retained | Not applicable |
| `dplyr::summarise()` | Absent | Not applicable |
| `dplyr::bind_rows(input, input)` | Retained | Not applicable |
| `dplyr::union_all(input, input)` | Retained | Not applicable |
| `vctrs::vec_rbind(input, input)` | Absent | Not applicable |
| `tidyr::nest(input, .by = g)` | Absent | Retained in both cells |
| `dplyr::nest_by(input, g)` | Absent | Absent in both cells |

The row-binding controls produced six rows while the retained `row_count`
still said three. Native tidyr nesting produced two-row and one-row inner
frames, each still carrying `row_count = 3`. This illustrated that preservation
by a primitive did not establish validity for a row-dependent attribute.

The subclass control differed: native `mutate()` retained its class but lost
the three custom attributes, whereas `bind_rows()`, `union_all()` and direct
`vec_rbind()` retained them. These measurements were not a guarantee about all
subclasses or a substitute for class-specific methods.

### marginplyr controls

| API, local plain frame or tibble | One `grouping_set(g)` | Two-set `rollup(g)` |
| --- | --- | --- |
| Summary | Outer attributes absent | Outer attributes absent |
| Expansion | Outer attributes retained | Outer attributes absent |
| Nesting | Outer and inner attributes absent | Outer and inner attributes absent |
| Rowwise nesting | Outer and inner attributes absent | Outer and inner attributes absent |

Subclass expansion lost the custom attributes in both plans. In the lazy
controls, dtplyr expansion retained them for one set and lost them for two;
Arrow expansion retained them for both. Summary lost them on both backends.
Native lazy roundtrip and mutation retained them. Native `union_all()` lost
them for dtplyr and retained them for Arrow. Native dtplyr nesting used
`tidyr::nest(source, data = -g)`: outer attributes were absent and the two inner
data tables retained them. The `.by` syntax was not used as an equivalent
dtplyr nesting control.

These were observations of the tested versions, classes and expressions.
The probe did not cover every custom subclass, Arrow Dataset/RecordBatch
input, file round trip or SQL driver.

### Why the actual marginplyr composition mattered

At the examined package source, `combine_margin_branches()` returned a single
branch unchanged. Multiple eager branches went through `bind_margin_branches()`
and its direct `vctrs::vec_rbind()`; lazy branches went through their backend's
union path. This explained why the one-set local expansion and native local
`union_all()` controls retained attributes while multi-set local expansion did
not. Source: the named functions in
[`R/grouping-adapter-union.R` at the examined source](https://github.com/sayuks/marginplyr/blob/cadf57fdf7b1c0b9f149736a49e6edf829ded317/R/grouping-adapter-union.R).

Nesting used `execute_margin_nest()` to expand the grouping sets and
`nest_expanded_margins()` to summarise those rows into list-column cells.
`nest_cell_expr()` built fresh cells from column lists through stock tibble or
data.table converters. It was not a direct call to native `tidyr::nest()`.
Finalization ungrouped, restored factors and placed public columns; rowwise
nesting added rowwise grouping. Sources: the named functions in
[`R/nest_with_margins.R`](https://github.com/sayuks/marginplyr/blob/cadf57fdf7b1c0b9f149736a49e6edf829ded317/R/nest_with_margins.R),
[`finalize_margin_operation()`](https://github.com/sayuks/marginplyr/blob/cadf57fdf7b1c0b9f149736a49e6edf829ded317/R/margin-operation.R)
and
[`R/nest_by_with_margins.R`](https://github.com/sayuks/marginplyr/blob/cadf57fdf7b1c0b9f149736a49e6edf829ded317/R/nest_by_with_margins.R).

Thus a specification referring only to the last verb, or promising identical
arbitrary attributes to one named native verb, would not describe this
composition. The precise policy choice remained whether marginplyr should
independently restore particular original-frame attributes. If that restoration
was not selected, the contract could state that no dataset-wide preservation
guarantee was added, while leaving attributes the actual operations retained
in place. That would be an explicit marginplyr boundary rather than an
unspecified universal upstream rule.
