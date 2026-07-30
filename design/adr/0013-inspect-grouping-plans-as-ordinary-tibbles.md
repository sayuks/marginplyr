# Inspect Grouping plans as ordinary tibbles

`inspect_grouping()` will expose the backend-independent Grouping plan as a
local, ungrouped tibble rather than as a custom class or an execution-plan
display. This keeps the inspected data usable in IDE viewers and ordinary
dplyr, export, and snapshot-testing workflows. Query inspection remains the
responsibility of `dplyr::show_query()` and backend tools.

## Interface

```r
inspect_grouping(
  .data,
  .by = NULL,
  .grouping = NULL,
  .duplicates = c("error", "drop", "keep"),
  .format = c("text", "list")
)
```

The function uses the same grouped-input, `.by`, `.grouping`, duplicate,
row-wise-input, and `.drop = FALSE` validation rules as Margin operations. It
does not aggregate or query source rows, inspect Margin label collisions, or
accept display-label, `.id`, or completion arguments. A lazy input still
returns a local tibble because only captured column metadata and the compiled
Grouping plan are represented.

There is one row per grouping-set occurrence after applying `.duplicates`.
Rows are guaranteed to appear in Grouping-plan order, and `set_id` therefore
increases from `1L` through the number of retained occurrences. This
deterministic inspection order does not imply a row-order guarantee for
Margin-operation results.

The columns are:

- `set_id`, the Grouping set identifier that the corresponding Margin
  operation would expose through `.id`;
- `fixed`, the `.by` or pre-existing grouped columns;
- `included`, the variable dimensions present in this occurrence;
- `omitted`, the variable dimensions absent from this occurrence;
- `grouping_bits`, in Grouping-plan dimension order, with `0L` for an included
  dimension and `1L` for an omitted dimension; and
- `grouping_id`, the SQL-compatible bit mask for those dimensions.

With `.format = "text"`, `fixed`, `included`, and `omitted` use a
parenthesized Grouping set display: `()` for zero columns, `(region)` for one,
and `(region, store)` for more than one. Names appear in Grouping-plan order
without quotes or backticks. `grouping_bits` is named text such as
`region=0, store=0, product=1`, without outer parentheses.

The text form is a human-readable display, not a serialization format. A
caller that needs exact names, including names containing separators, uses
`.format = "list"`. In that form the three collections are character
list-columns and `grouping_bits` is a named integer-vector list-column; empty
values are `character()` and `integer()`. The text format is the default
because IDE viewers commonly do not expose list-column contents directly.

`grouping_id` is an integer through 31 variable dimensions and
`NA_integer_` beyond that limit. `grouping_bits` remains complete at any
dimension count, so inspection does not lose the underlying absence pattern.
With no variable dimensions, the text representation is `()` and the list
representation is `integer()`.

The result has only the ordinary `tbl_df`, `tbl`, and `data.frame` classes and
no custom printer. A future tree or colorized view will be a separate display
function over this tibble rather than hidden behavior attached to its class.
