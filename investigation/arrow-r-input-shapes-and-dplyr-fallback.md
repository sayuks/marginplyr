# Arrow R input shapes and dplyr fallback

Investigated: 2026-09-08
Revised: 2026-09-09 — multi-branch RecordBatchReader reproduction below

Measured with R 4.6.1, arrow 25.0.1, and dplyr 1.2.1.  The Arrow sources and
reference pages linked below were read on the investigation date.  This note
records evidence; `R/grouping-backend.R`, `R/utils.R`, and the tests are
authoritative for marginplyr's present admission policy.

## Scope and the complete Arrow R class split

Arrow's [data-objects article](https://arrow.apache.org/docs/r/articles/data_objects.html)
lists its tabular objects as `RecordBatch`, `Table`, and `Dataset`; `Scalar`,
`Array`, and `ChunkedArray` are respectively scalar or one-dimensional, while
`Schema`, `Field`, and `DataType` are metadata.  They are therefore not
two-dimensional dplyr inputs.  `ArrowObject` is the common R6 base class, not
a tabular-input contract.

The Arrow dplyr implementation itself has a more exact admission list:
[`arrow_dplyr_query()` in `r/R/dplyr.R`](https://github.com/apache/arrow/blob/main/r/R/dplyr.R)
accepts `Dataset`, `RecordBatch`, `RecordBatchReader`, `Table`, an existing
`arrow_dplyr_query`, and `data.frame` (which it first converts to `Table`).
Thus `ArrowObject` is not a valid shorthand for an Arrow dplyr input.

`Scanner` is a distinct execution object.  Its
[reference](https://arrow.apache.org/docs/r/reference/Scanner.html) documents
`Scanner$create()` as accepting a `Dataset` or Dataset-based query, and its
`$ToTable()` and `$ToRecordBatchReader()` methods as the ways to materialize or
stream the scan.  It is not in Arrow's dplyr-query admission list.  The
low-level path is consequential: the data-objects article states that
`$ToTable()` materializes the Dataset, whereas `$ScanBatches()` executes the
scan and returns batches.

## Arrow's native dplyr behaviour

The official [dplyr article](https://arrow.apache.org/docs/r/articles/data_wrangling.html)
states that one-table verbs construct an `arrow_dplyr_query` lazily; `compute()`
materializes an Arrow `Table`, and `collect()` returns an R data frame/tibble.
The [Acero reference](https://arrow.apache.org/docs/r/reference/acero.html)
enumerates the dplyr verbs and mapped functions available in those queries.
It is a support list, not an assertion that every R expression is translated.

For an unsupported expression, the same official article draws this boundary:

| Source passed to Arrow dplyr | Arrow's documented or source-defined result |
| --- | --- |
| `Table` | Warn, automatically `collect()` to R, then run that verb in R. |
| `RecordBatch` | Same in the Arrow implementation: it is an in-memory tabular source on the shared query-construction path. The current article names only `Table`, so this part is source-backed and measured rather than explicitly documented prose. |
| `Dataset` | Raise an unsupported-expression error; the caller must explicitly `collect()` before the unsupported verb. |
| `RecordBatchReader` | Dataset-side behaviour: error rather than automatic fallback. In `r/R/dplyr.R`, `query_on_dataset()` treats it with `Dataset`; the source comments that a reader's stream is consumed. This is source-backed and measured, but not stated in the prose article. |
| `arrow_dplyr_query` | It inherits the behaviour of its source: queries over a `Table` or `RecordBatch` may fall back; queries over `Dataset` or `RecordBatchReader` error. The query class alone is insufficient to tell the two outcomes apart. |

The article explicitly explains the Table fallback and Dataset error, including
the instruction to put `collect()` in the middle of a Dataset pipeline.  The
same split is implemented in the official source's `query_on_dataset()` and
fallback handler.  Arrow's `NEWS.md` also records that `RecordBatchReader` was
added as a dplyr-query source in Arrow 8.0.0:
[source](https://github.com/apache/arrow/blob/main/r/NEWS.md).

## Local reproduction on the investigation date

For `summarise(z = paste(a, collapse = ","))`, Arrow 25.0.1 gave the following
results over a two-column, two-row input:

| Input | Result |
| --- | --- |
| `Table` | warning ending in “Pulling data into R”; local tibble result |
| `RecordBatch` | same warning and local tibble result |
| `InMemoryDataset` | Arrow unsupported-expression error telling the caller to `collect()` first |
| `RecordBatchReader` | same Arrow error, without an automatic collection |

On the same installation, `summarize_with_margins()` accepted `Table`,
`RecordBatch`, and `InMemoryDataset` and returned an `arrow_dplyr_query` for a
translatable `n()` summary.  It rejected `RecordBatchReader` before Arrow
constructs a query, with the documented remedy `arrow::as_arrow_table()`;
it rejected `Scanner` because it has no dplyr `group_vars()` method.  The
repository's test `"public Arrow table classes are supported"` records the
same allowed/rejected boundary.

## Consequences for a complete marginplyr input contract

The implementable categories are not just “Arrow table” versus “Arrow
dataset”.  Documentation and tests need to state all of these distinctions:

1. Accept `Table`, `RecordBatch`, `Dataset`, and `arrow_dplyr_query`; describe
   an `arrow_dplyr_query` by its source, not as one uniform fallback class.
2. Reject `RecordBatchReader` deliberately, even though Arrow itself can build
   a dplyr query from it.  It is single-pass and has Dataset-style unsupported
   expression failure; conversion to a `Table` is an explicit materialization
   choice.
3. Reject `Scanner` as a dplyr input; direct its caller to `$ToTable()` or
   `$ToRecordBatchReader()` and then make the resulting materialization/stream
   choice explicit.
4. Reject or leave to ordinary dplyr dispatch every non-tabular Arrow class
   (`Scalar`, `Array`, `ChunkedArray`, metadata classes, and bare
   `ArrowObject`); none is an Arrow R tabular/dplyr source.

The uncertainty is intentional and bounded.  Arrow's official prose documents
the fallback guarantee only as `Table` versus `Dataset`; it does not enumerate
the corresponding `RecordBatch`, `RecordBatchReader`, and query-over-source
rules.  Those rows rest on the current first-party implementation plus the
local 25.0.1 reproduction.  Since Arrow can add compute kernels or change this
internal classification, the supported-expression set and the fallback wording
must be checked against the Arrow version actually supported by marginplyr.

## Revisions (2026-09-09)

Arrow 25.0.1 correctly consumed both batches when one summary query was built
from a `RecordBatchReader`: two batches holding two and three rows produced a
count of five and a sum of fifteen. It did not make that reader reusable. A
second summary query executed against the same reader returned a count and sum
of zero.

The Margin-operation shape reproduced the original wrong-result reason. A
grouped summary and a grand-total summary were built lazily from the same
reader and combined with `dplyr::union_all()`. The input held `E` in the first
two-row batch and `W` in the second three-row batch. The complete answer was
`E = 2`, `W = 3`, and `Total = 5`; Arrow instead returned `W = 3` and
`Total = 2`. Thus constructing both queries before execution did not turn the
one-shot source into one shared scan.

`arrow::as_arrow_table()` consumed those same two batches once and produced a
five-row table whose value column summed to fifteen. The conversion is therefore
a valid remedy for the summary, expansion, and inspection guards: every branch
then refers to a reusable table rather than to the reader. It is not the direct
remedy for a nesting verb, because an Arrow table still cannot carry its list
column; that verb's existing `dplyr::collect()` remedy consumes the reader into
a local data frame instead.
