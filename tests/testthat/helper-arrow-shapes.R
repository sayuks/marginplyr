# The Arrow input shapes two suites take, in a helper file because testthat
# evaluates each test file in its own environment and only a `helper-*.R` file
# is sourced into the shared one, so a second suite needing a shape had to
# rebuild it (#313).
#
# `test-grouping-backends.R` asserts which condition each shape raises, and
# `test-query-policy.R` counts the reads each one causes. The second file's
# comments argue from what the first asserts -- its Dataset block says the
# refusal test there "asserts the condition and passes unchanged through such a
# move" -- and that claim holds only while the two build the same shapes. Two
# copies were not required to agree, so nothing failed when they stopped.
#
# What each shape *means* is stated where it is asserted, not here: this file
# builds the objects and the division between them is CONTEXT.md's *Absorbing
# backend*, at the seam ADR 0025 draws it at.

# A group, a numeric column, and a string column: the columns the absorbed
# expressions asserted over need, which is a group collapsed to one string, a
# subset inside an aggregate, and a statistic over two columns.
absorbed_summary_data <- function() {
  data.frame(
    k = c("E", "E", "W"),
    v = c(1, 2, 3),
    s = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
}

# A query rather than the object itself is the third shape, because
# `arrow_dplyr_query` is the one class that appears on both sides of the
# division: over a table it absorbs, over a dataset it refuses. `select()`
# builds one without a data-masked column reference, which keeps these
# helpers readable by `object_usage_linter()`.
absorbing_arrow_inputs <- function(data) {
  table <- arrow::Table$create(data)
  batch <- arrow::record_batch(data)
  list(
    table = table,
    record_batch = batch,
    table_query = dplyr::select(table, dplyr::all_of(names(data))),
    batch_query = dplyr::select(batch, dplyr::all_of(names(data)))
  )
}

# `InMemoryDataset` reaches the `Dataset` class with no file I/O.
refusing_arrow_inputs <- function(data) {
  dataset <- arrow::InMemoryDataset$create(arrow::Table$create(data))
  list(
    dataset = dataset,
    query = dplyr::select(dataset, dplyr::all_of(names(data)))
  )
}
