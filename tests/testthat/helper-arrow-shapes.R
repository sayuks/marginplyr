# The Arrow input shapes `test-grouping-backends.R` and `test-query-policy.R`
# both take. testthat evaluates each test file in its own environment and only
# a `helper-*.R` file is sourced into the shared one, so this is where a shape
# two suites need is built once (#313).
#
# The two suites assert different things over the same objects:
# `test-grouping-backends.R` which condition each shape raises, and
# `test-query-policy.R` how many reads each one causes. The second file's
# comments cite the first file's coverage, and that citation holds while both
# build their shapes here.

# A group column, a numeric column, and a string column: what the summary
# expressions written over this input in both suites read.
arrow_input_data <- function() {
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
