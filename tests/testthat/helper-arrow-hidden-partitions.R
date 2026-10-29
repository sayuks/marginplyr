# Physical dimensions and hidden Hive fields carry different values.
arrow_hidden_partition_fixture <- function(partitions) {
  path <- tempfile("margin-hidden-partitions-")
  dir.create(path)
  for (i in 1:2) {
    directory <- do.call(file.path, as.list(c(
      path, paste0(partitions, "=", i)
    )))
    dir.create(directory, recursive = TRUE)
    arrow::write_parquet(
      data.frame(g = 10L - i, row_id = i), file.path(directory, "facts.parquet")
    )
  }
  fields <- stats::setNames(
    rep(list(arrow::int32()), length(partitions)), partitions
  )
  dataset <- arrow::open_dataset(
    path, factory_options = list(selector_ignore_prefixes = character()),
    schema = arrow::schema(
      g = arrow::int32(), row_id = arrow::int32(), !!!fields
    ),
    partitioning = arrow::hive_partition(!!!fields)
  )
  list(path = path, dataset = dataset,
       source = dplyr::select(dataset, dplyr::all_of(c("g", "row_id"))))
}
