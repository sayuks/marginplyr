# Physical dimensions and hidden Hive fields carry different values.
arrow_hidden_partition_fixture <- function(partitions, empty = FALSE) {
  path <- tempfile("margin-hidden-partitions-")
  dir.create(path)
  for (i in 1:2) {
    directory <- do.call(file.path, as.list(c(
      path, paste0(partitions, "=", i)
    )))
    dir.create(directory, recursive = TRUE)
    data <- data.frame(g = 10L - i, row_id = i)
    if (empty) data <- data[FALSE, ]
    arrow::write_parquet(data, file.path(directory, "facts.parquet"))
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

# Three Dataset boundaries retain Hive expressions but omit their schema fields.
arrow_narrow_partition_sources <- function(fixture, partitions) {
  schema <- arrow::schema(g = arrow::int32(), row_id = arrow::int32())
  fields <- stats::setNames(
    rep(list(arrow::int32()), length(partitions)), partitions
  )
  list(
    explicit = arrow::open_dataset(
      fixture$path, schema = schema,
      partitioning = arrow::hive_partition(!!!fields),
      factory_options = list(selector_ignore_prefixes = character())
    ),
    replaced = fixture$dataset$WithSchema(schema),
    union = arrow::open_dataset(list(
      arrow::InMemoryDataset$create(arrow::Table$create(schema = schema)),
      fixture$dataset
    ), schema = schema)
  )
}
