# A partition field absent from the Parquet schema, with an explicit Dataset
# schema: collection cannot mistake a stored column for a recovered partition.
arrow_partition_fixture <- function(p = 1L, row_id = 1L) {
  path <- tempfile("margin-partitions-")
  dir.create(path)
  type <- if (is.integer(p)) arrow::int32() else arrow::utf8()
  files <- vapply(seq_along(p), function(i) {
    directory <- file.path(path, paste0("p=", p[[i]]))
    dir.create(directory, showWarnings = FALSE)
    file <- file.path(directory, paste0("row-", i, ".parquet"))
    arrow::write_parquet(data.frame(row_id = row_id[[i]]), file,
                         chunk_size = 1L, compression = "uncompressed")
    file
  }, character(1))
  schema <- arrow::schema(row_id = arrow::int32(), p = type)
  source <- arrow::open_dataset(
    path, schema = schema, partitioning = arrow::hive_partition(p = type)
  )
  expect_setequal(normalizePath(source$files), normalizePath(files))
  expect_length(unique(dirname(source$files)), length(unique(p)))
  expect_true(source$schema$Equals(schema))
  for (file in files) {
    reader <- arrow::ParquetFileReader$create(file)
    expect_identical(reader$num_rows, 1L)
    expect_identical(reader$num_row_groups, 1L)
    expect_true(reader$GetSchema()$Equals(
      arrow::schema(row_id = arrow::int32())
    ))
  }
  expected <- data.frame(row_id = row_id, p = p)
  actual <- as.data.frame(dplyr::collect(source))
  actual <- actual[order(actual$row_id), names(expected), drop = FALSE]
  rownames(actual) <- NULL
  expect_identical(actual, expected)
  list(path = path, source = source, data = expected, files = files,
       hashes = tools::md5sum(files), schema = schema)
}

# Sorting a literal expected bag keeps duplicate rows and compares R types.
arrow_partition_bag <- function(data) {
  data <- as.data.frame(data)
  positions <- do.call(order, c(unname(data), list(na.last = TRUE)))
  data <- data[positions, , drop = FALSE]
  rownames(data) <- NULL
  data
}

# Ordinary columns in Parquet and in memory represent the identical source bag.
arrow_partition_controls <- function(fixture) {
  path <- file.path(fixture$path, "ordinary")
  dir.create(path)
  for (i in seq_len(nrow(fixture$data))) {
    arrow::write_parquet(
      fixture$data[i, ], file.path(path, paste0(i, ".parquet"))
    )
  }
  dataset <- arrow::open_dataset(path, schema = fixture$schema)
  expect_length(dataset$files, nrow(fixture$data))
  expect_identical(arrow_partition_bag(dplyr::collect(dataset)),
                   arrow_partition_bag(fixture$data))
  list(partition = fixture$source,
       partition_query = dplyr::filter(fixture$source, .data$row_id > 0L),
       ordinary_dataset = dataset,
       table = arrow::Table$create(fixture$data, schema = fixture$schema))
}
