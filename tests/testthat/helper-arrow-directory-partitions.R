# Positional partition values differ from both stored values and occurrence IDs.
arrow_directory_fixture <- function(partitions, collision = character(),
                                    empty = FALSE) {
  path <- tempfile("margin-directory-")
  public <- c("g", "row_id", collision)
  public_fields <- stats::setNames(
    rep(list(arrow::int32()), length(public)), public
  )
  partition_fields <- stats::setNames(
    rep(list(arrow::int32()), length(partitions)), partitions
  )
  partitioning <- arrow::DirectoryPartitioning$create(
    arrow::schema(!!!partition_fields)
  )
  schema <- arrow::schema(!!!public_fields)
  for (i in 1:2) {
    directory <- do.call(file.path, as.list(c(
      path, rep(as.character(8L - i), length(partitions))
    )))
    dir.create(directory, recursive = TRUE)
    data <- data.frame(g = c(10L - i, NA_integer_, 10L - i), row_id = i)
    for (name in collision) data[[name]] <- i + 90L
    if (empty) data <- data[FALSE, , drop = FALSE]
    arrow::write_parquet(data, file.path(directory, "facts.parquet"))
  }
  full <- arrow::open_dataset(path, partitioning = partitioning)
  explicit <- arrow::open_dataset(
    path, schema = schema, partitioning = partitioning
  )
  sources <- list(
    explicit = explicit,
    replaced = full$WithSchema(schema),
    filtered = dplyr::filter(explicit, .data$row_id > 0L),
    union_query = dplyr::union_all(
      arrow::Table$create(schema = schema), explicit
    ),
    union_dataset = arrow::open_dataset(list(
      arrow::InMemoryDataset$create(arrow::Table$create(schema = schema)),
      explicit
    ), schema = schema)
  )
  list(path = path, full = full, sources = sources, public = public)
}

# The collected public input is the independent eager oracle; the literal
# one-row regression separately pins the two occurrence identifiers.
expect_directory_expansion <- function(source, id) {
  data <- dplyr::collect(source)
  grouping <- grouping_sets(
    grouping_set("g"), grouping_set(), grouping_set("g")
  )
  for (label in list(NULL, "Total")) {
    for (sort in c("none", "first", "last")) {
      info <- paste(label, sort, id)
      query <- expand_with_margins(
        source, .grouping = grouping, .duplicates = "keep",
        .margin_label = label, .sort = sort, .id = id
      )
      expected <- expand_with_margins(
        data, .grouping = grouping, .duplicates = "keep",
        .margin_label = label, .sort = sort, .id = id
      )
      expect_s3_class(query, "arrow_dplyr_query")
      expect_identical(names(query), names(expected), info = info)
      actual <- dplyr::collect(query)
      expect_identical(arrow_partition_bag(actual),
                       arrow_partition_bag(expected), info = info)
      if (sort != "none") {
        expect_identical(actual$g, expected$g, info = info)
        if (!is.null(id)) {
          expect_identical(actual[[id]], expected[[id]], info = info)
        }
      }
    }
  }
}
