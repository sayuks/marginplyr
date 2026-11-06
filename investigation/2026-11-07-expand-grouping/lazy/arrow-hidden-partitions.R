args <- commandArgs(trailingOnly = TRUE)
source_arg <- grep("^--source=", args, value = TRUE)
source <- if (length(source_arg)) sub("^--source=", "", source_arg[[1L]]) else "."
pkgload::load_all(source, quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
out <- dirname(normalizePath(script))
canonical <- function(x) {
  x <- as.data.frame(x)
  x <- x[, c("g", "sid", "rid"), drop = FALSE]
  if (nrow(x)) x <- x[order(x$sid, x$rid), , drop = FALSE]
  rownames(x) <- NULL
  x
}
results <- list()
for (hidden in c("..marginplyr_arrow_boundary_1", "..marginplyr_arrow_dimension_1", ".env", "a%b")) {
  directory <- tempfile(pattern = "marginplyr-hidden-partition-")
  dir.create(directory)
  data <- data.frame(g = c("a", "b", NA_character_), rid = 1:3)
  data[[hidden]] <- c("A", "a/b", NA_character_)
  arrow::write_dataset(data, directory, partitioning = hidden)
  dataset <- arrow::open_dataset(directory, factory_options = list(selector_ignore_prefixes = character()))
  for (shape in c("with_schema", "projection", "right_source_join", "renamed_partition")) {
    input <- switch(shape,
      with_schema = dataset$WithSchema(arrow::schema(g = arrow::utf8(), rid = arrow::int32())),
      projection = dplyr::select(dataset, g, rid),
      right_source_join = dplyr::select(dplyr::left_join(
        arrow::Table$create(data.frame(rid = 1:3)), dataset, by = "rid"
      ), g, rid),
      renamed_partition = dplyr::rename(dplyr::select(dataset, dplyr::all_of(c(hidden, "rid"))),
                                        g = dplyr::all_of(hidden))
    )
    ordinary <- as.data.frame(dplyr::collect(input))
    stopifnot(nrow(ordinary) == 3L)
    for (label in c("missing", "string")) {
      margin <- if (label == "missing") NULL else "Total"
      expected <- ordinary
      expected$sid <- 1L
      second <- ordinary
      second$g <- if (is.null(margin)) NA_character_ else margin
      second$sid <- 2L
      expected <- canonical(rbind(expected, second))
      for (sort in c("none", "first", "last")) {
        query <- expand_with_margins(input, .grouping = rollup(g), .margin_label = margin,
                                     .id = "sid", .sort = sort)
        for (route in c("collect", "compute", "reader")) {
          actual <- switch(route,
            collect = dplyr::collect(query),
            compute = dplyr::collect(dplyr::compute(query)),
            reader = as.data.frame(arrow::as_arrow_table(arrow::as_record_batch_reader(query)))
          )
          equal <- identical(names(actual), c("g", "sid", "rid")) &&
            identical(canonical(actual), expected)
          results[[length(results) + 1L]] <- data.frame(hidden, shape, label, sort, route, equal)
          if (!equal) {
            print(tail(results, 1L)); print(expected); print(canonical(actual))
            stop("Hidden partition expansion mismatch")
          }
        }
      }
    }
  }
  unlink(directory, recursive = TRUE)
}
result <- do.call(rbind, results)
utils::write.csv(result, file.path(out, "arrow-hidden-partitions.csv"), row.names = FALSE)
cat(nrow(result), "hidden/encoded/null partition expansion comparisons passed\n")
