# Ordinary Table and file-backed Dataset inputs with the same declared schema.
arrow_reexpansion_sources <- function(path, empty = FALSE) {
  data <- data.frame(g = c("b", "a"), h = c(2L, 1L), payload = c(20, 10))
  if (empty) data <- data[0L, ]
  arrow::write_parquet(data, path)
  list(table = arrow::Table$create(data), dataset = arrow::open_dataset(path))
}

# Both first operations retain h as a fixed key and carry the same payloads.
arrow_reexpansion_first <- function(source, summary, sort, id) {
  if (summary) {
    summarize_with_margins(
      source, payload = sum(.data$payload), .by = dplyr::all_of("h"),
      .grouping = rollup(dplyr::all_of("g")),
      .margin_label = NULL, .sort = sort, .id = id
    )
  } else {
    expand_with_margins(
      source, .by = dplyr::all_of("h"),
      .grouping = rollup(dplyr::all_of("g")), .margin_label = NULL,
      .sort = sort, .id = id
    )
  }
}
