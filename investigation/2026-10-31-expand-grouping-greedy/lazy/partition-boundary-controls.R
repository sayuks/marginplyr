# Dated controls for hidden names colliding with the deferred Arrow boundary.
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all('.', quiet = TRUE)
checks <- 0L
for (name in c('..marginplyr_arrow_boundary_1', '..marginplyr_arrow_boundary_1_', 'ordinary')) {
  for (type in c('integer', 'character')) {
    path <- tempfile('marginplyr-boundary-')
    dir.create(path)
    data <- tibble(g = c(9L, 8L), rid = 1:2)
    data[[name]] <- if (type == 'integer') 1:2 else c('a', 'b')
    arrow::write_dataset(data, path, partitioning = name)
    options <- list(selector_ignore_prefixes = character())
    full <- arrow::open_dataset(path, factory_options = options)
    narrow <- arrow::open_dataset(path, factory_options = options,
      schema = arrow::schema(g = arrow::int32(), rid = arrow::int32()))
    for (shape in c('full', 'narrow')) {
      source <- if (shape == 'full') select(full, g, rid) else narrow
      raw <- collect(source)
      stopifnot(nrow(raw) == 2L, length(full$files) == 2L,
        identical(sort(raw$g), 8:9))
      for (sort in c('none', 'first', 'last')) {
        result <- collect(expand_with_margins(source, .grouping = rollup(g),
          .margin_label = NULL, .id = 'sid', .sort = sort))
        aligned <- result[order(result$sid, result$rid), ]
        stopifnot(identical(names(result), c('g', 'sid', 'rid')),
          identical(aligned$g, c(9L, 8L, NA_integer_, NA_integer_)),
          identical(aligned$sid, rep(1:2, each = 2L)),
          identical(aligned$rid, rep(1:2, 2L)))
        if (sort == 'last') stopifnot(identical(result$g, c(8L, 9L, NA_integer_, NA_integer_)))
        if (sort == 'first') stopifnot(identical(result$g, c(NA_integer_, NA_integer_, 8L, 9L)))
        checks <- checks + 1L
      }
    }
    unlink(path, recursive = TRUE)
  }
}
cat(checks, 'partition boundary cases passed.\n')
