# Dated excluded lead: Arrow decimal64 order also differs in ordinary controls.
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all('.', quiet = TRUE)
rows <- list()
i <- 0L
for (type in list(arrow::decimal32(9, 2), arrow::decimal64(18, 4), arrow::decimal128(18, 4))) {
  array <- arrow::Array$create(c(2.25, 1.125), type = type)
  source <- arrow::Table$create(g = array)
  missing <- arrow::Scalar$create(NA, type = arrow::null())$cast(type)
  combined <- union_all(mutate(source, sid = 1L), mutate(source, g = missing, sid = 2L))
  controls <- list(simple = arrange(source, g),
    missing = arrange(source, is.na(g), g),
    union_simple = arrange(combined, sid, g),
    union_full = arrange(combined, sid, is.na(g), g),
    package = expand_with_margins(source, .grouping = rollup(g),
      .margin_label = NULL, .id = 'sid', .sort = 'last'))
  answers <- lapply(controls, function(query) tryCatch(collect(query)$g, error = identity))
  stopifnot(identical(answers$package, answers$union_full),
    arrow::schema(controls$package)$GetFieldByName('g')$type$Equals(type))
  cat('\n', type$ToString(), '\n')
  for (name in names(answers)) {
    result <- answers[[name]]
    cat(name, ': ')
    if (inherits(result, 'error')) cat(conditionMessage(result), '\n') else dput(result)
    i <- i + 1L
    rows[[i]] <- data.frame(type = type$ToString(), query = name,
      values = if (inherits(result, 'error')) '' else paste(result, collapse = ','),
      error = if (inherits(result, 'error')) conditionMessage(result) else '')
  }
}
cat('Three ordinary full-key union controls matched package results and physical types.\n')
