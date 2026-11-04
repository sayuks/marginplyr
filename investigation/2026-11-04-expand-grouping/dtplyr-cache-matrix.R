pkgload::load_all(quiet = TRUE)
shapes <- list(
  filter = function(source, name) {
    dplyr::compute(dplyr::filter(source, TRUE), name = name)
  },
  nested_cache = function(source, name) {
    parent <- dplyr::compute(dplyr::filter(source, TRUE), name = "inner")
    dplyr::compute(dplyr::filter(parent, TRUE), name = name)
  },
  mutate = function(source, name) {
    dplyr::compute(dplyr::mutate(source, row_id = row_id + 1L), name = name)
  }
)
cases <- expand.grid(
  n = c(0L, 1L, 3L),
  shape = names(shapes),
  name = c("ordinary_cache", "copy"),
  plan = c("null", "included", "rollup", "omitted_first"),
  label = c("typed", "text"),
  sort = c("none", "last", "first"),
  stringsAsFactors = FALSE
)
plans <- list(
  null = NULL,
  included = grouping_set(g),
  rollup = rollup(g),
  omitted_first = grouping_sets(grouping_set(), grouping_set(g))
)
normalize <- function(result, sort) {
  if (sort == "none") {
    result <- dplyr::arrange(result, .data$set, .data$row_id)
  }
  result
}
run <- function(n, shape, name, plan, label, sort) {
  rows <- seq_len(n)
  input <- data.table::data.table(g = c("b", "a", NA_character_)[rows],
                                 row_id = rows)
  before <- data.table::copy(input)
  source <- shapes[[shape]](dtplyr::lazy_dt(input), name)
  native <- dplyr::collect(source)
  expected_rows <- if (shape == "mutate") rows + 1L else rows
  stopifnot(identical(native, tibble::tibble(
    g = before$g, row_id = expected_rows
  )))
  spec <- plans[[plan]]
  label <- if (label == "typed") NULL else "Total"
  expected <- expand_with_margins(native, .grouping = spec, .id = "set",
                                  .margin_label = label, .sort = sort)
  result <- tryCatch({
    query <- expand_with_margins(source, .grouping = spec, .id = "set",
                                 .margin_label = label, .sort = sort)
    stopifnot(inherits(query, "dtplyr_step"))
    dplyr::collect(query)
  }, error = identity)
  stopifnot(identical(input, before), identical(dplyr::collect(source), native))
  if (inherits(result, "error")) {
    if (grepl('could not find function "copy"', conditionMessage(result),
              fixed = TRUE)) {
      return("copy error")
    }
    return(conditionMessage(result))
  }
  if (identical(normalize(result, sort), normalize(expected, sort))) {
    "matches"
  } else {
    "mismatch"
  }
}
results <- do.call(mapply, c(list(FUN = run), as.list(cases),
                             list(USE.NAMES = FALSE)))
print(table(cases$name, results))
stopifnot(all(results[cases$name == "ordinary_cache"] == "matches"))
if ("--assert" %in% commandArgs(TRUE)) stopifnot(all(results == "matches"))
