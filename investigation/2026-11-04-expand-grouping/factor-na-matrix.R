pkgload::load_all(quiet = TRUE)

is.na.display_na_factor <- function(x) is.na(as.character.factor(x))
vctrs::s3_register("vctrs::vec_ptype2", "display_na_factor.display_na_factor",
                   function(x, y, ...) vctrs::vec_slice(x, integer()))
vctrs::s3_register("vctrs::vec_cast", "display_na_factor.display_na_factor",
                   function(x, to, ...) x)

verbs <- list(
  expand = expand_with_margins,
  summary = function(data, ...) {
    summarize_with_margins(data, row_id = dplyr::first(row_id), ...)
  },
  nest = nest_with_margins,
  nest_by = nest_by_with_margins
)
cases <- expand.grid(
  backend = c("local", "dtplyr"),
  verb = names(verbs),
  type = c("plain_na_level", "custom_no_na_level", "custom_na_level"),
  n = c(0L, 1L, 3L),
  label = c("typed", "text"),
  position = c("first", "last"),
  plan = c("included", "rollup"),
  stringsAsFactors = FALSE
)
plans <- list(included = grouping_set(g), rollup = rollup(g))
run <- function(backend, verb, type, n, label, position, plan) {
  codes <- c(2L, 1L, NA_integer_)[seq_len(n)]
  levels <- c("a", if (type == "custom_no_na_level") "b" else NA_character_)
  classes <- c(if (type != "plain_na_level") "display_na_factor", "factor")
  input <- tibble::tibble(
    g = structure(codes, levels = levels, class = classes),
    row_id = seq_len(n)
  )
  before <- serialize(input, NULL)
  source <- if (backend == "dtplyr") dtplyr::lazy_dt(input) else input
  native <- dplyr::collect(source)
  stopifnot(identical(as.integer(native$g), codes))
  group_control <- dplyr::summarise(input, row_id = dplyr::first(row_id), .by = g)
  stopifnot(identical(as.integer(group_control$g), codes))
  result <- verbs[[verb]](
    source, .grouping = plans[[plan]], .id = "set",
    .margin_label = if (label == "typed") NULL else "Total",
    .margin_label_position = position
  )
  result <- dplyr::ungroup(dplyr::collect(result))
  detail <- result[result$set == 1L, ]
  detail_rows <- if ("data" %in% names(detail)) {
    vapply(detail$data, function(cell) cell$row_id[[1L]], integer(1))
  } else {
    detail$row_id
  }
  if (!identical(sort(detail_rows), seq_len(n))) {
    print(list(backend, verb, type, n, label, position, plan, detail_rows))
  }

  stopifnot(identical(sort(detail_rows), seq_len(n)),
            identical(serialize(input, NULL), before),
            identical(dplyr::collect(source), native))
  offset <- as.integer(label == "text" && position == "first")
  actual <- as.integer(detail$g)
  expected <- codes[detail_rows] + offset
  expected_levels <- if (label == "typed") {
    levels
  } else if (position == "first") {
    c("Total", levels)
  } else {
    c(levels, "Total")
  }
  stopifnot(identical(levels(result$g), expected_levels))
  if (identical(actual, expected)) return("matches")
  if (identical(actual, replace(expected, codes[detail_rows] %in% 2L, NA))) {
    return("lost NA-level code")
  }
  "other mismatch"
}
results <- do.call(mapply, c(list(FUN = run), as.list(cases),
                             list(USE.NAMES = FALSE)))
print(table(cases$type, cases$backend, results))
print(table(cases$verb, results))
stopifnot(all(results[cases$type != "custom_na_level"] == "matches"))
if ("--assert" %in% commandArgs(TRUE)) stopifnot(all(results == "matches"))
