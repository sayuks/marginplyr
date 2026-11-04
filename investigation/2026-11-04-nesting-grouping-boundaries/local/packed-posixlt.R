pkgload::load_all(quiet = TRUE)

desired <- "--assert" %in% commandArgs(trailingOnly = TRUE)
capture_error <- function(expr) tryCatch(expr, error = identity)

source <- tibble::tibble(
  g = tibble::tibble(ts = I(as.POSIXlt("2020-01-01", tz = "UTC"))),
  row = 1L
)
normalized <- dplyr::mutate(
  source, g = vctrs::vec_slice(g, seq_len(nrow(source)))
)
omitted <- dplyr::mutate(normalized, g = vctrs::vec_init(g, 1L))
native <- dplyr::union_all(normalized, omitted)
stopifnot(
  nrow(tidyr::nest(source, .by = g)) == 1L,
  nrow(dplyr::nest_by(source, g)) == 1L,
  nrow(native) == 2L,
  identical(native$row, c(1L, 1L)),
  identical(native$g$ts$year, c(120L, NA_integer_))
)
for (verb in list(nest_with_margins, nest_by_with_margins, expand_with_margins)) {
  value <- capture_error(
    verb(source, .grouping = rollup(g), .margin_label = NULL, .id = "set")
  )
  cat(if (inherits(value, "error")) conditionMessage(value) else "success", "\n")
}

operations <- list(
  nest = nest_with_margins,
  rowwise = nest_by_with_margins,
  expand = expand_with_margins
)
matrix_cases <- expand.grid(
  depth = 0:2,
  source_class = c("tibble", "data.frame"),
  scenario = c("empty", "populated", "with_missing"),
  label = c("NULL", "NA"),
  set_order = c("detail_first", "total_first"),
  keep = c(FALSE, TRUE),
  verb = names(operations),
  stringsAsFactors = FALSE
)
matrix_cases <- subset(matrix_cases, verb != "expand" | !keep)

wrap <- function(ts, depth) {
  if (depth >= 1L) {
    ts <- tibble::tibble(ts = ts)
  }
  if (depth == 2L) {
    ts <- tibble::tibble(inner = ts)
  }
  ts
}

strip_asis <- function(x) {
  # vctrs may accumulate equivalent AsIs markers during row slicing.
  if (is.data.frame(x)) {
    x[] <- lapply(x, strip_asis)
  }
  if (inherits(x, "AsIs")) {
    class(x) <- unique(class(x))
  }
  x
}

check_case <- function(case) {
  rows <- switch(
    case$scenario,
    empty = character(),
    populated = c("2020-01-02", "2020-01-01", "2020-01-02"),
    with_missing = c("2020-01-02", NA_character_, "2020-01-02")
  )
  ts <- I(as.POSIXlt(rows, format = "%Y-%m-%d", tz = "UTC"))
  source <- tibble::tibble(g = wrap(ts, case$depth), row = seq_along(rows))
  if (case$source_class == "data.frame") {
    source <- as.data.frame(source)
  }
  original <- serialize(source, NULL)
  normalized <- dplyr::mutate(
    source, g = vctrs::vec_slice(g, seq_len(nrow(source)))
  )
  stopifnot(is.data.frame(normalized$g))

  label <- if (case$label == "NULL") NULL else NA_character_
  detail_first <- case$set_order == "detail_first"
  plan <- if (detail_first) rollup(g) else grouping_sets(grouping_set(), g)
  detail_set <- if (detail_first) 1L else 2L
  verb <- operations[[case$verb]]
  args <- list(.grouping = plan, .margin_label = label, .id = "set")
  if (case$verb != "expand") {
    args$.keep <- case$keep
  }
  run <- function(input, ...) do.call(verb, c(list(input), args, list(...)))
  control <- run(normalized)
  missing <- vctrs::vec_detect_missing(control$g)
  stopifnot(all(missing[control$set != detail_set]))

  if (case$verb == "expand") {
    stopifnot(nrow(control) == 2L * length(rows))
    stopifnot(
      identical(control$row[control$set == detail_set], seq_along(rows)),
      identical(control$row[control$set != detail_set], seq_along(rows))
    )
  } else {
    groups <- vctrs::vec_group_loc(normalized$g)$loc
    actual <- lapply(control$data[control$set == detail_set], "[[", "row")
    signatures <- function(x) sort(vapply(x, paste, "", collapse = ","))
    stopifnot(identical(signatures(actual), signatures(groups)))
    total <- control$data[control$set != detail_set]
    stopifnot(length(total) == as.integer(length(rows) > 0L))
    if (length(total) == 1L) {
      stopifnot(identical(total[[1L]]$row, seq_along(rows)))
    }
    if (case$keep) {
      stopifnot(all(vapply(control$data, function(cell) {
        expected <- vctrs::vec_slice(normalized$g, cell$row)
        identical(strip_asis(cell$g), strip_asis(expected))
      }, logical(1))))
    }
  }

  result <- capture_error(run(source))
  stopifnot(identical(serialize(source, NULL), original))
  if (inherits(result, "error")) {
    stopifnot(
      inherits(result, "vctrs_error_ptype2"),
      grepl("<POSIXlt<UTC>>", conditionMessage(result), fixed = TRUE),
      grepl("<data.frame>", conditionMessage(result), fixed = TRUE)
    )
    return("branch_type_error")
  }
  stopifnot(identical(result$set, control$set))
  stopifnot(identical(strip_asis(result$g), strip_asis(control$g)))
  "match"
}

results <- vapply(seq_len(nrow(matrix_cases)), function(i) {
  check_case(matrix_cases[i, , drop = FALSE])
}, "")
print(with(matrix_cases, table(depth, results)))
cat(R.version.string, "\n")
for (package in c("dplyr", "tidyr", "tibble", "vctrs")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
if (desired) {
  stopifnot(all(results == "match"))
}
