# Run from the repository root. --assert requires the intended public result.
suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))

capture <- function(expr) tryCatch(force(expr), error = identity)

check_case <- function(name, key) {
  input <- tibble::tibble(key = key, rid = 1L)
  control <- dplyr::union_all(
    dplyr::mutate(input, sid = 1L),
    dplyr::mutate(input, key = vctrs::vec_init(key, 1L), sid = 2L)
  )
  control <- dplyr::relocate(control, sid, .after = key)
  included <- expand_with_margins(
    input, .grouping = grouping_set(key), .margin_label = NULL
  )
  stopifnot(identical(included, input), nrow(control) == 2L)
  actual <- capture(expand_with_margins(
    input, .grouping = rollup(key), .margin_label = NULL,
    .id = "sid", .sort = "none"
  ))
  cat(name, "ordinary rows:", nrow(control), "included-only: match\n")
  if (inherits(actual, "error")) {
    cat(name, "expansion:", class(actual)[[1L]],
        conditionMessage(actual), "\n")
  } else {
    cat(name, "expansion matches ordinary:", identical(actual, control), "\n")
  }
  list(actual = actual, control = control)
}

key <- as.Date("2026-01-01")
dim(key) <- c(1L, 1L)
date_case <- check_case("Date matrix", key)
stopifnot(identical(dim(date_case$control$key), c(2L, 1L)))

factor_key <- factor("a", levels = c("a", "unused"))
dim(factor_key) <- c(1L, 1L)
packed_case <- check_case(
  "Packed factor matrix", tibble::tibble(component = factor_key)
)

prototype <- vctrs::vec_init(key, 1L)
stopifnot(all(is.na(prototype)))
missingness <- capture(vctrs::vec_detect_missing(prototype))
cat("Date prototype missingness:",
    if (inherits(missingness, "error")) class(missingness)[[1L]] else missingness,
    "\n")

if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(
    identical(date_case$actual, date_case$control),
    identical(packed_case$actual, packed_case$control)
  )
}
