# Independent public-seam validation of the two dated candidate findings.
pkgload::load_all(".", quiet = TRUE)
source("tests/testthat/helper-optional-backends.R")
stopifnot(suggest_available("dtplyr"))
args <- commandArgs(trailingOnly = TRUE)
new_fold_factor <- function(na_level) {
  x <- factor(c("low-1", "low-2"),
              levels = c("low-1", "low-2", if (na_level) NA_character_), exclude = NULL)
  class(x) <- c("hunt_fold_factor", "factor")
  x
}
registerS3method("vec_proxy_equal", "hunt_fold_factor", function(x, ...) {
  class(x) <- "factor"
  sub("-[0-9]+$", "", as.character(x))
}, asNamespace("vctrs"))
registerS3method("vec_ptype2", "hunt_fold_factor.hunt_fold_factor", function(x, y, ...) {
  vctrs::vec_slice(x, integer())
}, asNamespace("vctrs"))
registerS3method("vec_cast", "hunt_fold_factor.hunt_fold_factor", function(x, to, ...) x,
                 asNamespace("vctrs"))
input <- tibble::tibble(g = new_fold_factor(TRUE), row_id = 1:2)
membership <- function(x) lapply(x$data, function(cell) cell$row_id)
stopifnot(identical(vctrs::vec_group_loc(input$g)$loc, list(1:2)),
          identical(membership(dplyr::nest_by(input, g)), list(1:2)))
combined <- dplyr::union_all(input, input)
stopifnot(inherits(combined$g, "hunt_fold_factor"),
          identical(vctrs::vec_group_loc(combined$g)$loc, list(1:4)))
factor_ok <- TRUE
for (verb in list(nest_with_margins, nest_by_with_margins)) {
  for (typed in c(FALSE, TRUE)) {
    result <- verb(input, .grouping = grouping_set(g), .keep = TRUE,
                   .margin_label = if (typed) NULL else "Total")
    cat("factor NA level, typed label", typed, ":")
    print(membership(result))
    factor_ok <- factor_ok && identical(membership(result), list(1:2))
    no_na <- input
    no_na$g <- new_fold_factor(FALSE)
    stopifnot(identical(membership(verb(no_na, .grouping = grouping_set(g))), list(1:2)),
              identical(membership(verb(input, .by = g)), list(1:2)))
  }
}
plain <- tibble::tibble(g = c("a", "b"), row_id = 1:2)
lazy <- dtplyr::lazy_dt(plain)
min_calls <- 0L
min <- function(...) { min_calls <<- min_calls + 1L; 0L }
stopifnot(identical(dplyr::collect(lazy), plain))
upstream <- dplyr::collect(dplyr::summarize(lazy, n = dplyr::n(), .by = g))
stopifnot(identical(upstream$g, plain$g), min_calls == 0L)
dt_ok <- TRUE
for (verb in list(nest_with_margins, nest_by_with_margins)) {
  local <- verb(plain, .grouping = grouping_set(g))
  stopifnot(identical(local$g, plain$g))
  result <- dplyr::collect(verb(lazy, .grouping = grouping_set(g)))
  cat("dtplyr displayed keys:")
  print(result$g)
  stopifnot(identical(membership(result), list(1L, 2L)))
  dt_ok <- dt_ok && identical(result$g, plain$g)
}
rm(min)
cat("caller min invoked", min_calls, "times; ordinary source, upstream summary, and local nesting controls passed\n")
stopifnot(min_calls > 0L)
if ("--assert-factor" %in% args) stopifnot(factor_ok)
if ("--assert-dtplyr" %in% args) stopifnot(dt_ok)
if ("--assert" %in% args) stopifnot(factor_ok, dt_ok) else {
  stopifnot(!factor_ok, !dt_ok)
  cat("DATED OBSERVATION: both independent findings reproduced\n")
}
