args <- commandArgs(trailingOnly = TRUE)
copy <- args[!grepl("^--", args)]
pkgload::load_all(if (length(copy)) copy[[1L]] else ".", quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
as.character.display_shift_factor <- function(x, ...) {
  c("B", "A")[as.integer(x)]
}
registerS3method("as.character", "display_shift_factor", as.character.display_shift_factor,
  envir = asNamespace("base"))
registerS3method("vec_ptype2", "display_shift_factor.display_shift_factor",
  function(x, y, ...) vctrs::vec_slice(x, integer()), envir = asNamespace("vctrs"))
registerS3method("vec_cast", "display_shift_factor.display_shift_factor",
  function(x, to, ...) x, envir = asNamespace("vctrs"))
d <- tibble::tibble(g = structure(1L, levels = c("A", "B"),
  class = c("display_shift_factor", "factor")), value = 10L)
before <- serialize(d, NULL)
stopifnot(identical(as.integer(dplyr::nest_by(d, g)$g), 1L),
  identical(as.integer(dplyr::union_all(d, d)$g), c(1L, 1L)))
observed <- integer()
for (verb in list(nest_with_margins, nest_by_with_margins)) {
  result <- verb(d, .grouping = grouping_set(g), .keep = TRUE)
  typed <- verb(d, .grouping = grouping_set(g), .keep = TRUE, .margin_label = NULL)
  fixed <- verb(d, .by = g, .keep = TRUE)
  stopifnot(identical(as.integer(result$data[[1L]]$g), 1L),
    identical(as.integer(typed$g), 1L), identical(as.integer(fixed$g), 1L),
    identical(levels(result$g), c("A", "B", "Total")))
  cat("Stored source code:", as.integer(d$g), "; outer code:", as.integer(result$g),
    "; retained cell code:", as.integer(result$data[[1L]]$g), "\n")
  observed <- c(observed, as.integer(result$g))
}
stopifnot(identical(before, serialize(d, NULL)))
cat("Native nesting/union, typed-missing, fixed-key, retained-cell and source controls passed.\n")
expanded <- expand_with_margins(d, .grouping = grouping_set(g))
summarized <- summarize_with_margins(d, count = dplyr::n(), .grouping = grouping_set(g))
cat("Shared expansion code:", as.integer(expanded$g),
  "; shared summary code:", as.integer(summarized$g), "\n")
observed <- c(observed, as.integer(expanded$g), as.integer(summarized$g))
if ("--assert" %in% args) {
  stopifnot("Detail keys must retain source factor values." = identical(observed, rep(1L, 4L)))
} else {
  stopifnot(identical(observed, rep(2L, 4L)))
  cat("Observed silent change from level A to level B in all four Margin verbs.\n")
}
