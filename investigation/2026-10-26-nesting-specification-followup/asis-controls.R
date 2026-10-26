# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args <- commandArgs(TRUE)
root <- if (length(args)) args[[1L]] else "."
pkgload::load_all(root, quiet = TRUE)
types <- list(
  logical = TRUE,
  integer = 1L,
  double = 1,
  character = "x",
  Date = as.Date("2026-10-26"),
  POSIXct = as.POSIXct("2026-10-26", tz = "Asia/Tokyo"),
  difftime = as.difftime(1, units = "hours"),
  factor = factor("x"),
  ordered = ordered("x"),
  integer64 = bit64::as.integer64(1),
  units = units::set_units(1, "m")
)
verbs <- list(nest_with_margins, nest_by_with_margins)
failures_observed <- 0L
successes <- 0L
for (type in names(types)) {
  wraps <- if (type %in% c("logical", "integer", "double", "character")) {
    c(FALSE, TRUE)
  } else {
    FALSE
  }
  for (wrap in wraps) {
    g <- if (wrap) I(types[[type]]) else types[[type]]
    loses_class <- !identical(class(rep(g[NA_integer_], 1L)), class(g))
    for (size in c(0L, 1L)) {
      column <- types[[type]][seq_len(size)]
      if (wrap) class(column) <- class(g)
      x <- tibble::tibble(g = column)
      lazy <- dtplyr::lazy_dt(x)
      for (verb in verbs) {
        for (keep in c(FALSE, TRUE)) {
          for (label in list("Total", NULL, NA_character_)) {
            run <- function(data, grouping) {
              dplyr::collect(verb(
                data, .grouping = grouping, .margin_label = label,
                .keep = keep, .id = "set"
              ))
            }
            local <- run(x, rollup(g))
            stopifnot(
              nrow(local) == 2L * size,
              sum(vapply(local$data, nrow, integer(1))) == 2L * size
            )
            single <- run(lazy, grouping_set(g))
            fixed <- dplyr::collect(verb(lazy, .by = g, .keep = keep))
            stopifnot(
              nrow(single) == size, nrow(fixed) == size,
              sum(vapply(single$data, nrow, integer(1))) == size,
              sum(vapply(fixed$data, nrow, integer(1))) == size
            )
            actual <- tryCatch(run(lazy, rollup(g)), error = identity)
            if (wrap && loses_class) {
              # Dated observation of the unfixed defect, not acceptance of it.
              stopifnot(
                inherits(actual, "error"),
                grepl("'AsIs'", conditionMessage(actual), fixed = TRUE),
                grepl("corresponding item", conditionMessage(actual), fixed = TRUE)
              )
              failures_observed <- failures_observed + 1L
            } else {
              stopifnot(
                !inherits(actual, "error"), nrow(actual) == 2L * size,
                sum(vapply(actual$data, nrow, integer(1))) == 2L * size
              )
              successes <- successes + 1L
            }
          }
        }
      }
    }
    cat(type, "AsIs", wrap, "rep preserves class", !loses_class, "PASS\n")
  }
}
# A class-preserving union is supported by dtplyr/data.table themselves.
x <- tibble::tibble(g = I(1))
lazy <- dtplyr::lazy_dt(x)
upstream <- dplyr::collect(dplyr::union_all(
  lazy, dplyr::mutate(lazy, g = I(NA_real_))
))
stopifnot(nrow(upstream) == 2L, inherits(upstream$g, "AsIs"))
for (verb in list(expand_with_margins, summarize_with_margins)) {
  actual <- dplyr::collect(verb(lazy, .grouping = rollup(g), .margin_label = NULL))
  stopifnot(nrow(actual) == 2L, inherits(actual$g, "AsIs"))
}
cat(
  "dtplyr rollups:", successes, "successful controls;",
  failures_observed, "expected current-defect observations\n"
)
cat("local rollup, dtplyr single-set and fixed-key controls:",
    3L * (successes + failures_observed), "PASS\n")
