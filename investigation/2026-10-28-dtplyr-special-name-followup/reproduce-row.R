# Dated minimal controls and reproductions at the PR #961 snapshot.
pkgload::load_all(".", quiet = TRUE)

run <- function(label, expr, expected_error = NULL) {
  answer <- tryCatch(force(expr), error = identity)
  if (is.null(expected_error)) {
    stopifnot(!inherits(answer, "error"))
    cat("PASS", label, "\n")
  } else {
    stopifnot(inherits(answer, "error"),
      grepl(expected_error, conditionMessage(answer), fixed = TRUE))
    cat("REPRODUCED", label, ":", conditionMessage(answer), "\n")
  }
  invisible(answer)
}

input <- tibble::tibble(.ROW = "a")
source <- dtplyr::lazy_dt(input, immutable = TRUE)
stopifnot(identical(dplyr::collect(source), input))
cat("PASS ordinary source collection\n")
error <- ".ROW can only be used with := NULL to delete rows"
for (verb_name in c("nest_with_margins", "nest_by_with_margins")) {
  verb <- get(verb_name)
  run(paste(verb_name, "local dimension"),
    verb(input, .grouping = rollup(.ROW)))
  run(paste(verb_name, "fixed key"),
    dplyr::collect(verb(source, .by = .ROW)))
  run(paste(verb_name, "dimension"),
    dplyr::collect(verb(source, .grouping = rollup(.ROW))), error)
  run(paste(verb_name, "id"),
    dplyr::collect(verb(dtplyr::lazy_dt(tibble::tibble(value = 1L)), .id = ".ROW")),
    error)
  run(paste(verb_name, "safe renamed dimension"),
    dplyr::collect(verb(dplyr::rename(source, safe = .ROW),
      .grouping = rollup(safe))))
}
stopifnot(identical(dplyr::collect(source), input))
query <- nest_with_margins(source, .grouping = rollup(.ROW))
stopifnot(inherits(query, "dtplyr_step"))
cat("PASS lazy dimension construction; failure was deferred to collection\n")
run("ordinary dtplyr mutation control",
  dplyr::collect(dplyr::mutate(source, .ROW = "Total")), error)
cat("PASS source remained unchanged\n")
