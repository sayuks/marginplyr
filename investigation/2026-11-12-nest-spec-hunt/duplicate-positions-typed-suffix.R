# Investigated SHA: 11471772e4a8a8518a689c538c2bc2929bd5d53d
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A duplicate refusal proven before typed metadata, when a later argument is a
# typed selection, is worded as the complete partition but lists only part of
# it, or splits one set's occurrences into several groups.
pkgload::load_all(".", quiet = TRUE)
d <- data.frame(a = "x", n = 1L)
e <- function(label, expr) {
  r <- tryCatch({ expr; "ok" }, error = function(e) conditionMessage(e))
  cat("## ", label, "\n", r, "\n", sep = "")
}
truth <- function(sets) {
  key <- vapply(sets, function(s) paste(sort(s), collapse = "|"), "")
  g <- split(seq_along(key), key)
  paste(vapply(g[lengths(g) > 1L], paste, "", collapse = ", "), collapse = "; ")
}
cat("# Four arguments: a, n, a, n\n")
cat("independent partition: ", truth(list("a", "n", "a", "n")), "\n", sep = "")
e("control: names", inspect_grouping(d, .grouping = grouping_sets(a, n, a, n)))
e("typed last argument", inspect_grouping(d, .grouping = grouping_sets(a, n, a, where(is.integer))))
e("typed last argument, dtplyr", inspect_grouping(dtplyr::lazy_dt(d), .grouping = grouping_sets(a, n, a, where(is.integer))))
cat("\n# Product cube(a, n) x cube(a) x n, eight sets\n")
cube_an <- list(c("a", "n"), "a", "n", character())
sets <- unlist(lapply(cube_an, function(x) lapply(list("a", character()), function(y) unique(c(x, y, "n")))), recursive = FALSE)
cat("independent partition: ", truth(sets), "\n", sep = "")
e("control: names, nest_with_margins", nest_with_margins(d, .grouping = grouping_spec(cube(a, n), cube(a), n)))
e("typed factor, nest_with_margins", nest_with_margins(d, .grouping = grouping_spec(cube(a, n), cube(a), where(is.integer))))
e("typed factor, nest_by_with_margins", nest_by_with_margins(d, .grouping = grouping_spec(cube(a, n), cube(a), where(is.integer))))
e("typed factor, summarize_with_margins", summarize_with_margins(d, k = n(), .grouping = grouping_spec(cube(a, n), cube(a), where(is.integer))))
