pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
width <- if (length(args)) as.integer(args[[1L]]) else 20L
cols <- paste0("d", seq_len(width))
input <- tibble::as_tibble(stats::setNames(rep(list(1L), width), cols))
all_subsets <- rlang::inject(cube(!!!rlang::syms(cols)))
nearly_complete <- lapply(seq_along(cols), function(i) {
  grouping_set(tidyselect::all_of(cols[-i]))
})
suffix <- rlang::inject(grouping_sets(!!!nearly_complete))
product <- grouping_spec(!!all_subsets, !!suffix)
expected <- c(list(cols), lapply(rev(seq_along(cols)), function(i) cols[-i]))
literal <- rlang::inject(grouping_sets(!!!lapply(expected, function(x) grouping_set(tidyselect::all_of(x)))))
control <- inspect_grouping(input, .grouping = literal, .duplicates = "drop", .format = "list")
stopifnot(identical(control$included, expected),
          identical(control$set_id, seq_len(width + 1L)))
cat("Literal final-family control matched", width + 1L, "sets at width", width, "\n")
result <- tryCatch(
  inspect_grouping(input, .grouping = product, .duplicates = "drop", .format = "list"),
  error = identity
)
if (inherits(result, "error")) {
  cat("Product condition:", conditionMessage(result), "\n")
} else {
  stopifnot(identical(result, control))
  cat("Product matched the literal final-family control\n")
}
cat(R.version.string, R.version$platform, "\n")
if ("--assert" %in% args) stopifnot(identical(result, control))
