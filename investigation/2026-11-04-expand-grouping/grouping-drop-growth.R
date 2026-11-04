# Run from the checkout root. --memory enables capped, duplicate-heavy cases.
pkgload::load_all(quiet = TRUE)

arguments <- commandArgs(trailingOnly = TRUE)
input <- tibble::tibble(key = 1L)
constructors <- list(
  cube = function(n) cube(!!!rep(list(rlang::sym("key")), n)),
  product = function(n) {
    part <- grouping_sets(grouping_set(key), grouping_set())
    grouping_spec(!!!rep(list(part), n))
  }
)

inspect_drop <- function(spec) {
  inspect_grouping(input, .grouping = spec, .duplicates = "drop", .format = "list")
}

for (construct in constructors) {
  spec <- construct(4L)
  dropped <- inspect_drop(spec)
  kept <- inspect_grouping(input, .grouping = spec, .duplicates = "keep", .format = "list")
  refused <- tryCatch(inspect_grouping(input, .grouping = spec), error = identity)
  expanded <- expand_with_margins(input, .grouping = spec, .duplicates = "drop", .id = "set")
  stopifnot(
    identical(dropped$included, list("key", character())),
    identical(dropped$set_id, 1:2),
    identical(dropped$grouping_id, 0:1),
    identical(kept$included, c(rep(list("key"), 15L), list(character()))),
    inherits(refused, "marginplyr_error"),
    grepl("Duplicate grouping sets", conditionMessage(refused), fixed = TRUE),
    identical(expanded$key[order(expanded$set)], c("1", "Total"))
  )
}
cat("Both constructors passed small drop/keep/error and expand controls.\n")

observe <- function(construct, n) {
  previous <- mem.maxVSize()
  on.exit(invisible(mem.maxVSize(previous)))
  invisible(mem.maxVSize(64))
  started <- proc.time()[["elapsed"]]
  result <- tryCatch(inspect_drop(construct(n)), error = identity)
  elapsed <- proc.time()[["elapsed"]] - started
  if (inherits(result, "error")) {
    stopifnot(grepl("vector memory limit", conditionMessage(result), fixed = TRUE))
    cat(n, "units or factors:", conditionMessage(result), "\n")
  } else {
    stopifnot(identical(result$included, list("key", character())))
    cat(n, "units or factors:", nrow(result), "rows\n")
  }
  cat("Elapsed seconds:", elapsed, "\n")
  !inherits(result, "error")
}

if ("--memory" %in% arguments) {
  completed <- logical()
  for (constructor in names(constructors)) {
    cat(constructor, "with 64 MiB vector heap\n")
    construct <- constructors[[constructor]]
    stopifnot(observe(construct, 12L))
    large <- if (constructor == "cube") 20L else 21L
    completed[[constructor]] <- observe(construct, large)
  }
  if ("--assert" %in% arguments) stopifnot(all(completed))
}
