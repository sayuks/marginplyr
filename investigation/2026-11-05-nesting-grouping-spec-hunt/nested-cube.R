# Run from the package root with R_MAX_VSIZE=96M.
# --assert requires successful results for the nested wrapper as well.
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
stopifnot(mem.maxVSize() == 96)
assert <- "--assert" %in% commandArgs(trailingOnly = TRUE)

probe <- function(width, consumer, wrapper, reverse = FALSE) {
  keys <- paste0("g", seq_len(width))
  input <- tibble::as_tibble(stats::setNames(rep(list("x"), width), keys))
  input$v <- 1L
  variable <- cube(tidyselect::all_of(keys))
  complete <- grouping_set(tidyselect::all_of(keys))
  wrapped <- switch(wrapper,
    direct = variable,
    union = grouping_sets(!!variable),
    product = grouping_spec(!!variable),
    union_product = grouping_sets(grouping_spec(!!variable)),
    union_product_set = grouping_sets(grouping_spec(!!variable, grouping_set()))
  )
  spec <- if (reverse) {
    grouping_spec(!!complete, !!wrapped)
  } else {
    grouping_spec(!!wrapped, !!complete)
  }
  result <- tryCatch({
    if (identical(consumer, inspect_grouping)) {
      plan <- consumer(input, .grouping = spec, .duplicates = "drop",
                       .format = "list")
      stopifnot(identical(plan$included, list(keys)),
                identical(plan$set_id, 1L),
                identical(plan$grouping_id, 0L))
    } else {
      nest <- consumer(input, .grouping = spec, .duplicates = "drop", .id = "set")
      stopifnot(identical(setdiff(names(nest), c("set", "data")), keys),
                identical(nest$set, 1L), identical(nest$data[[1L]]$v, 1L))
    }
    "one complete set"
  }, error = conditionMessage)
  cat(width, wrapper, reverse, result, "\n")
  gc(verbose = FALSE)
  result
}

variants <- c("direct", "union", "product", "union_product", "union_product_set")
for (variant in variants) {
  stopifnot(identical(probe(8L, inspect_grouping, variant), "one complete set"))
}
for (variant in variants[1:3]) {
  stopifnot(identical(probe(18L, inspect_grouping, variant), "one complete set"))
}
stopifnot(identical(probe(18L, inspect_grouping, "union", TRUE), "one complete set"))
for (consumer in list(nest_with_margins, nest_by_with_margins)) {
  stopifnot(identical(probe(18L, consumer, "direct"), "one complete set"))
}
consumers <- list(inspect_grouping, nest_with_margins, nest_by_with_margins)
observed <- c(
  probe(18L, inspect_grouping, "union_product_set"),
  vapply(consumers, function(consumer) {
    probe(18L, consumer, "union_product")
  }, character(1)),
  probe(18L, inspect_grouping, "union_product", TRUE)
)
if (assert) {
  stopifnot(all(observed == "one complete set"))
} else {
  stopifnot(all(grepl("vector memory limit", observed, fixed = TRUE)))
}
