# Run from the package root with R_MAX_VSIZE=96M. The default asserts the
# desired behavior. --expect-defect verifies the heap-limit witnesses.
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)

args <- commandArgs(trailingOnly = TRUE)
expect_defect <- identical(args, "--expect-defect")
stopifnot(length(args) == 0L || expect_defect, mem.maxVSize() == 96)

probe <- function(width, variant, consumer) {
  vars <- paste0("key_", seq_len(width))
  source <- tibble::as_tibble(stats::setNames(rep(list("detail"), width), vars))
  source$payload <- 1L
  all_keys <- grouping_set(tidyselect::all_of(vars))
  spec <- switch(
    variant,
    direct = all_keys,
    cube_first = grouping_spec(cube(tidyselect::all_of(vars)), !!all_keys),
    cube_last = grouping_spec(!!all_keys, cube(tidyselect::all_of(vars)))
  )
  result <- tryCatch({
    if (identical(consumer, inspect_grouping)) {
      plan <- consumer(source, .grouping = spec, .duplicates = "drop",
                       .format = "list")
      stopifnot(
        identical(plan$set_id, 1L),
        identical(plan$included, list(vars)),
        identical(plan$grouping_id, 0L)
      )
    } else {
      nested <- consumer(source, .grouping = spec, .duplicates = "drop",
                         .id = "set", .key = "cell")
      stopifnot(
        identical(nrow(nested), 1L),
        identical(nested$set, 1L),
        identical(nested$cell[[1L]]$payload, 1L)
      )
    }
    "one complete grouping set"
  }, error = conditionMessage)
  cat(width, variant, result, "\n")
  gc(verbose = FALSE)
  result
}

success <- "one complete grouping set"
stopifnot(
  identical(probe(10L, "cube_first", inspect_grouping), success),
  identical(probe(10L, "cube_last", inspect_grouping), success),
  identical(probe(18L, "direct", inspect_grouping), success),
  identical(probe(18L, "direct", nest_with_margins), success),
  identical(probe(18L, "direct", nest_by_with_margins), success)
)

actual <- c(
  inspect_cube_first = probe(18L, "cube_first", inspect_grouping),
  inspect_cube_last = probe(18L, "cube_last", inspect_grouping),
  nest_cube_first = probe(18L, "cube_first", nest_with_margins),
  rowwise_cube_last = probe(18L, "cube_last", nest_by_with_margins)
)
if (expect_defect) {
  stopifnot(all(grepl("vector memory limit", actual, fixed = TRUE)))
} else {
  stopifnot(all(actual == success))
}
cat(R.version.string, R.version$platform, "\n")
