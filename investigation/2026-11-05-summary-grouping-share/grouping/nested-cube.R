pkgload::load_all(quiet = TRUE)
run <- function() {
  width <- if ("--small" %in% commandArgs(trailingOnly = TRUE)) 6L else 18L
  cols <- paste0("g", seq_len(width))
  input <- stats::setNames(rep(list("a"), width), cols)
  input <- tibble::as_tibble(c(input, list(x = 1L)))
  all_columns <- grouping_set(tidyselect::all_of(cols))
  cube_item <- cube(tidyselect::all_of(cols))
  expected <- inspect_grouping(input, .grouping = all_columns, .format = "list")
  # A single-branch union and a single-factor Cartesian product are each
  # identity wrappers around this cube.
  wrap <- list(
    direct = cube_item,
    product = grouping_spec(cube_item),
    union = grouping_sets(cube_item),
    union_product = grouping_sets(grouping_spec(cube_item))
  )
  desired <- "--assert" %in% commandArgs(trailingOnly = TRUE)
  f <- function(plan) {
    inspect_grouping(input, .grouping = plan, .duplicates = "drop", .format = "list")
  }
  for (label in names(wrap)) {
    item <- wrap[[label]]
    plans <- list(grouping_spec(item, all_columns), grouping_spec(all_columns, item))
    for (i in seq_along(plans)) {
      got <- tryCatch(f(plans[[i]]), error = identity)
      cat(label, i, if (inherits(got, "error")) conditionMessage(got) else paste("rows", nrow(got)), "\n")
      if (width == 18L && label == "union_product" && !desired) {
        stopifnot(inherits(got, "error"), grepl("vector memory", conditionMessage(got), fixed = TRUE))
      } else {
        stopifnot(identical(got, expected))
      }
    }
  }
}
run()
