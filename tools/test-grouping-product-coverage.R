# Run from the package root with R_MAX_VSIZE=96M Rscript
# tools/test-grouping-product-coverage.R. The cap distinguishes compact planning
# from enumerating the 262,144 memberships of the enclosed cube (#1306).
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
stopifnot(mem.maxVSize() == 96)

keys <- paste0("g", seq_len(18L))
input <- tibble::as_tibble(stats::setNames(rep(list("x"), 18L), keys))
input <- input[rep(1L, 2L), , drop = FALSE]
input$value <- 1:2
variable <- cube(tidyselect::all_of(keys))
complete <- grouping_set(tidyselect::all_of(keys))
wrappers <- list(
  direct = variable,
  union = grouping_sets(!!variable),
  union_product = grouping_sets(grouping_spec(!!variable)),
  union_product_set = grouping_sets(grouping_spec(!!variable, grouping_set()))
)
for (wrapper in names(wrappers)) {
  for (reverse in c(FALSE, TRUE)) {
    spec <- if (reverse) {
      grouping_spec(!!complete, !!wrappers[[wrapper]])
    } else {
      grouping_spec(!!wrappers[[wrapper]], !!complete)
    }
    plan <- inspect_grouping(
      input, .grouping = spec, .duplicates = "drop", .format = "list"
    )
    stopifnot(identical(plan$included, list(keys)),
              identical(plan$set_id, 1L), identical(plan$grouping_id, 0L),
              identical(plan$grouping_bits,
                        list(stats::setNames(rep(0L, 18L), keys))))
    cat(wrapper, reverse, "inspect: one complete set\n")
    for (verb_name in c("nest_with_margins", "nest_by_with_margins")) {
      verb <- get(verb_name)
      nested <- verb(
        input, .grouping = spec, .duplicates = "drop", .id = "set",
        .keep = TRUE
      )
      stopifnot(identical(names(nested), c(keys, "set", "data")),
                identical(nested$set, 1L),
                identical(tibble::as_tibble(nested[keys]), input[1L, keys]),
                identical(nested$data, list(input)))
      cat(wrapper, reverse, verb_name, ": full source payload\n")
    }
    gc(verbose = FALSE)
  }
}
