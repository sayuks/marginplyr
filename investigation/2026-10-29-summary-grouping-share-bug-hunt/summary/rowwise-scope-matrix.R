args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) normalizePath(args[[1L]]) else getwd()
suppressPackageStartupMessages(library(dplyr))
devtools::load_all(repo, quiet = TRUE)
data <- tibble(g = "a", x = 1)
inputs <- list(tibble = data, data.frame = as.data.frame(data),
               data.table = data.table::as.data.table(data))
counts <- c(total = 0L, named_mismatch = 0L, positional_control = 0L)
for (backend in names(inputs)) {
  input <- inputs[[backend]]
  for (plan in c("set", "rollup", "fixed")) {
    spec <- switch(plan, set = grouping_set(g), rollup = rollup(g), fixed = NULL)
    fixed <- if (plan == "fixed") "g" else character()
    for (arg in c("data", "d", "da", "dat", "positional")) {
      arguments <- list(rlang::expr(dplyr::pick(everything())))
      names(arguments) <- if (arg == "positional") "" else arg
      nested <- rlang::call2("rowwise", !!!arguments, .ns = "dplyr")
      expr <- rlang::expr(ncol(!!nested))
      result <- rlang::eval_tidy(rlang::expr(summarize_with_margins(
        input, width = !!expr, .by = all_of(fixed), .grouping = spec,
        .margin_label = NULL
      )))
      counts[["total"]] <- counts[["total"]] + 1L
      cat(backend, plan, arg, "width", paste(result$width, collapse = ","), "\n")
      if (arg == "positional") {
        stopifnot(all(result$width == 1L))
        counts[["positional_control"]] <- counts[["positional_control"]] + 1L
      } else {
        stopifnot(any(result$width != 1L))
        counts[["named_mismatch"]] <- counts[["named_mismatch"]] + 1L
      }
    }
  }
}
print(counts)
