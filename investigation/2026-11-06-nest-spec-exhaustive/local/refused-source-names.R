args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(if (length(args)) args[[1L]] else ".", quiet = TRUE)
for (name in c("...", "..1")) {
  data <- tibble::new_tibble(stats::setNames(list(c("a", "b"), 1:2), c("g", name)), nrow = 2L)
  calls <- list(
    native_nest = function(data) tidyr::nest(data, .by = g),
    native_nest_by = function(data) dplyr::nest_by(data, g),
    margin_nest = function(data) nest_with_margins(data, .grouping = rollup(g)),
    margin_nest_by = function(data) nest_by_with_margins(data, .grouping = rollup(g))
  )
  for (label in names(calls)) {
    error <- tryCatch(calls[[label]](data), error = identity)
    stopifnot(inherits(error, "error"), grepl("Names can't be of the form", conditionMessage(error), fixed = TRUE) ||
                grepl("must not have names of the form", conditionMessage(error), fixed = TRUE))
    cat(name, label, class(error)[[1L]], "\n")
    safe <- data
    names(safe)[[2L]] <- "value"
    stopifnot(!inherits(tryCatch(calls[[label]](safe), error = identity), "error"))
  }
}
cat("Eight matching upstream refusal boundaries and eight successful safe-name controls.\n")
