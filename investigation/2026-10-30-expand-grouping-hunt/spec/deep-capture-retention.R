args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(if (length(args)) args[[1L]] else ".", quiet = TRUE)
data <- tibble::tibble(region = "east", amount = 1L)
parents <- c("grouping_set", "grouping_sets", "grouping_spec", "rollup", "cube")
cases <- 0L
for (operator in c("::", ":::")) {
  for (depth in c(0L, 1L, 4L, 16L, 64L)) {
    for (parent in parents) {
      for (backend in c("local", "dtplyr")) {
        for (consumer in c("inspect", "expand")) {
          env <- rlang::env(globalenv(), calls = 0L, operator_reads = 0L)
          env$choose <- function(...) {
            env$calls <- env$calls + 1L
            if (env$calls == 1L) "region" else "amount"
          }
          makeActiveBinding(operator, function() {
            env$operator_reads <- env$operator_reads + 1L
            function(package, name) env$choose
          }, env)
          head <- rlang::call2(operator, quote(marginplyr), quote(rollup))
          arg <- rlang::new_quosure(rlang::call2(head), env)
          for (i in seq_len(depth)) {
            outer <- rlang::env(globalenv())
            outer[[operator]] <- function(...) {
              stop("The outer namespace binding must not run.")
            }
            arg <- rlang::new_quosure(rlang::call2("(", arg), outer)
          }
          specification <- eval(rlang::call2(parent, arg))
          input <- if (backend == "local") data else dtplyr::lazy_dt(data)
          actual <- if (consumer == "inspect") {
            inspect_grouping(input, .grouping = specification, .format = "list")
          } else {
            expand_with_margins(
              input, .grouping = specification,
              .margin_label = NULL, .id = "sid"
            )
          }
          expected_spec <- eval(rlang::call2(parent, quote(region)))
          expected <- if (consumer == "inspect") {
            inspect_grouping(input, .grouping = expected_spec, .format = "list")
          } else {
            expand_with_margins(
              input, .grouping = expected_spec,
              .margin_label = NULL, .id = "sid"
            )
          }
          if (consumer == "expand") {
            actual <- dplyr::collect(actual)
            expected <- dplyr::collect(expected)
          }
          stopifnot(
            identical(actual, expected), env$calls == 1L,
            env$operator_reads == 1L
          )
          cases <- cases + 1L
        }
      }
    }
  }
}
cat("Qualified active-binding/deep-quosure comparisons:", cases, "\n")
cat(
  "Every result matched the literal-column control;",
  "one caller invocation and one operator read per case.\n"
)
