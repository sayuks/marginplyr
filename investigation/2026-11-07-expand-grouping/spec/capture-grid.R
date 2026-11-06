Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
reference_capture <- function(...) rlang::enquos(...)
opaque <- rlang::new_quosure(quote(!!stop("must remain defused")), globalenv())
arguments <- list(
  rlang::quo(a), rlang::quo(c(a, b)), opaque,
  quote(!!!list()), quote(!!!list("b")), quote(!!!list(rlang::quo(b))),
  quote(!!!rlang::quos(b, c)), quote(c(a, b)), quote(NULL), quote(a),
  rlang::missing_arg()
)
records <- list()
for (i in seq_along(arguments)) {
  for (j in seq_along(arguments)) {
    for (k in seq_along(arguments)) {
      positions <- c(i, j, k)
      actual_call <- as.call(c(
        list(as.name("grouping_sets")), arguments[positions]
      ))
      actual <- eval(actual_call)$args
      if (3L %in% positions) {
        expected_count <- sum(positions == 3L)
        actual_count <- sum(vapply(actual, identical, logical(1), y = opaque))
        same <- identical(expected_count, actual_count)
        oracle <- "opaque quosure remains identical without running injection"
      } else {
        reference_call <- as.call(c(
          list(reference_capture), arguments[positions]
        ))
        expected <- eval(reference_call)
        same <- identical(actual, expected)
        oracle <- "ordinary tidy-eval capture"
      }
      records[[length(records) + 1L]] <- data.frame(i, j, k, oracle, same)
    }
  }
}
results <- do.call(rbind, records)
write.csv(results, file.path(Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempdir()),
                           "capture-grid.csv"), row.names = FALSE)
print(table(results$oracle, results$same))
stopifnot(all(results$same))
