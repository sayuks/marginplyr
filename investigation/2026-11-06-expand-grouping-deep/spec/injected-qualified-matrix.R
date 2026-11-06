Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
output <- Sys.getenv(
  "MARGINPLYR_HUNT_OUTPUT",
  "investigation/2026-11-06-expand-grouping-deep/spec"
)
constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
consumers <- c("inspect", "expand", "summarize", "nest", "nest_by")
outcome <- function(expr) tryCatch(expr, error = identity)
consume <- function(data, spec, kind) {
  switch(kind,
    inspect = inspect_grouping(data, .grouping = spec, .format = "list"),
    expand = expand_with_margins(data, .grouping = spec, .margin_label = NULL),
    summarize = summarize_with_margins(
      data, n = dplyr::n(), .grouping = spec, .margin_label = NULL
    ),
    nest = nest_with_margins(data, .grouping = spec, .margin_label = NULL),
    nest_by = nest_by_with_margins(data, .grouping = spec, .margin_label = NULL)
  )
}
materialize <- function(x) {
  if (inherits(x, "dtplyr_step")) {
    x <- dplyr::collect(x)
  }
  x
}
data <- tibble::tibble(a = c("x", "y"), b = c(11L, 12L), value = c(101L, 102L))
rows <- list()
for (lexical in c("name", "position")) {
  env <- rlang::env(globalenv(), a = if (lexical == "name") "b" else 2L)
  for (payload in c("symbol", "combine")) {
    raw <- if (payload == "symbol") quote(a) else quote(c(a))
    q <- rlang::new_quosure(raw, env)
    forms <- list()
    for (operator in c("::", ":::")) {
      for (depth in c(1L, 3L)) {
        selected <- q
        head <- rlang::call2(operator, quote(base), quote(`(`))
        for (i in seq_len(depth)) selected <- rlang::call2(head, selected)
        forms[[paste0(operator, "-", depth)]] <- selected
      }
    }
    forms$syntactic <- rlang::call2("(", q)
    forms$identity <- rlang::call2("identity", q)
    forms$unembraced <- rlang::call2(quote(base::`(`), raw)
    for (form in names(forms)) {
      selected <- forms[[form]]
      reference <- names(tidyselect::eval_select(selected, data, env = env))
      stopifnot(identical(reference, if (form == "syntactic") "a" else "b"))
      for (input in c("local", "dtplyr")) {
        source <- if (input == "local") data else dtplyr::lazy_dt(data)
        for (constructor in constructors) {
          spec <- rlang::eval_tidy(rlang::call2(constructor, selected), env = env)
          expected_spec <- rlang::eval_tidy(
            rlang::call2(constructor, rlang::call2("all_of", reference)), env = env
          )
          for (consumer in consumers) {
            actual <- outcome(materialize(consume(source, spec, consumer)))
            expected <- outcome(materialize(consume(source, expected_spec, consumer)))
            expected_error <- inherits(expected, "error")
            actual_error <- inherits(actual, "error")
            match <- if (expected_error || actual_error) {
              expected_error && actual_error && identical(class(actual), class(expected))
            } else {
              identical(actual, expected)
            }
            rows[[length(rows) + 1L]] <- data.frame(
              lexical, payload, form, input, constructor, consumer,
              reference = paste(reference, collapse = ","),
              match, expected_error, actual_error,
              actual_message = if (actual_error) conditionMessage(actual) else "",
              expected_message = if (expected_error) conditionMessage(expected) else ""
            )
          }
        }
      }
    }
  }
}
results <- do.call(rbind, rows)
write.csv(results, file.path(output, "injected-qualified-matrix.csv"), row.names = FALSE)
print(as.data.frame(with(results, table(form, match, expected_error))))
failing_forms <- startsWith(results$form, "::-")
supported <- !results$expected_error
stopifnot(
  all(!results$match[failing_forms & supported]),
  all(results$match[!failing_forms & supported]),
  !any(results$actual_error[supported])
)
cat("Executed comparisons:", nrow(results), "\n")
cat("Successful ordinary controls:", sum(results$match & supported), "\n")
cat("Silent wrong results:", sum(!results$match & supported), "\n")
cat("Ordinary-backend exclusions:", sum(!supported), "\n")
print(sessionInfo())
