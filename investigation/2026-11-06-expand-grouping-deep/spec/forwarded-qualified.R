Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
output <- Sys.getenv(
  "MARGINPLYR_HUNT_OUTPUT",
  "investigation/2026-11-06-expand-grouping-deep/spec"
)
data <- tibble::tibble(a = c("x", "y"), b = c(11L, 12L), value = 1:2)
a <- "b"
templates <- list(
  embrace = quote(function(column) CONSTRUCTOR(base::`(`({{ column }}))),
  enquo = quote(function(column) CONSTRUCTOR(base::`(`(!!rlang::enquo(column)))),
  numbered_dots = quote(function(...) CONSTRUCTOR(base::`(`(..1))),
  parenthesized_dots = quote(function(...) CONSTRUCTOR(base::`(`((..1))))
)
reference_templates <- list(
  embrace = quote(function(column) {
    tidyselect::eval_select(rlang::expr(base::`(`({{ column }})), data)
  }),
  enquo = quote(function(column) {
    tidyselect::eval_select(rlang::expr(base::`(`(!!rlang::enquo(column))), data)
  }),
  numbered_dots = quote(function(...) {
    tidyselect::eval_select(rlang::quo(base::`(`(..1)), data)
  }),
  parenthesized_dots = quote(function(...) {
    tidyselect::eval_select(rlang::quo(base::`(`((..1))), data)
  })
)
consume <- function(source, spec, consumer) {
  result <- switch(consumer,
    inspect = inspect_grouping(source, .grouping = spec, .format = "list"),
    expand = expand_with_margins(source, .grouping = spec, .margin_label = NULL),
    summarize = summarize_with_margins(
      source, n = dplyr::n(), .grouping = spec, .margin_label = NULL
    ),
    nest = nest_with_margins(source, .grouping = spec, .margin_label = NULL),
    nest_by = nest_by_with_margins(source, .grouping = spec, .margin_label = NULL)
  )
  if (inherits(result, "dtplyr_step")) dplyr::collect(result) else result
}
results <- list()
for (method in names(templates)) {
  native <- eval(reference_templates[[method]])
  stopifnot(identical(names(native(a)), "b"))
  for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")) {
    expr <- do.call(base::substitute, list(
      templates[[method]], list(CONSTRUCTOR = as.name(constructor))
    ))
    forward <- eval(expr)
    spec <- forward(a)
    expected <- rlang::eval_tidy(rlang::call2(constructor, quote(b)))
    for (input in c("local", "dtplyr")) {
      source <- if (input == "local") data else dtplyr::lazy_dt(data)
      for (consumer in c("inspect", "expand", "summarize", "nest", "nest_by")) {
        actual <- consume(source, spec, consumer)
        reference <- consume(source, expected, consumer)
        results[[length(results) + 1L]] <- data.frame(
          method, constructor, input, consumer,
          matches_reference = identical(actual, reference)
        )
      }
    }
  }
}
results <- do.call(rbind, results)
write.csv(results, file.path(output, "forwarded-qualified.csv"), row.names = FALSE)
print(as.data.frame(with(results, table(method, matches_reference))))
stopifnot(nrow(results) == 200L, !any(results$matches_reference))

union_wrapper <- function(column) {
  grouping_sets(base::`(`({{ column }}), grouping_set(a))
}
dimension_wrapper <- function(column) grouping_set(base::`(`({{ column }}))
capture <- function(expr) tryCatch(expr, error = identity)
false_duplicate <- capture(inspect_grouping(data, .grouping = union_wrapper(a)))
false_overlap <- capture(inspect_grouping(data, .by = a, .grouping = dimension_wrapper(a)))
union_control <- inspect_grouping(
  data, .grouping = grouping_sets(grouping_set(b), grouping_set(a)), .format = "list"
)
overlap_control <- inspect_grouping(
  data, .by = a, .grouping = grouping_set(b), .format = "list"
)
stopifnot(
  inherits(false_duplicate, "marginplyr_error"),
  grepl("Duplicate grouping sets", conditionMessage(false_duplicate), fixed = TRUE),
  inherits(false_overlap, "marginplyr_error"),
  grepl("both `.by` and `.grouping`", conditionMessage(false_overlap), fixed = TRUE),
  identical(union_control$included, list("b", "a")),
  identical(overlap_control$fixed, list("a")),
  identical(overlap_control$included, list("b"))
)
cat("False duplicate:", conditionMessage(false_duplicate), "\n")
cat("False fixed-key overlap:", conditionMessage(false_overlap), "\n")
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(all(results$matches_reference))
}
