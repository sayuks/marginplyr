# Dated empirical audit of frame attributes, not a package regression test.
# Run from the repository root; the argument selects the output directory.
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else tempfile("table-attributes-")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(".", quiet = TRUE)

fixture <- function(shape) {
  input <- data.frame(g = c("a", "a", "b"), x = c(10, 20, 30))
  if (shape == "tibble") input <- tibble::as_tibble(input)
  if (shape == "subclass") class(input) <- c("table_attribute_audit", class(input))
  attr(input, "label") <- "Source dataset"
  attr(input, "source") <- "Study A"
  attr(input, "row_count") <- nrow(input)
  input
}

records <- list()
errors <- list()
record <- function(value, context, location) {
  records[[length(records) + 1L]] <<- as.data.frame(c(
    context,
    list(
      location = location,
      class = paste(class(value), collapse = "/"),
      rows = nrow(value),
      label = if (is.null(attr(value, "label", exact = TRUE))) {
        "<absent>"
      } else attr(value, "label", exact = TRUE),
      source = if (is.null(attr(value, "source", exact = TRUE))) {
        "<absent>"
      } else attr(value, "source", exact = TRUE),
      row_count_attribute = if (is.null(attr(value, "row_count", exact = TRUE))) {
        NA_integer_
      } else attr(value, "row_count", exact = TRUE)
    )
  ))
}

observe <- function(code, context) {
  result <- tryCatch({
    value <- force(code)
    if (!is.data.frame(value)) value <- dplyr::collect(value)
    value
  }, error = identity)
  if (inherits(result, "error")) {
    errors[[length(errors) + 1L]] <<- as.data.frame(c(
      context, list(message = conditionMessage(result))
    ))
    return(invisible(NULL))
  }
  record(result, context, "outer")
  if ("data" %in% names(result)) {
    for (i in seq_along(result$data)) {
      record(result$data[[i]], context, paste0("inner", i))
    }
  }
  invisible(NULL)
}

margin_result <- function(input, verb, plan) {
  switch(verb,
    summarize = summarize_with_margins(
      input, total = sum(x), .grouping = plan
    ),
    expand = expand_with_margins(input, .grouping = plan),
    nest = nest_with_margins(input, .grouping = plan),
    nest_by = nest_by_with_margins(input, .grouping = plan)
  )
}

for (shape in c("data.frame", "tibble", "subclass")) {
  input <- fixture(shape)
  native <- list(
    mutate = function() dplyr::mutate(input, x = x + 0),
    summarize = function() dplyr::summarise(input, total = sum(x), .by = g),
    bind_rows = function() dplyr::bind_rows(input, input),
    union_all = function() dplyr::union_all(input, input),
    vec_rbind = function() vctrs::vec_rbind(input, input),
    nest = function() tidyr::nest(input, .by = g),
    nest_by = function() dplyr::nest_by(input, g)
  )
  for (verb in names(native)) {
    context <- list(backend = "local", shape = shape, operation = verb,
                    plan = "native", owner = "upstream")
    observe(native[[verb]](), context)
  }
  for (plan_name in c("one", "rollup")) {
    plan <- if (plan_name == "one") grouping_set(g) else rollup(g)
    for (verb in c("summarize", "expand", "nest", "nest_by")) {
      context <- list(backend = "local", shape = shape, operation = verb,
                      plan = plan_name, owner = "marginplyr")
      observe(margin_result(input, verb, plan), context)
    }
  }
}

for (backend in c("dtplyr", "arrow")) {
  input <- fixture("tibble")
  source <- if (backend == "dtplyr") {
    dtplyr::lazy_dt(input)
  } else arrow::Table$create(input)
  native <- list(
    roundtrip = function() source,
    mutate = function() dplyr::mutate(source, x = x + 0),
    summarize = function() dplyr::summarise(source, total = sum(x), .by = g),
    union_all = function() dplyr::union_all(source, source)
  )
  for (verb in names(native)) {
    context <- list(backend = backend, shape = "tibble", operation = verb,
                    plan = "native", owner = "upstream")
    observe(native[[verb]](), context)
  }
  if (backend == "dtplyr") {
    context$operation <- "nest"
    observe(tidyr::nest(source, data = -g), context)
  }
  for (plan_name in c("one", "rollup")) {
    plan <- if (plan_name == "one") grouping_set(g) else rollup(g)
    verbs <- if (backend == "dtplyr") {
      c("summarize", "expand", "nest", "nest_by")
    } else c("summarize", "expand")
    for (verb in verbs) {
      context <- list(backend = backend, shape = "tibble", operation = verb,
                      plan = plan_name, owner = "marginplyr")
      observe(margin_result(source, verb, plan), context)
    }
  }
}

write.csv(do.call(rbind, records), file.path(output, "table-attributes.csv"),
          row.names = FALSE)
write.csv(if (length(errors)) do.call(rbind, errors) else data.frame(),
          file.path(output, "errors.csv"), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(output, "session-info.txt"))
cat("Recorded objects:", length(records), "\n")
cat("Errors:", length(errors), "\n")
cat("Output:", output, "\n")
