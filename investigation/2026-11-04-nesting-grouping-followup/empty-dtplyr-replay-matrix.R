pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
data.table::setDTthreads(1L)

build_source <- function(graph, size, keep_rows, fail_on_second = FALSE) {
  visits <- integer()
  predicate <- function(x) {
    visits <<- c(visits, length(x))
    if (fail_on_second && length(visits) > 1L) stop("source predicate evaluated twice")
    rep(keep_rows, length(x))
  }
  root <- dtplyr::lazy_dt(tibble(g = rep("a", size), value = seq_len(size)))
  data <- switch(graph,
    root = filter(root, predicate(value)),
    order = arrange(filter(root, predicate(value)), value),
    cache = filter(compute(root), predicate(value))
  )
  list(data = data, visits = function() visits)
}

run_case <- function(graph, size, keep_rows, plan, operation, id) {
  source <- build_source(graph, size, keep_rows)
  if (operation == "native") {
    result <- collect(source$data)
    stopifnot(nrow(result) == if (keep_rows) size else 0L)
  } else {
    verb <- get(operation)
    result <- switch(plan,
      absent = verb(source$data, .id = id),
      explicit = verb(source$data, .grouping = grouping_set(), .id = id),
      fixed = verb(source$data, .by = g, .id = id),
      dimension = verb(source$data, .grouping = grouping_set(g), .id = id)
    )
    if (operation == "nest_with_margins") {
      stopifnot(length(source$visits()) == 0L)
      result <- collect(result)
    }
    keyed <- plan %in% c("fixed", "dimension")
    expected <- if (keep_rows || (!keyed && operation == "nest_by_with_margins")) 1L else 0L
    stopifnot(nrow(result) == expected)
    if (expected == 1L) {
      cell <- result$data[[1L]]
      columns <- if (keyed) "value" else c("g", "value")
      stopifnot(identical(names(cell), columns),
                identical(cell$value, if (keep_rows) seq_len(size) else integer()))
    }
  }
  data.frame(graph, size, keep_rows, plan, operation, has_id = !is.null(id),
             reads = length(source$visits()), rows_touched = sum(source$visits()))
}

results <- list()
for (graph in c("root", "order", "cache")) {
  for (size in c(2L, 4L)) {
    for (keep_rows in c(FALSE, TRUE)) {
      results[[length(results) + 1L]] <- run_case(graph, size, keep_rows, "absent", "native", NULL)
      for (plan in c("absent", "explicit", "fixed", "dimension")) {
        for (operation in c("nest_with_margins", "nest_by_with_margins")) {
          for (id in list(NULL, "set")) {
            results[[length(results) + 1L]] <- run_case(graph, size, keep_rows, plan, operation, id)
          }
        }
      }
    }
  }
}
results <- do.call(rbind, results)
print(with(results, table(operation, reads)))
cat("No source predicate ran during construction of nest_with_margins() results.\n")
cat("Repeated reads were on these plan/filtered-output combinations:\n")
print(unique(results[results$reads > 1L, c("keep_rows", "plan", "operation")]))

source <- build_source("root", 2L, keep_rows = FALSE, fail_on_second = TRUE)
actual <- tryCatch(nest_by_with_margins(source$data), error = identity)
if (inherits(actual, "error")) cat(conditionMessage(actual), "\n")
cat("Source predicate input sizes:", paste(source$visits(), collapse = ", "), "\n")
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(!inherits(actual, "error"),
            nrow(actual) == 1L, identical(actual$data[[1L]]$value, integer()),
            identical(source$visits(), 2L), all(results$reads == 1L))
}
