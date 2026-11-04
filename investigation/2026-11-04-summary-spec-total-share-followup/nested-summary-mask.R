# Run from the investigated checkout root. --assert requires the repaired contract.
pkgload::load_all(quiet = TRUE)

options(stringsAsFactors = FALSE)
cases <- list(
  return = quote((function() {
    dplyr::mutate(return(sum(x)), z = 1L)
    99L
  })()),
  assignment = quote((function() {
    value <- 99L
    dplyr::mutate({ value <- sum(x); data.frame(z = value) }, z = z + 1L)
    value
  })()),
  inner_pick = quote((function() {
    value <- 99L
    dplyr::mutate(
      { value <- sum(x); data.frame(z = value) },
      extra = rowSums(dplyr::pick(z))
    )
    value
  })()),
  return_control = quote((function() {
    return(sum(x))
    99L
  })()),
  assignment_control = quote((function() {
    value <- 99L
    input <- { value <- sum(x); data.frame(z = value) }
    dplyr::mutate(input, extra = rowSums(dplyr::pick(z)))
    value
  })())
)

scope_results <- list()
data <- data.frame(g = c("a", "b"), x = c(1L, 2L))
constructors <- list(
  data.frame = identity,
  tibble = tibble::as_tibble,
  data.table = data.table::as.data.table
)
for (backend in names(constructors)) {
  input <- constructors[[backend]](data)
  for (name in names(cases)) {
    expr <- cases[[name]]
    ordinary <- rlang::inject(dplyr::summarize(input, answer = !!expr))
    detail <- rlang::inject(dplyr::summarize(input, answer = !!expr, .by = g))
    expected <- list(
      total = ordinary$answer,
      fixed = detail$answer,
      rollup = c(detail$answer, ordinary$answer)
    )
    stopifnot(identical(expected, list(
      total = 3L, fixed = 1:2, rollup = 1:3
    )))
    actual <- list(
      total = rlang::inject(summarize_with_margins(input, answer = !!expr))$answer,
      fixed = rlang::inject(summarize_with_margins(
        input, answer = !!expr, .by = g, .sort = "last"
      ))$answer,
      rollup = rlang::inject(summarize_with_margins(
        input, answer = !!expr, .grouping = rollup(g), .sort = "last"
      ))$answer
    )
    for (grain in names(expected)) {
      observed <- actual[[grain]]
      correct <- identical(observed, expected[[grain]])
      if (endsWith(name, "_control")) stopifnot(correct)
      if (!correct) stopifnot(identical(observed, rep(99L, length(expected[[grain]]))))
      scope_results[[length(scope_results) + 1L]] <- data.frame(
        backend = backend, shape = name, grain = grain,
        expected = toString(expected[[grain]]), observed = toString(observed),
        agrees = correct
      )
    }
  }
}
print(do.call(rbind, scope_results), row.names = FALSE)

observe <- function(expr) {
  warnings <- character()
  result <- tryCatch(withCallingHandlers(expr, warning = function(cnd) {
    warnings <<- c(warnings, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  }), error = identity)
  list(result = result, warnings = warnings)
}

probe_arrow <- function() {
  table <- arrow::Table$create(data)
  batch <- arrow::RecordBatch$create(data)
  dataset <- arrow::InMemoryDataset$create(table)
  inputs <- list(
    table = table, batch = batch, query = dplyr::collapse(table),
    dataset = dataset, dataset_query = dplyr::collapse(dataset)
  )
  trace_targets <- c(
    "collect.arrow_dplyr_query", "collect.ArrowTabular", "collect.Dataset"
  )
  arrow_namespace <- asNamespace("arrow")
  counter <- new.env(parent = emptyenv())
  counter$collections <- 0L
  for (target in trace_targets) {
    suppressMessages(trace(
      target, where = arrow_namespace, print = FALSE,
      tracer = function() {
        counter$collections <- counter$collections + 1L
      }
    ))
  }
  on.exit(for (target in trace_targets) {
    suppressMessages(untrace(target, where = arrow_namespace))
  })

  shape <- list(
    group_by = quote(dplyr::group_by(tibble::tibble(z = sum(x)), z)),
    mutate = quote(dplyr::mutate(tibble::tibble(z = sum(x)), z = z + 1L)),
    select = quote(dplyr::select(tibble::tibble(z = sum(x)), z))
  )
  rows <- list()
  for (backend in names(inputs)) {
    for (name in names(shape)) {
      input <- inputs[[backend]]
      expr <- shape[[name]]
      is_dataset <- startsWith(backend, "dataset")
      counter$collections <- 0L
      ordinary <- observe(rlang::inject(dplyr::summarize(input, !!expr)))
      ordinary_reads <- counter$collections
      if (is_dataset) {
        stopifnot(inherits(ordinary$result, "arrow_not_supported"), ordinary_reads == 0L)
      } else {
        stopifnot(is.data.frame(ordinary$result), ordinary_reads > 0L,
                  any(grepl("Pulling data into R", ordinary$warnings, fixed = TRUE)))
      }
      counter$collections <- 0L
      actual <- observe(rlang::inject(summarize_with_margins(input, !!expr)))
      actual_reads <- counter$collections
      stopifnot(inherits(actual$result, "error"), actual_reads == 0L)
      correct <- if (is_dataset) {
        inherits(actual$result, "arrow_not_supported") &&
          identical(conditionMessage(actual$result), conditionMessage(ordinary$result))
      } else {
        inherits(actual$result, "marginplyr_error") &&
          grepl(paste0("dplyr::", name), conditionMessage(actual$result), fixed = TRUE) &&
          grepl("Select the columns you need before collecting",
                conditionMessage(actual$result), fixed = TRUE)
      }
      if (!correct) {
        stopifnot(inherits(actual$result, "simpleError"),
                  identical(conditionMessage(actual$result), "bad value"))
      }
      rows[[length(rows) + 1L]] <- data.frame(
        backend = backend, shape = name,
        ordinary_class = class(ordinary$result)[[1L]], ordinary_reads = ordinary_reads,
        margin_class = class(actual$result)[[1L]], margin_reads = actual_reads,
        agrees = correct
      )
    }
    counter$collections <- 0L
    supported <- summarize_with_margins(input, answer = sum(x))
    stopifnot(inherits(supported, "arrow_dplyr_query"), counter$collections == 0L)
    stopifnot(identical(dplyr::collect(supported)$answer, 3L))
    absorbed <- observe(summarize_with_margins(input, paste(x, collapse = ",")))$result
    stopifnot(inherits(absorbed, if (startsWith(backend, "dataset")) {
      "arrow_not_supported"
    } else {
      "marginplyr_error"
    }))
  }
  do.call(rbind, rows)
}
arrow_results <- probe_arrow()
print(arrow_results, row.names = FALSE)
cat("\nEnvironment\n", R.version.string, "\n", R.version$platform, "\n", sep = "")
for (pkg in c("dplyr", "rlang", "tibble", "data.table", "arrow", "pkgload")) {
  cat(pkg, as.character(utils::packageVersion(pkg)), "\n")
}
matches <- c(vapply(scope_results, function(row) row$agrees, logical(1)),
             arrow_results$agrees)
cat("Matched contract", sum(matches), "of", length(matches), "\n")
if ("--assert" %in% commandArgs(trailingOnly = TRUE) && !all(matches)) {
  stop("A nested data-mask boundary changed the caller's evaluation", call. = FALSE)
}
