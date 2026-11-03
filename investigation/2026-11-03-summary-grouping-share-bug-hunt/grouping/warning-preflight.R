# Investigation artifact: 2026-11-03, no package implementation changes.
pkgload::load_all(quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
args <- commandArgs(trailingOnly = TRUE)
assert_contract <- "--assert" %in% args
out <- "investigation/2026-11-03-summary-grouping-share-bug-hunt/grouping"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
cat("SHA:", system("git rev-parse HEAD", intern = TRUE), "\n")
cat(R.version.string, "\n")
packages <- c("dplyr", "tidyselect", "rlang", "dtplyr", "data.table",
              "duckdb", "dbplyr", "DBI", "pkgload")
cat(paste(packages, vapply(packages, function(package) {
  as.character(utils::packageVersion(package))
}, character(1))), sep = "\n")
cat("\n")

run <- function() {
  old <- options(warn = 2, lifecycle_verbosity = "warning")
  on.exit(options(old), add = TRUE)
  data <- tibble::tibble(a = 1:2, b = c("x", "y"))
  step <- dtplyr::lazy_dt(data) |> dplyr::mutate(b = "z")
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(con, data, "warning_preflight")
  counter <- new.env(parent = emptyenv())
  counter$queries <- character()
  suppressMessages(do.call(trace, list(
    what = "dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI"), print = FALSE,
    tracer = function() {
      counter$queries <- c(counter$queries, get("statement", parent.frame()))
    }
  )))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI")
  )), add = TRUE, after = FALSE)
  capture <- function(expr, env = parent.frame()) {
    counter$queries <- character()
    warnings <- character()
    value <- tryCatch(withCallingHandlers(
      eval(expr, env), warning = function(cnd) {
        warnings <<- c(warnings, conditionMessage(cnd))
      }
    ), error = identity)
    error <- inherits(value, "error")
    message <- if (error) conditionMessage(value) else "<success>"
    data.frame(
      error = error,
      converted = error && grepl("converted from warning", message, fixed = TRUE),
      metadata_refusal = error && grepl("Can't safely determine", message, fixed = TRUE),
      queries = length(counter$queries),
      warning_count = length(warnings),
      condition_class = if (error) paste(class(value), collapse = "/") else "",
      message = message,
      sql = paste(counter$queries, collapse = "\n---\n"),
      stringsAsFactors = FALSE
    )
  }

  minimal <- capture(quote(inspect_grouping(
    remote, .grouping = grouping_set(one_of("absent"))
  )))
  cat("Minimal DuckDB result:\n")
  print(minimal)
  stopifnot(minimal$converted, minimal$queries == 1L)

  selectors <- list(
    unknown_one_of = quote(one_of("absent")),
    pronoun_deprecation = quote(.data$a)
  )
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
  consumers <- list(
    inspect = function(data, spec) inspect_grouping(data, .grouping = spec),
    summarize = function(data, spec) summarize_with_margins(
      data, count = dplyr::n(), .grouping = spec
    ),
    expand = function(data, spec) expand_with_margins(data, .grouping = spec),
    nest = function(data, spec) nest_with_margins(data, .grouping = spec),
    nest_by = function(data, spec) nest_by_with_margins(data, .grouping = spec)
  )
  inputs <- list(local = data, dtplyr = step, duckdb = remote)
  records <- list()
  index <- 0L
  for (selector_name in names(selectors)) {
    first <- selectors[[selector_name]]
    for (later in c("names", "typed")) {
      last <- if (later == "typed") quote(where(is.numeric)) else quote(b)
      for (constructor in constructors) {
        spec <- eval(rlang::call2(constructor, first, last))
        for (backend in names(inputs)) {
          for (consumer in names(consumers)) {
            if (backend == "duckdb" && consumer %in% c("nest", "nest_by")) {
              next
            }
            input <- inputs[[backend]]
            consume <- consumers[[consumer]]
            result <- capture(quote(consume(input, spec)))
            index <- index + 1L
            records[[index]] <- cbind(
              data.frame(selector = selector_name, later = later,
                         constructor = constructor, backend = backend,
                         consumer = consumer), result
            )
          }
        }
      }
    }
  }
  results <- do.call(rbind, records)
  utils::write.csv(results, file.path(out, "warning-preflight-results.csv"), row.names = FALSE)
  print(stats::aggregate(cbind(converted, metadata_refusal, queries) ~ backend + later,
                        results, sum))
  cat("Completed public calls:", nrow(results), "\n")
  stopifnot(nrow(results) == 260L)
  stopifnot(all(results$converted[results$backend == "local"]))
  stopifnot(all(results$converted[results$backend == "duckdb"]))
  stopifnot(all(results$queries[results$backend == "duckdb"] == 1L))
  dt_names_inspection <- results$backend == "dtplyr" &
    results$later == "names" & results$consumer == "inspect"
  dt_metadata <- results$backend == "dtplyr" & !dt_names_inspection
  stopifnot(all(results$metadata_refusal[dt_metadata]))
  stopifnot(all(results$converted[dt_names_inspection]))
  stopifnot(all(results$queries[results$backend != "duckdb"] == 0L))

  controls <- list()
  for (warn in c(1, 2)) {
    options(warn = warn)
    for (backend in names(inputs)) {
      input <- inputs[[backend]]
      for (position in c("fixed", "variable")) {
        expr <- if (position == "fixed") {
          quote(inspect_grouping(input, .by = one_of("absent")))
        } else {
          quote(inspect_grouping(input, .grouping = grouping_set(one_of("absent"))))
        }
        control <- capture(expr)
        controls[[length(controls) + 1L]] <- cbind(
          data.frame(warn = warn, backend = backend, position = position), control
        )
      }
    }
  }
  controls <- do.call(rbind, controls)
  utils::write.csv(controls, file.path(out, "warning-preflight-controls.csv"), row.names = FALSE)
  print(controls[c("warn", "backend", "position", "converted", "queries", "warning_count")])
  fixed <- controls$warn == 2 & controls$position == "fixed"
  stopifnot(all(controls$converted[fixed]), all(controls$queries[fixed] == 0L))
  options(warn = 2)
  oracle <- capture(quote(tidyselect::eval_select(
    rlang::quo(one_of("absent")), data
  )))
  stopifnot(oracle$converted, oracle$queries == 0L)
  if (assert_contract) {
    stopifnot(all(results$converted), all(results$queries == 0L))
  }
  invisible(results)
}
run()
