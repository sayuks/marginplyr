# Run from the checkout root with DuckDB and dtplyr installed.
pkgload::load_all(quiet = TRUE)

run <- function() {
  frame <- tibble::tibble(x = 1L, y = "y")
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(con, frame, "mixed_duplicate", temporary = TRUE)
  derived <- dplyr::mutate(dtplyr::lazy_dt(frame), y = toupper(y))

  queries <- new.env(parent = emptyenv())
  options(marginplyr.expand_grouping_probe_queries = queries)
  on.exit(options(marginplyr.expand_grouping_probe_queries = NULL), add = TRUE)
  suppressMessages(trace(
    "dbSendQuery",
    signature = c("duckdb_connection", "character"),
    tracer = quote({
      recorded <- getOption("marginplyr.expand_grouping_probe_queries")
      recorded$sql <- c(recorded$sql, statement)
    }),
    where = asNamespace("DBI"),
    print = FALSE
  ))
  on.exit(suppressMessages(untrace(
    "dbSendQuery",
    signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI")
  )), add = TRUE)

  specs <- list(
    names_only = grouping_sets(x, x, y),
    mixed_duplicate = grouping_sets(x, x, dplyr::where(is.character)),
    mixed_unique = grouping_sets(x, dplyr::where(is.character))
  )
  inputs <- list(local = frame, duckdb = remote, dtplyr = derived)
  verbs <- list(inspect = inspect_grouping, expand = expand_with_margins)
  results <- list()
  for (backend in names(inputs)) {
    for (case in names(specs)) {
      for (verb in names(verbs)) {
        queries$sql <- character()
        result <- tryCatch(
          verbs[[verb]](inputs[[backend]], .grouping = specs[[case]]),
          error = identity
        )
        message <- if (inherits(result, "error")) conditionMessage(result) else "success"
        duplicate <- grepl("Duplicate grouping sets", message, fixed = TRUE)
        metadata <- grepl("Can't safely determine column metadata", message, fixed = TRUE)
        stopifnot(!duplicate || inherits(result, "marginplyr_error"))
        if (length(queries$sql)) stopifnot(all(grepl("LIMIT 0", queries$sql, fixed = TRUE)))
        cat(backend, case, verb, "sent", length(queries$sql), "\n")
        cat(message, "\n")
        if (length(queries$sql)) cat(queries$sql, "\n")
        results[[length(results) + 1L]] <- data.frame(
          backend = backend, case = case, verb = verb,
          duplicate = duplicate, metadata = metadata,
          success = !inherits(result, "error"), sends = length(queries$sql)
        )
      }
    }
  }
  results <- do.call(rbind, results)
  pure <- results[results$case == "names_only", ]
  valid <- results[results$case == "mixed_unique", ]
  stopifnot(
    nrow(results) == 18L,
    all(pure$duplicate),
    all(pure$sends == 0L),
    all(valid$success[valid$backend != "dtplyr"]),
    all(valid$metadata[valid$backend == "dtplyr"]),
    all(valid$sends == as.integer(valid$backend == "duckdb"))
  )
  mixed <- results[results$case == "mixed_duplicate", ]
  if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(all(mixed$duplicate), all(mixed$sends == 0L))
  }
  print(results, row.names = FALSE)
}
run()
