script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(script)), "backend-harness.R"))

main <- function(args) {
  with_backend_harness(args, function(drivers, config, metadata) {
    verdicts <- get("share_dialect_verdicts", asNamespace("marginplyr"))
    saved_verdicts <- as.list(verdicts, all.names = TRUE)
    clear_cache <- function() {
      rm(list = ls(verdicts, all.names = TRUE), envir = verdicts)
    }
    on.exit({
      clear_cache()
      list2env(saved_verdicts, envir = verdicts)
    }, add = TRUE)
    capture <- NULL
    on.exit(stop_backend_query_capture(capture), add = TRUE)
    suspendInterrupts({
      capture <- start_backend_query_capture()
    })
    results <- list()
    run <- function(label, code) {
      capture$sql <- character()
      value <- tryCatch(force(code), error = identity)
      observed <- capture$sql
      rows <- last_sent_queries(details = TRUE)
      internal <- rows[rows$purpose != "result", ]
      stopifnot(identical(unname(internal$sql), unname(observed)))
      cat("\nCASE", label, "\n")
      print(rows[c("purpose", "source", "status")])
      cat("DBI dbSendQuery count", length(observed), "\n")
      for (i in seq_along(observed)) cat(i, observed[[i]], "\n")
      if (inherits(value, "condition")) {
        cat("PUBLIC ERROR", conditionMessage(value), "\n")
      }
      print(lapply(rows$conditions, as.data.frame))
      result <- c(public_backend_outcome(value),
                  list(rows = rows, submitted_sql = observed))
      results[[label]] <<- result
      result
    }
    margin <- function(input, check = FALSE) {
      summarize_with_margins(
        input, total = sum(v, na.rm = TRUE), .grouping = rollup(g),
        .check_margin_label = check
      )
    }
    share <- function(input, check = FALSE) {
      summarize_with_margins(
        input, total = sum(v, na.rm = TRUE), share = share_of_total(total),
        .grouping = rollup(g), .check_margin_label = check
      )
    }
    sources <- list()
    for (backend in names(drivers)) {
      input <- dplyr::copy_to(
        drivers[[backend]], data.frame(g = c("a", "b"), v = c(1, 2)),
        paste0("audit_live_", backend), temporary = TRUE
      )
      sources[[backend]] <- input
      run(paste(backend, "label off"), margin(input))
      run(paste(backend, "label on"), margin(input, TRUE))
      collision <- run(paste(backend, "label collision"),
                       margin(dplyr::mutate(input, g = "Total"), TRUE))
      stopifnot("marginplyr_error" %in% collision$classes,
                tail(collision$rows$status, 1L) == "returned")
      clear_cache()
      run(paste(backend, "cold share"), share(input, TRUE))
      run(paste(backend, "warm share"), share(input, TRUE))
    }
    clear_cache()
    pg <- drivers$pg
    on.exit({
      if (RPostgres::postgresIsTransacting(pg)) DBI::dbRollback(pg)
    }, add = TRUE)
    DBI::dbBegin(pg)
    transaction <- run("pg transaction cold share", share(sources$pg, TRUE))
    stopifnot(is.null(transaction$error),
              DBI::dbGetQuery(pg, "SELECT 1 AS intact")$intact == 1L)
    DBI::dbRollback(pg)
    internal <- transaction$rows[transaction$rows$purpose != "result", ]
    stopifnot(identical(internal$status, c(
      "returned", "returned", "errored", "returned", "returned", "returned",
      "returned"
    )), identical(internal$conditions[[3L]]$handling, "handled"))
    saveRDS(list(metadata = metadata, cases = results, transaction = transaction),
            file.path(config$output, "internal-live.rds"))
    cat("\n16 live scenarios passed; transaction preserved; ",
        "all 7 transaction-internal SQL rows match dbSendQuery\n", sep = "")
  })
}
main(commandArgs(TRUE))
