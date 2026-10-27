script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(script)), "backend-harness.R"))

main <- function(args) {
  with_backend_harness(args, function(drivers, config, metadata) {
    capture <- NULL
    on.exit(stop_backend_query_capture(capture), add = TRUE)
    suspendInterrupts({
      capture <- start_backend_query_capture()
    })
    results <- list()
    for (backend in names(drivers)) {
      for (type in c("normal", "factor", "empty", "missing", "unicode_names")) {
        data <- data.frame(g = c("a", "b"), v = c(1, 2))
        if (type == "factor") data$g <- factor(data$g, levels = c("a", "b", "Total"))
        if (type == "empty") data <- data[FALSE, ]
        if (type == "missing") data$g <- c(NA_character_, "a")
        if (type == "unicode_names") names(data) <- c("地域", "売上")
        input <- dplyr::copy_to(
          drivers[[backend]], data, paste0("audit_matrix_", backend, "_", type),
          temporary = TRUE
        )
        group <- names(data)[[1L]]
        measure <- names(data)[[2L]]
        for (verb in c("summarize", "expand", "inspect")) {
          for (label in c("Total", "NA", "NULL")) {
            key <- paste(backend, type, verb, label, sep = "/")
            actual_label <- switch(label, Total = "Total", "NA" = NA_character_,
                                   "NULL" = NULL)
            capture$sql <- character()
            value <- tryCatch({
              grouping <- rollup(dplyr::all_of(group))
              if (verb == "summarize") {
                summarize_with_margins(
                  input, total = sum(!!rlang::sym(measure), na.rm = TRUE),
                  .grouping = grouping, .margin_label = actual_label,
                  .check_margin_label = TRUE
                )
              } else if (verb == "expand") {
                expand_with_margins(
                  input, .grouping = grouping, .margin_label = actual_label,
                  .check_margin_label = TRUE
                )
              } else {
                inspect_grouping(input, .grouping = grouping)
              }
            }, error = identity)
            observed <- capture$sql
            rows <- last_sent_queries(FALSE, TRUE)
            expected_collision <- backend == "duck" && type == "factor" &&
              label == "Total" && verb != "inspect"
            stopifnot(identical(inherits(value, "condition"), expected_collision))
            if (expected_collision) stopifnot(inherits(value, "marginplyr_error"))
            stopifnot(identical(unname(rows$sql), unname(observed)),
                      all(rows$status == "returned"),
                      all(vapply(rows$conditions, nrow, integer(1)) == 0L))
            results[[key]] <- c(public_backend_outcome(value),
                               list(rows = rows, submitted_sql = observed))
          }
        }
      }
    }
    stopifnot(length(results) == 135L)
    saveRDS(list(metadata = metadata, cases = results),
            file.path(config$output, "internal-matrix.rds"))
    cat("135 live cases passed: exact internal SQL vs DBI dbSendQuery, ",
        "returned status, empty conditions\n", sep = "")
    print(table(vapply(results, function(case) length(case$submitted_sql),
                       integer(1))))
  })
}
main(commandArgs(TRUE))
