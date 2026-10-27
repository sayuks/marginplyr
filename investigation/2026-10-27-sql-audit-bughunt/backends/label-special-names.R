script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(script)), "backend-harness.R"))

main <- function(args) {
  with_backend_harness(args, function(drivers, config, metadata) {
    results <- list()
    for (backend in names(drivers)) {
      for (column in c(".env", ".data", "TRUE", "FALSE", "T", "F",
                       "...", "..1", "normal")) {
        data <- data.frame(g = c("a", "b"), v = c(1, 2))
        names(data)[[1L]] <- column
        input <- dplyr::copy_to(
          drivers[[backend]], data,
          paste0("audit_label_", backend, "_", length(results)), temporary = TRUE
        )
        spec <- rollup(dplyr::all_of(column))
        for (audited in c(FALSE, TRUE)) for (checked in c(FALSE, TRUE)) {
          for (verb in c("summarize", "expand")) {
            options(marginplyr.audit_sql = audited)
            value <- tryCatch(
              if (verb == "summarize") {
                summarize_with_margins(
                  input, total = sum(v, na.rm = TRUE), .grouping = spec,
                  .check_margin_label = checked
                )
              } else {
                expand_with_margins(
                  input, .grouping = spec, .check_margin_label = checked
                )
              }, error = identity
            )
            key <- paste(backend, column, audited, checked, verb, sep = "/")
            expected_error <- column %in% c("...", "..1") ||
              (identical(column, ".env") && checked)
            stopifnot(identical(inherits(value, "condition"), expected_error))
            if (identical(column, ".env") && checked) {
              stopifnot(grepl("rlang_ctxt_pronoun", conditionMessage(value),
                             fixed = TRUE))
            }
            results[[key]] <- c(
              public_backend_outcome(value),
              list(rows = if (audited) last_sent_queries(FALSE, TRUE) else NULL)
            )
            cat(key, if (expected_error) "EXPECTED BASELINE FAILURE" else "OK",
                "\n")
          }
        }
      }
    }
    stopifnot(length(results) == 216L)

    # A one-column source separates #930 from Boolean-word composition #928.
    env_controls <- list()
    for (backend in names(drivers)) for (source_value in c("a", "Total")) {
      input <- dplyr::copy_to(
        drivers[[backend]], data.frame(.env = source_value),
        paste0("audit_env_control_", length(env_controls)), temporary = TRUE
      )
      for (audited in c(FALSE, TRUE)) for (checked in c(FALSE, TRUE)) {
        options(marginplyr.audit_sql = audited)
        key <- paste(backend, source_value, audited, checked, sep = "/")
        value <- tryCatch(summarize_with_margins(
          input, rows = dplyr::n(),
          .grouping = rollup(dplyr::all_of(".env")),
          .check_margin_label = checked
        ), error = identity)
        stopifnot(identical(inherits(value, "condition"), checked))
        if (checked) {
          stopifnot(grepl("rlang_ctxt_pronoun", conditionMessage(value),
                         fixed = TRUE))
        }
        env_controls[[key]] <- c(
          public_backend_outcome(value),
          list(rows = if (audited) last_sent_queries(FALSE, TRUE) else NULL)
        )
      }
    }
    saveRDS(list(metadata = metadata, cases = results, env_controls = env_controls),
            file.path(config$output, "label-special-names.rds"))
    cat("216 named cases and 24 one-column collision controls matched baseline\n")
  })
}
main(commandArgs(TRUE))
