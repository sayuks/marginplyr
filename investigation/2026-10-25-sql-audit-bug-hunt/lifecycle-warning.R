# Investigated: 2026-10-25. Run from the repository root in a fresh R process.
# Each comparison uses separate child processes so lifecycle frequency state
# cannot leak from the unaudited control into the audited experiment.
args <- commandArgs(trailingOnly = TRUE)
child <- length(args) > 0L && identical(args[[1L]], "--child")

run_child <- function(backend, audited, warning_level, output) {
  pkgload::load_all(".", quiet = TRUE)
  Sys.unsetenv("TESTTHAT")
  options(marginplyr.audit_sql = audited, warn = warning_level)
  con <- switch(backend,
    sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  )
  on.exit({
    if (backend == "duckdb") {
      DBI::dbDisconnect(con, shutdown = TRUE)
    } else {
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "audit_source")
  remote <- dplyr::mutate(
    remote,
    g2 = if_else(str_like(.data$g, "a", ignore_case = FALSE), .data$g, .data$g)
  )
  observed <- character()
  value <- tryCatch(
    withCallingHandlers(
      marginplyr::summarize_with_margins(
        remote, total = sum(.data$v, na.rm = TRUE),
        .grouping = marginplyr::rollup(dplyr::all_of("g2")),
        .check_margin_label = TRUE
      ),
      warning = function(cnd) {
        observed <<- c(observed, conditionMessage(cnd))
      }
    ),
    error = identity
  )
  saveRDS(list(
    returned = inherits(value, "tbl_sql"), warnings = observed,
    error = if (inherits(value, "error")) conditionMessage(value) else NULL,
    rows = if (audited) marginplyr::last_sent_queries(details = TRUE) else NULL
  ), output)
}

compare <- function(backend) {
  script <- normalizePath(sub("^--file=", "", grep(
    "^--file=", commandArgs(), value = TRUE
  )))
  workspace <- tempfile("sql-audit-lifecycle-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  mismatches <- logical()
  for (warning_level in c(0L, 2L)) {
    results <- list()
    for (setting in c("off", "on")) {
      output <- file.path(workspace, paste0(setting, "-", warning_level, ".rds"))
      log <- suppressWarnings(system2(
        file.path(R.home("bin"), "Rscript"),
        c("--vanilla", shQuote(script), "--child", backend, setting,
          warning_level, shQuote(output)),
        stdout = TRUE, stderr = TRUE
      ))
      status <- attr(log, "status")
      if (!is.null(status) && status != 0L) {
        writeLines(log)
        stop("The child process failed before producing a comparison.")
      }
      results[[setting]] <- readRDS(output)
      result <- results[[setting]]
      cat("BACKEND", backend, "AUDIT", setting, "WARN", warning_level,
          "RETURNED", result$returned,
          "WARNING_SIGNALS", length(result$warnings), "\n")
      if (!is.null(result$rows)) {
        print(result$rows[c("purpose", "status")])
      }
    }
    mismatches <- c(mismatches,
      !identical(results$off$returned, results$on$returned) ||
        !identical(results$off$warnings, results$on$warnings)
    )
  }
  if (any(mismatches)) {
    stop("Auditing changed warning delivery or whether the public call returned.")
  }
  cat("MATCH: audited and unaudited controls agreed.\n")
}

if (child) {
  stopifnot(length(args) == 5L, args[[2L]] %in% c("sqlite", "duckdb"))
  run_child(args[[2L]], identical(args[[3L]], "on"),
            as.integer(args[[4L]]), args[[5L]])
} else {
  backend <- if (length(args)) args[[1L]] else "sqlite"
  stopifnot(backend %in% c("sqlite", "duckdb"))
  compare(backend)
}
