# Bounded local status-lifetime check. No package function or binding is replaced.
# Native cancellation targets this disposable R process only.
pkgload::load_all(quiet = TRUE)
options(marginplyr.audit_sql = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
remote <- dplyr::copy_to(con, data.frame(g = c("a", "b"), v = c(1, 2)), "status_public_source")
run <- function() {
  target <- Sys.getpid()
  sender <- parallel::mcparallel({
    Sys.sleep(0.1)
    deadline <- proc.time()[["elapsed"]] + 150
    while (proc.time()[["elapsed"]] < deadline) {
      tools::pskill(target, signal = 2L)
      Sys.sleep(0.025)
    }
  }, silent = TRUE, mc.set.seed = FALSE)
  clean <- function() {
    tools::pskill(sender$pid, signal = 9L)
    suppressWarnings(parallel::mccollect(sender, wait = TRUE))
  }
  on.exit(clean(), add = TRUE)
  started <- proc.time()[["elapsed"]]
  caught <- 0L
  violation <- FALSE
  counts <- integer()
  for (i in seq_len(10000L)) {
    if (proc.time()[["elapsed"]] - started > 150) break
    invisible(inspect_grouping(data.frame(g = 1)))
    value <- tryCatch(allowInterrupts(summarize_with_margins(
      remote, total = sum(v, na.rm = TRUE), .grouping = rollup(g),
      .check_margin_label = TRUE
    )), interrupt = identity)
    caught <- caught + inherits(value, "interrupt")
    rows <- last_sent_queries(FALSE, TRUE)
    status <- if (nrow(rows)) paste(rows$status, collapse = ",") else "no_row"
    if (!status %in% names(counts)) counts[[status]] <- 0L
    counts[[status]] <- counts[[status]] + 1L
    if (any(rows$status == "invoking")) {
      violation <- TRUE
      cat("VIOLATION iteration", i, "caught", caught, "status", status,
          "result_interrupt", inherits(value, "interrupt"), "\n")
      print(rows)
      break
    }
  }
  clean()
  on.exit(NULL)
  tryCatch(allowInterrupts(for (j in seq_len(10000L)) invisible(j)), interrupt = function(cnd) NULL)
  cat("iterations", i, "caught", caught, "\n")
  print(counts)
  if (violation) 1L else 2L
}
code <- suspendInterrupts(run())
DBI::dbDisconnect(con)
quit(status = code, save = "no")
