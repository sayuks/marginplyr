# Internal observer seam with a literal query and synthetic operation.
# No package function is replaced. Between attempts only the saved rows reset.
# Exit 1 reproduces post-unwind invoking; exit 2 is an inconclusive bounded run.
# The public entry path is separately checked by status-window.R fault injection.
pkgload::load_all(quiet = TRUE)
options(marginplyr.audit_sql = TRUE)
inspect_grouping(dbplyr::lazy_frame(v = 1))
with_query <- getFromNamespace("with_sent_query", "marginplyr")
state <- getFromNamespace("sent_queries", "marginplyr")$latest
empty <- state$record$rows
sql <- dbplyr::sql("SELECT 1")
run <- function() {
  target <- Sys.getpid()
  sender <- parallel::mcparallel({
    Sys.sleep(0.1)
    deadline <- proc.time()[["elapsed"]] + 18
    while (proc.time()[["elapsed"]] < deadline) {
      tools::pskill(target, signal = 2L)
      Sys.sleep(0.01)
    }
  }, silent = TRUE, mc.set.seed = FALSE)
  clean <- function() {
    tools::pskill(sender$pid, signal = 9L)
    suppressWarnings(parallel::mccollect(sender, wait = TRUE))
  }
  on.exit(clean(), add = TRUE)
  started <- proc.time()[["elapsed"]]
  caught <- 0L
  counts <- integer()
  for (i in seq_len(50000L)) {
    if (proc.time()[["elapsed"]] - started > 18) break
    state$record$rows <- empty
    value <- tryCatch(allowInterrupts(with_query("fixture", sql, 42L)), interrupt = identity)
    caught <- caught + inherits(value, "interrupt")
    rows <- last_sent_queries(FALSE, TRUE)
    status <- if (nrow(rows)) rows$status else "no_row"
    if (!status %in% names(counts)) counts[[status]] <- 0L
    counts[[status]] <- counts[[status]] + 1L
    if (identical(status, "invoking")) {
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
  if ("invoking" %in% names(counts)) 1L else 2L
}
code <- suspendInterrupts(run())
quit(status = code, save = "no")
