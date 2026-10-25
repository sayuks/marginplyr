# Investigated: 2026-10-25. A diagnostic harness, not a package test.
# Run from the repository root:
#   Rscript investigation/2026-10-25-sql-audit-bug-hunt/compound-condition.R
# Requires the development library, including testthat and RSQLite. The fixture
# replaces only the owned collection at runtime; no ordinary driver is claimed
# to signal this unusual multi-class condition. Exit 1 means an invariant failed.

pkgload::load_all(quiet = TRUE)

# Exercises a public construction call whose requested label check reaches the
# owned invocation observer. Each fresh condition is signalled exactly once.
run_case <- function(remote, terminating) {
  leaf <- errorCondition("driver failed", class = "warning")
  testthat::local_mocked_bindings(
    collect = function(...) {
      if (terminating) {
        stop(leaf)
      }
      signalCondition(leaf)
      data.frame(g = 0)
    },
    .package = "dplyr"
  )
  seen <- 0L
  outcome <- tryCatch(
    withCallingHandlers(
      summarize_with_margins(
        remote, total = sum(v), .grouping = rollup(g),
        .check_margin_label = TRUE
      ),
      condition = function(cnd) {
        if (identical(cnd, leaf)) {
          seen <<- seen + 1L
        }
      }
    ),
    error = identity
  )
  rows <- last_sent_queries(include_result = FALSE, details = TRUE)
  stopifnot(nrow(rows) == 1L)
  conditions <- rows$conditions[[1]]
  cat("Fixture=", if (terminating) "stop" else "signalCondition",
      "; external_signals=", seen,
      "; stored_summaries=", nrow(conditions),
      "; status=", rows$status, "\n", sep = "")
  print(conditions)
  # Both signals reach the outer exiting error handler because `leaf` inherits
  # error; even signalCondition() therefore unwinds the owned invocation here.
  seen == 1L && nrow(conditions) == 1L &&
    identical(conditions$classes[[1]], class(leaf)) &&
    identical(conditions$message, conditionMessage(leaf)) &&
    identical(conditions$phase, "invocation") &&
    identical(rows$status, "errored") && identical(outcome, leaf)
}

main <- function() {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "src")
  signalled <- run_case(remote, terminating = FALSE)
  stopped <- run_case(remote, terminating = TRUE)
  if (!signalled || !stopped) {
    stop("One signal must produce exactly one stored condition summary.")
  }
  cat("One signal produced one stored summary in both cases.\n")
}
main()
