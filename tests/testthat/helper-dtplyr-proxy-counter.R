# Count zero-root evaluations at the dtplyr backend boundary. Public results
# cannot reveal duplicate or premature schema acquisition (ADR 0002). Callers
# use nonempty source fixtures, so zero-root evaluations identify isolated
# metadata even when caller head/collect methods are bypassed.
local_dtplyr_proxy_counter <- function(.env = parent.frame()) {
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  counter$source_evaluations <- 0L
  dt_eval <- getFromNamespace("dt_eval", "dtplyr")
  dt_sources <- getFromNamespace("dt_sources", "dtplyr")
  testthat::local_mocked_bindings(
    dt_eval = function(x) {
      root_rows <- vapply(dt_sources(x), nrow, integer(1))
      if (length(root_rows) > 0L && all(root_rows == 0L)) {
        counter$n <- counter$n + 1L
      } else {
        counter$source_evaluations <- counter$source_evaluations + 1L
      }
      dt_eval(x)
    },
    .package = "dtplyr",
    .env = .env
  )
  counter
}
