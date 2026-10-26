# Checks the retained lifecycle observations without backend work.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
capture_dir <- args[[1L]]
matrix <- readRDS(file.path(capture_dir, "lifecycle.rds"))
stopifnot(nrow(matrix$cases) == 600L, length(matrix$observations) == 600L,
          length(matrix$failures) == 0L, matrix$status_checks == 540L)
checked <- 0L
for (observation in matrix$observations) {
  stopifnot(identical(observation$off, observation$on))
  config <- observation$case
  pending <- config$ending %in% c("error", "interrupt") ||
    (config$ending == "warning" && config$warn == 2)
  if (!(config$cleanup == "escape" && pending)) {
    body_status <- switch(config$ending,
      normal = "returned",
      warning = if (config$warn == 2) "errored" else "returned",
      error = "errored", interrupt = "interrupted", escape = "unknown"
    )
    expected <- switch(config$cleanup,
      normal = body_status,
      warning = if (config$warn == 2) "errored" else body_status,
      error = "errored", interrupt = "interrupted", escape = "unknown"
    )
    stopifnot(identical(observation$rows$status, expected))
    checked <- checked + 1L
  }
  stopifnot(all(observation$rows$conditions[[1L]]$phase == "invocation"))
}
stopifnot(checked == 540L)
retention <- readRDS(file.path(capture_dir, "retention.rds"))
stopifnot(nrow(retention$cases) == 24L,
          length(retention$observations) == 24L, retention$weakrefs == 88L)
for (observation in retention$observations) {
  ending <- observation$case$ending
  expected <- switch(ending,
    normal = "returned", error = "errored",
    interrupt = "interrupted", escape = "unknown"
  )
  stopifnot(identical(observation$status, expected),
            nrow(observation$conditions) ==
              if (ending %in% c("error", "interrupt")) 2L else 1L)
}
cat("PASS 600 lifecycle pairs, 540 terminal-status assertions,",
    "24 retention captures (88 weak references asserted during generation).\n")
