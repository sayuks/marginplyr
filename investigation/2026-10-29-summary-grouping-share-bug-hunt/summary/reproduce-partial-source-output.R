args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) normalizePath(args[[1L]]) else getwd()
suppressPackageStartupMessages(library(dplyr))
devtools::load_all(repo, quiet = TRUE)
run <- function() {
  sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  duckdb <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(sqlite), add = TRUE)
  on.exit(DBI::dbDisconnect(duckdb, shutdown = TRUE), add = TRUE)
  data <- tibble(v = 1)
  sources <- list(
    dtplyr = dtplyr::lazy_dt(data),
    sqlite = copy_to(sqlite, data, "source"),
    duckdb_native = copy_to(duckdb, data, "source")
  )
  failures <- 0L
  for (output in c(".", ".da", ".dat")) {
    dots <- setNames(rlang::quos(sum(v, na.rm = TRUE)), output)
    for (backend in names(sources)) {
      source <- sources[[backend]]
      expected <- collect(summarise(source, !!!dots))
      actual <- tryCatch(
        collect(summarize_with_margins(source, !!!dots)), error = identity
      )
      cat("BACKEND", backend, "OUTPUT", output, "\n")
      cat("DPLYR\n"); print(expected)
      cat("MARGIN\n")
      if (inherits(actual, "error")) {
        cat(conditionMessage(actual), "\n")
        failures <- failures + 1L
      } else {
        print(actual)
        stopifnot(isTRUE(all.equal(actual, expected)))
      }
    }
    source <- sources$duckdb_native
    actual <- tryCatch(
      collect(summarize_with_margins(
        source, !!!dots, .id = "set", .duplicates = "keep"
      )), error = identity
    )
    cat("BACKEND duckdb_portable OUTPUT", output, "\n")
    if (inherits(actual, "error")) {
      cat(conditionMessage(actual), "\n")
      failures <- failures + 1L
    } else {
      print(actual)
      stopifnot(identical(actual$set, 1L), identical(actual[[output]], 1))
    }
  }
  cat("FAILED DESIRED-BEHAVIOUR CHECKS", failures, "\n")
  if (failures) {
    stop("BUG: early injection lets output names partially capture a backend input.",
         call. = FALSE)
  }
}
run()
