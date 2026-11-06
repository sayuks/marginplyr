args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
source_dir <- Sys.getenv("MARGINPLYR_HUNT_SOURCE")
if (!nzchar(source_dir)) stop("Set MARGINPLYR_HUNT_SOURCE to the investigated source directory.")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
evidence_dir <- dirname(normalizePath(script))
pkgload::load_all(source_dir, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))

run_converter <- function(mask = TRUE, shape = "root", position = "dimension", consumer = "inspect") {
  source <- data.table::data.table(g = c("a", "b"), v = 1:2)
  before <- serialize(source, NULL)
  q <- dtplyr::lazy_dt(source)
  q <- switch(shape, root = q, cached = compute(q),
              filtered = filter(q, v > 0L), arithmetic = mutate(q, v = v + 1L))
  ns <- asNamespace("data.table")
  original <- utils::getS3method("as.data.table", "dtplyr_step", envir = ns)
  on.exit(registerS3method("as.data.table", "dtplyr_step", original, envir = ns))
  callback_rows <- integer()
  replacement <- function(x, ...) {
    result <- original(x, ...)
    callback_rows <<- c(callback_rows, nrow(result))
    if (nrow(result) == 0L) data.table::set(result, j = "g", value = integer())
    result
  }
  if (mask) registerS3method("as.data.table", "dtplyr_step", replacement, envir = ns)
  native <- collect(q)
  stopifnot(identical(native$g, c("a", "b")), identical(callback_rows, integer()))
  value <- tryCatch({
    fun <- switch(consumer, inspect = inspect_grouping, nest = nest_with_margins, nest_by = nest_by_with_margins)
    result <- if (position == "dimension") {
      fun(q, .grouping = grouping_set(where(is.character)))
    } else {
      fun(q, .by = where(is.character))
    }
    if (consumer == "inspect") {
      columns <- if (position == "dimension") result$included else result$fixed
      good <- identical(columns, "(g)")
    } else {
      if (consumer == "nest") result <- collect(result)
      good <- "g" %in% names(result) && nrow(result) == 2L
    }
    list(status = if (good) "correct" else "false metadata or partitions", result = result)
  }, error = function(e) {
    if (inherits(e, "marginplyr_error")) list(status = "safe refusal", result = conditionMessage(e)) else stop(e)
  })
  stopifnot(identical(serialize(source, NULL), before))
  list(status = value$status, callback_rows = callback_rows, result = value$result)
}

if ("--matrix" %in% args) {
  grid <- expand.grid(mask = c(FALSE, TRUE), shape = c("root", "cached", "filtered", "arithmetic"),
                      position = c("dimension", "fixed"), consumer = c("inspect", "nest", "nest_by"),
                      stringsAsFactors = FALSE)
  results <- lapply(seq_len(nrow(grid)), function(i) {
    row <- grid[i, ]
    result <- do.call(run_converter, as.list(row))
    data.frame(row, status = result$status, callback_rows = paste(result$callback_rows, collapse = ","))
  })
  results <- do.call(rbind, results)
  write.csv(results, Sys.getenv("MARGINPLYR_HUNT_OUTPUT", file.path(evidence_dir, "converter-matrix.csv")), row.names = FALSE)
  print(with(results, table(mask, shape, status)))
  stopifnot(all(results$status[!results$mask] == "correct"))
} else {
  control <- run_converter(FALSE)
  stopifnot(identical(control$status, "correct"))
  bad <- run_converter()
  print(bad$result)
  cat("Status:", bad$status, "\nConverter callback rows:", paste(bad$callback_rows, collapse = ","), "\n")
  if ("--assert" %in% args) stopifnot(bad$status %in% c("correct", "safe refusal"))
}
