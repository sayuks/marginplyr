args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
source <- Sys.getenv("MARGINPLYR_HUNT_SOURCE")
if (!nzchar(source)) stop("Set MARGINPLYR_HUNT_SOURCE to the investigated source directory.")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
evidence_dir <- dirname(normalizePath(script))
pkgload::load_all(source, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))

run <- function(mask = TRUE, consumer = "inspect", position = "dimension",
                shape = "root", mutable = FALSE) {
  ordinary <- get("[.data.table", asNamespace("data.table"))
  previous <- utils::getS3method("[", "data.table", envir = asNamespace("data.table"))
  calls <- integer()
  method <- function(x, ...) {
    calls <<- c(calls, nrow(x))
    value <- ordinary(x, ...)
    if (nrow(value) == 0L) data.table::set(value, j = "g", value = integer())
    value
  }
  if (mask) registerS3method("[", "data.table", method, envir = baseenv())
  on.exit(registerS3method("[", "data.table", previous, envir = baseenv()))
  d <- data.table::data.table(g = c("a", "b"), v = 1:2)
  before <- serialize(d, NULL)
  q <- dtplyr::lazy_dt(d, immutable = !mutable)
  q <- switch(shape,
    root = q,
    grouped = group_by(q, v),
    cached = compute(q),
    filtered = filter(q, v > 0L),
    arithmetic = mutate(q, v = v + 1L)
  )
  native <- collect(q)
  stopifnot(identical(native$g, c("a", "b")))
  native_calls <- calls
  calls <- integer()
  out <- tryCatch({
    fun <- switch(consumer,
      inspect = inspect_grouping,
      nest = nest_with_margins,
      nest_by = nest_by_with_margins
    )
    value <- if (position == "dimension") {
      fun(q, .grouping = grouping_set(where(is.character)))
    } else {
      fun(q, .by = where(is.character))
    }
    if (consumer == "inspect") {
      present <- grepl("g", if (position == "dimension") value$included else value$fixed, fixed = TRUE)
      list(status = if (all(present)) "correct" else "false metadata", value = value)
    } else {
      if (consumer == "nest") value <- collect(value)
      list(status = if ("g" %in% names(value) && nrow(value) == 2L) "correct" else "false partitions", value = value)
    }
  }, error = function(e) {
    if (!inherits(e, "marginplyr_error")) stop(e)
    status <- if (grepl("Can't supply `.by`", conditionMessage(e), fixed = TRUE)) {
      "structural refusal"
    } else {
      "safe metadata refusal"
    }
    list(status = status, value = conditionMessage(e))
  })
  stopifnot(identical(serialize(d, NULL), before))
  list(status = out$status, calls = calls, native_calls = native_calls, value = out$value)
}

if ("--matrix" %in% args) {
  grid <- expand.grid(mask = c(FALSE, TRUE), consumer = c("inspect", "nest", "nest_by"),
                      position = c("dimension", "fixed"),
                      shape = c("root", "grouped", "cached", "filtered", "arithmetic"),
                      stringsAsFactors = FALSE)
  results <- lapply(seq_len(nrow(grid)), function(i) {
    row <- grid[i, ]
    result <- do.call(run, as.list(row))
    data.frame(row, status = result$status, calls = paste(result$calls, collapse = ","),
               native_calls = paste(result$native_calls, collapse = ","))
  })
  results <- do.call(rbind, results)
  write.csv(results, Sys.getenv("MARGINPLYR_HUNT_OUTPUT", file.path(evidence_dir, "root-bracket-matrix.csv")), row.names = FALSE)
  print(with(results, table(mask, shape, status)))
  baseline <- !results$mask
  structural_refusal <- results$shape == "grouped" & results$position == "fixed"
  stopifnot(all(results$status[baseline & !structural_refusal] == "correct"))
  stopifnot(all(results$status[baseline & structural_refusal] == "structural refusal"))
} else {
  control <- run(mask = FALSE)
  stopifnot(identical(control$status, "correct"))
  bad <- run()
  print(bad$value)
  cat("Status:", bad$status, "\nBracket callback row counts:", paste(bad$calls, collapse = ","), "\n")
  name_control <- local({
    ordinary <- get("[.data.table", asNamespace("data.table"))
    on.exit(registerS3method("[", "data.table", ordinary, envir = baseenv()))
    registerS3method("[", "data.table", function(x, ...) stop("name-only inspection must not dispatch"), envir = baseenv())
    q <- dtplyr::lazy_dt(data.table::data.table(g = c("a", "b")))
    inspect_grouping(q, .grouping = grouping_set(g))
  })
  stopifnot(identical(name_control$included, "(g)"))
  if ("--assert" %in% args) stopifnot(bad$status %in% c("correct", "safe metadata refusal"))
}
