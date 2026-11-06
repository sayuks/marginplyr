args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 1L)
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(args[[1L]], quiet = TRUE)
invisible(loadNamespace("dtplyr"))

probe <- function() {
  converter <- "--converter" %in% args
  generic <- if (converter) "as.data.table" else "["
  class <- if (converter) "dtplyr_step" else "data.table"
  namespace <- if (converter) asNamespace("data.table") else baseenv()
  previous <- utils::getS3method(generic, class, envir = namespace)
  ordinary <- previous
  on.exit(registerS3method(generic, class, previous, envir = namespace))
  input <- data.table::data.table(region = c("East", "West"), units = c(11L, 29L))
  before <- serialize(input, NULL)
  lazy <- dtplyr::lazy_dt(input)
  expected <- inspect_grouping(lazy, .grouping = grouping_set(region), .format = "list")
  control <- inspect_grouping(lazy, .grouping = grouping_set(where(is.character)), .format = "list")
  stopifnot(identical(expected, control))
  seen <- integer()
  replacement <- function(x, ...) {
    result <- ordinary(x, ...)
    seen <<- c(seen, if (converter) nrow(result) else nrow(x))
    if (nrow(result) == 0L && "region" %in% names(result)) {
      data.table::set(result, j = "region", value = integer())
    }
    result
  }
  registerS3method(generic, class, replacement, envir = namespace)
  stopifnot(identical(dplyr::collect(lazy)$region, c("East", "West")))
  stopifnot(identical(inspect_grouping(lazy, .grouping = grouping_set(region), .format = "list"), expected))
  statuses <- character()
  for (fixed in c(FALSE, TRUE)) {
    for (consumer in c("inspect", "nest", "nest_by")) {
      fun <- switch(consumer, inspect = inspect_grouping,
                    nest = nest_with_margins, nest_by = nest_by_with_margins)
      seen <- integer()
      result <- tryCatch({
        value <- if (fixed) fun(lazy, .by = where(is.character)) else {
          fun(lazy, .grouping = grouping_set(where(is.character)))
        }
        if (consumer == "nest") value <- dplyr::collect(value)
        correct <- if (consumer == "inspect") {
          identical(if (fixed) value$fixed else value$included, "(region)")
        } else {
          "region" %in% names(value) && nrow(value) == 2L &&
            identical(sort(vapply(value$data, nrow, integer(1))), c(1L, 1L))
        }
        if (correct) "correct" else "wrong grain"
      }, marginplyr_error = function(e) "safe refusal")
      statuses <- c(statuses, result)
      cat(if (fixed) "fixed" else "dimension", consumer, result,
          "callback rows:", paste(seen, collapse = ","), "\n")
    }
  }
  stopifnot(identical(serialize(input, NULL), before))
  registerS3method(generic, class, previous, envir = namespace)
  stopifnot(identical(inspect_grouping(lazy, .grouping = grouping_set(where(is.character)), .format = "list"), expected))
  if ("--contract" %in% args) stopifnot(all(statuses %in% c("correct", "safe refusal")))
  else stopifnot(identical(statuses, rep("wrong grain", 6L)))
}
probe()
cat("Native, name-only, restored-registration and source-immutability controls passed.\n")
