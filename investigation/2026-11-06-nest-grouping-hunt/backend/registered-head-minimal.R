Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = "."), quiet = TRUE)
requireNamespace("data.table", quietly = TRUE)
main <- function() {
  original <- utils::getS3method("head", "data.table")
  on.exit(registerS3method("head", "data.table", original,
                          envir = asNamespace("utils")))
  source <- data.table::data.table(g = c("a", "b"))
  before <- serialize(source, NULL)
  input <- dtplyr::lazy_dt(source)
  rows <- integer()
  method <- function(x, n = 6L, ...) {
    rows <<- c(rows, nrow(x))
    out <- original(x, n = n, ...)
    if (nrow(out) == 0L) data.table::set(out, j = "g", value = integer())
    out
  }
  registerS3method("head", "data.table", method, envir = asNamespace("utils"))
  stopifnot(identical(dplyr::collect(input)$g, c("a", "b")),
            identical(inspect_grouping(input, .grouping = grouping_set(g))$included,
                      "(g)"), length(rows) == 0L)
  result <- tryCatch(inspect_grouping(
    input, .grouping = grouping_set(where(is.character))
  ), error = identity)
  correct <- !inherits(result, "error") && identical(result$included, "(g)")
  refusal <- inherits(result, "marginplyr_error") &&
    grepl("Can't safely determine column metadata", conditionMessage(result),
          fixed = TRUE)
  cat("Included: ", if (inherits(result, "error")) conditionMessage(result) else
        result$included, "\nHead input rows: ", paste(rows, collapse = ", "),
      "\n", sep = "")
  stopifnot(identical(serialize(source, NULL), before))
  registerS3method("head", "data.table", original, envir = asNamespace("utils"))
  stopifnot(identical(inspect_grouping(
    input, .grouping = grouping_set(where(is.character))
  )$included, "(g)"))
  if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(correct || refusal, !any(rows > 0L))
  }
}
main()
