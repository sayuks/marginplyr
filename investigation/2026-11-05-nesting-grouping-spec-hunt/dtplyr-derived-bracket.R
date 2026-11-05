suppressMessages(pkgload::load_all(".", quiet = TRUE))
assert <- "--assert" %in% commandArgs(trailingOnly = TRUE)

source <- data.table::data.table(g = c("a", "b"), v = 1:2)
seen <- integer()
query <- local({
  `[` <- function(x, ...) {
    seen <<- c(seen, nrow(x))
    out <- base::`[`(x, ...)
    if (nrow(x) == 0L) {
      data.table::set(out, j = "g", value = integer())
    }
    out
  }
  dplyr::mutate(dtplyr::lazy_dt(source), v2 = v + 1L)
})
native <- dplyr::collect(query)
stopifnot(identical(native$g, c("a", "b")), identical(seen, 2L))
selection <- grouping_set(dplyr::where(is.character))
stopifnot(identical(inspect_grouping(native, .grouping = selection)$included, "(g)"))
seen <- integer()
stopifnot(identical(inspect_grouping(query, .grouping = grouping_set(g))$included,
                    "(g)"), length(seen) == 0L)

observe <- function(label, call) {
  seen <<- integer()
  answer <- tryCatch(force(call), error = identity)
  if (inherits(answer, "marginplyr_error")) {
    return(!length(seen) && grepl("safely determine column metadata",
                                 conditionMessage(answer), fixed = TRUE))
  }
  sizes <- seen
  if (inherits(answer, "dtplyr_step")) {
    answer <- dplyr::collect(answer)
  }
  actual <- if (label == "inspect") answer$included else names(answer)
  expected <- if (label == "inspect") "(g)" else c("g", "data")
  cat(label, "construction/read sizes", toString(sizes),
      "columns", toString(actual), "\n")
  identical(actual, expected) && !any(sizes == 0L)
}

observations <- c(
  observe("inspect", inspect_grouping(query, .grouping = selection)),
  observe("nest", nest_with_margins(query, .grouping = selection)),
  observe("rowwise", nest_by_with_margins(query, .grouping = selection))
)
stopifnot(identical(source$g, c("a", "b")))
if (assert && !all(observations)) {
  stop("Derived metadata accepted a caller-dispatched zero-row bracket.")
}
