# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args <- commandArgs(TRUE)
root <- if (length(args)) args[[1L]] else "."
pkgload::load_all(root, quiet = TRUE)

# Data.table conversion itself cannot carry this POSIXlt column as one row.
x <- tibble::tibble(g = as.POSIXlt("2026-10-26", tz = "Asia/Tokyo"), v = 1L)
converted <- dplyr::collect(dplyr::mutate(dtplyr::lazy_dt(x), v = v))
stopifnot(nrow(x) == 1L, nrow(converted) == 11L)
cat("POSIXlt ordinary dtplyr mutate: 1 input row became 11 rows\n")

for (g in list(vctrs::list_of(1L), blob::blob(as.raw(1)))) {
  lazy <- dtplyr::lazy_dt(tibble::tibble(g = g, v = 1L))
  error <- tryCatch(
    dplyr::collect(dplyr::summarize(lazy, n = dplyr::n(), .by = g)),
    error = identity
  )
  stopifnot(
    inherits(error, "error"),
    grepl("type 'list'", conditionMessage(error), fixed = TRUE)
  )
  cat("list-class ordinary dtplyr grouping refusal: PASS\n")
}
for (g in list(as.raw(1), 1 + 1i)) {
  lazy <- dtplyr::lazy_dt(tibble::tibble(g = g, v = 1L))
  error <- tryCatch(
    dplyr::collect(dplyr::union_all(lazy, lazy)),
    error = identity
  )
  stopifnot(
    inherits(error, "error"),
    grepl("unsupported column type", conditionMessage(error), fixed = TRUE)
  )
  cat("raw/complex ordinary dtplyr union refusal: PASS\n")
}

# hms's own empty character conversion supplies a logical vector.
empty_hms <- hms::hms()
stopifnot(identical(as.character(empty_hms), logical()))
x <- tibble::tibble(g = empty_hms)
lazy <- dtplyr::lazy_dt(x)
details <- dplyr::mutate(lazy, g = as.character(g))
totals <- dplyr::mutate(lazy, g = "Total")
error <- tryCatch(
  dplyr::collect(dplyr::union_all(details, totals)),
  error = identity
)
stopifnot(
  inherits(error, "error"),
  grepl("'logical'", conditionMessage(error), fixed = TRUE)
)
cat("empty hms conversion and ordinary union refusal: PASS\n")
