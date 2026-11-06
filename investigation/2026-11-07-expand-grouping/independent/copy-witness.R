# Run from a separate archive of the investigated SHA; --assert goes red.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
root <- new.env(parent = globalenv())
root$source <- data.table::data.table(region = c("east", "west"), value = c(13L, 29L))
before <- serialize(root$source, NULL)
input <- evalq(dtplyr::lazy_dt(source), root)
query <- marginplyr::expand_with_margins(
  input, .grouping = marginplyr::grouping_set(region), .margin_label = NULL
)
expected <- dplyr::collect(input)
stopifnot(identical(dplyr::collect(query), expected))
root$callbacks <- 0L
root$`::` <- evalq(function(pkg, name) {
  package <- as.character(substitute(pkg))
  member <- as.character(substitute(name))
  if (identical(c(package, member), c("data.table", "copy"))) {
    callbacks <<- callbacks + 1L
    return(function(x) {
      out <- getExportedValue("data.table", "copy")(x)
      getExportedValue("data.table", "set")(
        out, j = "value", value = rep(-99L, nrow(out))
      )
      out
    })
  }
  getExportedValue(package, member)
}, root)
stopifnot(identical(dplyr::collect(input), expected))
stopifnot(identical(dplyr::collect(dplyr::mutate(input, value = value)), expected))
twice <- dplyr::collect(dplyr::union_all(input, input))
stopifnot(identical(twice$value, rep(expected$value, 2L)), root$callbacks == 0L)
actual <- dplyr::collect(query)
calls <- root$callbacks
stopifnot(identical(actual$region, expected$region))
stopifnot(identical(actual$value, c(-99L, -99L)), calls == 1L)
stopifnot(identical(serialize(root$source, NULL), before))
rm("::", envir = root)
stopifnot(identical(dplyr::collect(query), expected))
cat("ordinary input/mutate/union: expected payload 13, 29; callbacks 0\n")
cat("expansion: payload", paste(actual$value, collapse = ", "), "; callbacks", calls, "\n")
cat("source bytes unchanged; removing dispatcher restored expected expansion\n")
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(identical(actual, expected), calls == 0L)
}
