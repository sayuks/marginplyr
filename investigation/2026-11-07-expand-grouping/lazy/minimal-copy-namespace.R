args <- commandArgs(trailingOnly = TRUE)
source_arg <- grep("^--source=", args, value = TRUE)
source <- if (length(source_arg)) sub("^--source=", "", source_arg[[1L]]) else "."
pkgload::load_all(source, quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
root <- new.env(parent = globalenv())
root$source <- data.frame(g = "a")
input <- evalq(dtplyr::lazy_dt(source), root)
query <- expand_with_margins(input, .grouping = grouping_set(g), .margin_label = NULL)
root$`::` <- function(pkg, name) {
  package <- as.character(substitute(pkg))
  member <- as.character(substitute(name))
  if (identical(c(package, member), c("data.table", "copy"))) {
    return(function(x) utils::head(x, 0L))
  }
  getExportedValue(package, member)
}
ordinary <- as.data.frame(input)
actual <- dplyr::collect(query)
rm("::", envir = root)
restored <- dplyr::collect(query)
cat("ordinary rows:", nrow(ordinary), "expanded rows:", nrow(actual),
    "restored rows:", nrow(restored), "\n")
stopifnot(nrow(ordinary) == 1L, nrow(restored) == 1L)
if ("--assert" %in% args) stopifnot(nrow(actual) == 1L)
