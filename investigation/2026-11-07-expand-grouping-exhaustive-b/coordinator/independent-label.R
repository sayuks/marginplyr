Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "/private/tmp/marginplyr-exhaustive-b-root/source"), quiet = TRUE)
out_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", "/private/tmp/marginplyr-expand-grouping-exhaustive-20261107-b/investigation/2026-11-07-expand-grouping-exhaustive-b/coordinator")
scope <- new.env(parent = globalenv())
scope$source <- data.table::data.table(region = c("east", "west"), value = c(17L, 43L))
input <- evalq(dtplyr::lazy_dt(source, immutable = TRUE), scope)
before <- serialize(scope$source, NULL)
query <- expand_with_margins(input, .grouping = grouping_set(region))
expected <- dplyr::collect(query)
calls <- 0L
scope$`:::` <- function(package, name) {
  package <- as.character(substitute(package))
  name <- as.character(substitute(name))
  if (identical(c(package, name), c("marginplyr", "margin_display_character"))) {
    calls <<- calls + 1L
    return(function(x) paste0("changed:", x))
  }
  getFromNamespace(name, package)
}
source_control <- dplyr::collect(input)
mutation_control <- dplyr::collect(dplyr::mutate(input, value = value + 0L))
union_control <- dplyr::collect(dplyr::union_all(input, input))
stopifnot(calls == 0L, identical(source_control, expected), identical(mutation_control, expected), identical(union_control, dplyr::union_all(expected, expected)))
actual <- dplyr::collect(query)
stopifnot(calls == 1L, identical(actual$region, paste0("changed:", expected$region)), identical(actual$value, expected$value), identical(serialize(scope$source, NULL), before))
rm(list = ":::", envir = scope)
restored <- dplyr::collect(query)
stopifnot(identical(restored, expected))
cat("Source, mutation and union controls: zero callbacks; exact matches\n")
cat("Expected region:", paste(expected$region, collapse = ", "), "\n")
cat("Actual region:", paste(actual$region, collapse = ", "), "; generated-label callbacks:", calls, "\n")
cat("Payload and source bytes unchanged; restored binding recovers exact expected frame\n")
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) stopifnot(identical(actual, expected))
