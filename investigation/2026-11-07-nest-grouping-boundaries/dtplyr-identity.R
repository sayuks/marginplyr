args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, args[[2L]] %in% c("observed", "contract"))
pkgload::load_all(args[[1L]], quiet = TRUE)

root <- new.env(parent = globalenv())
root$source <- data.table::data.table(
  region = c("east", "east", "west"),
  units = c(17L, 34L, 51L)
)
root$lookups <- 0L
input <- evalq(dtplyr::lazy_dt(source, immutable = TRUE), root)
source_bytes <- serialize(root$source, NULL)
make_nest <- function(verb, plan) {
  verb(input, .grouping = plan, .margin_label = NULL, .id = "set", .sort = "last")
}
collect_result <- function(query) {
  if (inherits(query, "dtplyr_step")) dplyr::collect(query) else query
}
total_payloads <- function(result) {
  rows <- which(result$set == 2L)
  stopifnot(all(is.na(result$region[rows])))
  payloads <- lapply(result$data[rows], function(cell) cell$units)
  payloads[order(vapply(payloads, min, integer(1L)))]
}

ordinary <- make_nest(nest_with_margins, rollup(region))
stopifnot(inherits(ordinary, "dtplyr_step"))
expected <- collect_result(ordinary)
stopifnot(identical(total_payloads(expected), list(c(17L, 34L, 51L))))
deferred <- make_nest(nest_with_margins, rollup(region))

root[["::"]] <- function(package, name) {
  namespace <- as.character(substitute(package))
  member <- as.character(substitute(name))
  if (identical(c(namespace, member), c("vctrs", "vec_rep"))) {
    root$lookups <- root$lookups + 1L
    return(function(value, size) paste0("row_", seq_len(size)))
  }
  getExportedValue(namespace, member)
}
ordinary_result <- collect_result(deferred)
rowwise_result <- make_nest(nest_by_with_margins, rollup(region))
stopifnot(inherits(rowwise_result, "rowwise_df"))
observed <- list(
  ordinary = total_payloads(ordinary_result),
  rowwise = total_payloads(rowwise_result)
)
lookups <- root$lookups
print(observed)
cat("Generated repetition lookups:", lookups, "\n")
detail <- collect_result(make_nest(nest_with_margins, grouping_set(region)))
stopifnot(
  identical(dplyr::collect(input)$units, c(17L, 34L, 51L)),
  identical(detail$region, c("east", "west")),
  identical(root$lookups, lookups),
  identical(serialize(root$source, NULL), source_bytes)
)
rm("::", envir = root)
stopifnot(identical(collect_result(deferred), expected))
if (args[[2L]] == "observed") {
  stopifnot(all(vapply(observed, identical, logical(1L), as.list(c(17L, 34L, 51L)))),
            identical(lookups, 2L))
} else {
  stopifnot(all(vapply(observed, identical, logical(1L), list(c(17L, 34L, 51L)))),
            identical(lookups, 0L))
}
