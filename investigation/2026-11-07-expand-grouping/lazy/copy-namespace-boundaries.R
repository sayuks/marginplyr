args <- commandArgs(trailingOnly = TRUE)
source_arg <- grep("^--source=", args, value = TRUE)
source <- if (length(source_arg)) sub("^--source=", "", source_arg[[1L]]) else "."
pkgload::load_all(source, quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
out <- dirname(normalizePath(script))
rows <- list()
for (verb in c("expand", "summarize", "nest", "nest_by")) {
  root <- new.env(parent = globalenv())
  root$source <- data.table::data.table(g = "a", rid = 1L)
  input <- evalq(dtplyr::lazy_dt(source), root)
  run <- function() switch(verb,
    expand = expand_with_margins(input, .grouping = grouping_set(g), .margin_label = NULL),
    summarize = summarize_with_margins(input, n = dplyr::n(), .grouping = grouping_set(g), .margin_label = NULL),
    nest = nest_with_margins(input, .grouping = grouping_set(g), .margin_label = NULL),
    nest_by = nest_by_with_margins(input, .grouping = grouping_set(g), .margin_label = NULL)
  )
  ordinary <- run()
  if (verb != "nest_by") ordinary <- dplyr::collect(ordinary)
  root$`::` <- function(pkg, name) {
    p <- as.character(substitute(pkg)); n <- as.character(substitute(name))
    if (identical(c(p, n), c("data.table", "copy"))) {
      return(function(x) utils::head(x, 0L))
    }
    getExportedValue(p, n)
  }
  actual <- run()
  if (verb != "nest_by") actual <- dplyr::collect(actual)
  rows[[length(rows) + 1L]] <- data.frame(verb, ordinary_rows = nrow(ordinary), actual_rows = nrow(actual))
}
result <- do.call(rbind, rows)
utils::write.csv(result, file.path(out, "copy-namespace-boundaries.csv"), row.names = FALSE)
print(result)
stopifnot(all(result$ordinary_rows == 1L), all(result$actual_rows == 0L))

# A source expression the caller actually wrote still has ordinary dispatch.
root <- new.env(parent = globalenv())
root$source <- data.table::data.table(g = "a", rid = 1L)
input <- evalq(dtplyr::lazy_dt(source), root)
input <- evalq(dplyr::filter(input, data.table::copy(rid) == rid), list2env(list(input = input), parent = root))
callbacks <- 0L
root$`::` <- function(pkg, name) {
  p <- as.character(substitute(pkg)); n <- as.character(substitute(name))
  if (identical(c(p, n), c("data.table", "copy"))) callbacks <<- callbacks + 1L
  getExportedValue(p, n)
}
ordinary <- dplyr::collect(input)
ordinary_callbacks <- callbacks
callbacks <- 0L
query <- expand_with_margins(input, .grouping = grouping_set(g), .margin_label = NULL)
construction_callbacks <- callbacks
actual <- dplyr::collect(query)
cat("Authored source calls:", ordinary_callbacks,
    "construction calls:", construction_callbacks,
    "expanded calls:", callbacks, "\n")
stopifnot(ordinary_callbacks == 1L, construction_callbacks == 0L,
          callbacks == 2L, identical(ordinary, actual))
