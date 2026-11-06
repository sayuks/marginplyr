args <- commandArgs(trailingOnly = TRUE)
source_arg <- grep("^--source=", args, value = TRUE)
source <- if (length(source_arg)) sub("^--source=", "", source_arg[[1L]]) else "."
pkgload::load_all(source, quiet = TRUE)
Sys.setenv(TESTTHAT_PARALLEL = "false")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
out <- dirname(normalizePath(script))
rows <- list()
for (shape in c("root", "filter", "cache", "renamed_cache")) {
  for (id in c(FALSE, TRUE)) {
    root <- new.env(parent = globalenv())
    root$source <- data.table::data.table(g = c("a", "b"), rid = 1:2)
    input <- evalq(dtplyr::lazy_dt(source), root)
    if (shape == "filter") input <- dplyr::filter(input, TRUE)
    if (shape == "cache") input <- dplyr::compute(dplyr::filter(input, TRUE), name = "cached")
    if (shape == "renamed_cache") {
      input <- dplyr::rename(dplyr::compute(input, name = "cached"), key = g)
    }
    key <- if (shape == "renamed_cache") "key" else "g"
    before <- serialize(root$source, NULL)
    query <- expand_with_margins(input, .grouping = rollup(!!rlang::sym(key)),
                                 .margin_label = "Total", .id = if (id) "sid" else NULL)
    ordinary <- dplyr::collect(query)
    callbacks <- 0L
    root$`::` <- function(pkg, name) {
      p <- as.character(substitute(pkg)); n <- as.character(substitute(name))
      if (identical(c(p, n), c("data.table", "copy"))) {
        callbacks <<- callbacks + 1L
        return(base::identity)
      }
      getExportedValue(p, n)
    }
    warnings <- character()
    actual <- withCallingHandlers(dplyr::collect(query), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    rows[[length(rows) + 1L]] <- data.frame(shape, id, callbacks,
      detail_keys_present = all(c("a", "b") %in% actual[[key]]),
      expected_detail_keys_present = all(c("a", "b") %in% ordinary[[key]]),
      source_unchanged = identical(before, serialize(root$source, NULL)),
      warnings = length(warnings))
  }
}
result <- do.call(rbind, rows)
utils::write.csv(result, file.path(out, "copy-identity.csv"), row.names = FALSE)
print(result)
stopifnot(all(result$source_unchanged), all(result$expected_detail_keys_present),
          sum(!result$detail_keys_present) == 5L)
cat("Five of eight copy-identity controls lost detail keys; no caller table mutation observed\n")
