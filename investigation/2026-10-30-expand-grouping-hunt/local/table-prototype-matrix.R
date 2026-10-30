# Contrast one-dimensional table keys with their unclassed array controls.
suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
output <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", unset = tempdir())
dir.create(output, recursive = TRUE, showWarnings = FALSE)
capture <- function(expr) tryCatch(force(expr), error = identity)
results <- list()
index <- 0L
for (storage in c("integer", "double")) {
  for (size in c(0L, 1L, 3L)) {
    values <- rep(c(2, NA, 1), length.out = size)
    if (storage == "integer") values <- as.integer(values)
    for (family in c("table", "array")) {
      key <- array(values, dim = size)
      if (family == "table") key <- as.table(key)
      for (label_name in c("NULL", "NA_character_")) {
        label <- if (label_name == "NULL") NULL else NA_character_
        for (order in c("rollup", "reversed", "duplicate")) {
          grouping <- switch(
            order,
            rollup = rollup(key),
            reversed = grouping_sets(grouping_set(), grouping_set(key)),
            duplicate = grouping_sets(
              grouping_set(key), grouping_set(key), grouping_set()
            )
          )
          included <- switch(order, rollup = c(TRUE, FALSE),
                             reversed = c(FALSE, TRUE),
                             duplicate = c(TRUE, TRUE, FALSE))
          input <- tibble::tibble(key = key, rid = seq_len(size))
          before <- serialize(input, NULL)
          branches <- lapply(seq_along(included), function(occurrence) {
            branch <- input
            if (!included[[occurrence]]) {
              branch <- dplyr::mutate(
                branch, key = vctrs::vec_init(key, size)
              )
            }
            dplyr::mutate(branch, sid = occurrence)
          })
          control <- Reduce(dplyr::union_all, branches)
          control <- dplyr::relocate(control, sid, .after = key)
          for (sort in c("none", "first", "last")) {
            actual <- capture(expand_with_margins(
              input, .grouping = grouping, .margin_label = label,
              .duplicates = "keep", .id = "sid", .sort = sort
            ))
            verdict <- if (inherits(actual, "error")) {
              class(actual)[[1L]]
            } else {
              aligned <- actual[order(actual$sid, actual$rid), , drop = FALSE]
              expected <- control[order(control$sid, control$rid), , drop = FALSE]
              if (identical(aligned, expected)) "match" else "difference"
            }
            index <- index + 1L
            results[[index]] <- data.frame(
              storage = storage, size = size, family = family,
              label = label_name, order = order, sort = sort,
              verdict = verdict,
              ordinary_rows = nrow(control),
              source_unchanged = identical(serialize(input, NULL), before),
              detail = if (inherits(actual, "error")) {
                conditionMessage(actual)
              } else {
                ""
              }
            )
          }
        }
      }
    }
  }
}
results <- do.call(rbind, results)
print(with(results, table(family, verdict)))
print(table(results$source_unchanged))
write.csv(results, file.path(output, "table-prototype-matrix.csv"),
          row.names = FALSE)
stopifnot(nrow(results) == 216L, all(results$source_unchanged))
