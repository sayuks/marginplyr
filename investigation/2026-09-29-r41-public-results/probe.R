args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
case_lib <- normalizePath(args[[1L]], mustWork = TRUE)
out_dir <- args[[2L]]
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(length(.libPaths()) == 2L, identical(normalizePath(.libPaths()[[1L]]), case_lib))
library(marginplyr)
stopifnot(identical(normalizePath(find.package("marginplyr")), file.path(case_lib, "marginplyr")))

x <- data.frame(
  region = c("A", "A", "B"), store = c("x", "y", "x"),
  value = c(2L, 3L, 5L)
)
run <- function(f) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(f(), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(e) e
  )
  if (inherits(value, "error")) {
    list(ok = FALSE, class = class(value), message = conditionMessage(value), warnings = warnings)
  } else {
    list(ok = TRUE, value = value, warnings = warnings)
  }
}

results <- list()
results$plan <- run(function() inspect_grouping(
  x, .grouping = rollup(region, store), .format = "list"
))
results$summary <- run(function() summarize_with_margins(
  x, total = sum(value), .grouping = rollup(region, store),
  .id = "set_id", .sort = "last"
))
results$shares <- run(function() summarize_with_margins(
  x, total = sum(value), parent = share_of_parent(total),
  grand = share_of_total(total), .grouping = rollup(region, store),
  .id = "set_id", .sort = "last"
))
results$no_grand_total <- run(function() summarize_with_margins(
  x, total = sum(value), grand = share_of_total(total),
  .grouping = grouping_sets(region)
))
results$sqlite_collect <- run(function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(con, x, "r41_probe", temporary = TRUE)
  query <- summarize_with_margins(
    source, total = sum(value), .grouping = rollup(region, store),
    .id = "set_id", .sort = "last"
  )
  dplyr::collect(query)
})
results$sqlite_compute <- run(function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(con, x, "r41_probe", temporary = TRUE)
  query <- summarize_with_margins(
    source, total = sum(value), .grouping = rollup(region, store),
    .id = "set_id", .sort = "last"
  )
  dplyr::collect(dplyr::compute(query))
})

loaded <- loadedNamespaces()
paths <- vapply(loaded, function(p) normalizePath(find.package(p)), character(1))
external <- !startsWith(paths, paste0(normalizePath(.Library), "/"))
stopifnot(all(startsWith(paths[external], paste0(case_lib, "/"))))
loaded <- loaded[external]
manifest <- data.frame(
  package = loaded,
  version = vapply(loaded, function(p) as.character(packageVersion(p)), character(1)),
  path = unname(paths[external])
)
manifest <- manifest[order(manifest$package), , drop = FALSE]
write.csv(manifest, file.path(out_dir, "loaded-manifest.csv"), row.names = FALSE)
writeLines(c(
  R.version.string,
  paste("R.home:", R.home()),
  paste(".libPaths:", paste(.libPaths(), collapse = " | ")),
  paste("marginplyr:", find.package("marginplyr"))
), file.path(out_dir, "environment.txt"))
saveRDS(results, file.path(out_dir, "results.rds"))
for (name in names(results)) {
  result <- results[[name]]
  cat(name, if (result$ok) "OK" else "ERROR", "\n")
  if (result$ok && is.data.frame(result$value)) {
    cat("  rows:", nrow(result$value), "columns:", paste(names(result$value), collapse = ","), "\n")
    print(result$value)
    cat("  types:", paste(vapply(result$value, typeof, character(1)), collapse = ","), "\n")
  } else if (!result$ok) {
    cat("  class:", paste(result$class, collapse = ","), "\n")
    cat("  message:", result$message, "\n")
  } else print(result$value)
  if (length(result$warnings)) cat("  warnings:", paste(result$warnings, collapse = " | "), "\n")
}
