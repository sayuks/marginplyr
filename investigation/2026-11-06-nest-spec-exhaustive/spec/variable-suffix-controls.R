pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
output_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempfile("variable-suffix-"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
data <- tibble::tibble(a = c("x", "y"), b = 1:2, c = 3:4)
upstream_reads <- 0L
derive <- function(x) {
  upstream_reads <<- upstream_reads + 1L
  paste0(x, "!")
}
unsafe <- dplyr::mutate(dtplyr::lazy_dt(data), added = derive(a))
records <- list()
for (consumer in c("inspect_grouping", "nest_with_margins", "nest_by_with_margins")) {
  verb <- getExportedValue("marginplyr", consumer)
  for (input in c("local", "unsafe_dtplyr")) {
    for (shape in c("fixed_single", "fixed_two", "variable_rollup", "variable_cube")) {
      predicate_reads <- 0L
      predicate <- function(x) {
        predicate_reads <<- predicate_reads + 1L
        is.integer(x)
      }
      suffix <- switch(shape,
        fixed_single = grouping_set(where(predicate)),
        fixed_two = grouping_sets(grouping_set(where(predicate)), grouping_set()),
        variable_rollup = rollup(where(predicate)),
        variable_cube = cube(where(predicate))
      )
      specification <- grouping_spec(grouping_sets(a, a), !!suffix)
      actual <- tryCatch(verb(if (input == "local") data else unsafe, .grouping = specification), error = identity)
      duplicate <- inherits(actual, "marginplyr_error") && grepl("Duplicate grouping sets", conditionMessage(actual), fixed = TRUE)
      matches <- if (startsWith(shape, "fixed")) duplicate && predicate_reads == 0L else {
        if (input == "local") duplicate && predicate_reads == 3L else {
          inherits(actual, "marginplyr_error") && predicate_reads == 0L && grepl("safely determine column metadata", conditionMessage(actual), fixed = TRUE)
        }
      }
      records[[length(records) + 1L]] <- data.frame(consumer, input, shape, predicate_reads, duplicate, matches, observation = conditionMessage(actual))
    }
  }
}
results <- do.call(rbind, records)
write.csv(results, file.path(output_dir, "variable-suffix-controls.csv"), row.names = FALSE)
cat("Suffix observations:", nrow(results), "Matched recorded boundary:", sum(results$matches), "Upstream callbacks:", upstream_reads, "\n")
print(results[c("consumer", "input", "shape", "predicate_reads", "duplicate", "matches")], row.names = FALSE)
stopifnot(upstream_reads == 0L, all(results$matches))
