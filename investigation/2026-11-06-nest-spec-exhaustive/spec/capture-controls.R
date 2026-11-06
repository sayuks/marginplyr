pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
output_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempfile("capture-controls-"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
data <- tibble::tibble(a = c("x", "y"), b = 1:2)
records <- list()
spellings <- alist(rollup(a), (rollup)(a), ((rollup))(a), base::`(`(rollup(a)), (base::`(`)(rollup(a)), marginplyr::rollup(a))
for (i in seq_along(spellings)) {
  for (parent in c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")) {
    reads <- 0L
    rollup <- function(...) {
      reads <<- reads + 1L
      if (reads > 1L) stop("Constructor binding was re-read")
      "a"
    }
    `::` <- function(pkg, fun) {
      name <- as.character(substitute(fun))
      if (identical(as.character(substitute(pkg)), "marginplyr") && name == "rollup") return(rollup)
      getExportedValue(as.character(substitute(pkg)), name)
    }
    expr <- rlang::call2(getExportedValue("marginplyr", parent), spellings[[i]])
    specification <- eval(expr)
    result <- inspect_grouping(data, .grouping = specification, .format = "list")
    expected <- if (parent %in% c("rollup", "cube")) list("a", character()) else list("a")
    matches <- identical(result$included, expected) && reads == 1L
    records[[length(records) + 1L]] <- data.frame(spelling = i, parent, reads, matches)
  }
}
rm(`::`, rollup)
for (depth in 0:6) {
  run <- function(...) {
    expression <- quote(..1)
    for (i in seq_len(depth)) expression <- rlang::call2("(", expression)
    specification <- eval(rlang::call2("grouping_sets", expression))
    inspect_grouping(data, .grouping = specification, .format = "list")
  }
  result <- run(grouping_set(a))
  records[[length(records) + 1L]] <- data.frame(spelling = paste0("forwarding-depth-", depth), parent = "grouping_sets", reads = NA_integer_, matches = identical(result$included, list("a")))
}
results <- do.call(rbind, records)
write.csv(results, file.path(output_dir, "capture-controls.csv"), row.names = FALSE)
cat("Capture comparisons:", nrow(results), "Matches:", sum(results$matches), "\n")
if (!all(results$matches)) print(results[!results$matches, ], row.names = FALSE)
stopifnot(all(results$matches))
