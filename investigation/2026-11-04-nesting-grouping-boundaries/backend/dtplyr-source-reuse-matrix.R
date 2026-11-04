# Cross filtering, plan size, explicit source caching and the nesting callers.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(
  Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = "."),
  quiet = TRUE
)

source <- tibble::tibble(g = c("a", "b"), h = c("x", "y"), row = 1:2)
matrix_cases <- expand.grid(
  verb = c("nest", "rowwise", "expand"),
  plan = c("single", "rollup", "cube"),
  predicate = c("alternate", "first_only", "stable"),
  graph = c("uncached", "cached", "cached_renamed"),
  keep = c(FALSE, TRUE),
  sort = c("none", "last"),
  stringsAsFactors = FALSE
)
matrix_cases <- matrix_cases[matrix_cases$verb != "expand" | !matrix_cases$keep, ]
verb_names <- list(
  nest = nest_with_margins, rowwise = nest_by_with_margins,
  expand = expand_with_margins
)
specs <- list(single = grouping_set(g), rollup = rollup(g), cube = cube(g, h))

run_case <- function(verb, plan, predicate, graph, keep, sort) {
  reads <- integer()
  select_rows <- function(row) {
    reads <<- c(reads, length(row))
    switch(
      predicate,
      alternate = row == 1L + (length(reads) + 1L) %% 2L,
      first_only = row == 1L & length(reads) == 1L,
      stable = row == 1L
    )
  }
  base <- dtplyr::lazy_dt(source)
  lazy <- dplyr::filter(base, select_rows(row))
  if (graph != "uncached") {
    lazy <- dplyr::compute(lazy, name = "cached_source")
  }
  if (graph == "cached_renamed") {
    lazy <- dplyr::rename(lazy, renamed_row = row)
    native <- dplyr::rename(source[1L, ], renamed_row = row)
  } else {
    native <- source[1L, ]
  }
  stopifnot(identical(dplyr::collect(lazy), native), identical(reads, 2L))
  reads <- integer()

  arguments <- list(.grouping = specs[[plan]], .id = "set", .sort = sort)
  if (verb != "expand") arguments$.keep <- keep
  apply_verb <- function(data) {
    do.call(verb_names[[verb]], c(list(data), arguments))
  }
  expected <- apply_verb(native)
  actual <- apply_verb(lazy)
  if (verb != "rowwise") {
    stopifnot(length(reads) == 0L)
    invisible(dplyr::show_query(actual))
    stopifnot(length(reads) == 0L)
    actual <- dplyr::collect(actual)
  }
  stopifnot(identical(dplyr::collect(base), source), all(reads == 2L))
  if (verb == "rowwise") stopifnot(inherits(actual, "rowwise_df"))
  row_name <- if (graph == "cached_renamed") "renamed_row" else "row"
  rows_by_set <- function(result) {
    if (sort == "none") result <- result[order(result$set), ]
    if (verb == "expand") return(list(set = result$set, rows = result[[row_name]]))
    list(set = result$set, rows = lapply(result$data, function(cell) cell[[row_name]]))
  }
  c(reads = length(reads), agrees = identical(rows_by_set(expected), rows_by_set(actual)))
}

outcome <- mapply(run_case, matrix_cases$verb, matrix_cases$plan,
                  matrix_cases$predicate, matrix_cases$graph,
                  matrix_cases$keep, matrix_cases$sort)
matrix_cases$reads <- as.integer(outcome["reads", ])
matrix_cases$agrees <- as.logical(outcome["agrees", ])
stopifnot(nrow(matrix_cases) == 270L)
cat("Calls:", nrow(matrix_cases), "\n")
print(with(matrix_cases, table(graph, plan, predicate, agrees)))
print(with(matrix_cases, table(graph, plan, reads)))
print(with(matrix_cases, table(verb, agrees)))
path <- commandArgs(trailingOnly = TRUE)
if (length(path) > 0L) utils::write.csv(matrix_cases, path[[1L]], row.names = FALSE)
