# Run from the investigated source, or set MARGINPLYR_HUNT_SOURCE.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(
  Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = "."),
  quiet = TRUE
)

probe <- function(verb, cached = FALSE) {
  reads <- integer()
  keep_row <- function(row) {
    reads <<- c(reads, length(row))
    row == 1L + (length(reads) + 1L) %% 2L
  }

  input <- tibble::tibble(g = c("a", "b"), row = 1:2)
  lazy <- dplyr::filter(dtplyr::lazy_dt(input), keep_row(row))
  if (cached) {
    lazy <- dplyr::compute(lazy, name = "cached_source")
  }
  expected <- input[1L, ]
  stopifnot(identical(dplyr::collect(lazy), expected), identical(reads, 2L))
  reads <- integer()

  result <- verb(lazy, .grouping = rollup(g), .keep = TRUE,
                 .id = "set", .sort = "last")
  if (inherits(result, "dtplyr_step")) {
    stopifnot(length(reads) == 0L)
    print(dplyr::show_query(result))
    stopifnot(length(reads) == 0L)
    result <- dplyr::collect(result)
  }
  expected_result <- verb(expected, .grouping = rollup(g), .keep = TRUE,
                          .id = "set", .sort = "last")
  cell_rows <- function(data) lapply(data$data, function(cell) cell$row)
  stopifnot(
    identical(result$set, c(1L, 2L)),
    identical(result$g, c("a", "Total")),
    identical(cell_rows(expected_result), list(1L, 1L))
  )
  cat("Reads:", paste(reads, collapse = ","), "\n")
  cat("Source row in detail and total:",
      paste(unlist(cell_rows(result)), collapse = ","), "\n")
  matches <- identical(cell_rows(result), cell_rows(expected_result))
  if (cached) stopifnot(matches, identical(reads, 2L))
  matches && identical(reads, 2L)
}

matches <- c(
  ordinary = probe(nest_with_margins),
  rowwise = probe(nest_by_with_margins),
  caller_cached = probe(nest_with_margins, cached = TRUE),
  caller_cached_rowwise = probe(nest_by_with_margins, cached = TRUE)
)
print(matches)
print(sessionInfo())
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(all(matches))
}
