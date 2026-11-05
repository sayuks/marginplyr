pkgload::load_all(".", quiet = TRUE)
data <- data.frame(
  part = rep(c("k", "l"), each = 4),
  a = rep(c("a", "a", "b", "b"), 2),
  b = rep(c("c", "d"), 4)
)
connections <- list(
  SQLite = function() DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
  DuckDB = function() DBI::dbConnect(
    duckdb::duckdb(dbdir = ":memory:", shared_home = FALSE)
  )
)
groupings <- list(
  rollup = quote(rollup(a, b)),
  union = quote(grouping_sets(
    grouping_set(a, b), grouping_set(a), grouping_set()
  ))
)

visible_ratio <- function(result, row, parent) {
  occurrence <- result$grp[[row]]
  if (occurrence == 3L) return(1)
  denominator <- if (parent) occurrence + 1L else 3L
  take <- result$part[[row]] == result$part & result$grp == denominator
  if (denominator == 2L) take <- take & result$a[[row]] == result$a
  stopifnot(sum(take) == 1L)
  result$source[[row]] / result$source[take]
}

probe <- function(backend) {
  con <- connections[[backend]]()
  on.exit(DBI::dbDisconnect(con))
  facts <- dplyr::copy_to(con, data, "facts")
  source <- if (backend == "SQLite") {
    dbplyr::sql("ABS(RANDOM() % 999983) + 1")
  } else {
    dbplyr::sql("RANDOM() + 1")
  }
  for (shape in names(groupings)) {
    grouping <- groupings[[shape]]
    share <- list(total = quote(share_of_total(source)))
    if (shape == "rollup") share$parent <- quote(share_of_parent(source))
    for (sort in c("none", "last", "first")) {
      for (aggregate in c("portable", "auto")) {
        result <- dplyr::collect(summarize_with_margins(
          facts, source = !!source, !!!share,
          .id = "grp", .by = part, .grouping = !!grouping,
          .sort = sort, .aggregate = aggregate, .check_share_source = FALSE
        ))
        stopifnot(nrow(result) == 14L)
        rows <- seq_len(nrow(result))
        expected <- vapply(rows, function(row) {
          visible_ratio(result, row, parent = FALSE)
        }, numeric(1))
        stopifnot(isTRUE(all.equal(result$total, expected)))
        if (shape == "rollup") {
          expected <- vapply(rows, function(row) {
            visible_ratio(result, row, parent = TRUE)
          }, numeric(1))
          stopifnot(isTRUE(all.equal(result$parent, expected)))
        }
        cat(paste(backend, shape, sort, aggregate, "passed"), "\n", sep = "")
      }
    }
  }
}
for (backend in names(connections)) probe(backend)
