# Run from the checkout root: Rscript investigation/<note>/duplicate-prefix.R
pkgload::load_all(quiet = TRUE)

local({
  data <- tibble::tibble(a = "a", b = 1L, value = 2)
  reads <- 0L
  predicate <- function(x) {
    reads <<- reads + 1L
    is.double(x)
  }
  family <- grouping_sets(a, a, where(predicate))
  prefix <- grouping_spec(!!family, b)
  suffix <- grouping_spec(b, !!family)
  known <- grouping_spec(grouping_sets(a, a), where(predicate))

  duplicate_error <- function(failure) {
    inherits(failure, "marginplyr_error") &&
      grepl("Duplicate grouping sets", conditionMessage(failure), fixed = TRUE) &&
      grepl("1, 2", conditionMessage(failure), fixed = TRUE)
  }
  for (verb in list(inspect_grouping, nest_with_margins, nest_by_with_margins)) {
    for (spec in list(family, suffix, known)) {
      reads <- 0L
      stopifnot(duplicate_error(tryCatch(verb(data, .grouping = spec),
                                       error = identity)), reads == 0L)
    }
    reads <- 0L
    stopifnot(duplicate_error(tryCatch(verb(data, .grouping = prefix),
                                     error = identity)), reads == 3L)
  }
  cat("Inspection and both Nest verbs evaluated the predicate three times",
      "only when the partial family was a nonfinal factor.\n")

  upstream_reads <- 0L
  derive <- function(x) {
    upstream_reads <<- upstream_reads + 1L
    paste0(x, "!")
  }
  step <- dtplyr::lazy_dt(data) |>
    dplyr::mutate(added = derive(a))
  stopifnot(duplicate_error(tryCatch(inspect_grouping(step, .grouping = suffix),
                                   error = identity)))
  failure <- tryCatch(inspect_grouping(step, .grouping = prefix), error = identity)
  stopifnot(inherits(failure, "marginplyr_error"),
            grepl("metadata", conditionMessage(failure), fixed = TRUE),
            !duplicate_error(failure), upstream_reads == 0L)
  cat("The nonfinal partial family reached dtplyr's metadata refusal.\n")

  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  table <- dplyr::copy_to(con, data, "observed_prefix", temporary = FALSE)
  withr::local_options(marginplyr.audit_sql = TRUE)
  for (spec in list(family, suffix, known)) {
    reads <- 0L
    failure <- tryCatch(inspect_grouping(table, .grouping = spec), error = identity)
    stopifnot(duplicate_error(failure), reads == 0L,
              nrow(last_sent_queries(include_result = FALSE)) == 0L)
  }
  reads <- 0L
  failure <- tryCatch(inspect_grouping(table, .grouping = prefix), error = identity)
  queries <- last_sent_queries(include_result = FALSE)
  stopifnot(duplicate_error(failure), reads == 3L,
            identical(queries$purpose, "selection_proxy"),
            grepl("LIMIT 0", queries$sql, fixed = TRUE))
  cat("Only the nonfinal partial family sent one DuckDB LIMIT 0 metadata query.\n")
})

cat(R.version.string, R.version$platform, "\n")
for (package in c("dplyr", "dtplyr", "duckdb", "tidyselect", "rlang")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
