pkgload::load_all(".", quiet = TRUE)
assert_desired <- "--assert" %in% commandArgs(trailingOnly = TRUE)
output_dir <- Sys.getenv("MARGINPLYR_HUNT_OUTPUT", tempfile("non-singleton-suffix-"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
input <- tibble::tibble(a = c("x", "y"), b = 1:2)
predicate_reads <- 0L
predicate <- function(x) {
  predicate_reads <<- predicate_reads + 1L
  is.integer(x)
}
typed <- grouping_set(where(predicate))
prefix <- grouping_sets(a, a)
suffix <- grouping_sets(!!typed, grouping_set())
spec <- grouping_spec(!!prefix, !!suffix)
singleton <- grouping_spec(!!prefix, !!typed)
literal <- grouping_spec(!!prefix, grouping_sets(grouping_set(b), grouping_set()))
is_duplicate <- function(x) {
  inherits(x, "marginplyr_error") &&
    grepl("Duplicate grouping sets", conditionMessage(x), fixed = TRUE)
}
upstream_reads <- 0L
derive <- function(x) {
  upstream_reads <<- upstream_reads + 1L
  paste0(x, "!")
}
unsafe <- dplyr::mutate(dtplyr::lazy_dt(input), added = derive(a))
records <- list()
for (verb in list(inspect_grouping, nest_with_margins, nest_by_with_margins)) {
  name <- deparse(substitute(verb))
  for (data_kind in c("local", "unsafe_dtplyr")) {
    data <- if (data_kind == "local") input else unsafe
    for (shape in c("singleton", "literal", "two_set_suffix")) {
      chosen <- switch(shape, singleton = singleton, literal = literal,
                       two_set_suffix = spec)
      predicate_reads <- 0L
      observed <- tryCatch(verb(data, .grouping = chosen), error = identity)
      failure <- !is_duplicate(observed) || predicate_reads != 0L
      records[[length(records) + 1L]] <- data.frame(
        verb = if (identical(verb, inspect_grouping)) "inspect_grouping" else if (identical(verb, nest_with_margins)) "nest_with_margins" else "nest_by_with_margins",
        data_kind, shape, predicate_reads,
        class = class(observed)[[1L]],
        observation = conditionMessage(observed), contract_failure = failure
      )
      if (assert_desired) stopifnot(!failure)
      if (shape != "two_set_suffix") stopifnot(!failure)
    }
  }
}
stopifnot(upstream_reads == 0L)

local({
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  table <- dplyr::copy_to(con, input, "non_singleton_suffix")
  withr::local_options(marginplyr.audit_sql = TRUE)
  statements <- character()
  signature <- c("duckdb_connection", "character")
  suppressMessages(do.call(trace, list(
    what = "dbSendQuery", signature = signature, where = asNamespace("DBI"),
    print = FALSE, tracer = function() {
      statements <<- c(statements, get("statement", parent.frame()))
    }
  )))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = signature, where = asNamespace("DBI")
  )), add = TRUE, after = FALSE)
  for (shape in c("singleton", "literal", "two_set_suffix")) {
    chosen <- switch(shape, singleton = singleton, literal = literal,
                     two_set_suffix = spec)
    predicate_reads <<- 0L
    statements <- character()
    observed <- tryCatch(inspect_grouping(table, .grouping = chosen), error = identity)
    queries <- last_sent_queries(include_result = FALSE)
    failure <- !is_duplicate(observed) || predicate_reads != 0L || nrow(queries) != 0L || length(statements) != 0L
    records[[length(records) + 1L]] <<- data.frame(
      verb = "inspect_grouping", data_kind = "duckdb", shape,
      predicate_reads, class = class(observed)[[1L]],
      observation = paste(conditionMessage(observed),
                          paste(queries$sql, collapse = "; "), sep = "\n"),
      contract_failure = failure
    )
    cat("DuckDB", shape, "prepared internal queries", nrow(queries),
        "DBI statements", length(statements), "\n")
    if (length(statements)) cat(paste(statements, collapse = "\n"), "\n")
    if (assert_desired) stopifnot(!failure)
    if (shape != "two_set_suffix") stopifnot(!failure)
  }
})
output <- do.call(rbind, records)
write.csv(output, file.path(output_dir, "non-singleton-suffix-boundaries.csv"), row.names = FALSE)
print(output[c("verb", "data_kind", "shape", "predicate_reads", "class", "contract_failure")], row.names = FALSE)
stopifnot(nrow(output) == 21L,
          sum(output$contract_failure) == 7L,
          all(output$contract_failure == (output$shape == "two_set_suffix")))
cat(R.version.string, R.version$platform, "\n")
for (package in c("dplyr", "dtplyr", "duckdb", "tidyselect", "rlang")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
