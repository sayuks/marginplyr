# Dated investigation harness; run from the repository root.
pkgload::load_all(quiet = TRUE)
options(lifecycle_verbosity = "quiet")
args <- commandArgs(trailingOnly = TRUE)
assert_mode <- "--assert" %in% args
capture <- function(expr) tryCatch(expr, error = identity)
kind <- function(value) {
  if (inherits(value, "error")) return(class(value)[[1L]])
  "success"
}
message_of <- function(value) {
  if (inherits(value, "error")) return(conditionMessage(value))
  "success"
}
run <- function() {
  frame <- tibble::tibble(a = 1:2, b = c("x", "y"))
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  sql_input <- dplyr::copy_to(con, frame, "mixed_plan_preflight", temporary = TRUE)
  dt_input <- dplyr::mutate(dtplyr::lazy_dt(frame), b = "z")
  .GlobalEnv$grouping_probe_queries <- character()
  on.exit(rm("grouping_probe_queries", envir = .GlobalEnv), add = TRUE)
  suppressMessages(trace("dbSendQuery", signature = c("duckdb_connection", "character"),
    tracer = quote({
      .GlobalEnv$grouping_probe_queries <- c(.GlobalEnv$grouping_probe_queries, statement)
    }), where = asNamespace("DBI"), print = FALSE))
  on.exit(suppressMessages(untrace("dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI"))), add = TRUE)
  consumers <- list(
    inspect = function(input, specification) inspect_grouping(input, .by = a, .grouping = specification),
    summarize = function(input, specification) summarize_with_margins(input, n = dplyr::n(),
      .by = a, .grouping = specification, .margin_label = NULL),
    expand = function(input, specification) expand_with_margins(input, .by = a,
      .grouping = specification, .margin_label = NULL),
    nest = function(input, specification) nest_with_margins(input, .by = a,
      .grouping = specification, .margin_label = NULL),
    nest_by = function(input, specification) nest_by_with_margins(input, .by = a,
      .grouping = specification, .margin_label = NULL)
  )
  records <- list()
  index <- 0L
  for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")) {
    for (consumer in names(consumers)) {
      for (backend in c("local", "dtplyr", "duckdb")) {
        if (backend == "duckdb" && consumer %in% c("nest", "nest_by")) next
        input <- switch(backend, local = frame, dtplyr = dt_input, duckdb = sql_input)
        for (typed in c(FALSE, TRUE)) {
          second <- if (typed) rlang::expr(where(is.character)) else rlang::expr(all_of("b"))
          specification <- rlang::eval_tidy(rlang::call2(constructor, quote(a), second))
          .GlobalEnv$grouping_probe_queries <- character()
          result <- capture(consumers[[consumer]](input, specification))
          index <- index + 1L
          records[[index]] <- data.frame(constructor = constructor, consumer = consumer,
            backend = backend, typed = typed, condition_class = kind(result),
            overlap = grepl("Columns cannot appear in both", message_of(result), fixed = TRUE),
            metadata_refusal = grepl("Can't safely determine column metadata", message_of(result), fixed = TRUE),
            queries = length(.GlobalEnv$grouping_probe_queries), stringsAsFactors = FALSE)
          if (constructor == "grouping_sets" && consumer == "inspect") {
            cat("CASE", backend, if (typed) "typed" else "names-only", "\n")
            cat(message_of(result), "\n")
            if (length(.GlobalEnv$grouping_probe_queries)) print(.GlobalEnv$grouping_probe_queries)
          }
        }
      }
    }
  }
  records <- do.call(rbind, records)
  print(with(records, table(backend, typed, overlap, queries)))
  cat("total cases", nrow(records), "\n")
  stopifnot(nrow(records) == 130L)
  stopifnot(all(records$overlap[!records$typed]),
    all(records$queries[!records$typed] == 0L))
  stopifnot(all(records$overlap[records$typed & records$backend %in% c("local", "duckdb")]),
    all(records$metadata_refusal[records$typed & records$backend == "dtplyr"]),
    all(records$queries[records$typed & records$backend == "duckdb"] == 1L))
  utils::write.csv(records, "investigation/2026-11-02-summary-grouping-share-bug-hunt/grouping/mixed-plan-preflight.csv", row.names = FALSE)
  if (assert_mode) stopifnot(all(records$overlap), all(records$queries == 0L))
}
run()
print(sessionInfo())
