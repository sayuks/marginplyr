# Dated finite probe, run from the tested checkout's root.
pkgload::load_all(".", quiet = TRUE)
source_sha <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
cat("Tested SHA:", source_sha, "\n")
cat(R.version.string, "\n")
input <- tibble::tibble(g = c("b", "a"), v = 1:2)
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
remote <- dplyr::copy_to(con, input, "reentry_input", temporary = TRUE)
inner_calls <- list(
  inspect_local = function() inspect_grouping(input, .grouping = rollup(g)),
  inspect_sql = function() inspect_grouping(
    remote, .grouping = grouping_set(where(is.character))
  ),
  nest = function() nest_with_margins(input, .grouping = rollup(g)),
  nest_by = function() nest_by_with_margins(input, .grouping = rollup(g)),
  failure = function() tryCatch(
    nest_with_margins(input, .key = ""),
    marginplyr_error = function(e) invisible(e)
  )
)
observations <- list()
for (outer_audit in c(FALSE, TRUE)) {
  for (inner_audit in c(FALSE, TRUE)) {
    for (inner_name in names(inner_calls)) {
      for (outer_name in c("inspect", "nest", "nest_by")) {
        options(marginplyr.audit_sql = outer_audit)
        resumed <- 0L
        callback <- function() {
          before <- tryCatch(last_sent_queries(), marginplyr_error = identity)
          options(marginplyr.audit_sql = inner_audit)
          inner_calls[[inner_name]]()
          after <- tryCatch(last_sent_queries(), marginplyr_error = identity)
          stopifnot(identical(before, after))
          resumed <<- resumed + 1L
          rollup(g)
        }
        result <- switch(
          outer_name,
          inspect = inspect_grouping(remote, .grouping = callback()),
          nest = nest_with_margins(input, .grouping = callback()),
          nest_by = nest_by_with_margins(input, .grouping = callback())
        )
        record <- tryCatch(last_sent_queries(), marginplyr_error = identity)
        stopifnot(resumed == 1L)
        if (outer_audit) {
          stopifnot(is.data.frame(record), nrow(record) == 0L)
        } else {
          stopifnot(inherits(record, "marginplyr_error"))
        }
        observations[[length(observations) + 1L]] <- data.frame(
          outer = outer_name, inner = inner_name,
          outer_audit, inner_audit, verdict = "match"
        )
      }
    }
  }
}
# A typed callback runs after SQL A has recorded its own zero-row statement.
for (inner_name in names(inner_calls)) {
  options(marginplyr.audit_sql = TRUE)
  seen <- 0L
  predicate <- function(x) {
    before <- last_sent_queries()
    stopifnot(nrow(before) == 1L, before$purpose == "selection_proxy")
    inner_calls[[inner_name]]()
    stopifnot(identical(before, last_sent_queries()))
    seen <<- seen + 1L
    is.character(x)
  }
  result <- inspect_grouping(remote, .grouping = grouping_set(where(predicate)))
  stopifnot(seen == 2L, result$included == "(g)")
  stopifnot(nrow(last_sent_queries()) == 1L)
}
DBI::dbDisconnect(con, shutdown = TRUE)
cat("Entry-argument ownership matches:", length(observations), "\n")
cat("Post-SQL predicate ownership matches: 5 cases, 10 callbacks\n")
cat("No defect confirmed in this finite matrix.\n")
