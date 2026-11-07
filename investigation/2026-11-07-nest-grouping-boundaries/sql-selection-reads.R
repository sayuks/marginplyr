suppressPackageStartupMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(dplyr)
})
err <- function(expr) tryCatch(expr, error = identity)
plan <- function(x, ...) inspect_grouping(x, ..., .format = "list")
contains <- function(x, frag) inherits(x, "error") && grepl(frag, conditionMessage(x), fixed = TRUE)
f <- function() {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (backend in c("sqlite","duckdb")) {
    con <- DBI::dbConnect(switch(backend, sqlite = RSQLite::SQLite(), duckdb = duckdb::duckdb(shared_home = FALSE)), ":memory:")
    dat <- tibble(id = 1:2, number_text = c("1", "not a number"), `true` = c(11L,12L), `comma,name` = c("x","y"), `non ascii é` = c(2L,4L), `日本語` = c(0L,1L))
    src <- copy_to(con, dat, "t1", temporary = TRUE)
    case <- if (backend == "duckdb") {
      mutate(src, computed = dbplyr::sql("CAST(number_text AS INTEGER)"))
    } else {
      mutate(src, computed = dbplyr::sql("CASE WHEN id = 2 THEN abs(-9223372036854775808) ELSE id END"))
    }
    typed <- err(plan(case, .grouping = grouping_set(where(is.integer))))
    audit <- last_sent_queries(include_result = FALSE, details = TRUE)
    if (backend == "duckdb") {
      print(audit[c("purpose", "status", "sql")])
      stopifnot(identical(typed$included[[1L]], c("id", "true", "non ascii é", "日本語", "computed")),
                identical(audit$purpose, "selection_proxy"), identical(audit$status, "returned"),
                grepl("LIMIT 0", audit$sql, fixed = TRUE))
    } else {
      stopifnot(contains(typed, "predicate"), nrow(audit) == 0L)
    }
    named <- plan(case, .by = all_of("日本語"), .grouping = rollup(all_of(c("true","comma,name", "computed"))))
    stopifnot(identical(named$fixed[[1L]], "日本語"), identical(named$included[[1L]], c("true","comma,name", "computed")),
              nrow(last_sent_queries(include_result = FALSE, details = TRUE)) == 0L)
    invalid <- err(inspect_grouping(case, .format = "invalid", .by = where(is.integer)))
    stopifnot(inherits(invalid, "marginplyr_error"),
              nrow(last_sent_queries(include_result = FALSE, details = TRUE)) == 0L)
    payload <- err(collect(case))
    stopifnot(contains(payload, if (backend == "duckdb") "not a number" else "integer overflow"))
    DBI::dbDisconnect(con)

    cat(backend, "typed name-only invalid-option and source-row sentinel checks passed\n")
  }
}
f()
