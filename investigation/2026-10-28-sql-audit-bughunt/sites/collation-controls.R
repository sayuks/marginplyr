# Compare the candidate label collision with SQLite's own equality relation.
main <- function() {
  pkgload::load_all(".", quiet = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cases <- list(
    BINARY = list(value = "total", equal = FALSE),
    NOCASE = list(value = "total", equal = TRUE),
    RTRIM = list(value = "Total ", equal = TRUE)
  )
  for (collation in names(cases)) {
    case <- cases[[collation]]
    DBI::dbExecute(con, paste0("CREATE TABLE candidate (g TEXT COLLATE ", collation, ")"))
    DBI::dbExecute(con, "INSERT INTO candidate VALUES (?)", params = list(case$value))
    input <- dplyr::tbl(con, "candidate")
    unchecked <- summarize_with_margins(
      input, rows = dplyr::n(), .grouping = rollup(g),
      .check_margin_label = FALSE
    )
    values <- dplyr::collect(unchecked)
    stopifnot(setequal(values$g, c(case$value, "Total")))
    checked <- tryCatch(summarize_with_margins(
      input, rows = dplyr::n(), .grouping = rollup(g),
      .check_margin_label = TRUE
    ), error = identity)
    saved <- last_sent_queries(details = TRUE)
    stopifnot(identical(saved$status[saved$purpose != "result"], "returned"))
    equal <- DBI::dbGetQuery(con, "SELECT g = 'Total' AS same_value FROM candidate")$same_value
    stopifnot(identical(as.logical(equal), case$equal))
    if (case$equal) {
      stopifnot(inherits(checked, "marginplyr_error"),
                grepl("already present in grouping column", conditionMessage(checked), fixed = TRUE))
    } else {
      stopifnot(inherits(checked, "tbl_sql"),
                identical(dplyr::collect(checked), values))
    }
    stopifnot(identical(last_sent_queries(details = TRUE), saved))
    cat(collation, "database equality:", case$equal,
        "; matching collision outcome; record stable\n")
    DBI::dbRemoveTable(con, "candidate")
  }
  cat("PASS: 3 collation controls; no additional defect established\n")
}
main()
