args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(pkgload::load_all(args[[1L]], quiet = TRUE))
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
source <- dplyr::copy_to(con, data.frame(g = rep(seq_len(1000L), 100L), v = 1),
  "input", temporary = TRUE)
query <- summarize_with_margins(source, z = sum(v), mask = grouping_id(),
  .grouping = grouping_set(g), .margin_label = NULL, .sort = "none")
capture <- new.env(parent = emptyenv())
capture$sql <- character()
for (entry in "dbSendQuery") {
  suppressMessages(trace(entry, where = asNamespace("DBI"), print = FALSE,
    tracer = function() {
      statement <- get("statement", envir = parent.frame())
      capture$sql <- c(capture$sql, as.character(statement))
    }))
}
computed <- dplyr::compute(query, name = "trace_output", analyze = FALSE)
for (entry in "dbSendQuery") {
  suppressMessages(untrace(entry, where = asNamespace("DBI")))
}
cat("COMPUTE STATEMENTS", length(capture$sql), "\n")
for (sql in capture$sql) cat(sql, "\n---\n")
DBI::dbRemoveTable(con, "trace_output")
times <- replicate(7L, {
  time <- system.time(dplyr::compute(query, name = "timing_output", analyze = FALSE))
  DBI::dbRemoveTable(con, "timing_output")
  unname(time[["elapsed"]])
})
cat("elapsed seconds:", paste(times, collapse = ", "), "\n")
cat("median elapsed:", median(times), "\n")
DBI::dbDisconnect(con)
