suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
source("inst/suggests/guard.R")
input <- data.frame(part = "A", g = "a", v = 1)
run <- function(data, label) {
  for (empty in c(FALSE, TRUE)) for (duplicates in c("error", "keep")) {
    x <- if (empty) dplyr::filter(data, v < 0) else data
    query <- summarize_with_margins(x, bit = grouping_bit(g), mask = grouping_id(),
      .by = part, .grouping = rollup(g), .id = "sid",
      .margin_label = NULL, .duplicates = duplicates)
    result <- dplyr::collect(query)
    cat(label, "empty=", empty, "duplicates=", duplicates,
      "rows=", nrow(result), "\n")
    print(vapply(result[c("sid", "bit", "mask")], typeof, character(1)))
  }
}
run(input, "local")
if (marginplyr_suggest_available("dtplyr")) run(dtplyr::lazy_dt(input), "dtplyr")
if (marginplyr_suggest_available("arrow")) run(arrow::arrow_table(input), "arrow")
if (marginplyr_suggest_available("duckdb")) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  run(dplyr::copy_to(con, input, "type_probe"), "duckdb")
  DBI::dbDisconnect(con, shutdown = TRUE)
}
