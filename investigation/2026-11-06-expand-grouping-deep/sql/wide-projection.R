Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
library(dplyr)
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
for (width in c(490L, 499L, 500L, 501L, 510L)) {
  x <- as.data.frame(as.list(setNames(seq_len(width), paste0("d", seq_len(width)))))
  DBI::dbWriteTable(con, "facts", x, overwrite = TRUE)
  input <- tbl(con, "facts")
  for (sorting in c("none", "last", "first")) {
    outcome <- tryCatch({
      query <- expand_with_margins(input,
        .grouping = grouping_set(everything()), .margin_label = NULL,
        .sort = sorting)
      value <- collect(query)
      stopifnot(identical(as.data.frame(value), x))
      "match"
    }, error = conditionMessage)
    cat(width, sorting, outcome, "\n")
  }
  reference <- collect(arrange(input, across(everything())))
  stopifnot(identical(as.data.frame(reference), x))
  cat(width, "ordinary arranged control match\n")
}
DBI::dbDisconnect(con)
