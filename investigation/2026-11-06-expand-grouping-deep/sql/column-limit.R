Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
library(dplyr)
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
width <- as.integer(Sys.getenv("MARGINPLYR_HUNT_WIDTH", "1999"))
data <- as.data.frame(as.list(setNames(rep(list(c(2L, 1L)), width),
  c("g", paste0("p", seq_len(width - 1L))))))
DBI::dbWriteTable(con, "facts", data)
input <- tbl(con, "facts")
reference <- collect(arrange(input, g))
expected <- data[2:1, ]
rownames(expected) <- NULL
stopifnot(identical(as.data.frame(reference), expected))
cat("ordinary arranged source: 2 rows x", ncol(reference), "columns\n")
plain <- expand_with_margins(input, .grouping = grouping_set(g),
  .margin_label = NULL)
stopifnot(identical(as.data.frame(collect(plain)), data))
cat("unsorted included-only expansion: exact match\n")
query <- expand_with_margins(input, .grouping = grouping_set(g),
  .margin_label = NULL, .sort = "last")
cat("public columns:", ncol(query), "; private order columns:",
  length(attr(query, "marginplyr_order_keys")), "\n")
actual <- tryCatch(collect(query), error = identity)
cat("sorted included-only expansion:", if (inherits(actual, "error"))
  conditionMessage(actual) else "success", "\n")
DBI::dbDisconnect(con)
if ("--assert" %in% commandArgs(TRUE)) {
  stopifnot(!inherits(actual, "error"),
    identical(as.data.frame(actual), as.data.frame(reference)))
}
