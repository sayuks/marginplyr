Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
local({
  con <- DBI::dbConnect(RSQLite::SQLite())
  on.exit(DBI::dbDisconnect(con))
  src <- copy_to(con, tibble(g = 1L), "src", temporary = TRUE)
  ordinary <- collect(collapse(arrange(src, g)))
  stopifnot(identical(names(ordinary), "g"), identical(ordinary$g, 1L))
  x <- expand_with_margins(src, .grouping = grouping_set(g),
                           .margin_label = NULL, .sort = "last")
  direct <- collect(x)
  collapsed <- collapse(x)
  result <- collect(collapsed)
  stopifnot(identical(names(direct), "g"), identical(colnames(collapsed), "g"))
  cat("Expected public columns:", names(direct), "\n")
  cat("Reported collapsed columns:", colnames(collapsed), "\n")
  cat("Collected collapsed columns:", names(result), "\n")
  print(result)
  if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(identical(names(result), names(direct)))
  }
})
