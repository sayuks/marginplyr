#!/usr/bin/env Rscript

# Public acceptance for #1641 against the server selected by PG environment
# variables. Install the working tree first, then run from its root.
library(marginplyr)
library(testthat)
source("tests/testthat/helper-sql-arranged-summary.R")
con <- DBI::dbConnect(RPostgres::Postgres())
tryCatch(
  expect_window_ordered_typed_missing_summary(con),
  finally = DBI::dbDisconnect(con)
)
cat("PostgreSQL typed-missing summary over a window-ordered input: passed\n")
