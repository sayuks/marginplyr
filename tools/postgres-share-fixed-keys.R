#!/usr/bin/env Rscript

# Public acceptance for #785 against a disposable PostgreSQL server selected
# through standard PG environment variables. Install the working tree first.
library(marginplyr)
source("tests/testthat/helper-share-fixed-keys.R")

con <- DBI::dbConnect(RPostgres::Postgres())
tryCatch(
  expect_sql_share_fixed_keys(con),
  finally = DBI::dbDisconnect(con)
)
cat("PostgreSQL fixed-key Parent and Total shares passed.\n")
