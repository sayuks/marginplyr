#!/usr/bin/env Rscript

# Public acceptance for #1442 against a disposable PostgreSQL server selected
# through standard PG environment variables. Install the working tree first.
library(marginplyr)
source("tests/testthat/helper-sql-share-cardinality.R")

con <- DBI::dbConnect(RPostgres::Postgres())
tryCatch(
  expect_sql_share_cardinality(con, "postgres"),
  finally = DBI::dbDisconnect(con)
)
cat("PostgreSQL Parent and Total shares require one source value per group.\n")
