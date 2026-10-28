#!/usr/bin/env Rscript

# Public acceptance for #977 against the server selected by PG environment
# variables. Install the working tree first, then run from the repository root.
library(marginplyr)
library(testthat)
source("tests/testthat/helper-sql-expansion-input.R")
con <- DBI::dbConnect(RPostgres::Postgres())
tryCatch({
  print(DBI::dbGetQuery(con, "SELECT version()"))
  check_expansion_input_limits(con)
  check_expansion_shapes(con)
  cat("PostgreSQL selected-input expansion: passed\n")
}, finally = DBI::dbDisconnect(con))
