#!/usr/bin/env Rscript

# Run the shared public contract against a disposable PostgreSQL server.
library(marginplyr)
library(testthat)
source("tests/testthat/helper-sql-label-collision.R")
con <- DBI::dbConnect(RPostgres::Postgres())
tryCatch(check_sql_label_collision(con), finally = DBI::dbDisconnect(con))
