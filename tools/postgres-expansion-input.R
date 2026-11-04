#!/usr/bin/env Rscript

# Public acceptance for #977 and #1243 against the server selected by PG
# environment variables. Install the working tree first, then run from its root.
library(marginplyr)
library(testthat)
source("tests/testthat/helper-sql-expansion-input.R")
con <- DBI::dbConnect(RPostgres::Postgres())
tryCatch({
  print(DBI::dbGetQuery(con, "SELECT version()"))
  check_expansion_input_limits(con)
  check_expansion_shapes(con)
  cat("PostgreSQL selected-input expansion: passed\n")

  DBI::dbExecute(con, "CREATE TEMPORARY SEQUENCE expansion_payload_counter")
  source <- dplyr::tbl(con, dbplyr::sql(paste(
    "SELECT 'detail' AS g,",
    "CAST(nextval('pg_temp.expansion_payload_counter') AS INTEGER) AS v"
  )), vars = c("g", "v"))
  query <- expand_with_margins(source, .grouping = rollup("g"), .id = "set")
  result <- dplyr::collect(query) |> dplyr::arrange(.data$set)

  expect_identical(result, tibble::tibble(
    g = c("detail", "Total"), set = 1:2, v = c(1L, 1L)
  ))
  # Equal copies alone do not rule out an extra discarded evaluation.
  counter <- DBI::dbGetQuery(con, paste(
    "SELECT CAST(last_value AS INTEGER) AS v",
    "FROM pg_temp.expansion_payload_counter"
  ))
  expect_identical(counter$v, 1L)
  cat("PostgreSQL shared-input expansion: passed\n")
}, finally = DBI::dbDisconnect(con))
