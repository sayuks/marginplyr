#!/usr/bin/env Rscript
# Run from the investigated worktree root. --assert checks the expected order.
pkgload::load_all(quiet = TRUE)

main <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite())
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE facts (g TEXT COLLATE NOCASE)")
  DBI::dbExecute(con, "INSERT INTO facts VALUES ('B'), ('a')")
  source <- dplyr::tbl(con, "facts")
  options_cte <- dbplyr::sql_options(cte = TRUE)
  result <- expand_with_margins(
    source, .grouping = grouping_set(g), .margin_label = NULL, .sort = "last"
  )
  ordinary <- dplyr::arrange(dplyr::filter(source, !is.na(g)), g)
  expected <- c("a", "B")
  baseline <- dplyr::collect(result)$g
  actual <- dplyr::collect(result, sql_options = options_cte)$g
  raw_cte <- as.character(dbplyr::sql_render(result, sql_options = options_cte))
  raw_out <- DBI::dbGetQuery(con, raw_cte)$g
  corrected <- paste0(raw_cte, " COLLATE NOCASE")
  ordinary_default <- dplyr::collect(ordinary)$g
  ordinary_cte <- dplyr::collect(ordinary, sql_options = options_cte)$g
  restored <- DBI::dbGetQuery(con, corrected)$g
  stopifnot(
    identical(baseline, expected), identical(ordinary_default, expected),
    identical(ordinary_cte, expected), identical(raw_out, actual),
    identical(restored, expected)
  )
  print(list(
    expected = expected, direct_default = baseline, direct_cte = actual,
    first_default = dplyr::collect(result, n = 1L)$g,
    first_cte = dplyr::collect(result, n = 1L, sql_options = options_cte)$g,
    ordinary_cte = ordinary_cte, raw_cte = raw_out,
    raw_cte_explicit_collation = restored
  ))
  cat(raw_cte, "\n")
  if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
    stopifnot(identical(actual, expected))
  }
}

main()
