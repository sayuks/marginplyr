# Run from the earlier note's archived source root, not an installed package.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
stopifnot(!".aggregate" %in% names(formals(marginplyr::summarize_with_margins)))
main <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  input <- dplyr::copy_to(con, data.frame(g = c("a", "b"), x = c(1, 3)), "facts")
  for (label in c("auto", "portable")) {
    query <- marginplyr::summarize_with_margins(input, value = sum(x),
      .grouping = marginplyr::rollup(g), .id = "set", .aggregate = label)
    native <- grepl("GROUP BY GROUPING SETS", as.character(dbplyr::sql_render(query)), fixed = TRUE)
    ordinary_output <- ".aggregate" %in% dplyr::tbl_vars(query)
    cat("label", label, "ordinary_output", ordinary_output, "native", native, "\n")
    stopifnot(ordinary_output, native)
  }
}
main()
