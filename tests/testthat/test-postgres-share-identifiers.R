test_that("PostgreSQL share aliases fit the server identifier limit", {
  source <- dbplyr::lazy_frame(
    g = "a", x = 1, con = dbplyr::simulate_postgres()
  )
  for (stem in c(strrep("s", 35L), strrep("計", 12L))) {
    sources <- paste0(stem, c("a", "b"))
    ordinary <- stats::setNames(
      rlang::exprs(sum(.data$x), 2 * sum(.data$x)), sources
    )
    for (duplicates in c("error", "keep")) {
      query <- summarize_with_margins(
        source, !!!ordinary,
        first = share_of_parent(!!rlang::sym(sources[[1L]])),
        second = share_of_parent(!!rlang::sym(sources[[2L]])),
        total = share_of_total(!!rlang::sym(sources[[2L]])),
        .grouping = rollup(g), .duplicates = duplicates,
        .check_share_source = FALSE
      )
      expect_s3_class(query, "tbl_lazy")
      expect_identical(
        as.character(dplyr::tbl_vars(query)),
        c("g", sources, "first", "second", "total")
      )
      sql <- as.character(dbplyr::sql_render(query))
      identifiers <- regmatches(sql, gregexpr('"[^"]+"', sql))[[1L]]
      identifiers <- substring(identifiers, 2L, nchar(identifiers) - 1L)
      expect_true(all(nchar(identifiers, type = "bytes") <= 63L))
    }
  }
})
