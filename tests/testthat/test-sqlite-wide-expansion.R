test_that("SQLite expands a nine-dimension cube within compound-query limits", {
  skip_if_suggest_absent("RSQLite", "DBI")
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  dimensions <- paste0("d", 1:9)
  data <- as.data.frame(as.list(stats::setNames(seq_len(9L), dimensions)))
  input <- dplyr::copy_to(con, data, "wide_cube")
  sets <- unlist(lapply(9:0, function(size) {
    utils::combn(dimensions, size, simplify = FALSE)
  }), recursive = FALSE)
  arms <- vapply(seq_along(sets), function(i) {
    columns <- ifelse(dimensions %in% sets[[i]], dimensions, "NULL")
    projections <- paste(columns, "AS", dimensions, collapse = ", ")
    paste0("SELECT ", projections, ", ", i, " AS sid FROM wide_cube")
  }, character(1))
  halves <- vapply(split(arms, rep(1:2, each = 256L)), function(part) {
    paste0("SELECT * FROM (", paste(part, collapse = " UNION ALL "), ")")
  }, character(1))
  control <- DBI::dbGetQuery(con, paste(halves, collapse = " UNION ALL "))
  for (sort in c("none", "first", "last")) {
    build <- function() {
      testthat::local_mocked_bindings(
        dbSendQuery = function(...) stop("unexpected source read"),
        .package = "DBI"
      )
      expand_with_margins(
        input, .grouping = cube(dplyr::everything()), .margin_label = NULL,
        .id = "sid", .sort = sort
      )
    }
    expected <- expand_with_margins(
      data, .grouping = cube(dplyr::everything()), .margin_label = NULL,
      .id = "sid", .sort = sort
    )
    query <- build()
    actual <- as.data.frame(dplyr::collect(query))
    expect_identical(nrow(actual), 512L, info = sort)
    if (!identical(sort, "none")) {
      expect_identical(actual, expected, info = sort)
    }
    bag <- actual[order(actual$sid), ]
    rownames(bag) <- NULL
    expect_identical(bag, control, info = sort)
    expect_identical(last_sent_queries()$sql,
                     as.character(dbplyr::sql_render(query)), info = sort)
    if (identical(sort, "last")) {
      saved <- dplyr::compute(query, name = "cube_saved")
      expect_identical(DBI::dbListFields(con, "cube_saved"), names(expected))
      expect_identical(as.data.frame(dplyr::collect(saved)), expected)
    }
  }
})
