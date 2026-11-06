test_that("SQLite included-only Margin order fits a wide public result", {
  skip_if_suggest_absent("RSQLite", "DBI")
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (width in c(1998L, 1999L)) {
    data <- as.data.frame(stats::setNames(
      rep(list(c(2L, 1L)), width), c("g", paste0("p", seq_len(width - 1L)))
    ))
    DBI::dbWriteTable(con, "wide_order", data, overwrite = TRUE)
    input <- dplyr::tbl(con, "wide_order")
    expected <- data[2:1, ]
    rownames(expected) <- NULL
    ordinary <- dplyr::arrange(input, is.na(g), g)
    expect_identical(as.data.frame(dplyr::collect(ordinary)), expected)
    sql <- DBI::dbGetQuery(con, paste(
      "SELECT * FROM wide_order ORDER BY g IS NULL, g"
    ))
    expect_identical(sql, expected)
    unsorted <- expand_with_margins(
      input, .grouping = grouping_set(g), .margin_label = NULL
    )
    expect_identical(as.data.frame(dplyr::collect(unsorted)), data)
    for (sort in c("first", "last")) {
      build <- function() {
        testthat::local_mocked_bindings(
          dbSendQuery = function(...) stop("unexpected source read"),
          .package = "DBI"
        )
        query <- expand_with_margins(
          input, .grouping = grouping_set(g), .margin_label = NULL, .sort = sort
        )
        dbplyr::sql_render(query)
        query
      }
      query <- build()
      expect_identical(as.character(dplyr::tbl_vars(query)), names(expected))
      expect_identical(as.data.frame(dplyr::collect(query)), expected,
                       info = paste(width, sort))
      rendered <- dbplyr::sql_render(query)
      expect_identical(last_sent_queries()$sql, as.character(rendered))
      physical <- DBI::dbGetQuery(con, rendered)
      # The engine limit applies to public and private projections together.
      expect_lte(ncol(physical), 2000L)
      expect_identical(physical[names(expected)], expected)
      if (width != 1999L) next
      saved <- dplyr::compute(
        query, name = "wide_order_saved", overwrite = TRUE
      )
      expect_identical(DBI::dbListFields(con, "wide_order_saved"), names(data))
      for (endpoint in list(query, saved)) {
        for (n in c(0L, 1L, 3L)) {
          rows <- seq_len(min(n, nrow(expected)))
          expect_identical(as.data.frame(dplyr::collect(endpoint, n = n)),
                           expected[rows, ], info = paste(width, sort, n))
        }
      }
    }
  }
})

test_that("SQLite reused order values retain source and declared types", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(
    fixed = c(NA_real_, 1, 1), g = c(NA_integer_, 2L, 1L),
    text = NA_character_, payload = NA_integer_, number = NA_real_
  )
  input <- dplyr::copy_to(con, data, "order_source_types")
  expected <- data.frame(
    fixed = c(1, 1, NA_real_), g = c(1L, 2L, NA_integer_), sid = 1L,
    text = NA_character_, payload = NA_integer_, number = NA_real_
  )
  for (empty in c(FALSE, TRUE)) {
    source <- if (empty) dplyr::filter(input, FALSE) else input
    target <- if (empty) expected[integer(), ] else expected
    for (sort in c("first", "last")) {
      query <- expand_with_margins(
        source, .by = fixed, .grouping = grouping_set(g), .id = "sid",
        .margin_label = NULL, .sort = sort
      )
      saved <- dplyr::compute(query)
      for (endpoint in list(query, saved)) {
        for (n in c(Inf, 0L, 1L)) {
          rows <- seq_len(min(n, nrow(target)))
          expect_identical(as.data.frame(dplyr::collect(endpoint, n = n)),
                           target[rows, ], info = paste(empty, sort, n))
        }
      }
    }
  }
})

test_that("SQLite public-only order keys survive an anchored expansion", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(con, data.frame(g = NA_character_), "order_id_only")
  input <- expand_with_margins(source, .grouping = grouping_set(), .id = "sid")
  query <- expand_with_margins(
    input, .grouping = grouping_sets(grouping_set(), grouping_set()),
    .duplicates = "keep", .id = "second", .sort = "last"
  )
  expected <- data.frame(second = 1:2, sid = c(1L, 1L), g = NA_character_)
  for (endpoint in list(query, dplyr::compute(query))) {
    for (n in c(Inf, 0L, 1L)) {
      rows <- seq_len(min(n, nrow(expected)))
      expect_identical(as.data.frame(dplyr::collect(endpoint, n = n)),
                       expected[rows, ])
    }
  }
})
