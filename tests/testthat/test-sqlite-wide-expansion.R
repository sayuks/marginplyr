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
    expect_identical(actual, expected, info = sort)
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

test_that("SQLite includes every anchor at the wide-plan threshold", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(
    g = rep("x", 2L), fixed = rep(NA_real_, 2L),
    payload = rep(NA_integer_, 2L), text = rep(NA_character_, 2L)
  )
  input <- dplyr::copy_to(con, data, "wide_included")
  for (count in c(498L, 499L, 500L, 501L)) {
    spec <- grouping_sets(!!!rep(list(grouping_set(g)), count))
    id <- if (count == 498L) NULL else "sid"
    for (sort in c("none", "last", if (count == 500L) "first")) {
      for (empty in if (count == 500L) c(FALSE, TRUE) else FALSE) {
        source <- if (empty) dplyr::filter(input, FALSE) else input
        local <- if (empty) data[integer(), ] else data
        expected <- expand_with_margins(
          local, .by = fixed, .grouping = spec, .duplicates = "keep",
          .margin_label = NULL, .id = id, .sort = sort
        )
        query <- expand_with_margins(
          source, .by = fixed, .grouping = spec, .duplicates = "keep",
          .margin_label = NULL, .id = id, .sort = sort
        )
        info <- paste(count, sort, empty)
        actual <- as.data.frame(dplyr::collect(query))
        expect_identical(actual, expected, info = info)
        if (!is.null(id)) {
          expect_identical(actual$sid,
                           if (empty) integer() else
                             rep(seq_len(count), each = 2L),
                           info = info)
        }
        if (count != 500L) next
        saved <- dplyr::compute(query, name = "wide_saved", overwrite = TRUE)
        expect_identical(DBI::dbListFields(con, "wide_saved"), names(expected),
                         info = info)
        for (endpoint in list(query, saved)) {
          for (n in c(Inf, 0L, 1L)) {
            rows <- seq_len(min(n, nrow(expected)))
            expect_identical(as.data.frame(dplyr::collect(endpoint, n = n)),
                             expected[rows, ], info = paste(info, n))
          }
        }
      }
    }
  }
})
