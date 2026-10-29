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
        bag <- if (is.null(id)) actual else actual[order(actual$sid), ]
        rownames(bag) <- NULL
        expect_identical(bag, expected, info = info)
        if (count != 500L) next
        saved <- dplyr::compute(query, name = "wide_saved", overwrite = TRUE)
        expect_identical(DBI::dbListFields(con, "wide_saved"), names(expected),
                         info = info)
        for (endpoint in list(query, saved)) {
          for (n in c(Inf, 0L, 1L)) {
            rows <- seq_len(min(n, nrow(expected)))
            got <- as.data.frame(dplyr::collect(endpoint, n = n))
            target <- expected[rows, ]
            if (identical(sort, "none")) {
              expect_true(all(got$sid %in% expected$sid), info = info)
              if (is.infinite(n)) {
                got <- got[order(got$sid), ]
                rownames(got) <- NULL
              } else {
                target$sid <- got$sid
              }
            }
            expect_identical(got, target, info = paste(info, n))
          }
        }
      }
    }
  }
})
