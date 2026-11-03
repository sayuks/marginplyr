test_that("empty hms dimensions have character Margin displays", {
  skip_if_suggest_absent("hms")
  keys <- list(
    hms::hms(numeric()),
    as.difftime(numeric(), units = "secs"),
    character()
  )
  for (key in keys) {
    input <- tibble::tibble(g = key, row = integer())
    before <- serialize(input, NULL)
    expect_identical(nrow(dplyr::group_keys(dplyr::group_by(input, g))), 0L)
    expect_identical(nrow(dplyr::nest_by(input, g)), 0L)
    plans <- list(included = grouping_set(g), rollup = rollup(g), cube = cube(g))
    for (kind in names(plans)) {
      plan <- plans[[kind]]
      for (check in c(FALSE, TRUE)) {
        for (verb in list(nest_with_margins, nest_by_with_margins)) {
          result <- verb(input, .grouping = plan, .check_margin_label = check)
          expect_identical(nrow(result), 0L)
          expect_identical(result$g, character())
          expect_length(result$data, 0L)
        }
        expanded <- expand_with_margins(
          input, .grouping = plan, .check_margin_label = check
        )
        expect_identical(expanded$g, character())
        expect_identical(expanded$row, integer())
        summary <- summarize_with_margins(
          input, n = dplyr::n(), .grouping = plan,
          .check_margin_label = check
        )
        total <- kind != "included"
        expect_identical(summary$g, if (total) "Total" else character())
        expect_identical(summary$n, if (total) 0L else integer())
      }
    }
    expect_identical(serialize(input, NULL), before)
  }
})

test_that("hms nesting keeps source keys and payloads under both label policies", {
  skip_if_suggest_absent("hms")
  for (as_source in list(identity, as.data.frame)) {
    input <- as_source(tibble::tibble(g = hms::hms(c(1, 1)), row = 1:2))
    before <- serialize(input, NULL)
    native <- dplyr::nest_by(input, g, .keep = TRUE)
    expect_identical(native$data[[1L]]$row, 1:2)
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      for (keep in c(FALSE, TRUE)) {
        result <- verb(input, .grouping = rollup(g), .keep = keep)
        expect_identical(result$g, c("00:00:01", "Total"))
        for (cell in result$data) {
          expect_identical(cell$row, 1:2)
          expect_identical(names(cell), c("g"[keep], "row"))
          if (keep) expect_identical(cell$g, input$g)
        }
        for (label in list(NULL, NA_character_)) {
          typed <- verb(input, .grouping = rollup(g),
                        .margin_label = label, .keep = keep)
          expect_identical(typed$g, vctrs::vec_c(input$g[1L],
                                               vctrs::vec_init(input$g, 1L)))
          expect_identical(lapply(typed$data, function(x) x$row), list(1:2, 1:2))
        }
        expect_error(
          verb(input, .grouping = rollup(g), .margin_label = "00:00:01"),
          class = "marginplyr_error"
        )
      }
    }
    empty <- input[0L, ]
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      result <- verb(empty, .grouping = rollup(g), .margin_label = NULL)
      expect_identical(nrow(result), 0L)
      expect_identical(result$g, empty$g)
    }
    expect_identical(serialize(input, NULL), before)
  }
})
