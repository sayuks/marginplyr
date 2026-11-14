test_that("a literal removal keeps a special input measure", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only one optional backend is required.
  input <- data.table::data.table(f = c("P", "P"), g = c("a", "b"),
                                  .BY = c(1, 3))
  source <- dtplyr::lazy_dt(input)
  summarize <- function(read) {
    summarize_with_margins(
      source, value = sum(.BY), fraction = share_of_parent(value),
      observed = !!read, .by = f, .grouping = rollup(g)
    )
  }
  kept <- dplyr::collect(summarize(quote((function() {
    other <- 1
    rm("other")
    mean(.BY)
  })())))
  expect_identical(kept$observed, c(1, 3, 2))
  expect_identical(kept$fraction, c(0.25, 0.75, 1))
  # A name computed at run time can remove the measure itself.
  for (read in alist(
    (function() {
      other <- 1
      name <- "other"
      rm(list = name)
      mean(.BY)
    })(),
    (function() {
      other <- 1
      rm(paste0("other"))
      mean(.BY)
    })()
  )) {
    expect_error(
      summarize(read),
      "cannot safely distinguish input and lexical `.BY` reads",
      class = "marginplyr_error"
    )
  }
})
