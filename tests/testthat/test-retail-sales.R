test_that("retail_sales keeps the lazy-data configuration its usage assumes", {
  # `?retail_sales` gives the bare object name as its usage, and every other
  # test here reads `retail_sales` without `data()`. Both only hold while the
  # package is installed with `LazyData`; without it the data set would need
  # `data(retail_sales)` first.
  expect_identical(
    tolower(utils::packageDescription("marginplyr")$LazyData),
    "true"
  )
})

test_that("retail_sales is a plain data frame with the documented columns", {
  # A Margin verb hands its input's class through dplyr, so this is what the
  # examples print: a plain data frame in, a plain data frame out. Shipping a
  # tibble instead would make the `dplyr::as_tibble()` calls in the
  # documentation no-ops while their comments still claimed they were needed.
  expect_s3_class(retail_sales, "data.frame", exact = TRUE)
  expect_equal(dim(retail_sales), c(24L, 8L))
  expect_named(
    retail_sales,
    c(
      "year", "month", "region", "store", "product", "channel", "units",
      "revenue"
    )
  )
})

test_that("retail_sales has missing stores only on online-direct records", {
  # `store` is the only column documented to carry missing values; the
  # examples rely on every other column being complete.
  expect_identical(
    colSums(is.na(retail_sales)),
    c(
      year = 0, month = 0, region = 0, store = 4, product = 0, channel = 0,
      units = 0, revenue = 0
    )
  )
  expect_identical(
    unique(retail_sales$channel[is.na(retail_sales$store)]),
    "Online"
  )
  # The documentation uses this data to show that an omitted dimension and a
  # genuinely missing source value are different things, which only holds
  # while no source store is spelled like the default Margin label.
  expect_false("Total" %in% retail_sales$store)
})
