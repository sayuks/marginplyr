test_that("retail_sales is a tibble with the documented columns", {
  expect_s3_class(retail_sales, "tbl_df")
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
