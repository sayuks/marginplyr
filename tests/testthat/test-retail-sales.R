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
