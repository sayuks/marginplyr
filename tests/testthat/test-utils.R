test_that("assert_logical_scalar_scalar() works", {
  expect_no_error(assert_logical_scalar(TRUE))
  expect_no_error(assert_logical_scalar(FALSE))
  expect_error(assert_logical_scalar())
  expect_error(assert_logical_scalar(logical()))
  expect_error(assert_logical_scalar(c(TRUE, TRUE)))
  expect_error(assert_logical_scalar(list(TRUE)))
  expect_error(assert_logical_scalar(1))
  expect_error(assert_logical_scalar("a"))
  expect_error(assert_logical_scalar(NULL))
  expect_error(assert_logical_scalar(new.env()))
})

test_that("assert_sring_scalar() works", {
  expect_no_error(assert_string_scalar("a"))
  expect_no_error(assert_string_scalar(""))
  expect_no_error(assert_string_scalar(NA_character_))
  expect_error(assert_string_scalar())
  expect_error(assert_string_scalar(character()))
  expect_error(assert_string_scalar(c("1", "a")))
  expect_error(assert_string_scalar(list("a")))
  expect_error(assert_string_scalar(1))
  expect_error(assert_string_scalar(TRUE))
  expect_error(assert_string_scalar(NULL))
  expect_error(assert_string_scalar(new.env()))
})

test_that("assert_nest_possible() works", {
  expect_no_error(assert_nest_possible(data.frame()))
  expect_no_error(assert_nest_possible(dtplyr::lazy_dt(data.frame())))
  expect_error(assert_nest_possible())
  expect_error(assert_nest_possible(character()))
  expect_error(assert_nest_possible(list(data.frame())))
  expect_error(assert_nest_possible(1))
  expect_error(assert_nest_possible(TRUE))
  expect_error(assert_nest_possible(NULL))
  expect_error(assert_nest_possible(new.env()))
})

test_that("assert_lazy_table() works", {
  expect_error(
    assert_lazy_table(
      arrow::as_record_batch_reader(data.frame())
    ),
    "must not be an object of the following class"
  )
  expect_no_error(assert_lazy_table(data.frame()))
  expect_no_error(assert_lazy_table(arrow::arrow_table(x = 1)))
  expect_no_error(assert_lazy_table(arrow::record_batch(x = 1)))

  # arrow "Dataset" class
  tmp <- tempfile("test", fileext = ".parquet")
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE))

  arrow::write_parquet(
    x = data.frame(),
    sink = tmp
  )

  expect_no_error(
    assert_lazy_table(
      arrow::open_dataset(tmp)
    )
  )
})
