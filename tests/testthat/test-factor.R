test_that("reconstruct_factor() works with data.frame", {
  x <- c("a", "b", "c", NA_character_)
  data <- data.frame(
    x1 = factor(x),
    x2 = factor(x, ordered = TRUE),
    x3 = factor(x, exclude = NULL),
    x4 = factor(x, levels = c("a", "b", "c", "aaa"))
  )

  l_info <- lapply(
    colnames(data),
    \(col) {
      list(
        col = col,
        levels = levels(data[[col]]),
        has_na_in_level = anyNA(levels(data[[col]])),
        ordered = is.ordered(data[[col]])
      )
    }
  )

  data_chr <- dplyr::mutate(
    .data = data,
    x1 = as.character(x1),
    x2 = as.character(x2),
    x3 = as.character(x3),
    x4 = as.character(x4)
  )

  actual <- Reduce(
    function(df, info) {
      reconstruct_factor.data.frame(df, info, "aaa")
    },
    x = l_info,
    init = data_chr
  )

  expect_identical(
    actual,
    data.frame(
      x1 = factor(
        c("a", "b", "c", NA),
        levels = c("aaa", "a", "b", "c")
      ),
      x2 = ordered(
        c("a", "b", "c", NA),
        levels = c("aaa", "a", "b", "c")
      ),
      x3 = factor(
        c("a", "b", "c", NA),
        levels = c("aaa", "a", "b", "c", NA),
        exclude = NULL
      ),
      x4 = factor(
        c("a", "b", "c", NA),
        levels = c("aaa", "a", "b", "c")
      )
    )
  )
})

test_that("reconstruct_factor() works with duckdb", {
  x <- c("a", "b", "c", NA_character_)
  data <- data.frame(
    x1 = factor(x),
    x2 = factor(x, ordered = TRUE),
    x3 = factor(x, exclude = NULL),
    x4 = factor(x, levels = c("a", "b", "c", "aaa"))
  )

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "data", data)

  data <- dplyr::tbl(con, "data")

  tmp_df <- dplyr::collect(utils::head(data, n = 0))

  l_info <- lapply(
    colnames(tmp_df),
    \(col) {
      list(
        col = col,
        levels = levels(tmp_df[[col]]),
        has_na_in_level = anyNA(levels(tmp_df[[col]])),
        ordered = is.ordered(tmp_df[[col]])
      )
    }
  )

  data_chr <- dplyr::mutate(
    .data = data,
    x1 = as.character(x1),
    x2 = as.character(x2),
    x3 = as.character(x3),
    x4 = as.character(x4)
  )

  actual <- Reduce(
    function(df, info) {
      reconstruct_factor.tbl_duckdb_connection(df, info, "aaa")
    },
    x = l_info,
    init = data_chr
  )

  expect_identical(
    as.data.frame(
      dplyr::collect(actual)
    ),
    data.frame(
      x1 = factor(c("a", "b", "c", NA), levels = c("aaa", "a", "b", "c")),
      x2 = factor(c("a", "b", "c", NA), levels = c("aaa", "a", "b", "c")),
      x3 = factor(c("a", "b", "c", NA), levels = c("aaa", "a", "b", "c")),
      x4 = factor(c("a", "b", "c", NA), levels = c("aaa", "a", "b", "c"))
    )
  )
})

test_that("reconstruct_factor() works with dtplyr_step", {
  x <- c("a", "b", "c", NA_character_)
  data <- data.frame(
    x1 = factor(x),
    x2 = factor(x, ordered = TRUE),
    x3 = factor(x, exclude = NULL),
    x4 = factor(x, levels = c("a", "b", "c", "aaa"))
  )

  data <- dtplyr::lazy_dt(data)
  tmp_df <- dplyr::collect(utils::head(x = data, n = 0))

  l_info <- lapply(
    get_col_names.dtplyr_step(data = data, dplyr::everything()),
    \(col) {
      list(
        col = col,
        levels = levels(tmp_df[[col]]),
        has_na_in_level = anyNA(levels(tmp_df[[col]])),
        ordered = is.ordered(tmp_df[[col]])
      )
    }
  )

  data_chr <- dplyr::mutate(
    .data = data,
    x1 = as.character(x1),
    x2 = as.character(x2),
    x3 = as.character(x3),
    x4 = as.character(x4)
  )

  actual <- Reduce(
    function(df, info) {
      reconstruct_factor.dtplyr_step(df, info, "aaa")
    },
    x = l_info,
    init = data_chr
  )

  expect_identical(
    as.data.frame(
      dplyr::collect(actual)
    ),
    data.frame(
      x1 = factor(
        c("a", "b", "c", NA),
        levels = c("aaa", "a", "b", "c")
      ),
      x2 = ordered(
        c("a", "b", "c", NA),
        levels = c("aaa", "a", "b", "c")
      ),
      x3 = factor(
        c("a", "b", "c", NA),
        levels = c("aaa", "a", "b", "c", NA),
        exclude = NULL
      ),
      x4 = factor(
        c("a", "b", "c", NA),
        levels = c("aaa", "a", "b", "c")
      )
    )
  )
})
