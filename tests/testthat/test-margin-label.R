test_that("named Margin labels apply per dimension and default to last", {
  data <- data.frame(
    first = factor(c("a", "b"), levels = c("a", "b")),
    second = ordered(c("x", "y"), levels = c("x", "y")),
    value = 1:2
  )

  result <- summarize_with_margins(
    data,
    n = dplyr::n(),
    id = grouping_id(first, second),
    .grouping = rollup(first, second),
    .margin_label = c(second = "All second", first = "All first")
  )

  expect_s3_class(result$first, "factor")
  expect_s3_class(result$second, "ordered")
  expect_identical(levels(result$first), c("a", "b", "All first"))
  expect_identical(levels(result$second), c("x", "y", "All second"))
  expect_identical(
    as.character(result$first[result$id == 3L]),
    "All first"
  )
  expect_identical(
    as.character(result$second[result$id == 1L]),
    rep("All second", 2L)
  )
})

factor_contract_data <- function(has_na_level, has_missing_value) {
  factor_levels <- if (has_na_level) c("x", NA_character_) else "x"
  codes <- c(1L, if (has_missing_value) NA_integer_ else 1L)
  data.frame(
    group = structure(codes, levels = factor_levels, class = "factor"),
    value = 1:2
  )
}

test_that("factor NA levels and missing values follow the eight-case contract", { # nolint: line_length_linter
  cases <- data.frame(
    label = c(rep("NA", 4L), rep("NULL", 4L)),
    na_level = rep(c(TRUE, TRUE, FALSE, FALSE), 2L),
    missing_value = rep(c(TRUE, FALSE, TRUE, FALSE), 2L),
    errors = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )

  for (i in seq_len(nrow(cases))) {
    case <- cases[i, ]
    data <- factor_contract_data(case$na_level, case$missing_value)
    label <- if (case$label == "NA") NA_character_ else NULL
    operation <- function() {
      summarize_with_margins(
        data,
        n = dplyr::n(),
        bit = grouping_bit(group),
        .grouping = rollup(group),
        .margin_label = label
      )
    }

    if (case$errors) {
      error <- expect_error(operation(), info = paste(case, collapse = "/"))
      expect_match(
        deparse1(conditionCall(error)),
        "summarize_with_margins",
        fixed = TRUE
      )
    } else {
      result <- operation()
      expect_s3_class(result$group, "factor")
      margin <- result[result$bit == 1L, , drop = FALSE]
      expect_true(is.na(margin$group))
      expect_identical(margin$n, 2L)
      expect_identical(levels(result$group), levels(data$group))
    }
  }
})

test_that("NA factor levels stay structural when collision checks are disabled", { # nolint: line_length_linter
  with_na_level <- factor_contract_data(
    has_na_level = TRUE,
    has_missing_value = FALSE
  )
  missing_value <- factor_contract_data(
    has_na_level = FALSE,
    has_missing_value = TRUE
  )

  expect_error(
    summarize_with_margins(
      with_na_level,
      n = dplyr::n(),
      .grouping = rollup(group),
      .margin_label = NA_character_,
      .check_margin_label = FALSE
    ),
    "already a factor level"
  )
  expect_no_error(
    summarize_with_margins(
      missing_value,
      n = dplyr::n(),
      .grouping = rollup(group),
      .margin_label = NA_character_,
      .check_margin_label = FALSE
    )
  )
})

test_that("Margin verbs place factor labels first only when requested", {
  data <- data.frame(
    group = ordered(c("a", "b"), levels = c("a", "b")),
    value = 1:2
  )
  operations <- list(
    summary = function() {
      summarize_with_margins(
        data,
        n = dplyr::n(),
        .grouping = rollup(group),
        .margin_label_position = "first"
      )
    },
    expand = function() {
      expand_with_margins(
        data,
        .grouping = rollup(group),
        .margin_label_position = "first"
      )
    },
    nest = function() {
      nest_with_margins(
        data,
        .grouping = rollup(group),
        .margin_label_position = "first"
      )
    },
    nest_by = function() {
      nest_by_with_margins(
        data,
        .grouping = rollup(group),
        .margin_label_position = "first"
      )
    }
  )

  for (operation in operations) {
    result <- operation()
    expect_s3_class(result$group, "ordered")
    expect_identical(levels(result$group), c("Total", "a", "b"))
  }
})

test_that("named Margin labels require exact dimension coverage", {
  data <- data.frame(
    fixed = 1L,
    first = "a",
    second = "b",
    value = 1L
  )
  operation <- function(label) {
    expand_with_margins(
      data,
      .by = fixed,
      .grouping = rollup(first, second),
      .margin_label = label
    )
  }

  expect_error(operation(c(first = "All")), "missing `second`")
  expect_error(
    operation(c(first = "All", second = "All", unknown = "All")),
    "unknown dimension name `unknown`"
  )
  expect_error(
    operation(c(fixed = "All", first = "All", second = "All")),
    "fixed `.by` column `fixed`"
  )
  expect_error(
    operation(stats::setNames(c("All", "All"), c("first", ""))),
    "names must not be empty"
  )
  expect_error(
    operation(stats::setNames(c("All", "All"), c("first", "first"))),
    "names must not be duplicated"
  )
  expect_error(
    operation(stats::setNames(c("All", "All"), c("first", NA_character_))),
    "names must not be missing"
  )
})

test_that("factor collisions include unused levels and remain column-specific", { # nolint: line_length_linter
  data <- data.frame(
    first = factor(c("a", "b"), levels = c("a", "b", "All first")),
    second = factor(c("x", "y"), levels = c("x", "y", "All second")),
    value = 1:2
  )

  error <- expect_error(
    summarize_with_margins(
      data,
      n = dplyr::n(),
      .grouping = rollup(first, second),
      .margin_label = c(first = "All first", second = "New second")
    ),
    "grouping column `first`"
  )
  expect_match(
    deparse1(conditionCall(error)),
    "summarize_with_margins",
    fixed = TRUE
  )

  result <- summarize_with_margins(
    data,
    n = dplyr::n(),
    .grouping = rollup(first, second),
    .margin_label = c(first = "All first", second = "All second"),
    .margin_label_position = "first",
    .check_margin_label = FALSE
  )
  expect_identical(levels(result$first), c("All first", "a", "b"))
  expect_identical(levels(result$second), c("All second", "x", "y"))
})

test_that("dtplyr applies mixed named labels lazily and restores factors", {
  skip_if_backend_absent("dtplyr")
  data <- data.frame(
    first = factor(c("a", "b")),
    second = ordered(c("x", "y")),
    value = 1:2
  )

  query <- summarize_with_margins(
    dtplyr::lazy_dt(data),
    n = dplyr::n(),
    id = grouping_id(first, second),
    .grouping = rollup(first, second),
    .margin_label = c(first = "All first", second = NA_character_)
  )
  expect_s3_class(query, "dtplyr_step")

  result <- dplyr::collect(query)
  expect_s3_class(result$first, "factor")
  expect_s3_class(result$second, "ordered")
  expect_identical(levels(result$first), c("a", "b", "All first"))
  expect_identical(levels(result$second), c("x", "y"))
  expect_true(all(is.na(result$second[result$id == 1L])))
})

test_that("Arrow applies mixed named labels lazily with typed missing values", {
  skip_if_backend_absent("arrow")
  data <- data.frame(
    first = c("a", "b"),
    second = c(1L, 2L),
    value = 1:2
  )

  query <- expand_with_margins(
    arrow::Table$create(data),
    .grouping = rollup(first, second),
    .margin_label = c(first = "All first", second = NA_character_)
  )
  expect_s3_class(query, "arrow_dplyr_query")

  result <- dplyr::collect(query)
  expect_type(result$first, "character")
  expect_type(result$second, "integer")
  expect_true(any(result$first == "All first"))
  expect_true(anyNA(result$second))
})

test_that("portable SQL consumes named per-column labels lazily", {
  skip_if_no_sqlite_simulation()
  remote <- dbplyr::tbl_lazy(
    data.frame(first = "a", second = "b", value = 1L),
    con = dbplyr::simulate_sqlite()
  )

  query <- expand_with_margins(
    remote,
    .grouping = rollup(first, second),
    .margin_label = c(first = "All first", second = "All second")
  )
  sql <- dbplyr::sql_render(query)

  expect_s3_class(query, "tbl_lazy")
  expect_match(sql, "'All first'", fixed = TRUE)
  expect_match(sql, "'All second'", fixed = TRUE)
  expect_match(sql, "UNION ALL", fixed = TRUE)
})

test_that("DuckDB uses typed missing for a missing factor Margin label", {
  skip_if_backend_absent("DBI", "duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(
    con,
    data.frame(group = factor(c("a", "b")), value = 1:2),
    "missing_factor_margin",
    overwrite = TRUE,
    temporary = TRUE
  )

  query <- summarize_with_margins(
    source,
    n = dplyr::n(),
    bit = grouping_bit(group),
    .grouping = rollup(group),
    .margin_label = NA_character_
  )
  expect_s3_class(query, "tbl_lazy")

  result <- dplyr::collect(query)
  expect_s3_class(result$group, "factor")
  expect_false(anyNA(levels(result$group)))
  expect_true(is.na(result$group[result$bit == 1L]))
})

test_that("Margin verbs share scalar, named, NA, and NULL label behavior", {
  data <- data.frame(group = factor(c("a", "b")), value = 1:2)
  labels <- list(
    scalar = "All",
    named = c(group = "All"),
    missing = NA_character_,
    absent = NULL
  )
  operations <- list(
    summary = function(label) {
      summarize_with_margins(
        data,
        n = dplyr::n(),
        .grouping = rollup(group),
        .margin_label = label
      )
    },
    expand = function(label) {
      expand_with_margins(
        data,
        .grouping = rollup(group),
        .margin_label = label
      )
    },
    nest = function(label) {
      nest_with_margins(
        data,
        .grouping = rollup(group),
        .margin_label = label
      )
    },
    nest_by = function(label) {
      nest_by_with_margins(
        data,
        .grouping = rollup(group),
        .margin_label = label
      )
    }
  )

  for (operation in operations) {
    for (label_name in names(labels)) {
      result <- operation(labels[[label_name]])
      expect_s3_class(result$group, "factor")
      if (label_name %in% c("scalar", "named")) {
        expect_true("All" %in% levels(result$group))
        expect_true(any(as.character(result$group) == "All"))
      } else {
        expect_identical(levels(result$group), levels(data$group))
        expect_true(anyNA(result$group))
      }
    }
  }
})

test_that("factor level position is a no-op for typed-missing labels", {
  data <- data.frame(group = factor(c("a", "b")), value = 1:2)

  for (label in list(NA_character_, NULL)) {
    last <- expand_with_margins(
      data,
      .grouping = rollup(group),
      .margin_label = label,
      .margin_label_position = "last"
    )
    first <- expand_with_margins(
      data,
      .grouping = rollup(group),
      .margin_label = label,
      .margin_label_position = "first"
    )

    expect_identical(levels(first$group), levels(last$group))
    expect_identical(is.na(first$group), is.na(last$group))
  }
})

test_that("non-missing labels preserve factor NA levels and missing codes", {
  data <- data.frame(
    group = structure(
      c(1L, 2L, NA_integer_),
      levels = c("x", NA_character_),
      class = "factor"
    ),
    value = 1:3
  )

  result <- summarize_with_margins(
    data,
    n = dplyr::n(),
    bit = grouping_bit(group),
    .grouping = rollup(group),
    .margin_label = "All"
  )
  detail <- result[result$bit == 0L, , drop = FALSE]

  expect_identical(levels(result$group), c("x", NA_character_, "All"))
  expect_true(any(is.na(detail$group)))
  expect_true(any(!is.na(detail$group) & is.na(as.character(detail$group))))
  expect_identical(as.character(result$group[result$bit == 1L]), "All")
})

test_that("collision checks use the displayed value of non-factor columns", {
  data <- data.frame(
    group = as.POSIXct("2020-01-01", tz = "UTC"),
    value = 1L
  )

  error <- expect_error(
    expand_with_margins(
      data,
      .grouping = rollup(group),
      .margin_label = "2020-01-01"
    ),
    "already present"
  )
  expect_match(
    deparse1(conditionCall(error)),
    "expand_with_margins",
    fixed = TRUE
  )
})

test_that("Margin label option errors use the package condition seam", {
  data <- data.frame(
    fixed = "f",
    first = "a",
    second = "x",
    value = 1L
  )
  operation <- function(label) {
    expand_with_margins(
      data,
      .by = fixed,
      .grouping = rollup(first, second),
      .margin_label = label
    )
  }
  cases <- list(
    list(
      label = 1L,
      message = "must be `NULL`, an unnamed character scalar"
    ),
    list(
      label = c("All", "Total"),
      message = "unnamed `\\.margin_label` must be a character vector"
    ),
    list(
      label = stats::setNames(c("All", "All"), c("first", NA_character_)),
      message = "names must not be missing"
    ),
    list(
      label = stats::setNames(c("All", "All"), c("first", "")),
      message = "names must not be empty"
    ),
    list(
      label = stats::setNames(c("All", "All"), c("first", "first")),
      message = "names must not be duplicated"
    ),
    list(
      label = c(fixed = "All", first = "All", second = "All"),
      message = "must not name fixed `\\.by` column `fixed`"
    ),
    list(
      label = c(first = "All", second = "All", unknown = "All"),
      message = "unknown dimension name `unknown`"
    ),
    list(
      label = c(first = "All"),
      message = "must name every Margin dimension; missing `second`"
    )
  )

  for (case in cases) {
    error <- expect_error(operation(case$label), case$message)
    expect_s3_class(error, "marginplyr_error")
    expect_identical(
      rlang::call_name(conditionCall(error)),
      "expand_with_margins"
    )
  }
})
