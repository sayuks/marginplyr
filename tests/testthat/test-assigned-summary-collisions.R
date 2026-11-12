test_that("an expanded summary may share its assigned name with an id", {
  data <- data.frame(g = c("b", "a", "a"), x = 1:3)
  frame <- function(columns) data.frame(out = sum(columns[[1L]]))
  result <- summarize_with_margins(
    data, frame(dplyr::pick(x)), later = out * 2L,
    .grouping = rollup(g), .id = "frame(dplyr::pick(x))"
  )
  expect_identical(names(result),
                   c("g", "frame(dplyr::pick(x))", "out", "later"))
  expect_identical(result$g, c("b", "a", "Total"))
  expect_identical(result[["frame(dplyr::pick(x))"]], c(1L, 1L, 2L))
  expect_identical(result$out, c(1L, 5L, 6L))
  expect_identical(result$later, c(2L, 10L, 12L))
})

check_assigned_public_names <- function(convert) {
  output <- 'frame(dplyr::pick("x"))'
  for (role in c("fixed", "dimension", "id")) {
    for (sort in c("none", "first", "last")) {
      data <- convert(data.frame(g = c("b", "a", "a"), x = 1:3))
      if (role != "id") data[[output]] <- data$g
      counter <- new.env(parent = emptyenv())
      counter$calls <- 0L
      frame <- function(columns) {
        counter$calls <- counter$calls + 1L
        data.frame(out = sum(columns[[1L]]))
      }
      result <- switch(role,
        fixed = summarize_with_margins(
          data, frame(dplyr::pick("x")), later = .data$out * 2L,
          .by = dplyr::all_of(output), .sort = sort
        ),
        dimension = summarize_with_margins(
          data, frame(dplyr::pick("x")), later = .data$out * 2L,
          .grouping = rollup(dplyr::all_of(output)), .sort = sort
        ),
        id = summarize_with_margins(
          data, frame(dplyr::pick("x")), later = .data$out * 2L,
          .grouping = rollup("g"), .id = output, .sort = sort
        )
      )
      expected <- if (role == "fixed") {
        if (sort == "none") c(1L, 5L) else c(5L, 1L)
      } else {
        switch(sort, none = c(1L, 5L, 6L), first = c(6L, 5L, 1L),
               last = c(5L, 1L, 6L))
      }
      expected_names <- if (role == "id") {
        c("g", output, "out", "later")
      } else {
        c(output, "out", "later")
      }
      expect_identical(names(result), expected_names)
      expected_keys <- if (role == "fixed") {
        if (sort == "none") c("b", "a") else c("a", "b")
      } else {
        switch(sort, none = c("b", "a", "Total"),
               first = c("Total", "a", "b"), last = c("a", "b", "Total"))
      }
      key <- if (role == "id") "g" else output
      expect_identical(result[[key]], expected_keys)
      if (role == "id") {
        expect_identical(result[[output]],
                         if (sort == "first") c(2L, 1L, 1L) else c(1L, 1L, 2L))
      }
      expect_identical(result$out, expected)
      expect_identical(result$later, expected * 2L)
      expect_identical(counter$calls, if (role == "fixed") 2L else 3L)
    }
  }
}

test_that("plain frames check public names after unnamed expansion", {
  check_assigned_public_names(identity)
})

test_that("tibbles check public names after unnamed expansion", {
  check_assigned_public_names(tibble::as_tibble)
})

test_that("eager tables check public names after unnamed expansion", {
  skip_if_suggest_absent("data.table")
  check_assigned_public_names(data.table::as.data.table)
})

test_that("a retained Assigned summary name still collides with public names", {
  output <- "frame(dplyr::pick(x))"
  data <- data.frame(g = c("b", "a", "a"), x = 1:3)
  data[[output]] <- data$g
  frame <- function(columns) sum(columns[[1L]])
  expect_error(
    summarize_with_margins(
      data, frame(dplyr::pick(x)), .by = dplyr::all_of(output)
    ),
    "cannot overwrite grouping column", class = "marginplyr_error"
  )
  expect_error(
    summarize_with_margins(
      data, frame(dplyr::pick(x)),
      .grouping = rollup(dplyr::all_of(output))
    ),
    "cannot overwrite grouping column", class = "marginplyr_error"
  )
  data[[output]] <- NULL
  expect_error(
    summarize_with_margins(data, frame(dplyr::pick(x)), .id = output),
    "conflicts with a summary output", class = "marginplyr_error"
  )
})

test_that("actual expanded names still collide with public names", {
  output <- "frame(dplyr::pick(x))"
  data <- data.frame(g = c("b", "a", "a"), x = 1:3)
  data[[output]] <- data$g
  frame <- function(columns) {
    stats::setNames(data.frame(sum(columns[[1L]])), output)
  }
  expect_error(
    summarize_with_margins(
      data, frame(dplyr::pick(x)), .by = dplyr::all_of(output)
    ),
    "cannot overwrite grouping column", class = "marginplyr_error"
  )
  expect_error(
    summarize_with_margins(
      data, frame(dplyr::pick(x)),
      .grouping = rollup(dplyr::all_of(output))
    ),
    "cannot overwrite grouping column", class = "marginplyr_error"
  )
  data[[output]] <- NULL
  expect_error(
    summarize_with_margins(data, frame(dplyr::pick(x)), .id = output),
    "conflicts with a summary output", class = "marginplyr_error"
  )
})

check_unnamed_frame_fields <- function(convert) {
  data <- convert(data.frame(g = c("a", "b"), x = 1:2))
  grouping_col <- rlang::sym("g")
  frame <- function(x, bit) {
    if (bit == 0L) data.frame(detail = sum(x)) else data.frame(total = sum(x))
  }
  error <- expect_error(
    summarize_with_margins(
      data, frame(.data$x, grouping_bit(!!grouping_col)),
      .grouping = rollup("g")
    ),
    "consistent field names", class = "marginplyr_error"
  )
  expect_identical(rlang::call_name(conditionCall(error)),
                   "summarize_with_margins")
  expect_match(conditionMessage(error), "`detail`", fixed = TRUE)
  expect_match(conditionMessage(error), "`total`", fixed = TRUE)
  expect_match(conditionMessage(error), "give the summary a name", fixed = TRUE)

  expanded <- summarize_with_margins(
    data, data.frame(value = sum(.data$x)), .grouping = rollup("g")
  )
  expect_identical(names(expanded), c("g", "value"))
  expect_identical(expanded$value, c(1L, 2L, 3L))

  packed <- summarize_with_margins(
    data, out = frame(.data$x, grouping_bit(!!grouping_col)),
    .grouping = rollup("g")
  )
  expect_identical(names(packed), c("g", "out"))
  expect_s3_class(packed$out, "data.frame")
  expect_identical(packed$out$detail, c(1L, 2L, NA_integer_))
  expect_identical(packed$out$total, c(NA_integer_, NA_integer_, 3L))
}

test_that("plain frames diagnose incompatible unnamed summary fields", {
  check_unnamed_frame_fields(identity)
})

test_that("tibbles diagnose incompatible unnamed summary fields", {
  check_unnamed_frame_fields(tibble::as_tibble)
})

test_that("eager tables diagnose incompatible unnamed summary fields", {
  skip_if_suggest_absent("data.table")
  check_unnamed_frame_fields(data.table::as.data.table)
})

test_that("runtime across unpacking diagnoses incompatible expanded fields", {
  data <- data.frame(g = c("a", "b"), x = 1:2, ..marginplyr_name_rules_1 = 3:4)
  frame <- function(x, bit) {
    if (bit == 0L) data.frame(detail = sum(x)) else data.frame(total = sum(x))
  }
  unpack <- TRUE
  error <- expect_error(
    summarize_with_margins(
      data, dplyr::across(x, ~ frame(.x, grouping_bit(g)), .unpack = unpack),
      .grouping = rollup(g)
    ),
    "consistent field names", class = "marginplyr_error"
  )
  expect_match(conditionMessage(error), "`x_detail`", fixed = TRUE)
  expect_match(conditionMessage(error), "`x_total`", fixed = TRUE)
})

test_that("unnamed summary fields align by name across grouping sets", {
  data <- data.frame(g = c("a", "b"), x = 1:2)
  frame <- function(x) {
    value <- data.frame(value = sum(x), count = length(x))
    if (length(x) == 1L) value else value[c("count", "value")]
  }
  result <- summarize_with_margins(data, frame(x), .grouping = rollup(g))
  expect_identical(result$value, c(1L, 2L, 3L))
  expect_identical(result$count, c(1L, 1L, 2L))
})

check_summary_presence <- function(convert) {
  data <- convert(data.frame(g = c("a", "a", "b"), v = 1:3))
  key <- rlang::sym("g")
  expect_absent <- function(object, summary) {
    error <- expect_error(object, class = "marginplyr_error")
    expect_identical(rlang::call_name(conditionCall(error)),
                     "summarize_with_margins")
    message <- cli::ansi_strip(conditionMessage(error))
    expect_match(message, paste0("`", summary, "`"), fixed = TRUE)
    expect_match(message, "every grouping set", fixed = TRUE)
    expect_match(message, "in place of `NULL`", fixed = TRUE)
    expect_no_match(message, "branch", fixed = TRUE)
  }

  expect_absent(
    summarize_with_margins(
      data, val = if (grouping_bit(!!key) == 1L) NULL else sum(.data$v),
      .grouping = rollup("g")
    ),
    "val"
  )
  expect_absent(
    summarize_with_margins(
      data,
      n = dplyr::n(),
      val = if (grouping_bit(!!key) == 0L) NULL else sum(.data$v),
      .grouping = rollup("g")
    ),
    "val"
  )
  expect_absent(
    summarize_with_margins(
      data, if (grouping_bit(!!key) == 1L) NULL else sum(.data$v),
      .grouping = rollup("g")
    ),
    "if (grouping_bit(g) == 1L) NULL else sum(.data$v)"
  )
  expect_absent(
    summarize_with_margins(
      data,
      dplyr::across("v", ~ if (grouping_bit(!!key) == 1L) NULL else sum(.x)),
      .grouping = rollup("g")
    ),
    "v"
  )

  removed <- summarize_with_margins(
    data, n = dplyr::n(), val = NULL, .grouping = rollup("g")
  )
  expect_identical(names(removed), c("g", "n"))
  expect_identical(removed$n, c(2L, 1L, 3L))
}

test_that("plain frames diagnose a summary absent from some grouping sets", {
  check_summary_presence(identity)
})

test_that("tibbles diagnose a summary absent from some grouping sets", {
  check_summary_presence(tibble::as_tibble)
})

test_that("eager tables diagnose a summary absent from some grouping sets", {
  skip_if_suggest_absent("data.table")
  check_summary_presence(data.table::as.data.table)
})

test_that("an unnamed frame against NULL keeps the frame field diagnostic", {
  data <- data.frame(g = c("a", "a", "b"), v = 1:3)
  key <- rlang::sym("g")
  error <- expect_error(
    summarize_with_margins(
      data,
      if (grouping_bit(!!key) == 1L) NULL else data.frame(a = sum(.data$v)),
      .grouping = rollup("g")
    ),
    "consistent field names", class = "marginplyr_error"
  )
  expect_match(conditionMessage(error), "`a`", fixed = TRUE)
})

test_that("a later summary evaluation error keeps its original cause", {
  data <- data.frame(g = c("a", "b"), h = c("c", "d"), x = 1:2)
  frame <- function(x, id) {
    if (id == 0L) return(data.frame(detail = sum(x)))
    if (id == 1L) return(data.frame(subtotal = sum(x)))
    rlang::abort("Caller summary failed.", class = "caller_summary_error")
  }
  error <- expect_error(
    summarize_with_margins(
      data, frame(x, grouping_id(g, h)), .grouping = rollup(g, h)
    ),
    "Caller summary failed.", fixed = TRUE
  )
  expect_s3_class(error$parent, "caller_summary_error")
})

test_that("unnamed frames check their stored names against public names", {
  data <- tibble::tibble(g = c("x", "y"), x = c(1, 3))
  stored <- "out"
  reported <- "g"
  rlang::local_bindings(
    names.margin_reported_summary = function(x) {
      if (nrow(x) > 0L) reported else attr(x, "names", exact = TRUE)
    },
    .env = asNamespace("base")$.__S3MethodsTable__.
  )
  frame <- function(x) {
    vctrs::new_data_frame(
      stats::setNames(list(sum(x)), stored), n = 1L,
      class = "margin_reported_summary"
    )
  }
  expected <- tibble::tibble(g = c("x", "y"), out = c(1, 3))
  expect_identical(dplyr::summarise(data, frame(x), .by = g), expected)
  expect_identical(summarize_with_margins(data, frame(x), .by = g), expected)
  expect_identical(
    summarize_with_margins(
      data, frame(dplyr::pick(x)$x), .grouping = grouping_set(g)
    ),
    expected
  )
  reported <- "set"
  expect_identical(
    summarize_with_margins(data, frame(x), .id = "set"),
    tibble::tibble(set = 1L, out = 4)
  )

  reported <- "out"
  stored <- "g"
  expect_error(
    summarize_with_margins(data, frame(x), .by = g),
    "cannot overwrite grouping column.*`g`", class = "marginplyr_error"
  )
  expect_error(
    summarize_with_margins(
      data, frame(dplyr::pick(x)$x), .grouping = grouping_set(g)
    ),
    "cannot overwrite grouping column.*`g`", class = "marginplyr_error"
  )
  stored <- "set"
  expect_error(
    summarize_with_margins(data, frame(x), .id = "set"),
    "`set`.*conflicts with a summary output", class = "marginplyr_error"
  )
})
