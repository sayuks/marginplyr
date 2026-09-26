summary_label_check_capture <- new.env(parent = emptyenv())

summary_label_check_collect <- function(con, sql, ...) {
  summary_label_check_capture$n <- summary_label_check_capture$n + 1L
  result <- data.frame(FALSE)
  names(result) <- attr(con, "check_names", exact = TRUE)
  result
}

summary_proxy_capture <- new.env(parent = emptyenv())

summary_proxy_counter_head <- function(x, ...) {
  result <- NextMethod()
  class(result) <- unique(c("margin_summary_proxy_counter", class(result)))
  result
}

summary_proxy_counter_collect <- function(x, ...) {
  summary_proxy_capture$n <- summary_proxy_capture$n + 1L
  NextMethod()
}

register_summary_proxy_methods <- function() {
  registerS3method(
    "head",
    "margin_summary_proxy_counter",
    summary_proxy_counter_head,
    envir = asNamespace("utils")
  )
  registerS3method(
    "collect",
    "margin_summary_proxy_counter",
    summary_proxy_counter_collect,
    envir = asNamespace("dplyr")
  )
}

test_that("invalid summary selections precede lazy label collision queries", {
  registerS3method(
    "db_collect",
    "summary_label_check_connection",
    summary_label_check_collect,
    envir = asNamespace("dbplyr")
  )
  con <- dbplyr::simulate_sqlite()
  class(con) <- append(
    class(con),
    "summary_label_check_connection",
    after = 1L
  )
  attr(con, "check_names") <- "group"
  remote <- dbplyr::tbl_lazy(
    data.frame(group = "x", value = 1),
    con = con
  )
  summary_label_check_capture$n <- 0L

  expect_error(
    summarize_with_margins(
      remote,
      dplyr::across(unknown, sum),
      .grouping = rollup(group),
      .check_margin_label = TRUE
    ),
    "Column `unknown` doesn't exist"
  )

  expect_identical(summary_label_check_capture$n, 0L)
})

test_that("invalid summary selections precede label name coverage errors", {
  data <- data.frame(first = "x", second = "y", value = 1L)

  expect_error(
    summarize_with_margins(
      data,
      dplyr::across(unknown, sum),
      .grouping = rollup(first, second),
      .margin_label = c(first = "All first")
    ),
    "Column `unknown` doesn't exist"
  )
})

test_that("shared lifecycle options use Package conditions", {
  data <- data.frame(group = "x")
  cases <- list(
    check_margin_label = list(
      expr = quote(summarize_with_margins(
        data,
        id = dplyr::cur_group_id(),
        .grouping = rollup(group),
        .check_margin_label = 1
      )),
      message = "`\\.check_margin_label` must be a logical scalar"
    ),
    check_margin_label_null = list(
      expr = quote(summarize_with_margins(
        data,
        id = dplyr::cur_group_id(),
        .grouping = rollup(group),
        .check_margin_label = NULL
      )),
      message = "`\\.check_margin_label` must be a logical scalar"
    ),
    margin_label_position = list(
      expr = quote(summarize_with_margins(
        data,
        n = dplyr::n(),
        .margin_label_position = "middle"
      )),
      message = "`\\.margin_label_position` must be one of"
    ),
    duplicates = list(
      expr = quote(summarize_with_margins(
        data,
        n = dplyr::n(),
        .duplicates = "merge"
      )),
      message = "`\\.duplicates` must be one of"
    )
  )

  for (case in cases) {
    error <- expect_error(eval(case$expr), case$message)
    expect_s3_class(error, "marginplyr_error")
    expect_identical(
      rlang::call_name(conditionCall(error)),
      "summarize_with_margins"
    )
  }
})

test_that("shared lifecycle options preserve user-expression conditions", {
  data <- data.frame(group = "x")
  user_option <- function() {
    rlang::abort(
      "User option evaluation failed.",
      class = "marginplyr_test_user_option_error",
      provenance = "user option"
    )
  }
  baseline <- expect_error(user_option())

  error <- expect_error(
    summarize_with_margins(
      data,
      n = dplyr::n(),
      .duplicates = user_option()
    )
  )

  expect_identical(class(error), class(baseline))
  expect_identical(error$provenance, "user option")
  expect_false(inherits(error, "marginplyr_error"))
})

test_that("summary rejects grouping before typed metadata acquisition", {
  skip_if_suggest_absent("dtplyr")
  register_summary_proxy_methods()
  source <- dtplyr::lazy_dt(data.frame(group = c("x", "y"), value = 1:2))
  class(source) <- c("margin_summary_proxy_counter", class(source))
  summary_proxy_capture$n <- 0L

  error <- expect_error(
    summarize_with_margins(
      source,
      n = dplyr::n(),
      .grouping = rollup(unknown)
    ),
    "Column `unknown` doesn't exist"
  )

  expect_identical(summary_proxy_capture$n, 0L)
  expect_s3_class(error, "vctrs_error_subscript_oob")
  expect_false(inherits(error, "marginplyr_error"))
})

test_that("removed .groups is rejected before typed metadata acquisition", {
  skip_if_suggest_absent("dtplyr")
  register_summary_proxy_methods()
  source <- dtplyr::lazy_dt(data.frame(group = c("x", "y"), value = 1:2))
  class(source) <- c("margin_summary_proxy_counter", class(source))
  summary_proxy_capture$n <- 0L
  summary_options <- list(.groups = "drop")

  expect_error(
    summarize_with_margins(
      source,
      n = dplyr::n(),
      .grouping = rollup(group),
      !!!summary_options
    ),
    "`summarize_with_margins\\(\\)` has no `\\.groups` argument"
  )

  expect_identical(summary_proxy_capture$n, 0L)
})

test_that("dtplyr summary reuses one typed snapshot across selections", {
  skip_if_suggest_absent("dtplyr")
  register_summary_proxy_methods()
  source <- dtplyr::lazy_dt(data.frame(
    group = c("x", "y"),
    code = c(1L, 2L),
    value = c(10, 20)
  ))
  class(source) <- c("margin_summary_proxy_counter", class(source))
  summary_proxy_capture$n <- 0L

  query <- summarize_with_margins(
    source,
    dplyr::across(
      where(is.numeric),
      sum,
      .names = "total_{.col}"
    ),
    .grouping = rollup(where(is.character)),
    .margin_label = NULL
  )

  expect_s3_class(query, "dtplyr_step")
  expect_identical(summary_proxy_capture$n, 1L)
  result <- dplyr::collect(query)
  expect_identical(
    names(result),
    c("group", "total_code", "total_value")
  )
  expect_setequal(result$total_value, c(10, 20, 30))
})

test_that("dtplyr unwraps a `.fns` list of one into the function it holds", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr is the only backend whose `across()` output names are normalized
  # before staging: the `.names` template is expanded into the selection, and
  # a `.fns` list holding one function is unwrapped to that function, since a
  # list would otherwise name the outputs a second time. The rebuild reads
  # that list off the parse rather than out of the argument list it is
  # rewriting (#174), so the unwrapping is asserted here. Compared against the
  # local result, which needs no optional backend.
  data <- data.frame(
    group = c("x", "x", "y"),
    units = c(1, 3, 6),
    revenue = c(2, 4, 8)
  )
  summarize <- function(source) {
    summarize_with_margins(
      source,
      dplyr::across(c(units, revenue), list(sum), .names = "{.col}_total"),
      .grouping = rollup(group),
      .margin_label = NULL
    ) |>
      dplyr::arrange(group)
  }

  expected <- summarize(data)
  query <- summarize(dtplyr::lazy_dt(data))
  expect_s3_class(query, "dtplyr_step")
  expect_equal(
    as.data.frame(dplyr::collect(query)),
    as.data.frame(expected)
  )
  # The template named each output once, rather than the list naming it again.
  expect_named(expected, c("group", "units_total", "revenue_total"))
})

test_that("summary selection errors use the Package condition seam", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  summary_options <- list(.groups = "drop")
  cases <- list(
    removed_groups = list(
      expr = rlang::expr(summarize_with_margins(
        data,
        n = dplyr::n(),
        .grouping = rollup(group),
        !!!summary_options
      )),
      message = "`summarize_with_margins\\(\\)` has no `\\.groups` argument"
    ),
    context_helper = list(
      expr = rlang::expr(summarize_with_margins(
        data,
        id = dplyr::cur_group_id(),
        .grouping = rollup(group)
      )),
      message = "does not support:\ni `cur_group_id\\(\\)`\\."
    ),
    group_overwrite = list(
      expr = rlang::expr(summarize_with_margins(
        data,
        group = sum(value),
        .grouping = rollup(group)
      )),
      message = "cannot overwrite grouping column:\ni `group`\\."
    )
  )

  for (case in cases) {
    error <- expect_error(eval(case$expr), case$message)
    expect_s3_class(error, "marginplyr_error")
    expect_identical(
      rlang::call_name(conditionCall(error)),
      "summarize_with_margins"
    )
  }
})

test_that("summary tidyselect conditions retain their class and cause", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  baseline <- expect_error(
    tidyselect::eval_select(rlang::quo(unknown), data = data["value"])
  )

  error <- expect_error(
    summarize_with_margins(
      data,
      dplyr::across(unknown, sum),
      .grouping = rollup(group)
    )
  )

  expect_identical(class(error), class(baseline))
  expect_false(inherits(error, "marginplyr_error"))
  expect_match(conditionMessage(error), "Column `unknown` doesn't exist")
})

test_that("local selectors see preceding ordinary summaries", {
  data <- data.frame(group = c("a", "b"), value = c(1, 2))
  expected <- dplyr::summarise(
    data,
    total = sum(value),
    dplyr::across(total, ~ .x * 2, .names = "double_{.col}"),
    dplyr::across(
      dplyr::starts_with("tot"), ~ .x * 3, .names = "triple_{.col}"
    ),
    .by = group
  )
  expected <- dplyr::bind_rows(
    expected,
    dplyr::summarise(
      data,
      total = sum(value),
      dplyr::across(total, ~ .x * 2, .names = "double_{.col}"),
      dplyr::across(
        dplyr::starts_with("tot"), ~ .x * 3, .names = "triple_{.col}"
      )
    ) |>
      dplyr::mutate(group = "Total", .before = 1L)
  )

  actual <- summarize_with_margins(
    data,
    total = sum(value),
    dplyr::across(total, ~ .x * 2, .names = "double_{.col}"),
    dplyr::across(
      dplyr::starts_with("tot"), ~ .x * 3, .names = "triple_{.col}"
    ),
    .grouping = rollup(group)
  )
  expect_equal(actual, expected)
  expect_equal(actual$double_total, c(2, 4, 6))
})

test_that("local selections use current types and dplyr output order", {
  data <- data.frame(group = c("a", "b"), value = c(1, 2))
  summaries <- function(input, by = NULL) {
    dplyr::summarise(
      input,
      value = as.character(sum(value)),
      dplyr::across(
        dplyr::where(is.character), ~ paste0(.x, "!"),
        .names = "text_{.col}"
      ),
      selected = paste(names(dplyr::pick(dplyr::where(is.character))),
                       collapse = ","),
      all_seen = paste(names(dplyr::pick(dplyr::everything())),
                       collapse = ","),
      .by = {{ by }}
    )
  }
  expected <- dplyr::bind_rows(
    summaries(data, group),
    dplyr::mutate(
      summaries(dplyr::select(data, -group)),
      group = "Total",
      .before = 1L
    )
  )
  actual <- summarize_with_margins(
    data,
    value = as.character(sum(value)),
    dplyr::across(
      dplyr::where(is.character), ~ paste0(.x, "!"),
      .names = "text_{.col}"
    ),
    selected = paste(names(dplyr::pick(dplyr::where(is.character))),
                     collapse = ","),
    all_seen = paste(names(dplyr::pick(dplyr::everything())),
                     collapse = ","),
    .grouping = rollup(group)
  )
  expect_equal(actual, expected)
  expect_identical(actual$selected, rep("value,text_value", 3L))
})

test_that("local selection rewrites preserve frame expansion and packing", {
  data <- data.frame(group = c("a", "b"), value = c(1, 2))
  range_frame <- function(x) {
    data.frame(lo = min(x), hi = max(x))
  }
  actual <- summarize_with_margins(
    data,
    range_frame(dplyr::pick(value)),
    packed = range_frame(dplyr::pick(value)),
    .grouping = rollup(group)
  )
  expect_identical(names(actual), c("group", "lo", "hi", "packed"))
  expect_equal(actual$lo, c(1, 2, 1))
  expect_equal(actual$hi, c(1, 2, 2))
  expect_s3_class(actual$packed, "data.frame")
  expect_equal(actual$packed$lo, actual$lo)
})

test_that("data.frame controls do not collide with an identifier", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2)
  controls <- c(
    "row.names", "check.rows", "check.names", "fix.empty.names",
    "stringsAsFactors"
  )

  for (id in controls) {
    actual <- summarize_with_margins(
      data,
      base::data.frame(
        x = sum(x), row.names = NULL, check.rows = FALSE,
        check.names = FALSE, fix.empty.names = TRUE,
        stringsAsFactors = FALSE
      ),
      .grouping = grouping_set(g), .id = id
    )
    expect_identical(names(actual), c("g", id, "x"))
    expect_identical(actual[[id]], c(1L, 1L))
    expect_identical(actual$x, c(1L, 2L))
  }
})

test_that("a grouping column may share a data.frame control name", {
  data <- tibble::tibble(check.names = c("a", "b"), x = 1:2)
  actual <- summarize_with_margins(
    data, data.frame(x = sum(x), check.names = FALSE),
    .grouping = grouping_set(check.names)
  )

  expect_identical(names(actual), c("check.names", "x"))
  expect_identical(actual$check.names, c("a", "b"))
  expect_identical(actual$x, c(1L, 2L))

  error <- expect_error(
    summarize_with_margins(
      data,
      data.frame(
        setNames(list(sum(x)), "check.names"), check.names = FALSE
      ),
      .grouping = grouping_set(check.names)
    ),
    "cannot overwrite grouping column"
  )
  expect_s3_class(error, "marginplyr_error")
})

test_that("tibble controls are not outputs but ordinary names are", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2)
  for (id in c(".rows", ".name_repair")) {
    actual <- summarize_with_margins(
      data,
      tibble::tibble(x = sum(x), .rows = 1L, .name_repair = "minimal"),
      .grouping = grouping_set(g), .id = id
    )
    expect_identical(names(actual), c("g", id, "x"))
    expect_identical(actual[[id]], c(1L, 1L))
  }

  error <- expect_error(
    summarize_with_margins(
      data, tibble::tibble(check.names = sum(x)),
      .grouping = grouping_set(g), .id = "check.names"
    ),
    "`\\.id` \\(`check.names`\\) conflicts with a summary output"
  )
  expect_s3_class(error, "marginplyr_error")

  error <- expect_error(
    suppressWarnings(summarize_with_margins(
      data, tibble::data_frame(.name_repair = sum(x)),
      .grouping = grouping_set(g), .id = ".name_repair"
    )),
    "`\\.id` \\(`\\.name_repair`\\) conflicts with a summary output"
  )
  expect_s3_class(error, "marginplyr_error")

  error <- expect_error(
    summarize_with_margins(
      data, data.frame(x = sum(x), check.n = FALSE),
      .grouping = grouping_set(g), .id = "check.n"
    ),
    "`\\.id` \\(`check.n`\\) conflicts with a summary output"
  )
  expect_s3_class(error, "marginplyr_error")
})

test_that("named frame summaries stay packed with constructor controls", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2)
  actual <- summarize_with_margins(
    data, packed = data.frame(x = sum(x), check.names = FALSE),
    .grouping = grouping_set(g), .id = "check.names"
  )

  expect_identical(names(actual), c("g", "check.names", "packed"))
  expect_s3_class(actual$packed, "data.frame")
  expect_identical(actual$packed$x, c(1L, 2L))
})

test_that("frame argument names are checked after their output expands", {
  data <- tibble::tibble(g = "a", x = 1L)
  compare <- function(expr, id = "set") {
    expected <- rlang::eval_tidy(
      rlang::expr(dplyr::summarise(data, !!expr, .by = g))
    )
    actual <- rlang::eval_tidy(rlang::expr(summarize_with_margins(
      data, !!expr, .grouping = grouping_set(g), .id = !!id
    )))
    expect_identical(names(actual), c("g", id, names(expected)[-1L]))
    expect_identical(actual[[id]], 1L)
    expect_identical(actual[names(expected)], expected)
  }

  compare(quote(tibble::tibble(g = NULL, total = sum(x))))
  compare(quote(data.frame(g = data.frame(total = sum(x)))))
  compare(quote(data.frame(g = matrix(
    c(sum(x), sum(x) + 1L), nrow = 1L,
    dimnames = list(NULL, c("lo", "hi"))
  ))))
  compare(quote(tibble::tibble(
    g = sum(x), .name_repair = function(nm) paste0(nm, "_out")
  )))
  compare(quote(data.frame("a b" = sum(x))), id = "a b")
})

test_that("frame collisions use expanded names, and named frames stay packed", {
  data <- tibble::tibble(g = "a", x = 1L)
  group_error <- expect_error(summarize_with_margins(
    data, data.frame(foo = data.frame(g = sum(x))),
    .grouping = grouping_set(g)
  ), "cannot overwrite grouping column.*`g`")
  id_error <- expect_error(summarize_with_margins(
    data, data.frame(foo = data.frame(set = sum(x))),
    .grouping = grouping_set(g), .id = "set"
  ), "`.id`.*`set`.*conflicts with a summary output")
  expect_s3_class(group_error, "marginplyr_error")
  expect_s3_class(id_error, "marginplyr_error")

  expected <- dplyr::summarise(
    data, packed = data.frame(g = sum(x)), .by = g
  )
  actual <- summarize_with_margins(
    data, packed = data.frame(g = sum(x)),
    .grouping = grouping_set(g), .id = "set"
  )
  expect_identical(names(actual), c("g", "set", "packed"))
  expect_identical(actual[c("g", "packed")], expected)
})

test_that("a caller-bound frame function determines its summary output", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2)
  tibble <- function(...) data.frame(z = 1L)

  actual <- summarize_with_margins(
    data, tibble(foo = x),
    .grouping = grouping_set(g), .id = "foo"
  )

  expect_identical(names(actual), c("g", "foo", "z"))
  expect_identical(actual$foo, c(1L, 1L))
  expect_identical(actual$z, c(1L, 1L))
})

test_that("other unqualified frame spellings honor caller bindings", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2)
  # This binding must match the base constructor's name to test its lookup.
  data.frame <- function(...) { # nolint: object_name_linter.
    base::data.frame(z = 1L)
  }
  data_frame <- function(...) base::data.frame(z = 1L)

  base_spelling <- summarize_with_margins(
    data, data.frame(foo = x),
    .grouping = grouping_set(g), .id = "foo"
  )
  tibble_spelling <- summarize_with_margins(
    data, data_frame(foo = x),
    .grouping = grouping_set(g), .id = "foo"
  )

  expect_identical(names(base_spelling), c("g", "foo", "z"))
  expect_identical(names(tibble_spelling), c("g", "foo", "z"))
})

test_that("an unbound frame spelling keeps R's function lookup error", {
  caller <- new.env(parent = baseenv())
  caller$data <- base::data.frame(g = c("a", "b"), x = 1:2)

  error <- expect_error(eval(quote(marginplyr::summarize_with_margins(
    data, tibble(foo = x),
    .grouping = marginplyr::grouping_set(g), .id = "foo"
  )), envir = caller), "could not find function")
  expect_false(inherits(error, "marginplyr_error"))
})

test_that("caller-bound frame outputs still protect identifiers and groups", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2)
  tibble <- function(...) data.frame(z = 1L)

  id_error <- expect_error(summarize_with_margins(
    data, tibble(foo = x),
    .grouping = grouping_set(g), .id = "z"
  ), "`.id`.*`z`.*conflicts with a summary output")
  expect_s3_class(id_error, "marginplyr_error")

  group_data <- tibble::tibble(z = c("a", "b"), x = 1:2)
  group_error <- expect_error(summarize_with_margins(
    group_data, tibble(foo = x),
    .grouping = grouping_set(z)
  ), "cannot overwrite grouping column.*`z`")
  expect_s3_class(group_error, "marginplyr_error")
})

test_that("frame prediction does not force a delayed constructor", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2)
  counter <- new.env(parent = emptyenv())
  counter$binding <- 0L
  counter$body <- 0L
  delayedAssign("tibble", {
    counter$binding <- counter$binding + 1L
    function(...) {
      counter$body <- counter$body + 1L
      data.frame(z = 1L)
    }
  })

  actual <- summarize_with_margins(
    data, tibble(foo = x),
    .grouping = grouping_set(g), .id = "foo"
  )
  expect_identical(counter$binding, 1L)
  expect_identical(counter$body, 2L)
  expect_identical(names(actual), c("g", "foo", "z"))
})

test_that("frame prediction does not invoke an active constructor binding", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2)
  counter <- new.env(parent = emptyenv())
  counter$binding <- 0L
  counter$body <- 0L
  actual <- local({
    makeActiveBinding("tibble", function() {
      counter$binding <- counter$binding + 1L
      function(...) {
        counter$body <- counter$body + 1L
        data.frame(z = 1L)
      }
    }, environment())
    summarize_with_margins(
      data, tibble(foo = x),
      .grouping = grouping_set(g), .id = "foo"
    )
  })

  expect_identical(counter$binding, 2L)
  expect_identical(counter$body, 2L)
  expect_identical(names(actual), c("g", "foo", "z"))
})

test_that("an unnamed rewritten frame is available to later summaries", {
  data <- tibble::tibble(
    group = c("a", "a", "b"),
    value = 1:3,
    total = 100
  )
  total_frame <- function(columns) {
    data.frame(total = sum(columns[[1L]]))
  }
  detail <- dplyr::summarise(
    data,
    total_frame(dplyr::pick(value)),
    later = sum(total),
    .by = group
  )

  cases <- list(
    list(grouping = grouping_set(group), totals = c(3L, 3L)),
    list(grouping = rollup(group), totals = c(3L, 3L, 6L))
  )
  for (case in cases) {
    actual <- summarize_with_margins(
      data,
      total_frame(dplyr::pick(value)),
      later = sum(total),
      .grouping = case$grouping
    )
    expect_identical(actual$total[seq_len(2L)], detail$total)
    expect_identical(actual$later[seq_len(2L)], detail$later)
    expect_identical(actual$total, case$totals)
    expect_identical(actual$later, case$totals)
  }
})

test_that("later summaries see a frame without an input total", {
  data <- tibble::tibble(group = c("a", "a", "b"), value = 1:3)
  text_total <- function(columns) {
    data.frame(total = as.character(sum(columns[[1L]])))
  }

  cases <- list(
    list(grouping = grouping_set(group), totals = c(3L, 3L)),
    list(grouping = rollup(group), totals = c(3L, 3L, 6L))
  )
  for (case in cases) {
    actual <- summarize_with_margins(
      data,
      text_total(dplyr::pick(value)),
      later = as.integer(total),
      dplyr::across(
        dplyr::where(is.character), ~ paste0(.x, "!"),
        .names = "text_{.col}"
      ),
      selected = paste(
        names(dplyr::pick(dplyr::where(is.character))),
        collapse = ","
      ),
      .grouping = case$grouping
    )
    expected <- case$totals
    expect_identical(actual$later, expected)
    expect_identical(actual$total, as.character(expected))
    expect_identical(actual$text_total, paste0(expected, "!"))
    expect_identical(
      actual$selected,
      rep("total,text_total", length(expected))
    )
  }
})

test_that("a later summary can overwrite an expanded frame column", {
  data <- tibble::tibble(group = c("a", "a", "b"), value = 1:3)
  total_frame <- function(columns) {
    data.frame(total = sum(columns[[1L]]))
  }

  cases <- list(
    list(grouping = grouping_set(group), rows = 2L),
    list(grouping = rollup(group), rows = 3L)
  )
  for (case in cases) {
    actual <- summarize_with_margins(
      data,
      total_frame(dplyr::pick(value)),
      total = 999L,
      .grouping = case$grouping
    )
    expect_identical(actual$total, rep(999L, case$rows))
  }
})

test_that("an unnamed rewritten NULL summary remains absent", {
  data <- tibble::tibble(group = c("a", "b"), value = 1:2)
  empty <- function(columns) NULL
  actual <- summarize_with_margins(
    data,
    empty(dplyr::pick(value)),
    later = sum(value),
    .grouping = grouping_set(group)
  )
  expect_identical(names(actual), c("group", "later"))
  expect_identical(actual$later, 1:2)
})

test_that("local selection planning does not execute caller summaries", {
  data <- data.frame(group = c("a", "b"), value = c(1, 2))
  calls <- 0L
  counted_sum <- function(x) {
    calls <<- calls + 1L
    sum(x)
  }
  actual <- summarize_with_margins(
    data,
    total = counted_sum(value),
    dplyr::across(total, ~ .x * 2, .names = "double_{.col}"),
    .grouping = rollup(group)
  )
  expect_equal(actual$double_total, c(2, 4, 6))
  expect_identical(calls, 3L)

  predicates <- 0L
  numeric_probe <- function(x) {
    predicates <<- predicates + 1L
    is.numeric(x)
  }
  summarize_with_margins(
    data,
    total = counted_sum(value),
    dplyr::across(dplyr::where(numeric_probe), sum,
                  .names = "again_{.col}"),
    .grouping = rollup(group)
  )
  expect_identical(calls, 6L)
  expect_identical(predicates, 4L)

  selectors <- 0L
  selected_name <- function() {
    selectors <<- selectors + 1L
    "total"
  }
  summarize_with_margins(
    data,
    total = sum(value),
    dplyr::across(dplyr::all_of(selected_name()), identity),
    .grouping = rollup(group)
  )
  expect_identical(selectors, 2L)
})

test_that("local across defaults to current ordinary summary columns", {
  data <- data.frame(group = c("a", "b"), value = 1:2)
  actual <- summarize_with_margins(
    data,
    total = sum(value),
    dplyr::across(.fns = sum, .names = "copy_{.col}"),
    .grouping = rollup(group)
  )
  expect_equal(actual$copy_value, c(1, 2, 3))
  expect_equal(actual$copy_total, actual$total)
})

test_that("selections after shares run caller expressions only in branches", {
  data <- data.frame(group = c("a", "b"), value = c(1, 2))
  name_calls <- 0L
  selected_name <- function() {
    name_calls <<- name_calls + 1L
    "again_{.col}"
  }
  predicate_calls <- 0L
  numeric_probe <- function(x) {
    predicate_calls <<- predicate_calls + 1L
    is.numeric(x)
  }
  actual <- summarize_with_margins(
    data,
    total = sum(value),
    share = share_of_total(total),
    dplyr::across(dplyr::where(numeric_probe), sum,
                  .names = selected_name()),
    .grouping = rollup(group)
  )
  expect_identical(name_calls, 2L)
  expect_identical(predicate_calls, 4L)
  expect_equal(actual$again_total, c(1, 2, 3))
})

test_that("local selectors exclude the complete Grouping plan", {
  data <- data.frame(
    region = c("a", "b"),
    store = c("x", "y"),
    value = c(1, 2)
  )
  actual <- summarize_with_margins(
    data,
    total = sum(value),
    selected = paste(names(dplyr::pick(dplyr::everything())),
                     collapse = ","),
    dplyr::across(dplyr::everything(), length,
                  .names = "again_{.col}"),
    .grouping = rollup(region, store)
  )
  expect_identical(actual$selected, rep("value,total", nrow(actual)))
  expect_identical(
    names(actual),
    c("region", "store", "total", "selected", "again_value",
      "again_total", "again_selected")
  )
})

test_that("local if_any and if_all select preceding outputs", {
  data <- data.frame(group = c("a", "b"), value = c(1, 2))
  actual <- summarize_with_margins(
    data,
    total = sum(value),
    positive = dplyr::if_all(total, ~ .x > 0),
    large = dplyr::if_any(dplyr::starts_with("tot"), ~ .x > 1),
    .grouping = rollup(group)
  )
  expect_identical(actual$positive, rep(TRUE, 3L))
  expect_identical(actual$large, c(FALSE, TRUE, TRUE))
})

test_that("ordinary selections still cannot read preceding shares", {
  data <- data.frame(group = c("a", "b"), value = c(1, 2))
  error <- expect_error(summarize_with_margins(
    data,
    total = sum(value),
    share = share_of_total(total),
    dplyr::across(share, identity),
    .grouping = rollup(group)
  ))
  expect_s3_class(error, "marginplyr_error")
  expect_match(conditionMessage(error), "share")

  actual <- summarize_with_margins(
    data,
    total = sum(value),
    share = share_of_total(total),
    selected = paste(names(dplyr::pick(dplyr::everything())),
                     collapse = ","),
    .grouping = rollup(group)
  )
  expect_identical(actual$selected, rep("value,total", 3L))
})

test_that("an unnamed pick after a share cannot select that share", {
  data <- data.frame(group = c("a", "b"), value = 1:2)
  error <- expect_error(summarize_with_margins(
    data,
    total = sum(value),
    share = share_of_total(total),
    dplyr::pick(share),
    .grouping = rollup(group)
  ))
  expect_s3_class(error, "marginplyr_error")
  expect_match(conditionMessage(error), "earlier Total share")
})

test_that("a later share name does not hide an input from earlier selections", {
  data <- data.frame(group = c("a", "b"), share = c(10, 20), value = 1:2)
  actual <- summarize_with_margins(
    data,
    total = sum(value),
    selected = paste(names(dplyr::pick(dplyr::everything())), collapse = ","),
    share = share_of_total(total),
    .grouping = rollup(group)
  )
  expect_identical(actual$selected, rep("share,value,total", 3L))
})

test_that("lazy summary selections keep their backend limitation", {
  data <- data.frame(group = c("a", "b"), value = c(1, 2))
  remote <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_dbi())
  expect_error(summarize_with_margins(
    remote,
    total = sum(value),
    dplyr::across(total, ~ .x * 2),
    .grouping = rollup(group)
  ), "Column `total` doesn't exist")
})

# A backend whose selection proxy is the lazy table itself carries no column
# types, so tidyselect refuses a predicate rather than answering it. The
# refusal stands -- reading the types would be a query nobody asked for
# (ADR 0020) -- but the caller is owed the argument they wrote and the verb
# they wrote it in (#453).
test_that("a summary predicate is refused with the argument and the verb", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  remote <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_dbi())

  error <- expect_error(summarize_with_margins(
    remote,
    dplyr::across(dplyr::where(is.numeric), sum),
    .grouping = rollup(group)
  ))
  expect_s3_class(error, "marginplyr_error")
  # Matched rather than compared whole, because the rendered message ends with
  # the cause, whose line names the internal frame tidyselect refused in.
  expect_match(
    conditionMessage(error),
    paste0(
      "Can't select with a predicate in ",
      "`dplyr::across(dplyr::where(is.numeric), sum)`.\n",
      "i This input's backend doesn't report column types without a query, ",
      "and marginplyr sends none you didn't ask for.\n",
      "i Select the columns by name, or collect the input first.\n",
      "i A Margin operation on collected data defaults `.check_margin_label` ",
      "from `FALSE` to `TRUE`; set it explicitly."
    ),
    fixed = TRUE
  )
  # The verb the caller wrote is what the refusal blames, and tidyselect's own
  # diagnostic is still reachable as the cause.
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "summarize_with_margins"
  )
  expect_s3_class(error$parent, "tidyselect_error_predicates_unsupported")

  # A proxy that carries types answers the predicate, as it did before.
  expect_named(
    summarize_with_margins(
      data,
      dplyr::across(dplyr::where(is.numeric), sum),
      .grouping = rollup(group)
    ),
    c("group", "value")
  )
})
