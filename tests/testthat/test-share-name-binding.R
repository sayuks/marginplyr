# On a local frame a share's own name is bound from its dot position on, so a
# later summary that reads it through a callee the dependency walk does not
# know gets the share's marker and not an input column of the same name
# (#1845, ADR 0010).

# `z` and any warning text of the later summary `text`, over an input whose
# `sh` column is `column`, or "error" when the call ends in a condition.
later_share_read <- function(text, column) {
  # tibble_() and its tidyr twins are deprecated; their warning is not the
  # read under test.
  rlang::local_options(lifecycle_verbosity = "quiet")
  data <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4), sh = column)
  call <- str2lang(paste0(
    "summarize_with_margins(data, s = sum(v), sh = share_of_total(s), ",
    "z = ", text, ", .grouping = rollup(g))"
  ))
  warnings <- character()
  tryCatch(
    withCallingHandlers(
      list(z = eval(call, list(data = data))$z, warnings = warnings),
      warning = function(cnd) {
        warnings <<- c(warnings, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(cnd) "error"
  )
}

# Each read ends in a condition or in a value the input column does not move.
expect_unmoved_by_input_column <- function(texts) {
  for (text in texts) {
    first <- later_share_read(text, c(100, 200, 300))
    second <- later_share_read(text, c(5, 6, 7))
    expect_identical(first, second, label = text)
  }
}

test_that("an evaluator the walk does not know cannot read the input column", {
  expect_unmoved_by_input_column(c(
    "glue::identity_transformer('sum(sh)')",
    "tibble::tibble_('sum(sh)')[[1]]",
    "cli::format_error('{sh}')",
    "cli::format_warning('{sh}')",
    "cli::cli_warn('{sh}')"
  ))
})

test_that("tidyr's evaluators cannot read the input column", {
  skip_if_suggest_absent("tidyr")
  expect_unmoved_by_input_column(c(
    "tidyr::crossing_('sum(sh)')[[1]]",
    "tidyr::nesting_('sum(sh)')[[1]]"
  ))
})

test_that("a later summary whose value holds a share's marker is refused", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  data <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4),
                         sh = c(100, 200, 300))
  error <- expect_error(summarize_with_margins(
    data, s = sum(v), sh = share_of_total(s),
    z = tibble::tibble_("sh")[[1L]], .grouping = rollup(g)
  ), "cannot use an earlier Total share (`sh`)", fixed = TRUE)
  expect_s3_class(error, "marginplyr_error")

  nested <- expect_error(summarize_with_margins(
    data, s = sum(v), sh = share_of_total(s),
    tibble::tibble(z = list(list(tibble::tibble_("sh")[[1L]]))),
    .grouping = rollup(g)
  ), "cannot use an earlier Total share (`sh`)", fixed = TRUE)
  expect_s3_class(nested, "marginplyr_error")
})

test_that("a frame creating a share's name is still a name conflict", {
  plain <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
  shadowed <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 4),
                             sh = c(100, 200, 300))
  for (data in list(plain, shadowed, plain[0L, ], shadowed[0L, ])) {
    later <- expect_error(summarize_with_margins(
      data, s = sum(v), sh = share_of_total(s), tibble::tibble(sh = 99),
      .grouping = rollup(g)
    ), "Total-share output name `sh` conflicts with an ordinary summary",
    fixed = TRUE)
    expect_s3_class(later, "marginplyr_error")
    earlier <- expect_error(summarize_with_margins(
      data, s = sum(v), tibble::tibble(sh = 99), sh = share_of_total(s),
      .grouping = rollup(g)
    ), "Total-share output name `sh` conflicts with an ordinary summary",
    fixed = TRUE)
    expect_s3_class(earlier, "marginplyr_error")
  }
})

test_that("summaries reading no share keep their values and order", {
  data <- tibble::tibble(k = "x", g = c("a", "a", "b"), v = c(1, 2, 4),
                         sh = c(100, 200, 300))
  result <- summarize_with_margins(
    data[-1L], s = sum(v), q = sum(sh), sh = share_of_total(s),
    m = mean(v), .grouping = rollup(g)
  )
  expect_identical(names(result), c("g", "s", "q", "sh", "m"))
  expect_identical(result$q, c(300, 300, 600))
  expect_equal(result$sh, c(3, 4, 7) / 7)
  expect_equal(result$m, c(1.5, 4, 7 / 3))

  grouped <- summarize_with_margins(
    dplyr::group_by(data, k), s = sum(v), q = sum(sh),
    sh = share_of_total(s), m = mean(v), .grouping = rollup(g)
  )
  expect_identical(names(grouped), c("k", "g", "s", "q", "sh", "m"))
  expect_identical(grouped$q, c(300, 300, 600))
})
