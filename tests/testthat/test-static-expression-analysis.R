# The analysis that reads summary expressions before execution used to assume
# shapes real expressions do not always have, and raised the base-R condition
# that assumption produced: an `NA` `if()` condition, an out-of-bounds `[[`,
# and a `vapply()` type error (#100). None of the three is catchable by class
# or tells the caller what to change, so each is asserted here by class as
# well as by message.
#
# Which outcome is right differs per site, and that is the point ADR-0015
# draws: a shape the analysis simply does not recognize must fall through and
# evaluate, an error raised by the caller's own code must reach them with its
# own class intact, and only a fault the analysis itself detects becomes a
# Package condition.

test_that("a call whose head is a call is evaluated, not classified", {
  # `call_name()` answers `NULL` here, so the `$`/`[[` test used to compare
  # `NULL` against a character vector, yielding `NA` inside `if()`. Such a call
  # is not a data-mask reference, so the analysis has nothing to say about it
  # and dplyr must see it unchanged.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  fns <- list(total = function(x, na_rm) sum(x, na.rm = na_rm))

  from_list <- summarize_with_margins(
    data,
    value = fns$total(value, TRUE),
    .grouping = rollup(region),
    .margin_label = NULL
  )
  from_lambda <- summarize_with_margins(
    data,
    value = (function(x) sum(x))(value),
    .grouping = rollup(region),
    .margin_label = NULL
  )

  expected <- data |>
    dplyr::group_by(region) |>
    dplyr::summarise(value = fns$total(value, TRUE), .groups = "drop")

  expect_equal(
    dplyr::arrange(from_list[!is.na(from_list$region), ], region),
    as.data.frame(expected),
    ignore_attr = "row.names"
  )
  expect_equal(from_lambda, from_list)
})

test_that("falling through a call head still sees its arguments", {
  # Falling through must reach the arguments rather than abandon the
  # expression: the dependency check that forbids reading an earlier share
  # from an ordinary summary is the same walk, so a blind fall-through would
  # let this call through and produce a wrong number instead of an error.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  fns <- list(double = function(x) x * 2)

  error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      share = share_of_total(total),
      derived = fns$double(share),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(error, "marginplyr_error")
})

test_that("a `[[` call head is walked, and other head shapes are not", {
  # The function position is dropped so `sum` never counts as a column, which
  # leaves a read inside a call-valued head unseen. `[[` is the one head shape
  # whose parts are all mask reads, so it is walked; `$` and a function
  # definition are not, because walking them with the rules this analysis has
  # today would report a read that is not one. #130 carries the rest, and
  # these are the two halves that must not drift into each other.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  # nolint start: object_usage_linter.
  # `fns` is read from the summary expressions below, which codetools cannot
  # follow through the data mask.
  fns <- list(
    total = function(x, na_rm) sum(x, na_rm),
    double = function(x) x * 2
  )
  # nolint end

  from_head <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = fns[[if (length(share)) "double" else "double"]](value),
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_head, "marginplyr_error")

  # `total` in `fns$total` names a field of `fns` rather than reading the
  # share of that name, so this call must execute. Rejecting it is the defect
  # #100 was filed against, reached from the other direction.
  expect_no_error(
    summarize_with_margins(
      data,
      units = sum(value),
      total = share_of_total(units),
      derived = fns$total(value, TRUE),
      .grouping = rollup(region)
    )
  )
})

test_that("a `get()` call with no name argument raises the caller's error", {
  # The analysis reads the looked-up name out of `get()`, and a call that
  # supplies neither `x` nor a positional argument has none to read. Nothing
  # is wrong with the analysis, so the call evaluates and base R's own
  # condition reaches the caller -- as it does from plain `summarise()`.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  baseline <- expect_error(
    data |>
      dplyr::group_by(region) |>
      dplyr::summarise(z = get(mode = "numeric"), .groups = "drop")
  )
  error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      z = get(mode = "numeric"),
      .grouping = rollup(region)
    )
  )

  expect_identical(class(error), class(baseline))
  expect_identical(class(error$parent), class(baseline$parent))
  expect_s3_class(error$parent, "missingArgError")
  expect_false(inherits(error, "marginplyr_error"))
})

test_that("an `across()` `.names` template must name one output per column", {
  # This one the analysis does detect: it expands the template itself, so it
  # knows before any backend read that the template names two outputs for one
  # column. ADR-0005 puts the rejection here rather than leaving it to the
  # query dplyr would otherwise build.
  data <- data.frame(
    region = c("East", "East", "West"),
    units = c(1, 3, 6)
  )

  error <- expect_error(
    summarize_with_margins(
      data,
      dplyr::across(c(units), sum, .names = "{c('x','y')}"),
      .grouping = rollup(region)
    ),
    "must produce one name per column"
  )

  expect_s3_class(error, "marginplyr_error")
  # The template is what the caller has to rewrite, so the message quotes it.
  expect_match(conditionMessage(error), "{c('x','y')}", fixed = TRUE)
  expect_match(conditionMessage(error), "`units`", fixed = TRUE)
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "summarize_with_margins"
  )
})

test_that("no analysed shape reaches the caller as an untyped condition", {
  # The classes below are what each site raised before #100. Asserting their
  # absence together keeps a future rewrite that reintroduces one of them from
  # passing on the message alone.
  data <- data.frame(
    region = c("East", "East", "West"),
    units = c(1, 3, 6)
  )
  fns <- list(total = function(x, na_rm) sum(x, na.rm = na_rm))

  errors <- list(
    call_head = tryCatch(
      summarize_with_margins(
        data,
        units = fns$total(units, TRUE),
        .grouping = rollup(region)
      ),
      error = function(cnd) cnd
    ),
    missing_get_name = tryCatch(
      summarize_with_margins(
        data,
        z = get(mode = "numeric"),
        .grouping = rollup(region)
      ),
      error = function(cnd) cnd
    ),
    across_names = tryCatch(
      summarize_with_margins(
        data,
        dplyr::across(c(units), sum, .names = "{c('x','y')}"),
        .grouping = rollup(region)
      ),
      error = function(cnd) cnd
    )
  )

  expect_s3_class(errors$call_head, "data.frame")
  for (error in errors[c("missing_get_name", "across_names")]) {
    expect_s3_class(error, "condition")
    expect_false(inherits(error, "simpleError"))
    expect_false(inherits(error, "subscriptOutOfBoundsError"))
  }
})
