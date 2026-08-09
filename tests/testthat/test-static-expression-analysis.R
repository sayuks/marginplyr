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
  # whose parts are all mask reads, so it is walked; a function definition is
  # not, because it binds its own formals and walking it would report a read
  # that is not one. A `$` head is not walked either, so the object it reads
  # is missed -- #130 carries that, and these are the two halves that must not
  # drift into each other.
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

test_that("an ordinary `$` field name is not a data-mask read", {
  # `cfg$share` reads a field of a list. The name after `$` is fixed text
  # rather than a lookup, so collecting it made the guard against reading an
  # earlier share believe an ordinary summary read the share of that name, and
  # legal code was rejected -- while `cfg[["share"]]`, the same access spelled
  # with a string, ran and returned the right answer (#101).
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  # nolint start: object_usage_linter.
  # `cfg` is read from the summary expressions below, which codetools cannot
  # follow through the data mask.
  cfg <- list(share = "annual", total = 99)
  # nolint end

  from_dollar <- summarize_with_margins(
    data,
    total = sum(value),
    share = share_of_parent(total),
    note = cfg$share,
    .grouping = rollup(region)
  )
  from_bracket <- summarize_with_margins(
    data,
    total = sum(value),
    share = share_of_parent(total),
    note = cfg[["share"]],
    .grouping = rollup(region)
  )

  expect_equal(from_dollar, from_bracket)
  expect_identical(unique(from_dollar$note), "annual")

  # The same disagreement reached from the other side: the field name made a
  # share source look as though it depended on an earlier summary alias.
  source_dollar <- summarize_with_margins(
    data,
    total = sum(value),
    scaled = sum(value) * cfg$total,
    sh = share_of_parent(scaled),
    .grouping = rollup(region)
  )
  source_bracket <- summarize_with_margins(
    data,
    total = sum(value),
    scaled = sum(value) * cfg[["total"]],
    sh = share_of_parent(scaled),
    .grouping = rollup(region)
  )

  expect_equal(source_dollar, source_bracket)
})

test_that("the object of a `$` and the index of a `[[` are still read", {
  # Only the field name stops counting. Dropping `$` calls from the walk
  # altogether would let a read of the object through, and `[[` is not the
  # same shape at all: its index is evaluated, so `cfg[[share]]` reads the
  # share the guard exists to catch.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  from_object <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = share$field,
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_object, "marginplyr_error")

  from_index <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = list(a = 1)[[share]],
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(from_index, "marginplyr_error")

  expect_identical(expression_data_symbols(quote(cfg$share)), "cfg")
  expect_identical(
    expression_data_symbols(quote(cfg[[share]])),
    c("cfg", "share")
  )
  # `@` names a slot the same literal way, and the walk reaches it through the
  # same fall-through, so it is fixed in the same pass (#101). It is asserted
  # on the walk rather than through a summary because an S4 object in a
  # summary expression would add a dependency the package does not otherwise
  # need.
  expect_identical(expression_data_symbols(quote(cfg@share)), "cfg")
})

test_that("a genuine read of an earlier share is still rejected", {
  # The two true positives the false positives above sit next to: an ordinary
  # summary that really does name the share, and a share source that really
  # does depend on an earlier alias. Both messages are the ones #101 must
  # leave untouched.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )

  ordinary <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      share = share_of_parent(total),
      note = share,
      .grouping = rollup(region)
    ),
    "Ordinary summaries cannot use an earlier Parent share (`share`)",
    fixed = TRUE
  )
  expect_s3_class(ordinary, "marginplyr_error")

  source <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      scaled = total * 2,
      sh = share_of_parent(scaled),
      .grouping = rollup(region)
    ),
    paste0(
      "Parent share `sh` cannot use source summary `scaled` because it ",
      "depends on earlier summary alias `total`"
    ),
    fixed = TRUE
  )
  expect_s3_class(source, "marginplyr_error")
})

test_that("`.data$x` still reads a column and `.env$x` still reads none", {
  # The pronoun branches sit above the `$` handling and are unchanged: the
  # fix must not reach them.
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 3, 6)
  )
  # nolint start: object_usage_linter.
  # Read through `.env` from the summary expression below.
  share <- "from the environment"
  # nolint end

  pronoun <- expect_error(
    summarize_with_margins(
      data,
      units = sum(value),
      share = share_of_total(units),
      derived = .data$share * 2,
      .grouping = rollup(region)
    ),
    "`share`"
  )
  expect_s3_class(pronoun, "marginplyr_error")

  from_env <- summarize_with_margins(
    data,
    units = sum(value),
    share = share_of_total(units),
    note = .env$share,
    .grouping = rollup(region)
  )
  expect_identical(unique(from_env$note), "from the environment")
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

test_that("an unnamed `across()` argument is numbered by its own position", {
  # The placeholder names the analysis gives the unnamed additional arguments
  # of an `across()` call are only ever read back in a diagnostic, so a wrong
  # one is invisible until something quotes it. Building them over *every*
  # additional argument while assigning to the unnamed ones is visible sooner
  # than that: the two sides differ in length whenever any additional argument
  # is named, so base R recycles and warns from a call that otherwise
  # succeeds, once per grouping-set branch (#104).
  data <- data.frame(
    region = c("East", "East", "West"),
    units = c(1, 3, 6),
    revenue = c(2, 4, 8)
  )

  # Only untyped warnings are collected. Passing an argument through
  # `across()`'s `...` at all is deprecated by dplyr, which warns for its own
  # reasons from the same call; that warning is dplyr's to raise, carries its
  # own class, and is untouched by this fix, whereas a bare `simpleWarning`
  # here can only have come from the analysis.
  untyped_warnings <- character()
  withCallingHandlers(
    summarize_with_margins(
      data,
      dplyr::across(c(units, revenue), sum, na.rm = TRUE, TRUE),
      .grouping = rollup(region)
    ),
    warning = function(cnd) {
      if (inherits(cnd, "simpleWarning")) {
        untyped_warnings <<- c(untyped_warnings, conditionMessage(cnd))
      }
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(untyped_warnings, character())

  error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(revenue),
      dplyr::across(
        c(total),
        share_of_total,
        .names = "{.col}_s",
        na.rm = TRUE,
        TRUE
      ),
      .grouping = rollup(region)
    ),
    "does not accept additional function arguments"
  )
  # dplyr names an argument passed on to the function by its position among
  # those arguments, so the unnamed one here -- second of the two -- is `..2`.
  expect_match(conditionMessage(error), "`na.rm`, `..2`", fixed = TRUE)
})

test_that("`across()` argument numbering holds without a mix of names", {
  # The two ends of the same rule: with no named argument the ordinals are
  # already what a sequence over everything would produce, and with no unnamed
  # one there is nothing to number. Neither reaches the subassignment that
  # #104 was about, so both must keep behaving as they did.
  data <- data.frame(
    region = c("East", "East", "West"),
    revenue = c(2, 4, 8)
  )
  base_call <- function(...) {
    summarize_with_margins(
      data,
      total = sum(revenue),
      ...,
      .grouping = rollup(region)
    )
  }

  all_unnamed <- expect_error(
    base_call(dplyr::across(
      c(total),
      share_of_total,
      .names = "{.col}_s",
      TRUE,
      TRUE
    )),
    "does not accept additional function arguments"
  )
  expect_match(conditionMessage(all_unnamed), "`..1`, `..2`", fixed = TRUE)

  all_named <- expect_error(
    base_call(dplyr::across(
      c(total),
      share_of_total,
      .names = "{.col}_s",
      na.rm = TRUE
    )),
    "does not accept additional function arguments"
  )
  expect_match(conditionMessage(all_named), "`na.rm`.", fixed = TRUE)
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
