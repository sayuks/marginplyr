test_that("direct Parent shares use the immediate rollup parent", {
  data <- data.frame(
    region = c("East", "East", "West"),
    store = c("A", "B", "C"),
    revenue = c(10, 30, 60)
  )

  result <- summarize_with_margins(
    data,
    level = grouping_id(region, store),
    total = sum(revenue),
    share = share_of_parent(total),
    .grouping = rollup(region, store),
    .margin_label = NULL
  ) |>
    dplyr::arrange(level, region, store)

  expect_identical(
    names(result),
    c("region", "store", "level", "total", "share")
  )
  expect_equal(result$total, c(10, 30, 60, 40, 60, 100))
  expect_equal(result$share, c(0.25, 0.75, 1, 0.4, 0.6, 1))
  expect_type(result$share, "double")
})

test_that("default Margin labels preserve typed grouping identity", {
  dimensions <- list(
    integer = c(1L, 2L),
    double = c(1.5, 2.5),
    logical = c(TRUE, FALSE),
    Date = as.Date(c("2026-01-01", "2026-01-02")),
    factor = factor(c("x", "y")),
    character = c("x", "y")
  )

  for (dimension_type in names(dimensions)) {
    data <- data.frame(
      dimension = dimensions[[dimension_type]],
      value = c(1, 3)
    )
    result <- summarize_with_margins(
      data,
      level = grouping_bit(dimension),
      total = sum(value),
      share = share_of_parent(total),
      .grouping = rollup(dimension)
    )

    expect_identical(nrow(result), 3L, info = dimension_type)
    expect_setequal(result$total, c(1, 3, 4))
    expect_setequal(result$share, c(0.25, 0.75, 1))
    expect_type(result$share, "double")
    expect_identical(dplyr::group_vars(result), character())
  }
})

test_that("Parent identity separates missing keys from displayed margins", {
  data <- data.frame(
    fixed = c(NA_character_, NA_character_, "A", "A"),
    group = c(NA_character_, "Total", NA_character_, "Total"),
    ..marginplyr_parent_n_1 = c(1, 3, 2, 2),
    check.names = FALSE
  )
  summarize <- function(margin_label) {
    summarize_with_margins(
      data,
      level = grouping_bit(group),
      total = sum(.data[["..marginplyr_parent_n_1"]]),
      share = share_of_parent(total),
      .by = fixed,
      .grouping = rollup(group),
      .margin_label = margin_label,
      .check_margin_label = FALSE
    )
  }

  displayed <- summarize("Total")
  missing <- summarize(NULL)

  expect_identical(nrow(displayed), 6L)
  expect_identical(names(displayed), names(missing))
  expect_identical(dplyr::group_vars(displayed), character())
  expect_setequal(
    displayed$share[displayed$level == 0L],
    c(0.25, 0.75, 0.5, 0.5)
  )
  expect_identical(displayed$share[displayed$level == 1L], c(1, 1))
  expect_equal(displayed$share, missing$share)
  expect_identical(displayed$total, missing$total)
})

test_that("three-dimension Parent shares advance one rollup level", {
  data <- expand.grid(
    first = c("a", "b"),
    second = c("x", "y"),
    third = c("i", "j"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  data$value <- 1

  result <- summarize_with_margins(
    data,
    level = grouping_id(first, second, third),
    total = sum(value),
    share = share_of_parent(total),
    .grouping = rollup(first, second, third),
    .margin_label = NULL
  )

  expect_true(all(result$share[result$level %in% c(0L, 1L, 3L)] == 0.5))
  expect_identical(result$share[result$level == 7L], 1)
})

test_that("across creates Parent shares for preceding ordinary summaries", {
  data <- data.frame(
    region = c("East", "East", "West"),
    store = c("A", "B", "C"),
    revenue = c(10, 30, 60),
    units = c(1L, 3L, 6L)
  )

  result <- summarize_with_margins(
    data,
    dplyr::across(c(revenue, units), sum),
    dplyr::across(
      c(revenue, units),
      share_of_parent,
      .names = "{.col}_share"
    ),
    .grouping = rollup(region, store),
    .margin_label = NULL
  ) |>
    dplyr::arrange(region, store)

  expect_identical(
    names(result),
    c(
      "region", "store", "revenue", "units",
      "revenue_share", "units_share"
    )
  )
  expect_equal(result$revenue_share, result$units_share)
  expect_setequal(result$revenue_share, c(0.25, 0.75, 1, 0.4, 0.6, 1))
})

test_that("Parent shares use statically named across function-list outputs", {
  data <- data.frame(group = c("x", "x", "y"), value = c(1, 3, 6))

  result <- summarize_with_margins(
    data,
    dplyr::across(
      value,
      list(total = sum, average = mean)
    ),
    dplyr::across(
      c(value_total, value_average),
      share_of_parent,
      .names = "{.col}_share"
    ),
    .grouping = rollup(group),
    .margin_label = NULL
  )

  expect_true(all(c(
    "value_total_share",
    "value_average_share"
  ) %in% names(result)))
  expect_type(result$value_total_share, "double")
  expect_type(result$value_average_share, "double")
})

test_that(paste0(
  "Parent shares support composite dimensions, fixed keys, ",
  "and duplicates"
), {
  data <- data.frame(
    year = c(2025L, 2025L, 2025L, 2026L),
    region = c("East", "East", "West", "East"),
    store = c("A", "B", "C", "A"),
    product = c("P1", "P2", "P1", "P1"),
    value = c(10, 30, 60, 5)
  )

  composite <- summarize_with_margins(
    data,
    total = sum(value),
    share = share_of_parent(total),
    .by = year,
    .grouping = rollup(grouping_set(region, store), product),
    .margin_label = NULL
  )

  expect_setequal(
    composite$share[composite$year == 2025L],
    c(1, 1, 1, 0.1, 0.3, 0.6, 1)
  )
  expect_true(all(composite$share[composite$year == 2026L] == 1))

  duplicated <- summarize_with_margins(
    data[data$year == 2025L, ],
    total = sum(value),
    share = share_of_parent(total),
    .grouping = rollup(region, region),
    .duplicates = "keep",
    .id = "set",
    .margin_label = NULL
  )

  expect_setequal(unique(duplicated$set), 1:3)
  expect_equal(
    duplicated$share[duplicated$set %in% c(1L, 2L)],
    c(0.4, 0.6, 0.4, 0.6)
  )
  expect_identical(duplicated$share[duplicated$set == 3L], 1)
})

test_that(paste0(
  "Parent-share values handle missing, zero, negative, ",
  "and empty summaries"
), {
  data <- data.frame(
    group = c("missing", "positive", "negative"),
    value = c(NA_real_, 2, -2)
  )

  result <- summarize_with_margins(
    data,
    total = if (all(is.na(value))) NA_real_ else sum(value, na.rm = TRUE),
    share = share_of_parent(total),
    .grouping = rollup(group),
    .margin_label = NULL
  )

  detail <- !is.na(result$group)
  expect_true(is.na(result$share[detail & result$group == "missing"]))
  expect_true(is.na(result$share[detail & result$group == "positive"]))
  expect_true(is.na(result$share[detail & result$group == "negative"]))
  expect_identical(result$share[is.na(result$group)], 1)

  unclamped <- summarize_with_margins(
    data.frame(group = c("large", "negative"), value = c(3, -1)),
    total = sum(value),
    share = share_of_parent(total),
    .grouping = rollup(group),
    .margin_label = NULL
  )
  detail <- !is.na(unclamped$group)
  expect_identical(unclamped$share[detail & unclamped$group == "large"], 1.5)
  expect_identical(
    unclamped$share[detail & unclamped$group == "negative"],
    -0.5
  )

  empty <- data.frame(group = character(), value = double())
  root <- summarize_with_margins(
    empty,
    total = sum(value),
    share = share_of_parent(total),
    .grouping = rollup(group),
    .margin_label = NULL
  )
  partitioned <- summarize_with_margins(
    empty,
    total = sum(value),
    share = share_of_parent(total),
    .by = group,
    .grouping = rollup(value),
    .margin_label = NULL
  )

  expect_identical(root$share, 1)
  expect_identical(partitioned$share, double())
})

test_that(paste0(
  "Parent matching ignores display labels and preserves ",
  "expression order"
), {
  data <- data.frame(
    fixed = c(NA_character_, NA_character_),
    group = c("Total", "x"),
    value = c(1, 3)
  )

  result <- summarize_with_margins(
    data,
    total = sum(value),
    share = share_of_parent(total),
    n = dplyr::n(),
    .by = fixed,
    .grouping = rollup(group),
    .check_margin_label = FALSE
  )

  expect_identical(
    names(result),
    c("fixed", "group", "total", "share", "n")
  )
  expect_setequal(result$share[result$group == "Total"], c(0.25, 1))
})

test_that("share_of_parent reports its required context", {
  data <- data.frame(group = "x", value = 1)

  context_error <- expect_error(
    share_of_parent(value),
    "only be used inside `summarize_with_margins\\(\\)`"
  )
  expect_s3_class(context_error, "marginplyr_error")
  expect_identical(
    rlang::call_name(conditionCall(context_error)),
    "share_of_parent"
  )
  expect_error(
    dplyr::summarise(data, share = share_of_parent(value)),
    "following `dplyr::mutate\\(\\)`"
  )
  expect_error(
    dplyr::mutate(data, share = share_of_parent(value)),
    "following `dplyr::mutate\\(\\)`"
  )
})

test_that("Parent shares require one pure rollup", {
  data <- data.frame(a = "x", b = "y", value = 1)
  unsupported <- list(
    grouping_set(a),
    grouping_sets(grouping_set(a), grouping_set()),
    cube(a),
    grouping_spec(rollup(a), rollup(b))
  )

  for (spec in unsupported) {
    error <- expect_error(
      summarize_with_margins(
        data,
        total = sum(value),
        share = share_of_parent(total),
        .grouping = spec
      ),
      "requires `.grouping` to be one pure `rollup\\(\\)`"
    )
    expect_s3_class(error, "marginplyr_error")
  }
})

test_that("direct Parent-share syntax and dependency errors are targeted", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  expect_error(
    summarize_with_margins(
      data,
      sum(value),
      share_of_parent(`sum(value)`),
      .grouping = rollup(group)
    ),
    "must have an explicit output name"
  )
  syntax_error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      share = share_of_parent(sum(value)),
      .grouping = rollup(group)
    ),
    "requires exactly one bare name"
  )
  expect_s3_class(syntax_error, "marginplyr_error")
  expect_identical(
    rlang::call_name(conditionCall(syntax_error)),
    "summarize_with_margins"
  )
  invalid_arguments <- list(
    quote(share_of_parent("total")),
    quote(share_of_parent(total, value))
  )
  for (argument in invalid_arguments) {
    expect_error(
      rlang::inject(summarize_with_margins(
        data,
        total = sum(value),
        share = !!argument,
        .grouping = rollup(group)
      )),
      "requires exactly one bare name"
    )
  }
  expect_error(
    summarize_with_margins(
      data,
      share = share_of_parent(total),
      total = sum(value),
      .grouping = rollup(group)
    ),
    "forward reference"
  )
  expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      total = mean(value),
      share = share_of_parent(total),
      .grouping = rollup(group)
    ),
    "defined exactly once"
  )
  expect_error(
    summarize_with_margins(
      data,
      first = sum(value),
      total = first + 1,
      share = share_of_parent(total),
      .grouping = rollup(group)
    ),
    "depends on earlier summary alias `first`"
  )
  expect_error(
    summarize_with_margins(
      data,
      base::data.frame(total = sum(value)),
      share = share_of_parent(total),
      .grouping = rollup(group)
    ),
    "data-frame-valued summary"
  )
  expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      share = share_of_parent(total),
      derived = share * 100,
      .grouping = rollup(group)
    ),
    "Ordinary summaries cannot use an earlier Parent share"
  )
  expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      share = 100 * share_of_parent(total),
      .grouping = rollup(group)
    ),
    "complete right-hand side"
  )
  expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      first_share = share_of_parent(total),
      second_share = share_of_parent(first_share),
      .grouping = rollup(group)
    ),
    "cannot use Parent share `first_share`"
  )
})

test_that("Parent-share outputs reject every static collision", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  calls <- list(
    quote(summarize_with_margins(
      data,
      total = sum(value),
      group = share_of_parent(total),
      .grouping = rollup(group)
    )),
    quote(summarize_with_margins(
      data,
      total = sum(value),
      total = share_of_parent(total),
      .grouping = rollup(group)
    )),
    quote(summarize_with_margins(
      data,
      total = sum(value),
      set = share_of_parent(total),
      .grouping = rollup(group),
      .id = "set"
    )),
    quote(summarize_with_margins(
      data,
      total = sum(value),
      share = share_of_parent(total),
      share = share_of_parent(total),
      .grouping = rollup(group)
    ))
  )

  for (call in calls) {
    expect_error(eval(call), "Parent-share output name .* conflicts")
  }
})

test_that("Parent-share across accepts name-based tidyselect", {
  data <- data.frame(
    group = c("x", "y"),
    x = c(1, 3),
    y = c(2, 2),
    z = c(4, 4)
  )
  selections <- list(
    quote(c(alpha, beta)),
    quote(alpha:gamma),
    quote(alpha | gamma),
    quote(-beta),
    quote(all_of(c("alpha", "gamma"))),
    quote(any_of(c("alpha", "unknown"))),
    quote(everything()),
    quote(starts_with("g"))
  )
  expected <- list(
    c("alpha_share", "beta_share"),
    c("alpha_share", "beta_share", "gamma_share"),
    c("alpha_share", "gamma_share"),
    c("alpha_share", "gamma_share"),
    c("alpha_share", "gamma_share"),
    "alpha_share",
    c("alpha_share", "beta_share", "gamma_share"),
    "gamma_share"
  )

  for (i in seq_along(selections)) {
    result <- rlang::inject(summarize_with_margins(
      data,
      alpha = sum(x),
      beta = sum(y),
      gamma = sum(z),
      dplyr::across(
        !!selections[[i]],
        share_of_parent,
        .names = "{.col}_share"
      ),
      .grouping = rollup(group)
    ))
    expect_true(all(expected[[i]] %in% names(result)))
  }

  qualified <- summarize_with_margins(
    data,
    alpha = sum(x),
    dplyr::across(
      alpha,
      marginplyr::share_of_parent,
      .names = "{.col}_share"
    ),
    .grouping = rollup(group)
  )
  expect_true("alpha_share" %in% names(qualified))

  empty_selection <- summarize_with_margins(
    data,
    alpha = sum(x),
    dplyr::across(
      any_of("unknown"),
      share_of_parent,
      .names = "{.col}_share"
    ),
    .grouping = rollup(group)
  )
  expect_identical(names(empty_selection), c("group", "alpha"))
})

test_that("Parent-share across rejects non-name contracts", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  base_call <- function(expr) {
    rlang::inject(summarize_with_margins(
      data,
      total = sum(value),
      !!expr,
      .grouping = rollup(group)
    ))
  }

  expect_error(
    base_call(quote(dplyr::across(
      total,
      share_of_parent
    ))),
    "requires an explicit `.names`"
  )
  expect_error(
    base_call(quote(dplyr::across(
      where(is.numeric),
      share_of_parent,
      .names = "{.col}_share"
    ))),
    "only supports name-based tidyselect"
  )
  expect_error(
    base_call(quote(dplyr::across(
      value,
      share_of_parent,
      .names = "{.col}_share"
    ))),
    "Select only eligible preceding ordinary summaries"
  )
  invalid_fns <- list(
    quote(\(x) share_of_parent(x)),
    quote(~share_of_parent(.x)),
    quote(list(share_of_parent))
  )
  for (fn in invalid_fns) {
    expect_error(
      base_call(rlang::expr(dplyr::across(
        total,
        !!fn,
        .names = "{.col}_share"
      ))),
      "`.fns` must be `share_of_parent`"
    )
  }
  expect_error(
    base_call(quote(dplyr::across(
      total,
      share_of_parent,
      na.rm = TRUE,
      .names = "{.col}_share"
    ))),
    "does not accept additional function arguments"
  )
  expect_error(
    base_call(quote(dplyr::across(
      total,
      share_of_parent,
      .names = "{.col}_share",
      .unpack = TRUE
    ))),
    "requires `.unpack = FALSE`"
  )
})

test_that("Parent-share sources are numeric scalar summaries", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  cardinality_error <- expect_error(
    summarize_with_margins(
      data,
      total = c(min(value), max(value)),
      share = share_of_parent(total),
      .grouping = rollup(group)
    ),
    "exactly one value per grouping row"
  )
  expect_s3_class(
    cardinality_error,
    "marginplyr_parent_cardinality_error"
  )
  expect_s3_class(cardinality_error, "marginplyr_error")
  expect_identical(cardinality_error$parent_output, "share")
  expect_identical(cardinality_error$source_summary, "total")
  expect_identical(
    rlang::call_name(conditionCall(cardinality_error)),
    "summarize_with_margins"
  )
  expect_match(conditionMessage(cardinality_error), "Parent share `share`")
  expect_match(conditionMessage(cardinality_error), "source summary `total`")
  expect_error(
    suppressWarnings(summarize_with_margins(
      data,
      total = numeric(),
      share = share_of_parent(total),
      .grouping = rollup(group)
    )),
    "exactly one value per grouping row"
  )
  type_error <- expect_error(
    summarize_with_margins(
      data,
      total = any(value > 0),
      share = share_of_parent(total),
      .grouping = rollup(group)
    ),
    "plain integer or double scalar"
  )
  expect_s3_class(type_error, "marginplyr_error")
  expect_identical(
    rlang::call_name(conditionCall(type_error)),
    "summarize_with_margins"
  )
  expect_match(conditionMessage(type_error), "Parent share `share`")
  expect_match(conditionMessage(type_error), "source summary `total`")
  expect_error(
    summarize_with_margins(
      data,
      total = paste(value, collapse = ""),
      share = share_of_parent(total),
      .grouping = rollup(group)
    ),
    "plain integer or double scalar"
  )
})

test_that("cardinality errors identify the affected Parent-share request", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  error <- expect_error(
    summarize_with_margins(
      data,
      good = sum(value),
      bad = c(min(value), max(value)),
      good_share = share_of_parent(good),
      bad_share = share_of_parent(bad),
      .grouping = rollup(group)
    ),
    "exactly one value per grouping row"
  )

  expect_match(conditionMessage(error), "Parent share `bad_share`")
  expect_match(conditionMessage(error), "source summary `bad`")
  expect_s3_class(error, "marginplyr_parent_cardinality_error")
  expect_identical(error$parent_output, "bad_share")
  expect_identical(error$source_summary, "bad")

  across_error <- expect_error(
    summarize_with_margins(
      data,
      dplyr::across(
        value,
        ~c(min(.x), max(.x)),
        .names = "total"
      ),
      share = share_of_parent(total),
      .grouping = rollup(group)
    ),
    "exactly one value per grouping row"
  )
  expect_s3_class(
    across_error,
    "marginplyr_parent_cardinality_error"
  )
  expect_identical(across_error$parent_output, "share")
  expect_identical(across_error$source_summary, "total")

  shifted_error <- expect_error(
    summarize_with_margins(
      data,
      first = sum(value),
      second = mean(value),
      dplyr::across(
        c(first, second),
        share_of_parent,
        .names = "{.col}_share"
      ),
      bad = c(min(value), max(value)),
      bad_share = share_of_parent(bad),
      .grouping = rollup(group)
    ),
    "exactly one value per grouping row"
  )
  expect_s3_class(
    shifted_error,
    "marginplyr_parent_cardinality_error"
  )
  expect_identical(shifted_error$parent_output, "bad_share")
  expect_identical(shifted_error$source_summary, "bad")
})

test_that("Parent-share execution preserves user-expression conditions", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  user_summary <- function(x) {
    rlang::abort(
      "Can't recycle this user-defined result.",
      class = "marginplyr_test_user_error",
      provenance = "user summary"
    )
  }

  baseline <- expect_error(
    dplyr::summarise(data, total = user_summary(value))
  )
  error <- expect_error(
    summarize_with_margins(
      data,
      total = user_summary(value),
      share = share_of_parent(total),
      .grouping = rollup(group)
    )
  )

  expect_identical(class(error), class(baseline))
  expect_identical(class(error$parent), class(baseline$parent))
  expect_identical(error$parent$provenance, "user summary")
  expect_identical(
    rlang::call_name(conditionCall(error$parent)),
    "user_summary"
  )
  expect_false(inherits(error, "marginplyr_error"))

  callless_summary <- function(x) {
    stop("Can't recycle this base error.", call. = FALSE)
  }
  callless_baseline <- expect_error(
    dplyr::summarise(data, total = callless_summary(value))
  )
  callless_error <- expect_error(
    summarize_with_margins(
      data,
      total = callless_summary(value),
      share = share_of_parent(total),
      .grouping = rollup(group)
    )
  )
  expect_identical(class(callless_error), class(callless_baseline))
  expect_identical(
    class(callless_error$parent),
    class(callless_baseline$parent)
  )
  expect_identical(
    conditionCall(callless_error$parent),
    conditionCall(callless_baseline$parent)
  )

  callless_grouping <- function() {
    stop("User grouping failed.", call. = FALSE)
  }
  grouping_error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      share = share_of_parent(total),
      .grouping = callless_grouping()
    )
  )
  expect_null(conditionCall(grouping_error))
  expect_false(inherits(grouping_error, "marginplyr_error"))
})

test_that(paste0(
  "Parent matching handles missing variable keys and ",
  "same-name summaries"
), {
  data <- data.frame(
    region = c(NA_character_, NA_character_, "East"),
    store = c("A", "B", "C"),
    value = c(1, 3, 6)
  )

  result <- summarize_with_margins(
    data,
    value = sum(value),
    share = share_of_parent(value),
    .grouping = rollup(region, store),
    .margin_label = NULL
  )
  missing_region_detail <- !is.na(result$store) & is.na(result$region)

  expect_setequal(result$share[missing_region_detail], c(0.25, 0.75))
  expect_false(any(vapply(result, is.list, logical(1))))

  alias_result <- summarise_with_margins(
    data,
    value = sum(value),
    share = marginplyr::share_of_parent(value),
    .grouping = rollup(region),
    .margin_label = NULL
  )
  expect_type(alias_result$share, "double")
})

test_that(paste0(
  "Parent-share root and denominator missing values follow ",
  "the contract"
), {
  all_missing <- data.frame(group = c("x", "y"), value = c(NA_real_, NA_real_))
  result <- summarize_with_margins(
    all_missing,
    total = mean(value),
    share = share_of_parent(total),
    .grouping = rollup(group),
    .margin_label = NULL
  )

  expect_true(all(is.na(result$share[!is.na(result$group)])))
  expect_identical(result$share[is.na(result$group)], 1)
})

test_that("Parent-share across validates generated names and unpacking", {
  data <- data.frame(group = c("x", "y"), x = 1:2, y = 3:4)

  allowed <- summarize_with_margins(
    data,
    x_total = sum(x),
    dplyr::across(
      x_total,
      share_of_parent,
      .names = "{.col}_share",
      .unpack = FALSE
    ),
    .grouping = rollup(group)
  )
  expect_true("x_total_share" %in% names(allowed))

  collision_calls <- list(
    quote(summarize_with_margins(
      data,
      x_total = sum(x),
      y_total = sum(y),
      dplyr::across(
        c(x_total, y_total),
        share_of_parent,
        .names = "share"
      ),
      .grouping = rollup(group)
    )),
    quote(summarize_with_margins(
      data,
      x_total = sum(x),
      dplyr::across(
        x_total,
        share_of_parent,
        .names = "{.col}"
      ),
      .grouping = rollup(group)
    )),
    quote(summarize_with_margins(
      data,
      x_total = sum(x),
      dplyr::across(
        x_total,
        share_of_parent,
        .names = "group"
      ),
      .grouping = rollup(group)
    )),
    quote(summarize_with_margins(
      data,
      x_total = sum(x),
      dplyr::across(
        x_total,
        share_of_parent,
        .names = "set"
      ),
      .grouping = rollup(group),
      .id = "set"
    ))
  )
  messages <- c(
    "output names must be unique",
    "output name .* conflicts",
    "output name .* conflicts",
    "output name .* conflicts"
  )

  for (i in seq_along(collision_calls)) {
    expect_error(eval(collision_calls[[i]]), messages[[i]])
  }
  expect_error(
    summarize_with_margins(
      data,
      x_total = sum(x),
      dplyr::across(
        x_total,
        share_of_parent,
        .names = ""
      ),
      .grouping = rollup(group)
    ),
    "output names must not be empty"
  )
})

test_that("Parent-share across cannot select keys or earlier Parent shares", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      dplyr::across(
        group,
        share_of_parent,
        .names = "{.col}_share"
      ),
      .grouping = rollup(group)
    ),
    "Select only eligible preceding ordinary summaries"
  )
  expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      direct_share = share_of_parent(total),
      dplyr::across(
        direct_share,
        share_of_parent,
        .names = "{.col}_copy"
      ),
      .grouping = rollup(group)
    ),
    "Select only eligible preceding ordinary summaries"
  )
})

test_that("semantic and nonnumeric classes are not Parent-share sources", {
  data <- data.frame(group = c("x", "y"))
  sources <- list(
    Date = as.Date("2026-01-01"),
    POSIXct = as.POSIXct("2026-01-01", tz = "UTC"),
    difftime = as.difftime(1, units = "days"),
    factor = factor("x"),
    list = list(1)
  )

  for (source_type in names(sources)) {
    source <- sources[[source_type]]
    error <- expect_error(summarize_with_margins(
      data,
      total = .env$source,
      share = share_of_parent(total),
      .grouping = rollup(group)
    ))
    expect_s3_class(error, "marginplyr_error")
    expect_match(
      conditionMessage(error),
      paste0("detected type ", source_type)
    )
  }
})

test_that("Parent dependencies distinguish data-mask and environment lookups", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  expect_error(
    summarize_with_margins(
      data,
      first = sum(value),
      total = .data[["first"]] + 1,
      share = share_of_parent(total),
      .grouping = rollup(group)
    ),
    "depends on earlier summary alias `first`"
  )
  expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      share = share_of_parent(total),
      derived = .data[["share"]] * 100,
      .grouping = rollup(group)
    ),
    "Ordinary summaries cannot use an earlier Parent share"
  )

  first <- 10
  allowed <- summarize_with_margins(
    data,
    first = sum(value),
    total = sum(value) + .env$first,
    share = share_of_parent(total),
    .grouping = rollup(group)
  )
  expect_type(allowed$share, "double")

  lookup_env <- rlang::env(first = 10)
  external <- summarize_with_margins(
    data,
    first = sum(value),
    total = sum(value) + base::get("first", envir = lookup_env),
    share = share_of_parent(total),
    .grouping = rollup(group)
  )
  expect_type(external$share, "double")
})

test_that("Parent dependencies detect get indirection through the data mask", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  expect_error(
    summarize_with_margins(
      data,
      first = sum(value),
      total = get("first") + 1,
      share = share_of_parent(total),
      .grouping = rollup(group)
    ),
    "depends on earlier summary alias `first`"
  )
  expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      share = share_of_parent(total),
      derived = get("share") * 100,
      .grouping = rollup(group)
    ),
    "Ordinary summaries cannot use an earlier Parent share"
  )
})

test_that("Parent-share everything selects only eligible summaries", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  result <- summarize_with_margins(
    data,
    base::data.frame(hidden = sum(value)),
    total = sum(value),
    dplyr::across(
      everything(),
      share_of_parent,
      .names = "{.col}_share"
    ),
    .grouping = rollup(group)
  )

  expect_true("total_share" %in% names(result))
  expect_false("hidden_share" %in% names(result))
  expect_error(
    summarize_with_margins(
      data,
      base::data.frame(hidden = sum(value)),
      dplyr::across(
        hidden,
        share_of_parent,
        .names = "{.col}_share"
      ),
      .grouping = rollup(group)
    ),
    "Select only eligible preceding ordinary summaries"
  )
})

test_that("Parent-share across does not infer bare predicate symbols", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      dplyr::across(
        is.numeric,
        share_of_parent,
        .names = "{.col}_share"
      ),
      .grouping = rollup(group)
    ),
    "unknown summary `is.numeric`"
  )
  expect_false(grepl("predicate", conditionMessage(error), fixed = TRUE))
})

test_that("Parent-share across classifies source-name failures", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  duplicate_error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      total = mean(value),
      dplyr::across(
        total,
        share_of_parent,
        .names = "{.col}_share"
      ),
      .grouping = rollup(group)
    ),
    "summary `total` was defined more than once"
  )
  expect_s3_class(duplicate_error, "marginplyr_error")

  unavailable_error <- expect_error(
    summarize_with_margins(
      data,
      base::data.frame(hidden = sum(value)),
      dplyr::across(
        hidden,
        share_of_parent,
        .names = "{.col}_share"
      ),
      .grouping = rollup(group)
    ),
    "summary `hidden` is not available"
  )
  expect_s3_class(unavailable_error, "marginplyr_error")

  unknown_error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      dplyr::across(
        missing,
        share_of_parent,
        .names = "{.col}_share"
      ),
      .grouping = rollup(group)
    ),
    "unknown summary `missing`"
  )
  expect_s3_class(unknown_error, "marginplyr_error")

  predicate_error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      dplyr::across(
        dplyr::where(is.numeric),
        share_of_parent,
        .names = "{.col}_share"
      ),
      .grouping = rollup(group)
    ),
    "only supports name-based tidyselect"
  )
  expect_s3_class(predicate_error, "marginplyr_error")

  expect_snapshot(conditionMessage(duplicate_error))
  expect_snapshot(conditionMessage(unavailable_error))
  expect_snapshot(conditionMessage(unknown_error))
  expect_snapshot(conditionMessage(predicate_error))
})

test_that("caller function symbols are not inferred to be predicates", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  caller_selection <- function(x) is.numeric(x)

  error <- expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      dplyr::across(
        caller_selection,
        share_of_parent,
        .names = "{.col}_share"
      ),
      .grouping = rollup(group)
    ),
    "unknown summary `caller_selection`"
  )

  expect_s3_class(error, "marginplyr_error")
  expect_false(grepl("predicate", conditionMessage(error), fixed = TRUE))
})

test_that("across sources cannot hide earlier summary-alias dependencies", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  expect_error(
    summarize_with_margins(
      data,
      value = sum(value),
      dplyr::across(
        all_of("value"),
        sum,
        .names = "{.col}_again"
      ),
      share = share_of_parent(value_again),
      .grouping = rollup(group)
    ),
    "depends on earlier summary alias `value`"
  )
})

test_that("repeated across name placeholders are planned like dplyr", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  result <- summarize_with_margins(
    data,
    dplyr::across(
      value,
      sum,
      .names = "{.col}_{.col}"
    ),
    share = share_of_parent(value_value),
    dplyr::across(
      value_value,
      share_of_parent,
      .names = "{.col}_{.col}_share"
    ),
    .grouping = rollup(group)
  )

  expect_true("value_value" %in% names(result))
  expect_true("value_value_value_value_share" %in% names(result))
})

test_that("across name templates support static glue expressions", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  result <- summarize_with_margins(
    data,
    dplyr::across(
      value,
      sum,
      .names = "{toupper(.col)}"
    ),
    share = share_of_parent(VALUE),
    dplyr::across(
      VALUE,
      share_of_parent,
      .names = "{tolower(.col)}_{toupper('share')}"
    ),
    .grouping = rollup(group)
  )

  expect_true(all(c("VALUE", "share", "value_SHARE") %in% names(result)))
  expect_equal(result$share, result$value_SHARE)
})

test_that("Parent planning preserves every public-call environment", {
  data <- data.frame(
    fixed = c("a", "a", "b"),
    group = c("x", "y", "x"),
    value = c(1, 3, 6)
  )

  summarize_from_local_scope <- function(data) {
    fixed_cols <- "fixed"
    dimension_cols <- "group"
    summary_cols <- "value"
    total <- function(x) sum(x)

    summarize_with_margins(
      data,
      dplyr::across(
        .fns = total,
        .cols = dplyr::all_of(summary_cols),
        .names = "total"
      ),
      dplyr::across(
        .fns = share_of_parent,
        .cols = total,
        .names = "share"
      ),
      .by = dplyr::all_of(fixed_cols),
      .grouping = rollup(dplyr::all_of(dimension_cols)),
      .margin_label = NULL
    )
  }

  result <- summarize_from_local_scope(data)

  expect_identical(names(result), c("fixed", "group", "total", "share"))
  expect_setequal(result$total, c(1, 3, 4, 6, 6))
  expect_setequal(result$share, c(0.25, 0.75, 1, 1, 1))
})

test_that("Parent planning evaluates across arguments once", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  ordinary_cols_n <- 0L
  ordinary_names_n <- 0L
  parent_cols_n <- 0L
  parent_names_n <- 0L
  ordinary_cols <- function() {
    ordinary_cols_n <<- ordinary_cols_n + 1L
    dplyr::all_of("value")
  }
  ordinary_names <- function() {
    ordinary_names_n <<- ordinary_names_n + 1L
    "{.col}"
  }
  parent_cols <- function() {
    parent_cols_n <<- parent_cols_n + 1L
    dplyr::all_of("value")
  }
  parent_names <- function() {
    parent_names_n <<- parent_names_n + 1L
    "{.col}_share"
  }

  result <- summarize_with_margins(
    data,
    dplyr::across(
      ordinary_cols(),
      sum,
      .names = ordinary_names()
    ),
    dplyr::across(
      parent_cols(),
      share_of_parent,
      .names = parent_names()
    ),
    .grouping = rollup(group)
  )

  expect_true("value_share" %in% names(result))
  expect_identical(ordinary_cols_n, 1L)
  expect_identical(ordinary_names_n, 1L)
  expect_identical(parent_cols_n, 1L)
  expect_identical(parent_names_n, 1L)
})

test_that("Parent planning evaluates the grouping expression once", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  evaluations <- 0L
  grouping <- function() {
    evaluations <<- evaluations + 1L
    rollup(group)
  }

  summarize_with_margins(
    data,
    total = sum(value),
    share = share_of_parent(total),
    .grouping = grouping()
  )

  expect_identical(evaluations, 1L)
})

parent_preflight_capture <- new.env(parent = emptyenv())

parent_preflight_head <- function(x, ...) {
  result <- NextMethod()
  class(result) <- unique(c("parent_preflight_counter", class(result)))
  result
}

parent_preflight_collect <- function(x, ...) {
  parent_preflight_capture$n <- parent_preflight_capture$n + 1L
  NextMethod()
}

test_that("Parent syntax and local execution errors precede typed metadata", {
  skip_if_not_installed("dtplyr")
  registerS3method(
    "head",
    "parent_preflight_counter",
    parent_preflight_head,
    envir = asNamespace("utils")
  )
  registerS3method(
    "collect",
    "parent_preflight_counter",
    parent_preflight_collect,
    envir = asNamespace("dplyr")
  )
  source <- dtplyr::lazy_dt(data.frame(group = "x", value = 1))
  class(source) <- c("parent_preflight_counter", class(source))

  parent_preflight_capture$n <- 0L
  expect_error(
    summarize_with_margins(
      source,
      total = sum(value),
      share = 100 * share_of_parent(total),
      .grouping = rollup(group)
    ),
    "complete right-hand side"
  )
  expect_identical(parent_preflight_capture$n, 0L)

  expect_error(
    summarize_with_margins(
      source,
      total = sum(value),
      share = share_of_parent(total),
      .grouping = cube(group)
    ),
    "requires `.grouping` to be one pure `rollup\\(\\)`"
  )
  expect_identical(parent_preflight_capture$n, 0L)

  query <- summarize_with_margins(
    source,
    total = sum(value),
    share = share_of_parent(total),
    .grouping = rollup(group),
    .margin_label = NULL
  )
  expect_s3_class(query, "dtplyr_step")
  expect_identical(parent_preflight_capture$n, 1L)
  result <- dplyr::collect(query)
  expect_setequal(result$share, c(1, 1))
})
