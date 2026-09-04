# Margin labels through the public verbs. ADR 0012 owns the contract separating
# a factor's NA level from a missing Margin value, which these assert.

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

# The eight rows of ADR 0012's table, in its order. Shared rather than written
# per backend: what the dtplyr test below asserts is that the contract does not
# depend on which backend the call was handed (#408), and a second copy of the
# table could only weaken that.
factor_contract_cases <- function() {
  data.frame(
    label = c(rep("NA", 4L), rep("NULL", 4L)),
    na_level = rep(c(TRUE, TRUE, FALSE, FALSE), 2L),
    missing_value = rep(c(TRUE, FALSE, TRUE, FALSE), 2L),
    # Row 3 joins row 7 under ADR 0012's amendment: both spellings of a
    # typed-missing label produce the same displayed result on a column
    # holding a missing value, so neither is refused for producing it.
    errors = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
}

test_that("factor NA levels and missing values obey the eight-case contract", {
  cases <- factor_contract_cases()

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

# The same eight rows on the one backend that can lose one of them. The local
# branches are combined by `bind_rows()`, which keeps a declared NA level;
# dtplyr combines them with `data.table`'s rbind, which drops one outright and
# turns the values that used it into missing codes, so the row that preserves
# an NA level under a typed-missing label went missing there with no diagnostic
# (#408). dtplyr is where this is asserted because it is the only backend that
# preserves such a level at all -- a plain duckdb round trip already drops it
# and arrow refuses the column -- so it is the only one ADR 0012's "local and
# factor-preserving lazy adapters" clause reaches.
#
# Each case is compared against the local result rather than against a literal,
# because what the contract says is that the two agree.
# A result's rows in an order both backends produce. Two are compared here and
# they place a missing value within one grouping set differently -- the local
# branch puts it last and `data.table` puts it first -- which is an ordering
# property ADR 0018 leaves to the backend and not what these cases assert.
#
# The integer code is what is compared rather than the displayed value: it is
# the only reading that separates a value on an NA level, which has one, from
# the typed missing a margin row carries, which has none.
factor_contract_rows <- function(result) {
  rows <- data.frame(
    code = as.integer(result$group),
    bit = result$bit,
    n = result$n
  )
  rows <- rows[order(rows$code, rows$bit, rows$n, na.last = TRUE), ]
  row.names(rows) <- NULL
  rows
}

test_that("dtplyr obeys the eight-case contract as the local backend does", {
  skip_if_suggest_absent("dtplyr")
  cases <- factor_contract_cases()

  for (i in seq_len(nrow(cases))) {
    case <- cases[i, ]
    data <- factor_contract_data(case$na_level, case$missing_value)
    label <- if (case$label == "NA") NA_character_ else NULL
    info <- paste(case, collapse = "/")
    operation <- function(input) {
      summarize_with_margins(
        input,
        n = dplyr::n(),
        bit = grouping_bit(group),
        .grouping = rollup(group),
        .margin_label = label
      )
    }

    if (case$errors) {
      error <- expect_error(operation(dtplyr::lazy_dt(data)), info = info)
      # The same assertion the local loop makes, so that the two error rows
      # are compared across backends as the six allowed ones are.
      expect_match(
        deparse1(conditionCall(error)),
        "summarize_with_margins",
        fixed = TRUE
      )
      next
    }

    result <- dplyr::collect(operation(dtplyr::lazy_dt(data)))
    expected <- operation(data)
    expect_s3_class(result$group, "factor")
    expect_identical(levels(result$group), levels(data$group), info = info)
    expect_identical(
      levels(result$group),
      levels(expected$group),
      info = info
    )
    expect_identical(
      factor_contract_rows(result),
      factor_contract_rows(expected),
      info = info
    )
  }
})

# The eight cases above declare an NA level that no value uses, so they assert
# the level survives and not that a value on it stays distinguishable from the
# typed missing a margin row carries. That distinction is the whole of what
# ADR 0012 separates, and it is what an encoding that merely restored the
# levels would lose: `as.character()` spells both as `NA`.
test_that("dtplyr keeps a used NA level apart from a typed missing", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(
    group = structure(
      c(1L, 2L),
      levels = c("x", NA_character_),
      class = "factor"
    ),
    value = 1:2
  )

  # Every Margin verb, because the branch union they share is where the level
  # was lost; the Grouping set identifier is what says which rows a branch
  # stands for, since the displayed value cannot.
  results <- list(
    summarize_with_margins = dplyr::collect(summarize_with_margins(
      dtplyr::lazy_dt(data),
      n = dplyr::n(),
      .grouping = rollup(group),
      .margin_label = NULL,
      .id = "set"
    )),
    expand_with_margins = dplyr::collect(expand_with_margins(
      dtplyr::lazy_dt(data),
      .grouping = rollup(group),
      .margin_label = NULL,
      .id = "set"
    )),
    nest_with_margins = dplyr::collect(nest_with_margins(
      dtplyr::lazy_dt(data),
      .grouping = rollup(group),
      .margin_label = NULL,
      .id = "set"
    )),
    # Local already: this verb returns a row-wise result rather than a query.
    nest_by_with_margins = nest_by_with_margins(
      dtplyr::lazy_dt(data),
      .grouping = rollup(group),
      .margin_label = NULL,
      .id = "set"
    )
  )

  for (verb in names(results)) {
    result <- results[[verb]]
    expect_identical(levels(result$group), c("x", NA), info = verb)
    source_rows <- result[result$set == 1L, , drop = FALSE]
    margin_rows <- result[result$set == 2L, , drop = FALSE]
    # The source row that uses the NA level prints as `<NA>` while `is.na()`
    # is false; every margin row is a typed missing, where it is true.
    expect_identical(
      sort(as.integer(source_rows$group)),
      c(1L, 2L),
      info = verb
    )
    expect_false(any(is.na(source_rows$group)), info = verb)
    expect_true(all(is.na(margin_rows$group)), info = verb)
  }
})

# A factor column that is not a Margin dimension crosses the same branch union
# and loses the same declared NA level there (#415). Neither a fixed `.by` key
# nor a passed-through column is labelled, so each takes the route #408 opened
# for a dimension whose Margin label is missing, and the set `factor_info`
# names widens from the Margin dimensions to the factor columns crossing the
# union.
na_level_carried_data <- function(ordered = FALSE) {
  carried_class <- if (ordered) c("ordered", "factor") else "factor"
  data.frame(
    key = structure(
      c(1L, 2L),
      levels = c("p", NA_character_),
      class = "factor"
    ),
    passthrough = structure(
      c(1L, 2L),
      levels = c("a", NA_character_),
      class = carried_class
    ),
    plain = factor(c("q", "r")),
    group = factor(c("g1", "g2")),
    value = 1:2
  )
}

# Compared against the local result the way the #408 cases are, and by integer
# code for the reason `factor_contract_rows()` gives: a value on the NA level
# and a typed missing are one displayed value and two codes. Sorted because
# ADR 0018 leaves within-set row order to the backend.
expect_passthrough_agrees <- function(result, expected) {
  expect_identical(levels(result$passthrough), c("a", NA))
  expect_identical(levels(result$passthrough), levels(expected$passthrough))
  expect_identical(
    sort(as.integer(result$passthrough)),
    sort(as.integer(expected$passthrough))
  )
  expect_false(any(is.na(result$passthrough)))
  expect_identical(
    any(is.na(result$passthrough)),
    any(is.na(expected$passthrough))
  )
}

test_that("dtplyr keeps a used NA level on a fixed .by key", {
  skip_if_suggest_absent("dtplyr")
  data <- na_level_carried_data()
  operation <- function(input) {
    summarize_with_margins(
      input,
      n = dplyr::n(),
      .by = key,
      .grouping = rollup(group)
    )
  }

  result <- dplyr::collect(operation(dtplyr::lazy_dt(data)))
  expected <- operation(data)
  expect_s3_class(result$key, "factor")
  expect_identical(levels(result$key), c("p", NA))
  expect_identical(levels(result$key), levels(expected$key))
  expect_identical(
    sort(as.integer(result$key)),
    sort(as.integer(expected$key))
  )
  # Every row of both results holds a value on the NA level, so `is.na()` is
  # false throughout: the key is never a margin row's typed missing.
  expect_false(any(is.na(result$key)))
  expect_identical(any(is.na(result$key)), any(is.na(expected$key)))
})

test_that("dtplyr keeps a used NA level on a passed-through column", {
  skip_if_suggest_absent("dtplyr")
  data <- na_level_carried_data()
  operation <- function(input) {
    expand_with_margins(input, .grouping = rollup(group))
  }

  result <- dplyr::collect(operation(dtplyr::lazy_dt(data)))
  expected <- operation(data)
  expect_s3_class(result$passthrough, "factor")
  expect_passthrough_agrees(result, expected)
})

test_that("a passed-through ordered factor keeps its ordering", {
  skip_if_suggest_absent("dtplyr")
  data <- na_level_carried_data(ordered = TRUE)
  operation <- function(input) {
    expand_with_margins(input, .grouping = rollup(group))
  }

  result <- dplyr::collect(operation(dtplyr::lazy_dt(data)))
  expected <- operation(data)
  expect_s3_class(result$passthrough, "ordered")
  expect_s3_class(expected$passthrough, "ordered")
  expect_passthrough_agrees(result, expected)
})

test_that("a Margin label position adds no level to a carried column", {
  skip_if_suggest_absent("dtplyr")
  data <- na_level_carried_data()

  for (position in c("first", "last")) {
    result <- dplyr::collect(expand_with_margins(
      dtplyr::lazy_dt(data),
      .grouping = rollup(group),
      .margin_label = "All",
      .margin_label_position = position
    ))
    # The dimension takes the label at the requested end; neither carried
    # column takes one at either, so their levels are what the input declared.
    expect_identical(
      levels(result$group),
      if (identical(position, "first")) {
        c("All", "g1", "g2")
      } else {
        c("g1", "g2", "All")
      },
      info = position
    )
    expect_identical(levels(result$key), c("p", NA), info = position)
    expect_identical(levels(result$passthrough), c("a", NA), info = position)
  }
})

# The route a carried column takes is unobservable in a result that has taken
# it correctly: a factor with no NA level is rebuilt on the levels it already
# had. So what the acceptance asks for -- that such a column is not encoded to
# character and not rebuilt -- is asserted where the decision is made.
test_that("a carried factor with no NA level takes no encode route", {
  skip_if_suggest_absent("dtplyr")
  data <- na_level_carried_data()
  proxy <- grouping_selection_proxy(dtplyr::lazy_dt(data))
  info <- margin_column_info(
    proxy,
    dimensions = "group",
    carried = c("key", "passthrough", "plain"),
    backend = grouping_backend(dtplyr::lazy_dt(data))
  )

  encode <- vapply(info$factors, function(x) x$encode_missing_label, logical(1))
  names(encode) <- vapply(info$factors, function(x) x$col, character(1))
  expect_setequal(names(encode), c("group", "key", "passthrough", "plain"))
  # `plain` is the carried column the criterion is about: a factor with no NA
  # level.
  expect_false(encode[["plain"]])
  expect_false(encode[["group"]])
  expect_true(encode[["key"]])
  expect_true(encode[["passthrough"]])
  # Prototypes stand for the value an omitted dimension writes, and only a
  # dimension is ever omitted, so widening the factor read leaves them alone.
  expect_identical(names(info$prototypes), "group")
})

# A nesting verb folds every column but the grouping columns into a cell, so a
# carried column crosses the union like any other and is inside the cell by the
# time the finalizer runs (#421). `.keep = TRUE` adds a second site: it copies
# each grouping column under an internal name before the union, so the outer
# column and its own nested copy can disagree about the level.
#
# A fixture of its own rather than `na_level_carried_data()`, which holds a
# column named `key`: a cell is built by `pick(everything())`, which dtplyr
# translates to a `data.table()` call, so that name arrives as that function's
# `key` argument and the call fails. Nothing here is marginplyr's -- the same
# `summarize(list(pick(everything())))` on a bare `lazy_dt()` fails the same
# way -- and it is filed as #424.
na_level_nested_data <- function(ordered = FALSE, na_level_group = FALSE) {
  carried_class <- if (ordered) c("ordered", "factor") else "factor"
  data.frame(
    group = if (na_level_group) {
      structure(c(1L, 2L), levels = c("g1", NA_character_), class = "factor")
    } else {
      factor(c("g1", "g2"))
    },
    passthrough = structure(
      c(1L, 2L),
      levels = c("a", NA_character_),
      class = carried_class
    ),
    plain = factor(c("q", "r")),
    value = 1:2
  )
}

# Rows are one per Grouping set member and ADR 0018 leaves their order to the
# backend, so the cells are paired by the outer column's integer code, which is
# distinct per row in every case below. Reading the code rather than the
# displayed value is also what keeps a value on the NA level apart from a
# margin row's typed missing, which `as.character()` spells the same way.
nest_cells <- function(result, outer, .key = "data") {
  lapply(result[[.key]][order(as.integer(result[[outer]]))], as.data.frame)
}

# By integer code for the reason `expect_passthrough_agrees()` gives, with
# `na.last` because `sort()` drops the missing a lost level leaves behind --
# the one difference the assertion exists to catch.
expect_cell_column_agrees <- function(result,
                                      expected,
                                      outer,
                                      col,
                                      expected_levels) {
  result_cells <- nest_cells(result, outer)
  expected_cells <- nest_cells(expected, outer)
  expect_identical(length(result_cells), length(expected_cells))
  for (i in seq_along(result_cells)) {
    got <- result_cells[[i]][[col]]
    want <- expected_cells[[i]][[col]]
    expect_identical(levels(got), expected_levels, info = as.character(i))
    expect_identical(levels(got), levels(want), info = as.character(i))
    expect_identical(
      sort(as.integer(got), na.last = TRUE),
      sort(as.integer(want), na.last = TRUE),
      info = as.character(i)
    )
  }
}

test_that("dtplyr keeps a used NA level on a nested payload column", {
  skip_if_suggest_absent("dtplyr")
  data <- na_level_nested_data()
  operation <- function(input) {
    nest_with_margins(input, .grouping = rollup(group))
  }

  result <- dplyr::collect(operation(dtplyr::lazy_dt(data)))
  expected <- operation(data)
  expect_cell_column_agrees(
    result,
    expected,
    "group",
    "passthrough",
    c("a", NA)
  )
  # `plain` is the payload column with no NA level: rebuilding it takes nothing
  # away, which is what keeps the widened route from costing the common case.
  expect_cell_column_agrees(result, expected, "group", "plain", c("q", "r"))
})

test_that("a nested payload ordered factor keeps its ordering", {
  skip_if_suggest_absent("dtplyr")
  data <- na_level_nested_data(ordered = TRUE)
  operation <- function(input) {
    nest_with_margins(input, .grouping = rollup(group))
  }

  result <- dplyr::collect(operation(dtplyr::lazy_dt(data)))
  expected <- operation(data)
  expect_s3_class(nest_cells(result, "group")[[1L]]$passthrough, "ordered")
  expect_cell_column_agrees(
    result,
    expected,
    "group",
    "passthrough",
    c("a", NA)
  )
})

test_that("nest_by_with_margins keeps a used NA level in its cell", {
  skip_if_suggest_absent("dtplyr")
  data <- na_level_nested_data()
  operation <- function(input) {
    nest_by_with_margins(input, .grouping = rollup(group))
  }

  # `nest_by_with_margins()` collects, so both results are already local.
  result <- operation(dtplyr::lazy_dt(data))
  expected <- operation(data)
  expect_cell_column_agrees(
    result,
    expected,
    "group",
    "passthrough",
    c("a", NA)
  )
})

test_that(".keep = TRUE keeps a used NA level on a nested grouping copy", {
  skip_if_suggest_absent("dtplyr")
  data <- na_level_nested_data(na_level_group = TRUE)
  operation <- function(input) {
    nest_with_margins(input, .grouping = rollup(group), .keep = TRUE)
  }

  result <- dplyr::collect(operation(dtplyr::lazy_dt(data)))
  expected <- operation(data)
  # The outer column takes the Margin label. Its nested copy holds the source
  # value the branch was built from, so it takes none and is rebuilt on the
  # levels the input declared.
  expect_identical(levels(result$group), c("g1", NA, "Total"))
  expect_identical(levels(result$group), levels(expected$group))
  expect_cell_column_agrees(result, expected, "group", "group", c("g1", NA))
  expect_cell_column_agrees(
    result,
    expected,
    "group",
    "passthrough",
    c("a", NA)
  )
})

test_that("NA factor levels stay structural when collision checks are off", {
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

  expect_error(operation(c(first = "All")), "Missing `second`")
  expect_error(
    operation(c(first = "All", second = "All", unknown = "All")),
    "unknown dimension name:\ni `unknown`"
  )
  expect_error(
    operation(c(fixed = "All", first = "All", second = "All")),
    "fixed `.by` column:\ni `fixed`"
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

test_that("factor collisions include unused levels and stay column-specific", {
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
    "grouping column:\ni `first`"
  )
  expect_match(
    deparse1(conditionCall(error)),
    "summarize_with_margins",
    fixed = TRUE
  )

  both <- expect_error(
    summarize_with_margins(
      data,
      n = dplyr::n(),
      .grouping = rollup(first, second),
      .margin_label = c(first = "All first", second = "All second")
    ),
    "grouping columns:\ni `first` and `second`"
  )
  expect_match(conditionMessage(both), "are already factor levels")
})

# The other half of the check reads the data and is what `.check_margin_label`
# opts into; this half reads the levels marginplyr already holds, so there is
# nothing to opt into and the argument does not reach it (ADR 0020).
test_that("a declared collision is rejected however the label check is set", {
  data <- data.frame(
    group = factor(c("a", "b"), levels = c("a", "b", "All")),
    value = 1:2
  )

  for (check in list(TRUE, FALSE)) {
    error <- expect_error(
      summarize_with_margins(
        data,
        n = dplyr::n(),
        .grouping = rollup(group),
        .margin_label = "All",
        .check_margin_label = check
      ),
      "already a factor level in grouping column:\ni `group`",
      fixed = TRUE
    )
    expect_s3_class(error, "marginplyr_error")
    # Turning the read off is not a remedy for a collision no read found, so
    # the diagnostic must not send a caller to that argument.
    expect_no_match(
      conditionMessage(error),
      ".check_margin_label",
      fixed = TRUE
    )
  }
})

test_that("a check with no column left to read contacts nothing", {
  factor_info <- list(
    list(
      col = "group",
      levels = c("a", "b"),
      ordered = FALSE,
      has_na_in_level = FALSE,
      preserve_missing_value = TRUE
    )
  )
  # A sentinel with no dplyr methods: any attempt to read it fails rather than
  # aggregating a set of constants, which is what a factor-only check used to
  # send to a lazy backend.
  unreadable <- structure(list(), class = "marginplyr_unreadable_input")

  expect_error(dplyr::select(unreadable, dplyr::all_of("group")))
  expect_no_error(check_observed_label_collision(
    unreadable,
    margin_labels = list(group = "All"),
    factor_info = factor_info
  ))
  expect_no_error(check_observed_label_collision(
    unreadable,
    margin_labels = list(group = NULL),
    factor_info = list()
  ))
  # Nor does the other spelling of a typed-missing label: whether the column
  # holds a missing value is no longer asked, so neither reaches the read that
  # this unreadable column would fail.
  expect_no_error(check_observed_label_collision(
    unreadable,
    margin_labels = list(group = NA_character_),
    factor_info = factor_info
  ))
})

test_that("dtplyr rejects a declared collision and stays silent on a value", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(
    declared = factor(c("a", "b"), levels = c("a", "b", "Total")),
    observed = c("Total", "x"),
    value = 1:2
  )

  # `.check_margin_label` defaults to `FALSE` here, because the input is lazy.
  error <- expect_error(
    summarize_with_margins(
      dtplyr::lazy_dt(data),
      n = dplyr::n(),
      .grouping = rollup(declared)
    ),
    "already a factor level in grouping column:\ni `declared`",
    fixed = TRUE
  )
  expect_s3_class(error, "marginplyr_error")

  expect_no_error(
    query <- summarize_with_margins(
      dtplyr::lazy_dt(data),
      n = dplyr::n(),
      bit = grouping_bit(observed),
      .grouping = rollup(observed)
    )
  )
  result <- dplyr::collect(query)
  colliding <- result[result$observed == "Total", , drop = FALSE]
  expect_identical(nrow(colliding), 2L)
  expect_setequal(colliding$bit, c(0L, 1L))

  expect_error(
    summarize_with_margins(
      dtplyr::lazy_dt(data),
      n = dplyr::n(),
      .grouping = rollup(observed),
      .check_margin_label = TRUE
    ),
    "already present in grouping column:\ni `observed`",
    fixed = TRUE
  )
})

# The reproduction #122 was filed with, on the backend it was filed against.
# DuckDB carries a factor as an `ENUM`, so its levels arrive through the
# zero-row read ADR 0020 exempts rather than as a factor column, which is a
# different route to the same rejection than the one dtplyr takes above.
test_that("DuckDB rejects a declared collision without being asked", {
  skip_if_suggest_absent("duckdb", "DBI")

  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    data.frame(
      g = factor(c("a", "(all)", "b"), levels = c("a", "(all)", "b")),
      v = c(1, 2, 3)
    ),
    "margin_label_declared",
    overwrite = TRUE,
    temporary = TRUE
  )

  expect_error(
    summarize_with_margins(
      remote,
      t = sum(v, na.rm = TRUE),
      .grouping = rollup(g),
      .margin_label = "(all)"
    ),
    "already a factor level in grouping column:\ni `g`",
    fixed = TRUE
  )

  # A label that collides with nothing leaves the genuine level where it was.
  result <- dplyr::collect(summarize_with_margins(
    remote,
    t = sum(v, na.rm = TRUE),
    .grouping = rollup(g),
    .margin_label = "Total"
  ))
  expect_identical(levels(result$g), c("a", "(all)", "b", "Total"))
})

# The silence is the contract, so it is asserted rather than left to the
# absence of a failing expectation: a later change to `.check_margin_label`'s
# default has to fail here instead of passing quietly. SQLite is where the
# whole collision is observed -- it carries no factor type, so the level that
# is declared in the source data frame reaches the database as text and the
# check above it has nothing to read the collision off.
test_that("RSQLite leaves an observed collision silent until it is asked", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    data.frame(
      group = factor(c("Total", "x"), levels = c("Total", "x")),
      value = 1:2
    ),
    "margin_label_silence",
    temporary = TRUE
  )

  expect_no_error(
    query <- summarize_with_margins(
      remote,
      total = sum(value, na.rm = TRUE),
      bit = grouping_bit(group),
      .grouping = rollup(group)
    )
  )
  result <- dplyr::collect(query)
  colliding <- result[result$group == "Total", , drop = FALSE]

  # What the silence costs: two rows the grouping column cannot tell apart,
  # and a Grouping bit that can.
  expect_identical(nrow(colliding), 2L)
  expect_setequal(colliding$bit, c(0L, 1L))
  expect_setequal(as.numeric(colliding$total), c(1, 3))

  expect_error(
    summarize_with_margins(
      remote,
      total = sum(value, na.rm = TRUE),
      .grouping = rollup(group),
      .check_margin_label = TRUE
    ),
    "already present in grouping column:\ni `group`",
    fixed = TRUE
  )
})

test_that("dtplyr applies mixed named labels lazily and restores factors", {
  skip_if_suggest_absent("dtplyr")
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

test_that("dtplyr expansion verbs restore ordered factor dimensions", {
  skip_if_suggest_absent("dtplyr")
  # Two rows are load-bearing: data.table replaces a one-row factor column but
  # recycles a scalar assignment into a multi-row one without replacing it.
  data <- data.frame(
    group = ordered(c("small", "large"), levels = c("small", "large")),
    value = 1:2
  )
  source <- dtplyr::lazy_dt(data)
  lazy <- list(
    expand_with_margins = expand_with_margins(
      source,
      .grouping = rollup(group),
      .margin_label = "(all)",
      .margin_label_position = "first"
    ),
    nest_with_margins = nest_with_margins(
      source,
      .grouping = rollup(group),
      .margin_label = "(all)",
      .margin_label_position = "first"
    )
  )

  results <- lapply(names(lazy), function(verb) {
    expect_s3_class(lazy[[verb]], "dtplyr_step")
    dplyr::collect(lazy[[verb]])
  })
  names(results) <- names(lazy)
  results$nest_by_with_margins <- nest_by_with_margins(
    source,
    .grouping = rollup(group),
    .margin_label = "(all)",
    .margin_label_position = "first"
  )

  for (verb in names(results)) {
    result <- results[[verb]]
    expect_true(is.ordered(result$group), info = verb)
    expect_identical(
      levels(result$group),
      c("(all)", "small", "large"),
      info = verb
    )
    expect_setequal(
      as.character(result$group),
      c("small", "large", "(all)")
    )
  }
})

test_that("Arrow applies mixed named labels lazily with typed missing values", {
  skip_if_suggest_absent("arrow")
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
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
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
      message = "must not name fixed `\\.by` column:\ni `fixed`"
    ),
    list(
      label = c(first = "All", second = "All", unknown = "All"),
      message = "unknown dimension name:\ni `unknown`"
    ),
    list(
      label = c(first = "All"),
      message = "must name every Margin dimension\\.\ni Missing `second`"
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

# A per-dimension `NULL` (#371). `c()` drops a `NULL` element before
# `.margin_label` is seen, so the list is the only spelling that carries one.
test_that("a named list carries a per-dimension NULL", {
  data <- data.frame(
    region = c("E", "E", "W"),
    store = c("a", NA, "b"),
    value = 1:3
  )

  result <- summarize_with_margins(
    data,
    n = sum(value),
    region_bit = grouping_bit(region),
    store_bit = grouping_bit(store),
    .grouping = rollup(region, store),
    .margin_label = list(region = "All", store = NULL)
  )

  # The one report the vector cannot express: `region` labelled, `store` typed
  # missing, and the check left on.
  expect_identical(result$region[result$region_bit == 1L], "All")
  expect_true(all(is.na(result$store[result$store_bit == 1L])))
  expect_false(any(result$store %in% "All", na.rm = TRUE))

  # What `store_bit` is carrying: the source missing value and the margin over
  # it display identically, which is the ambiguity `NULL` has always been
  # allowed to produce.
  ambiguous <- result[result$region == "E" & is.na(result$store), ]
  expect_identical(nrow(ambiguous), 2L)
  expect_setequal(ambiguous$store_bit, c(0L, 1L))
})

test_that("a named list means what the equivalent character vector means", {
  data <- data.frame(
    first = c("a", "b"),
    second = c("x", "y"),
    value = 1:2
  )
  operation <- function(label) {
    summarize_with_margins(
      data,
      n = sum(value),
      id = grouping_id(first, second),
      .grouping = rollup(first, second),
      .margin_label = label
    )
  }

  expect_identical(
    operation(list(first = "All first", second = "All second")),
    operation(c(first = "All first", second = "All second"))
  )
})

test_that("a named list refuses an element that is not a label", {
  data <- data.frame(first = c("a", "b"), second = c("x", "y"), value = 1:2)
  operation <- function(label) {
    summarize_with_margins(
      data,
      n = sum(value),
      .grouping = rollup(first, second),
      .margin_label = label
    )
  }

  cases <- list(
    list(
      label = list(first = "All", second = 1L),
      message = "must each be `NULL` or a character scalar:\ni `second`"
    ),
    list(
      label = list(first = "All", second = c("x", "y")),
      message = "must each be `NULL` or a character scalar:\ni `second`"
    ),
    list(
      label = list("All", "Total"),
      message = "must be `NULL`, an unnamed character scalar"
    ),
    # A one-row data frame is a named list whose columns are character
    # scalars, so it is refused for its shape rather than read as labels.
    list(
      label = data.frame(first = "All", second = "Total"),
      message = "must be `NULL`, an unnamed character scalar"
    )
  )

  for (case in cases) {
    error <- expect_error(operation(case$label), case$message, fixed = TRUE)
    expect_s3_class(error, "marginplyr_error")
  }
})

# The name rules are the vector's rules, so a list reaches the same refusals
# rather than a second set. An unnamed dimension stays a refusal: read as
# `NULL` it would turn a misspelled name into a silent whole-call change.
test_that("a named list reaches the vector's name refusals", {
  data <- data.frame(
    fixed = c("k", "k"),
    first = c("a", "b"),
    second = c("x", "y"),
    value = 1:2
  )
  operation <- function(label) {
    summarize_with_margins(
      data,
      n = sum(value),
      .by = fixed,
      .grouping = rollup(first, second),
      .margin_label = label
    )
  }

  expect_error(
    operation(list(first = "All", second = NULL, fixed = NULL)),
    "must not name fixed `.by` column:\ni `fixed`",
    fixed = TRUE
  )
  expect_error(
    operation(list(first = "All", second = NULL, unknown = NULL)),
    "unknown dimension name:\ni `unknown`",
    fixed = TRUE
  )
  expect_error(
    operation(list(first = "All")),
    "must name every Margin dimension.\ni Missing `second`",
    fixed = TRUE
  )
})

# ADR 0012's amendment: a typed-missing label displays as missing wherever the
# column already holds missing values, which `NULL` has always been allowed to
# do, so `NA_character_` is not refused for doing the same.
test_that("a typed-missing label is not an observed collision", {
  data <- data.frame(
    region = c("E", "E", "W"),
    store = c("a", NA, "b"),
    value = 1:3
  )
  operation <- function(label) {
    summarize_with_margins(
      data,
      n = sum(value),
      .grouping = rollup(region, store),
      .margin_label = label,
      .check_margin_label = TRUE,
      .sort = "last"
    )
  }

  expect_no_error(operation(c(region = "All", store = NA_character_)))
  expect_identical(
    operation(c(region = "All", store = NA_character_)),
    operation(list(region = "All", store = NULL))
  )
})

test_that("an all-typed-missing label reads no column", {
  data <- data.frame(group = c("a", NA), value = 1:2)
  labels <- list(NA_character_, NULL)

  for (label in labels) {
    expect_no_error(check_observed_label_collision(
      # A column this cannot select is what proves nothing was selected: a
      # check that read anything here would fail on the missing column.
      dplyr::select(data, "value"),
      margin_labels = list(group = label),
      factor_info = list()
    ))
  }
})

test_that("a non-missing label is still an observed collision", {
  data <- data.frame(group = c("All", "b"), value = 1:2)

  error <- expect_error(
    summarize_with_margins(
      data,
      n = sum(value),
      .grouping = rollup(group),
      .margin_label = "All"
    ),
    "already present in grouping column:\ni `group`",
    fixed = TRUE
  )
  expect_match(conditionMessage(error), "`.check_margin_label = FALSE`")
})

# The remedy has to be one. A bare `NULL` is the whole of `.margin_label`, so
# it was a remedy only where there was a single dimension.
test_that("the NA-level refusal names a remedy a caller can write", {
  status <- structure(
    c(1L, 2L, NA_integer_),
    levels = c("kept", NA_character_),
    class = "factor"
  )
  data <- data.frame(region = c("E", "E", "W"), status = status, value = 1:3)
  operation <- function(label) {
    summarize_with_margins(
      data,
      n = sum(value),
      .grouping = rollup(region, status),
      .margin_label = label
    )
  }

  error <- expect_error(
    operation(c(region = "All", status = NA_character_)),
    "already a factor level in grouping column:\ni `status`",
    fixed = TRUE
  )
  expect_match(conditionMessage(error), "list", fixed = TRUE)

  # Following the remedy has to work with more than one dimension in play.
  expect_no_error(operation(list(region = "All", status = NULL)))
})
