# ADR 0013 owns the decision that a Grouping plan is inspected as an ordinary
# tibble, which is what these assert through `inspect_grouping()`.

lazy_grouping_join_fixture <- function() {
  data <- data.frame(
    year = 2026L,
    region = "East",
    store = "A",
    revenue = 1
  )
  remote <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())
  list(
    remote = remote,
    report = summarize_with_margins(
      remote,
      revenue = sum(.data[["revenue"]], na.rm = TRUE),
      .by = dplyr::all_of("year"),
      .grouping = rollup(dplyr::all_of(c("region", "store"))),
      .id = "set"
    )
  )
}

test_that("inspect_grouping describes a rollup in Grouping plan order", {
  data <- data.frame(
    fixed = 1L,
    region = "East",
    store = "A"
  )

  result <- inspect_grouping(
    data,
    .by = fixed,
    .grouping = rollup(region, store),
    .format = c(label = "text")
  )

  expect_identical(
    result,
    tibble::tibble(
      set_id = 1:3,
      fixed = rep("(fixed)", 3L),
      included = c("(region, store)", "(region)", "()"),
      omitted = c("()", "(store)", "(region, store)"),
      grouping_bits = c(
        "region=0, store=0",
        "region=0, store=1",
        "region=1, store=1"
      ),
      grouping_id = c(0L, 1L, 3L)
    )
  )
  expect_identical(class(result), c("tbl_df", "tbl", "data.frame"))
  expect_identical(dplyr::group_vars(result), character())
})

test_that("list inspection preserves exact non-syntactic dimension names", {
  data <- data.frame(
    "region, west" = "West",
    "store=number" = "A",
    check.names = FALSE
  )

  result <- inspect_grouping(
    data,
    .grouping = rollup(`region, west`, `store=number`),
    .format = c(label = "list")
  )

  expect_identical(
    result$fixed,
    rep(list(character()), 3L)
  )
  expect_identical(
    result$included,
    list(
      c("region, west", "store=number"),
      "region, west",
      character()
    )
  )
  expect_identical(
    result$omitted,
    list(
      character(),
      "store=number",
      c("region, west", "store=number")
    )
  )
  expect_identical(
    result$grouping_bits,
    list(
      c("region, west" = 0L, "store=number" = 0L),
      c("region, west" = 0L, "store=number" = 1L),
      c("region, west" = 1L, "store=number" = 1L)
    )
  )
})

test_that("inspection preserves Grouping bits at integer-mask boundaries", {
  empty_text <- inspect_grouping(data.frame(value = 1L))
  empty_list <- inspect_grouping(
    data.frame(value = 1L),
    .format = "list"
  )

  expect_identical(empty_text$included, "()")
  expect_identical(empty_text$omitted, "()")
  expect_identical(empty_text$grouping_bits, "()")
  expect_identical(empty_text$grouping_id, 0L)
  expect_identical(empty_list$included, list(character()))
  expect_identical(empty_list$omitted, list(character()))
  expect_identical(empty_list$grouping_bits, list(integer()))

  one <- data.frame(dimension = "x")
  one_result <- inspect_grouping(
    one,
    .grouping = rollup(dimension),
    .format = "list"
  )
  expect_identical(one_result$grouping_id, c(0L, 1L))
  expect_identical(
    one_result$grouping_bits,
    list(c(dimension = 0L), c(dimension = 1L))
  )

  names_31 <- paste0("dimension_", seq_len(31L))
  data_31 <- stats::setNames(
    as.data.frame(as.list(rep("x", 31L))),
    names_31
  )
  result_31 <- inspect_grouping(
    data_31,
    .grouping = rollup(dplyr::all_of(names_31)),
    .format = "list"
  )
  expect_identical(result_31$grouping_id[[1L]], 0L)
  expect_identical(result_31$grouping_id[[32L]], .Machine$integer.max)
  expect_length(result_31$grouping_bits[[32L]], 31L)

  names_32 <- paste0("dimension_", seq_len(32L))
  data_32 <- stats::setNames(
    as.data.frame(as.list(rep("x", 32L))),
    names_32
  )
  result_32 <- inspect_grouping(
    data_32,
    .grouping = rollup(dplyr::all_of(names_32)),
    .format = "list"
  )
  expect_identical(result_32$grouping_id, rep(NA_integer_, 33L))
  expect_length(result_32$grouping_bits[[33L]], 32L)
})

test_that("inspection applies duplicate policy before numbering occurrences", {
  data <- data.frame(
    first = "x",
    second = "y",
    value = 1L
  )
  specification <- grouping_sets(
    grouping_set(second),
    grouping_set(first, second),
    grouping_set(second),
    grouping_set()
  )

  expect_error(
    inspect_grouping(data, .grouping = specification),
    "Duplicate grouping sets"
  )

  dropped <- inspect_grouping(
    data,
    .grouping = specification,
    .duplicates = "drop",
    .format = "list"
  )
  kept <- inspect_grouping(
    data,
    .grouping = specification,
    .duplicates = "keep",
    .format = "list"
  )
  margin_result <- summarize_with_margins(
    data,
    n = dplyr::n(),
    .grouping = specification,
    .duplicates = "keep",
    .margin_label = NULL,
    .id = "occurrence"
  )

  expect_identical(dropped$set_id, 1:3)
  expect_identical(
    dropped$included,
    list("second", c("second", "first"), character())
  )
  expect_identical(kept$set_id, 1:4)
  expect_identical(
    kept$included,
    list(
      "second",
      c("second", "first"),
      "second",
      character()
    )
  )
  expect_identical(
    sort(margin_result$occurrence),
    kept$set_id
  )
})

test_that("duplicate-drop cubes retain descending size and combination order", {
  data <- data.frame(a = 1L, b = 2L, `a,b` = 3L, check.names = FALSE)
  specification <- cube(grouping_set(b, a), a, `a,b`, `a,b`)

  plan <- inspect_grouping(
    data, .grouping = specification, .duplicates = "drop", .format = "list"
  )

  expect_identical(plan$included, list(
    c("b", "a", "a,b"), c("a", "a,b"), c("b", "a"),
    "a,b", "a", character()
  ))
  expect_identical(plan$grouping_id, c(0L, 4L, 1L, 6L, 5L, 7L))
  expect_identical(plan$set_id, 1:6)
})

test_that("duplicate-drop nested products retain the first occurrence order", {
  data <- data.frame(a = 1L, b = 2L, c = 3L)
  specification <- grouping_spec(
    grouping_sets(grouping_set(b, a), grouping_set(a, b), grouping_set()),
    grouping_spec(
      grouping_sets(grouping_set(c), grouping_set()),
      grouping_sets(grouping_set(a), grouping_set())
    )
  )

  plan <- inspect_grouping(
    data, .grouping = specification, .duplicates = "drop", .format = "list"
  )

  expect_identical(plan$included, list(
    c("b", "a", "c"), c("b", "a"), c("a", "c"), "c", "a", character()
  ))
  expect_identical(plan$grouping_id, c(0L, 1L, 4L, 6L, 5L, 7L))
  expect_identical(plan$set_id, 1:6)
})

test_that("a fixed product factor preserves first occurrence dimensions", {
  data <- data.frame(a = 1L, b = 2L)
  specification <- grouping_spec(
    grouping_spec(grouping_sets(b, grouping_set()),
                  grouping_sets(a, grouping_set())),
    grouping_set(a, b)
  )
  plan <- inspect_grouping(
    data, .grouping = specification, .duplicates = "drop", .format = "list"
  )

  expect_identical(plan$included, list(c("b", "a")))
  expect_identical(plan$grouping_bits, list(c(b = 0L, a = 0L)))
  expect_identical(plan$set_id, 1L)
})

test_that("cube products retain first occurrence order and Grouping bits", {
  data <- data.frame(a = 1L, b = 2L, c = 3L, d = 4L, e = 5L)
  units <- list(c("b", "a"), c("a", "d"), c("e", "c"), c("b", "e"))
  alternatives <- list(c("d", "e"), "c", character())
  fixed <- list(c("a", "e"))
  unit_specs <- lapply(units, function(cols) {
    grouping_set(tidyselect::all_of(cols))
  })
  cube_spec <- cube(!!!unit_specs)
  choices <- grouping_sets(grouping_set(d, e), c, grouping_set())
  required <- grouping_set(a, e)

  # Enumerate every combination in descending cube size and Cartesian order.
  subsets <- do.call(c, lapply(length(units):0L, function(size) {
    utils::combn(seq_along(units), size, simplify = FALSE)
  }))
  memberships <- lapply(subsets, function(subset) {
    unique(c(character(), unlist(units[subset], use.names = FALSE)))
  })
  wrapped <- grouping_sets(e, cube_spec, grouping_sets(d))
  wrapped_memberships <- c(list("e"), memberships, list("d"))
  specs <- list(
    grouping_spec(cube_spec, grouping_spec(choices, required)),
    grouping_spec(grouping_spec(required, choices), cube_spec),
    grouping_spec(wrapped, choices, required),
    grouping_spec(required, choices, wrapped)
  )
  families <- list(
    list(memberships, alternatives, fixed),
    list(fixed, alternatives, memberships),
    list(wrapped_memberships, alternatives, fixed),
    list(fixed, alternatives, wrapped_memberships)
  )

  for (i in seq_along(specs)) {
    expanded <- Reduce(function(left, right) {
      unlist(lapply(left, function(prefix) {
        lapply(right, function(suffix) unique(c(prefix, suffix)))
      }), recursive = FALSE)
    }, families[[i]])
    dimensions <- unique(unlist(expanded, use.names = FALSE))
    included <- unique(lapply(expanded, function(set) {
      dimensions[dimensions %in% set]
    }))
    bits <- lapply(included, function(set) {
      stats::setNames(as.integer(!dimensions %in% set), dimensions)
    })
    plan <- inspect_grouping(
      data, .grouping = specs[[i]], .duplicates = "drop", .format = "list"
    )

    expect_identical(plan$included, included)
    expect_identical(plan$grouping_bits, bits)
    expect_identical(plan$set_id, seq_along(included))
  }
})

test_that("cube products preserve all occurrences for keep and error", {
  data <- data.frame(a = 1L, b = 2L)
  specs <- list(
    grouping_spec(cube(b, a), grouping_set(a, b)),
    grouping_spec(grouping_set(b, a), cube(a, b))
  )
  for (spec in specs) {
    kept <- inspect_grouping(
      data, .grouping = spec, .duplicates = "keep", .format = "list"
    )
    expect_identical(kept$included, rep(list(c("b", "a")), 4L))
    expect_identical(kept$set_id, 1:4)
    expect_error(
      inspect_grouping(data, .grouping = spec),
      "1, 2, 3, 4.", fixed = TRUE, class = "marginplyr_error"
    )
  }
})

test_that("duplicate-drop products support wide grouping sets", {
  dimensions <- paste0("dimension_", seq_len(10001L))
  data <- tibble::as_tibble(stats::setNames(rep(list(1L), 10001L), dimensions))
  part <- grouping_sets(grouping_set(everything()), grouping_set())
  plan <- inspect_grouping(
    data, .grouping = grouping_spec(part, part),
    .duplicates = "drop", .format = "list"
  )

  expect_identical(plan$included, list(dimensions, character()))
  expect_identical(plan$set_id, 1:2)
})

test_that("duplicate keep and error retain cube and product occurrences", {
  data <- data.frame(key = 1L)
  family <- grouping_sets(grouping_set(key), grouping_set())
  for (specification in list(cube(key, key), grouping_spec(family, family))) {
    kept <- inspect_grouping(
      data, .grouping = specification, .duplicates = "keep", .format = "list"
    )
    expect_identical(kept$included, list("key", "key", "key", character()))
    expect_identical(kept$set_id, 1:4)
    expect_error(
      inspect_grouping(data, .grouping = specification),
      "1, 2, 3.", fixed = TRUE, class = "marginplyr_error"
    )
  }
})

test_that("dropping repeated sets preserves caller selection order", {
  data <- data.frame(key = 1L)
  selected <- character()
  pick <- function(label) {
    selected <<- c(selected, label)
    "key"
  }
  predicate <- function(column) {
    selected <<- c(selected, "predicate")
    is.integer(column)
  }
  cube_spec <- cube(pick("cube"), tidyselect::where(predicate))
  required <- grouping_set(pick("required"))
  specs <- list(
    grouping_spec(cube_spec, required),
    grouping_spec(required, cube_spec)
  )
  orders <- list(
    c("cube", "predicate", "required"), c("required", "cube", "predicate")
  )

  for (i in seq_along(specs)) {
    selected <- character()
    plan <- inspect_grouping(
      data, .grouping = specs[[i]], .duplicates = "drop", .format = "list"
    )
    expect_identical(selected, orders[[i]])
    expect_identical(plan$included, list("key"))
  }
})

test_that("inspection shares grouped and row-wise input validation", {
  data <- data.frame(
    fixed = factor("x", levels = c("x", "unused")),
    dimension = "y"
  )
  grouped <- dplyr::group_by(data, fixed)

  implicit <- inspect_grouping(
    grouped,
    .grouping = rollup(dimension)
  )
  explicit <- inspect_grouping(
    dplyr::ungroup(grouped),
    .by = fixed,
    .grouping = rollup(dimension)
  )

  expect_identical(implicit, explicit)
  expect_error(
    inspect_grouping(
      grouped,
      .by = fixed,
      .grouping = rollup(dimension)
    ),
    "Can't supply `.by`"
  )
  expect_error(
    inspect_grouping(dplyr::rowwise(data)),
    "`rowwise\\(\\)` input is not supported"
  )
  expect_error(
    inspect_grouping(dplyr::group_by(data, fixed, .drop = FALSE)),
    "`.drop = FALSE` is not supported"
  )
})

renaming_grouping_data <- function() {
  data.frame(region = c("x", "y"), revenue = 1:2)
}

renaming_grouping_message <- function() {
  paste0(
    "Can't rename grouping dimension:\n",
    "i `area = region`.\n",
    "i Grouping dimensions must name existing columns."
  )
}

test_that("inspection and execution refuse a renaming selection alike", {
  data <- renaming_grouping_data()

  inspected <- expect_error(
    inspect_grouping(
      data,
      .grouping = rollup(tidyselect::all_of(c(area = "region")))
    )
  )
  expect_s3_class(inspected, "marginplyr_error")
  expect_identical(conditionMessage(inspected), renaming_grouping_message())
  expect_identical(
    rlang::call_name(conditionCall(inspected)),
    "inspect_grouping"
  )

  # Every Margin verb resolves the same specification, so each one reports what
  # the inspection verb reported. The calls are written out rather than built
  # from the verb names, so `codetools` can follow them.
  summarized <- expect_error(
    summarize_with_margins(
      data,
      revenue = sum(revenue),
      .grouping = rollup(tidyselect::all_of(c(area = "region")))
    )
  )
  expect_identical(conditionMessage(summarized), renaming_grouping_message())

  expanded <- expect_error(
    expand_with_margins(
      data,
      .grouping = rollup(tidyselect::all_of(c(area = "region")))
    )
  )
  expect_identical(conditionMessage(expanded), renaming_grouping_message())

  nested <- expect_error(
    nest_with_margins(
      data,
      .grouping = rollup(tidyselect::all_of(c(area = "region")))
    )
  )
  expect_identical(conditionMessage(nested), renaming_grouping_message())

  nested_by <- expect_error(
    nest_by_with_margins(
      data,
      .grouping = rollup(tidyselect::all_of(c(area = "region")))
    )
  )
  expect_identical(conditionMessage(nested_by), renaming_grouping_message())

  for (error in list(summarized, expanded, nested, nested_by)) {
    expect_s3_class(error, "marginplyr_error")
  }
})

test_that("a renaming selection resolved from typed metadata is refused", {
  # A selection carrying a predicate cannot be resolved from column names
  # alone, so it reaches the typed selection proxy rather than the name proxy
  # that rejects every other renaming selection before a backend is read.
  error <- expect_error(
    inspect_grouping(
      renaming_grouping_data(),
      .grouping = rollup(c(area = region, where(is.numeric)))
    )
  )
  expect_s3_class(error, "marginplyr_error")
  expect_identical(conditionMessage(error), renaming_grouping_message())
})

renaming_by_data <- function() {
  data.frame(region = c("x", "y"), area = c("p", "q"), revenue = 1:2)
}

renaming_by_message <- function() {
  paste0(
    "Can't rename `.by` column:\n",
    "i `area = region`.\n",
    "i Fixed `.by` keys must name existing columns."
  )
}

test_that("inspection and execution refuse a renaming .by alike", {
  # `area` is a column too, so the resolution this replaces reported the name
  # the caller wrote, fixed the plan on `area`, and partitioned every result by
  # a column the selection never named (#134).
  data <- renaming_by_data()

  inspected <- expect_error(
    inspect_grouping(data, .by = c(area = region), .grouping = rollup(revenue))
  )
  expect_s3_class(inspected, "marginplyr_error")
  expect_identical(conditionMessage(inspected), renaming_by_message())
  expect_identical(
    rlang::call_name(conditionCall(inspected)),
    "inspect_grouping"
  )

  # Every Margin verb resolves `.by` through the same helper, so each one
  # reports what the inspection verb reported. The calls are written out rather
  # than built from the verb names, so `codetools` can follow them.
  summarized <- expect_error(
    summarize_with_margins(
      data,
      n = dplyr::n(),
      .by = c(area = region),
      .grouping = rollup(revenue)
    )
  )
  expect_identical(conditionMessage(summarized), renaming_by_message())

  expanded <- expect_error(
    expand_with_margins(
      data,
      .by = c(area = region),
      .grouping = rollup(revenue)
    )
  )
  expect_identical(conditionMessage(expanded), renaming_by_message())

  nested <- expect_error(
    nest_with_margins(
      data,
      .by = c(area = region),
      .grouping = rollup(revenue)
    )
  )
  expect_identical(conditionMessage(nested), renaming_by_message())

  nested_by <- expect_error(
    nest_by_with_margins(
      data,
      .by = c(area = region),
      .grouping = rollup(revenue)
    )
  )
  expect_identical(conditionMessage(nested_by), renaming_by_message())

  for (error in list(summarized, expanded, nested, nested_by)) {
    expect_s3_class(error, "marginplyr_error")
  }
})

test_that("a renaming .by is refused whether or not the name is a column", {
  # The renamed-to name is no column of this input, which used to be reported
  # as an unknown `.by` column the caller never wrote.
  error <- expect_error(
    inspect_grouping(
      data.frame(region = c("x", "y"), revenue = 1:2),
      .by = c(area = region),
      .grouping = rollup(revenue)
    )
  )
  expect_s3_class(error, "marginplyr_error")
  expect_identical(conditionMessage(error), renaming_by_message())
})

test_that("a renaming .by resolved from typed metadata is refused", {
  # A selection carrying a predicate cannot be resolved from column names
  # alone, so it reaches the typed selection proxy rather than the name proxy
  # that rejects every other renaming `.by` before a backend is read.
  error <- expect_error(
    inspect_grouping(
      renaming_by_data(),
      .by = c(area = region, where(is.numeric)),
      .grouping = grouping_set()
    )
  )
  expect_s3_class(error, "marginplyr_error")
  expect_identical(conditionMessage(error), renaming_by_message())
})

test_that("fixed .by keys still resolve without renaming", {
  data <- renaming_by_data()

  plan <- inspect_grouping(data, .by = region, .grouping = rollup(revenue))
  expect_identical(plan$fixed, c("(region)", "(region)"))

  # A grouped input supplies the same keys through `dplyr::group_vars()`.
  grouped <- inspect_grouping(
    dplyr::group_by(data, region),
    .grouping = rollup(revenue)
  )
  expect_identical(grouped$fixed, c("(region)", "(region)"))

  # A predicate resolves against the typed snapshot instead of the names.
  typed <- inspect_grouping(
    data,
    .by = where(is.numeric),
    .grouping = rollup(region)
  )
  expect_identical(typed$fixed, c("(revenue)", "(revenue)"))

  # No `.by` at all fixes nothing.
  expect_identical(
    inspect_grouping(data, .grouping = rollup(region))$fixed,
    c("()", "()")
  )
})

test_that("lazy inspection reads typed metadata once, executing no margins", {
  skip_if_suggest_absent("dtplyr")
  # Count backend acquisitions: public results hide early or repeated reads
  # of the typed snapshot (ADR 0002).
  inspect_proxy_capture <- local_dtplyr_proxy_counter()
  source <- dtplyr::lazy_dt(data.frame(
    group = c("Total", "x"),
    value = 1:2
  ))
  inspect_proxy_capture$n <- 0L
  inspect_proxy_capture$source_evaluations <- 0L

  result <- inspect_grouping(
    source,
    .grouping = rollup(where(is.character))
  )

  expect_identical(inspect_proxy_capture$n, 1L)
  expect_identical(inspect_proxy_capture$source_evaluations, 0L)
  expect_identical(class(result), c("tbl_df", "tbl", "data.frame"))
  expect_identical(result$included, c("(group)", "()"))
  expect_identical(result$grouping_id, c(0L, 1L))
})

test_that("inspection scopes predicate collection guidance to Margin verbs", {
  skip_if_suggest_absent("RSQLite", "DBI")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    data.frame(group = "Total", value = 1L),
    "inspection_predicate_collision",
    temporary = TRUE
  )

  error <- expect_error(
    inspect_grouping(remote, .grouping = rollup(where(is.character))),
    class = "marginplyr_error"
  )
  expect_match(
    conditionMessage(error),
    paste0(
      "A Margin operation on collected data defaults `.check_margin_label` ",
      "from `FALSE` to `TRUE`; set it explicitly."
    ),
    fixed = TRUE
  )
  expect_false(grepl(
    "re-running this call",
    conditionMessage(error),
    fixed = TRUE
  ))

  local <- dplyr::collect(remote)
  inspection <- inspect_grouping(
    local,
    .grouping = rollup(where(is.character))
  )
  expect_identical(
    inspection,
    inspect_grouping(local, .grouping = rollup(group))
  )
  expect_error(
    summarize_with_margins(
      local,
      n = dplyr::n(),
      .grouping = rollup(where(is.character))
    ),
    "already present in grouping column"
  )
  result <- summarize_with_margins(
    local,
    n = dplyr::n(),
    .grouping = rollup(where(is.character)),
    .check_margin_label = FALSE,
    .id = "set"
  )

  expect_identical(result$group, c("Total", "Total"))
  expect_setequal(result$set, c(1L, 2L))
})

test_that("a name-only grouping error precedes a .by predicate's read", {
  # A `.by` predicate is resolved from typed metadata, but the Grouping
  # specification beside it may still be resolvable from names — and ADR-0005
  # puts its rejection before any backend read. Deferred fixed keys stand in as
  # none for that pass, so the read a predicate needs cannot be pulled ahead of
  # an error the names already decide.
  skip_if_suggest_absent("dtplyr")
  inspect_proxy_capture <- local_dtplyr_proxy_counter()
  source <- dtplyr::lazy_dt(data.frame(
    group = c("x", "y"),
    value = 1:2
  ))
  inspect_proxy_capture$n <- 0L
  inspect_proxy_capture$source_evaluations <- 0L

  error <- expect_error(
    inspect_grouping(
      source,
      .by = where(is.numeric),
      .grouping = rollup(tidyselect::all_of("absent"))
    )
  )

  expect_identical(inspect_proxy_capture$n, 0L)
  expect_identical(inspect_proxy_capture$source_evaluations, 0L)
  expect_match(conditionMessage(error), "absent")
})

test_that("dbplyr inspection returns local plan data without a source query", {
  source <- dbplyr::tbl_lazy(
    data.frame(group = "Total", value = 1L),
    con = dbplyr::simulate_sqlite()
  )

  result <- inspect_grouping(
    source,
    .grouping = rollup(group)
  )

  expect_identical(class(result), c("tbl_df", "tbl", "data.frame"))
  expect_identical(result$included, c("(group)", "()"))
})

test_that("inspect_grouping() options use the Package condition seam", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  error <- expect_error(
    inspect_grouping(data, .grouping = rollup(group), .format = "json"),
    "`\\.format` must be one of"
  )

  expect_s3_class(error, "marginplyr_error")
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "inspect_grouping"
  )
})

test_that("a lazy Margin result cannot join its local Grouping plan directly", {
  fixture <- lazy_grouping_join_fixture()
  plan <- inspect_grouping(
    fixture$remote,
    .by = year,
    .grouping = rollup(region, store)
  ) |>
    dplyr::select(set_id)

  error <- expect_error(
    dplyr::left_join(fixture$report, plan, by = c(set = "set_id"))
  )

  expect_match(
    conditionMessage(error),
    "must share the same source",
    fixed = TRUE
  )
  expect_match(conditionMessage(error), 'copy = "inline"', fixed = TRUE)
})

test_that("a list-format Grouping plan cannot be joined as inline SQL", {
  fixture <- lazy_grouping_join_fixture()
  plan <- inspect_grouping(
    fixture$remote,
    .by = year,
    .grouping = rollup(region, store),
    .format = "list"
  )

  error <- expect_error(suppressWarnings(dplyr::show_query(dplyr::left_join(
    fixture$report,
    plan,
    by = c(set = "set_id"),
    copy = "inline"
  ))))

  expect_match(conditionMessage(error), "sql_cast_dispatch", fixed = TRUE)
  expect_match(conditionMessage(error), 'class "list"', fixed = TRUE)
})

test_that("lazy helper arguments use one typed snapshot", {
  skip_if_suggest_absent("dtplyr")
  inspect_proxy_capture <- local_dtplyr_proxy_counter()
  source <- dtplyr::lazy_dt(tibble::tibble(a = c("x", "y"), b = c(1, 1)))
  selection <- rlang::expr(tidyselect::all_of(
    names(tidyselect::peek_data())[
      vapply(tidyselect::peek_data(), is.numeric, logical(1))
    ]
  ))

  inspect_proxy_capture$n <- 0L
  inspect_proxy_capture$source_evaluations <- 0L
  fixed <- inspect_grouping(source, .by = !!selection)
  expect_identical(fixed$fixed, "(b)")
  expect_identical(inspect_proxy_capture$n, 1L)
  expect_identical(inspect_proxy_capture$source_evaluations, 0L)

  inspect_proxy_capture$n <- 0L
  inspect_proxy_capture$source_evaluations <- 0L
  dimension <- inspect_grouping(
    source,
    .by = a,
    .grouping = grouping_set(!!selection)
  )
  expect_identical(dimension$included, "(b)")
  expect_identical(inspect_proxy_capture$n, 1L)
  expect_identical(inspect_proxy_capture$source_evaluations, 0L)

  inspect_proxy_capture$n <- 0L
  inspect_proxy_capture$source_evaluations <- 0L
  expect_error(
    inspect_grouping(
      source,
      .by = !!selection,
      .grouping = grouping_set(tidyselect::all_of("absent"))
    ),
    "absent",
    class = "vctrs_error_subscript_oob"
  )
  expect_identical(inspect_proxy_capture$n, 0L)
  expect_identical(inspect_proxy_capture$source_evaluations, 0L)
})

test_that("names obtained from selection data keep early-error guarantees", {
  skip_if_suggest_absent("dtplyr")
  inspect_proxy_capture <- local_dtplyr_proxy_counter()
  source <- dtplyr::lazy_dt(tibble::tibble(a = c("x", "y"), b = c(1, 1)))
  data_alias <- tidyselect::peek_data
  names_alias <- base::names
  selections <- list(
    rlang::expr(tidyselect::all_of(
      c(names(tidyselect::peek_data()), "absent")
    )),
    rlang::expr(tidyselect::all_of(c(names_alias(data_alias()), "absent"))),
    rlang::expr(tidyselect::all_of(c(tidyselect::peek_vars(), "absent"))),
    rlang::expr(tidyselect::all_of(unname(c(x = "absent")))),
    rlang::expr(tidyselect::all_of(tolower("ABSENT"))),
    rlang::expr(tidyselect::all_of(as.character("absent")))
  )
  for (selection in selections) {
    inspect_proxy_capture$n <- 0L
    inspect_proxy_capture$source_evaluations <- 0L
    expect_error(
      inspect_grouping(source, .by = !!selection),
      "absent", class = "vctrs_error_subscript_oob"
    )
    expect_identical(inspect_proxy_capture$n, 0L)
    expect_identical(inspect_proxy_capture$source_evaluations, 0L)

    inspect_proxy_capture$n <- 0L
    inspect_proxy_capture$source_evaluations <- 0L
    expect_error(
      inspect_grouping(source, .grouping = grouping_set(!!selection)),
      "absent", class = "vctrs_error_subscript_oob"
    )
    expect_identical(inspect_proxy_capture$n, 0L)
    expect_identical(inspect_proxy_capture$source_evaluations, 0L)
  }
})


test_that("dispatched helper arguments use one zero-row snapshot", {
  skip_if_suggest_absent("dtplyr")
  inspect_proxy_capture <- local_dtplyr_proxy_counter()
  source <- dtplyr::lazy_dt(tibble::tibble(a = c("x", "y"), b = 1:2))
  selector <- matrix(1L)
  rows <- NULL
  format.matrix <- function(x, ...) {
    proxy <- tidyselect::peek_data()
    rows <<- nrow(proxy)
    names(proxy)[vapply(proxy, is.character, logical(1))]
  }
  selection <- rlang::quo(tidyselect::all_of(format(selector)))

  inspect_proxy_capture$n <- 0L
  inspect_proxy_capture$source_evaluations <- 0L
  expect_identical(inspect_grouping(source, .by = !!selection)$fixed, "(a)")
  expect_identical(inspect_proxy_capture$n, 1L)
  expect_identical(inspect_proxy_capture$source_evaluations, 0L)
  expect_identical(rows, 0L)

  inspect_proxy_capture$n <- 0L
  inspect_proxy_capture$source_evaluations <- 0L
  expect_identical(
    inspect_grouping(source, .grouping = rollup(!!selection))$included,
    c("(a)", "()")
  )
  expect_identical(inspect_proxy_capture$n, 1L)
  expect_identical(inspect_proxy_capture$source_evaluations, 0L)
  expect_identical(rows, 0L)
})

test_that("typed DuckDB inspection refuses dynamic pivots before row reads", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  DBI::dbExecute(con, "CREATE SEQUENCE inspected_rows START 1")
  sequence_value <- function() {
    DBI::dbGetQuery(con, "SELECT currval('inspected_rows')")[[1L]]
  }
  first <- DBI::dbGetQuery(con, "SELECT nextval('inspected_rows')")[[1L]]
  source <- dplyr::tbl(con, dbplyr::sql(paste(
    "PIVOT (SELECT CASE WHEN nextval('inspected_rows') > 0",
    "THEN region ELSE 'other' END AS k, value",
    "FROM (VALUES ('east', 1), ('west', 2), ('west', 3)) AS t(region, value))",
    "ON k USING sum(value)"
  )), vars = c("east", "west"))

  plan <- inspect_grouping(source, .grouping = grouping_set(east))
  expect_identical(plan$included, "(east)")
  expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
  expect_identical(sequence_value(), first)

  expect_error(
    inspect_grouping(source, .grouping = grouping_set(where(is.numeric))),
    "dplyr::collect()", fixed = TRUE, class = "marginplyr_error"
  )
  expect_identical(sequence_value(), first)
  audit <- last_sent_queries(details = TRUE)
  expect_identical(audit$purpose, "selection_proxy")
  expect_identical(audit$status, "errored")
  expect_identical(audit$conditions[[1L]]$handling, "handled")
})

test_that("DuckDB metadata preserves static pivots and ordinary queries", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  DBI::dbExecute(con, "CREATE SEQUENCE inspected_rows START 1")
  first <- DBI::dbGetQuery(con, "SELECT nextval('inspected_rows')")[[1L]]
  rows <- paste(
    "SELECT CASE WHEN nextval('inspected_rows') > 0",
    "THEN region ELSE 'other' END AS k, value",
    "FROM (VALUES ('east', 1), ('west', 2), ('west', 3)) AS t(region, value)"
  )
  static <- dplyr::tbl(con, dbplyr::sql(paste0(
    "PIVOT (", rows, ") ON k IN ('east', 'west') USING sum(value)"
  )), vars = c("east", "west"))
  ordinary <- dplyr::tbl(con, dbplyr::sql(rows), vars = c("k", "value"))
  for (case in list(list(static, "(east, west)"), list(ordinary, "(value)"))) {
    plan <- inspect_grouping(case[[1L]], .grouping = grouping_set(
      where(is.numeric)
    ))
    expect_identical(plan$included, case[[2L]])
    expect_identical(
      DBI::dbGetQuery(con, "SELECT currval('inspected_rows')")[[1L]], first
    )
    audit <- last_sent_queries(details = TRUE)
    expect_identical(audit$purpose, "selection_proxy")
    expect_identical(audit$status, "returned")
    expect_match(audit$sql, "LIMIT 0", fixed = TRUE)
  }
})

test_that("other DuckDB metadata failures retain their external condition", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  source <- dplyr::tbl(
    con, dbplyr::sql("SELECT k FROM absent_table"), vars = "k"
  )
  err <- expect_error(
    inspect_grouping(source, .grouping = grouping_set(where(is.numeric))),
    "absent_table", class = "rlang_error"
  )
  expect_false(inherits(err, "marginplyr_error"))
  audit <- last_sent_queries(details = TRUE)
  expect_identical(audit$purpose, "selection_proxy")
  expect_identical(audit$status, "errored")
  expect_identical(audit$conditions[[1L]]$handling, "propagated")
})

test_that("typed DuckDB inspection requires matching input column names", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  source <- dplyr::tbl(
    con, dbplyr::sql("SELECT 1 AS region, 'west' AS REGION"),
    vars = c("region", "REGION")
  )

  plan <- inspect_grouping(source, .grouping = grouping_set(REGION))
  expect_identical(plan$included, "(REGION)")
  expect_equal(nrow(last_sent_queries()), 0L)

  expect_error(
    inspect_grouping(source, .grouping = grouping_set(where(is.character))),
    "Alias the input columns with unique names", fixed = TRUE,
    class = "marginplyr_error"
  )
  audit <- last_sent_queries(details = TRUE)
  expect_identical(audit$purpose, "selection_proxy")
  expect_identical(audit$status, "returned")
  expect_error(
    inspect_grouping(source, .by = where(is.character)),
    "Alias the input columns with unique names", fixed = TRUE,
    class = "marginplyr_error"
  )
  expect_error(
    inspect_grouping(
      source, .by = REGION, .grouping = grouping_set(where(is.character))
    ),
    "Alias the input columns with unique names", fixed = TRUE,
    class = "marginplyr_error"
  )
})
