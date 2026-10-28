test_that("scalar factor overwrites refuse metadata before any evaluation", {
  skip_if_suggest_absent("dtplyr")
  evaluations <- 0L
  dt_eval <- getFromNamespace("dt_eval", "dtplyr")
  testthat::local_mocked_bindings(
    dt_eval = function(x) {
      evaluations <<- evaluations + 1L
      dt_eval(x)
    },
    .package = "dtplyr"
  )

  for (rows in c(0L, 1L, 3L)) {
    source <- data.table::data.table(
      g = factor(rep("a", rows), levels = c("a", "unused")),
      value = seq_len(rows)
    )
    before <- data.table::copy(source)
    step <- dtplyr::lazy_dt(source) |>
      dplyr::mutate(g = "z")

    for (spec in list(grouping_set(g), rollup(g))) {
      for (verb in forwarded_margin_verbs) {
        expect_error(
          verb(step, NULL, spec),
          "Can't safely determine column metadata",
          class = "marginplyr_error"
        )
        expect_identical(source, before)
      }
    }
  }
  expect_identical(evaluations, 0L)
})

test_that("constant arithmetic overwrites cannot supply stale column types", {
  skip_if_suggest_absent("dtplyr")
  step <- dtplyr::lazy_dt(data.frame(g = "a", value = 1L)) |>
    dplyr::mutate(value = 1 / 2)

  expect_error(
    inspect_grouping(step, .grouping = grouping_set(where(is.integer))),
    "Can't safely determine column metadata",
    class = "marginplyr_error"
  )
})

test_that("type-changing scalar overwrites are refused by typed selection", {
  skip_if_suggest_absent("dtplyr")

  for (rows in c(0L, 1L, 3L)) {
    source <- data.table::data.table(g = seq_len(rows), value = seq_len(rows))
    before <- data.table::copy(source)
    step <- dtplyr::lazy_dt(source) |>
      dplyr::mutate(g = "z")

    expect_error(
      inspect_grouping(step, .grouping = grouping_set(where(is.numeric))),
      "Can't safely determine column metadata",
      class = "marginplyr_error"
    )
    expect_error(
      inspect_grouping(step, .by = where(is.character)),
      "Can't safely determine column metadata",
      class = "marginplyr_error"
    )
    for (verb in forwarded_margin_verbs) {
      expect_error(
        verb(step, NULL, rollup(where(is.numeric))),
        "Can't safely determine column metadata",
        class = "marginplyr_error"
      )
    }
    expect_identical(source, before)
  }
})

test_that("scalar overwrite refusals precede explicit Margin label checks", {
  skip_if_suggest_absent("dtplyr")

  for (rows in c(0L, 1L, 3L)) {
    source <- data.table::data.table(
      g = factor(rep("a", rows), levels = c("a", "unused")),
      value = seq_len(rows)
    )
    before <- data.table::copy(source)
    step <- dtplyr::lazy_dt(source) |>
      dplyr::mutate(g = "z")

    for (verb in list(
      summarize_with_margins, expand_with_margins,
      nest_with_margins, nest_by_with_margins
    )) {
      expect_error(
        verb(
          step, .grouping = rollup(g),
          .margin_label = "z", .check_margin_label = TRUE
        ),
        "Can't safely determine column metadata",
        class = "marginplyr_error"
      )
    }
    expect_identical(source, before)

    if (rows > 0L) {
      materialized <- dplyr::collect(step)
      expect_identical(as.character(materialized$g), rep("z", rows))
      expect_error(
        expand_with_margins(
          materialized, .grouping = rollup(g),
          .margin_label = "z", .check_margin_label = TRUE
        ),
        class = "marginplyr_error"
      )
      result <- expand_with_margins(
        materialized, .grouping = rollup(g), .id = "set"
      )
      expect_identical(as.character(result$g[result$set == 1L]), rep("z", rows))
      if (rows > 1L) {
        expect_identical(levels(result$g), c("a", "unused", "z", "Total"))
      }
    }
    expect_identical(source, before)
  }
})

test_that("faithful dtplyr derivations retain metadata and detail values", {
  skip_if_suggest_absent("dtplyr")

  for (rows in c(0L, 1L, 3L)) {
    source <- data.table::data.table(
      g = factor(rep("a", rows), levels = c("a", "unused")),
      value = seq_len(rows), discard = rep(FALSE, rows)
    )
    before <- data.table::copy(source)
    step <- dtplyr::lazy_dt(source) |>
      dplyr::mutate(
        copy = g, tag = "fixed", constant = 1 / 2,
        value = value * 2, discard = NULL
      )

    plan <- inspect_grouping(step, .grouping = grouping_set(where(is.factor)))
    expect_identical(plan$included[[1L]], "(g, copy)")
    plan <- inspect_grouping(step, .grouping = grouping_set(where(is.double)))
    expect_identical(plan$included[[1L]], "(value, constant)")
    for (spec in list(grouping_set(copy), rollup(copy))) {
      query <- expand_with_margins(step, .grouping = spec, .id = "set")
      expect_s3_class(query, "dtplyr_step")
      result <- dplyr::collect(query)
      detail <- result[result$set == 1L, ]
      expect_identical(as.character(detail$copy), rep("a", rows))
      expect_identical(detail$value, seq_len(rows) * 2)
      expect_identical(detail$constant, rep(0.5, rows))
      expect_identical(detail$tag, rep("fixed", rows))
      expect_false("discard" %in% names(result))
      expect_identical(levels(result$copy), c("a", "unused", "Total"))
    }
    expect_identical(source, before)
  }
})

test_that("grouped renamed dtplyr inputs inherit fixed nesting keys", {
  skip_if_suggest_absent("dtplyr")
  source <- dtplyr::lazy_dt(data.frame(g = c("b", "a", "b"), value = 1:3)) |>
    dplyr::rename(`fixed key` = g)
  grouped <- dplyr::group_by(source, `fixed key`)

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    actual <- verb(grouped, .sort = "last")
    expected <- verb(source, .by = `fixed key`, .sort = "last")
    if (identical(verb, nest_with_margins)) {
      expect_s3_class(actual, "dtplyr_step")
      actual <- dplyr::collect(actual)
      expected <- dplyr::collect(expected)
    }
    expect_identical(actual, expected)
    expect_identical(actual[["fixed key"]], c("a", "b"))
    expect_identical(lapply(actual$data, function(cell) cell$value),
                     list(2L, c(1L, 3L)))
  }
})

test_that("metadata-preserving grouped derivations match explicit fixed keys", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only dtplyr is an optional backend here.
  source <- data.table::data.table(
    `fixed key` = c("b", "a", "b", "a"),
    dimension = factor(c("x", "y", "x", "x"), levels = c("y", "x", "unused")),
    value = c(3L, 2L, 3L, 0L), discard = FALSE
  )
  before <- data.table::copy(source)
  root <- dtplyr::lazy_dt(source)
  operations <- list(
    rename = function(x) dplyr::rename(x, `caller value` = value),
    select = function(x) dplyr::select(x, dimension, value, `fixed key`),
    filter = function(x) dplyr::filter(x, value > 0L)
  )

  for (operation in operations) {
    explicit <- operation(root)
    steps <- list(
      dplyr::group_by(explicit, `fixed key`),
      operation(dplyr::group_by(root, `fixed key`))
    )
    for (step in steps) {
      for (keep in c(FALSE, TRUE)) {
        for (sort in c("none", "first", "last")) {
          for (verb in list(nest_with_margins, nest_by_with_margins)) {
            actual <- verb(
              step, .grouping = rollup(dimension), .keep = keep,
              .key = "caller rows", .id = "caller set", .sort = sort
            )
            expected <- verb(
              explicit, .by = `fixed key`, .grouping = rollup(dimension),
              .keep = keep, .key = "caller rows",
              .id = "caller set", .sort = sort
            )
            if (identical(verb, nest_with_margins)) {
              expect_s3_class(actual, "dtplyr_step")
              actual <- dplyr::collect(actual)
              expected <- dplyr::collect(expected)
            } else {
              expect_s3_class(actual, "rowwise_df")
            }
            expect_identical(actual, expected)
            expect_identical(
              names(actual),
              c("fixed key", "dimension", "caller set", "caller rows")
            )
            expect_identical(
              levels(actual$dimension), c("y", "x", "unused", "Total")
            )
          }
        }
      }
      actual <- dplyr::collect(expand_with_margins(
        step, .grouping = rollup(dimension), .id = "set", .sort = "last"
      ))
      expected <- dplyr::collect(expand_with_margins(
        explicit, .by = `fixed key`, .grouping = rollup(dimension),
        .id = "set", .sort = "last"
      ))
      expect_identical(actual, expected)
      actual <- dplyr::collect(summarize_with_margins(
        step, n = dplyr::n(), .grouping = rollup(dimension), .sort = "first"
      ))
      expected <- dplyr::collect(summarize_with_margins(
        explicit, n = dplyr::n(), .by = `fixed key`,
        .grouping = rollup(dimension), .sort = "first"
      ))
      expect_identical(actual, expected)
      expect_identical(source, before)
    }
  }
})

test_that("grouped dtplyr planning leaves source rows unread", {
  skip_if_suggest_absent("dtplyr")
  seen <- integer()
  probe <- function(x) {
    seen <<- c(seen, length(x))
    x > 0L
  }
  source <- data.table::data.table(
    g = c("b", "a", "b"),
    dimension = factor(c("x", "y", "x"), levels = c("y", "x", "unused")),
    value = 1:3, discard = FALSE
  )
  before <- data.table::copy(source)
  root <- dtplyr::lazy_dt(source)
  derive <- function(x) {
    x |>
      dplyr::rename(`caller value` = value) |>
      dplyr::select(dimension, `caller value`, g) |>
      dplyr::filter(probe(`caller value`))
  }
  explicit <- derive(root)
  for (step in list(
    dplyr::group_by(explicit, g),
    derive(dplyr::group_by(root, g))
  )) {
    for (spec in list(rollup(dimension), rollup(where(is.factor)),
                      rollup(where(is.integer)))) {
      actual <- inspect_grouping(step, .grouping = spec)
      expected <- inspect_grouping(explicit, .by = g, .grouping = spec)
      expect_identical(actual, expected)
    }
    query <- nest_with_margins(step, .grouping = rollup(dimension))
    expect_s3_class(query, "dtplyr_step")
    expect_s3_class(expand_with_margins(step, .grouping = rollup(dimension)),
                    "dtplyr_step")
    expect_s3_class(summarize_with_margins(
      step, n = dplyr::n(), .grouping = rollup(dimension)
    ), "dtplyr_step")
    expect_identical(seen, integer())
    expect_identical(source, before)

    dplyr::collect(query)
    expect_true(any(seen > 0L))
    expect_identical(source, before)
    seen <- integer()
    result <- nest_by_with_margins(step, .grouping = rollup(dimension))
    expect_s3_class(result, "rowwise_df")
    expect_true(any(seen > 0L))
    expect_identical(source, before)
    seen <- integer()
  }
})

test_that("grouping wrappers cannot admit unsafe dtplyr metadata", {
  skip_if_suggest_absent("dtplyr")
  seen <- integer()
  probe <- function(x) {
    seen <<- c(seen, length(x))
    x
  }
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  root <- dtplyr::lazy_dt(source)
  operations <- list(
    function(x) dplyr::mutate(x, value = probe(value)),
    function(x) dplyr::mutate(x, value = factor(value)),
    function(x) dplyr::mutate(x, value = if (dplyr::n() > 0L) "text" else 0L),
    function(x) dplyr::mutate(x, value = "text")
  )

  for (operation in operations) {
    for (step in list(
      dplyr::group_by(operation(root), g),
      operation(dplyr::group_by(root, g))
    )) {
      for (verb in forwarded_margin_verbs) {
        expect_error(
          verb(step, NULL, grouping_set()),
          "Can't safely determine column metadata", class = "marginplyr_error"
        )
      }
      expect_error(
        inspect_grouping(step, .grouping = rollup(where(is.numeric))),
        "Can't safely determine column metadata", class = "marginplyr_error"
      )
      expect_identical(
        nrow(inspect_grouping(step, .grouping = grouping_set())), 1L
      )
      expect_identical(seen, integer())
      expect_identical(source, before)
    }
  }
})

test_that("derived .SD typed selections agree with collected Grouping plans", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(.SD = c("a", "b"), value = 1:2, discard = FALSE)
  root <- dtplyr::lazy_dt(input)
  for (step in list(
    root,
    dplyr::rename(root, payload = value),
    dplyr::filter(root, TRUE),
    dplyr::filter(root, value > 1L),
    dplyr::filter(root, FALSE)
  )) {
    collected <- dplyr::collect(step)
    expect_identical(
      inspect_grouping(step, .by = where(is.character),
                       .grouping = rollup(where(is.integer))),
      inspect_grouping(collected, .by = where(is.character),
                       .grouping = rollup(where(is.integer)))
    )
    expect_identical(
      inspect_grouping(step, .grouping = rollup(where(is.character))),
      inspect_grouping(collected, .grouping = rollup(.SD))
    )
    expect_identical(
      inspect_grouping(step, .grouping = rollup(.SD)),
      inspect_grouping(collected, .grouping = rollup(.SD))
    )
  }
})

test_that(".SD metadata skips predicates and leaves Mutable sources unchanged", {
  skip_if_suggest_absent("dtplyr")
  predicate <- function(x) stop("row predicate evaluated")
  # data.table is imported by dtplyr; `.SD` is admitted after construction.
  source <- data.table::data.table(special = c("a", "b"), value = 1:2)
  data.table::setnames(source, "special", ".SD")
  before <- data.table::copy(source)
  for (immutable in c(TRUE, FALSE)) {
    step <- dtplyr::lazy_dt(source, immutable = immutable) |>
      dplyr::filter(predicate(value)) |>
      dplyr::select(value, .SD)
    expect_identical(
      inspect_grouping(step, .by = where(is.character),
                       .grouping = rollup(where(is.integer))),
      inspect_grouping(tibble::as_tibble(before), .by = .SD,
                       .grouping = rollup(value))
    )
    if (immutable) {
      expect_s3_class(nest_with_margins(step), "dtplyr_step")
    } else {
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        expect_error(verb(step), "immutable = FALSE",
                     class = "marginplyr_error")
      }
    }
    expect_identical(source, before)
  }
})

test_that(".SD columns do not admit unsafe derived metadata", {
  skip_if_suggest_absent("dtplyr")
  probe <- function(x) stop("source rows evaluated")
  step <- dtplyr::lazy_dt(tibble::tibble(.SD = "a", value = 1L)) |>
    dplyr::mutate(value = probe(value)) |>
    dplyr::filter(TRUE)
  expect_identical(
    inspect_grouping(step, .grouping = rollup(.SD))$included,
    c("(.SD)", "()")
  )
  expect_error(
    inspect_grouping(step, .grouping = rollup(where(is.character))),
    "Can't safely determine column metadata", class = "marginplyr_error"
  )
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    expect_error(verb(step), "Can't safely determine column metadata",
                 class = "marginplyr_error")
  }
})
