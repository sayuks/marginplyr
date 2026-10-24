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
