test_that("optimized special input measures retain Parent and Total shares", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only one optional backend is required.
  # Below level 2 the ordinary/no-share controls already fail upstream.
  for (optimization in c(2, Inf)) {
    rlang::local_options(datatable.optimize = optimization)
    for (column in c(".SD", ".GRP", ".NGRP", ".SDall", ".xSD", ".iSD")) {
      input <- data.table::data.table(
        f = c("P", "P"), g = c("a", "b"), x = c(1, 3)
      )
      data.table::setnames(input, "x", column)
      before <- data.table::copy(input)
      for (style in c("direct", "across")) {
        dots <- if (style == "direct") {
          rlang::exprs(value = sum(!!rlang::sym(column)))
        } else {
          rlang::exprs(dplyr::across(
            dplyr::all_of(column), sum, .names = "value"
          ))
        }
        source <- dtplyr::lazy_dt(input)
        ordinary <- dplyr::summarise(source, !!!dots, .by = c(f, g))
        expect_equal(dplyr::collect(ordinary)$value, c(1, 3))
        plain <- summarize_with_margins(
          source, !!!dots, .by = f, .grouping = rollup(g)
        )
        expect_equal(dplyr::collect(plain)$value, c(1, 3, 4))
        for (helper in c("share_of_parent", "share_of_total")) {
          summarize <- function(data) {
            summarize_with_margins(
              data, !!!dots, fraction = !!rlang::call2(helper, quote(value)),
              .by = f, .grouping = rollup(g)
            )
          }
          expected <- summarize(tibble::as_tibble(input))
          query <- summarize(source)
          expect_s3_class(query, "dtplyr_step")
          expect_identical(input, before)
          actual <- dplyr::collect(query)
          expect_equal(actual, expected)
          expect_equal(actual$value, c(1, 3, 4))
          expect_identical(actual$fraction, c(0.25, 0.75, 1))
          expect_identical(input, before)
        }
      }
    }
  }
})

test_that("special input measures share one lazy validation boundary", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only one optional backend is required.
  columns <- c(".BY", ".SD", ".GRP", ".NGRP", ".SDall", ".xSD", ".iSD")
  input <- data.table::data.table(f = c("P", "P"), g = c("a", "b"))
  for (column in columns) input[[column]] <- c(1, 3)
  before <- data.table::copy(input)
  observed <- new.env(parent = emptyenv())
  observed$rows <- 0L
  observed$summaries <- 0L
  predicate <- function(x) {
    observed$rows <- observed$rows + length(x)
    x > 0
  }
  probe <- function(x) {
    observed$summaries <- observed$summaries + 1L
    sum(x)
  }
  source <- dplyr::filter(dtplyr::lazy_dt(input), predicate(.BY))
  for (helper in c("share_of_parent", "share_of_total")) {
    observed$rows <- 0L
    observed$summaries <- 0L
    query <- summarize_with_margins(
      source,
      dplyr::across(dplyr::all_of(columns), probe, .names = "{.col}_value"),
      dplyr::across(dplyr::all_of(paste0(columns, "_value")),
                    !!rlang::sym(helper), .names = "{.col}_share"),
      .by = f, .grouping = rollup(g)
    )
    expect_s3_class(query, "dtplyr_step")
    expect_identical(observed$rows, 0L)
    expect_identical(observed$summaries, 0L)
    actual <- dplyr::collect(query)
    for (column in columns) {
      expect_equal(actual[[paste0(column, "_value")]], c(1, 3, 4))
      expect_identical(
        actual[[paste0(column, "_value_share")]], c(0.25, 0.75, 1)
      )
    }
    expect_gt(observed$rows, 0L)
    expect_gt(observed$summaries, 0L)
    expect_identical(input, before)
  }
})

test_that("special input measures retain scalar and type validation", {
  skip_if_suggest_absent("dtplyr")
  cases <- list(
    list(body = quote(c(.x, .x)), message = "exactly one value"),
    list(body = quote(numeric()), message = "exactly one value"),
    list(body = quote(as.logical(sum(.x))), message = "logical"),
    list(body = quote(as.character(sum(.x))), message = "character")
  )
  for (column in c(".SD", ".GRP", ".NGRP", ".SDall", ".xSD", ".iSD")) {
    input <- tibble::tibble(
      f = c("P", "P"), g = c("a", "b"), !!column := c(1, 3)
    )
    for (helper in c("share_of_parent", "share_of_total")) {
      for (style in c("direct", "across")) {
        for (case in cases) {
          dots <- if (style == "direct") {
            body <- do.call(
              "substitute", list(case$body, list(.x = rlang::sym(column)))
            )
            rlang::exprs(value = !!body)
          } else {
            rlang::exprs(dplyr::across(
              dplyr::all_of(column), ~ !!case$body, .names = "value"
            ))
          }
          query <- summarize_with_margins(
            dtplyr::lazy_dt(input), !!!dots,
            fraction = !!rlang::call2(helper, quote(value)),
            .by = f, .grouping = rollup(g), .check_share_source = FALSE
          )
          error <- expect_error(dplyr::collect(query), case$message,
                                class = "marginplyr_error")
          expect_match(conditionMessage(error), "value")
          expect_match(conditionMessage(error), "fraction")
        }
      }
    }
  }
})

test_that("special measures retain lexical and evaluation ownership", {
  skip_if_suggest_absent("dtplyr")
  for (column in c(".SD", ".GRP", ".NGRP", ".SDall", ".xSD", ".iSD")) {
    input <- tibble::tibble(f = c("P", "P"), g = c("a", "b"),
                            !!column := c(1, 3), x = c(2, 6))
    for (helper in c("share_of_parent", "share_of_total")) {
      for (body in c(
        "(function(%s) sum(%s))(c(5, 7))",
        "local({%s <- c(5, 7); sum(%s)})"
      )) {
        independent <- rlang::parse_expr(sprintf(body, column, column))
        query <- summarize_with_margins(
          dtplyr::lazy_dt(input), value = sum(x), independent = !!independent,
          fraction = !!rlang::call2(helper, quote(value)),
          .by = f, .grouping = rollup(g)
        )
        actual <- dplyr::collect(query)
        expect_equal(actual$independent, rep(12, 3))
        expect_equal(actual$value, c(2, 6, 8))
        expect_identical(actual$fraction, c(0.25, 0.75, 1))
      }
      for (body in c(
        'local({"%s" <- c(5, 7); sum(%s)})',
        "eval(quote(sum(%s)), list(%s = c(5, 7)))"
      )) {
        independent <- rlang::parse_expr(sprintf(body, column, column))
        expect_error(
          summarize_with_margins(
            dtplyr::lazy_dt(input), value = sum(x), independent = !!independent,
            fraction = !!rlang::call2(helper, quote(value)),
            .by = f, .grouping = rollup(g)
          ),
          "cannot safely distinguish", class = "marginplyr_error"
        )
      }
    }
  }
})

test_that("ordinary special-measure optimization restrictions stay upstream", {
  skip_if_suggest_absent("dtplyr")
  for (optimization in c(0, 1)) {
    rlang::local_options(datatable.optimize = optimization)
    for (column in c(".SD", ".GRP", ".NGRP", ".SDall", ".xSD", ".iSD")) {
      input <- tibble::tibble(
        f = c("P", "P"), g = c("a", "b"), !!column := c(1, 3)
      )
      source <- dtplyr::lazy_dt(input)
      for (dots in list(
        rlang::exprs(value = sum(!!rlang::sym(column))),
        rlang::exprs(dplyr::across(
          dplyr::all_of(column), sum, .names = "value"
        ))
      )) {
        ordinary <- dplyr::summarise(source, !!!dots, .by = c(f, g))
        expect_error(dplyr::collect(ordinary), "locked binding")
        plain <- summarize_with_margins(
          source, !!!dots, .by = f, .grouping = rollup(g)
        )
        expect_error(dplyr::collect(plain), "locked binding")
      }
    }
  }
})
