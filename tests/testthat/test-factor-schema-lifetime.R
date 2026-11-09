test_that("a reused dtplyr expansion refuses a changed captured factor", {
  skip_if_suggest_absent("dtplyr")
  input <- data.table::data.table(k = factor(c("a", "b")), payload = 1:2)
  source <- dtplyr::lazy_dt(input, immutable = TRUE)
  query <- expand_with_margins(source, .grouping = rollup(k), .id = "set")
  data.table::set(input, j = "k", value = factor(c("z", "b")))
  before <- serialize(input, NULL)
  for (i in 1:2) {
    expect_error(dplyr::collect(query), "k.*changed|changed.*k",
                 class = "marginplyr_error")
    expect_error(dplyr::collect(query), "[Rr]econstruct|[Rr]ebuild",
                 class = "marginplyr_error")
  }
  rebuilt <- dplyr::collect(expand_with_margins(
    source, .grouping = rollup(k), .id = "set"
  ))
  expect_identical(as.character(rebuilt$k[rebuilt$set == 1L]), c("z", "b"))
  expect_identical(serialize(input, NULL), before)
})

test_that("factor validation adds no source evaluation or metadata snapshot", {
  skip_if_suggest_absent("dtplyr")
  counter <- local_dtplyr_proxy_counter()
  input <- data.table::data.table(k = factor(c("a", "b")), row_id = 1:2)
  source <- dplyr::mutate(dtplyr::lazy_dt(input), payload = row_id + 1L)
  before <- serialize(input, NULL)
  query <- expand_with_margins(source, .grouping = rollup(k), .id = "set")
  expect_identical(counter$n, 1L)
  expect_identical(counter$source_evaluations, 0L)
  invisible(utils::capture.output(dplyr::show_query(query)))
  expect_identical(counter$n, 1L)
  expect_identical(counter$source_evaluations, 0L)
  for (i in 1:2) {
    expect_identical(nrow(dplyr::collect(query)), 4L)
    expect_identical(counter$n, 1L)
    expect_identical(counter$source_evaluations, i)
  }
  expect_identical(serialize(input, NULL), before)
})

test_that("DuckDB keeps its ordinary unknown-value ENUM cast error", {
  skip_if_suggest_absent("duckdb")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "factor_input", data.frame(k = factor(c("a", "b"))))
  source <- dplyr::tbl(con, "factor_input")
  query <- expand_with_margins(source, .grouping = rollup(k), .id = "set")
  DBI::dbRemoveTable(con, "factor_input")
  DBI::dbWriteTable(con, "factor_input", data.frame(k = factor(c("z", "b"))))
  error <- tryCatch(dplyr::collect(query), error = identity)
  expect_s3_class(error, "error")
  expect_false(inherits(error, "marginplyr_error"))
  expect_match(conditionMessage(error), "[Cc]onversion|[Ee][Nn][Uu][Mm]")
  rebuilt <- dplyr::collect(expand_with_margins(
    dplyr::tbl(con, "factor_input"), .grouping = rollup(k), .id = "set"
  ))
  expect_setequal(as.character(rebuilt$k[rebuilt$set == 1L]), c("z", "b"))
})

test_that("Arrow factor dimensions retain their character output boundary", {
  skip_if_suggest_absent("arrow")
  input <- arrow::Table$create(data.frame(k = factor(c("a", "b"))))
  query <- expand_with_margins(input, .grouping = rollup(k), .id = "set")
  result <- dplyr::collect(query)
  expect_type(result$k, "character")
  expect_setequal(result$k[result$set == 1L], c("a", "b"))
})

test_that("native single-branch passthrough remains reusable", {
  skip_if_suggest_absent("dtplyr")
  input <- data.table::data.table(k = ordered(c("a", "b")), row_id = 1:2)
  before <- serialize(input, NULL)
  source <- dtplyr::lazy_dt(input, immutable = TRUE)
  first <- expand_with_margins(source, .grouping = grouping_set(k),
                               .margin_label = NULL)
  second <- expand_with_margins(first, .grouping = grouping_set(k),
                                .margin_label = NULL)
  result <- dplyr::collect(second)
  expect_identical(result$k, input$k)
  expect_identical(result$row_id, 1:2)
  expect_identical(serialize(input, NULL), before)
})

test_that("removing ordered status requires rebuilding the dtplyr result", {
  skip_if_suggest_absent("dtplyr")
  input <- data.table::data.table(k = ordered(c("a", "b")))
  query <- expand_with_margins(dtplyr::lazy_dt(input), .grouping = rollup(k))
  data.table::set(input, j = "k", value = factor(c("a", "b")))
  expect_error(dplyr::collect(query), "k.*changed|changed.*k",
               class = "marginplyr_error")
})

test_that("captured sentinel-looking levels remain ordinary factor values", {
  skip_if_suggest_absent("dtplyr")
  levels <- c("..marginplyr_missing_factor_code", "Total", NA_character_)
  input <- data.table::data.table(k = structure(
    c(1L, 2L, 3L, NA_integer_), levels = levels, class = "factor"
  ))
  for (label in list("Other", NULL)) {
    result <- dplyr::collect(expand_with_margins(
      dtplyr::lazy_dt(input), .grouping = rollup(k), .id = "set",
      .margin_label = label
    ))
    expect_identical(as.integer(result$k[result$set == 1L]),
                     c(1L, 2L, 3L, NA_integer_))
    expect_identical(levels(result$k), c(levels, label))
  }
})

test_that("native and encoded dtplyr factors refuse every changed definition", {
  skip_if_suggest_absent("dtplyr")
  changes <- list(
    factor(c("z", "b")),
    factor(c("a", "b"), levels = c("a", "b", "unused", "added")),
    factor(c("a", "b"), levels = c("a", "b")),
    factor(c("a", "b"), levels = c("b", "a", "unused")),
    ordered(c("a", "b"), levels = c("a", "b", "unused")),
    factor(c("a", NA), levels = c("a", "b", "unused", NA), exclude = NULL),
    factor(c("..marginplyr_missing_factor_code", "b")),
    factor(c("Total", "b"))
  )
  for (empty in c("none", "source", "filtered")) {
    for (label in list("Total", NULL, NA_character_)) {
      for (changed in changes) {
        input <- data.table::data.table(
          k = factor(c("a", "b"), levels = c("a", "b", "unused")),
          payload = 1:2
        )
        if (empty == "source") input <- input[0L]
        source <- dtplyr::lazy_dt(input, immutable = TRUE)
        if (empty == "filtered") source <- dplyr::filter(source, payload < 0L)
        query <- expand_with_margins(source, .grouping = rollup(k),
                                     .margin_label = label)
        if (empty == "source") changed <- changed[integer()]
        data.table::set(input, j = "k", value = changed)
        expect_error(dplyr::collect(query), "k.*changed|changed.*k",
                     class = "marginplyr_error", info = empty)
      }
    }
  }
})

# Each public verb reaches a different branch/fold boundary with these roles.
factor_schema_verbs <- list(
  expand = expand_with_margins,
  summary = function(.data, ...) {
    summarize_with_margins(.data, rows = list(.data$row_id), ...)
  },
  nest = nest_with_margins,
  nest_by = nest_by_with_margins
)

test_that("dtplyr validates factors before grouping and payload folding", {
  skip_if_suggest_absent("dtplyr")
  for (role in c("dimension", "fixed", "payload")) {
    for (label in list("Total", NULL)) {
      for (captured_na in c(FALSE, TRUE)) {
        for (verb_name in names(factor_schema_verbs)) {
          if (role == "payload" && verb_name == "summary") next
          input <- data.table::data.table(
            k = factor(c("a", "b"),
                       levels = c("a", "b", if (captured_na) NA),
                       exclude = NULL), h = "x", row_id = 1:2
          )
          source <- dtplyr::lazy_dt(input, immutable = TRUE)
          rowwise <- verb_name == "nest_by"
          plan <- if (role == "dimension") {
            grouping_sets(k, k, grouping_set())
          } else {
            grouping_sets(h, h, grouping_set())
          }
          verb <- factor_schema_verbs[[verb_name]]
          duplicate_policy <- if (verb_name %in% c("nest", "nest_by")) {
            "drop"
          } else {
            "keep"
          }
          build <- function() {
            if (role == "fixed") {
              verb(source, .by = k, .grouping = plan, .id = "set",
                   .margin_label = label, .duplicates = duplicate_policy)
            } else {
              verb(source, .grouping = plan, .id = "set", .margin_label = label,
                   .duplicates = duplicate_policy)
            }
          }
          if (rowwise) {
            # The wrapper collects immediately. Selection runs after the
            # schema snapshot and before that collection.
            mutate_after_metadata <- function(x) {
              data.table::set(input, j = "k", value = factor(c("z", "b")))
              role == "fixed" && is.factor(x)
            }
            rowwise_plan <- if (role == "dimension") rollup(k) else rollup(h)
            expect_error(verb(
              source, .by = tidyselect::where(mutate_after_metadata),
              .grouping = rowwise_plan, .margin_label = label
            ), "k.*changed|changed.*k",
            class = "marginplyr_error", info = verb_name)
            next
          }
          query <- build()
          data.table::set(input, j = "k", value = factor(c("z", "b")))
          expect_error(dplyr::collect(query), "k.*changed|changed.*k",
                       class = "marginplyr_error", info = verb_name)
        }
      }
    }
  }
})

test_that("same-schema value changes remain lazy and preserve factor codes", {
  skip_if_suggest_absent("dtplyr")
  for (ordered in c(FALSE, TRUE)) {
    for (label in list("Total", NULL)) {
      for (verb in factor_schema_verbs[c("expand", "summary", "nest")]) {
        input <- data.table::data.table(
          `..marginplyr_missing_factor_code` = structure(
            c(1L, 2L, NA_integer_), levels = c("a", "b", NA_character_),
            class = if (ordered) c("ordered", "factor") else "factor"
          ), row_id = 1:3
        )
        source <- dtplyr::lazy_dt(input, immutable = TRUE)
        query <- verb(source,
                      .grouping = rollup(`..marginplyr_missing_factor_code`),
                      .id = "set", .margin_label = label)
        changed <- structure(
          c(2L, 3L, NA_integer_), levels = c("a", "b", NA_character_),
          class = if (ordered) c("ordered", "factor") else "factor"
        )
        data.table::set(input, j = "..marginplyr_missing_factor_code",
                        value = changed)
        before <- serialize(input, NULL)
        for (i in 1:2) {
          result <- dplyr::collect(query)
          frame <- dplyr::ungroup(result[result$set == 1L, ])
          ids <- if ("data" %in% names(frame)) {
            vapply(frame$data, function(cell) cell$row_id, integer(1))
          } else if ("rows" %in% names(frame)) {
            unlist(frame$rows)
          } else {
            frame$row_id
          }
          detail <- frame[["..marginplyr_missing_factor_code"]][order(ids)]
          expect_identical(as.integer(detail), c(2L, 3L, NA_integer_))
          expect_identical(is.ordered(detail), ordered)
          expect_identical(levels(detail), c("a", "b", NA_character_, label))
          expect_identical(is.na(detail), c(FALSE, FALSE, TRUE))
        }
        expect_identical(serialize(input, NULL), before)
      }
    }
  }
})
