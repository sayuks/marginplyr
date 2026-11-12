# Variable labels and table metadata at the public Margin verb boundaries.

label_source <- function() {
  tibble::tibble(
    k = structure(c("one", "one"), label = "Fixed key"),
    g = structure(ordered(c("a", "b")), label = "Dimension"),
    x = structure(c(2, 4), label = "Measurement")
  )
}

test_that("original nested columns retain labels after native slicing", {
  input <- tibble::tibble(
    g = c("b", "a"),
    payload = I(as.POSIXlt(c("2020-01-01", "2020-01-02"), tz = "UTC"))
  )
  attr(input$payload, "label") <- "Timestamp description"
  expect_identical(attr(input$payload, "label"), "Timestamp description")
  expect_identical(class(input$payload), c("AsIs", "POSIXlt", "POSIXt"))
  before <- serialize(input, NULL)
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (keep in c(FALSE, TRUE)) {
      result <- verb(input, .grouping = rollup(g), .keep = keep)
      expect_identical(vapply(result$data, nrow, integer(1)), c(1L, 1L, 2L))
      for (cell in result$data) {
        expect_identical(attr(cell$payload, "label"), "Timestamp description")
      }
    }
  }
  expect_identical(serialize(input, NULL), before)
})

test_that("ordered nested source labels cover payloads and retained keys", {
  skip_if_suggest_absent("tidyr")
  for (labels in list(c(NA_character_, ""), c("", NA_character_))) {
    input <- tibble::tibble(
      k = I(as.POSIXlt(rep("2020-01-01", 2L), tz = "UTC")),
      g = c("b", "a"),
      payload = I(as.POSIXlt(c("2020-01-01", "2020-01-02"), tz = "UTC"))
    )
    attr(input$k, "label") <- labels[[1L]]
    attr(input$g, "label") <- "Dimension"
    attr(input$payload, "label") <- labels[[2L]]
    expect_identical(lapply(input, attr, which = "label"),
                     list(k = labels[[1L]], g = "Dimension",
                          payload = labels[[2L]]))
    before <- serialize(input, NULL)
    for (rowwise in c(FALSE, TRUE)) {
      for (keep in c(FALSE, TRUE)) {
        native <- function(keys) {
          payload <- if (keep) names(input) else "payload"
          if (rowwise) {
            dplyr::ungroup(dplyr::nest_by(
              dplyr::select(input, dplyr::all_of(unique(c(keys, payload)))),
              !!!rlang::syms(keys), .keep = keep
            ))
          } else {
            tidyr::nest(input, data = dplyr::all_of(payload),
                        .by = dplyr::all_of(keys))
          }
        }
        detail <- native(c("k", "g"))
        total <- dplyr::mutate(native("k"), g = "Total")
        verb <- if (rowwise) nest_by_with_margins else nest_with_margins
        for (multi in c(FALSE, TRUE)) {
          expected <- if (multi) dplyr::union_all(detail, total) else detail
          actual <- verb(input, .by = k, .grouping = if (multi) rollup(g) else
                           grouping_set(g), .keep = keep, .sort = "last")
          expect_identical(actual$g, structure(c("a", "b", "Total"[multi]),
                                               label = "Dimension"))
          expect_identical(attr(actual$k, "label"), labels[[1L]])
          for (i in seq_along(actual$data)) {
            cell <- actual$data[[i]]
            reference <- expected$data[[match(actual$g[[i]], expected$g)]]
            expect_named(cell, if (keep) names(input) else "payload")
            expect_identical(nrow(cell), nrow(reference))
            for (column in names(cell)) {
              # Native slicing owns class state; ADR 0042 separately retains
              # the source variable label, including a retained original key.
              value <- reference[[column]]
              attr(value, "label") <- attr(input[[column]], "label")
              expect_identical(cell[[column]], value)
            }
          }
        }
      }
    }
    expect_identical(serialize(input, NULL), before)
  }
})

test_that("empty native-sliced nesting retains source label representations", {
  input <- tibble::tibble(
    g = character(), payload = I(as.POSIXlt(character(), tz = "UTC"))
  )
  attr(input$g, "label") <- ""
  attr(input$payload, "label") <- NA_character_
  expect_identical(attr(input$g, "label"), "")
  expect_identical(attr(input$payload, "label"), NA_character_)
  before <- serialize(input, NULL)
  native <- dplyr::nest_by(input)$data[[1L]]
  for (rowwise in c(FALSE, TRUE)) {
    verb <- if (rowwise) nest_by_with_margins else nest_with_margins
    for (keep in c(FALSE, TRUE)) {
      for (plan in list(NULL, grouping_set(g), rollup(g))) {
        result <- verb(input, .grouping = plan, .keep = keep, .sort = "last")
        expect_identical(nrow(result), as.integer(rowwise && is.null(plan)))
        if ("g" %in% names(result)) expect_identical(
          attr(result$g, "label"), ""
        )
        for (cell in result$data) {
          expect_named(cell, names(input))
          expect_identical(nrow(cell), 0L)
          expect_identical(attr(cell$g, "label"), "")
          expect_identical(attr(cell$payload, "label"), NA_character_)
          for (column in names(cell)) {
            value <- native[[column]]
            attr(value, "label") <- attr(input[[column]], "label")
            expect_identical(cell[[column]], value)
          }
        }
      }
    }
  }
  expect_identical(serialize(input, NULL), before)
})

test_that("source variable labels survive every local Margin verb", {
  source <- label_source()
  original <- source
  for (plan in list(grouping_set(g), rollup(g))) {
    for (sorting in c("none", "first", "last")) {
      summary <- summarize_with_margins(
        source, n = dplyr::n(), .by = k, .grouping = plan, .sort = sorting
      )
      alias <- summarise_with_margins(
        source, n = dplyr::n(), .by = k, .grouping = plan, .sort = sorting
      )
      expanded <- expand_with_margins(
        source, .by = k, .grouping = plan, .sort = sorting
      )
      for (result in list(summary, alias, expanded)) {
        expect_identical(attr(result$k, "label"), "Fixed key")
        expect_identical(attr(result$g, "label"), "Dimension")
        expect_true(is.ordered(result$g))
      }
      expect_identical(attr(expanded$x, "label"), "Measurement")
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        nested <- verb(
          source, .by = k, .grouping = plan, .sort = sorting, .keep = TRUE
        )
        expect_identical(attr(nested$k, "label"), "Fixed key")
        expect_identical(attr(nested$g, "label"), "Dimension")
        for (cell in nested$data) {
          expect_identical(attr(cell$k, "label"), "Fixed key")
          expect_identical(attr(cell$g, "label"), "Dimension")
          expect_identical(attr(cell$x, "label"), "Measurement")
          expect_true(all(as.character(cell$g) %in% c("a", "b")))
        }
      }
    }
  }
  expect_identical(source, original)
})

test_that("summary labels survive agreement and missing labels", {
  source <- label_source()
  for (labels in list(
    list(NULL, "Description"), list(NA_character_, "Description"),
    list("", NULL), list(NULL, NA_character_), list(NULL, NULL),
    list("First", "Second")
  )) {
    count <- 0L
    statistic <- function(x) {
      count <<- count + 1L
      structure(sum(x), label = labels[[count]])
    }
    expected <- if (identical(labels, list("First", "Second"))) {
      attr(vctrs::vec_c(structure(6, label = "First"),
                        structure(6, label = "Second")), "label")
    } else if (identical(labels, list(NULL, "Description")) ||
                 identical(labels, list(NA_character_, "Description"))) {
      "Description"
    } else if (identical(labels, list("", NULL))) {
      ""
    } else if (identical(labels, list(NULL, NA_character_))) {
      NA_character_
    } else {
      NULL
    }
    result <- summarize_with_margins(
      source, z = statistic(x),
      .grouping = grouping_sets(grouping_set(), grouping_set()),
      .duplicates = "keep"
    )
    expect_identical(attr(result$z, "label"), expected)
    expect_identical(unname(as.vector(result$z)), c(6, 6))
  }
  for (plan in list(NULL, grouping_set(g))) {
    result <- summarize_with_margins(
      source, x = mean(x), avg = mean(x),
      labelled = structure(sum(x), label = "Calculated"), .grouping = plan
    )
    native <- if (is.null(plan)) {
      dplyr::summarise(source, x = mean(x), avg = mean(x),
                       labelled = structure(sum(x), label = "Calculated"))
    } else {
      dplyr::summarise(source, x = mean(x), avg = mean(x),
                       labelled = structure(sum(x), label = "Calculated"),
                       .by = g)
    }
    for (column in c("x", "avg", "labelled")) {
      expect_identical(result[[column]], native[[column]])
    }
  }
})

# Dataset metadata is compared independently of structural frame fields.
frame_metadata <- function(data) {
  attributes(data)[setdiff(names(attributes(data)), c(
    "names", "row.names", "class", "groups", ".internal.selfref", "sorted"
  ))]
}

test_that("eager dataset metadata follows native expansion and nesting", {
  skip_if_suggest_absent("tidyr")
  for (kind in c("frame", "tibble")) {
    source <- label_source()
    if (kind == "frame") source <- as.data.frame(source)
    attr(source, "label") <- "Dataset description"
    attr(source, "independent") <- list(owner = "Source")
    for (empty in c(FALSE, TRUE)) {
      input <- if (empty) source[0, ] else source
      for (plan in list(grouping_set(g), rollup(g))) {
        expanded <- expand_with_margins(input, .by = k, .grouping = plan)
        native <- dplyr::mutate(input, g = as.character(g))
        if (identical(plan, rollup(g))) {
          native <- dplyr::union_all(native, dplyr::mutate(input, g = "Total"))
        }
        expect_identical(frame_metadata(expanded), frame_metadata(native))
        summary <- summarize_with_margins(
          input, n = dplyr::n(), .by = k, .grouping = plan
        )
        native_summary <- dplyr::summarise(input, n = dplyr::n(), .by = c(k, g))
        expect_identical(
          frame_metadata(summary), frame_metadata(native_summary)
        )
        for (keep in c(FALSE, TRUE)) {
          nested <- nest_with_margins(
            input, .by = k, .grouping = plan, .keep = keep
          )
          native_nested <- tidyr::nest(
            input, data = dplyr::all_of(if (keep) names(input) else "x"),
            .by = c(k, g)
          )
          expect_identical(
            frame_metadata(nested), frame_metadata(native_nested)
          )
          for (cell in nested$data) {
            expect_identical(frame_metadata(cell), list(
              label = "Dataset description",
              independent = list(owner = "Source")
            ))
          }
          rowwise <- nest_by_with_margins(
            input, .by = k, .grouping = plan, .keep = keep
          )
          native_rowwise <- dplyr::nest_by(input, k, g, .keep = keep)
          expect_identical(
            frame_metadata(rowwise), frame_metadata(native_rowwise)
          )
          for (cell in rowwise$data) {
            expect_identical(
              frame_metadata(cell), frame_metadata(native_rowwise$data[[1L]])
            )
          }
        }
      }
    }
  }
})

test_that("dtplyr keeps captured source labels and native frame metadata", {
  skip_if_suggest_absent("dtplyr")
  source <- label_source()
  source$g <- structure(as.character(source$g), label = "Dimension")
  attr(source, "label") <- "Dataset description"
  attr(source, "independent") <- "Source metadata"
  input <- dtplyr::lazy_dt(source)
  for (plan in list(grouping_set(g), rollup(g))) {
    summary <- summarize_with_margins(
      input, n = dplyr::n(), .by = k, .grouping = plan
    )
    expanded <- expand_with_margins(input, .by = k, .grouping = plan)
    native <- dplyr::mutate(input, g = as.character(g))
    if (identical(plan, rollup(g))) {
      native <- dplyr::union_all(native, dplyr::mutate(input, g = "Total"))
    }
    expect_identical(frame_metadata(dplyr::collect(expanded)),
                     frame_metadata(dplyr::collect(native)))
    for (query in list(summary, expanded)) {
      result <- dplyr::collect(query)
      expect_identical(attr(result$k, "label"), "Fixed key")
      expect_identical(attr(result$g, "label"), "Dimension")
    }
    expect_identical(attr(dplyr::collect(expanded)$x, "label"), "Measurement")
    for (keep in c(FALSE, TRUE)) {
      nested <- nest_with_margins(
        input, .by = k, .grouping = plan, .keep = keep
      )
      result <- dplyr::collect(nested)
      expect_length(frame_metadata(result), 0L)
      for (cell in result$data) {
        expect_identical(frame_metadata(cell), list(
          label = "Dataset description", independent = "Source metadata"
        ))
        expect_identical(attr(cell$x, "label"), "Measurement")
        if (keep) expect_identical(attr(cell$g, "label"), "Dimension")
      }
      rowwise <- nest_by_with_margins(
        input, .by = k, .grouping = plan, .keep = keep
      )
      for (cell in rowwise$data) {
        expect_length(frame_metadata(cell), 0L)
        expect_identical(attr(cell$x, "label"), "Measurement")
      }
    }
  }
  captured <- expand_with_margins(input, .by = k, .grouping = rollup(g))
  data.table::setattr(input$parent$g, "label", "Changed dimension")
  expect_identical(attr(dplyr::collect(captured)$g, "label"), "Dimension")
  rebuilt <- expand_with_margins(input, .by = k, .grouping = rollup(g))
  expect_identical(
    attr(dplyr::collect(rebuilt)$g, "label"), "Changed dimension"
  )
})

test_that("labelled dtplyr unions return data.tables updatable by reference", {
  skip_if_suggest_absent("dtplyr")
  source <- data.frame(
    year = c(1L, 1L, 2L, 2L), region = c("a", "b", "a", "b"), v = 1:4
  )
  attr(source$year, "label") <- "Fiscal year"
  table <- data.table::as.data.table(source)
  before <- data.table::copy(table)
  input <- dtplyr::lazy_dt(table)
  native <- dplyr::union_all(
    dplyr::summarise(input, tot = sum(v), .by = c(year, region)),
    dplyr::summarise(input, tot = sum(v), .by = year) |>
      dplyr::mutate(region = "Total") |>
      dplyr::select(year, region, tot)
  )
  add_flag <- function(x) data.table::set(x, j = "flag", value = TRUE)
  queries <- list(
    native = native,
    summary = summarize_with_margins(
      input, tot = sum(v), .by = year, .grouping = rollup(region)
    ),
    identified = summarize_with_margins(
      input, tot = sum(v), .by = year, .grouping = rollup(region),
      .id = "sid", .margin_label = NA_character_
    ),
    expanded = expand_with_margins(
      input, .by = year, .grouping = rollup(region)
    )
  )
  for (query in queries) {
    expect_no_warning(dplyr::collect(dplyr::mutate(query, z = 1L)))
    result <- data.table::as.data.table(query)
    expect_identical(attr(result$year, "label"), "Fiscal year")
    add_flag(result)
    expect_true("flag" %in% names(result))
  }
  expect_identical(table, before)
})

test_that("dtplyr summary combination keeps native labels without inference", {
  skip_if_suggest_absent("dtplyr")
  input <- dtplyr::lazy_dt(label_source())
  for (plan in list(NULL, grouping_set(g))) {
    for (output in c("x", "avg", "labelled")) {
      expression <- if (output == "labelled") {
        rlang::expr(structure(sum(x), label = "Calculated"))
      } else {
        rlang::expr(mean(x))
      }
      result <- dplyr::collect(summarize_with_margins(
        input, !!output := !!expression, .grouping = plan
      ))
      native <- dplyr::summarise(
        input, !!output := !!expression,
        .by = dplyr::all_of(if (is.null(plan)) character() else "g")
      )
      native <- dplyr::collect(native)
      expect_identical(result[[output]], native[[output]])
    }
  }
  result <- dplyr::collect(summarize_with_margins(
    input, avg = mean(x),
    .grouping = grouping_sets(grouping_set(), grouping_set(g))
  ))
  expect_identical(attr(result$avg, "label"), "Measurement")
})

test_that("Arrow labels follow source and native summaries", {
  skip_if_suggest_absent("arrow")
  source <- label_source()
  attr(source, "label") <- "Dataset description"
  attr(source, "independent") <- "Source metadata"
  input <- arrow::Table$create(source)
  for (plan in list(grouping_set(g), rollup(g))) {
    for (sorting in c("none", "last")) {
      summary <- summarize_with_margins(input, n = dplyr::n(), .by = k,
                                        .grouping = plan, .sort = sorting)
      expanded <- expand_with_margins(
        input, .by = k, .grouping = plan, .sort = sorting
      )
      for (query in list(summary, expanded)) {
        result <- dplyr::collect(query)
        expect_identical(attr(result$k, "label"), "Fixed key")
        expect_identical(attr(result$g, "label"), "Dimension")
      }
      result <- dplyr::collect(expanded)
      native <- dplyr::mutate(input, g = as.character(g))
      if (identical(plan, rollup(g))) {
        native <- dplyr::union_all(native, dplyr::mutate(input, g = "Total"))
      }
      expect_identical(attr(result$x, "label"), "Measurement")
      expect_identical(frame_metadata(result),
                       frame_metadata(dplyr::collect(native)))
    }
  }
  for (plan in list(NULL, grouping_set(g))) {
    for (output in c("x", "avg")) {
      expression <- rlang::expr(mean(x))
      result <- dplyr::collect(summarize_with_margins(
        input, !!output := !!expression, .grouping = plan
      ))
      native <- dplyr::collect(dplyr::summarise(
        input, !!output := !!expression,
        .by = dplyr::all_of(if (is.null(plan)) character() else "g")
      ))
      expect_identical(result[[output]], native[[output]])
    }
  }
})

test_that("source labels survive empty and typed-missing results", {
  for (label in list("Description", "", NA_character_, NULL)) {
    for (empty in c(FALSE, TRUE)) {
      source <- tibble::tibble(
        g = structure(if (empty) integer() else 1:2, label = label),
        x = structure(if (empty) double() else c(2, 4), label = label)
      )
      original <- source
      for (margin_label in list("Total", NA_character_, NULL)) {
        for (plan in list(grouping_set(g), rollup(g))) {
          summary <- summarize_with_margins(
            source, n = dplyr::n(),
            .grouping = plan, .margin_label = margin_label
          )
          expanded <- expand_with_margins(source, .grouping = plan,
                                          .margin_label = margin_label)
          expect_identical(attr(summary$g, "label"), label)
          expect_identical(attr(expanded$g, "label"), label)
          expect_identical(attr(expanded$x, "label"), label)
          for (verb in list(nest_with_margins, nest_by_with_margins)) {
            nested <- verb(
              source, .grouping = plan, .margin_label = margin_label,
              .keep = TRUE
            )
            expect_identical(attr(nested$g, "label"), label)
            for (cell in nested$data) {
              expect_identical(attr(cell$g, "label"), label)
              expect_identical(attr(cell$x, "label"), label)
            }
          }
        }
      }
      expect_identical(source, original)
    }
  }
})

test_that("native no-payload cells retain row counts and dataset metadata", {
  skip_if_suggest_absent("tidyr")
  source <- structure(tibble::tibble(g = c("a", "b")),
                      label = "Dataset description", independent = "Other")
  for (empty in c(FALSE, TRUE)) {
    input <- if (empty) source[0, ] else source
    result <- nest_with_margins(input, .grouping = rollup(g))
    expect_identical(
      vapply(result$data, nrow, integer(1)),
      if (empty) integer() else c(1L, 1L, 2L)
    )
    for (cell in result$data) {
      expect_named(cell, character())
      expect_identical(frame_metadata(cell), frame_metadata(source))
    }
    by <- nest_by_with_margins(input)
    native_by <- dplyr::nest_by(input)
    expect_identical(frame_metadata(by), frame_metadata(native_by))
    expect_identical(frame_metadata(by$data[[1L]]),
                     frame_metadata(native_by$data[[1L]]))
    expect_identical(nrow(by$data[[1L]]), nrow(input))
  }
})

test_that("dtplyr projected nesting retains its native source-frame metadata", {
  skip_if_suggest_absent("dtplyr")
  source <- structure(label_source(), label = "Dataset", independent = "Other")
  input <- dplyr::select(dtplyr::lazy_dt(source), g, x)
  native <- dplyr::collect(tidyr::nest(input, data = x))
  result <- dplyr::collect(nest_with_margins(input, .grouping = rollup(g)))
  for (cell in result$data) {
    expect_identical(frame_metadata(cell), frame_metadata(native$data[[1L]]))
  }
})

test_that("direct data.table metadata uses its native frame references", {
  skip_if_suggest_absent("data.table")
  source <- data.table::as.data.table(label_source())
  source$g <- structure(as.character(source$g), label = "Dimension")
  data.table::setattr(source, "label", "Dataset")
  data.table::setattr(source, "independent", "Other")
  original <- data.table::copy(source)
  for (plan in list(grouping_set(g), rollup(g))) {
    result <- expand_with_margins(source, .by = k, .grouping = plan)
    native <- dplyr::mutate(source, g = as.character(g))
    if (identical(plan, rollup(g))) {
      native <- dplyr::union_all(native, dplyr::mutate(source, g = "Total"))
    }
    expect_identical(frame_metadata(result), frame_metadata(native))
    expect_identical(attr(result$g, "label"), "Dimension")
    expect_identical(attr(result$x, "label"), "Measurement")
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      result <- verb(source, .by = k, .grouping = plan, .keep = TRUE)
      expect_length(frame_metadata(result), 0L)
      for (cell in result$data) {
        expect_length(frame_metadata(cell), 0L)
        expect_identical(attr(cell$g, "label"), "Dimension")
        expect_identical(attr(cell$x, "label"), "Measurement")
      }
    }
  }
  expect_identical(source, original)
})

test_that("SQLite collection retains native metadata", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  input <- dplyr::copy_to(con, label_source(), "label_source", temporary = TRUE)
  for (plan in list(grouping_set(g), rollup(g))) {
    summary <- dplyr::collect(summarize_with_margins(
      input, avg = mean(x), .grouping = plan
    ))
    native <- dplyr::collect(dplyr::summarise(input, avg = mean(x), .by = g))
    expect_identical(attributes(summary$avg), attributes(native$avg))
    expect_identical(frame_metadata(summary), frame_metadata(native))
    expanded <- dplyr::collect(expand_with_margins(input, .grouping = plan))
    native <- dplyr::collect(dplyr::mutate(input, g = as.character(g)))
    expect_identical(frame_metadata(expanded), frame_metadata(native))
    expect_identical(attr(expanded$g, "label"), attr(native$g, "label"))
  }
})

test_that("DuckDB collection retains native metadata", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  input <- dplyr::copy_to(con, label_source(), "label_source", temporary = TRUE)
  for (plan in list(grouping_set(g), rollup(g))) {
    summary <- dplyr::collect(summarize_with_margins(
      input, avg = mean(x), .grouping = plan
    ))
    native <- dplyr::collect(dplyr::summarise(input, avg = mean(x), .by = g))
    expect_identical(attributes(summary$avg), attributes(native$avg))
    expect_identical(frame_metadata(summary), frame_metadata(native))
    expanded <- dplyr::collect(expand_with_margins(input, .grouping = plan))
    native <- dplyr::collect(dplyr::mutate(input, g = as.character(g)))
    expect_identical(frame_metadata(expanded), frame_metadata(native))
    expect_identical(attr(expanded$g, "label"), attr(native$g, "label"))
  }
})

test_that("Arrow labels do not transfer after ordinary operations", {
  skip_if_suggest_absent("arrow")
  input <- arrow::Table$create(label_source())
  result <- expand_with_margins(input, .grouping = rollup(g))
  table <- dplyr::collect(result, as_data_frame = FALSE)
  expect_identical(attr(dplyr::collect(table)$g, "label"), "Dimension")
  changed <- dplyr::mutate(result, g = "Replacement")
  native <- dplyr::mutate(input, g = "Replacement")
  expect_identical(attr(dplyr::collect(changed)$g, "label"),
                   attr(dplyr::collect(native)$g, "label"))
  twice <- expand_with_margins(
    result, .grouping = rollup(g), .check_margin_label = FALSE
  )
  expect_identical(attr(dplyr::collect(twice)$g, "label"), "Dimension")
})

test_that("label combination retains native dtplyr union failures", {
  skip_if_suggest_absent("dtplyr")
  source <- label_source()
  source$g <- as.character(source$g)
  input <- dtplyr::lazy_dt(source)
  detailed <- dplyr::summarise(
    input, z = if (dplyr::n() == 1L) 1L else "two", .by = g
  )
  total <- dplyr::mutate(
    dplyr::summarise(input, z = if (dplyr::n() == 1L) 1L else "two"),
    g = "Total"
  )
  total <- dplyr::select(total, g, z)
  native <- tryCatch(
    dplyr::collect(dplyr::union_all(detailed, total)), error = identity
  )
  result <- tryCatch(dplyr::collect(summarize_with_margins(
    input, z = if (dplyr::n() == 1L) 1L else "two", .grouping = rollup(g)
  )), error = identity)
  expect_s3_class(native, "error")
  expect_identical(class(result), class(native))
  expect_false(inherits(result, "marginplyr_error"))
})

test_that("nesting applies native inner metadata to original values per set", {
  skip_if_suggest_absent("tidyr")
  source <- structure(label_source(), label = "Dataset", independent = "Other")
  for (keep in c(FALSE, TRUE)) {
    result <- nest_with_margins(
      source, .grouping = rollup(g), .keep = keep, .id = "set"
    )
    for (set in 1:2) {
      keys <- if (set == 1L) "g" else character()
      payload <- if (keep) names(source) else c("k", "x")
      native <- tidyr::nest(
        source, data = dplyr::all_of(payload), .by = dplyr::all_of(keys)
      )
      rows <- which(result$set == set)
      for (row in rows) {
        index <- if (set == 1L) {
          match(as.character(result$g[[row]]), as.character(native$g))
        } else {
          1L
        }
        cell <- result$data[[row]]
        expected <- native$data[[index]]
        expect_identical(frame_metadata(cell), frame_metadata(expected))
        expect_setequal(names(cell), names(expected))
        for (column in names(expected)) {
          expect_identical(cell[[column]], expected[[column]])
        }
      }
    }
    rowwise <- nest_by_with_margins(
      source, .grouping = rollup(g), .keep = keep, .id = "set"
    )
    total <- rowwise$data[[which(rowwise$set == 2L)]]
    native_total <- dplyr::nest_by(source)
    expect_identical(
      frame_metadata(total), frame_metadata(native_total$data[[1L]])
    )
    expect_identical(nrow(total), 2L)
  }
})

test_that("unusual labels add no validation and generated outputs infer none", {
  source <- label_source()
  attr(source$g, "label") <- 1:2
  expect_no_error(expand_with_margins(source, .grouping = rollup(g)))
  attr(source, "label") <- "Dataset"
  summary <- summarize_with_margins(
    source, total = sum(x), share = share_of_total(total),
    .grouping = rollup(g), .id = "set"
  )
  expect_null(attr(summary$share, "label"))
  expect_null(attr(summary$set, "label"))
  expect_null(attr(inspect_grouping(source, .grouping = rollup(g)), "label"))
})

test_that("shares preserve native calculated labels without weakening guards", {
  skip_if_suggest_absent("dtplyr")
  source <- label_source()
  source$g <- as.character(source$g)
  input <- dtplyr::lazy_dt(source)
  for (optimization in 0:2) {
    withr::local_options(list(datatable.optimize = optimization))
    for (fun in c("mean", "sum", "min", "max", "median", "prod", "var", "sd")) {
      expression <- rlang::call2(fun, quote(x))
      native <- dplyr::collect(dplyr::summarise(
        input, avg = !!expression, .by = g
      ))
      result <- dplyr::collect(summarize_with_margins(
        input, avg = !!expression, share = share_of_total(avg),
        .grouping = rollup(g)
      ))
      expect_identical(attr(result$avg, "label"), attr(native$avg, "label"))
      expect_null(attr(result$share, "label"))
    }
  }
  env <- rlang::env(source = source, mean = base::mean)
  input <- rlang::eval_bare(quote(dtplyr::lazy_dt(source)), env)
  summary <- rlang::new_quosure(quote(mean(x)), env)
  result <- summarize_with_margins(
    input, avg = !!summary, share = share_of_total(avg), .grouping = rollup(g)
  )
  env$mean <- function(x) "refused"
  error <- tryCatch(dplyr::collect(result), error = identity)
  expect_s3_class(error, "marginplyr_error")
  expect_match(conditionMessage(error), "plain integer or", fixed = TRUE)
  masked <- local({
    mean <- function(x) "refused"
    input <- dtplyr::lazy_dt(source)
    summarize_with_margins(
      input, avg = mean(x), share = share_of_total(avg), .grouping = rollup(g)
    )
  })
  error <- tryCatch(dplyr::collect(masked), error = identity)
  expect_s3_class(error, "marginplyr_error")
  expect_match(conditionMessage(error), "plain integer or", fixed = TRUE)
})

test_that("native classed aggregates remain ineligible share sources", {
  skip_if_suggest_absent("dtplyr")
  source <- label_source()
  source$g <- as.character(source$g)
  source$x <- structure(
    c(2, 4), class = "margin_label_numeric", label = "Measurement"
  )
  registry <- get(".__S3MethodsTable__.", asNamespace("base"))
  registerS3method("mean", "margin_label_numeric", function(x, ...) "refused",
                   envir = asNamespace("base"))
  withr::defer(rm("mean.margin_label_numeric", envir = registry))
  input <- dtplyr::lazy_dt(source)
  for (optimization in 1:2) {
    withr::local_options(list(datatable.optimize = optimization))
    functions <- if (optimization == 1L) {
      "mean"
    } else {
      c("mean", "sum", "min", "max", "median", "prod")
    }
    for (fun in functions) {
      expression <- rlang::call2(fun, quote(x))
      native <- dplyr::collect(dplyr::summarise(
        input, avg = !!expression, .by = g
      ))
      expect_s3_class(native$avg, "margin_label_numeric")
      result <- summarize_with_margins(
        input, avg = !!expression, share = share_of_total(avg),
        .grouping = rollup(g)
      )
      error <- tryCatch(dplyr::collect(result), error = identity)
      expect_s3_class(error, "marginplyr_error")
      expect_match(
        conditionMessage(error), "margin_label_numeric", fixed = TRUE
      )
    }
  }
  withr::local_options(list(datatable.optimize = 0L))
  result <- summarize_with_margins(
    input, avg = mean(x), share = share_of_total(avg), .grouping = rollup(g)
  )
  error <- tryCatch(dplyr::collect(result), error = identity)
  expect_s3_class(error, "marginplyr_error")
  expect_match(conditionMessage(error), "character", fixed = TRUE)
})

test_that("native first and last labels survive share validation", {
  skip_if_suggest_absent("dtplyr")
  withr::local_options(list(datatable.optimize = 2L))
  source <- label_source()
  source$g <- as.character(source$g)
  input <- dtplyr::lazy_dt(source)
  input$env <- rlang::env(parent = input$env,
                          first = data.table::first, last = data.table::last)
  for (fun in c("first", "last", "head", "tail")) {
    expression <- if (fun %in% c("head", "tail")) {
      rlang::call2(fun, quote(x), 1L)
    } else {
      rlang::call2(fun, quote(x))
    }
    native <- dplyr::collect(dplyr::summarise(
      input, avg = !!expression, .by = g
    ))
    result <- dplyr::collect(summarize_with_margins(
      input, avg = !!expression, share = share_of_total(avg),
      .grouping = rollup(g), .by = k
    ))
    expect_identical(attr(result$avg, "label"), attr(native$avg, "label"))
  }
})

test_that("ordinary dtplyr edits retain their values", {
  skip_if_suggest_absent("dtplyr")
  result <- summarize_with_margins(
    dtplyr::lazy_dt(label_source()), avg = mean(x), share = share_of_total(avg),
    .grouping = rollup(g)
  )
  changed <- dplyr::collect(dplyr::mutate(result, avg = 99))
  expect_identical(unname(as.vector(changed$avg)), rep(99, 3L))
})

test_that("qualified and partial native means retain labels through shares", {
  skip_if_suggest_absent("dtplyr")
  source <- label_source()
  source$g <- as.character(source$g)
  input <- dtplyr::lazy_dt(source)
  for (expression in list(quote(base::mean(x)), quote(mean(x, na = TRUE)))) {
    native <- dplyr::collect(dplyr::summarise(
      input, avg = !!expression, .by = g
    ))
    result <- dplyr::collect(summarize_with_margins(
      input, avg = !!expression, share = share_of_total(avg),
      .grouping = rollup(g)
    ))
    expect_identical(attr(result$avg, "label"), attr(native$avg, "label"))
  }
})

test_that("dtplyr no-payload metadata matches native nesting", {
  skip_if_suggest_absent("dtplyr")
  source <- structure(
    tibble::tibble(g = c("a", "b")), label = "Dataset", custom = "Other"
  )
  input <- dtplyr::lazy_dt(source)
  native <- dplyr::collect(tidyr::nest(
    input, data = dplyr::all_of(character())
  ))
  result <- dplyr::collect(nest_with_margins(input, .grouping = rollup(g)))
  for (cell in result$data) {
    expect_identical(frame_metadata(cell), frame_metadata(native$data[[1L]]))
  }
  expect_identical(vapply(result$data, nrow, integer(1)), c(1L, 1L, 2L))
})

test_that("plain-frame index and sorted attributes remain dataset metadata", {
  skip_if_suggest_absent("tidyr")
  for (convert in list(identity, as.data.frame)) {
    source <- convert(tibble::tibble(g = c("a", "b"), x = 1:2))
    attr(source, "index") <- "Dataset index description"
    attr(source, "sorted") <- "Dataset sorting description"
    for (plan in list(grouping_set(g), rollup(g))) {
      actual <- nest_with_margins(source, .grouping = plan)
      native <- tidyr::nest(source, data = x, .by = g)
      for (cell in actual$data) {
        for (name in c("index", "sorted")) {
          expect_identical(attr(cell, name, exact = TRUE),
                           attr(native$data[[1L]], name, exact = TRUE))
        }
      }
    }
  }
})

test_that("native scalar grammar preserves summary metadata and errors", {
  skip_if_suggest_absent("dtplyr")
  source <- tibble::tibble(g = c("a", "a", "b"),
                           x = structure(c(1, 2, 4), label = "Measure"),
                           w = c(1, 2, 1))
  expressions <- list(quote(x[1L]), quote(x[[1L]]), quote(x[1.5]),
                      quote(weighted.mean(x, w)),
                      quote(weighted.mean(x)),
                      quote(mean(x, na.rm = (TRUE))),
                      quote(sum(x, na.rm = (TRUE))),
                      quote(mean(x, na.rm = 1)))
  for (optimization in 0:2) {
    withr::local_options(list(datatable.optimize = optimization))
    for (classed in c(FALSE, TRUE)) {
      input <- source
      if (classed) class(input$x) <- "margin_native_scalar"
      input <- dtplyr::lazy_dt(input)
      for (expression in expressions) {
        native <- tryCatch(dplyr::collect(dplyr::summarise(
          input, value = !!expression, .by = g
        )), error = identity)
        result <- summarize_with_margins(
          input, value = !!expression, p = share_of_total(value),
          .grouping = rollup(g)
        )
        actual <- tryCatch(dplyr::collect(result), error = identity)
        if (inherits(native, "error")) {
          expect_s3_class(actual, "error")
          expect_identical(class(actual), class(native))
          expect_identical(conditionMessage(actual), conditionMessage(native))
        } else if (is.object(native$value)) {
          expect_s3_class(actual, "marginplyr_error")
          expect_match(conditionMessage(actual), "margin_native_scalar",
                       fixed = TRUE)
        } else {
          expect_false(inherits(actual, "error"))
          expect_identical(attr(actual$value, "label"),
                           attr(native$value, "label"))
          expect_identical(class(actual$value), class(native$value))
        }
      }
    }
  }
})

test_that("native optimization inspects weight classes only at execution", {
  skip_if_suggest_absent("dtplyr")
  withr::local_options(list(datatable.optimize = 2L))
  source <- tibble::tibble(g = c("a", "a", "b"),
                           x = structure(c(1, 2, 4), label = "Measure"),
                           w = structure(c(1, 2, 1),
                                         class = "margin_native_weights"))
  for (answer in c(TRUE, FALSE)) {
    calls <- 0L
    registerS3method("is.numeric", "margin_native_weights", function(x) {
      calls <<- calls + 1L
      answer
    }, envir = asNamespace("base"))
    registry <- get(".__S3MethodsTable__.", asNamespace("base"))
    withr::defer({
      if (exists("is.numeric.margin_native_weights", registry,
                 inherits = FALSE)) {
        rm("is.numeric.margin_native_weights", envir = registry)
      }
    })
    input <- dtplyr::lazy_dt(source)
    native <- dplyr::collect(dplyr::summarise(
      input, value = weighted.mean(x, w), .by = g
    ))
    expected_calls <- calls
    calls <- 0L
    result <- summarize_with_margins(
      input, value = weighted.mean(x, w), p = share_of_total(value),
      .grouping = rollup(g)
    )
    expect_identical(calls, 0L)
    actual <- tryCatch(dplyr::collect(result), error = identity)
    expect_identical(calls, expected_calls)
    if (is.object(native$value)) {
      expect_s3_class(actual, "marginplyr_error")
      expect_match(conditionMessage(actual), class(native$value)[[1L]],
                   fixed = TRUE)
    } else {
      expect_identical(attr(actual$value, "label"),
                       attr(native$value, "label"))
    }
    rm("is.numeric.margin_native_weights", envir = registry)
  }
})

test_that("native scalar checks precede empty-column padding", {
  skip_if_suggest_absent("dtplyr")
  withr::local_options(list(datatable.optimize = 2L))
  source <- tibble::tibble(g = c("a", "a", "b"), x = c(1, 2, 4),
                           w = structure(c(1, 2, 1),
                                         class = "margin_padding_weights"))
  registerS3method("is.numeric", "margin_padding_weights", function(x) FALSE,
                   envir = asNamespace("base"))
  registerS3method("Summary", "margin_padding_weights", function(...) numeric(),
                   envir = asNamespace("base"))
  registry <- get(".__S3MethodsTable__.", asNamespace("base"))
  withr::defer(rm("is.numeric.margin_padding_weights",
                  "Summary.margin_padding_weights", envir = registry))
  input <- dtplyr::lazy_dt(source)
  native <- dplyr::collect(dplyr::summarise(
    input, value = weighted.mean(x, w), other = 1, .by = g
  ))
  expect_identical(native$value, c(NA_real_, NA_real_))
  result <- summarize_with_margins(
    input, value = weighted.mean(x, w), other = 1, p = share_of_total(value),
    .grouping = rollup(g)
  )
  expect_error(dplyr::collect(result),
               class = "marginplyr_share_cardinality_error")
  ordinary <- summarize_with_margins(
    input, extra = weighted.mean(x, w), value = sum(x),
    p = share_of_total(value),
    .grouping = rollup(g)
  )
  expect_error(dplyr::collect(ordinary),
               class = "marginplyr_share_cardinality_error")
})

test_that("native optimized non-scalar primitives retain share guards", {
  skip_if_suggest_absent("dtplyr")
  withr::local_options(list(datatable.optimize = 2L))
  input <- dtplyr::lazy_dt(tibble::tibble(g = c("a", "a", "b"), x = 1:3))
  for (expression in list(quote(head(x, 2L)),
                          quote(data.table::shift(x)),
                          quote(data.table::shift(x, n = c(1L, 2L))))) {
    result <- summarize_with_margins(
      input, value = !!expression, p = share_of_total(value),
      .grouping = rollup(g)
    )
    expect_error(dplyr::collect(result),
                 class = "marginplyr_share_cardinality_error")
    ordinary <- summarize_with_margins(
      input, extra = !!expression, value = sum(x), p = share_of_total(value),
      .grouping = rollup(g)
    )
    expect_error(dplyr::collect(ordinary),
                 class = "marginplyr_share_cardinality_error")
  }
})

test_that("native summary choices defer active and promised callees", {
  skip_if_suggest_absent("dtplyr")
  for (binding in c("active", "promise")) {
    calls <- 0L
    env <- rlang::env(
      source = tibble::tibble(g = c("a", "a", "b"), x = c(1, 2, 4)),
      weighted.mean = stats::weighted.mean
    )
    input <- rlang::eval_bare(quote(dtplyr::lazy_dt(source)), env)
    summary <- rlang::new_quosure(quote(weighted.mean(x)), env)
    result <- summarize_with_margins(
      input, value = !!summary, p = share_of_total(value), .grouping = rollup(g)
    )
    rm("weighted.mean", envir = env)
    if (binding == "active") {
      makeActiveBinding("weighted.mean", function() {
        calls <<- calls + 1L
        function(x) sum(x)
      }, env)
    } else {
      provide <- function() {
        calls <<- calls + 1L
        function(x) sum(x)
      }
      delayedAssign("weighted.mean", provide(), assign.env = env)
    }
    expect_identical(calls, 0L)
    actual <- dplyr::collect(result)
    expect_identical(calls, if (binding == "active") 3L else 1L)
    expect_identical(actual$value, c(3, 4, 7))
    expect_equal(actual$p, c(3 / 7, 4 / 7, 1))
  }
})

test_that("native summaries retain one parent evaluation and ordinary edits", {
  skip_if_suggest_absent("dtplyr")
  calls <- 0L
  touch <- function(x) {
    calls <<- calls + 1L
    x
  }
  input <- dtplyr::lazy_dt(tibble::tibble(
    g = c("a", "a", "b"), x = structure(c(1, 2, 4), label = "Measure")
  )) |>
    dplyr::filter(touch(x) > 0)
  native <- dplyr::union_all(
    dplyr::summarise(input, value = mean(x), .by = g),
    dplyr::relocate(
      dplyr::mutate(dplyr::summarise(input, value = mean(x)), g = "Total"), g
    )
  )
  calls <- 0L
  expected <- dplyr::collect(native)
  expected_calls <- calls
  result <- summarize_with_margins(
    input, value = mean(x), p = share_of_total(value), .grouping = rollup(g)
  )
  calls <- 0L
  actual <- dplyr::collect(result)
  expect_identical(calls, expected_calls)
  expect_identical(calls, 2L)
  expect_equal(unname(as.vector(actual$value)),
               unname(as.vector(expected$value)))
  expect_identical(attr(actual$value, "label"), attr(expected$value, "label"))
  changed <- dplyr::collect(dplyr::mutate(result, value = 99))
  expect_identical(unname(as.vector(changed$value)), rep(99, 3L))
  resummarized <- dplyr::collect(dplyr::summarise(result, value = sum(value)))
  expect_equal(resummarized$value, sum(actual$value))
})

test_that("native source refusal precedes later primitive evaluation", {
  skip_if_suggest_absent("dtplyr")
  withr::local_options(list(datatable.optimize = 2L))
  input <- dtplyr::lazy_dt(tibble::tibble(
    g = c("a", "a", "b"),
    x = structure(c(1, 2, 4), class = "margin_native_scalar"), y = 1:3
  ))
  calls <- 0L
  result <- summarize_with_margins(
    input, value = sum(x), later = mean(y, na.rm = {
      warning("Later native argument")
      TRUE
    }),
    p = share_of_total(value), .grouping = rollup(g)
  )
  expect_identical(calls, 0L)
  capture_warning <- function(cnd) {
    calls <<- calls + 1L
    invokeRestart("muffleWarning")
  }
  actual <- tryCatch(
    withCallingHandlers(dplyr::collect(result), warning = capture_warning),
    error = identity
  )
  expect_s3_class(actual, "marginplyr_error")
  expect_identical(calls, 0L)
})
