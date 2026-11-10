# Variable labels and table metadata at the public Margin verb boundaries.

label_source <- function() {
  tibble::tibble(
    k = structure(c("one", "one"), label = "Fixed key"),
    g = structure(ordered(c("a", "b")), label = "Dimension"),
    x = structure(c(2, 4), label = "Measurement")
  )
}

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
  con <- DBI::dbConnect(duckdb::duckdb())
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
