expect_selected_expansion <- function(query, expected, sort) {
  canonical <- function(data) {
    dplyr::arrange(as.data.frame(data), !!!rlang::syms(names(expected)))
  }
  for (result in list(
    dplyr::collect(query),
    dplyr::collect(dplyr::compute(query, temporary = TRUE, analyze = FALSE))
  )) {
    expect_identical(names(result), names(expected))
    expect_equal(canonical(result), canonical(expected))
    if (sort != "none") {
      expect_identical(result$g, expected$g)
    }
  }
  for (n in c(0L, 3L, nrow(expected), nrow(expected) + 1L)) {
    result <- dplyr::collect(query, n = n)
    expect_identical(names(result), names(expected))
    expect_identical(nrow(result), min(n, nrow(expected)))
    if (n == 0L) {
      if ("set" %in% names(expected)) {
        expect_identical(result$set, integer())
      }
    } else if (n == 3L) {
      expect_identical(nrow(dplyr::anti_join(
        result, expected, by = names(expected), na_matches = "na"
      )), 0L)
      expect_identical(anyDuplicated(result), 0L)
    } else {
      expect_equal(canonical(result), canonical(expected))
    }
  }
}

check_expansion_input_limits <- function(con) {
  source <- dplyr::copy_to(
    con, data.frame(g = c("c", "b", "a"), v = 1:3), "facts"
  )
  selected <- head(dplyr::arrange(source, .data$v), 2L)
  local <- dplyr::collect(selected)
  expect_identical(local$v, 1:2)
  specifications <- list(
    grouping_sets(grouping_set(), grouping_set("g")),
    grouping_sets(grouping_set("g"), grouping_set()),
    rollup("g")
  )
  materialized <- dplyr::compute(selected, temporary = TRUE, analyze = FALSE)
  for (spec in specifications) {
    for (label in list(NULL, NA_character_, "Total")) {
      for (id in list(NULL, "set")) {
        for (sort in c("none", "first", "last")) {
          expected <- expand_with_margins(
            local, .grouping = spec, .margin_label = label,
            .id = id, .sort = sort
          )
          for (input in list(selected, dplyr::collapse(selected))) {
            query <- expand_with_margins(
              input, .grouping = spec, .margin_label = label,
              .id = id, .sort = sort
            )
            expect_selected_expansion(query, expected, sort)
          }
        }
      }
    }
    control <- expand_with_margins(
      materialized, .grouping = spec, .margin_label = NULL, .id = "set"
    )
    expect_selected_expansion(control, expand_with_margins(
      local, .grouping = spec, .margin_label = NULL, .id = "set"
    ), "none")
  }

  windowed <- head(dplyr::mutate(
    dplyr::arrange(source, .data$v), rank = dplyr::row_number()
  ), 2L)
  query <- expand_with_margins(
    windowed, .grouping = rollup("g"), .margin_label = NULL, .id = "set"
  )
  expect_selected_expansion(query, expand_with_margins(
    dplyr::collect(windowed), .grouping = rollup("g"),
    .margin_label = NULL, .id = "set"
  ), "none")

  one_set <- expand_with_margins(
    selected, .grouping = grouping_set("g"), .margin_label = NULL
  )
  ranked <- dplyr::arrange(dplyr::collect(
    dplyr::mutate(one_set, rank = dplyr::row_number())
  ), .data$rank)
  expect_identical(ranked$g, c("c", "b"))
  expect_equal(ranked$rank, 1:2)
}

# Exercise selection shapes that can expose a clause at compound scope.
# The caller supplies a live connection; construction is observed at DBI.
check_expansion_shapes <- function(con) {
  source <- dplyr::copy_to(
    con, data.frame(g = c("c", "b", "a"), v = 1:3), "selection_shapes"
  )
  ordered <- dplyr::arrange(source, .data$v)
  inputs <- list(head(source, 2L), ordered, head(ordered, 2L),
                 dplyr::collapse(head(ordered, 2L)))
  for (input in inputs) {
    local <- dplyr::collect(input)
    for (spec in list(grouping_sets(grouping_set(), grouping_set("g")),
                      rollup("g"))) {
      for (label in list(NULL, "Total")) {
        for (sort in c("none", "first", "last")) {
          expected <- expand_with_margins(
            local, .grouping = spec, .margin_label = label,
            .id = "set", .sort = sort
          )
          build <- function() {
            expand_with_margins(
              input, .grouping = spec, .margin_label = label,
              .id = "set", .sort = sort
            )
          }
          # ADR 0020 permits DuckDB's zero-row selection proxy.
          query <- if (inherits(con, "duckdb_connection")) {
            build()
          } else {
            testthat::with_mocked_bindings(
              build(),
              dbSendQuery = function(...) stop("Unexpected construction query"),
              dbExecute = function(...) stop("Unexpected construction command"),
              .package = "DBI"
            )
          }
          expect_selected_expansion(query, expected, sort)
        }
      }
    }
  }
}

# Build both Margin verbs over inputs ordered by a raw `sql()` term or a
# constant, which dbplyr's own `select()` cannot rename (#1622). Each result is
# compared with the same verb over the collected input, so the input's `head()`
# is what both sides read. The caller supplies a live connection.
check_raw_sql_order_inputs <- function(con) {
  source <- dplyr::copy_to(
    con,
    data.frame(g = c("a", "b", "a", "b"), h = c("x", "y", "y", "x"), v = 1:4),
    "raw_order_facts"
  )
  inputs <- list(
    head(dplyr::arrange(source, dbplyr::sql("v DESC")), 2L),
    head(dplyr::arrange(source, dbplyr::sql("v DESC NULLS LAST")), 2L),
    head(dplyr::arrange(source, .data$g, dbplyr::sql("v DESC")), 3L),
    head(dplyr::arrange(source, dbplyr::sql("v DESC"), .data$g), 3L),
    head(dplyr::arrange(source, 1L), 2L),
    dplyr::arrange(source, dbplyr::sql("random()")),
    dplyr::arrange(source, dbplyr::sql("v DESC"))
  )
  calls <- list(
    list(.grouping = rollup("g"), .id = "set", .sort = "none"),
    list(.grouping = rollup("g"), .id = "set", .sort = "first"),
    list(.grouping = rollup("g"), .id = "set", .sort = "last"),
    list(.grouping = rollup("h")),
    list(.grouping = rollup("h"), .by = "g", .sort = "last"),
    list(.grouping = grouping_set("g"), .id = "set")
  )
  verbs <- list(
    expand = function(data, args) {
      do.call(expand_with_margins, c(list(data), args))
    },
    summarize = function(data, args) {
      summary <- list(data, n = quote(dplyr::n()))
      do.call(summarize_with_margins, c(summary, args))
    }
  )
  canonical <- function(data) {
    data <- as.data.frame(data)
    dplyr::arrange(data, !!!rlang::syms(names(data)))
  }
  for (input in inputs) {
    local <- dplyr::collect(input)
    for (verb in names(verbs)) {
      for (args in calls) {
        info <- paste(verb, deparse1(args))
        build <- function() verbs[[verb]](input, args)
        # ADR 0020 permits DuckDB's zero-row selection proxy.
        query <- if (inherits(con, "duckdb_connection")) {
          build()
        } else {
          testthat::with_mocked_bindings(
            build(),
            dbSendQuery = function(...) stop("Unexpected construction query"),
            dbExecute = function(...) stop("Unexpected construction command"),
            .package = "DBI"
          )
        }
        expected <- verbs[[verb]](local, args)
        result <- dplyr::collect(query)
        expect_identical(names(result), names(expected), info = info)
        expect_equal(canonical(result), canonical(expected), info = info)
      }
      # The caller-requested label-collision query reads the same input.
      checked <- list(.grouping = rollup("g"), .check_margin_label = TRUE)
      expect_equal(
        canonical(dplyr::collect(
          verbs[[verb]](input, c(checked, .margin_label = "Total"))
        )),
        canonical(verbs[[verb]](local, c(checked, .margin_label = "Total"))),
        info = verb
      )
      expect_error(
        verbs[[verb]](input, c(checked, .margin_label = local$g[[1L]])),
        class = "marginplyr_error"
      )
    }
  }
}
