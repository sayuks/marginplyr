test_that("included-only expansion preserves Arrow dictionary dimensions", {
  skip_if_suggest_absent("arrow")
  for (values in arrow_dictionary_values()) {
    for (empty in c(FALSE, TRUE)) {
      expected <- if (empty) values[integer()] else values
      for (source in arrow_dictionary_sources(values, empty)) {
        type <- arrow::schema(source)$GetFieldByName("g")$type
        for (label in list(NULL, NA_character_)) {
          # Arrow exposes non-string dictionaries as R factors with a warning.
          query <- suppressWarnings(expand_with_margins(
            source, .grouping = grouping_set(g), .margin_label = label,
            .id = "occurrence"
          ))
          expect_s3_class(query, "arrow_dplyr_query")
          expect_true(
            arrow::schema(query)$GetFieldByName("g")$type$Equals(type)
          )
          materialized <- dplyr::compute(query)
          expect_true(materialized$schema$GetFieldByName("g")$type$Equals(type))
          decoded <- materialized$g$cast(type$value_type)$as_vector()
          actual <- suppressWarnings(dplyr::collect(materialized))
          rows <- order(actual$row_id)
          direct <- suppressWarnings(dplyr::collect(query))
          expect_equal(direct[order(direct$row_id), ], actual[rows, ])
          expect_identical(decoded[rows], expected)
          expect_identical(actual$row_id[rows], seq_along(expected))
          expect_identical(actual$occurrence, rep(1L, length(expected)))
        }
      }
    }
  }
})

test_that("Arrow rollups retain typed missing dictionary margins", {
  skip_if_suggest_absent("arrow")
  for (values in arrow_dictionary_values()) {
    for (empty in c(FALSE, TRUE)) {
      included <- if (empty) values[integer()] else values
      for (source in arrow_dictionary_sources(values, empty)) {
        type <- arrow::schema(source)$GetFieldByName("g")$type
        for (label in list(NULL, NA_character_)) {
          query <- suppressWarnings(expand_with_margins(
            source, .grouping = rollup(g), .margin_label = label,
            .id = "occurrence"
          ))
          expect_s3_class(query, "arrow_dplyr_query")
          expect_true(
            arrow::schema(query)$GetFieldByName("g")$type$Equals(type)
          )
          materialized <- dplyr::compute(query)
          expect_true(materialized$schema$GetFieldByName("g")$type$Equals(type))
          actual <- suppressWarnings(dplyr::collect(materialized))
          rows <- order(actual$occurrence, actual$row_id)
          direct <- suppressWarnings(dplyr::collect(query))
          expect_equal(
            direct[order(direct$occurrence, direct$row_id), ], actual[rows, ]
          )
          decoded <- materialized$g$cast(type$value_type)$as_vector()
          expect_identical(
            decoded[rows],
            c(included, values[rep(NA_integer_, length(included))])
          )
          expect_identical(actual$row_id[rows], rep(seq_along(included), 2L))
          expect_identical(actual$occurrence[rows],
                           rep(1:2, each = length(included)))

          summary <- suppressWarnings(summarize_with_margins(
            source, n = dplyr::n(), .grouping = rollup(g),
            .margin_label = label, .id = "occurrence"
          ))
          expect_s3_class(summary, "arrow_dplyr_query")
          expect_true(
            arrow::schema(summary)$GetFieldByName("g")$type$Equals(type)
          )
          computed <- dplyr::compute(summary)
          expect_true(computed$schema$GetFieldByName("g")$type$Equals(type))
          counts <- suppressWarnings(dplyr::collect(computed))
          keys <- computed$g$cast(type$value_type)$as_vector()
          detail <- counts$occurrence == 1L
          for (key in unique(included)) {
            matches <- if (is.na(key)) {
              is.na(keys)
            } else {
              !is.na(keys) & keys == key
            }
            expected_n <- if (is.na(key)) {
              1L
            } else {
              sum(included == key, na.rm = TRUE)
            }
            expect_equal(counts$n[detail & matches], expected_n)
          }
          expect_identical(sum(detail), if (empty) 0L else 3L)
          expect_equal(counts$n[!detail], length(included))
          expect_identical(keys[!detail], values[NA_integer_])
        }
      }
    }
  }
})

test_that("Arrow dictionary display labels produce character dimensions", {
  skip_if_suggest_absent("arrow")
  for (values in arrow_dictionary_values()) {
    for (empty in c(FALSE, TRUE)) {
      for (source in arrow_dictionary_sources(values, empty)) {
        # Ordinary Arrow conversion owns how each physical value is displayed.
        control <- suppressWarnings(dplyr::collect(dplyr::mutate(
          source, g = as.character(.data$g)
        )))
        control <- control[order(control$row_id), ]
        for (summary in c(FALSE, TRUE)) {
          query <- suppressWarnings(if (summary) {
            summarize_with_margins(
              source, n = dplyr::n(), .grouping = rollup(g),
              .margin_label = "Total", .id = "occurrence"
            )
          } else {
            expand_with_margins(
              source, .grouping = rollup(g),
              .margin_label = "Total", .id = "occurrence"
            )
          })
          expect_s3_class(query, "arrow_dplyr_query")
          expect_true(arrow::schema(query)$GetFieldByName("g")$type$Equals(
            arrow::utf8()
          ))
          actual <- dplyr::collect(query)
          expect_type(actual$g, "character")
          included <- actual$occurrence == 1L
          if (summary) {
            expect_setequal(actual$g[included], unique(control$g))
            expect_identical(actual$g[!included], "Total")
            expect_equal(actual$n[!included], nrow(control))
          } else {
            rows <- order(actual$row_id[included])
            expect_identical(actual$g[included][rows], control$g)
            expect_identical(actual$g[!included], rep("Total", nrow(control)))
            expect_identical(sort(actual$row_id[!included]), control$row_id)
          }
        }
      }
    }
  }
})

test_that("dictionary inspection, fixed keys and payload remain valid", {
  skip_if_suggest_absent("arrow")
  for (values in arrow_dictionary_values()) {
    for (empty in c(FALSE, TRUE)) {
      expected <- if (empty) values[integer()] else values
      for (source in arrow_dictionary_sources(values, empty)) {
        inspection <- suppressWarnings(inspect_grouping(
          source, .grouping = rollup(g)
        ))
        expect_identical(inspection$included, c("(g)", "()"))
        expect_identical(suppressWarnings(inspect_grouping(
          source, .grouping = rollup(where(is.factor))
        )), inspection)
        type <- arrow::schema(source)$GetFieldByName("g")$type
        for (fixed in c(FALSE, TRUE)) {
          query <- suppressWarnings(expand_with_margins(
            source, .by = dplyr::all_of(if (fixed) "g" else character()),
            .grouping = rollup(row_id), .margin_label = NULL,
            .id = "occurrence"
          ))
          expect_s3_class(query, "arrow_dplyr_query")
          expect_true(
            arrow::schema(query)$GetFieldByName("g")$type$Equals(type)
          )
          materialized <- dplyr::compute(query)
          actual <- suppressWarnings(dplyr::collect(materialized))
          decoded <- materialized$g$cast(type$value_type)$as_vector()
          for (occurrence in 1:2) {
            expect_identical(
              sort(decoded[actual$occurrence == occurrence], na.last = TRUE),
              sort(expected, na.last = TRUE)
            )
          }
          expect_identical(actual$row_id[actual$occurrence == 1L],
                           seq_along(expected))
          expect_identical(actual$row_id[actual$occurrence == 2L],
                           rep(NA_integer_, length(expected)))
        }
      }
    }
  }
})
