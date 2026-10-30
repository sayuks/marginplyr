test_that("nesting keeps equal package versions in one detail cell", {
  input <- tibble::tibble(
    g = package_version(c("1.0", "1.0.0")),
    row_id = 1:2
  )

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    result <- verb(input, .grouping = grouping_set(g), .keep = TRUE)

    expect_identical(nrow(result), 1L)
    expect_identical(lapply(result$data, function(cell) cell$row_id), list(1:2))
    expect_identical(result$data[[1L]]$g, input$g)
  }
})

test_that("equal source keys retain their rows across nesting options", {
  input <- tibble::tibble(
    g = package_version(c("1.0", "1.0.0")),
    row_id = 1:2
  )
  verbs <- list(
    nest_with_margins = nest_with_margins,
    nest_by_with_margins = nest_by_with_margins
  )
  options <- expand.grid(
    tibble = c(FALSE, TRUE),
    verb = names(verbs),
    keep = c(FALSE, TRUE),
    sort = c("none", "last", "first"),
    reverse = c(FALSE, TRUE),
    payload = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(options))) {
    option <- options[i, ]
    source <- input[if (option$reverse) 2:1 else 1:2, ]
    if (!option$payload) {
      source$row_id <- NULL
    }
    if (!option$tibble) {
      source <- as.data.frame(source)
    }
    for (sets in 1:2) {
      plan <- list(grouping_set(g), rollup(g))[[sets]]
      info <- paste(i, paste(unlist(option), collapse = "/"), sets)
      result <- verbs[[option$verb]](
        source, .grouping = plan, .id = "set",
        .keep = option$keep, .sort = option$sort
      )
      expected_sets <- seq_len(sets)
      expect_identical(sort(result$set), expected_sets, info = info)
      expect_identical(names(result), c("g", "set", "data"), info = info)
      expect_identical(
        vapply(result$data, nrow, integer(1)),
        rep(2L, length(expected_sets)), info = info
      )
      expect_true(result$g[result$set == 1L] %in% c("1.0", "1.0.0"))
      expect_identical(
        result$g[result$set == 2L],
        if (length(expected_sets) == 2L) "Total" else character()
      )
      if (option$sort != "none") {
        expect_identical(
          result$set,
          if (option$sort == "first") rev(expected_sets) else expected_sets,
          info = info
        )
      }
      if (option$verb == "nest_by_with_margins") {
        expect_s3_class(result, "rowwise_df")
        expect_identical(dplyr::group_vars(result), c("g", "set"), info = info)
      } else {
        expect_identical(dplyr::group_vars(result), character(), info = info)
        if (option$tibble) {
          expect_s3_class(result, "tbl_df")
        } else {
          expect_identical(class(result), "data.frame", info = info)
        }
      }
      for (cell in result$data) {
        expect_identical(
          names(cell),
          c("g"[option$keep], "row_id"[option$payload]),
          info = info
        )
        if (option$payload) {
          expect_identical(cell$row_id, source$row_id, info = info)
        }
        if (option$keep) {
          expect_identical(cell$g, source$g, info = info)
        }
      }
    }
  }
})

test_that("nesting respects case-insensitive vctrs equality", {
  registerS3method("vec_proxy_equal", "margin_nest_casefold", function(x, ...) {
    tolower(vctrs::vec_data(x))
  }, envir = asNamespace("vctrs"))
  registerS3method("as.character", "margin_nest_casefold", function(x, ...) {
    vctrs::vec_data(x)
  }, envir = asNamespace("base"))
  input <- tibble::tibble(
    g = vctrs::new_vctr(c("A", "a", "B", "b"), class = "margin_nest_casefold"),
    row_id = 1:4
  )

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (keep in c(FALSE, TRUE)) {
      result <- verb(input, .grouping = rollup(g), .id = "set", .keep = keep)

      expect_identical(sort(result$set), c(1L, 1L, 2L))
      expect_setequal(
        lapply(result$data[result$set == 1L], function(cell) cell$row_id),
        list(1:2, 3:4)
      )
      expect_identical(result$data[[which(result$set == 2L)]]$row_id, 1:4)
      if (keep) {
        for (cell in result$data) {
          expect_identical(cell$g, input$g[cell$row_id])
        }
      }
    }
  }
})

test_that("typed-missing labels and fixed keys retain source equality", {
  input <- tibble::tibble(
    g = package_version(c("1.0", "1.0.0")),
    row_id = 1:2
  )

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (keep in c(FALSE, TRUE)) {
      results <- list(
        verb(input, .grouping = rollup(g), .margin_label = NULL, .keep = keep),
        verb(input, .grouping = rollup(g),
             .margin_label = NA_character_, .keep = keep),
        verb(input, .by = g, .grouping = grouping_set(), .keep = keep)
      )
      expect_identical(vapply(results, nrow, integer(1)), c(2L, 2L, 1L))
      for (result in results) {
        for (cell in result$data) {
          expect_identical(cell$row_id, 1:2)
          if (keep) {
            expect_identical(cell$g, input$g)
          }
        }
      }
    }
  }
})

test_that("typed-missing list keys retain their values and row membership", {
  input <- tibble::tibble(
    g = list(NULL, NULL, 1:2, 1:2, NA_real_, integer()),
    row_id = 1:6
  )

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (keep in c(FALSE, TRUE)) {
      result <- verb(
        input, .grouping = rollup(g), .id = "set",
        .margin_label = NULL, .keep = keep
      )
      detail <- which(result$set == 1L)
      total <- which(result$set == 2L)
      cells <- lapply(result$data[detail], function(cell) cell$row_id)
      cells <- cells[order(vapply(cells, function(ids) ids[[1L]], integer(1)))]
      expect_identical(cells, list(1:2, 3:4, 5L, 6L))
      expect_identical(result$data[[total]]$row_id, 1:6)
      for (i in detail) {
        cell <- result$data[[i]]
        expect_identical(result$g[i], input$g[cell$row_id[[1L]]])
      }
      if (keep) {
        for (cell in result$data) {
          expect_identical(cell$g, input$g[cell$row_id])
        }
      }
    }
  }
})
