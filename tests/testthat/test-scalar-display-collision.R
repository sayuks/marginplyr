# A scalar array whose display retains its one-dimensional shape.
scalar_display_input <- function() {
  registerS3method("as.character", "margin_display_array", function(x, ...) {
    array(as.character(unclass(x)), dim = dim(x))
  }, envir = asNamespace("base"))
  tibble::tibble(
    g = structure(c(1L, 1L, 2L), dim = 3L,
                  class = c("margin_display_array", "array")),
    row = 1:3
  )
}

test_that("scalar array displays distinguish Margin label collisions", {
  input <- scalar_display_input()
  before <- serialize(input, NULL)
  native <- dplyr::nest_by(input, g)
  expect_identical(lapply(native$data, function(x) x$row), list(1:2, 3L))
  for (verb in list(nest_with_margins, nest_by_with_margins,
                    expand_with_margins, summarize_with_margins)) {
    args <- if (identical(verb, summarize_with_margins)) {
      list(n = rlang::quo(dplyr::n()))
    } else {
      list()
    }
    for (plan in list(grouping_set(g), rollup(g))) {
      result <- rlang::inject(verb(input, !!!args, .grouping = plan))
      expect_true(all(as.vector(result$g) %in% c("1", "2", "Total")))
      expect_error(
        rlang::inject(verb(input, !!!args, .grouping = plan,
                           .margin_label = "1")),
        "already present in grouping column", class = "marginplyr_error"
      )
    }
  }
  expect_identical(serialize(input, NULL), before)
})

# Source grouping supplies membership; literal displays and copies pin labels.
expect_scalar_display_nesting <- function(as_source) {
  input <- as_source(scalar_display_input())
  before <- serialize(input, NULL)
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (keep in c(FALSE, TRUE)) {
      for (total in c(FALSE, TRUE)) {
        plan <- if (total) rollup("g") else grouping_set("g")
        result <- verb(input, .grouping = plan, .keep = keep)
        expect_identical(as.vector(result$g), c("1", "2", "Total"[total]))
        expect_identical(lapply(result$data, function(x) x$row),
                         c(list(1:2, 3L), if (total) list(1:3)))
        for (cell in result$data) {
          expect_identical(names(cell), c("g"[keep], "row"))
          if (keep) {
            expect_identical(cell$g, vctrs::vec_slice(input$g, cell$row))
          }
        }
        unchecked <- verb(input, .grouping = plan, .keep = keep,
                          .check_margin_label = FALSE)
        expect_identical(result, unchecked)
      }
      for (label in list(NULL, NA_character_)) {
        result <- verb(input, .grouping = rollup("g"), .keep = keep,
                       .margin_label = label)
        expect_identical(lapply(result$data, function(x) x$row),
                         list(1:2, 3L, 1:3))
        expect_identical(result$g, vctrs::vec_c(
          vctrs::vec_slice(input$g, c(1L, 3L)), vctrs::vec_init(input$g, 1L)
        ))
      }
      fixed <- verb(input, .by = "g", .grouping = grouping_set(), .keep = keep)
      expect_identical(lapply(fixed$data, function(x) x$row), list(1:2, 3L))
      expect_identical(fixed$g, vctrs::vec_slice(input$g, c(1L, 3L)))
    }
  }
  expect_identical(serialize(input, NULL), before)
}

test_that("scalar array displays retain nesting memberships and source keys", {
  expect_scalar_display_nesting(identity)
  expect_scalar_display_nesting(as.data.frame)
})

test_that("data.table scalar displays retain nesting memberships", {
  skip_if_suggest_absent("data.table")
  expect_scalar_display_nesting(data.table::as.data.table)
})

test_that("non-missing labels still refuse nonscalar array dimensions", {
  for (key in list(matrix(1:4, nrow = 2L), array(1:8, c(2L, 2L, 2L)))) {
    input <- tibble::tibble(g = key, row = 1:2)
    for (verb in list(nest_with_margins, nest_by_with_margins,
                      expand_with_margins, summarize_with_margins)) {
      expect_error(verb(input, .grouping = rollup(g)),
                   "matrix or array", class = "marginplyr_error")
    }
  }
})
