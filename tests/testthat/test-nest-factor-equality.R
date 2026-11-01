# Factor subclasses with compatible union methods and distinct equality rules.
nest_equality_factor <- function(x, levels, class = "nest_casefold_factor") {
  structure(as.integer(factor(x, levels = levels, exclude = NULL)),
            levels = levels, class = c(class, "factor"))
}

registerS3method("vec_proxy_equal", "nest_casefold_factor", function(x, ...) {
  tolower(levels(x)[unclass(x)])
}, envir = asNamespace("vctrs"))
registerS3method("vec_ptype2", "nest_casefold_factor.nest_casefold_factor",
                 function(x, y, ...) vctrs::vec_slice(x, integer()),
                 envir = asNamespace("vctrs"))
registerS3method("vec_cast", "nest_casefold_factor.nest_casefold_factor",
                 function(x, to, ...) x, envir = asNamespace("vctrs"))

registerS3method("vec_proxy_equal", "nest_suffix_factor", function(x, ...) {
  as.integer(sub("^[[:alpha:]]+", "", levels(x)[unclass(x)]))
}, envir = asNamespace("vctrs"))
registerS3method("vec_ptype2", "nest_suffix_factor.nest_suffix_factor",
                 function(x, y, ...) vctrs::vec_slice(x, integer()),
                 envir = asNamespace("vctrs"))
registerS3method("vec_cast", "nest_suffix_factor.nest_suffix_factor",
                 function(x, to, ...) x, envir = asNamespace("vctrs"))

test_that("NA-level factor nesting retains source equality groups", {
  input <- tibble::tibble(
    g = nest_equality_factor(c("A", "a"), c("A", "a", NA_character_)),
    row_id = 1:2
  )
  expect_identical(
    lapply(dplyr::nest_by(input, g)$data, function(cell) cell$row_id),
    list(1:2)
  )

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    result <- verb(input, .grouping = grouping_set(g))
    expect_identical(lapply(result$data, function(cell) cell$row_id), list(1:2))
  }
})

test_that("NA-level factor source membership survives nesting options", {
  fixtures <- list(
    casefold = nest_equality_factor(
      c("A", "B", "a", "b"), c("A", "a", "B", "b", NA_character_)
    ),
    suffix = nest_equality_factor(
      c("x01", "y02", "q1", "z2"),
      c("x01", "q1", "y02", "z2", NA_character_), "nest_suffix_factor"
    )
  )
  verbs <- list(nest_with_margins, nest_by_with_margins)
  for (g in fixtures) {
    input <- tibble::tibble(g = g, row_id = 1:4)
    before <- serialize(input, NULL)
    ordinary <- dplyr::union_all(input, input)
    expect_identical(
      vctrs::vec_group_loc(ordinary$g)$loc,
      list(c(1L, 3L, 5L, 7L), c(2L, 4L, 6L, 8L))
    )
    for (empty in c(FALSE, TRUE)) {
      source <- input[if (empty) integer() else 1:4, ]
      source_before <- serialize(source, NULL)
      for (verb in verbs) {
        for (label in list("Total", NULL)) {
          for (keep in c(FALSE, TRUE)) {
            for (sort in c("none", "first", "last")) {
              for (plan in list(grouping_set(g), rollup(g))) {
                result <- verb(
                  source, .grouping = plan, .id = "set", .keep = keep,
                  .sort = sort, .margin_label = label
                )
                info <- paste(class(g)[[1L]], empty, deparse(label), keep, sort)
                detail <- lapply(result$data[result$set == 1L], function(cell) {
                  cell$row_id
                })
                detail <- detail[order(vapply(detail, function(ids) ids[[1L]],
                                              integer(1)))]
                expect_identical(
                  detail, if (empty) list() else list(c(1L, 3L), c(2L, 4L)),
                  info = info
                )
                total <- lapply(result$data[result$set == 2L], function(cell) {
                  cell$row_id
                })
                expect_identical(
                  total,
                  if (empty || identical(plan, grouping_set(g))) {
                    list()
                  } else {
                    list(1:4)
                  },
                  info = info
                )
                for (cell in result$data) {
                  expect_identical(names(cell), c("g"[keep], "row_id"))
                  if (keep) {
                    expect_identical(as.character(cell$g),
                                     as.character(source$g[cell$row_id]))
                  }
                }
              }
            }
          }
        }
      }
      expect_identical(serialize(source, NULL), source_before)
    }
    expect_identical(serialize(input, NULL), before)
  }
})

test_that("fixed and no-NA-level factor controls retain source equality", {
  for (na_level in c(FALSE, TRUE)) {
    input <- tibble::tibble(
      g = nest_equality_factor(
        c("A", "a"), c("A", "a", if (na_level) NA_character_)
      ),
      row_id = 1:2
    )
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      fixed <- verb(input, .by = g)
      detail <- verb(input, .grouping = grouping_set(g))
      for (result in list(fixed, detail)) {
        expect_identical(
          lapply(result$data, function(cell) cell$row_id), list(1:2)
        )
      }
    }
  }
})

test_that("dtplyr nesting retains its factor-code equality groups", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(
    g = nest_equality_factor(c("A", "a"), c("A", "a", NA_character_)),
    row_id = 1:2
  )
  source <- dtplyr::lazy_dt(input)
  ordinary <- dplyr::collect(dplyr::summarize(
    source, ids = list(row_id), .by = g
  ))
  expect_identical(ordinary$ids, list(1L, 2L))

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (label in list("Total", NULL)) {
      result <- verb(source, .grouping = rollup(g), .id = "set",
                     .margin_label = label)
      if (inherits(result, "dtplyr_step")) {
        result <- dplyr::collect(result)
      }
      expect_setequal(
        lapply(result$data[result$set == 1L], function(cell) cell$row_id),
        list(1L, 2L)
      )
      expect_identical(result$data[[which(result$set == 2L)]]$row_id, 1:2)
    }
  }
})

test_that("factor nesting separates NA levels, missing codes and margins", {
  for (ordered in c(FALSE, TRUE)) {
    g <- structure(c(1L, 1L, 2L, NA_integer_),
                   levels = c("a", NA_character_),
                   class = if (ordered) c("ordered", "factor") else "factor")
    input <- tibble::tibble(g = g, row_id = 1:4)
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      for (label in list("Total", NULL)) {
        for (keep in c(FALSE, TRUE)) {
          result <- verb(input, .grouping = rollup(g), .id = "set",
                         .margin_label = label, .keep = keep)
          expect_setequal(
            lapply(result$data[result$set == 1L], function(cell) cell$row_id),
            list(1:2, 3L, 4L)
          )
          expect_identical(result$data[[which(result$set == 2L)]]$row_id, 1:4)
          na_level <- which(vapply(result$data, function(cell) {
            identical(cell$row_id, 3L)
          }, logical(1)))
          missing <- which(vapply(result$data, function(cell) {
            identical(cell$row_id, 4L)
          }, logical(1)))
          expect_false(is.na(result$g[na_level]))
          expect_true(is.na(result$g[missing]))
          if (keep) {
            for (cell in result$data) {
              expect_identical(cell$g, input$g[cell$row_id])
            }
          }
        }
      }
    }
  }
})
