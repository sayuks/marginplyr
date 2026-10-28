test_that("an empty local pick keeps dplyr's refusal across a rollup", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2)
  baseline <- expect_error(
    dplyr::summarise(dplyr::group_by(data, g), z = nrow(dplyr::pick())),
    "Must supply at least one input"
  )
  error <- expect_error(
    summarize_with_margins(data, z = nrow(dplyr::pick()),
                           .grouping = rollup(g)),
    "Must supply at least one input"
  )
  expect_identical(class(error), class(baseline))
  expect_false(inherits(error, "marginplyr_error"))
  expect_identical(conditionMessage(error$parent),
                   conditionMessage(baseline$parent))
})

# jarl-ignore unnecessary_parentheses: Redundant parentheses are under test.
test_that("empty local pick spellings refuse named, unnamed and nested uses", {
  pick <- dplyr::pick
  spellings <- rlang::exprs(
    pick(), dplyr::pick(), (pick)(), (dplyr::pick)(),
    ((pick())), ((dplyr::pick)())
  )
  for (data in list(tibble::tibble(x = 1:2), tibble::tibble(x = integer()))) {
    for (spelling in spellings) {
      expressions <- list(
        spelling,
        rlang::expr(nrow(!!spelling)),
        rlang::expr(identity(nrow(!!spelling)))
      )
      for (expr in expressions) {
        for (name in c("", "z")) {
          dots <- stats::setNames(list(rlang::new_quosure(expr)), name)
          baseline <- expect_error(
            dplyr::summarise(data, !!!dots),
            "Must supply at least one input"
          )
          error <- expect_error(
            summarize_with_margins(data, !!!dots),
            "Must supply at least one input"
          )
          expect_identical(class(error), class(baseline))
          expect_false(inherits(error, "marginplyr_error"))
          expect_identical(conditionMessage(error$parent),
                           conditionMessage(baseline$parent))
        }
      }
    }
  }
})

test_that("an unrelated valid Total share does not hide an empty pick", {
  for (data in list(tibble::tibble(x = 1:2), tibble::tibble(x = integer()))) {
    for (expr in rlang::exprs(dplyr::pick(), nrow((dplyr::pick)()))) {
      for (name in c("", "z")) {
        dots <- stats::setNames(list(rlang::new_quosure(expr)), name)
        error <- expect_error(
          summarize_with_margins(
            data, !!!dots, total = sum(x), share = share_of_total(total)
          ),
          "Must supply at least one input"
        )
        expect_false(inherits(error, "marginplyr_error"))
      }
    }
  }
})

test_that("explicit pick selections retain their sequential summary meanings", {
  for (data in list(tibble::tibble(x = 1:2), tibble::tibble(x = integer()))) {
    result <- summarize_with_margins(
      data,
      total = sum(x),
      columns = ncol(dplyr::pick(dplyr::everything())),
      rows = nrow(dplyr::pick(c())),
      selected = ncol(dplyr::pick(total, columns))
    )
    expect_identical(result, dplyr::summarise(
      data,
      total = sum(x),
      columns = ncol(dplyr::pick(dplyr::everything())),
      rows = nrow(dplyr::pick(c())),
      selected = ncol(dplyr::pick(total, columns))
    ))
    expect_identical(result$columns, 2L)
    expect_identical(result$rows, 1L)
    expect_identical(result$selected, 2L)
  }
})

test_that("dtplyr answers an empty pick with its own ordinary result", {
  skip_if_suggest_absent("dtplyr")
  source <- dtplyr::lazy_dt(tibble::tibble(x = 1:2))
  result <- summarize_with_margins(source, z = nrow(dplyr::pick()))
  expect_s3_class(result, "dtplyr_step")
  expect_identical(
    dplyr::collect(result),
    dplyr::collect(dplyr::summarise(source, z = nrow(dplyr::pick())))
  )
})

test_that("dbplyr retains its empty pick refusal without executing a query", {
  source <- dbplyr::tbl_lazy(tibble::tibble(x = 1:2),
                             con = dbplyr::simulate_dbi())
  baseline <- expect_error(
    dplyr::summarise(source, z = nrow(dplyr::pick())),
    "Must supply at least one input"
  )
  error <- expect_error(
    summarize_with_margins(source, z = nrow(dplyr::pick())),
    "Must supply at least one input"
  )
  expect_identical(class(error), class(baseline))
  expect_identical(conditionMessage(error), conditionMessage(baseline))
})

test_that("an Arrow dataset retains its own empty pick refusal", {
  skip_if_suggest_absent("arrow")
  source <- arrow::InMemoryDataset$create(tibble::tibble(x = 1:2))
  baseline <- expect_error(
    dplyr::summarise(source, z = nrow(dplyr::pick())),
    class = "arrow_not_supported"
  )
  error <- expect_error(
    summarize_with_margins(source, z = nrow(dplyr::pick())),
    class = "arrow_not_supported"
  )
  expect_identical(class(error), class(baseline))
  expect_identical(conditionMessage(error), conditionMessage(baseline))
})

test_that("an Arrow table still refuses absorption for an empty pick", {
  skip_if_suggest_absent("arrow")
  source <- arrow::arrow_table(tibble::tibble(x = 1:2))
  expect_error(
    summarize_with_margins(source, z = nrow(dplyr::pick())),
    "Arrow would read the whole input",
    class = "marginplyr_error"
  )
})
