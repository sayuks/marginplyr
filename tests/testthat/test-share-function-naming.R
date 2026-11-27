# `match.fun()` handed a function names it as the string does: `match.fun(get)`
# evaluates to the primitive `match.fun("get")` does, so a head built from it
# performs the lookup `get()` performs (#1830). "a primitive named through
# another primitive is still itself" in test-static-expression-analysis.R pins
# the string spellings.
match_fun_reads <- function() {
  alist(
    sum(match.fun(get)("prior")),
    sum(match.fun(base::get)("prior")),
    sum(match.fun((get))("prior")),
    sum(match.fun(FUN = get0)("prior")),
    sum(match.fun(eval)(quote(prior))),
    sum(match.fun(rlang::eval_tidy)(quote(prior))),
    sum(do.call(match.fun(get), list("prior")))
  )
}

test_that("match.fun() handed a primitive names that primitive", {
  hidden <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  plain <- data.frame(g = c("a", "b"), x = c(1, 3))
  expect_share_dependencies(hidden, match_fun_reads())
  expect_share_dependencies(plain, match_fun_reads())
  expect_identical(
    expression_data_symbols(quote(match.fun(get)("share"))),
    c("get", "share")
  )
})

test_that("match.fun() handed a primitive names it on data.table", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(g = c("a", "b"), x = c(1, 3),
                                 prior = c(100, 300))
  expect_share_dependencies(data, match_fun_reads())
})

test_that("match.fun() handed a primitive names it on dtplyr", {
  skip_if_suggest_absent("dtplyr")
  data <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), x = c(1, 3)))
  expect_error(
    summarize_with_margins(
      data, source = sum(x), prior = share_of_total(source),
      observed = sum(match.fun(get)("prior")), .grouping = rollup(g)
    ),
    "Ordinary summaries cannot use an earlier", class = "marginplyr_error"
  )
})

test_that("match.fun() naming no reflective primitive keeps its value", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  name <- "sum"
  result <- summarize_with_margins(
    data,
    value = sum(x),
    share = share_of_total(value),
    direct = match.fun(sum)(x),
    named = match.fun(name)(x),
    applied = sum(sapply(x, match.fun(identity))),
    formal = (function(share) match.fun(get)("share"))(2),
    local = {
      share <- 5
      match.fun(get)("share")
    },
    .grouping = rollup(g)
  )
  expect_equal(result$direct, c(1, 3, 4))
  expect_equal(result$named, c(1, 3, 4))
  expect_equal(result$applied, c(1, 3, 4))
  expect_equal(result$formal, rep(2, 3L))
  expect_equal(result$local, rep(5, 3L))
})
