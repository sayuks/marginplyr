# A function built in place from quoted language is the literal it builds:
# `as.function(list(quote(x)))` is `function() x` and
# `rlang::new_function(NULL, quote(x))` is the same, each enclosed by the
# frame that builds it, so its body reads that frame's names and the mask
# behind it (#1831).
constructed_function_reads <- function() {
  as.list(quote(alist(
    sum(as.function(list(quote(prior)))()),
    sum(as.function(alist(prior))()),
    sum(as.function(list(x = 1, quote(prior)))()),
    # A default evaluated where the list is built leaves the body readable.
    sum(as.function(list(x = -1, quote(prior)))()),
    sum(rlang::new_function(list(x = c(1, 2)), quote(prior))()),
    sum(as.function(list(as.name("prior")))()),
    sum(as.function(list(quote(prior)), envir = environment())()),
    sum(rlang::new_function(NULL, quote(prior))()),
    sum(rlang::new_function(NULL, rlang::expr(prior))()),
    sum(rlang::new_function(rlang::pairlist2(), quote(prior),
                            rlang::current_env())()),
    sum(rlang::new_function(list(x = quote(prior)), quote(x))()),
    sum(rlang::new_function(alist(x = ), quote(prior))(1)),
    sum(rlang::new_function(rlang::pairlist2(x = , y = 2), quote(prior))(1)),
    sum(rlang::new_function(rlang::exprs(x = 1), str2lang("prior"))()),
    {
      f <- rlang::new_function(NULL, quote(sum(prior)))
      f()
    },
    # Never called: a literal reads its free names where it is created.
    {
      f <- as.function(list(quote(prior)))
      1
    },
    sum(rlang::as_function(rlang::new_formula(NULL, quote(prior)))())
  )))[-1L]
}

test_that("a function built from quoted language reads its body", {
  hidden <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  plain <- data.frame(g = c("a", "b"), x = c(1, 3))
  expect_share_dependencies(hidden, constructed_function_reads())
  expect_share_dependencies(plain, constructed_function_reads())
})

test_that("a function built from quoted language reads it on data.table", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(g = c("a", "b"), x = c(1, 3),
                                 prior = c(100, 300))
  expect_share_dependencies(data, constructed_function_reads())
})

test_that("a function built reading no earlier summary keeps its value", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  result <- summarize_with_margins(
    data,
    value = sum(x),
    share = share_of_total(value),
    column = sum(rlang::new_function(NULL, quote(x))()),
    summed = rlang::new_function(NULL, quote(sum(x)))(),
    formal = (function(share) rlang::new_function(NULL, quote(share))())(2),
    default = rlang::new_function(list(share = 2), quote(share))(),
    negative = as.function(list(share = -2, quote(share)))(),
    local = {
      share <- 2
      as.function(list(quote(share)))()
    },
    base = rlang::new_function(NULL, quote(1), env = baseenv())(),
    literal = sum(as.function(function() x)()),
    .grouping = rollup(g)
  )
  expect_equal(result$column, c(1, 3, 4))
  expect_equal(result$summed, c(1, 3, 4))
  expect_equal(result$formal, rep(2, 3L))
  expect_equal(result$default, rep(2, 3L))
  expect_equal(result$negative, rep(-2, 3L))
  expect_equal(result$local, rep(2, 3L))
  expect_equal(result$base, rep(1, 3L))
  expect_equal(result$literal, c(1, 3, 4))
})
