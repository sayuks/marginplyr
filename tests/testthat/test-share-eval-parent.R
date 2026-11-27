# `eval.parent(expr, n)` is `eval(expr, parent.frame(n + 1))` from inside
# itself, so from its caller it evaluates in `parent.frame(n)`, and with
# `n = 0` in the caller's own frame (#1808).
eval_parent_reads <- function() {
  as.list(quote(alist(
    {
      f <- function() sum(eval.parent(quote(prior)))
      f()
    },
    {
      f <- function() {
        prior <- 1
        sum(eval.parent(quote(prior)))
      }
      f()
    },
    {
      f <- function() {
        g <- function() sum(eval.parent(quote(prior), 2))
        g()
      }
      f()
    },
    sum(eval.parent(quote(prior), 0)),
    sum(eval.parent(quote(prior), n = 0)),
    sum(base::eval.parent(quote(prior), 0)),
    sum(eval.parent(parse(text = "prior"), 0)),
    sum(do.call("eval.parent", list(quote(prior), 0))),
    sum(match.fun("eval.parent")(quote(prior), 0))
  )))[-1L]
}

test_that("eval.parent() is placed as the eval() it performs", {
  hidden <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  plain <- data.frame(g = c("a", "b"), x = c(1, 3))
  expect_share_dependencies(hidden, eval_parent_reads())
  expect_share_dependencies(plain, eval_parent_reads())
})

test_that("eval.parent() is placed as the eval() it performs on data.table", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(g = c("a", "b"), x = c(1, 3),
                                 prior = c(100, 300))
  expect_share_dependencies(data, eval_parent_reads())
})

test_that("an eval.parent() removal exposes the alias behind it", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  removals <- alist(
    {
      prior <- 2
      f <- function() eval.parent(quote(rm("prior")))
      f()
      sum(prior)
    },
    {
      prior <- 2
      eval.parent(quote(rm("prior")), 0)
      sum(prior)
    }
  )
  for (removal in removals) {
    expect_error(
      summarize_with_margins(
        data, prior = sum(x), value = !!removal,
        fraction = share_of_total(value), .grouping = rollup(group)
      ),
      "depends on earlier summary alias `prior`",
      class = "marginplyr_error"
    )
  }
})

test_that("an eval.parent() reading no earlier summary keeps its value", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  result <- summarize_with_margins(
    data,
    prior = sum(x),
    value = eval.parent(quote(sum(x) * 100), 0),
    column = sum(eval.parent(quote(x), 0)),
    local = {
      prior <- 5
      eval.parent(quote(prior), 0)
    },
    share = share_of_total(value),
    .grouping = rollup(g)
  )
  expect_equal(result$value, c(100, 300, 400))
  expect_equal(result$column, c(1, 3, 4))
  expect_equal(result$local, rep(5, 3L))
})
