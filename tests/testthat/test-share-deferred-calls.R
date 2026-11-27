# `do.call()` evaluates a language object its literal argument list holds in
# the data mask, so `do.call(sum, list(quote(prior)))` reads `prior` as
# `sum(prior)` does (#1807).
deferred_quoted_reads <- function() {
  as.list(quote(alist(
    do.call(sum, list(quote(prior))),
    do.call("sum", list(base::quote(prior))),
    do.call(sum, list(as.name("prior"))),
    do.call(sum, list(as.symbol("prior"))),
    do.call(sum, list(str2lang("prior"))),
    do.call(identity, list(str2expression("sum(prior)"))),
    do.call(identity, list(parse(text = "sum(prior)"))),
    do.call(sum, list(rlang::expr(prior))),
    do.call(sum, list(substitute(prior))),
    do.call(sum, list(expression(prior))),
    do.call(identity, list(quote(sum(prior)))),
    do.call(max, list(quote(x), quote(prior))),
    do.call(sum, list(x = quote(prior)), quote = FALSE),
    match.fun("do.call")(sum, list(quote(prior)))
  )))[-1L]
}

test_that("a do.call() argument list's quoted language is read", {
  hidden <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  plain <- data.frame(g = c("a", "b"), x = c(1, 3))
  expect_share_dependencies(hidden, deferred_quoted_reads())
  expect_share_dependencies(plain, deferred_quoted_reads())
})

test_that("a do.call() argument list's quoted language is read on data.table", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(g = c("a", "b"), x = c(1, 3),
                                 prior = c(100, 300))
  expect_share_dependencies(data, deferred_quoted_reads())
})

test_that("a do.call() reading no earlier summary keeps its value", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  result <- summarize_with_margins(
    data,
    value = sum(x),
    share = share_of_total(value),
    listed = do.call("sum", list(x)) * 2,
    quoted = do.call(sum, list(quote(x))),
    coerced = do.call(sum, as.list(x)),
    floored = do.call(pmax, list(sum(x), 1)),
    combined = do.call(paste0, c(list("a"), "b")),
    applied = do.call(max, lapply(1:2, function(i) sum(x) * i)),
    .grouping = rollup(g)
  )
  expect_equal(result$listed, c(2, 6, 8))
  expect_equal(result$quoted, c(1, 3, 4))
  expect_equal(result$coerced, c(1, 3, 4))
  expect_equal(result$floored, c(1, 3, 4))
  expect_equal(result$combined, rep("ab", 3L))
  expect_equal(result$applied, c(2, 6, 8))
  source <- summarize_with_margins(
    data.frame(g = c("a", "b"), x = c(1, 3), y = c(10, 30)),
    prior = sum(y),
    value = sum(x) + do.call(sum, list(y)) + do.call(sum, list(quote(y))),
    total = share_of_total(value),
    .grouping = rollup(g)
  )
  expect_equal(source$value, c(21, 63, 84))
})

test_that("a do.call() that quotes its arguments evaluates no language", {
  # `quote = TRUE` hands each element to the function as the language object
  # itself, so nothing in the list is evaluated in the mask.
  expect_identical(
    expression_data_symbols(
      quote(do.call(identity, list(quote(prior)), quote = TRUE))
    ),
    "identity"
  )
})

# rlang's `exec()` builds and evaluates the call `do.call()` builds, with its
# arguments written in place, and `invoke()` is its deprecated twin taking a
# list; each is read as its `do.call()` twin is (#1820). purrr's `exec()` is
# rlang's re-exported. The bare spellings bind rlang's own functions here, as
# attaching rlang would.
rlang_builder_reads <- function() {
  exec <- rlang::exec
  invoke <- rlang::invoke
  c(
    alist(
      rlang::exec("get", "prior"),
      rlang::exec("get0", "prior"),
      rlang::exec("eval", quote(prior)),
      rlang::exec(get, "prior"),
      rlang::exec(sum, quote(prior)),
      rlang::invoke("get", list("prior")),
      rlang::invoke(get, list("prior")),
      rlang::invoke(sum, list(quote(prior))),
      rlang::invoke(sum, list(1), quote(prior))
    ),
    rlang::quos(invoke("get", list("prior")), exec(get, "prior")),
    # Parsed so that the test names no package it does not declare: purrr is
    # not a dependency, and the walk never runs the call.
    list(str2lang("purrr::exec('get', 'prior')"))
  )
}

test_that("rlang's call builders are read as do.call() is", {
  hidden <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  plain <- data.frame(g = c("a", "b"), x = c(1, 3))
  expect_share_dependencies(hidden, rlang_builder_reads())
  expect_share_dependencies(plain, rlang_builder_reads())
})

test_that("rlang's call builders are read as do.call() is on data.table", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(g = c("a", "b"), x = c(1, 3),
                                 prior = c(100, 300))
  expect_share_dependencies(data, rlang_builder_reads())
})

test_that("a call builder removing a local exposes the alias behind it", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  removals <- alist(
    rlang::exec("rm", "prior"),
    rlang::exec(rm, "prior"),
    rlang::invoke("rm", list("prior"))
  )
  for (removal in removals) {
    expression <- rlang::expr({
      prior <- 2
      identity(!!removal)
      sum(prior)
    })
    expect_error(
      summarize_with_margins(
        data, prior = sum(x), value = !!expression,
        fraction = share_of_total(value), .grouping = rollup(group)
      ),
      "depends on earlier summary alias `prior`",
      class = "marginplyr_error"
    )
  }
})

test_that("a call builder reading no earlier summary keeps its value", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  # A caller's own function of the same name is not rlang's.
  exec <- function(.fn, ...) 7
  invoke <- function(.fn, .args = list(), ...) 7
  result <- rlang::inject(summarize_with_margins(
    data,
    value = sum(x),
    share = share_of_total(value),
    named = rlang::exec("sum", x),
    passed = rlang::exec(sum, x),
    listed = suppressWarnings(rlang::invoke("sum", list(x))),
    own_exec = exec("get", "share"),
    own_invoke = invoke("get", list("share")),
    .grouping = rollup(g)
  ))
  expect_equal(result$named, c(1, 3, 4))
  expect_equal(result$passed, c(1, 3, 4))
  expect_equal(result$listed, c(1, 3, 4))
  expect_equal(result$own_exec, rep(7, 3L))
  expect_equal(result$own_invoke, rep(7, 3L))
})
