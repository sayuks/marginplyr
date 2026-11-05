test_that("share dependencies follow a literal closure's invocation", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expressions <- alist(
    (function(prior) {
      read_prior <- function() sum(prior)
      rm(prior)
      read_prior()
    })(1),
    (function(prior, read_prior = function() sum(prior)) {
      rm(prior)
      read_prior()
    })(1),
    (function(prior, read_prior = function() sum(prior)) {
      force(read_prior)
      rm(prior)
      read_prior()
    })(1),
    (function(prior) {
      remove_prior <- function() rm("prior", envir = parent.frame())
      remove_prior()
      sum(prior)
    })(1),
    (function(prior) {
      read_prior <- function() sum(prior)
      rm(prior)
      (function(prior) (read_prior)())(2)
    })(1),
    (function() {
      remove_prior <- function() rm("prior", envir = parent.frame())
      (function(prior) {
        remove_prior()
        sum(prior)
      })(1)
    })(),
    (function(prior) {
      remove_prior <- function() rm("prior", inherits = TRUE)
      (function(prior) remove_prior())(2)
      sum(prior)
    })(1),
    (function(prior) {
      rm(prior)
      read_prior <- function() sum(prior)
      do.call("read_prior", list())
    })(1),
    (function(prior) {
      rm(prior)
      read_prior <- function() sum(prior)
      dplyr::summarise(data.frame(x = 1), z = read_prior())$z
    })(1)
  )
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (expr in expressions) {
      expect_error(
        summarize_with_margins(
          data, value = sum(x), prior = !!share, derived = !!expr,
          .grouping = rollup(group)
        ),
        "Ordinary summaries cannot use an earlier .* share",
        class = "marginplyr_error"
      )
      expect_error(
        summarize_with_margins(
          data, prior = sum(x), value = !!expr, ratio = !!share,
          .grouping = rollup(group)
        ),
        "depends on earlier summary alias `prior`",
        class = "marginplyr_error"
      )
    }
  }
})

test_that("retained bindings and uncalled local closures are valid", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expressions <- alist(
    (function(prior) {
      read_prior <- function() sum(prior)
      observed <- read_prior()
      rm(prior)
      observed
    })(1),
    (function(prior) {
      read_prior <- function() {
        stop("This closure is never called.")
        sum(prior)
      }
      rm(prior)
      1
    })(1),
    (function(prior, read_prior = function() sum(prior)) {
      force(read_prior)
      rm(prior)
      1
    })(1),
    (function(prior) {
      remove_prior <- function() rm("prior", envir = parent.frame())
      (function(prior) remove_prior())(2)
      sum(prior)
    })(1),
    (function() {
      recurse <- function(n) if (n == 0) 1 else recurse(n - 1)
      recurse(2)
    })()
  )
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (expr in expressions) {
      forward <- summarize_with_margins(
        data, value = sum(x), prior = !!share, derived = !!expr,
        .grouping = rollup(group)
      )
      expect_identical(forward$derived, c(1, 1, 1))
      expect_identical(forward$prior, c(0.25, 0.75, 1))
      reverse <- summarize_with_margins(
        data, prior = sum(x), value = !!expr, ratio = !!share,
        .grouping = rollup(group)
      )
      expect_identical(reverse$value, c(1, 1, 1))
      expect_identical(reverse$ratio, c(1, 1, 1))
    }
  }
})

test_that("a saved closure passed to unknown code is analyzed conservatively", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  invoke <- function(fn) fn()
  expect_error(
    summarize_with_margins(
      data, value = sum(x), prior = share_of_parent(value),
      derived = (function(prior) {
        read_prior <- function() sum(prior)
        rm(prior)
        invoke(read_prior)
      })(1),
      .grouping = rollup(group)
    ),
    "Ordinary summaries cannot use an earlier Parent share",
    class = "marginplyr_error"
  )
})
