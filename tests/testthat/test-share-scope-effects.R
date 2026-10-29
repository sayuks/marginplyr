test_that("nested removal exposes both forbidden share dependencies", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  original <- data
  removals <- alist(
    identity(rm(prior)),
    if (sum(x) > 0) remove(prior),
    eval(quote(rm(prior))),
    evalq(remove(prior)),
    identity({
      rm(prior)
      NULL
    }),
    invisible(base::rm(list = "prior")),
    for (i in 1) rm(prior)
  )
  removals <- c(removals, list(rlang::call2("if", TRUE, quote(rm(prior)))))
  expressions <- c(lapply(removals, function(removal) {
    rlang::expr({
      prior <- 0
      !!removal
      sum(prior)
    })
  }), alist(
    {
      for (prior in 1) rm(prior)
      sum(prior)
    },
    (function(prior = 0) {
      identity(rm(prior))
      sum(prior)
    })(),
    {
      prior <- 0
      sum({
        rm(prior)
        0
      }, prior)
    },
    {
      prior <- 0
      other <- identity(rm(prior))
      sum(prior)
    }
  ))

  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (kind in c("parent", "total")) {
      helper <- rlang::call2(paste0("share_of_", kind), quote(value))
      for (expression in expressions) {
        expect_error(
          summarize_with_margins(
            input, prior = sum(x), value = !!expression,
            fraction = !!helper, .grouping = rollup(group)
          ),
          "depends on earlier summary alias `prior`",
          class = "marginplyr_error"
        )
        expect_error(
          summarize_with_margins(
            input, value = sum(x), prior = !!helper,
            derived = !!expression, .grouping = rollup(group)
          ),
          "Ordinary summaries cannot use an earlier .* share",
          class = "marginplyr_error"
        )
      }
    }
  }
  expect_identical(data, original)
})

test_that("retained lexical bindings and ordinary aliases keep their values", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  expressions <- alist(
    {
      prior <- 2
      identity(NULL)
      sum(prior)
    },
    {
      prior <- 2
      if (FALSE) rm(prior)
      sum(prior)
    },
    {
      prior <- 2
      if (TRUE) NULL else rm(prior)
      sum(prior)
    },
    {
      prior <- 2
      quote(rm(prior))
      sum(prior)
    },
    {
      prior <- 2
      function() rm(prior)
      sum(prior)
    },
    {
      prior <- 2
      local({
        prior <- 0
        rm(prior)
      })
      sum(prior)
    },
    {
      prior <- 0
      prior <- {
        rm(prior)
        2
      }
      sum(prior)
    }
  )
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (expression in expressions) {
      result <- summarize_with_margins(
        data, prior = sum(x), value = !!expression,
        fraction = !!helper, .grouping = rollup(group)
      )
      expect_equal(result$value, c(2, 2, 2))
      expect_equal(result$fraction, c(1, 1, 1))
      result <- summarize_with_margins(
        data, value = sum(x), prior = !!helper,
        derived = !!expression, .grouping = rollup(group)
      )
      expect_equal(result$derived, c(2, 2, 2))
    }
  }
  result <- summarize_with_margins(
    data, prior = sum(x),
    value = {
      prior <- 0
      identity(rm(prior))
      sum(prior)
    },
    .grouping = rollup(group)
  )
  expect_equal(result$value, c(1, 3, 4))
})

test_that("nested removal analysis never evaluates caller expressions", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  calls <- 0L
  touch <- function() {
    calls <<- calls + 1L
  }
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    expect_error(
      summarize_with_margins(
        input, value = sum(x), prior = share_of_total(value),
        derived = {
          prior <- 0
          identity({
            touch()
            rm(prior)
          })
          sum(prior)
        },
        .grouping = rollup(group)
      ),
      "Ordinary summaries cannot use an earlier Total share",
      class = "marginplyr_error"
    )
    expect_identical(calls, 0L)
  }
})
