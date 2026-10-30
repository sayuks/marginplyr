test_that("visible reparenting exposes both share dependency directions", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  original <- data
  reads <- alist(
    get("prior", envir = outside),
    base::get0("prior", envir = outside),
    mget("prior", envir = outside, inherits = TRUE)[[1L]],
    as.integer(exists("prior", envir = outside))
  )
  mutations <- alist(
    parent.env(outside) <- environment(),
    base::`parent.env<-`(outside, environment())
  )
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (kind in c("parent", "total")) {
      helper <- rlang::call2(paste0("share_of_", kind), quote(value))
      for (read in reads) {
        for (mutation in mutations) {
          for (placement in c("same", "prior", "separate")) {
            outside <- new.env(parent = baseenv())
            expression <- if (placement == "same") {
              rlang::expr({
                !!mutation
                !!read
              })
            } else {
              read
            }
            ordinary <- if (placement == "prior") {
              rlang::expr({
                !!mutation
                sum(x)
              })
            } else {
              quote(sum(x))
            }
            mark <- if (placement == "separate") {
              list(mark = rlang::expr({
                !!mutation
                0
              }))
            } else {
              list()
            }
            expect_error(
              summarize_with_margins(
                input, !!!mark, prior = !!ordinary, value = !!expression,
                fraction = !!helper, .grouping = rollup(group)
              ),
              "depends on earlier summary alias `prior`",
              class = "marginplyr_error"
            )
            expect_identical(parent.env(outside), baseenv())
            expect_error(
              summarize_with_margins(
                input, !!!mark, value = !!ordinary, prior = !!helper,
                derived = sum(!!expression), .grouping = rollup(group)
              ),
              "Ordinary summaries cannot use an earlier .* share",
              class = "marginplyr_error"
            )
            expect_identical(parent.env(outside), baseenv())
          }
        }
      }
    }
  }
  expect_identical(data, original)
})

test_that("external and non-inheriting reads survive visible reparenting", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  expressions <- alist(
    get("prior", envir = outside),
    {
      parent.env(outside) <- baseenv()
      get("prior", envir = outside)
    },
    {
      base::parent.env(outside) <- base::baseenv()
      get("prior", envir = outside)
    },
    {
      base::`parent.env<-`(outside, baseenv())
      get("prior", envir = outside)
    },
    {
      parent.env(outside) <- emptyenv()
      get("prior", envir = outside)
    },
    {
      parent.env(outside) <- environment()
      get("prior", envir = outside, inherits = FALSE)
    },
    {
      parent.env(outside) <- environment()
      get0("prior", outside, inherits = FALSE)
    },
    {
      parent.env(outside) <- environment()
      mget("prior", outside)[[1L]]
    },
    {
      parent.env(outside) <- environment()
      as.numeric(exists("prior", outside, inherits = FALSE))
    },
    {
      parent.env(other) <- environment()
      get("prior", envir = outside)
    },
    {
      quote(parent.env(outside) <- environment())
      get("prior", envir = outside)
    }
  )
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (expression in expressions) {
      outside <- new.env(parent = baseenv())
      outside$prior <- 7
      other <- new.env(parent = baseenv())
      result <- summarize_with_margins(
        data, prior = sum(x), value = !!expression,
        fraction = !!helper, .grouping = rollup(group)
      )
      expected <- if (any(grepl("exists", rlang::expr_deparse(expression)))) {
        1
      } else {
        7
      }
      expect_equal(result$value, rep(expected, 3))
      expect_equal(result$fraction, c(1, 1, 1))
      result <- summarize_with_margins(
        data, value = sum(x), prior = !!helper,
        derived = !!expression, .grouping = rollup(group)
      )
      expect_equal(result$derived, rep(expected, 3))
    }
  }
  outside <- new.env(parent = baseenv())
  result <- summarize_with_margins(
    data, prior = sum(x),
    value = {
      parent.env(outside) <- environment()
      get("prior", envir = outside)
    },
    .grouping = rollup(group)
  )
  expect_equal(result$value, c(1, 3, 4))
})

test_that("reparenting invalidates inherited and aliased environment proofs", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  expressions <- alist(
    {
      parent.env(outside) <- environment()
      get("prior", envir = alias)
    },
    {
      parent.env(outside) <- environment()
      get("prior", envir = child)
    },
    {
      local_env <- alias
      parent.env(local_env) <- environment()
      get("prior", envir = alias)
    },
    {
      parent.env(.env$outside) <- environment()
      get("prior", envir = .env[["outside"]])
    },
    eval(quote({
      parent.env(outside) <- environment()
      get("prior", envir = outside)
    })),
    {
      (function(other) {
        parent.env(other) <- environment()
      })(outside)
      get("prior", envir = outside)
    }
  )
  for (expression in expressions) {
    outside <- new.env(parent = baseenv())
    alias <- outside
    other <- new.env(parent = baseenv())
    child <- new.env(parent = outside)
    expect_error(
      summarize_with_margins(
        data, value = sum(x), prior = share_of_total(value),
        derived = sum(!!expression), .grouping = rollup(group)
      ),
      "Ordinary summaries cannot use an earlier Total share",
      class = "marginplyr_error"
    )
    expect_identical(parent.env(outside), baseenv())
  }
})

test_that("unresolved reparenting targets never execute caller code", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  calls <- 0L
  outside <- new.env(parent = baseenv())
  target <- function() {
    calls <<- calls + 1L
    outside
  }
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    expect_error(
      summarize_with_margins(
        input, value = sum(x), prior = share_of_total(value),
        derived = {
          parent.env(target()) <- environment()
          get("prior", envir = outside)
        },
        .grouping = rollup(group)
      ),
      "Ordinary summaries cannot use an earlier Total share",
      class = "marginplyr_error"
    )
    expect_identical(calls, 0L)
    expect_identical(parent.env(outside), baseenv())
  }
})
