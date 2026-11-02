test_that("resolved removals expose both share dependency directions", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  removals <- alist(
    (get("rm"))(prior),
    (get0("remove"))(prior),
    (match.fun("rm"))(prior),
    (getFunction("remove"))(prior),
    do.call("rm", list("prior")),
    do.call(base::remove, list("prior")),
    do.call(get("rm"), list("prior")),
    (get("rm", envir = {
      base::rm(prior)
      baseenv()
    }))("other")
  )
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (kind in c("parent", "total")) {
      helper <- rlang::call2(paste0("share_of_", kind), quote(value))
      for (removal in removals) {
        expression <- rlang::expr({
          prior <- 2
          other <- 0
          identity(!!removal)
          sum(prior)
        })
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
})

test_that("caller functions with mutation names keep ordinary lookup", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  expressions <- alist(
    {
      prior <- 2
      rm("prior")
      sum(prior)
    },
    {
      prior <- 2
      (get("rm"))("prior")
      sum(prior)
    },
    {
      prior <- 2
      (get0("rm"))("prior")
      sum(prior)
    },
    {
      prior <- 2
      (getFunction("rm", where = functions))("prior")
      sum(prior)
    },
    {
      prior <- 2
      (get("rm", envir = functions))("prior")
      sum(prior)
    },
    {
      prior <- 2
      (get("rm", envir = child))("prior")
      sum(prior)
    },
    {
      prior <- 2
      do.call("rm", list("prior"))
      sum(prior)
    },
    {
      prior <- 2
      do.call(get("rm"), list("prior"))
      sum(prior)
    },
    {
      (get("parent.env<-"))(outside, environment())
      get("prior", envir = outside)
    },
    {
      do.call("parent.env<-", list(outside, environment()))
      get("prior", envir = outside)
    },
    {
      `parent.env<-`(outside, environment())
      get("prior", envir = outside)
    }
  )
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (expression in expressions) {
      outside <- new.env(parent = baseenv())
      outside$prior <- 2
      scope <- rlang::env(
        outside = outside, rm = function(...) NULL,
        `parent.env<-` = function(...) NULL
      )
      scope$functions <- scope
      scope$child <- new.env(parent = scope)
      expression <- rlang::new_quosure(expression, scope)
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
      expect_identical(parent.env(outside), baseenv())
    }
  }
})

test_that("parent changes invalidate inherited callee shadows", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (kind in c("parent", "total")) {
      functions <- rlang::env(baseenv(), rm = function(...) NULL)
      outside <- new.env(parent = functions)
      helper <- rlang::call2(paste0("share_of_", kind), quote(value))
      expect_error(
        summarize_with_margins(
          input, prior = sum(x), value = {
            prior <- 2
            parent.env(outside) <- baseenv()
            (get("rm", envir = outside))("prior")
            sum(prior)
          }, fraction = !!helper, .grouping = rollup(group)
        ),
        "depends on earlier summary alias `prior`",
        class = "marginplyr_error"
      )
      expect_identical(parent.env(outside), functions)
    }
  }
})

test_that("indirect mutations retain independent lexical and external reads", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  expressions <- alist(
    {
      prior <- 7
      other <- 0
      (get("rm"))(other)
      sum(prior)
    },
    {
      prior <- 7
      other <- 0
      do.call("remove", list("other"))
      sum(prior)
    },
    {
      prior <- 7
      quote((get("rm"))(prior))
      sum(prior)
    },
    {
      prior <- 7
      if (FALSE) do.call("rm", list("prior"))
      sum(prior)
    },
    {
      prior <- 7
      local({
        prior <- 0
        (get("rm"))(prior)
      })
      sum(prior)
    },
    {
      (get("parent.env<-"))(outside, baseenv())
      get("prior", envir = outside)
    },
    {
      do.call("parent.env<-", list(outside, emptyenv()))
      get("prior", envir = outside)
    },
    {
      (match.fun("parent.env<-"))(outside, environment())
      get("prior", envir = outside, inherits = FALSE)
    },
    {
      do.call("parent.env<-", list(outside, environment()))
      get("prior", envir = outside, inherits = FALSE)
    },
    {
      (getFunction("parent.env<-"))(other, environment())
      get("prior", envir = outside)
    },
    {
      do.call("parent.env<-", list(other, environment()))
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
      expect_equal(result$value, c(7, 7, 7))
      expect_equal(result$fraction, c(1, 1, 1))
      result <- summarize_with_margins(
        data, value = sum(x), prior = !!helper,
        derived = !!expression, .grouping = rollup(group)
      )
      expect_equal(result$derived, c(7, 7, 7))
    }
  }
})

test_that("indirect scope mutations keep ordinary no-share evaluation", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  expressions <- alist(
    {
      prior <- 2
      (get("rm"))(prior)
      sum(prior)
    },
    {
      prior <- 2
      do.call("rm", list("prior"))
      sum(prior)
    },
    {
      (get("parent.env<-"))(outside, environment())
      get("prior", envir = outside)
    },
    {
      do.call("parent.env<-", list(outside, environment()))
      get("prior", envir = outside)
    }
  )
  for (expression in expressions) {
    outside <- new.env(parent = baseenv())
    result <- summarize_with_margins(
      data, prior = sum(x), value = !!expression, .grouping = rollup(group)
    )
    expect_equal(result$value, c(1, 3, 4))
  }
})

test_that("unreadable mutations never execute caller code in planning", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  expressions <- alist(
    {
      prior <- 2
      do.call("rm", arguments())
      sum(prior)
    },
    {
      do.call("parent.env<-", arguments())
      get("prior", envir = outside)
    },
    {
      (get("parent.env<-"))(target(), environment())
      get("prior", envir = outside)
    },
    {
      (get("parent.env<-", envir = target()))(outside, environment())
      get("prior", envir = outside)
    },
    {
      prior <- 2
      (get("rm", envir = target()))(prior)
      sum(prior)
    }
  )
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (kind in c("parent", "total")) {
      helper <- rlang::call2(paste0("share_of_", kind), quote(value))
      for (expression in expressions) {
        calls <- rlang::env(n = 0L)
        outside <- new.env(parent = baseenv())
        target <- arguments <- function() {
          calls$n <- calls$n + 1L
          stop("Caller code ran during planning")
        }
        expect_error(
          summarize_with_margins(
            input, prior = sum(x), value = !!expression,
            fraction = !!helper, .grouping = rollup(group)
          ),
          "depends on earlier summary alias `prior`",
          class = "marginplyr_error"
        )
        expect_identical(calls$n, 0L)
        expect_identical(parent.env(outside), baseenv())
      }
    }
  }
})

test_that("callee proofs respect resolver lookup and deferred bindings", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  for (resolver in c("get", "get0", "match.fun", "getFunction", "do.call")) {
    outside <- new.env(parent = baseenv())
    outside$prior <- 7
    scope <- rlang::env(outside = outside)
    rlang::env_bind(scope, !!resolver := function(...) function(...) NULL)
    mutation <- if (resolver == "do.call") {
      quote(do.call("parent.env<-", list(outside, environment())))
    } else {
      rlang::call2(rlang::call2(resolver, "parent.env<-"),
                   quote(outside), quote(environment()))
    }
    expression <- rlang::new_quosure(rlang::expr({
      !!mutation
      base::get("prior", envir = outside)
    }), scope)
    result <- summarize_with_margins(
      data, prior = sum(x), value = !!expression,
      fraction = share_of_total(value), .grouping = rollup(group)
    )
    expect_equal(result$value, c(7, 7, 7))
    expect_identical(parent.env(outside), baseenv())
  }
  for (mode in c("active", "lazy")) {
    calls <- rlang::env(n = 0L)
    scope <- rlang::env()
    callback <- function() {
      calls$n <- calls$n + 1L
      base::rm
    }
    if (mode == "active") {
      makeActiveBinding("rm", callback, scope)
    } else {
      delayedAssign("rm", callback(), assign.env = scope)
    }
    expression <- rlang::new_quosure(quote({
      prior <- 2
      (get("rm"))(prior)
      sum(prior)
    }), scope)
    expect_error(
      summarize_with_margins(
        data, prior = sum(x), value = !!expression,
        fraction = share_of_total(value), .grouping = rollup(group)
      ),
      "depends on earlier summary alias `prior`",
      class = "marginplyr_error"
    )
    expect_identical(calls$n, 0L)
  }
})

test_that("resolved parent setters expose both share dependency directions", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  mutations <- alist(
    (get("parent.env<-"))(outside, environment()),
    (get0("parent.env<-"))(outside, environment()),
    (match.fun("parent.env<-"))(outside, environment()),
    (getFunction("parent.env<-"))(outside, environment()),
    do.call("parent.env<-", list(outside, environment())),
    do.call(base::`parent.env<-`, list(env = outside, value = environment())),
    do.call(get("parent.env<-"), list(outside, environment()))
  )
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (kind in c("parent", "total")) {
      helper <- rlang::call2(paste0("share_of_", kind), quote(value))
      for (mutation in mutations) {
        for (placement in c("same", "earlier")) {
          outside <- new.env(parent = baseenv())
          expression <- if (placement == "same") {
            rlang::expr({
              !!mutation
              sum(get("prior", envir = outside))
            })
          } else {
            quote(sum(get("prior", envir = outside)))
          }
          earlier <- if (placement == "earlier") {
            rlang::expr({
              !!mutation
              sum(x)
            })
          } else {
            quote(sum(x))
          }
          expect_error(
            summarize_with_margins(
              input, prior = !!earlier, value = !!expression,
              fraction = !!helper, .grouping = rollup(group)
            ),
            "depends on earlier summary alias `prior`",
            class = "marginplyr_error"
          )
          expect_identical(parent.env(outside), baseenv())
          expect_error(
            summarize_with_margins(
              input, value = !!earlier, prior = !!helper,
              derived = !!expression, .grouping = rollup(group)
            ),
            "Ordinary summaries cannot use an earlier .* share",
            class = "marginplyr_error"
          )
          expect_identical(parent.env(outside), baseenv())
        }
      }
    }
  }
})
