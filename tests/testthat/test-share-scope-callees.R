test_that("a captured local function cannot hide a removed binding", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expression <- rlang::new_quosure(quote({
    prior <- 0
    local(rm(prior))
    sum(prior)
  }), rlang::env(local = identity))
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (kind in c("parent", "total")) {
      helper <- rlang::call2(paste0("share_of_", kind), quote(value))
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
})

test_that("known base callees retain ordinary function lookup", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  local_body <- quote({
    prior <- 2
    local({
      prior <- 0
      rm(prior)
    })
    sum(prior)
  })
  qualified_local <- quote({
    prior <- 2
    base::local({
      prior <- 0
      rm(prior)
    })
    sum(prior)
  })
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    locals <- c(lapply(list(base::local, 1), function(binding) {
      rlang::new_quosure(local_body, rlang::env(local = binding))
    }), list(rlang::new_quosure(qualified_local, rlang::env(local = identity))))
    for (expression in locals) {
      result <- summarize_with_margins(
        data, prior = sum(x), value = !!expression,
        fraction = !!helper, .grouping = rollup(group)
      )
      expect_equal(result$value, c(2, 2, 2))
      expect_equal(result$fraction, c(1, 1, 1))
    }
    for (name in c("baseenv", "emptyenv")) {
      bindings <- list(get(name, envir = base::baseenv()), 1, environment)
      for (binding in bindings) {
        outside <- new.env(parent = base::baseenv())
        outside$prior <- 7
        scope <- rlang::env(outside = outside)
        rlang::env_bind(scope, !!name := binding)
        parent <- if (identical(binding, environment)) {
          rlang::call2(name, .ns = "base")
        } else {
          rlang::call2(name)
        }
        expression <- rlang::new_quosure(rlang::expr({
          parent.env(outside) <- !!parent
          get("prior", envir = outside)
        }), scope)
        result <- summarize_with_margins(
          data, prior = sum(x), value = !!expression,
          fraction = !!helper, .grouping = rollup(group)
        )
        expect_equal(result$value, c(7, 7, 7))
        expect_equal(result$fraction, c(1, 1, 1))
      }
    }
  }
})

test_that("callee proofs never invoke active bindings or force promises", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  for (name in c("local", "baseenv", "emptyenv")) {
    for (mode in c("active", "lazy")) {
      calls <- 0L
      scope <- rlang::env(outside = new.env(parent = base::baseenv()))
      callback <- function() {
        calls <<- calls + 1L
        get(name, envir = base::baseenv())
      }
      if (mode == "active") {
        makeActiveBinding(name, callback, scope)
      } else {
        delayedAssign(name, callback(), assign.env = scope)
      }
      expression <- if (name == "local") {
        quote({
          prior <- 0
          local(rm(prior))
          sum(prior)
        })
      } else {
        rlang::expr({
          parent.env(outside) <- !!rlang::call2(name)
          sum(get("prior", envir = outside))
        })
      }
      expression <- rlang::new_quosure(expression, scope)
      for (input in list(data, dbplyr::tbl_lazy(data))) {
        expect_error(
          summarize_with_margins(
            input, prior = sum(x), value = !!expression,
            fraction = share_of_total(value), .grouping = rollup(group)
          ),
          "depends on earlier summary alias `prior`",
          class = "marginplyr_error"
        )
        expect_identical(calls, 0L)
        expect_identical(parent.env(scope$outside), base::baseenv())
      }
    }
  }
})

test_that("shadowed environment constructors prove no safe parent", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (kind in c("parent", "total")) {
      helper <- rlang::call2(paste0("share_of_", kind), quote(value))
      for (name in c("baseenv", "emptyenv")) {
        for (binding in c("captured", "assigned")) {
          for (direction in c("source", "derived")) {
            outside <- new.env(parent = base::baseenv())
            scope <- rlang::env(outside = outside)
            rlang::env_bind(scope, !!name := base::environment)
            assignment <- if (binding == "assigned") {
              list(rlang::call2("<-", rlang::sym(name), quote(environment)))
            } else {
              list()
            }
            expression <- rlang::new_quosure(rlang::expr({
              !!!assignment
              parent.env(outside) <- !!rlang::call2(name)
              sum(get("prior", envir = outside))
            }), scope)
            if (direction == "source") {
              expect_error(
                summarize_with_margins(
                  input, prior = sum(x), value = !!expression,
                  fraction = !!helper, .grouping = rollup(group)
                ),
                "depends on earlier summary alias `prior`",
                class = "marginplyr_error"
              )
            } else {
              expect_error(
                summarize_with_margins(
                  input, value = sum(x), prior = !!helper,
                  derived = !!expression, .grouping = rollup(group)
                ),
                "Ordinary summaries cannot use an earlier .* share",
                class = "marginplyr_error"
              )
            }
            expect_identical(parent.env(outside), base::baseenv())
          }
        }
      }
    }
  }
})
