test_that("visible list2env rebinding exposes both share dependencies", {
  data <- data.frame(
    partition = c("P", "P", NA, NA), group = c("a", "b", "a", "b"),
    x = c(1, 3, 2, 6), prior = c(100, 300, 200, 600)
  )
  setters <- alist(
    base::list2env({
      list(outside = environment())
    }, envir = environment()),
    base::list2env(identity(list(outside = environment())),
                   envir = environment()),
    base::list2env(setNames(list(environment()), "outside"),
                   envir = environment()),
    list2env(list(outside = environment()), envir = environment()),
    base::list2env(list(outside = environment()), envir = environment()),
    (get("list2env"))(list(outside = environment()), envir = environment()),
    (get0("list2env"))(list(outside = environment()), envir = environment()),
    (match.fun("list2env"))(
      list(outside = environment()), envir = environment()
    ),
    (getFunction("list2env"))(
      list(outside = environment()), envir = environment()
    ),
    do.call("list2env", list(
      list(outside = environment()), envir = environment()
    )),
    do.call(base::list2env, list(x = list(outside = environment()),
                                 envir = environment())),
    base::list2env(
      x = (base::list(outside = environment())), en = environment()
    ),
    {
      list2env <- base::list2env
      list2env(list(outside = environment()), envir = environment())
    },
    assign("outside", environment(), envir = environment())
  )
  inputs <- list(data, tibble::as_tibble(data), dbplyr::tbl_lazy(data))
  if (suggest_available("data.table")) {
    inputs <- c(inputs, list(data.table::as.data.table(data)))
  }
  for (input in inputs) for (fixed in c(FALSE, TRUE)) {
    for (kind in c("parent", "total")) for (setter in setters) {
      outside <- new.env(parent = baseenv())
      helper <- rlang::call2(paste0("share_of_", kind), quote(value))
      expression <- rlang::expr({
        !!setter
        sum(get("prior", envir = outside))
      })
      expect_error(
        summarize_with_margins(
          input, prior = sum(x), value = !!expression,
          fraction = !!helper, .grouping = rollup(group),
          .by = !!if (fixed) quote(partition) else NULL
        ),
        "depends on earlier summary alias `prior`",
        class = "marginplyr_error"
      )
      expect_error(
        summarize_with_margins(
          input, value = sum(x), prior = !!helper,
          derived = !!expression, .grouping = rollup(group),
          .by = !!if (fixed) quote(partition) else NULL
        ),
        "Ordinary summaries cannot use an earlier .* share",
        class = "marginplyr_error"
      )
    }
  }
})

test_that("list2env preserves independent lookups and ordinary evaluation", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  controls <- alist(
    NULL,
    base::list2env({
      list(outside = environment())
    }),
    base::list2env(identity(list(outside = environment())), envir = NULL),
    base::list2env(list(outside = environment())),
    base::list2env(list(outside = environment()), envir = NULL),
    base::list2env(list(outside = environment()), envir = (NULL),
                   parent = environment()),
    do.call("list2env", list(list(outside = environment()))),
    base::list2env(list(other = environment()), envir = environment()),
    base::list2env(list(), envir = environment()),
    quote(base::list2env(list(outside = environment()), envir = environment())),
    opaque()
  )
  for (kind in c("parent", "total")) for (mutation in controls) {
    outside <- rlang::env(baseenv(), prior = 7)
    opaque <- function() NULL
    expression <- rlang::expr({
      !!mutation
      sum(get("prior", envir = outside))
    })
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
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
    expect_equal(result$prior, c(0.25, 0.75, 1))
    expect_equal(result$derived, c(7, 7, 7))
  }
  outside <- new.env(parent = baseenv())
  result <- summarize_with_margins(
    data, prior = sum(x), value = {
      base::list2env(list(outside = environment()), envir = environment())
      sum(get("prior", envir = outside))
    }, .grouping = rollup(group)
  )
  expect_equal(result$value, c(1, 3, 4))
})

test_that("caller-defined list2env callees retain ordinary lookup", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  mutations <- alist(
    list2env(list(outside = environment()), envir = environment()),
    (get("list2env"))(list(outside = environment()), envir = environment()),
    (get0("list2env"))(list(outside = environment()), envir = environment()),
    (getFunction("list2env", where = functions))(
      list(outside = environment()), envir = environment()
    ),
    do.call("list2env", list(
      list(outside = environment()), envir = environment()
    )),
    do.call(get("list2env"), list(list(outside = environment()),
                                  envir = environment()))
  )
  for (kind in c("parent", "total")) for (mutation in mutations) {
    outside <- rlang::env(baseenv(), prior = 7)
    scope <- rlang::env(outside = outside, list2env = function(...) NULL)
    scope$functions <- rlang::env(baseenv(), list2env = function(...) NULL)
    expression <- rlang::new_quosure(rlang::expr({
      !!mutation
      sum(get("prior", envir = outside))
    }), scope)
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
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
})

test_that("list2env rebinding analysis never evaluates caller expressions", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  outside <- new.env(parent = baseenv())
  calls <- 0L
  expression <- quote({
    base::list2env(list(outside = {
      calls <<- calls + 1L
      environment()
    }), envir = environment())
    get("prior", envir = outside)
  })
  expect_error(
    summarize_with_margins(
      data, prior = sum(x), value = !!expression,
      fraction = share_of_total(value), .grouping = rollup(group)
    ),
    "depends on earlier summary alias `prior`", class = "marginplyr_error"
  )
  expect_identical(calls, 0L)
  expect_length(ls(outside), 0L)
})

test_that("unresolved list2env keys are never evaluated for lookup proofs", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  outside <- rlang::env(baseenv(), prior = 7)
  calls <- 0L
  keys <- function() {
    calls <<- calls + 1L
    list(outside = environment())
  }
  expression <- quote({
    base::list2env(keys(), envir = environment())
    sum(get("prior", envir = outside))
  })
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    expect_error(
      summarize_with_margins(
        data, prior = sum(x), value = !!expression,
        fraction = !!helper, .grouping = rollup(group)
      ),
      "depends on earlier summary alias `prior`", class = "marginplyr_error"
    )
    expect_error(
      summarize_with_margins(
        data, value = sum(x), prior = !!helper,
        derived = !!expression, .grouping = rollup(group)
      ),
      "Ordinary summaries cannot use an earlier .* share",
      class = "marginplyr_error"
    )
  }
  expect_identical(calls, 0L)
  expect_equal(outside$prior, 7)
})

test_that("injected list2env syntax keeps its callee environment", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  outside <- rlang::env(baseenv(), prior = 7)
  scope <- rlang::env(outside = outside, list2env = function(...) NULL)
  lookup <- rlang::new_quosure(quote({
    list2env(list(outside = environment()), envir = environment())
    get("prior", envir = outside)
  }), scope)
  result <- summarize_with_margins(
    data, prior = sum(x), value = sum(!!lookup),
    fraction = share_of_total(value), .grouping = rollup(group)
  )
  expect_equal(result$value, c(7, 7, 7))
  expect_equal(result$fraction, c(1, 1, 1))
})
