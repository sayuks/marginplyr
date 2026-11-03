test_that("eval-carried rebinding exposes both share dependencies", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  mutations <- alist(
    base::assign("outside", environment(), envir = environment()),
    eval(quote(base::assign("outside", environment(), envir = environment()))),
    eval(quote(base::list2env(
      list(outside = environment()), envir = environment()
    ))),
    base::eval(expression(
      base::assign("outside", environment(), envir = environment())
    )),
    eval(quote(eval(quote(
      base::assign("outside", environment(), envir = environment())
    )))),
    rlang::eval_bare(quote(
      base::assign("outside", environment(), envir = environment())
    )),
    evalq(base::assign("outside", environment(), envir = environment())),
    eval(str2lang(
      'base::assign("outside", environment(), envir = environment())'
    ))
  )
  inputs <- list(data, tibble::as_tibble(data), dbplyr::tbl_lazy(data))
  for (input in inputs) for (kind in c("parent", "total")) {
    for (mutation in mutations) {
      outside <- new.env(parent = baseenv())
      expression <- rlang::expr({
        !!mutation
        sum(get("prior", envir = outside))
      })
      helper <- rlang::call2(paste0("share_of_", kind), quote(value))
      expect_error(
        summarize_with_margins(
          input, prior = sum(x), value = !!expression,
          fraction = !!helper, .grouping = rollup(group)
        ),
        "depends on earlier summary alias `prior`", class = "marginplyr_error"
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

test_that("eval preserves independent lookups and ordinary evaluation", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  controls <- alist(
    eval(quote(NULL)),
    eval(quote(base::assign("other", environment(), envir = environment()))),
    eval(quote(base::list2env(
      list(other = environment()), envir = environment()
    ))),
    eval(quote(base::list2env(list(outside = environment())))),
    eval(quote(base::list2env(list(outside = environment()), envir = NULL))),
    quote(eval(quote(base::assign(
      "outside", environment(), envir = environment()
    )))),
    eval(quote(quote(base::assign(
      "outside", environment(), envir = environment()
    )))),
    eval(quote(opaque()))
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
    expect_equal(result$derived, c(7, 7, 7))
  }
  for (mutation in alist(
    eval(quote(base::assign("outside", environment(), envir = environment()))),
    eval(quote(base::list2env(
      list(outside = environment()), envir = environment()
    )))
  )) {
    outside <- new.env(parent = baseenv())
    result <- summarize_with_margins(
      data, prior = sum(x), value = {
        !!mutation
        sum(get("prior", envir = outside))
      }, .grouping = rollup(group)
    )
    expect_equal(result$value, c(1, 3, 4))
  }
})

test_that("eval-carried rebinding is analyzed without running caller code", {
  data <- data.frame(group = c("a", "b"), x = c(1, 3))
  outside <- new.env(parent = baseenv())
  calls <- 0L
  for (mutation in alist(
    eval(quote(base::assign("outside", {
      calls <<- calls + 1L
      environment()
    }, envir = environment()))),
    eval(quote(base::list2env(list(outside = {
      calls <<- calls + 1L
      environment()
    }), envir = environment())))
  )) {
    expect_error(
      summarize_with_margins(
        data, prior = sum(x), value = {
          !!mutation
          sum(get("prior", envir = outside))
        }, fraction = share_of_total(value), .grouping = rollup(group)
      ),
      "depends on earlier summary alias `prior`", class = "marginplyr_error"
    )
    expect_identical(calls, 0L)
    expect_length(ls(outside), 0L)
  }
})
