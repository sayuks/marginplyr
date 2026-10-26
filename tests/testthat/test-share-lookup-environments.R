test_that("explicit mask lookups follow both share dependency rules", {
  data <- data.frame(group = c("x", "y"), value = c(20, 60), first = c(20, 60))
  lookups <- alist(
    get("first", pos = -1L),
    get("first", -1),
    get("first", envir = environment()),
    base::get("first", envir = base::environment()),
    (get)("first", envir = (environment())),
    get("first", en = environment()),
    get0("first", envir = environment()),
    get0("first", environment()),
    exists("first", where = -1L),
    exists("first", -1),
    exists("first", envir = environment()),
    exists("first", frame = sys.nframe()),
    mget("first", envir = environment(), inherits = TRUE)[[1L]],
    mget("first", pos = -1L, inherits = TRUE)[[1L]],
    get("first", envir = parent.frame()),
    get("first", envir = sys.frame(sys.nframe())),
    get("first", envir = as.environment(-1L)),
    get("first", envir = list2env(list(), parent = environment())),
    get("first", pos = 1L, envir = environment()),
    exists("first", where = 1L, envir = environment()),
    get("first", envir = ),
    get("first", unknown = environment())
  )

  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (kind in c("parent", "total")) {
      source_share <- rlang::call2(paste0("share_of_", kind), quote(total))
      for (lookup in lookups) {
        expect_error(
          rlang::inject(summarize_with_margins(
            input,
            first = sum(value),
            total = !!lookup,
            share = !!source_share,
            .grouping = rollup(group)
          )),
          "depends on earlier summary alias `first`",
          class = "marginplyr_error"
        )
        expect_error(
          rlang::inject(summarize_with_margins(
            input,
            total = sum(value),
            first = !!source_share,
            derived = sum(!!lookup),
            .grouping = rollup(group)
          )),
          paste0("Ordinary summaries cannot use an earlier ",
                 if (kind == "parent") "Parent" else "Total", " share"),
          class = "marginplyr_error"
        )
      }
    }
  }
})

test_that("actual external environments remain independent of shares", {
  data <- data.frame(group = c("x", "y"), value = c(20, 60), first = c(20, 60))
  outside <- rlang::env(first = 10)
  lookups <- alist(
    get("first", envir = outside),
    get("first", pos = outside),
    get("first", envir = .env$outside),
    get("first", envir = .env[["outside"]]),
    get("first", (outside)),
    get0("first", envir = outside),
    exists("first", where = outside),
    exists("first", frame = sys.nframe(), envir = outside),
    mget("first", outside, inherits = TRUE)[[1L]]
  )
  injected <- rlang::expr(get("first", envir = !!outside))

  for (kind in c("parent", "total")) {
    source_share <- rlang::call2(paste0("share_of_", kind), quote(total))
    for (lookup in c(lookups, list(injected))) {
      result <- rlang::inject(summarize_with_margins(
        data,
        first = sum(value),
        total = sum(value) + as.numeric(!!lookup),
        share = !!source_share,
        .grouping = rollup(group)
      ))
      is_exists <- rlang::is_call(lookup, "exists")
      expected <- if (is_exists) {
        c(21 / 81, 61 / 81, 1)
      } else {
        c(1 / 3, 7 / 9, 1)
      }
      expect_equal(result$share, expected)

      result <- rlang::inject(summarize_with_margins(
        data,
        total = sum(value),
        first = !!source_share,
        derived = as.numeric(!!lookup),
        .grouping = rollup(group)
      ))
      expect_equal(result$first, c(0.25, 0.75, 1))
      expect_equal(result$derived, rep(if (is_exists) 1 else 10, 3))
    }
    for (lookup in lookups) {
      lazy <- rlang::inject(summarize_with_margins(
        dbplyr::tbl_lazy(data),
        total = sum(value),
        first = !!source_share,
        derived = !!lookup,
        .grouping = rollup(group),
        .check_share_source = FALSE
      ))
      expect_s3_class(lazy, "tbl_lazy")
    }
  }
})

test_that("unresolved lookup environments never run caller code in planning", {
  data <- data.frame(group = c("x", "y"), value = c(20, 60), first = c(20, 60))
  scope <- rlang::env()
  calls <- 0L
  scope$make_env <- function() {
    calls <<- calls + 1L
    environment()
  }
  makeActiveBinding("active", scope$make_env, scope)
  delayedAssign("deferred", make_env(), assign.env = scope, eval.env = scope)
  scope$position <- -1L
  expressions <- alist(
    get("first", envir = make_env()),
    get("first", envir = active),
    get("first", envir = deferred),
    get("first", pos = position),
    get("first", envir = absent),
    get("first", envir = .env[[paste0("act", "ive")]]),
    get(paste0("fir", "st"), envir = environment())
  )
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (expr in expressions) {
      lookup <- rlang::new_quosure(expr, scope)
      expect_error(
        rlang::inject(summarize_with_margins(
          input,
          total = sum(value),
          first = share_of_total(total),
          derived = !!lookup,
          .grouping = rollup(group)
        )),
        "Ordinary summaries cannot use an earlier Total share",
        class = "marginplyr_error"
      )
      expect_identical(calls, 0L)
    }
  }
})

test_that("mask and local bindings cannot claim an external environment", {
  # The expression below may shadow this lexical environment.
  outside <- rlang::env(first = 10)
  expect_true(is.environment(outside))
  data <- data.frame(group = c("x", "y"), value = c(20, 60))
  expressions <- alist(
    {
      outside <- environment()
      get("first", envir = outside)
    },
    {
      if (length(value) > 0L) outside <- environment()
      get("first", envir = outside)
    },
    {
      first <- 1
      get("first", envir = parent.env(environment()))
    },
    (function(outside) get("first", envir = outside))(environment()),
    ({
      if (length(value) > 0L) outside <- environment()
      function() get("first", envir = outside)
    })(),
    {
      for (outside in list(environment())) get("first", envir = outside)
    }
  )
  for (expr in expressions) {
    expect_error(
      rlang::inject(summarize_with_margins(
        data,
        total = sum(value),
        first = share_of_total(total),
        derived = !!expr,
        .grouping = rollup(group)
      )),
      "Ordinary summaries cannot use an earlier Total share",
      class = "marginplyr_error"
    )
  }
  expect_error(
    summarize_with_margins(
      data,
      outside = environment(),
      total = sum(value),
      first = share_of_total(total),
      derived = get("first", envir = outside),
      .grouping = rollup(group)
    ),
    "Ordinary summaries cannot use an earlier Total share",
    class = "marginplyr_error"
  )
})
