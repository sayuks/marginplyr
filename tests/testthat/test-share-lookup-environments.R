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
    get("first", unknown = environment()),
    eval(substitute(get("first", envir = environment()), env = list()))
  )

  rebound <- alist(
    {
      if (length(value) > 0L) .env <- list(outside = environment())
      get("first", envir = .env$outside)
    },
    {
      if (length(value) > 0L) .env <- list(outside = environment())
      get("first", envir = .env[["outside"]])
    },
    {
      assign("outside", environment())
      get("first", envir = outside)
    },
    {
      assign(paste0("out", "side"), environment())
      get("first", envir = outside)
    },
    {
      assign("outside", environment(), envir = .env)
      get("first", envir = .env$outside)
    },
    {
      delayedAssign("outside", environment())
      get("first", envir = outside)
    },
    {
      makeActiveBinding("outside", function() environment(), environment())
      get("first", envir = outside)
    },
    {
      assign(".env", list(outside = environment()))
      get("first", envir = .env$outside)
    }
  )
  lookups <- c(lookups, lapply(
    rebound, rlang::new_quosure,
    env = rlang::env(outside = rlang::env(first = 10))
  ))

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

test_that(".env selects an external environment shadowed by a column", {
  outside <- rlang::env(first = 10)
  data <- data.frame(group = c("x", "y"), value = c(20, 60), outside = 1:2)
  result <- summarize_with_margins(
    data,
    total = sum(value),
    first = share_of_total(total),
    derived = get("first", envir = .env$outside),
    .grouping = rollup(group)
  )
  expect_equal(result$derived, c(10, 10, 10))
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

test_that("literal mask reads follow both share dependency rules", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  calls <- 0L
  observe <- function(value) {
    calls <<- calls + 1L
    sum(value)
  }
  reads <- alist(
    parent.env(environment())$prior,
    parent.env(environment())[["prior"]],
    parent.env(environment())[["prior", exact = TRUE]],
    parent.env(environment())[["prior", exact = FALSE]],
    getElement(parent.env(environment()), "prior"),
    base::getElement(parent.env(base::environment()), "prior"),
    .subset2(parent.env(environment()), "prior"),
    base::.subset2(parent.env(base::environment()), "prior"),
    rlang::env_get(rlang::current_env(), "prior", inherit = TRUE),
    rlang::env_get(environment(), "prior", inherit = TRUE),
    rlang::env_get(base::environment(), "prior", inherit = TRUE),
    rlang::env_get(
      rlang::current_env(), "prior", inherit = TRUE, last = emptyenv()
    ),
    rlang::env_get(
      rlang::current_env(), "prior", inherit = TRUE, last = baseenv()
    ),
    rlang::env_get(
      rlang::current_env(), "prior", inherit = TRUE, last = globalenv()
    )
  )

  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(source))
    for (read in reads) {
      expect_error(
        summarize_with_margins(
          data, prior = sum(x), source = observe(!!read), ratio = !!share,
          .grouping = rollup(g)
        ),
        "depends on earlier summary alias `prior`",
        class = "marginplyr_error"
      )
      expect_error(
        summarize_with_margins(
          data, source = sum(x), prior = !!share, observed = observe(!!read),
          .grouping = rollup(g)
        ),
        paste0("Ordinary summaries cannot use an earlier ",
               if (kind == "parent") "Parent" else "Total", " share"),
        class = "marginplyr_error"
      )
    }
  }
  expect_identical(calls, 0L)
})

test_that("literal reads from independent environments remain independent", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  external <- rlang::new_environment(list(prior = 7), parent = emptyenv())
  environment <- function() new.env(parent = external)
  reads <- alist(
    external$prior,
    external[["prior"]],
    external[["prior", exact = TRUE]],
    external[["prior", exact = FALSE]],
    getElement(external, "prior"),
    base::getElement(external, "prior"),
    .subset2(external, "prior"),
    base::.subset2(external, "prior"),
    parent.env(environment())$prior,
    parent.env(environment())[["prior", exact = TRUE]],
    rlang::env_get(external, "prior", inherit = TRUE),
    rlang::env_get(external, "prior", inherit = TRUE, last = emptyenv()),
    rlang::env_get(external, "prior", inherit = TRUE, last = baseenv()),
    rlang::env_get(external, "prior", inherit = TRUE, last = globalenv()),
    rlang::env_get(environment(), "prior", inherit = TRUE),
    (function(prior) {
      rlang::env_get(rlang::current_env(), "prior", inherit = TRUE)
    })(7),
    (function(prior) (function() parent.env(base::environment())$prior)())(7),
    rlang::env_get(rlang::current_env(), "prior", default = 7),
    rlang::env_get(
      rlang::current_env(), "prior", default = 7, inherit = TRUE,
      last = rlang::current_env()
    )
  )

  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(source))
    for (read in reads) {
      forward <- summarize_with_margins(
        data, source = sum(x), prior = !!share, observed = !!read,
        .grouping = rollup(g)
      )
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(forward$observed, c(7, 7, 7))

      reverse <- summarize_with_margins(
        data, prior = sum(x), source = !!read, ratio = !!share,
        .grouping = rollup(g)
      )
      expect_equal(reverse$source, c(7, 7, 7))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
})

test_that("literal accessors preserve ordinary caller function lookup", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  # Base fixes the accessor name whose caller binding is under test.
  getElement <- function(...) 7 # nolint: object_name_linter.
  .subset2 <- function(...) 7
  emptyenv <- function() parent.frame()
  reads <- alist(
    getElement(parent.env(environment()), "prior"),
    .subset2(parent.env(environment()), "prior"),
    rlang::env_get(
      rlang::current_env(), "prior", default = 7,
      inherit = TRUE, last = emptyenv()
    )
  )

  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(source))
    for (read in reads) {
      forward <- summarize_with_margins(
        data, source = sum(x), prior = !!share, observed = !!read,
        .grouping = rollup(g)
      )
      expect_equal(forward$observed, c(7, 7, 7))
      expect_equal(forward$prior, c(0.25, 0.75, 1))

      reverse <- summarize_with_margins(
        data, prior = sum(x), source = !!read, ratio = !!share,
        .grouping = rollup(g)
      )
      expect_equal(reverse$source, c(7, 7, 7))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
})
