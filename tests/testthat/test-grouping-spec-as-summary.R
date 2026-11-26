# A Grouping specification written into `...` instead of `.grouping` (#1699).
#
# dplyr evaluates it as a summary and refuses it as a non-vector, without
# naming `.grouping`, while a lazy input translates the constructor into SQL
# and returns a query. Both readings are replaced by one refusal raised before
# any backend work.

spec_summary_data <- function() {
  data.frame(g = c("a", "a", "b"), v = c(1, 2, 3))
}

spec_summary_refusal <- function(...) {
  rlang::catch_cnd(
    summarize_with_margins(spec_summary_data(), ...),
    classes = "marginplyr_error"
  )
}

test_that("a specification in `...` is refused with the `.grouping` rewrite", {
  unnamed <- spec_summary_refusal(s = sum(v), rollup(g))
  expect_s3_class(unnamed, "marginplyr_error")
  expect_match(
    conditionMessage(unnamed),
    "Did you mean `.grouping = rollup(g)`?",
    fixed = TRUE
  )

  # The name a caller writes when they drop the dot reaches `...` like any
  # other summary name.
  named <- spec_summary_refusal(s = sum(v), grouping = rollup(g))
  expect_s3_class(named, "marginplyr_error")
  expect_match(
    conditionMessage(named),
    "Did you mean `.grouping = rollup(g)`?",
    fixed = TRUE
  )
})

test_that("every constructor spelling resolving to marginplyr is refused", {
  spec <- grouping_sets(g, NULL)
  cases <- list(
    list(dot = quote(marginplyr::cube(g)),
         rewrite = ".grouping = marginplyr::cube(g)"),
    list(dot = quote(grouping_spec(rollup(g))),
         rewrite = ".grouping = grouping_spec(rollup(g))"),
    list(dot = quote(spec), rewrite = ".grouping = spec")
  )
  for (case in cases) {
    refusal <- spec_summary_refusal(s = sum(v), !!case$dot)
    expect_s3_class(refusal, "marginplyr_error")
    expect_match(
      conditionMessage(refusal),
      paste0("Did you mean `", case$rewrite, "`?"),
      fixed = TRUE
    )
  }
})

test_that("a summary that only shares a constructor's name is computed", {
  # A caller's own `rollup()` is ordinary lookup (ADR 0019), and a column named
  # like a specification-bound local is what the data mask reads.
  rollup <- function(x) sum(x)
  spec <- marginplyr::rollup(g)
  data <- transform(spec_summary_data(), spec = v * 10)

  result <- summarize_with_margins(
    data,
    s = rollup(v),
    t = max(spec),
    .grouping = marginplyr::rollup(g)
  )
  expect_identical(result$s, c(3, 3, 6))
  expect_identical(result$t, c(20, 30, 30))

  one_row_per_group <- data.frame(g = c("a", "b"), spec = c(1, 2))
  expect_identical(
    summarize_with_margins(
      one_row_per_group,
      spec,
      .grouping = marginplyr::grouping_set(g)
    )$spec,
    c(1, 2)
  )
})

test_that("a name whose binding fails is left to the data mask", {
  delayedAssign("broken", stop("caller's own failure"))
  expect_error(
    summarize_with_margins(
      spec_summary_data(),
      s = broken,
      .grouping = rollup(g)
    ),
    "caller's own failure"
  )
})

test_that("the refusal is raised on a lazy input before any query", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  source <- dplyr::copy_to(con, spec_summary_data(), "spec_summary")
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE)

  lazy <- rlang::catch_cnd(
    summarize_with_margins(source, s = sum(v), rollup(g)),
    classes = "marginplyr_error"
  )
  expect_identical(
    conditionMessage(lazy),
    conditionMessage(spec_summary_refusal(s = sum(v), rollup(g)))
  )
  expect_identical(counter$queries, 0L)
  expect_identical(last_sent_queries()$purpose, character())
})

# The data mask reads a strictly earlier summary before the caller's
# environment, so on a local input a bare name an earlier named summary carries
# is that summary, whatever the environment binds (#1778).
for (input_kind in c("data.frame", "tibble", "data.table")) {
  test_that(paste("an earlier summary's name is computed on", input_kind), {
    if (input_kind == "data.table") skip_if_suggest_absent("data.table")
    data <- spec_summary_data()
    data <- switch(input_kind,
      tibble = tibble::as_tibble(data),
      data.table = data.table::as.data.table(data), data
    )
    spec <- rollup(g)

    result <- summarize_with_margins(
      data,
      spec = dplyr::n(),
      again = spec,
      .grouping = spec
    )
    expect_identical(result$spec, c(2L, 1L, 3L))
    expect_identical(result$again, c(2L, 1L, 3L))

    # Written before the summary of that name exists, the mask reads the
    # environment's specification.
    before <- rlang::catch_cnd(
      summarize_with_margins(
        data,
        again = spec,
        spec = dplyr::n(),
        .grouping = spec
      ),
      classes = "marginplyr_error"
    )
    expect_match(
      conditionMessage(before),
      "Did you mean `.grouping = spec`?",
      fixed = TRUE
    )

    # A summary written as `NULL` produces no column, so the mask again reads
    # the environment's specification.
    null <- rlang::catch_cnd(
      summarize_with_margins(
        data,
        spec = NULL,
        again = spec,
        .grouping = spec
      ),
      classes = "marginplyr_error"
    )
    expect_match(
      conditionMessage(null),
      "Did you mean `.grouping = spec`?",
      fixed = TRUE
    )
  })
}

test_that("an earlier summary's name is still refused on a lazy input", {
  # dbplyr cannot reuse an alias of the same `summarise()`, so on a lazy input
  # the name reaches the environment's specification (#1778).
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(con, spec_summary_data(), "spec_summary_alias")
  spec <- rollup(g)

  lazy <- rlang::catch_cnd(
    summarize_with_margins(
      source,
      spec = dplyr::n(),
      again = spec,
      .grouping = spec
    ),
    classes = "marginplyr_error"
  )
  expect_match(
    conditionMessage(lazy),
    "Did you mean `.grouping = spec`?",
    fixed = TRUE
  )
})

# On a local input a caller's unevaluated argument and an active binding are
# left to the data mask, so the check never runs caller code (#1790). The
# reference is the same Margin call with the name parenthesised, which the
# check does not read.
for (input_kind in c("data.frame", "tibble", "data.table")) {
  test_that(paste("a bare-name summary is read in order on", input_kind), {
    if (input_kind == "data.table") skip_if_suggest_absent("data.table")
    data <- spec_summary_data()
    data <- switch(input_kind,
      tibble = tibble::as_tibble(data),
      data.table = data.table::as.data.table(data), data
    )
    seen <- new.env()
    seen$trace <- character()
    note <- function(label, value) {
      seen$trace <- c(seen$trace, label)
      value
    }
    bare <- function(w) {
      summarize_with_margins(data, s = note("a", sum(v)), k = w,
                             .grouping = rollup(g))
    }
    wrapped <- function(w) {
      summarize_with_margins(data, s = note("a", sum(v)), k = (w),
                             .grouping = rollup(g))
    }

    bare(note("w", 1))
    bare_trace <- seen$trace
    seen$trace <- character()
    wrapped(note("w", 1))
    expect_identical(bare_trace, seen$trace)
    expect_identical(bare_trace[[1L]], "a")

    # A failing argument runs once, and its warning keeps dplyr's context.
    seen$trace <- character()
    expect_error(bare(note("f", stop("caller's own failure"))),
                 "caller's own failure")
    expect_identical(sum(seen$trace == "f"), 1L)
    warned <- rlang::catch_cnd(
      bare({
        warning("caller's own warning")
        1
      }),
      classes = "warning"
    )
    expect_match(conditionMessage(warned), "In argument: `k = w`", fixed = TRUE)
    expect_match(conditionMessage(warned), "caller's own warning", fixed = TRUE)

    # An unbound name is the data mask's to report, as for the reference.
    unbound <- function(k) {
      rlang::catch_cnd(summarize_with_margins(data, k = !!k,
                                              .grouping = rollup(g)), "error")
    }
    expect_false(
      binding_is_evaluated(environment(), "spec_summary_unbound_name")
    )
    absent <- unbound(quote(spec_summary_unbound_name))
    expect_false(inherits(absent, "marginplyr_error"))
    expect_identical(class(absent),
                     class(unbound(quote((spec_summary_unbound_name)))))

    # A refused call evaluates nothing the caller passed.
    seen$trace <- character()
    refused <- function(w) {
      summarize_with_margins(data, k = w, rollup(g), .grouping = rollup(g))
    }
    expect_error(refused(note("w", 1)), class = "marginplyr_error")
    expect_identical(seen$trace, character())

    # An active binding is invoked as often as the reference call invokes it.
    seen$calls <- 0L
    makeActiveBinding("live", function() {
      seen$calls <- seen$calls + 1L
      1
    }, environment())
    summarize_with_margins(data, k = live, .grouping = rollup(g))
    bare_calls <- seen$calls
    seen$calls <- 0L
    summarize_with_margins(data, k = (live), .grouping = rollup(g))
    expect_identical(bare_calls, seen$calls)
  })
}

test_that("a forced argument holding a specification is still refused", {
  forced <- function(data, sp) {
    force(sp)
    summarize_with_margins(data, s = sum(v), sp, .grouping = rollup(g))
  }
  refusal <- rlang::catch_cnd(
    forced(spec_summary_data(), rollup(g)),
    classes = "marginplyr_error"
  )
  expect_match(
    conditionMessage(refusal),
    "Did you mean `.grouping = sp`?",
    fixed = TRUE
  )
})

test_that("a lazy input reads an argument once and refuses a specification", {
  # The backend evaluates the name while the query is built in any case, so a
  # lazy input keeps the read, and what it raises leaves the call (#1790).
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(con, spec_summary_data(), "spec_summary_promise")
  wrapper <- function(data, w) {
    summarize_with_margins(data, s = sum(v), k = w, .grouping = rollup(g))
  }

  seen <- new.env()
  seen$runs <- 0L
  failing <- function() {
    seen$runs <- seen$runs + 1L
    stop("caller's own failure")
  }
  expect_error(wrapper(source, failing()), "caller's own failure")
  expect_identical(seen$runs, 1L)

  refusal <- rlang::catch_cnd(
    wrapper(source, rollup(g)),
    classes = "marginplyr_error"
  )
  expect_match(
    conditionMessage(refusal),
    "Did you mean `.grouping = w`?",
    fixed = TRUE
  )
})
