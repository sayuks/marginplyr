test_that("computed inheritance cannot hide eager share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  reads <- alist(
    sum(rlang::env_get(environment(), "prior", inherit = isTRUE(TRUE)))
  )
  expect_share_dependencies(
    data, reads, "depends on earlier summary alias `prior`"
  )
  expect_share_dependencies(
    tibble::as_tibble(data), reads, "depends on earlier summary alias `prior`"
  )
})

test_that("unproven stop frames cannot hide eager share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  reads <- alist(
    sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                       last = .GlobalEnv)),
    sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                       last = identity(globalenv())))
  )
  expect_share_dependencies(
    data, reads, "depends on earlier summary alias `prior`"
  )
  expect_share_dependencies(
    tibble::as_tibble(data), reads, "depends on earlier summary alias `prior`"
  )
})

expect_replaced_frame_deps <- function(data) {
  stop_frame <- function() parent.frame()
  reads <- alist(
    {
      environment(stop_frame) <- rlang::env(
        parent.frame = function() globalenv()
      )
      sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                         last = stop_frame()))
    },
    {
      body(stop_frame) <- quote(globalenv())
      sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                         last = stop_frame()))
    }
  )
  expect_share_dependencies(
    data, reads, "depends on earlier summary alias `prior`"
  )
}

test_that("replaced caller closures cannot prove eager search bounds", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_replaced_frame_deps(data)
  expect_replaced_frame_deps(tibble::as_tibble(data))
})

test_that("replaced caller closures cannot prove data.table search bounds", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(100, 300)
  )
  before <- data.table::copy(data)
  expect_replaced_frame_deps(data)
  expect_identical(data, before)
})

test_that("unproven search bounds cannot hide data.table dependencies", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(100, 300)
  )
  before <- data.table::copy(data)
  reads <- alist(
    sum(rlang::env_get(environment(), "prior", inherit = isTRUE(TRUE))),
    sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                       last = .GlobalEnv)),
    sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                       last = identity(globalenv())))
  )
  expect_share_dependencies(
    data, reads, "depends on earlier summary alias `prior`"
  )
  expect_identical(data, before)
})

expect_literal_bound_deps <- function(data) {
  reads <- alist(
    sum(rlang::env_get(environment(), "prior", inherit = TRUE)),
    sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                       last = globalenv())),
    sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                       last = emptyenv())),
    sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                       last = baseenv())),
    sum(get("prior", envir = environment(), inherits = isTRUE(TRUE)))
  )
  expect_share_dependencies(
    data, reads, "depends on earlier summary alias `prior`"
  )
}

test_that("known eager search bounds preserve share dependency refusals", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_literal_bound_deps(data)
  expect_literal_bound_deps(tibble::as_tibble(data))
})

test_that("known data.table search bounds preserve dependency refusals", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(100, 300)
  )
  expect_literal_bound_deps(data)
})

expect_independent_bounds <- function(data) {
  stop_frame <- function() parent.frame()
  other_frame <- function() globalenv()
  external <- rlang::new_environment(list(prior = 7), parent = baseenv())
  reads <- alist(
    rlang::env_get(
      rlang::current_env(), "prior", inherit = TRUE,
      last = stop_frame(), default = 7
    ),
    {
      environment(other_frame) <- baseenv()
      rlang::env_get(rlang::current_env(), "prior", inherit = TRUE,
                     last = stop_frame(), default = 7)
    },
    sum(rlang::env_get(external, "prior", inherit = isTRUE(TRUE))),
    sum(rlang::env_get(external, "prior", inherit = isTRUE(TRUE),
                       last = identity(globalenv()))),
    (function(prior) {
      sum(rlang::env_get(environment(), "prior", inherit = FALSE))
    })(7),
    (function(prior) {
      sum(rlang::env_get(environment(), "prior", inherit = isTRUE(FALSE)))
    })(7),
    (function(prior) {
      sum(rlang::env_get(environment(), "prior", inherit = isTRUE(TRUE),
                         last = identity(globalenv())))
    })(7),
    sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                       last = environment(), default = 7)),
    sum(rlang::env_get(environment(), "prior", inherit = isTRUE(TRUE),
                       last = rlang::current_env(), default = 7)),
    (function() {
      sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                         last = parent.env(environment()), default = 7))
    })(),
    sum(rlang::env_get(environment(), "prior", default = 7))
  )
  for (kind in c("parent", "total")) {
    for (read in reads) {
      share <- rlang::call2(paste0("share_of_", kind), quote(source))
      forward <- summarize_with_margins(
        data, source = sum(.data$x), prior = !!share, observed = !!read,
        .grouping = rollup("g")
      )
      expect_equal(forward$observed, c(7, 7, 7))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      share <- rlang::call2(paste0("share_of_", kind), quote(observed))
      reverse <- summarize_with_margins(
        data, prior = sum(.data$x), observed = !!read, ratio = !!share,
        .grouping = rollup("g")
      )
      expect_equal(reverse$observed, c(7, 7, 7))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
}

test_that("independent eager environment searches remain valid for shares", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_independent_bounds(data)
  expect_independent_bounds(tibble::as_tibble(data))
})

test_that("independent data.table environment searches remain valid", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(100, 300)
  )
  before <- data.table::copy(data)
  expect_independent_bounds(data)
  expect_identical(data, before)
})

test_that("search-bound refusal does not evaluate caller option expressions", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  probe <- new.env(parent = emptyenv())
  probe$inherit_reads <- probe$last_reads <- 0L
  probe$replacement_reads <- 0L
  inherit_option <- function() {
    probe$inherit_reads <- probe$inherit_reads + 1L
    TRUE
  }
  last_option <- function() {
    probe$last_reads <- probe$last_reads + 1L
    globalenv()
  }
  replacement_environment <- function() {
    probe$replacement_reads <- probe$replacement_reads + 1L
    rlang::env(parent.frame = last_option)
  }
  shadowed_stop_frame <- local({
    # The base frame reader fixes the binding name under test.
    parent.frame <- last_option # nolint: object_name_linter.
    function() parent.frame()
  })
  rebound_stop_frame <- function() parent.frame()
  reads <- alist(
    sum(rlang::env_get(environment(), "prior", inherit = inherit_option())),
    sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                       last = last_option())),
    sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                       last = shadowed_stop_frame())),
    {
      rebound_stop_frame <- last_option
      sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                         last = rebound_stop_frame()))
    },
    {
      environment(rebound_stop_frame) <- replacement_environment()
      sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                         last = rebound_stop_frame()))
    }
  )
  expect_share_dependencies(
    data, reads, "depends on earlier summary alias `prior`"
  )
  expect_identical(probe$inherit_reads, 0L)
  expect_identical(probe$last_reads, 0L)
  expect_identical(probe$replacement_reads, 0L)
  expect_true(inherit_option())
  expect_identical(last_option(), globalenv())
  expect_identical(replacement_environment()$parent.frame, last_option)
  expect_identical(probe$inherit_reads, 1L)
  expect_identical(probe$last_reads, 1L)
  expect_identical(probe$replacement_reads, 1L)
})
