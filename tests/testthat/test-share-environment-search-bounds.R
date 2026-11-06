expect_bound_dependencies <- function(data, reads) {
  for (kind in c("parent", "total")) {
    for (read in reads) {
      share <- rlang::call2(paste0("share_of_", kind), quote(source))
      expect_error(
        summarize_with_margins(
          data, source = sum(.data$x), prior = !!share, observed = !!read,
          .grouping = rollup("g")
        ),
        "Ordinary summaries cannot use an earlier", class = "marginplyr_error"
      )
      share <- rlang::call2(paste0("share_of_", kind), quote(observed))
      expect_error(
        summarize_with_margins(
          data, prior = sum(.data$x), observed = !!read, ratio = !!share,
          .grouping = rollup("g")
        ),
        "depends on earlier summary alias `prior`", class = "marginplyr_error"
      )
    }
  }
}

test_that("computed inheritance cannot hide eager share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  reads <- alist(
    sum(rlang::env_get(environment(), "prior", inherit = isTRUE(TRUE)))
  )
  expect_bound_dependencies(data, reads)
  expect_bound_dependencies(tibble::as_tibble(data), reads)
})

test_that("unproven stop frames cannot hide eager share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  reads <- alist(
    sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                       last = .GlobalEnv)),
    sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                       last = identity(globalenv())))
  )
  expect_bound_dependencies(data, reads)
  expect_bound_dependencies(tibble::as_tibble(data), reads)
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
  expect_bound_dependencies(data, reads)
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
  expect_bound_dependencies(data, reads)
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
  external <- rlang::new_environment(list(prior = 7), parent = baseenv())
  reads <- alist(
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
  inherit_option <- function() {
    probe$inherit_reads <- probe$inherit_reads + 1L
    TRUE
  }
  last_option <- function() {
    probe$last_reads <- probe$last_reads + 1L
    globalenv()
  }
  reads <- alist(
    sum(rlang::env_get(environment(), "prior", inherit = inherit_option())),
    sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                       last = last_option()))
  )
  expect_bound_dependencies(data, reads)
  expect_identical(probe$inherit_reads, 0L)
  expect_identical(probe$last_reads, 0L)
  expect_true(inherit_option())
  expect_identical(last_option(), globalenv())
  expect_identical(probe$inherit_reads, 1L)
  expect_identical(probe$last_reads, 1L)
})
