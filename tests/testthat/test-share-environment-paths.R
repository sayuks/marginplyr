expect_path_dependencies <- function(data) {
  reads <- alist(
    sum(parent.env(environment())$prior),
    sum(parent.env(environment())[["prior", exact = TRUE]]),
    sum(getElement(parent.env(environment()), "prior")),
    (function() {
      sum(get("prior", envir = parent.env(parent.env(environment()))))
    })(),
    (function() sum(parent.env(parent.env(environment()))$prior))(),
    local(sum(parent.env(parent.env(environment()))$prior)),
    sum(rlang::env_parent(environment())$prior),
    sum(rlang::env_get(parent.env(environment()), "prior", inherit = TRUE)),
    sum(rlang::env_get(parent.env(environment()), "prior", inherit = FALSE)),
    sum(rlang::env_get(parent.env(environment()), "prior")),
    sum(rlang::env_get(parent.env(environment()), "prior", inherit = FALSE,
                       last = environment())),
    sum(as.environment(parent.env(environment()))$prior),
    (function() sum(parent.env(parent.frame())$prior))()
  )
  for (kind in c("parent", "total")) {
    for (read in reads) {
      share <- rlang::call2(paste0("share_of_", kind), quote(source))
      expect_error(
        summarize_with_margins(
          data, source = sum(.data$x), prior = !!share, observed = !!read,
          .grouping = rollup("g")
        ),
        "Ordinary summaries cannot use an earlier",
        class = "marginplyr_error"
      )
      share <- rlang::call2(paste0("share_of_", kind), quote(observed))
      expect_error(
        summarize_with_margins(
          data, prior = sum(.data$x), observed = !!read, ratio = !!share,
          .grouping = rollup("g")
        ),
        "depends on earlier summary alias `prior`",
        class = "marginplyr_error"
      )
    }
  }
}

test_that("explicit environment paths preserve eager share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_path_dependencies(data)
  expect_path_dependencies(tibble::as_tibble(data))
})

test_that("explicit environment paths preserve data.table share dependencies", {
  skip_if_suggest_absent("data.table")
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_path_dependencies(data.table::as.data.table(data))
})

expect_independent_paths <- function(data) {
  external <- rlang::new_environment(list(prior = 7), parent = baseenv())
  external_child <- new.env(parent = external)
  reads <- alist(
    (function() sum(external$prior))(),
    sum(rlang::env_parent(external_child)$prior),
    sum(rlang::env_get(external, "prior", inherit = TRUE)),
    sum(as.environment(external)$prior),
    (function(prior) {
      (function() sum(as.environment(parent.env(environment()))$prior))()
    })(7),
    (function(prior) {
      local(sum(rlang::env_parent(environment())$prior))
    })(7),
    (function(prior) {
      (function() {
        sum(rlang::env_get(parent.env(environment()), "prior", inherit = TRUE))
      })()
    })(7)
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

test_that("independent eager environment paths remain valid for both shares", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_independent_paths(data)
  expect_independent_paths(tibble::as_tibble(data))
})

test_that("independent data.table environment paths remain valid for shares", {
  skip_if_suggest_absent("data.table")
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_independent_paths(data.table::as.data.table(data))
})

test_that("environment dependency refusal does not evaluate a lazy input", {
  skip_if_suggest_absent("dtplyr")
  probe <- new.env(parent = emptyenv())
  probe$calls <- 0L
  observe <- function(x) {
    probe$calls <- probe$calls + 1L
    x
  }
  data <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), x = c(1, 3)))
  data <- dplyr::filter(data, observe(x) > 0)
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(source))
    expect_error(
      summarize_with_margins(
        data, source = sum(x), prior = !!share,
        observed = (function() {
          sum(parent.env(parent.env(environment()))$prior)
        })(),
        .grouping = rollup(g)
      ),
      "Ordinary summaries cannot use an earlier", class = "marginplyr_error"
    )
    share <- rlang::call2(paste0("share_of_", kind), quote(observed))
    expect_error(
      summarize_with_margins(
        data, prior = sum(x), observed = (function() {
          sum(parent.env(parent.env(environment()))$prior)
        })(), ratio = !!share, .grouping = rollup(g)
      ),
      "depends on earlier summary alias `prior`", class = "marginplyr_error"
    )
  }
  expect_identical(probe$calls, 0L)
  expect_equal(dplyr::collect(data)$x, c(1, 3))
  expect_identical(probe$calls, 1L)
})
