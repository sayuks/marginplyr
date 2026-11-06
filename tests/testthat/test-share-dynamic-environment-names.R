expect_dynamic_dependencies <- function(data) {
  .env <- rlang::.env
  field <- "prior"
  reads <- rlang::exprs(
    sum(rlang::env_get(environment(), paste0("pri", "or"), inherit = TRUE)),
    sum(rlang::env_get(environment(), field, inherit = TRUE)),
    sum(rlang::env_get(environment(), .env$field, inherit = TRUE)),
    {
      field <- "prior"
      sum(rlang::env_get(environment(), field, inherit = TRUE))
    },
    sum(rlang::env_get(rlang::current_env(), field, inherit = TRUE)),
    sum(rlang::env_get(parent.env(environment()), field)),
    sum(rlang::env_get(parent.env(environment()), field, inherit = FALSE)),
    sum(rlang::env_get(environment(), field, inherit = TRUE,
                       last = emptyenv())),
    sum(rlang::env_get(environment(), "prior", inherit = TRUE)),
    sum(base::get(paste0("pri", "or"), envir = environment()))
  )
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
        "depends on earlier summary alias", class = "marginplyr_error"
      )
    }
  }
}

test_that("dynamic environment names cannot hide eager share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_dynamic_dependencies(data)
  expect_dynamic_dependencies(tibble::as_tibble(data))
})

test_that("dynamic environment names cannot hide data.table dependencies", {
  skip_if_suggest_absent("data.table")
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_dynamic_dependencies(data.table::as.data.table(data))
})

expect_independent_env_reads <- function(data) {
  field <- "prior"
  external <- rlang::new_environment(list(prior = 7), parent = baseenv())
  reads <- rlang::exprs(
    sum(rlang::env_get(external, field, inherit = TRUE)),
    sum(rlang::env_get(external, paste0("pri", "or"), inherit = TRUE)),
    (function(prior) {
      sum(rlang::env_get(environment(), field, inherit = FALSE))
    })(7),
    (function(prior) {
      field <- "prior"
      sum(rlang::env_get(environment(), field))
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

test_that("independent eager dynamic environment reads remain valid", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_independent_env_reads(data)
  expect_independent_env_reads(tibble::as_tibble(data))
})

test_that("independent data.table dynamic environment reads remain valid", {
  skip_if_suggest_absent("data.table")
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_independent_env_reads(data.table::as.data.table(data))
})

test_that("dynamic lookup refusal evaluates neither names nor lazy input", {
  skip_if_suggest_absent("dtplyr")
  probe <- new.env(parent = emptyenv())
  probe$input_reads <- probe$name_reads <- 0L
  observe <- function(x) {
    probe$input_reads <- probe$input_reads + 1L
    x
  }
  binding_name <- function() {
    probe$name_reads <- probe$name_reads + 1L
    "prior"
  }
  data <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), x = c(1, 3)))
  data <- dplyr::filter(data, observe(x) > 0)
  read <- quote(sum(rlang::env_get(environment(), binding_name(),
                                   inherit = TRUE)))
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(source))
    expect_error(
      summarize_with_margins(
        data, source = sum(x), prior = !!share, observed = !!read,
        .grouping = rollup(g)
      ),
      "Ordinary summaries cannot use an earlier", class = "marginplyr_error"
    )
    share <- rlang::call2(paste0("share_of_", kind), quote(observed))
    expect_error(
      summarize_with_margins(
        data, prior = sum(x), observed = !!read, ratio = !!share,
        .grouping = rollup(g)
      ),
      "depends on earlier summary alias", class = "marginplyr_error"
    )
  }
  expect_identical(probe$input_reads, 0L)
  expect_identical(probe$name_reads, 0L)
  expect_equal(dplyr::collect(data)$x, c(1, 3))
  expect_identical(probe$input_reads, 1L)
  expect_identical(binding_name(), "prior")
  expect_identical(probe$name_reads, 1L)
})
