expect_bulk_share_refusals <- function(data, reads) {
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

bulk_mask_reads <- function() {
  alist(
    sum(rlang::env_get_list(environment(), "prior", inherit = TRUE)$prior),
    sum(rlang::env_get_list(environment(), c("prior", "x"),
                            inherit = TRUE)[["prior"]]),
    sum(rlang::env_get_list(parent.env(environment()), "prior")$prior),
    sum(rlang::env_get_list(environment(), "prior",
                            inherit = isTRUE(TRUE))$prior),
    sum(rlang::env_get_list(environment(), "prior", inherit = TRUE,
                            last = identity(globalenv()))$prior),
    sum(rlang::env_get(environment(), "prior", inherit = TRUE)),
    sum(mget("prior", envir = environment(), inherits = TRUE)$prior)
  )
}

test_that("bulk environment reads retain eager share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  reads <- bulk_mask_reads()
  expect_bulk_share_refusals(data, reads)
  expect_bulk_share_refusals(tibble::as_tibble(data), reads)
})

test_that("environment materialization retains eager share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  reads <- alist(
    sum(as.list(parent.env(environment()))$prior),
    sum(base::as.list(parent.env(environment()))[["prior"]])
  )
  expect_bulk_share_refusals(data, reads)
  expect_bulk_share_refusals(tibble::as_tibble(data), reads)
})

test_that("bulk and materialized mask reads retain data.table dependencies", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(g = c("a", "b"), x = c(1, 3),
                                 prior = c(100, 300))
  reads <- c(bulk_mask_reads(), alist(
    sum(as.list(parent.env(environment()))$prior),
    sum(as.list(parent.env(environment()))[["prior"]])
  ))
  expect_bulk_share_refusals(data, reads)
})

expect_independent_bulk_reads <- function(data) {
  external <- rlang::env(prior = 7)
  reads <- alist(
    sum(rlang::env_get_list(external, "prior", inherit = TRUE)$prior),
    sum(as.list(external)[["prior"]]),
    (function(prior) {
      sum(rlang::env_get_list(environment(), "prior", inherit = TRUE)$prior)
    })(7),
    (function(prior) sum(as.list(environment())$prior))(7)
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

test_that("independent eager bulk and materialized reads remain valid", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_independent_bulk_reads(data)
  expect_independent_bulk_reads(tibble::as_tibble(data))
})

test_that("independent data.table bulk and materialized reads remain valid", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(g = c("a", "b"), x = c(1, 3),
                                 prior = c(100, 300))
  expect_independent_bulk_reads(data)
})

test_that("bulk refusal does not evaluate the caller's binding names", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  calls <- 0L
  binding_names <- function() {
    calls <<- calls + 1L
    "prior"
  }
  read <- quote(sum(rlang::env_get_list(environment(), binding_names(),
                                        inherit = TRUE)$prior))
  expect_bulk_share_refusals(data, list(read))
  expect_identical(calls, 0L)
  expect_identical(binding_names(), "prior")
  expect_identical(calls, 1L)
})

test_that("bulk and materialized reads preserve caller function lookup", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expressions <- alist(
    sum(rlang::env_get_list(environment(), "prior", inherit = TRUE)$prior),
    sum(base::as.list(parent.env(environment()))$prior),
    {
      # This binding uses the base API name to exercise function lookup.
      as.list <- function(...) list(prior = 7) # nolint: object_name_linter.
      sum(as.list(parent.env(environment()))$prior)
    }
  )
  for (expr in expressions) {
    calls <- 0L
    dispatcher <- function(package, member) {
      package <- as.character(substitute(package))
      member <- as.character(substitute(member))
      if ((identical(package, "rlang") && identical(member, "env_get_list")) ||
            (identical(package, "base") && identical(member, "as.list"))) {
        calls <<- calls + 1L
        return(function(...) list(prior = 7))
      }
      get(member, envir = asNamespace(package), inherits = FALSE)
    }
    caller <- rlang::env()
    caller[["::"]] <- dispatcher
    read <- rlang::new_quosure(expr, caller)
    for (kind in c("parent", "total")) {
      share <- rlang::call2(paste0("share_of_", kind), quote(source))
      calls <- 0L
      expect_error(
        summarize_with_margins(
          data, source = sum(x), prior = !!share, observed = !!read,
          invalid = share_of_total(absent), .grouping = rollup(g)
        ),
        "unknown preceding ordinary summary `absent`",
        class = "marginplyr_error"
      )
      expect_identical(calls, 0L)
      forward <- summarize_with_margins(
        data, source = sum(x), prior = !!share, observed = !!read,
        .grouping = rollup(g)
      )
      expect_equal(forward$observed, c(7, 7, 7))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      share <- rlang::call2(paste0("share_of_", kind), quote(observed))
      reverse <- summarize_with_margins(
        data, prior = sum(x), observed = !!read, ratio = !!share,
        .grouping = rollup(g)
      )
      expect_equal(reverse$observed, c(7, 7, 7))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
})
