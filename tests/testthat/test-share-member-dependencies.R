expect_mask_member_reads <- function(data) {
  field <- "prior"
  probe <- new.env(parent = emptyenv())
  probe$key_reads <- 0L
  binding_name <- function() {
    probe$key_reads <- probe$key_reads + 1L
    "prior"
  }
  reads <- rlang::exprs(
    sum(parent.env(environment())[[paste0("pri", "or")]]),
    sum(parent.env(environment())[[field]]),
    {
      key <- "prior"
      sum(parent.env(environment())[[key]])
    },
    sum(parent.env(environment())[[paste0("pri", "or"), exact = TRUE]]),
    sum(parent.env(environment())[[field, exact = FALSE]]),
    sum(getElement(parent.env(environment()), paste0("pri", "or"))),
    sum(getElement(parent.env(environment()), field)),
    {
      key <- "prior"
      sum(getElement(parent.env(environment()), key))
    },
    sum(.subset2(parent.env(environment()), paste0("pri", "or"))),
    sum(.subset2(parent.env(environment()), field)),
    {
      key <- "prior"
      sum(.subset2(parent.env(environment()), key))
    },
    sum(parent.env(environment())[[binding_name()]]),
    sum(parent.env(environment())[["prior"]]),
    sum(get(paste0("pri", "or"), envir = parent.env(environment()))),
    sum(rlang::env_get(environment(), paste0("pri", "or"), inherit = TRUE))
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
  expect_identical(probe$key_reads, 0L)
  expect_identical(binding_name(), "prior")
  expect_identical(probe$key_reads, 1L)
}

test_that("computed mask members preserve eager share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_mask_member_reads(data)
  expect_mask_member_reads(tibble::as_tibble(data))
})

test_that("computed mask members preserve data.table share dependencies", {
  skip_if_suggest_absent("data.table")
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_mask_member_reads(data.table::as.data.table(data))
})

expect_external_member_reads <- function(data) {
  field <- "prior"
  external <- rlang::new_environment(list(prior = 7), parent = baseenv())
  reads <- rlang::exprs(
    sum(external[[paste0("pri", "or")]]),
    sum(external[[field]]),
    {
      key <- "prior"
      sum(external[[key]])
    },
    sum(getElement(external, paste0("pri", "or"))),
    sum(getElement(external, field)),
    {
      key <- "prior"
      sum(getElement(external, key))
    },
    sum(.subset2(external, paste0("pri", "or"))),
    sum(.subset2(external, field)),
    {
      key <- "prior"
      sum(.subset2(external, key))
    }
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

test_that("independent eager environment members remain valid", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_external_member_reads(data)
  expect_external_member_reads(tibble::as_tibble(data))
})

test_that("independent data.table environment members remain valid", {
  skip_if_suggest_absent("data.table")
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_external_member_reads(data.table::as.data.table(data))
})
