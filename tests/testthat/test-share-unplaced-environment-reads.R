# An inheriting rlang lookup on an environment the walk cannot place may reach
# the mask through its parents, as `get()` on the same environment may (#1829).
unplaced_inheriting_reads <- alist(
  sum(rlang::env_get(new.env(), "prior", inherit = TRUE)),
  sum(rlang::env_get(rlang::env(), "prior", inherit = TRUE)),
  sum(rlang::env_get(rlang::child_env(environment()), "prior", inherit = TRUE)),
  sum(rlang::env_get(
    rlang::new_environment(parent = environment()), "prior", inherit = TRUE
  )),
  sum(rlang::env_get(environment(NULL), "prior", inherit = TRUE)),
  sum(rlang::env_get(rlang::caller_env(0), "prior", inherit = TRUE)),
  {
    e <- new.env()
    sum(rlang::env_get(e, "prior", inherit = TRUE))
  },
  sum(rlang::env_get(local(environment()), "prior", inherit = TRUE)),
  local(sum(rlang::env_get(parent.frame(2), "prior", inherit = TRUE))),
  sum(rlang::env_get_list(rlang::env(), "prior", inherit = TRUE)[[1L]])
)

test_that("inheriting reads of unplaced environments keep share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_share_dependencies(
    data, unplaced_inheriting_reads, "depends on earlier summary alias `prior`"
  )
  expect_share_dependencies(
    tibble::as_tibble(data), unplaced_inheriting_reads,
    "depends on earlier summary alias `prior`"
  )
})

test_that("an inheriting read of a caller-built environment is refused", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  external <- rlang::new_environment(list(prior = 7), parent = emptyenv())
  # A caller function named like base's; the walk does not read what it
  # returns, so its result is unplaced, as it is for `get()`.
  environment <- function() new.env(parent = external)
  expect_share_dependencies(
    data, alist(rlang::env_get(environment(), "prior", inherit = TRUE)),
    "depends on earlier summary alias `prior`"
  )
})

test_that("inheriting unplaced data.table reads keep share refusals", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(100, 300)
  )
  expect_share_dependencies(
    data, unplaced_inheriting_reads, "depends on earlier summary alias `prior`"
  )
})

expect_unplaced_controls <- function(data) {
  external <- rlang::new_environment(list(prior = 7), parent = baseenv())
  reads <- alist(
    sum(rlang::env_get(new.env(), "prior", default = 7)),
    {
      e <- new.env()
      e$prior <- 7
      sum(rlang::env_get(e, "prior"))
    },
    sum(rlang::env_get(new.env(), "prior", inherit = FALSE, default = 7)),
    sum(rlang::env_get(external, "prior", inherit = TRUE)),
    sum(rlang::env_get_list(external, "prior", inherit = TRUE)[[1L]])
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

test_that("non-inheriting and external environment reads remain valid", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_unplaced_controls(data)
})

test_that("non-inheriting and external data.table reads remain valid", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(100, 300)
  )
  expect_unplaced_controls(data)
})
