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

# Each reads 7 and no earlier summary; quosures keep `external` in reach.
unplaced_control_reads <- function() {
  external <- rlang::new_environment(list(prior = 7), parent = baseenv())
  rlang::quos(
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
}

test_that("non-inheriting and external environment reads remain valid", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  expect_share_dependency_values(data, unplaced_control_reads(), 7)
})

test_that("non-inheriting and external data.table reads remain valid", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(100, 300)
  )
  expect_share_dependency_values(data, unplaced_control_reads(), 7)
})
