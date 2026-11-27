# A literal base `assign()` into its own frame binds a local of any value, as
# `<-` does, so a later read of that name reads the local (#1833).
assigned_local_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

assigned_local_reads <- alist(
  {
    assign("prior", 2)
    prior
  },
  {
    base::assign("prior", 2)
    prior
  },
  {
    assign("prior", 2, envir = environment())
    prior
  },
  (function() {
    assign("prior", 2)
    prior
  })(),
  local({
    assign("prior", 2)
    prior
  })
)

test_that("a literal assign() of a value binds a local", {
  expect_share_dependency_values(
    assigned_local_data(), assigned_local_reads, 2
  )
  expect_share_dependency_values(
    tibble::as_tibble(assigned_local_data()), assigned_local_reads, 2
  )
  result <- summarize_with_margins(
    assigned_local_data(), prior = sum(x), value = {
      assign("prior", sum(x))
      prior
    }, ratio = share_of_total(value), .grouping = rollup("g")
  )
  expect_equal(result$value, c(1, 3, 4))
})

test_that("a literal assign() of a value binds a data.table local", {
  skip_if_suggest_absent("data.table")
  expect_share_dependency_values(
    data.table::as.data.table(assigned_local_data()), assigned_local_reads, 2
  )
})

test_that("assignments that do not definitely bind the local stay refused", {
  reads <- alist(
    {
      assign("prior", prior)
      prior
    },
    {
      assign("prior", prior + 1)
      prior
    },
    {
      if (sum(x) > 2) assign("prior", 2)
      prior
    },
    {
      f <- function() assign("prior", 2)
      prior
    },
    {
      assign("prior", 2)
      rm(prior)
      prior
    },
    {
      assign("prior", 2)
      rlang::local_bindings(prior = rlang::zap())
      prior
    },
    {
      assign("prior", 2, envir = new.env())
      prior
    },
    {
      assign("prior", 2, envir = parent.frame())
      prior
    },
    # Refused before it runs, so the global environment is not written.
    {
      assign("prior", 2, envir = globalenv())
      prior
    },
    {
      assign("other", 2)
      prior
    },
    {
      assign(paste0("pri", "or"), 2)
      prior
    },
    {
      assign <- function(...) NULL
      assign("prior", 2)
      prior
    },
    {
      y <- prior
      assign("prior", 2)
      y
    },
    # The walk does not model when a promise or an active binding runs, and
    # the other binders need a placement proof of their own.
    {
      delayedAssign("prior", 2)
      prior
    },
    {
      makeActiveBinding("prior", function() 2, environment())
      prior
    },
    {
      list2env(list(prior = 2), environment())
      prior
    },
    {
      rlang::env_bind(environment(), prior = 2)
      prior
    },
    {
      rlang::env_poke(environment(), "prior", 2)
      prior
    },
    {
      rlang::local_bindings(prior = 2)
      prior
    }
  )
  expect_share_dependencies(
    assigned_local_data(), reads, "depends on earlier summary alias `prior`"
  )
})
