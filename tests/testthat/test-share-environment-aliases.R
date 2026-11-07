environment_alias_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

environment_alias_reads <- function() {
  alist(
    {
      mask <- {
        parent.env(environment())
      }
      sum(mask$prior)
    },
    {
      mask <- if (TRUE) parent.env(environment()) else stop("Unselected arm")
      sum(mask$prior)
    },
    {
      mask <- switch(2L, stop("Unselected arm"), parent.env(environment()))
      sum(mask$prior)
    },
    {
      mask <- identity(parent.env(environment()))
      sum(mask$prior)
    },
    {
      mask <- base::identity(x = parent.env(environment()))
      sum(mask$prior)
    },
    {
      mask <- parent.env(environment())
      sum(mask$prior)
    },
    {
      mask <- parent.env(environment())
      sum(mask[["prior"]])
    },
    {
      mask <- parent.env(environment())
      sum(getElement(mask, "prior"))
    },
    {
      mask <- parent.env(environment())
      sum(.subset2(mask, "prior"))
    },
    {
      mask <- parent.env(environment())
      sum(rlang::env_get(mask, "prior"))
    },
    {
      frame <- environment()
      sum(rlang::env_get(frame, "prior", inherit = TRUE))
    },
    {
      mask <- parent.env(environment())
      sum(rlang::env_get_list(mask, "prior")$prior)
    },
    {
      frame <- environment()
      sum(rlang::env_get_list(frame, "prior", inherit = TRUE)$prior)
    },
    {
      mask <- parent.env(environment())
      sum(as.list(mask)$prior)
    },
    {
      frame <- environment()
      sum(parent.env(frame)$prior)
    },
    {
      mask <- parent.env(environment())
      copied <- mask
      sum(copied$prior)
    },
    {
      mask <- parent.env(environment())
      (function() sum(mask$prior))()
    }
  )
}

expect_environment_aliases <- function(data) {
  before <- serialize(data, NULL)
  expect_share_dependencies(data, environment_alias_reads())
  expect_share_dependencies(data, alist(
    sum(parent.env(environment())$prior),
    sum(parent.env(environment())[["prior"]]),
    sum(rlang::env_get(parent.env(environment()), "prior")),
    sum(rlang::env_get_list(parent.env(environment()), "prior")$prior),
    sum(as.list(parent.env(environment()))$prior),
    {
      mask <- parent.env(environment())
      sum(get("prior", envir = mask, inherits = FALSE))
    }
  ))
  external <- rlang::new_environment(list(prior = 7), parent = emptyenv())
  replace <- FALSE
  reads <- alist(
    {
      mask <- {
        external
      }
      sum(mask$prior)
    },
    {
      mask <- if (TRUE) external else stop("Unselected arm")
      sum(mask$prior)
    },
    {
      mask <- switch(2L, stop("Unselected arm"), external)
      sum(mask$prior)
    },
    {
      mask <- identity(external)
      sum(mask$prior)
    },
    {
      mask <- base::identity(x = external)
      sum(mask$prior)
    },
    {
      mask <- external
      sum(mask$prior)
    },
    {
      mask <- external
      sum(mask[["prior"]])
    },
    {
      mask <- external
      sum(getElement(mask, "prior"))
    },
    {
      mask <- external
      sum(.subset2(mask, "prior"))
    },
    {
      mask <- external
      sum(rlang::env_get(mask, "prior"))
    },
    {
      mask <- external
      sum(rlang::env_get_list(mask, "prior")$prior)
    },
    {
      mask <- external
      sum(as.list(mask)$prior)
    },
    {
      mask <- parent.env(environment())
      mask <- external
      sum(mask$prior)
    },
    (function(prior) {
      frame <- environment()
      sum(rlang::env_get(frame, "prior", inherit = TRUE))
    })(7),
    (function(prior) {
      frame <- environment()
      sum(rlang::env_get_list(frame, "prior", inherit = TRUE)$prior)
    })(7),
    (function(prior) {
      frame <- environment()
      (function() sum(rlang::env_get(frame, "prior", inherit = TRUE)))()
    })(7),
    (function(prior) {
      frame <- environment()
      if (replace) frame <- external
      sum(rlang::env_get(frame, "prior", inherit = TRUE))
    })(7)
  )
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (read in reads) {
      forward <- summarize_with_margins(
        data, value = sum(.data$x), prior = !!helper, observed = !!read,
        .grouping = rollup("g")
      )
      reverse <- summarize_with_margins(
        data, prior = sum(.data$x), value = !!read, ratio = !!helper,
        .grouping = rollup("g")
      )
      expect_equal(forward$observed, c(7, 7, 7))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$value, c(7, 7, 7))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
  expect_identical(serialize(data, NULL), before)
}

test_that("saved eager mask environments retain share dependencies", {
  expect_environment_aliases(environment_alias_data())
  expect_environment_aliases(tibble::as_tibble(environment_alias_data()))
})

test_that("saved data.table mask environments retain share dependencies", {
  skip_if_suggest_absent("data.table")
  data <- data.table::as.data.table(environment_alias_data())
  expect_environment_aliases(data)
})

test_that("saved environment analysis never invokes caller code", {
  probe <- rlang::env(calls = 0L)
  observe <- function(x) {
    probe$calls <- probe$calls + 1L
    x
  }
  reads <- lapply(environment_alias_reads(), function(read) {
    rlang::call2("observe", read)
  })
  expect_share_dependencies(environment_alias_data(), reads)
  expect_identical(probe$calls, 0L)
})

test_that("conditional alias writes retain possible mask dependencies", {
  external <- rlang::new_environment(list(prior = 7), parent = emptyenv())
  replace <- FALSE
  expect_share_dependencies(environment_alias_data(), alist(
    {
      mask <- parent.env(environment())
      if (replace) mask <- external
      sum(mask$prior)
    },
    {
      mask <- parent.env(environment())
      if (replace) mask <- external
      sum(rlang::env_get(mask, "prior"))
    },
    {
      mask <- external
      if (!replace) mask <- parent.env(environment())
      sum(mask$prior)
    },
    {
      mask <- parent.env(environment())
      if (replace) mask <- external
      delayedAssign("unused", 1)
      sum(mask$prior)
    },
    (function(prior) {
      mask <- environment()
      if (!replace) mask <- parent.env(parent.env(environment()))
      sum(rlang::env_get(mask, "prior", inherit = TRUE))
    })(7)
  ))
})

test_that("environment-valued blocks do not execute planning operands", {
  probe <- rlang::env(calls = 0L)
  observe <- function() {
    probe$calls <- probe$calls + 1L
  }
  expect_share_dependencies(environment_alias_data(), alist({
    mask <- base::identity({
      observe()
      saved <- parent.env(environment())
      saved
    })
    sum(mask$prior)
  }))
  expect_identical(probe$calls, 0L)
})

test_that("caller identity bindings retain independent environment values", {
  external <- rlang::new_environment(list(prior = 7), parent = emptyenv())
  identity <- function(x) external
  data <- environment_alias_data()
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    read <- quote({
      mask <- identity(parent.env(environment()))
      sum(mask$prior)
    })
    forward <- summarize_with_margins(
      data, value = sum(.data$x), prior = !!helper, observed = !!read,
      .grouping = rollup("g")
    )
    reverse <- summarize_with_margins(
      data, prior = sum(.data$x), value = !!read, ratio = !!helper,
      .grouping = rollup("g")
    )
    expect_equal(forward$observed, c(7, 7, 7))
    expect_equal(reverse$value, c(7, 7, 7))
  }
})
