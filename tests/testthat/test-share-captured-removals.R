# Removing a name a function literal captured reads that name (ADR 0010).

captured_removal_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
}

test_that("removing a captured alias reads it whatever value is called", {
  call_value <- local(function(x) function() 7, envir = new.env())
  rebind <- function(...) get("x", envir = parent.frame())
  # Only the planning walk sees these: each refusal precedes evaluation, so
  # the closure environments above are never written.
  expect_share_dependencies(captured_removal_data(), alist(
    (function(prior) {
      f <- function() sum(prior)
      `function` <- function(...) f
      h <- function() 7
      rm(prior)
      h()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      assign("function", rebind, envir = environment(call_value))
      h <- call_value(f)
      rm(prior)
      h()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      assign(paste0("y", "z"), 1)
      h <- f
      rm(prior)
      h()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      g <- function(x) {
        force(x)
        function() 7
      }
      h <- g(f)
      rm(prior)
      environment(h)$x()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      g <- function(x) {
        force(x)
        function() 7
      }
      h <- g(f)
      rm(prior)
      environment(h)[["x"]]()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      g <- function(x) {
        force(x)
        function() 7
      }
      h <- g(f)
      rm(prior)
      get("x", environment(h))()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      s <- structure(1, store = new.env())
      attr(s, "store")$f <- f
      rm(prior)
      attr(s, "store")$f()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      h <- mget("f")[[1]]
      rm(prior)
      h()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      e <- environment()
      rm(prior)
      e$f()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      l <- as.list(environment())
      rm(prior)
      l$f()
    })(1)
  ), reverse_message = "depends on earlier summary alias `prior`")
})

test_that("an earlier summary's write leaves a captured removal refused", {
  data <- captured_removal_data()
  call_value <- local(function(x) function() 7, envir = new.env())
  rebind <- function(...) get("x", envir = parent.frame())
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    expect_error(
      summarize_with_margins(
        data, prior = sum(x),
        s = {
          assign("function", rebind, envir = environment(call_value))
          1
        },
        value = (function(prior) {
          f <- function() sum(prior)
          h <- call_value(f)
          rm(prior)
          h()
        })(1),
        total = !!share, .grouping = rollup(g)
      ),
      "depends on earlier summary alias `prior`", class = "marginplyr_error"
    )
  }
  expect_null(environment(call_value)[["function"]])
})

test_that("closures returned by functionals stay refused after removal", {
  expect_share_dependencies(captured_removal_data(), alist(
    (function(prior) {
      l <- lapply(1, function(i) function() sum(prior))
      rm(prior)
      l[[1]]()
    })(1),
    (function(prior) {
      l <- Map(function(i) function() sum(prior), 1)
      rm(prior)
      l[[1]]()
    })(1)
  ), reverse_message = "depends on earlier summary alias `prior`")
})

test_that("unreadable removals and replaced enclosures read captured aliases", {
  expect_share_dependencies(captured_removal_data(), alist(
    (function(prior) {
      f <- function() sum(prior)
      rm(prior)
      7
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      rm(list = ls())
      7
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      name <- "prior"
      rm(list = name)
      7
    })(1),
    {
      h <- (function(prior) function() sum(prior))(1)
      rm("prior", envir = environment(h))
      h()
    },
    (function(prior) {
      f <- function() sum(prior)
      h <- mget("f")[[1]]
      environment(h) <- parent.env(environment())
      h()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      h <- mget("f")[[1]]
      base::environment(h) <- parent.env(environment())
      h()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      `environment<-`(f)
      7
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      parent.env(environment()) <- baseenv()
      7
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      f <- do.call("environment<-", list(f, parent.env(environment())))
      f()
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      h <- mget("f")[[1]]
      local(rm("prior", envir = parent.frame(2)))
      h()
    })(1)
  ), reverse_message = "depends on earlier summary alias `prior`")
})

test_that("removals named only by their target read captured aliases", {
  outside <- new.env()
  expect_share_dependencies(captured_removal_data(), list(
    rlang::quo((function(prior) {
      f <- function() sum(prior)
      rm("prior", envir = outside)
      7
    })(1)),
    quote((function(prior) {
      f <- function() sum(prior)
      drop <- function(...) rm(..., envir = parent.frame())
      drop(prior)
      7
    })(1)),
    quote((function(prior) {
      f <- function() sum(prior)
      drop <- function() rm(prior, inherits = TRUE)
      drop()
      7
    })(1)),
    quote((function(prior) {
      key <- "prior"
      f <- function() sum(get(key))
      rm(prior)
      7
    })(1))
  ), reverse_message = "depends on earlier summary alias `prior`")
})

test_that("a local rm or independent reparenting keeps a capture shareable", {
  outside <- new.env()
  expect_share_dependency_values(captured_removal_data(), list(
    quote((function(prior) {
      f <- function() sum(prior)
      rm <- function(...) NULL
      rm(prior)
      f()
    })(1)),
    quote((function(prior) {
      f <- function() sum(prior)
      `environment<-` <- function(fun, value) fun
      environment(f) <- emptyenv()
      f()
    })(1)),
    rlang::quo((function(prior) {
      f <- function() sum(prior)
      parent.env(outside) <- baseenv()
      f()
    })(1))
  ), 1)
})

test_that("capture alone and removal alone leave a source shareable", {
  call_value <- local(function(x) function() 7, envir = new.env())
  data <- captured_removal_data()
  expect_share_dependency_values(data, alist(
    (function(p) {
      f <- function() sum(p)
      assign(paste0("y", "z"), 1)
      h <- f
      h()
    })(1),
    (function(prior) {
      f <- function() 1
      rm(prior)
      f()
    })(1)
  ), 1)
  expect_share_dependency_values(data, list(rlang::quo((function(prior) {
    f <- function() sum(prior)
    h <- call_value(f)
    h()
  })(1))), 7)
})

test_that("rlang removals and enclosure setters read as their base spellings", {
  env_unbind <- rlang::env_unbind
  zap <- rlang::zap
  expect_share_dependencies(captured_removal_data(), list(
    quote((function(prior) {
      rlang::env_unbind(environment(), "prior")
      sum(prior)
    })(1)),
    rlang::quo((function(prior) {
      env_unbind(nms = "prior")
      sum(prior)
    })(1)),
    quote((function(prior) {
      key <- "prior"
      rlang::env_unbind(environment(), key)
      sum(prior)
    })(1)),
    quote((function(prior) {
      rlang::env_bind(environment(), prior = rlang::zap())
      sum(prior)
    })(1)),
    rlang::quo((function(prior) {
      rlang::env_bind(environment(), prior = zap())
      sum(prior)
    })(1)),
    quote((function(prior) {
      rlang::env_bind(environment(), !!!list(prior = rlang::zap()))
      sum(prior)
    })(1)),
    rlang::expr((function(prior) {
      rlang::env_bind(environment(), !!!list(prior = rlang::zap()))
      sum(prior)
    })(1)),
    rlang::expr((function(prior) {
      rlang::env_poke(environment(), "prior", !!rlang::zap())
      sum(prior)
    })(1)),
    quote((function(prior) {
      rlang::env_bind(environment(), prior = if (TRUE) rlang::zap())
      sum(prior)
    })(1)),
    quote((function(prior) {
      rlang::env_poke(environment(), "prior", rlang::zap())
      sum(prior)
    })(1)),
    quote((function(prior) {
      f <- function() sum(prior)
      drop <- function(...) rlang::env_bind(parent.frame(), ...)
      drop(prior = rlang::zap())
      7
    })(1)),
    quote((function(prior) {
      f <- function() sum(prior)
      rlang::env_unbind(environment(), "prior")
      7
    })(1)),
    quote((function(prior) {
      f <- function() sum(prior)
      rlang::fn_env(f) <- parent.env(environment())
      f()
    })(1)),
    quote((function(prior) {
      f <- function() sum(prior)
      f <- rlang::set_env(f, parent.env(environment()))
      f()
    })(1)),
    quote((function(prior) {
      f <- local(function() sum(prior))
      rlang::env_poke_parent(environment(f), parent.env(environment()))
      f()
    })(1))
  ), reverse_message = "depends on earlier summary alias `prior`")
})

# Each source, as `value` after any summaries in `...`, computes `expected`
# under both share helpers.
expect_share_source_values <- function(data, sources, expected, ...) {
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (source in sources) {
      result <- summarize_with_margins(
        data, ..., value = !!source, total = !!share, .grouping = rollup(g)
      )
      expect_equal(result$value, expected)
    }
  }
}

test_that("rlang removals without a captured alias stay shareable", {
  data <- data.frame(g = c("a", "b"), x = c(10, 20))
  expect_share_source_values(data, alist(
    (function(p) {
      rlang::env_unbind(environment(), "p")
      sum(x)
    })(1)
  ), c(10, 20, 30))
  expect_share_dependency_values(captured_removal_data(), alist(
    (function(prior) {
      env_unbind <- function(...) NULL
      env_unbind(environment(), "prior")
      prior
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      f <- rlang::set_env(f)
      f()
    })(1)
  ), 1)
  expect_share_dependency_values(captured_removal_data(), alist(
    (function(prior) {
      rlang::env_bind(environment(), prior = 2)
      prior
    })(1)
  ), 2)
})

test_that("a removal or setter held as a value is not followed (#1652)", {
  # The walk does not follow a function held as a value, so these sources
  # stay accepted and read the earlier alias.
  data <- data.frame(g = c("a", "b"), prior = c(1, 3), x = c(10, 20))
  expect_share_source_values(data, alist(
    (function(prior) {
      r <- rm
      r(prior)
      sum(prior)
    })(1),
    (function(prior) {
      Map(rm, list = "prior", envir = list(environment()))
      sum(prior)
    })(1),
    (function(prior) {
      f <- function() sum(prior)
      se <- `environment<-`
      f <- se(f, parent.env(environment()))
      f()
    })(1)
  ), c(10, 20, 30), prior = sum(x))
})

test_that("a removal from the caller is followed through a stock functional", {
  # The removal is attributed where the closure is invoked (ADR 0010).
  expect_share_dependencies(captured_removal_data(), alist(
    (function(prior) {
      box <- lapply(list(function() rm("prior", envir = parent.frame())),
                    identity)
      box[[1L]]()
      sum(prior)
    })(1),
    (function(prior) {
      f <- Reduce(identity, list(function() {
        rm("prior", envir = parent.frame())
      }))
      f()
      sum(prior)
    })(1)
  ), reverse_message = "depends on earlier summary alias `prior`")
})
