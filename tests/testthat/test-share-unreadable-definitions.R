unreadable_definition_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
}

test_that("an empty block or explicit local() environment holds no closure", {
  expect_share_dependency_values(unreadable_definition_data(), alist(
    (function(prior) {
      held <- {}
      rm(prior)
      if (is.null(held)) 7 else held()
    })(1),
    (function(prior) {
      held <- local(function() 7, envir = new.env())
      rm(prior)
      held()
    })(1)
  ), 7)
})

test_that("closure transfers missing their input hold no closure", {
  # Each malformed transfer fails when R evaluates it; the handler supplies
  # the closure the summary calls.
  expect_share_dependency_values(unreadable_definition_data(), alist(
    (function(prior) {
      held <- tryCatch(I(), error = function(cnd) function() 7)
      rm(prior)
      held()
    })(1),
    (function(prior) {
      held <- tryCatch(
        `attr<-`(which = "tag", value = 1),
        error = function(cnd) function() 7
      )
      rm(prior)
      held()
    })(1),
    (function(prior) {
      held <- function() sum(prior)
      held <- tryCatch(
        `body<-`(value = quote(sum(prior))),
        error = function(cnd) function() 7
      )
      rm(prior)
      held()
    })(1)
  ), 7)
})

test_that("unreadable definition replacements leave the closure unresolved", {
  inlined <- factor("sum(prior)")
  expect_share_dependencies(unreadable_definition_data(), c(
    alist(
      {
        replacement <- quote(sum(prior))
        held <- function() 7
        body(held) <- replacement
        held()
      },
      {
        replacement <- quote(sum(prior))
        held <- function(arg) arg
        formals(held) <- pairlist(arg = replacement)
        held()
      },
      {
        held <- function() 7
        body(held) <- quote(expr = )
        held()
      },
      {
        held <- function() 7
        formals(held) <- 3
        held()
      },
      {
        held <- function(arg) arg
        formals(held) <- formals()
        held(1)
      },
      {
        held <- function(arg) arg
        formals(held) <- `$<-`(formals(held), "arg")
        held()
      },
      {
        held <- function(arg) arg
        formals(held) <- `$<-`(3, "arg", 1)
        held()
      },
      (function(prior) {
        factory <- function() function() 7
        replacement <- quote(function() sum(prior))
        body(factory) <- replacement
        made <- factory()
        rm(prior)
        made()
      })(1)
    ),
    rlang::exprs({
      held <- function() 7
      body(held) <- !!inlined
      held()
    })
  ))
})

test_that("a closure's own name read while it runs adds no dependency", {
  expect_share_dependency_values(unreadable_definition_data(), alist(
    (function(prior) {
      held <- function() {
        held
        7
      }
      rm(prior)
      held()
    })(1),
    (function(prior) {
      held <- function() 7
      environment(held) <- baseenv()
      held
      held()
    })(1)
  ), 7)
})

test_that("a fresh enclosure's closure fields are called as functions", {
  expect_share_dependency_values(unreadable_definition_data(), alist(
    (function(prior) {
      held <- function() field()
      environment(held) <- list2env(
        list(field = function() 7), parent = baseenv()
      )
      rm(prior)
      held()
    })(1)
  ), 7)
  expect_share_dependencies(unreadable_definition_data(), alist(
    (function(prior) {
      held <- function() field()
      environment(held) <- list2env(
        list(field = function() sum(prior)), parent = baseenv()
      )
      rm(prior)
      held()
    })(1)
  ))
})

test_that("an emptied class keeps a closure's own reads", {
  expect_share_dependencies(unreadable_definition_data(), alist(
    (function(prior) {
      held <- function() sum(prior)
      class(held) <- c()
      rm(prior)
      held()
    })(1)
  ))
})
