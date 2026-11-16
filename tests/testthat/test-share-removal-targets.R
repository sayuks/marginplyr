# The frame a removal's target environment names (ADR 0010).

removal_target_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
}

test_that("a removal from an unplaced environment may reach any frame", {
  # The removal reaches the outer frame, where `prior` is the formal.
  expect_share_dependencies(removal_target_data(), alist(
    (function(prior) {
      r <- function() {
        e <- parent.frame()
        rm("prior", envir = e)
      }
      r()
      sum(prior)
    })(1),
    (function(prior) {
      r <- function(e) rm("prior", envir = e)
      r(environment())
      sum(prior)
    })(1),
    (function(prior) {
      # `sys.parent` is a formal, so the frame is not placed. R removes only
      # the inner `prior`, and the refusal over-reports (#130).
      (function(prior, sys.parent) { # nolint: object_name_linter.
        rm("prior", envir = sys.frame(sys.parent()))
      })(0, function() base::sys.parent())
      sum(prior)
    })(1)
  ), reverse_message = "depends on earlier summary alias `prior`")
})

test_that("caller_env() and parent.env() name the frame they reach", {
  expect_share_dependencies(removal_target_data(), alist(
    (function(prior) {
      (function() rm("prior", envir = parent.env(environment())))()
      sum(prior)
    })(1),
    (function(prior) {
      r <- function() rlang::env_unbind(rlang::caller_env(), "prior")
      r()
      sum(prior)
    })(1)
  ), reverse_message = "depends on earlier summary alias `prior`")
})

test_that("a placed removal leaves the frames it does not reach bound", {
  # Each removal takes the inner formal, so `sum(prior)` reads the outer one.
  expect_share_dependency_values(removal_target_data(), alist(
    (function(prior) {
      (function(prior) {
        (function() rm("prior", envir = parent.env(environment())))()
      })(2)
      sum(prior)
    })(1),
    (function(prior) {
      (function(prior) {
        (function() rlang::env_unbind(rlang::caller_env(), "prior"))()
      })(2)
      sum(prior)
    })(1),
    (function(prior) {
      (function(prior) {
        (function() {
          e <- parent.frame()
          rm("prior", envir = e)
        })()
      })(2)
      sum(prior)
    })(1)
  ), 1)
})

test_that("an unplaced removal of a name nothing reads stays shareable", {
  data <- data.frame(g = c("a", "b"), x = c(10, 20))
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    result <- summarize_with_margins(
      data,
      value = (function(p) {
        r <- function() {
          e <- parent.frame()
          rm("p", envir = e)
        }
        r()
        sum(x)
      })(1),
      ratio = !!share,
      .grouping = rollup("g")
    )
    expect_equal(result$value, c(10, 20, 30))
    expect_equal(result$ratio, c(1 / 3, 2 / 3, 1))
  }
})
