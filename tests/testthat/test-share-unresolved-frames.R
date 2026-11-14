unresolved_frame_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
}

test_that("a numbered dot without a dots binding holds no closure", {
  # `..1` fails when R evaluates it; the handler supplies the closure the
  # summary calls.
  expect_share_dependency_values(unresolved_frame_data(), alist(
    (function(prior) {
      held <- tryCatch(..1, error = function(cnd) function() 7)
      rm(prior)
      held()
    })(1)
  ), 7)
  expect_share_dependencies(unresolved_frame_data(), alist(
    (function(prior, ...) {
      held <- ..1
      rm(prior)
      held()
    })(1, function() sum(prior))
  ))
})

test_that("a removal beyond the known caller frames keeps local bindings", {
  expect_share_dependency_values(unresolved_frame_data(), alist(
    (function(prior) {
      (function() {
        suppressWarnings(base::rm("prior", envir = parent.frame(9)))
      })()
      sum(prior)
    })(1)
  ), 1)
  expect_share_dependencies(unresolved_frame_data(), alist(
    (function(prior) {
      (function() base::rm("prior", envir = parent.frame(1)))()
      sum(prior)
    })(1)
  ))
})

test_that("an environment parent past the column mask is not the mask", {
  # The enclosure of `read` skips its caller, so three parents pass the
  # summary frame and the mask and reach this test's own frame.
  prior <- 5
  for (helper in c("share_of_parent", "share_of_total")) {
    result <- summarize_with_margins(
      unresolved_frame_data(), value = sum(.data$x),
      fraction = !!rlang::call2(helper, quote(value)),
      observed = {
        read <- function() rlang::env_parent(environment(), 3)$prior
        (function() read())()
      },
      .grouping = rollup("g")
    )
    expect_identical(result$observed, rep(prior, 3L))
    expect_identical(result$fraction, c(0.25, 0.75, 1))
  }
  expect_share_dependencies(unresolved_frame_data(), alist({
    read <- function() rlang::env_parent(environment(), 2)$prior
    (function() read())()
  }))
})

test_that("a stop frame from a reparented environment bounds no search", {
  enclosure <- new.env()
  stop_frame <- local(function() parent.frame(), enclosure)
  expect_share_dependencies(unresolved_frame_data(), alist({
    parent.env(enclosure) <- globalenv()
    sum(rlang::env_get(environment(), "prior", inherit = TRUE,
                       last = stop_frame()))
  }))
})
