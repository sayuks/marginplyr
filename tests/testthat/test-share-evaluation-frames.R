# Caller frames and evaluation environments of code that `local()`, `eval()`
# or `evalq()` evaluates (ADR 0010).

evaluation_frame_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
}

test_that("a removal past an evaluated frame may reach any walked frame", {
  # Only the planning walk sees these: each refusal precedes evaluation.
  expect_share_dependencies(evaluation_frame_data(), alist(
    (function(prior) {
      local(rm("prior", envir = parent.frame(2)))
      sum(prior)
    })(1),
    (function(prior) {
      local(rm("prior", envir = sys.frame(sys.parent(2))))
      sum(prior)
    })(1),
    (function(prior) {
      eval(quote(rm("prior", envir = parent.frame(2))), new.env())
      sum(prior)
    })(1)
  ))
})

test_that("code evaluated in an unwalked or caller frame reads the mask", {
  expect_share_dependencies(evaluation_frame_data(), alist(
    (function(prior) eval(quote(sum(prior)), parent.frame()))(1),
    (function(prior) evalq(sum(prior), parent.frame()))(1),
    (function(prior) eval(quote(sum(prior)), parent.env(environment())))(1)
  ))
})

test_that("the first caller of an evaluated frame is base's own frame", {
  # `rm()` warns that `prior` is absent from the frame it reaches.
  suppressWarnings(expect_share_dependency_values(
    evaluation_frame_data(), alist(
      (function(prior) {
        local(rm("prior", envir = parent.frame()))
        sum(prior)
      })(1)
    ), 1
  ))
})

test_that("code evaluated in a walked frame reads that frame's bindings", {
  expect_share_dependency_values(evaluation_frame_data(), alist(
    (function(prior) evalq(sum(prior)))(1),
    (function(prior) eval(quote(sum(prior)), environment()))(1),
    (function(prior) {
      (function() eval(quote(sum(prior)), parent.frame()))()
    })(1)
  ), 1)
})

test_that("a removal evaluated in a caller frame removes from that frame", {
  expect_share_dependencies(evaluation_frame_data(), alist(
    (function(prior) {
      (function() eval(quote(rm("prior")), parent.frame()))()
      sum(prior)
    })(1),
    (function(prior) {
      (function() evalq(rm("prior"), parent.frame()))()
      sum(prior)
    })(1)
  ))
})
