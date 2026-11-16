# Caller frames and evaluation environments of code that `local()`, `eval()`,
# `evalq()`, `with()`, `rlang::eval_bare()`, `rlang::with_env()` or
# `rlang::inject()` evaluates (ADR 0010).

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

test_that("rlang::eval_bare() removes from the frame its env names", {
  expect_share_dependencies(evaluation_frame_data(), alist(
    (function(prior) {
      (function() rlang::eval_bare(quote(rm("prior")), parent.frame()))()
      sum(prior)
    })(1),
    (function(prior) {
      e <- environment()
      (function() rlang::eval_bare(quote(rm("prior")), e))()
      sum(prior)
    })(1)
  ))
  expect_share_dependency_values(evaluation_frame_data(), alist(
    (function(prior) {
      other <- 1
      (function() rlang::eval_bare(quote(rm("other")), parent.frame()))()
      sum(prior)
    })(1)
  ), 1)
})

test_that("an attached eval_bare() removes from the frame its env names", {
  eval_bare <- rlang::eval_bare
  expect_share_dependencies(evaluation_frame_data(), list(
    rlang::quo((function(prior) {
      (function() eval_bare(quote(rm("prior")), parent.frame()))()
      sum(prior)
    })(1))
  ))
})

test_that("rlang::eval_tidy() removes from the mask's child, not its env", {
  reads <- alist(
    (function(prior) {
      (function() rlang::eval_tidy(quote(rm("prior")), env = parent.frame()))()
      sum(prior)
    })(1)
  )
  # `rm()` warns that `prior` is absent from the child it reaches.
  suppressWarnings(
    expect_share_dependency_values(evaluation_frame_data(), reads, 1)
  )
})

test_that("with() and rlang::with_env() remove from the frame they name", {
  expect_share_dependencies(evaluation_frame_data(), alist(
    (function(prior) {
      (function() with(parent.frame(), rm("prior")))()
      sum(prior)
    })(1),
    (function(prior) {
      (function() rlang::with_env(parent.frame(), rm("prior")))()
      sum(prior)
    })(1),
    (function(prior) {
      (function(e) with(e, rm("prior")))(environment())
      sum(prior)
    })(1)
  ))
  expect_share_dependency_values(evaluation_frame_data(), alist(
    (function(prior) {
      other <- 1
      (function() with(parent.frame(), rm("other")))()
      sum(prior)
    })(1),
    (function(prior) {
      other <- 1
      (function() rlang::with_env(parent.frame(), rm("other")))()
      sum(prior)
    })(1)
  ), 1)
})

test_that("rlang::inject() removes from the frame its env names", {
  expect_share_dependencies(evaluation_frame_data(), alist(
    (function(prior) {
      (function() rlang::inject(rm("prior"), parent.frame()))()
      sum(prior)
    })(1)
  ))
  expect_share_dependency_values(evaluation_frame_data(), alist(
    (function(prior) {
      other <- 1
      (function() rlang::inject(rm("other"), parent.frame()))()
      sum(prior)
    })(1)
  ), 1)
})

test_that("rlang::inject() runs its operands in the calling frame", {
  # `expect_error()` would inject the operand while capturing; a quosure
  # injected into a direct call reaches the walk with it intact.
  read <- rlang::new_quosure(quote((function() {
    (function(prior) {
      rlang::inject(c(!!rm("prior")), parent.frame())
      sum(prior)
    })(1)
  })()))
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    reverse <- rlang::catch_cnd(summarize_with_margins(
      evaluation_frame_data(), prior = sum(.data$x), value = !!read,
      ratio = !!share, .grouping = rollup("g")
    ), "marginplyr_error")
    expect_match(conditionMessage(reverse), "depends on earlier summary alias")
    share <- rlang::call2(paste0("share_of_", kind), quote(source))
    forward <- rlang::catch_cnd(summarize_with_margins(
      evaluation_frame_data(), source = sum(.data$x), prior = !!share,
      observed = !!read, .grouping = rollup("g")
    ), "marginplyr_error")
    expect_match(conditionMessage(forward), "Ordinary summaries cannot use")
  }
})

test_that("with() evaluates in the frame it names, or beside a data value", {
  expect_share_dependencies(evaluation_frame_data(), alist(
    (function(prior) with(parent.frame(), sum(prior)))(1)
  ))
  expect_share_dependency_values(evaluation_frame_data(), alist(
    (function(prior) (function() with(parent.frame(), sum(prior)))())(1),
    (function(prior) with(list(a = 1), sum(prior)))(1),
    (function(prior) rlang::with_env(data.frame(a = 1), sum(prior)))(1)
  ), 1)
})
