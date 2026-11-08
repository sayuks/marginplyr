test_that("member body replacement refreshes share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  expect_share_dependencies(data, alist({
    box <- list(f = function() 7)
    body(box$f) <- quote(sum(prior))
    box$f()
  }))
})

member_definition_cases <- function(definition, write) {
  containers <- list(
    rlang::expr(list(f = !!definition)),
    rlang::expr(list(!!definition)),
    rlang::expr(list(inner = list(f = !!definition))),
    rlang::expr(list2env(list(f = !!definition)))
  )
  members <- alist(box$f, box[[1L]], box$inner$f, box$f)
  Map(function(initial, receiver) {
    updated <- do.call(substitute, list(write, list(member = receiver)))
    rlang::call2("{", rlang::call2("<-", quote(box), initial), updated,
                 rlang::call2(receiver))
  }, containers, members)
}

test_that("member formal replacement refreshes share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  reads <- c(
    member_definition_cases(
      quote(function(prior) sum(prior)),
      quote(formals(member)$prior <- NULL)
    ),
    member_definition_cases(
      quote(function(arg = 7) arg),
      quote(formals(member)[["arg"]] <- quote(sum(prior)))
    )
  )
  expect_share_dependencies(data, reads)
})

test_that("member enclosure replacement refreshes share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  reads <- member_definition_cases(
    quote(function() sum(prior)),
    quote(environment(member) <- parent.env(environment()))
  )
  reads <- lapply(reads, function(read) {
    rlang::expr((function(prior) !!read)(7))
  })
  expect_share_dependencies(data, reads)
})

member_definition_reads <- function() {
  enclosures <- member_definition_cases(
    quote(function() sum(prior)),
    quote(environment(member) <- parent.env(environment()))
  )
  c(
    member_definition_cases(quote(function() 7),
                            quote(body(member) <- quote(sum(prior)))),
    member_definition_cases(quote(function(prior) sum(prior)),
                            quote(formals(member)$prior <- NULL)),
    member_definition_cases(quote(function(arg = 7) arg),
                            quote(formals(member)$arg <- quote(sum(prior)))),
    lapply(enclosures, function(read) {
      rlang::expr((function(prior) !!read)(7))
    })
  )
}

test_that("member replacement paths preserve explicit setter controls", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  enclosures <- member_definition_cases(
    quote(function() sum(prior)),
    quote(member <- base::`environment<-`(
      member, value = parent.env(environment())
    ))
  )
  reads <- c(
    member_definition_cases(
      quote(function() 7),
      quote(member <- base::`body<-`(member, value = quote(sum(prior))))
    ),
    member_definition_cases(
      quote(function(prior) sum(prior)),
      quote(member <- base::`formals<-`(member, value = NULL))
    ),
    lapply(enclosures, function(read) rlang::expr((function(prior) !!read)(7)))
  )
  expect_share_dependencies(data, c(member_definition_reads(), reads))
})

test_that("independent member replacements discard obsolete dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  controls <- c(
    member_definition_cases(quote(function() 11), quote(NULL)),
    member_definition_cases(quote(function() sum(prior)),
                            quote(body(member) <- quote(11))),
    member_definition_cases(quote(function() sum(prior)),
                            quote(formals(member)$prior <- 11)),
    member_definition_cases(
      quote(function() sum(prior)),
      quote(environment(member) <- list2env(
        list(prior = 11), parent = baseenv()
      ))
    )
  )
  controls <- lapply(controls, function(read) {
    statements <- as.list(read)[-1L]
    invocation <- statements[[length(statements)]]
    read <- rlang::call2("{", !!!head(statements, -1L), quote(rm(prior)),
                         invocation)
    rlang::expr((function(prior) !!read)(7))
  })
  expect_share_dependency_values(data, controls, 11)
})

test_that("member definition setters retain ordinary function lookup", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  controls <- c(
    member_definition_cases(quote(function() 11), quote({
      `body<-` <- function(fun, value) fun
      body(member) <- quote(sum(prior))
    })),
    member_definition_cases(quote(function(prior = 11) sum(prior)), quote({
      `formals<-` <- function(fun, value) fun
      formals(member)$prior <- NULL
    })),
    member_definition_cases(quote(function(prior = 11) sum(prior)), quote({
      `environment<-` <- function(fun, value) fun
      environment(member) <- parent.env(environment())
    }))
  )
  expect_share_dependency_values(data, controls, 11)
})

test_that("ordinary member replacements retain values and eager input", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  for (input in list(data, tibble::as_tibble(data))) {
    before <- serialize(input, NULL)
    for (read in member_definition_reads()) {
      ordinary <- summarize_with_margins(
        input, prior = sum(.data$x), observed = !!read, .grouping = rollup("g")
      )
      expect_equal(ordinary$observed, c(1, 3, 4))
    }
    expect_share_dependencies(input, member_definition_reads())
    expect_identical(serialize(input, NULL), before)
  }
})

test_that("member definition planning leaves caller code unexecuted", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  probe <- rlang::env(written = 0L, invoked = 0L)
  reads <- member_definition_cases(quote(function() 7), quote(body(member) <- {
    probe$written <- probe$written + 1L
    quote({
      probe$invoked <- probe$invoked + 1L
      sum(prior)
    })
  }))
  expect_share_dependencies(data, reads)
  expect_identical(probe$written, 0L)
  expect_identical(probe$invoked, 0L)
})

test_that("member definition refusal leaves lazy input unread and unchanged", {
  skip_if_suggest_absent("dtplyr")
  probe <- rlang::env(calls = 0L)
  observe <- function(x) {
    probe$calls <- probe$calls + 1L
    x
  }
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  before <- serialize(data, NULL)
  lazy <- dtplyr::lazy_dt(data, immutable = TRUE)
  lazy <- dplyr::filter(lazy, observe(x) > 0)
  expect_share_dependencies(lazy, member_definition_reads())
  expect_identical(probe$calls, 0L)
  expect_identical(serialize(data, NULL), before)
  expect_equal(dplyr::collect(lazy)$x, c(1, 3))
  expect_identical(probe$calls, 1L)
  expect_identical(serialize(data, NULL), before)
})
