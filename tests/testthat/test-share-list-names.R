list_name_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

list_name_reads <- function(body = quote(sum(prior)),
                            remove = quote(rm(prior)), argument = 1,
                            lookup = quote(carrier$other), invoke = TRUE) {
  writes <- alist(
    names(carrier) <- "other",
    attr(carrier, "names") <- "other",
    attributes(carrier) <- list(names = "other")
  )
  lapply(writes, function(write) {
    substitute((function(prior) {
      carrier <- list(read = function() BODY)
      WRITE
      REMOVE
      LOOKUP
    })(ARGUMENT), list(BODY = body, WRITE = write, REMOVE = remove,
                       ARGUMENT = argument,
                       LOOKUP = if (invoke) rlang::call2(lookup) else lookup))
  })
}

test_that("literal list names retain selected closure dependencies", {
  expect_share_dependencies(list_name_data(), list_name_reads())
})

swapped_list_name_reads <- function(lookup = quote(carrier$other)) {
  writes <- alist(
    names(carrier) <- c("other", "read"),
    attr(carrier, "names") <- c("other", "read"),
    attributes(carrier) <- list(names = c("other", "read"))
  )
  lapply(writes, function(write) {
    substitute((function(prior) {
      carrier <- list(read = function() sum(prior), other = function() 7)
      WRITE
      rm(prior)
      LOOKUP()
    })(1), list(WRITE = write, LOOKUP = lookup))
  })
}

test_that("swapped list names select the independent and dependent closures", {
  expect_share_dependency_values(
    list_name_data(), swapped_list_name_reads(quote(carrier$read)), expected = 7
  )
  expect_share_dependencies(list_name_data(), swapped_list_name_reads())
})

expect_list_name_controls <- function(data) {
  before <- serialize(data, NULL)
  expect_share_dependencies(data, list_name_reads())
  lookups <- as.list(quote(alist(
    carrier[[1L]], carrier[["other"]], carrier[["other", exact = TRUE]],
    carrier$oth, carrier[["oth", exact = FALSE]]
  )))[-1L]
  for (lookup in lookups) {
    expect_share_dependencies(data, list_name_reads(lookup = lookup))
  }
  expect_share_dependency_values(
    data, list_name_reads(remove = quote(NULL)), expected = 1
  )
  expect_share_dependency_values(
    data, list_name_reads(body = quote(7)), expected = 7
  )
  expect_share_dependency_values(
    data, swapped_list_name_reads(quote(carrier$read)), expected = 7
  )
  expect_share_dependencies(data, swapped_list_name_reads())
  expect_identical(serialize(data, NULL), before)
}

test_that("renamed list lookup preserves eager controls and inputs", {
  expect_list_name_controls(list_name_data())
  expect_list_name_controls(tibble::as_tibble(list_name_data()))
})

test_that("data.table renamed list lookup preserves controls and inputs", {
  skip_if_suggest_absent("data.table")
  expect_list_name_controls(data.table::as.data.table(list_name_data()))
})

test_that("renamed list lookup respects exact and missing names", {
  for (lookup in alist(carrier[["oth"]], carrier[["oth", exact = TRUE]],
                       carrier$read, carrier[["read", exact = FALSE]])) {
    reads <- list_name_reads(invoke = FALSE, lookup = rlang::expr({
      selected <- !!lookup
      if (is.null(selected)) 7 else selected()
    }))
    expect_share_dependency_values(list_name_data(), reads, expected = 7)
  }
})

test_that("list name planning does not invoke closures or force arguments", {
  probe <- rlang::env(invoked = 0L, forced = 0L)
  reads <- list_name_reads(body = quote({
    probe$invoked <- probe$invoked + 1L
    sum(prior)
  }), argument = quote({
    probe$forced <- probe$forced + 1L
    1
  }))
  expect_share_dependencies(list_name_data(), reads)
  expect_identical(probe$invoked, 0L)
  expect_identical(probe$forced, 0L)
})

test_that("renamed list dependencies leave lazy input unread", {
  skip_if_suggest_absent("dtplyr")
  probe <- rlang::env(calls = 0L)
  observe <- function(x) {
    probe$calls <- probe$calls + 1L
    x
  }
  data <- dplyr::filter(dtplyr::lazy_dt(list_name_data()), observe(x) > 0)
  expect_share_dependencies(data, list_name_reads())
  expect_share_dependencies(data, swapped_list_name_reads())
  expect_identical(probe$calls, 0L)
  expect_equal(dplyr::collect(data)$x, c(1, 3))
  expect_identical(probe$calls, 1L)
})

test_that("classed attributes leave list member dispatch to the caller", {
  registerS3method("$", "margin_share_named_carrier", function(x, name) {
    if (identical(name, "other")) function() 7 else .subset2(unclass(x), name)
  }, envir = asNamespace("base"))
  read <- quote((function(prior) {
    carrier <- list(read = function() sum(prior))
    attributes(carrier) <- list(names = "other",
                                class = "margin_share_named_carrier")
    rm(prior)
    carrier$other()
  })(1))
  expect_identical(eval(read), 7)
  expect_share_dependency_values(list_name_data(), list(read), expected = 7)
})

test_that("incomplete attribute syntax reaches dplyr's empty argument error", {
  read <- quote((function(prior) {
    carrier <- list(read = function() sum(prior))
    attributes(carrier) <- list(names = )
    rm(prior)
    carrier$other()
  })(1))
  data <- list_name_data()
  direct <- expect_error(dplyr::summarise(data, observed = !!read))
  expect_match(conditionMessage(direct$parent), "argument 1 is empty")
  helpers <- list(quote(share_of_parent(value)), quote(share_of_total(value)))
  for (helper in helpers) {
    actual <- expect_error(summarize_with_margins(
      data, value = sum(.data$x), prior = !!helper, observed = !!read,
      .grouping = rollup(g)
    ))
    expect_identical(class(actual), class(direct))
    expect_identical(class(actual$parent), class(direct$parent))
    expect_identical(conditionMessage(actual$parent),
                     conditionMessage(direct$parent))
  }
})
