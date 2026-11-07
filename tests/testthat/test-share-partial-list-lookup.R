partial_list_lookup_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

partial_list_lookup_read <- function(lookup, body = quote(sum(prior)),
                                     remove = quote(rm(prior)), argument = 1) {
  substitute((function(prior) {
    fs <- list(reader = function() BODY)
    REMOVE
    LOOKUP()
  })(ARGUMENT), list(BODY = body, REMOVE = remove, LOOKUP = lookup,
                     ARGUMENT = argument))
}

test_that("unique dollar lookup preserves contextual share dependencies", {
  read <- partial_list_lookup_read(quote(fs$read))
  expect_share_dependencies(partial_list_lookup_data(), list(read))
})

test_that("explicit partial brackets preserve contextual share dependencies", {
  read <- partial_list_lookup_read(quote(fs[["read", exact = FALSE]]))
  expect_share_dependencies(partial_list_lookup_data(), list(read))
})

partial_list_lookups <- function() {
  as.list(quote(alist(
    fs$read,
    fs[["read", exact = FALSE]],
    fs$reader,
    fs[["reader"]],
    fs[["reader", exact = TRUE]],
    fs[["reader", exact = FALSE]]
  )))[-1L]
}

expect_partial_list_values <- function(data, reads, expected) {
  for (kind in c("parent", "total")) {
    for (read in reads) {
      helper <- rlang::call2(paste0("share_of_", kind), quote(source))
      forward <- summarize_with_margins(
        data, source = sum(.data$x), prior = !!helper, observed = !!read,
        .grouping = rollup("g")
      )
      helper <- rlang::call2(paste0("share_of_", kind), quote(observed))
      reverse <- summarize_with_margins(
        data, prior = sum(.data$x), observed = !!read, ratio = !!helper,
        .grouping = rollup("g")
      )
      expect_equal(forward$observed, rep(expected, 3L))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$observed, rep(expected, 3L))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
}

expect_partial_list_lookup <- function(data) {
  before <- serialize(data, NULL)
  lookups <- partial_list_lookups()
  expect_share_dependencies(data, lapply(lookups, partial_list_lookup_read))
  expect_partial_list_values(
    data, lapply(lookups, partial_list_lookup_read, remove = quote(NULL)),
    expected = 1
  )
  expect_partial_list_values(
    data, lapply(lookups, partial_list_lookup_read, body = quote(7)),
    expected = 7
  )
  expect_identical(serialize(data, NULL), before)
}

test_that("partial list lookups preserve eager closure dependencies", {
  expect_partial_list_lookup(partial_list_lookup_data())
  expect_partial_list_lookup(tibble::as_tibble(partial_list_lookup_data()))
})

test_that("data.table partial lookups preserve closure dependencies", {
  skip_if_suggest_absent("data.table")
  expect_partial_list_lookup(data.table::as.data.table(
    partial_list_lookup_data()
  ))
})

test_that("partial lookup planning never invokes or forces caller code", {
  probe <- rlang::env(invoked = 0L, forced = 0L)
  reads <- lapply(partial_list_lookups(), partial_list_lookup_read,
    body = quote({
      probe$invoked <- probe$invoked + 1L
      sum(prior)
    }),
    argument = quote({
      probe$forced <- probe$forced + 1L
      1
    })
  )
  expect_share_dependencies(partial_list_lookup_data(), reads)
  expect_identical(probe$invoked, 0L)
  expect_identical(probe$forced, 0L)
})

test_that("list lookup distinguishes exact, ambiguous and nonmatching names", {
  cases <- list(
    list(quote(fs$read), quote(list(
      reader = function() sum(prior), read = function() 7
    ))),
    list(quote(fs[["read", exact = FALSE]]), quote(list(
      reader = function() sum(prior), read = function() 7
    ))),
    list(quote(fs$read), quote(list(
      reader = function() sum(prior), reading = function() sum(prior)
    ))),
    list(quote(fs[["read", exact = FALSE]]), quote(list(
      reader = function() sum(prior), reading = function() sum(prior)
    ))),
    list(quote(fs$absent), quote(list(reader = function() sum(prior)))),
    list(quote(fs[["absent", exact = FALSE]]),
         quote(list(reader = function() sum(prior)))),
    list(quote(fs[["read"]]), quote(list(reader = function() sum(prior)))),
    list(quote(fs[["read", exact = TRUE]]),
         quote(list(reader = function() sum(prior))))
  )
  reads <- lapply(cases, function(case) {
    substitute((function(prior) {
      fs <- MEMBERS
      rm(prior)
      selected <- LOOKUP
      if (is.null(selected)) 7 else selected()
    })(1), list(MEMBERS = case[[2L]], LOOKUP = case[[1L]]))
  })
  expect_partial_list_values(partial_list_lookup_data(), reads, expected = 7)
})
