plain_list_transform_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

plain_list_transform_reads <- function(body = quote(sum(prior)),
                                       remove = quote(rm(prior)),
                                       argument = 1) {
  carriers <- as.list(quote(alist(
    c(original),
    append(list(), original),
    original[1L],
    base::c(NULL, original, list()),
    base::append(values = original, x = list(function() 9), after = 0L),
    base::`[`(original, "read")
  )))[-1L]
  lapply(carriers, function(carrier) {
    substitute((function(prior) {
      original <- list(read = function() BODY)
      functions <- CARRIER
      REMOVE
      functions[[1L]]()
    })(ARGUMENT), list(BODY = body, CARRIER = carrier, REMOVE = remove,
                       ARGUMENT = argument))
  })
}

expect_list_transform_values <- function(data, reads, expected) {
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
      expect_equal(forward$observed, rep(expected, 3L))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$value, rep(expected, 3L))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
}

expect_plain_list_transforms <- function(data) {
  before <- serialize(data, NULL)
  expect_share_dependencies(data, plain_list_transform_reads())
  expect_list_transform_values(
    data, plain_list_transform_reads(remove = quote(NULL)), expected = 1
  )
  expect_list_transform_values(
    data, plain_list_transform_reads(body = quote(7)), expected = 7
  )
  expect_identical(serialize(data, NULL), before)
}

test_that("plain list transformations preserve eager closure dependencies", {
  expect_plain_list_transforms(plain_list_transform_data())
  expect_plain_list_transforms(tibble::as_tibble(plain_list_transform_data()))
})

test_that("data.table list transformations preserve closure dependencies", {
  skip_if_suggest_absent("data.table")
  expect_plain_list_transforms(data.table::as.data.table(
    plain_list_transform_data()
  ))
})

test_that("list transformation planning never invokes or forces caller code", {
  probe <- rlang::env(invoked = 0L, forced = 0L)
  reads <- plain_list_transform_reads(
    body = quote({
      probe$invoked <- probe$invoked + 1L
      sum(prior)
    }),
    argument = quote({
      probe$forced <- probe$forced + 1L
      1
    })
  )
  expect_share_dependencies(plain_list_transform_data(), reads)
  expect_identical(probe$invoked, 0L)
  expect_identical(probe$forced, 0L)
})
