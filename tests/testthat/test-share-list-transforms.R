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

expect_plain_list_transforms <- function(data) {
  before <- serialize(data, NULL)
  expect_share_dependencies(data, plain_list_transform_reads())
  expect_share_dependency_values(
    data, plain_list_transform_reads(remove = quote(NULL)), expected = 1
  )
  expect_share_dependency_values(
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
