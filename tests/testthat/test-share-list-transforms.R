plain_list_transform_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

plain_list_transform_reads <- function(body = quote(sum(prior)),
                                       remove = quote(rm(prior)),
                                       argument = 1) {
  carriers <- as.list(quote(alist(
    original,
    identity(original),
    invisible(original),
    force(original),
    base::identity(x = original),
    ((base::invisible))(original),
    base::force(x = original),
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

test_that("list value transfers preserve ordinary caller function lookup", {
  data <- plain_list_transform_data()
  before <- serialize(data, NULL)
  for (callee in c("identity", "invisible", "force")) {
    probe <- rlang::env(calls = 0L)
    caller <- rlang::env()
    caller[[callee]] <- function(x) {
      probe$calls <- probe$calls + 1L
      list(function() 7)
    }
    read <- substitute((function(prior) {
      original <- list(function() sum(prior))
      functions <- transfer
      rm(prior)
      functions[[1L]]()
    })(1), list(transfer = rlang::call2(callee, quote(original))))
    read <- rlang::new_quosure(read, caller)
    expect_share_dependency_values(data, list(read), expected = 7)
    expect_identical(probe$calls, 12L)
    expect_error(
      summarize_with_margins(
        data, value = sum(.data$x), prior = share_of_total(value),
        observed = !!read, forbidden = prior, .grouping = rollup("g")
      ),
      "Ordinary summaries cannot use an earlier", class = "marginplyr_error"
    )
    expect_identical(probe$calls, 12L)

    read <- substitute((function(prior) {
      binding <- 0
      original <- list(function() sum(prior))
      functions <- transfer
      rm(prior)
      functions[[1L]]()
    })(1), list(
      binding = rlang::sym(callee),
      transfer = rlang::call2(callee, quote(original))
    ))
    expect_share_dependencies(data, list(read))
  }
  expect_identical(serialize(data, NULL), before)
})
