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

test_that("static list subsets preserve retained closure dependencies", {
  subsets <- alist(
    functions[-2L],
    functions[c(1L)],
    functions[1L, drop = FALSE],
    functions[1L],
    functions[-1L],
    functions[base::c(2L, 2L)],
    functions[2L, drop = TRUE]
  )
  reads <- lapply(subsets, function(subset) {
    substitute((function(prior) {
      functions <- list(dependent = function() sum(prior), safe = function() 7)
      functions <- SUBSET
      rm(prior)
      functions[[1L]]()
    })(1), list(SUBSET = subset))
  })
  data <- plain_list_transform_data()
  expect_share_dependencies(data, reads[1:4])
  expect_share_dependency_values(data, reads[5:7], expected = 7)
})

test_that("logical and negative member indices select the same closure as R", {
  indices <- alist(TRUE, -2L, 1L, -1L)
  reads <- lapply(indices, function(index) {
    substitute((function(prior) {
      functions <- list(function() sum(prior), function() 7)
      rm(prior)
      functions[[INDEX]]()
    })(1), list(INDEX = index))
  })
  data <- plain_list_transform_data()
  expect_share_dependencies(data, reads[1:3])
  expect_share_dependency_values(data, reads[4L], expected = 7)
})

test_that("list selector code runs only when summaries evaluate", {
  probe <- rlang::env(calls = 0L)
  choose <- function() {
    probe$calls <- probe$calls + 1L
    -2L
  }
  caller <- rlang::env(choose = choose)
  caller[["-"]] <- function(x) abs(choose())
  caller[["c"]] <- function(...) abs(choose())
  subsets <- alist(
    functions[-2L],
    functions[c(1L)],
    functions[base::`-`(choose())],
    functions[2L, drop = choose() != 0L]
  )
  reads <- lapply(subsets, function(subset) {
    read <- substitute((function(prior) {
      functions <- list(function() sum(prior), function() 7)
      functions <- SUBSET
      rm(prior)
      functions[[1L]]()
    })(1), list(SUBSET = subset))
    rlang::new_quosure(read, caller)
  })
  expect_share_dependency_values(plain_list_transform_data(), reads, 7)
  expect_identical(probe$calls, 48L)
})

list_accessor_read <- function(accessor, body = quote(sum(prior)),
                               remove = quote(rm(prior)), before = quote(NULL)) {
  substitute((function(prior) {
    BEFORE
    callbacks <- list(dependent = function() BODY, safe = function() 7)
    REMOVE
    ACCESSOR()
  })(11), list(ACCESSOR = accessor, BODY = body, REMOVE = remove,
               BEFORE = before))
}

test_that("literal getElement selects the same closure as exact brackets", {
  data <- plain_list_transform_data()
  dependent <- alist(
    callbacks[["dependent"]],
    getElement(callbacks, "dependent"),
    base::getElement(name = "dependent", object = callbacks)
  )
  expect_share_dependencies(data, lapply(dependent, list_accessor_read))
  expect_share_dependency_values(data, lapply(
    dependent, list_accessor_read, remove = quote(NULL)
  ), 11)
  expect_share_dependency_values(data, lapply(alist(
    callbacks[["safe"]], getElement(callbacks, "safe")
  ), list_accessor_read), 7)
})

test_that("literal .subset2 selects the same closure as exact brackets", {
  data <- plain_list_transform_data()
  dependent <- alist(
    callbacks[[1L]],
    .subset2(callbacks, 1L),
    base::.subset2(x = callbacks, key = "dependent"),
    .subset2(callbacks, "dependent", exact = TRUE)
  )
  expect_share_dependencies(data, lapply(dependent, list_accessor_read))
  expect_share_dependency_values(data, lapply(
    dependent, list_accessor_read, remove = quote(NULL)
  ), 11)
  expect_share_dependency_values(data, lapply(alist(
    callbacks[[2L]], .subset2(callbacks, 2L)
  ), list_accessor_read), 7)
})

test_that("plain list accessors preserve ordinary function lookup", {
  data <- plain_list_transform_data()
  for (name in c("getElement", ".subset2")) {
    caller <- rlang::env()
    caller[[name]] <- function(...) function() 7
    bare <- rlang::call2(name, quote(callbacks), "dependent")
    qualified <- rlang::call2(name, quote(callbacks), "dependent", .ns = "base")
    read <- rlang::new_quosure(list_accessor_read(bare), caller)
    expect_share_dependency_values(data, list(read), 7)
    reads <- list(
      rlang::new_quosure(list_accessor_read(qualified), caller),
      list_accessor_read(bare, before = rlang::call2("<-", rlang::sym(name), 0))
    )
    expect_share_dependencies(data, reads)

    caller[[name]] <- 0L
    computed <- rlang::call2(rlang::call2("(", rlang::sym(name)),
                             quote(callbacks), "dependent")
    read <- rlang::new_quosure(list_accessor_read(computed), caller)
    expect_error(
      summarize_with_margins(
        data, value = sum(.data$x), prior = share_of_total(value),
        observed = !!read, .grouping = rollup("g")
      ),
      "attempt to apply non-function"
    )
  }
})

test_that("plain list accessor planning leaves caller code unevaluated", {
  data <- plain_list_transform_data()
  probe <- rlang::env(invoked = 0L, keys = 0L)
  key <- function() {
    probe$keys <- probe$keys + 1L
    "safe"
  }
  caller <- rlang::env(key = key, probe = probe)
  body <- quote({
    probe$invoked <- probe$invoked + 1L
    sum(prior)
  })
  expect_share_dependencies(data, lapply(alist(
    getElement(callbacks, "dependent"), .subset2(callbacks, "dependent")
  ), list_accessor_read, body = body))
  reads <- lapply(alist(
    getElement(callbacks, key()), .subset2(callbacks, key())
  ), function(accessor) {
    rlang::new_quosure(list_accessor_read(accessor, body = body), caller)
  })
  expect_share_dependency_values(data, reads, 7)
  expect_identical(probe$invoked, 0L)
  expect_identical(probe$keys, 24L)
})
