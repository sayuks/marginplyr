singleton_binding_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
}

singleton_binding_reads <- function() {
  alist(
    (function(prior) {
      h <- function() 7
      for (i in 1L) h <- function() sum(prior)
      rm(prior)
      h()
    })(11),
    (function(prior) {
      callback <- function() sum(prior)
      h <- function() 7
      for (i in (1L)) {
        h <- callback
      }
      rm(prior)
      h()
    })(11)
  )
}

test_that("singleton closure bindings retain eager share dependencies", {
  expect_share_dependencies(singleton_binding_data(), singleton_binding_reads())
  expect_share_dependencies(tibble::as_tibble(singleton_binding_data()),
                            singleton_binding_reads())
})

singleton_binding_controls <- function() {
  alist(
    (function(prior) {
      h <- function() 7
      for (i in 1L) h <- function() sum(prior)
      h()
    })(7),
    (function(prior) {
      h <- function() sum(prior)
      for (i in 1L) {
        h <- function() 7
      }
      rm(prior)
      h()
    })(11),
    (function(prior) {
      h <- function() 7
      for (i in NULL) h <- function() sum(prior)
      rm(prior)
      h()
    })(11)
  )
}

expect_singleton_controls <- function(data) {
  for (kind in c("parent", "total")) {
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (read in singleton_binding_controls()) {
      forward <- summarize_with_margins(
        data, value = sum(.data$x), prior = !!share, observed = !!read,
        .grouping = rollup("g")
      )
      reverse <- summarize_with_margins(
        data, prior = sum(.data$x), value = !!read, ratio = !!share,
        .grouping = rollup("g")
      )
      if (!is.data.frame(forward)) forward <- dplyr::collect(forward)
      if (!is.data.frame(reverse)) reverse <- dplyr::collect(reverse)
      expect_equal(forward$observed, c(7, 7, 7))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$value, c(7, 7, 7))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
}

test_that("singleton bindings preserve eager legal controls and input", {
  for (data in list(singleton_binding_data(),
                    tibble::as_tibble(singleton_binding_data()))) {
    before <- serialize(data, NULL)
    expect_singleton_controls(data)
    expect_identical(serialize(data, NULL), before)
  }
})

test_that("data.table singleton bindings preserve dependencies and values", {
  skip_if_suggest_absent("data.table")
  data <- data.table::as.data.table(singleton_binding_data())
  before <- serialize(data, NULL)
  expect_share_dependencies(data, singleton_binding_reads())
  expect_singleton_controls(data)
  expect_identical(serialize(data, NULL), before)
})

test_that("singleton bindings preserve loop exits and caller lookup", {
  for (exit in alist(break, next, eval(parse(text = "break")))) {
    read <- substitute((function(prior) {
      h <- function() 7
      for (i in 1L) {
        EXIT
        h <- function() sum(prior)
      }
      rm(prior)
      h()
    })(11), list(EXIT = exit))
    expect_share_dependency_values(singleton_binding_data(), list(read), 7)
  }
  read <- quote((function(prior) {
    `for` <- function(...) NULL
    h <- function() 7
    for (i in 1L) h <- function() sum(prior)
    rm(prior)
    h()
  })(11))
  expect_share_dependency_values(singleton_binding_data(), list(read), 7)

  caller <- rlang::env(`(` = function(value) {
    if (identical(value, 1L)) NULL else value
  })
  read <- quote((function(prior) {
    h <- function() 7
    for (i in (1L)) h <- function() sum(prior)
    rm(prior)
    h()
  })(11))
  expect_share_dependency_values(
    singleton_binding_data(), list(rlang::new_quosure(read, caller)), 7
  )
})

test_that("singleton binding planning never invokes caller closures", {
  probe <- rlang::env(invoked = 0L, forced = 0L)
  read <- quote((function(prior) {
    h <- function() 7
    for (i in 1L) h <- function() {
      probe$invoked <- probe$invoked + 1L
      sum(prior)
    }
    rm(prior)
    h()
  })({
    probe$forced <- probe$forced + 1L
    11
  }))
  expect_share_dependencies(singleton_binding_data(), list(read))
  expect_identical(probe$invoked, 0L)
  expect_identical(probe$forced, 0L)
})

test_that("dtplyr singleton bindings refuse dependencies without execution", {
  skip_if_suggest_absent("dtplyr")
  probe <- rlang::env(read = 0L)
  observe <- function(x) {
    probe$read <- probe$read + 1L
    x
  }
  source <- singleton_binding_data()
  before <- serialize(source, NULL)
  data <- dtplyr::lazy_dt(source, immutable = TRUE)
  data <- dplyr::filter(data, observe(x) > 0)
  expect_share_dependencies(data, singleton_binding_reads())
  expect_identical(probe$read, 0L)
  expect_identical(serialize(source, NULL), before)
  expect_equal(dplyr::collect(data)$x, c(1, 3))
  expect_identical(probe$read, 1L)
})

test_that("dtplyr singleton controls preserve values at explicit collection", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::as.data.table(singleton_binding_data())
  before <- serialize(source, NULL)
  data <- dtplyr::lazy_dt(source, immutable = TRUE)
  expect_singleton_controls(data)
  expect_identical(serialize(source, NULL), before)
})

test_that("legal dtplyr singleton closures run only at collection", {
  skip_if_suggest_absent("dtplyr")
  probe <- rlang::env(invoked = 0L)
  for (kind in c("parent", "total")) {
    probe$invoked <- 0L
    share <- rlang::call2(paste0("share_of_", kind), quote(value))
    result <- summarize_with_margins(
      dtplyr::lazy_dt(singleton_binding_data(), immutable = TRUE),
      value = (function() {
        h <- function() 0
        for (i in 1L) h <- function() {
          probe$invoked <- probe$invoked + 1L
          7
        }
        h()
      })(),
      ratio = !!share, .grouping = rollup("g")
    )
    expect_identical(probe$invoked, 0L)
    observed <- dplyr::collect(result)
    expect_equal(observed$value, c(7, 7, 7))
    expect_equal(observed$ratio, c(1, 1, 1))
    expect_identical(probe$invoked, 3L)
  }
})
