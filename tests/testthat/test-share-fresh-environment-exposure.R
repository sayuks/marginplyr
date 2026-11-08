fresh_env_exposure_reads <- function() {
  alist(
    (function(prior) {
      box <- list2env(list(read = function() sum(prior)))
      invisible(box)
      rm(prior)
      box$read()
    })(11),
    (function(prior) {
      box <- list2env(list(read = function() sum(prior)))
      alias <- box
      rm(prior)
      alias$read()
    })(11)
  )
}

test_that("unchanged fresh environment exposure retains share dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  before <- serialize(data, NULL)
  expect_share_dependencies(data, fresh_env_exposure_reads())
  expect_identical(serialize(data, NULL), before)
})

fresh_env_exposure_controls <- function() {
  alist(
    (function(prior) {
      box <- list2env(list(read = function() sum(prior)))
      invisible(box)
      box$read()
    })(11),
    (function(prior) {
      box <- list2env(list(read = function() sum(prior)))
      alias <- box
      alias$read()
    })(11)
  )
}

fresh_env_replacement_controls <- function() {
  alist(
    (function(prior) {
      box <- list2env(list(read = function() sum(prior)))
      invisible(box)
      box$read <- function() 7
      rm(prior)
      box$read()
    })(11),
    (function(prior) {
      box <- list2env(list(read = function() sum(prior)))
      alias <- box
      alias$read <- function() 7
      rm(prior)
      box$read()
    })(11),
    (function(prior) {
      box <- list2env(list(read = function() sum(prior)))
      alias <- box
      # A nested data.table expression reads the data column by its bare name.
      if (sum(x) > 0) { # nolint: object_usage_linter.
        alias <- list2env(list(read = function() sum(prior)))
      }
      second <- alias
      second$read <- function() 7
      rm(prior)
      alias$read()
    })(11),
    (function(prior) {
      box <- list2env(list(read = function() sum(prior)))
      other <- list2env(list(read = function() 7))
      box$read <- function() sum(prior)
      rm(prior)
      other$read()
    })(11)
  )
}

test_that("exposed fresh environments preserve independent lexical reads", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  expect_share_dependency_values(
    data, fresh_env_exposure_controls(), 11
  )
  expect_share_dependency_values(
    data, fresh_env_replacement_controls(), 7
  )
})

test_that("immutable dtplyr refuses exposed fresh environment dependencies", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(10, 20)
  )
  before <- serialize(data, NULL)
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  expect_share_dependencies(input, fresh_env_exposure_reads())
  expect_identical(serialize(data, NULL), before)
})

test_that("immutable dtplyr retains legal exposed environment values", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(10, 20)
  )
  before <- serialize(data, NULL)
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  controls <- c(fresh_env_exposure_controls(),
                fresh_env_replacement_controls())
  expected <- c(11, 11, 7, 7, 7, 7)
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (index in seq_along(controls)) {
      read <- controls[[index]]
      forward <- summarize_with_margins(
        input, value = sum(.data$x), prior = !!helper, observed = !!read,
        .grouping = rollup("g")
      )
      reverse <- summarize_with_margins(
        input, prior = sum(.data$x), value = !!read, ratio = !!helper,
        .grouping = rollup("g")
      )
      forward <- dplyr::collect(forward)
      reverse <- dplyr::collect(reverse)
      expect_equal(forward$observed, rep(expected[[index]], 3L))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$value, rep(expected[[index]], 3L))
      expect_equal(reverse$ratio, c(1, 1, 1))
    }
  }
  expect_identical(serialize(data, NULL), before)
})

test_that("fresh environment analysis never runs caller code", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(10, 20)
  )
  before <- serialize(data, NULL)
  probe <- new.env()
  probe$calls <- 0L
  read <- quote((function(prior) {
    box <- list2env(list(read = function() {
      probe$calls <- probe$calls + 1L
      sum(prior)
    }))
    invisible(box)
    rm(prior)
    box$read()
  })(11))
  expect_share_dependencies(data, list(read))
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  expect_share_dependencies(input, list(read))
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    result <- summarize_with_margins(
      input, value = (function(prior) {
        box <- list2env(list(read = function() {
          probe$calls <- probe$calls + 1L
          sum(prior)
        }))
        alias <- box
        alias$read()
      })(11), p = !!helper, .grouping = rollup("g")
    )
    expect_s3_class(result, "dtplyr_step")
    expect_identical(probe$calls, 0L)
    collected <- dplyr::collect(result)
    expect_equal(collected$value, c(11, 11, 11))
    expect_equal(collected$p, c(1, 1, 1))
    probe$calls <- 0L
  }
  expect_identical(serialize(data, NULL), before)
})

conditional_fresh_env_reads <- function() {
  alist(
    (function(prior) {
      box <- list2env(list(read = function() 7))
      alias <- box
      # A nested data.table expression reads the data column by its bare name.
      if (sum(x) > 0) { # nolint: object_usage_linter.
        alias <- list2env(list(read = function() sum(prior)))
      }
      box$read <- function() 7
      rm(prior)
      alias$read()
    })(11),
    (function(prior) {
      box <- list2env(list(read = function() sum(prior)))
      alias <- list2env(list(read = function() sum(prior)))
      # A nested data.table expression reads the data column by its bare name.
      if (sum(x) > 0) alias <- box # nolint: object_usage_linter.
      box$read <- function() 7
      rm(prior)
      alias$read()
    })(11),
    (function(prior) {
      box1 <- list2env(list(read = function() sum(prior)))
      box2 <- list2env(list(read = function() sum(prior)))
      a <- box1
      b <- box1
      # These conditions read the data column in the nested data.table call.
      if (sum(x) > 0) a <- box2 # nolint: object_usage_linter.
      # The separate condition does not establish that a and b are aliases.
      if (sum(x) < 0) b <- box2 # nolint: object_usage_linter.
      b$read <- function() 7
      rm(prior)
      a$read()
    })(11)
  )
}

test_that("conditional fresh aliases retain every possible field dependency", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  expect_share_dependencies(data, conditional_fresh_env_reads())
})

test_that("dtplyr conditional aliases are refused without source reads", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(10, 20)
  )
  before <- serialize(data, NULL)
  probe <- rlang::env(calls = 0L)
  observe <- function(x) {
    probe$calls <- probe$calls + 1L
    x
  }
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  input <- dplyr::filter(input, observe(x) > 0)
  expect_share_dependencies(input, conditional_fresh_env_reads())
  expect_identical(probe$calls, 0L)
  expect_identical(serialize(data, NULL), before)
})

conditional_factory_read <- function(kind, probe) {
  force(probe)
  first <- if (kind == "retained") {
    quote(function(retained = 11) {
      probe$factories <- probe$factories + 1L
      function() {
        probe$closures <- probe$closures + 1L
        retained
      }
    })
  } else {
    quote(function() {
      probe$factories <- probe$factories + 1L
      function() {
        probe$closures <- probe$closures + 1L
        7
      }
    })
  }
  second <- switch(kind,
    dependent = quote(function() {
      probe$factories <- probe$factories + 1L
      function() {
        probe$closures <- probe$closures + 1L
        sum(prior)
      }
    }),
    retained = quote(function(prior = 11) {
      probe$factories <- probe$factories + 1L
      function() {
        probe$closures <- probe$closures + 1L
        sum(prior)
      }
    }),
    independent = quote(function() {
      probe$factories <- probe$factories + 1L
      function() {
        probe$closures <- probe$closures + 1L
        7 + 0
      }
    })
  )
  predicate <- quote(sum(x) > 0)
  rlang::quo((function(prior) {
    box <- list2env(list(make = !!first))
    alias <- box
    if (!!predicate) alias <- list2env(list(make = !!second))
    box$make <- !!first
    made <- alias$make()
    rm(prior)
    made()
  })(11))
}

test_that("conditional factories retain returned closure dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  probe <- rlang::env(factories = 0L, closures = 0L)
  read <- conditional_factory_read("dependent", probe)
  expect_share_dependencies(data, list(read))
  expect_identical(probe$factories, 0L)
  expect_identical(probe$closures, 0L)
})

test_that("dtplyr conditional factories refuse dependencies before execution", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(10, 20)
  )
  before <- serialize(data, NULL)
  probe <- rlang::env(factories = 0L, closures = 0L, reads = 0L)
  observe <- function(x) {
    probe$reads <- probe$reads + 1L
    x
  }
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  input <- dplyr::filter(input, observe(x) > 0)
  read <- conditional_factory_read("dependent", probe)
  expect_share_dependencies(input, list(read))
  expect_identical(probe$factories, 0L)
  expect_identical(probe$closures, 0L)
  expect_identical(probe$reads, 0L)
  expect_identical(serialize(data, NULL), before)
})

test_that("factory alternatives retain independent and captured values", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  probe <- rlang::env(factories = 0L, closures = 0L)
  for (control in c("independent", "retained")) {
    read <- conditional_factory_read(control, probe)
    expected <- if (control == "retained") 11 else 7
    expect_share_dependency_values(data, list(read), expected)
  }
  expect_gt(probe$factories, 0L)
  expect_gt(probe$closures, 0L)
})

test_that("dtplyr factory alternatives defer captured and independent values", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(10, 20)
  )
  before <- serialize(data, NULL)
  probe <- rlang::env(factories = 0L, closures = 0L, reads = 0L)
  observe <- function(x) {
    probe$reads <- probe$reads + 1L
    x
  }
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  input <- dplyr::filter(input, observe(x) > 0)
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (control in c("independent", "retained")) {
      read <- conditional_factory_read(control, probe)
      expected <- if (control == "retained") 11 else 7
      forward <- summarize_with_margins(
        input, value = sum(.data$x), prior = !!helper, observed = !!read,
        .grouping = rollup("g")
      )
      reverse <- summarize_with_margins(
        input, prior = sum(.data$x), value = !!read, ratio = !!helper,
        .grouping = rollup("g")
      )
      expect_identical(probe$factories, 0L)
      expect_identical(probe$closures, 0L)
      expect_identical(probe$reads, 0L)
      forward <- dplyr::collect(forward)
      reverse <- dplyr::collect(reverse)
      expect_equal(forward$observed, rep(expected, 3L))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_equal(reverse$value, rep(expected, 3L))
      expect_equal(reverse$ratio, c(1, 1, 1))
      expect_gt(probe$factories, 0L)
      expect_gt(probe$closures, 0L)
      expect_gt(probe$reads, 0L)
      probe$factories <- probe$closures <- probe$reads <- 0L
    }
  }
  expect_identical(serialize(data, NULL), before)
})
