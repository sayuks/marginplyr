mixed_factory_read <- function(dependent, probe, retained = FALSE,
                               list_first = FALSE, invoke_callable = FALSE) {
  force(probe)
  callable_body <- if (xor(dependent, invoke_callable)) 7 else quote(sum(prior))
  callable <- rlang::expr(function() {
    probe$factories <- probe$factories + 1L
    function() {
      probe$closures <- probe$closures + 1L
      !!callable_body
    }
  })
  body <- if (dependent || retained || invoke_callable) quote(sum(prior)) else 7
  listed <- rlang::expr(function() {
    probe$factories <- probe$factories + 1L
    list(read = function() {
      probe$closures <- probe$closures + 1L
      !!body
    })
  })
  if (retained) listed[[2L]] <- as.pairlist(alist(prior = 11))
  first <- if (list_first) listed else callable
  second <- if (list_first) callable else listed
  predicate <- if (xor(list_first, invoke_callable)) {
    quote(sum(x) < 0)
  } else quote(sum(x) > 0)
  invocation <- rlang::call2(
    if (invoke_callable) quote(made) else quote(made$read)
  )
  rlang::quo((function(prior) {
    box <- list2env(list(make = !!first))
    alias <- box
    if (!!predicate) alias <- list2env(list(make = !!second))
    made <- alias$make()
    rm(prior)
    !!invocation
  })(11))
}

test_that("mixed factory result kinds retain list callback dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  before <- serialize(data, NULL)
  probe <- rlang::env(factories = 0L, closures = 0L)
  reads <- lapply(c(FALSE, TRUE), function(first) {
    mixed_factory_read(TRUE, probe, list_first = first)
  })
  expect_share_dependencies(data, reads)
  expect_identical(probe$factories, 0L)
  expect_identical(probe$closures, 0L)
  expect_identical(serialize(data, NULL), before)
})

test_that("mixed factories leave unused callable bodies deferred", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  probe <- rlang::env(factories = 0L, closures = 0L)
  for (first in c(FALSE, TRUE)) {
    for (retained in c(FALSE, TRUE)) {
      read <- mixed_factory_read(FALSE, probe, retained, first)
      expect_share_dependency_values(data, list(read), if (retained) 11 else 7)
    }
  }
  expect_gt(probe$factories, 0L)
  expect_gt(probe$closures, 0L)
})

test_that("dtplyr mixed factories preserve independent and captured values", {
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
    for (first in c(FALSE, TRUE)) {
      for (retained in c(FALSE, TRUE)) {
        read <- mixed_factory_read(FALSE, probe, retained, first)
        expected <- if (retained) 11 else 7
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
  }
  expect_identical(serialize(data, NULL), before)
})

test_that("dtplyr mixed factory dependencies are refused without execution", {
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
  reads <- lapply(c(FALSE, TRUE), function(first) {
    mixed_factory_read(TRUE, probe, list_first = first)
  })
  expect_share_dependencies(input, reads)
  expect_identical(probe$factories, 0L)
  expect_identical(probe$closures, 0L)
  expect_identical(probe$reads, 0L)
  expect_identical(serialize(data, NULL), before)
})

test_that("invoked mixed factory callables retain their dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  probe <- rlang::env(factories = 0L, closures = 0L)
  reads <- lapply(c(FALSE, TRUE), function(first) {
    mixed_factory_read(TRUE, probe, list_first = first, invoke_callable = TRUE)
  })
  expect_share_dependencies(data, reads)
  expect_identical(probe$factories, 0L)
  expect_identical(probe$closures, 0L)
})

test_that("dtplyr invoked mixed callables retain dependencies without reads", {
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
  reads <- lapply(c(FALSE, TRUE), function(first) {
    mixed_factory_read(TRUE, probe, list_first = first, invoke_callable = TRUE)
  })
  expect_share_dependencies(input, reads)
  expect_identical(probe$factories, 0L)
  expect_identical(probe$closures, 0L)
  expect_identical(probe$reads, 0L)
  expect_identical(serialize(data, NULL), before)
})

partial_list_write_read <- function(dependent, probe, nested = FALSE,
                                    overwrite = FALSE) {
  force(probe)
  body <- if (dependent) quote(sum(prior)) else 7
  small <- rlang::expr(list(read = function() {
    probe$closures <- probe$closures + 1L
    !!body
  }))
  large <- quote(list(read = function() 7, other = function() 7))
  if (nested) {
    small <- rlang::expr(list(cfg = !!small))
    large <- rlang::expr(list(cfg = !!large))
  }
  selected <- if (nested) quote(callbacks$cfg$read) else quote(callbacks$read)
  target <- if (nested) quote(callbacks$cfg[[3L]]) else quote(callbacks[[3L]])
  write <- rlang::call2("<-", target, quote(function() 7))
  replacement <- if (overwrite) {
    rlang::call2("<-", selected, quote(function() 7))
  }
  predicate <- quote(sum(x) < 0)
  rlang::quo((function(prior) {
    box <- list2env(list(cfg = !!small))
    alias <- box
    if (!!predicate) alias <- list2env(list(cfg = !!large))
    callbacks <- alias$cfg
    !!write
    !!replacement
    rm(prior)
    (!!selected)()
  })(11))
}

test_that("partially handled list writes retain unchanged callback proofs", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  before <- serialize(data, NULL)
  probe <- rlang::env(closures = 0L)
  reads <- lapply(c(FALSE, TRUE), function(nested) {
    partial_list_write_read(TRUE, probe, nested)
  })
  expect_share_dependencies(data, reads)
  expect_identical(probe$closures, 0L)
  expect_identical(serialize(data, NULL), before)
})

test_that("dtplyr partially handled list writes refuse before execution", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(10, 20)
  )
  before <- serialize(data, NULL)
  probe <- rlang::env(closures = 0L, reads = 0L)
  observe <- function(x) {
    probe$reads <- probe$reads + 1L
    x
  }
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  input <- dplyr::filter(input, observe(x) > 0)
  reads <- lapply(c(FALSE, TRUE), function(nested) {
    partial_list_write_read(TRUE, probe, nested)
  })
  expect_share_dependencies(input, reads)
  expect_identical(probe$closures, 0L)
  expect_identical(probe$reads, 0L)
  expect_identical(serialize(data, NULL), before)
})

test_that("partial list writes preserve independent replacement values", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  probe <- rlang::env(closures = 0L)
  for (nested in c(FALSE, TRUE)) {
    for (overwrite in c(FALSE, TRUE)) {
      read <- partial_list_write_read(overwrite, probe, nested, overwrite)
      expect_share_dependency_values(data, list(read), 7)
    }
  }
})

test_that("dtplyr partial list writes keep legal values until collection", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(10, 20)
  )
  before <- serialize(data, NULL)
  probe <- rlang::env(closures = 0L, reads = 0L)
  observe <- function(x) {
    probe$reads <- probe$reads + 1L
    x
  }
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  input <- dplyr::filter(input, observe(x) > 0)
  for (kind in c("parent", "total")) {
    helper <- rlang::call2(paste0("share_of_", kind), quote(value))
    for (nested in c(FALSE, TRUE)) {
      for (overwrite in c(FALSE, TRUE)) {
        read <- partial_list_write_read(overwrite, probe, nested, overwrite)
        forward <- summarize_with_margins(
          input, value = sum(.data$x), prior = !!helper, observed = !!read,
          .grouping = rollup("g")
        )
        reverse <- summarize_with_margins(
          input, prior = sum(.data$x), value = !!read, ratio = !!helper,
          .grouping = rollup("g")
        )
        expect_identical(probe$closures, 0L)
        expect_identical(probe$reads, 0L)
        forward <- dplyr::collect(forward)
        reverse <- dplyr::collect(reverse)
        expect_equal(forward$observed, c(7, 7, 7))
        expect_equal(forward$prior, c(0.25, 0.75, 1))
        expect_equal(reverse$value, c(7, 7, 7))
        expect_equal(reverse$ratio, c(1, 1, 1))
        expect_gt(probe$reads, 0L)
        probe$closures <- probe$reads <- 0L
      }
    }
  }
  expect_identical(serialize(data, NULL), before)
})

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

list_alternative_read <- function(kind, selector, dependent, probe,
                                  choices = TRUE, retained = FALSE,
                                  remap = FALSE, overwrite = FALSE) {
  force(probe)
  member <- function(body) {
    rlang::expr(function() {
      probe$closures <- probe$closures + 1L
      !!body
    })
  }
  payload <- function(body, reverse = FALSE) {
    parts <- list(member(body), member(quote(sum(prior))))
    if (selector == "name") {
      names(parts) <- c("read", "other")
      if (reverse) parts <- rev(parts)
    }
    rlang::call2("list", !!!parts)
  }
  first <- payload(if (!choices && dependent) quote(sum(prior)) else 7,
                   reverse = TRUE)
  second <- payload(if (dependent || retained) quote(sum(prior)) else 7)
  if (kind == "factory") {
    wrap <- function(value) {
      body <- rlang::expr({
        probe$factories <- probe$factories + 1L
        !!value
      })
      if (retained) {
        rlang::expr(function(prior = 11) !!body)
      } else {
        rlang::expr(function() !!body)
      }
    }
    first <- wrap(first)
    second <- wrap(second)
  }
  key <- if (kind == "factory") "make" else "cfg"
  initial <- rlang::call2("list", !!!stats::setNames(list(first), key))
  replacement <- rlang::call2("list", !!!stats::setNames(list(second), key))
  predicate <- quote(sum(x) > 0)
  setup <- if (choices) rlang::expr({
    # The injected accessor reads this alias.
    alias <- box # nolint: object_usage_linter.
    if (!!predicate) alias <- list2env(!!replacement)
    !!rlang::call2("<-", rlang::call2("$", quote(box), rlang::sym(key)), first)
  })
  carrier <- if (choices) quote(alias) else quote(box)
  value <- rlang::call2("$", carrier, rlang::sym(key))
  if (kind == "factory") value <- rlang::call2(value)
  remapping <- if (remap) rlang::expr({
    callbacks <- !!value
    callbacks <- c(callbacks)
    callbacks <- append(list(), callbacks)
    callbacks <- rev(rev(callbacks))
    callbacks <- callbacks[c(1L, 2L)]
    callbacks[2L] <- list(
      # This callback is injected into the summary closure binding prior.
      function() sum(prior) # nolint: object_usage_linter.
    )
    names(callbacks) <- c("read", "other")
    !!if (overwrite) quote(callbacks[[1L]] <- function() 7) else NULL
  })
  if (remap) value <- quote(callbacks)
  selection <- if (selector == "name") {
    rlang::call2("$", value, quote(read))
  } else {
    rlang::call2("[[", value, 1L)
  }
  rlang::quo((function(prior) {
    # The injected setup and accessor read this carrier.
    box <- list2env(!!initial) # nolint: object_usage_linter.
    !!setup
    !!remapping
    selected <- !!selection
    rm(prior)
    selected()
  })(11))
}

list_alternative_dependencies <- function(probe) {
  reads <- list()
  for (kind in c("factory", "nested")) {
    for (selector in c("name", "index")) {
      for (choices in c(FALSE, TRUE)) {
        reads <- c(reads, list(list_alternative_read(
          kind, selector, TRUE, probe, choices = choices
        )))
      }
    }
  }
  reads
}

test_that("list candidates retain selected closure dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  probe <- rlang::env(factories = 0L, closures = 0L)
  expect_share_dependencies(data, list_alternative_dependencies(probe))
  expect_identical(probe$factories, 0L)
  expect_identical(probe$closures, 0L)
})

test_that("known list operations preserve alternative dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  probe <- rlang::env(factories = 0L, closures = 0L)
  reads <- lapply(c("factory", "nested"), function(kind) {
    list_alternative_read(kind, "index", TRUE, probe, remap = TRUE)
  })
  expect_share_dependencies(data, reads)
})

test_that("known list remaps and independent replacements retain values", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  probe <- rlang::env(factories = 0L, closures = 0L)
  for (kind in c("factory", "nested")) {
    for (overwrite in c(FALSE, TRUE)) {
      read <- list_alternative_read(kind, "index", overwrite, probe,
                                    remap = TRUE, overwrite = overwrite)
      expect_share_dependency_values(data, list(read), 7)
    }
  }
})

test_that("dtplyr list candidates refuse dependencies without source reads", {
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
  expect_share_dependencies(input, list_alternative_dependencies(probe))
  expect_identical(probe$factories, 0L)
  expect_identical(probe$closures, 0L)
  expect_identical(probe$reads, 0L)
  expect_identical(serialize(data, NULL), before)
})

test_that("list choices preserve literal names, indices and captured formals", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
  probe <- rlang::env(factories = 0L, closures = 0L)
  for (kind in c("factory", "nested")) {
    for (selector in c("name", "index")) {
      read <- list_alternative_read(kind, selector, FALSE, probe)
      expect_share_dependency_values(data, list(read), 7)
      if (kind == "factory") {
        read <- list_alternative_read(kind, selector, FALSE, probe,
                                      retained = TRUE)
        expect_share_dependency_values(data, list(read), 11)
      }
    }
  }
})

test_that("dtplyr list choices preserve independent values at collection", {
  skip_if_suggest_absent("dtplyr")
  data <- data.table::data.table(
    g = c("a", "b"), x = c(1, 3), prior = c(10, 20)
  )
  before <- serialize(data, NULL)
  input <- dtplyr::lazy_dt(data, immutable = TRUE)
  probe <- rlang::env(factories = 0L, closures = 0L)
  for (kind in c("factory", "nested")) {
    for (selector in c("name", "index")) {
      for (retained in if (kind == "factory") c(FALSE, TRUE) else FALSE) {
        read <- list_alternative_read(kind, selector, FALSE, probe,
                                      retained = retained)
        expected <- if (retained) 11 else 7
        for (share in c("parent", "total")) {
          helper <- rlang::call2(paste0("share_of_", share), quote(value))
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
          forward <- dplyr::collect(forward)
          reverse <- dplyr::collect(reverse)
          expect_equal(forward$observed, rep(expected, 3L))
          expect_equal(forward$prior, c(0.25, 0.75, 1))
          expect_equal(reverse$value, rep(expected, 3L))
          expect_equal(reverse$ratio, c(1, 1, 1))
          probe$factories <- probe$closures <- 0L
        }
      }
    }
  }
  expect_identical(serialize(data, NULL), before)
})
