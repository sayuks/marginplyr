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
  expected <- c(11, 11, 7, 7)
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
