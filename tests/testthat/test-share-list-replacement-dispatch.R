list_replacement_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

unchanged_list_replacements <- function(original, replacement) {
  cases <- list(
    alist(`$<-` <- function(x, name, value) x,
          carrier$read <- function() REPLACEMENT),
    alist(`[<-` <- function(x, i, value) x,
          carrier[1L] <- list(function() REPLACEMENT)),
    alist(`[[<-` <- function(i, x, value) x,
          carrier[[i = 1L]] <- function() REPLACEMENT)
  )
  lapply(cases, function(parts) {
    write <- do.call(substitute, list(parts[[2L]],
                                      list(REPLACEMENT = replacement)))
    substitute((function(prior) {
      SETTER
      carrier <- list(read = function() ORIGINAL)
      WRITE
      rm(prior)
      carrier$read()
    })(1), list(SETTER = parts[[1L]], ORIGINAL = original, WRITE = write))
  })
}

test_that("local replacements can retain the original list member", {
  data <- list_replacement_data()
  expect_share_dependencies(
    data, unchanged_list_replacements(quote(sum(prior)), 7)
  )
  expect_share_dependency_values(
    data, unchanged_list_replacements(7, quote(sum(prior))), expected = 7
  )
})

test_that("an unknown replacement does not establish a list member dependency", {
  data <- list_replacement_data()
  probe <- rlang::env(calls = 0L)
  read <- rlang::quo((function(prior) {
    `$<-` <- function(x, name, value) {
      assign("calls", probe$calls + 1L, envir = probe)
      base::`$<-`(x, "read", value)
    }
    carrier <- list(read = function() sum(prior))
    carrier$read <- function() 7
    rm(prior)
    carrier$read()
  })(1))
  expect_share_dependency_values(data, list(read), expected = 7)
  expect_identical(probe$calls, 12L)
  expect_share_dependency_values(data, alist((function(prior) {
    `[[<-` <- function(x, i, value) i
    carrier <- list(read = function() sum(prior))
    carrier[[i = list(read = function() 7)]] <- function() sum(prior)
    rm(prior)
    carrier$read()
  })(1)), expected = 7)
})

test_that("base list replacements follow function and namespace lookup", {
  expect_share_dependencies(list_replacement_data(), alist(
    (function(prior) {
      `$<-` <- 0
      carrier <- list(read = function() 7)
      carrier$read <- function() sum(prior)
      rm(prior)
      carrier$read()
    })(1),
    (function(prior) {
      `$<-` <- function(x, name, value) x
      carrier <- list(read = function() 7)
      base::`$`(carrier, read) <- function() sum(prior)
      rm(prior)
      carrier$read()
    })(1)
  ))
})
