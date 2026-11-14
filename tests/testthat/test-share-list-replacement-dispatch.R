list_replacement_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
}

unchanged_list_replacements <- function(original, replacement) {
  cases <- as.list(quote(list(
    alist(
      `$<-` <- function(x, name, value) {
        x
      },
      carrier$read <- function() REPLACEMENT
    ),
    alist(`[<-` <- function(x, i, value) x,
          carrier[1L] <- list(function() REPLACEMENT)),
    alist(`[[<-` <- function(i, x, value) x,
          carrier[[i = 1L]] <- function() REPLACEMENT)
  )))[-1L]
  lapply(cases, function(parts) {
    write <- do.call(substitute, list(parts[[3L]],
                                      list(REPLACEMENT = replacement)))
    substitute((function(prior) {
      SETTER
      carrier <- list(read = function() ORIGINAL)
      WRITE
      rm(prior)
      carrier$read()
    })(1), list(SETTER = parts[[2L]], ORIGINAL = original, WRITE = write))
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

# The walk cannot prove these setters return their input, so after the
# removal the carrier may still hold the closure reading the earlier alias
# and the source refuses (#1666).
test_that("an unknown replacement leaves list dependencies unproven", {
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
  expect_share_dependencies(data, list(read))
  expect_identical(probe$calls, 0L)
  expect_share_dependencies(data, alist((function(prior) {
    `[[<-` <- function(x, i, value) i
    carrier <- list(read = function() sum(prior))
    carrier[[i = list(read = function() 7)]] <- function() sum(prior)
    rm(prior)
    carrier$read()
  })(1)))
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

# Neither setter is proven to return its input, so the original member may
# survive the removal and the source refuses (#1666).
test_that("unproven member and name setters may keep the original member", {
  reads <- alist(
    (function(prior) {
      `[<-` <- function(x, i, ..., value) base::`[<-`(x, i, value = value)
      carrier <- list(read = function() sum(prior))
      carrier[1L, extra = TRUE] <- list(function() 7)
      rm(prior)
      carrier$read()
    })(1),
    (function(prior) {
      `names<-` <- function(x, value) base::`names<-`(x, c("other", "read"))
      carrier <- list(read = function() sum(prior), other = function() 7)
      names(carrier) <- c("read", "other")
      rm(prior)
      carrier$read()
    })(1)
  )
  expect_share_dependencies(list_replacement_data(), reads)
})
