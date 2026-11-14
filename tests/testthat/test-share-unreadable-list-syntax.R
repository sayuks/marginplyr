unreadable_list_data <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3), prior = c(10, 20))
}

# Malformed list syntax is left to R: the summary is planned, and the error is
# R's own, raised by the call it names while dplyr evaluates the summary.
expect_share_evaluation_errors <- function(data, reads) {
  for (kind in c("parent", "total")) {
    for (read in reads) {
      share <- rlang::call2(paste0("share_of_", kind), quote(source))
      forward <- expect_error(summarize_with_margins(
        data, source = sum(.data$x), prior = !!share, observed = !!read,
        .grouping = rollup("g")
      ))
      share <- rlang::call2(paste0("share_of_", kind), quote(observed))
      reverse <- expect_error(summarize_with_margins(
        data, prior = sum(.data$x), observed = !!read, ratio = !!share,
        .grouping = rollup("g")
      ))
      for (cnd in list(forward, reverse)) {
        expect_false(inherits(cnd, "marginplyr_error"))
        expect_s3_class(cnd$parent, "error")
        expect_false(inherits(cnd$parent, "rlang_error"))
      }
    }
  }
}

test_that("malformed member selectors hold no closure", {
  # Each selector fails when R evaluates it; the handler supplies the closure
  # the summary calls.
  expect_share_dependency_values(unreadable_list_data(), alist(
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      held <- tryCatch(getElement(carrier, ), error = function(cnd) {
        function() 7
      })
      rm(prior)
      held()
    })(1),
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      held <- tryCatch(carrier[["read", other = TRUE]], error = function(cnd) {
        function() 7
      })
      rm(prior)
      held()
    })(1),
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      held <- tryCatch(carrier[[]], error = function(cnd) function() 7)
      rm(prior)
      held()
    })(1),
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      held <- tryCatch(carrier[[c("read", )]], error = function(cnd) {
        function() 7
      })
      rm(prior)
      held()
    })(1),
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      held <- tryCatch(carrier[[c("read", unknown())]], error = function(cnd) {
        function() 7
      })
      rm(prior)
      held()
    })(1)
  ), 7)
})

test_that("member selectors the walk cannot read leave the member opaque", {
  expect_share_dependency_values(unreadable_list_data(), alist(
    (function(prior) {
      carrier <- list(read = function() 7)
      held <- .subset2(carrier, exact = TRUE, "read")
      rm(prior)
      held()
    })(1),
    (function(prior) {
      carrier <- list(read = function() 7)
      held <- carrier[["read", exact = NA]]
      rm(prior)
      held()
    })(1),
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      held <- carrier[[NA_character_]]
      rm(prior)
      if (is.null(held)) 7 else held()
    })(1),
    (function(prior) {
      carrier <- list(function() 8, function() 7)
      held <- carrier[[3 - 1]]
      rm(prior)
      held()
    })(1),
    (function(prior) {
      carrier <- list(nested = list(read = function() 7))
      held <- carrier[[c("nested", "read", recursive = TRUE)]]
      rm(prior)
      held()
    })(1)
  ), 7)
})

test_that("a quoted dollar name selects the written member", {
  expect_share_dependencies(unreadable_list_data(), alist(
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      held <- `$`(carrier, "read")
      rm(prior)
      held()
    })(1)
  ))
})

test_that("list constructors the walk cannot read hold no members", {
  expect_share_dependency_values(unreadable_list_data(), alist(
    (function(prior) {
      carrier <- c(list(function() 7), recursive = FALSE)
      rm(prior)
      carrier[[1L]]()
    })(1),
    (function(prior) {
      carrier <- Filter(is.numeric, list(function() sum(prior)))
      rm(prior)
      if (length(carrier) == 0L) 7 else carrier[[1L]]()
    })(1),
    (function(prior) {
      carrier <- Filter(is.function, list(function() 7, function() 8))
      rm(prior)
      carrier[[1L]]()
    })(1)
  ), 7)
  expect_share_evaluation_errors(unreadable_list_data(), alist(
    (function(prior) {
      carrier <- c(list(function() 7), )
      7
    })(1),
    (function(prior) {
      carrier <- append(values = list(function() 7))
      7
    })(1),
    (function(prior) {
      carrier <- append(unknown(), list(function() 7))
      7
    })(1),
    (function(prior) {
      carrier <- append(list(function() 7), list(function() 8), after = -1)
      7
    })(1),
    (function(prior) {
      carrier <- unknown()[1L]
      7
    })(1),
    (function(prior) {
      carrier <- list(function() 7)[1i]
      7
    })(1),
    (function(prior) {
      carrier <- list(function() 7, function() 8)[c(-1, 2)]
      7
    })(1),
    (function(prior) {
      carrier <- Filter(is.function, list(function() 7), extra)
      7
    })(1),
    {
      carrier <- list(...)
      7
    }
  ))
})

test_that("list constructors keep the members they can read", {
  expect_share_dependencies(unreadable_list_data(), alist(
    (function(prior) {
      carrier <- list(function() sum(prior))[]
      rm(prior)
      carrier[[1L]]()
    })(1),
    (function(prior) {
      # The base callee fixes the binding name under test.
      Filter <- function(f, x) x # nolint: object_name_linter.
      carrier <- Filter(is.function, list(function() sum(prior)))
      rm(prior)
      carrier[[1L]]()
    })(1),
    (function(prior, ...) {
      carrier <- (function() list(...))()
      rm(prior)
      carrier[[1L]]()
    })(1, function() sum(prior))
  ))
})

test_that("list writes the walk cannot apply leave R to report them", {
  expect_share_evaluation_errors(unreadable_list_data(), alist(
    (function(prior) {
      1$read <- function() 7
      7
    })(1),
    (function(prior) {
      carrier <- list(function() 7)
      carrier[1, 2] <- list(function() 8)
      7
    })(1),
    (function(prior) {
      carrier <- list(function() 7, function() 8)
      carrier[c(-1, 2)] <- list(function() 9)
      7
    })(1),
    (function(prior) {
      carrier <- list(function() 7)
      carrier[1] <- unknown()
      7
    })(1),
    (function(prior) {
      carrier <- list(read = function() 7)
      carrier[[unknown()]] <- function() 8
      7
    })(1),
    (function(prior) {
      carrier <- list(nested = list(read = function() 7))
      carrier[[c("missing", "read")]] <- function() 8
      7
    })(1),
    (function(prior) {
      carrier <- list(nested = list(read = function() 7))
      carrier[[c("nested", "missing", "read")]] <- function() 8
      7
    })(1),
    (function(prior) {
      carrier <- list(read = function() 7)
      names(carrier, 1) <- "other"
      7
    })(1),
    (function(prior) {
      carrier <- list(read = function() 7)
      attributes(carrier) <- unknown()
      7
    })(1),
    (function(prior) {
      carrier <- list(read = function() 7)
      names(carrier) <- unknown()
      7
    })(1),
    (function(prior) {
      carrier <- list(read = function() 7)
      names(carrier) <- c("one", "two")
      7
    })(1)
  ))
  # A literal mixed-sign index, as opposed to the `c()` call above.
  mixed <- c(-1, 2)
  expect_share_evaluation_errors(unreadable_list_data(), rlang::exprs(
    (function(prior) {
      carrier <- list(function() 7, function() 8)
      carrier[!!mixed] <- list(function() 9)
      7
    })(1)
  ))
})

test_that("list writes with unreadable keys or names leave members opaque", {
  key <- factor("read")
  expect_share_dependency_values(unreadable_list_data(), c(
    rlang::exprs((function(prior) {
      carrier <- list(read = function() 7)
      carrier[[!!key]] <- function() 7
      rm(prior)
      carrier$read()
    })(1)),
    alist(
      (function(prior) {
        carrier <- list(function() 7)
        carrier[NA] <- list(function() sum(prior))
        rm(prior)
        carrier[[1L]]()
      })(1),
      (function(prior) {
        carrier <- list(read = function() 7)
        attributes(carrier) <- list(names = "other", names = "read")
        rm(prior)
        carrier[[1L]]()
      })(1)
    )
  ), 7)
})

test_that("list writes the walk applies keep the members they leave", {
  expect_share_dependency_values(unreadable_list_data(), alist(
    (function(prior) {
      carrier <- list(function() sum(prior), function() 7)
      carrier[1] <- NULL
      rm(prior)
      carrier[[1L]]()
    })(1),
    (function(prior, choose) {
      fields <- new.env()
      fields$make <- function() list(read = function() sum(prior))
      if (choose) {
        fields$make <- function() list(read = function() sum(prior), other = 1)
      }
      carrier <- list(nested = fields$make())
      carrier[[c("nested", "read")]] <- function() 7
      rm(prior)
      carrier$nested$read()
    })(1, TRUE)
  ), 7)
  expect_share_dependencies(unreadable_list_data(), alist(
    (function(prior) {
      carrier <- list(function() sum(prior))
      carrier[1] <- list()
      rm(prior)
      carrier[[1L]]()
    })(1),
    (function(prior) {
      carrier <- list(read = function() 7)
      carrier$"read" <- function() sum(prior)
      rm(prior)
      carrier$read()
    })(1),
    (function(prior) {
      carrier <- list(read = function() sum(prior))
      attr(carrier, "other") <- 1
      rm(prior)
      carrier$read()
    })(1),
    (function(prior, choose) {
      fields <- new.env()
      fields$make <- function() list(read = function() 7)
      if (choose) fields$make <- function() list(read = function() 8, other = 1)
      carrier <- list(nested = fields$make())
      carrier[[c("nested", "read")]] <- function() sum(prior)
      rm(prior)
      carrier$nested$read()
    })(1, TRUE)
  ))
})

test_that("unproven local member setters do not keep the carrier", {
  # Each setter replaces the carrier, so the member the summary calls is the
  # one the setter returned and not the one written before it.
  expect_share_dependency_values(unreadable_list_data(), alist(
    (function(prior, choose) {
      setters <- new.env()
      setters$set <- function(x, name, value) x
      if (choose) {
        setters$set <- function(x, name, value) list(read = function() 7)
      }
      `$<-` <- setters$set
      carrier <- list(read = function() sum(prior))
      carrier$other <- 1
      rm(prior)
      carrier$read()
    })(1, TRUE),
    (function(prior) {
      # The formal shadows `{`, so the body calls it rather than the base
      # primitive.
      `$<-` <- function(x, name, value,
                        `{` = function(...) list(read = function() 7)) {
        x
      }
      carrier <- list(read = function() sum(prior))
      carrier$other <- 1
      rm(prior)
      carrier$read()
    })(1)
  ), 7)
})
