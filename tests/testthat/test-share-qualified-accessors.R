test_that("qualified literal accessors preserve caller namespace lookup", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  for (operator in c("::", ":::")) {
    calls <- 0L
    dispatcher <- function(package, member) {
      package <- as.character(substitute(package))
      member <- as.character(substitute(member))
      if (identical(package, "base") && identical(member, "getElement")) {
        calls <<- calls + 1L
        return(function(...) 7)
      }
      get(member, envir = asNamespace(package), inherits = FALSE)
    }
    caller <- rlang::env()
    caller[[operator]] <- dispatcher
    read <- rlang::new_quosure(rlang::call2(
      rlang::call2(operator, quote(base), quote(getElement)),
      quote(parent.env(environment())), "prior"
    ), caller)
    for (kind in c("parent", "total")) {
      share <- rlang::call2(paste0("share_of_", kind), quote(source))
      calls <- 0L
      expect_error(
        summarize_with_margins(
          data, source = sum(x), prior = !!share, observed = !!read,
          invalid = share_of_total(absent), .grouping = rollup(g)
        ),
        "unknown preceding ordinary summary `absent`",
        class = "marginplyr_error"
      )
      expect_identical(calls, 0L)

      forward <- summarize_with_margins(
        data, source = sum(x), prior = !!share, observed = !!read,
        .grouping = rollup(g)
      )
      expect_equal(forward$observed, c(7, 7, 7))
      expect_equal(forward$prior, c(0.25, 0.75, 1))
      expect_identical(calls, 3L)

      calls <- 0L
      reverse <- summarize_with_margins(
        data, prior = sum(x), source = !!read, ratio = !!share,
        .grouping = rollup(g)
      )
      expect_equal(reverse$source, c(7, 7, 7))
      expect_equal(reverse$ratio, c(1, 1, 1))
      expect_identical(calls, 3L)
    }
  }
})

test_that("qualified accessor operators honor lexical function lookup", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  reads <- alist(
    {
      `::` <- function(...) function(...) 7
      base::getElement(parent.env(environment()), "prior")
    },
    {
      `::` <- function(...) function(...) 7
      rlang::env_get(environment(), "prior", inherit = TRUE)
    }
  )
  for (operator in c("::", ":::")) {
    for (expr in reads) {
      read <- parse(text = gsub(
        "::", operator, paste(deparse(expr), collapse = "\n"), fixed = TRUE
      ))[[1L]]
      for (kind in c("parent", "total")) {
        share <- rlang::call2(paste0("share_of_", kind), quote(source))
        forward <- summarize_with_margins(
          data, source = sum(x), prior = !!share, observed = !!read,
          .grouping = rollup(g)
        )
        expect_equal(forward$observed, c(7, 7, 7))
        expect_equal(forward$prior, c(0.25, 0.75, 1))

        reverse <- summarize_with_margins(
          data, prior = sum(x), source = !!read, ratio = !!share,
          .grouping = rollup(g)
        )
        expect_equal(reverse$source, c(7, 7, 7))
        expect_equal(reverse$ratio, c(1, 1, 1))
      }
    }
  }
})

test_that("non-function lexical operators retain literal mask dependencies", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  reads <- alist(
    {
      `::` <- 1
      base::getElement(parent.env(environment()), "prior")
    },
    {
      `::` <- 1
      rlang::env_get(environment(), "prior", inherit = TRUE)
    }
  )
  for (operator in c("::", ":::")) {
    for (expr in reads) {
      read <- parse(text = gsub(
        "::", operator, paste(deparse(expr), collapse = "\n"), fixed = TRUE
      ))[[1L]]
      for (kind in c("parent", "total")) {
        share <- rlang::call2(paste0("share_of_", kind), quote(source))
        expect_error(
          summarize_with_margins(
            data, source = sum(x), prior = !!share, observed = !!read,
            .grouping = rollup(g)
          ),
          "Ordinary summaries cannot use an earlier",
          class = "marginplyr_error"
        )
        expect_error(
          summarize_with_margins(
            data, prior = sum(x), source = !!read, ratio = !!share,
            .grouping = rollup(g)
          ),
          "depends on earlier summary alias `prior`",
          class = "marginplyr_error"
        )
      }
    }
  }
})

test_that("unresolved namespace operators are never forced in planning", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  for (operator in c("::", ":::")) {
    for (binding in c("active", "lazy")) {
      calls <- 0L
      caller <- rlang::env()
      if (identical(binding, "active")) {
        makeActiveBinding(operator, function() {
          calls <<- calls + 1L
          get(operator, envir = baseenv())
        }, caller)
      } else {
        delayedAssign(operator, {
          calls <<- calls + 1L
          get(operator, envir = baseenv())
        }, assign.env = caller, eval.env = environment())
      }
      read <- rlang::new_quosure(rlang::call2(
        rlang::call2(operator, quote(base), quote(getElement)),
        quote(parent.env(environment())), "prior"
      ), caller)
      expect_error(
        summarize_with_margins(
          data, source = sum(x), prior = share_of_total(source),
          observed = !!read,
          invalid = share_of_total(absent), .grouping = rollup(g)
        ),
        "unknown preceding ordinary summary `absent`",
        class = "marginplyr_error"
      )
      expect_identical(calls, 0L)
    }
  }
})

test_that("nonlocal operator writes invalidate earlier namespace bindings", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  reads <- alist(
    {
      assign("::", function(...) function(...) 7, envir = caller)
      `::` <- 1
      base::getElement(parent.env(environment()), "prior")
    },
    {
      `::` <<- function(...) function(...) 7
      `::` <- 1
      base::getElement(parent.env(environment()), "prior")
    },
    {
      `::` <- 1
      if (length(x) > 0L) `::` <- function(...) function(...) 7
      base::getElement(parent.env(environment()), "prior")
    }
  )
  for (operator in c("::", ":::")) {
    for (expr in reads) {
      for (kind in c("parent", "total")) {
        caller <- rlang::env()
        caller[[operator]] <- get(operator, envir = baseenv())
        read <- rlang::new_quosure(parse(text = gsub(
          "::", operator, paste(deparse(expr), collapse = "\n"), fixed = TRUE
        ))[[1L]], caller)
        share <- rlang::call2(paste0("share_of_", kind), quote(source))
        forward <- summarize_with_margins(
          data, source = sum(x), prior = !!share, observed = !!read,
          .grouping = rollup(g)
        )
        expect_equal(forward$observed, c(7, 7, 7))
        expect_equal(forward$prior, c(0.25, 0.75, 1))

        caller[[operator]] <- get(operator, envir = baseenv())
        reverse <- summarize_with_margins(
          data, prior = sum(x), source = !!read, ratio = !!share,
          .grouping = rollup(g)
        )
        expect_equal(reverse$source, c(7, 7, 7))
        expect_equal(reverse$ratio, c(1, 1, 1))
      }
    }
  }
})

test_that("literal accessor frames preserve qualified caller functions", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
  external <- rlang::env(prior = 7)
  cases <- list(
    list(quote(base::.subset2(parent.env(environment()), "prior")),
         "base", ".subset2", function(...) 7),
    list(quote(parent.env(base::environment())[["prior", exact = TRUE]]),
         "base", "environment", function() new.env(parent = external)),
    list(quote(base::parent.env(environment())[["prior", exact = FALSE]]),
         "base", "parent.env", function(...) external),
    list(quote(rlang::env_get(environment(), "prior", inherit = TRUE)),
         "rlang", "env_get", function(...) 7),
    list(quote(rlang::env_get(rlang::current_env(), "prior", inherit = TRUE)),
         "rlang", "current_env", function() external),
    list(quote(rlang::env_get(base::environment(), "prior", inherit = TRUE)),
         "base", "environment", function() external),
    list(quote(rlang::env_get(environment(), "prior", inherit = TRUE,
                              default = 7, last = base::emptyenv())),
         "base", "emptyenv", function() parent.frame()),
    list(quote(rlang::env_get(environment(), "prior", inherit = TRUE,
                              default = 7, last = base::baseenv())),
         "base", "baseenv", function() parent.frame()),
    list(quote(rlang::env_get(environment(), "prior", inherit = TRUE,
                              default = 7, last = base::globalenv())),
         "base", "globalenv", function() parent.frame())
  )
  for (case in cases) {
    calls <- 0L
    dispatcher <- function(package, member) {
      package <- as.character(substitute(package))
      member <- as.character(substitute(member))
      if (identical(package, case[[2L]]) && identical(member, case[[3L]])) {
        calls <<- calls + 1L
        return(case[[4L]])
      }
      get(member, envir = asNamespace(package), inherits = FALSE)
    }
    for (operator in c("::", ":::")) {
      inherited <- rlang::env()
      inherited[[operator]] <- dispatcher
      caller <- rlang::new_environment(parent = inherited)
      caller[[operator]] <- 1
      expr <- parse(text = gsub("::", operator, deparse1(case[[1L]]),
                                fixed = TRUE))[[1L]]
      read <- rlang::new_quosure(expr, caller)
      for (kind in c("parent", "total")) {
        share <- rlang::call2(paste0("share_of_", kind), quote(source))
        calls <- 0L
        expect_error(
          summarize_with_margins(
            data, source = sum(x), prior = !!share, observed = !!read,
            invalid = share_of_total(absent), .grouping = rollup(g)
          ),
          "unknown preceding ordinary summary `absent`",
          class = "marginplyr_error"
        )
        expect_identical(calls, 0L)

        forward <- summarize_with_margins(
          data, source = sum(x), prior = !!share, observed = !!read,
          .grouping = rollup(g)
        )
        expect_equal(forward$observed, c(7, 7, 7))
        expect_equal(forward$prior, c(0.25, 0.75, 1))
        expect_identical(calls, 3L)

        calls <- 0L
        reverse <- summarize_with_margins(
          data, prior = sum(x), source = !!read, ratio = !!share,
          .grouping = rollup(g)
        )
        expect_equal(reverse$source, c(7, 7, 7))
        expect_equal(reverse$ratio, c(1, 1, 1))
        expect_identical(calls, 3L)
      }
    }
  }
})
