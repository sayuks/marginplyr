# Public inspection and execution share the positional constructor contract.
constructor_arg_consumers <- function(data) {
  list(
    function(expr) inspect_grouping(data, .grouping = eval(expr)),
    function(expr) {
      summarize_with_margins(data, n = dplyr::n(), .grouping = eval(expr))
    }
  )
}

# Forwarding dots must preserve both their tags and their promise environments.
forward_constructor_args <- function(constructor, ..., .nested = FALSE) {
  expr <- rlang::call2(constructor, quote(...))
  if (.nested) {
    expr <- rlang::call2("grouping_sets", expr)
  }
  eval(expr)
}

test_that("capture controls are refused without evaluating their values", {
  data <- data.frame(region = c("E", "W"), value = 1:2)
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                    "grouping_spec")
  controls <- c(".named", ".ignore_empty", ".ignore_null", ".unquote_names",
                ".homonyms", ".check_assign")
  for (constructor in constructors) {
    for (tag in controls) {
      args <- setNames(list(quote(stop("must not run"))), tag)
      direct <- as.call(c(list(as.name(constructor)), args))
      forwarded <- as.call(c(list(quote(forward_constructor_args), constructor),
                             args))
      forwarded_nested <- as.call(c(as.list(forwarded), list(.nested = TRUE)))
      for (expr in list(direct, rlang::call2("grouping_sets", direct),
                        forwarded, forwarded_nested)) {
        for (consume in constructor_arg_consumers(data)) {
          error <- expect_error(consume(expr), class = "marginplyr_error")
          expect_match(conditionMessage(error),
                       paste0("`", constructor, "()` takes no named arguments"),
                       fixed = TRUE)
          expect_match(conditionMessage(error), paste0(tag, " = "),
                       fixed = TRUE)
        }
      }
    }
  }
})

test_that("named values cannot configure capture directly or through dots", {
  data <- data.frame(region = c("E", "W"), value = 1:2)
  controls <- list(.named = TRUE, .ignore_empty = "all", .ignore_null = "all",
                   .unquote_names = FALSE, .homonyms = "last",
                   .check_assign = TRUE, .unknown = quote(stop("must not run")),
                   `caller tag` = quote(stop("must not run")))
  for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube",
                        "grouping_spec")) {
    for (tag in names(controls)) {
      args <- c(list(quote(region)), setNames(controls[tag], tag))
      direct <- as.call(c(list(as.name(constructor)), args))
      forwarded <- as.call(c(list(quote(forward_constructor_args), constructor),
                             args))
      forwarded_nested <- as.call(c(as.list(forwarded), list(.nested = TRUE)))
      for (expr in list(direct, rlang::call2("grouping_sets", direct),
                        forwarded, forwarded_nested)) {
        for (consume in constructor_arg_consumers(data)) {
          error <- expect_error(consume(expr), class = "marginplyr_error")
          expect_match(conditionMessage(error),
                       paste0("`", constructor, "()` takes no named arguments"),
                       fixed = TRUE)
          expect_match(conditionMessage(error), paste0(tag, " = "),
                       fixed = TRUE)
        }
      }
    }
  }
})

test_that("positional capture preserves selections, forwarding and injection", {
  data <- data.frame(region = c("E", "W"), value = 1:2)
  selection <- rlang::quo(region)
  selections <- rlang::quos(region)
  for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube",
                        "grouping_spec")) {
    expected <- if (constructor %in% c("rollup", "cube")) {
      c("(region)", "()")
    } else {
      "(region)"
    }
    expressions <- list(
      rlang::call2(constructor, quote(region)),
      rlang::parse_expr(paste0(constructor, "(region, )")),
      rlang::call2("forward_constructor_args", constructor, quote(region)),
      rlang::call2(constructor, quote(!!selection)),
      rlang::call2(constructor, quote(!!!selections))
    )
    for (expr in expressions) {
      expect_identical(inspect_grouping(data, .grouping = eval(expr))$included,
                       expected)
      summary <- summarize_with_margins(data, n = dplyr::n(),
                                        .grouping = eval(expr))
      expect_identical(summary$n,
                       if (length(expected) == 2L) c(1L, 1L, 2L) else c(1L, 1L))
    }
  }
})

test_that("untagged empty positions still name the constructor and position", {
  data <- data.frame(region = "E", value = 1L)
  for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube",
                        "grouping_spec")) {
    for (position in 1:2) {
      args <- if (position == 1L) ", region" else "region, , value"
      call <- rlang::parse_expr(paste0(constructor, "(", args, ")"))
      for (expr in list(call, rlang::call2("grouping_sets", call))) {
        for (consume in constructor_arg_consumers(data)) {
          error <- expect_error(consume(expr), class = "marginplyr_error")
          expect_match(conditionMessage(error),
                       paste0("Argument ", position, " of `", constructor,
                              "()` is empty."), fixed = TRUE)
        }
      }
    }
  }
})
