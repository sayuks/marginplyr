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

# Assert the original constructor and tag through each consumer and call form.
expect_constructor_tag_refused <- function(constructor, args, tag, data) {
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

test_that("capture controls are refused without evaluating their values", {
  data <- data.frame(region = c("E", "W"), value = 1:2)
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                    "grouping_spec")
  controls <- c(".named", ".ignore_empty", ".ignore_null", ".unquote_names",
                ".homonyms", ".check_assign")
  for (constructor in constructors) {
    for (tag in controls) {
      args <- setNames(list(quote(stop("must not run"))), tag)
      expect_constructor_tag_refused(constructor, args, tag, data)
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
      expect_constructor_tag_refused(constructor, args, tag, data)
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

test_that("captured selections omit trailing commas through injection", {
  data <- data.frame(region = c("E", "W"), value = 1:2)
  selection <- rlang::quo(region)
  for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube",
                        "grouping_spec")) {
    expected <- if (constructor %in% c("rollup", "cube")) {
      c("(region)", "()")
    } else {
      "(region)"
    }
    expressions <- c(
      paste0("rlang::inject(", constructor, "(!!selection, ))"),
      paste0("rlang::inject(forward_constructor_args('", constructor,
             "', !!selection, ))")
    )
    for (expr in expressions) {
      expect_identical(
        inspect_grouping(
          data, .grouping = eval(rlang::parse_expr(expr))
        )$included,
        expected
      )
    }
  }
})

test_that("captured selections retain empty positions around splices", {
  data <- data.frame(region = c("E", "W"), value = 1:2)
  selection <- rlang::quo(region)
  for (constructor in c("grouping_set", "grouping_sets", "rollup", "cube",
                        "grouping_spec")) {
    expected <- if (constructor %in% c("rollup", "cube")) {
      c("(region)", "()")
    } else {
      "(region)"
    }
    selections <- list(
      list(selection, quote(!!!list())),
      list(selection, quote(!!!list()), rlang::missing_arg())
    )
    for (args in selections) {
      expr <- as.call(c(list(as.name(constructor)), args))
      expect_identical(inspect_grouping(data, .grouping = eval(expr))$included,
                       expected)
    }
    shapes <- list(
      list(position = 1L,
           args = list(rlang::missing_arg(), selection, quote(!!!list()))),
      list(position = 2L,
           args = list(selection, rlang::missing_arg(), quote(!!!list()))),
      list(position = 2L,
           args = list(selection, rlang::missing_arg(), rlang::missing_arg()))
    )
    for (shape in shapes) {
      expr <- as.call(c(list(as.name(constructor)), shape$args))
      for (consume in constructor_arg_consumers(data)) {
        error <- expect_error(consume(expr), class = "marginplyr_error")
        expect_match(conditionMessage(error),
                     paste0("Argument ", shape$position, " of `", constructor,
                            "()` is empty."), fixed = TRUE)
      }
    }
  }
})

# jarl-ignore missing_argument: The empty position is the capture witness.
test_that("literal inspection refuses an empty position before a splice", {
  data <- data.frame(region = c("E", "W"), value = 1:2)
  error <- tryCatch(
    inspect_grouping(data, .grouping = grouping_set(, !!!list())),
    error = identity
  )
  expect_s3_class(error, "marginplyr_error")
  if (inherits(error, "error")) {
    expect_match(conditionMessage(error),
                 "Argument 1 of `grouping_set()` is empty.", fixed = TRUE)
  }
})

# jarl-ignore missing_argument: Literal and stored empty positions are compared.
test_that("literal public calls refuse empty positions like stored specs", {
  data <- data.frame(region = c("E", "W"), value = 1:2)
  # The closures write .grouping at the public capture boundary. eval(expr)
  # inside .grouping and tidy-evaluating assertions recapture the witness.
  calls_for <- function(interior) {
    if (interior) {
      return(list(
        function(consume) {
          consume(data, .grouping = grouping_set(region, , !!!list()))
        },
        function(consume) {
          consume(data, .grouping = grouping_sets(region, , !!!list()))
        },
        function(consume) {
          consume(data, .grouping = rollup(region, , !!!list()))
        },
        function(consume) {
          consume(data, .grouping = cube(region, , !!!list()))
        },
        function(consume) {
          consume(data, .grouping = grouping_spec(region, , !!!list()))
        }
      ))
    }
    list(
      function(consume) consume(data, .grouping = grouping_set(, !!!list())),
      function(consume) consume(data, .grouping = grouping_sets(, !!!list())),
      function(consume) consume(data, .grouping = rollup(, !!!list())),
      function(consume) consume(data, .grouping = cube(, !!!list())),
      function(consume) consume(data, .grouping = grouping_spec(, !!!list()))
    )
  }
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                    "grouping_spec")
  consumers <- list(inspect_grouping, summarize_with_margins,
                    expand_with_margins, nest_with_margins,
                    nest_by_with_margins)
  for (position in 1:2) {
    calls <- calls_for(interior = position == 2L)
    stored <- if (position == 1L) {
      list(grouping_set(, !!!list()), grouping_sets(, !!!list()),
           rollup(, !!!list()), cube(, !!!list()), grouping_spec(, !!!list()))
    } else {
      list(grouping_set(region, , !!!list()),
           grouping_sets(region, , !!!list()), rollup(region, , !!!list()),
           cube(region, , !!!list()), grouping_spec(region, , !!!list()))
    }
    for (i in seq_along(constructors)) {
      for (consume in consumers) {
        literal <- tryCatch(calls[[i]](consume), error = identity)
        bound <- tryCatch(
          consume(data, .grouping = stored[[i]]), error = identity
        )
        expect_s3_class(literal, "marginplyr_error")
        expect_s3_class(bound, "marginplyr_error")
        if (inherits(literal, "error") && inherits(bound, "error")) {
          expect_identical(conditionMessage(literal), conditionMessage(bound))
          expect_match(conditionMessage(literal),
                       paste0("Argument ", position, " of `", constructors[[i]],
                              "()` is empty."), fixed = TRUE)
        }
      }
    }
  }
})

# jarl-ignore missing_argument: The inner empty position is the witness.
test_that("nested qualified public calls retain empty constructor positions", {
  data <- data.frame(region = c("E", "W"), value = 1:2)
  calls <- list(
    function(consume) {
      consume(data, .grouping = grouping_sets(
        ((marginplyr::grouping_set)(region, , !!!list())), value
      ))
    },
    function(consume) {
      consume(data, .grouping = grouping_sets(
        ((marginplyr::grouping_sets)(region, , !!!list())), value
      ))
    },
    function(consume) {
      consume(data, .grouping = grouping_sets(
        ((marginplyr::rollup)(region, , !!!list())), value
      ))
    },
    function(consume) {
      consume(data, .grouping = grouping_sets(
        ((marginplyr::cube)(region, , !!!list())), value
      ))
    },
    function(consume) {
      consume(data, .grouping = grouping_sets(
        ((marginplyr::grouping_spec)(region, , !!!list())), value
      ))
    }
  )
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                    "grouping_spec")
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    for (i in seq_along(calls)) {
      error <- tryCatch(calls[[i]](consume), error = identity)
      expect_s3_class(error, "marginplyr_error")
      if (inherits(error, "error")) {
        expect_match(conditionMessage(error),
                     paste0("Argument 2 of `", constructors[[i]],
                            "()` is empty."), fixed = TRUE)
      }
    }
  }
})

# jarl-ignore missing_argument: Trailing and selection commas are controls.
test_that("literal capture retains trailing omission and quosure scope", {
  data <- data.frame(region = c("E", "W"), value = 1:2)
  selection <- local({
    selected <- "region"
    rlang::quo(all_of(selected))
  })
  selected <- "value"
  calls <- list(
    function(consume) {
      consume(data, .grouping = grouping_set(!!selection, !!!list(), ))
    },
    function(consume) {
      consume(data, .grouping = grouping_sets(!!selection, !!!list(), ))
    },
    function(consume) {
      consume(data, .grouping = rollup(!!selection, !!!list(), ))
    },
    function(consume) {
      consume(data, .grouping = cube(!!selection, !!!list(), ))
    },
    function(consume) {
      consume(data, .grouping = grouping_spec(!!selection, !!!list(), ))
    }
  )
  expected <- list(grouping_set(region), grouping_sets(region), rollup(region),
                   cube(region), grouping_spec(region))
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    for (i in seq_along(calls)) {
      actual <- calls[[i]](consume)
      expect_identical(actual, consume(data, .grouping = expected[[i]]))
    }
    ordinary <- consume(data, .grouping = grouping_set(c(region, , !!!list())))
    expect_identical(ordinary, consume(data, .grouping = grouping_set(region)))
    nonempty <- tryCatch(
      consume(data, .grouping = grouping_set(region, , !!!list(quote(value)))),
      error = identity
    )
    expect_s3_class(nonempty, "marginplyr_error")
    if (inherits(nonempty, "error")) {
      expect_match(conditionMessage(nonempty),
                   "Argument 2 of `grouping_set()` is empty.", fixed = TRUE)
    }
    repeated <- consume(
      data, .grouping = grouping_sets(region, region, !!!list()),
      .duplicates = "drop"
    )
    expect_identical(repeated, consume(
      data, .grouping = grouping_sets(region, region), .duplicates = "drop"
    ))
    identity_result <- consume(data, .grouping = grouping_spec(!!!list()))
    expect_identical(identity_result,
                     consume(data, .grouping = grouping_set()))
  }
  expect_identical(selected, "value")
})

# jarl-ignore missing_argument: Refusal must preserve the splice's read budget.
test_that("literal public capture evaluates injection operands once", {
  data <- data.frame(region = c("E", "W"), value = 1:2)
  reads <- 0L
  splice <- function() {
    reads <<- reads + 1L
    if (reads > 1L) stop("second splice evaluation")
    list()
  }
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    reads <- 0L
    error <- tryCatch(
      consume(data, .grouping = grouping_set(region, , !!!splice())),
      error = identity
    )
    expect_s3_class(error, "marginplyr_error")
    expect_identical(reads, 1L)
    reads <- 0L
    actual <- consume(data, .grouping = grouping_set(region, !!!splice()))
    expect_identical(actual, consume(data, .grouping = grouping_set(region)))
    expect_identical(reads, 1L)
  }
})

# jarl-ignore missing_argument: The callee must receive a missing argument.
test_that("literal capture preserves missing arguments for caller functions", {
  data <- data.frame(region = c("E", "W"), value = 1:2)
  grouping_set <- function(x, ...) {
    if (missing(x)) {
      marginplyr::grouping_set(region)
    } else {
      marginplyr::grouping_set(value)
    }
  }
  stored <- grouping_set(, !!!list())
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    actual <- consume(data, .grouping = grouping_set(, !!!list()))
    expect_identical(actual, consume(data, .grouping = stored))
  }
})
