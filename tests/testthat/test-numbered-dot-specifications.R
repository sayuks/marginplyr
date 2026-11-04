# Build a dots wrapper around a written Nested specification position. The
# consumer receives the specification without evaluating any dots itself.
numbered_spec_wrapper <- function(parent, reference, depth, consume,
                                  following = FALSE, tagged = FALSE) {
  argument <- as.name(reference)
  for (i in seq_len(depth)) argument <- rlang::call2("(", argument)
  args <- if (following) list(argument, quote(a)) else list(argument)
  if (tagged) names(args) <- c("first", rep("", length(args) - 1L))
  body <- rlang::call2("consume", rlang::call2(parent, !!!args))
  rlang::new_function(alist(... = ), body, environment())
}

test_that("a dots-shaped column outside a dots frame is a selection", {
  data <- data.frame(key = c("a", "b"), value = 1:2)
  data[["..01"]] <- c("u", "v")
  expect_identical(
    inspect_grouping(data, .by = (..01), .format = "list")$fixed,
    list("..01")
  )

  alias <- rlang::new_quosure(quote(..01), rlang::current_env())
  for (parent in c("grouping_set", "grouping_sets", "rollup", "cube",
                   "grouping_spec")) {
    constructor <- get(parent, envir = asNamespace("marginplyr"))
    explicit <- constructor(tidyselect::all_of("..01"))
    # jarl-ignore unnecessary_parentheses: Redundant pairs are the input.
    wrapped <- constructor(((..01)))
    injected <- constructor(!!alias)
    expected <- inspect_grouping(data, .grouping = explicit, .format = "list")
    expect_identical(expected$included[[1L]], "..01")
    expect_identical(
      inspect_grouping(data, .grouping = wrapped, .format = "list"), expected
    )
    expect_identical(
      inspect_grouping(data, .grouping = injected, .format = "list"), expected
    )
    for (consume in forwarded_margin_verbs) {
      expect_identical(consume(data, grouping = wrapped),
                       consume(data, grouping = explicit))
    }
  }

  expanded <- expand_with_margins(
    data, .grouping = rollup(!!alias), .margin_label = NULL, .id = "set"
  )
  expect_identical(expanded$set, c(1L, 1L, 2L, 2L))
  expect_identical(expanded$key, rep(c("a", "b"), 2L))
  expect_identical(expanded[["..01"]], c("u", "v", NA, NA))

  summarized <- summarize_with_margins(
    data, members = list(value),
    .grouping = rollup(!!alias), .margin_label = NULL, .id = "set"
  )
  expect_identical(summarized$set, c(1L, 1L, 2L))
  expect_identical(summarized$members, list(1L, 2L, 1:2))
})

test_that("unrelated dots bindings are not forced for column selection", {
  data <- data.frame(check.names = FALSE, "..01" = c("a", "b"))
  env <- rlang::env()
  calls <- 0L
  makeActiveBinding("...", function() {
    calls <<- calls + 1L
    42L
  }, env)
  alias <- rlang::new_quosure(quote((..01)), env)
  inspect <- function() {
    inspect_grouping(data, .grouping = grouping_sets(!!alias), .format = "list")
  }

  expect_identical(inspect()$included, list("..01"))
  expect_identical(calls, 0L)
  rm("...", envir = env)
  delayedAssign("...", stop("unrelated dots forced"), assign.env = env)
  expect_identical(inspect()$included, list("..01"))
})

test_that("parenthesized numbered dots retain admitted kinds and refusals", {
  data <- data.frame(a = c("x", "y"), value = 1:2)
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                    "grouping_spec")
  children <- list(
    set = grouping_set(a), sets = grouping_sets(a), rollup = rollup(a),
    cube = cube(a), product = grouping_spec(a), empty = grouping_set(),
    selected_empty = grouping_set(tidyselect::any_of("absent"))
  )
  consume <- function(spec) {
    inspect_grouping(data, .grouping = spec, .format = "list")
  }
  references <- c("..1", "..2", "..01", "..02", "..001", "..+1", ".. 1",
                  "..\t1", ".. +02")
  for (parent in constructors) {
    for (kind in names(children)) {
      child <- children[[kind]]
      refused <- parent == "grouping_set" ||
        (parent %in% c("rollup", "cube") && kind != "set")
      for (reference in references) {
        # Noncanonical spellings are allowed column names, deliberately
        # carrying a different partition from the forwarded specification.
        data <- data.frame(a = c("x", "y"), value = 1:2)
        if (!reference %in% c("..1", "..2")) data[[reference]] <- "wrong"
        supplied <- if (grepl("2$", reference)) {
          list(NULL, child)
        } else {
          list(child)
        }
        for (depth in c(0L, 1L, 3L)) {
          wrapper <- numbered_spec_wrapper(parent, reference, depth, consume)
          info <- paste(parent, kind, reference, depth)
          if (refused) {
            message <- if (parent == "grouping_set") {
              "can contain columns, not another grouping family"
            } else if (kind %in% c("empty", "selected_empty")) {
              "empty"
            } else {
              "only accepts columns"
            }
            expect_error(do.call(wrapper, supplied), message,
                         class = "marginplyr_error", info = info)
          } else {
            expected <- if (kind %in% c("empty", "selected_empty")) {
              list(character())
            } else if (parent %in% c("rollup", "cube") ||
                         kind %in% c("rollup", "cube")) {
              list("a", character())
            } else {
              list("a")
            }
            expect_identical(do.call(wrapper, supplied)$included, expected,
                             info = info)
          }
        }
      }
    }
  }
})

test_that("numbered specifications retain identifiers and source membership", {
  data <- data.frame(a = c("x", "y"), value = 1:2)
  data[["..01"]] <- "wrong"
  data[["..02"]] <- "wrong"
  consumers <- list(
    function(spec) inspect_grouping(data, .grouping = spec, .format = "list"),
    function(spec) {
      summarize_with_margins(
        data, members = list(value), bits = grouping_id(a),
        .grouping = spec, .id = "set", .margin_label = NULL
      )
    },
    function(spec) {
      expand_with_margins(
        data, .grouping = spec, .id = "set", .margin_label = NULL
      )
    },
    function(spec) {
      nest_with_margins(
        data, .grouping = spec, .id = "set", .margin_label = NULL
      )
    },
    function(spec) {
      nest_by_with_margins(
        data, .grouping = spec, .id = "set", .margin_label = NULL
      )
    }
  )
  child <- grouping_set(a)
  for (parent in c("grouping_set", "grouping_sets", "rollup", "cube",
                   "grouping_spec")) {
    direct <- eval(rlang::call2(parent, child))
    for (consume in consumers) {
      expected <- if (parent == "grouping_set") NULL else consume(direct)
      for (reference in c("..1", "..2", "..01", "..02")) {
        supplied <- if (grepl("2$", reference)) {
          list(NULL, child)
        } else {
          list(child)
        }
        for (depth in c(0L, 1L, 3L)) {
          wrapper <- numbered_spec_wrapper(parent, reference, depth, consume)
          if (parent == "grouping_set") {
            expect_error(do.call(wrapper, supplied),
                         "can contain columns, not another grouping family",
                         class = "marginplyr_error")
          } else {
            expect_identical(do.call(wrapper, supplied), expected,
                             info = paste(parent, reference, depth))
          }
        }
      }
    }
  }
  nested <- numbered_spec_wrapper("grouping_sets", "..01", 1L, consumers[[4L]])
  result <- nested(child)
  expect_identical(result$set, c(1L, 1L))
  expect_identical(result$a, c("x", "y"))
  expect_identical(lapply(result$data, `[[`, "value"), list(1L, 2L))
})

test_that("forwarded specification occurrences keep their evaluation order", {
  data <- data.frame(a = c("x", "y"), b = "z", value = 1:2)
  run <- function(depth) {
    events <- character()
    env <- rlang::env(rlang::current_env())
    makeActiveBinding("chosen", function() {
      events <<- c(events, "chosen")
      grouping_set(a)
    }, env)
    makeActiveBinding("later", function() {
      events <<- c(events, "later")
      grouping_set(b)
    }, env)
    argument <- quote(..02)
    for (i in seq_len(depth)) argument <- rlang::call2("(", argument)
    body <- rlang::expr(inspect_grouping(
      data, .grouping = grouping_sets(!!argument, !!argument, (..1)),
      .duplicates = "keep", .format = "list"
    ))
    wrapper <- rlang::new_function(alist(... = ), body, env)
    result <- eval(quote(wrapper(last = later, first = chosen)), env)
    list(result = result, events = events)
  }
  expected <- run(0L)
  expect_identical(expected$result$included, list("a", "a", "b"))
  expect_identical(expected$result$set_id, 1:3)
  expect_identical(expected$events, c("chosen", "chosen", "later"))
  expect_identical(run(1L), expected)
  expect_identical(run(3L), expected)

  consume <- function(spec) {
    inspect_grouping(data, .grouping = spec, .format = "list")
  }
  wrapper <- numbered_spec_wrapper("grouping_sets", "..01", 2L, consume)
  bound <- grouping_set(a)
  named <- function(x = bound) wrapper(x)
  expect_identical(named()$included, list("a"))
  expect_identical(named(x = grouping_set(b))$included, list("b"))
  # jarl-ignore unnecessary_parentheses: Parenthesis transparency is tested.
  ordinary <- function(x = bound) consume(grouping_sets(((x))))
  expect_identical(ordinary(), named())
})

test_that("missing numbered specifications name the public argument boundary", {
  data <- data.frame(a = "x")
  consume <- function(spec) inspect_grouping(data, .grouping = spec)
  for (parent in c("grouping_set", "grouping_sets", "rollup", "cube",
                   "grouping_spec")) {
    for (reference in c("..1", "..01", "..2", "..02")) {
      for (depth in c(0L, 1L, 3L)) {
        wrapper <- numbered_spec_wrapper(parent, reference, depth, consume,
                                         following = TRUE)
        # jarl-ignore missing_argument: A missing dots payload is the input.
        missing <- expect_error(wrapper(, , NULL), class = "marginplyr_error")
        expect_match(conditionMessage(missing), paste0(
          "Argument 1 of `", parent, "()` is empty."
        ), fixed = TRUE)
        absent <- expect_error(wrapper())
        expect_match(conditionMessage(absent), "fewer than|incorrect context")
        expect_false(grepl("Column", conditionMessage(absent), fixed = TRUE))
      }
    }
  }
})

test_that("tagged forwarded empty arguments are refused by position", {
  data <- data.frame(a = "x")
  consume <- function(spec) inspect_grouping(data, .grouping = spec)
  for (parent in c("grouping_set", "grouping_sets", "rollup", "cube",
                   "grouping_spec")) {
    constructor <- get(parent, envir = asNamespace("marginplyr"))
    literal <- expect_error(consume(constructor(first = , a)),
                            class = "marginplyr_error")
    for (reference in c("..1", "..01")) {
      for (depth in c(0L, 1L, 3L)) {
        wrapper <- numbered_spec_wrapper(parent, reference, depth, consume,
                                         following = TRUE, tagged = TRUE)
        # jarl-ignore missing_argument: An explicitly missing dot is the input.
        forwarded <- expect_error(wrapper(, stop("unused dot was forced")),
                                  class = "marginplyr_error")
        expect_identical(conditionMessage(forwarded), conditionMessage(literal))
        expect_error(wrapper(stop("tagged dot was forced")),
                     "named arguments", class = "marginplyr_error")
        if (depth > 0L) {
          invocation <- call("wrapper", quote(!!stop("injection ran")))
          expect_error(eval(invocation),
                       "named arguments", class = "marginplyr_error")
        }
      }
    }
  }
})

test_that("numbered specifications keep explicit selection and injection", {
  data <- data.frame(a = c("x", "y"), value = 1:2)
  data[["..01"]] <- "same"
  state <- rlang::env(calls = 0L)
  arbitrary <- function() {
    state$calls <- state$calls + 1L
    grouping_set(a)
  }
  consume <- function(spec) {
    inspect_grouping(data, .grouping = spec, .format = "list")
  }
  wrapper <- numbered_spec_wrapper("grouping_sets", "..01", 2L, consume)
  expect_identical(wrapper(tidyselect::all_of("..01"))$included, list("..01"))
  child <- grouping_set(a)
  expect_identical(wrapper(child, stop("unused dot was forced"))$included,
                   list("a"))
  later <- numbered_spec_wrapper("grouping_sets", "..02", 2L, consume)
  expect_identical(later(stop("unused dot was forced"), child)$included,
                   list("a"))
  injected <- rlang::quo(!!child)
  expect_identical(wrapper(!!injected)$included, list("a"))
  inner <- rlang::env(rlang::current_env(), chosen = child)
  injected <- rlang::new_quosure(quote(chosen), inner)
  expect_identical(wrapper((!!injected))$included, list("a"))
  expect_error(wrapper(arbitrary()), "Grouping specification",
               class = "marginplyr_error")
  expect_identical(state$calls, 1L)
  expect_error(wrapper(stop("caller error")), "caller error")
  expect_identical(state$calls, 1L)
})

test_that("numbered nested specifications retain unexecuted lazy plans", {
  data <- dbplyr::lazy_frame(a = "x", value = 1L,
                             con = dbplyr::simulate_postgres())
  consumers <- list(
    function(spec) inspect_grouping(data, .grouping = spec, .format = "list"),
    function(spec) {
      dbplyr::sql_render(summarize_with_margins(
        data, n = dplyr::n(), .grouping = spec, .id = "set"
      ))
    },
    function(spec) {
      dbplyr::sql_render(expand_with_margins(
        data, .grouping = spec, .id = "set"
      ))
    }
  )
  child <- grouping_set(a)
  for (consume in consumers) {
    expected <- consume(grouping_sets(child))
    for (reference in c("..1", "..01", ".. +01")) {
      wrapper <- numbered_spec_wrapper("grouping_sets", reference, 3L, consume)
      expect_identical(wrapper(child), expected)
    }
  }
})
