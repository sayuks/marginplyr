# jarl-ignore missing_argument: Quoted empty positions are factory input data.
test_that("evaluated bquote substitutions retain authored empty positions", {
  data <- data.frame(a = 1L)
  stored <- base::bquote(.(grouping_set(, !!!list())))
  expected <- tryCatch(inspect_grouping(data, .grouping = stored),
                       error = identity)
  expect_s3_class(expected, "marginplyr_error")
  actual <- tryCatch(
    inspect_grouping(
      data, .grouping = base::bquote(.(grouping_set(, !!!list())))
    ),
    error = identity
  )
  expect_s3_class(actual, "marginplyr_error")
  if (inherits(actual, "error")) {
    expect_match(conditionMessage(actual),
                 "Argument 1 of `grouping_set()` is empty.", fixed = TRUE)
  }
  stored <- eval(substitute(x, list(x = grouping_set(, !!!list()))))
  expected <- tryCatch(inspect_grouping(data, .grouping = stored),
                       error = identity)
  actual <- tryCatch(
    inspect_grouping(data, .grouping = eval(substitute(x, list(
      x = grouping_set(, !!!list())
    )))),
    error = identity
  )
  expect_s3_class(expected, "marginplyr_error")
  expect_s3_class(actual, "marginplyr_error")
  if (inherits(actual, "error")) {
    expect_identical(conditionMessage(actual), conditionMessage(expected))
    expect_match(conditionMessage(actual),
                 "Argument 1 of `grouping_set()` is empty.", fixed = TRUE)
  }
})

# jarl-ignore missing_argument: Quoted empty positions are factory input data.
test_that("quoted factories agree inline and stored at every public boundary", {
  data <- data.frame(a = 1L, b = 2L)
  drop_last <- function(expr) {
    expr[[length(expr)]] <- NULL
    eval(expr)
  }
  empty <- drop_last(quote(grouping_set(, a)))
  retained <- drop_last(quote(grouping_set(a, , b)))
  expect_identical(inspect_grouping(data, .grouping = empty)$included, "()")
  expect_identical(inspect_grouping(data, .grouping = retained)$included, "(a)")
  plain_capture <- function(x) rlang::eval_tidy(rlang::enquo(x))
  plain <- plain_capture(drop_last(quote(grouping_set(, a))))
  expect_identical(inspect_grouping(data, .grouping = plain),
                   inspect_grouping(data, .grouping = empty))
  plain <- plain_capture(drop_last(quote(grouping_set(a, , b))))
  expect_identical(inspect_grouping(data, .grouping = plain),
                   inspect_grouping(data, .grouping = retained))
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    actual <- tryCatch(
      consume(data, .grouping = drop_last(quote(grouping_set(, a)))),
      error = identity
    )
    expect_identical(actual, consume(data, .grouping = empty))
    actual <- tryCatch(
      consume(data, .grouping = drop_last(quote(grouping_set(a, , b)))),
      error = identity
    )
    expect_identical(actual, consume(data, .grouping = retained))
  }
})

# jarl-ignore missing_argument: Editing the quoted call leaves a trailing comma.
test_that("quoted factories preserve dtplyr metadata and source ownership", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table, so dtplyr is the only optional backend here.
  source <- data.table::data.table(a = 1:2, b = c("x", "y"))
  before <- serialize(source, NULL)
  step <- dplyr::select(dtplyr::lazy_dt(source, immutable = FALSE), b, a)
  counter <- local_dtplyr_proxy_counter()
  drop_last <- function(expr) {
    expr[[length(expr)]] <- NULL
    eval(expr)
  }
  stored <- drop_last(quote(grouping_set(where(is.numeric), , b)))
  expected <- inspect_grouping(step, .grouping = stored)
  counter$n <- 0L
  counter$source_evaluations <- 0L
  actual <- inspect_grouping(
    step, .grouping = drop_last(quote(grouping_set(where(is.numeric), , b)))
  )
  expect_identical(actual, expected)
  expect_identical(actual$included, "(a)")
  expect_identical(counter$n, 1L)
  expect_identical(counter$source_evaluations, 0L)
  expect_identical(serialize(source, NULL), before)
  counter$n <- 0L
  actual <- inspect_grouping(
    step, .grouping = base::bquote(.(
      grouping_set(where(is.numeric), !!!list())
    ))
  )
  expect_identical(actual, expected)
  expect_identical(counter$n, 1L)
  expect_identical(counter$source_evaluations, 0L)
  expect_identical(serialize(source, NULL), before)
})

# jarl-ignore missing_argument: The quoted empty position becomes trailing.
test_that("quoted factories leave an inspected Arrow reader unconsumed", {
  skip_if_suggest_absent("arrow")
  data <- data.frame(a = 1:2, b = c("x", "y"))
  reader <- arrow::RecordBatchReader$create(arrow::record_batch(data))
  drop_last <- function(expr) {
    expr[[length(expr)]] <- NULL
    eval(expr)
  }
  stored <- drop_last(quote(grouping_set(where(is.numeric), , b)))
  expected <- inspect_grouping(arrow::Table$create(data), .grouping = stored)
  actual <- inspect_grouping(
    reader, .grouping = drop_last(quote(grouping_set(where(is.numeric), , b)))
  )
  expect_identical(actual, expected)
  expect_identical(actual$included, "(a)")
  actual <- inspect_grouping(
    reader, .grouping = base::bquote(.(
      grouping_set(where(is.numeric), !!!list())
    ))
  )
  expect_identical(actual, expected)
  expect_identical(as.data.frame(arrow::as_arrow_table(reader)), data)
})

# jarl-ignore missing_argument: Template equality observes unmodified language.
test_that("quoted and expression templates keep their selected column", {
  data <- data.frame(a = 1L, b = 2L)
  counter <- new.env(parent = emptyenv())
  counter$reads <- 0L
  choose <- function(expr) {
    counter$reads <- counter$reads + 1L
    if (identical(expr, quote(grouping_set(, a)))) {
      grouping_set(a)
    } else {
      grouping_set(b)
    }
  }
  calls <- list(
    function(consume) {
      consume(data, .grouping = choose(quote(grouping_set(, a))))
    },
    function(consume) {
      consume(data, .grouping = choose(base::quote(grouping_set(, a))))
    },
    function(consume) {
      consume(data, .grouping = choose((quote)(grouping_set(, a))))
    },
    function(consume) {
      consume(data, .grouping = choose((base::quote)(grouping_set(, a))))
    },
    function(consume) {
      consume(data, .grouping = choose(expression(grouping_set(, a))[[1L]]))
    },
    function(consume) {
      consume(data, .grouping = choose(
        base::expression(grouping_set(, a))[[1L]]
      ))
    }
  )
  stored <- choose(quote(grouping_set(, a)))
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    expected <- consume(data, .grouping = stored)
    for (call in calls) {
      counter$reads <- 0L
      actual <- call(consume)
      expect_identical(actual, expected)
      expect_identical(counter$reads, 1L)
    }
  }
})

# jarl-ignore missing_argument: The factory inspects the function body's syntax.
test_that("a supplied function body remains ordinary factory input", {
  data <- data.frame(a = 1L, b = 2L)
  choose <- function(fn) {
    if (identical(body(fn), quote(grouping_set(, a)))) {
      grouping_set(a)
    } else {
      grouping_set(b)
    }
  }
  stored <- choose(function() grouping_set(, a))
  expect_identical(inspect_grouping(data, .grouping = stored)$included, "(a)")
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    actual <- consume(data, .grouping = choose(function() grouping_set(, a)))
    expect_identical(actual, consume(data, .grouping = stored))
  }
})

test_that("an immediately executed literal refuses an empty before a splice", {
  data <- data.frame(a = 1L)
  stored <- (function() grouping_set(a, , !!!list()))()
  expected <- tryCatch(inspect_grouping(data, .grouping = stored),
                       error = identity)
  expect_s3_class(expected, "marginplyr_error")
  actual <- tryCatch(
    inspect_grouping(
      data, .grouping = (function() grouping_set(a, , !!!list()))()
    ),
    error = identity
  )
  expect_s3_class(actual, "marginplyr_error")
  if (inherits(actual, "error")) {
    expect_identical(conditionMessage(actual), conditionMessage(expected))
    expect_match(conditionMessage(actual),
                 "Argument 2 of `grouping_set()` is empty.", fixed = TRUE)
  }
})

test_that("executed literals retain constructor refusals at every boundary", {
  data <- data.frame(a = 1L, b = 2L)
  calls <- list(
    function(consume) {
      consume(data, .grouping = (function() grouping_set(a, , !!!list()))())
    },
    function(consume) {
      consume(data, .grouping = (function() grouping_sets(a, , !!!list()))())
    },
    function(consume) {
      consume(data, .grouping = (function() rollup(a, , !!!list()))())
    },
    function(consume) {
      consume(data, .grouping = (function() cube(a, , !!!list()))())
    },
    function(consume) {
      consume(data, .grouping = (function() grouping_spec(a, , !!!list()))())
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
    leading <- tryCatch(
      consume(data, .grouping = (function() grouping_set(, !!!list()))()),
      error = identity
    )
    expect_s3_class(leading, "marginplyr_error")
    if (inherits(leading, "error")) {
      expect_match(conditionMessage(leading),
                   "Argument 1 of `grouping_set()` is empty.", fixed = TRUE)
    }
    nested <- tryCatch(
      consume(data, .grouping = ((function() {
        (function() {
          grouping_sets(((marginplyr::grouping_set)(a, , !!!list())), b)
        })()
      }))()),
      error = identity
    )
    expect_s3_class(nested, "marginplyr_error")
    if (inherits(nested, "error")) {
      expect_match(conditionMessage(nested),
                   "Argument 2 of `grouping_set()` is empty.", fixed = TRUE)
    }
  }
})

test_that("executed literals preserve injection scope and evaluation budgets", {
  data <- data.frame(a = 1L, b = 2L)
  counts <- new.env(parent = emptyenv())
  counts$reads <- counts$runs <- 0L
  splice <- function() {
    counts$reads <- counts$reads + 1L
    if (counts$reads > 1L) stop("second splice evaluation")
    list()
  }
  selection <- local({
    selected <- "a"
    rlang::quo(all_of(selected))
  })
  selected <- "b"
  stored <- grouping_set(a)
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    counts$reads <- counts$runs <- 0L
    error <- tryCatch(
      consume(data, .grouping = (function() {
        counts$runs <- counts$runs + 1L
        grouping_set(a, , !!!splice())
      })()),
      error = identity
    )
    expect_s3_class(error, "marginplyr_error")
    expect_identical(counts$reads, 1L)
    expect_identical(counts$runs, 1L)
    counts$reads <- counts$runs <- 0L
    actual <- consume(data, .grouping = (function() {
      counts$runs <- counts$runs + 1L
      grouping_set(!!selection, !!!splice(), )
    })())
    expect_identical(actual, consume(data, .grouping = stored))
    expect_identical(counts$reads, 1L)
    expect_identical(counts$runs, 1L)
  }
  expect_identical(selected, "b")
})

# jarl-ignore missing_argument: These bodies and quotes are inspected as data.
test_that("executed literals still preserve supplied syntax for factories", {
  data <- data.frame(a = 1L, b = 2L)
  choose <- function(expr) {
    if (is.function(expr)) expr <- body(expr)
    if (identical(expr, quote(grouping_set(, a)))) {
      grouping_set(a)
    } else {
      grouping_set(b)
    }
  }
  stored <- choose(function() grouping_set(, a))
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    expected <- consume(data, .grouping = stored)
    actual <- consume(data, .grouping = (function() {
      choose(base::quote(grouping_set(, a)))
    })())
    expect_identical(actual, expected)
    actual <- consume(data, .grouping = (function() {
      choose(function() grouping_set(, a))
    })())
    expect_identical(actual, expected)
  }
})

# jarl-ignore missing_argument: Evaluated operands contain authored empties.
test_that("evaluated capture operands refuse empties at every boundary", {
  data <- data.frame(a = 1L, b = 2L)
  calls <- list(
    function(consume) {
      consume(data, .grouping = base::bquote(.(grouping_set(a, , !!!list()))))
    },
    function(consume) {
      consume(data, .grouping = base::bquote(.(grouping_sets(a, , !!!list()))))
    },
    function(consume) {
      consume(data, .grouping = base::bquote(.(rollup(a, , !!!list()))))
    },
    function(consume) {
      consume(data, .grouping = base::bquote(.(cube(a, , !!!list()))))
    },
    function(consume) {
      consume(data, .grouping = base::bquote(.(grouping_spec(a, , !!!list()))))
    },
    function(consume) {
      consume(
        data, .grouping = eval(substitute(x, list(
          x = grouping_set(a, , !!!list())
        )))
      )
    },
    function(consume) {
      consume(
        data, .grouping = eval(substitute(x, list(
          x = grouping_sets(a, , !!!list())
        )))
      )
    },
    function(consume) {
      consume(
        data, .grouping = eval(substitute(x, list(
          x = rollup(a, , !!!list())
        )))
      )
    },
    function(consume) {
      consume(
        data, .grouping = eval(substitute(x, list(
          x = cube(a, , !!!list())
        )))
      )
    },
    function(consume) {
      consume(
        data, .grouping = eval(substitute(x, list(
          x = grouping_spec(a, , !!!list())
        )))
      )
    },
    function(consume) {
      consume(
        data, .grouping = base::bquote(.(x), where = list(
          x = grouping_set(a, , !!!list())
        ))
      )
    },
    function(consume) {
      consume(
        data, .grouping = base::bquote(.(x), where = list(
          x = grouping_sets(a, , !!!list())
        ))
      )
    },
    function(consume) {
      consume(
        data, .grouping = base::bquote(.(x), where = list(
          x = rollup(a, , !!!list())
        ))
      )
    },
    function(consume) {
      consume(
        data, .grouping = base::bquote(.(x), where = list(
          x = cube(a, , !!!list())
        ))
      )
    },
    function(consume) {
      consume(
        data, .grouping = base::bquote(.(x), where = list(
          x = grouping_spec(a, , !!!list())
        ))
      )
    },
    function(consume) {
      consume(data, .grouping = base::bquote(.(
        grouping_sets(((marginplyr::grouping_set)(a, , !!!list())), b)
      )))
    },
    function(consume) {
      consume(data, .grouping = base::bquote(.(
        grouping_sets(((marginplyr::grouping_sets)(a, , !!!list())), b)
      )))
    },
    function(consume) {
      consume(data, .grouping = base::bquote(.(
        grouping_sets(((marginplyr::rollup)(a, , !!!list())), b)
      )))
    },
    function(consume) {
      consume(data, .grouping = base::bquote(.(
        grouping_sets(((marginplyr::cube)(a, , !!!list())), b)
      )))
    },
    function(consume) {
      consume(data, .grouping = base::bquote(.(
        grouping_sets(((marginplyr::grouping_spec)(a, , !!!list())), b)
      )))
    }
  )
  stored <- list(
    grouping_set(a, , !!!list()),
    grouping_sets(a, , !!!list()),
    rollup(a, , !!!list()),
    cube(a, , !!!list()),
    grouping_spec(a, , !!!list())
  )
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                    "grouping_spec")
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    for (i in seq_along(calls)) {
      constructor <- (i - 1L) %% length(constructors) + 1L
      expected <- tryCatch(consume(data, .grouping = stored[[constructor]]),
                           error = identity)
      actual <- tryCatch(calls[[i]](consume), error = identity)
      expect_s3_class(expected, "marginplyr_error")
      expect_s3_class(actual, "marginplyr_error")
      if (inherits(actual, "error")) {
        expect_identical(conditionMessage(actual), conditionMessage(expected))
        expect_match(conditionMessage(actual),
                     paste0("Argument 2 of `", constructors[[constructor]],
                            "()` is empty."), fixed = TRUE)
      }
    }
    nested <- tryCatch(
      consume(data, .grouping = (base::bquote)(.(
        grouping_sets(((marginplyr::grouping_set)(a, , !!!list())), b)
      ))),
      error = identity
    )
    expect_s3_class(nested, "marginplyr_error")
    if (inherits(nested, "error")) {
      expect_match(conditionMessage(nested),
                   "Argument 2 of `grouping_set()` is empty.", fixed = TRUE)
    }
  }
})

# jarl-ignore missing_argument: Quoted empties and trailing commas are controls.
test_that("capture operands preserve scope, source values and read budgets", {
  data <- data.frame(a = 1:2, b = c("x", "y"))
  before <- serialize(data, NULL)
  selection <- local({
    selected <- "a"
    rlang::quo(all_of(selected))
  })
  selected <- "b"
  counts <- new.env(parent = emptyenv())
  counts$reads <- counts$runs <- 0L
  splice <- function() {
    counts$reads <- counts$reads + 1L
    if (counts$reads > 1L) stop("second splice evaluation")
    list()
  }
  choose <- function(expr) {
    if (identical(expr, quote(grouping_set(, a)))) {
      grouping_set(a)
    } else {
      grouping_set(b)
    }
  }
  stored <- grouping_sets(a, a, grouping_set())
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    counts$reads <- counts$runs <- 0L
    actual <- consume(data, .grouping = base::bquote(.({
      counts$runs <- counts$runs + 1L
      grouping_sets(!!selection, !!selection, grouping_set(), !!!splice(), )
    })), .duplicates = "drop")
    expect_identical(actual, consume(data, .grouping = stored,
                                     .duplicates = "drop"))
    expect_identical(counts$reads, 1L)
    expect_identical(counts$runs, 1L)
    counts$reads <- 0L
    error <- tryCatch(
      consume(data, .grouping = eval(substitute(x, list(
        x = grouping_set(a, , !!!splice())
      )))),
      error = identity
    )
    expect_s3_class(error, "marginplyr_error")
    expect_identical(counts$reads, 1L)
    quoted <- consume(data, .grouping = choose(base::bquote(grouping_set(, a))))
    expect_identical(quoted, consume(data, .grouping = grouping_set(a)))
    ordinary <- consume(data, .grouping = base::bquote(.(
      grouping_set(c(a, , !!!list()))
    )))
    expect_identical(ordinary, consume(data, .grouping = grouping_set(a)))
  }
  expanded <- expand_with_margins(
    data, .grouping = base::bquote(.(grouping_sets(a, a, grouping_set()))),
    .duplicates = "keep", .id = "sid"
  )
  expect_identical(expanded$sid, c(1L, 1L, 2L, 2L, 3L, 3L))
  expect_identical(expanded$b, rep(c("x", "y"), 3L))
  expect_identical(serialize(data, NULL), before)
  expect_identical(selected, "b")
})

# jarl-ignore missing_argument: The factory reads the original call as language.
test_that("an NSE factory preserves the caller Grouping syntax", {
  data <- data.frame(a = c(1L, 2L), b = c(3L, 4L))
  make <- function(expr) {
    raw <- substitute(expr)
    if (identical(raw, quote(grouping_set(, a)))) rollup(a) else rollup(b)
  }
  stored <- make(grouping_set(, a))
  expected <- expand_with_margins(data, .grouping = stored, .id = "sid")
  expect_identical(expected$a, c("1", "2", "Total", "Total"))
  expect_identical(expected$b, c(3L, 4L, 3L, 4L))
  expect_identical(expected$sid, c(1L, 1L, 2L, 2L))
  actual <- expand_with_margins(
    data, .grouping = make(grouping_set(, a)), .id = "sid"
  )
  expect_identical(actual, expected)
})

# jarl-ignore missing_argument: Factory inputs are language data.
test_that("NSE and aliased quotation factories agree at public boundaries", {
  data <- data.frame(a = c(1L, 1L, 2L), b = c(3L, 4L, 5L))
  before <- serialize(data, NULL)
  counts <- new.env(parent = emptyenv())
  counts$nse <- counts$quoted <- 0L
  make <- function(expr) {
    counts$nse <- counts$nse + 1L
    if (identical(substitute(expr), quote(grouping_set(, a)))) {
      rollup(a)
    } else {
      rollup(b)
    }
  }
  q <- base::quote
  choose <- function(expr) {
    counts$quoted <- counts$quoted + 1L
    if (identical(expr, base::quote(grouping_set(, a)))) {
      rollup(a)
    } else {
      rollup(b)
    }
  }
  drop_last <- function(expr) {
    expr[[length(expr)]] <- NULL
    eval(expr)
  }
  stored <- make(grouping_set(, a))
  quoted <- choose(q(grouping_set(, a)))
  empty <- drop_last(q(grouping_set(, a)))
  expect_identical(inspect_grouping(data, .grouping = stored)$included,
                   c("(a)", "()"))
  expect_identical(inspect_grouping(data, .grouping = empty)$included, "()")
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    counts$nse <- counts$quoted <- 0L
    expected <- consume(data, .grouping = stored)
    actual <- consume(data, .grouping = make(grouping_set(, a)))
    expect_identical(actual, expected)
    actual <- consume(data, .grouping = choose(q(grouping_set(, a))))
    expect_identical(actual, expected)
    expect_identical(actual, consume(data, .grouping = quoted))
    expect_identical(counts$nse, 1L)
    expect_identical(counts$quoted, 1L)
    actual <- consume(data, .grouping = drop_last(q(grouping_set(, a))))
    expect_identical(actual, consume(data, .grouping = empty))
  }
  expected <- expand_with_margins(data, .grouping = stored, .id = "sid")
  actual <- expand_with_margins(
    data, .grouping = choose(q(grouping_set(, a))), .id = "sid"
  )
  expect_identical(actual, expected)
  expect_identical(actual$a, c("1", "1", "2", "Total", "Total", "Total"))
  expect_identical(actual$b, c(3L, 4L, 5L, 3L, 4L, 5L))
  expect_identical(actual$sid, c(1L, 1L, 1L, 2L, 2L, 2L))
  expect_identical(serialize(data, NULL), before)
})

# jarl-ignore missing_argument: Rebound capture names execute their operands.
test_that("caller capture names follow their actual evaluation semantics", {
  data <- data.frame(a = 1:2, b = 3:4)
  quote <- substitute <- expression <- identity
  calls <- list(
    function(consume) {
      consume(data, .grouping = quote(grouping_set(a, , !!!list())))
    },
    function(consume) {
      consume(data, .grouping = quote(grouping_sets(a, , !!!list())))
    },
    function(consume) {
      consume(data, .grouping = quote(rollup(a, , !!!list())))
    },
    function(consume) {
      consume(data, .grouping = quote(cube(a, , !!!list())))
    },
    function(consume) {
      consume(data, .grouping = quote(grouping_spec(a, , !!!list())))
    },
    function(consume) {
      consume(data, .grouping = substitute(grouping_set(a, , !!!list())))
    },
    function(consume) {
      consume(data, .grouping = substitute(grouping_sets(a, , !!!list())))
    },
    function(consume) {
      consume(data, .grouping = substitute(rollup(a, , !!!list())))
    },
    function(consume) {
      consume(data, .grouping = substitute(cube(a, , !!!list())))
    },
    function(consume) {
      consume(data, .grouping = substitute(grouping_spec(a, , !!!list())))
    },
    function(consume) {
      consume(data, .grouping = expression(grouping_set(a, , !!!list())))
    },
    function(consume) {
      consume(data, .grouping = expression(grouping_sets(a, , !!!list())))
    },
    function(consume) {
      consume(data, .grouping = expression(rollup(a, , !!!list())))
    },
    function(consume) {
      consume(data, .grouping = expression(cube(a, , !!!list())))
    },
    function(consume) {
      consume(data, .grouping = expression(grouping_spec(a, , !!!list())))
    }
  )
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                    "grouping_spec")
  stored <- list(
    quote(grouping_set(a, , !!!list())),
    quote(grouping_sets(a, , !!!list())),
    quote(rollup(a, , !!!list())),
    quote(cube(a, , !!!list())),
    quote(grouping_spec(a, , !!!list()))
  )
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    for (i in seq_along(calls)) {
      constructor <- (i - 1L) %% length(constructors) + 1L
      expected <- tryCatch(consume(data, .grouping = stored[[constructor]]),
                           error = identity)
      actual <- tryCatch(calls[[i]](consume), error = identity)
      expect_s3_class(expected, "marginplyr_error")
      expect_s3_class(actual, "marginplyr_error")
      if (inherits(actual, "error")) {
        expect_identical(conditionMessage(actual), conditionMessage(expected))
        expect_match(conditionMessage(actual),
                     paste0("Argument 2 of `", constructors[[constructor]],
                            "()` is empty."), fixed = TRUE)
      }
    }
    nested <- tryCatch(consume(
      data, .grouping = expression(grouping_sets(
        ((marginplyr::grouping_set)(a, , !!!list())), b
      ))
    ), error = identity)
    expect_s3_class(nested, "marginplyr_error")
    if (inherits(nested, "error")) {
      expect_match(conditionMessage(nested),
                   "Argument 2 of `grouping_set()` is empty.", fixed = TRUE)
    }
  }
})

# jarl-ignore missing_argument: The factory inspects a nonexecuted constructor.
test_that("public outer injection evaluates its original operand", {
  data <- data.frame(a = 1:2, b = 3:4)
  make <- function(expr) {
    if (identical(substitute(expr), quote(grouping_set(, a)))) {
      rollup(a)
    } else {
      rollup(b)
    }
  }
  stored <- rollup(a)
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    actual <- tryCatch(consume(data, .grouping = !!rollup(a)), error = identity)
    expect_identical(actual, consume(data, .grouping = stored))
    actual <- consume(data, .grouping = !!make(grouping_set(, a)))
    expect_identical(actual, consume(data, .grouping = stored))
  }
})

test_that("caller constructor bindings retain public outer injection", {
  data <- data.frame(a = 1:2, b = 3:4)
  stored <- rollup(a)
  grouping_set <- identity
  for (consume in list(inspect_grouping, summarize_with_margins,
                       expand_with_margins, nest_with_margins,
                       nest_by_with_margins)) {
    actual <- tryCatch(
      consume(data, .grouping = grouping_set(!!stored)), error = identity
    )
    expect_identical(actual, consume(data, .grouping = stored))
  }
})
