# Dated evidence for the 2026-10-29 summary/grouping/share bug hunt.
# Run from the checkout root. --assert checks the desired mixed-type refusal.
# --wide additionally replays the 256 shape pairs and 1,200 seeded summaries.
pkgload::load_all(".", quiet = TRUE)
options(dplyr.summarise.inform = FALSE)
arguments <- commandArgs(trailingOnly = TRUE)
assert_desired <- "--assert" %in% arguments
wide <- "--wide" %in% arguments

capture <- function(expr) {
  tryCatch(suppressWarnings(force(expr)), error = identity)
}

show_value <- function(value) {
  if (inherits(value, "error")) {
    cat("Class:", paste(class(value), collapse = ", "), "\n")
    cat(conditionMessage(value), "\n")
  } else {
    print(value)
  }
}

run_ordinary <- function(input, dots) {
  rlang::eval_tidy(rlang::expr(dplyr::summarise(
    input, !!!dots, .by = dplyr::all_of("g")
  )))
}

run_margin <- function(input, dots) {
  rlang::eval_tidy(rlang::expr(summarize_with_margins(
    input, !!!dots, .by = dplyr::all_of("g")
  )))
}

mixed <- function(x) {
  if (length(x) > 1L) data.frame(z = sum(x)) else sum(x)
}

data <- data.frame(g = c(1L, 1L, 2L), x = 1:3)
dots <- rlang::exprs(mixed(x))
cat("MINIMAL: one unnamed summary returns a frame in one group, a scalar ",
    "in another\n", sep = "")
ordinary <- capture(run_ordinary(data, dots))
actual <- capture(run_margin(data, dots))
cat("Ordinary dplyr:\n")
show_value(ordinary)
cat("Margin summary:\n")
show_value(actual)
stopifnot(inherits(ordinary, "error"))

# Each input class and encounter order must retain the ordinary refusal.
inputs <- list(
  frame = data,
  tibble = tibble::as_tibble(data),
  table = data.table::as.data.table(data)
)
scalars <- list(
  integer = 1L, double = 1, character = "z", logical = TRUE,
  list = list(1L), matrix = matrix(1L, nrow = 1L),
  date = as.Date("2026-01-01")
)
expected_refusals <- 0L
missed_refusals <- 0L
for (input_name in names(inputs)) {
  for (scalar_name in names(scalars)) {
    for (reverse in c(FALSE, TRUE)) {
      value <- scalars[[scalar_name]]
      varying <- local({
        scalar <- value
        function(x) {
          if (length(x) > 1L) data.frame(z = sum(x)) else scalar
        }
      })
      input <- inputs[[input_name]]
      if (reverse) input <- input[3:1, ]
      ordinary_case <- capture(run_ordinary(
        input, rlang::exprs(varying(x))
      ))
      margin_case <- capture(run_margin(input, rlang::exprs(varying(x))))
      stopifnot(inherits(ordinary_case, "error"))
      expected_refusals <- expected_refusals + 1L
      if (!inherits(margin_case, "error")) {
        missed_refusals <- missed_refusals + 1L
      }
    }
  }
}
cat("MIXED-SHAPE MATRIX:", expected_refusals, "ordinary refusals;",
    missed_refusals, "Margin summaries accepted\n")

# A caller-written summary name bypasses assigned-name expansion.
named_dots <- rlang::exprs(out = mixed(x))
named_ordinary <- capture(run_ordinary(data, named_dots))
named_margin <- capture(run_margin(data, named_dots))
stopifnot(inherits(named_ordinary, "error"), inherits(named_margin, "error"))
cat("NAMED CONTROL: both refused incompatible values\n")

# Ordinary scalar and frame values continue to work, including sequential use.
scalar_only <- function(x) sum(x)
frame_only <- function(x) data.frame(z = sum(x))
for (input in inputs) {
  for (control in list(
    rlang::exprs(scalar_only(x)),
    rlang::exprs(frame_only(x), later = z * 2L)
  )) {
    expected <- run_ordinary(input, control)
    got <- run_margin(input, control)
    stopifnot(identical(as.data.frame(expected), as.data.frame(got)))
  }
}
cat("HOMOGENEOUS CONTROLS: six scalar/frame comparisons passed\n")

# The wrapper does not evaluate the expression twice; it changes type checking.
counter <- new.env(parent = emptyenv())
counter$calls <- 0L
counted <- function(x) {
  counter$calls <- counter$calls + 1L
  mixed(x)
}
invisible(capture(run_margin(data, rlang::exprs(counted(x)))))
stopifnot(counter$calls == 2L)
cat("EVALUATION CONTROL:", counter$calls, "calls for two groups\n")

# Between-set variation is a different boundary: each branch succeeds alone,
# but its output schema differs. This is retained as an observation only.
between <- data.frame(g = 1:2, x = 1:2)
cat("BETWEEN-SET CONTROL:\n")
show_value(capture(summarize_with_margins(
  between, mixed(x), .grouping = rollup(g), .margin_label = NULL
)))

# Reproduce the cause with ordinary dplyr, without private grouping aliases.
wrap_value <- function(value) {
  if (is.data.frame(value)) return(value)
  vctrs::new_data_frame(list("mixed(x)" = value), n = vctrs::vec_size(value))
}
cat("WRAPPING CONTROL: ordinary dplyr after the same per-value wrapping\n")
show_value(dplyr::summarise(
  data, wrap_value(mixed(.data$x)), .by = dplyr::all_of("g")
))

if (wide) {
  values <- list(
    int = 1L, dbl = 1, chr = "x", lgl = TRUE, nil = NULL,
    int0 = integer(), list = list(1L), mat = matrix(1L, nrow = 1L),
    df = data.frame(z = 1L), df0 = data.frame(z = integer()),
    tib = tibble::tibble(z = 1L), dflist = data.frame(z = I(list(1L))),
    dfdifferent = data.frame(a = 1L), df2 = data.frame(z = 1:2),
    dfempty = data.frame(), expr = quote(a)
  )
  shape_count <- 0L
  shape_differences <- 0L
  for (first in names(values)) {
    for (second in names(values)) {
      pair <- local({
        first_value <- values[[first]]
        second_value <- values[[second]]
        function(x) {
          if (length(x) > 1L) first_value else second_value
        }
      })
      expected <- capture(run_ordinary(data, rlang::exprs(pair(x))))
      got <- capture(run_margin(data, rlang::exprs(pair(x))))
      shape_count <- shape_count + 1L
      paired_error <- inherits(expected, "error") && inherits(got, "error")
      if (!paired_error && !isTRUE(all.equal(expected, got))) {
        shape_differences <- shape_differences + 1L
        cat("SHAPE DIFFERENCE:", first, second, "\n")
      }
    }
  }
  cat("SHAPES:", shape_count, "pairs;", shape_differences,
      "differences (frame/non-frame combinations)\n")

  set.seed(82910)
  preceding <- list(
    rlang::exprs(total = sum(x)),
    rlang::exprs(y = mean(x)),
    rlang::exprs(x = sum(x)),
    rlang::exprs(x = as.character(sum(x))),
    rlang::exprs(x = NULL),
    rlang::exprs(total = list(x)),
    rlang::exprs(y = tibble::tibble(a = sum(x))),
    rlang::exprs(tibble::tibble(total = sum(x), y = mean(x))),
    rlang::exprs(identity(tibble::tibble(total = sum(x), y = mean(x)))),
    rlang::exprs(data.frame(total = sum(x), y = mean(x))),
    rlang::exprs((data.frame(total = sum(x), y = mean(x))))
  )
  selectors <- list(
    rlang::expr(dplyr::everything()),
    rlang::expr(dplyr::where(is.numeric)),
    rlang::expr(dplyr::starts_with("t")),
    rlang::expr(dplyr::any_of(c("x", "y", "total"))),
    rlang::expr(c(x, y)),
    rlang::expr(c(dplyr::everything(), -y))
  )
  functions <- list(
    rlang::expr(sum), rlang::expr(~sum(.x)),
    rlang::expr(~length(.x)), rlang::expr(~length(.x) + dplyr::n()),
    rlang::expr(~mean(.x)), rlang::expr(list(n = length, m = mean)),
    rlang::expr(~tibble::tibble(lo = min(.x), hi = max(.x)))
  )
  forms <- c("across", "paren_across", "pick", "pick_list", "any", "all")
  paired_errors <- 0L
  successes <- 0L
  random_differences <- 0L
  for (i in seq_len(1200L)) {
    input <- tibble::tibble(
      g = sample(c("a", "b", "c"), 6L, replace = TRUE),
      x = sample(c(-2:2, NA), 6L, replace = TRUE),
      y = sample(1:5, 6L, replace = TRUE)
    )
    if (i %% 11L == 0L) input <- input[0L, ]
    before <- preceding[[sample.int(length(preceding), 1L)]]
    selection <- selectors[[sample.int(length(selectors), 1L)]]
    function_expr <- functions[[sample.int(length(functions), 1L)]]
    form <- sample(forms, 1L)
    after <- switch(form,
      across = rlang::expr(dplyr::across(
        !!selection, !!function_expr, .names = "out_{.col}_{.fn}",
        .unpack = TRUE
      )),
      paren_across = rlang::expr((dplyr::across)(
        !!selection, !!function_expr, .names = "out_{.col}_{.fn}",
        .unpack = TRUE
      )),
      pick = rlang::expr(dplyr::pick(!!selection)),
      pick_list = rlang::expr(list(dplyr::pick(!!selection))),
      any = rlang::expr(dplyr::if_any(!!selection, ~.x > 0)),
      all = rlang::expr(dplyr::if_all(!!selection, ~.x > 0))
    )
    random_dots <- append(before, list(after))
    if (form %in% c("pick_list", "any", "all")) {
      names(random_dots)[[length(random_dots)]] <- "end"
    }
    expected <- capture(run_ordinary(input, random_dots))
    got <- capture(run_margin(input, random_dots))
    if (inherits(expected, "error") && inherits(got, "error")) {
      paired_errors <- paired_errors + 1L
      next
    }
    if (!inherits(expected, "error")) successes <- successes + 1L
    if (!isTRUE(all.equal(expected, got))) {
      random_differences <- random_differences + 1L
      cat("RANDOM DIFFERENCE:", i, "\n")
      print(random_dots)
      show_value(expected)
      show_value(got)
    }
  }
  cat("SEEDED SUMMARIES: 1200 cases;", paired_errors, "paired errors;",
      successes, "oracle successes;", random_differences, "differences\n")
}

cat("VERSIONS\n", R.version.string, "\n", sep = "")
for (package in c("marginplyr", "dplyr", "tibble", "vctrs", "data.table")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
if (assert_desired) {
  stopifnot(inherits(actual, "error"), missed_refusals == 0L)
}
