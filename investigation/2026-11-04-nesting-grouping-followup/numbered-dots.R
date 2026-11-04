pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)

capture <- function(expr) tryCatch(expr, error = identity)
same_answer <- function(actual, expected) {
  if (inherits(expected, "error")) {
    return(inherits(actual, class(expected)[[1L]]) &&
      identical(conditionMessage(actual), conditionMessage(expected)))
  }
  identical(actual, expected)
}

witnesses <- function() {
  input <- tibble::tibble(a = c("x", "y"), `..01` = c("s", "s"))
  selection <- inspect_grouping(
    input, .grouping = grouping_set(dplyr::all_of("..01")), .format = "list"
  )
  column <- capture(inspect_grouping(
    input, .grouping = grouping_set((`..01`)), .format = "list"
  ))
  stopifnot(
    identical(tidyselect::eval_select(quote((`..01`)), input), c(`..01` = 2L)),
    identical(selection$included, list("..01"))
  )
  bare <- function(...) {
    inspect_grouping(input, .grouping = grouping_sets(alias = ..1, a))
  }
  wrapped <- function(...) {
    inspect_grouping(input, .grouping = grouping_sets(alias = (..1), a))
  }
  empty <- capture(bare(, TRUE))
  tagged <- capture(wrapped(, TRUE))
  filled <- capture(wrapped(a, TRUE))
  stopifnot(
    inherits(empty, "marginplyr_error"),
    grepl("Argument 1 .* is empty", conditionMessage(empty)),
    inherits(filled, "marginplyr_error"),
    grepl("takes no named arguments", conditionMessage(filled), fixed = TRUE)
  )
  cat("parenthesized input column: ",
      if (inherits(column, "error")) conditionMessage(column) else column$included[[1L]],
      "\n", sep = "")
  cat("empty named numbered dot: ", conditionMessage(tagged), "\n", sep = "")
  c(column = identical(column, selection), empty = same_answer(tagged, empty))
}

if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  matched <- witnesses()
  print(matched)
  stopifnot(all(matched))
  quit(save = "no")
}

print(witnesses())
constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
consumers <- list(
  inspect = function(d, s) inspect_grouping(d, .grouping = s, .format = "list"),
  nest = function(d, s) nest_with_margins(d, .grouping = s, .margin_label = NULL),
  nest_by = function(d, s) nest_by_with_margins(d, .grouping = s, .margin_label = NULL),
  expand = function(d, s) expand_with_margins(d, .grouping = s, .margin_label = NULL),
  summarize = function(d, s) summarize_with_margins(
    d, n = dplyr::n(), .grouping = s, .margin_label = NULL
  )
)
parenthesize <- function(expr, depth) {
  for (i in seq_len(depth)) expr <- call("(", expr)
  expr
}

# These spellings are all admitted data names, unlike canonical ..1/..2.
aliases <- c("..01", "..001", "..+1", ".. 1", ".. +1",
             "..02", "..002", "..+2", ".. 2", ".. +2")
cols <- expand.grid(alias = aliases, constructor = constructors,
                    consumer = names(consumers), depth = 0:2)
cols$match <- FALSE
dots <- cols

for (i in seq_len(nrow(cols))) {
  case <- cols[i, ]
  alias <- as.character(case$alias)
  d <- rlang::set_names(
    tibble::tibble(a = c("x", "y"), b = c("s", "s")),
    c("a", alias)
  )
  constructor <- as.character(case$constructor)
  consume <- consumers[[as.character(case$consumer)]]
  ref <- parenthesize(rlang::sym(alias), case$depth)
  explicit <- rlang::call2(constructor, rlang::call2("all_of", alias, .ns = "dplyr"))
  supplied <- rlang::call2(constructor, ref)
  cols$match[[i]] <- same_answer(capture(consume(d, eval(supplied))), consume(d, eval(explicit)))

  # The input also carries the alias, but this wrapper actually forwards a
  # supplied specification. Even explicit source-alias collisions must keep
  # the original populated dot and the parent's nesting rules.
  literal <- rlang::call2(constructor, quote(grouping_set(a)))
  wrapper <- function(...) NULL
  body(wrapper) <- rlang::call2("consume", quote(d), supplied)
  result <- if (endsWith(alias, "1")) {
    capture(wrapper(grouping_set(a), stop("unused second dot was forced")))
  } else {
    capture(wrapper(stop("unused first dot was forced"), grouping_set(a)))
  }
  dots$match[[i]] <- same_answer(result, capture(consume(d, eval(literal))))
}
cat("\nColumn readings by depth\n")
print(with(cols, table(depth, match)))
cat("\nTrue forwarding with same-spelled input column\n")
print(with(dots, table(depth, match)))

make_wrapper <- function(constructor, dot, depth, tagged, consumer, input) {
  ref <- parenthesize(rlang::sym(dot), depth)
  arguments <- list(ref, quote(b))
  if (tagged) names(arguments) <- c("alias", "")
  specification <- rlang::call2(constructor, !!!arguments)
  wrapper <- function(...) NULL
  environment(wrapper) <- environment()
  body(wrapper) <- rlang::call2("consumer", quote(input), specification)
  wrapper
}
diag <- expand.grid(constructor = constructors, consumer = names(consumers),
                    depth = 0:2, tagged = c(FALSE, TRUE), empty = c(FALSE, TRUE),
                    dot = c("..1", "..02"))
diag$outcome <- character(nrow(diag))
for (i in seq_len(nrow(diag))) {
  case <- diag[i, ]
  dot <- as.character(case$dot)
  wrapper <- make_wrapper(
    as.character(case$constructor), dot, case$depth, case$tagged,
    consumers[[as.character(case$consumer)]], tibble::tibble(a = 1, b = 2)
  )
  result <- if (identical(dot, "..1")) {
    if (case$empty) capture(wrapper(, TRUE)) else capture(wrapper(a, TRUE))
  } else {
    if (case$empty) capture(wrapper(TRUE, , TRUE)) else capture(wrapper(TRUE, a, TRUE))
  }
  diag$outcome[[i]] <- if (!inherits(result, "error")) {
    "success"
  } else if (grepl("Argument 1 .* is empty", conditionMessage(result))) {
    "empty position"
  } else if (grepl("takes no named arguments", conditionMessage(result), fixed = TRUE)) {
    "named argument"
  } else {
    paste("other", conditionMessage(result))
  }
}
cat("\nTag and missingness controls\n")
print(with(diag, ftable(empty, tagged, depth, outcome)))
print(sessionInfo())
