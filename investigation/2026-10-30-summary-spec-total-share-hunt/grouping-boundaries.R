suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
options(lifecycle_verbosity = "quiet", width = 110)
observe <- function(expr) tryCatch(force(expr), error = identity)
checks <- 0L
record <- function(actual, expected) {
  checks <<- checks + 1L
  if (!identical(actual, expected)) { str(actual); str(expected); stop("boundary mismatch") }
}
zero_sources <- list(zero_zero = data.frame(), zero_three = data.frame(row.names = 1:3),
                     named_empty = data.frame(a = integer(), b = character()))
empty_exprs <- list(quote(NULL), quote(grouping_set()), quote(grouping_spec()), quote(grouping_sets(NULL)),
                    quote(grouping_set(NULL)), quote(grouping_set(integer())), quote(grouping_spec(NULL)))
by_exprs <- list(quote(NULL), quote(integer()), quote(character()), quote(tidyselect::any_of("absent")), quote(c()))
for (source in zero_sources) {
  for (expr in empty_exprs) for (by in by_exprs) for (fmt in c("text", "list")) {
    actual <- rlang::inject(inspect_grouping(source, .by = !!by, .grouping = !!expr, .format = fmt))
    record(class(actual), c("tbl_df", "tbl", "data.frame"))
    record(nrow(actual), 1L)
    record(actual$grouping_id, 0L)
    record(actual$included, if (fmt == "list") list(character()) else "()")
    record(actual$fixed, if (fmt == "list") list(character()) else "()")
  }
}
# Resolution from each quosure's own environment, including a different local
# function with the same spelling in the outer frame.
source <- data.frame(a = 1, b = "b", c = 2L)
outer_env <- rlang::env(globalenv(), cols = "a", helper = function() "a")
inner_env <- rlang::env(globalenv(), cols = "b", helper = function() "b")
forms <- list(quote(cols), quote(tidyselect::all_of(cols)), quote(tidyselect::all_of(helper())), quote(where(is.character)))
for (expr in forms) {
  q <- rlang::new_quosure(expr, inner_env)
  for (depth in 0:3) {
    injected <- q
    for (i in seq_len(depth)) injected <- rlang::new_quosure(rlang::call2("(", injected), outer_env)
    actual <- eval(rlang::expr(inspect_grouping(source, .grouping = grouping_set(!!injected), .format = "list")), outer_env)
    record(actual$included, list("b"))
    actual_by <- eval(rlang::expr(inspect_grouping(source, .by = !!injected, .format = "list")), outer_env)
    record(actual_by$fixed, list("b"))
  }
}
# Constructors evaluated once, ordinary column-selection bindings evaluated once.
for (ctor in c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")) {
  for (kind in c("constructor", "name", "quosure")) {
    reads <- 0L
    env <- rlang::env(globalenv(), source = source)
    makeActiveBinding("selected", function() { reads <<- reads + 1L; "a" }, env)
    expr <- if (kind == "constructor") {
      env$rollup <- function(...) { reads <<- reads + 1L; grouping_set(a) }
      quote(rollup(a))
    } else if (kind == "name") quote(selected) else rlang::new_quosure(quote(selected), env)
    spec <- eval(rlang::call2(ctor, expr), env)
    actual <- observe(inspect_grouping(source, .grouping = spec, .format = "list"))
    if (ctor == "grouping_set" && kind == "constructor") {
      record(inherits(actual, "marginplyr_error"), TRUE)
    } else {
      record(actual$included[[1L]], "a")
    }
    record(reads, 1L)
  }
}
# S3 dispatch through classed helper arguments must use real column types.
probe <- function(x) UseMethod("probe")
probe.selector <- function(x) names(tidyselect::peek_data())[vapply(tidyselect::peek_data(), is.character, logical(1))]
classed <- structure("a", class = "selector")
forms <- list(quote(tidyselect::all_of(probe(classed))), quote(tidyselect::any_of(probe(classed))),
              quote(tidyselect::all_of((function(x) probe(x))(classed))))
for (expr in forms) for (role in c("by", "grouping")) {
  q <- rlang::new_quosure(expr, environment())
  result <- if (role == "by") rlang::inject(inspect_grouping(source, .by = !!q, .format = "list")) else {
    rlang::inject(inspect_grouping(source, .grouping = grouping_set(!!q), .format = "list"))
  }
  record(result[[if (role == "by") "fixed" else "included"]], list("b"))
}
cat("FINAL PASS", checks, "boundary assertions\n")
