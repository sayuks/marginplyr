# Dated investigation evidence; investigated 2026-10-29 on main 4c63bb9.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)
options(lifecycle_verbosity = "quiet")

constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
data <- data.frame(a = c("x", "y"), b = c(1L, 2L))
inputs <- list(local = data, dtplyr = dtplyr::lazy_dt(data))
wrap <- function(expr, depth) {
  for (i in seq_len(depth)) expr <- rlang::call2("(", expr)
  expr
}
consumers <- function(input) {
  list(
    inspect = function(spec) inspect_grouping(input, .grouping = spec),
    nest = function(spec) nest_with_margins(input, .grouping = spec),
    nest_by = function(spec) nest_by_with_margins(input, .grouping = spec)
  )
}
observe <- function(fun, spec) tryCatch(fun(spec), error = identity)
checks <- 0L
for (backend in names(inputs)) {
  for (constructor in constructors) {
    for (depth in c(0L, 1L, 4L)) {
      for (typed in c(FALSE, TRUE)) {
        for (consume in consumers(inputs[[backend]])) {
          reads <- 0L
          env <- rlang::env(rlang::current_env())
          child <- if (typed) grouping_set(where(is.character)) else grouping_set(a)
          makeActiveBinding("bound", function() {
            reads <<- reads + 1L
            child
          }, env)
          invisible(env$bound)
          stopifnot(reads == 1L)
          spec <- eval(rlang::call2(constructor, wrap(quote(bound), depth),
                                  .ns = "marginplyr"), env)
          reads <- 0L
          result <- observe(consume, spec)
          stopifnot(reads == 1L)
          if (identical(constructor, "grouping_set")) {
            stopifnot(inherits(result, "marginplyr_error"))
          } else {
            stopifnot(!inherits(result, "error"))
          }
          checks <- checks + 1L

          # An ordinary selection is retained per occurrence, even when an
          # active binding changes which column it answers with on each read.
          reads <- 0L
          ordinary <- rlang::env(rlang::current_env())
          makeActiveBinding("selected", function() {
            reads <<- reads + 1L
            if (reads %% 2L == 1L) "a" else "b"
          }, ordinary)
          spec <- eval(rlang::call2(constructor, wrap(quote(selected), depth),
                                  .ns = "marginplyr"), ordinary)
          control <- eval(rlang::call2(constructor, quote(a), .ns = "marginplyr"))
          result <- consume(spec)
          stopifnot(reads == 1L, identical(result, consume(control)))
          checks <- checks + 1L

          # A constructor-shaped ordinary function forces its argument only
          # during preflight, preserving its first returned selection.
          reads <- 0L
          forced <- 0L
          ordinary$rollup <- function(x) {
            reads <<- reads + 1L
            force(x)
            if (reads == 1L) "a" else "b"
          }
          ordinary$argument <- function() {
            forced <<- forced + 1L
            invisible(NULL)
          }
          spec <- eval(rlang::call2(constructor,
                                   wrap(quote(rollup(argument())), depth),
                                   .ns = "marginplyr"), ordinary)
          result <- consume(spec)
          stopifnot(reads == 1L, forced == 1L,
                    identical(result, consume(control)))
          checks <- checks + 1L
        }
      }
    }
  }
}
cat("PASS:", checks, "active-binding / constructor evaluations and public consumer comparisons\n")

# A colliding name is read once where the parent admits a composite, and never
# where the parent admits no nested kind, through each consumer and backend.
collisions <- 0L
collision_data <- data
collision_data$bound <- c("u", "v")
for (input in list(collision_data, dtplyr::lazy_dt(collision_data))) {
  for (constructor in constructors) {
    for (depth in c(0L, 3L)) {
      for (consume in consumers(input)) {
        reads <- 0L
        env <- rlang::env(rlang::current_env())
        makeActiveBinding("bound", function() {
          reads <<- reads + 1L
          grouping_set(a)
        }, env)
        spec <- eval(rlang::call2(constructor, wrap(quote(bound), depth),
                                 .ns = "marginplyr"), env)
        observed <- observe(consume, spec)
        if (identical(constructor, "grouping_set")) {
          stopifnot(reads == 0L, !inherits(observed, "error"))
        } else {
          stopifnot(reads == 1L, inherits(observed, "marginplyr_error"),
                    grepl("both a column", conditionMessage(observed), fixed = TRUE))
        }
        collisions <- collisions + 1L
      }
    }
  }
}
cat("PASS:", collisions, "nested-name ambiguity / non-reading controls\n")

# Promises cache forcing independently of active bindings. Repeated syntax
# must still capture the wrapper caller's environment and force once.
forced <- 0L
make_child <- function() {
  forced <<- forced + 1L
  grouping_set(a)
}
forward <- function(value) grouping_spec(value, value)
promised <- forward(make_child())
force_counts <- integer()
for (input in inputs) {
  for (consume in consumers(input)) {
    forced <- 0L
    observed <- consume(promised)
    stopifnot(!inherits(observed, "error"))
    force_counts <- c(force_counts, forced)
  }
}
stopifnot(identical(force_counts, c(1L, rep(0L, 5L))))
cat("PASS: one promise forced once across repeated occurrences and reused public calls\n")
