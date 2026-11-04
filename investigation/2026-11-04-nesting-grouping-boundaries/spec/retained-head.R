# Replays the 2026-11-04 constructor-shaped selection retention observation.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
assert_desired <- "--assert" %in% commandArgs(trailingOnly = TRUE)

input <- tibble::tibble(a = c("x", "y"), b = c(1L, 1L), row_id = 1:2)
consumers <- list(
  inspect = function(spec) {
    inspect_grouping(input, .grouping = spec, .format = "list")
  },
  nest = function(spec) nest_with_margins(input, .grouping = spec),
  nest_by = function(spec) nest_by_with_margins(input, .grouping = spec),
  expand = function(spec) expand_with_margins(input, .grouping = spec),
  summarize = function(spec) {
    summarize_with_margins(input, n = dplyr::n(), .grouping = spec)
  }
)

# This mask is stateless. The first lookup sees the caller's original closure;
# replay instead exposes marginplyr's different, retained-value closure.
minimal <- local({
  rollup <- function() "a"
  `(` <- function(f) {
    if (!is.function(f) || identical(f, rollup)) return(f)
    function() "b"
  }
  list(
    native = names(tidyselect::eval_select(rlang::quo((rollup)()), input)),
    plain = consumers$inspect(grouping_sets(rollup()))$included[[1L]],
    wrapped = consumers$inspect(grouping_sets((rollup)()))$included[[1L]]
  )
})
print(minimal)
stopifnot(identical(minimal$native, "a"), identical(minimal$plain, "a"))
if (!assert_desired) stopifnot(identical(minimal$wrapped, "b"))
selector_warning <- simpleWarning("second head evaluation warned")
selector_error <- simpleError("second head evaluation failed")
contains_error <- function(cnd) {
  if (!inherits(cnd, "error")) {
    return(FALSE)
  }
  identical(cnd, selector_error) || contains_error(cnd$parent)
}

probe <- function(verb, constructor, qualification, shape, behavior) {
  calls <- 0L
  head_calls <- 0L
  warn_count <- 0L
  choose <- function(...) {
    calls <<- calls + 1L
    if (behavior == "nested") grouping_set(a) else "a"
  }
  env <- rlang::env(globalenv())
  assign(constructor, choose, envir = env)
  if (qualification != "bare") {
    assign(qualification, function(...) choose, envir = env)
  }
  if (behavior != "base") {
    env[["("]] <- function(x) {
      if (is.function(x)) {
        head_calls <<- head_calls + 1L
        if (head_calls == 2L) {
          if (behavior == "replace") return(function(...) "b")
          if (behavior == "warn") warning(selector_warning)
          if (behavior == "error") stop(selector_error)
        }
      }
      x
    }
  }
  head <- rlang::sym(constructor)
  if (qualification != "bare") {
    head <- rlang::call2(qualification, quote(marginplyr), head)
  }
  if (shape == "head") head <- rlang::call2("(", head)
  expr <- rlang::call2(head)
  if (shape == "outer") expr <- rlang::call2("(", expr)
  selector <- rlang::new_quosure(expr, env)
  spec <- grouping_sets(!!selector)

  consume <- consumers[[verb]]
  expected <- consume(grouping_sets(a))
  alternative <- consume(grouping_sets(b))
  actual <- tryCatch(
    withCallingHandlers(
      consume(spec),
      warning = function(cnd) {
        stopifnot(identical(cnd, selector_warning))
        warn_count <<- warn_count + 1L
        invokeRestart("muffleWarning")
      }
    ),
    error = identity
  )
  outcome <- if (identical(actual, expected)) {
    "first selection"
  } else if (identical(actual, alternative)) {
    "second head changed selection"
  } else if (contains_error(actual)) {
    "second head failed"
  } else {
    stop("Unexpected result: ", paste(capture.output(str(actual)), collapse = " "))
  }
  data.frame(
    verb, constructor, qualification, shape, behavior, outcome,
    calls, head_calls, warn_count
  )
}

cases <- expand.grid(
  verb = names(consumers),
  constructor = c("grouping_set", "grouping_sets", "rollup", "cube",
                  "grouping_spec"),
  qualification = c("bare", "::", ":::"),
  shape = c("plain", "outer", "head"),
  behavior = c("replace", "warn", "error", "nested", "base"),
  stringsAsFactors = FALSE
)
outcomes <- do.call(rbind, lapply(seq_len(nrow(cases)), function(i) {
  do.call(probe, as.list(cases[i, ]))
}))
print(with(outcomes, table(behavior, shape, outcome)))
print(with(outcomes, table(behavior, shape, head_calls)))
print(with(outcomes, table(behavior, shape, warn_count)))
print(sessionInfo())

if (assert_desired) {
  stopifnot(identical(minimal$wrapped, "a"))
  stopifnot(
    all(outcomes$calls == 1L),
    all(outcomes$head_calls <= 1L),
    all(outcomes$outcome == "first selection"),
    all(outcomes$warn_count == 0L)
  )
} else {
  replay <- with(outcomes, shape == "head" &
                   behavior %in% c("replace", "warn", "error"))
  changed <- replay & outcomes$behavior == "replace"
  failed <- replay & outcomes$behavior == "error"
  warned <- replay & outcomes$behavior == "warn"
  stopifnot(
    all(outcomes$calls == 1L),
    all(outcomes$head_calls[replay] == 2L),
    all(outcomes$head_calls[!replay] <= 1L),
    all(outcomes$outcome[changed] == "second head changed selection"),
    all(outcomes$outcome[failed] == "second head failed"),
    all(outcomes$outcome[!changed & !failed] == "first selection"),
    all(outcomes$warn_count[warned] == 1L),
    all(outcomes$warn_count[!warned] == 0L)
  )
}
