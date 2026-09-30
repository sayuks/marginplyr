# Public #756 acceptance oracle. Exit 1 means an accepted contract is unmet.
# Run from the candidate checkout; optional first argument sets the raw RDS path.
pkgload::load_all(".", quiet = TRUE)
original_warn <- getOption("warn")
raw_path <- commandArgs(TRUE)
if (length(raw_path) > 1L) stop("Supply at most one output RDS path")
if (!length(raw_path)) raw_path <- tempfile("marginplyr-handler-", fileext = ".rds")
new_interrupt <- function(message) {
  structure(list(message = message, payload = new.env(parent = emptyenv())),
            class = c("interrupt", "condition"))
}
capture <- function(expr) tryCatch(expr, error = identity, interrupt = identity)
is_interrupt <- function(x) inherits(x, "interrupt") && !inherits(x, "error")
retains_condition <- function(outcome, original) {
  while (inherits(outcome, "condition")) {
    if (identical(outcome, original)) return(TRUE)
    outcome <- outcome$parent
  }
  FALSE
}

run_case <- function(kind, warn = 2L) {
  old <- options(warn = warn)
  on.exit(options(old), add = TRUE)
  input <- data.frame(g = c("a", "b"), h = c("u", "v"), v = c(2, 5))
  original <- input
  first <- new_interrupt("first cancellation")
  second <- new_interrupt("handler cancellation")
  failure <- errorCondition("handler failure", parent = simpleError("handler parent"),
                            payload = new.env(parent = emptyenv()), class = "audit_handler_error")
  execution <- errorCondition("execution failure", parent = simpleError("execution parent"),
                              payload = new.env(parent = emptyenv()), class = "audit_execution_error")
  failure_bytes <- serialize(failure, NULL)
  execution_bytes <- serialize(execution, NULL)
  events <- character()
  observed <- list()
  effects <- numeric()
  outcome <- NULL
  branch <- function(v, omitted) {
    if (length(v) == 2L) {
      events <<- c(events, "later branch")
      if (kind == "prior_error_interrupt") stop(execution)
      signalCondition(first)
      rlang::interrupt()
    }
    effects <<- c(effects, sum(v))
    events <<- c(events, if (omitted == 0L) "produce first" else "produce second")
    warning(if (omitted == 0L) "first warning" else "second warning")
    sum(v)
  }
  operation <- function() {
    summarize_with_margins(
      input, z = branch(.data$v, grouping_bit(g)),
      .grouping = grouping_sets(grouping_set("g"), grouping_set("g"),
                                grouping_set("h"), grouping_set()),
      .duplicates = "keep"
    )
  }
  handler <- function(cnd) {
    events <<- c(events, "replay")
    observed[[length(observed) + 1L]] <<- cnd
    if (kind == "handler_error") stop(failure)
    if (kind %in% c("handler_interrupt", "prior_error_interrupt")) {
      signalCondition(second)
      rlang::interrupt()
    }
    invokeRestart("muffleWarning")
  }
  if (kind == "unavailable") {
    older <- function(cnd) {
      if (identical(conditionMessage(cnd), "outer trigger")) {
        outcome <<- capture(operation())
        invokeRestart("muffleWarning")
      }
      events <<- c(events, "unavailable older")
    }
    withCallingHandlers(
      withCallingHandlers(warning("outer trigger"), warning = function(cnd) {
        events <<- c(events, "outer newer")
      }), warning = older
    )
  } else if (kind == "direct_callback") {
    callback <- function(cnd) {
      if (identical(conditionMessage(cnd), "direct invocation")) {
        outcome <<- capture(operation())
      } else handler(cnd)
    }
    withCallingHandlers(
      do.call(callback, list(warningCondition("direct invocation")),
              envir = .GlobalEnv), warning = callback
    )
  } else if (kind == "restart_token") {
    outcome <- capture(withRestarts({
      token <- findRestart("caller_escape")
      withCallingHandlers(operation(), warning = function(cnd) {
        events <<- c(events, "replay", "caller restart requested")
        observed[[length(observed) + 1L]] <<- cnd
        invokeRestart(token, "caller transfer")
      })
    }, caller_escape = function(value) {
      events <<- c(events, "caller restart completed")
      value
    }))
  } else if (kind == "conversion") {
    outcome <- capture(withCallingHandlers(operation(), warning = function(cnd) {
      events <<- c(events, "replay")
      observed[[length(observed) + 1L]] <<- cnd
    }))
  } else {
    outcome <- capture(withCallingHandlers(operation(), warning = handler))
  }
  # The public outcome is retained before rendering it or probing another call.
  raw <- list(outcome = outcome, first = first, second = second,
              handler_error = failure, execution_error = execution,
              effects = effects, events = events, observed = observed)
  stable <- identical(input, original) && identical(getOption("warn"), warn) &&
    identical(serialize(failure, NULL), failure_bytes) &&
    identical(serialize(execution, NULL), execution_bytes)
  later <- capture(summarize_with_margins(
    input, z = sum(.data$v), .grouping = rollup("g")
  ))
  base_checks <- c(
    reached = "later branch" %in% events,
    effects = identical(effects, rep(c(2, 5), 3L)),
    input_options_originals = stable,
    later_summary = is.data.frame(later) && identical(later$z, c(2, 5, 7))
  )
  fields <- if (is.list(outcome)) outcome else list()
  retained_first <- identical(outcome, first) ||
    (is.list(outcome) && identical(fields$interrupt, first))
  checks <- switch(kind,
    handler_error = c(
      interruption = is_interrupt(outcome), first = retained_first,
      exact_handler_error = identical(fields$replay_error, failure),
      error_parent = identical(fields$replay_error$parent, failure$parent),
      error_payload = identical(fields$replay_error$payload, failure$payload),
      stopped_replay = length(observed) == 1L
    ),
    handler_interrupt = c(
      interruption = is_interrupt(outcome), first = retained_first,
      no_invented_error = is.null(fields$replay_error), stopped_replay = length(observed) == 1L
    ),
    prior_error_interrupt = c(
      interruption = is_interrupt(outcome), original_interrupt = identical(fields$interrupt, second),
      exact_execution_chain = retains_condition(fields$parent, execution),
      stopped_replay = length(observed) == 1L
    ),
    muffle = c(
      first = identical(outcome, first), all_replayed = length(observed) == 2L,
      ordered = length(observed) == 2L &&
        grepl("first warning", conditionMessage(observed[[1L]]), fixed = TRUE) &&
        grepl("second warning", conditionMessage(observed[[2L]]), fixed = TRUE),
      count = length(observed) > 0L &&
        grepl("1 further grouping set", conditionMessage(observed[[1L]]), fixed = TRUE)
    ),
    conversion = c(
      interruption = is_interrupt(outcome), first = retained_first,
      conversion_retained = inherits(fields$replay_error, "error") &&
        grepl("converted from warning", conditionMessage(fields$replay_error),
              fixed = TRUE),
      stopped_replay = length(observed) == 1L
    ),
    unavailable = c(
      no_reactivated_handler = sum(events == "outer newer") == 1L &&
        !"unavailable older" %in% events,
      first = retained_first, conversion_retained = inherits(fields$replay_error, "error")
    ),
    direct_callback = c(first = identical(outcome, first), available = length(observed) == 2L),
    restart_token = c(
      original_transfer = identical(outcome, "caller transfer"),
      original_restart = sum(events == "caller restart completed") == 1L,
      stopped_replay = length(observed) == 1L
    )
  )
  raw$checks <- c(base_checks, checks)
  raw$accepted <- all(raw$checks)
  raw
}

cases <- list()
for (kind in c("handler_error", "handler_interrupt", "prior_error_interrupt", "muffle")) {
  for (warn in c(1L, 2L)) cases[[paste(kind, warn, sep = "-")]] <- run_case(kind, warn)
}
for (kind in c("conversion", "unavailable", "direct_callback", "restart_token")) {
  cases[[kind]] <- run_case(kind)
}
stopifnot(identical(getOption("warn"), original_warn))
for (name in names(cases)) {
  result <- cases[[name]]
  failed <- names(result$checks)[!result$checks]
  cat(name, " accepted=", result$accepted,
      "; failed=", paste(failed, collapse = ","),
      "; events=", paste(result$events, collapse = ">"), "\n", sep = "")
}
saveRDS(list(source = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
             session = sessionInfo(), cases = cases), raw_path[[1L]])
cat("Raw outcomes:", raw_path[[1L]], "\n")
quit(status = if (all(vapply(cases, `[[`, logical(1), "accepted"))) 0L else 1L)
