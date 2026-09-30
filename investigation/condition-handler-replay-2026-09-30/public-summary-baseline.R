# This records the unresolved external-handler case in the #756 working tree.
pkgload::load_all(".", quiet = TRUE)
first <- structure(list(message = "first cancellation", payload = new.env()),
                   class = c("interrupt", "condition"))
handler_error <- structure(
  list(message = "caller handler failure", call = NULL,
       parent = simpleError("handler's original cause")),
  class = c("probe_handler_failure", "error", "condition")
)
input <- data.frame(g = c("a", "b"), v = c(2, 5))
original_input <- input
old_options <- options(warn = 2L)
for (mode in c("error", "native_interrupt")) {
  events <- character()
  branch <- function(v) {
    if (length(v) == 2L) {
      events <<- c(events, "later branch")
      signalCondition(first)
      rlang::interrupt()
    }
    events <<- c(events, "earlier side effect")
    warning("earlier warning")
    sum(v)
  }
  outcome <- tryCatch(
    withCallingHandlers(
      summarize_with_margins(input, z = branch(.data$v),
                             .grouping = rollup("g")),
      warning = function(w) {
        events <<- c(events, "caller warning handler")
        if (mode == "error") stop(handler_error)
        tools::pskill(Sys.getpid(), 2L)
        Sys.sleep(0.1)
        stop("native interrupt was not delivered")
      }
    ),
    interrupt = identity, error = identity
  )
  stopifnot(identical(events, c("earlier side effect", "earlier side effect",
                              "later branch", "caller warning handler")),
            identical(input, original_input), getOption("warn") == 2L)
  if (mode == "error") {
    stopifnot(identical(outcome, handler_error), !inherits(outcome, "interrupt"))
  } else {
    stopifnot(inherits(outcome, "interrupt"), !identical(outcome, first),
              is.null(outcome$interrupt))
  }
  cat(mode, ": escaping classes = ", paste(class(outcome), collapse = "/"),
      "; first cancellation retained = ",
      identical(outcome, first) || identical(outcome$interrupt, first), "\n",
      sep = "")
}
options(old_options)
stopifnot(identical(getOption("warn"), old_options$warn))
stopifnot(identical(
  summarize_with_margins(input, z = sum(.data$v), .grouping = rollup("g"))$z,
  c(2, 5, 7)
))
cat("unchanged input/options and subsequent valid summary confirmed\n")
