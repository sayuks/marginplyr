read_stack <- function() {
  .Internal(.addCondHands(NULL, NULL, parent.frame(), NULL, TRUE))
}

interleave <- function(expr, original) {
  live <- read_stack()
  captures <- as.list(live)[seq_len(length(live) - length(original))]
  rebuilt <- captures
  for (entry in as.list(original)) {
    rebuilt <- c(rebuilt, list(entry), captures)
  }
  .Internal(.resetCondHands(as.pairlist(rebuilt)))
  force(expr)
}

capture_replay <- function(expr) {
  original <- read_stack()
  tryCatch(
    interleave(expr, original),
    error = function(e) list(kind = "error", condition = e),
    interrupt = function(e) list(kind = "interrupt", condition = e)
  )
}

failure <- structure(list(message = "handler failure", call = NULL, payload = new.env()),
                     class = c("probe_failure", "error", "condition"))
interrupt <- structure(list(message = "handler interrupt", payload = new.env()),
                       class = c("probe_interrupt", "interrupt", "condition"))
observed <- character()
outer <- tryCatch(
  withCallingHandlers(
    capture_replay(warning("first")),
    warning = function(w) stop(failure)
  ),
  error = function(e) list(kind = "outer error", condition = e)
)
stopifnot(identical(outer$kind, "error"), identical(outer$condition, failure))
cat("caller handler failure: exact condition captured\n")

outer <- tryCatch(
  withCallingHandlers(
    capture_replay(warning("first")),
    warning = function(w) {
      signalCondition(interrupt)
      stop("a returning interrupt was not caught")
    }
  ),
  interrupt = function(e) list(kind = "outer interrupt", condition = e)
)
stopifnot(identical(outer$kind, "interrupt"), identical(outer$condition, interrupt))
cat("caller handler interrupt: exact condition captured\n")

old <- options(warn = 2)
observed <- character()
outer <- withCallingHandlers(
  withCallingHandlers(
    capture_replay(warning("first")),
    warning = function(w) {
      observed <<- c(observed, "inner")
    }
  ),
  warning = function(w) {
    observed <<- c(observed, "outer")
    invokeRestart("muffleWarning")
  }
)
stopifnot(identical(outer, "first"), identical(observed, c("inner", "outer")), getOption("warn") == 2)
cat("nested calling handlers: native order and muffleWarning retained at warn=2\n")

outer <- withCallingHandlers(capture_replay(warning("first")), warning = function(w) NULL)
stopifnot(identical(outer$kind, "error"), grepl("converted from warning", conditionMessage(outer$condition)))
cat("warn=2: native conversion captured after returning caller handler\n")
options(old)

observed <- character()
outer <- withCallingHandlers(
  withCallingHandlers(
    capture_replay(warning("first")),
    warning = function(w) {
      observed <<- c(observed, "inner")
      warning("nested")
    }
  ),
  warning = function(w) stop(failure)
)
stopifnot(identical(outer$kind, "error"), identical(outer$condition, failure), identical(observed, "inner"))
cat("nested warning from caller handler: downstream failure captured\n")

observed <- character()
outer <- withCallingHandlers({
  withCallingHandlers(capture_replay(warning("first")), warning = function(w) invokeRestart("muffleWarning"))
  warning("later")
}, warning = function(w) {
  observed <<- c(observed, conditionMessage(w))
  invokeRestart("muffleWarning")
})
stopifnot(identical(observed, "later"))
cat("after replay: caller handler stack restored\n")

cat(R.version.string, "\n")

native <- withCallingHandlers(
  capture_replay(warning("first")),
  warning = function(w) {
    tools::pskill(Sys.getpid(), 2L)
    Sys.sleep(0.1)
    stop("native interrupt was not caught")
  }
)
stopifnot(identical(native$kind, "interrupt"), inherits(native$condition, "interrupt"))
cat("caller handler self-delivered SIGINT: native interrupt captured\n")
