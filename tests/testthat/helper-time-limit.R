# A time limit that expires where it is called, standing in for a round trip
# longer than the caller's bound. A test calls it from inside the statement it
# is about, so that it is that statement, and nothing planned before it, that
# the limit lands on. The wait evaluates R rather than sleeping because R
# checks a limit while evaluating, and a sleep that outlasts it does not
# deliver it.
expire_time_limit <- function() {
  setTimeLimit(elapsed = 0.1, transient = TRUE)
  deadline <- proc.time()[["elapsed"]] + 0.5
  while (proc.time()[["elapsed"]] < deadline) NULL
}

# Evaluates `code`, in which `expire_time_limit()` is reached, and returns
# what it raised, or `NULL` when it returned. R's byte compiler catches every
# error raised while it compiles a closure, a time limit among them, and
# `load_all()` leaves the package's closures for it to compile on first use,
# so it is switched off here: the package's own handlers are then the only
# ones the limit can reach.
catch_time_limited <- function(code) {
  jit <- compiler::enableJIT(0)
  on.exit(compiler::enableJIT(jit), add = TRUE)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
  tryCatch({
    code
    NULL
  }, error = identity)
}
