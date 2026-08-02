# The only constructor for Package conditions. Every error a caller can avoid by
# rewriting the call within the documented public interface goes through here;
# unreachable invariants and upstream defects use bare `stop()` or `stopifnot()`
# instead. The rule and the reasoning behind it are recorded in
# design/adr/0015-separate-package-conditions-from-internal-invariants.md, and
# the contract it promises is documented in `?marginplyr`.
#
# `class` adds a narrower subclass for handlers marginplyr itself needs. Those
# subclasses stay implementation details; `marginplyr_error` is the promised
# public class.
abort_marginplyr <- function(message,
                             ...,
                             class = NULL,
                             call = rlang::caller_call()) {
  rlang::abort(
    message = message,
    ...,
    class = c(class, "marginplyr_error"),
    call = call
  )
}
