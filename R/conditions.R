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
