args <- commandArgs(TRUE)
route <- args[[1L]]
handler <- args[[2L]]
callbacks <- args[[3L]]
log_event <- function(event, ...) {
  cat(jsonlite::toJSON(c(list(event = event), list(...)), auto_unbox = TRUE, null = "null"), "\n", sep = "")
  flush.console()
}
original_error <- structure(list(message = "execution failure", call = quote(caller_summary()), parent = simpleError("root cause")), class = c("execution_failure", "error", "condition"))
original_error_bytes <- serialize(original_error, NULL)
original_interrupt <- tryCatch(rlang::interrupt(), interrupt = identity)
replay_error <- simpleError("warning replay failure")
rich <- original_interrupt
rich$message <- "Interrupted after an execution failure"
rich$interrupt <- original_interrupt
rich$parent <- original_error
rich$replay_error <- replay_error
condition_event <- function(event, cnd) {
  log_event(event, class = class(cnd), rich = identical(cnd, rich), inherits_error = inherits(cnd, "error"), parent_retained = identical(cnd$parent, original_error), original_interrupt_retained = identical(cnd$interrupt, original_interrupt), replay_retained = identical(cnd$replay_error, replay_error), original_error_untouched = identical(serialize(original_error, NULL), original_error_bytes))
}
error_hook <- function() log_event("error_option")
interrupt_hook <- function() log_event("interrupt_option")
options(error = NULL, interrupt = NULL)
if(callbacks == "error_function") options(error = error_hook)
if(callbacks == "error_expression") options(error = quote(log_event("error_option_expression")))
if(callbacks == "interrupt_function") options(interrupt = interrupt_hook)
if(callbacks == "both") options(error = error_hook, interrupt = interrupt_hook)
log_event("configuration", route = route, handler = handler, callbacks = callbacks, R = as.character(getRversion()), rlang = as.character(packageVersion("rlang")), platform = R.version$platform)
run <- function() {
  if(route == "rich_then_native") signalCondition(rich)
  rlang::interrupt()
  log_event("returned_from_signaler")
}
error_handler <- function(cnd) {condition_event("error_handler", cnd); cnd}
interrupt_handler <- function(cnd) {condition_event("interrupt_handler", cnd); cnd}
condition_handler <- function(cnd) {condition_event("condition_handler", cnd); cnd}
calling_handler <- function(cnd) condition_event("calling_handler", cnd)
if(handler == "mixed") {
  out <- tryCatch(run(), error = error_handler, interrupt = interrupt_handler)
  log_event("handled", rich = identical(out, rich))
}
if(handler == "calling_and_exiting") {
  out <- tryCatch(withCallingHandlers(run(), interrupt = calling_handler), error = error_handler, interrupt = interrupt_handler)
  log_event("handled", rich = identical(out, rich))
}
if(handler == "condition_only") {
  out <- tryCatch(run(), condition = condition_handler)
  log_event("handled", rich = identical(out, rich))
}
if(handler == "error_only") {
  out <- tryCatch(run(), error = error_handler)
  log_event("after_error_only")
}
if(handler == "calling_return") withCallingHandlers(run(), interrupt = calling_handler)
if(handler == "global_return") {globalCallingHandlers(interrupt = calling_handler); run()}
if(handler == "no_handler") run()
log_event("end_of_script")
