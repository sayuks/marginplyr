# Independent native-action comparison at the public summary boundary (#756).
args <- commandArgs(TRUE)
.libPaths(c(args[[1L]], .libPaths()))
library(marginplyr)
.data <- rlang::.data
route <- args[[2L]]
handler <- args[[3L]]
hooks <- args[[4L]]
output <- args[[5L]]
state <- new.env(parent = emptyenv())
state$events <- character()
state$observed <- list()
record <- function(event, cnd = NULL) {
  state$events <- c(state$events, event)
  if (!is.null(cnd)) {
    state$observed[[length(state$observed) + 1L]] <- cnd
  }
  saveRDS(list(events = state$events, observed = state$observed), output)
}
if (hooks %in% c("error_function", "both")) {
  options(error = function() record("error hook"))
}
if (hooks == "error_expression") options(error = quote(record("error hook")))
if (hooks %in% c("interrupt", "both")) {
  options(interrupt = function() record("interrupt hook"))
}
options(warn = 2L)
branch <- function(v) {
  if (length(v) == 2L) {
    record("later branch")
    rlang::interrupt()
  }
  record("earlier effect")
  warning("earlier warning")
  sum(v)
}
operation <- function() {
  if (route == "native") rlang::interrupt()
  summarize_with_margins(data.frame(g = c("a", "b"), v = c(2, 5)),
                         z = branch(.data$v), .grouping = rollup("g"))
}
calling <- function() {
  withCallingHandlers(operation(),
                      interrupt = function(cnd) record("calling", cnd))
}
record("started")
switch(handler,
  exiting = tryCatch(operation(),
                     interrupt = function(cnd) record("exiting", cnd)),
  both = tryCatch(calling(), interrupt = function(cnd) record("exiting", cnd)),
  calling = calling(),
  error_only = tryCatch(operation(),
                        error = function(cnd) record("error handler", cnd)),
  none = operation()
)
record("returned")
