# Temporary CI condition probe, confined to the disposable full-command fixture.
local({
  report <- function(cnd, site, parent = NULL) {
    emit <- function(...) cat("[DEBUG-1121-monitor] ", ..., "\n", sep = "", file = stderr())
    emit("site=", site, "; class=", paste(class(cnd), collapse = ","))
    emit("call=", paste(deparse(conditionCall(cnd)), collapse = " "))
    for (field in intersect(c("errno", "pid"), names(cnd))) {
      value <- cnd[[field]]
      if (is.atomic(value) && length(value) == 1L) emit(field, "=", value)
    }
    if (!is.null(parent)) {
      alive <- tryCatch(ps::ps_is_running(parent), error = function(cnd) NA)
      emit("owned_root_pid=", ps::ps_pid(parent), "; running=", alive)
    }
    frames <- sys.frames()
    for (index in seq_along(frames)) {
      for (field in c("pid", "child_pid", "mypid")) {
        if (!exists(field, frames[[index]], inherits = FALSE)) next
        value <- get(field, frames[[index]], inherits = FALSE)
        if (is.numeric(value) && length(value) == 1L) emit("frame=", index, "; ", field, "=", value)
      }
    }
    calls <- sys.calls()
    for (index in seq_along(calls)) {
      call <- calls[[index]]
      if (!is.call(call) || is.function(call[[1L]])) next
      text <- paste(deparse(call), collapse = " ")
      if (grepl(paste("ps_handle|ps_children|ps_create_time|ps_ppid|review_ready|process_factory",
          "parse_results|read_output|read_error|poll_io", sep = "|"), text)) {
        emit("stack[", index, "]=", text)
      }
    }
  }
  children <- ps::ps_children
  namespace <- asNamespace("ps")
  unlockBinding("ps_children", namespace)
  assign("ps_children", function(p = ps::ps_handle(), recursive = FALSE) {
    withCallingHandlers(children(p, recursive), error = function(cnd) report(cnd, "ps_children", p))
  }, namespace)
  lockBinding("ps_children", namespace)
  globalCallingHandlers(error = function(cnd) report(cnd, "unhandled"))
})
