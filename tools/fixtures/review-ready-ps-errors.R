# R_PROFILE_USER fixture replaces only ps's public process-inspection boundary.
local({
  handle <- ps::ps_handle
  children <- ps::ps_children
  vanished <- new.env(parent = emptyenv())
  replace <- function(name, value) {
    namespace <- asNamespace("ps")
    unlockBinding(name, namespace)
    assign(name, value, namespace)
    lockBinding(name, namespace)
  }
  replace("ps_handle", function(pid = NULL, time = NULL) {
    if (!is.null(pid) && exists(as.character(pid), vanished, inherits = FALSE)) {
      stop("No such file or directory", call. = FALSE)
    }
    handle(pid, time)
  })
  replace("ps_children", function(p = handle(), recursive = FALSE) {
    command <- tryCatch(ps::ps_cmdline(p), error = function(cnd) character())
    if (Sys.getenv("FIXTURE_INSPECTION_MODE") == "exited" && any(basename(command) == "git")) {
      stopifnot(all(ps::ps_wait(list(p), timeout = 2000L)))
      vanished[[as.character(ps::ps_pid(p))]] <- TRUE
      writeLines("race injected", Sys.getenv("FIXTURE_RACE_INJECTED"))
      stop("No such file or directory", call. = FALSE)
    }
    if (Sys.getenv("FIXTURE_INSPECTION_MODE") == "live" &&
        file.exists(Sys.getenv("FIXTURE_INSPECTION_GATE")) &&
        file.exists(Sys.getenv("FIXTURE_READY")) &&
        any(grepl("worker[.]R", command))) {
      stopifnot(ps::ps_is_running(p))
      writeLines(as.character(ps::ps_pid(p)), Sys.getenv("FIXTURE_INSPECTION_BLOCKED"))
      stop("No such file or directory", call. = FALSE)
    }
    children(p, recursive)
  })
})
