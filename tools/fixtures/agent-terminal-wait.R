# Synthetic shell-tool fixture; it never invokes Review-ready or acquires a lock.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !args[[1L]] %in% c("success", "failure", "timeout", "cancel")) {
  stop("Supply success|failure|timeout|cancel and a delay in seconds.")
}
delay <- as.numeric(args[[2L]])
if (length(delay) != 1L || is.na(delay) || delay < 1 || delay > 30) {
  stop("Delay must be between 1 and 30 seconds.")
}

# Return a fixture status after interruptible work and observable cleanup.
run_fixture <- function(mode, delay) {
  on.exit({
    cat("fixture: cleanup started\n")
    flush.console()
    Sys.sleep(0.2)
    cat("fixture: cleanup completed\n")
    flush.console()
  })
  tryCatch({
    cat("fixture: admission waiting\n")
    flush.console()
    Sys.sleep(delay)
    cat("fixture: execution\nfixture: worker diagnostic preserved\n")
    flush.console()
    switch(mode,
      success = 0L,
      failure = {
        cat("fixture: controlled failure\n")
        1L
      },
      timeout = {
        cat("fixture: controlled deadline expired\n")
        124L
      },
      cancel = {
        cat("fixture: expected interrupt was not received\n")
        2L
      }
    )
  }, interrupt = function(condition) {
    cat("fixture: cancellation received\n")
    130L
  })
}

status <- run_fixture(args[[1L]], delay)
cat(sprintf("fixture: terminal status %d\n", status))
quit(save = "no", status = status)
