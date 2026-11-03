# Full Review-ready supplies the common recorder from its pinned source archive.
local({
  recorder <- Sys.getenv("MARGINPLYR_REVIEW_TIMING_HELPER")
  if (nzchar(recorder)) {
    tryCatch(
      withCallingHandlers(
        source(recorder, local = TRUE)$value(),
        warning = function(cnd) stop(conditionMessage(cnd))
      ),
      error = function(cnd) {
        cat("Review-ready timing disabled: ", conditionMessage(cnd), "\n",
            sep = "")
      }
    )
  }
})
