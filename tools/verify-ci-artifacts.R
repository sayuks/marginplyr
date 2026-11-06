# Verify current workflow syntax, executable contracts and verifier wiring.
# No comparison base, installed launcher or package-check history is required.
verify <- function() {
  if (file.exists(".claude/settings.json")) jsonlite::read_json(".claude/settings.json")
  source("tools/repository-contracts.R", local = TRUE)
  validate_repository_contracts()
  paths <- sort(list.files(".github/workflows", pattern = "[.]ya?ml$", full.names = TRUE))
  cat("Completed CI artifact verification: workflow executable contracts\n")
  actionlint <- Sys.which("actionlint")
  if (!nzchar(actionlint)) {
    stop("CI validation unresolved: install actionlint; package checking is no fallback.")
  }
  run <- function(command, args) {
    result <- processx::run(command, args, error_on_status = FALSE, timeout = 60000)
    cat(result$stdout, result$stderr, sep = "")
    if (result$status != 0L) stop(sprintf("Command failed with exit status %d", result$status))
  }
  run(actionlint, c("-shellcheck=", "-pyflakes=", paths))
  cat("Completed CI artifact verification: actionlint workflow syntax\n")
  run(file.path(R.home("bin"), "Rscript"), ".github/scripts/verify-verifier-invocation.R")
  cat("Completed CI artifact verification: verifier-invocation wiring\n")
}
tryCatch(verify(), error = function(cnd) {
  message(conditionMessage(cnd))
  quit(status = 1L)
})
