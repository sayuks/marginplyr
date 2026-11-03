# Verify current workflow syntax, executable contracts and verifier wiring.
# No comparison base, installed launcher or package-check history is required.
verify <- function() {
  if (file.exists(".claude/settings.json")) jsonlite::read_json(".claude/settings.json")
  paths <- sort(list.files(".github/workflows", pattern = "\\.ya?ml$", full.names = TRUE))
  expected <- jsonlite::read_json("tools/ci-workflow-contracts.json")
  if (!identical(paths, sort(names(expected)))) {
    stop("CI validation unresolved: workflow inventory differs from supported contracts.")
  }
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
  # YAML 1.1 otherwise collapses yes and true. Sequences remain lists even
  # when they have one scalar; only mappings are compared independent of order.
  tags <- c("bool#yes", "bool#no", "int", "int#hex", "int#oct", "int#base60",
            "float#fix", "float#exp", "float#base60", "float#inf", "float#neginf", "float#nan", "seq")
  handlers <- stats::setNames(rep(list(identity), length(tags)), tags)
  canonical <- function(x) {
    if (!is.list(x)) return(x)
    if (!is.null(names(x))) x <- x[order(names(x))]
    lapply(x, canonical)
  }
  actual <- stats::setNames(lapply(paths, yaml::yaml.load_file,
                                 eval.expr = FALSE, handlers = handlers), paths)
  different <- paths[!vapply(paths, function(path) {
    identical(canonical(actual[[path]]), canonical(expected[[path]]))
  }, logical(1))]
  if (length(different)) {
    stop(paste0("Workflow executable contract changed (conditions, environment, steps, ",
                "triggers, names and setup are protected): ", paste(different, collapse = ", "),
                ". Update the contract and fixtures for an intentional workflow change."))
  }
  cat("Completed CI artifact verification: workflow executable contracts\n")
  run(file.path(R.home("bin"), "Rscript"), ".github/scripts/verify-verifier-invocation.R")
  cat("Completed CI artifact verification: verifier-invocation wiring\n")
}
tryCatch(verify(), error = function(cnd) {
  message(conditionMessage(cnd))
  quit(status = 1L)
})
