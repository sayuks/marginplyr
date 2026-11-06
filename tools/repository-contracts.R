# Compare independently maintained repository contracts without executing workflows.
# This entry performs static reads only; syntax and verifier wiring belong to CI.
validate_repository_contracts <- function(root = ".") {
  validate_workflow_contracts(root)
  invisible(TRUE)
}

# Preserve workflow inventory, scalar and sequence identity; normalize mapping order.
validate_workflow_contracts <- function(root = ".") {
  directory <- file.path(root, ".github/workflows")
  contract <- file.path(root, "tools/ci-workflow-contracts.json")
  if (!dir.exists(directory) || !file.exists(contract)) {
    stop("Workflow contract input missing: .github/workflows or tools/ci-workflow-contracts.json.", call. = FALSE)
  }
  paths <- sort(file.path(".github/workflows",
    list.files(directory, pattern = "[.]ya?ml$")))
  expected <- tryCatch(jsonlite::read_json(contract), error = function(cnd) {
    stop("Workflow contract JSON invalid: ", conditionMessage(cnd), call. = FALSE)
  })
  if (!is.list(expected) || !length(expected) || is.null(names(expected)) ||
      anyDuplicated(names(expected)) || !identical(paths, sort(names(expected)))) {
    stop("Workflow contract: workflow inventory differs from supported contracts. Expected: ",
      paste(names(expected), collapse = ", "), "; actual: ", paste(paths, collapse = ", "), call. = FALSE)
  }
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
  actual <- stats::setNames(lapply(paths, function(path) {
    tryCatch(yaml::yaml.load_file(file.path(root, path), eval.expr = FALSE,
      handlers = handlers), error = function(cnd) {
        stop("Workflow YAML invalid: ", path, ": ", conditionMessage(cnd), call. = FALSE)
      })
  }), paths)
  different <- paths[!vapply(paths, function(path) {
    identical(canonical(actual[[path]]), canonical(expected[[path]]))
  }, logical(1))]
  if (length(different)) {
    describe <- function(expected, actual, path) {
      if (identical(expected, actual)) return(NULL)
      if (is.list(expected) && is.list(actual) &&
          identical(names(expected), names(actual)) && length(expected) == length(actual)) {
        for (index in seq_along(expected)) {
          key <- if (is.null(names(expected))) paste0("[", index, "]") else paste0("$", names(expected)[[index]])
          found <- describe(expected[[index]], actual[[index]], paste0(path, key))
          if (!is.null(found)) return(found)
        }
      }
      display <- function(value) {
        text <- paste(capture.output(str(value, max.level = 1L, vec.len = 3L)), collapse = " ")
        substr(text, 1L, 300L)
      }
      paste0(path, ": expected ", display(expected), "; actual ", display(actual))
    }
    first <- different[[1L]]
    detail <- describe(canonical(expected[[first]]), canonical(actual[[first]]), first)
    stop(paste0("Workflow executable contract changed (conditions, environment, steps, ",
                "triggers, names and setup are protected): ", paste(different, collapse = ", "), "; ", detail,
                ". Update the contract and fixtures for an intentional workflow change."))
  }
  cat("Repository contract consistency passed: workflow executable contracts\n")
}
