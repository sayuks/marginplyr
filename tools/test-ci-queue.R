# Exercise routine workflow lifetimes and the trusted close handler's API boundary.
expect <- function(ok, detail) if (!isTRUE(ok)) stop(detail, call. = FALSE)
workflows <- lapply(sort(list.files(".github/workflows", pattern = "[.]yaml$",
  full.names = TRUE)), function(path) yaml::read_yaml(path,
    handlers = list("bool#yes" = identity, "bool#no" = identity)))
names(workflows) <- sort(list.files(".github/workflows", pattern = "[.]yaml$"))
routine <- c("R-CMD-check.yaml", "altdoc.yaml", "document.yaml", "lint.yaml",
  "postgres-transactions.yaml", "release-matrix.yaml", "test-coverage.yaml")
for (path in routine) {
  workflow <- workflows[[path]]
  concurrency <- if (path == "altdoc.yaml") workflow$jobs$altdoc$concurrency else workflow$concurrency
  expect(!is.null(concurrency), paste(path, "needs automatic cancellation"))
  expect(grepl("manual-", concurrency$group, fixed = TRUE),
    paste(path, "must isolate manual runs"))
  expect(grepl("github.ref", concurrency$group, fixed = TRUE),
    paste(path, "must preserve existing automatic group identities"))
  expect(identical(concurrency$`cancel-in-progress`,
    "${{ github.event_name != 'workflow_dispatch' }}"), path)
}
cat("Routine concurrency and manual isolation contracts passed.\n")
hygiene <- workflows[["lint.yaml"]]$jobs$`repository-hygiene`
expect(!is.null(hygiene), "Base-R hygiene checks must share one job")
expect(identical(hygiene$`runs-on`, "ubuntu-latest"), "Hygiene uses a bare Ubuntu R")
commands <- c("verify-context-budget.R", "verify-doc-references.R", "verify-codex-config.R",
  "verify-agent-network-policy.R", "verify-cran-preflight.R", "verify-cran-release.R",
  "verify-review-ready-check.R")
checks <- Filter(function(step) !is.null(step$run), hygiene$steps)
expect(identical(vapply(checks, `[[`, character(1), "run"),
  paste("Rscript .github/scripts/", commands, sep = "")), "Preserve all seven hygiene checks")
for (step in checks) {
  expect(identical(step$`if`, "${{ !cancelled() && steps.setup-r.outcome == 'success' }}") &&
    is.null(step$`continue-on-error`) && nzchar(step$name),
    "Each check must run after failures, retain its name and fail the job")
}
expect(is.null(hygiene$needs) && !any(vapply(hygiene$steps, function(step)
  !is.null(step$uses) && grepl("setup-r-dependencies", step$uses, fixed = TRUE), logical(1))),
  "Hygiene is independent of package installation")
expect(identical(workflows[["lint.yaml"]]$jobs$lint$`runs-on`, "macos-latest"),
  "Package-aware lint stays separate on macOS")
expect(identical(workflows[["altdoc.yaml"]]$jobs$deploy$concurrency,
  list(group = "pages", `cancel-in-progress` = "false")), "Pages serialization stays independent")
expect(is.null(workflows[["altdoc.yaml"]]$concurrency), "Build cancellation must not cancel Pages deployment")
cat("Independent hygiene step outcomes and failure continuation contracts passed.\n")
policy <- jsonlite::read_json("tools/ci-close-event-policy.json", simplifyVector = TRUE)
expect(identical(policy$conditions$workflow_path$include, ".github/workflows/retire-pr-ci.yaml") &&
  length(policy$conditions$workflow_path$exclude) == 0L &&
  identical(policy$rules$type, "restrict_action_events") &&
  identical(policy$rules$parameters$allowed_events[[1L]], "pull_request_target"),
  "The event-policy allowance must target only the handler")
close <- workflows[["retire-pr-ci.yaml"]]
expect(!is.null(close), "A trusted PR-close handler must exist")
expect(identical(close$on$pull_request_target$types, "closed"), "Close events cover conflicting PRs")
expect(identical(close$permissions, list(actions = "write", `pull-requests` = "read")),
  "Only the handler receives cancellation permission")
expect(length(close$jobs$retire$steps) == 1L &&
  identical(close$jobs$retire$steps[[1L]]$uses, "actions/github-script@v8"),
  "The handler runs API-only default-branch code without checkout, artifacts or caches")
workspace <- tempfile("ci-queue-")
dir.create(workspace)
script <- file.path(workspace, "handler.js")
lifetimes <- file.path(workspace, "lifetimes.json")
writeLines(close$jobs$retire$steps[[1L]]$with$script, script)
jsonlite::write_json(lapply(workflows[routine], function(workflow)
  list(name = workflow$name, concurrency = if (workflow$name == "altdoc")
    workflow$jobs$altdoc$concurrency else workflow$concurrency)), lifetimes, auto_unbox = TRUE)
result <- processx::run("node", c("tools/fixtures/ci-queue-api.cjs", script, lifetimes),
  error_on_status = FALSE, timeout = 60000, cleanup_tree = TRUE)
cat(result$stdout, result$stderr, sep = "")
unlink(workspace, recursive = TRUE)
expect(result$status == 0L, "PR-close API fixtures failed")
