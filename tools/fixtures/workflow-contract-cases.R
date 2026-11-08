# Shared mutation inputs for the existing local and CI command seams.
workflow_contract_cases <- function() {
  edit_workflow <- function(old, new, workflow = "lint.yaml") function(root) {
    path <- file.path(root, ".github/workflows", workflow)
    text <- paste0(paste(readLines(path), collapse = "\n"), "\n")
    stopifnot(grepl(old, text, fixed = TRUE))
    cat(sub(old, new, text, fixed = TRUE), file = path)
  }
  mismatch <- "Workflow executable contract changed"
  step <- paste0("      - name: Verify committed test priority registration\n",
    "        run: Rscript tools/verify-test-priority.R\n")
  list(
    matching = list(edit = function(root) NULL, diagnostic = NULL),
    comment = list(edit = function(root) cat("\n# fixture comment\n",
      file = file.path(root, ".github/workflows/lint.yaml"), append = TRUE), diagnostic = NULL),
    mapping_order = list(edit = function(root) {
      path <- file.path(root, "tools/ci-workflow-contracts.json")
      x <- jsonlite::read_json(path)
      x[[".github/workflows/lint.yaml"]] <- rev(x[[".github/workflows/lint.yaml"]])
      jsonlite::write_json(x, path, auto_unbox = TRUE, null = "null")
    }, diagnostic = NULL),
    historical_missing_priority_contract = list(edit = function(root) {
      path <- file.path(root, "tools/ci-workflow-contracts.json")
      x <- jsonlite::read_json(path)
      for (job in names(x[[".github/workflows/lint.yaml"]]$jobs)) {
        steps <- x[[".github/workflows/lint.yaml"]]$jobs[[job]]$steps
        keep <- !vapply(steps, function(step) identical(step$run,
          "Rscript tools/verify-test-priority.R"), logical(1))
        x[[".github/workflows/lint.yaml"]]$jobs[[job]]$steps <- steps[keep]
      }
      jsonlite::write_json(x, path, auto_unbox = TRUE, null = "null")
    }, diagnostic = mismatch),
    added_workflow = list(edit = function(root) writeLines("name: extra",
      file.path(root, ".github/workflows/extra.yaml")), diagnostic = "workflow inventory differs"),
    removed_workflow = list(edit = function(root) unlink(file.path(root,
      ".github/workflows/lint.yaml")), diagnostic = "workflow inventory differs"),
    command = list(edit = edit_workflow("run: Rscript tools/verify-test-priority.R",
      "run: echo skipped"), diagnostic = mismatch),
    json_check_setup = list(edit = edit_workflow(
      "run: Rscript .github/scripts/provision-duckdb-json.R", "run: echo skipped",
      "R-CMD-check.yaml"), diagnostic = mismatch),
    json_structure_setup = list(edit = edit_workflow(
      "run: Rscript .github/scripts/provision-duckdb-json.R", "run: echo skipped",
      "release-matrix.yaml"), diagnostic = mismatch),
    json_setup_fixtures = list(edit = edit_workflow(
      "run: Rscript tools/test-duckdb-json-provision.R", "run: echo skipped",
      "release-matrix.yaml"), diagnostic = mismatch),
    json_backend_isolation = list(edit = edit_workflow(
      "if: matrix.backend.name == 'duckdb'", "if: true",
      "release-matrix.yaml"), diagnostic = mismatch),
    json_coverage_setup = list(edit = edit_workflow(
      "run: Rscript .github/scripts/provision-duckdb-json.R", "run: echo skipped",
      "test-coverage.yaml"), diagnostic = mismatch),
    setup_go_version = list(edit = edit_workflow("actions/setup-go@v7",
      "actions/setup-go@v6"), diagnostic = mismatch),
    removed_step = list(edit = edit_workflow(step, ""), diagnostic = mismatch),
    disabled_step = list(edit = edit_workflow(step,
      sub("        run:", "        if: false\n        run:", step, fixed = TRUE)), diagnostic = mismatch),
    scalar = list(edit = edit_workflow("LINTR_ERROR_ON_LINT: true",
      "LINTR_ERROR_ON_LINT: yes"), diagnostic = mismatch),
    sequence_scalar = list(edit = edit_workflow("branches: [main, master]",
      "branches: main"), diagnostic = mismatch),
    sequence_order = list(edit = edit_workflow("branches: [main, master]",
      "branches: [master, main]"), diagnostic = mismatch),
    missing_workflows = list(edit = function(root) unlink(file.path(root,
      ".github/workflows"), recursive = TRUE), diagnostic = "Workflow contract input missing"),
    missing_contract = list(edit = function(root) unlink(file.path(root,
      "tools/ci-workflow-contracts.json")), diagnostic = "Workflow contract input missing"),
    malformed_contract = list(edit = function(root) writeLines("{invalid json",
      file.path(root, "tools/ci-workflow-contracts.json")), diagnostic = "Workflow contract JSON invalid"),
    malformed_inventory = list(edit = function(root) writeLines("[]",
      file.path(root, "tools/ci-workflow-contracts.json")), diagnostic = "workflow inventory differs"),
    malformed_workflow = list(edit = function(root) cat("\ninvalid: [\n",
      file = file.path(root, ".github/workflows/lint.yaml"), append = TRUE), diagnostic = "Workflow YAML invalid")
  )
}
