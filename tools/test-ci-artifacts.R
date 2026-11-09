# Exercise CI artifact verification through its command boundary.
source_root <- normalizePath(".")
rscript <- file.path(R.home("bin"), "Rscript")
expect <- function(ok, detail) if (!isTRUE(ok)) stop(detail, call. = FALSE)
run <- function(command, args, wd, env = NULL) {
  processx::run(command, args, wd = wd, env = env, error_on_status = FALSE,
               timeout = 60000, cleanup_tree = TRUE)
}
main <- function() {
  workspace <- tempfile("ci-artifacts-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  repo <- file.path(workspace, "source")
  git <- function(args, wd = repo) {
    result <- run("git", args, wd)
    expect(result$status == 0L, paste(result$stdout, result$stderr))
    result$stdout
  }
  git(c("clone", "--quiet", "--shared", source_root, repo), source_root)
  git(c("config", "user.name", "Artifact fixture"))
  git(c("config", "user.email", "artifact@example.com"))
  files <- c("tools/verify-ci-artifacts.R", "tools/ci-workflow-contracts.json", "tools/repository-contracts.R",
             "tools/dependency-requirements.R", "tools/fixtures/workflow-contract-cases.R",
             "tools/fixtures/review-dependency-contract-cases.R", ".github/scripts/verify-cran-preflight.R",
             ".github/scripts/verify-doc-references.R", ".github/scripts/summarize-check-log.R",
             ".github/scripts/provision-duckdb-json.R",
             list.files(file.path(source_root, ".github/workflows"),
                        pattern = "\\.ya?ml$", full.names = FALSE))
  files[!grepl("/", files, fixed = TRUE)] <- file.path(".github/workflows", files[!grepl("/", files, fixed = TRUE)])
  for (path in files) expect(file.copy(file.path(source_root, path), file.path(repo, path), overwrite = TRUE), path)
  commit <- function() {
    git(c("add", "-A"))
    git(c("commit", "--allow-empty", "-qm", "artifact fixture"))
  }
  check <- function(status = 0L, fragment = "workflow executable contracts", env = NULL) {
    commit()
    result <- run(rscript, "tools/verify-ci-artifacts.R", repo, env)
    output <- paste(result$stdout, result$stderr)
    expect(result$status == status && grepl(fragment, output, fixed = TRUE), output)
  }
  check()
  source("tools/fixtures/workflow-contract-cases.R", local = TRUE)
  initial <- trimws(git(c("rev-parse", "HEAD")))
  source("tools/fixtures/review-dependency-contract-cases.R", local = TRUE)
  for (name in names(review_dependency_contract_cases())) {
    cat("Preflight dependency fixture: ", name, "\n", sep = "")
    case <- review_dependency_contract_cases()[[name]]
    case$edit(repo)
    commit()
    sha <- trimws(git(c("rev-parse", "HEAD")))
    result <- run(rscript, ".github/scripts/verify-cran-preflight.R", repo)
    output <- paste(result$stdout, result$stderr)
    expect(result$status == if (is.null(case$diagnostic)) 0L else 1L, paste(name, output))
    expect(grepl(if (is.null(case$diagnostic)) "review dependency contract" else case$diagnostic,
      output, fixed = TRUE), paste(name, output))
    expect(identical(trimws(git(c("rev-parse", "HEAD"))), sha) &&
      !nzchar(trimws(git(c("status", "--porcelain=v1", "--untracked-files=all")))),
      "CI dependency validation never repairs committed inputs")
    git(c("reset", "--hard", initial))
    git(c("clean", "-fd"))
  }
  if ("--dependency-contracts-only" %in% commandArgs(trailingOnly = TRUE)) {
    cat("Preflight dependency public-command fixtures passed.\n")
    return(invisible(NULL))
  }
  for (name in names(workflow_contract_cases())) {
    case <- workflow_contract_cases()[[name]]
    case$edit(repo)
    check(if (is.null(case$diagnostic)) 0L else 1L,
      if (is.null(case$diagnostic)) "workflow executable contracts" else case$diagnostic)
    expect(!nzchar(trimws(git(c("status", "--porcelain=v1", "--untracked-files=all")))),
      "CI validation never repairs committed inputs")
    git(c("reset", "--hard", initial))
    git(c("clean", "-fd"))
  }
  workflow <- file.path(repo, ".github/workflows/lint.yaml")
  read <- function(path) paste0(paste(readLines(path, warn = FALSE), collapse = "\n"), "\n")
  write <- function(text, path) cat(text, file = path)
  original <- read(workflow)
  write(paste0(original, "\n# Artifact verification fixture\n"), workflow)
  check()
  write(original, workflow)
  # Mapping order has no contract meaning; scalar spelling and sequences do.
  contract_path <- file.path(repo, "tools/ci-workflow-contracts.json")
  contract <- jsonlite::read_json(contract_path)
  reordered <- contract
  reordered[[".github/workflows/lint.yaml"]] <- rev(reordered[[".github/workflows/lint.yaml"]])
  jsonlite::write_json(reordered, contract_path, auto_unbox = TRUE, null = "null")
  check()
  jsonlite::write_json(contract, contract_path, auto_unbox = TRUE, null = "null")
  changes <- list(
    c("name: lint.yaml", "name: R-CMD-check.yaml"),
    c("run: Rscript .github/scripts/verify-context-budget.R", "run: echo skipped"),
    c("run: Rscript tools/verify-test-priority.R", "run: echo skipped"),
    c("  repository-hygiene:\n", "  repository-hygiene:\n    if: github.event_name == 'push'\n"),
    c("Rscript tools/test-review-ready-progress.R", "echo skipped"),
    c("Rscript tools/test-review-ready-priority.R", "echo skipped"),
    c("Rscript tools/test-review-ready-runner.R", "echo skipped"),
    c("any::roxygen2, ", ""),
    c("Rscript tools/verify-ci-artifacts.R", "echo skipped"),
    c("Rscript tools/test-ci-artifacts.R", "echo skipped"),
    c("Rscript tools/test-interruption-supervisors.R", "echo skipped"),
    c("      - uses: r-lib/actions/setup-tinytex@v2\n        env:\n          TINYTEX_INSTALLER: TinyTeX\n\n", ""),
    c("TINYTEX_INSTALLER: TinyTeX", "TINYTEX_INSTALLER: TinyTeX-0"),
    c("runs-on: macos-latest", "runs-on: ubuntu-latest"),
    c('c("knitr", "xfun")', 'c("knitr")'),
    c("LINTR_ERROR_ON_LINT: true", "LINTR_ERROR_ON_LINT: false"),
    c("LINTR_ERROR_ON_LINT: true", "LINTR_ERROR_ON_LINT: yes"),
    c("branches: [main, master]", "branches: [main]"),
    c("branches: [main, master]", "branches: main")
  )
  for (change in changes) {
    expect(grepl(change[[1L]], original, fixed = TRUE), paste("Missing mutation target", change[[1L]]))
    write(sub(change[[1L]], change[[2L]], original, fixed = TRUE), workflow)
    check(1L, "Workflow executable contract changed")
  }
  write(paste0(original, "\ninvalid: [\n"), workflow)
  check(1L, "Parser error")
  write(original, workflow)
  unknown <- file.path(dirname(workflow), "unknown.yaml")
  write("# no verified contract\n", unknown)
  check(1L, "workflow inventory differs")
  unlink(unknown)
  unlink(workflow)
  check(1L, "workflow inventory differs")
  write(original, workflow)
  postgres <- file.path(repo, ".github/workflows/postgres-transactions.yaml")
  postgres_original <- read(postgres)
  alias_step <- paste0(
    "      - name: Verify share aliases survive identifier limits\n",
    "        run: Rscript tools/postgres-share-identifiers.R\n"
  )
  expect(grepl(alias_step, postgres_original, fixed = TRUE), "Missing alias step")
  write(sub(alias_step, "", postgres_original, fixed = TRUE), postgres)
  check(1L, "Workflow executable contract changed")
  write(postgres_original, postgres)
  deadlines <- list(c("R-CMD-check.yaml", "R-CMD-check", "120", "119"),
                    c("release-matrix.yaml", "structure", "360", "359"),
                    c("release-matrix.yaml", "depends-only", "90", "89"),
                    c("release-matrix.yaml", "tarball", "90", "89"),
                    c("release-matrix.yaml", "backend", "120", "119"),
                    c("test-coverage.yaml", "test-coverage", "360", "359"))
  for (deadline in deadlines) {
    path <- file.path(dirname(workflow), deadline[[1L]])
    contents <- readLines(path)
    job <- match(paste0("  ", deadline[[2L]], ":"), contents)
    jobs <- c(grep("^  [A-Za-z].*:$", contents), length(contents) + 1L)
    limit <- which(contents == paste0("    timeout-minutes: ", deadline[[3L]]))
    limit <- limit[limit > job & limit < min(jobs[jobs > job])]
    expect(length(limit) == 1L, paste("Missing unique deadline for", deadline[[2L]]))
    changed <- contents
    changed[[limit]] <- paste0("    timeout-minutes: ", deadline[[4L]])
    writeLines(changed, path)
    check(1L, "Workflow executable contract changed")
    writeLines(contents, path)
  }
  settings <- file.path(repo, ".claude/settings.json")
  previous_settings <- if (file.exists(settings)) read(settings) else NULL
  dir.create(dirname(settings), showWarnings = FALSE)
  write("{invalid json", settings)
  check(1L, "invalid")
  if (is.null(previous_settings)) unlink(settings) else write(previous_settings, settings)
  write("{invalid json", contract_path)
  check(1L, "invalid")
  removed <- contract
  removed[[".github/workflows/lint.yaml"]] <- NULL
  jsonlite::write_json(removed, contract_path, auto_unbox = TRUE, null = "null")
  check(1L, "workflow inventory differs")
  jsonlite::write_json(contract, contract_path, auto_unbox = TRUE, null = "null")
  bin <- file.path(workspace, "bin")
  dir.create(bin)
  for (name in c("git", "Rscript")) expect(file.symlink(Sys.which(name), file.path(bin, name)), name)
  env <- Sys.getenv()
  env[["PATH"]] <- bin
  check(1L, "install actionlint; package checking is no fallback", env)
  broken <- file.path(repo, ".github/scripts/verify-fixture-uninvoked.R")
  write("stop('not invoked')\n", broken)
  check(1L, "verify-fixture-uninvoked.R")
  unlink(broken)
  reference <- file.path(repo, "design/artifact-reference-probe.md")
  for (valid in c(TRUE, FALSE)) {
    write(if (valid) "See `design/architecture.md`.\n" else "See `R/not-a-real-file.R`.\n", reference)
    commit()
    result <- run(rscript, ".github/scripts/verify-doc-references.R", repo)
    expect(result$status == if (valid) 0L else 1L, result$stderr)
    expect(grepl(if (valid) "Verified:" else "not-a-real-file", paste(result$stdout, result$stderr), fixed = TRUE), result$stderr)
  }
  unlink(reference)
  check_dir <- file.path(workspace, "check/fixture.Rcheck")
  dir.create(check_dir, recursive = TRUE)
  summary <- file.path(workspace, "summary.md")
  env <- Sys.getenv()
  env[c("MARGINPLYR_CHECK_DIR", "MARGINPLYR_CHECK_LABEL", "GITHUB_STEP_SUMMARY")] <-
    c(dirname(check_dir), "Fixture check", summary)
  log <- file.path(check_dir, "00check.log")
  write(paste0("* checking examples ... ERROR\nexample detail\n",
               "* checking tests ... WARNING\nwarning detail\n",
               "* checking metadata ... NOTE\nnote detail\n",
               "* checking completed ... OK\nStatus: 1 ERROR, 1 WARNING, 1 NOTE\n"), log)
  summarize <- function() {
    result <- run(rscript, ".github/scripts/summarize-check-log.R", repo, env)
    expect(result$status == 0L, result$stderr)
    result
  }
  summarize()
  actual <- read(summary)
  for (part in c("## Fixture check", "Status: 1 ERROR, 1 WARNING, 1 NOTE",
                 "example detail", "warning detail", "note detail")) expect(grepl(part, actual, fixed = TRUE), actual)
  expect(!grepl("checking completed", actual, fixed = TRUE), actual)
  unlink(summary)
  write("* checking tests ... OK\n", log)
  summarize()
  expect(grepl("No status line in the check log.", read(summary), fixed = TRUE), read(summary))
  unlink(c(log, summary))
  result <- summarize()
  expect(grepl("nothing to summarize", result$stderr, fixed = TRUE) && !file.exists(summary), result$stderr)
  unlink(file.path(repo, "tools/review-ready-admission.py"))
  unlink(file.path(repo, ".git"), recursive = TRUE)
  result <- run(rscript, "tools/verify-ci-artifacts.R", repo)
  expect(result$status == 0L && grepl("workflow executable contracts", result$stdout, fixed = TRUE), paste(result$stdout, result$stderr))
  cat("CI artifact command fixtures passed.\n")
}
main()
