# Documentation fixtures exercise real testthat and R CMD check boundaries.
source("tools/review-ready-check-lib.R")
expect_identical <- testthat::expect_identical
expect_true <- testthat::expect_true
expect_error <- function(expression, pattern, label) {
  testthat::expect_error(expression, regexp = pattern, fixed = TRUE, info = label)
}
source("tools/review-ready-documentation.R")

local({
  root <- tempfile("documentation-assertions-")
  dir.create(file.path(root, "tests", "testthat"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines(c("Package: fixture", "Version: 0.0.1", "Title: Fixture",
    "Description: Documentation assertion fixture.", "License: GPL-3",
    "Config/testthat/edition: 3"), file.path(root, "DESCRIPTION"))
  writeLines(character(), file.path(root, "NAMESPACE"))
  tests <- file.path(root, "tests", "testthat")
  writeLines("fixture_helper <- TRUE", file.path(tests, "helper-fixture.R"))
  writeLines("options(fixture_setup = TRUE)", file.path(tests, "setup-fixture.R"))
  writeLines("options(fixture_teardown = TRUE)", file.path(tests, "teardown-fixture.R"))
  targets <- review_ready_documentation_tests()
  for (target in targets) {
    writeLines("test_that('lifecycle', { testthat::expect_true(fixture_helper); testthat::expect_true(getOption('fixture_setup')) })",
      file.path(tests, target))
  }
  writeLines("stop('Unselected suite sentinel')", file.path(tests, "test-unselected.R"))
  capture.output(run_review_ready_documentation_tests(root))
  expect_true(getOption("fixture_teardown"), "teardown ran after selected assertions")
  writeLines("test_that('skipped relevant assertion', { skip('fixture skip') })",
    file.path(tests, targets[[1L]]))
  capture.output(expect_error(run_review_ready_documentation_tests(root),
    "empty or skipped", "skipped documentation assertion"))
  writeLines("# empty", file.path(tests, targets[[1L]]))
  capture.output(expect_error(run_review_ready_documentation_tests(root),
    "empty or skipped", "empty documentation file"))
  unlink(file.path(tests, targets[[1L]]))
  expect_error(run_review_ready_documentation_tests(root), "match every registered file",
    "a missing selected file never expands to the suite")
})

local({
  workspace <- tempfile("documentation-check-fixtures-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  repositories <- review_ready_offline_repositories(workspace)
  for (case in c("valid", "malformed-help", "codoc", "example")) {
    directory <- file.path(workspace, case)
    root <- file.path(directory, "fixture")
    dir.create(file.path(root, "R"), recursive = TRUE)
    dir.create(file.path(root, "man"))
    dir.create(file.path(root, "tests"))
    writeLines(c("Package: fixture", "Version: 0.0.1", "Title: Check Documentation Fixtures",
      "Description: Exercise retained documentation checks without running tests.",
      "Authors@R: person('Test', 'Author', email = 'test@example.com', role = c('aut', 'cre'))",
      "License: GPL-3", "Encoding: UTF-8"), file.path(root, "DESCRIPTION"))
    writeLines("export(fixture)", file.path(root, "NAMESPACE"))
    writeLines("fixture <- function(x) x", file.path(root, "R", "fixture.R"))
    writeLines("stop('Full suite must be omitted')", file.path(root, "tests", "fail.R"))
    rd <- c("\\name{fixture}", "\\alias{fixture}", "\\title{Fixture}",
      "\\description{Documentation fixture.}", "\\usage{fixture(x)}",
      "\\arguments{\\item{x}{Input.}}", "\\value{The input.}",
      "\\examples{fixture(1)}")
    if (case == "malformed-help") rd <- c(rd, "\\notAnRdMacro{broken}")
    if (case == "codoc") rd <- sub("fixture(x)", "fixture(x, y)", rd, fixed = TRUE)
    if (case == "example") rd <- sub("fixture(1)", "stop('Example sentinel')", rd, fixed = TRUE)
    writeLines(rd, file.path(root, "man", "fixture.Rd"))
    built <- review_ready_run_system(file.path(R.home("bin"), "R"),
      c("CMD", "build", root), directory = directory, capture = TRUE)
    expect_identical(built$status, 0L, paste(case, "fixture builds"))
    tarball <- list.files(directory, "[.]tar[.]gz$", full.names = TRUE)
    result <- rcmdcheck::rcmdcheck(tarball, args = c("--as-cran", "--no-tests"),
      build_args = NULL, check_dir = file.path(directory, "check"),
      error_on = "never", quiet = TRUE, repos = repositories,
      env = review_ready_rcmdcheck_env(character(), "Documentation"))
    findings <- paste(c(result$errors, result$warnings), collapse = "\n")
    if (case == "valid") {
      expect_identical(findings, "", "valid documentation passes with a failing unexecuted suite")
    } else {
      marker <- switch(case, `malformed-help` = "Rd", codoc = "mismatch", example = "Example sentinel")
      expect_true(grepl(marker, findings, ignore.case = TRUE),
        paste(case, "is still rejected by R CMD check --no-tests"))
    }
  }
  cat("Documentation R CMD check fixtures passed (full suite omitted).\n")
})

source("tools/review-ready-generated.R")
local({
  root <- tempfile("documentation-generation-fixture-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  git <- function(args) {
    result <- review_ready_run_system("git", c("-C", root, args),
      directory = root, capture = TRUE)
    expect_identical(result$status, 0L, paste("fixture git", args[[1L]]))
  }
  writeLines(c("Package: fixture", "Version: 0.0.1", "Title: Generate Documentation Fixtures",
    "Description: Exercise generated documentation comparison.",
    "Authors@R: person('Test', 'Author', email = 'test@example.com', role = c('aut', 'cre'))",
    "License: GPL-3", "Encoding: UTF-8"), file.path(root, "DESCRIPTION"))
  writeLines(c("#' Fixture", "#' @param x Input.", "#' @return The input.",
    "#' @export", "fixture <- function(x) x"), file.path(root, "R", "fixture.R"))
  writeLines(c("---", "format: gfm", "---", "", "# Fixture", "", "A fixture."),
    file.path(root, "README.qmd"))
  generated <- review_ready_run_system(file.path(R.home("bin"), "Rscript"),
    c("-e", "roxygen2::roxygenise(); devtools::build_readme()"),
    directory = root, capture = TRUE)
  expect_identical(generated$status, 0L, paste("initial fixture generation",
    paste(generated$output, collapse = "\n")))
  git(c("init", "-q"))
  git(c("config", "user.name", "Fixture"))
  git(c("config", "user.email", "fixture@example.com"))
  git(c("add", "."))
  git(c("commit", "-qm", "generated fixture"))
  capture.output(run_review_ready_generated(root))
  for (artifact in c("README.md", "man/fixture.Rd", "man/figures/README-obsolete.png", "NAMESPACE")) {
    git(c("reset", "--hard", "HEAD"))
    path <- file.path(root, artifact)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    cat(if (artifact == "NAMESPACE") "\n# stale\n" else "stale", file = path, append = TRUE)
    git(c("add", "."))
    git(c("commit", "-qm", "stale artifact"))
    capture.output(expect_error(run_review_ready_generated(root),
      artifact, paste("stale generated artifact", artifact)))
    git(c("reset", "--hard", "HEAD~1"))
  }
  source_path <- file.path(root, "R", "fixture.R")
  lines <- readLines(source_path)
  writeLines(c("#' @evalRd { writeLines('# generated runtime', 'R/generated.R'); 'Text.' }", lines),
    source_path)
  git(c("add", "."))
  git(c("commit", "-qm", "unexpected runtime output"))
  output <- capture.output(expect_error(run_review_ready_generated(root),
    "Stale generated documentation", "unexpected output is never hidden by Full fallback"))
  expect_true(any(grepl("Non-documentation generation effects require Full: R/generated.R", output,
    fixed = TRUE)), "unexpected generated runtime output is classified")
  cat("Generated README, help and obsolete-figure fixtures passed.\n")
})
