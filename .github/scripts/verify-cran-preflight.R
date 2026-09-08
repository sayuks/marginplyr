# Exercises the release-policy helpers with fixtures that need neither a
# package installation nor a network service. The local preflight and release
# matrix both source the same policy, so this verifier is where changes to its
# deliberately narrow NOTE allowance fail before either expensive gate runs.

source(".github/scripts/cran-note-policy.R")
source("tools/cran-preflight-lib.R")

expect_identical <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    stop(
      sprintf(
        "%s: expected %s, got %s.",
        label,
        paste(deparse(expected), collapse = " "),
        paste(deparse(actual), collapse = " ")
      ),
      call. = FALSE
    )
  }
}

expect_true <- function(actual, label) {
  expect_identical(isTRUE(actual), TRUE, label)
}

new_submission <- paste(
  "* checking CRAN incoming feasibility ... NOTE",
  "Maintainer: 'Yusuke Sasaki <sayuks.dev@gmail.com>'",
  "",
  "New submission",
  sep = "\n"
)

allowed <- classify_cran_note(new_submission, "unpublished")
expect_identical(
  allowed$status,
  "allowed-note",
  "the initial-submission NOTE"
)
expect_identical(
  allowed$marker,
  "New submission",
  "the required cran-comments marker"
)

near_miss <- paste(new_submission, "Possibly misspelled words", sep = "\n")
expect_identical(
  classify_cran_note(near_miss, "unpublished")$status,
  "unknown-note",
  "an incoming NOTE with another finding"
)
expect_identical(
  classify_cran_note(new_submission, "published")$status,
  "unknown-note",
  "the initial-submission NOTE in the wrong state"
)
expect_identical(
  classify_cran_note("Days since last update: 1", "unpublished")$status,
  "unknown-note",
  "the former resubmission allowance"
)
unknown <- classify_cran_note("An unknown NOTE", "unpublished")
expect_identical(
  cran_notes_block(list(unknown), strict = FALSE),
  FALSE,
  "an unknown NOTE in routine CI"
)
expect_identical(
  cran_notes_block(list(unknown), strict = TRUE),
  TRUE,
  "an unknown NOTE in a manual release run"
)
expect_identical(
  cran_notes_block(list(allowed), strict = TRUE),
  FALSE,
  "an allowed NOTE in a manual release run"
)

description <- read_description("DESCRIPTION")
preflight_requirements <- dependency_requirements(description_value(
  description,
  "Config/Needs/preflight"
))
expect_identical(
  preflight_requirements$package,
  c("checktor", "rcmdcheck", "urlchecker"),
  "the preflight-only dependency set"
)
checktor_requirement <- preflight_requirements[
  preflight_requirements$package == "checktor",
  ,
  drop = FALSE
]
expect_identical(
  checktor_requirement$operator,
  ">=",
  "the checktor baseline operator"
)
expect_identical(
  checktor_requirement$version,
  "0.1.0",
  "the checktor baseline version"
)
package_requirements <- rbind(
  dependency_requirements(description_value(description, "Imports")),
  dependency_requirements(description_value(description, "Suggests"))
)
expect_identical(
  intersect(preflight_requirements$package, package_requirements$package),
  character(),
  "preflight-only packages outside Imports and Suggests"
)

build_ignore <- readLines(".Rbuildignore", warn = FALSE)
expect_true("^tools$" %in% build_ignore, "the tools tarball exclusion")

preflight_sources <- paste(
  unlist(lapply(
    c("tools/cran-preflight.R", "tools/cran-preflight-lib.R"),
    readLines,
    warn = FALSE
  )),
  collapse = "\n"
)
forbidden_calls <- c(
  "roxygenise(", "render(\"README.Rmd\"", "att_amend_desc(",
  "att_from_namespace(", "url_update(", "use_release_issue(",
  "test_dir(", "run_examples(", "build_vignettes(", "covr::"
)
for (call in forbidden_calls) {
  expect_identical(
    grepl(call, preflight_sources, fixed = TRUE),
    FALSE,
    paste("the prohibited preflight call", call)
  )
}
expect_identical(
  lengths(regmatches(
    preflight_sources,
    gregexpr("spelling::spell_check_package", preflight_sources, fixed = TRUE)
  )),
  1L,
  "one spelling invocation"
)
expect_identical(
  lengths(regmatches(
    preflight_sources,
    gregexpr("rcmdcheck::rcmdcheck", preflight_sources, fixed = TRUE)
  )),
  1L,
  "one R CMD check invocation"
)
expect_identical(
  grepl("--no-manual", preflight_sources, fixed = TRUE),
  FALSE,
  "the full PDF manual check"
)
expect_true(
  grepl("_R_CHECK_CRAN_INCOMING_REMOTE_", preflight_sources, fixed = TRUE),
  "the incoming remote check setting"
)

url_fixture <- data.frame(
  Status = c("404", "500", "429", "", "301"),
  Message = c(
    "Not Found", "Internal Server Error", "Too Many Requests",
    "Could not resolve host", "Moved Permanently"
  ),
  New = c("", "", "", "", "https://example.invalid/new"),
  stringsAsFactors = FALSE
)
expect_identical(
  classify_url_results(url_fixture),
  c("failed", "unavailable", "unavailable", "unavailable", "failed"),
  "URL retry classifications"
)
expect_true(
  check_reports_url_problem(list(
    errors = character(),
    warnings = "* checking URLs in DESCRIPTION ... WARNING",
    notes = character()
  )),
  "a check URL finding"
)

lint_workflow <- paste(
  readLines(".github/workflows/lint.yaml", warn = FALSE),
  collapse = "\n"
)
expect_true(
  grepl(
    "Rscript .github/scripts/verify-cran-preflight.R",
    lint_workflow,
    fixed = TRUE
  ),
  "the preflight verifier workflow invocation"
)

fixture_root <- tempfile("marginplyr-preflight-fixture-")
dir.create(fixture_root)
on.exit(unlink(fixture_root, recursive = TRUE), add = TRUE)

package_dir <- file.path(fixture_root, "marginplyr")
dir.create(package_dir)
writeLines(
  c(
    "Package: marginplyr",
    "Version: 0.1.0",
    "Config/marginplyr/cran-status: unpublished"
  ),
  file.path(package_dir, "DESCRIPTION")
)
tarball <- file.path(fixture_root, "marginplyr_0.1.0.tar.gz")
old_wd <- setwd(fixture_root)
utils::tar(basename(tarball), basename(package_dir), compression = "gzip")
setwd(old_wd)
unpacked <- file.path(fixture_root, "unpacked")
identity <- verify_candidate_tarball(
  tarball,
  expected_package = "marginplyr",
  expected_version = "0.1.0",
  unpack_dir = unpacked
)
expect_identical(identity$package, "marginplyr", "the tarball package")
expect_identical(identity$version, "0.1.0", "the tarball version")
expect_true(nzchar(identity$sha256), "the tarball SHA-256")
expect_identical(
  cran_status_from_tarball(tarball),
  "unpublished",
  "the tarball CRAN status"
)

comments <- file.path(fixture_root, "cran-comments.md")
writeLines(
  c(
    "## R CMD check results",
    "",
    "0 errors | 0 warnings | 1 note",
    "",
    "New submission"
  ),
  comments
)
counts <- c(errors = 0L, warnings = 0L, notes = 1L)
expect_identical(
  cran_comments_problems(comments, counts, list(allowed)),
  character(),
  "matching cran-comments"
)
expect_true(
  length(cran_comments_problems(
    comments,
    c(errors = 0L, warnings = 1L, notes = 1L),
    list(allowed)
  )) > 0L,
  "mismatched cran-comments counts"
)
writeLines("0 errors | 0 warnings | 1 note", comments)
expect_true(
  length(cran_comments_problems(comments, counts, list(allowed))) > 0L,
  "a missing allowed-NOTE explanation"
)

expect_identical(
  preflight_exit_code(candidate_failed = FALSE, tool_failed = FALSE),
  0L,
  "a successful preflight exit"
)
expect_identical(
  preflight_exit_code(candidate_failed = TRUE, tool_failed = FALSE),
  1L,
  "a candidate failure exit"
)
expect_identical(
  preflight_exit_code(candidate_failed = TRUE, tool_failed = TRUE),
  2L,
  "a tooling failure exit"
)
expect_identical(
  preflight_exit_code(
    candidate_failed = TRUE,
    tool_failed = TRUE,
    interrupted = TRUE
  ),
  130L,
  "an interrupted preflight exit"
)
expect_true(
  worktree_is_unchanged("", ""),
  "an unchanged clean worktree"
)
expect_identical(
  worktree_is_unchanged("", "?? unexpected"),
  FALSE,
  "a mutated clean worktree"
)

evidence <- file.path(fixture_root, "evidence")
dir.create(evidence)
state <- new_preflight_state(evidence)
state$candidate_sha <- paste(rep("a", 40L), collapse = "")
state$package <- "marginplyr"
state$version <- "0.1.0"
state$cran_status <- "unpublished"
state$tarball <- basename(tarball)
state$tarball_sha256 <- identity$sha256
state$counts <- counts
state$worktree_before <- ""
state$worktree_after <- ""
record_preflight_step(
  state,
  "fixture",
  "passed",
  elapsed = 0.1,
  evidence = basename(tarball),
  detail = "fixture completed"
)
write_preflight_evidence(state, exit_code = 0L)
expect_true(file.exists(file.path(evidence, "summary.md")), "the human summary")
expect_true(
  file.exists(file.path(evidence, "results.dcf")),
  "the machine result"
)
expect_true(file.exists(file.path(evidence, "steps.tsv")), "the step results")

run_in_dir <- function(path, command, args, stdout) {
  old <- setwd(path)
  on.exit(setwd(old), add = TRUE)
  status <- suppressWarnings(system2(
    command,
    vapply(args, shQuote, character(1)),
    stdout = stdout,
    stderr = stdout
  ))
  if (is.null(status) || length(status) == 0L) {
    return(0L)
  }
  if (is.character(status)) {
    code <- attr(status, "status")
    return(if (is.null(code)) 0L else as.integer(code))
  }
  as.integer(status)
}

cli_root <- file.path(fixture_root, "cli")
dir.create(file.path(cli_root, "tools"), recursive = TRUE)
dir.create(file.path(cli_root, ".github", "scripts"), recursive = TRUE)
copied_tools <- file.copy(
  c("tools/cran-preflight.R", "tools/cran-preflight-lib.R"),
  file.path(cli_root, "tools")
)
expect_true(all(copied_tools), "the fixture tool copies")
copied_policy <- file.copy(
  ".github/scripts/cran-note-policy.R",
  file.path(cli_root, ".github", "scripts")
)
expect_true(copied_policy, "the fixture policy copy")
writeLines(
  c(
    "Package: marginplyr",
    "Title: CRAN Preflight Fixture",
    "Version: 0.1.0",
    "Authors@R: person('Preflight', 'Fixture', role = c('aut', 'cre'),",
    "    email = 'fixture@example.invalid')",
    paste(
      "Description: A minimal package used to exercise the source-tarball",
      "preflight fixture."
    ),
    "License: MIT",
    "Suggests: marginplyrFixtureMissing",
    "Config/marginplyr/cran-status: unpublished",
    "Config/Needs/preflight: marginplyrFixtureMissing",
    "Encoding: UTF-8"
  ),
  file.path(cli_root, "DESCRIPTION")
)
writeLines("^tools$", file.path(cli_root, ".Rbuildignore"))
dir.create(file.path(cli_root, "R"))
writeLines("fixture <- function() TRUE", file.path(cli_root, "R", "fixture.R"))
writeLines("export(fixture)", file.path(cli_root, "NAMESPACE"))
expect_identical(
  run_in_dir(cli_root, "git", c("init", "--quiet"), TRUE),
  0L,
  "fixture git init"
)
expect_identical(
  run_in_dir(cli_root, "git", c("add", "."), TRUE),
  0L,
  "fixture git add"
)
expect_identical(run_in_dir(
  cli_root,
  "git",
  c(
    "-c", "user.name=Preflight Fixture",
    "-c", "user.email=fixture@example.invalid",
    "commit", "--quiet", "-m", "fixture"
  ),
  TRUE
), 0L, "fixture git commit")

build_evidence <- file.path(fixture_root, "build-evidence")
dir.create(build_evidence)
built_tarball <- build_candidate_tarball(
  cli_root,
  build_evidence,
  "marginplyr",
  "0.1.0"
)
built_identity <- verify_candidate_tarball(
  built_tarball,
  "marginplyr",
  "0.1.0",
  file.path(fixture_root, "built-unpacked")
)
expect_true(nzchar(built_identity$sha256), "the built fixture SHA-256")
built_contents <- utils::untar(built_tarball, list = TRUE)
expect_identical(
  any(startsWith(built_contents, "marginplyr/tools/")),
  FALSE,
  "the built tarball's tools exclusion"
)

rscript <- file.path(R.home("bin"), "Rscript")
clean_log <- file.path(fixture_root, "clean.log")
clean_status <- run_in_dir(
  cli_root,
  rscript,
  c(
    "tools/cran-preflight.R",
    "--output",
    file.path(fixture_root, "clean-evidence")
  ),
  clean_log
)
expect_identical(clean_status, 2L, "a missing-prerequisite subprocess")
expect_identical(
  readLines(file.path(fixture_root, "clean-evidence", "worktree-before.txt")),
  readLines(file.path(fixture_root, "clean-evidence", "worktree-after.txt")),
  "the tool-failure worktree invariant"
)

writeLines("dirty", file.path(cli_root, "untracked.txt"))
dirty_log <- file.path(fixture_root, "dirty.log")
dirty_status <- run_in_dir(
  cli_root,
  rscript,
  c(
    "tools/cran-preflight.R",
    "--output",
    file.path(fixture_root, "dirty-evidence")
  ),
  dirty_log
)
expect_identical(dirty_status, 1L, "a dirty-worktree subprocess")
expect_identical(
  readLines(file.path(fixture_root, "dirty-evidence", "worktree-before.txt")),
  readLines(file.path(fixture_root, "dirty-evidence", "worktree-after.txt")),
  "the candidate-failure worktree invariant"
)
unlink(file.path(cli_root, "untracked.txt"))

wrong_root_log <- file.path(fixture_root, "wrong-root.log")
wrong_root_status <- run_in_dir(
  fixture_root,
  rscript,
  c(
    file.path(cli_root, "tools", "cran-preflight.R"),
    "--output",
    file.path(fixture_root, "wrong-root-evidence")
  ),
  wrong_root_log
)
expect_identical(wrong_root_status, 2L, "a wrong-root subprocess")

command_failure <- run_system(
  rscript,
  c("-e", "quit(status = 7L)"),
  stdout = file.path(fixture_root, "command-failure.log")
)
expect_identical(
  command_failure$status,
  7L,
  "a checker subprocess failure status"
)

message(
  "Verified the CRAN preflight contract against policy, artifact, and ",
  "subprocess fixtures."
)
