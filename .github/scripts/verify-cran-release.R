# Exercises the stateless CRAN release helpers against local fixtures. Remote
# release actions stay in the playbook; these checks never create an issue,
# dispatch a workflow, upload a tarball, or mutate CRAN/GitHub state.

source("tools/cran-release.R")

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

expect_release_error <- function(expr, status, label) {
  error <- tryCatch(expr, marginplyr_release_error = identity)
  expect_true(inherits(error, "marginplyr_release_error"), label)
  expect_identical(error$status, status, paste(label, "status"))
  invisible(error)
}

# Returns one numbered stage so its release contract is tested in isolation.
release_playbook_stage <- function(lines, stage) {
  heading <- paste0("^### ", stage, "\\. ")
  starts <- grep(heading, lines)
  if (length(starts) != 1L) {
    stop("The release playbook must contain exactly one requested stage.", call. = FALSE)
  }

  later <- grep("^### [0-9]+\\. ", lines)
  ends <- later[later > starts]
  end <- if (length(ends)) ends[[1L]] - 1L else length(lines)
  paste(lines[starts:end], collapse = "\n")
}

# Requires every policy marker that a stage promises to release operators.
expect_stage_markers <- function(stage, markers, label) {
  missing <- markers[!vapply(markers, grepl, logical(1L), x = stage, fixed = TRUE)]
  expect_identical(missing, character(), label)
}

# Requires one complete contract while allowing prose to wrap at a different width.
expect_stage_patterns <- function(stage, patterns, label) {
  missing <- patterns[!vapply(
    patterns,
    grepl,
    logical(1L),
    x = stage,
    perl = TRUE
  )]
  expect_identical(missing, character(), label)
}

# Checks the failure cases of the release-playbook contract assertions.
expect_error <- function(expr, message, label) {
  error <- tryCatch({
    force(expr)
    NULL
  }, error = identity)
  expect_true(inherits(error, "error"), paste(label, "raises"))
  expect_true(
    grepl(message, conditionMessage(error), fixed = TRUE),
    paste(label, "message")
  )
}

description_lines <- c(
  "Package: marginplyr",
  "Version: 0.1.0.9000",
  "Config/marginplyr/cran-status: published",
  "Config/Needs/release: attachment, devtools, rhub, roxygen2"
)
news_lines <- c(
  "# marginplyr 0.1.0.9000",
  "",
  "* Development changes.",
  "",
  "# marginplyr 0.1.0",
  "",
  "* Initial CRAN submission."
)

prepared <- release_prepare_files(
  list(DESCRIPTION = description_lines, `NEWS.md` = news_lines),
  "0.1.1"
)
expect_identical(
  release_dcf_field(prepared$DESCRIPTION, "Version"),
  "0.1.1",
  "the update release version"
)
expect_identical(
  prepared$`NEWS.md`[[1L]],
  "# marginplyr 0.1.1",
  "the update NEWS heading"
)
expect_identical(
  release_prepare_files(prepared, "0.1.1"),
  prepared,
  "the idempotent preparation edit"
)

initial_files <- list(
  DESCRIPTION = c(
    "Package: marginplyr",
    "Version: 0.1.0",
    "Config/marginplyr/cran-status: unpublished",
    "Config/Needs/release: attachment, devtools, rhub, roxygen2"
  ),
  `NEWS.md` = c("# marginplyr 0.1.0", "", "* Initial submission."),
  `README.qmd` = c(
    "<!-- badges: start -->",
    "[![R-CMD-check](check.svg)](check)",
    "<!-- badges: end -->",
    "",
    "## Installation",
    "",
    "`marginplyr` is not on CRAN yet. Install it from",
    "[GitHub](https://github.com/sayuks/marginplyr) with:",
    "",
    "``` r",
    "install.packages(\"pak\")",
    "pak::pkg_install(\"sayuks/marginplyr\")",
    "```",
    "",
    "## Usage"
  ),
  `README.md` = c(
    "<!-- badges: start -->",
    "[![R-CMD-check](check.svg)](check)",
    "<!-- badges: end -->",
    "",
    "## Installation",
    "",
    "`marginplyr` is not on CRAN yet. Install it from",
    "[GitHub](https://github.com/sayuks/marginplyr) with:",
    "",
    "``` r",
    "install.packages(\"pak\")",
    "pak::pkg_install(\"sayuks/marginplyr\")",
    "```",
    "",
    "## Usage"
  )
)

release_packages <- character()
invisible(release_prerequisites(
  description = read.dcf("DESCRIPTION"),
  package_available = function(package) {
    release_packages <<- c(release_packages, package)
    TRUE
  }
))
expect_identical(
  release_packages,
  c("attachment", "devtools", "rhub", "roxygen2"),
  "the declared release packages"
)
expect_release_error(
  release_prerequisites(
    description = read.dcf("DESCRIPTION"),
    package_available = function(package) package != "attachment"
  ),
  2L,
  "a missing release prerequisite"
)

expect_release_error(
  release_prepare_files(initial_files[c("DESCRIPTION", "NEWS.md")], "0.2.0"),
  1L,
  "an unexpected initial version"
)

post <- release_post_files(initial_files, "0.1.0")
expect_identical(
  release_dcf_field(post$DESCRIPTION, "Version"),
  "0.1.0.9000",
  "the post-release development version"
)
expect_identical(
  release_dcf_field(
    post$DESCRIPTION,
    "Config/marginplyr/cran-status"
  ),
  "published",
  "the initial publication state"
)
expect_identical(
  post$`NEWS.md`[[1L]],
  "# marginplyr 0.1.0.9000",
  "the development NEWS heading"
)
expect_true(
  any(grepl(
    "cran.r-project.org/package=marginplyr",
    tolower(post$`README.qmd`),
    fixed = TRUE
  )),
  "the CRAN badge or link"
)
expect_true(
  "install.packages(\"marginplyr\")" %in% post$`README.qmd`,
  "the CRAN installation call"
)
expect_identical(
  release_post_files(post, "0.1.0"),
  post,
  "the idempotent initial post-release edit"
)

partial_initial <- initial_files
partial_initial$`README.qmd` <- append(
  partial_initial$`README.qmd`,
  paste0(
    "[![CRAN status](https://www.r-pkg.org/badges/version/marginplyr)]",
    "(https://CRAN.R-project.org/package=marginplyr)"
  ),
  after = 2L
)
expect_release_error(
  release_post_files(partial_initial, "0.1.0"),
  1L,
  "partial initial-only publication text"
)

published_without_claim <- initial_files
published_without_claim$DESCRIPTION <- release_set_dcf_field(
  published_without_claim$DESCRIPTION,
  "Config/marginplyr/cran-status",
  "published"
)
expect_release_error(
  release_post_files(published_without_claim, "0.1.0"),
  1L,
  "an update missing initial-only documentation"
)

published_with_badge <- published_without_claim
published_with_badge$`README.qmd` <- append(
  published_with_badge$`README.qmd`,
  c(
    paste0(
      "[![CRAN status](https://www.r-pkg.org/badges/version/marginplyr)]",
      "(https://CRAN.R-project.org/package=marginplyr)"
    ),
    "install.packages(\"marginplyr\")"
  ),
  after = 2L
)
expect_true(
  release_readme_claims_cran(published_with_badge$`README.qmd`),
  "a published README with an install call and badge"
)

packages <- matrix(
  c("marginplyr", "0.1.0", "other", "9.9.9"),
  ncol = 2L,
  byrow = TRUE,
  dimnames = list(NULL, c("Package", "Version"))
)
expect_identical(
  verify_cran_publication("0.1.0", packages = packages),
  "0.1.0",
  "a matching CRAN publication"
)
expect_release_error(
  verify_cran_publication("0.1.1", packages = packages),
  1L,
  "a CRAN publication version mismatch"
)

fixture_root <- tempfile("marginplyr-release-fixture-")
dir.create(fixture_root)
on.exit(unlink(fixture_root, recursive = TRUE), add = TRUE)

write_fixture <- function(root, files) {
  for (path in names(files)) {
    writeLines(files[[path]], file.path(root, path))
  }
}

write_fixture(fixture_root, initial_files)
old_wd <- setwd(fixture_root)
on.exit(setwd(old_wd), add = TRUE)
expect_identical(system2("git", c("init", "--quiet")), 0L, "fixture git init")
expect_identical(system2("git", c("add", ".")), 0L, "fixture git add")
expect_identical(
  system2(
    "git",
    c(
      "-c", "user.name=ReleaseFixture",
      "-c", "user.email=fixture@example.invalid",
      "commit", "--quiet", "-m", "fixture"
    )
  ),
  0L,
  "fixture git commit"
)
setwd(old_wd)

prepare_paths <- c("DESCRIPTION", "NEWS.md")
before_prepare <- release_read_files(fixture_root, prepare_paths)
zero_diff_output <- capture.output(
  zero_diff <- cran_release_operation(
    "prepare",
    "0.1.0",
    root = fixture_root,
    apply = FALSE
  ),
  type = "message"
)
expect_identical(zero_diff, character(), "the zero-diff preparation result")
expect_true(
  any(grepl(
    "prepare is already complete for 0.1.0.",
    zero_diff_output,
    fixed = TRUE
  )),
  "the zero-diff preparation diagnostic"
)
expect_identical(
  release_read_files(fixture_root, prepare_paths),
  before_prepare,
  "the non-mutating zero-diff preparation"
)

before <- readLines(file.path(fixture_root, "DESCRIPTION"), warn = FALSE)
preview <- capture.output(cran_release_operation(
  "post-release",
  "0.1.0",
  root = fixture_root,
  apply = FALSE,
  packages = packages
))
expect_identical(
  readLines(file.path(fixture_root, "DESCRIPTION"), warn = FALSE),
  before,
  "a non-mutating preview"
)
expect_true(
  any(grepl("DESCRIPTION", preview, fixed = TRUE)),
  "the previewed DESCRIPTION diff"
)

render_state <- new.env(parent = emptyenv())
render_state$calls <- 0L
render_state$args <- list()
successful_runner <- function(command, args, root) {
  render_state$calls <- render_state$calls + 1L
  render_state$args[[render_state$calls]] <- args
  if (render_state$calls == 2L) {
    writeLines(
      readLines(file.path(root, "README.qmd"), warn = FALSE),
      file.path(root, "README.md")
    )
  }
  list(status = 0L, output = "fixture command passed")
}
cran_release_operation(
  "post-release",
  "0.1.0",
  root = fixture_root,
  apply = TRUE,
  packages = packages,
  runner = successful_runner
)
expect_identical(
  render_state$calls,
  2L,
  "the post-release generation commands"
)
expect_true(
  any(grepl("quarto::quarto_render('README.qmd'", render_state$args[[2L]], fixed = TRUE)),
  "the Quarto README render command"
)
expect_true(
  !any(grepl("pandoc_version", render_state$args[[2L]], fixed = TRUE)),
  "the unpinned Pandoc renderer"
)
expect_identical(
  release_dcf_field(
    readLines(file.path(fixture_root, "DESCRIPTION"), warn = FALSE),
    "Version"
  ),
  "0.1.0.9000",
  "the applied development version"
)
expect_true(
  release_readme_claims_cran(readLines(
    file.path(fixture_root, "README.md"),
    warn = FALSE
  )),
  "the applied generated README"
)

old_wd <- setwd(fixture_root)
expect_identical(system2("git", c("add", ".")), 0L, "initial follow-up add")
expect_identical(
  system2(
    "git",
    c(
      "-c", "user.name=ReleaseFixture",
      "-c", "user.email=fixture@example.invalid",
      "commit", "--quiet", "-m", "initial-follow-up"
    )
  ),
  0L,
  "initial follow-up commit"
)
setwd(old_wd)

cran_release_operation(
  "prepare",
  "0.1.1",
  root = fixture_root,
  apply = TRUE
)
old_wd <- setwd(fixture_root)
expect_identical(system2("git", c("add", ".")), 0L, "update preparation add")
expect_identical(
  system2(
    "git",
    c(
      "-c", "user.name=ReleaseFixture",
      "-c", "user.email=fixture@example.invalid",
      "commit", "--quiet", "-m", "update-preparation"
    )
  ),
  0L,
  "update preparation commit"
)
setwd(old_wd)

update_packages <- packages
update_packages[update_packages[, "Package"] == "marginplyr", "Version"] <-
  "0.1.1"
render_state$calls <- 0L
cran_release_operation(
  "post-release",
  "0.1.1",
  root = fixture_root,
  apply = TRUE,
  packages = update_packages,
  runner = successful_runner
)
expect_identical(
  render_state$calls,
  2L,
  "the update post-release generation commands"
)
expect_identical(
  release_dcf_field(
    readLines(file.path(fixture_root, "DESCRIPTION"), warn = FALSE),
    "Version"
  ),
  "0.1.1.9000",
  "the applied update development version"
)

expect_release_error(
  release_assert_clean(fixture_root),
  1L,
  "a dirty release worktree"
)

failed_runner <- function(command, args, root) {
  list(status = 7L, output = "fixture command failed")
}
expect_release_error(
  release_render_readme(fixture_root, runner = failed_runner),
  2L,
  "a failed generated-file command"
)

parsed <- parse_cran_release_args(c(
  "post-release", "--version", "0.1.0", "--apply"
))
expect_identical(parsed$operation, "post-release", "the parsed operation")
expect_identical(parsed$version, "0.1.0", "the parsed version")
expect_identical(parsed$apply, TRUE, "the parsed apply flag")

release_source <- paste(readLines("tools/cran-release.R"), collapse = "\n")
forbidden <- c(
  "codex exec", "use_release_issue(", "gh issue", "rhub_check(",
  "win-builder", "upload", "submit"
)
for (marker in forbidden) {
  expect_identical(
    grepl(marker, release_source, fixed = TRUE),
    FALSE,
    paste("the absent release coordinator marker", marker)
  )
}

# Keeps the local R-hub customizations in place when rhub::rhub_setup()
# regenerates the otherwise generic workflow.
workflow_job <- function(lines, name) {
  start <- grep(paste0("^  ", name, ":$"), lines)
  expect_identical(
    length(start),
    1L,
    paste("the R-hub workflow", name, "job")
  )
  later_jobs <- grep("^  [[:alnum:]-]+:$", lines)
  end_candidates <- later_jobs[later_jobs > start]
  end <- if (length(end_candidates)) end_candidates[[1L]] - 1L else length(lines)
  lines[start:end]
}

expect_workflow_markers <- function(lines, markers, label) {
  present <- vapply(
    markers,
    function(marker) any(grepl(marker, lines, fixed = TRUE)),
    logical(1L)
  )
  missing <- markers[!present]
  expect_identical(missing, character(), label)
}

workflow_lines <- readLines(".github/workflows/rhub.yaml", warn = FALSE)
linux_containers <- workflow_job(workflow_lines, "linux-containers")
other_platforms <- workflow_job(workflow_lines, "other-platforms")

expect_identical(
  any(grepl("quarto-dev/quarto-actions/setup@", linux_containers, fixed = TRUE)),
  FALSE,
  "the Linux containers portable Quarto installation"
)
expect_workflow_markers(
  linux_containers,
  c(
    "Install portable Quarto from the official tarball",
    "set -euo pipefail",
    "x86_64 | amd64)",
    "quarto_arch=amd64",
    "aarch64 | arm64)",
    "quarto_arch=arm64",
    "Unsupported architecture for Quarto",
    "https://quarto.org/download/latest/quarto-linux-${quarto_arch}.tar.gz",
    "mktemp -d",
    "curl --fail --location --show-error --silent",
    "tar -xzf",
    "quarto_executable=\"$(find",
    "if [ ! -x \"$quarto_executable\" ]",
    "did not contain a usable executable",
    "quarto_bin_dir=\"$(dirname \"$quarto_executable\")\"",
    "printf '%s\\n' \"$quarto_bin_dir\" >> \"$GITHUB_PATH\"",
    "GITHUB_PATH",
    "command -v quarto",
    "quarto --version",
    "quarto pandoc --version"
  ),
  "the Linux containers portable Quarto contract"
)
quarto_package_manager_commands <- c(
  "jq", "sudo", "apt", "apt-get", "dnf", "yum", "checksum"
)
expect_identical(
  quarto_package_manager_commands[vapply(
    quarto_package_manager_commands,
    grepl,
    logical(1L),
    x = paste(linux_containers, collapse = "\n"),
    fixed = TRUE
  )],
  character(),
  "the Linux containers package-manager-free Quarto installation"
)
linux_containers_text <- paste(linux_containers, collapse = "\n")
expect_true(
  grepl(
    "x86_64 \\| amd64\\)[[:space:]]+quarto_arch=amd64",
    linux_containers_text
  ),
  "the x86_64 and amd64 Quarto architecture mapping"
)
expect_true(
  grepl(
    "aarch64 \\| arm64\\)[[:space:]]+quarto_arch=arm64",
    linux_containers_text
  ),
  "the aarch64 and arm64 Quarto architecture mapping"
)

step_index <- function(lines, marker) {
  matches <- grep(marker, lines, fixed = TRUE)
  expect_identical(length(matches) > 0L, TRUE, paste("the workflow marker", marker))
  matches[[1L]]
}

expect_true(
  step_index(linux_containers, "r-hub/actions/platform-info@v1") <
    step_index(linux_containers, "Install portable Quarto from the official tarball"),
  "platform information before portable Quarto installation"
)
expect_true(
  step_index(linux_containers, "Install portable Quarto from the official tarball") <
    step_index(linux_containers, "r-hub/actions/setup-deps@v1"),
  "portable Quarto installation before R-hub dependencies"
)
expect_true(
  any(grepl("quarto-dev/quarto-actions/setup@v2", other_platforms, fixed = TRUE)),
  "the other-platforms Quarto setup action"
)
rhub_zstd_step <- "Preinstall Zstandard-compressed macOS dependencies"
expect_workflow_markers(
  other_platforms,
  c(
    rhub_zstd_step,
    "if: runner.os == 'macOS'",
    "c(\"knitr\", \"xfun\")",
    "dependencies = NA",
    "type = \"binary\""
  ),
  "the R-hub macOS Zstandard dependency workaround"
)
expect_true(
  step_index(other_platforms, "r-hub/actions/setup-r@v1") <
    step_index(other_platforms, rhub_zstd_step),
  "R-hub setup before the macOS dependency workaround"
)
expect_true(
  step_index(other_platforms, rhub_zstd_step) <
    step_index(other_platforms, "r-hub/actions/setup-deps@v1"),
  "the macOS dependency workaround before R-hub dependencies"
)

playbook <- readLines("tools/cran-release.md", warn = FALSE)
stage_three <- release_playbook_stage(playbook, 3L)
expect_stage_markers(
  stage_three,
  c(
    "disposable copy",
    "exact clean",
    "preparation worktree",
    "never run",
    "release worktree",
    "durable absolute directory",
    "attachment::att_amend_desc()",
    "preparation commit SHA",
    "`attachment` version",
    "invocation",
    "console output",
    "DESCRIPTION diff",
    "set -e",
    "test -z",
    "attachment-invocation.txt",
    "direct and bare-name usage",
    "package code",
    "tests",
    "examples",
    "vignettes",
    "installed files",
    "optional-dependency guards",
    "VignetteBuilder",
    "Config/Needs/website",
    "`remove`",
    "`keep`",
    "`reclassify`",
    "`scanner false result`",
    "genuinely optional",
    "accepted before making",
    "tracked dependency-metadata edit",
    "Partial scanner output",
    "evaluation errors",
    "not a clean result",
    "Block if the dependency-audit evidence is incomplete",
    "git worktree add --detach",
    "Tracked-diff path",
    "Zero-diff path",
    "zero-diff preparation evidence",
    "current `origin/main`",
    "Do not create an empty commit",
    "approved zero-diff preparation evidence",
    "obtain approval before creating",
    "obtain approval before fetching again",
    "preparation PR is not required"
  ),
  "the Stage 3 dependency-audit contract"
)

stage_four <- release_playbook_stage(playbook, 4L)
expect_stage_markers(
  stage_four,
  c(
    "zero-diff preparation evidence",
    "audit SHA",
    "obtain approval before",
    "path-safe basic UTC",
    "+%Y%m%dT%H%M%SZ",
    "runs package-aware lintr",
    "checks spelling, checktor",
    ".Platform$path.sep",
    "before candidate build or check",
    "invocation unavailable with",
    "exit `2`, without relocating the bundle",
    "candidate-library-identity.dcf",
    "expected candidate",
    "fresh child's `find.package()` and `loadNamespace()` paths",
    "their match",
    "missing or mismatched candidate-library",
    "identity is tooling",
    "unavailable with exit `2`",
    "never a",
    "candidate finding or a passing check",
    "candidate-remediable",
    "one remediation PR",
    "one authoritative clean",
    "restart this stage"
  ),
  "the release-readiness audit contract"
)

expect_stage_patterns(
  stage_four,
  c(
    paste0(
      "First derive the public attempt ID and the standard private bundle location\\.\\s+",
      "The candidate SHA is the full 40-character value\\. Use the path-safe basic UTC\\s+",
      "timestamp required by (?s:.*?)attempt_started_at=\\$\\(date -u \\+%Y%m%dT%H%M%SZ\\)\\s+",
      "attempt_id=\"preflight-\\$\\{audit_sha\\}-\\$\\{attempt_started_at\\}\""
    ),
    paste0(
      "The standard location and every approved override are `--output` paths\\.\\s+",
      "Their\\s+spelling and resolved targets must omit `\\.Platform\\$path\\.sep`; ",
      "otherwise the\\s+command stops before candidate build or check as invocation ",
      "unavailable with\\s+exit `2`, without relocating the bundle\\."
    ),
    "candidate-library identity in a fresh base-only\\s+child",
    paste0(
      "`candidate-library-identity\\.dcf`\\. That record names the expected candidate\\s+",
      "directory, the fresh child's `find\\.package\\(\\)` and `loadNamespace\\(\\)` paths, and\\s+",
      "their match\\."
    ),
    paste0(
      "A missing or mismatched candidate-library\\s+identity is tooling\\s+unavailable ",
      "with exit `2`, never a candidate finding or a\\s+passing check;"
    )
  ),
  "the formal-preflight release-playbook contract"
)

expect_stage_patterns(
  stage_four,
  c(
    paste0(
      "execution identity until it returns a terminal completion result\\. ",
      "Only that\\s+attempt's terminal exit or interruption outcome is evidence\\."
    ),
    paste0(
      "Do not infer\\s+completion or interruption from partial output, ",
      "elapsed time, process absence,\\s+or a separate process check\\."
    ),
    paste0(
      "If the execution identity or terminal result is lost, the evidence is ",
      "unavailable\\.\\s+Report that before any replacement run, and do not ",
      "silently rerun the preflight\\."
    ),
    paste0(
      "retain the complete `exec_command` result\\. When it contains a\\s+live ",
      "`session_id`, continue that same terminal session with `write_stdin` ",
      "until\\s+a result includes `exit_code`; do not reduce an in-progress ",
      "result to `\\.output`\\s+alone\\."
    )
  ),
  "the formal-preflight execution-observation contract"
)

preflight <- paste(readLines("tools/cran-preflight.md", warn = FALSE), collapse = "\n")
expect_stage_markers(
  preflight,
  c(
    "Its public attempt",
    "path-safe basic form `YYYYMMDDTHHMMSSZ`",
    "`.Platform$path.sep`",
    "before candidate build or check",
    "exit `2` (invocation unavailable)",
    "expected path, `find.package()` path,",
    "`loadNamespace()` path, and match result",
    "A missing or mismatched",
    "identity is tooling",
    "unavailable with exit `2`",
    "never a",
    "candidate finding or a passing check",
    "`results.dcf` for machine-readable",
    "terminal evidence, `summary.md` for the human-readable summary, `steps.tsv` for",
    "per-stage evidence, and `candidate-library-identity.dcf` for the completed",
    "check's candidate-library identity."
  ),
  "the formal-preflight evidence-file contract"
)

expect_stage_patterns(
  preflight,
  c(
    paste0(
      "Its public attempt\\s+identifier is also the standard evidence-directory component, ",
      "so its UTC\\s+timestamp uses the path-safe basic form `YYYYMMDDTHHMMSSZ`:"
    ),
    paste0(
      "An explicit `--output <new-directory>` and its resolved target must not contain\\s+",
      "`\\.Platform\\$path\\.sep`, because R CMD check adds its installed candidate library\\s+",
      "to `R_LIBS`\\. The command rejects an unsafe path before candidate build or check\\s+",
      "work with exit `2` \\(invocation unavailable\\); it never relocates the bundle\\."
    ),
    paste0(
      "a fresh base-only R child that resolves `marginplyr` through the check\\s+",
      "library and retains its expected path, `find\\.package\\(\\)` path,\\s+",
      "`loadNamespace\\(\\)` path, and match result;"
    ),
    paste0(
      "A missing or mismatched\\s+candidate-library identity is tooling\\s+unavailable ",
      "with exit `2`, never a\\s+candidate finding or a passing check\\."
    )
  ),
  "the formal-preflight executable contract"
)

stage_five <- release_playbook_stage(playbook, 5L)
expect_stage_markers(
  stage_five,
  c(
    "approved audit SHA",
    "approved preparation SHA",
    "stage 4 repeats",
    "three SHAs differ",
    "outside the candidate worktree"
  ),
  "the candidate-freeze contract"
)

ledger_start <- grep("^## Release issue ledger$", playbook)
expect_identical(length(ledger_start), 1L, "the release ledger section")
ledger <- paste(playbook[ledger_start:length(playbook)], collapse = "\n")
expect_stage_markers(
  ledger,
  c(
    "Run the release-readiness audit for `<sha>`",
    "before freezing the Candidate SHA",
    "Preparation PR: <merged URL / not required (zero diff)>",
    "Zero-diff preparation evidence, when applicable",
    "Release-readiness audit exit 0",
    "Human approval of the audit result"
  ),
  "the zero-diff release-ledger contract"
)

stage_seven <- release_playbook_stage(playbook, 7L)
expect_stage_markers(
  stage_seven,
  c(
    "Stage 3 dependency-audit evidence",
    "every recorded",
    "disposition with the exact candidate",
    "exact candidate",
    "explicitly remains unchanged",
    "Block if the",
    "dependency-audit evidence",
    "Stage 7 disposition is missing"
  ),
  "the Stage 7 dependency-audit confirmation"
)

expect_stage_markers(
  paste(
    "### 3. Prepare and merge the release PR",
    "attachment::att_amend_desc()",
    sep = "\n"
  ),
  c("attachment::att_amend_desc()"),
  "a complete dependency-audit fixture"
)
expect_error(
  expect_stage_markers(
    "### 3. Prepare and merge the release PR",
    "attachment::att_amend_desc()",
    "a missing dependency-audit record"
  ),
  "expected character(0)",
  "a missing dependency-audit record"
)
expect_error(
  release_playbook_stage(c("### 3. Duplicate", "### 3. Duplicate"), 3L),
  "exactly one requested stage",
  "a duplicate release-playbook stage"
)

# The senders run against a stand-in for curl, so nothing is sent.
source("tools/submit-win-builder.R")

win_builder_page <- function(status = "", viewstate = "a+/=",
                             release_file = "FileUpload1") {
  c(
    "<form>",
    paste0(
      '<input type="hidden" name="__VIEWSTATE" id="__VIEWSTATE" value="',
      viewstate, '" />'
    ),
    '<input type="hidden" name="__VIEWSTATEGENERATOR" value="69164837" />',
    '<input type="hidden" name="__EVENTVALIDATION" value="b+/=" />',
    "<h2>R-release</h2>",
    paste0('<input type="file" name="', release_file, '" />'),
    '<input type="submit" name="Button1" value="Upload File" />',
    '<span id="Label1"></span>',
    "<h2>R-devel</h2>",
    "<!-- Currently unavailable-->",
    '<input type="file" name="FileUpload2" />',
    '<input type="submit" name="Button2" value="Upload File" />&nbsp;<br />',
    paste0('<span id="Label2">', status, "</span></div>"),
    "<!-- <h2>R-devel UCRT (new toolchain)</h2>",
    '<input type="file" name="FileUpload4" /> -->',
    "<h2>R-oldrelease</h2>",
    '<input type="file" name="FileUpload3" />',
    '<input type="submit" name="Button3" value="Upload File" />',
    "</form>"
  )
}

win_builder_status <- function(name, size) {
  paste0(
    "File name: ", name, "<br>File Size: ", size,
    " bytes<br>Content type: application/x-gzip"
  )
}

# Answers each curl call from `pages` in turn and records its arguments. A
# number stands for a curl that failed with that status. A page's `redirect`
# attribute is the redirect target curl reports, and its `status` attribute a
# failure that came with an answer.
submission_stub <- function(...) {
  pages <- list(...)
  calls <- list()
  list(
    run = function(args, directory) {
      calls[[length(calls) + 1L]] <<- args
      page <- pages[[length(calls)]]
      if (inherits(page, "interrupt")) stop(page)
      if (is.numeric(page)) {
        return(list(status = as.integer(page), stdout = "", stderr = "failed"))
      }
      output <- args[[which(args == "--output") + 1L]]
      writeLines(page, file.path(directory, output), useBytes = TRUE)
      redirect <- attr(page, "redirect")
      status <- attr(page, "status")
      list(status = if (is.null(status)) 0L else status, stdout = if (is.null(redirect)) "" else redirect,
           stderr = "")
    },
    calls = function() calls
  )
}

expect_submission_error <- function(expr, status, calls, stub, label) {
  error <- tryCatch(
    {
      output <- utils::capture.output(force(expr))
      NULL
    },
    marginplyr_submission_error = identity
  )
  expect_true(inherits(error, "marginplyr_submission_error"), label)
  expect_identical(error$status, status, paste(label, "status"))
  expect_identical(length(stub$calls()), calls, paste(label, "curl calls"))
}

# Masks cat() for one call, so that it fails on the line starting `prefix` as it
# does when the reader of the output has gone.
with_failing_line <- function(prefix, expr) {
  assign("cat", envir = globalenv(), function(...) {
    if (startsWith(paste0(...), prefix)) stop("the output is closed")
    base::cat(...)
  })
  on.exit(rm("cat", envir = globalenv()))
  force(expr)
}

win_builder_directory <- tempfile("win builder, fixture;")
dir.create(win_builder_directory)
win_builder_tarball <- file.path(win_builder_directory, "marginplyr_0.1.0.tar.gz")
writeBin(charToRaw("fixture"), win_builder_tarball)
win_builder_digest <- unname(tools::sha256sum(win_builder_tarball))
win_builder_args <- c(
  "--tarball", win_builder_tarball, "--sha256", win_builder_digest
)

stub <- submission_stub(win_builder_page())
output <- utils::capture.output(
  outcome <- submit_win_builder(win_builder_args, stub$run)
)
expect_identical(outcome, "preview", "the Win-Builder preview")
expect_identical(length(stub$calls()), 1L, "the preview's one curl call")
expect_true(
  any(grepl("nothing was sent", output, fixed = TRUE)), "the preview notice"
)

stub <- submission_stub(
  win_builder_page(),
  win_builder_page(win_builder_status("marginplyr_0.1.0.tar.gz", 7))
)
output <- utils::capture.output(
  outcome <- submit_win_builder(c(win_builder_args, "--send"), stub$run)
)
expect_identical(outcome, "accepted", "the accepted Win-Builder upload")
win_builder_sent <- stub$calls()[[2L]]
expect_identical(
  win_builder_sent[win_builder_sent %in% c(
    "__VIEWSTATE=a+/=", "__VIEWSTATEGENERATOR=69164837",
    "__EVENTVALIDATION=b+/=", "Button2=Upload File"
  ) | grepl("^FileUpload", win_builder_sent)],
  c(
    "__VIEWSTATE=a+/=", "__VIEWSTATEGENERATOR=69164837",
    "__EVENTVALIDATION=b+/=",
    "FileUpload1=@empty;filename=;type=application/octet-stream",
    "FileUpload2=@marginplyr_0.1.0.tar.gz;type=application/x-gzip",
    "FileUpload3=@empty;filename=;type=application/octet-stream",
    "Button2=Upload File"
  ),
  "the Win-Builder upload fields"
)
for (args in stub$calls()) {
  expect_identical(
    args[c(which(args == "--proto") + 1L, length(args))],
    c("=https", "https://win-builder.r-project.org/upload.aspx"),
    "the HTTPS-only Win-Builder destination"
  )
  expect_identical(
    any(c("-k", "--insecure") %in% args), FALSE, "the certificate check"
  )
  expect_identical(args[[1L]], "--disable", "the ignored curl configuration")
  expect_identical(
    args[[which(args == "--max-time") + 1L]], "300", "the Win-Builder time limit"
  )
}

writeBin(raw(100000), win_builder_tarball)
stub <- submission_stub(
  win_builder_page(),
  win_builder_page(win_builder_status("marginplyr_0.1.0.tar.gz", "100000"))
)
output <- utils::capture.output(
  outcome <- submit_win_builder(
    c(
      "--tarball", win_builder_tarball, "--sha256",
      unname(tools::sha256sum(win_builder_tarball)), "--send"
    ),
    stub$run
  )
)
expect_identical(outcome, "accepted", "an upload whose size prints as 1e+05")
writeBin(charToRaw("fixture"), win_builder_tarball)

# Nothing is sent: no curl call, or only the form's.
refusals <- list(
  "a wrong digest" = c(
    "--tarball", win_builder_tarball, "--sha256", strrep("0", 64L), "--send"
  ),
  "a missing tarball" = c(
    "--tarball", file.path(win_builder_directory, "marginplyr_0.2.0.tar.gz"),
    "--sha256", win_builder_digest, "--send"
  ),
  "another file" = c(
    "--tarball", "DESCRIPTION", "--sha256", win_builder_digest, "--send"
  ),
  "an absent digest" = c("--tarball", win_builder_tarball, "--send"),
  "an unknown option" = c(win_builder_args, "--send", "--force"),
  "a missing CA bundle" = c(
    win_builder_args, "--send", "--ca-bundle", win_builder_digest
  )
)
for (label in names(refusals)) {
  stub <- submission_stub()
  expect_submission_error(
    submit_win_builder(refusals[[label]], stub$run), 1L, 0L, stub, label
  )
}
forms <- list(
  "an unreachable form" = 60,
  "a form without its state" = win_builder_page(viewstate = ""),
  "a field name curl would read as a file" = win_builder_page(
    release_file = "a=@/etc/hosts;x"
  ),
  "a form without R-devel" = setdiff(win_builder_page(), "<h2>R-devel</h2>")
)
for (label in names(forms)) {
  stub <- submission_stub(forms[[label]])
  expect_submission_error(
    submit_win_builder(c(win_builder_args, "--send"), stub$run),
    1L, 1L, stub, label
  )
}

# An upload was attempted, and it is not repeated.
responses <- list(
  "a lost response" = 28,
  "a response without an upload" = win_builder_page(),
  "a response naming another size" = win_builder_page(
    win_builder_status("marginplyr_0.1.0.tar.gz", 70)
  ),
  "a response naming another file" = win_builder_page(
    win_builder_status("marginplyr_0.2.0.tar.gz", 7)
  ),
  "an unreadable response" = "<html></html>",
  "a response that is not UTF-8" = win_builder_page("\xff\xfe"),
  "an interrupted upload" = structure(
    class = c("interrupt", "condition"), list(message = "", call = NULL)
  )
)
for (label in names(responses)) {
  stub <- submission_stub(win_builder_page(), responses[[label]])
  expect_submission_error(
    submit_win_builder(c(win_builder_args, "--send"), stub$run),
    2L, 2L, stub, label
  )
}
stub <- submission_stub(
  win_builder_page(),
  win_builder_page(win_builder_status("marginplyr_0.1.0.tar.gz", 7))
)
expect_submission_error(
  with_failing_line(
    "Accepted.", submit_win_builder(c(win_builder_args, "--send"), stub$run)
  ),
  2L, 2L, stub, "an accepted upload that cannot be printed"
)
unlink(win_builder_directory, recursive = TRUE)

# The CRAN submitter, against the same stand-in.
source("tools/submit-cran.R")

cran_directory <- tempfile("cran fixture, ;")
dir.create(file.path(cran_directory, "marginplyr"), recursive = TRUE)
cran_description <- c(
  "Package: marginplyr", "Version: 0.1.0", "Title: 'A' Title",
  "Description: Text that", "    continues & compares.",
  "Author: A Person [aut, cre]", "Maintainer: A Person <a@example.org>",
  "License: MIT + file LICENSE", "Depends: R (>= 4.1.0)",
  "Imports: cli (>= 3.4.0), dplyr,", "        glue", "Suggests: DBI"
)
writeLines(
  cran_description, file.path(cran_directory, "marginplyr", "DESCRIPTION")
)
cran_comments <- c("## Results", "", "None <here> \u2014 & 'there'.")
writeLines(
  enc2utf8(cran_comments), file.path(cran_directory, "comments.md"),
  useBytes = TRUE
)
old_cran_wd <- setwd(cran_directory)
utils::tar("marginplyr_0.1.0.tar.gz", "marginplyr", compression = "gzip")
setwd(old_cran_wd)
cran_tarball <- file.path(cran_directory, "marginplyr_0.1.0.tar.gz")
cran_args <- c(
  "--tarball", cran_tarball,
  "--sha256", unname(tools::sha256sum(cran_tarball)),
  "--comments", file.path(cran_directory, "comments.md"),
  "--comments-sha256",
  unname(tools::sha256sum(file.path(cran_directory, "comments.md")))
)

cran_form_page <- c(
  '<FORM action="index2.php" method="post" enctype="multipart/form-data">',
  '<input name="pkg_id" type="hidden"  >',
  '<INPUT size=19 name="name"><INPUT size=19 name="email">',
  '<input name="uploaded_file" type="file" id="userfile">',
  "<textarea name='comment' cols=50 rows=10></textarea>",
  '<INPUT type="submit" value="Upload the package" name="upload">', "</FORM>"
)
cran_review_page <- c(
  "<strong><font color=red>Please review and submit.</font></strong>",
  '<input name="pkg_id" type="hidden" value="77">',
  '<tr><td>Your name:</td><td><INPUT name="name" value="A Person"></td></tr>',
  "<textarea name='comment' cols=50 rows=10>## Results", "",
  "None &lt;here&gt; \u2014 &amp; &#039;there&#039;.", "</textarea>",
  '<td>Package:</td><td><input name="uploaded_file" type="file">',
  '<tr><td>Package:</td><td><INPUT size=19 value="marginplyr" readonly></td>',
  '<tr><td>Version:</td><td><INPUT size=19 value="0.1.0" readonly></td></tr>',
  paste0(
    '<tr><td valign="top">Title:</td><td><textarea cols=50 readonly>',
    "'A' Title</textarea></td></tr>"
  ),
  paste0(
    '<tr><td valign="top">Description:</td><td><textarea readonly>',
    "Text that continues &amp; compares.</textarea></td></tr>"
  ),
  '<tr><td>Author(s):</td><td><INPUT value="A Person [aut, cre]" readonly></td>',
  '<tr><td>Maintainer Name:</td><td><INPUT value="A Person" readonly></td>',
  '<tr><td>Maintainer Email:</td><td><INPUT value="a@example.org" readonly></td>',
  '<tr><td>License:</td><td><INPUT value="MIT + file LICENSE" readonly></td>',
  "<tr><td>Depends:</td><td><textarea readonly>R (&gt;= 4.1.0)</textarea></td>",
  "<tr><td>Suggests:</td><td><textarea readonly>DBI</textarea></td></tr>",
  paste0(
    "<tr><td>Imports:</td><td><textarea readonly>",
    "cli (&gt;= 3.4.0), dplyr, glue</textarea></td></tr>"
  ),
  "<tr><td>Linking To:</td><td><textarea readonly></textarea></td></tr>",
  '<INPUT type="submit" value="Submit package" name="submit">'
)
cran_answer <- function(target) {
  structure("", redirect = paste0("https://xmpalantir.wu.ac.at/cransubmit/", target))
}
cran_uploaded <- cran_answer("index.php?strErr=99&edit=1&pkg_id=77")
cran_submitted <- cran_answer("index.php?strErr=0&submit=1")

stub <- submission_stub(cran_form_page)
output <- utils::capture.output(outcome <- submit_cran(cran_args, stub$run))
expect_identical(outcome, "preview", "the CRAN preview")
expect_identical(length(stub$calls()), 1L, "the CRAN preview's one curl call")
expect_true(
  any(output == "Submitter: A Person <a@example.org>"), "the previewed submitter"
)

stub <- submission_stub(
  cran_form_page, cran_uploaded, cran_review_page, cran_submitted
)
output <- utils::capture.output(
  outcome <- submit_cran(c(cran_args, "--send"), stub$run)
)
expect_identical(outcome, "submitted", "the accepted CRAN submission")
expect_identical(length(stub$calls()), 4L, "the CRAN submission's curl calls")
cran_fields <- function(args) {
  args[which(args %in% c("--form-string", "-F")) + 1L]
}
cran_identity <- c("name=A Person", "email=a@example.org")
expect_identical(
  cran_fields(stub$calls()[[2L]]),
  c(
    "pkg_id=", cran_identity, "upload=Upload the package",
    "uploaded_file=@marginplyr_0.1.0.tar.gz;type=application/x-gzip",
    "comment=<comments.txt;type=text/plain"
  ),
  "the CRAN upload fields"
)
expect_identical(
  cran_fields(stub$calls()[[4L]]),
  c(
    "pkg_id=77", cran_identity, "submit=Submit package",
    "uploaded_file=@empty;filename=;type=application/octet-stream",
    "comment=<comments.txt;type=text/plain"
  ),
  "the CRAN submission fields"
)
for (args in stub$calls()) {
  expect_identical(args[[1L]], "--disable", "the ignored curl configuration")
  expect_identical(
    args[[which(args == "--proto") + 1L]], "=https", "the HTTPS-only CRAN call"
  )
  expect_true(
    startsWith(args[[length(args)]], "https://xmpalantir.wu.ac.at/cransubmit/"),
    "the CRAN destination"
  )
  expect_identical(
    any(c("-k", "--insecure") %in% args), FALSE, "the CRAN certificate check"
  )
  expect_true(
    all(c("--fail", "--cookie", "--cookie-jar") %in% args),
    "the CRAN call's failure and session options"
  )
  expect_identical(
    args[[which(args == "--max-time") + 1L]], "300", "the CRAN time limit"
  )
}
expect_identical(
  vapply(stub$calls(), function(args) basename(args[[length(args)]]), ""),
  c(
    "cransubmit", "index2.php", "index.php?strErr=99&edit=1&pkg_id=77",
    "index2.php"
  ),
  "the CRAN pages"
)
for (args in stub$calls()[c(2L, 4L)]) {
  expect_identical(
    args[[which(args == "--write-out") + 1L]], "%{redirect_url}",
    "the CRAN redirect report"
  )
}
cran_failed <- function(page) structure(page, status = 22L)

cran_refusals <- list(
  "a wrong CRAN digest" = replace(cran_args, 4L, strrep("0", 64L)),
  "missing CRAN comments" = replace(cran_args, 6L, "absent.md"),
  "absent CRAN comments" = cran_args[-(5:6)],
  "unapproved CRAN comments" = replace(cran_args, 8L, strrep("0", 64L))
)
for (label in names(cran_refusals)) {
  stub <- submission_stub()
  expect_submission_error(
    submit_cran(c(cran_refusals[[label]], "--send"), stub$run),
    1L, 0L, stub, label
  )
}
cran_forms <- list(
  "an unreachable CRAN form" = 6,
  "a CRAN form closed for maintenance" = "<html></html>"
)
for (label in names(cran_forms)) {
  stub <- submission_stub(cran_forms[[label]])
  expect_submission_error(
    submit_cran(c(cran_args, "--send"), stub$run), 1L, 1L, stub, label
  )
}

# The upload was attempted and nothing was submitted.
cran_uploads <- list(
  "a lost CRAN upload" = list(28),
  "a refused CRAN upload" = list(cran_answer("index.php?strErr=1")),
  "a CRAN upload redirected elsewhere" = list(
    structure("", redirect = sub(
      "wu.ac.at", "ex.am.pl", attr(cran_uploaded, "redirect"), fixed = TRUE
    ))
  ),
  "a CRAN upload with another error" = list(
    cran_answer("index.php?strErr=1&edit=1&pkg_id=77")
  ),
  "a failed CRAN upload" = list(cran_failed(cran_uploaded)),
  "a lost CRAN review page" = list(cran_uploaded, 28),
  "a failed CRAN review page" = list(
    cran_uploaded, cran_failed(cran_review_page)
  ),
  "another submission's CRAN review page" = list(
    cran_uploaded, sub('value="77"', 'value="78"', cran_review_page, fixed = TRUE)
  ),
  "another maintainer name's CRAN review page" = list(
    cran_uploaded,
    sub('"A Person" readonly', '"B Person" readonly', cran_review_page,
        fixed = TRUE)
  ),
  "another maintainer's CRAN review page" = list(
    cran_uploaded,
    sub('"a@example.org" readonly', '"b@example.org" readonly',
        cran_review_page, fixed = TRUE)
  ),
  "a CRAN review page with two versions" = list(
    cran_uploaded,
    append(
      cran_review_page,
      grep("Version:", cran_review_page, fixed = TRUE, value = TRUE)
    )
  )
)
for (line in grep("readonly|Please|Submit package|pkg_id|None", cran_review_page)) {
  label <- paste("a CRAN review page without", cran_review_page[[line]])
  cran_uploads[[label]] <- list(cran_uploaded, cran_review_page[-line])
}
cran_uploads[["a CRAN review page with another version"]] <- list(
  cran_uploaded, sub("0.1.0", "0.1.1", cran_review_page, fixed = TRUE)
)
for (label in names(cran_uploads)) {
  stub <- do.call(submission_stub, c(list(cran_form_page), cran_uploads[[label]]))
  expect_submission_error(
    submit_cran(c(cran_args, "--send"), stub$run),
    2L, 1L + length(cran_uploads[[label]]), stub, label
  )
}

cran_outcomes <- list(
  "a lost CRAN submission" = 28,
  "a refused CRAN submission" = cran_answer("index.php?strErr=5&submit=1"),
  "a failed CRAN submission" = cran_failed(cran_submitted)
)
for (label in names(cran_outcomes)) {
  stub <- submission_stub(
    cran_form_page, cran_uploaded, cran_review_page, cran_outcomes[[label]]
  )
  expect_submission_error(
    submit_cran(c(cran_args, "--send"), stub$run), 2L, 4L, stub, label
  )
}
stub <- submission_stub(
  cran_form_page, cran_uploaded, cran_review_page, cran_submitted
)
expect_submission_error(
  with_failing_line(
    "Submitted.", submit_cran(c(cran_args, "--send"), stub$run)
  ),
  2L, 4L, stub, "an accepted submission that cannot be printed"
)
unlink(cran_directory, recursive = TRUE)

message(
  "Verified the CRAN release helpers against initial, update, preview, ",
  "zero-diff, idempotence, publication, dirty-state, and command-failure ",
  "fixtures, and the Win-Builder and CRAN senders against preview, accepted, ",
  "refused, and unknown-outcome fixtures."
)
