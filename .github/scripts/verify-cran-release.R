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
    "UTC\\s+timestamp uses the path-safe basic form `YYYYMMDDTHHMMSSZ`",
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

message(
  "Verified the CRAN release helpers against initial, update, preview, ",
  "zero-diff, idempotence, publication, dirty-state, and command-failure ",
  "fixtures."
)
