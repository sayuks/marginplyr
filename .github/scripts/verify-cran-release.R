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

description_lines <- c(
  "Package: marginplyr",
  "Version: 0.1.0.9000",
  "Config/marginplyr/cran-status: published"
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
    "Config/marginplyr/cran-status: unpublished"
  ),
  `NEWS.md` = c("# marginplyr 0.1.0", "", "* Initial submission."),
  `README.Rmd` = c(
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
    tolower(post$`README.Rmd`),
    fixed = TRUE
  )),
  "the CRAN badge or link"
)
expect_true(
  "install.packages(\"marginplyr\")" %in% post$`README.Rmd`,
  "the CRAN installation call"
)
expect_identical(
  release_post_files(post, "0.1.0"),
  post,
  "the idempotent initial post-release edit"
)

partial_initial <- initial_files
partial_initial$`README.Rmd` <- append(
  partial_initial$`README.Rmd`,
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
successful_runner <- function(command, args, root) {
  render_state$calls <- render_state$calls + 1L
  if (render_state$calls == 2L) {
    writeLines(
      readLines(file.path(root, "README.Rmd"), warn = FALSE),
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

message(
  "Verified the CRAN release helpers against initial, update, preview, ",
  "idempotence, publication, dirty-state, and command-failure fixtures."
)
