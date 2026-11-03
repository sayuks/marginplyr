# Reproduces scope-decision inputs; scope remains the agent's policy judgment.
source("tools/review-ready-timing.R")
source("tools/review-ready-priority.R")

local({
  workspace <- tempfile("1193-scope-boundary-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  baseline <- file.path(workspace, "baseline")
  candidate <- file.path(workspace, "candidate")
  dir.create(baseline)
  dir.create(candidate)
  sha <- "1c26360d4c3fed4da9250b3870ca636dae23c925"
  archive <- file.path(workspace, "source.tar")
  invisible(processx::run("git", c("archive", "--format=tar", paste0("--output=", archive), sha)))
  utils::untar(archive, exdir = baseline)
  utils::untar(archive, exdir = candidate)
  retained <- "/Users/sayuks/Documents/marginplyr/.worktrees/1185-1186-list2env/.review-ready/runs/20261103T122403Z-32687-7faf7928b8"
  run <- file.path(workspace, "run")
  dir.create(run)
  stopifnot(all(file.copy(file.path(retained, c("invocation.rds", "result.rds", "timing")), run, recursive = TRUE)))
  originals <- c(file.path(retained, c("invocation.rds", "result.rds")),
    list.files(file.path(retained, "timing"), full.names = TRUE))
  original_hashes <- tools::md5sum(originals)
  paths <- strsplit(processx::run("git", c("ls-tree", "-r", "--name-only", sha))$stdout,
    "\n", fixed = TRUE)[[1L]]
  before_hashes <- unname(tools::md5sum(file.path(baseline, paths)))
  changed_paths <- function() paths[before_hashes != unname(tools::md5sum(file.path(candidate, paths)))]
  description <- readLines(file.path(candidate, "DESCRIPTION"))
  raw <- review_ready_priority_parse(description, "scope fixture")$priority
  writeLines(review_ready_priority_description(description, unique(raw)), file.path(candidate, "DESCRIPTION"))
  before <- testthat::find_test_scripts(file.path(baseline, "tests/testthat"),
    full.names = FALSE, start_first = raw)
  after <- testthat::find_test_scripts(file.path(candidate, "tests/testthat"),
    full.names = FALSE, start_first = review_ready_priority_read(readLines(file.path(candidate, "DESCRIPTION"))))
  stopifnot(identical(before, after), length(after) == 204L, !anyDuplicated(after),
    identical(changed_paths(), "DESCRIPTION"))
  output <- capture.output({ outcome <- review_ready_priority_propose(candidate, run) })
  stopifnot(outcome$eligible)
  cat("pure removal: only DESCRIPTION changed; exact native order unchanged; retained evidence eligible\n")

  helper <- "tests/testthat/helper-optional-backends.R"
  cat("\n# Scope boundary fixture\n", file = file.path(candidate, helper), append = TRUE)
  output <- capture.output({ outcome <- review_ready_priority_propose(candidate, run) })
  stopifnot(setequal(changed_paths(), c("DESCRIPTION", helper)), !outcome$eligible,
    grepl("test/helper content changed", outcome$reason, fixed = TRUE))
  cat("helper change: DESCRIPTION +", helper, "; retained evidence refused\n")
  stopifnot(file.copy(file.path(baseline, helper), file.path(candidate, helper), overwrite = TRUE))

  entry <- "tests/testthat.R"
  writeLines(c('Sys.setenv(TESTTHAT_CPUS = "1")', readLines(file.path(candidate, entry))), file.path(candidate, entry))
  output <- capture.output({ outcome <- review_ready_priority_propose(candidate, run) })
  stopifnot(setequal(changed_paths(), c("DESCRIPTION", entry)), !outcome$eligible,
    grepl("test/helper content changed: tests/testthat.R", outcome$reason, fixed = TRUE))
  cat("native dispatch entry change: DESCRIPTION +", entry, "; retained evidence refused\n")
  stopifnot(file.copy(file.path(baseline, entry), file.path(candidate, entry), overwrite = TRUE))

  runner <- "tools/review-ready-check-lib.R"
  text <- readLines(file.path(candidate, runner))
  stopifnot(sum(grepl('TESTTHAT_CPUS = "4"', text, fixed = TRUE)) == 1L)
  writeLines(sub('TESTTHAT_CPUS = "4"', 'TESTTHAT_CPUS = "1"', text, fixed = TRUE), file.path(candidate, runner))
  output <- capture.output({ outcome <- review_ready_priority_propose(candidate, run) })
  stopifnot(setequal(changed_paths(), c("DESCRIPTION", runner)), outcome$eligible)
  cat("runner worker-count change: DESCRIPTION +", runner,
    "; proposal evidence eligible, but dispatch machinery changed\n")
  stopifnot(identical(tools::md5sum(originals), original_hashes))
  cat("Original full records unchanged; no tests or package checks executed.\n")
})
