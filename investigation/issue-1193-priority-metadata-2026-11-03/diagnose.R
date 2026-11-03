# Dated diagnostic experiment; no production changes or package-suite execution.
source("tools/review-ready-timing.R")
source("tools/review-ready-priority.R")
description <- readLines("DESCRIPTION")
raw_priority <- trimws(strsplit(read.dcf("DESCRIPTION")[[1L,
  "Config/testthat/start-first"]], ",", fixed = TRUE)[[1L]])
cat("duplicates:", raw_priority[duplicated(raw_priority)], "\n")
fixed <- review_ready_priority_description(description, unique(raw_priority))
stopifnot(identical(review_ready_priority_read(fixed), unique(raw_priority)))
files <- testthat::find_test_scripts("tests/testthat", full.names = FALSE)
native <- testthat::find_test_scripts("tests/testthat", full.names = FALSE,
  start_first = raw_priority)
deduplicated <- testthat::find_test_scripts("tests/testthat", full.names = FALSE,
  start_first = unique(raw_priority))
stopifnot(!anyDuplicated(native), setequal(files, native),
  identical(native, deduplicated))
cat("native dispatch:", length(files), "files, exactly once; removing the later duplicate preserves exact order\n")
for (sha in c("3d631db7", "1a338e2a", "41ad3c5e", "a3b389a5", "4b34fdec")) {
  lines <- processx::run("git", c("show", paste0(sha, ":DESCRIPTION")))$stdout
  con <- textConnection(lines)
  dcf <- read.dcf(con)
  close(con)
  p <- trimws(strsplit(dcf[[1L, "Config/testthat/start-first"]], ",", fixed = TRUE)[[1L]])
  cat(sha, "entries", length(p), "duplicates", sum(duplicated(p)), "\n")
}
local({
  workspace <- tempfile("1193-retained-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  root <- file.path(workspace, "source")
  dir.create(root)
  archive <- file.path(workspace, "source.tar")
  sha <- "1c26360d4c3fed4da9250b3870ca636dae23c925"
  processx::run("git", c("archive", "--format=tar", paste0("--output=", archive), sha))
  utils::untar(archive, exdir = root)
  retained <- "/Users/sayuks/Documents/marginplyr/.worktrees/1185-1186-list2env/.review-ready/runs/20261103T122403Z-32687-7faf7928b8"
  run <- file.path(workspace, "run")
  dir.create(run)
  stopifnot(all(file.copy(file.path(retained, c("invocation.rds", "result.rds", "timing")),
    run, recursive = TRUE)))
  d <- readLines(file.path(root, "DESCRIPTION"))
  p <- trimws(strsplit(read.dcf(file.path(root, "DESCRIPTION"))[[1L,
    "Config/testthat/start-first"]], ",", fixed = TRUE)[[1L]])
  writeLines(review_ready_priority_description(d, unique(p)), file.path(root, "DESCRIPTION"))
  result <- review_ready_priority_propose(root, run)
  stopifnot(!result$eligible,
    identical(result$reason, "priority must contain unique literal test filename stems"))
  cat("retained evidence: fixing current DESCRIPTION alone still fails on archived duplicate\n")
  # Diagnostic experiment: normalize only the historical read in a private environment.
  # Raw artifacts, hashes and the current-priority strict reader remain unchanged.
  experiment <- new.env(parent = globalenv())
  source("tools/review-ready-priority.R", local = experiment)
  f <- experiment$review_ready_priority_evidence
  text <- deparse(body(f))
  target <- "measured_priority = review_ready_priority_read(old_description)"
  stopifnot(sum(grepl(target, text, fixed = TRUE)) == 1L)
  text <- sub(target, "measured_priority = read_historical_priority(old_description)",
    text, fixed = TRUE)
  body(f) <- parse(text = paste(text, collapse = "\n"))[[1L]]
  experiment$read_historical_priority <- function(description) {
    con <- textConnection(description)
    on.exit(close(con), add = TRUE)
    p <- trimws(strsplit(read.dcf(con)[[1L, "Config/testthat/start-first"]],
      ",", fixed = TRUE)[[1L]])
    # The strict reader still rejects nonliteral and empty historical entries.
    review_ready_priority_read(review_ready_priority_description(description, unique(p)))
  }
  experiment$review_ready_priority_evidence <- f
  recovered <- experiment$review_ready_priority_propose(root, run)
  stopifnot(recovered$eligible, identical(recovered$sha, sha),
    file.exists(file.path(recovered$directory, "proposal.rds")),
    file.exists(file.path(recovered$directory, "explanation.md")))
  print(recovered$models)
  cat("diagnostic historical normalization: eligible proposal recovered without modifying retained evidence or running tests\n")
})
