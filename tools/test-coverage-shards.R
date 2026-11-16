# Exercise sharded coverage through tools/coverage-check.R on a small package:
# merged shard results must reproduce the unsharded verdict, uncovered lines
# and per-line hits, and the merge refuses an incomplete set of shards.
source_root <- normalizePath(".")
rscript <- file.path(R.home("bin"), "Rscript")
expect <- function(ok, detail) if (!isTRUE(ok)) stop(detail, call. = FALSE)

# A package in the shape coverage-check-lib.R reads: one asserted optional
# Suggest, no exclusions, and four test files so a three-way split is uneven.
write_fixture <- function(root) {
  for (path in c("R", "tests/testthat", "inst/suggests", "tools")) {
    dir.create(file.path(root, path), recursive = TRUE)
  }
  writeLines(c("Package: shardfixture", "Version: 0.0.1",
    "Title: Coverage Shard Fixture", "Description: Coverage shard fixture.",
    "License: MIT", "Encoding: UTF-8",
    "Authors@R: person(\"A\", \"B\", role = c(\"aut\", \"cre\"), email = \"a@b.c\")",
    "Suggests: testthat (>= 3.0.0)", "Config/testthat/edition: 3"),
    file.path(root, "DESCRIPTION"))
  writeLines("export(alpha, beta, gamma)", file.path(root, "NAMESPACE"))
  writeLines(c("alpha <- function(x) {", "  x + 1", "}",
    "beta <- function(x) {", "  if (x > 0) {", "    return(\"positive\")",
    "  }", "  \"other\"", "}"), file.path(root, "R", "alpha.R"))
  writeLines(c("gamma <- function(x) {", "  x * 2", "}",
    "unused <- function() {", "  \"never\"", "}"),
    file.path(root, "R", "gamma.R"))
  writeLines("coverage: {}", file.path(root, "codecov.yml"))
  writeLines(c("library(testthat)", "library(shardfixture)",
    "test_check(\"shardfixture\")"), file.path(root, "tests", "testthat.R"))
  writeLines(c("optional_suggest_spec <- function() {",
    "  list(testthat = list(asserted = TRUE, companions = character()))",
    "}"), file.path(root, "tests/testthat/helper-optional-backends.R"))
  tests <- list(
    `test-a.R` = "expect_equal(alpha(1), 2)",
    `test-b.R` = c("expect_equal(beta(1), \"positive\")", "expect_equal(alpha(2), 3)"),
    `test-c.R` = "expect_equal(gamma(2), 4)",
    `test-d.R` = "expect_equal(alpha(3), 4)"
  )
  for (name in names(tests)) {
    writeLines(c("test_that(\"fixture\", {", paste0("  ", tests[[name]]), "})"),
      file.path(root, "tests/testthat", name))
  }
  file.copy(file.path(source_root, "inst/suggests/guard.R"),
    file.path(root, "inst/suggests"))
  for (path in c("coverage-check.R", "coverage-check-lib.R",
                 "required-test-suggests.R")) {
    file.copy(file.path(source_root, "tools", path), file.path(root, "tools"))
  }
}

# Each invocation gets its own RUNNER_TEMP, as each CI leg has its own runner;
# covr would otherwise reuse the previous install and its traces.
run <- function(args) {
  runner <- tempfile("runner-")
  dir.create(runner)
  on.exit(unlink(runner, recursive = TRUE), add = TRUE)
  output <- suppressWarnings(system2(rscript, c("tools/coverage-check.R", args),
    stdout = TRUE, stderr = TRUE, env = paste0("RUNNER_TEMP=", runner)))
  status <- attr(output, "status")
  list(status = if (is.null(status)) 0L else status,
    output = paste(output, collapse = "\n"))
}

in_root <- function(root, args) {
  previous <- setwd(root)
  on.exit(setwd(previous), add = TRUE)
  run(args)
}

# The verdict lines a reader compares: the tally and the uncovered list.
verdict <- function(output) {
  lines <- strsplit(output, "\n", fixed = TRUE)[[1L]]
  start <- grep("^covr [^:]+: [0-9]+/[0-9]+ lines", lines)
  expect(length(start) == 1L, paste("No coverage tally in:", output))
  lines[start:length(lines)]
}

line_hits <- function(path) {
  classes <- xml2::xml_find_all(xml2::read_xml(path), "//class")
  sort(unlist(lapply(classes, function(class) {
    lines <- xml2::xml_find_all(class, "./lines/line")
    paste(xml2::xml_attr(class, "filename"), xml2::xml_attr(lines, "number"),
      xml2::xml_attr(lines, "hits"), sep = ":")
  })))
}

main <- function() {
  workspace <- tempfile("coverage-shards-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  root <- file.path(workspace, "fixture")
  write_fixture(root)

  whole <- in_root(root, c("--cobertura", file.path(workspace, "whole.xml")))
  expect(whole$status == 1L, whole$output)
  expected <- verdict(whole$output)
  expect(any(expected == "R/gamma.R:5"), whole$output)

  shards <- file.path(workspace, "shards")
  dir.create(shards)
  for (index in 1:3) {
    leg <- in_root(root, c("--shard", paste0(index, "/3"), "--output",
      file.path(shards, paste0("coverage-shard-", index, ".rds"))))
    expect(leg$status == 0L, leg$output)
  }
  merged <- in_root(root, c("--merge", shards, "--cobertura",
    file.path(workspace, "merged.xml")))
  expect(merged$status == 1L, merged$output)
  expect(identical(verdict(merged$output), expected),
    paste("Merged verdict differs:", merged$output, "Unsharded:", whole$output))
  expect(identical(line_hits(file.path(workspace, "merged.xml")),
    line_hits(file.path(workspace, "whole.xml"))), "Merged line hits differ")
  # alpha() runs in three test files, which fall in two shards.
  expect(all(c("R/alpha.R:2:3", "R/gamma.R:5:0") %in%
    line_hits(file.path(workspace, "merged.xml"))), "Merged hits are not summed")

  for (bad in c("0/3", "4/3", "1/0", "1/5", "2")) {
    refused <- in_root(root, c("--shard", bad, "--output",
      file.path(workspace, "bad.rds")))
    expect(refused$status != 0L && !file.exists(file.path(workspace, "bad.rds")),
      paste("Accepted shard", bad, refused$output))
  }

  unlink(file.path(shards, "coverage-shard-2.rds"))
  missing <- in_root(root, c("--merge", shards, "--cobertura",
    file.path(workspace, "missing.xml")))
  expect(missing$status != 0L && grepl("exactly 3 shard results", missing$output,
    fixed = TRUE) && !file.exists(file.path(workspace, "missing.xml")),
    missing$output)

  empty <- file.path(workspace, "empty")
  dir.create(empty)
  none <- in_root(root, c("--merge", empty))
  expect(none$status != 0L && grepl("no shard results", none$output,
    fixed = TRUE), none$output)

  # A shard measured against a different test inventory is not this tree's.
  writeLines(c("test_that(\"fixture\", {", "  expect_equal(gamma(1), 2)", "})"),
    file.path(root, "tests/testthat/test-e.R"))
  leg <- in_root(root, c("--shard", "2/3", "--output",
    file.path(shards, "coverage-shard-2.rds")))
  expect(leg$status == 0L, leg$output)
  unlink(file.path(root, "tests/testthat/test-e.R"))
  stale <- in_root(root, c("--merge", shards))
  expect(stale$status != 0L && grepl("test files", stale$output, fixed = TRUE),
    stale$output)
  cat("Coverage shard command fixtures passed.\n")
}
main()
