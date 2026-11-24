# Exercise the CRAN tier (ADR 0045) at the two places a check meets it: the
# filter tests/testthat.R hands test_check(), and the time budget
# .github/scripts/verify-cran-test-budget.R holds a finished check to.
source_root <- normalizePath(".")
rscript <- file.path(R.home("bin"), "Rscript")
expect <- function(ok, detail) if (!isTRUE(ok)) stop(detail, call. = FALSE)

tier_variables <- c("NOT_CRAN", "MARGINPLYR_COMPLETE_SUITE")

# Runs `code` with exactly the named variables set among `tier_variables` and
# the Review-ready reporter hook, then restores the caller's values.
with_environment <- function(values, code) {
  names <- c(tier_variables, "MARGINPLYR_REVIEW_PROGRESS_REPORTER")
  previous <- Sys.getenv(names, unset = NA_character_)
  on.exit({
    Sys.unsetenv(names[is.na(previous)])
    held <- previous[!is.na(previous)]
    if (length(held) > 0L) do.call(Sys.setenv, as.list(held))
  }, add = TRUE)
  Sys.unsetenv(names)
  if (length(values) > 0L) do.call(Sys.setenv, as.list(values))
  force(code)
}

# What `tests_dir`/testthat.R asks testthat for: its script evaluated from that
# directory, as R CMD check runs it, with test_check() replaced by a recorder.
# `files` is the filter it passed resolved by testthat's own file search, and
# `registered` the names it registers.
selection <- function(tests_dir, values) {
  with_environment(values, {
    received <- NULL
    scope <- new.env(parent = globalenv())
    scope$library <- function(...) invisible(NULL)
    scope$test_check <- function(package, ...) received <<- list(...)
    previous <- setwd(tests_dir)
    on.exit(setwd(previous), add = TRUE)
    sys.source("testthat.R", envir = scope)
    expect(!is.null(received), "tests/testthat.R never called test_check().")
    list(registered = scope$cran_tier,
      files = testthat::find_test_scripts("testthat", filter = received$filter,
        full.names = FALSE))
  })
}

# Which members of `names` some file in `files` calls, optionally as the first
# argument of one of `callers`.
called_in <- function(files, names, callers = NULL) {
  text <- unlist(lapply(files, readLines, warn = FALSE))
  vapply(names, function(name) {
    quoted <- gsub(".", "[.]", name, fixed = TRUE)
    pattern <- if (is.null(callers)) {
      paste0("(^|[^A-Za-z0-9_.])", quoted, "[(]")
    } else {
      paste0("(", paste(callers, collapse = "|"), ")[(][[:space:]]*\"", quoted, "\"")
    }
    any(grepl(pattern, text))
  }, logical(1))
}

main <- function() {
  tests_dir <- file.path(source_root, "tests")
  every <- list.files(file.path(tests_dir, "testthat"), pattern = "^test-.*[.]R$")

  # Nothing asking for the complete suite is how CRAN runs the check.
  cran <- selection(tests_dir, character())
  tier <- cran$files
  expect(length(tier) > 0L && length(tier) < length(every) && all(tier %in% every),
    paste0("CRAN semantics must select a strict, non-empty subset; selected ",
      length(tier), " of ", length(every), "."))
  # A registered name with no file would shrink the tier without a failure.
  unmatched <- setdiff(paste0("test-", cran$registered, ".R"), tier)
  expect(length(unmatched) == 0L && !anyDuplicated(cran$registered),
    paste0("tests/testthat.R must register each existing test file once; no ",
      "file answers: ", paste(unmatched, collapse = ", "), "."))
  expect(identical(selection(tests_dir, c(NOT_CRAN = "false"))$files, tier),
    "NOT_CRAN=false must select the CRAN tier.")
  for (name in tier_variables) {
    complete <- selection(tests_dir, stats::setNames("true", name))$files
    expect(identical(sort(complete), sort(every)),
      paste0(name, "=true must select every test file."))
  }

  # The tier is chosen to reach the whole public surface: every export, and a
  # guarded test for every optional backend a minimal check asserts absent.
  paths <- file.path(tests_dir, "testthat", tier)
  namespace <- parse(file.path(source_root, "NAMESPACE"))
  exports <- unlist(lapply(namespace, function(directive) {
    if (identical(directive[[1L]], quote(export))) {
      vapply(as.list(directive)[-1L], as.character, character(1))
    }
  }))
  expect(length(exports) > 0L, "NAMESPACE exports nothing.")
  uncalled <- exports[!called_in(paths, exports)]
  expect(length(uncalled) == 0L, paste0("No CRAN-tier test calls: ",
    paste(uncalled, collapse = ", "), ". Register a test file that does in ",
    "tests/testthat.R."))
  helpers <- new.env()
  sys.source(file.path(tests_dir, "testthat", "helper-optional-backends.R"),
    envir = helpers)
  backends <- helpers$optional_backends()
  unguarded <- backends[!called_in(paths, backends,
    c("skip_if_suggest_absent", "suggest_available"))]
  expect(length(unguarded) == 0L, paste0("No CRAN-tier test is guarded on: ",
    paste(unguarded, collapse = ", "), ". Register a test file that is in ",
    "tests/testthat.R."))

  # cran-comments.md tells CRAN that the check skips no test when every
  # Suggested package is installed, and testthat skips a snapshot expectation
  # there.
  skipped <- vapply(paths, function(path) {
    any(grepl("(^|[^A-Za-z0-9_.])(expect_snapshot[a-z_]*|skip_on_cran)[(]",
      readLines(path, warn = FALSE)))
  }, logical(1))
  expect(!any(skipped), paste0("CRAN skips a test in: ",
    paste(tier[skipped], collapse = ", "), ". Take the file out of `cran_tier` ",
    "in tests/testthat.R, or rewrite the account of skips in cran-comments.md."))

  # The budget reads the proc.time() trailer R CMD BATCH ends a test log with.
  source(file.path(source_root, ".github", "scripts", "cran-test-budget.R"),
    local = TRUE)
  log <- function(elapsed) c("[ FAIL 0 | WARN 0 | SKIP 13 | PASS 7425 ]", "> ",
    "> proc.time()", "   user  system elapsed ",
    sprintf(" 73.507   5.577 %s ", elapsed))
  expect(identical(test_elapsed_seconds(log("78.962")), 78.962),
    "The elapsed seconds of a finished test log must be read.")
  fixture <- tempfile("cran-tier-")
  check <- file.path(fixture, "check")
  rcheck <- file.path(check, "marginplyr.Rcheck")
  dir.create(file.path(rcheck, "tests"), recursive = TRUE)
  on.exit(unlink(fixture, recursive = TRUE), add = TRUE)
  finished <- function(lines, name = "testthat.Rout") {
    unlink(list.files(file.path(rcheck, "tests"), full.names = TRUE))
    writeLines(lines, file.path(rcheck, "tests", name))
    rcheck
  }
  expect(is.null(cran_test_budget_problem(finished(log("78.962")), budget = 300)),
    "A run inside the budget must raise nothing.")
  expect(is.null(cran_test_budget_problem(finished(log("300.000")), budget = 300)),
    "A run exactly on the budget must raise nothing.")
  over <- cran_test_budget_problem(finished(log("1301.250")), budget = 300)
  expect(is.character(over) && grepl("1301", over, fixed = TRUE) &&
    grepl("300", over, fixed = TRUE), "A run over the budget must be reported.")
  unread <- cran_test_budget_problem(finished(log("78.962")[1:2]), budget = 300)
  expect(is.character(unread) && grepl("elapsed", unread, fixed = TRUE),
    "A log without the trailer must be reported, not passed.")
  # A failing script's log is renamed, and is not a finished run.
  stopped <- cran_test_budget_problem(
    finished(log("78.962"), "testthat.Rout.fail"), budget = 300)
  expect(is.character(stopped) && grepl("finished", stopped, fixed = TRUE),
    "A check without a finished test log must be reported, not passed.")
  absent <- cran_test_budget_problem(character(), budget = 300)
  expect(is.character(absent), "A check directory that is absent must be reported.")

  # The budget's reading of the environment must match tests/testthat.R's.
  cases <- list(character(), c(NOT_CRAN = "false"), c(NOT_CRAN = "true"),
    c(MARGINPLYR_COMPLETE_SUITE = "true"),
    c(NOT_CRAN = "false", MARGINPLYR_COMPLETE_SUITE = "true"))
  for (values in cases) {
    ran_tier <- length(selection(tests_dir, values)$files) < length(every)
    expect(identical(with_environment(values, cran_tier_run()), ran_tier),
      paste0("cran_tier_run() disagrees with tests/testthat.R under: ",
        paste(names(values), values, sep = "=", collapse = " ")))
  }

  # The verifier, as a workflow step runs it against a finished check.
  verify <- function(lines, values) {
    finished(lines)
    with_environment(values, {
      Sys.setenv(MARGINPLYR_CHECK_DIR = check)
      on.exit(Sys.unsetenv("MARGINPLYR_CHECK_DIR"), add = TRUE)
      output <- suppressWarnings(system2(rscript,
        ".github/scripts/verify-cran-test-budget.R", stdout = TRUE, stderr = TRUE))
      status <- attr(output, "status")
      list(status = if (is.null(status)) 0L else status,
        output = paste(output, collapse = "\n"))
    })
  }
  inside <- verify(log("78.962"), c(NOT_CRAN = "false"))
  expect(inside$status == 0L && grepl("79 s", inside$output, fixed = TRUE),
    inside$output)
  outside <- verify(log("1301.250"), c(NOT_CRAN = "false"))
  expect(outside$status != 0L && grepl("1301", outside$output, fixed = TRUE),
    outside$output)
  # setup-r exports NOT_CRAN=true into a job that does not set it; such a job
  # ran the complete suite and must not report the tier's gate green.
  defaulted <- verify(log("78.962"), c(NOT_CRAN = "true"))
  expect(defaulted$status != 0L &&
    grepl("NOT_CRAN: false", defaulted$output, fixed = TRUE), defaulted$output)
  # Only a job that names the complete suite passes unmeasured.
  for (values in list(c(MARGINPLYR_COMPLETE_SUITE = "true"),
                      c(NOT_CRAN = "true", MARGINPLYR_COMPLETE_SUITE = "true"))) {
    named <- verify(log("1301.250"), values)
    expect(named$status == 0L &&
      grepl("does not apply", named$output, fixed = TRUE), named$output)
  }

  cat("CRAN tier fixtures passed.\n")
}
main()
