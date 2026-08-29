# Confirms that the `backend` jobs cover the whole test suite by construction.
#
# Each of those jobs installs one optional backend and withholds the rest, so
# between them they execute every test that requires at most one backend. A test
# that requires two is executed by none of them: it skips in every job, and
# every job still reports success. Nothing about a green workflow says which
# case a test is in. It skips in a job that happens to hold both as well,
# because this script hides all but one whatever the run's library holds, which
# is why the guarantee does not rest on a job's package count.
#
# `release-matrix.yaml` used to answer that by naming contract tests in a
# `proves` list per job. Naming catches a deleted test, which is also the most
# visible change a diff can carry, and misses the invisible one -- a guard added
# to a test body makes it skip in every isolated job, and the diff shows one
# added line. A list also cannot survive AI authorship: a gate whose remedy is
# "regenerate the list and commit it" records a change rather than refusing one.
# So this script makes naming unnecessary instead of making it better (#93).
#
# The policy it enforces is one sentence, whose home is `AGENTS.md`'s *Release
# matrix* section:
#
#     No test may require more than one member of `optional_backends()`.
#
# While that holds, the isolated jobs cover the suite and nothing has to be
# listed or kept in step. `test-margin-order.R` already states the rule for its
# own section and gives the idiom that makes it free: compare each backend
# against the *local* result, which needs no optional backend, so a backend
# cannot pass by being self-consistently wrong the way two backends agreeing
# with each other can.
#
# The measurement is a simulation rather than an aggregate over those jobs'
# artifacts, for one reason: an agent can run this before pushing. A guard is
# the only thing that decides whether a test skips, and the hook drives the
# guards, so simulating absence answers the same structural question the real
# jobs would. What it cannot answer -- whether a backend works when the others
# are genuinely gone -- is what the `backend` jobs are for.
#
# A simulation that stopped working would pass vacuously: nothing would skip and
# every test would appear to run everywhere. So the mechanism is asserted before
# any conclusion is drawn, both halves of it -- that every optional backend is
# genuinely installed, and that in each configuration the withheld packages
# report unavailable while the selected one reports available.
#
# Run it locally with:
#
#     Rscript .github/scripts/verify-suite-coverage.R
#
# This is the one script here that reads the working tree rather than a built
# tarball, which is why it takes no `needs` in the workflow and downloads
# nothing. The structural question is about the tests as written; a tarball
# would add a build step without changing the answer.

source(".github/scripts/ci-helpers.R")

# testthat skips snapshot expectations under CRAN semantics. Without this the
# five snapshot tests skip in every configuration and are reported as requiring
# two backends, which is a false positive produced entirely by this script.
# Set here rather than in the workflow so a local run gives the same verdict.
Sys.setenv(NOT_CRAN = "true")

backends <- optional_backends()
if (length(backends) == 0L) {
  stop(call. = FALSE, "`optional_backends()` is empty, so this proves nothing.")
}

# Half one of the mechanism: this run's library holds every backend, at a
# version DESCRIPTION accepts. Asked of the library rather than through
# `suggest_available()`, because the guards are the thing under test here --
# `suggest_status()` reads DESCRIPTION and the installed version and knows
# nothing of `MARGINPLYR_HIDE_SUGGESTS`, which is what the simulation drives.
#
# The version half matters as much as the presence half. A backend installed
# below its constraint now reports unavailable, so it would skip in every
# configuration and be reported below as requiring two backends -- a violation
# invented entirely by the environment this ran in (#123).
status <- lapply(backends, suggest_status)
unusable <- Filter(function(one) !one$available, status)
if (length(unusable) > 0L) {
  stop(call. = FALSE, sprintf(
    paste0(
      "This job simulates each backend's absence, so it needs all of them ",
      "installed at the version DESCRIPTION requires. These are not: %s."
    ),
    paste(
      vapply(unusable, function(one) one$reason, character(1)),
      collapse = "; "
    )
  ))
}

skip_messages <- function(expectations) {
  skips <- Filter(
    function(expectation) inherits(expectation, "expectation_skip"),
    expectations
  )
  vapply(skips, conditionMessage, character(1))
}

# Skips read `Reason: {arrow} is not installed`, the wording
# `skip_if_suggest_absent()` writes. Parsing it is what lets a violation name
# the backends it requires rather than only the test.
skipped_packages <- function(messages) {
  named <- regmatches(messages, regexpr("\\{[^}]+\\}", messages))
  unique(gsub("[{}]", "", named))
}

# Runs the whole suite with every backend but `selected` hidden, and reports one
# row per test. "Executed" means the test ran to its end and asserted something:
# a test that passes three expectations and then skips has not executed, and
# counting it as executed is precisely the mistake that would hide a test whose
# second half never runs.
observe <- function(selected) {
  withheld <- setdiff(backends, selected)
  Sys.setenv(MARGINPLYR_HIDE_SUGGESTS = paste(withheld, collapse = ","))
  on.exit(Sys.unsetenv("MARGINPLYR_HIDE_SUGGESTS"), add = TRUE)

  # Half two of the mechanism, asserted inside the configuration it describes.
  reported <- vapply(backends, suggest_available, logical(1))
  if (!isTRUE(reported[[selected]]) || any(reported[withheld])) {
    stop(call. = FALSE, sprintf(
      paste0(
        "The absence simulation is not working: with %s selected and %s ",
        "hidden, `suggest_available()` reports %s. Every conclusion below ",
        "would be vacuous."
      ),
      selected,
      paste(withheld, collapse = ", "),
      paste(sprintf("%s=%s", backends, reported), collapse = ", ")
    ))
  }

  results <- testthat::test_local(
    ".",
    reporter = "silent",
    stop_on_failure = FALSE
  )

  lapply(results, function(entry) {
    outcomes <- vapply(
      entry$results,
      function(expectation) class(expectation)[[1]],
      character(1)
    )
    broken <- any(
      outcomes %in% c("expectation_failure", "expectation_error")
    )
    skipped <- any(outcomes == "expectation_skip")
    messages <- skip_messages(entry$results)
    list(
      # File and test together, because two files may name a test the same way
      # and reporting one violation for both would name the wrong one.
      key = sprintf("%s::%s", entry$file, entry$test),
      executed = !skipped && !broken && any(outcomes == "expectation_success"),
      broken = broken,
      requires = skipped_packages(messages),
      # Every skip reason this configuration produced, so the same attribution
      # `verify-backend.R` makes from a `backend` job's log can be made here.
      # Without it the two gates disagree: a test can execute in some
      # configuration -- so this script passes it -- while skipping elsewhere
      # for a reason no withheld backend explains, which reds a `backend` job.
      # `test-share-backends.R` had two such tests, and only CI could say so.
      skips = messages
    )
  })
}

observations <- unlist(
  lapply(backends, observe),
  recursive = FALSE,
  use.names = FALSE
)

# The wording `skip_if_suggest_absent()` writes for a backend this run hid.
# Anything else is a skip a `backend` job could not attribute either.
attributable <- sprintf("Reason: {%s} is not installed", backends)

keys <- vapply(observations, function(row) row$key, character(1))
executed <- vapply(observations, function(row) row$executed, logical(1))
broken <- vapply(observations, function(row) row$broken, logical(1))

tests <- unique(keys)
runs <- vapply(tests, function(key) sum(executed[keys == key]), integer(1))

# The backends a test requires: every package it was seen to skip on, across the
# configurations. A test needing dtplyr and Arrow skips on Arrow in the dtplyr
# configuration and on dtplyr in the Arrow one, so the union is exactly the set
# no single job can satisfy.
requirements <- function(key) {
  named <- unlist(lapply(observations[keys == key], function(row) row$requires))
  unique(named[named %in% backends])
}

describe_requirements <- function(key) {
  needed <- requirements(key)
  if (length(needed) == 0L) {
    # Reachable when a test asserts nothing rather than when it skips, so the
    # wording says which of the two happened instead of naming no backend.
    return("asserted nothing in any configuration")
  }
  sprintf("requires %s", paste(needed, collapse = " and "))
}

write_step_summary(c(
  "## Suite coverage by construction",
  "",
  sprintf(
    "%d tests over %d single-backend configurations (%s).",
    length(tests),
    length(backends),
    paste(backends, collapse = ", ")
  ),
  "",
  vapply(
    0:length(backends),
    function(n) {
      sprintf("- %d test(s) executed in %d configuration(s)", sum(runs == n), n)
    },
    character(1)
  )
))

problems <- character()

uncovered <- tests[runs == 0L]
if (length(uncovered) > 0L) {
  # The remedy is part of the message on purpose. A gate that only refuses
  # invites the guard to be deleted instead of the test being split.
  problems <- c(problems, sprintf(
    paste0(
      "These tests execute in no single-backend configuration, so no ",
      "`backend` job runs them and their contracts are proven nowhere: %s. ",
      "Split each one so it requires at most one optional backend, and ",
      "compare that backend against the local result for the same input, as ",
      "`expect_margin_order_agrees()` in `test-margin-order.R` does. A local ",
      "reference is the stronger one: a backend cannot pass it by being ",
      "self-consistently wrong."
    ),
    paste(
      sprintf(
        "`%s` (%s)",
        uncovered,
        vapply(uncovered, describe_requirements, character(1))
      ),
      collapse = "; "
    )
  ))
}

# A skip no hidden backend explains fails the `backend` job that meets it, and
# this is the only place that verdict can be had before pushing. Reported as its
# own problem rather than folded into the coverage count, because the remedy
# differs: the test does run somewhere, and what has to change is the guard.
unattributable <- unique(unlist(lapply(
  observations,
  function(row) setdiff(row$skips, attributable)
)))
if (length(unattributable) > 0L) {
  problems <- c(problems, sprintf(
    paste0(
      "These skip reasons name no optional backend, so a `backend` job ",
      "meeting one cannot attribute it to a package it withheld and fails: ",
      "%s. Guard the test with `skip_if_suggest_absent()` on the one backend ",
      "it needs, splitting it first if it selects among several."
    ),
    paste(sprintf("\"%s\"", unattributable), collapse = "; ")
  ))
}

# A failing test is not a structural finding, but swallowing it here would leave
# this script reporting a healthy suite while one configuration is red.
failing <- unique(keys[broken])
if (length(failing) > 0L) {
  problems <- c(problems, sprintf(
    "These tests failed in at least one single-backend configuration: %s.",
    paste(sprintf("`%s`", failing), collapse = "; ")
  ))
}

if (length(problems) > 0L) {
  stop(call. = FALSE, paste(problems, collapse = " "))
}

message(sprintf(
  "Verified: all %d tests execute in at least one of %s.",
  length(tests),
  paste(backends, collapse = ", ")
))
