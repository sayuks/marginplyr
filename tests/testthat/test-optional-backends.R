# The release matrix relies on these helpers to tell a proved backend contract
# apart from a silently skipped one, so the helpers themselves need coverage:
# a regression here would be invisible in every other test file.

# Runs `code` with `MARGINPLYR_REQUIRED_SUGGESTS` set, restoring whatever the
# checking environment had before. Written with `on.exit()` rather than withr
# so the tests add no dependency beyond the ones DESCRIPTION declares.
with_required_suggests <- function(value, code) {
  previous <- Sys.getenv("MARGINPLYR_REQUIRED_SUGGESTS", unset = NA)
  on.exit(
    if (is.na(previous)) {
      Sys.unsetenv("MARGINPLYR_REQUIRED_SUGGESTS")
    } else {
      Sys.setenv(MARGINPLYR_REQUIRED_SUGGESTS = previous)
    },
    add = TRUE
  )
  Sys.setenv(MARGINPLYR_REQUIRED_SUGGESTS = value)
  force(code)
}

# A name no CRAN package uses, so it is reliably absent from every checking
# environment.
absent_package <- "marginplyrNoSuchBackend"

# The `known` tables the tests below inject. Neither name belongs in
# `optional_suggests()` -- the sentinel is not a package, and `stats` is a base
# package rather than an optional backend -- but between them they make both
# sides of `backend_available()` reachable without depending on which optional
# backends the checking environment happens to have.
known_absent <- stats::setNames(TRUE, absent_package)
known_installed <- c(stats = TRUE)

test_that("required suggests are parsed from the environment variable", {
  with_required_suggests("", expect_identical(required_suggests(), character()))
  with_required_suggests(
    " duckdb , DBI ,,",
    expect_identical(required_suggests(), c("duckdb", "DBI"))
  )
})

test_that("an absent backend is skippable when no job promised it", {
  with_required_suggests("", {
    expect_false(backend_available(absent_package, known = known_absent))
    # Caught rather than asserted with `expect_error()`, because testthat lets
    # a skip condition escape an expectation and skip the whole test instead.
    skipped <- tryCatch(
      {
        skip_if_backend_absent(absent_package, known = known_absent)
        NULL
      },
      skip = function(condition) condition
    )
    expect_s3_class(skipped, "skip")
    expect_match(conditionMessage(skipped), "is not installed")
  })
})

test_that("an absent backend fails when the job promised to prove it", {
  with_required_suggests(absent_package, {
    expect_error(
      backend_available(absent_package, known = known_absent),
      "MARGINPLYR_REQUIRED_SUGGESTS"
    )
    expect_error(
      skip_if_backend_absent(absent_package, known = known_absent),
      "MARGINPLYR_REQUIRED_SUGGESTS"
    )
  })
})

test_that("promising one backend does not make an unrelated one required", {
  with_required_suggests("duckdb", expect_false(backend_available(
    absent_package,
    known = known_absent
  )))
})

test_that("an installed backend is available whether or not it is required", {
  with_required_suggests(
    "",
    expect_true(backend_available("stats", known = known_installed))
  )
  with_required_suggests("stats", {
    expect_true(backend_available("stats", known = known_installed))
    expect_no_error(skip_if_backend_absent("stats", known = known_installed))
  })
})

test_that("a package outside the tracked list is refused rather than skipped", {
  with_required_suggests("", {
    expect_error(backend_available(absent_package), "optional_suggests")
    expect_error(skip_if_backend_absent(absent_package), "optional_suggests")
  })
})

test_that("an untracked package is refused even when it is installed", {
  # The refusal runs before `requireNamespace()`. Placed after it, this call
  # would return TRUE, and a guard on an unregistered backend would go
  # unnoticed on every provisioned machine and in `R-CMD-check.yaml`.
  with_required_suggests(
    "",
    expect_error(backend_available("stats"), "optional_suggests")
  )
})

test_that("optional_backends() is the subset a job can be asked to withhold", {
  tracked <- optional_suggests()
  expect_identical(optional_backends(), names(tracked)[tracked])
  expect_true(all(nzchar(names(tracked))))
})

test_that("every tracked Suggest is declared in DESCRIPTION", {
  # A typo in `optional_suggests()` would make `backend_available()` refuse the
  # real backend at every guard, which reads as a registration error rather
  # than as the typo it is.
  declared <- strsplit(packageDescription("marginplyr")$Suggests, ",")[[1]]
  declared <- trimws(sub("\\(.*", "", declared))
  expect_true(all(names(optional_suggests()) %in% declared))
})

test_that("DBI is untrackable because dbplyr puts it in the hard closure", {
  # The reason `optional_suggests()` records for `DBI = FALSE`, asserted rather
  # than only written down. If dbplyr ever drops DBI, DBI becomes genuinely
  # absent under `_R_CHECK_DEPENDS_ONLY_=true` and belongs in the withheld
  # subset, and this test is what says so.
  expect_match(packageDescription("dbplyr")$Imports, "\\bDBI\\b")
  expect_false(optional_suggests()[["DBI"]])
})
