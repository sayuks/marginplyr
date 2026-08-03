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

test_that("required suggests are parsed from the environment variable", {
  with_required_suggests("", expect_identical(required_suggests(), character()))
  with_required_suggests(
    " duckdb , DBI ,,",
    expect_identical(required_suggests(), c("duckdb", "DBI"))
  )
})

test_that("an absent backend is skippable when no job promised it", {
  with_required_suggests("", {
    expect_false(backend_available(absent_package))
    # Caught rather than asserted with `expect_error()`, because testthat lets
    # a skip condition escape an expectation and skip the whole test instead.
    skipped <- tryCatch(
      {
        skip_if_backend_absent(absent_package)
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
      backend_available(absent_package),
      "MARGINPLYR_REQUIRED_SUGGESTS"
    )
    expect_error(
      skip_if_backend_absent(absent_package),
      "MARGINPLYR_REQUIRED_SUGGESTS"
    )
  })
})

test_that("promising one backend does not make an unrelated one required", {
  with_required_suggests("duckdb", expect_false(backend_available(
    absent_package
  )))
})

test_that("an installed backend is available whether or not it is required", {
  with_required_suggests("", expect_true(backend_available("stats")))
  with_required_suggests("stats", {
    expect_true(backend_available("stats"))
    expect_no_error(skip_if_backend_absent("stats"))
  })
})
