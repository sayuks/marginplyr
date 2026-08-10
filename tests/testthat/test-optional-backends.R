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

# The same for the absence-simulation hook. `verify-suite-coverage.R` sets
# `MARGINPLYR_HIDE_SUGGESTS` for the whole suite, so a test that unset it would
# hand the rest of its configuration a library it was not meant to see.
with_hidden_suggests <- function(value, code) {
  previous <- Sys.getenv("MARGINPLYR_HIDE_SUGGESTS", unset = NA)
  on.exit(
    if (is.na(previous)) {
      Sys.unsetenv("MARGINPLYR_HIDE_SUGGESTS")
    } else {
      Sys.setenv(MARGINPLYR_HIDE_SUGGESTS = previous)
    },
    add = TRUE
  )
  Sys.setenv(MARGINPLYR_HIDE_SUGGESTS = value)
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

# The `Suggests` field each injected table is paired with. `suggests` has to
# travel with `known`, because neither name is in DESCRIPTION and the guard
# refuses a package DESCRIPTION does not suggest -- that refusal is what keeps a
# vignette or an example from guarding on a name whose constraint could never be
# read. A pretend table needs a pretend field for the same reason it needs a
# pretend `known`.
suggests_absent <- absent_package
suggests_installed <- "stats"
# What a constrained Suggest whose installed version is too old looks like. No
# real constraint could produce this case in an environment where the suite
# passes, so it is supplied rather than found.
suggests_too_old <- "stats (>= 999.0.0)"

test_that("required suggests are parsed from the environment variable", {
  with_required_suggests("", expect_identical(required_suggests(), character()))
  with_required_suggests(
    " duckdb , DBI ,,",
    expect_identical(required_suggests(), c("duckdb", "DBI"))
  )
})

test_that("an absent backend is skippable when no job promised it", {
  with_required_suggests("", {
    expect_false(backend_available(
      absent_package,
      known = known_absent,
      suggests = suggests_absent
    ))
    # Caught rather than asserted with `expect_error()`, because testthat lets
    # a skip condition escape an expectation and skip the whole test instead.
    skipped <- tryCatch(
      {
        skip_if_backend_absent(
          absent_package,
          known = known_absent,
          suggests = suggests_absent
        )
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
      backend_available(
        absent_package,
        known = known_absent,
        suggests = suggests_absent
      ),
      "MARGINPLYR_REQUIRED_SUGGESTS"
    )
    expect_error(
      skip_if_backend_absent(
        absent_package,
        known = known_absent,
        suggests = suggests_absent
      ),
      "MARGINPLYR_REQUIRED_SUGGESTS"
    )
  })
})

test_that("promising one backend does not make an unrelated one required", {
  with_required_suggests("duckdb", expect_false(backend_available(
    absent_package,
    known = known_absent,
    suggests = suggests_absent
  )))
})

test_that("an installed backend is available whether or not it is required", {
  with_required_suggests(
    "",
    expect_true(backend_available(
      "stats",
      known = known_installed,
      suggests = suggests_installed
    ))
  )
  with_required_suggests("stats", {
    expect_true(backend_available(
      "stats",
      known = known_installed,
      suggests = suggests_installed
    ))
    expect_no_error(skip_if_backend_absent(
      "stats",
      known = known_installed,
      suggests = suggests_installed
    ))
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

test_that("hidden suggests are parsed from the environment variable", {
  with_hidden_suggests("", expect_identical(hidden_suggests(), character()))
  with_hidden_suggests(
    " arrow , dtplyr ,,",
    expect_identical(hidden_suggests(), c("arrow", "dtplyr"))
  )
})

test_that("a hidden backend reports absent even though it is installed", {
  # The whole of `verify-suite-coverage.R` rests on this one substitution: an
  # installed package has to answer the guards the way an absent one would, or
  # the simulation reports that every test runs in every configuration.
  with_required_suggests("", {
    with_hidden_suggests("stats", {
      expect_false(backend_available(
        "stats",
        known = known_installed,
        suggests = suggests_installed
      ))
      skipped <- tryCatch(
        {
          skip_if_backend_absent(
            "stats",
            known = known_installed,
            suggests = suggests_installed
          )
          NULL
        },
        skip = function(condition) condition
      )
      expect_s3_class(skipped, "skip")
    })
  })
})

test_that("hiding a backend a job promised to prove is refused", {
  # Nothing structural stops the two variables from naming the same package,
  # and if they did the hook would turn a `backend` job's proof into a skip and
  # the job would pass. This is the refusal that makes that impossible.
  with_required_suggests("stats", {
    with_hidden_suggests("stats", {
      expect_error(
        backend_available(
          "stats",
          known = known_installed,
          suggests = suggests_installed
        ),
        "MARGINPLYR_HIDE_SUGGESTS"
      )
      expect_error(
        skip_if_backend_absent(
          "stats",
          known = known_installed,
          suggests = suggests_installed
        ),
        "MARGINPLYR_REQUIRED_SUGGESTS"
      )
    })
  })
})

test_that("hiding one backend leaves an unrelated promise alone", {
  # Refusal is per queried package rather than per variable. Refusing whenever
  # the two lists intersected at all would fail every test in this file under
  # `verify-suite-coverage.R`, which hides real backends while these tests
  # promise sentinel ones.
  with_required_suggests("stats", {
    with_hidden_suggests("arrow", {
      expect_true(backend_available(
        "stats",
        known = known_installed,
        suggests = suggests_installed
      ))
      expect_false(backend_available(
        absent_package,
        known = known_absent,
        suggests = suggests_absent
      ))
    })
  })
})

test_that("a backend job installs its backend and its companions", {
  # `generate-backend-matrix.R` builds each job's `required` list and its
  # `extra-packages` from this, so a driver backend that arrived without DBI
  # would install, run nothing, and report green.
  expect_identical(backend_job_packages("duckdb"), c("duckdb", "DBI"))
  expect_identical(backend_job_packages("RSQLite"), c("RSQLite", "DBI"))
  expect_identical(backend_job_packages("dtplyr"), "dtplyr")
  expect_error(backend_job_packages(absent_package), "optional_backend_spec")
})

test_that("every companion is itself a tracked Suggest", {
  # A companion outside the table would be installed by a `backend` job and
  # refused by `verify-library-isolation.R`, which reads the same `required`
  # list and knows only what the table names.
  companions <- unlist(lapply(
    optional_backend_spec(),
    function(entry) entry$companions
  ))
  expect_true(all(companions %in% names(optional_suggests())))
})

test_that("optional_backends() is the subset a job can be asked to withhold", {
  backends <- optional_backends()
  # `verify-depends-only.R` iterates over this to require one skip line per
  # backend, and `verify-library-isolation.R` to decide what must be absent. A
  # derivation that returned nothing would leave both asserting nothing while
  # still reporting success, so emptiness is the failure worth naming.
  expect_type(backends, "character")
  expect_gt(length(backends), 0L)
  expect_null(names(backends))
  # Approached from the other side than the derivation, so an inverted or
  # ignored flag fails here rather than restating the function body.
  untrackable <- names(optional_suggests())[!optional_suggests()]
  expect_false(any(untrackable %in% backends))
  expect_true(all(nzchar(names(optional_suggests()))))
})

test_that("every tracked Suggest is declared in DESCRIPTION", {
  # A typo in `optional_suggests()` would make `backend_available()` refuse the
  # real backend at every guard, which reads as a registration error rather
  # than as the typo it is.
  #
  # Read through the shipped guard's own reading of the field rather than a
  # second parse written here, so this test fails when that reading stops
  # understanding DESCRIPTION instead of quietly agreeing with a broken one.
  declared <- suggest_guard()$marginplyr_suggest_names(declared_suggests())
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

# A guard that reports a backend usable now promises the version DESCRIPTION
# requires, not only that the package is installed (#123). The two claims came
# apart under `duckdb (>= 1.5.5)`, where the older `duckdb()` rejects the
# `shared_home` argument outright: `requireNamespace()` answered TRUE, the
# guarded code ran, and it failed at the feature instead of skipping.
#
# `suggests` is the seam these tests drive, through `suggests_too_old` above.
test_that("a version constraint is read out of the Suggests field", {
  requirement <- suggest_guard()$marginplyr_suggest_requirement
  expect_null(requirement("DBI", suggests = "arrow (>= 1.0), DBI, tibble"))

  read <- requirement("arrow", suggests = "arrow (>= 13.0.0), DBI")
  expect_identical(read$text, ">= 13.0.0")
  expect_identical(read$comparisons[[1]]$operator, ">=")
  expect_identical(read$comparisons[[1]]$version, package_version("13.0.0"))
})

test_that("an entry survives the wrapping a DESCRIPTION field arrives with", {
  # `read.dcf()` hands back the field with its newlines and indentation intact,
  # so an entry can carry whitespace inside it as readily as around it.
  requirement <- suggest_guard()$marginplyr_suggest_requirement
  wrapped <- "\n    duckdb (>=\n    1.5.5),\n    knitr\n"
  expect_identical(requirement("duckdb", suggests = wrapped)$text, ">= 1.5.5")
  expect_null(requirement("knitr", suggests = wrapped))
})

test_that("a two-sided constraint is split on entries, not on every comma", {
  # `pkg (>= 1.0, < 2.0)` is one legal entry whose constraint contains a comma.
  # Splitting the field on every comma would make it two entries, neither of
  # which parses, so this is the case a naive split turns into a hard error.
  guard <- suggest_guard()
  field <- "pkg (>= 1.0, < 2.0), other"
  expect_identical(
    guard$marginplyr_suggest_entries(field),
    c("pkg (>= 1.0, < 2.0)", "other")
  )
  read <- guard$marginplyr_suggest_requirement("pkg", suggests = field)
  expect_length(read$comparisons, 2L)
  expect_identical(
    vapply(read$comparisons, function(one) one$operator, character(1)),
    c(">=", "<")
  )
})

test_that("an unreadable constraint halts rather than being passed over", {
  # A constraint this cannot read is one that silently stopped being honored,
  # which is the whole failure the guard exists to prevent.
  guard <- suggest_guard()
  expect_error(
    guard$marginplyr_suggest_requirement("pkg", suggests = "pkg >= 1.0"),
    "not readable as"
  )
  expect_error(
    guard$marginplyr_suggest_requirement("pkg", suggests = "pkg (1.0)"),
    "constraint on \\{pkg\\} is not readable"
  )
})

test_that("a package DESCRIPTION does not suggest is refused, not answered", {
  # Returning "no constraint" for an unlisted name would answer the
  # version-blind question at exactly the call sites with no other registry: a
  # vignette or an example naming a typo, or a Suggest that moved to
  # `Config/Needs/website`, would guard on installation alone and read as
  # protection. `backend_available()` refuses an unregistered backend for the
  # same reason, but nothing outside `tests/` reaches that refusal.
  guard <- suggest_guard()
  expect_error(
    guard$marginplyr_suggest_available("dukcdb", suggests = "duckdb (>= 1.0)"),
    "not a Suggested package of marginplyr"
  )
  # Named in DESCRIPTION without a constraint is a different answer from named
  # nowhere, and the two must not collapse into each other.
  expect_true(guard$marginplyr_suggest_available(
    "stats",
    suggests = suggests_installed
  ))
})

test_that("an installed backend below its constraint is not available", {
  with_required_suggests("", {
    expect_true(backend_available(
      "stats",
      known = known_installed,
      suggests = suggests_installed
    ))
    expect_false(backend_available(
      "stats",
      known = known_installed,
      suggests = suggests_too_old
    ))
  })
})

test_that("a too-old backend skips with a reason that is not \"absent\"", {
  # Reported as absent, this skip would send a reader looking for a package
  # sitting in their library. It also has to stay distinguishable to
  # `verify-backend.R`, which attributes a skip by matching the absent wording.
  with_required_suggests("", {
    skipped <- tryCatch(
      {
        skip_if_backend_absent(
          "stats",
          known = known_installed,
          suggests = suggests_too_old
        )
        NULL
      },
      skip = function(condition) condition
    )
    expect_s3_class(skipped, "skip")
    expect_no_match(conditionMessage(skipped), "is not installed")
    expect_match(conditionMessage(skipped), "marginplyr requires >= 999.0.0")
    expect_match(conditionMessage(skipped), "\\{stats\\}")
  })
})

test_that("a too-old backend fails when the job promised to prove it", {
  # A `backend` job installs the version DESCRIPTION asks for, so a job holding
  # an older one has not proved its contract and must not report that it did.
  with_required_suggests("stats", {
    expect_error(
      backend_available(
        "stats",
        known = known_installed,
        suggests = suggests_too_old
      ),
      "MARGINPLYR_REQUIRED_SUGGESTS"
    )
    expect_error(
      backend_available(
        "stats",
        known = known_installed,
        suggests = suggests_too_old
      ),
      "marginplyr requires"
    )
  })
})

test_that("a hidden backend reports absent whatever its version says", {
  # `MARGINPLYR_HIDE_SUGGESTS` claims a package is gone, and both
  # `verify-suite-coverage.R` and `verify-depends-only.R` attribute the skip it
  # produces by matching the absent wording. A simulated absence that announced
  # a version instead would be a skip neither could attribute.
  with_required_suggests("", {
    with_hidden_suggests("stats", {
      expect_identical(
        backend_absence_reason("stats", suggests = suggests_too_old),
        "{stats} is not installed"
      )
    })
  })
})

test_that("the constraints the guard honors are the ones DESCRIPTION states", {
  # The mechanism before the conclusion: every test above supplies its own
  # `suggests`, so all of them would pass against a DESCRIPTION the guard reads
  # nothing out of. This is the one that reads the real field, and it fails if
  # a tracked backend's constraint stops being found.
  requirement <- suggest_guard()$marginplyr_suggest_requirement
  found <- Filter(
    Negate(is.null),
    lapply(names(optional_suggests()), requirement)
  )
  expect_gt(length(found), 0L)
  expect_identical(
    requirement("duckdb")$comparisons[[1]]$operator,
    ">="
  )
  # Every tracked backend present in the checking environment satisfies what
  # DESCRIPTION asks of it, which is what makes a skip elsewhere in the suite
  # attributable to absence rather than to this run's own library.
  for (package in names(optional_suggests())) {
    status <- suggest_status(package)
    expect_true(status$available || !status$installed)
  }
})

test_that("the guard the tests read is the guard the vignettes source", {
  # The vignettes and the `summarize_with_margins()` example cannot reach
  # `tests/`, so they reach `inst/suggests/guard.R` through `system.file()`.
  # These helpers source the same file, which is what makes one reading of
  # DESCRIPTION serve all of them; a second copy here is exactly the drift
  # `AGENTS.md` keeps `optional_backend_spec()` single to prevent.
  expect_true(is.function(suggest_guard()$marginplyr_suggest_available))
  installed <- system.file("suggests", "guard.R", package = "marginplyr")
  expect_true(nzchar(installed))

  # These helpers prefer the repository copies of the guard and of DESCRIPTION,
  # so that a working tree is tested against its own changes. That preference is
  # also how the two can differ: a stale install would leave the vignettes and
  # examples enforcing floors the tests no longer do, and every assertion here
  # would still pass. Comparing them is what makes the preference safe rather
  # than merely convenient. Under `R CMD check` there is no repository copy to
  # compare against and the installed package is the only reading, which is the
  # package the tarball built.
  repository <- repository_file(file.path("inst", "suggests", "guard.R"))
  if (!is.na(repository)) {
    expect_identical(
      readLines(repository, warn = FALSE),
      readLines(installed, warn = FALSE)
    )
  }
  repository_description <- repository_file("DESCRIPTION")
  if (!is.na(repository_description)) {
    expect_identical(
      declared_suggests(),
      unname(read.dcf(
        system.file("DESCRIPTION", package = "marginplyr"),
        fields = "Suggests"
      )[1L, 1L])
    )
  }
})
