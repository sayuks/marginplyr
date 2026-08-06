## Local checks

### Linting

Load the package before linting, so the local run matches `.github/workflows/lint.yaml`:

```r
pkgload::load_all(".", quiet = TRUE)
lintr::lint_package()
```

`object_usage_linter()` resolves symbols through the package namespace. Without
`load_all()` it reports every internal marginplyr function as an undefined
global, and installing the package is not enough — it must be loaded.

When a repository needs lint configuration, `.lintr` records it; an inline
`# nolint` records a fact about one expression. Neither substitutes for giving
the linter the information it needs, so a lint that only appears in a blind
environment is a reason to fix the environment, not to suppress it. Every
`# nolint` names the linter it
suppresses, and every one in R code sits next to a comment stating the
expression-specific reason — a glue string or an NSE pronoun that `codetools`
cannot follow, or a name fixed by another package's API.

A `# nolint` on a roxygen (`#'`) line is different: roxygen copies it into the
generated `.Rd` as visible help text, where it also trips
`spelling::spell_check_package()`. The one exception is a markdown table row,
because roxygen discards whatever follows the row's final `|`. Everywhere else,
rewrite the line rather than suppress it — a long URL becomes a reference-style
markdown link, as in `R/marginplyr-package.R` and the `[guide]` links in
`R/grouping-spec.R` and its siblings. The table-row exception rests on roxygen
behaviour that is not documented, so `document.yaml` below is what keeps it
honest: any leak into `man/` fails CI.

### Documentation

`man/` and `NAMESPACE` are generated. After changing roxygen comments run
`roxygen2::roxygenise()` and commit the result;
`.github/workflows/document.yaml` regenerates both and fails when either
differs from what the roxygen comments produce.

### Dependency metadata

`DESCRIPTION`'s Imports/Suggests split is audited by hand, not with
`attachment::att_amend_desc()`. That tool statically scans `R/` for
`pkg::fun()` calls and promotes anything it finds to Imports, but this package
uses several Suggests conditionally — for example `arrow::schema()` in
`R/backend-metadata.R` sits behind a backend-kind guard, so the call site
exists without arrow ever being a hard dependency. A static scanner cannot see
the guard; running the tool promotes arrow to Imports and drops genuinely used
Suggests such as knitr, which would violate the "no package is promoted to
Imports merely to make a check pass" rule the metadata is held to. No `pkg_ignore`
or `extra.suggests` configuration fixes this, because the false positive comes
from what the scanner can express, not from missing configuration.

The manual audit is a grep for `pkg::` and bare-name usage of each Suggested
package across `R/`, `tests/`, and `vignettes/`, checked against DESCRIPTION by
eye when a dependency is added, removed, or moved between Imports and
Suggests. That scan only answers "is this Suggest referenced somewhere" — it
cannot tell whether a Suggest is genuinely optional at runtime. The
`_R_CHECK_DEPENDS_ONLY_=true` check mode is the authority on optionality, not
the manual scan: it rebuilds examples, tests, and vignettes with Suggested
packages absent, which is the only way to confirm code guarded behind a
Suggest actually degrades correctly instead of erroring. Run it locally with
`_R_CHECK_DEPENDS_ONLY_=true R CMD check` against a source tarball. CI runs the
same mode in `release-matrix.yaml`'s `depends-only` job, so this is a gate
rather than a manual step, but running it locally is still the fastest way to
find out which guard is missing.

A Suggest can also be non-optional without being an Import, because an Import
already requires it. `dbplyr` declares `Imports: DBI`, so DBI sits in the hard
dependency closure and is installed under `_R_CHECK_DEPENDS_ONLY_=true` like
any Import; tibble reaches the same place through dplyr. Such a package is
never absent, so a guard written against it never fires and
`optional_suggests()` cannot claim it absent — that is why `DBI = FALSE`
there. Check with `packageDescription("<import>")$Imports`, not by reading this
package's DESCRIPTION: the closure is a property of another package's metadata,
so it can change without a commit here. Keeping the Suggests entry is still
right, because it records a direct use that would need declaring if the closure
stopped supplying it.

### Release matrix

`.github/workflows/release-matrix.yaml` checks one built tarball rather than
the working tree, because a check that passes on the development tree can be
passing on a file the tarball does not ship. Its `backend` jobs each install a
single optional backend and set `MARGINPLYR_REQUIRED_SUGGESTS`, which turns
that backend's absence into a test failure instead of a skip.

That variable is what makes skipping safe everywhere else. Optional-backend
tests skip when their package is missing, which is correct for CRAN's minimal
flavors but means a green job proves nothing about the backend. Every such
test therefore goes through `skip_if_backend_absent()` or `backend_available()`
from `tests/testthat/helper-optional-backends.R` — never `skip_if_not_installed()`
or `rlang::is_installed()` directly, since those cannot be told to fail.
(`skip_if_not_installed("dbplyr")` is not an exception: dbplyr is an Import, so
it is never absent.)

Snapshot expectations run only where `NOT_CRAN` is set: testthat skips them
under CRAN semantics, so a snapshot never fails in a job that emulates CRAN.
That is the `backend` jobs, which set it in the workflow, and the `structure`
job, whose script sets it so a local run matches CI. `structure` takes no
`needs`, so in practice it is the first job to report a stale snapshot — but it
runs against the working tree, and a `backend` job is still what proves the
snapshot inside `R CMD check`.

An installed package still does not prove its tests ran, and the answer is
structural rather than a list of test names (#93). One policy carries it:

> No test may require more than one member of `optional_backends()`.

While that holds, the `backend` jobs cover the whole suite by construction —
each installs one backend and withholds the rest, so between them they execute
every test that requires at most one. A test requiring two is executed by none
of them and skips in all of them. Splitting such a test is the fix, and the
idiom that makes it free is in `test-margin-order.R`: compare each backend
against the **local** result, which needs no optional backend, so a backend
cannot pass by being self-consistently wrong the way two agreeing backends can.

Two gates hold the policy up, and neither is a list:

- `verify-suite-coverage.R`, run by the `structure` job, runs the whole suite
  once per optional backend with the others hidden through
  `MARGINPLYR_HIDE_SUGGESTS`, and fails naming any test that executed in no
  configuration. It asserts its own mechanism before concluding anything, since
  a simulation that stopped working would report that every test runs
  everywhere. Run it locally with
  `Rscript .github/scripts/verify-suite-coverage.R`; it gives the same verdict
  as CI.
- `verify-backend.R`, inside each `backend` job, reads that job's own testthat
  log and fails unless the suite started, passed something, failed nothing, and
  skipped nothing for a reason other than a backend the job withheld. A stray
  `skip_if()`, a `skip_on_os()`, or `NOT_CRAN` being dropped so snapshots skip
  all fail the job now; none of them did under the old named-test gate.

Which packages a job installs is the whole signal of that design, so the
dependency cache is part of it. `setup-r-dependencies@v2` falls back to a
`restore-keys` prefix of `<os>-<R version>-<arch>-<cache-version>-`, which
means every job sharing a `cache-version` shares a library. Each dependency
request therefore gets its own value — `full-1`, `hard-1`,
`backend-<name>-1`, named in `release-matrix.yaml`'s header — and a new job
that copies an existing `cache-version` inherits that job's library rather than
installing its own. `verify-library-isolation.R` is what keeps the scheme
honest, since a wrong cache key is otherwise indistinguishable from a correct
run: it fails the job when an optional backend the job did not declare in
`MARGINPLYR_REQUIRED_SUGGESTS` is on `.libPaths()`. `check-tarball.R` sources
it rather than the workflow calling it as a step, so the assertion cannot be
dropped from a job that still checks a tarball.

Adding an optional backend means editing two places. Every partial edit fails
loudly:

1. `optional_backend_spec()` in `tests/testthat/helper-optional-backends.R`,
   the one table every other consumer derives from — `optional_suggests()` for
   its `asserted` column, `optional_backends()` for the subset a job can be
   asked to withhold, and `verify-depends-only.R`,
   `verify-library-isolation.R`, `verify-suite-coverage.R`, and
   `generate-backend-matrix.R` through those. An `asserted = TRUE` entry alone
   fails `depends-only`, where it makes `verify-depends-only.R` require a
   `{<package>} is not installed` line that no test would produce. The
   `backend` job the entry generates does not fail — it installs the package,
   runs a suite that never mentions it, and finds nothing to complain about,
   which is why `depends-only` is the gate named here. An `asserted = FALSE`
   entry claims no absence
   and gets no job, so nothing asserts it — which is the DBI case above, and
   the reason that value exists. `companions` names what the generated job
   installs alongside the backend, which is how `DBI` reaches the driver jobs
   without a job of its own.
2. the `skip_if_backend_absent()` or `backend_available()` call in the tests.
   Doing only this errors immediately: those helpers refuse a package
   `optional_suggests()` does not name, since nothing would execute it and
   nothing would assert it absent.

There is no third place, and that is the point (#93). The `backend` matrix used
to be four hand-written entries, so a backend could be tracked by the tests and
executed by no job — `verify-matrix-coverage.R` and the `coverage` job existed
only to catch that (#71), and both are gone. `generate-backend-matrix.R` builds
the matrix from the table instead, deriving each job's `required` list,
installed packages, and `cache-version`, and writes what it produced to the step
summary because `release-matrix.yaml` no longer shows its own jobs. Generating
one job body also closed #73, which asked for an assertion that every job
withholding optional backends checks a tarball: with a single body there is no
second shape for such a job to have.

## Agent skills

### Issue tracker

Issues and implementation tickets are tracked in GitHub Issues. See `design/agents/issue-tracker.md`.

### Triage labels

Triage uses the five standard roles: `needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, and `wontfix`. See `design/agents/triage-labels.md`.

### Domain docs

This is a single-context repo. Read the root `CONTEXT.md` and relevant ADRs under `design/adr/`. See `design/agents/domain.md`.

### Investigation notes

`investigation/` holds dated research notes. A note records what was
established on its date and is not maintained afterwards; a later investigation
adds a dated revisions section rather than rewriting it. Authority splits by
subject, not by recency: the note is authoritative for the evidence — sources
read, what they said, what was measured — while a workflow comment, ADR,
`AGENTS.md`, or the code is authoritative for the decision and for current
state. A note therefore never describes the present, and once an artifact cites
a note as a reason, the durable part belongs in `AGENTS.md` or an ADR. See
`investigation/README.md` for the header block, the supersession form, and the
grep to run before amending a note.
