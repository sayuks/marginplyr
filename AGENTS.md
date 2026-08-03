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
or `rlang::is_installed()` directly, since those cannot be told to fail. A new
optional backend needs both a `skip_if_backend_absent()` call and a `backend`
matrix entry; adding only the first produces a contract nothing executes.

## Agent skills

### Issue tracker

Issues and implementation tickets are tracked in GitHub Issues. See `design/agents/issue-tracker.md`.

### Triage labels

Triage uses the five standard roles: `needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, and `wontfix`. See `design/agents/triage-labels.md`.

### Domain docs

This is a single-context repo. Read the root `CONTEXT.md` and relevant ADRs under `design/adr/`. See `design/agents/domain.md`.
