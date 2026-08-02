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

`.lintr` records repo-wide configuration and an inline `# nolint` records a fact
about one expression. Neither substitutes for giving the linter the information
it needs, so a lint that only appears in a blind environment is a reason to fix
the environment, not to suppress it. Every `# nolint` in `R/` names the linter
it suppresses and sits next to a comment stating the expression-specific reason
(a glue string or an NSE pronoun that `codetools` cannot follow).

Never put a `# nolint` directive on a roxygen (`#'`) line: roxygen copies it
into the generated `.Rd` as visible help text. Rewrite the line instead — long
URLs become reference-style markdown links, as in `R/marginplyr-package.R`.

### Documentation

`man/` is generated. After changing roxygen comments run `roxygen2::roxygenise()`
and commit the result; `.github/workflows/document.yaml` fails on any drift.

## Agent skills

### Issue tracker

Issues and implementation tickets are tracked in GitHub Issues. See `design/agents/issue-tracker.md`.

### Triage labels

Triage uses the five standard roles: `needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, and `wontfix`. See `design/agents/triage-labels.md`.

### Domain docs

This is a single-context repo. Read the root `CONTEXT.md` and relevant ADRs under `design/adr/`. See `design/agents/domain.md`.
