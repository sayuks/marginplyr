# Local review-ready checks

This is the operational authority for the local boundary before a marginplyr
commit is published for review. It defines which commits need the boundary,
what the fixed check proves, and when its evidence expires. Pull-request CI
remains the cross-platform and release-matrix authority.

## Language

A **Package-affecting change** can change the source package, an input that
generates it, installed behavior, or the result of checking it. This includes
package code, tests, examples, vignettes, package data, installed files,
dependency metadata, generated documentation, generation sources, and build
exclusions. A tests-only change is Package-affecting. When the classification
is uncertain, use this one.

A **Repository-only change** can be shown to affect none of those things.
Design and investigation notes, agent instructions, and CI or release tooling
excluded from the package are ordinary examples. Repository-only does not mean
unchecked: run the verifier or generation command owned by the changed
artifact. It means the package boundary below is not additionally required.

The **Review-ready check** is the fixed local gate for one Package-affecting
commit. It is not a name for focused tests or for the wider release process.

## Boundary

Run the Review-ready check before any of these transitions:

- opening a non-draft pull request;
- marking a draft pull request ready or requesting review; or
- pushing a Package-affecting change to a pull request already ready for
  review.

The alternative for the last case is to return the pull request to draft,
push the change, and pass the check before making it ready again. A later
Package-affecting change invalidates the earlier result. A Repository-only
change does not.

The checked state is a clean committed `HEAD`. The command archives that exact
commit into a disposable directory, so ignored local files cannot enter the
evidence. Run it from the repository root:

```sh
Rscript tools/review-ready-check.R
```

The command takes no options. It runs, in order:

1. the full testthat suite, including local snapshot expectations;
2. `jarl check .`;
3. package-aware lintr after `pkgload::load_all()`; and
4. a source-tarball `R CMD check --as-cran` with remote incoming checks
   disabled.

It stops on the first failed step. An ERROR or WARNING fails the package check.
A nonzero or timed-out check process also fails, even when no condition could
be parsed from its incomplete output. Every NOTE is printed. The shared CRAN
NOTE policy identifies an existing classification; every other NOTE needs a
written explanation before the commit is review-ready. The command remains
non-mutating and removes its disposable checkout, tarball, and check directory
when it finishes.

Record the exact identity and outcome in the pull request:

```text
- Review-ready check: <40-character commit SHA>
  - full testthat suite: passed
  - jarl: passed
  - package-aware lintr: passed
  - source-tarball R CMD check: <errors/warnings/notes and NOTE dispositions>
```

## Roles outside the boundary

Focused tests are the tight feedback loop while changing a module. They do not
replace the full suite at the boundary. The full suite does not replace the
source-tarball check: CRAN semantics skip snapshot expectations, while the
tarball check alone sees package metadata and installed-package boundaries.
Neither test run replaces jarl or lintr.

The Review-ready check uses the developer's fully provisioned library. The
release matrix remains responsible for depends-only, suite-coverage, library
isolation, optional-backend execution, other R versions, and other operating
systems. The CRAN release playbook owns the stricter exact-candidate preflight
and remote checks.

Unused-Suggests review is not part of the daily Review-ready check. Dependency
metadata still follows `AGENTS.md`'s *Dependency metadata* rules when changed;
the broader dependency-hygiene review belongs to CRAN-release preparation.
