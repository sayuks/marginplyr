# Classifying lintr as a package or repository dependency

Investigated: 2026-09-16

This investigation asked why marginplyr can run `lintr` locally and in CI
without declaring `lintr` in `DESCRIPTION`. It distinguishes a package needed
by a complete `R CMD check` from a repository-level quality-assurance tool
that invokes an additional check outside `R CMD check`.

## Conclusion

`lintr` does not need to be in marginplyr's `DESCRIPTION` for the configuration
examined on 2026-09-16. No shipped marginplyr code, example, test, or vignette
called `lintr`. Instead, an excluded GitHub Actions workflow and an excluded
review-ready wrapper installed or required `lintr`, then ran
`lintr::lint_package()` as a separate QA operation. That operation can fail
before `R CMD check`, but it is not thereby part of the `R CMD check` command
whose dependencies Writing R Extensions requires the package to declare.

This classification follows the first-party lintr and r-lib/actions design.
lintr documents linting as a separate continuous-integration workflow, and the
official r-lib/actions `lint.yaml` installs `lintr` through
`extra-packages: any::lintr, local::.` rather than requiring the target
package to list `lintr` in `DESCRIPTION`.

The boundary would move if shipped package material called `lintr`. A test or
example that calls `lintr` makes it a package-check dependency and normally a
`Suggests` entry. An unconditional call from installed package code normally
makes it an `Imports` entry. The identity of the tool does not create a special
exception; the calling surface determines the dependency field.

## Evidence

### R Core defines package dependencies around installation, loading, and check

Writing R Extensions says that all packages needed to run `R CMD check`
successfully must be listed in `Depends`, `Imports`, or `Suggests`, including
packages used conditionally in examples or tests. It says this allows checkers
to install everything required for a complete check
([Writing R Extensions, package dependencies](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-Dependencies)).

The same section defines the relevant roles:

- `Imports` contains packages imported through `NAMESPACE` or accessed with
  `::` or `:::` by package code; R Core says `R CMD check` checks for Imports
  entries that are not imported or accessed.
- `Suggests` contains packages that are not necessarily needed, explicitly
  including packages used only in examples, demos, tests, or vignettes and
  packages used conditionally in function bodies.
- The dependency information should be complete and accurate because package
  installation, version checking, reverse-dependency computation, and parallel
  installation use it.

These are package-check semantics, not a rule that every program used by every
repository workflow must be a package dependency. Writing R Extensions'
description of `R CMD check` separately enumerates its package checks and says
that it checks package references in `library`, `require`, `NAMESPACE`, and
`::`/`:::` calls, while noting that the scan is not exhaustive
([Writing R Extensions, checking package subdirectories](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Checking-package-subdirectories)).
It does not define an arbitrary wrapper command run before or after `R CMD
check` as part of that command.

`.Rbuildignore` supplies the source-package boundary: matching paths are not
put into the built package
([Writing R Extensions, building package tarballs](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Building-package-tarballs)).
The CRAN Repository Policy governs package dependencies and their availability,
but it does not require CRAN packages to use lintr or classify a separate lint
workflow as part of `R CMD check`
([CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)).

### lintr documents linting as an additional CI operation

lintr describes itself as static code analysis that checks style, syntax, and
possible semantic issues. Its continuous-integration documentation says that
lintr can run in CI for a package or for a general project so that commits and
pull requests do not deteriorate code style. For packages it recommends the
r-lib/actions `lint` example and says that the generated workflow runs
`lintr::lint_package()`
([lintr continuous-integration vignette](https://lintr.r-lib.org/articles/continuous-integration.html),
[lintr repository](https://github.com/r-lib/lintr)).

The same documentation also covers non-package projects. This demonstrates
that `lint_package()`'s ability to inspect a package does not turn lintr into a
dependency of the inspected package. lintr is a program in the QA environment;
the source tree is its input.

For package-aware linters such as `object_usage_linter()`, lintr warns that the
target package and its dependencies must be installed or loaded, for example
with `pkgload::load_all()`. This requirement runs in the opposite direction:
the lint process needs a usable target-package namespace; it does not mean the
target package needs lintr in order to install, load, or pass `R CMD check`
([lintr continuous-integration vignette](https://lintr.r-lib.org/articles/continuous-integration.html)).

### The official r-lib lint workflow installs lintr outside DESCRIPTION

The r-lib/actions `setup-r-dependencies` action defines `extra-packages` as
packages to install outside those listed in the package dependencies
([setup-r-dependencies action source](https://github.com/r-lib/actions/blob/v2-branch/setup-r-dependencies/action.yaml),
[setup-r-dependencies documentation](https://github.com/r-lib/actions/tree/v2-branch/setup-r-dependencies)).

The official package lint example uses exactly that mechanism:

```yaml
- uses: r-lib/actions/setup-r-dependencies@v2
  with:
    extra-packages: any::lintr, local::.
    needs: lint

- name: Lint
  run: lintr::lint_package()
```

It installs the local package and its declared dependencies, adds `lintr` as a
workflow tool, then runs lint as its own step
([r-lib/actions `lint.yaml`](https://github.com/r-lib/actions/blob/v2-branch/examples/lint.yaml)).
lintr's own official documentation directs package authors to this example.

usethis copies r-lib/actions workflows into `.github/workflows` and, by
default, adds those workflow files to `.Rbuildignore`. Its documentation lists
`R CMD check`, pkgdown, test coverage, and other workflows as distinct CI
operations
([usethis `use_github_action()`](https://usethis.r-lib.org/reference/use_github_action.html),
[usethis GitHub Actions setup](https://usethis.r-lib.org/reference/github_actions.html)).
This reinforces the difference between a package-check dependency and the
runner needed to implement another repository check.

### Repository evidence

The following statements describe the marginplyr tree inspected on
2026-09-16, not a maintained claim about later revisions:

- `DESCRIPTION` did not list `lintr` in `Depends`, `Imports`, or `Suggests`.
- `.Rbuildignore` excluded `.github/` and `tools/` from the source package.
- No call to `lintr` occurred under `R/`, `tests/`, or `vignettes/`.
- `.github/workflows/lint.yaml` installed `any::lintr`, `any::pkgload`, and
  `local::.` through `extra-packages`, loaded the package, and ran
  `lintr::lint_package()` in a dedicated `Lint` step.
- `tools/review-ready-check-lib.R` treated `lintr` and `pkgload` as local
  prerequisites and ran a named `package-aware lintr` step before building the
  tarball and invoking `rcmdcheck::rcmdcheck()`.

The last ordering is particularly direct evidence. The repository's wrapper
called lint and `R CMD check` as two separate child operations. Calling both
from one wrapper did not make the first operation part of the second.

## Findings

### “Needed for a complete R CMD check” has a bounded meaning

In Writing R Extensions, a complete check means the package operations that
`R CMD check` performs, including its examples, tests, and vignettes. It does
not mean every gate that a repository chooses to require before review. A
repository can require formatting, linting, spelling, security analysis, or a
release audit in addition to `R CMD check`; those requirements do not become
R package dependencies merely because the wrapper requires all of them.

A useful dependency-direction test is:

```text
Does a shipped package surface call package X while R CMD check exercises it?
  yes -> X belongs in a standard DESCRIPTION dependency field
  no  -> continue

Does an excluded repository workflow call X to inspect or orchestrate the tree?
  yes -> X belongs to that workflow/tool environment, not necessarily DESCRIPTION
```

For marginplyr on the investigation date, `lintr` followed the second path.

### The official lintr workflow is affirmative evidence, not an omission

It would be possible to put `lintr` in `Suggests`, and CRAN Policy does not
publish a special ban on doing so. But the first-party workflow deliberately
uses `extra-packages`, whose documented purpose is to install packages outside
the target package's dependency declarations. Therefore absence from
`DESCRIPTION` is consistent with the official integration model rather than a
missing declaration that happens to work.

### Calling lintr from package tests changes the answer

If `tests/testthat/test-lint.R` or another shipped test called
`lintr::lint_package()`, `R CMD check` would execute the call. `lintr` would
then be needed for that complete package check and would ordinarily belong in
`Suggests`, with the test conditioned appropriately when optional. The same
classification applies to examples or vignettes that call lintr.

If installed package code called lintr only for optional functionality,
`Suggests` plus a runtime availability guard would ordinarily apply. If the
package could not load or its ordinary functionality could not work without
lintr, it would normally be an `Imports` dependency. These conclusions follow
the R Core field definitions; `lintr` receives no special classification as a
QA package.

Putting a lint assertion into `tests/` is therefore not metadata-neutral. It
moves repository style policy into the shipped package check and makes every
complete checker install the linter and its dependency closure. Keeping lint
as a separate workflow keeps that policy in the repository QA environment.

## Implications for marginplyr

The configuration examined on 2026-09-16 was coherent:

1. `DESCRIPTION` declared dependencies of the built and checked package.
2. The lint workflow declared `lintr` explicitly in `extra-packages`.
3. The local review-ready tool checked that `lintr` was installed before using
   it.
4. Both repository-only entry points were excluded from the source tarball.
5. The actual source-tarball `R CMD check` did not invoke lintr.

No `Suggests: lintr` entry was needed to make those statements true. Adding
one would trade metadata precision for the convenience of having a general
all-Suggests installation also install the repository linter. That is a
repository policy choice, not a requirement arising from `R CMD check`.

This classification also explains why another tool cannot be classified by
analogy to lintr without inspecting its call sites. If `spelling`, for
example, is called by a shipped `tests/spelling.R`, it is a package-check
dependency. If that test is removed and spelling is called only by excluded
workflow and review scripts, it occupies the same repository-tool role as
lintr. The package name is irrelevant; the shipped caller is decisive.

## Uncertainties and limits

- Writing R Extensions does not define a universal dependency manifest for an
  entire Git repository. It defines R package metadata. Separate tooling may
  use `extra-packages`, `Config/Needs/*`, a lockfile, or another environment
  declaration.
- The CRAN Repository Policy does not require linting and does not state a
  specific rule for `lintr` in `DESCRIPTION`. This investigation therefore
  cannot turn the first-party r-lib workflow convention into a CRAN mandate;
  it establishes that the convention is compatible with the documented R
  package boundary.
- `R CMD check`'s static undeclared-package scan is documented as
  non-exhaustive. A clean check cannot prove that all shipped runtime package
  references are declared. That limitation does not expand the declaration
  boundary to excluded CI scripts.
- A repository may deliberately choose to list some repository tools in
  `Suggests` for contributor convenience. That can be accepted as a local
  metadata policy, but it should not be justified by saying the tool is needed
  for `R CMD check` when the check command never invokes it.
