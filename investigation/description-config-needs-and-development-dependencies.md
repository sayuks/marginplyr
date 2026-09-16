# DESCRIPTION dependency fields and repository development dependencies

Investigated: 2026-09-15

This investigation asked how R Core's `Depends`, `Imports`, and `Suggests`
fields differ from the r-lib `Config/Needs/*` convention, whether repository
development tools must be declared in `DESCRIPTION`, and what it would mean to
leave `spelling` in `Suggests` after removing `tests/spelling.R`. It used R
Core manuals, the CRAN Repository Policy, and first-party documentation and
source from pak/pkgdepends, r-lib/actions, pkgdown, and spelling.

## Conclusion

`Depends`, `Imports`, and `Suggests` are R Core package-dependency fields.
`Config/Needs/*` is not one of R Core's dependency fields. It is nevertheless
a valid custom `DESCRIPTION` field: R Core places no restriction on additional
fields, and pak/pkgdepends defines the `Config/Needs/*` convention for named
extra dependency sets. These sets are opt-in. Base R does not install them,
and pak says that they are not used from CRAN-like package sources unless
explicitly requested for a direct non-repository installation.

The useful boundary is the source package rather than the whole Git
repository. A package needed to install, load, exercise an example, run a
package test, or build/check a vignette belongs in an R Core dependency field.
A package used only by a repository script excluded with `.Rbuildignore` is
outside that R Core declaration rule. Recording such a package in a requested
`Config/Needs/<task>` set, or explicitly in the workflow that runs the task,
improves reproducibility but is not a CRAN requirement.

No rule forbidding an unused `Suggests` entry was found in the CRAN Repository
Policy. Thus, assuming the suggested package satisfies the Policy's
availability rules, leaving `spelling` in `Suggests` after deleting
`tests/spelling.R` is not an identified CRAN Policy violation. It is, however,
less exact package metadata if no shipped package code, example, test, or
vignette uses `spelling`: R's full-dependency installation then installs a
package that is needed only by excluded repository tooling. `Config/Needs/*`
expresses that repository-task role more precisely.

## R Core dependency fields

R Core defines the fields in terms of the installed and checked package, not a
special "developer" audience:

- `Depends` packages are attached before the current package when it is
  attached. R Core says this field should be used rarely, for facilities
  intended to be placed on the search path for the end user
  ([Writing R Extensions, package dependencies](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-Dependencies)).
- `Imports` packages have namespaces that the package imports or accesses with
  `::`/`:::` without attaching them. Every `Imports` package must be installed
  before the package can be installed; `R CMD check` checks for entries that
  are not actually imported or accessed
  ([Writing R Extensions, package dependencies](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-Dependencies)).
- `Suggests` packages are not necessarily needed. R Core explicitly includes
  packages used only in examples, demos, tests, or vignettes, and packages
  used conditionally in function bodies
  ([Writing R Extensions, package dependencies](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-Dependencies),
  [Writing R Extensions, suggested packages](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Suggested-packages)).

All packages required for a successful complete `R CMD check` must be listed
in `Depends`, `Imports`, or `Suggests`; conditionally run examples and tests
still declare their packages in `Suggests`
([Writing R Extensions, package dependencies](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-Dependencies)).
R Core also says that the fields must be complete and accurate because package
management uses them for version checks, reverse-dependency computation, and
parallel installation.

The `install.packages()` default installs hard dependencies: `Depends`,
`Imports`, and `LinkingTo`. `dependencies = TRUE` additionally installs the
direct package's `Suggests`; the documented purpose is to install what is
needed to run the package, its examples, tests, and vignettes
([R `install.packages` documentation](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/install.packages.html)).
This option is available to any caller. It does not identify the caller as a
package developer. A user who asks for all optional functionality receives the
same `Suggests` closure. pak similarly describes `dependencies = TRUE` as
installing required plus optional and development dependencies
([pak `pkg_install()` documentation](https://pak.r-lib.org/reference/pkg_install.html)).

## What `Config/Needs/*` is

R Core permits additional fields in `DESCRIPTION`, while noting that the
fields used by R are the capitalized fields it documents
([Writing R Extensions, `DESCRIPTION`](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file)).
That makes a field such as `Config/Needs/preflight` syntactically valid, but it
does not make it a dependency type understood by base R. The base
`install.packages()` and `tools::package_dependencies()` documentation
enumerates `Depends`, `Imports`, `LinkingTo`, `Suggests`, and `Enhances`, not
`Config/Needs/*`
([R `install.packages` documentation](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/install.packages.html),
[R `package_dependencies` documentation](https://stat.ethz.ch/R-manual/R-devel/library/tools/html/package_dependencies.html)).

The extra-dependency semantics come from pkgdepends/pak. Their documentation
divides dependencies into hard, soft, and extra groups. A
`Config/Needs/<name>` field contains comma-separated package references for a
named extra group, for example:

```dcf
Config/Needs/website: r-lib/pkgdown
```

The group must be explicitly included in a pkgdepends `deps` or pak
`dependencies` argument. pkgdepends says these extra groups are for direct
installations not from CRAN-like repositories and are not normally used when
the package comes from a CRAN-like repository
([pkgdepends dependency types](https://r-lib.github.io/pkgdepends/reference/as_pkg_dependencies.html),
[pak dependency types](https://pak.r-lib.org/reference/package-dependency-types.html)).
Unlike R Core dependency fields, the values can be pak package references such
as a GitHub repository, not only package names with R-style version
constraints.

`r-lib/actions/setup-r-dependencies` is the first-party bridge used in GitHub
Actions. Its `needs` input names suffixes; the action prepends
`Config/Needs/` and passes those dependency types to pak. For example,
`needs: website` selects `Config/Needs/website`
([setup-r-dependencies README](https://github.com/r-lib/actions/tree/v2-branch/setup-r-dependencies),
[setup-r-dependencies action source](https://github.com/r-lib/actions/blob/v2-branch/setup-r-dependencies/action.yaml)).

pkgdown documents `Config/Needs/website` for a package needed to build the
site but not intended as a CRAN package dependency. The documented guarantee
depends on using the associated usethis/r-lib GitHub workflow; it is the action
that installs the field. This is evidence for a task-specific repository
dependency, not evidence that pkgdown generally treats every `Config/Needs/*`
field as a package dependency
([pkgdown template-package documentation](https://pkgdown.r-lib.org/articles/customise.html#template-packages)).

## Undeclared development packages

R Core's declaration requirement reaches the package surfaces that `R CMD
build` and `R CMD check` process. Its check scans package references in shipped
code and checks that packages required by package examples and tests are
declared, although the static scan is not exhaustive
([Writing R Extensions, checking package subdirectories](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Checking-package-subdirectories)).
The package dependency section separately requires every package needed for a
complete check to be declared.

`.Rbuildignore` defines the other side of this boundary: its matching files
are excluded from the source package
([Writing R Extensions, building package tarballs](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Building-package-tarballs)).
R Core specifies no `DESCRIPTION` declaration rule for an arbitrary R script
that exists only in the Git repository and is excluded from the source
package. Consequently, using an undeclared package in such a script is not by
itself forbidden by Writing R Extensions or the CRAN Repository Policy.

It does leave the script's environment implicit. Base R will not infer or
install that dependency from `DESCRIPTION`. With r-lib tooling, the explicit
ways to make the task reproducible are a requested `Config/Needs/<task>` field
or the action's `extra-packages` input. The former records a reusable named
dependency set in `DESCRIPTION`; the latter keeps a dependency local to one
workflow. Neither becomes a base-R package dependency.

One narrow exception shows why "development" cannot be classified solely by
who runs the command: R Core permits vignette outputs to have been built with
private packages available only on the author's machine when the built output
is included, but packages required to rebuild/check non-Sweave vignettes must
be declared
([Writing R Extensions, writing package vignettes](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-package-vignettes),
[Writing R Extensions, non-Sweave vignettes](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Non_002dSweave-vignettes)).
The operation and shipped surface, rather than the human's role, determines the
metadata field.

## CRAN Policy and an unused `Suggests` entry

The CRAN Repository Policy distinguishes strong dependencies (`Depends`,
`Imports`, and `LinkingTo`) from `Suggests`/`Enhances`. It requires strong
dependencies to be available from CRAN or Bioconductor, requires an obtaining
route for suggested packages outside the standard repositories, and requires
conditional use of a suggested package that cannot be installed
straightforwardly on the major platforms
([CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)).
As investigated on 2026-09-15, it did not state that every `Suggests` entry
must be statically used or prohibit an entry used only by repository
development tooling. The `spelling` package was available from CRAN
([CRAN package page](https://cran.r-project.org/package=spelling)).

Therefore, retaining `spelling` in `Suggests` after deleting
`tests/spelling.R` falls into three distinct judgments:

1. **CRAN Policy:** no explicit violation was identified. The package is on
   CRAN, and the Policy contains no found unused-Suggest prohibition.
2. **R Core metadata semantics:** the entry becomes less exact if no shipped
   package surface uses it. `install.packages(dependencies = TRUE)` promises a
   closure for the package, examples, tests, and vignettes, not specifically a
   clone's excluded review scripts.
3. **Repository convenience:** retaining it makes tools that install all
   direct soft dependencies obtain `spelling` without another task-specific
   field. That is a real convenience, but it applies to every full-dependency
   installer rather than only developers.

The spelling package's own setup function illustrates the ordinary
`Suggests` case: it adds `spelling` to `Suggests` together with a unit test that
runs during `R CMD check` under `NOT_CRAN`. Its first-party documentation ties
the dependency declaration to that package test
([spelling package documentation](https://docs.ropensci.org/spelling/reference/spell_check_package.html)).
Deleting `tests/spelling.R` removes that particular package-level reason but
does not create a CRAN Policy offence.

## Application to the investigated marginplyr tree

On 2026-09-15, marginplyr's `.Rbuildignore` excluded `tools/`, `.github/`, and
`investigation/`. Its `tests/spelling.R` was the shipped package use supporting
the `Suggests: spelling` entry. Separately, the excluded
`tools/review-ready-check-lib.R` and `tools/cran-preflight-lib.R` used
`spelling`, and `.github/workflows/lint.yaml` installed dependencies with
r-lib/actions.

If `tests/spelling.R` is removed, two coherent metadata choices remain:

- Keep `spelling` in `Suggests` as a deliberate convenience for full
  development installs. This is simple and no explicit CRAN Policy rule found
  in this investigation forbids it, but it broadens the package's advertised
  soft dependency closure beyond shipped package needs.
- Move it to one or more requested extra groups, such as
  `Config/Needs/preflight` and/or a named lint/review group, or list it in the
  relevant workflow's `extra-packages`. This states the repository-task role
  more precisely, at the cost of deciding which task owns the dependency and
  ensuring each local/CI entry point installs or checks that group.

Neither option is forced by CRAN Policy. The choice is a repository dependency
model decision: convenience through the standard all-soft-dependencies path,
or precision through explicit task-specific tooling dependencies.
