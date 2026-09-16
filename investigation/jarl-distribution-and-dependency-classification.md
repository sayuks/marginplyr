# Jarl distribution and dependency classification

Investigated: 2026-09-16

This investigation asked whether Jarl is an R package, how it is distributed,
how its command-line interface relates to R, and where marginplyr should record
the dependency used by its lint workflow and local review-ready check.

## Conclusion

The Jarl examined on 2026-09-16 was **not an R package**. It was a standalone
command-line application, written in Rust, that parses and analyzes R source
code. “A linter for R” describes the language it inspects; it does not describe
the packaging technology used to install the linter.

The distinction was visible at every layer:

- Jarl's documentation called it a command-line tool and explicitly instructed
  users to run it in a terminal, not an R console.
- Its source repository was a Cargo workspace whose user-facing executable was
  the `jarl` Rust crate.
- Its supported installation routes delivered an executable through GitHub
  release binaries, Homebrew, Scoop, PyPI, conda-forge, or Cargo. None used
  `install.packages()` or installed an R package into an R library.
- `setup-jarl` fetched Jarl release artifacts, added the executable to `PATH`,
  and ran `jarl check` by default.

Consequently, a `Config/Needs/*` entry is not an appropriate declaration for
the Jarl executable. pak defines those fields as lists of **R package
references**, and `r-lib/actions/setup-r-dependencies` resolves them with pak.
The Jarl repository is not an R package source that pak can install into an R
library. marginplyr's separate `setup-jarl` action and its local `Sys.which()`
check represented the executable dependency accurately.

## Evidence

### Jarl is a Rust command-line application for R source code

The official homepage described Jarl as a fast linter for R with a
command-line interface, showed `$ jarl check test.R` in a terminal, and said it
was built on Air, an R formatter written in Rust
([Jarl homepage](https://jarl.etiennebacher.com/)). The getting-started guide
was even more explicit: “Jarl is a command-line tool” and must run in the
terminal rather than an R console
([Jarl getting started](https://jarl.etiennebacher.com/getting-started)).

The contributor guide said Jarl was written in Rust and described its source
as Rust crates:

- `jarl` implemented the command-line tool;
- `jarl-core` parsed and checked R code and implemented lint rules; and
- `jarl-lsp` supplied Language Server Protocol integration.

Its development commands were Cargo commands such as
`cargo run --bin jarl -- check file.R` and
`cargo install --path crates/jarl --profile=release`
([Jarl contributing guide](https://jarl.etiennebacher.com/contributing)). The
repository root was a Cargo workspace with `crates/*` as members and Rust crate
dependencies
([Jarl `Cargo.toml`](https://github.com/etiennebacher/jarl/blob/main/Cargo.toml)).

These facts distinguish two separate roles:

```text
R files and R package trees         Jarl program
          input               ->    Rust executable on PATH
                                      |
                                      +-- `jarl check ...`
                                      +-- diagnostics and exit status
```

Jarl understands R syntax and some R-package structure, but it does not run as
an R namespace loaded with `library(jarl)` or `jarl::...`.

### Distribution produces an executable, not an installed R package

The official installation page documented these release routes
([Jarl installation](https://jarl.etiennebacher.com/#installation)):

- download prebuilt binaries from GitHub Releases or run the release installer
  scripts for macOS, Linux, or Windows;
- `brew install jarl`;
- install from Scoop;
- install the PyPI distribution named `jarl-linter` with `uv` or `pipx`;
- install the conda-forge distribution through Pixi or mise; or
- compile and install the development version with
  `cargo install --git https://github.com/etiennebacher/jarl jarl --profile=release`.

The same page said release binaries were placed in `$HOME/.local/bin` and
Cargo installs in `$HOME/.cargo/bin`. The PyPI name is therefore an additional
delivery route for the CLI, not evidence of either an R namespace or an R
package. Release 0.6.0 likewise announced PyPI, conda-forge, and Homebrew as new
ways to install Jarl
([Jarl 0.6.0 release](https://github.com/etiennebacher/jarl/releases/tag/0.6.0)).

On the investigation date, the official CRAN source-package index contained
25,077 `Package:` records and no exact `Package: jarl` record
([CRAN `PACKAGES.gz`](https://cran.r-project.org/src/contrib/PACKAGES.gz)). An
exact `package:jarl` query to R-universe's documented global package search
returned an empty results array
([R-universe search documentation](https://docs.r-universe.dev/browse/search.html),
[R-universe search API query](https://r-universe.dev/api/search?limit=200&all=true&q=package%3Ajarl)).
Those negative catalogue results are corroborating evidence, not the main
classification: the official Jarl source and installation documentation
already identify its actual form.

### setup-jarl installs and invokes the executable

The official `setup-jarl` README called the project a GitHub Action for Jarl.
It documented that the action:

- accepts a Jarl `version` independently of the action version;
- fetches Jarl release artifacts through the GitHub API;
- adds `jarl` to `PATH` for subsequent steps; and
- runs `jarl check` by default after installation.

It can also install the executable without a lint run by overriding the
default arguments
([setup-jarl repository](https://github.com/etiennebacher/setup-jarl),
[setup-jarl README](https://github.com/etiennebacher/setup-jarl/blob/main/README.md)).

Therefore `uses: etiennebacher/setup-jarl@...` is not analogous to
`install.packages("jarl")`. The action is a binary setup-and-run step. The
executable's version and the action implementation have separate version
coordinates.

### Config/Needs installs R package references

pak groups `Depends`, `Imports`, and `LinkingTo` as hard R package
dependencies, `Suggests` and `Enhances` as soft dependencies, and
`Config/Needs/*` as extra dependency types. The extra fields may contain
comma-separated **package references** and must be requested explicitly when a
direct package is installed
([pak package dependency types](https://pak.r-lib.org/reference/package-dependency-types.html)).

pak's supported references identify R package sources: CRAN, Bioconductor,
CRAN-like repositories, GitHub or Git repositories containing an R package,
local R package files/directories, and URLs to package archives. pak installs
the resulting packages into an R library
([pak package sources](https://pak.r-lib.org/reference/pak_package_sources.html)).

The official `r-lib/actions/setup-r-dependencies` action defines its `needs`
input as additional `Config/Needs` fields. Its implementation converts each
name to `Config/Needs/<name>` and passes those dependency types to
`pak::lockfile_create()` before installing the lockfile
([setup-r-dependencies action source](https://github.com/r-lib/actions/blob/v2-branch/setup-r-dependencies/action.yaml)).

Thus neither of these declarations would describe the current Jarl correctly:

```text
Config/Needs/lint: jarl
Config/Needs/lint: etiennebacher/jarl
```

The first asks pak to resolve an R package named `jarl`; the second asks pak to
treat the GitHub repository as an R package source. The official repository is
a Cargo workspace without a root R-package `DESCRIPTION`, so neither installs
the documented Jarl executable. A future, separate R wrapper package could be
a package reference, but it would be a dependency distinct from the CLI binary
it wraps or downloads.

### R package metadata has a separate field for genuine external requirements

Writing R Extensions says that dependencies external to the R system should
be listed in `SystemRequirements`
([Writing R Extensions, `DESCRIPTION`](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file)).
That field is relevant when external software is a requirement of the R
package being installed, loaded, used, built, or checked. It does not turn an
excluded repository lint gate into an R package dependency, and it does not
install the external program.

If marginplyr package code, examples, tests, or vignettes actually invoked the
`jarl` executable, `SystemRequirements` would be the R metadata category to
evaluate for the executable, alongside any R package wrapper declared in a
normal dependency field. That was not the repository shape inspected here.

### Repository evidence

The following statements record the marginplyr tree inspected on 2026-09-16;
they do not claim that later revisions retained the same configuration:

- `.github/workflows/lint.yaml` first installed R packages through
  `r-lib/actions/setup-r-dependencies`, then used
  `etiennebacher/setup-jarl@v0.1.1` with Jarl version `0.6.0`. The two installers
  were separate steps.
- `tools/review-ready-check-lib.R` checked R packages with
  `requireNamespace()`, but checked Jarl with `Sys.which("jarl")`. It later ran
  the returned executable path with arguments `check .`.
- `.Rbuildignore` excluded `.github/`, `tools/`, and `jarl.toml` from the source
  package.
- `DESCRIPTION` did not list Jarl in an R package dependency field or in
  `SystemRequirements`. Its existing `Config/Needs/preflight` and
  `Config/Needs/website` entries contained R package references installed by
  the relevant R dependency setup.

The local implementation therefore made the same type distinction as the
upstream documentation:

```text
R packages                    external CLI
----------                    ------------
requireNamespace("lintr")     Sys.which("jarl")
setup-r-dependencies          setup-jarl
R package library             executable on PATH
```

## Findings

### “For R” does not mean “an R package”

Jarl consumes R code and recognizes R-package layouts. That is comparable to a
compiler, formatter, or language server that understands a language without
being packaged in that language's package ecosystem. The executable can lint
an R package while remaining an external program.

### The CLI is the product, not a front end to an installed R namespace

The official user interface is `jarl check`, the official implementation is a
Rust crate, and all documented distribution routes make that executable
available. No evidence found on the investigation date supported a model in
which an R package named `jarl` was installed first and the CLI merely called
into it.

### Config/Needs is extensible by purpose, not by artifact type

The word after `Config/Needs/` can be project-defined (`website`, `preflight`,
`lint`, and so on), but the values retain pak's package-reference semantics.
Creating `Config/Needs/lint` does not make arbitrary binaries installable by
pak. It can collect R packages needed by a lint environment, while a tool such
as Jarl still needs its own binary installer.

## Implications for marginplyr

The configuration inspected on 2026-09-16 should keep Jarl out of
`Config/Needs/*`. The existing division is accurate:

1. `setup-r-dependencies` installs R packages required by the lint job.
2. `setup-jarl` installs and runs the pinned Jarl executable.
3. The local review-ready tool requires `jarl` to be discoverable on `PATH`
   and reports it as a missing prerequisite otherwise.

This does leave local installation as a contributor-environment concern.
Useful documentation can name an official Jarl installation method or the
required version, but adding `Config/Needs/lint: jarl` would not solve that
problem. A reproducible local-tool manifest capable of installing non-R
binaries would be another valid repository-level mechanism if the project
later adopted one.

`SystemRequirements: Jarl ...` was also not warranted by the inspected use.
Jarl was required by an additional repository QA wrapper, not by the source
package or `R CMD check`, and all call sites and configuration were excluded
from the tarball. If that boundary changes, the metadata should be reconsidered
from the new call site rather than from the tool's name.

## Uncertainties and limits

- This note classifies the upstream Jarl repository and distributions as they
  were inspected on 2026-09-16. A future project could publish an R wrapper
  package also named `jarl`; that would not retroactively change the format of
  the CLI examined here.
- The upstream repository contained an `old/` directory, but on the
  investigation date it held only a Rust-design research note. GitHub's commit
  API returned no commits for either a root `DESCRIPTION` path or
  `old/DESCRIPTION`. No primary-source evidence was found that this Jarl had
  previously been an R package. This is a bounded negative search, not proof
  that no unpublished prototype ever existed elsewhere.
- Absence from CRAN or R-universe alone would not prove that software is not an
  R package, because an R package can be distributed only from GitHub. Here the
  absence only corroborates the positive source, build, and installation
  evidence.
- `SystemRequirements` records an external requirement but does not prescribe
  a portable installation command. If Jarl ever becomes necessary to build or
  check the shipped package, a separate provisioning mechanism would still be
  required in CI.
