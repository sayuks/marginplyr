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

### Chunks that must fail

A vignette showing a rejected call executes it rather than quoting its error,
so the reader sees the diagnostic their own session would produce. Quarto's
`error: true` is the wrong option on its own: it *permits* an error without
requiring one, so a chunk that stops failing renders a success underneath
prose that still claims a failure, and nothing reports it. knitr and Quarto
offer no option for the other half, and no package supplies one
(`investigation/requiring-a-documentation-chunk-to-fail.md`).

`inst/vignette-hooks/must-error.R` therefore defines a `must_error` chunk
option. It implies `error: true`, so the two are never set inconsistently, and
it halts the render naming the chunk when a chunk marked with it completes
without raising an error. Mark a chunk with it instead of `error: true`
whenever the surrounding prose asserts that the call fails.

It takes two forms. `must_error: true` accepts any error, which is what the
option has always meant. `must_error: marginplyr_error` additionally requires
the error to carry that condition class. Prefer the class form wherever the
prose names what refuses the call: a bare `true` passes when the call fails for
an unrelated reason — a renamed argument, a typo, a changed column — and the
reader is then shown a diagnostic the prose does not describe. The check reads
the `parent` chain, because a Package condition usually reaches the reader
wrapped by the dplyr verb that raised it, and it is the wrapped class the prose
names. Any other value halts the render rather than being ignored, since a
header this cannot read is one whose assertion silently stopped happening.

The definition lives under `inst/` so that every vignette reaches it in one
line rather than carrying a copy:

```r
source(system.file("vignette-hooks", "must-error.R", package = "marginplyr"))
```

That works because vignettes are built against the *installed* package, which
is also why a working tree whose hook file changed must be reinstalled before
its vignettes are rendered. The call needs no availability guard even though
knitr is a Suggest: `VignetteBuilder: quarto` is visible while vignettes are
rebuilt even under `_R_CHECK_DEPENDS_ONLY_=true`, and quarto imports rmarkdown,
which imports knitr — the `DBI = FALSE` case from *Dependency metadata*, so a
guard on knitr in a vignette would never fire
(`investigation/restoring-knitr-hooks-a-vignette-installs.md`).

Three properties are load-bearing and easy to lose in a rewrite. It is
implemented as a wrapper around knitr's `evaluate` hook, which inspects the
returned result objects; that keeps knitr's own error rendering, whereas
catching the condition in a helper prints an `<error/rlang_error>` header and
a backtrace through the helper, which no reader would see. Because knitr
does not call that hook for a chunk it does not evaluate, a chunk withheld by
an availability guard is skipped without a special case — a guarded chunk that
never runs must not be reported as a chunk that stopped failing, or
`_R_CHECK_DEPENDS_ONLY_` builds break. And the definition undoes itself from an
`after.knit` hook, which `knit()` runs from `on.exit()` so that a render halted
by the option restores as a completed one does; knitr restores neither the
`opts_hooks` entry nor a `knit_hooks` entry installed while it runs, and the
`document` hook — the obvious alternative — is not called on the halted path
(`investigation/restoring-knitr-hooks-a-vignette-installs.md`).

`.github/scripts/verify-must-error.R` is the gate, run by `altdoc.yaml` and
locally with `Rscript .github/scripts/verify-must-error.R`. It knits fixture
documents covering each form, the guarded chunk, a malformed option value, and
the restoration, because nothing else in the repository fails when the option
stops working — an option that asserts nothing reports nothing, which reads
exactly like a set of vignettes whose rejected calls are all still rejected.
It is not a testthat test: `release-matrix.yaml`'s `backend` jobs install the
hard dependencies plus one optional backend, so knitr is absent there — a
vignette rebuild is what puts it in the closure, and those jobs pass
`--ignore-vignettes` — and `verify-backend.R` fails a job for any skip that does
not name a backend the job withheld.

Its guarded-chunk fixture withholds a name no library holds, because what a
fixture can vary is whether knitr evaluated a chunk and not why. A genuinely
absent Suggest is covered where one is absent: `depends-only` rebuilds the
vignettes with every Suggest withheld, and `recipes.qmd`'s `nested-aggregate`
chunk is a `must_error` chunk behind `has_duckdb`, so an option that reported it
fails that job.

### Site verification

`.github/workflows/altdoc.yaml` renders the site and then runs
`.github/scripts/verify-site.R` over `docs/`. That script derives the pages it
requires instead of listing them: one per `vignettes/*.qmd`, one per `man/*.Rd`
that is not marked `\keyword{internal}`, `docs/index.html`, and one per
`file: $ALTDOC_*` slot in `altdoc/quarto_website.yml` whose repository file is
present. Adding a vignette or an exported function therefore needs no edit to
the script for its page to be covered.

Only the last of those keeps a table, because a placeholder cannot say on its
own whether the file behind it exists, and an assertion is what stops that
table behaving like the list this replaced: a `file:` slot the site config
declares and the table does not name fails the job rather than escaping it. A
second assertion counts the derived pages against the sources they came from,
since every check iterates over that set and a set that arrived empty is a set
that passes.

The derivation is what decides coverage; the marker list is not. Every derived
page has to exist, reach `</html>`, carry no build-machine path, and contain
neither `installed.packages` nor the retired name `union_all_with_margins`. The
last two scans also run over `docs/search.json`, which carries every page's
text and is served beside them. `markers` adds page-specific prose on top of
that, and a key naming no derived page is an error, so renaming a vignette
moves its markers rather than silently dropping them. Marking a `must_error`
chunk's rendered diagnostic is the strongest marker available: prose survives a
chunk that stopped running, a diagnostic does not.

Both halves replaced something weaker. The hand-written required list omitted
`recipes.html` entirely, so a silent render failure of the newest vignette left
the job green (#114); the path scan named only `Rtmp` and ran against one
article, which is why #99's `/Users/<user>/.duckdb` reached two shipped
vignettes unnoticed. As with the backend matrix, the cost is that the workflow
no longer shows what it checks, so the script writes the derived set to the job
summary.

To run it locally, render first with
`altdoc::render_docs(parallel = FALSE, freeze = FALSE)`, then
`Rscript .github/scripts/verify-site.R`. The render executes vignette and
example code against the *installed* marginplyr, not the working tree, so
install the working tree first — otherwise the reference pages fail on
functions the installed version does not export, and the failure looks like a
broken vignette rather than a stale library.

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

The audit runs in the other direction too, and a scanner is no more use there:
a package the shipped sources never reference does not belong in Suggests,
however real the tool that needs it. `att_amend_desc()` does prune, but by the
same static reading that over-prunes above, so what it removes is not evidence
either. `altdoc` was declared there while nothing outside `.Rbuildignore`d
paths used it — `.github/workflows/altdoc.yaml` and the `altdoc::render_docs()`
call in this file build the site, `altdoc/` configures it, and
`.github/scripts/verify-site.R` reads its output — so the entry installed a
dependency closure for a package the tarball never mentions (#113).

`Config/Needs/website` is where such a dependency belongs, and it is not a
weaker home: `setup-r-dependencies@v2` resolves that field through pak, which
parses *and enforces* a version constraint written there, so a floor moved
across loses nothing. That is worth re-checking rather than assuming, because
nothing in this repository would fail if it stopped holding — a dropped
constraint silently installs an older altdoc. Check it by putting an
unsatisfiable constraint in the field of a throwaway package and resolving it:
`pak::pkg_deps("local::<pkg>", dependencies = list(direct =
"Config/Needs/website", indirect = "Config/Needs/website"))` must fail naming
the constraint, not resolve.

The grep above is what finds an entry like this, because `R CMD check` does not
— an unused Suggest raises no NOTE, which is why nothing flagged it for as long
as it stood.

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
