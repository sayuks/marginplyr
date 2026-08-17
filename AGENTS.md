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

`man/`, `NAMESPACE`, and `README.md` are generated. After changing roxygen
comments run `roxygen2::roxygenise()`; after changing `README.Rmd` run
`rmarkdown::render("README.Rmd")`. Commit what either produces;
`.github/workflows/document.yaml` regenerates all three and fails when any
differs from what its source produces.

The README is the generated file with the widest reach — GitHub shows it and
the website's home page includes it — and the only one whose source
`.Rbuildignore` keeps out of the tarball, so nothing a check run reaches
records what it should have contained. Rendering it executes its chunks
against the *installed* marginplyr, as the site build does, so install the
working tree first or the regenerated file shows an older package's output.

Its renderer is pinned, unlike roxygen2's, because pandoc's markdown writer
is not stable across versions while the check is a byte comparison: pandoc
3.8.3 — what `setup-pandoc@v2` installs by default — writes a `text` info
string on the fence holding the reporting-levels block, and 3.10.1 omits it,
so an unpinned job reds on a README nobody touched. `document.yaml` names the
version the committed file came from; regenerating locally against a
different pandoc produces a diff that is a pandoc difference and not a stale
file. Moving the pin means regenerating `README.md` in the same commit.

### Installation instructions

An installation instruction is a claim about the outside world, and no file
here can be read to check it: a README saying `install.packages()` finds
marginplyr reads exactly the same whether CRAN has published the package or
not. `DESCRIPTION`'s `Config/marginplyr/cran-status` field is where that fact
is recorded — `unpublished` or `published`, no other value — and it is the
only place it is written down. While it reads `unpublished`, the route the
documentation gives is the one an external user can actually run today,
`pak::pkg_install("sayuks/marginplyr")`.

`test-documentation.R` asserts both directions against the field, over the Rd
topics, the vignette sources, and both halves of the README. While the field
reads `unpublished` no page may name the CRAN installation call, the cranlogs
badge, or this package's CRAN page — the badge included because it renders
`CRAN downloads 0/month` for a package CRAN has never seen, which is a claim
of availability and not a report of zero interest. Once the field reads
`published`, `README.md` has to carry both halves of what publication gives a
reader: the instruction to run, and the badge or link saying where it goes.
Both directions are load-bearing: the first is what stops the documentation
getting ahead of CRAN, and the second is what stops a release flipping the
field while the README still sends readers to GitHub alone.

The rule is written as a function of the state and the pages rather than as a
branch on the field, so the fixtures beside it execute the `published`
direction today. A branch first evaluated on the day of the release is a
branch nothing has ever run, which is the objection *Chunks that must fail*
makes to an assertion that cannot fail.

The scan is deliberately blunt, as the version-blind guard scan above is:
prose that needs to name the CRAN installation call has to spell it some other
way. Its markers all name marginplyr, because the README's comparison table
links to another package's CRAN page and that is not a claim about this one,
and they match case-insensitively, because `cran.r-project.org` and
`CRAN.R-project.org` are one host and a claim is not less of one for being
typed the second way.

The milestone is publication, not submission. The field flips on the day
`https://cran.r-project.org/package=marginplyr` resolves — not when the
tarball is uploaded, not when `cran-comments.md` is written, and not when a
release ticket is closed. A submission can be rejected or archived, and an
instruction made true by uploading one would be false for however long that
took. On that day:

1. set the field to `published`;
2. restore the CRAN paragraph to `README.Rmd`'s installation section, above
   the GitHub route, and a CRAN badge to its badge block;
3. regenerate `README.md`; and
4. revisit the vignette installation blocks. They name the GitHub route only,
   which stays true in both states and is why nothing asserts them — but the
   distinction between the released and the development version becomes worth
   drawing again, and it is deliberately not drawn now.

Steps 1 to 3 hold each other up rather than relying on this list being
followed: 1 without 2 fails the suite, 2 without 1 fails it too, and 3 is what
`document.yaml` checks, so a `README.md` regenerated from a `README.Rmd` that
step 2 never touched fails at step 2's assertion instead. Step 4 is the one a
release can genuinely skip, which is why it is last and why it changes prose
that is true either way. No other file needs editing: the field is the release
process's copy of this fact, and it sits in `DESCRIPTION` beside the `Version`
a release is already bumping.

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

### Optional-dependency guards

Every guard deciding whether optional code runs goes through
`marginplyr_suggest_available()` in `inst/suggests/guard.R`. Never
`requireNamespace()` and never `rlang::is_installed()`, which answer "is it
installed" and not "is it new enough" — the question DESCRIPTION actually asks,
since several Suggests carry a version constraint. An installed-but-too-old
package passes an installation check, the guarded code runs, and it fails at
the feature instead of skipping (#123). `duckdb (>= 1.5.5)` is the case that
showed it: under 1.5.4.x `duckdb::duckdb(shared_home = FALSE)` is a hard error
rather than a degraded result.

`R CMD check` is not the exposure. It stops at `checking package dependencies`
with "required and available but unsuitable version" before any test, example,
or vignette runs, so CRAN, the release matrix, and any local check were always
safe. What the guard covers is everything that is not `R CMD check`:
`devtools::test()`, `pkgload::load_all()` plus `testthat::test_local()`, an
example run interactively, and the site build — whose `Config/Needs/website`
entries carry no versions, so a site job can legitimately install a version the
package's own Suggests entry rejects.

*Suggest* and *backend* are used from here on in the senses *Release matrix*
below separates them into, since the helpers this section names are shared with
the jobs that section describes.

DESCRIPTION states each constraint and the guard is the only thing that reads
one, so there is no version written down twice and nothing to keep in step. The
guard lives under `inst/` for the reason `must-error.R` does: the four
vignettes, the four examples, and
`tests/testthat/helper-optional-backends.R` each reach it in one `source()`
call on a `system.file()` path, and a copy in `tests/` would be the copy the
shipped sites drift from. Registering an optional Suggest with the test suite
still means editing the two places *Release matrix* names — a version is not a
third place, because the guard reads it.

`suggest_available()` consults the guard, so a too-old package skips rather
than running, unless the job named it in `MARGINPLYR_REQUIRED_SUGGESTS` — then
it errors, exactly as an absent required package does, and a `backend` job
holding a stale version reds as a failure rather than passing on a skipped
suite. The skip says which case it is: `{duckdb} 1.5.4.3 is installed, but
marginplyr requires >= 1.5.5`, deliberately not the `{pkg} is not installed`
wording, which would send a reader looking for a package sitting in their
library. A `backend` job cannot produce that skip — a package it named errors
and one it withheld is not installed — but the wording is still what stops
`verify-backend.R` attributing a version failure to a withheld package if one
ever reached that path. A package hidden by `MARGINPLYR_HIDE_SUGGESTS` keeps
the absent wording, because a simulated absence that announced a version is a
skip neither `verify-suite-coverage.R` nor `verify-depends-only.R` could
attribute.

Guarding on a package DESCRIPTION does not suggest is an error, not an answer.
`suggest_available()` already refuses a package `optional_suggests()` does not
name, but a vignette and an example have no such registry, and a typo or a
dependency that moved to `Config/Needs/website` would otherwise guard on
installation alone while reading as protection.

Two scans in `test-documentation.R` are what keep this from decaying, and they
scan rather than list, for the reason every other gate here derives rather than
lists. Each runs over three sources — the Rd topics, the vignette sources, and
both halves of the README: no page may name a version-blind guard, and a page
using the guard must source it and vice versa. The Rd half reads `man/` when it
is present and `tools::Rd_db("marginplyr")` otherwise, so it holds under
`R CMD check` too; the vignette half is repository-only, because `R CMD check`
unpacks the tarball beside the `.Rcheck` directory rather than inside it; the
README is read from the repository where there is one, and otherwise from the
installed `README.md`, which exists only from R 4.6.0 — "Package `README.md`
files are now installed and featured in HTML help" — while `DESCRIPTION`
supports 4.1.0, so an oldrel job checking a tarball reaches neither half. The
README is in the set because it is installation documentation, which is where a
version-blind test is likeliest to be written in the first place — the same
reason `verify-site.R` forbids `installed.packages` anywhere on the rendered
site. Prose that needs to name a version-blind call has to spell it some other
way — the scan is deliberately blunt.

A source is added where it is reachable rather than skipped for where it is
not, since a skip naming no withheld backend is what `verify-backend.R` fails a
job over. Reaching nothing at all is the other case, and
`documentation_sources()` stops on it: every scan iterates over that set, so a
set that arrived empty is a set that passes.

### Release matrix

`.github/workflows/release-matrix.yaml` checks one built tarball rather than
the working tree, because a check that passes on the development tree can be
passing on a file the tarball does not ship. Each of its `backend` jobs is for
one member of `optional_backends()`, and installs that member plus the
companions its entry declares. `MARGINPLYR_REQUIRED_SUGGESTS` names all of
them, so any one of them failing to install fails the job instead of skipping
its tests.

Two words are in use in this section and in *Optional-dependency guards*
above, and they name different sets (#185). A *Suggest* is an optional package
the test suite guards on — an entry in `optional_suggest_spec()`. A *backend*
is the narrower thing a generated job exists for: the subset
`optional_backends()` returns, which is what those jobs iterate over. `DBI` is
a Suggest and not a backend — it has no job of its own — while `data.table` is
both, and what its job proves is an input class rather than a query
translation. A Suggested package only a vignette or an example guards on is
neither, and belongs in neither place: `tidyr` reaches
`marginplyr_suggest_available()` directly, and DESCRIPTION is the whole of its
registration.

`MARGINPLYR_REQUIRED_SUGGESTS` is what makes skipping safe everywhere else. A
test behind an optional package skips when it is missing, which is correct for
CRAN's minimal flavors but means a green job proves nothing about it. Every
such test goes through `skip_if_suggest_absent()` or `suggest_available()` from
`tests/testthat/helper-optional-backends.R` — never `skip_if_not_installed()`
or `rlang::is_installed()` directly, since those cannot be told to fail — and
never `requireNamespace()` either, for the separate reason in
*Optional-dependency guards* above. (`skip_if_not_installed("dbplyr")` is not
an exception: dbplyr is an Import, so it is never absent.)

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
each installs one backend, plus whatever companions its entry declares, and
withholds everything else, so between them they execute every test that
requires at most one. A test requiring two is executed by none of them and
skips in all of them — including in a job that happens to hold both, because
`verify-suite-coverage.R` hides all but one whatever a job installs, which is
why the guarantee does not rest on a job's package count. Splitting such a test
is the fix, and the
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

Registering an optional Suggest with the test suite means editing two places.
Every partial edit fails loudly:

1. `optional_suggest_spec()` in `tests/testthat/helper-optional-backends.R`,
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
   installs alongside the entry, which is how `DBI` reaches the driver jobs
   without a job of its own. It is also how an entry declares what its own
   dependencies drag in, and there the companion is a tracked entry rather
   than an untracked one: dtplyr declares `Imports: data.table`, so its job
   installs data.table whichever way the entry is written, and leaving it
   undeclared makes `verify-library-isolation.R` fail the job for a leak that
   is really the requested package's own closure.
2. the `skip_if_suggest_absent()` or `suggest_available()` call in the tests.
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

### Queries against a lazy input

marginplyr sends no query that reads a lazy input's data unless the caller
asked for one, with the two exemptions ADR 0020 enumerates. A change that adds
a `collect()`, `compute()`, or any other execution entry point to `R/` is
therefore a change to a public contract, not an implementation detail, and is
justified against that ADR in the same commit — or made conditional on an
argument the caller sets.

The snapshots in `test-query-policy.R` are what make such a change visible
rather than assumed: one records the internal functions that reach an execution
entry point, one records the set of entry points scanned for, and one records
which backend kinds hold `collect_selection_proxy`. A scan that stopped
covering an entry point would otherwise report a clean result, which reads
exactly like a package that added none. They run only where `NOT_CRAN` is set.

Deciding by billing model is the option that was rejected, and
`investigation/query-cost-across-lazy-backends.md` records why: four models are
in use across the backends dbplyr reaches, two of them appear under one
`grouping_backend()` kind, and `is.data.frame()` is the only predicate that
answers whether an external system is involved at all.

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
