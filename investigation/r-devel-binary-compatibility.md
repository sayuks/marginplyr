# R-devel binary compatibility

Investigated: 2026-08-03

This note establishes what actually guarantees that Arrow and DuckDB binaries
built for the release R series keep working when `R-CMD-check.yaml`'s
`ubuntu-latest` / `r: devel` job loads them under R-devel. Primary sources are
the R manuals and help pages shipped with the locally installed R 4.6.1
(`/Library/Frameworks/R.framework/Resources`), the R source tree at
`svn.r-project.org`, CRAN Repository Policy, and Posit Package Manager's own
documentation.

## Findings

### 1. R states no binary-compatibility guarantee, and breaks at minor versions

- There is no stated ABI or binary-compatibility guarantee anywhere in the R
  manuals. Searching *Writing R Extensions*, *R Internals*, and *R Installation
  and Administration* for a compatibility guarantee returns nothing: no manual
  says binaries remain valid across any range of R versions, and none states a
  granularity at which compatibility is promised. **The absence is the finding.**
  Cite the absence, not an inferred rule.
- The strongest positive statement is descriptive rather than a promise. *R
  Installation and Administration* §6.3.2 (macOS): "Note that most binary
  packages which include compiled code are tied to a particular series (e.g. R
  4.5.x or 4.4.x) of R." That names the **minor** series (`x.y`), not the major
  version, as the unit a binary is tied to.
- R's own tooling uses the same granularity. `?update.packages` documents
  `checkBuilt`: "If `TRUE`, a package built under an earlier major.minor version
  of R (e.g., '3.4') is considered to be 'old'", and its Details section says
  `old.packages` reports packages "that were built under an earlier minor
  version of R (for example built under 3.3.x when running R 3.4.0)". R treats
  a minor-version bump as reason enough to reinstall.
- R's own NEWS records repeated ABI breaks at `x.y.0` **minor** releases, not
  only at major ones:
  - R 3.5.0: "The object header layout has been changed to support merging the
    ALTREP branch. This requires re-installing packages that use compiled code."
    (`doc/NEWS.3`, CHANGES IN R 3.5.0, C-level facilities.) This affected every
    package with compiled code.
  - R 3.4.0: "Packages which register native routines for `.C` or `.Fortran`
    need to be re-installed for this version"; and separately, on removal of the
    `styles` member of `R_CMethodDef`, "Packages using these will need to be
    re-installed for R 3.4.0." (`doc/NEWS.3`.)
  - The graphics engine version `R_GE_version` has been bumped at R 4.1.0 (to
    14), R 4.2.0 (15), R 4.3.0 (16), and R 4.6.0 (17), each time with the note
    "packages that provide graphics devices should be reinstalled"
    (`doc/NEWS`). Every one of those is a minor release.
- The rationale is documented, if not the guarantee. *Writing R Extensions*
  §6.23 ("Moving into C API compliance") repeatedly says the internal
  representation is R's to change — for example on attributes: "The current
  implementation (R 4.5.0) represents attributes internally as a linked list. It
  may be useful to change this at some point, so external code should not rely
  on this representation"; and on `CHARSXP` encoding bits: "The structure of the
  object header is, however, internal to R and may have to change in the
  future."

**Conclusion:** compatibility can and does break at minor versions. R publishes
no guarantee at any granularity; the operative unit in practice, per R-admin
§6.3.2 and `?update.packages`, is the `x.y` minor series.

### 2. What the load-time version gate actually checks

Two separate checks run, in two different functions. `library(pkg)` runs both,
because it calls `loadNamespace()`.

**`base::library()` → its local `testRversion()`** (read by deparsing
`base::library` under R 4.6.1) checks, in order:

1. `stop()` if `Meta/package.rds` has no `Built` field ("package %s has not been
   installed properly").
2. `stop()` if `Built$R < "3.0.0"` ("was built before R 3.0.0: please
   re-install it").
3. `stop()` if any `Rdepends2` entry from DESCRIPTION's `Depends: R (...)` is
   unsatisfied.
4. **`warning()`** — not an error — if `R_version_built_under > current`, that
   is, only if the package was built under an R *newer* than the running one
   ("package %s was built under R version %s").
5. On Windows only, `stop()` if the built platform is not `mingw`; and `stop()`
   if a sub-architecture is in use and `libs/<arch>` is missing.

There is no check at all for the case at hand — a package built under an
*older* R than the running one. The background note's reading of the gate is
correct.

**`base::loadNamespace()`** applies a different and stricter pair:

1. `stop()` if `Built$R < "4.0.0"` ("package %s was installed before R 4.0.0:
   please re-install it") — note the threshold is 4.0.0 here, not the 3.0.0 that
   `library()` uses. It has no "built under a newer R" warning.
2. The real gate. For any package with `NeedsCompilation: yes` or a `libs`
   directory, it reads `Meta/features.rds`, takes `features$internalsID`, and
   errors if it differs from `.Internal(internalsID())`
   (`src/library/base/R/namespace.R`, R SVN trunk, around lines 494–518):

   ```
   ## Check that the internals version used to build this package
   ## matches the version of current R. Failure in this test
   ## should only occur if the R version is an unreleased devel
   ## version or the package was build with an unrelease devel
   ## version.  Other mismatches should be caught earlier by the
   ## version checks.
   ```

   The error is `"package %s was installed by an R version with different
   internals; it needs to be reinstalled for use with this R version"`.

- The value compared is a compile-time constant in R's **private** header
  `src/include/Defn.h`, with the only statement of its contract being its own
  comment: "UUID identifying the internals version -- packages using compiled
  code should be re-installed when this changes"; as read on 2026-08-03 the
  definition was
  `#define R_INTERNALS_UUID "2fdf6c18-697a-4ba7-b8ef-11c0d92f1327"`.
- **The intended contract is documented nowhere else.** `?library`'s Note
  section documents only that an installed package is detected by a `Built:`
  field and that Unix-alike platform similarity is checked; it says nothing
  about R-version comparison. `internalsID`, `R_INTERNALS_UUID`, "different
  internals", and `features.rds`'s contents do not appear anywhere in R NEWS,
  including the R-devel NEWS. *R Internals* §4.1 lists `features.rds` among the
  `Meta` files but describes no field, and states outright: "The structure of
  these files (and their existence and names) is private to R, so the
  description here is for those trying to follow the R sources: there should be
  no reference to these files in non-base packages."

**Conclusion:** the version gate that matters is undocumented and explicitly
private. Its behaviour is verifiable only from the R source, and R reserves the
right to change it. That is the honest status; nothing in the documented API
underwrites the current arrangement.

### 3. The failure mode was a clean error on 2026-08-03 — but only because of an undocumented guard

The working hypothesis (crash or unclear failure) is **refuted for the specific
case in question, against the R 4.6.1 and trunk sources read for this note**,
and confirmed as the general position.

- Refuted for this case: when R's internals UUID differs, `loadNamespace()`
  stops with a clear, actionable message naming the package and the required
  fix. It is a hard `stop()`, and it is reached before the package's shared
  object is loaded. A CI job would see a plain error, not a segfault. This is
  precisely the R-devel-versus-release scenario the source comment names.
- Confirmed in general: R's documented position on calling into internals across
  versions is that the outcome is process termination, not a diagnosable error.
  *Writing R Extensions* §5.5: "It is not portable to call compiled code in R or
  other packages via `.Internal`, `.C`, `.Fortran`, `.Call` or `.External`,
  since such interfaces are subject to change without notice and will probably
  result in your code terminating the R process."
- And R's own description of memory-corruption symptoms is that they are
  delayed and unclear. *Writing R Extensions* §4.3: "Often the crash appears
  long after the invalid memory access: in particular damage to the structures
  which R itself has allocated may only become apparent at the next garbage
  collection (or even at later garbage collections after objects have been
  deleted)."
- The guard is coarse and not the only one. It fires only when R's developers
  choose to bump the UUID. A parallel, narrower guard exists for graphics
  devices: *R Internals* §6.1.1 documents `R_GE_checkVersionOrDie(R_GE_version)`
  — "If a graphics device calls `R_GE_checkVersionOrDie(R_GE_version)` it can
  ensure it will only be used in versions of R which provide the API it was
  designed for and compiled against." That is opt-in on the device's part.
  Neither guard is a general ABI check.

**Conclusion:** an ABI break that R's maintainers record by bumping
`R_INTERNALS_UUID` produces a clean, immediate load error. An ABI break they do
not record has, by R's own wording, an undefined and probably fatal outcome. The
clean error is a convention maintained by R's developers, not a guarantee.

### 4. Posit Package Manager's stated scope

Quoted verbatim from *Serving Package Binaries* (Package Manager docs version
2026.06.0), section "Supported R Versions":

- "Package Manager provides binary packages for the current R version and the
  four previous minor versions. For example, if R 4.6 is the current version,
  binaries are available for R 4.6 (current), 4.5, 4.4, 4.3, and 4.2."
- "Binaries are built against the oldest compatible patch release within each
  minor version series. This ensures that binaries are compatible with any patch
  release in that series (e.g., binaries for R 4.6 will work with R 4.6.0
  through R 4.6.X)."

Two things follow:

- P3M does not mention R-devel or unreleased R anywhere in that document. There
  is no positive statement that it declines to build for R-devel; there is
  simply no such series in the supported list, which is defined by released
  minor versions. The `investigation/p3m-binary-actions.md` finding that "P3M
  publishes binaries for release R series, not R-devel" is consistent with the
  documentation, though the documentation states it by omission.
- **P3M's applicability claim stops at the patch level of one minor series.**
  Its only compatibility promise is "binaries for R 4.6 will work with R 4.6.0
  through R 4.6.X". Loading a 4.6 binary under R 4.7.0-devel is outside anything
  P3M asserts. Posit is not the warrant for this arrangement working.
- When no matching binary exists, P3M does not fail — it silently substitutes
  source: "In cases where the binary package is unavailable or unsupported on
  the user's system, Package Manager falls back to serving the source version of
  the package." This is the failure mode the workflow's `Built` assertion
  targets; see the decision material below.

### 5. CRAN r-devel flavors and unavailable Suggests

- R CMD check's behaviour is switchable, and CRAN switches it off. *R Internals*
  (Tools chapter, check environment variables) documents
  `_R_CHECK_FORCE_SUGGESTS_`: "If true, give an error if suggested packages are
  not available. Default: true (but false for CRAN submission checks)." The same
  chapter adds: "the incoming checks also use `_R_CHECK_FORCE_SUGGESTS_=FALSE`
  since some packages do suggest other packages not available on CRAN or other
  commonly-used repositories."
- CRAN Repository Policy requires the conditional use that makes skipping the
  correct outcome: "A package listed in 'Suggests' or 'Enhances' should be used
  conditionally in examples or tests if it cannot straightforwardly be installed
  on the major R platforms. ('Writing R Extensions' recommends that they are
  *always* used conditionally.)"
- *Writing R Extensions* §1.1.3.1 ("Suggested packages") states the premise
  directly: "Note that someone wanting to run the examples/tests/vignettes may
  not have a suggested package available (and it may not even be possible to
  install it for that platform)." It goes on to recommend
  `_R_CHECK_DEPENDS_ONLY_=true` and `_R_CHECK_SUGGESTS_ONLY_=true` runs: "It is
  recommended that a package is checked with each of these set, as well as with
  neither."
- **Tests skipping because a Suggested package is unavailable on a flavor is not
  a submission problem.** With `_R_CHECK_FORCE_SUGGESTS_=FALSE` there is no
  error to report, and conditional use is what policy asks for. The submission
  risk is the opposite one: policy says "Packages for which `R CMD check` gives
  an 'ERROR' when a new R *x.y.0* version is released will be archived", so an
  unconditional dependency on a Suggest that a flavor lacks is what causes
  archiving. Note that this archiving trigger is keyed to `x.y.0` — a **minor**
  release — which matches the granularity established in finding 1.
- CRAN's own check-flavor page (`check_flavors.html`) lists the r-devel flavors
  (`r-devel-linux-x86_64-debian-clang`, `-debian-gcc`,
  `r-devel-linux-x86_64-fedora-clang`, `-fedora-gcc`, `r-devel-windows-x86_64`)
  with compiler and locale details only. It publishes no statement about which
  Suggests are installed on each flavor. Which specific packages are present on
  a given r-devel flavor at a given time is therefore **not settled from a
  primary source**; the strongest available evidence is the policy and check
  documentation above, which assume unavailability is normal and require code to
  tolerate it.

## Decision material for CI

### How long the arrangement holds, and what ends it

The arrangement rests on one value. Measured against the R source tree on
2026-08-03:

| Source | `R_INTERNALS_UUID` |
|---|---|
| `branches/R-4-1-branch` (4.1.3 Patched) | not present |
| `branches/R-4-2-branch` (4.2.3 Patched) | `2fdf6c18-697a-4ba7-b8ef-11c0d92f1327` |
| `branches/R-4-3-branch` (4.3.3 Patched) | `2fdf6c18-...` |
| `branches/R-4-4-branch` (4.4.3 Patched) | `2fdf6c18-...` |
| `branches/R-4-5-branch` (4.5.3 Patched) | `2fdf6c18-...` |
| `branches/R-4-6-branch` (4.6.1 Patched) | `2fdf6c18-...` |
| `trunk` (4.7.0 Under development) | `2fdf6c18-...` |

On the same date, local R 4.6.1 reported `.Internal(internalsID())` as
`2fdf6c18-697a-4ba7-b8ef-11c0d92f1327`, and the `Meta/features.rds` of every
locally installed compiled package — including `arrow` — recorded the same
value. R-devel's internals UUID was therefore **identical** to the release
series', and that, and nothing else, is why release binaries loaded under
R-devel. `R-CMD-check.yaml`'s R-devel comment is authoritative for what that
job relies on; this note is not.

The mechanism was introduced between R 4.1.3 and R 4.2.3, and as of 2026-08-03
its value had not changed since. The
`0310d4b8-ccb1-4bb8-ba94-d36a55f60262` constant in `namespace.R` is a sentinel
assigned to packages installed before `features.rds` existed, not a superseded
real value.

**The specific event that ends the arrangement is a single commit to R-devel
changing `R_INTERNALS_UUID` in `src/include/Defn.h`.** It is not tied to a
release date, a version number, or anything observable from outside the R
sources. It can land on any day of the R-devel cycle. When it lands, every P3M
release-series binary with compiled code stops loading under R-devel
immediately, and no P3M-side change can help, because P3M does not build for
R-devel at all (finding 4). The only remedies then are to compile from source on
R-devel — measured at over 55 minutes for the full Suggests set against a
60-minute job timeout, per `investigation/github-actions-modernization.md` — or
to drop the optional backends from the R-devel job.

Given a four-year history of stability across five minor releases, a break is
not imminent. Given that it is an undocumented private constant with no
deprecation path and no announcement channel, it can happen without warning.
Plan for the job to break abruptly rather than to degrade.

### Is `library(arrow)` / `library(duckdb)` a sufficient detector?

**Yes for this specific failure, and this is a stronger detector than it looks —
but it is not sufficient on its own, and it is redundant with the check that
already runs.**

- It is a real detector. `library()` calls `loadNamespace()`, which runs the
  `internalsID` comparison for any package with `NeedsCompilation: yes` or a
  `libs` directory. Both arrow and duckdb qualify. If R-devel's UUID moves, the
  call errors immediately with a message naming the package. The same is true of
  `requireNamespace("arrow")` and of a bare `arrow::` — the guard is in
  `loadNamespace()`, not in `library()`, so any load path trips it. (Only the
  cosmetic "built under R version" *warning* is exclusive to `library()`.)
- It is not sufficient, for three reasons. First, it detects only breaks that R
  chose to record by bumping the UUID; per finding 3, an unrecorded internals
  change has no clean symptom and may surface as a delayed crash during the
  check rather than at load. Second, a successful `library()` proves the
  namespace loaded, not that the package's compiled paths behave correctly under
  R-devel. Third, it is **already covered**: the workflow runs the full `R CMD
  check` with arrow and duckdb tests enabled, and those tests load both packages
  and exercise them far past a bare `library()` call. Adding an explicit
  `library()` step would surface the same error a few minutes earlier with a
  clearer message, which is a diagnosis convenience, not new coverage.
- What would be sufficient is not available cheaply. Nothing short of building
  the backends against R-devel proves R-devel compatibility of the backends
  themselves. The existing `release-matrix.yaml` `tarball` job on
  `ubuntu-latest` / `r: devel` already covers what actually matters — that
  *marginplyr* works on R-devel — by installing hard dependencies only. Since
  marginplyr is pure R and arrow/duckdb are Suggests, R-devel compatibility of
  the package is fully established there without any binary workaround. The
  binary arrangement in `R-CMD-check.yaml` buys backend coverage on R-devel,
  which is a convenience, not a CRAN obligation: per finding 5, tests skipping
  because a Suggest is absent is the documented, policy-compliant outcome.

### Does the `Built`-metadata assertion catch a different failure mode?

**Yes. The working hypothesis is correct, and the two checks are not
substitutes.**

- The `Built` assertion detects a **silent source build**. Per finding 4, P3M's
  documented behaviour when a binary is unavailable is to fall back to serving
  source, not to fail. `install.packages()` would then compile arrow under
  R-devel, succeed, and produce a package whose `Built` field reads `R 4.7.0`.
  Nothing would error; the job would just take an hour and hit the timeout, or
  quietly stop being the test it was written to be. The assertion that `Built`
  starts with `R <release>.` is the only thing that distinguishes "P3M served
  the binary" from "P3M served source and it happened to compile". It is
  checking a **supply-chain** property.
- The load check detects an **ABI mismatch**. It fires when the binary was
  served correctly but R-devel has moved underneath it. It is checking a
  **compatibility** property.
- Neither implies the other. A source build on R-devel loads perfectly and
  passes any load check while failing the `Built` assertion. A correctly served
  release binary passes the `Built` assertion and fails to load once the UUID
  moves. Keeping the `Built` assertion is right, and it is not made redundant by
  anything the check phase does. Note that it is also the more likely of the two
  to fire, since P3M's source fallback is routine documented behaviour whereas a
  UUID bump has not happened in four years.

### Confidence and what remains uncertain

Confidence is **high** on findings 1, 2, 4, and 5, and on the mechanism in
finding 3. Every load-time behaviour above was read directly from
`base::library`, `base::loadNamespace`, `src/library/base/R/namespace.R`,
`src/main/version.c`, and `src/include/Defn.h`, and the UUID values were read
from the R release branches themselves rather than inferred.

What remains uncertain:

- **Whether `R_INTERNALS_UUID` is bumped reliably.** Its only contract is a
  source comment. There is no primary source stating that R's maintainers commit
  to bumping it for every internals change, and no NEWS entry has ever mentioned
  it. If a change ships without a bump, the guard is silent and finding 3's
  general case applies. This is unsettled and cannot be settled from a primary
  source.
- **When exactly it was introduced.** Bounded to between R 4.1.3 and R 4.2.3 by
  branch inspection; the exact revision was not traced, and no NEWS entry
  records it.
- **Which Suggests are present on any given CRAN r-devel flavor.** CRAN does not
  publish this; see finding 5.
- **Whether any ABI-relevant difference between R 4.6 and R 4.7.0-devel exists
  that the UUID does not cover.** Not determinable without diffing R's internal
  headers across the branch and trunk, which was out of scope here. The
  `R_GE_version` bump to 17 in R 4.6.0 is a reminder that R maintains at least
  one versioned interface independent of the UUID — though it is irrelevant to
  arrow and duckdb, neither of which provides a graphics device.

## Primary sources

- [R Installation and Administration §6.3 "Installing packages"](https://cran.r-project.org/doc/manuals/r-devel/R-admin.html#Installing-packages) — §6.3.2 macOS, "tied to a particular series"
- [Writing R Extensions §1.1.3.1 "Suggested packages"](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Suggested-packages)
- [Writing R Extensions §4.3 "Checking memory access"](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Checking-memory-access)
- [Writing R Extensions §5.5 "Creating shared objects"](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Creating-shared-objects)
- [Writing R Extensions §6.23 "Moving into C API compliance"](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Moving-into-C-API-compliance)
- [R Internals §4.1 "Metadata"](https://cran.r-project.org/doc/manuals/r-devel/R-ints.html#Metadata) — `Meta/*.rds` are "private to R"
- [R Internals §6.1.1 "Device structures"](https://cran.r-project.org/doc/manuals/r-devel/R-ints.html#Device-structures) — `R_GE_checkVersionOrDie`
- [R Internals, "Tools" chapter](https://cran.r-project.org/doc/manuals/r-devel/R-ints.html#Tools) — `_R_CHECK_FORCE_SUGGESTS_`
- [R NEWS](https://cran.r-project.org/doc/manuals/r-devel/NEWS.html) — `R_GE_version` bumps at 4.1.0/4.2.0/4.3.0/4.6.0; R 3.5.0 object header layout; R 3.4.0 native routine registration
- [`src/library/base/R/namespace.R`](https://svn.r-project.org/R/trunk/src/library/base/R/namespace.R) — the `internalsID` check and its comment
- [`src/main/version.c`](https://svn.r-project.org/R/trunk/src/main/version.c) — `do_internalsID` returns `R_INTERNALS_UUID`
- [`src/include/Defn.h`](https://svn.r-project.org/R/trunk/src/include/Defn.h) — the `R_INTERNALS_UUID` definition and its comment
- R 4.6.1 as installed locally: `base::library` (its `testRversion`), `base::loadNamespace`, `?library` Note section, `?update.packages` (`checkBuilt`), `include/Rversion.h`, `include/R_ext/GraphicsEngine.h` (`R_GE_version`)
- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)
- [CRAN check flavors](https://cran.r-project.org/web/checks/check_flavors.html)
- [Posit Package Manager: Serving Package Binaries](https://packagemanager.posit.co/__docs__/admin/serving-binaries.html) — "Supported R Versions"
