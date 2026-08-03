# P3M binaries in GitHub Actions

Investigated on 2026-07-19 for `marginplyr`. Revised on 2026-08-04 to record
what `investigation/r-devel-binary-compatibility.md` established about why
loading release-series binaries under R-devel works, and what ends it.

## Findings

- Posit Package Manager (P3M) supports Linux binaries for multiple Ubuntu
  releases and architectures. Its explicit repository URL includes the
  distribution, architecture, and R release series, for example:
  `https://packagemanager.posit.co/cran/latest/bin/linux/noble-x86_64/4.6`.
- P3M currently serves Arrow and DuckDB from that repository as binaries. HEAD
  requests for Arrow 25.0.0 and DuckDB 1.5.4.3 returned
  `X-Package-Type: binary` and `X-Package-Binary-Tag: 4.6-noble`.
- The generic `__linux__/noble/latest` repository also content-negotiates
  binaries from R's HTTP user agent. P3M documents both this mechanism and the
  explicit environment URL.
- `r-lib/actions/setup-r@v2` uses public P3M by default on supported x86_64
  Linux runners and exports an `RSPM` URL selected for the runner's
  distribution. Its `http-user-agent: release` input is useful for an R-devel
  job because P3M publishes binaries for release R series, not R-devel. Using
  this URL avoids hard-coding Ubuntu's release codename or architecture. (P3M
  states that scope by omission rather than positively; see the revisions
  below.)
- pak still resolves CRAN packages against the running R-devel series. Setting
  `PKG_R_VERSIONS` or only replacing the repository URL did not stop pak from
  selecting source builds in the observed workflow. Base R's
  `install.packages()` did install the P3M Linux binaries, as documented by
  Posit. pak then keeps packages whose versions are already installed.
- `usethis::use_github_action("check-standard")` is a useful way to copy the
  current standard r-lib/actions check workflow. It is scaffolding, not a P3M
  binary installer, and rerunning it would overwrite project-specific workflow
  changes. The existing workflow should therefore keep its explicit P3M step.

## Revisions from the compatibility investigation (2026-08-04)

`investigation/r-devel-binary-compatibility.md` reread the primary sources for
the parts of this note that were inferred rather than quoted. Four corrections
and additions:

- "P3M publishes binaries for release R series, not R-devel" is right, but P3M
  states it **by omission**. *Serving Package Binaries* defines its supported
  set as the current released minor version and the four before it, and never
  mentions R-devel. Its only compatibility promise is patch-level within one
  minor series ("binaries for R 4.6 will work with R 4.6.0 through R 4.6.X"),
  so loading a 4.6 binary under 4.7.0-devel is outside anything Posit asserts.
  Posit is not the warrant for this arrangement working.
- What actually makes it work is `R_INTERNALS_UUID`, a compile-time constant in
  R's private `src/include/Defn.h`. `loadNamespace()` compares it against the
  installed package's `Meta/features.rds` for any package with compiled code
  and `stop()`s on a mismatch. The value has been identical across R-4-2
  through R-4-6 and trunk, unchanged since it appeared between 4.1.3 and 4.2.3.
  R publishes no binary-compatibility guarantee at any granularity, and ABI
  breaks have landed at minor versions before (R 3.5.0, R 3.4.0).
- The arrangement therefore ends with **one commit to `Defn.h`**, on any day,
  with no announcement channel and no deprecation path. When the bump happens
  the failure is a clean error naming the package, raised before the shared
  object loads, so it is diagnosable — but that holds only for internals
  changes R chose to record with a bump. Whether the maintainers bump it for
  every such change is unsettled from primary sources; an unrecorded change
  passes the guard, and R's own position (WRE §5.5) is that calling into
  changed internals terminates the process rather than erroring. The mechanism
  is documented nowhere and R-ints §4.1 calls `Meta/*.rds` private to R. Plan
  for an abrupt break, not a graceful one.
- The `Built` assertion in step 6 below is worth keeping for a reason this note
  did not state: P3M's documented behavior when a binary is unavailable is to
  serve source **silently**, without an error. Such a build compiles, loads
  perfectly, and passes any load check, while taking the hour that this
  workflow's 60-minute timeout would cut off with no explanation. The `Built`
  field is the only thing that tells the two apart, and this failure is likelier
  than an internals mismatch.

## Implemented approach

For the Ubuntu R-devel matrix job:

1. Keep `http-user-agent: release` in `setup-r`.
2. Derive the current release minor version from that user agent, avoiding a
   hard-coded R version.
3. Use the P3M `RSPM` URL selected by `setup-r` for the runner's current Linux
   distribution and architecture.
4. Preinstall the package's check dependencies from P3M with base R
   `install.packages()`.
5. Run `setup-r-dependencies` for system requirements and any missing package.
6. Before and after pak runs, assert that Arrow and DuckDB have `Built` metadata
   from the release R series. A source build on R-devel therefore fails the job.
7. Load both packages explicitly once they are installed. This is not coverage
   — the check in step 8 loads them anyway — but it names binary
   incompatibility as the cause immediately instead of leaving it buried in the
   check log.
8. Run the full package check, including Arrow and DuckDB backend tests.

The job is backend coverage on R-devel, not the R-devel compatibility gate.
That gate is `release-matrix.yaml`'s `tarball` job on `ubuntu-latest` /
`r: devel`, which installs hard dependencies plus the test harness and
VignetteBuilder, and needs none of this.

## Primary sources

- [Posit Package Manager: Serving Package Binaries](https://packagemanager.posit.co/__docs__/admin/serving-binaries.html)
- [r-lib/actions setup-r documentation](https://github.com/r-lib/actions/blob/v2/setup-r/README.md)
- [r-lib/actions standard check workflow](https://github.com/r-lib/actions/blob/v2/examples/check-standard.yaml)
- [pak configuration](https://pak.r-lib.org/reference/pak-config.html)
- [pak FAQ](https://pak.r-lib.org/reference/faq.html)
- [usethis `use_github_action()`](https://usethis.r-lib.org/reference/use_github_action.html)
