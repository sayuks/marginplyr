# P3M binaries in GitHub Actions

Investigated on 2026-07-19 for `marginplyr`.

## Findings

- Posit Package Manager (P3M) supports Linux binaries for Ubuntu 24.04
  (Noble), x86_64. Its explicit repository URL includes the distribution,
  architecture, and R release series, for example:
  `https://packagemanager.posit.co/cran/latest/bin/linux/noble-x86_64/4.6`.
- P3M currently serves Arrow and DuckDB from that repository as binaries. HEAD
  requests for Arrow 25.0.0 and DuckDB 1.5.4.3 returned
  `X-Package-Type: binary` and `X-Package-Binary-Tag: 4.6-noble`.
- The generic `__linux__/noble/latest` repository also content-negotiates
  binaries from R's HTTP user agent. P3M documents both this mechanism and the
  explicit environment URL.
- `r-lib/actions/setup-r@v2` uses public P3M by default on supported x86_64
  Linux runners. Its `http-user-agent: release` input is useful for an R-devel
  job because P3M publishes binaries for release R series, not R-devel.
- pak still resolves CRAN packages against the running R-devel series. Setting
  `PKG_R_VERSIONS` or only replacing the repository URL did not stop pak from
  selecting source builds in the observed workflow. Base R's
  `install.packages()` did install the P3M Linux binaries, as documented by
  Posit. pak then keeps packages whose versions are already installed.
- `usethis::use_github_action("check-standard")` is a useful way to copy the
  current standard r-lib/actions check workflow. It is scaffolding, not a P3M
  binary installer, and rerunning it would overwrite project-specific workflow
  changes. The existing workflow should therefore keep its explicit P3M step.

## Implemented approach

For the Ubuntu R-devel matrix job:

1. Keep `http-user-agent: release` in `setup-r`.
2. Derive the current release minor version from that user agent, avoiding a
   hard-coded R version.
3. Construct the explicit Noble x86_64 P3M repository URL.
4. Preinstall the package's check dependencies from P3M with base R
   `install.packages()`.
5. Run `setup-r-dependencies` for system requirements and any missing package.
6. Before and after pak runs, assert that Arrow and DuckDB have `Built` metadata
   from the release R series. A source build on R-devel therefore fails the job.
7. Run the full package check, including Arrow and DuckDB backend tests.

## Primary sources

- [Posit Package Manager: Serving Package Binaries](https://packagemanager.posit.co/__docs__/admin/serving-binaries.html)
- [r-lib/actions setup-r documentation](https://github.com/r-lib/actions/blob/v2/setup-r/README.md)
- [r-lib/actions standard check workflow](https://github.com/r-lib/actions/blob/v2/examples/check-standard.yaml)
- [pak configuration](https://pak.r-lib.org/reference/pak-config.html)
- [pak FAQ](https://pak.r-lib.org/reference/faq.html)
- [usethis `use_github_action()`](https://usethis.r-lib.org/reference/use_github_action.html)
