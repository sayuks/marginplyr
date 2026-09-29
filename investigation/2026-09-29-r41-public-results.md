# Public Margin results on R 4.1.3

Investigated: 2026-09-29
Issue: #744
Package source: `b0a1fa6c77ae8a691c179f7bfd0d73da54325ae0`
Host: macOS arm64

## Result

A source package built from the same baseline commit as the earlier R 4.5.2
near-floor control installed and loaded in a disposable R 4.1.3 environment.
The selected 30-package source graph satisfied all 148 declared
Depends/Imports/LinkingTo constraints before and after installation. Five
public success cases matched a fresh R 4.5.2 near-floor control by
`identical()`; an intentional Total-share rejection matched in condition class
and full diagnostic. No product compatibility violation appeared in the tested
paths.

This establishes the tested R 4.1.3 graph and calls. It does not establish
R 4.1.0–4.1.2 or every optional backend.

## Source and graph

The marginplyr tarball SHA-256 was
`6ac5800bc60c5edddfb6de1a752f32aeaa8d00abe5e369e215593bb5b2746ba0`,
the exact artifact used in the [earlier control][prior]. The R 4.1.3 arm64
installer was downloaded from the [official CRAN archive][r41] with SHA-256
`d973134c1417afeb8c54a8bd0b53ddbc47719e0e30fd9c2122a71d13a57106c4`.
It was expanded under `/private/tmp/marginplyr-744`, without installing it
into the system framework. The R executable and packages ran from that
disposable location. Its binary wrapper was redirected to the extracted
framework, and a case-local `Makevars` linked compilation to the R 4.1.3
library.

[`source-manifest.csv`](2026-09-29-r41-public-results/source-manifest.csv)
records every selected version, source URL, and SHA-256. It starts with the
same core near-floor versions as the R 4.5.2 control: dplyr 1.2.0,
dbplyr 2.6.0, cli 3.6.2, glue 1.6.2, rlang 1.1.7, tidyselect 1.2.1, and
vctrs 0.7.1. DBI 1.3.0 and RSQLite 3.53.3 supply a live, disposable SQLite
retrieval path. The R 4.5.2 near-floor library contained 33 packages; this
R 4.1.3 library contained 30. The three omitted packages were optional
data.table, dtplyr, and duckdb. In particular, that control's duckdb 1.5.5
declares R >= 4.2.0, so including it would make an R 4.1 graph invalid. The
other two were outside this local/SQLite probe, not rejected as incompatible.
All 30 selected package versions matched the corresponding control versions.
No invalid graph was installed.

The [replay setup](2026-09-29-r41-public-results/replay-setup.py) checks all
source hashes before installation; its [verification log](2026-09-29-r41-public-results/source-verification.log)
records each archive. The sources' own DESCRIPTION fields passed the pre-install
[`source-graph-check.csv`](2026-09-29-r41-public-results/source-graph-check.csv).
After installation, [`installed-manifest-r41.csv`](2026-09-29-r41-public-results/installed-manifest-r41.csv)
and [`installed-graph-check-r41.csv`](2026-09-29-r41-public-results/installed-graph-check-r41.csv)
confirmed actual versions, locations, and the same zero-violation result. Both
constraint scans can be rerun with the checked-in
[`audit-graph.R`](2026-09-29-r41-public-results/audit-graph.R).
The [installation status](2026-09-29-r41-public-results/install-status.log)
and [audit log](2026-09-29-r41-public-results/audit-installed-r41.log)
recorded successful completion. A later
[hash-checked installation command](2026-09-29-r41-public-results/verify-install-source.sh)
reinstalled the baseline tarball in both libraries. Its
[R 4.1.3](2026-09-29-r41-public-results/install-identity-r41.log) and
[R 4.5.2](2026-09-29-r41-public-results/install-identity-r45-control.log)
logs join the source commit and SHA-256 to the exact `R CMD INSTALL` input,
successful installation, and fresh-process loaded path, version, and `Built`
field. The public probes were rerun after these installations.

## Public-call comparison

The checked-in [`probe.R`](2026-09-29-r41-public-results/probe.R) ran in
separate fresh processes with only the case library and R's standard library
in `.libPaths()`. The recorded [R 4.1.3](2026-09-29-r41-public-results/environment-r41.txt)
and [R 4.5.2](2026-09-29-r41-public-results/environment-r45-control.txt)
environments, plus their [loaded R 4.1.3](2026-09-29-r41-public-results/loaded-manifest-r41.csv)
and [loaded R 4.5.2](2026-09-29-r41-public-results/loaded-manifest-r45-control.csv)
namespace manifests, identify the R installation and actual loaded dependency
versions and paths. Every non-base loaded namespace came from its case library.
The R 4.5.2 process used the already isolated library and same installed
source artifact from the earlier experiment; it did not alter that library.

| Call | R 4.1.3 outcome | Control comparison |
| --- | --- | --- |
| `inspect_grouping()` over `rollup(region, store)` | Three Grouping-plan rows | Values, columns, types, rows identical |
| Local `summarize_with_margins()` | Six rows; totals 2, 3, 5, 5, 5, 10 | Values, columns, types, rows identical |
| Parent and Total shares | Six rows; share columns are double | Values, columns, types, rows identical |
| Total share without a Grand total set | `marginplyr_error` | Class and full diagnostic identical |
| SQLite lazy result via `collect()` | Six rows | Values, columns, types, rows identical |
| SQLite lazy result via `compute()` then `collect()` | Six rows | Values, columns, types, rows identical |

The [comparison CSV](2026-09-29-r41-public-results/comparison.csv) is generated
by [`compare.R`](2026-09-29-r41-public-results/compare.R) and checks values,
columns, row counts, types, error class, diagnostics, and warnings separately.
The complete [R 4.1.3](2026-09-29-r41-public-results/probe-r41.log) and
[R 4.5.2](2026-09-29-r41-public-results/probe-r45-control.log) output logs
show the values and type summaries. Both SQLite processes reported the same
usual dbplyr missing-value aggregation warning on `collect()`; it did not
change the result. The in-memory SQLite databases were disconnected after
each call.

## Outcome classification and limits

- **No violation found:** source-package installation, loading, Grouping-plan
  inspection, local summary and shares, SQLite retrieval, and the intentional
  rejection all matched the control.
- **Harness-only failure:** the first `cli` 3.6.2 source build used the
  unmodified wrapper inside the extracted R distribution, so its compiler
  searched `/Library/Frameworks/R.framework/Resources/include` (the host R
  4.6 headers) and reported undeclared `Rf_findVar` and `STRING_PTR`.
  Redirecting that wrapper to the extracted R 4.1.3 framework let the same
  source build and load. A later output-directory name `r41` collided with
  the `R41` wrapper on this case-insensitive filesystem; `case-r41` resolved
  that logging-path error. A first R 4.5.2 source-confirmation attempt also
  reached the host R wrapper and crashed during lazy loading; the case-local
  R 4.5 wrapper installed the unchanged source. The
  [failure excerpts and commands](2026-09-29-r41-public-results/harness-wrapper-failures.md)
  record all three harness mistakes. None reached a marginplyr public call.
- **Invalid dependency graph avoided:** duckdb 1.5.5 requires R >= 4.2.0 and
  was omitted. The selected graph itself had no declared violation.
- **Upstream difference and environment block:** none remained in the selected
  graph. The earlier dbplyr/dplyr mismatch required dplyr 1.2.0 and was
  already accounted for by [issue #743's investigation][floor].

The remaining R-version scope is R 4.1.0–4.1.2 and R 4.2–4.4; R 4.5.2 was
only the comparison here, while the earlier investigation also covered R 4.6.1.
Other operating systems, optional Arrow/dtplyr/duckdb paths, the full suite
under R 4.1, and untested dependency combinations remain unverified. No
skipped optional path is counted as passing. Normal R libraries, existing
databases, and permanent CI configuration were unchanged.

[prior]: 2026-09-29-minimum-dependency-compatibility.md
[floor]: 2026-09-29-dependency-floor-correction.md
[r41]: https://cran.r-project.org/bin/macosx/big-sur-arm64/base/R-4.1.3-arm64.pkg
