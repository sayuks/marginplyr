# Public Margin results at the declared R 4.1.0 floor

Investigated: 2026-09-30
Issue: #744
Package source: `b0a1fa6c77ae8a691c179f7bfd0d73da54325ae0`
Host: macOS arm64

## Result

The exact declared R minimum, 4.1.0, installed and loaded the same source
tarball and 30-package dependency graph used in the [R 4.1.3 investigation][prior].
All 148 declared Depends/Imports/LinkingTo constraints passed both source and
installed-package audits. Five successful public calls returned values, columns,
row counts, and types identical to a fresh R 4.1.3 control. One deliberately
rejected call returned the same condition class and complete diagnostic.
Warnings also matched. The [comparison CSV](2026-09-30-r410-public-results/comparison-r410-r413.csv)
records every check. No product compatibility violation appeared in these paths.

The repository's `DESCRIPTION` declares `R (>= 4.1.0)`. This experiment
supports that exact floor for the selected graph and calls. It does not prove
every backend or dependency combination. Lowering the floor below 4.1.0 is
not supported by this experiment; `R/summary-selections.R` also contains a
native `|>` expression, which R introduced in 4.1.0 ([R release announcement][pipe]).

## Source identity and isolation

The official [CRAN macOS arm64 archive][archive] supplied
`R-4.1.0-arm64.pkg`, SHA-256
`0de3f60670d51f0e9721aecc5f01c0542b8a25b51cb2b537566b8451afb94452`.
It was expanded under `/private/tmp/marginplyr-744-r410`, without installing
over the host R framework. The extracted R wrapper was redirected to its own
framework, and an isolated Makevars linked compiled packages to its own R
library. R 4.1.3 ran from a separate extracted framework and library. Each
fresh probe had only its case library and R's standard library in `.libPaths()`;
each non-base loaded namespace came from that case library. The
[R 4.1.0](2026-09-30-r410-public-results/environment-r410.txt) and
[R 4.1.3](2026-09-30-r410-public-results/environment-r413.txt) environment
records, plus the [R 4.1.0 loaded manifest](2026-09-30-r410-public-results/loaded-manifest-r410.csv)
and [R 4.1.3 loaded manifest](2026-09-30-r410-public-results/loaded-manifest-r413.csv),
show the resolved versions and historical paths.

The marginplyr source tarball was the same `marginplyr_0.1.0.tar.gz` built
from the baseline commit for the earlier investigations, SHA-256
`6ac5800bc60c5edddfb6de1a752f32aeaa8d00abe5e369e215593bb5b2746ba0`.
The [hash-checked installation log](2026-09-30-r410-public-results/install-identity-r410.log)
joins that exact archive to the successful `R CMD INSTALL`, then records the
version, path, and `Built` field loaded in a fresh R 4.1.0 process. The
[source manifest](2026-09-29-r41-public-results/source-manifest.csv) records
all 30 dependency versions, URLs, and hashes; the
[verification log](2026-09-30-r410-public-results/source-verification-r410.log)
confirms the archives used here. Their
[source](2026-09-30-r410-public-results/source-graph-check-r410.csv) and
[installed](2026-09-30-r410-public-results/installed-graph-check-r410.csv)
constraint scans each show 148 valid constraints and zero violations. The
selected graph includes dplyr 1.2.0, dbplyr 2.6.0, and RSQLite 3.53.3.

## Public calls and comparison

The checked-in [probe](2026-09-29-r41-public-results/probe.R) ran in separate
fresh processes against the two isolated libraries. The checked-in
[comparison script](2026-09-29-r41-public-results/compare.R) compared its
serialized results. The complete [R 4.1.0](2026-09-30-r410-public-results/probe-r410.log)
and [R 4.1.3](2026-09-30-r410-public-results/probe-r413.log) output logs retain
the observed rows, types, warnings, and diagnostic.

| Call | R 4.1.0 result | R 4.1.3 comparison |
| --- | --- | --- |
| `inspect_grouping()` over `rollup(region, store)` | Three plan rows | Identical |
| Local `summarize_with_margins()` | Six rows, Grand total 10 | Identical |
| Parent and Total shares | Six rows with double shares | Identical |
| Total share without Grand total | `marginplyr_error` | Same class and full diagnostic |
| SQLite lazy `collect()` | Six rows | Identical |
| SQLite lazy `compute()` then `collect()` | Six rows | Identical |

The two SQLite cases used in-memory databases that were disconnected after
each call. Both R versions reported the same dbplyr missing-value aggregation
warning on collection. No skipped optional backend was counted as a pass.

## Replay and limits

The existing [setup script](2026-09-29-r41-public-results/replay-setup.py)
accepts `--r-version 4.1.0` and the baseline tarball to prepare the extracted
R, verify all source hashes, and, with `--install`, build the graph. It was
run without `--install` after this experiment to verify the recorded installer
and all 30 archived source hashes. The graph audits use
[`audit-graph.R`](2026-09-29-r41-public-results/audit-graph.R), and the
marginplyr source identity was checked with
[`verify-install-source.sh`](2026-09-29-r41-public-results/verify-install-source.sh).
For a fresh replay, use separate scratch roots for R 4.1.0 and 4.1.3, run
the setup script with `--install` for each, then run `probe.R` with each
case library and `compare.R` over the resulting `results.rds` files. The
absolute paths in the evidence files are historical observations, not required
installation locations.

This selected graph and public-call probe do not establish the full suite on
R 4.1.0, optional Arrow/dtplyr/duckdb paths, other operating systems, or
untested dependency combinations. R 4.1.1–4.1.2 and R 4.2–4.4 remain outside
the version-specific experiments. The user R library, existing databases,
and permanent CI configuration were unchanged.

[prior]: 2026-09-29-r41-public-results.md
[archive]: https://cran.r-project.org/bin/macosx/big-sur-arm64/base/
[pipe]: https://stat.ethz.ch/pipermail/r-announce/2021/000670.html
