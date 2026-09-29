# Correcting the required dplyr floor

Investigated: 2026-09-29
Source: issue #743; working tree based on `14e68d7`

## Versioned upstream boundary

The [dbplyr 2.6.0 DESCRIPTION][db-desc] declares `dplyr (>= 1.1.2)`,
while its [NAMESPACE][db-ns] imports `dplyr::filter_out`.
The [dplyr 1.1.2 NAMESPACE][dplyr-old] does not export that symbol;
the [dplyr 1.2.0 NAMESPACE][dplyr-new] does, and the
[dplyr 1.2.0 changelog][dplyr-news] lists it as a new function. The
[dbplyr 2.6.0 release notes][db-release] describe the new translation.
As checked on this date, dbplyr's development `DESCRIPTION` still declared
`dplyr (>= 1.1.2)`, and 2.6.0 was its latest release. These sources establish
1.2.0 as the effective dplyr minimum for dbplyr 2.6.0 to load.

## Isolated source-package replay

An R source tarball was built from tracked files at `14e68d7` plus this
branch's `DESCRIPTION`, regression test, and ADR, with vignettes omitted from
the build. Its SHA-256 was
`4c525446ffa89ad683b9d100004cfc5185486ad1be3c4d51d8ec239a2e21dc65`.
Each R process used a fresh temporary installation library, one isolated
dependency library from the [earlier investigation][prior], and R's standard
library. `R_LIBS`, `R_LIBS_USER`, and `R_LIBS_SITE` listed only those temporary
libraries; user profile and environment files were disabled. The normal user
library was not in `.libPaths()` and was not modified. The paths below are
historical observations under `/private/tmp`, not reusable destinations.

| Case | R | Temporary installation library | Dependency library | Outcome |
| --- | --- | --- | --- | --- |
| Historical graph | 4.5.2 | `/private/tmp/marginplyr-743/lib-failing` | `/private/tmp/marginplyr-minver-kalaij/lib-r45stack` | `library(dbplyr)` and source-tarball installation failed: `filter_out` is not exported by dplyr. |
| Corrected near-floor graph | 4.5.2 | `/private/tmp/marginplyr-743/lib-corrected` | `/private/tmp/marginplyr-minver-kalaij/lib-r45near` | Tarball installed and `library(marginplyr)` loaded. |
| Current-dependency control | 4.6.1 | `/private/tmp/marginplyr-743/lib-control` | `/private/tmp/marginplyr-minver-kalaij/lib-control` | The same tarball installed and loaded. |

The historical process loaded dplyr 1.1.2, cli 3.6.1, glue 1.6.2, rlang
1.1.1, tidyselect 1.2.1, and vctrs 0.6.3 from its dependency library;
dbplyr 2.6.0 was installed there but could not load. Installation failed at
`** byte-compile and prepare package for lazy loading`, before a marginplyr
public call. This is an upstream namespace/declaration mismatch, not a
marginplyr summary failure. The earlier investigation audited this graph's
declared constraints; none were violated.

The corrected process loaded marginplyr 0.1.0 from its temporary installation
library and dbplyr 2.6.0, dplyr 1.2.0, cli 3.6.2, glue 1.6.2, rlang 1.1.7,
tidyselect 1.2.1, and vctrs 0.7.1 from its dependency library. The control
loaded marginplyr 0.1.0 from its own installation library and dbplyr 2.6.0,
dplyr 1.2.1, cli 3.6.6, glue 1.8.1, rlang 1.3.0, tidyselect 1.2.1, and
vctrs 0.7.3 from its dependency library. The temporary process checked the
loaded namespace paths for all eight packages. The earlier investigation
audited every installed dependency constraint of the corrected graph, with no
declared violations; it also explains why this graph's other versions are
higher than the nominal marginplyr minima.

In each successful fresh process, a local `summarize_with_margins()` over
`rollup(g)` and a SQLite lazy summary retrieved with `dplyr::collect()`
returned the same three rows: `A = 5`, `B = 5`, `Total = 10`. Both result
objects were `identical()` between corrected graph and control. SQLite's
standard missing-value aggregation warning appeared in both; it did not
affect the result. A first direct build from the checkout included unrelated
worktrees because they were present under that directory, so it was discarded.
The replay used the clean staged source described above. That was an
installation-harness correction, not product evidence.

The full suite, source-tarball check, lint, and coverage belong to the
repository's Review-ready check; this replay isolates the dependency floor
and selected public paths.

[db-desc]: https://github.com/tidyverse/dbplyr/blob/v2.6.0/DESCRIPTION
[db-ns]: https://github.com/tidyverse/dbplyr/blob/v2.6.0/NAMESPACE
[dplyr-old]: https://github.com/tidyverse/dplyr/blob/v1.1.2/NAMESPACE
[dplyr-new]: https://github.com/tidyverse/dplyr/blob/v1.2.0/NAMESPACE
[dplyr-news]: https://dplyr.tidyverse.org/news/index.html#dplyr-120
[db-release]: https://github.com/tidyverse/dbplyr/releases/tag/v2.6.0
[prior]: 2026-09-29-minimum-dependency-compatibility.md
