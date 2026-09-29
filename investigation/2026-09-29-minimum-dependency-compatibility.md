# Minimum-dependency compatibility hunt

Investigated: 2026-09-29
Baseline: `b0a1fa6c77ae8a691c179f7bfd0d73da54325ae0`
Host: macOS arm64; R 4.6.1 and R 4.5.2

## Question and result

Could a dependency graph admitted by marginplyr's package metadata install, load,
and execute ordinary public calls even when its dependencies were older than
those in the development library?

One graph satisfied every declared dependency constraint but could not load
`dbplyr` or `marginplyr`: dbplyr 2.6.0 imported `dplyr::filter_out`, while its
own metadata admitted dplyr 1.1.2, which did not export that function. The
failure also occurred during marginplyr source-package installation. This was a
**declaration/guard mismatch in upstream dbplyr**, exposed by marginplyr's
required import, rather than a failure in a marginplyr public-operation path.
The experiment does not itself choose whether marginplyr should change a bound
or wait for an upstream correction.

A viable R 4.5.2 near-floor graph with dplyr 1.2.0 and dbplyr 2.6.0 installed,
loaded, and passed the exercised Margin calls. The separately tested dtplyr
1.3.2 and Arrow 13.0.0 paths also passed their selected public calls. Across
14 selected control/candidate suites, all 139 recorded comparisons matched.
This is evidence for those graphs and calls, not a claim about every version or
backend.

## Declared boundary and why the failing graph was valid on paper

At the baseline commit, marginplyr's `DESCRIPTION` declared R >= 4.1.0 and
required dbplyr >= 2.6.0, dplyr >= 1.1.1, cli >= 3.4.0, rlang >= 1.1.0,
tidyselect >= 1.2.0, and glue without a version floor. Arrow >= 13.0.0 and
dtplyr >= 1.3.2 were optional `Suggests`. The code and `DESCRIPTION`, rather
than this dated note, govern the repository's later state.

The installed dbplyr 2.6.0 `DESCRIPTION` required dplyr >= 1.1.2, and its
`NAMESPACE` imported `filter_out` from dplyr. The [dplyr 1.2.0
changelog](https://dplyr.tidyverse.org/news/index.html#dplyr-120) identifies
`filter_out()` as a new function; the [dbplyr 2.6.0 release
notes](https://github.com/tidyverse/dbplyr/releases/tag/v2.6.0) report its new
translation. Thus dplyr 1.1.2 meets both declared minimums but lacks the
imported symbol. Only dplyr 1.1.2 was executed in the failing graph; the
changelog and import imply the same load problem for other pre-1.2.0 releases
that satisfy dbplyr's declaration, but those releases were not run here.
Dplyr 1.1.1 was *not* an eligible comparison: it is below dbplyr 2.6.0's
declared minimum. The same transitive-constraint check ruled out independently
pinning cli 3.4.0, glue 1.3.2, rlang 1.1.0, or tidyselect 1.2.0 with dbplyr
2.6.0.

The constraint audit counted 32 installed packages and 160 checked
Depends/Imports/LinkingTo constraints in the failing graph, with zero declared
violations. The working near-floor graph had 33 packages, 164 constraints, and
zero violations; the Arrow graph had 34 packages, 174 constraints, and zero
violations. These counts concern declared constraints, not namespace loadability.

## Source and execution isolation

The package source was archived once from the baseline commit and built into
`marginplyr_0.1.0.tar.gz`. The historical tarball's SHA-256 was
`6ac5800bc60c5edddfb6de1a752f32aeaa8d00abe5e369e215593bb5b2746ba0`.
The same tarball was installed in each final successful configuration; the
failing graph's final installation attempt failed during lazy loading.
R 4.5.2 and R 4.6.1 used separate isolated libraries and fresh R processes.
Each successful process checked that `.libPaths()` contained only its case
library and that R installation's standard library, and that the loaded
marginplyr and selected dependencies resolved inside its case library. The
ordinary user R library, original checkout, existing databases, and system
settings were not changed. The source hash identifies the exact historical
artifact; rebuilding from the commit is the portable source identity and need
not reproduce the tarball byte for byte.

The archived evidence is in the adjacent
[`2026-09-29-minimum-dependency-compatibility/`](2026-09-29-minimum-dependency-compatibility/)
directory:

- [`manifest.csv`](2026-09-29-minimum-dependency-compatibility/manifest.csv):
  all installed packages in nine configurations, including historical isolated
  installation paths. The `r45stack` marginplyr row reflects an earlier copy
  restored by R after the failed reinstall; it is **not** proof that the final
  source-package installation succeeded.
- [`loaded-manifest.csv`](2026-09-29-minimum-dependency-compatibility/loaded-manifest.csv):
  versions and paths verified from fresh processes for the eight successful
  configurations. An empty version means the optional package was absent.
- [`dependency-check-r45stack.csv`](2026-09-29-minimum-dependency-compatibility/dependency-check-r45stack.csv),
  [`dependency-check-r45near.csv`](2026-09-29-minimum-dependency-compatibility/dependency-check-r45near.csv), and
  [`dependency-check-r45arrow.csv`](2026-09-29-minimum-dependency-compatibility/dependency-check-r45arrow.csv):
  the exact declared constraints audited for the important graphs.
- [`comparison.csv`](2026-09-29-minimum-dependency-compatibility/comparison.csv):
  the 139 named comparisons, their control/candidate pairs, execution status,
  and `identical()` outcome.

The original, more extensive scratch bundle was
`/private/tmp/marginplyr-minver-kalaij` on the investigation host. It contained
installation and probe logs, RDS results, helper scripts, and upstream source
archives; that temporary location is not a durable dependency of this note.
The CSVs above preserve the manifests and pairwise results needed to interpret
the findings after the scratch bundle disappears. Their absolute paths are
historical observations, not reusable install destinations.

## Configurations and exercised paths

| Configuration | R and targeted difference | Outcome |
| --- | --- | --- |
| `control` | R 4.6.1; dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3, Arrow 25.0.1 | Working reference. |
| `dtplyr` | R 4.6.1; dtplyr 1.3.2 with other targeted dependencies from `control` | Selected dtplyr summary, derived-step, share, and nest calls matched. |
| `r45control` | R 4.5.2 with then-current compatible binaries | Working R-version control. |
| `r45glue` | R 4.5.2; glue 1.6.2 | Selected calls matched `r45control`. |
| `r45dplyr120` | R 4.5.2; dplyr 1.2.0 | Selected calls matched `r45control`. |
| `r45near` | R 4.5.2; dplyr 1.2.0, dbplyr 2.6.0, cli 3.6.2, glue 1.6.2, rlang 1.1.7, tidyselect 1.2.1, vctrs 0.7.1 | Installed and loaded; selected local, SQLite, DuckDB, dtplyr, and simulated PostgreSQL paths passed. |
| `r45near-dt` | `r45near` with dtplyr 1.3.2 | Selected derived-step, share, and nest paths matched. |
| `r45arrow` | R 4.5.2; Arrow 13.0.0, otherwise a contemporary compatible graph | Table, RecordBatch, derived query, schema selection, `.by`, summary, expansion, and collection passed. Its seven Arrow-specific comparisons matched `control`, which also differed in R version. |
| `r45stack` | R 4.5.2; dplyr 1.1.2, dbplyr 2.6.0, cli 3.6.1, glue 1.6.2, rlang 1.1.1, tidyselect 1.2.1, vctrs 0.6.3 | Declared graph valid; dbplyr and marginplyr failed to load. |

The small pilot first checked installation and loading, then public Grouping
plan inspection, a normal Margin summary, Parent/Total shares, `across()`,
`pick()`, expansion, and result retrieval. The extended probes added nested
Grouping specifications, fixed keys, factor labels, diagnostics, SQLite
`collect()` and `compute()`, a derived dtplyr step, and live in-memory DuckDB
native SQL collection. Arrow probes included a Table, RecordBatch, derived
query, and schema-backed selection. A `simulate_postgres()` probe checked SQL
rendering only: it did not execute against PostgreSQL. Where an unsorted
DuckDB result differed only by row order, the final comparison requested
`.sort = "last"` before comparing. The 139 `comparison.csv` rows are the final
selected comparisons; all have both execution flags and `identical_value` true.
Skipped or absent optional-backend cases are not evidence for those backends.

## Minimal load reproduction and control

After constructing a *valid* isolated graph with dplyr 1.1.2 and dbplyr
2.6.0, this independent upstream call failed in a fresh process:

```r
stopifnot(
  utils::packageVersion("dplyr") == package_version("1.1.2"),
  utils::packageVersion("dbplyr") == package_version("2.6.0")
)
library(dbplyr)
# Error: object 'filter_out' is not exported by 'namespace:dplyr'
```

Replacing the final call with `library(marginplyr)` produced the same missing
export diagnostic. Installing the package source into that graph failed at
`** byte-compile and prepare package for lazy loading` with
`ERROR: lazy loading failed for package 'marginplyr'`. The corresponding
R 4.5.2 graph with dplyr 1.2.0 and dbplyr 2.6.0 installed and loaded; its
public calls passed. This contrast isolates the missing import from
marginplyr's summary logic.

A comparable fresh-process replay, *after* resolving and installing a valid
complete dependency graph in `$CASE_LIB`, is:

```sh
export R_LIBS_USER="$CASE_LIB" R_LIBS="$CASE_LIB" R_LIBS_SITE="$CASE_LIB"
export R_PROFILE_USER=/dev/null R_ENVIRON_USER=/dev/null
"$R_BIN" --vanilla --slave -e 'print(R.version.string); print(.libPaths()); print(packageVersion("dplyr")); print(packageVersion("dbplyr")); library(dbplyr)'
"$R_BIN" CMD INSTALL --library="$CASE_LIB" "$SOURCE_TARBALL"
"$R_BIN" --vanilla --slave -e 'library(marginplyr); print(find.package("marginplyr"))'
```

The first two commands were expected to fail in `r45stack`; run the product
load only if an installed copy is present and verify its source identity.
Run a working control in a separate case library. Version pins alone do not
establish a valid graph: inspect every installed package's declared
Depends/Imports/LinkingTo constraints and the actual loaded paths.

## Environment and harness failures kept separate

On R 4.6.1, old rlang 1.1.1 and cli 3.6.1 source builds failed against the R
headers (including undeclared `PRENV`/`Rf_findVar` calls). Arrow 13.0.0's old
source also failed to build against that R installation's C API. The same
selected versions built under R 4.5.2, where their relevant probes ran. These
R 4.6.1 attempts were **environment blocked**, not marginplyr product failures.

An initial R 4.5.2 build accidentally linked the R 4.6 framework and crashed
at load. An isolated Makevars setting pointing to R 4.5.2 corrected the
linkage. An initial final-comparison process also inherited the R 4.5 Arrow
library under R 4.6 and crashed; rerunning it with the isolated R 4.6 control
library succeeded. Both incidents were **harness failures**, excluded from the
product verdict. No testthat run was used as product compatibility evidence;
the passing public calls ran in new R processes without testthat.

## What the experiment did not establish

R 4.1 through 4.4, other operating systems, every combination of dependency
versions, the full testthat suite under old dependencies, and live PostgreSQL
with an old-dependency graph were not executed. The working Arrow 13.0.0 graph
was on R 4.5.2; its R 4.6.1 source build failure says nothing about
marginplyr's behavior if Arrow 13.0.0 is successfully supplied there.

For a minimum-R follow-up, begin with R 4.1 in a disposable environment,
audit the entire graph before installation, and use the same source commit and
public pilot before the expanded cases. For a live PostgreSQL follow-up, use a
new disposable database and compare `collect()`/`compute()` under a viable
near-floor graph with a current-dependency control. In both cases record R,
server and package versions, loaded locations, source identity, values, types,
columns, row counts, diagnostics, and the exact blocking cause if construction
fails. Neither follow-up should claim a skipped path passed.
