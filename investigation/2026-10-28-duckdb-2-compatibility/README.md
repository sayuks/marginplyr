# DuckDB 2.0 compatibility validation

Investigated: 2026-10-28
Base: `cb957d9be09440796defde808bf6f04006edd6dd` (latest `origin/main` fetched before validation)
Package-affecting snapshot: `5eaf65aa527c6d0b8cd503b45637450fa1b8843e`

## Result and scope

On macOS ARM64 / R 4.6.1, the supported Margin paths exercised with the
official R `duckdb` 2.0.0 and its bundled DuckDB v2.0.0 engine passed the
comparisons below. No change in `R/` was needed. The implementation adds a
permanent NULL-key regression test and reproducible, separately invoked
Arrow/parser/type/storage probes. The dependency floor stays at 1.5.5.

This is evidence for the measured client/engine/dependency/platform
combinations. Linux, Windows, other R releases, Arrow 13, custom builds,
remote DuckDB services, and new feature adoption were not tested locally.
The repository's existing cross-platform CI remains the authority for its
own runs; a green run on an unspecified DuckDB version is not evidence for
2.0.0 specifically.

## Versions and provenance

| R client | Bundled engine | Engine source ID | Source |
| --- | --- | --- | --- |
| 1.5.5 | v1.5.5 | `d8cdaa33fda` | Existing developer installation, used without upgrading it |
| 1.5.6 | v1.5.6 | `069cc9f9b5` | Posit CRAN snapshot dated 2026-10-20, macOS Sonoma ARM64 / R 4.6 binary |
| 2.0.0 | v2.0.0 | `d54cbbe3df` | CRAN, macOS Sonoma ARM64 / R 4.6 binary |

Each client was loaded in a separate R process. Version-specific libraries
were prepended to `R_LIBS_USER`; the existing developer installation was
left unchanged. Shared dependencies were R 4.6.1, DBI 1.3.0, dbplyr 2.6.0,
dplyr 1.2.1, Arrow 25.0.1, and testthat 3.3.2. The stream harness also used
nanoarrow 0.9.0 in a dedicated shared library. The macOS release was Tahoe
26.6.2; the R platform was `aarch64-apple-darwin23`.

The retrieved binaries and SHA256 values were:

- [CRAN duckdb 2.0.0](https://cran.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/duckdb_2.0.0.tgz):
  `0dae0380a36a64d49d230cd8d52792b8ed6a643565a180b9af3f553d31406adf`.
  This matched the CRAN `PACKAGES` index before installation.
- [Posit snapshot duckdb 1.5.6](https://packagemanager.posit.co/cran/2026-10-20/bin/macosx/sonoma-arm64/contrib/4.6/duckdb_1.5.6.tgz):
  `93fb1d4b0f09dc51524a9f435e2a49b532b3f0d0fc4e37023dfcefe41820c035`.
  The snapshot index named version 1.5.6.

The CRAN binary's version, installed-package checksum check, and `PRAGMA
version` were checked independently. The r-universe development package was
not substituted for the stable release. C++17 source builds were not run.

## Test results

| Check | 1.5.5 | 1.5.6 | 2.0.0 |
| --- | --- | --- | --- |
| Unchanged focused suite, six files | 2,329 expectations passed | 2,329 expectations passed | 2,329 expectations passed |
| Final `test-duckdb-scalar-missing.R`, including the new test | 636 expectations passed | 636 expectations passed | 636 expectations passed |
| Additional share/query-policy/SQL/audit/metamorphic tests | Not repeated as a group | Not repeated as a group | 7,786 expectations passed |
| Standalone acceptance groups | 4 passed | 6 passed | 6 passed |

All listed testthat runs had zero failures, errors, warnings, and skips.
The focused six files were `duckdb-scalar-missing`, `duckdb-storage`,
`grouping-backends`, `factor`, `margin-order`, and `sql-env-label-collision`.
The final scalar file's count includes its existing tests; it is not 636
new expectations. Counts across rows overlap and must not be added.

The added test exercises untyped `NULL` and explicit INTEGER, VARCHAR,
DATE, and DECIMAL NULL keys, with empty and nonempty input. It observes
public summary/expansion results, direct collection, `compute()` followed
by collection, and zero-row collection. SQL type is compared with the
input's declared type: 1.5.x's INTEGER representation and 2.0's NULL type
are both admitted. The test also checks row counts, `.id`, ordinary summary
values, and expansion payloads. It does not assign a new type promise to
arbitrary user summaries.

### Arrow and parser acceptance

[`acceptance.R`](acceptance.R) starts a fresh R worker per group with a
60-second timeout. Its successful groups covered:

- Arrow Table, RecordBatch, InMemoryDataset, and queries, including a
  filtered projection, registered into DuckDB and used for Margin summaries
  and expansions. The input included dictionary/factor keys and missing data.
- SQL NULL output through `arrow::to_arrow()` and, from 1.5.6 onwards,
  `duckdb::to_arrow_stream()`, including empty output, schema equality, and
  explicit materialization before reuse as an Arrow Margin input.
- Direct readers and readers embedded in a query, grouped query, union, or
  right input of a join. Summary/expansion refused them with the reader
  diagnostic; inspection succeeded, and subsequent consumption proved that
  both original rows remained unread.
- A 1,000,005-row stream read to exhaustion across multiple batches. The
  independent count and arithmetic total were checked. Reusing its own
  connection invalidated an unfinished stream with an error; querying a
  different connection preserved all rows. These two stream groups were
  not applicable to 1.5.5, which has no `to_arrow_stream()` API.
- Changed SQL literals and regex semantics, and division/remainder by zero.
  R `%/%` and `%%` inside Margin summaries were compared with ordinary
  dbplyr summaries for the same result or external error class.

`arrow::to_arrow()` returned a `RecordBatchReader` in the measured Arrow
version. Its documentation's materialization statement concerns the DuckDB
result before export; it does not mean the function returns an Arrow Table.
The reusable Table in these checks was created explicitly with
`arrow::as_arrow_table()`. This corrects the earlier plan's Table/reader
conflation. The R APIs remain available; Python method removals do not
describe the R interface. See the [R stream documentation](https://r.duckdb.org/reference/to_arrow_stream.html)
and [tagged R release notes](https://github.com/duckdb/duckdb-r/blob/v2.0.0/NEWS.md).

### Type observations and upstream limits

[`type-observations.R`](type-observations.R) saves observed schemas, values,
materialized results, and conditions. It is an observation script, not a
pass/fail suite: unsupported-type errors are retained as evidence.

- Untyped NULL remained SQL NULL in 2.0 and INTEGER in both 1.5.x controls.
  Explicitly typed NULL, DECIMAL, BIGINT, TIMESTAMP, and TIMESTAMPTZ worked
  through the probed summary/expansion and materialization paths.
- The SQL spelling `TIMESTAMPTZ_NS` worked in 2.0 with empty/nonempty
  input through both verbs and `compute()`. `TIMESTAMP_TZ_NS` is the name
  used in the R release notes, not an accepted SQL alias in this build.
  R reported its nanosecond-resolution coercion warning; no extra precision
  guarantee is added here.
- TUPLE summaries and materialization worked in 2.0, improving on the older
  clients' materialization failure. Empty STRUCT was accepted by 2.0.
  Expansion with those structured dimensions still reached dbplyr's
  `Unknown input type: list` refusal. Lazy structured-key support is not
  added by this work; see ADR 0016's *typed-missing local packed dimensions*.
- `B'1010'` became BIT in 2.0, and ordinary `DBI::dbGetQuery()` rejected it
  with `Unknown type for column ...: BIT` before any Margin verb could run.
  This is an upstream R vector-conversion limit. `X'AB'` became BLOB and
  passed expansion. The regex and integer-division changes likewise followed
  ordinary DuckDB/dbplyr behavior, not a change authored by marginplyr.

The underlying changes are documented in the [2.0 migration guide](https://duckdb.org/docs/2.0/operations_manual/migration)
and [R release notes](https://github.com/duckdb/duckdb-r/blob/v2.0.0/NEWS.md).
The observations neither enable the legacy parser nor change connection-wide
compatibility settings.

### File storage

[`storage.R`](storage.R) used disposable database files only. A persistent
Margin result created by 1.5.5 was read by 1.5.6, updated by 2.0, and read
again by 1.5.5 with the expected values. A new database created by 2.0 held
the expected persistent result and was rejected by 1.5.5, as documented.
The older client printed version `999` in that error; the test does not
mistake this diagnostic for an independent measurement of the new format.
See [Storage Format](https://duckdb.org/docs/2.0/operations_manual/migration#storage-format).

## Final package boundary

`Rscript tools/review-ready-check.R` completed with exit status 0 on the
package-affecting snapshot above, on its first check attempt:

- test-dependency syntax and all required test Suggests: passed;
- package spelling: passed;
- `jarl check .`: passed;
- package-aware lintr after `pkgload::load_all()`: passed;
- source-tarball `R CMD check --as-cran`: 0 ERRORs, 0 WARNINGs, 0 NOTEs;
- full test suite with `NOT_CRAN=true` and required optional Suggests: passed;
- examples, built/rebuilt vignettes, and manual checks: passed.

The check's test process completed in approximately 582 seconds. No native
rendering retry was needed. The terminal result, rather than partial log
output, established the pass.

The package-affecting snapshot above was checked with the 2.0.0 library first
on `R_LIBS_USER`. Later changes in this branch were confined to excluded
investigation artifacts; the freshness rule is
[`design/agents/local-checks.md`](../../design/agents/local-checks.md).

No generated documentation, installation guidance, dependency floor, optional
backend registration, or production execution entry point changed. The
Arrow/DuckDB integration checks remain outside `tests/testthat` to respect
the single-optional-backend test policy in `AGENTS.md`.

## Reproduction

From the repository root, place exactly one target DuckDB installation first
on `R_LIBS_USER`, followed by the shared harness dependencies and existing R
library. In a fresh process, use that client's exact version:

```sh
Rscript investigation/2026-10-28-duckdb-2-compatibility/acceptance.R 2.0.0 /tmp/duckdb-200-evidence
Rscript investigation/2026-10-28-duckdb-2-compatibility/type-observations.R /tmp/duckdb-200-types.rds
```

The acceptance directory contains the measured client/engine/library/session,
per-worker logs, and `cases.tsv`. The type script requires its output path.
For storage, run `storage.R create PATH` under 1.5.5, `read PATH` under
1.5.6, `update PATH` under 2.0, and `read PATH` under 1.5.5. For a second,
fresh path, run `create PATH` under 2.0 and `reject PATH` under 1.5.5.

The full package boundary is `Rscript tools/review-ready-check.R` on a clean,
committed checkout with the chosen target library inherited by child R
processes. It is intentionally separate from the standalone integration
harness and from the repository's platform and dependency-isolation matrix.
