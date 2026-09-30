# Arrow physical layout invariance

Investigated: 2026-09-30
Target: `c14746aed1962f8fda78ed3c05323e73251536b0`
Scope: fixed dependencies, small local synthetic inputs, unmodified package code

## Finding

One public-contract violation was reproduced: expansion of a partition field
used as a Grouping dimension silently restored the source partition value in
rows whose dimension should have held a Margin label. Direct collection,
collection after direct `compute()`, and repeated collection reproduced it.
The source-row count and occurrence counts survived; the grouping values did
not. The 132 failing decision records represented manifestations of this one
failure family, not 132 independent bugs.

An Arrow-only control reproduced the value change after projection of a union.
That established an upstream reproduction, without removing marginplyr's
responsibility for the supported public operation it generated. The investigation
did not decide where to implement a repair or file an upstream report.

No source-row loss, double counting, or violation of the selected declared-type
checks was found in the other equivalent-input cases. That was a result for the
selected cases and versions, not a claim about every Arrow layout.

## Contract and prior evidence

The contract was read from the investigated revision's public reference,
[Grouping model](../CONTEXT.md),
[typed metadata decision](../design/adr/0002-acquire-typed-metadata-once.md),
[Margin order decision](../design/adr/0018-order-margin-results-by-grouping-structure.md),
[lazy-input query policy](../design/adr/0020-ask-before-reading-a-lazy-input.md), and
[declared-output type decision](../design/adr/0033-preserve-declared-types-in-empty-margin-results.md).
For that revision, summary and expansion accepted reusable Table, RecordBatch,
Dataset, and reader-free queries. A reader, including a reader-backed query,
was refused for these verbs; inspection was allowed without consuming it.
Nested Arrow operations required prior collection. Expansion promised to
replace omitted dimensions with the requested label. `.id` promised an R
integer, including zero rows; direct remote Grouping helpers promised numeric
representations and correct bit/mask values, rather than one universal R type.

The two earlier notes were read as historical evidence:
[input shapes and fallback](arrow-r-input-shapes-and-dplyr-fallback.md) and
[zero-row type policy](zero-row-type-policy-2026-09-28.md). Subsequent code and
accepted ADR amendments owned the behavior being tested.

| Existing evidence at the target | What it established | What this investigation added |
| --- | --- | --- |
| `helper-arrow-shapes.R`, backend and query-policy tests | Single Table / RecordBatch; InMemoryDataset; corresponding queries; absorption/refusal and execution-entry behavior | Explicit physical inventory before correctness comparisons |
| Multi-batch reader tests | Two batches in a one-shot reader, source sharing and exhaustion; public refusal | Refusal/inspection controls only; no unsafe shared-reader union execution |
| `test-utils.R` | Real one-file Dataset accepted by the reusable-input assertion | Margin results over real single/multiple Parquet files and Hive partitions |
| Other Arrow Margin tests | Grouping values, metadata, empty types, order and other contracts over memory inputs | Reusable multi-chunk Tables, shifted per-column boundaries, retained empty chunks/files, multiple row groups |

The two-batch reader test was not evidence for a reusable multi-file Dataset.
The scan of test sources found no multi-file or partitioned Margin-result test
at this revision. The one-file assertion test did not execute a Margin result.

Arrow's [data-object article](https://arrow.apache.org/docs/r/articles/data_objects.html)
described chunking as an implementation detail and Tables as named ChunkedArrays.
The [Dataset article](https://arrow.apache.org/docs/r/articles/dataset.html)
described multi-file inputs and partition fields recovered from directory paths.
Both pages identified Arrow R 25.0.1 when read. These were the basis for the
layout transformations; actual equivalence and structure were measured separately.

## Fixed environment and isolation

The original working tree was clean before and after the experiment. A Git
archive of the target was installed into a dedicated library outside that tree.
No product source, permanent test, CI configuration, ordinary library, existing
data, or GitHub resource was changed during the experiment.

| Component | Version / configuration |
| --- | --- |
| R / platform | 4.6.1 / macOS arm64 (`aarch64-apple-darwin23`) |
| Arrow R / C++ | 25.0.1 / 25.0.1; Acero, Dataset and Parquet available |
| dplyr / dbplyr | 1.2.1 / 2.6.0 |
| rlang / vctrs / tidyselect | 1.3.0 / 0.7.3 / 1.2.1 |
| testthat | 3.3.2 |
| Threads | CPU 1, I/O 2; CPU 2 on selected follow-ups |

The [dependency manifest](2026-09-30-arrow-layout-invariance/dependency-versions.csv)
recorded 51 package versions. The local experiment also retained installation
paths and MD5s for 2,162 installed files and the source archive. Those hashes,
and 504 source-Parquet hash observations, matched at the final audit. Paths and
installed-file manifests were retained locally rather than published here.

## Calibration, inputs and independent expectations

A 13-case pilot calibrated A, RecordBatch, B, shifted B, InMemoryDataset, C,
multi-row-group C, D, multi-row-group D, E, and B/D/E with an empty middle
fragment. Every accepted input was collected in a calibration interval before
calling marginplyr and compared with its original fixture: names, schema,
values, duplicate multiplicity, row IDs, and relevant attributes. The pilot
passed before the 177-case main matrix ran.

The comparator preserved row multiplicity. Deliberate deletion, duplication,
wrong occurrence ID, wrong Grouping mask, wrong sum, and a logical empty `.id`
column were all detected. Mean comparisons allowed `1e-12`; integer values,
identifiers and row multiplicity were exact. NA/NaN mean normalization did not
permit missing rows or duplicate rows.

The principal fixture was:

| row_id | k | p | g | h | v |
| --- | --- | --- | --- | --- | --- |
| 1 | X | 1 | A | x | 1 |
| 2 | X | 1 | A | y | 2 |
| 3 | X | 1 | B | x | 4 |
| 4 | X | 1 | B | x | 1 |
| 5 | X | 2 | A | x | 8 |
| 6 | X | 2 | A | y | 2 |
| 7 | Y | 2 | B | y | 3 |
| 8 | Y | 2 | B | y | 9 |
| 9 | Y | 1 | C | x | 3 |
| 10 | Y | 1 | C | x | 5 |
| 11 | Y | 2 | A | x | 11 |
| 12 | Y | 2 | A | y | 5 |

`row_id`, `p`, `v`, and a second measure `w` were int32; `k/g/h` were UTF-8.
`w` initially equaled `v`; all-missing-fragment cases made `w[9:10]` missing.
The count/sum were 12/54 globally, 6/18 for X, and 6/36 for Y. X/A had count 4,
sum 13, mean 3.25 and three distinct `v` values. With `.by = k`, rollup(g,h)
had 14 summary rows and 36 expansion rows; cube had 18/48; composite
rollup(grouping_set(g,h)) had 9/24; duplicate `(g),(g),()` occurrences retained
with `.duplicates = "keep"` had 12/36.

Grouping sets and occurrence IDs were hand-written; neither inspection results
nor the package's internal plan generator supplied expectations. Three controls
were compared: Margin results across equivalent layouts; ordinary dplyr on the
same Arrow input, collecting each grouping-set branch independently; and a
base-R oracle over the small source fixture. Expansion checked each source row
ID against each requested occurrence. Explicit Margin order was checked apart
from unordered row bags. Default file enumeration and output row order were
not treated as public ordering guarantees.

## Actual physical coverage and public paths

| Layout | Actual structures checked | Public paths |
| --- | --- | --- |
| A / RecordBatch | One chunk per Table column; one RecordBatch | Inspection, summary/alias, expansion |
| B | Two/three/more retained chunks; group-aligned, crossing and mixed cuts; empty chunks before/between/after data | Same, direct collect and compute-collect |
| Shifted B | Per-column lengths `2/5/5` versus `5/4/3` | Same |
| InMemoryDataset | Dataset over a retained multi-chunk Table | Same |
| C | One Parquet, one row group; alternatively six groups of two rows | Same |
| D | Multiple Parquet files with one/multiple row groups; retained empty files | Same |
| E | Hive `p=1` / `p=2`, multiple files per partition; explicitly restored int32 partition field | Same, p as fixed key and as variable dimension |
| Derived queries | Partial `p == 1` filter, all-excluded `row_id < 0` filter, select, prior partition-field rename | Corresponding supported paths above |

The [physical inventory](2026-09-30-arrow-layout-invariance/physical-inventory.csv)
contained 238 observations, including schemas, per-column chunk counts/lengths,
file counts, row groups, empty-file counts, and calibrated scan batch sizes.
The largest checked file layout contained six files, eight row groups and two
empty files. An empty-chunk example retained lengths `4/0/4/4`. Local per-case
records additionally retained file hashes, recognized file sets and row IDs in
each row group. Writer arguments alone were not accepted as structure evidence.
A calibration Scanner used `batch_size = 2`; its batches did not establish the
batch boundaries or execution order of a Margin scan.

The public paths included `inspect_grouping(.format = "list")`,
`summarize_with_margins()` and its `summarise_with_margins()` alias,
`expand_with_margins()`, direct collect, and direct compute followed by collect.
Multiple queries were built from one reusable input before reverse-order and
repeated collection. Source files remained static.

One set, rollup, cube, composite dimensions and duplicate occurrences were
covered. Count, sum, mean, n_distinct, Grouping bits/masks, `.id`, and
none/first/last Margin order were checked. Follow-ups covered all-missing
measure fragments in each position, missing keys, and ordered factors. B/D/E
also exercised int64 and UTC timestamp(ms) dimensions with typed missing,
empty results and Grand totals, checking query and directly computed schemas.

Construction-read observation reused the five observer functions extracted
from the query-policy tests, without running or modifying that test suite.
Input calibration was outside the observation interval. Ten shapes across
unfiltered/partial/empty queries produced 30 construction observations with
zero R execution-entry calls; explicit collect positive controls were detected,
and schema-only negative controls remained zero. This did not prove absence of
all I/O within C++.

## Minimal partition-expansion reproduction

The retained generator wrote one uncompressed Parquet file with one row and
one row group. Its physical schema was `row_id: int32`; the directory was
`p=1/one.parquet`. Explicit Hive partitioning restored `p: int32`, making the
logical Dataset schema `row_id: int32, p: int32`. Calibration read exactly
`row_id = 1, p = 1`.

```r
expand_with_margins(
  ds, .grouping = rollup(p), .id = "occurrence", .margin_label = NULL
) |>
  dplyr::collect()
```

| occurrence | row_id | Expected p | Actual p, direct / computed |
| --- | --- | --- | --- |
| 1 | 1 | 1 | 1 |
| 2 | 1 | NA | 1 |

The explicit two-row expectation, the same logical input as an ordinary-column
Table, and Margin summary all gave the intended values. Ordinary mutation to
NA, including an explicitly typed Arrow Scalar, was correct. Ordinary
`union_all()` was correct before projection. Selecting or renaming after that
union restored the partition value; projecting a single branch did not.

The [standalone script](2026-09-30-arrow-layout-invariance/reproduce.R) retained
these controls, schema checks, file-count/row-group checks, direct and computed
results, repeated collection, and generated data. Its `--arrow-only` mode did
not load marginplyr. The observed Arrow-only sequence was:

```r
missing <- arrow::Scalar$create(NA_integer_, type = arrow::int32())
q <- dplyr::union_all(
  dplyr::mutate(ds, occurrence = 1L),
  dplyr::mutate(ds, p = !!missing, occurrence = 2L)
)
dplyr::collect(q)  # p = 1, NA
dplyr::collect(dplyr::select(q, p, occurrence, row_id))  # p = 1, 1
```

At the target, the portable adapter built branches, combined them with
`union_all()`, and the finalizer applied order and public projection. The
reproduction implicated that composition; it did not identify a specific C++
fault or establish that every possible projection failed.

The minimal witness ran in two fresh R processes. A fresh isolated install of
the archived target also reproduced the 12-row partition-dimension case. No
product guards, internal plan functions or package code were bypassed or edited.

Follow-up expansion varied partition names p/region/z, integer/string fields,
CPU 1/2 with I/O 2, typed-missing/text labels, `.id` presence, and Margin order.
Of 144 scenarios, 120 failed and 24 passed. Direct and computed bags agreed in
all 144. An ID or explicit Margin order triggered the measured failure; with
neither, these scenarios passed. Prior partition-field rename avoided some
measured cases. Reducing the input query to the partition column alone removed
the ID-only trigger, while the ordering trigger remained. These observations
were not promoted to general workaround guarantees.

Partition fixed-key cases, summary/alias, and all-excluded filters did not show
this violation. Ordinary-column C/D, Tables and InMemoryDataset controls also
passed. The selected cross-layout comparisons numbered 995; their 22 differences
were direct/computed expansion symptoms of this same family. See the
[comparison record](2026-09-30-arrow-layout-invariance/layout-comparisons.csv).

## Classifications and limitations

The [decision record](2026-09-30-arrow-layout-invariance/summary.csv) contained:

| Classification | Records | Interpretation |
| --- | --- | --- |
| CONFIRMED MARGINPLYR BUG | 132 | One partition-expansion family, including follow-ups |
| NO VIOLATION FOUND | 304 | Selected assertions passed |
| INPUTS NOT EQUIVALENT | 3 | Input changes before a Margin operation |
| EXPECTED / UNSUPPORTED | 1 | Reader boundary control |
| UPSTREAM / STORAGE BEHAVIOR | 1 | Related Arrow-only reproduction |

These 441 records included controls, type checks and observation, not 441
distinct layouts. Initial harness failures, mutation calibration, minimization
probes and fresh-install witnesses had separate local records. No selected
main case remained without a decision or blocking diagnosis.

Disabling Parquet dictionary encoding changed ordered factor levels from
C/B/A to A/B/C while preserving decoded values and `ordered = TRUE`. It was
rejected by input calibration, before Margin comparison. A separate inference
probe combined an empty Boolean fragment with int32 values 0/1/3: first-fragment
inference produced FALSE/TRUE/TRUE, schema unification refused the conflict,
and explicit int32 preserved 0/1/3. This reproduced an input-schema issue,
not a new Margin defect.

Summary/expansion refused direct and query-backed readers; inspection left
their five rows available for later materialization. Unsafe shared-reader
execution was not tried. An initial generator passed names to
`Table$SelectColumns()`, which required zero-based indices; that harness error
was corrected before the accepted pilot. I/O thread count one produced an Arrow
warning and was replaced by two. Sandbox CPU-cache diagnostics continued but
did not prevent the selected computations. None was counted as a product bug.

The main matrix took 205.9 seconds. The supervisor limited each case to 60
seconds, each phase to 3,600 seconds, and storage to 250 MiB. Maximum observed
main-child peak RSS was 265,863,168 bytes, measured at process completion.
Sandbox restrictions prevented live RSS monitoring: an enforced 2 GiB bound
was not achieved. The retained local bundle occupied approximately 18 MiB.

Unexecuted scope included large data, external storage, other OS/dependency
versions, non-Parquet formats, nested/multilevel partitions, nullable Hive
partition values, arbitrary dictionary-index recoding, nested/list columns,
every Arrow expression/type, and source mutation during queries. Actual Margin
C++ scan batches were not observed; CPU two was not used for the entire matrix.
No product fix, permanent regression test or new input support was implemented.

## Re-execution and retained artifacts

The committed bundle published portable minimal generators and path-free CSV
summaries. It did not publish installed libraries, absolute-path manifests,
binary RDS files, generated Parquet files, or the full exploratory harness.
The latter, its logs and generated data remained in the isolated local cache
under `arrow-layout-invariance/2026-09-30-c14746a/run-001`.

From a checkout containing the investigated commit, the following command
verified the recorded package versions, archived that commit, installed only
into a fresh output directory, and ran the Margin and Arrow-only witnesses in
separate processes:

```sh
Rscript investigation/2026-09-30-arrow-layout-invariance/replay.R \
  /private/tmp/marginplyr-arrow-layout-replay-001
```

Use an unused output directory. It retained `source.tar`, the isolated
installation, install/console logs, and each witness's data, `evidence.rds` and
`evidence.txt`. It did not install or update dependencies. Exit zero meant
that the historical defect had been reproduced and its controls passed, not
that the package had been repaired. The regenerated minimal bundle was
executed successfully before publication.

With an already isolated installation, either witness could also be run alone:

```sh
Rscript investigation/2026-09-30-arrow-layout-invariance/reproduce.R \
  /private/tmp/marginplyr-arrow-margin-001 /path/to/isolated/library
Rscript investigation/2026-09-30-arrow-layout-invariance/reproduce.R \
  /private/tmp/marginplyr-arrow-only-001 --arrow-only
```

The full local experiment could be repeated from its retained `run-001`:

```sh
python3 scripts/replay.py --phase pilot
python3 scripts/replay.py --phase full
python3 scripts/replay.py --phase full --case E-mixed-base-part_dim-none-none-t1
```

That runner verified installed-file hashes and replayed into a new sibling
directory. The individual fresh-install case command was executed; the full
wrapper's constituent stages were executed during the original experiment.
The committed minimal runner did not claim to replay all 441 decision records.

Every selected layout/public path had a decision, the violation had a minimal
unmodified-code witness and independent controls, and physical coverage was
recorded. Finding more bugs was not an exit condition.
