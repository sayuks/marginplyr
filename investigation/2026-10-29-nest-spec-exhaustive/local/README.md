# Local nesting source-row and structural audit

Investigated: 2026-10-29
Baseline: `ee4321789ca60b6259c53df2852f1574aaede584` (`main`)

The local track found no new independently actionable defect in its completed
probes. It retained four complementary oracles rather than treating agreement
between the two nesting verbs as a correctness test. No package source or
package regression test was changed.

## Method and coverage

The track read `CLAUDE.md`, `AGENTS.md`, `CONTEXT.md`, the public nesting and
shared result/grouping references, ADRs 0016, 0018, and 0029, and the local
coverage and findings in the 2026-10-28 and 2026-10-29 nesting investigations.
In particular, the preceding recursive, scalar-shape, classed, and sf sweeps
were used to choose additional cases rather than counted as observations of
this baseline.

`local-fixtures.R` declared 27 value shapes: logical, integer, double, signed
zero/nonfinite double, character, NA-level factor, ordered factor, complex,
Date, zoned POSIXct, POSIXlt, difftime, heterogeneous list, named nested list,
language/expression list, matrix, zero-column matrix, three-dimensional array,
packed tibble, zero-column packed tibble, recursive packed tibble, integer64
above double precision, hms, units, a two-field vctrs record, AsIs double, and
list_of. The factor included both a declared NA level and an actual missing
code, an unused level, and a value spelling a private-sentinel prefix.

The four explicit plans varied grouping-set order, absent dimensions, a
single complete set, and the Grand total set. Optional fixed keys were
NA-level factors. The deterministic sweeps included both `.keep` modes,
both nesting verbs, empty and populated inputs, and all Margin order choices.

| Evidence | Completed observations |
| --- | --- |
| `local-oracle.R` | Seed 29102911; 3,888 calls, including 1,296 implicit-group/explicit-`.by` pairs; 14,118 source-membership and projection comparisons; zero refusals or unexplained differences |
| `order-oracle.R` | Seed 29102912; 1,728 calls; exact cell order matched independently constructed Grouping-bit, missingness, source-value, and set-identifier terms |
| `zero-payload.R` | Seed 29102913; 1,998 calls; 7,356 keyed cells with zero columns retained their independent source-group cardinalities; additional columnless-input cases retained the documented empty-input distinction |
| `data-table-input.R` | 4,320 calls; 1,992 successful direct-data.table/tibble pairs and 168 matching documented refusal pairs; every serialized source stayed identical |

The source-row oracle used `vctrs::vec_group_loc()` directly on the source keys,
matched cell row identifiers independently of output row order, and compared
every cell with the exact source projection. For `.keep = TRUE`, that included
the original fixed keys and grouping dimensions. Every fixture's serialized
source was identical after its direct and implicit-group calls.

The order oracle did not sort the package result to create its expectation.
It grouped the source independently, selected one representative source row
per group, and built the expected key from the explicit sets. An omitted
dimension received a constant value placeholder behind its distinct Grouping
bit, so the placeholder could not affect within-grain value order. The oracle
set C collation explicitly to agree with dplyr's documented default. An early
comparison using the process locale put uppercase `Total` after lowercase
letters; that was an oracle collation mismatch, not a package finding.

The zero-payload oracle removed every source column into fixed keys or grouping
dimensions, then matched each output key to the independently grouped source
keys and compared cell row counts. It included `.key` values `data`, `..1`,
and `.rows`, and `.id` values `set`, `...`, and `..01`. Separate zero-column
source inputs had 0, 1, or 6 rows; the ordinary nesting verb retained zero
outer rows on the empty source and the rowwise verb retained its one empty
cell.

The direct-data.table matrix varied ten special source names (`.data`, `.env`,
`.rows`, `.SD`, `.SDall`, `.xSD`, `.iSD`, `.N`, `T`, and a private-name prefix)
in fixed-key, dimension, and payload roles, empty/populated inputs, both keep
modes, all sort choices, both verbs, and three special output-name pairs.
Every accepted result matched the same source represented explicitly as a
tibble. The 168 refused pairs exercised declared `.key`/`.id` collisions or
the documented `.rows` rowwise-key restriction. The nonmutation check compared
serialized data.table names, columns, and attributes before and after every
call, not merely its collected values.

## Controlled differences and limits

The source-projection sweep recorded 96 POSIXlt class differences. On this
baseline, ordinary `dplyr::union_all()` converted the same POSIXlt input to
POSIXct too. Kept time values matched the original source instants.
`delegation-controls.R` recorded that control alongside the single-set raw
key case: raw typed-missing rejection occurred when the plan omitted the
raw dimension, while the complete single-set plan succeeded. Neither was
published as a new defect under ADR 0016's delegation boundary.

These finite cases did not establish behavior for every arbitrary S3/S4 method,
all data-frame subclasses, every dependency version, another operating system,
unbounded plans, or very large data. They did not add an attribute-restoration
promise. The retained scripts asserted membership, public output structure,
package-created ordering, original kept values, and source nonmutation.

## Replay

Run from the investigated repository root; every script loaded that checkout
with `pkgload::load_all()`:

```sh
Rscript investigation/2026-10-29-nest-spec-exhaustive/local/local-oracle.R
Rscript investigation/2026-10-29-nest-spec-exhaustive/local/order-oracle.R
Rscript investigation/2026-10-29-nest-spec-exhaustive/local/zero-payload.R
Rscript investigation/2026-10-29-nest-spec-exhaustive/local/data-table-input.R
Rscript investigation/2026-10-29-nest-spec-exhaustive/local/delegation-controls.R
```

Adjacent `.log` files captured the completed runs. The scripts exited zero
when their stated assertions passed; those outcomes did not assert the absence
of all possible nesting bugs.
