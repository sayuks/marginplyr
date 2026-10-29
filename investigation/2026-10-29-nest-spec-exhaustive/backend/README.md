# Backend graph and nesting investigation

Investigated: 2026-10-29
Baseline: `ee4321789ca60b6259c53df2852f1574aaede584` (`main`)

## Finding: shared cached branches lost schema isolation

The backend track reproduced one defect, published as
[#1024](https://github.com/sayuks/marginplyr/issues/1024). Typed Grouping-plan
inspection and both nesting verbs could receive incorrect metadata, artificial
warnings, or a false metadata refusal from a supported immutable dtplyr graph.
All caller data.table snapshots remained unchanged. The measured metadata
executions used isolated zero-row roots; no read of real source rows was
established by these observations.

The decisive typed fixture used one integer-key root and two cached branches.
One branch added an integer column by arithmetic; the other added a scalar
character column of the same name. An inner join collected correctly, with
integer `new.x` and character `new.y`. Typed inspection with
`.by = where(is.character)` instead reported no fixed keys. Both nesting verbs
returned only `data`, where their explicitly collected-input controls returned
`new.y` and `data`. A typed rollup refused the valid character dimension with
`` `rollup()` requires at least one dimension. ``

`shared-cache-types.R` expanded that case to independently chosen logical,
integer, double, and character scalar assignments on both branches, four join
kinds, four typed predicates, and inspection plus both nesting verbs. Of 768
public calls, 288 selected incorrect fixed keys or returned incorrect outer
column names; 480 matched their direct collected-column controls. Every call
emitted the artificial shallow-copy warning. Ordinary collection of every
source graph succeeded without warnings. Each join kind accounted for 72
mismatches and 120 matches. The arithmetic-left/scalar-character-right minimal
fixture happened to preserve types under `left_join()`; that local control did
not generalize to two scalar assignments.

Two further manifestations shared this metadata boundary:

- A cached arithmetic mutation reused on both sides of `union_all()` collected
  without warnings, but typed inspection and nesting construction emitted a
  data.table shallow-copy warning. Name-only inspection emitted none, and
  collection of the already constructed nesting result emitted none. Under
  `options(warn = 2)`, inspection and both nesting verbs falsely refused the
  same input as unsafe metadata.
- A cached rename reused on both sides of `union_all()` collected correctly,
  but typed inspection and both nesting verbs falsely refused it. The private
  isolated evaluation failed because the second rename could no longer find
  the original column name. Uncached, cached-only, and explicitly collected
  controls succeeded without warnings.

`metadata-cause-controls.R` located the shared cause. The package's zero-row
rewrite removed a cached `dtplyr_step_assign` while retaining downstream copy
flags calculated for the cache reference. The first rewritten branch could
therefore perform a reference write directly on a shared synthetic root.
An earlier mutation's new column then existed when a later scalar assignment
ran on zero rows, retaining the first branch's type. A repeated rename instead
looked for a column the earlier branch had renamed.

The isolated root tables had valid data.table self references immediately
before source extraction. dtplyr's same-name source merge yielded a synthetic
source with an invalid self reference, explaining the warning on a bare write.
The ordinary cached graph protected its writes with a saved copy expression.
Retaining the cache and its already-safe arithmetic bindings in an experimental
zero-row control, or restoring the downstream copy flag on the package's
rewritten graph, eliminated the warning and restored the character type.
These were investigation-only graph alterations; neither was implemented as a
package repair. Keeping arbitrary saved bindings would require separate
reasoning about row predicates, whose non-evaluation #996 had established.

The bounded controls included two and three source rows, reversed join inputs,
uncached branches, independently rooted branches, cached-only inputs, and
explicitly collected inputs. No caller table changed in names, values, rows,
or compared attributes. The issue was kept as one ticket because faithful typed
selection and safe metadata acquisition were one public behavior across these
manifestations; suppressing only the warning would leave incorrect plans.

## Search coverage and exclusions

Counts describe separate experiments; they were not added into one successful
test total. A matching value result could still emit the warning above.

| Retained script | Dated observations |
| --- | --- |
| `execution-graphs.R` | 12 input derivations, six post-operations, nine single/join/set shapes, both nesting verbs and keep modes: 2,592 calls; 2,364 value/column matches against explicitly collected-input nesting, 216 existing head safety-gate refusals, and 12 cached-rename metadata false refusals belonging to #1024. Eight warning occurrences led to the arithmetic-cache reproduction. Fresh caller snapshots stayed identical. |
| `grouped-caches.R` | 192 calls across implicit fixed groups, eight cached/derived shapes, both keep modes, three Margin orders, and both nesting verbs matched their collected-input controls; all source snapshots stayed identical. |
| `nest-backend-boundaries.R` | 128 cached/mutable graph boundary checks and 20 cached unsafe-metadata checks passed. Construction and typed inspection ran no row-only predicate or unsafe transformation callback; rowwise nesting evaluated populated rows only where explicit collection was permitted. Mutable contributing roots were refused before source changes. |
| `shared-cache-types.R` | 768 typed public calls, 288 mismatches, 480 matches, and 768 artificial-warning cases; 64 ordinary source collections were warning-free. |
| `remote-boundaries.R` | 48 Arrow graph calls preserved 12 one-shot readers. Live SQLite supplied 28 public boundary calls, seven typed refusals and zero queries. Live DuckDB supplied 28 calls and 14 queries, all `LIMIT 0`. Nesting refusals did not evaluate the supplied Grouping specification callback. |

The broad execution matrix's 12 false-refusal observations were four keep/verb
combinations for each of `rename/cache/semi`, `rename/cache/union`, and
`rename/cache/set`. The retained minimal repeated-rename reproduction explained
the union path directly; the other graph observations were not assigned a
more specific mechanism than the shared schema-isolation boundary.

The SQL fixtures carried `.env` and `TRUE` columns. A derived rename failed
ordinary dbplyr collection with the same pronoun-to-SQL translation diagnostic
on both engines, so that source shape was excluded before counting the public
boundary calls. This established an upstream source limitation rather than a
new marginplyr finding. The 216 fixed-head fixtures reached the existing conservative
metadata-safety gate before a proxy was acquired. ADR 0020 and the query-read
reference documented the general faithful-metadata/refusal boundary, not a
head-specific exclusion or complete inference for every safe transformation.
dtplyr generated a bare `head()` call resolved through the caller environment,
whereas the recognized metadata-safe reference-write functions were bound by
dtplyr itself. Support completeness for the fixed-head fixtures was not
adjudicated as a finding or counted as a correctness pass. Those observations
were separate from an admitted proxy that returned a wrong answer.

An implicit matrix-column arithmetic lead was excluded because conversion to
the dtplyr root had already expanded its columns and ordinary collection of the
unexpanded-name expression failed. Unknown derived transformations remained
refused before callbacks ran. Existing repairs #968, #995, #996, #997, and
#1003 were consulted rather than republished. No package implementation,
package tests, dependencies, or generated documentation were edited.

## Replay

Run from the investigated checkout root. These commands asserted desired
behavior and intentionally exited 1 on the baseline:

```sh
Rscript investigation/2026-10-29-nest-spec-exhaustive/backend/shared-cache-types.R --assert
Rscript investigation/2026-10-29-nest-spec-exhaustive/backend/reproduce-metadata-shallow-warning.R --assert
Rscript investigation/2026-10-29-nest-spec-exhaustive/backend/reproduce-cached-rename-metadata.R --assert
```

Without `--assert`, those scripts recorded the broken observations and exited
0. `metadata-cause-controls.R`, `execution-graphs.R`, `grouped-caches.R`,
`nest-backend-boundaries.R`, and `remote-boundaries.R` also exited 0. Their
terminal success established completion of the stated measurement, not repair
of #1024. The two matrices wrote their adjacent CSV evidence. The assertion
logs retained the exact intended-behavior failure.

These finite experiments did not establish behavior on a live PostgreSQL
server, another operating system or dependency version, arbitrary extension
methods, unbounded graph depth, or a large performance load. The SQL and Arrow
experiments measured inspection/refusal boundaries, not nesting support on
those backends.
