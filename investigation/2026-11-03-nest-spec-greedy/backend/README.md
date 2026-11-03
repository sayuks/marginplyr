# dtplyr Margin-order helper and remote safety investigation

Investigated: 2026-11-03
Baseline: `4b34fdecb2d8a41034b3d9b5d5af8e51592555e1` (`main`)

## Confirmed finding

The shared dtplyr Margin-order expressions resolved `is.na` and `%in%` from
an input's root environment. Unrelated caller functions changed the order
returned by both nesting Margin verbs, although the caller supplied neither
ordering expressions nor summary expressions. One independent repair ticket
was recommended for this shared ordering path. The two helpers were
manifestations of the same package-owned sorting contract. No package
implementation or permanent package test was changed.

A two-row source with character key values `NA` and `a` and an integer row
identifier sufficed. An included-only `grouping_set(g)` result was constructed
with `.sort = "last"` before its root acquired an `is.na` function returning
the inverse of base missingness. Collecting that same lazy result returned
`NA, a`, contrary to Margin order's missing-last promise. The original outer
key values and each cell's original source row remained intact. A rollup
result with a root-local `%in%` returning the inverse of base membership
returned the Grand total set first under `.sort = "last"`. Rebinding only the
respective helper to its base function restored the same lazy query's order.

The [public-seam reproducer](reproduce-sort-helpers.R) retained both variants,
the emitted queries, independent upstream controls, and desired-behavior
assertions. Source collection followed by ordinary `arrange(g)`, grouped
counting followed by `arrange(g)`, and grouped tidyr nesting followed by
`arrange(g)` all kept the expected keys and invoked the masked helper zero
times. The source serialization and unordered cell/key/identifier projections
stayed identical. Explicit user-written calls to these helpers retained their
ordinary lexical behavior; their different result was a positive control,
not a request to override that behavior.

The contract sources were `CONTEXT.md`'s **Margin order**, ADR 0018's *Missing
values come last, and that is promised* and *fixed keys order their missing
values last too* amendment, and the nesting reference's inherited sorting
contract. `margin_missing_last_expr()` emitted unqualified `is.na`, and
`margin_grouping_bit_expr()` emitted unqualified `%in%`. dtplyr deferred those
calls into the root environment. The generated query showed the missingness
and Grouping-bit terms in the final ordering, outside the nesting fold.

### Ranked hypotheses and separating controls

1. The package-generated sort terms looked up caller helpers when collected.
   A binding installed after query construction would change order; restoring
   it alone would recover the same query, with cells and source unchanged.
2. The binding changed source values, schema, or cell membership. Ordinary
   collection or serialized source/cell projections would then differ too.
3. Grouped backend execution generally depended on those helpers. Ordinary
   grouped counts or upstream nesting followed by an ordinary key arrangement
   would then invoke the callbacks or change their output as well.

The first prediction held. The second and third controls refuted those
explanations for these cases. `.sort = "none"` supplied a separate negative
control: every matrix comparison in that branch matched. A fixed `.by` key
also reached the `is.na` failure while generating no Grouping bit, separating
missingness from the `%in%` term. An included-only Grouping specification
likewise reproduced missingness corruption without an omitted Margin label.

## Completed finite measurements

[The helper matrix](sort-helper-matrix.R) completed **576 comparisons**:
`is.na` / `%in%`, ordinary / `.SD` key names, character / integer / factor
keys, zero / three source rows, fixed / variable key roles, all three sorting
choices, both `.keep` values and both nesting verbs. Each unmasked lazy result
first matched nesting of the explicitly materialized local source. For
`.sort = "none"` that oracle compared unordered rows, because default row
order was unspecified. Other choices compared exact order. Cell classes were
normalized to tibbles only for the comparison, not as a proposed API guarantee.

| Masked helper | Wrong order | Matching result | Wrong `first` | Wrong `last` | Wrong `none` |
| --- | ---: | ---: | ---: | ---: | ---: |
| `is.na` | 96 | 192 | 48 | 48 | 0 |
| `%in%` | 48 | 240 | 24 | 24 | 0 |

All 576 comparisons retained identical source serialization and unordered
cell/key/identifier projections. Empty cases and fixed-key cases without a
Grouping-bit term explained many matching controls; matching output alone did
not establish callback isolation. [The per-comparison CSV](sort-helper-results.csv)
retained callback reads and every measured dimension. These finite observations
were not package regression tests or proof about unmeasured inputs.

An initial matrix draft incorrectly demanded equal backend row order under
`.sort = "none"`. Its first populated fixed-key case failed that unsuitable
oracle. The completed run used unordered comparison only for that unpromised
order; partial development counts were excluded. The initial minimal control
also used upstream tidyr's unsupported `.by` translation and was corrected to
ordinary grouped nesting with an explicit payload selection before the retained
successful control runs. Neither fixture error was reported as a package bug.

### SQL and multiple-reader Arrow boundaries

[The remote probe](remote-multiple-inputs.R) added five live SQL graphs per
engine: a coalesced derived key, a cross join, an anti join, a windowed limit,
and a branching union. Each graph received both nesting refusals with unforced
Grouping specifications, two locally invalid inspection calls and one
name-only plan inspection. SQLite completed 25 calls and sent zero statements.
DuckDB completed 25 calls and sent five statements, all `LIMIT 0`. Nesting
refusals and locally invalid calls sent none, and refusal left their supplied
selection promises unforced.

Four Arrow graphs carried two independent one-shot reader roots each: union,
join, reversed join, and a join whose right payload was hidden by projection.
Both nesting refusals and typed/name-only inspection completed 16 calls without
consuming either root. All eight readers subsequently returned all 16 original
rows. These observations established inspection/refusal safety for the measured
graphs, not nesting support for SQL or Arrow.

## Prior evidence and duplicate checks

The complete October 30 and October 31 backend notes and the November 2
[main record](../../2026-11-02-nest-grouping-hunt/README.md) and
[backend record](../../2026-11-02-nest-grouping-hunt/backend/README.md) were read.
Their old measurements did not count as executions on this baseline. The
`list`-mask candidate had already broken ordinary grouped upstream controls
there, and was not repackaged as a new finding. Small name-helper probes also
found that root-local `setnames` and `copy` functions did not replace those
backend-owned helper calls in the measured rename input.

All-state searches used `gh issue list --state all --limit 100` with
`dtplyr "is.na"`, `dtplyr "%in%"`, `dtplyr sort mask`, and
`dtplyr order helper`; PR searches used `gh pr list --state all --limit 100`
with `dtplyr "is.na"` and `dtplyr sort helper`. The adjacent `duplicate-*.json`
files retained the queries' matches. GitHub tokenized the membership-operator
query broadly; its matches were assessed as broad candidates, not exact-text
proof of absence.

The complete bodies and comments were fetched with explicit JSON fields for
[#85](https://github.com/sayuks/marginplyr/issues/85),
[#87](https://github.com/sayuks/marginplyr/issues/87),
[#1089](https://github.com/sayuks/marginplyr/issues/1089), and
[#1143](https://github.com/sayuks/marginplyr/issues/1143). The first two established
the sorting feature; they did not report caller helper ownership. #1089 isolated
`min` and `seq_len` in nesting's displayed representative extraction. #1143
isolated `as.character`, `rep`, `length` and `sum` in labels, omitted identities
and collision checks. Their repaired expressions were distinct from the
ordering terms demonstrated here. No existing report for this ordering
boundary was found in those searches.

## Replay, environment and terminal evidence

Run from the investigated checkout root, or point `MARGINPLYR_HUNT_SOURCE` to
a source copy of the pinned baseline:

```sh
Rscript investigation/2026-11-03-nest-spec-greedy/backend/reproduce-sort-helpers.R
Rscript investigation/2026-11-03-nest-spec-greedy/backend/reproduce-sort-helpers.R --assert
Rscript investigation/2026-11-03-nest-spec-greedy/backend/reproduce-sort-helpers.R '%in%'
Rscript investigation/2026-11-03-nest-spec-greedy/backend/reproduce-sort-helpers.R '%in%' --assert
Rscript investigation/2026-11-03-nest-spec-greedy/backend/sort-helper-matrix.R sort-helper-results.csv
Rscript investigation/2026-11-03-nest-spec-greedy/backend/remote-multiple-inputs.R
```

Both observation reproductions returned terminal exit 0; both desired-behavior
`--assert` commands returned terminal exit 1 after the controls passed. Those
assertions required the intended order rather than pinning the defect. The
completed matrix returned terminal exit 0 through retained session `19510`;
the remote probe returned terminal exit 0 through retained session `77154`.
Earlier incomplete matrix sessions `17391` and `57666` returned terminal exit 1
and were collected before the corrected run. All minimal invocations completed
synchronously, and every yielded session was retained through its terminal
outcome. No process remained running for this track.

Each script loaded the isolated `git archive` copy of the baseline through
`pkgload::load_all()` with testthat parallelism disabled. No dependency was
installed or changed. [The matrix log](sort-helper-matrix.log) recorded R 4.6.1
on macOS Tahoe 26.6.2 arm64, dplyr 1.2.1, dtplyr 1.3.3,
data.table 1.18.6.1, vctrs 0.7.3, tibble 3.3.1 and tidyr 1.3.2.
[The remote log](remote-multiple-inputs.log) added DBI 1.3.0,
RSQLite 3.53.3, DuckDB 1.5.5 and Arrow 25.0.1. Adjacent logs retained the
minimal observations and intentional desired-behavior failures.

The finite track did not establish every dtplyr graph, arbitrary caller
bindings, custom extension methods, other dependency/OS versions, hosted
backends, resource exhaustion, or an unbounded proof of correctness. No package
build, full suite, implementation repair or permanent regression test was run.
