# Lazy expansion source and scalar-prototype boundaries

Investigated: 2026-11-03
Baseline: `4b34fdecb2d8a41034b3d9b5d5af8e51592555e1`
Scope: dated evidence; no implementation or package tests changed

The track read the October 30, October 31, and November 2 expansion hunts,
the October 29 deep nesting hunt, the public expansion/inspection contract,
and ADRs 0002, 0016, 0018, 0020, 0029, and 0033. Historical measurements were
not counted as new execution. Every package load accepted the optional
`MARGINPLYR_HUNT_SOURCE` source path, defaulting to the worktree root.

## LZY1: one-row dtplyr scalar dimensions lost their omitted type

A one-row immutable dtplyr input held a `logLik` dimension and an integer
source-row identifier. Expanding a rollup with a NULL Margin label constructed
a lazy result, but collecting it raised `dt_join_type_mismatch_error`:
`Item 1 of x is 'logLik' but the corresponding item of y is 'numeric'.`
An ordinary dtplyr mutation using a row-sized `vctrs::vec_init()` value followed
by `union_all()` succeeded, returning the original value, a missing `logLik`,
and both source-row identifiers. A one-distance `dist` dimension reached the
same failure. Included-only expansion succeeded.

The expectation combined expansion's one-copy-per-source-row contract with
its typed-missing label and ordinary mutate/union composition. It did not
require arbitrary attribute restoration under ADR 0016 or new backend support.

The minimal replay was:

```sh
Rscript investigation/2026-11-03-expand-grouping-greedy/lazy/reproduce-dtplyr-loglik.R
Rscript investigation/2026-11-03-expand-grouping-greedy/lazy/reproduce-dtplyr-loglik.R --assert
```

Observation mode printed the successful control and package failure and exited
zero. The expected-behavior assertion intentionally exited one after the
control passed. It required the public expansion to succeed and agree with the
ordinary result; it did not assert the defect as correct behavior.

### Ranked explanations and probes

Before the cause probes, three explanations were ranked and sent to the
coordinator:

1. NA subsetting dropped the omitted prototype's class. A class-preserving
   missing value would allow ordinary branches to combine.
2. Ordinary dtplyr could not combine this class. Explicitly classed included
   and missing branches would then fail too.
3. The zero-row proxy had already lost the class. Inspecting that proxy would
   then report a bare numeric column before missing-value construction.

The observations supported the first explanation. The metadata proxy retained
`logLik`; its `[NA_integer_]` slice was numeric; `vec_init(proxy$g, 1L)` retained
`logLik`; the explicitly typed ordinary branches combined successfully. The
backend metadata seam selected `vec_init()` for local and array/data-frame
prototypes, while this lazy scalar used the NA slice. No premature source-row
read was inferred from this defect.

The row-count boundary mattered. A full-length replacement on one source row
could replace the column with the bare prototype; on three rows data.table's
recycling retained the existing class, so those cases did not establish the
same failure. The finite matrix measured this rather than assuming all input
sizes failed.

### Completed matrix and coverage gap

`dtplyr-class-matrix.R` completed 1,728 observations across two classes
(`dist`, `logLik`), zero/one/three source rows, dimension/fixed/payload roles,
plain/filter/cache input shapes, included-only/rollup/omitted-first/retained-
duplicate plans, NULL and NA-character labels, absent/present public occurrence
identifiers, and none/last sorts.

All ordinary controls succeeded and every caller-owned data.table remained
identical. There were 1,584 matching comparisons and 144 package-only errors,
72 per class. Every error was a one-row dimension with an omitted branch.
Empty, three-row, included-only, fixed-key, and payload controls passed.

The scalar-class package tests added for #1093 covered local data frames,
tibbles, and direct data.tables, including the same row counts and plan forms.
They did not construct lazy dtplyr inputs. No package test was added or edited
in this investigation.

### Duplicate search and ticket boundary

`duplicate-search.R` recorded all-state Issue and PR queries in
`duplicate-search.json`: Issue searches `logLik`, `logLik dtplyr`,
`dist dtplyr prototype`, and `typed-missing dtplyr`; PR searches `logLik` and
`scalar dtplyr`. No logLik/dtplyr or dist/dtplyr prototype report matched.
The broader results supplied the following candidates, whose complete bodies
and comments were read:

- [#1093](https://github.com/sayuks/marginplyr/issues/1093) and merged
  [#1130](https://github.com/sayuks/marginplyr/pull/1130) explicitly covered
  local scalar dimensions and retained the existing lazy paths.
- [#833](https://github.com/sayuks/marginplyr/issues/833) and
  [#860](https://github.com/sayuks/marginplyr/issues/860) covered plain/classed
  list dimensions and an invalid generated list expression during construction.
- [#427](https://github.com/sayuks/marginplyr/issues/427) and merged
  [#433](https://github.com/sayuks/marginplyr/pull/433) covered non-missing text
  labels; its issue specifically reported typed-missing atomic controls as
  unaffected.
- Merged [#796](https://github.com/sayuks/marginplyr/pull/796) refused unsafe
  caller scalar overwrites during typed metadata acquisition; this finding
  used a plain root whose faithful proxy retained the correct class.

One independent ticket could repair and verify the lazy scalar-prototype
boundary. No prefactoring or dependency on another finding was needed. The
coordinator owned later-main confirmation and publication.

## Reader graph consumption controls

`reader-graph.R` completed 88 observations: 22 source shapes, name/typed
selection, and inspection/expansion. The shapes covered a direct reader,
select/filter/mutate/group/summary/arrange/head/collapse/distinct, both join
input positions, semi/anti joins, both union positions, and nested join/union
graphs. All 44 inspections succeeded and all 44 expansions raised the expected
reader refusal. Afterwards every original reader still supplied all three
source rows. This established nonconsumption for those graphs and fixtures,
not an arbitrary Arrow graph proof.

## UnionDataset and hidden-partition graph controls

`union-dataset-graphs.R` completed 960 matching comparisons across four
hidden partition names, ten source graphs, NULL/text labels, three sorts,
ordinary/hidden-name occurrence identifiers, and absent/present fixed keys.
The sources included UnionDatasets over two filesystem children, mixed
filesystem/in-memory children, nested UnionDatasets, filtering/ordering/renaming,
both join input positions, unions of queries, and unions of joined relations.

An independent oracle copied the ordinarily collected source rows, wrote
the omitted character value and occurrence identifier itself, and constructed
the requested bit/missingness/value key separately. All exact values/types,
public column placements, row multiplicities, and requested key sequences
matched. Payload ties were not assigned an order. Hidden names included the
package's identifier, dimension, and boundary prefixes. The probe established
these finite partition graphs, not arbitrary fragment metadata or every
Arrow physical type.

## Harness corrections

The first reader execution finished its 88 measurements and CSV write but
failed when a data.frame print received a tibble-only argument. The corrected
script was rerun to terminal success; only that rerun supplied the completed
verdict.

The initial scalar matrix included a rename over a cached dtplyr alias. Its
ordinary multi-branch union reused a reference-writing rename and failed with
`Items of 'old' not found in column names: [old]`. All 432 observations from
that ordinary-failure population were excluded. The final matrix removed that
shape and reran every remaining case; it was not inferred by subtracting
failures from the initial result. The initial CSV/log were retained separately
and were not additional passing evidence.

The first UnionDataset fixture attempted to infer a partition field from a
child holding only missing values. Ordinary `open_dataset()` refused to infer
that field; supplying its explicit source schema corrected the fixture before
any package comparison was counted.

## Environment and limits

`environment.txt` recorded R 4.6.1 on macOS arm64, dplyr 1.2.1, dbplyr 2.6.0,
dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1, vctrs 0.7.3, tibble 3.3.1,
rlang 1.3.0 and tidyselect 1.2.1. Arrow emitted sandbox CPU-cache metadata
diagnostics; those were not marginplyr conditions. No dependency was installed.
All owned executions were retained through their terminal results. No full
suite, build, remote database service, other OS/dependency matrix, arbitrary
custom class/callback enumeration, or performance claim was attempted.
