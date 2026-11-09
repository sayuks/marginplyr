# Independent Nest and Grouping specification investigation

Investigated: 2026-11-09
Tested main: `5c667c956fd50ba39be621bf1b617a17abda7b14`

Freshly fetched main supplied a new Git worktree on
`investigation/nest-independent-20261109`. Three fresh-context agents examined
implementation, public contracts and processing consistency at the same SHA.
They shared no candidates before their initial reports. The coordinator
separately prepared an independent membership oracle and small input generator.
Every lane used its own disposable Git archive, serial R execution and
`TESTTHAT_PARALLEL=false`. Programs and logs stayed outside the submitting
worktree. Inputs were synthetic; no database was needed or accessed.

No previously unreported package defect was confirmed in this finite corpus.
One already reported defect remained reproducible: [#1503][empty], through
its [nested-conversion residual][residual]. The same cause and acceptance
boundary were already recorded there, so `to-tickets` produced no duplicate
Issue. This note added evidence only, without repairing runtime or package tests.

## Reproduced existing defect: empty dtplyr cell conversion

A keyless empty `nest_by_with_margins()` input entered the documented singleton
cell compensation. That path selected the stock
`as.data.table.data.frame()` method, but the method internally invoked
`as.data.frame()` on the generated tibble. A registered
`as.data.frame.tbl_df` replacement changed the empty integer `rid` payload to
an empty character `injected` column. There was one callback, no warning, and
no error. The outer row count, zero cell rows and integer `.id = 1L` remained
correct. Cardinality alone would not detect the wrong schema.

This standalone minimum used the tested source, restored registration on exit
and did not modify its input:

```r
library(marginplyr)
reproduce <- function() {
  ns <- asNamespace("base")
  old <- getS3method("as.data.frame", "tbl_df", envir = ns)
  on.exit(registerS3method("as.data.frame", "tbl_df", old, envir = ns))
  registerS3method("as.data.frame", "tbl_df", function(x, ...) {
    if (identical(names(x), "rid")) {
      return(data.frame(injected = character(nrow(x))))
    }
    old(x, ...)
  }, envir = ns)
  input <- dtplyr::lazy_dt(data.table::data.table(rid = integer()))
  stopifnot(identical(names(dplyr::collect(input)), "rid"))
  nest_by_with_margins(input, .id = "set")$data[[1L]]
}
reproduce()
# Expected: integer rid, zero rows.
# Observed: character injected, zero rows.
```

Native collection, ordinary empty dtplyr Nest and local empty row-wise Nest
retained `rid` with zero targeted callbacks. Restoring registration restored
the same row-wise dtplyr schema. Source serialization remained unchanged.
The observed-result program exited 0. Its desired-schema mode exited 1 after
all controls and restoration completed, specifically on the `rid` assertion.

The [Nest reference][nest] promised cells holding the input's non-key columns
and one cell containing the empty input for a keyless row-wise request.
[ADR 0016][attributes] placed generated-cell conversion on marginplyr's side
of the boundary. This was schema replacement, rather than a request for
arbitrary attribute restoration or identical element subclasses across backends.

The existing `test-nest-dtplyr-frame-converter.R` registered
`as.data.table.tbl_df` and `as.data.table.data.frame`, not
`as.data.frame.tbl_df`. Its exact-schema assertions would catch the symptom
under the latter condition, but the test did not establish that condition.
The empty schema/single-evaluation assertions in `test-nest-operation.R`
likewise did not install the nested replacement. This inspection did not
establish a new regression-suite pass.

The independently registered local `as_tibble.data.frame` witness from
[#1512][frame] was also replayed. Both local verbs retained `rid = 17L` and
made zero targeted generated-frame callbacks. The aliased-quote witness from
[#1505][capture] returned identical inline/stored results and the expected
two source groups through both verbs. Those selected repaired paths did not
establish that the separate #1503 residual had been repaired.

## Independent expectations and finite coverage

Expected grouping families were written by hand: included-only set, two-column
rollup, cube, composite rollup, Cartesian product and duplicate-dropped union.
No compiler, expansion or inspection output generated them. The coordinator
partitioned original source positions by each set's fixed/included keys and
extracted the original rows directly. Explicit selections kept the tracing
`rid` out of the keys. Each output key and Grouping set identifier was matched
to its expected cell; comparisons covered row-ID multisets, all payload values,
column names/order, original keys under `.keep`, factor levels/codes, classed
column attributes and result grouping state. Physical source-row order inside
cells was not required.

The seeded input generator was separate from the expectation code. It varied
zero to eight rows, duplicate and missing keys, optional existing fixed groups,
ordered/unordered factors with unused levels, list payloads, both keep modes,
NULL/string/typed-missing labels and all sort choices. True payload-free inputs
acquired no tracing column: direct key partitions supplied their multiplicities.
Agreement between the two verbs alone was never the oracle.

Counts below described separate, overlapping finite lanes, not exhaustive
coverage or an aggregate package-check result.

| Lane | Fresh execution evidence |
| --- | --- |
| Coordinator literal families | 2,304 calls over eight key types, zero/one/six rows, six families, optional fixed key, both verbs/backends/keep modes. All 1,152 local and 1,008 dtplyr supported cases matched; 144 dtplyr list-key refusals were separated with native controls. |
| Coordinator generated inputs | 120 inputs with seed `20261109`, 480 calls: all matched original source slices and the relevant types/grouping contract. |
| Coordinator payload-free | 48 calls over empty/singleton/duplicate input, optional fixed key, both verbs/backends/keep modes: promised cell row counts matched. |
| A: implementation and shapes | 166 distinct Margin calls, including 165 Nest calls, plus one inspection. Core membership, factor and special-key assertions passed; unusual-shape differences were adjudicated with native construction/union/nesting controls. |
| B: public contracts | 180 primary conditions covering empty fixed keys, keep/original values, key-only duplicates, factor missingness, output names, literal families and capture/forwarding. A final 40-condition audit covered empty and populated fixed factors with NA levels and missing codes through both verbs/backends. Additional native and deliberately corrupted-cell controls established the classifications and oracle sensitivity. |
| C: consistency | 112 basic calls: 94 matches and 18 explained refusals; 24 fixed-factor NA-level/missing-code comparisons; 14 derived single-evaluation/ownership paths; 12 Mutable-step refusals; six exact External-condition comparisons and twelve masked-setter/copy controls. |

The coordinator's types were character, numeric with NA/NaN/infinities,
unordered/ordered factors, factor with an NA level and missing codes, Date,
POSIXct with timezone and local list keys. Payloads included lists and factors.
A additionally placed matrices, arrays, packed frames, list-matrices,
factor/Date/difftime matrices and `I(POSIXlt)` in payload/fixed/dimension roles.
B covered `.key`/`.id` names including data.table specials, `.rows`, boolean
spellings, private prefixes, Unicode, newlines and numbered-dot names.

## Short hypothesis ledger

| Hypothesis and basis | Execution result and disposition | Next evidence |
| --- | --- | --- |
| `.keep` copies or omitted dimensions mixed membership or original values. | Literal/seeded source-slice oracles and independent lanes matched both verbs and backends. Refuted for the selected admitted fixtures. | A different admitted key type or input graph. |
| Empty fixed-key inputs received keyless compensation, or payload-free cells lost duplicate rows. | Empty/fixed/implicit-group and genuine key-only comparisons matched the distinct Nest/Nest_by contracts. B's deliberately one-row payload-free cells failed its multiplicity oracle. Refuted. | A different key representation or grouping state. |
| NA factor levels and actual missing codes merged, or payload factor restoration lost values. | Source codes, levels and cell memberships matched in the selected factor fixtures. Refuted. | A different admitted factor representation. |
| `.key`/`.id` or internal-looking names overwrote payloads. | Role-separated name controls matched; prohibited `..1`/`...` payload names were also refused by native cell conversion. Refuted for the accepted names tested. | An untested accepted spelling. |
| Unusual payload/key shapes were damaged by Nest. | dtplyr matrix expansion and array truncation already occurred in native input collection. Local `I(POSIXlt)` multi-set cast failures and difftime-matrix shape loss also occurred in ordinary `union_all()`. Factor/Date matrix recursion also occurred in native tidyr nesting. No independent package defect established. | An admitted source whose direct native composition preserves the property. |
| Complex dtplyr keys failed because of private identity grouping. | Single-set, fixed-key and native nesting succeeded; rollup and ordinary `union_all(step, step)` both raised the same unsupported-complex error. Union delegation explained the failure. | A case whose ordinary union succeeds. |
| Forwarding/splicing selected a different specification or repeated caller code. | B's two-layer embrace, forwarded quosures, quosure/spec/product splices and selection-returning constructor/namespace controls matched expected source slices and one-read budgets. Arbitrary nested factories refused while top-level factories were accepted. Refuted. | A different capture boundary. |
| Derived dtplyr branches re-evaluated source work or mutated caller tables. | Filter/cache/arrange/rename/group/join/union paths evaluated their callback once, with no ordinary-Nest construction-time evaluation, and retained source bytes. Mutable roots on either contributing input refused before callbacks or writes. Refuted for the selected graphs. | A different admitted graph. |
| Nest changed an External condition or resolved a caller's setter/copy binding. | Six selection/execution sentinels remained identical condition objects; twelve masking controls received zero calls. Refuted for these boundaries. | A different callback or condition path. |
| List-key rollup's `Invalid input` was a Nest-specific failure. | Native list-key grouping refused too; ordinary list-literal `mutate()` reproduced rollup's translation error. Native list union succeeded, so this was distinguished from the complex-union boundary. No supported successful list-key Nest path was established. | An admitted native list-key grouping path or a successful list-literal translation. |
| Stock converter selection isolated all empty generated cells. | The nested `as.data.frame.tbl_df` registration replaced the schema, with native/restoration/ownership controls. Confirmed existing #1503 residual. | The implementation task's public regression and repair. |

Numbered-dot names required distinguishing their roles. `..1` and `...` as
payload names reached native converter refusals. As `.key`, they were retained;
ordinary dtplyr `collect()` could still refuse its default name repair, while
`as_tibble(..., .name_repair = "minimal")` retained the correct cells.
No additional package-specific name guarantee was inferred.

After the independent reports were integrated, a coverage audit found B's
initial empty fixed-factor check was local-only and its NA-level fixture used
only a variable dimension. B added both verbs/backends with empty and populated
fixed NA-level/missing-code keys; C independently checked fixed keys and payload
factor restoration under every sort/keep choice. These checks closed the
identified gaps. No concrete, executable, promising hypothesis remained
unresolved when the investigation moved to publication. The #1503 singleton
fallback was deliberately limited to the row-wise keyless empty dtplyr path;
ordinary Nest, keyed-empty and local behavior supplied separate controls.

Oracle errors were corrected before results were accepted. The literal matrix
completed its comparisons but its first summary print passed tibble's `n`
argument to a plain data frame; the corrected full matrix exited 0. The seeded
oracle initially tried to combine ordered factors with different outer Margin
levels when matching active display values; comparing their displayed active
values resolved that oracle error. An attempted dtplyr `tidyr::nest(.by = a)`
control created a list column named `.by` instead of selecting the intended
keys. The corrected `nest(data = -a)` and grouped-summary controls both refused
list keys. All 480 generated and 48 payload-free comparisons were rerun before
the successful native controls. A corrected its own data.table column-indexing
comparison. These were harness errors, not package findings.

## History, duplicate search, environment and limits

The [recent independent note][prior], [empty-cell note][prior-empty] and
[membership/converter note][prior-list], together with source history through
PR #1524, supplied prior candidates and repair boundaries. Their earlier
execution counts were not reused. Required context included `CLAUDE.md`,
`AGENTS.md`, `CONTEXT.md`, local-check/integration/Issue/PR procedures, public
Nest/Grouping references and the relevant ADRs on lifecycle, capture,
Grouping-kind rules, identifiers, factors, attributes, query policy and Mutable
steps. The full existing #1503 and #1505 bodies and comments were retrieved.

All-state CLI snapshots contained 785 Issues and 737 PRs. Searches covered
Nest/Grouping/cell conversion, `as.data.frame.tbl_df`, empty fallback and
nested converter terms, including closed reports and merged PRs. The #1503
comment already contained the same nested-conversion minimum and ownership
boundary; a new Issue would duplicate it. No tracker state was changed.
A fresh main fetch after reproduction still resolved to the tested SHA.
The submitted note's separate integration and verification records belonged
on its PR; investigation results certified only the tested SHA and environment.

Executions used arm64 macOS Tahoe 26.6.2, R 4.6.1, C.UTF-8 locale, dplyr
1.2.1, tidyr 1.3.2, tibble 3.3.1, dtplyr 1.3.3, data.table 1.18.6.1,
rlang 1.3.0, tidyselect 1.2.1, vctrs 0.7.3, testthat 3.3.2 and pkgload 1.5.3.
All R execution handles were collected through terminal completion. Temporary
evidence roots were:

- `/private/tmp/marginplyr-nest-parent-20261109/`: fixed source archive,
  literal and seeded oracles, native controls, residual observed/desired
  reproductions, historical replays and all-state tracker snapshots.
- `/private/tmp/nest-20261109-a-unique/`: implementation/shape programs,
  native controls and `ledger.md`.
- `/private/tmp/nest-20261109-b-unique/`: public-contract/name/capture probes
  and `ledger-b.md`.
- `/private/tmp/nest-20261109-c-unique/`: processing-consistency probes.
  `ledger.md` recorded derived-source, factor, condition and dependency controls.

Arbitrary S3/S4 classes and registrations, unbounded specification trees,
other R/dependency/OS versions, interruption/resource failures, untested derived
graphs and external database systems remained unverified. No uniform element
subclass across backends or arbitrary input-attribute restoration was required.

The note's consumers were investigation readers. No runtime, package test,
dependency, generated documentation or build behavior changed. Those effects
justified Review-ready `source`, with package build/check, complete suite and
snapshots omitted. The evidence-only submission required no NEWS entry.

[empty]: https://github.com/sayuks/marginplyr/issues/1503
[residual]: https://github.com/sayuks/marginplyr/issues/1503#issuecomment-6721252926
[frame]: https://github.com/sayuks/marginplyr/issues/1512
[capture]: https://github.com/sayuks/marginplyr/issues/1505
[nest]: ../man/nest_with_margins.Rd
[attributes]: ../design/adr/0016-delegate-result-class-and-attributes-to-dplyr.md
[prior]: nest-independent-conversion-boundaries-2026-11-08.md
[prior-empty]: nest-empty-cell-conversion-2026-11-08.md
[prior-list]: nest-cell-membership-and-converters-2026-11-08.md
