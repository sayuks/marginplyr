# Independent Nest membership and conversion-boundary investigation

Investigated: 2026-11-08
Tested main: `d6f1f45824ddc04fd25fcd67860a016b2f9b6ea0`

A freshly fetched main supplied a new worktree on
`investigation/nest-independent-20261108`. Three fresh-context agents examined
implementation, public contracts and processing consistency at that same SHA.
They did not share candidates before their first reports. The coordinator
independently prepared literal grouping-set expectations and small synthetic
inputs. Each lane used a separate disposable Git archive, serial R execution
and `TESTTHAT_PARALLEL=false`. Scripts and logs stayed outside the submitting
worktree. No database was needed or accessed.

One previously unreported, independently repairable defect was published as
[#1512][new]. Two other reproduced paths belonged to existing reports:
[#1503][empty] and [#1505][capture]. They received additional evidence instead
of duplicate Issues. This submission recorded evidence only, with no runtime
or package-test repair.

## New finding: a generated frame reached registered tibble conversion

A registered `as_tibble.data.frame` method received the expanded intermediate
table before either local Nest verb folded it into cells. A replacement acting
only on receivers with a private `..marginplyr_nest_` column changed the single
payload value of `tibble(rid = 17L)` to `-99L`. Both verbs returned the replaced
value, without warning or error. One outer row and one cell row remained, so
cardinality-only assertions would miss the defect.

| Single-condition control | Measured result |
| --- | --- |
| Explicit conversion of the original input | `17L`, zero targeted callbacks |
| Native tidyr nesting, dplyr nesting, and `list(pick())` summary | `17L`, zero targeted callbacks |
| Either Margin Nest verb under registration | `-99L`, one targeted callback |
| Either Margin verb after restoring registration | `17L` |

The captured receiver names were `rid` and `..marginplyr_nest_1`. This located
the effect on a generated intermediate table rather than ordinary conversion
of a caller-owned subclass. Source serialization remained unchanged. The
desired-value assertion exited 1 after native, restoration, cardinality and
ownership controls passed; the observed-result mode exited 0.

The path was `nest_expanded_margins()` through `margin_column_source()` and
its `dplyr::as_tibble()` generic. The public [Nest reference][nest-ref] promised
cells holding the input's non-key columns and original keys under `.keep`.
[ADR 0016][attrs] delegated arbitrary class/attribute restoration, but its
generated-converter amendment did not delegate replacement of source values.
The native and explicit-input controls distinguished those boundaries.

### Adjacent paths and repair unit

The implementation lane crossed three carriers, both verbs, four plan shapes
and both keep values. All 32 tibble/eager-data.table cases replaced values;
sixteen plain-data.frame controls retained values with no targeted callback.
Restoring registration restored every baseline. A separate coordinator process
used three payload values with keyless, fixed and rollup plans through both
verbs and keep modes: twelve paths reproduced the replacement.

Six outer-key comparisons changed visible keys while leaving source row IDs
intact. Fixed-only grouping merged two expected cells into one containing all
source IDs. A dimension rollup retained identity-separated cells but displayed
the replacement key. A mixed fixed-key rollup shrank from four cells to three,
including merged subtotal partitions. These symptoms shared the same conversion
boundary and were kept in one Issue.

The consistency lane independently distinguished local/eager and lazy paths.
The populated tibble/eager-data.table cases reproduced value replacement;
plain frames and dtplyr retained values with no targeted callback. The
zero-length replacement did not establish a schema difference for empty local
inputs even where the callback ran. No additional arbitrary-attribute guarantee
was inferred.

[#1490][list] had repaired local generated **list** conversion, and #1503 had
repaired a different empty dtplyr **cell** conversion. Their repaired calls did
not bypass the earlier frame generic. [PR #962][sf] had introduced subclass
normalization to preserve valid sf composition; that behavior remained a repair
constraint, not an excuse to restore every subclass attribute.

Exactly `test-nest-local-list-converter.R` was selected after inspecting its
helpers and execution ownership. It passed with no skips and parallel execution
disabled. Its registration targeted `as_tibble.list`; the empty dtplyr tests
targeted `as.data.table` methods. A source search found no registered
`as_tibble.data.frame` condition. Their ordinary value assertions did not
execute this failure. The focused pass was development evidence, not a full
suite or Review-ready result.

Issue #1512 contained the standalone minimum, contract sources, expected and
actual values, impact, environment and failing-public-regression acceptance
criteria. It covered one complete fix-and-verification slice across both
verbs, with no blockers or new dependency, labelled `bug` and `ready-for-agent`
under `to-tickets`. The user authorized publication and delegated ticket
granularity without a preliminary approval step.

## Existing cause: the empty fallback's stock converter dispatched again

The #1503 repair selected stock `as.data.table.data.frame()` for the singleton
empty dtplyr cell. That method internally called the `as.data.frame()` generic
on the generated tibble. A registered `as.data.frame.tbl_df` method therefore
still replaced integer `rid` with character `injected` in keyless empty
`nest_by_with_margins()`. The outer cardinality, zero cell rows and integer
identifier `1L` stayed correct.

Native collection, ordinary empty dtplyr Nest, local empty row-wise Nest and
registration restoration retained the original schema. The failing fallback
invoked the replacement once. Source bytes remained unchanged. Two independent
archive processes reproduced the minimum; observed assertions exited 0 and
desired-schema assertions exited 1 after the controls and restoration.

The consistency lane's 200-comparison converter boundary matrix also separated
this path from #1512. Eight initially-empty/filtered-empty dtplyr row-wise
cases replaced the schema; populated, keyed-empty, ordinary Nest and eager/local
controls did not. Filtered-empty inputs ran their predicate once. The trace
identified the generated tibble receiver and the nested `as.data.frame(x)`
call inside the stock converter.

This remained the same empty-fallback ownership and acceptance boundary as
#1503, rather than another independent ticket. [Additional evidence][empty-add]
recorded the post-#1511 SHA, minimum and nested conversion mechanism. The report
was left closed; no implementation or tracker-state change was made.

## Existing cause: aliased language capture escaped the quotation boundary

The #1505 repair preserved directly spelled language captures. Aliases of
`quote`, `expression` and `alist` still let public preprocessing insert
`marginplyr_keep_empty` into quoted constructor language before a top-level
factory inspected it. A factory matching an ordinary quoted template selected
`a` under raw evaluation and separately evaluated ordinary rlang capture, but
selected `b` when written inline in `.grouping`.

With two different `a` values and one common `b` value, the stored/native
controls produced two cells, with source IDs `1` and `2`; both Nest verbs'
inline calls produced one cell holding `1, 2`. Thus the changed language caused
wrong row membership, not only an invisible attribute difference. Each factory
ran once.

A twelve-comparison matrix crossed four capture aliases with inspection and
both Nest verbs: nine quote/expression/alist paths differed, while three
substitute controls matched. The coordinator independently reproduced both
Nest paths. Observed assertions exited 0 and desired-contract assertions
exited 1. The initial coordinator control incorrectly placed its ordinary
capture inside public capture; moving that control outside established the
intended independent boundary before accepting the result.

This was the existing quoted-factory mutation cause, so [additional
evidence][capture-add] went to #1505. It requested preservation of the original
language alongside the existing authored-empty/splice protection; no new Issue,
dependency or specification decision was introduced.

## Independent expectations, inputs and finite execution coverage

Input generation and expected-value generation were separate. The expected
families were literal lists of included columns, not compiler or inspection
output. Original source positions were partitioned by fixed/included keys and
extracted directly. Explicit selections kept the tracing row ID out of keys.
Checks matched outer keys and `.id` to source-row multisets, cell columns,
values, retained original keys and guaranteed factor types/levels/codes. True
payload-free fixtures acquired no artificial ID; their source-key partitions
supplied expected multiplicities. Agreement between the two verbs alone was
never the correctness oracle.

Counts below described overlapping finite lanes; they were not summed into
exhaustive coverage or a package-check result.

| Lane | Fresh execution and result |
| --- | --- |
| Coordinator membership | 1,872 matches over eight type fixtures, zero/one/six rows, six literal families, optional fixed key, keep modes, both verbs and local/dtplyr. Structured list/matrix/packed keys ran locally; their dtplyr combinations were excluded from this matrix. |
| Coordinator generated inputs | Seed `20261108`, 100 separately generated inputs and 400 successful calls through both verbs/backends. Existing fixed groups, NA-level ordered factors, actual missing codes, duplicate keys, list payloads, keep choices and all sort choices were represented. |
| Coordinator payload-free | 48 matches across empty/singleton/duplicate key-only input, optional fixed key, both verbs/backends and keep modes. |
| Coordinator names | 856 role-separated calls: 780 exact matches and 76 documented/upstream refusals. Names included private prefixes, `.rows`, `.SD`, dot-dot names, pronouns, boolean words, a backtick, newline and Unicode spellings. Four native dot-dot payload controls refused too. Sixteen extra `.key`/payload-name overlap calls retained original values. |
| A: implementation | 384 manual comparisons: 336 matched; 48 were classified by native list-key grouping refusal or packed-column expansion before Nest. Seventy-two custom equality/display comparisons and a packed-factor payload control matched. Four deliberately corrupted membership signatures were rejected. |
| B: contracts | 240 manual-family matches over five fixed-key types, empty/populated input, union/composite cube/overlapping product, keep, both verbs and explicit/existing groups. Another 120 opaque-quosure quote/function/IIFE comparisons matched across all five constructors and inspection/both Nest. |
| C: derived input | 480 calls over thirty graphs: local 236 matched and four grouped-plus-by calls refused as specified; dtplyr 124 matched, 112 received safe metadata refusals and four grouped-plus-by calls refused. Root/filter/select/rename/arrange/mutate/group/cache, joins and set operations were represented. |
| C: empty/evaluation | 240 exact empty-schema comparisons across five schemas, initially/filtered empty inputs, both verbs/keep/all sort/key choices. Keyless Nest returned zero rows; Nest_by returned one schema-shaped cell. Stateful filtering shared one evaluation across sets; six error/warning controls preserved class, message, parent and once-only count. |

The first coordinator oracle misspelled the product constructor as
`grouping_product`; its 312 lookup failures were harness errors. The corrected
`grouping_spec` matrix was rerun and supplied the 1,872 matches above. The
name oracle initially expected six outer rows where the hand-enumerated fixed
partitions required five; all 856 cases were rerun after correction. A native
summary that never selected a prohibited payload name was removed as an invalid
refusal control; the four native nesting controls supplied the final evidence.
These corrections were not package findings.

## Short candidate ledger

| Hypothesis and basis | Execution and disposition | Next evidence |
| --- | --- | --- |
| `.keep` copies or omitted dimensions mixed source membership or factor values. | Manual/generated oracles matched source slices, factor codes/levels and kept keys. Refuted for the admitted fixtures. | A different admitted type or input graph. |
| Empty fixed-key input entered keyless compensation, or payload-free duplicate rows disappeared. | Fixed/existing-group, empty and true key-only comparisons matched their distinct contracts. Refuted. | A new key representation or grouping state. |
| Internal or output names overwrote payloads. | Role-separated names and overlap controls matched or received the documented/upstream refusal. Refuted for the chosen names. | An untested accepted spelling. |
| Derived dtplyr branches mutated inputs or mixed cached/joined sources. | Native derived-source slices, row-ID multisets and source serialization matched; unprovable metadata paths refused. Refuted for the selected graphs. | A different admitted graph. |
| Generated conversion was isolated once the list and direct frame fixes landed. | Generated-frame payload/partition corruption confirmed as #1512; nested fallback dispatch confirmed as #1503 residual. | Public regressions and repairs in those implementation tasks. |
| Recursive quosure preservation corrupted language data. | 120 ordinary-selection controls retained opaque quosures and read deferred active bindings zero times. Refuted. | A different capture/injection boundary. |
| Quotation protection covered aliases. | Nine mismatches and independent wrong-cell reproduction confirmed the #1505 residual. | Extend that report's capture regression. |

After integrating all three reports, each confirmed path was checked through
both verbs where the path existed. The dtplyr fallback was deliberately limited
to row-wise keyless empty behavior; ordinary Nest and local controls were
separate. Element-class differences were accepted as documented. No concrete,
executable, promising candidate remained pending at the checkpoint.

## Freshness, duplicate search, environment and artifacts

Main was fetched again after reproduction and before publication; it still
resolved to the tested SHA. All-state CLI exports contained 777 Issues and
732 PRs before #1512. Title/body searches used `as_tibble.data.frame`, frame
conversion and Nest/cell conversion terms; separate open/closed Issue and
all-state PR searches for `as_tibble.data.frame` returned no matches. Full
bodies/comments of #1490, #1503 and #1505 and the #962 PR body were read.
The #1512 title, open state and labels were fetched after publication.

Recent [cell-membership][prior-list], [empty-cell][prior-empty] and
[systematic][prior-systematic] notes and source history through #1511 supplied
historical context. Their execution counts were not reused. The residual
findings explicitly concerned post-repair main; tracker closure was never used
as evidence of correct behavior.

Executions used arm64 macOS Tahoe 26.6.2, R 4.6.1 and C.UTF-8 locale; dplyr
1.2.1, tidyr 1.3.2, tibble 3.3.1, dtplyr 1.3.3, data.table 1.18.6.1,
rlang 1.3.0, tidyselect 1.2.1, vctrs 0.7.3, testthat 3.3.2 and pkgload 1.5.3.
All original execution handles were collected through terminal completion.
Temporary evidence roots were:

- `/private/tmp/marginplyr-nest-independent-root/`: coordinator archive,
  oracles, generated inputs, controls, independent failing assertions, tracker
  snapshots and publication drafts.
- `/private/tmp/marginplyr-nest-independent-a/`: implementation archive,
  converter minima/matrix, custom equality probes, focused-test log and ledger.
- `/private/tmp/marginplyr-nest-independent-b` and
  `/private/tmp/marginplyr-nest-b-*`: contract archive, oracles, opaque-quosure
  controls, alias minima and ledger.
- `/private/tmp/marginplyr-nest-independent-c` and
  `/private/tmp/marginplyr-nest-c-evidence/`: consistency archive, derived and
  empty-input probes, converter traces, minima and boundary matrix.

Other dependency/R/OS versions, arbitrary S3/S4 classes or method registrations,
unbounded specification trees, untested derived graphs, interruption and
resource failures, and external database systems remained unverified. Further
evidence in those areas could reopen a hypothesis. No source-row order within
cells, additional arbitrary-attribute restoration or common backend cell
subclass was required.

The submission added only this dated evidence note for investigation readers.
It changed no package code, tests, installed files, dependencies, generated help
or build behavior and needed no NEWS entry. Those consumers and effects
justified Review-ready `source`; package build/check, complete suite and
snapshots were omitted. Integration, validation and review records named the
separately committed note revision on its PR.

[new]: https://github.com/sayuks/marginplyr/issues/1512
[empty]: https://github.com/sayuks/marginplyr/issues/1503
[capture]: https://github.com/sayuks/marginplyr/issues/1505
[empty-add]: https://github.com/sayuks/marginplyr/issues/1503#issuecomment-6721252926
[capture-add]: https://github.com/sayuks/marginplyr/issues/1505#issuecomment-6721238497
[list]: https://github.com/sayuks/marginplyr/issues/1490
[sf]: https://github.com/sayuks/marginplyr/pull/962
[nest-ref]: ../man/nest_with_margins.Rd
[attrs]: ../design/adr/0016-delegate-result-class-and-attributes-to-dplyr.md
[prior-list]: nest-cell-membership-and-converters-2026-11-08.md
[prior-empty]: nest-empty-cell-conversion-2026-11-08.md
[prior-systematic]: nest-spec-systematic-validation-2026-11-07.md
