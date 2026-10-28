# Nesting and Grouping specification bug hunt

Investigated: 2026-10-28
Base: `ac6ad79ff6026abe5fc45538d6dfefadf03a8ca3`

## Outcome and scope

The investigation examined `nest_with_margins()`, `nest_by_with_margins()`,
the five Grouping specification constructors, and `inspect_grouping()`.
Shared selection, metadata, naming, and finalization code was followed where
it affected those interfaces. A new managed worktree was created from `main`
on `codex/nest-spec-audit-20261028`; fetching confirmed that `main` and
`origin/main` both named the base above at the start of the audit.

Four independently reproduced defects were published through `to-tickets`:

| Issue | Defect | Priority | Blocked by |
| --- | --- | --- | --- |
| [#946](https://github.com/sayuks/marginplyr/issues/946) | A constructor-shaped ordinary selection was evaluated twice and could change the Grouping plan and source-row partitions | P2 | None |
| [#947](https://github.com/sayuks/marginplyr/issues/947) | A filtered dtplyr input containing `.SD` failed while acquiring its typed metadata | P2 | None |
| [#948](https://github.com/sayuks/marginplyr/issues/948) | dtplyr nesting collided with locked `.SDall`, `.xSD`, and `.iSD` bindings | P2 | None |
| [#949](https://github.com/sayuks/marginplyr/issues/949) | Temporary column renaming invalidated an accepted sf input before nesting | P2 | None |

Each ticket covered one cause and an independently verifiable public path;
symptoms across the two nesting verbs stayed together. All received `bug` and
`ready-for-agent`. Their implementations did not depend on one another. The
user delegated ticket granularity and publication, so the skill's breakdown
interview was omitted. No parent issue was changed.

This commit added investigation evidence only. It did not repair package code,
add package regression tests, change generated documentation, or amend a public
contract. The fix and post-fix cleanup phases of `diagnosing-bugs` were outside
the requested investigation scope. The failing scripts deliberately remained
red on the audited source.

## F1: a constructor-shaped selection lost its first answer

A two-row input had `a = c("x", "y")`, `b = c(1L, 1L)`, and a payload row
identifier. A caller bound `rollup` to a zero-argument function returning
`"a"` on its first invocation and `"b"` thereafter, then placed that call
inside `marginplyr::grouping_sets()`. Both nesting verbs invoked it twice,
selected `b`, and returned one cell holding both source rows. The identical
function bound to `ordinary` was invoked once and returned the two expected
cells keyed by `a`. Inspection exposed the wrong plan without nesting.

Three explanations were separated: loss of the preflight answer, loss of the
captured environment, and a nesting/finalization error. Identical-function
alias controls preserved the environment and succeeded. Inspection reproduced
the problem before finalization. Reading
[`read_grouping_arg()`](../../R/grouping-plan.R) showed that the spelling gate
evaluated the call, but retained a non-specification value only when the
original expression was a bare name. The call's first value was discarded and
its original expression was evaluated again during typed selection.

The retained matrix crossed all five constructor spellings, plain calls,
parentheses around the whole call or its head, and both nesting verbs. Its
three modes established wrong membership, a caller warning raised twice, and
an unnecessary failure from a function that errored only on its second call.
All 90 checks failed; all 90 ordinary-name controls passed. A separate
exploratory matrix reproduced the double evaluation across five parent
constructors and five public consumers (125 cases).

[ADR 0019](../../design/adr/0019-resolve-contextual-helper-names-by-static-spelling.md)
established that the caller's own constructor binding ran.
[ADR 0008](../../design/adr/0008-centralize-grouping-specification-kind-rules.md)'s
amendment for #873 explained retaining a non-specification bare-name answer.
Its evaluation table did **not** explicitly assign a budget to the
constructor-shaped ordinary-selection case. The finding therefore recorded
the discarded answer and observable extra invocation, rather than claiming
that table already promised one invocation for this shape. Ticket #946 asked
for that shape to be settled explicitly while preserving the other budgets.

## F2: `.SD` was refused while rebuilding a filter's proxy

An immutable dtplyr root containing a character column named `.SD` nested
successfully. Adding `filter(TRUE)` made both nesting verbs fail during
construction with:

```text
A column may not be called .SD. That has special meaning.
```

The minimum needed only one source column and one source row. Ordinary
collection of the filtered input still succeeded. Root typed inspection and
filtered name-only inspection also succeeded; filtered typed inspection failed.
These controls separated an input/backend limitation from the package's proxy
construction.

The subset branch of
[`dtplyr_selection_proxy()`](../../R/backend-metadata.R) rebuilt parent
columns using a list-to-data.table converter that refused `.SD`, although a
source data.table could already carry that name. This occurred before the
nesting fold's separate `.SD` protection was reached. The retained script
measured 13 failing assertions across empty/nonempty inputs, typed fixed-key
and dimension inspection, both nesting verbs, both `.keep` settings, and
Mutable-step typed inspection. A calibrated row-predicate counter stayed at
zero and the caller table stayed identical.

Direct column-selection steps involving `.SD` also failed during ordinary
dtplyr collection in additional controls. Those calls were excluded from the
positive-input evidence and from #947's acceptance scope. The confirmed
finding required only a working row-filter input. Ticket #947 kept
[ADR 0020](../../design/adr/0020-ask-before-reading-a-lazy-input.md)'s prohibition
on processing source rows and
[ADR 0029](../../design/adr/0029-refuse-a-step-a-backend-would-write-to.md)'s
Mutable-step boundary.

## F3: additional data.table internal names broke the fold

A one-row, one-column input named `.SDall` was sufficient: lazy nesting
constructed, but collection raised:

```text
cannot change value of locked binding for '.SDall'
```

`.xSD` and `.iSD` had the same failure. The row-wise verb reached the error
inside its own collection. Source collection, local nesting, a grouped scalar
summary, Margin expansion, and safe-renamed nesting were successful controls.
Ordinary tidyr nesting on the same lazy source also failed, so upstream
nesting compatibility was not used as evidence for this finding.

[`margin_safe_column_reads()`](../../R/utils.R) protected other data.table
special names but omitted these three locked bindings. The fold's direct
column references still collided with data.table's evaluation environment.
The nesting reference's explicit lack of reserved input names, and the
package's existing private-alias handling, were the relevant contract and
ownership boundary.

The agent's initial matrix made 90 calls. The retained replay expanded it to
180 by crossing both `.keep` settings: three names, zero/one/three rows,
payload/fixed-key/dimension/identifier/list-column roles, and both verbs.
All 144 payload, fixed-key, dimension, and `.id` calls failed. All 36 `.key`
controls succeeded and matched local source-cell values. The caller inputs
and independently collected roots stayed unchanged. #948 was separate from
#947 because its cause and failure boundary were the fold's evaluation names,
rather than metadata reconstruction.

## F4: temporary aliases broke sf's geometry-column metadata

A two-row sf data frame with an ordinary character key `g` and point geometry
failed through both nesting verbs, with no grouping or with `.by = g`:

```text
internal error: can't find sf column
```

Both local upstream nesting counterparts represented those source cells.
Converting the input to a plain tibble while retaining the identical geometry
vector also allowed nesting with a rollup, `.keep`, `.id`, and Margin order.
The geometry payload itself was therefore not the failing boundary.

[`reorder_margin_columns()`](../../R/utils.R) temporarily renamed every local
column to a numeric alias through `dplyr::select()`. sf's method correctly
updated `sf_column` to the geometry alias, `"2"`. Restoring ordinary names
through `rlang::set_names()` left that attribute pointing at `"2"` while the
columns were again `g, geometry`. The two-step rename alone created the
invalid intermediate object. The next sf method reported its broken invariant.
The retained script passed the upstream and plain-tibble controls before
detecting all four public failures, and verified the source was unchanged.

This did not establish a requirement to preserve an outer sf result or restore
arbitrary subclass attributes.
[ADR 0016](../../design/adr/0016-delegate-result-class-and-attributes-to-dplyr.md)
left those properties to dplyr and vctrs. The defect was package-owned naming
invalidating an accepted input during that composition. The ticket required
an end-to-end nesting result: sf's own summary method imposed additional
geometry constraints, so correcting the first rename alone would not prove
the completed nesting path worked.

## Preserved replay and negative results

Run the scripts from the audited repository root. They loaded that working
tree with `pkgload::load_all()` and required the recorded installed dependencies.
The four `reproduce-*` scripts asserted intended behavior and exited 1 on this
snapshot. The other scripts exited 0. Adjacent `.txt` files retained their
output, with trailing whitespace removed for Git; multiple failed cases
represented variants of four causes, not
independent findings.

| Script | Oracle and measured outcome |
| --- | --- |
| [reproduce-constructor-selection.R](reproduce-constructor-selection.R) | Explicit two-cell source-row oracle; 90 failures and 90 successful alias controls |
| [reproduce-sd-metadata.R](reproduce-sd-metadata.R) | Collected/local plan controls and source-preservation checks; 13 failures, zero row-predicate work |
| [reproduce-locked-bindings.R](reproduce-locked-bindings.R) | Local source-cell controls; 144 failures and 36 successful `.key` controls |
| [reproduce-sf.R](reproduce-sf.R) | Upstream nesting, identical geometry payload, and metadata-validity controls; four failures |
| [specification-algebra.R](specification-algebra.R) | Seed `20261028`, 1,000 trees of all five kinds to depth three; independent expansion matched exact membership and order with duplicate occurrences retained |
| [local-membership.R](local-membership.R) | 936 special fixed-key/dimension-name combinations with an NA-level/sentinel factor and matrix key; all 3,744 grouping-set membership comparisons matched independent `vctrs::vec_group_loc()` groups |
| [dtplyr-differential.R](dtplyr-differential.R) | Seed `5871`, 250 mixed configurations; 196 local/dtplyr value-and-cell comparisons passed, 54 declared factor-level collisions matched explicit expected refusals |
| [dtplyr-empty.R](dtplyr-empty.R) | 84 empty row-wise reconstruction cases matched local cells and names |
| [inspection-types.R](inspection-types.R) | 240 comparisons across local, dtplyr, Arrow Table/RecordBatch, and live DuckDB; typed fixed-key and all five constructor selections matched each source's collected representation |

For example:

```sh
Rscript investigation/2026-10-28-nest-spec-bug-hunt/reproduce-sd-metadata.R
Rscript investigation/2026-10-28-nest-spec-bug-hunt/specification-algebra.R
```

Additional exploratory work covered scalar and structured local columns,
integer64/labelled/time/list classes, special names in five roles, derived
dtplyr graph shapes, and 12 local data.table source-preservation combinations.
Those scratch probes were not part of the retained replay bundle and their
attempt counts were not added to the table. Reproductions and retained
negative-result harnesses were replayed by the coordinating investigator.
The `.SD`, sf, and locked-name failures were also independently confirmed in
small public calls; `.SD` and sf were additionally confirmed after installing
the audited source into a disposable library outside the repository.

Rejected leads included manually forged non-quosure specification arguments
(outside the documented constructor interface under ADR 0015), dot-dot output
names rejected by upstream collection, and data.table's unsupported list/raw/
complex grouping. Matrix stack overflow or packed-column unpacking already
present in `lazy_dt()` was not attributed to nesting. Unsafe derived dtplyr
metadata remained a deliberate refusal under ADR 0020. Repeated cube units
expanded correctly in bounded timing probes and were not filed as a
performance defect.

Existing related evidence was read in
[the 2026-10-26 nesting audit](../nesting-grouping-bug-hunt-2026-10-26.md),
[its typed-selection follow-up](../nesting-specification-followup-2026-10-26.md),
and [the 2026-10-27 audit](../2026-10-27-nest-grouping-bughunt/README.md).
Open issues and targeted all-state searches for the new failure names were
checked before publication. Existing repaired `.rows`, injected-parentheses,
AsIs, foreign-helper, and raw-missing defects were not refiled.

## Validation, environment, and limits

The focused existing suite exited 0 with:

```r
testthat::test_local(
  ".",
  filter = paste(
    "nest-operation|nest-rows|dtplyr-special-nesting|dtplyr-asis-nesting",
    "grouping-plan|grouping-interface|inspect-grouping|grouping-selection-budget",
    "injected-nested-grouping|cyclic-grouping|packed-grouping|matrix-grouping",
    sep = "|"
  ),
  reporter = "summary",
  stop_on_failure = TRUE
)
```

The existing `.Rbuildignore` excluded this entire investigation directory.
The commit was Repository-only under
[local checks](../../design/agents/local-checks.md), so it did not require a
full Review-ready check. The pull request recorded both linters and the
context-budget/document-reference checks run before publication.

[environment.txt](environment.txt) recorded the dependency versions: R 4.6.1
on aarch64 macOS, dplyr 1.2.1, tidyr 1.3.2, dtplyr 1.3.3, data.table
1.18.6.1, rlang 1.3.0, tidyselect 1.2.1, sf 1.1-2, Arrow 25.0.1, and
DuckDB 1.5.5. The tested operating system and dependency versions bounded the
observations; older releases, other systems, unlimited specification depth,
large resource stress, and live PostgreSQL were not covered. A finite audit
could establish these failures and negative results, not universal absence
of other bugs. CI monitoring was not performed.
