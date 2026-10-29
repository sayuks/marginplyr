# Nesting and Grouping selector dispatch follow-up

Investigated: 2026-10-29
Baseline: `59d55a7867ffc0255f75be32a233d9d475bcb7fb` (`origin/main`)

## Outcome and ticket boundaries

This investigation started from fetched main in a new worktree on
`investigation/nest-spec-bug-hunt-2026-10-29`. The primary targets were
`nest_with_margins()`, `nest_by_with_margins()`, Grouping specification
constructors, and Grouping-plan inspection. Four independently actionable
defects were reproduced and published through `to-tickets`:

| Finding | Impact observed | Ticket |
| --- | --- | --- |
| F1: typed selector dispatch | Fixed keys disappeared, nesting merged groups, or constructors refused a valid dimension | [#995](https://github.com/sayuks/marginplyr/issues/995) |
| F2: cached row predicates in metadata | Valid inputs were refused or emitted warnings during empty-proxy evaluation | [#996](https://github.com/sayuks/marginplyr/issues/996) |
| F3: dimensional scalar identifiers | Empty rowwise nesting failed after the same identifier succeeded on populated input | [#997](https://github.com/sayuks/marginplyr/issues/997) |
| F4: repeated cached rename | An ordinary immutable source failed when reused across grouping-set branches | [#1003](https://github.com/sayuks/marginplyr/issues/1003) |

Every ticket had `bug` and `ready-for-agent` labels and no blockers. F1 grouped
two false names-only classifications under the same dispatched-selection
behavior. F2 and F4 shared a cached-input fixture but concerned different
operations: acquiring metadata and executing generated branches. Either could
be repaired and verified independently. F3 concerned only the admitted scalar
identifier's passage through empty-result reconstruction. No prefactoring
ticket or parent-issue change was needed. The user delegated ticket splitting
and publication, so the skill's approval interview was omitted.

Only dated investigation evidence was added. Package implementation, package
tests, generated documentation, dependencies, and configuration were unchanged.
The defects remained reproducible; no repair was attempted.

## Context and method

The investigation read `CONTEXT.md`, the public nesting/constructor/inspection
references, the architecture's preparation and review boundaries, relevant
ADRs 0002, 0005, 0007–0009, 0013, 0015, 0016, 0018–0020, 0026, and 0029,
and the dated nesting investigations from 2026-10-25, 2026-10-26, and
`investigation/2026-10-29-nesting-grouping-bug-hunt/README.md`. The latest
expansion/Grouping and 2026-10-28 summary/Grouping hunts supplied additional
typed-selection history. Those records' successful probes were not counted
as measurements of this snapshot.

Three parallel tracks examined local nesting, selector evaluation, and backend
preparation. Public-call experiments used direct typed tidyselect, independent
`vctrs::vec_group_loc()` source membership, exact source projections, explicitly
collected input controls, and ordinary backend operations. The coordinating
investigator independently replayed the decisive dispatch, identifier, cached
predicate, and cached rename cases. Ordinary backend reproduction established
mechanisms; it did not by itself decide whether a package contract applied.

Existing and recently closed issues were checked before publication. Related
typed-selection repairs #825, #889, #924, and #975, scalar-name repair #831,
metadata-composition repairs #837 and #892, and upstream-report draft #468
did not cover these remaining boundaries. A final refresh also found other
investigations' #992–#994; their Arrow/SQL/expansion findings were outside these
four ticket boundaries and were not republished.

## F1: ordinary dispatch was mistaken for a names-only selection

`is_name_only_helper_arg()` accepted unclassed atomic values and a whitelist
of base functions as sufficient evidence that a helper argument needed only
column names. Ordinary S3 generics could dispatch on implicit classes even
when `is.object()` was false. The called method could legitimately inspect
the typed selection context.

The minimal fixture used a one-element integer matrix and a caller-defined
`format.matrix` method returning the names of character columns from
`tidyselect::peek_data()`. On input with character `region` values east/west
and integer `amount`, direct tidyselect selected `region`. Inspection with
the same selection in `.by` reported no fixed key. Both nesting verbs returned
one two-row cell instead of two one-row cells. A rollup reported:

```text
`rollup()` requires at least one dimension.
```

Giving the matrix an explicit class attribute or using an opaque wrapper
around the generic restored the expected selection. The six implicit classes
matrix, array, character, numeric, integer, and logical reproduced the issue
for `format()`, `unique()`, and `sort()`. The other six tested functions did
not dispatch the supplied implicit-class method in these fixtures and agreed
with their direct-selection controls. No claim was made that those functions
could never dispatch under other inputs.

The public matrix exercised 324 cases: 270 failed the intended selection or
row-count comparison and 54 produced the right plan. The successful cases
were local `grouping_set()`, `grouping_sets()`, and `grouping_spec()` inspections
whose canonical typed pass recovered the answer. Local rollup/cube could
refuse before that pass; the dtplyr inspection shortcut retained the wrong
names-only answer across all five constructors. Fixed-key inspection,
summarization, and both nesting verbs were affected on both backends.

A second public reproduction injected a classed integer selector implementing
valid vctrs integer cast methods. Direct typed tidyselect and its `all_of()`
control selected `region`; `.by = !!selector` selected nothing.
`is_name_only_expr()` classified that atomic object without excluding its
classed dispatch. This supplied the second F1 boundary without a malformed
Grouping specification or a deprecated bare external-vector selection.

`specification/reproduce-generic-dispatch.R --assert` and
`specification/reproduce-classed-selector.R --assert` both exited 1 at their
intended-behavior assertions. Their ordinary-selection controls succeeded.
The constructor and inspection references delegated selection to tidyselect;
ADR 0008 supplied the evaluation-budget boundary and ADR 0013 the inspected
plan contract. No new selection syntax was proposed.

## F2: a saved cached filter escaped the zero-row rewrite

An immutable dtplyr root held character `g` and integer `v`. Filtering by
whether the first `v` element exceeded zero, then caching and arranging,
produced a source that collected successfully. Typed inspection and both
nesting verbs instead reported:

```text
Can't safely determine column metadata for `.data` from this dtplyr step.
```

The same arranged filter without the cache, the cached filter without a
downstream operation, and the explicitly collected input succeeded. Replacing
the first-element predicate with a positive-minimum check emitted:

```text
no non-missing arguments to min; returning Inf
```

Ordinary collection of the populated source emitted no such warning. A
counting predicate recorded a zero-length invocation during metadata
acquisition. The traced dtplyr schema evaluations had isolated zero-row roots;
no full source-row read was established by this evidence.

`zero_row_dtplyr_proxy_input()` removed the ordinary filter step's row clause
but retained the original filter expression in the cached local bindings.
A downstream operation forced that saved expression on the synthetic empty
input. `isolated_dtplyr_proxy()` then converted its first-element error into
the metadata refusal; a warning-producing predicate could escape as a warning.
This was a false refusal and an unnecessary caller-code evaluation, rather
than evidence that these row-only filters needed real rows to determine types.

The affected matrix covered two populated source sizes, two predicates,
rename/arrange/join/union derivations, and three consumers. Twenty-four
first-element cases were refused; all 24 minimum cases emitted a warning.
The 144 uncached, cached-only, and explicitly collected controls succeeded.
Four minimum/rename cases also reached the separate F4 collection error;
those overlapping outcomes were not counted as additional F2 refusals.
Caller tables remained unchanged. The metadata and Mutable-step boundaries
remained those in ADRs 0002, 0020, and 0029.

## F3: an admitted scalar `.id` kept its dimensions only in the empty fallback

This was sufficient to reproduce the failure:

```r
empty <- tibble::tibble(value = integer())
nest_by_with_margins(empty, .id = matrix("set", 1L, 1L))
```

The identifier passed scalar-string admission and `rlang::is_string()`.
The same identifier succeeded on a nonempty input. The empty rowwise call
failed with a tidyselect error that its subscript was a matrix; a one-element
three-dimensional array failed analogously. Local and dtplyr inputs reproduced
both failures. A plain, named, or AsIs string succeeded and returned one row,
integer identifier 1, and one zero-row cell. Ungrouped nesting retained its
documented zero-row result.

`normalize_margin_id()` removed names while retaining dimensions. Ordinary
finalization concatenated its leading column names into a plain vector. The
empty rowwise reconstruction instead passed `.id` directly to
`reorder_margin_columns()`, whose `all_of()` refused a matrix or array.
This located the failure after admission, in a branch that already promised
the one-empty-cell result. The scalar-shape sweep independently found exactly
these four failures among 496 altered calls and their 496 scalar controls.
`coordinator/reproduce-dimensional-id.R --assert` exited 1 after its ordinary
empty-input and dimensional nonempty-input controls passed.

## F4: immutable cached aliases were renamed twice across result branches

The input used only ordinary columns and the default immutable permission:

```r
input <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), v = 1:2)) |>
  dplyr::filter(TRUE) |>
  dplyr::compute() |>
  dplyr::rename(amount = v)
dplyr::collect(nest_with_margins(input, .grouping = rollup(g)))
```

Ordinary collection of `input` succeeded. Both nesting verbs failed for the
rollup with:

```text
Items of 'old' not found in column names: [v]. Consider skip_absent=TRUE.
```

The single-grouping-set control succeeded, as did caching again after the
rename. Portable expansion and summarization also failed. An ordinary dtplyr
union of the renamed step with itself reproduced the error: the first
reference renamed the shared cached alias in place, and the second attempted
the same rename after `v` had gone. Caller source tables stayed unchanged.

That upstream control explained the mechanism without removing the package
boundary: a Margin operation introduced the multiple references behind a
supported immutable input, and no unusual source class or attribute restoration
was involved. Open #468's draft concerned roots explicitly built with
`immutable = FALSE`. This cached step had default immutable roots, passed the
package's root-permission check, and still failed. F4 was therefore published
separately from that upstream-report work and from F2's metadata evaluation.

## Bounded search coverage and exclusions

Counts below refer to different feedback loops and were not combined into one
test total. Matching error outcomes were controls, not successful nesting.

| Retained evidence | Completed observations |
| --- | --- |
| `specification/generic-dispatch-sweep.R` | Nine functions × six implicit classes: 18 dispatch-related mismatches and 36 matching direct-selection outcomes |
| `specification/generic-public-matrix.R` | 324 public cases across two backends, four fixed-key consumers, and five constructors: 270 intended-behavior failures and 54 correct local plans |
| `nesting/recursive-local.R` | Seed 291029; 260 datasets, 0–10 rows, 17 scalar/structured shapes, four explicit plans, fixed-key presence, both verbs and keep modes, sampled Margin order: 984 admitted calls and 1,992 source-membership/projection comparisons matched; 56 raw-dimension calls received the documented typed-missing refusal |
| `nesting/classed-probes.R` | 768 calls / 384 backend pairs: 300 membership agreements, 72 upstream refusal pairs, and 12 Period membership differences explained by the independent controls |
| `nesting/scalar-shapes.R` | Eight argument roles and named/AsIs/matrix/array scalar shapes, empty/populated inputs, local/dtplyr, both verbs, no-grouping/rollup: 492 matches and four F3 errors, each compared with its scalar control |
| `backend/graph-metadata.R` | 4,476 typed plan/error comparisons over root/derived, post-operation, and join/set graphs matched; 280 graph construction/collection or invalid-grouping fixtures were skipped |
| `backend/extended-boundaries.R` | 24 Arrow graph calls preserved all eight one-shot readers; 32 live SQL boundary calls sent zero SQLite statements and eight DuckDB statements, all `LIMIT 0`; 12 unsafe-schema refusals ran no caller callback |
| `backend/cached-predicate.R` | 48 affected F2 cases and 144 successful controls, with F4 overlap recorded separately |

The recursive matrix included recursively packed frames, zero-column packed
frames/matrices, 3D arrays, NULL/expression list values, NA-level factors,
missing/nonfinite doubles, complex/raw, dates, zoned timestamps, and difftime.
Its oracle compared original source rows and exact nested projections,
including original keys under `.keep`.

The classed extension included integer64 values above double precision, hms,
units, blob, zoo and yearmon/yearqtr, lubridate Interval/Period/Duration,
haven labelled/tagged-NA/SPSS, list_of, and AsIs values. The 134 cell-value
differences were confined to Interval/Period S4 slots. Ordinary dtplyr filtering
and union reproduced inconsistent Interval slot lengths, and ordinary grouping
merged one-month and two-month Period keys whose underlying seconds were zero.
Ordinary data.table grouping also refused the list-like keys, and ordinary
dplyr union refused the zoo indexes. `nesting/upstream-s4.R` retained those
controls. These arbitrary class/backend limits were excluded under ADR 0016;
they were distinct from F4's ordinary-column cached-alias failure.

Deprecated bare external-vector selections, malformed hand-forged specifications,
and dot-dot source names refused by ordinary tidyselect were not promoted to
findings. No absence-of-bugs conclusion was inferred from the passing cases.

## Replay, package checks, and limits

Run from the investigated checkout. Every retained R script accepts an optional
repository-root first argument and loads that checkout with `pkgload::load_all()`.
The following commands assert the desired repaired behavior and intentionally
exited 1 on the baseline:

```sh
Rscript investigation/2026-10-29-nest-spec-dispatch-followup/specification/reproduce-generic-dispatch.R --assert
Rscript investigation/2026-10-29-nest-spec-dispatch-followup/specification/reproduce-classed-selector.R --assert
Rscript investigation/2026-10-29-nest-spec-dispatch-followup/backend/cached-predicate.R --assert
Rscript investigation/2026-10-29-nest-spec-dispatch-followup/coordinator/reproduce-dimensional-id.R --assert
Rscript investigation/2026-10-29-nest-spec-dispatch-followup/backend/reproduce-cached-rename.R --assert
```

The other sweep scripts reported dated observations and exited 0; they were
investigation evidence, rather than permanent package regressions. Some
deliberately recognized the still-broken outcomes. Their passing exit codes
did not imply that any finding had been repaired. The logs and CSVs retained
the measured results; `MANIFEST.sha256` identified the delivered evidence.

The existing focused suite completed with terminal exit 0 for ten files:

```sh
NOT_CRAN=true Rscript -e 'testthat::test_local(".", filter="^(nest-operation|nest-rows|grouping-plan|grouping-interface|inspect-grouping|selection-helper-arguments|grouping-backends|dtplyr-special-nesting|cyclic-grouping|injected-nested-grouping)$", reporter="summary")'
```

The investigation material was excluded from the source package by the existing
`.Rbuildignore`. Under `design/agents/local-checks.md` this was Repository-only,
so the Package-affecting Review-ready check was not required. Applicable lint,
context-budget, reference, diff, and two-axis review outcomes were recorded on
the pull request. CI monitoring was outside the request.

`environment.txt` recorded the local macOS/R/package versions. The finite
probes did not establish behavior on a live PostgreSQL server or hosted
database, another operating system or dependency version, arbitrary custom
methods beyond the explicit dispatch fixtures, unbounded plans/depth, or large
performance loads. SQL/Arrow checks established the stated inspection and
refusal boundaries; they did not establish nesting support on those backends.
