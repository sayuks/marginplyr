# Nesting backend graph and bridge investigation

Investigated: 2026-11-04
Baseline: `78ebbc36200bdc5cef70838dd4e772c5c4a1401c` (`main`)

## Outcome and contracts

The graph and bridge search established no additional independently actionable
backend defect. A subsequent independent validation confirmed the local track's
factor-subclass value-loss finding on both local and dtplyr inputs through all
four Margin verbs. No package implementation or permanent package test changed.

The complete October 30, October 31, November 2 and November 3 backend records
were consulted, together with the November 3 coordinating record. Their
measurements were not counted as executions on this baseline. The nesting and
inspection references, `CONTEXT.md`, and ADRs 0002, 0005, 0016, 0018, 0020 and
0029 supplied the contracts. Source inspection followed
`dtplyr_metadata_safe_step()`, `dtplyr_selection_proxy()`,
`dtplyr_step_input_fields()`, `isolate_dtplyr_cached_inputs()`,
`nest_cell_expr()` and `nest_expanded_margins()`.

The ranked hypotheses were that generated reads would confuse data.table
special names, that cached fan-in would corrupt cells or caller tables, and
that a bridge would hide a one-shot Arrow source from inspection safety. The
finite controls below did not establish those failures.

## Completed measurements

| Evidence | Finite coverage | Outcome |
| --- | --- | --- |
| [Special-name graphs](special-name-graphs.R) | 22 names in grouping, payload and list-column-name roles; both nesting verbs and keep choices; immutable root, filtered root and cached root | 792 comparisons: 702 exact projected matches, 78 local refusals, and 12 default-collection name-repair errors. All 792 caller-table snapshots stayed identical. |
| [Cached fan-in](cached-fan-in.R) | 28 graph builders over zero, one and four rows, with or without a `.SD` payload; cache/rename/order/filter/select, repeated caches, join/semi/anti/set operations and shared branches | Of 168 source graphs, 12 failed native construction or collection and were excluded. The remaining 156 supplied 468 public-operation observations: 416 exact matches, 36 metadata refusals, 12 local Grouping-overlap refusals and four nested-cell-order differences. No caller-table snapshot changed. |
| [Cell-order controls](cached-order-controls.R) | A cached joined input reused by a union with one ordered branch | All seven cell multisets matched the local-source oracle, and ordinary collection of the source query before and after nesting was identical. |
| [Remote bridge safety](remote-bridge-safety.R) | Arrow readers behind root/join/union/grouped/windowed/collapsed queries, registered with DuckDB | 24 boundary calls: 12 nesting refusals did not force their Grouping specification or send a statement; 12 name-only/typed inspections sent exactly 12 `LIMIT 0` statements. All six readers retained all 24 original rows. |

The special-name set included `.SD`, `.N`, `.I`, `.BY`, `.GRP`, `.NGRP`,
`.EACHI`, `.rows`, `T`, `F`, `...`, `..1`, `.data`, `.env`, data.table
constructor formal names, an internal-looking name, whitespace and Japanese.
The local-source oracle compared exact outer order under `.sort = "last"`
and exact cell values, normalizing cell classes to tibbles only for comparison.
It did not impose one cell class across backends.

The 12 default-collection errors occurred only for non-rowwise nesting with
`.key = "..."` or `"..1"`. `dtplyr::collect()` passed these names to strict
tibble repair. `test-dot-dot-nesting.R` already made this boundary explicit by
using `tibble::as_tibble(result, .name_repair = "minimal")` for lazy results;
rowwise nesting applied its own minimal conversion and matched. These errors
were therefore classified as upstream conversion constraints, not new package
findings. The other local refusals covered dot-dot input names and the
documented `.rows` rowwise-identifier refusal.

The four fan-in differences occurred only in populated shared cached joined
unions, with and without the special payload, through both nesting verbs.
They changed ordering inside cells, not row multiplicity, source values,
outer keys or Grouping set identifiers. The independent control sorted each
cell by both original row identifiers and required exact equality. It passed.
No documented nested-cell ordering violation was established, so no repair
ticket was proposed from those observations. The 36 conservative metadata
refusals were distinct/cache graphs; a refusal was not counted as a successful
execution of the refused operation. The 12 local inspection refusals selected
the grouped fixed key `h` again through `where(is.character)`.

The bare `list()` expression was also considered against prior evidence. Both
October 31 and November 2 backend records had already shown that masking it
also broke ordinary grouped dtplyr summary and upstream tidyr nesting. This
track did not repackage that unresolved upstream ownership boundary as a new
finding. Those graph and bridge candidates required no Issue publication.

## Independent factor-subclass validation

The local track reported that a factor subclass's display conversion could
replace source values with missing factor codes. The independently authored
[minimal validation](factor-subclass-independent.R) used a different subclass
and a character method that preserved empty length and missingness, decorating
only nonmissing labels. Same-class vctrs combination and cast methods admitted
ordinary grouping and nesting without changing the factor's codes or levels.

For a one-row source whose factor had code `1L` and level `a`, native
collection, grouped summary and tidyr nesting retained code `1L` on both
backends. All four Margin verbs with an included-only `grouping_set(g)` and
the default nonmissing label instead returned code `NA_integer_`, despite
retaining levels `a, Total`. Passing `.margin_label = NULL` retained code `1L`.
The caller's serialized source stayed identical in every case. The probe
required no omitted grouping set, union or external helper mask.

The public *Display labels and grouping identity* and *Result class and
attributes* sections promised factor reconstruction with its declared levels;
ADR 0016 made this the package's own exception to delegated restoration, and
ADR 0012 specified the distinction between a level and a missing code. This
was a value-loss finding rather than a request to restore an arbitrary
subclass. `margin_display_character()` dispatched the subclass's character
method, but `reconstruct_factor_vector()` matched that decorated display
against the original undecorated levels. The resulting nonmatch became a
missing code. `encode_factor_for_margin()` made the same assumption in its
separate factor-preservation route.

The [finite validation matrix](factor-subclass-validation.R) completed 340
calls across both backends, all four verbs where the role applied, variable
dimensions, fixed keys, payloads, both nesting keep choices and string/NULL
labels. Its five fixtures were a one-row factor, ordinary repeated factors,
an unused NA level, a used NA level and actual missing codes alongside an NA
level. Native collection and nesting controls first preserved each fixture's
codes and levels. Of 260 applicable outer-code comparisons, 120 differed and
140 matched; of 160 applicable inner-code comparisons, 48 differed and 112
matched. The remaining fields were inapplicable, not passes. All 340 source
snapshots stayed identical, and no call raised.

With an NA level declared, even fixed-key and payload factors could cross the
encoding route and lose their nonmissing codes; this also reached copies made
for `.keep = TRUE`. Without an NA level, the default-label included-dimension
route sufficed. These manifestations shared the same display-versus-level
assumption and were recommended as one repair ticket covering the two
conversion routes. No separate backend ticket or prefactor was necessary.
The coordinator owned current-main and duplicate checks and published the
shared repair as [#1215](https://github.com/sayuks/marginplyr/issues/1215).

## Replay and terminal evidence

From the investigated checkout root, or with `MARGINPLYR_HUNT_SOURCE` pointing
to a source copy of the baseline:

```sh
Rscript investigation/2026-11-04-nest-spec-greedy/backend/special-name-graphs.R /tmp/special-name-graphs.csv
Rscript investigation/2026-11-04-nest-spec-greedy/backend/cached-fan-in.R /tmp/cached-fan-in.csv
Rscript investigation/2026-11-04-nest-spec-greedy/backend/cached-order-controls.R
Rscript investigation/2026-11-04-nest-spec-greedy/backend/remote-bridge-safety.R
```

The four graph/bridge commands returned terminal exit 0. Yielded sessions `4593`
(special names), `10089` (fan-in), and `70137` (bridge) were polled through
their terminal results; the independent cell-order control completed
synchronously. An initial fan-in draft used `names()` on a dtplyr step rather
than the collected input's names; session `96057` was stopped and collected at
exit 1 before correction. An initial bridge draft required both integer columns
from the join even for the name-only `row` selection; session `16026` returned
exit 1 before that oracle was corrected. Neither incomplete run counted toward
the completed totals or a package finding. No owned execution remained active.

The final factor matrix returned terminal exit 0 through session `61868` after
an earlier complete run's native nesting assertion was strengthened. The
minimal independent validation with `--assert` returned terminal exit 1 through
session `54041`, after its native and typed-missing controls passed; its desired
assertion required code preservation and did not pin the defect. Adjacent logs
retained those results. Replay with:

```sh
Rscript investigation/2026-11-04-nest-spec-greedy/backend/factor-subclass-independent.R --assert
Rscript investigation/2026-11-04-nest-spec-greedy/backend/factor-subclass-validation.R /tmp/factor-subclass-validation.csv
```

Every execution loaded a dedicated `git archive` copy of the pinned baseline
through `pkgload::load_all()`, with testthat parallelism disabled. Adjacent CSVs
and logs retained the final observations and `sessionInfo()`. The environment
was R 4.6.1 on macOS Tahoe 26.6.2 arm64, dplyr 1.2.1, dtplyr 1.3.3,
data.table 1.18.6.1, vctrs 0.7.3, tibble 3.3.1, DBI 1.3.0, DuckDB 1.5.5
and Arrow 25.0.1. No dependency was installed or changed.

These finite probes did not establish behavior for arbitrary callback bindings,
extension methods, every input graph or graph depth, nondeterministic inputs,
hosted backends, other dependency/R/OS versions, or resource exhaustion. Bridge
observations established inspection/refusal safety, not nesting support for
SQL or Arrow. No package build or full-suite run was performed by this track.
