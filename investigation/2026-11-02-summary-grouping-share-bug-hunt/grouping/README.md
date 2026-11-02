# Grouping specification and inspection boundaries

Investigated: 2026-11-02
Baseline: `978d7bef4247bf1348af5b90bfab47924db25500` (`origin/main`)

This lane investigated Grouping specification capture, retained selection values,
structural preflight, and the correspondence between fixed keys and dimensions.
No package implementation or package regression test was changed. The retained
harnesses loaded this checkout through `pkgload::load_all()` and ran from its
root. The checks below were focused investigation loops, not Review-ready or a
complete-suite result.

The sources read included `CLAUDE.md`, `AGENTS.md`, `CONTEXT.md`,
`design/agents/local-checks.md`, and ADRs 0005, 0007, 0008, 0013, and 0026. Recent
context came from the October 29 exhaustive Grouping lane, the October 31
Grouping selection/expansion lane, and the mixed-selection finding in
`summary-grouping-total-share-bughunt-2026-10-31.md`. Those dated observations
were not counted as executions in this lane.

## Confirmed: a known fixed-key overlap waited for typed metadata

ADR 0005 required locally decidable preparation failures before backend reads.
A Grouping dimension already resolved to a fixed `.by` column satisfied that
boundary even when an independent later selection needed types.

The minimized supported derived dtplyr input was:

```r
frame <- tibble::tibble(a = 1:2, b = c("x", "y"))
step <- dtplyr::lazy_dt(frame) |> dplyr::mutate(b = "z")
inspect_grouping(
  step,
  .by = a,
  .grouping = grouping_sets(a, where(is.character))
)
```

The observed condition was:

```text
Can't safely determine column metadata for `.data` from this dtplyr step.
ℹ Obtaining its types or factor levels here could evaluate upstream operations on source rows.
ℹ If you want that work to happen now, collect the input with `dplyr::collect()` and pass the resulting data frame.
```

Changing only the later selection from `where(is.character)` to `all_of("b")`
produced the locally decidable condition without asking for collection:

```text
Columns cannot appear in both `.by` and `.grouping`:
ℹ `a`.
```

An ordinary local input produced that overlap condition in both forms. A live
DuckDB input also eventually produced the overlap condition in both forms, but
the typed form first invoked `DBI::dbSendQuery()` with:

```sql
SELECT *
FROM mixed_plan_preflight
LIMIT 0
```

A temporary trace observed the actual DBI method call. This evidence was not
inferred from a construction-time Sent query record. The names-only control
invoked no query. Tracing was removed and the connection disconnected at script
exit.

The ranked hypotheses were:

1. The discarded mixed-selection pass resolved a valid leading selection but
   did not apply the fixed-key overlap rule to its answer.
2. The caller's `.by` was still unresolved before metadata acquisition.
3. The difference was specific to dtplyr's metadata safety refusal.

The names-only control, local/DuckDB comparison, and implementation inspection
separated the first hypothesis. `resolve_fixed_keys()` had already resolved the
bare `.by = a` before structural preflight. The mixed path called
`validate_leading_selections(preflight, data_vars)`, which expanded/resolved
leading selections but received no fixed keys. The overlap check remained in
`compile_grouping_spec_impl()`, after typed metadata acquisition on that path.
The entirely names-only path compiled the discarded plan and therefore reached
that overlap check before metadata acquisition. Existing
`test-grouping-leading-errors.R` covered invalid selections and completed nested
family errors; its mixed-preflight cases used no fixed keys and did not
establish this overlap boundary.

### Completed matrix and controls

`mixed-plan-preflight.R` crossed five constructors, five consumers, local and
unsafe-metadata dtplyr inputs, and names-only/typed later selections. DuckDB
added inspection, summarization, and expansion. SQL nesting was excluded
because input admission refused it before Grouping compilation. The completed
matrix comprised 130 public calls:

- All 65 names-only controls reported the overlap and invoked no metadata query.
- All 25 typed local calls reported the overlap.
- All 25 typed dtplyr calls substituted the metadata refusal for the overlap.
- All 15 typed DuckDB calls invoked one zero-row metadata query before reporting
  the overlap.

Report mode asserted these observed outcomes and exited 0. `--assert` required
all calls to report the overlap and invoke no query; it intentionally exited 1
on `all(records$overlap) is not TRUE`. The baseline remained unrepaired.

The five consumers were `inspect_grouping()`, `summarize_with_margins()`,
`expand_with_margins()`, `nest_with_margins()`, and `nest_by_with_margins()`.
Every constructor demonstrated the same failure, so the evidence supported one
shared preparation ticket rather than separate backend, constructor, or verb
tickets.

### Duplicate search and distinction

The full body and comments of
[#1090](https://github.com/sayuks/marginplyr/issues/1090), and the full body and
comments of its merged repair
[PR #1120](https://github.com/sayuks/marginplyr/pull/1120), were read. That report
and repair concerned a leading selection that was itself invalid, such as a
missing column. Here each selection was valid: the failure arose from the
relationship between a resolved dimension and an already resolved fixed key.
The repaired selection traversal did not receive the latter. The original
report's missing-column reproduction remained covered by the repair; this lane
did not propose reopening it or duplicate its demonstrated trigger.

The recorded searches used `gh issue list --state all` for
`"Columns cannot appear in both"` (no matches), `"overlap" "metadata"`,
`"fixed" "typed"`, and `"Duplicate grouping" "metadata"`; the broader searches
returned other typed-selection, naming, and backend reports but none describing
this trigger. `gh pr list --state all` searches for `"1090"` returned #1120
and investigation PR #1098; `"overlap" "metadata"` returned other merged
selection/metadata and investigation changes. Open and closed issues, and merged
PRs, were included. No independently matching report was identified by these
queries. The coordinator performed its own main confirmation and publication
checks separately.

## Bounded negative loop: retained ordinary selection values

`retained-selection-values.R` compared public inspection with direct typed
`tidyselect::eval_select()` using the same written selection shape. The matrix
crossed 23 values, active bare bindings versus caller-defined
constructor-shaped calls, zero through two redundant-parenthesis layers, and
all five constructor positions: 690 comparisons.

Values included empty selections, positive/negative/missing integer indices,
logical values, character vectors, symbols and calls, expression vectors,
quosures, lists, factors, complex/raw/matrix values, and supported/refused
predicate functions. Constructor-shaped calls deliberately held an unbound
argument that their returned-value function did not force. Each case checked
the selected first-set columns or the complete External-condition class vector,
the documented empty rollup/cube refusal where applicable, and the number of
caller reads. All 690 comparisons matched and read the original binding or
constructor once.

An initial recorder compared constructor-call results against a bare-name
oracle. Tidyselect treats a function returned by a call differently from a
bare function-valued external name, so that comparison was not like-for-like.
The final oracle retained the same call or bare-name shape as the public call.
The empty-dimension diagnostic matcher was also corrected to its actual stable
fragment. The completed retained log reported 690 matches and exited 0; neither
initial harness mismatch established a package finding.

## Replay and limits

Run from the repository root:

```sh
Rscript investigation/2026-11-02-summary-grouping-share-bug-hunt/grouping/mixed-plan-preflight.R
Rscript investigation/2026-11-02-summary-grouping-share-bug-hunt/grouping/mixed-plan-preflight.R --assert
Rscript investigation/2026-11-02-summary-grouping-share-bug-hunt/grouping/retained-selection-values.R
```

The adjacent CSVs and logs retained each completed matrix, minimized symptoms,
query SQL, and assertions. The report-mode log recorded R 4.6.1 on aarch64 macOS
Tahoe 26.6.2, dplyr 1.2.1, dtplyr 1.3.3, data.table 1.18.6.1, DuckDB 1.5.5,
DBI 1.3.0, dbplyr 2.6.0, tidyselect 1.2.1, rlang 1.3.0, and pkgload 1.5.3.
`jarl check` on this lane's artifact directory passed after the retained
harness's loop braces and negation were made explicit.

These finite observations did not establish arbitrary selection callbacks,
custom specification field methods, unbounded trees, alternate dependency
versions, remote databases, interruption/concurrency behavior, or the safety of
rejecting a known duplicate family before a later unresolved selection. The
latter can change existing plan-error precedence and was not promoted to a
separate finding. No implementation, package test, complete-suite execution,
package build, or rendering was performed in this lane.
