# Summary, Grouping specification, and share bug hunt

Investigated: 2026-10-26

## Snapshot and method

The inspected main commit was
`6b91254016dbcce60cc2cc69071f01f69eb8084e`. The investigation used a new
managed worktree and branch `audit/margin-bug-hunt-2026-10-26`. No package
implementation, package tests, generated documentation, or dependency metadata
was changed. This directory preserves dated evidence, not a replacement for
the public references or ADRs.

The investigation read `CLAUDE.md` and its `AGENTS.md` closure, `CONTEXT.md`,
the relevant agent instructions, and the decisions governing grouping,
Contextual helpers, shares, conditions, and typed metadata. Three parallel
investigations covered ordinary summary evaluation, Grouping specifications,
and contextual shares; a fourth compared backend behavior and unusual names.
Each finding was reproduced through the public API, minimized, compared with
controls, and traced to a specific planning or rewriting boundary. No fix was
attempted.

The runtime was macOS arm64, R 4.6.1, marginplyr 0.1.0 loaded from the pinned
working tree, pkgload 1.5.3, dplyr 1.2.1, tidyselect 1.2.1, rlang 1.3.0,
dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5,
RSQLite 3.53.3, and testthat 3.3.2.

## Replaying the findings

From the repository root:

```sh
Rscript investigation/2026-10-26-summary-grouping-share-bug-hunt/reproduce.R
```

On the inspected commit the command completed in approximately one second,
printed six independent `FAIL` verdicts, and exited 1. This is an intentional
red-capable investigation harness: each assertion states the expected contract,
so a corrected implementation can make that probe pass. An unexpected execution
error also fails the probe. Run one probe by appending its identifier, for
example `F1`. [Measured output](reproduce-output.txt) preserves the actual
values and diagnostics; [the script](reproduce.R) preserves the inputs.

## Findings and tickets

Each ticket is an independently verifiable repair with no blocking edge to
another finding. F3 and F4 both involve eager summary planning, but preserving
an External condition and preserving a selector's evaluation context are
separate behaviors. F5 combines both dependency directions because one
reflection exemption causes both failures. No prerequisite refactoring was
identified.

| Probe | Priority | Finding | Ticket |
| --- | --- | --- | --- |
| F1 | P2 | Qualified arithmetic selects the wrong fixed key | Pending publication |
| F2 | P2 | Parenthesized `across()` changes per-group evaluation | Pending publication |
| F3 | P2 | A failed `.names` expression is retried and its error disappears | Pending publication |
| F4 | P2 | Adding a share removes an ordinary selector's summary context | Pending publication |
| F5 | P2 | Explicit mask lookup bypasses share dependency validation | Pending publication |
| F6 | P3 | A non-specification external binding is read twice | Pending publication |

### F1: Qualified arithmetic selected the wrong fixed key

For character `a = c("x", "x")` and numeric `b = 1:2`, a valid tidy-select
expression chose column position two when `a` was character. Direct dplyr
therefore grouped by `b` and returned two count-one rows. The Margin summary
grouped by `a` and returned one count-two row. `inspect_grouping()` independently
reported the wrong fixed key. Putting the same selection in a rollup with
fixed `a` produced a false `.by` overlap error; wrapping that selection in
`identity()` produced the correct plan.

The expression used a qualified arithmetic head, `` base::`*`(...) ``, whose
operand consulted `tidyselect::peek_data()`. Bare `*` is refused by tidyselect,
but qualified or parenthesized `*` and qualified `^` are evaluated as ordinary
calls. `is_name_only_expr()` used `static_call_name()` and then the
`selection_refused_operators()` test, erasing that distinction and answering
that every such spelling was settled by names alone. `resolve_fixed_keys()`
consequently settled the key against `grouping_name_proxy()`, whose synthetic
integer column made the operand choose position one. The key was not
re-resolved against typed metadata. A false Grouping-specification refusal
occurred before any typed snapshot was acquired.

The discriminating controls ruled out damaged typed metadata and quosure
injection: `identity()` used the same metadata successfully, and an inline
selection without injection reproduced the failure. The violated boundary was
the name-only reader's contract and ADR 0005's distinction between locally
settled and schema-dependent checks. Existing refused-operator tests exercised
bare operators; they did not exercise these ordinary-call spellings. Closed
#342 established the names-alone rule, and #825 repaired type-dependent helper
arguments; this finding reached a different classifier branch.

### F2: Parenthesized across changed per-group evaluation

For `g = c("a", "b", "b")` and `x = c(1, 10, 20)`, an ordinary function
factory captured `n()` and returned a function adding that offset to `sum(x)`.
Parenthesized `across()` returned `x = c(2, 32)` under dplyr and
`x = c(1, 30)` under marginplyr. Parentheses around either the head or the
whole call reproduced the discrepancy. Unparenthesized `across()` agreed at
`c(1, 30)`, and `identity(across(...))` agreed at `c(2, 32)`.

The shared static readers recognized through parentheses, and the rewrite
reconstructed an unwrapped top-level `across()` call. That activated dplyr's
syntactic `expand_across()` path: the factory observed expansion's zero-row
summary context instead of each group's context. The cause was the interaction
of `rewrite_summary_selections()` with `rewrite_evaluated_call_parts()`,
`map_call_parts()`, and `rebuild_static_call()`.

This finding exposes a contract conflict, rather than ignoring an intentional
decision. The summary reference promises dplyr data-masking semantics and
ordinary lookup for `n()`. ADR 0019's parentheses amendment prescribes
transparency on the premise that `(` being identity makes evaluation unchanged.
The executable example falsified that premise in dplyr's syntactic expansion.
The ticket asks to retain static recognition while preserving the evaluation
boundary and reconciling that premise. Existing spelling tests did not give
`.fns` a factory observing the current group. Parenthesized `pick()` and
`if_any()` did not exhibit across's particular promotion mechanism.

### F3: A failed names expression disappeared on retry

An `across()` naming callback raised a custom error on its first invocation
and returned `"total"` on subsequent invocations. Without a share, the local
Margin call raised the error after one read. Adding a Total share of `total`
made the call succeed with `total = 3`, share `1`, and three callback reads.
The original External condition disappeared.

`rewrite_across_selection()` caught errors while evaluating `.names`, kept the
original expression, and allowed another evaluation.
`known_across_output_names()` had a corresponding error-swallowing prediction
path. The first unnamed local selection took these paths when a share was
present. Direct dplyr controls raised after one read; lazy dtplyr and simulated
SQL Margin construction retried and succeeded even without a share. Live
DuckDB native construction also succeeded after two reads. Arrow Margin
construction behaved similarly, but its ordinary uncollected dplyr control
delayed naming evaluation, so no uncollected Arrow error comparison was claimed.

The violated guarantee was the External condition contract in `CONTEXT.md` and
ADRs 0015 and 0022: context may change, but the caller's error must not be
suppressed and replaced by success. Existing tests used permanently failing
names expressions, which still errored after retry. Closed #684 covered extra
successful naming callbacks; it did not eliminate this failed-evaluation path.

### F4: Adding a share removed an ordinary selector's context

The first unnamed ordinary `across()` selected `x` using a selector consulting
`n()`, followed by the independent source `total = sum(x)`. Direct dplyr and
the no-share Margin call returned ordinary columns `unused = 3, total = 3`.
Adding a Total share of `total` raised the data-masking-only `n()` error before
execution. Moving the same `across()` after `total` made the call succeed.
The probe used `n() == 0L` because that was the context ordinary dplyr's
top-level expansion actually provided; it did not assume a row-count meaning
that dplyr itself lacked.

`plan_summary_expressions()` deferred local ordinary selections except the
first unnamed expression when any share existed. `resolve_summary_selections()`
therefore evaluated that expression's `.cols` without dplyr's summary mask.
Direct and caller-callback selectors both reproduced the failure. The share
used `total`, not the across output, so this was not a necessary source-name
resolution or a dependency refusal. The violated contract was the documented
local summary-mask semantics. Closed #701 and its regression covered a first
named packed selection; this finding reached the unnamed branch.

### F5: Explicit mask lookup bypassed share dependency validation

A source written as `first + 1` was refused because it depended on the earlier
ordinary alias `first`. Replacing it by
`get("first", pos = -1L) + 1`, or by a lookup with
`envir = environment()`, bypassed that refusal. Parent and Total shares both
accepted the source. `get0()`, `exists(where = -1L)`, and `mget()` with
inheritance reproduced the same dependency gap.

The reverse direction also escaped: after defining a share named `share`, a
later ordinary summary used `sum(get("share", pos = -1L))`. With a same-named
input column containing `20, 60`, the result returned `derived = 20, 60, 80`,
aggregating the original input column while displaying shares `.25, .75, 1`.
The corresponding direct reference was refused. An actual external environment
remained a successful control. Simulated PostgreSQL also admitted the forbidden
source; that established a planning result, not live PostgreSQL execution.

`lookup_has_external_env()` treated the presence of `pos`, `envir`, `where`, or
`frame` as proof that the lookup could not reach the data mask.
`reflective_lookup_symbols()` then returned an empty dependency set. Targeted
inspection confirmed that an omitted environment reported `first` as a
dependency while an explicitly supplied default position reported none.
ADRs 0010 and 0017 and closed #173 require the same dependency policy for
statically recoverable reflective lookup, preserving genuinely external
lookup without evaluating caller code during analysis. Existing controls
covered omitted environments and actual external environments, leaving the
explicit default/current-mask forms between them.

### F6: A non-specification external binding was read twice

An active binding `cols` alternated between character selections `"a"` and
`"b"`. Direct tidyselect and `.by = cols` each read it once and selected `a`.
All five Grouping specification constructors read it twice and selected `b`.
The process-local stage probe measured a preflight binding read, one typed
snapshot, and a second binding read during the canonical selection pass.
`all_of(cols)` was a successful one-read control.

`grouping_arg_spec()` evaluated the external name to ask whether it held a
specification. When it held a character vector, preflight retained the original
quosure rather than that already-read selection value. `is_name_only_expr()`
correctly deferred an externally bound name, so there was no name-only
compilation pass; the sole typed pass evaluated the binding again.

This is the narrow gap in ADR 0008's evaluation table: a column selection costs
one evaluation per compilation pass, with two explicitly accepted only when a
names-only pass also runs. The amendment primarily addressed recognized nested
specifications, so its scope must be reconciled explicitly when repairing this
unrecorded non-specification case. This finding does not demand one evaluation
for every name-only selection. Bare external vectors are deprecated by
tidyselect, although accepted in the measured version; `all_of()` is the
public workaround. Its lower priority reflects that limited exposure. Existing
parentheses tests compared relative counts, while absolute one-read assertions
covered bindings returning specifications, not ordinary selection vectors.

## Search breadth and successful controls

- An independent seeded grammar oracle checked 1,500 legal trees over the five
  constructors, depth at most three, six column names, nested unions/products,
  overlapping dimensions, composites, empty sets, identity products, and
  duplicate retention. Exact grouping-set order, dimension order, Grouping bits,
  and Grouping identifiers all matched.
- Ordinary-summary differential exploration covered 39 expressions on its
  regular fixture and 52 expressions on each of four boundary fixtures, 247
  comparisons in total. Scalars, lists, packed and expanded frames, follow-up
  masks, `NULL`, empty data, NA/NaN/Inf, classed values, matrix summaries,
  dynamic `across()` functions, and `pick()` produced no additional confirmed
  defect after accounting for unspecified row order.
- A finite-share oracle checked 384 configurations: 12 datasets, fixed-key
  presence/absence, four plans, and local/dtplyr/live SQLite/live DuckDB.
  Sum/count/mean numerators and Parent/Total denominators were independently
  recomputed from facts. Negative values, cancellation, missing measures, zero
  denominators, empty input, composite parents, missing fixed keys, cube, and
  duplicate Grand total sets matched after explicitly normalizing SQL's empty
  `SUM` with `coalesce()`.
- Two additional seeded backend matrices covered 105 fixtures across local,
  dtplyr, Arrow, SQLite, and DuckDB, varying zero-to-ten input rows, grouping
  families, duplicate sets, typed-missing/display labels, and Margin order.
  Their remaining discrepancies were delegated SQL empty-aggregate behavior
  and backend character representations of logical values.
- A 28-name sweep exercised pronouns, option names, Boolean words, data.table
  special names, quotes, whitespace, Unicode, rowid, and private-alias-like
  names. The `..1` case was excluded because rlang capture/dplyr themselves
  reject that input spelling; it did not establish a Margin regression.

The focused existing suite completed with exit 0 for seven test files selected
by this filter:

```r
testthat::test_local(
  filter = paste0(
    "grouping-interface|grouping-plan|grouping-backends|",
    "summarize-operation|share$|share-backends|metamorphic-margin-semantics"
  ),
  reporter = "summary"
)
```

A separate disposable-copy run of `testthat::test_local(filter = "share")`
completed all 11 selected files with exit 0 and no skipped tests. Initial
read-only worktree attempts stopped when testthat's snapshot writer lacked
filesystem permission; those attempts were not treated as test results. The
completed seven-file run had worktree write permission. No tracked snapshot
changed. These successful suites demonstrate that the six probes exposed
coverage gaps; they do not make the six failures passing tests.

## Exclusions and limits

- SQL `SUM` over no nonmissing rows returned missing where local R returned
  zero. Logical-to-character conversions also differed by backend. Neither
  difference established a package-contract violation.
- Native SQL rejected a `GROUPING()` call nested inside an aggregate. That was
  recorded as a backend expression limitation, not a new Margin bug.
- Condition restatement changed a caller warning containing an internal key
  token; ADR 0021 explicitly accepts that collision, so it was excluded.
- Forged specification objects with non-quosure arguments fell outside the
  documented interface under ADR 0015. No ticket was created for those errors.
- Row-order-only differences under `.sort = "none"`, inherently exponential
  cube expansion, and an active binding generating infinitely many fresh
  specifications did not establish additional defects.
- Arrow shares were intentionally refused. No live PostgreSQL server or
  unlisted SQL engine was exercised; simulated PostgreSQL established only
  construction-time behavior. The random oracles were finite and did not prove
  arbitrary-depth, arbitrary-type, or arbitrary-backend correctness. Infinite
  share sources were outside the finite cross-backend guarantee used here.
- Existing open issues were searched; the related closed issues named above
  were read before ticketing. No existing open ticket covered these six paths.
  Existing parent issues were left unchanged.

This was investigation-only repository work under the classification in
`design/agents/local-checks.md`. The package-affecting Review-ready check was
not required. Publication validation and the review decision are recorded in
the pull request. CI monitoring was not part of the request.
