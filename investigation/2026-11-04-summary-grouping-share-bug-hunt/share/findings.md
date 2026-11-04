# Contextual-share ratio lookup and formal-default scope investigation

Investigated: 2026-11-04
Repository snapshot: `78ebbc36200bdc5cef70838dd4e772c5c4a1401c`

This lane investigated local/dtplyr contextual shares after the November 3
share findings and the subsequent repairs of #1185, #1186 and #1203. It changed
only standalone investigation material. Every script loaded the recorded
checkout with `pkgload::load_all()` and set `TESTTHAT_PARALLEL=false`. One
reproduction execution ran at a time; every yielded handle was retained and
collected before another reproduction began. No full suite or package build
was used as a discovery loop. `environment.txt` recorded R 4.6.1 on aarch64
macOS, dplyr 1.2.1, dtplyr 1.3.3, data.table 1.18.6.1 and rlang 1.3.0.

## S1: generated dtplyr ratio helpers used caller root bindings

ADRs 0010 and 0017 defined numeric ratios with missing and zero handling;
ADR 0014 selected the dtplyr adapter without changing those values. An
unrelated root binding used by an implementation expression changed that
calculation after the query had already been built:

```r
root <- new.env(parent = globalenv())
root$source <- data.frame(g = c("a", "b"), x = c(1, 3))
input <- evalq(dtplyr::lazy_dt(source), root)
query <- summarize_with_margins(
  input, value = sum(x), fraction = share_of_total(value),
  .grouping = rollup(g)
)
root$is.na <- function(x) rep(TRUE, length(x))
dplyr::collect(query)
```

Expected: displayed values one, three, four and shares .25, .75, one.
Observed: the same displayed values and shares NA, NA, one. The original input
and the no-share Margin query remained correct. Removing that binding restored
the correct ratios. A raising callback instead broke collection.

`dtplyr-ratio-reproduction.R` asserted this exact observed behavior and those
controls and exited zero. Its `--assert` mode required the desired ratio
contract and exited one. Both modes were executed on the recorded snapshot.

The ranked alternatives were: (1) ratio-expression lookup, (2) denominator
selection lookup, and (3) an upstream failure affecting every dtplyr operation.
The original-input and no-share controls refuted (3). The displayed summaries,
root-binding removal, complete generated ratio expression and callback
variants isolated (1): `apply_share_ratios()` emitted unqualified `is.na`,
`as.double`, multiplication, division, equality and Boolean-or calls. dtplyr
resolved them in the root environment. Denominator matching used the package
private function and the repaired membership control remained isolated.

`dtplyr-ratio-matrix.R` completed 336 cases. Six affected helpers crossed both
share kinds, preexisting/late bindings, counting/raising/corrupting callbacks,
fixed/no-fixed keys and direct/across ordinary sources. Those 288 cases
established 96 callback invocations, 96 callback errors and 96 wrong ratios.
The 48 `%in%` controls produced correct values without invoking their callback.
Every case also passed original-input, no-share query and serialized-source
byte controls. The fixed-key fixture included a missing partition, and the
rollup had two variable dimensions.

`dtplyr-ratio-controls.R` independently established zero callback invocations
and zero source-row visits during construction of a filtered input. Explicit
collection invoked the callback twice and visited four source rows across the
branches. Source bytes remained unchanged. A caller-written `is.na()` in an
ordinary no-share summary retained ordinary root lookup, which a repair must
preserve rather than globally replacing the caller environment.

The recommended independent P2 ticket covers generated ratio helpers only.
Acceptance should retain both share kinds, scalar/double/missing/zero rules,
fixed and missing partitions, source immutability and lazy construction under
preexisting and late counting, raising and corrupting callbacks. Ordinary
caller expressions must retain their own lookup. Native SQL needs its
translatable spellings, so a blanket qualification across backends is not an
established repair.

## S2: a formal default removed a binding that still hid later alias reads

ADRs 0010 and 0017 required self-contained share sources and refused an
ordinary summary reading an earlier share in the same call. The conservative
analysis policy in #130 and the nested-removal repair in #1042 required a
removed lexical binding to stop hiding such a read. A removal in a visibly
written formal default did not propagate into the body:

```r
input <- data.frame(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
summarize_with_margins(
  input, value = sum(x), prior = share_of_total(value),
  derived = (function(prior = {base::rm(prior); 1}) {
    prior
    sum(prior)
  })(),
  .grouping = rollup(g)
)
```

Expected: a Package condition refusing the same-call share dependency.
Observed: successful output with displayed prior .25, .75, one and derived
100, 300, 400 from the same-named input payload. The first body read forced the
default; the default removed its own formal binding; the second body read then
resolved in the enclosing summary mask. Moving only the removal into the body
produced the intended Package refusal.

The reverse direction also bypassed source eligibility: first defining
ordinary prior as sum(x), then using the same function as ordinary value, and
requesting a share of value succeeded with value one, three, four. Here the
removed formal exposed the earlier ordinary alias, making that source
non-self-contained; it was not a read of the original 100/300 payload.

`formal-default-reproduction.R` retained both directions and both helpers,
including the body-removal controls. Report mode exited zero; `--assert`
required all four Package refusals and exited one. `formal-default-matrix.R`
completed 192 missing refusals: four default forms (base rm, string remove,
list-based rm, and a literal-true conditional), bare/force demand, both
helpers, both directions, three eager input classes and fixed/no-fixed keys.
All 192 body-removal controls refused, all retained-default and no-share
ordinary controls succeeded, and every serialized caller input was unchanged.
The three eager classes were data.frame, tibble and data.table.

The ranked alternatives were: (1) default-to-body scope effects were lost,
(2) base removal was not recognized, and (3) the result was an ordinary dplyr
summary evaluation error. The directly relocated body-removal control refuted
(2); successful no-share evaluation and independent base-R semantics refuted
(3). `definition_data_symbols()` analyzed defaults and body with the same
original formal-bound set. A default's nested removal could alter its own
local walk, but the body was analyzed afresh with that formal still bound.
No opaque callback inference or planning-time execution was needed to identify
this directly written base mutation.

The recommended second P2 ticket covers conservative propagation of visible
formal-default removals into later reads, in both dependency directions. It
should retain genuinely surviving formals, ordinary no-share evaluation,
caller-shadowed removal functions and the rule against executing default
expressions during analysis. It does not require precise prediction of which
promise is demanded at runtime; undecidable cases remain conservative.

## Duplicate comparison

All-state issue and PR searches were run on the investigation date, with a
100-result limit. The retained JSONs recorded their results. Queries were:

- `repo:sayuks/marginplyr dtplyr share helper`
- `repo:sayuks/marginplyr "as.double"`
- `repo:sayuks/marginplyr "is.na" "shares"`
- `repo:sayuks/marginplyr share formal default`
- `repo:sayuks/marginplyr "removal" "share"`
- `repo:sayuks/marginplyr "formal" "rm"`
- `repo:sayuks/marginplyr "removed" "lexical"`
- `repo:sayuks/marginplyr "default" "removal"`

Full bodies/comments were read for #1203, #491, #130, #1042 and #1091, and for
merged PRs #1208 and #1064. The closest matches were:

- [#1203](https://github.com/sayuks/marginplyr/issues/1203) and
  [#1208](https://github.com/sayuks/marginplyr/pull/1208) isolated generated
  membership. Its Agent Brief explicitly excluded other helper lookup, and
  the repaired membership passed S1's controls. S1 concerned ratio arithmetic,
  missingness and conversion instead. [#491](https://github.com/sayuks/marginplyr/issues/491)
  concerned private internal function heads, rather than ordinary base names
  accidentally read by generated ratio expressions.
- [#1042](https://github.com/sayuks/marginplyr/issues/1042) and
  [#1064](https://github.com/sayuks/marginplyr/pull/1064) propagated nested
  removals within an evaluated body, including removal of a formal in that
  body. S2's body-removal control demonstrated that repair. The remaining edge
  was from a demanded formal default into a subsequent body read.
  [#1091](https://github.com/sayuks/marginplyr/issues/1091) covered indirect
  mutation callees, while S2 used a direct base-qualified removal.

No open report or matching merged repair covered either residual boundary.
Both remained reproducible on the recorded main SHA. Neither ticket blocked
the other. This lane published no issue or comment; the coordinator owned
publication and the independent replay.

## Other probes, exclusions and replay

`helper-discovery.R` recorded 56 discovery cases over 14 root spellings, both
share kinds and preexisting/late bindings. The six repaired/irrelevant
spellings kept correct results. `[` and `list` callbacks also failed, but those
were ordinary generated data.table operations and were not claimed as a new
share-specific defect. The six ratio helpers alone formed S1.

Several matrix drafts failed because their harness used an invalid `.sort`
value, a missing local across expression, or rlang injection inside a formal
pairlist. Those setup results were excluded from every count. The final
formal matrix constructed its function/default AST explicitly; its retained
log was the completed corrected run. No package implementation was edited.

Run the following from the recorded checkout root, one at a time:

```sh
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/share/dtplyr-ratio-reproduction.R
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/share/dtplyr-ratio-reproduction.R --assert
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/share/dtplyr-ratio-matrix.R
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/share/dtplyr-ratio-controls.R
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/share/formal-default-reproduction.R
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/share/formal-default-reproduction.R --assert
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/share/formal-default-matrix.R
```

All report, matrix and control commands completed with exit zero. The two
`--assert` commands completed with exit one against the missing desired
behavior. No optional guards were skipped. These finite probes established no
arbitrary callback grammar, other dependency version, live SQL/Arrow,
concurrency or interruption guarantee. The coordinator's SQL lane supplied
separate evidence; none is attributed to these scripts.
