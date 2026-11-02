# Contextual-share lookup rebinding investigation

Investigated: 2026-11-02
Repository snapshot: `978d7bef4247bf1348af5b90bfab47924db25500`

This investigation read the always-loaded context, `CONTEXT.md`, ADRs 0010,
0014, 0017, 0019 and 0020, the investigation conventions and the recent
October 29–31 summary/share investigations. It changed no package implementation
or package regression test. The replay scripts loaded the checkout with
`pkgload::load_all()` and explicitly disabled testthat parallel execution.

## S1: list2env rebinding retained an external-lookup proof

The source and later-summary contracts in ADRs 0010/0017 and the share
reference required refusal when a source read an earlier ordinary summary,
or an ordinary summary read an earlier contextual share. A visibly written
`list2env()` call rebound the environment name used by a later reflective
lookup, bypassing both restrictions.

```r
input <- tibble::tibble(g = c("a", "b"), x = c(1, 3), prior = c(100, 300))
e <- new.env(parent = baseenv())
summarize_with_margins(
  input,
  prior = sum(x),
  value = {
    base::list2env(list(e = environment()), envir = environment())
    sum(get("prior", envir = e))
  },
  fraction = share_of_total(value),
  .grouping = rollup(g)
)
```

The result contained `prior = 1, 3, 4`, `value = 1, 3, 4`, and
`fraction = .25, .75, 1`. The source therefore depended on the preceding
ordinary summary. Replacing only the setter with
`base::assign("e", environment(), envir = environment())` produced the intended
Package-condition refusal naming prior. No parent environment was changed by
either call: the local binding e itself was replaced.

The opposite dependency direction exposed a silently wrong public column:

```r
e <- new.env(parent = baseenv())
summarize_with_margins(
  input,
  value = sum(x),
  prior = share_of_total(value),
  derived = {
    base::list2env(list(e = environment()), envir = environment())
    sum(get("prior", envir = e))
  },
  .grouping = rollup(g)
)
```

The displayed prior was `.25, .75, 1`, while derived held the same-named input
column's `100, 300, 400`. The intended result was a refusal, rather than a
derived value evaluated against the internal ordinary-summary stage. The
assign control refused this direction as well.

The ranked hypotheses were (1) an unrecorded visible rebinding of e, (2) a
changed parent chain, and (3) a difference in ordinary summary evaluation.
The setter-only comparison, unchanged external environment identity and parent,
and successful no-share controls isolated the first. The static
`lookup_assignment_names()` reader named `<-`, `=`, `<<-`, `for`, `assign`,
`delayedAssign` and `makeActiveBinding`, but not `list2env`. Consequently
`lookup_rebound_names()` omitted e, and `lookup_environment_value()` reused
its captured external environment value even though the visibly written base
setter replaced that binding before the lookup.

This was a visible base binding mutation with a literal list key, rather than
a request to infer arbitrary opaque callback side effects or evaluate caller
expressions during planning. The existing lookup-environment tests included
`list2env()` as a newly constructed explicit lookup environment, but did not
cover its mutation of the binding supplying a subsequent lookup environment.

`list2env-reproduction.R` minimized both directions and verified assign and
no-share controls. Report mode exited 0; `--assert` required the documented
refusals and exited 1 with two missing refusals. The replay was run twice,
once in each mode, with the same public values.

`list2env-matrix.R` covered three eager input classes (tibble, data.frame and
data.table), Parent/Total helpers, both dependency directions, and rollups
with or without fixed keys. Fixed-key fixtures included a missing partition.
Five setter spellings were tested: direct, base-qualified, literal-name get,
literal-name match.fun, and do.call. All 120 investigated calls were accepted
instead of refused; all 120 paired no-share controls succeeded. A further 96
controls passed: canonical assign refusal, stable external lookup, list2env
writing an unrelated local binding, and a caller-shadowed ordinary list2env
that made no mutation. Each case created a fresh external environment.

The recommended ticket was one P2 delivery covering both dependency directions
for this setter and preserving those independent controls. It needed no
blocking ticket. No implementation was attempted.

## Duplicate comparison

The following all-state GitHub searches were executed on the investigation
date: issue search `list2env` (no matches), PR search `list2env` (no matches),
issue search `rebound environment` (closed #1045), and issue search
`external lookup`. The latter located #1045, #872, #1091, #918, #783, #1042,
#173, #100, #254 and #23. The complete bodies and comments of the three
closest matches, #872, #1045 and #1091, were read.

- [#872](https://github.com/sayuks/marginplyr/issues/872) repaired explicit
  default/current-mask lookup environments and preserved actual external ones.
  Here the expression named a captured external environment, whose binding was
  then visibly replaced.
- [#1045](https://github.com/sayuks/marginplyr/issues/1045) repaired visible
  parent replacement. Here no parent was changed and the missing invalidation
  was a binding replacement.
- [#1091](https://github.com/sayuks/marginplyr/issues/1091), implemented by
  [#1133](https://github.com/sayuks/marginplyr/pull/1133), applied the existing
  callee-resolution boundary to rm/remove and parent setters. Its fixes were
  present in the investigated snapshot. The new failure included the direct
  base-qualified setter and therefore did not depend on that repaired
  indirect-callee boundary.

## Bounded negative results and limits

Exploratory local/dtplyr checks of ordinary single-column across sources using
sum, an external function binding, a formula, an anonymous function, and a
literal function list produced the expected finite Total shares. Dynamic
function factories and a string-valued function specification were refused as
unknown preceding sources; no additional static-output guarantee was established
for those forms, so those refusals were not promoted to findings.

A replay of the October 31 visible evalq-removal pattern produced the intended
source dependency refusal on the investigated snapshot. Assigning e inside an
earlier ordinary expression's own evaluation frame did not contaminate the
later expression: the later lookup retained its external constant. That
control did not establish behavior for mutations of a caller-owned environment.

These checks established no new numeric denominator-selection defect. They
did not constitute a fresh exhaustive numerical oracle, and no such count was
claimed. The retained finding matrix covered eager inputs only; no live SQL,
Arrow, hosted-service, arbitrary callback, concurrency or interruption claim
followed from it. The coordinator investigated backend behavior separately.

## Replay

Run from the checkout root:

```sh
Rscript investigation/2026-11-02-summary-grouping-share-bug-hunt/share/list2env-reproduction.R
Rscript investigation/2026-11-02-summary-grouping-share-bug-hunt/share/list2env-reproduction.R --assert
Rscript investigation/2026-11-02-summary-grouping-share-bug-hunt/share/list2env-matrix.R
```

Adjacent logs recorded the terminal outputs. The second command deliberately
failed on the investigated snapshot; it was an investigation signal rather
than a package regression test. The matrix regenerated its adjacent CSV.
