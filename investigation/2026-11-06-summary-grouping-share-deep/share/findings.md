# Contextual-share reflective mask reads

Investigated: 2026-11-06
Repository snapshot: `547d77095c2923573216d002f50bef21778247f9`

The lane ran against a separate git archive at
`/private/tmp/share-deep-source`. No package implementation or package tests
were changed. ADRs 0010 and 0017 require a share source to be independent of
earlier ordinary aliases and forbid later ordinary summaries from reading an
earlier contextual share. The conservative unresolved-lookup decision in
[#130](https://github.com/sayuks/marginplyr/issues/130) and its reflective
application in [#173](https://github.com/sayuks/marginplyr/issues/173) supplied
the expected refusal when a read could reach the active mask.

The [retained harness](reflective-mask-reads.R) used groups a/b, measure x =
1/3 and an input prior payload of 100/300. In its forward direction, source
was sum(x), prior was its contextual share, and observed was the expression
under investigation. In its reverse direction, prior was sum(x), observed
was the candidate share source, and ratio requested its share. All three
boundaries below returned observed = 100/300/400 beside displayed prior =
.25/.75/1 in the forward direction; the reverse direction returned observed
= 1/3/4. Both directions required the existing Package condition instead.

## S1. Unknown mask-member targets were treated as no dependency

The minimal expression was:

```r
sum(parent.env(environment())[[paste0("pri", "or")]])
```

A literal "prior" index on the same environment correctly refused. The
equivalent computed-name base get and scalar rlang env_get calls also
correctly refused. A lexical key, a key assigned within the summary, an
explicit exact argument, getElement, and .subset2 reproduced the missing
refusal. Reading the dynamic key from an independent external environment
remained valid and returned seven in every grouping set.

The ranked hypotheses were (1) a recognized mask-member branch accepted only
literal targets and dropped an unresolved target, (2) the environment path
reached a different runtime frame, or (3) computed keys generally defeated
the dependency walker. The literal-index control separated (2), and the
computed-name get/env_get controls separated (3). Reading scoped_data_symbols
showed that the mask-member branch returned a name for a literal string but
fell through for other indices. The ordinary recursive walk then collected
the index expression's own reads without representing the binding it could
resolve. No execution of the caller's key expression was needed to preserve
an unresolved dependency.

The index mode completed 120 comparisons: 72 missing refusals, 36 correct
refusals and 12 valid independent reads. Both share helpers, both dependency
directions and three eager input classes were exercised. Its observation mode
exited zero; index --assert exited one with `Error: refusal is not TRUE`.

[#1318](https://github.com/sayuks/marginplyr/issues/1318) explicitly bounded its
acceptance to literal fields. The literal exact/accessor repairs in
[#1345](https://github.com/sayuks/marginplyr/issues/1345) and environment-path
repairs in [#1368](https://github.com/sayuks/marginplyr/issues/1368) remained
effective as controls. [#1021](https://github.com/sayuks/marginplyr/issues/1021)
concerned computed pronoun indices; this witness contained neither pronoun.
[#1362](https://github.com/sayuks/marginplyr/issues/1362) concerned a scalar
rlang lookup's dynamic name; that repaired computed-name call refused here.

## S2. Unknown accessor search options incorrectly established independence

The minimal expression was:

```r
sum(rlang::env_get(environment(), "prior", inherit = isTRUE(TRUE)))
```

Changing only inherit to literal TRUE correctly refused. The same failure
arose with literal inherit = TRUE and last = .GlobalEnv, or last wrapped in
identity(globalenv()). The equivalent literal last = globalenv() control
correctly refused, as did the base get call with a computed inheritance
argument. External-environment lookup with the same computed inherit
argument remained valid. A non-inheriting lookup of a proven local formal
returned one, and a lookup explicitly stopped at its current environment
returned its default seven; both were legal controls.

The ranked hypotheses were (1) unknown search options were treated as proof
that the mask was unreachable, (2) the current-environment target was not
recognized, or (3) the mask binding was absent when the lookup executed. The
literal-inherit/current-environment control separated (2); the successful
forward and reverse values and base-get comparison separated (3).
In inheriting_env_get_symbols the nonliteral inherit and an unrecognized last
path both returned character(), despite neither proving that lookup stopped
before the mask. The known literal calls reached scoped dependency analysis.

The options mode completed 108 comparisons: 36 missing refusals, 36 correct
refusals and 36 legal controls. Observation mode exited zero; options --assert
exited one at the absent refusal. [#1362](https://github.com/sayuks/marginplyr/issues/1362)
varied the binding name while retaining the literal inheritance option. This
boundary retained a literal binding name and varied whether the search path
could be proven. [#1345](https://github.com/sayuks/marginplyr/issues/1345)
covered statically readable stop-frame calls, which refused in these controls.

## S3. Bulk environment reads lost their mask dependencies

The minimal expression was:

```r
sum(rlang::env_get_list(environment(), "prior", inherit = TRUE)$prior)
```

Reading the same literal key with scalar env_get or base mget correctly
refused. Requesting two keys and selecting prior, explicitly targeting the
parent mask, and materializing that mask with base as.list before dollar or
literal-index extraction all bypassed both dependency rules. Equivalent
external-environment bulk/list reads returned seven and remained valid. Bulk
reads of an established local formal returned one and remained valid.

The ranked hypotheses were (1) the bulk-read boundary was absent from
reflective dependency classification, (2) a list's field name was globally
discarded even when its value expression carried the dependency, or (3) bulk
lookup searched a different environment than scalar lookup. The mget list
control separated (2), and identical env_get/env_get_list targets and runtime
values separated (3). scoped_data_symbols had a scalar env_get branch and
base reflective-lookup recognition, but no bulk/list materialization branch.
The ordinary dollar/index walk consequently found no mask binding read.
These APIs remained ordinary caller functions; the needed decision was
whether their recognized mask-reading forms could conceal a dependency,
without invoking caller code while planning.

The bulk mode completed 132 comparisons: 60 missing refusals, 24 correct
refusals and 48 legal independent reads. Observation mode exited zero;
bulk --assert exited one at the absent refusal. #173 and
[#872](https://github.com/sayuks/marginplyr/issues/872) supplied the
reflective-read contract and covered the base mget control; #1318/#1345
covered individual literal members and scalar accessors. None of the reports
found described bulk rlang environment reads or as.list mask materialization.

## S4. Known base value-return and assignment calls lost closure provenance

The separate [closure-transfer harness](closure-transfer-residual.R) found a
follow-up to [#1361](https://github.com/sayuks/marginplyr/issues/1361)'s
visible closure-transfer boundary. An inline function bound prior = 1,
defined read as a closure summing prior, transferred read through
identity(read), invisible(read), or literal assign("copied", read), removed
prior, and invoked copied. All three transfers lost the invocation-time
dependency and returned the same forbidden forward/reverse values described
above. Direct invocation, a bare-name copy made before removal, assign
performed only after removal, and the earlier literal-list and literal-factory
carriers correctly refused. The invisible/assignment transfers with the local
binding retained returned one and remained valid.

The ranked hypotheses were loss of the closure value across an ordinary
value-returning call or reflective assignment, failure to carry removal
effects through the enclosing scope, or a runtime invocation during transfer.
The repaired direct and bare-copy controls distinguished the first from the
second; identity/invisible returned the existing closure without invoking its
body, excluding the third. scope_value_closure recognized the repaired
literal/symbol/list/factory carriers, while ordinary identity/invisible calls
and the reflective assignment did not establish the transferred closure.

This retained the existing dependency policy. The repaired enumerated
one/multi-step assignments, literal list and literal factory in #1361 were
not failing witnesses here; the residual concerned closure values returned by
known ordinary calls or installed by a known reflective assignment. One
follow-up slice could cover these three transfers with the repaired carriers
as controls, without reopening the closed report or splitting each ordinary
function spelling into a ticket. The earlier note's identity-wrapper
observation concerned a
different ordering; copying through identity before removal was not a valid
refusal control in this follow-up. An initial mistaken control assertion
failed and was corrected before the retained archived validation.

The final archived observation covered 120 comparisons: 36 missing refusals,
60 correct refusals and 24 legal reads across both helpers/directions and
three eager input classes. Retained session 20130 collected observation,
desired-contract and controls-only outcomes of zero, one and zero; the green
control mode covered 84 comparisons. This was
follow-up evidence, not a package regression test or an implementation repair.

## Replay, controls and finite boundary

From an archive of the named SHA, run Rscript with the absolute path of
reflective-mask-reads.R. With no arguments it runs all three modes. A mode
argument selects index, options or bulk; adding --assert requires the desired
refusal and intentionally fails on this dated source. The complete observation
run returned exit zero through retained terminal session 59212, with 360
comparisons: 168 missing refusals, 96 correct refusals and 96 legal reads.
All three separately selected contract runs exited one. Each input's
serialized data remained unchanged after its matrix.

Adding --controls-only selects just the existing refusal and independent-read
controls and does not execute or display a witness. All three --assert
--controls-only runs exited zero, through retained session 17768: 48 index,
72 options and 72 bulk comparisons. These green paths checked the harness's
contract branches and established no implementation repair.

The existing dynamic-environment-name tests exercised computed names with
literal inherit/last options; the environment-path and lookup-environment
tests exercised literal mask fields and literal known stop-frame calls.
Searches of share/static-analysis test sources found no env_get_list,
as.list(parent.env(...)), computed inherit option, or .GlobalEnv stop-frame
witness. The retained public-interface probes supplied those missing
distinctions without modifying the package test suite.

The environment was R 4.6.1 on aarch64-apple-darwin23, dplyr 1.2.1,
rlang 1.3.0, pkgload 1.5.3, tibble 3.3.1 and data.table 1.18.6.1.
The eager inputs were data.frame, tibble and data.table. This matrix did not
establish dtplyr/SQL execution, arbitrary caller reflection or closure
transfer, unknown package versions, other platforms, concurrency or
interruption. It checked the existing dependency conditions and output values;
it did not assert an independent Margin-order contract.

Duplicate checks searched 695 exported all-state issue bodies and 684 all-state
PR bodies. Terms included env_get_list, as.list near mask/parent.env,
inherit near isTRUE, last near GlobalEnv, computed/dynamic/nonliteral mask
indices, and reflective lookup. Live all-state Issue/PR searches for
env_get_list returned none; the mask/dynamic search returned #1362/#1368
and unrelated selectors. Full bodies and comments of #130, #173, #872,
#1021, #1318, #1345, #1361, #1362 and #1368 were read. The preceding sections
recorded which repaired witnesses remained refusal controls. No duplicate
report of these demonstrated residual boundaries was found in that retrieval.

The recommended repair slices were unresolved explicit mask-member targets,
unproven scalar accessor search options, bulk environment reads, and the
known-call closure-transfer follow-up. Each could be verified independently
with its own harness mode and had no blocking edge to the other slices.
Their separating controls and failed gates differed.
