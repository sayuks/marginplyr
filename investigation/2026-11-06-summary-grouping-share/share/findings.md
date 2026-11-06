# Contextual-share dependency investigation

Investigated: 2026-11-06
Repository snapshot: `952a0d9b18fd3ff4631f153432f6024ad62fcac2`

The lane ran against an independent source archive in
`/private/tmp/marginplyr-share-hunt-20261106/source`. No package implementation
or package tests were changed. The November 5
[share investigation](../../2026-11-05-summary-grouping-share/share/findings.md)
and the November 4 investigations supplied repaired controls.

ADRs 0010 and 0017 require an eligible share source to be self-contained,
without dependencies on earlier ordinary aliases, and prohibit a later
ordinary summary from using an earlier contextual share. The
[decision in #130](https://github.com/sayuks/marginplyr/issues/130) resolves
undecidable dependencies toward over-reporting or refusal, rather than silent
acceptance. [#173](https://github.com/sayuks/marginplyr/issues/173) applies that
decision to unresolved reflective data-mask lookup and preserves lookups
confined to an independent environment.

## S1. Copying a local closure lost its invocation-time dependency

The minimal later ordinary summary was:

```r
(function(prior) {
  read <- function() sum(prior)
  copied <- read
  rm(prior)
  copied()
})(1)
```

With groups a/b, x = 1/3 and an input prior column of 100/300, a preceding
source summary of sum(x) and a share named prior, the operation returned
observed = 100/300/400 beside displayed prior = .25/.75/1. The operation
should have refused the forbidden dependency. The reverse direction defined
prior = sum(x), used this expression as observed, and requested a share of
observed. It returned observed = 1/3/4 rather than refusing the earlier-alias
dependency.

Calling read directly after removing prior correctly refused. Moving only
the copying assignment after removal also correctly refused. Retaining the
formal prior returned one in all groups and remained legal. A second bare
binding copy preserved the missing refusal. The same closure carried through
a literal one-element list, or returned by a directly invoked literal
function factory, also lost its dependency after removal. These were grouped
with the copied binding as visible closure-value flow, rather than independent
dependency rules.

The ranked explanations were loss of the copied closure's identity, stale
enclosing-binding state shared with the direct form, or inconsistent runtime
lookup between direct and copied calls. The controls separated the first:
the direct form used the changed binding state and refused, while copying
before the removal changed only the planning result. Reading
scope_bind_local and scope_closure_binding showed that a literal function
definition received a tracked closure, while a bare-name right-hand side did
not carry that closure to the new binding. At the copying statement the
original formal was still bound, so the definition-time walk could not replace
the omitted invocation-time check.

[closure-copies.R](closure-copies.R) completed 60 comparisons: two missing-
refusal witnesses, two refusal controls and one legal control, crossed with
three eager input classes, both helpers and both dependency directions.
It established 24 missing refusals, 24 correct refusals and 12 valid calls.
The observation command exited zero; --assert exited one with
`Error: error is not TRUE` at the first absent refusal. This was a desired-
contract failure signal, not a regression test installed in the package.

[closure-carriers.R](closure-carriers.R) separately exercised the literal-list
and function-factory carriers, preserving the original copy matrix. Its 48
comparisons established 24 missing refusals, 12 correct direct-closure
refusals and 12 valid retained bindings. Those controls overlapped the copy
matrix; the two counts did not describe disjoint API inputs. Its observation
and --assert modes also exited zero and one respectively.

[#1317](https://github.com/sayuks/marginplyr/issues/1317) and merged
[PR #1332](https://github.com/sayuks/marginplyr/pull/1332) repaired a literal
closure invoked under its original local name. That witness refused here.
[#1232](https://github.com/sayuks/marginplyr/issues/1232) concerned callable
and non-callable literal actuals/defaults and function lookup. The new
witness required an ordinary local binding-to-binding copy before removal.
It did not require a new interpretation of literal functions or a different
runtime dependency rule.

## S2. A nonliteral rlang environment key disabled dependency validation

The minimal later ordinary summary was:

```r
sum(rlang::env_get(environment(), paste0("pri", "or"), inherit = TRUE))
```

It returned the same forbidden 100/300/400 forward values and 1/3/4 reverse
values as S1. Replacing only the computed key with the literal "prior"
correctly refused. Equivalent base get with the computed key correctly
refused as well. A caller variable holding the key, a lexical-pronoun read
of that variable, and a key assigned locally in the expression all bypassed
the refusal.

A nonliteral key selected from a caller-owned independent environment
containing prior = 7 remained valid and returned seven throughout. Reading a
bound prior = 1 from the current inline-function environment with
inherit = FALSE also remained valid. The issue was an unresolved lookup
starting on a path that could reach the mask, rather than dynamic keys in
general.

The ranked explanations were an unsupported reflective callee, loss of the
current-frame classification, or the literal-key gate returning no dependency
instead of the unresolved-lookup marker. The literal-key and base-get
controls separated the third. inheriting_env_get_symbols returned
character() when the key was not a literal string, before it classified the
environment; reflective_lookup_symbols used the unresolved marker for the
equivalent unknown base-get key. No caller expression needed evaluation to
apply #130's conservative decision.

[env-get-dynamic.R](env-get-dynamic.R) completed 96 comparisons across the
same helper/direction/input axes: 48 missing refusals, 24 correct refusals
and 24 valid independent calls. Observation mode exited zero; --assert
exited one with `Error: error is not TRUE` at the first absent refusal.

[#1318](https://github.com/sayuks/marginplyr/issues/1318) explicitly bounded
its acceptance to statically recognized mask and field expressions, including
the literal rlang accessor. Its literal witness refused here. The residual
literal matching/accessor forms in
[#1345](https://github.com/sayuks/marginplyr/issues/1345) were a separate
reported slice, and their fixed forms were visible in the tested source.
The new witness retained the established current-environment spelling and
changed the key from literal text to an unresolved expression; #173's
fail-closed rule supplied the expected behavior.

## S3. Literal environment paths to the mask lost field dependencies

The minimal later summary was:

```r
(function() sum(parent.env(parent.env(environment()))$prior))()
```

The extra lexical frame required one additional parent traversal to reach
the same mask. The operation returned observed = 100/300/400 forward, or
1/3/4 reverse, where the documented dependency refusal was required. Replacing
the field access with base get over the same literal environment correctly
refused. The repaired top-level field path also refused.

The missing refusal also occurred inside implicit local(), through
rlang::env_parent(environment()), through the explicit parent-mask target
of rlang::env_get with a literal key, through an as.environment wrapper of
that parent mask, and through a parent.env(parent.frame()) path in a directly
invoked literal function. Each path reached the same original input values.
Corresponding accesses on independent environments holding prior = 7 remained
valid. The repaired explicit-exact bracket and getElement forms from #1345
correctly refused on this snapshot.

The ranked explanations were a wrong runtime frame interpretation, loss of
the literal member name for paths outside one exact environment shape, or
failure of the prior source alias to reach the read at execution. Comparing
the base-get read over the identical target ruled out the first and third:
its dependency refusal was correct. is_static_mask_parent recognized only
the exact top-level parent.env(environment()) path, and
inheriting_env_get_symbols recognized the current evaluation frame without
the explicit parent-mask path. The member-name walk otherwise dropped the
literal field, leaving the actual dependency unreported.

[mask-environment-paths.R](mask-environment-paths.R) completed 168 comparisons:
72 missing refusals across the six paths, 48 correct refusal controls and
48 legal external-environment controls. All paths crossed three eager input
classes, both helpers and both forbidden dependency directions. Observation
mode exited zero; --assert exited one at the missing refusal. This slice
concerned literal environment-path recognition. S2 retained the already
recognized environment and varied only the unresolved key, so its expected
conservative decision and its failing gate were separate.

[#1318](https://github.com/sayuks/marginplyr/issues/1318) and
[#1345](https://github.com/sayuks/marginplyr/issues/1345) described the
repaired exact-path field/index/base-element/rlang forms, not these residual
environment paths. No report in the all-state search described the six new
witnesses. The expected dependency rule was unchanged, and no repair was
attempted here.

## Additional observations and scope limits

A visible identity wrapper around the local closure correctly refused, as
did while/repeat removal, an on.exit removal probe, a forced delayedAssign
removal, and evalq reads. These were individual finite probes. Top-level
current_env/environment field access itself yielded no useful input value
in the probe and was not reported as a defect.

[numeric-keys.R](numeric-keys.R) ran a separate seeded value oracle over six
dimension types: character, numeric with NA/NaN, a factor with an NA level,
Date, POSIXct and integer64 including adjacent values above 2^53. Every pair
of types crossed local/dtplyr and ordinary/repeated-dimension rollups, with
fixed partitions including missing keys, zero/negative/missing source values,
typed-missing labels and both share helpers. All 144 operations passed the
direct-input source oracle and both displayed-denominator ratio oracles for
4,166 output rows. This finite loop found no additional defect and did not
assert an independent row-order contract.

The dependency matrices did not cover dtplyr execution, SQL translation,
arbitrary closure carriers, all reflective APIs, concurrency, other R or
dependency versions, or other operating systems. They did not establish that
arbitrary R expressions could be fully classified statically.

## Duplicate search and replay

All-state GitHub issue searches used `closure alias`, `share closure copy`,
`env_get`, and `mask parent.env`; an all-state PR search used `closure`.
The full exported issue and PR title/body sets were also searched locally
with these expressions:

```text
closure.*cop|cop.*closure|copied.*read|again.*copied
env_get|inheriting_env_get
unresolved.*lookup|reflective.*lookup
env_parent|as.environment
mask.*environ|environ.*mask
nested.*field|field.*nested
```

The relevant matches were #1317, #1232 and PR #1332 for S1; #1318, #173,
#872 and PRs #1332, #888 and #180 for S2. The bodies and comments of #130,
#173, #1317, #1318, #1232, #1345 and PR #1332 were read. The separating
controls above distinguished the repaired witnesses. The search found no
issue or PR describing the new witnesses. The searches and full exported
sets were task-local scratch evidence; the matching report links above were
the retained duplicate record.

From an archive of the tested SHA, with evidence pointing to this directory:

```sh
Rscript "$evidence/closure-copies.R"
Rscript "$evidence/closure-copies.R" --assert
Rscript "$evidence/closure-carriers.R"
Rscript "$evidence/closure-carriers.R" --assert
Rscript "$evidence/env-get-dynamic.R"
Rscript "$evidence/env-get-dynamic.R" --assert
Rscript "$evidence/mask-environment-paths.R"
Rscript "$evidence/mask-environment-paths.R" --assert
Rscript "$evidence/numeric-keys.R"
```

The normal/contract terminal status pairs were 0/1 for each dependency
script. The value oracle exited zero. The scripts set TESTTHAT_PARALLEL=false
and loaded the archived source with pkgload. No
package suite, database server or uncollected child process was involved.

The environment was R 4.6.1 on aarch64-apple-darwin23, pkgload 1.5.3,
dplyr 1.2.1, rlang 1.3.0, tibble 3.3.1, data.table 1.18.6.1, dtplyr 1.3.3
and bit64 4.8.6. Full
observation output was retained alongside each script.
