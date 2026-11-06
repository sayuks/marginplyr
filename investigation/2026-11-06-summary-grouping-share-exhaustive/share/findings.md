# Share dependency value-flow boundaries

Investigated: 2026-11-06
Repository snapshot: `9ffb24614a71512170298a3827bc11c80354e742`

This lane used public `summarize_with_margins()` calls against the fetched
main worktree. No package implementation or package test was edited.
The preceding [share hunt](../../2026-11-06-summary-grouping-share/share/findings.md)
and [deep share hunt](../../2026-11-06-summary-grouping-share-deep/share/findings.md)
supplied repaired controls rather than presumed outstanding failures.

ADRs 0010 and 0017 require a share source to be independent of earlier ordinary
summary aliases and forbid a later ordinary summary from reading an earlier
contextual share. The [#130 decision](https://github.com/sayuks/marginplyr/issues/130)
and [reflective application in #173](https://github.com/sayuks/marginplyr/issues/173)
require a conservative refusal for unresolved mask reads without executing
caller code during planning.

The fixture had groups a/b, x = 1/3 and an input prior payload = 100/300.
The forward direction defined source = sum(x), prior = its contextual share,
and then observed = the candidate expression. Each confirmed witness returned
observed = 100/300/400 beside prior = .25/.75/1. It should have raised the
existing forbidden-share dependency Package condition. The reverse direction
defined prior = sum(x), source = the candidate, and ratio = the share of source.
That should have raised the existing earlier-alias dependency Package condition.
Eager witnesses returned source = 1/3/4; the closure witnesses on dtplyr returned
source = 100/300/400, because dtplyr did not reuse the preceding alias inside
the same summary. Both were missing the required planning refusal.

## S1. Syntactic closure-valued expressions lost invocation-time provenance

The minimal expression was:

```r
(function(prior) {
  copied <- { function() sum(prior) }
  rm(prior)
  copied()
})(1)
```

Removing only the braces made the same closure refuse correctly. A literal
if branch and switch arm reproduced the missing refusal. The same result
occurred when a literal factory used explicit return, when its body contained
a preceding independent statement, or when a visible factory was invoked
through its local name. The directly invoked, single-expression literal
factory repaired under [#1361](https://github.com/sayuks/marginplyr/issues/1361)
remained an effective refusal control.

The ranked explanations were loss of the closure value at the RHS boundary,
a general failure to propagate removal, or a runtime lookup changed by the
wrapper. Bare closure, bare-copy and identity controls refused after exactly
the same removal; moving removal before the braced definition also refused.
Retaining the local prior gave one for every result row. These separated
value provenance from removal and runtime environment identity.

`scope_value_closure()` recognized literal definitions, local names, a narrow
literal-factory shape and the existing carriers, but not a block or selected
branch as a closure value. Its factory special case accepted a directly
returned literal definition, not explicit return or a multi-statement body.
The ordinary dependency walk visited the definition while prior was still
locally bound; invocation through the untracked result consequently failed
to revisit that definition after removal.

This slice concerned visible syntax/value flow, rather than a promise to
classify arbitrary caller functions. It could be repaired and verified
independently of the other three slices.

## S2. Existing base closure-returning operations lost the transferred value

The minimal force witness was:

```r
(function(prior) {
  read <- function() sum(prior)
  copied <- force(read)
  rm(prior)
  copied()
})(1)
```

Literal get("read") and match.fun(read) transfers reproduced the same missing
refusal. Direct invocation, a bare-name copy, and identity(read) all refused;
retaining prior made every transfer legal and returned one.

The competing explanations were an unsupported returned value, an unintended
invocation during transfer, or stale removal tracking. The base operations
returned the existing function without invoking its body, and repaired
identity/direct controls used the same closure and removal order. This
isolated loss of the returned closure's provenance.

`scope_value_closure()` recognized the identity/invisible return operations
fixed by [#1389](https://github.com/sayuks/marginplyr/issues/1389), but not the
three demonstrated base operations. The reflective get reader classified its
lookup as a local binding read; that classification did not carry the
binding's closure value to copied. Existing tests that forced a closure and
then called its original name did not cover assigning force's return value.
This follow-up left the #1389 witnesses fixed and did not reopen that report.

## S3. Literal list element updates lost closure dependencies

The minimal append witness was:

```r
(function(prior) {
  carrier <- list()
  carrier[[1L]] <- function() sum(prior)
  rm(prior)
  carrier[[1L]]()
})(1)
```

Replacing a pre-existing independent closure in a one-element literal list,
or assigning the literal closure through a dollar field, reproduced the
missing refusal. Constructing the same dependent list element inside list()
correctly refused. Retaining prior made the appended/replaced closure legal
and returned one.

The ranked explanations were failure to update tracked list values, loss of
all indexed closure calls, or a different runtime closure enclosure. The
literal-list control refuted the second; its identical function definition
and removal order refuted the third. `statement_reads_and_scope()` read the
replacement target and RHS but did not update the carrier's tracked closure
elements. `scope_closure_list()` therefore had no dependent element for an
append, or retained the old independent element for replacement.

[#1361](https://github.com/sayuks/marginplyr/issues/1361) covered a list whose
closure elements were installed at construction. This slice concerned later
literal element writes; the repaired construction control remained refused.
It did not depend on the unknown-return or syntax-value repairs.

## S4. Local aliases of known mask environments bypassed repaired accessors

The minimal expression was:

```r
{ mask <- parent.env(environment()); sum(mask$prior) }
```

Replacing only mask by its literal RHS correctly refused. Saving that same
environment and using a literal double-bracket index, rlang scalar accessor,
rlang bulk accessor, or base as.list conversion also bypassed the dependency
rule. Saving environment() itself and performing an inheriting scalar
accessor lookup reproduced the failure.

The ranked explanations were loss of the saved environment identity, loss of
the field name generally, or an accessor running in a different environment.
Direct field/scalar/bulk/materialization controls refused; base get on the
same saved mask refused. They separated identity loss from the latter two
explanations. Aliases of an independent external environment holding prior = 7
remained legal across every accessor family; an inheriting alias of a frame
holding an independent local prior = 7 also remained legal.

`static_environment_frame()` classified literal current/parent-frame paths
but not a local name assigned one of those paths. The repaired member,
scalar/bulk accessor and materialization readers therefore lacked the mask
identity once the target became that name. The base reflective lookup's
unresolved-target handling still detected a possible dependency.

[#1318](https://github.com/sayuks/marginplyr/issues/1318) explicitly bounded
its repair to statically recognized mask/field expressions;
[#1345](https://github.com/sayuks/marginplyr/issues/1345) covered equivalent
literal accessors; [#1368](https://github.com/sayuks/marginplyr/issues/1368)
covered literal environment paths; and
[#1388](https://github.com/sayuks/marginplyr/issues/1388) covered direct bulk
reads/materialization. Those witnesses refused here. The new boundary was a
visible assignment of the environment value followed by the same recognized
read, not another accessor spelling or nonliteral key.

## Completed matrices and replay

[closure-values.R](closure-values.R) crossed twelve witnesses, six repaired
refusal controls and twelve retained-binding controls with both helpers, both
dependency directions, and data.frame/tibble/data.table/dtplyr inputs. Its
observation run completed 480 comparisons: 192 missing refusals, 96 correct
refusals and 192 legal reads. The syntax, transfer and mutation families had
96, 48 and 48 missing-refusal comparisons respectively. Serializing each input
before and after its matrix found no mutation. The dtplyr results were
explicitly collected by the caller.

[environment-aliases.R](environment-aliases.R) crossed six witnesses, five
repaired refusal controls and six independent-read controls with both helpers,
both directions and the three eager input classes. It completed 204
comparisons: 72 missing refusals, 60 correct refusals and 72 legal reads.
Input serialization again stayed unchanged.

Run these scripts from the tested source root, with the script path absolute
when the source is an archive elsewhere. For closure-values, the first
argument may be syntax, transfer, mutation or all. Observation mode asserts
the dated incorrect payload. Adding --assert instead requires the existing
targeted dependency refusal and can succeed after a repair without requiring
that payload. Adding --controls-only omits witnesses and exercises the same
assertion branches using repaired refusals and legal reads.

```sh
Rscript "$evidence/closure-values.R"
Rscript "$evidence/closure-values.R" syntax --assert
Rscript "$evidence/closure-values.R" transfer --assert
Rscript "$evidence/closure-values.R" mutation --assert
Rscript "$evidence/closure-values.R" all --assert --controls-only
Rscript "$evidence/environment-aliases.R"
Rscript "$evidence/environment-aliases.R" --assert
Rscript "$evidence/environment-aliases.R" --assert --controls-only
```

| Command mode | Terminal status | Completed comparisons |
| --- | --- | --- |
| Closure observation all | 0 | 480 |
| Closure syntax / transfer / mutation --assert | 1 / 1 / 1, each at the absent refusal | One witness per failing mode |
| Closure all --assert --controls-only | 0 | 288 |
| Environment observation | 0 | 204 |
| Environment --assert | 1, at the absent refusal | One witness |
| Environment --assert --controls-only | 0 | 132 |

The retained terminal handles were 50462 for final closure observation,
90815 for the 288-comparison closure controls-only run, 13223 for the final
eager environment observation, and 7891 for the sequential four desired-contract
runs plus environment controls. All owned work was collected before reporting
the lane complete. The log files and CSVs beside the harnesses retain the
results; the coordinator's independent replay is separate evidence.

One initial closure observation used the eager sequential-alias payload for
dtplyr's reverse direction and failed an oracle assertion. A direct dtplyr
reproduction established its input-column payload; only that dated observation
expectation was corrected. A preliminary environment-alias probe also included
dtplyr, where its temporary-variable translation produced backend diagnostics
for five forms and zero for the index form, rather than the eager payload.
The final environment matrix was explicitly restricted to eager inputs;
those preliminary dtplyr observations supplied no missing-refusal value claim
for that matrix and no additional ticket.

## Duplicate checks, environment and finite boundary

All-state exports held 702 issue bodies and 691 PR bodies. Searches combined
closure/carrier/factory/copied with block, condition, switch, force, match.fun,
replacement and append; environment searches used environment alias, mask
alias, aliased environment, env_get with alias, and as.list with alias. Live
all-state Issue/PR searches for environment alias returned none. Earlier
closure-conditional and closure-list-assignment issue searches also returned
none; a live closure PR search found the preceding repair/research PRs.
An initial combined repo query was rejected by gh; explicit --repo searches
provided the successful results.

The full bodies/comments of #1361, #1389, #1318, #1345, #1368, #1388 and #173
were read, alongside exported #130/#1317/#1232 bodies. The preceding sections
name their scope distinctions and repaired controls. No report describing
these demonstrated new value-flow witnesses was found. This was a duplicate
check at the dated retrieval, not proof against reports created later.

The environment was R 4.6.1 on aarch64-apple-darwin23; pkgload 1.5.3,
dplyr 1.2.1, rlang 1.3.0, tibble 3.3.1, data.table 1.18.6.1 and dtplyr 1.3.3.
The scripts loaded the tested source with pkgload and disabled testthat
parallel execution. No package suite or database server was started.

The matrices established these finite public-interface witnesses. They did
not establish complete static classification of arbitrary R programs,
unknown functions or environment/closure carriers, SQL/Arrow behavior, all
extension classes, other dependency versions, other operating systems,
concurrency or interruption. The four recommended tickets were independently
verifiable repair slices without blocking edges to one another. No
implementation repair was attempted.
