# Contextual-share carrier and lookup boundaries

Investigated: 2026-11-07
Repository snapshot: `e1dabaabb392a3791177e5c48e983f780c7c93ec`

This lane investigated Parent share and Total share dependency planning through
the public `summarize_with_margins()` interface. No package implementation or
package regression test was edited. The source was loaded through pkgload in
the coordinator's fetched-main investigation worktree. Standalone research
scripts and outputs stayed outside that worktree during investigation.

The previous November 6 deep and exhaustive hunts supplied repaired refusal
controls. ADRs 0010 and 0017 require a share source to be independent of an
earlier ordinary summary alias and forbid a later ordinary summary from using
an earlier share. The decisions in issues #130 and #173 require conservative
dependency handling without executing caller code during planning.

## Shared fixture and finite result

The input had groups a/b, x = 1/3 and a source-data column prior = 100/300.
The forward call defined source = sum(x), prior = its contextual share, and
observed = the candidate expression. Every reported witness succeeded and
returned observed = 100/300/400 beside prior = .25/.75/1, instead of raising the
existing forbidden-share dependency Package condition. The reverse call
defined prior = sum(x), source = the candidate, and ratio = the share of
source. Every reported witness succeeded with source = 1/3/4 instead of
raising the earlier-summary-alias dependency Package condition.

The matrix crossed both helpers, both dependency directions and data.frame,
tibble and eager data.table inputs. It established 168 missing refusals,
84 correct refusals and 168 legal independent reads: 420 comparisons.
Serialization before and after each input's matrix established no mutation of
the caller's data in these comparisons. No dtplyr or SQL value-equivalence
claim is made by this matrix.

## S1. Plain list value transformations discarded closure provenance

The smallest concatenation witness was:

```r
(function(prior) {
  fs <- c(list(function() sum(prior)))
  rm(prior)
  fs[[1L]]()
})(1)
```

Base append of the same one-element list to an empty list and copying that
list with a literal single-bracket subset reproduced the failure. The
literal-list construction control without the transformation correctly
refused. Retaining the local prior kept every transformed carrier legal and
returned one. There were 36 missing refusals and 36 retained-binding controls.

The ranked hypotheses were loss of the list's tracked closure values at a
known transformation, failure to propagate removal generally, or a changed
runtime closure enclosure. The repaired literal-list/direct-closure controls
used the same removal and function body and refused, separating list-value
provenance from the latter explanations. `scope_closure_list()` recognized
literal construction and a local saved list, but none of the three plain base
transformations. No arbitrary caller function was needed to produce the
carrier. This extends the construction-time carrier in closed #1361; its
repaired witness remained refused.

## S2. Structural list writes retained an obsolete element map

The deletion witness was:

```r
(function(prior) {
  fs <- list(function() 7, function() sum(prior))
  fs[[1L]] <- NULL
  rm(prior)
  fs[[1L]]()
})(1)
```

Deleting a named first element with dollar assignment reproduced the same
positional shift. Replacing an independent closure with a dependent closure
through a literal single-bracket list assignment also missed refusal. The
existing double-bracket replacement control correctly refused. Keeping prior
made each write legal and returned one. These three forms supplied 36 missing
refusals and 36 retained-binding controls.

The ranked hypotheses were an element-map update inconsistent with R list
semantics, loss of every list-index invocation, or a failed removal analysis.
Literal-list and double-bracket replacement controls refuted the latter two.
`scope_after_list_write()` treated double-bracket/dollar assignment of NULL as
retaining an empty tracked slot, whereas R removed the slot and shifted the
dependent second element to position one. Its recognized write operators
also excluded single-bracket assignment. Closed #1403 covered element
addition/replacement through double-bracket and dollar writes, whose repaired
controls still refused; it did not cover these structural writes.

## S3. Partial list-name lookup lost the selected closure

```r
(function(prior) {
  fs <- list(reader = function() sum(prior))
  rm(prior)
  fs$read()
})(1)
```

The uniquely matching partial dollar name and the explicit double-bracket
lookup with exact = FALSE both bypassed refusal. An exact dollar lookup of
reader correctly refused. Retaining prior kept both partial lookups legal and
returned one. These forms supplied 24 missing refusals and 24 legal controls.

The ranked hypotheses were exact-only static lookup, losing all named-list
closures, or a distinct R function being selected. The exact-name control and
the shared one-element list separated the first explanation.
`scope_value_closure()` used exact name matching and required a two-argument
index call, so neither demonstrated partial lookup recovered the existing
closure. No source data, custom list class or unknown index was necessary.
Neither #1361's literal carriers nor #1403's element-write scope described
this selected-name boundary.

## S4. Known environment-valued expressions lost mask identity

```r
{ mask <- { parent.env(environment()) }; sum(mask$prior) }
```

Selecting that same mask path with a literal if branch or numeric switch arm,
or returning it through base identity, also bypassed both dependency rules.
The repaired direct mask-alias and inheriting current-frame-alias controls
refused. Replacing the mask-producing path in each witness with an external
environment holding prior = 7 remained legal and returned seven. These forms
supplied 48 missing refusals and 48 independent-environment controls.

The ranked hypotheses were loss of the environment value at the RHS boundary,
failure of all saved aliases, or member lookup ignoring the actual target.
Direct-alias and external-environment controls separated value flow from the
latter explanations. `static_environment_frames()` followed names, literal
paths and as.environment(), but not these selected/value-returning forms.
Closed #1404 covered assignments of the literal paths themselves; those
repaired controls remained refused. This scope concerns environment values,
whereas #1401's visible-syntax work explicitly concerned closure values.

## S5. A literal delayed closure binding bypassed invocation-time checking

```r
(function(prior) {
  delayedAssign("copied", function() sum(prior))
  rm(prior)
  copied()
})(1)
```

The base-qualified spelling reproduced the same missing refusal. Literal
base assign of that closure correctly refused, and keeping prior made both
delayed bindings legal and returned one. These forms supplied 24 missing
refusals and 24 retained-binding controls.

The ranked hypotheses were a missing deferred-binding value, an unrecognized
assignment name generally, or removal failing after any reflective write.
The repaired assign control and direct-closure control refuted the latter
two. The analysis recognized delayedAssign as a binding mutation and forgot
the old binding kind, but `scope_local_closure_assignment()` retained closure
values for assign alone. The later copied() therefore lacked the visible
deferred closure's invocation-time provenance. Closed #1389 covered literal
assign, and #1402 covered force/get/match.fun return values, rather than
installation and forcing of this delayed binding.

## Replay and desired-contract feedback

Run the retained script by absolute path from an archive of the pinned source:

```sh
Rscript "$evidence/value-boundaries.R"
Rscript "$evidence/value-boundaries.R" listflow --assert
Rscript "$evidence/value-boundaries.R" listwrite --assert
Rscript "$evidence/value-boundaries.R" partial --assert
Rscript "$evidence/value-boundaries.R" environment --assert
Rscript "$evidence/value-boundaries.R" deferred --assert
Rscript "$evidence/value-boundaries.R" all --assert --controls-only
```

Observation mode asserted the dated successful-but-forbidden payload. Each
family's desired-contract mode instead required the existing dependency
refusal and intentionally failed on its first witness with exit one.
Controls-only used those same assertions on repaired refusals and legal reads.
The CSVs and family logs record the completed results. No full suite was run.

The observation execution was retained as session 95340 through terminal exit
zero. Sequential contract/control execution session 54663 was retained through
terminal exit zero. The five desired-contract child modes each returned one;
the controls-only child returned zero after 252 comparisons. The runner
collected all six children. contract-statuses.csv and individual logs retain
their results; no owned executions remained when the lane was reported.

## Duplicate checks, environment and remaining boundaries

All-state live Issue searches used delayedAssign; closure with list;
environment with alias; partial with closure; list with deletion; share with
partial matching; share with concatenation; share with delayed; closure with
subset; and environment with switch. All-state PR searches used delayedAssign
and share with list. Matches relevant to this lane were #1361, #1389,
#1401-#1404 and repair PR #1408. Their full bodies and comments were read,
alongside #130 and #173. No report describing these five demonstrated
boundaries was found at the dated retrieval. The coordinator owns refreshed
current-main confirmation and publication-time global duplicate checks.

The environment was R 4.6.1 on aarch64-apple-darwin23, macOS Tahoe 26.6.2;
pkgload 1.5.3, dplyr 1.2.1, rlang 1.3.0, tibble 3.3.1 and
data.table 1.18.6.1. The retained environment.txt records sessionInfo().
testthat parallel execution was explicitly disabled.

An exploratory inline zero-argument caller-frame factory produced zero rather
than the alleged dependency payload; that non-equivalent expression was
excluded. An environment stored inside a literal list also bypassed refusal
in an initial forward probe, but received no complete matrix or separate
finding in this lane. An eval/quote closure control and the repaired bare
closure control correctly refused. None of these exploratory probes was
counted in the 420-comparison matrix.

These are finite public-interface witnesses, not complete classification of
arbitrary R programs. Unknown caller transformations, custom list classes,
all possible structural writes, ambiguous partial names, arbitrary environment
carriers, nondefault delayed-binding environments, SQL/dtplyr/Arrow execution,
concurrency, interruption, other platforms and other dependency versions were
not established. The five recommended repair tickets are independently
verifiable and have no necessary blocking edges. No prefactoring prerequisite
was established and no implementation repair was attempted.
