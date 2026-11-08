# Callable mutation boundaries in Margin summaries and shares

Investigated: 2026-11-08
Repository snapshot: `2298c561633591844576992ef98e039af264a6c4`

The investigation began from freshly fetched main in a new worktree at
`/Users/sayuks/Documents/marginplyr-summary-share-hunt-20261108`, on branch
`investigation/summary-share-hunt-20261108`. Fetches before publication still
resolved to that SHA. No implementation repair or package test was added.

Three fresh-context investigators independently generated initial hypotheses
from implementation, public contracts and processing consistency. They shared
candidates only after their initial reports. The coordinator independently
prepared explicit-grain reference calculations and small generated inputs.
Every reproduction process held one shared filelock, serializing executions
from this worktree, and explicitly disabled testthat parallel execution.
All execution handles were retained through terminal completion. SQLite and
DuckDB connections contained only synthetic, process-owned temporary data;
Arrow used synthetic tables, batches and a temporary dataset.

## Contract and historical sources

The public references for `summarize_with_margins()`, the five Grouping
constructors, inspection, Grouping helpers and shares supplied expectations.
CONTEXT.md and ADRs 0008, 0010, 0014, 0016, 0017, 0019, 0026, 0028, 0029 and
0033 supplied grammar, capture, denominator, attribute, dependency, selection,
input-ownership and declared-type boundaries. Errors and warnings were checked
against the package reference and ADRs 0021 and 0022.

The November 7 [systematic investigation](summary-spec-share-systematic-2026-11-07.md),
[carrier investigation](summary-grouping-share-carrier-bughunt-2026-11-07.md),
contracts and invariants investigations, and relevant November 6 boundary
investigations supplied repaired controls and neighboring conditions. Source
history through PR #1501 was inspected. Historical executions were not counted
as this investigation's evidence. The search covered whole feature paths,
including older staging and static-analysis assumptions, rather than only the
latest changes.

## Confirmed findings

The `to-tickets` workflow published two independent repair slices, each with
`bug`, `ready-for-agent`, acceptance criteria, dependencies and no blockers.
The user delegated splitting and publication without a confirmation checkpoint.
No prerequisite prefactoring was established.

| Boundary | Observed effect | Issue |
| --- | --- | --- |
| Closure body, formals and default replacement | Invocation used an obsolete definition proof or lost provenance, admitting a prohibited share dependency. | [#1502](https://github.com/sayuks/marginplyr/issues/1502) |
| Closure enclosure replacement | Invocation retained the old captured-frame proof after its environment changed, admitting prohibited dependencies locally and on dtplyr. | [#1504](https://github.com/sayuks/marginplyr/issues/1504) |

Both reports used the synthetic input `g = c("a", "b")`, `x = c(1, 3)` and
`prior = c(10, 20)`. The expected result was a Package condition: a share
source cannot depend on an earlier ordinary alias, and a later ordinary
summary cannot use an earlier share. The share reference's *Eligible source
summaries*, ADR 0010's *Decision* and ADR 0017 established those restrictions;
#130 established conservative handling of unresolved dependencies.

### Executable-definition replacement

Either of these ordinary expressions exposed the first failure:

```r
{ f <- function(prior) sum(prior); formals(f)$prior <- NULL; f() }
{ f <- function() 7; body(f) <- quote(sum(prior)); f() }
```

After `prior = sum(x)`, using the expression as `measure` and then requesting
`share_of_total(measure)` accepted a dependent source, returning measure
1/3/4 and shares .25/.75/1. In the reverse arrangement, an earlier share named
`prior` displayed .25/.75/1 while this expression as `later` returned
10/20/30. A direct closure with the actual final body correctly refused.
Keeping a local formal bound to an independent 11 remained legal.

Four valid replacement shapes, including qualified body replacement and a
changed literal formal default, crossed three eager input classes, both
helpers and both dependency directions: 48 missing refusals. A correctly
named explicit base body setter also reproduced. The coordinator's separate
eight-call replay independently asserted body/formals failures, direct
refusals and legal controls. dtplyr intercepted the demonstrated direct
definition replacements with its own alias diagnostic; no silent dtplyr
numeric result was established for those forms.

Independent replacements exposed the reverse stale-proof effect. Replacing
a dependent body with `quote(11)`, or installing a `prior = 11` formal, then
removing the obsolete enclosing binding, returned 11 in ordinary dplyr but
was falsely refused under bare replacement syntax. Correctly named explicit
base setter reassignment controls returned 11. These were literal finite
controls, not a claim that arbitrary computed code must be inferred.

The inspected carrier-write analysis handled closure class/attributes and
member containers but not body/formals setters. Bare replacement retained the
old closure descriptor; explicit setter reassignment could lose it. The
relevant closure-carrier tests exercised copies and invocation-time removals,
not replacing a local function's executable definition before invocation.

### Captured-environment replacement

The second expression changed which frame the closure captured:

```r
(function(prior) {
  f <- function() sum(prior)
  environment(f) <- parent.env(environment())
  f()
})(7)
```

The formal initially hid the summary mask; the replacement exposed it.
The dependent-source arrangement accepted measure 1/3/4 locally and
10/20/30 on dtplyr. The latter produced shares 1/3, 2/3 and 1 instead of
.25/.75/1. The after-share arrangement returned later 10/20/30 on both
inputs beside visible shares .25/.75/1. The coordinator independently
reran both arrangements on both inputs for Total and Parent shares; all
eight desired-refusal assertions failed as expected on the investigated SHA.

Removing only the replacement preserved the legal value 7. An independent
replacement enclosure containing `prior = 11` over baseenv returned 11 in
ordinary dplyr. Removing the obsolete inline formal afterwards could instead
produce a false Margin dependency refusal. Same-enclosure and safe-enclosure
controls without that removal succeeded.

The carrier-write analysis retained the descriptor's old enclosing relation.
Existing parent-environment analysis concerned `parent.env<-`, which changes
an environment's parent, rather than `environment<-`, which changes a
closure's captured environment. Updating executable-definition metadata and
updating captured-frame identity were independent repair boundaries, despite
sharing a carrier-write entry point. Neither reproduction required the other.

## Completed comparisons and refuted candidates

Counts describe separate finite comparisons, not a package-suite result.
Expectations used ordinary dplyr or simple R calculations over explicit
grouping sets; internal Grouping-plan expansion was not its own oracle.

| Area | Executed scope | Outcome |
| --- | --- | --- |
| Local numeric oracle | 80 generated small inputs, five explicitly enumerated plans, fixed-key presence/absence and four aggregates; 3,200 comparisons | Keys, occurrence identifiers, aggregate values and Total shares matched. Included empty inputs, missing keys, duplicate facts/sets, negative/zero/missing/NaN/infinite values. Nonfinite comparisons were local only. |
| Ordinary summary semantics | 16 expression sequences, three sizes and three eager classes; 144 comparisons | Per-grain ordinary dplyr matched names, values, packed/expanded shape and errors. Included source-name overwrite, preceding aliases, typed/pattern selectors, parenthesized across, named/unnamed frame results and unpacking. |
| Names | 21 spellings in source/output/dimension/fixed-key roles; 84 comparisons | 83 comparisons matched the reference. The .y output refusal was the deliberate option-typo admission rule, not a new defect. Included punctuation, Unicode, dot-dot names and names overlapping ordinary functions/pronouns. |
| Structured keys | 13 shapes, empty/populated, fixed/dimension roles, three sort requests; 156 comparisons | Independent per-grain keys, types, counts, values and both share ratios matched. Included zero-width frame/matrix/array, nested/packed frames, lists, temporal types, AsIs and factor NA levels. Exact Margin order was not asserted. |
| Unrelated ordinary cardinality | 28 local/dtplyr requests, before/after source, expansion at all/root/detail grains | Scalar controls passed; zero/multiple values were refused by dplyr locally or the dtplyr package cardinality guard. |
| Selection receivers | Local, temporary SQLite and Arrow; native/direct/wrapped context selectors, fixed-key counts and five constructors | Pure registered proxy reordering agreed with native tidyselect, including SQLite's actual tbl_lazy receiver. The suspected extension of #1481 was refuted. |
| Live SQL values | SQLite/DuckDB, rollup/cube/repeated roots, missing/fixed keys and empty/populated inputs | Ordinary Margin values and share values matched; eight independent per-grain dplyr controls passed. Typed empty-result corrections passed. |
| Volatile SQL sources | SUM plus RANDOM; SQLite and native/forced-portable DuckDB | Ratios used displayed denominators. DuckDB adapter paths were checked in rendered SQL. |
| Conditions and ownership | Custom source errors/warnings; eager data.table and six immutable dtplyr derivations, with/without shares | Original error causes/payloads survived; local repeated warnings were consolidated, lazy warnings arose at collection. Source serializations stayed unchanged in corrected controls. Nonprinting dtplyr observed zero source calls during construction and three at collection. |
| Arrow | Table, RecordBatch and Dataset; nine assertions | Ordinary summaries matched independent dplyr; Total requests refused without invoking the source measure; inputs stayed unchanged. |

Higher-order invocation through vapply/lapply/sapply/Map after an enclosing
binding removal correctly refused the prohibited dependency. Direct and
inline callback controls agreed. This ruled out the initial callback hypothesis
before the function-replacement boundary was isolated.

Four additional ordinary-selection controls checked the share-marker
intersection: implicit pick/across everything exposed only eligible ordinary
columns, an explicit earlier-share request refused, and a negative any_of
selection excluded the unavailable share name. No private marker leak was
established. The first harness wrongly expected every implicit selection to
refuse; a later bare negative selection also requested an unavailable name.
Neither was evidence of a computation defect.

Harness corrections were retained separately from successful comparisons:
empty ifelse expectations produced logical(0) rather than double(0) in the
structured and SQL probes; corrected typed expectations passed. Printing a
dtplyr query executed it, invalidating the initial laziness measurement; the
nonprinting control supplied the stated result. Comparing a live data.table
with an unserialized copy compared external pointers; before/after source
serialization supplied the ownership check. Explicit body-setter calls that
passed the body as positional envir rather than named value raised upstream
errors and were excluded from finding counts.

## Duplicate checks, environment and limits

All-state discovery exported 772 Issue and 727 PR bodies/labels to temporary
storage, below each export's 1,000-item cap. Searches included formals,
signature, body replacement, body setter, closure mutation and captured
environment replacement. Relevant full bodies/comments included #130, #1042,
#1045, #1070, #1269, #1317, #1473, #1481, #1493 and PR #1064. Live Issue/PR
searches for formals and body setters and recent Issue/open-PR refreshes
supplemented the export. Newly published #1503 concerned empty dtplyr Nest
conversion and was read and distinguished from both findings.

The reports distinguished function-definition replacement from lexical
binding removal/closure transfer, and closure-enclosure replacement from
parent mutation or exposure of a separate carrier environment. Both new
published Issue titles, bodies and labels were fetched and checked.

R was 4.6.1 on arm64 macOS, Darwin 25.6.0. Installed versions were dplyr
1.2.1, dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1, tibble 3.3.1,
vctrs 0.7.3, rlang 1.3.0, tidyselect 1.2.1, DBI 1.3.0, RSQLite 3.53.3,
DuckDB 1.5.5, Arrow 25.0.1 and pkgload 1.5.3.

After combining the three lanes, the coordinator rechecked the summary,
selection, source-provenance, staging, denominator, restoration, error and
ownership paths and the important intersections listed above. No concrete,
promising executable hypothesis remained pending. This was a finite
functional investigation, not a proof over arbitrary R programs. PostgreSQL
and other live SQL dialects, SQL execution of the new R closure witnesses,
arbitrary computed definitions/enclosures or opaque effects, every custom
class/proxy, exact structured Margin order, the complete structured-key by
duplicate-plan product, other versions/platforms and interruption remained
unverified. Such work would require a concrete boundary witness or the
corresponding backend environment.

Temporary scripts, raw logs and exploratory records remained under
`/tmp/marginplyr-hunt-root`, `/tmp/marginplyr-hunt-A`,
`/tmp/marginplyr-hunt-B` and `/tmp/marginplyr-hunt-C`. Key entries were the
coordinator's oracle.R, ordinary.R, names.R, replay-closure.R and
selection-after-share.R; A's minimal.R, closure-mutations.R and
closure-repair-controls.R; B's structured-oracle.R and sqlite-proxy.R; and
C's probe-sql-controls.R and probe-arrow.R. Observation assertions established
the dated failure, while desired-refusal assertions intentionally exited one;
those failures were not passing contract checks. All owned executions were
collected before verification and publication.

The submission added dated prose for research readers only. Package code,
tests, dependencies, installed files, generation sources, build exclusions and
verification tooling were unchanged. The existing source-package exclusion
for investigation content remained unchanged. These content/consumer effects
justified Review-ready source scope, with package build/check, complete suite
and snapshots omitted. No NEWS entry was needed for this research-only
submission. Pinned verification, integration and review records belonged on
the PR; CI monitoring was outside the requested scope.
