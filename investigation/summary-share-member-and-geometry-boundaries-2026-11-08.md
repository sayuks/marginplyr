# Summary member replacement and geometry denominator boundaries

Investigated: 2026-11-08
Repository snapshot: `d6f1f45824ddc04fd25fcd67860a016b2f9b6ea0`

The investigation began from freshly fetched main in a new worktree at
`/private/tmp/marginplyr-summary-share-hunt`, branch
`investigation/summary-share-deep-20261108`. The prepublication fetch resolved
to the same SHA. No implementation repair, package test or dependency change
was made.

Three fresh-context investigators independently generated candidates from
implementation, public contracts and processing consistency. Each used a
separate detached worktree at the same SHA. Candidates were shared only after
all three initial reports. The coordinator separately prepared explicit-grain
reference calculations and small inputs. Each worktree ran at most one
reproduction or focused-test execution at a time, with
`TESTTHAT_PARALLEL=false`; terminal results were collected before another
execution. Database work used only process-owned in-memory SQLite/DuckDB
databases and synthetic temporary tables.

## Sources and expectations

The public references for `summarize_with_margins()`, Grouping constructors,
inspection, Grouping helpers and both shares supplied the expected behavior.
CONTEXT.md and relevant ADRs, including 0007, 0008, 0010, 0014, 0016, 0017,
0019, 0026, 0028 and 0033, supplied capture, grammar, source eligibility,
denominator, dependency, attribute and declared-type boundaries. The
November 7 systematic and November 8
[summary discovery](summary-share-discovery-2026-11-08.md) and
[callable-mutation investigation](summary-share-callable-mutation-hunt-2026-11-08.md)
supplied historical assumptions and repaired controls. Commit history through
PR #1511 was inspected. Historical runs were not counted as new evidence.

Input generation was separate from expected-value calculation. The coordinator
explicitly listed each plan's grouping sets, ran ordinary per-grain dplyr
summaries, and calculated denominators/ratios with ordinary R arithmetic and
fixed-key joins. SQL controls used ordinary SQL-backed dplyr summaries at each
explicit grain to retain the database's own aggregate semantics. Grouping-plan
internals and agreement between share helpers were not correctness oracles.

## Confirmed unreported defects

The `to-tickets` workflow published two independent repair slices with `bug`
and `ready-for-agent`, contract sources, minimal reproductions, actual and
expected behavior, environments, acceptance criteria and no blockers. Splitting
and publication had been delegated without a confirmation checkpoint. No
prerequisite prefactoring was established.

| Boundary | Observed failure | Report |
| --- | --- | --- |
| Definition/enclosure replacement through a known closure member | The old callable proof survived; prohibited dependencies were accepted and proven independent replacements could be refused. | [#1513](https://github.com/sayuks/marginplyr/issues/1513) |
| A non-key active geometry summary in a private denominator mapping | Adding shares renamed the ordinary geometry and exposed denominator geometry; legal suffix-like names could raise internal sf errors. | [#1515](https://github.com/sayuks/marginplyr/issues/1515) |

### Closure member replacement

The minimal input was `g = c("a", "b")`, `x = c(1, 3)` and
`prior = c(10, 20)`. After `prior = sum(x)`, this expression as `measure`,
followed by `p = share_of_total(measure)`, was accepted:

```r
{
  box <- list(f = function() 7)
  body(box$f) <- quote(sum(prior))
  box$f()
}
```

It returned measure `1, 3, 4` and shares `.25, .75, 1`, instead of refusing
a source dependent on an earlier ordinary alias. The bare receiver control
`f <- function() 7; body(f) <- quote(sum(prior)); f()` correctly refused.
An unchanged independent member and replacing its body with `quote(11)`
remained legal.

In the reverse arrangement, a share named `prior` preceded this expression
as `later`. It returned input-derived `10, 20, 30` beside the visible share
`.25, .75, 1`, instead of refusing a later expression's dependency on that
share. Parent shares reproduced both directions.

Formals deletion and captured-environment replacement exposed the same
member-receiver boundary. The completed matrix crossed three replacements,
four member forms (`$`, positional `[[`, nested list and fresh environment),
both helpers and both dependency directions. All 48 nested-assignment cases
missed the required refusal. All 48 explicit member-reassignment controls
correctly refused. The coordinator independently asserted the body witness's
four helper/direction cases and direct/independent controls in another process.
Replacing an originally dependent member with an independent body/formals or
enclosure could also leave an obsolete dependency and falsely refuse; ordinary
dplyr controls returned 11.

The replacement-target reader descended through `body(box$f)` to `$` and
missed its definition-write route. The generic carrier-write route also
left the old callable proof in place. This was a common receiver/writeback
boundary across the three setters, rather than three independent proposed
repairs. Explicitly assigning a base setter's result into the member used a
working path.

Twelve dtplyr checks distinguished its own limitations: body/formals witnesses
were rejected by upstream translation in eight cases, while four enclosure
cases accepted the forbidden dependency. Those returned measure `10, 20, 30`
and shares `1/3, 2/3, 1`, or later `10, 20, 30` beside the earlier share.
These were explicit collection results; no early data read was used to
establish eligibility.

Closed #1502 and #1504, repaired by PR #1511, concerned bare receivers and
definition/enclosure metadata. Their bodies, comments and acceptance criteria
were compared with the member witness. Existing definition-replacement and
closure-list tests exercised each behavior separately, not this intersection.
The new report retained those repaired controls and bounded the requested
repair to finite visible member paths; it did not demand inference of arbitrary
R programs or execute caller functions during planning.

### Geometry retained in denominator mappings

A two-row sf input had ordinary `g`, numeric `x = c(1, 3)` and active point
geometry. The admitted summary explicitly computed both `value = sum(x)` and
`geometry = sf::st_union(geometry)` under `rollup(g)`. Ordinary sf per-grain
dplyr summaries and the Margin summary without shares succeeded.

Adding `p = share_of_total(value)` changed the ordinary output name
`geometry` to `geometry.x` and added unrequested `geometry.y`. Names were
`g, value, p, geometry.x, geometry.y`, instead of the requested
`g, value, geometry, p`. Ratios were still `.25, .75, 1`. Parent shares
reproduced; both helpers together could produce three geometry columns,
including `geometry.x.x` and `geometry.x.y`.

Eighteen initial comparisons crossed a nongeometry fixed key's
absence/presence, all three Margin orders and Total/Parent/both. All exposed
the schema defect. Their 18 identical-column tibble controls and ordinary sf
controls passed. Ratio assertions and serialized-input comparisons passed.
A separate coordinator two-row replay asserted both helper failures and the
normal names, ratio and input controls.

Twelve additional public-name cases placed suffix-like names in share,
numeric-source, ordinary-output and `.id` roles. Four `geometry.x` cases
raised an sf invariant error or `object(s) should be of class 'sfg'`
(`Rcpp::exception`). Corresponding ordinary sf and tibble-share controls
succeeded. These were consequences of the same private-column contamination.

A further 64-case matrix crossed empty/populated input, geometry/shape active
names, nongeometry fixed keys absent/present, two labels and rollup/cube/
repeated-root/Grand-total-only plans. All 48 cases requiring a join exposed
schema contamination; all 16 no-join Grand-total-only controls passed. Source
values, identifiers, ratios, geometry elements and input serialization were
asserted. The failure therefore included empty inputs and custom geometry
names. Comparing empty sf column attributes initially demanded an attribute
guarantee ADR 0016 did not make; the corrected assertions compared geometry
elements and public schema instead.

sf's `transmute()` retained active geometry in a private denominator relation
even though the projection selected only keys and denominator values. The
subsequent outer-class conversion retained that extra column, so the ordinary
join suffixed the geometries. The backend followed its sticky-geometry rules;
the private relation exposed a column the caller had not requested.

Closed #1494 repaired sf-to-sf join refusal when geometry itself was a fixed
key. That key needed retention, whereas this witness had geometry as an
ordinary summary payload. The sf share regressions placed geometry in `.by`,
so their assertions could not detect this case. Related #1000/#993 concerned
ordinary restoration/order, and #1276 concerned nesting. This report requested
the explicit public columns and no arbitrary class/attribute restoration
beyond ADR 0016.

## Other candidates and finite verification

Each lane kept short hypothesis/evidence/result/disposition/next-step records
outside the submitting worktree. The following table summarizes their completed
comparisons, rather than a package-suite result.

| Hypothesis and distinguishing check | Executed evidence and disposition | Next evidence needed |
| --- | --- | --- |
| Basic grouping sets or Total denominators might differ from explicit-grain calculation. | 200 coordinator comparisons: five small inputs, five plans, fixed keys absent/present, local/dtplyr/SQLite/DuckDB. After correcting the oracle's empty type and SQL aggregate assumptions, all matched. | A different concrete grain/value boundary or backend. |
| Runtime local selection might retain deleted columns, stale types or packed/expanded schemas. | 30 ordinary-summary comparisons over populated tibble, empty tibble and plain frame; NULL deletion, type overwrite, unpacking, packed frames, empty frames, rewritten assigned frames/matrices and data-dependent naming. Results or expected upstream errors agreed with ordinary per-grain dplyr. | A new expression/evaluation boundary. |
| Structured keys might change Total denominator identity or mutate inputs. | 102 lane comparisons: 17 key kinds, fixed/dimension placement and three sort choices. Ninety-nine matched independent ratios/ordinary output and preserved inputs; three raw typed-missing dimensions were refused as documented. | Exact arbitrary-class/backend ordering was not established. |
| Fixed-key database equivalence might differ in the denominator. | 288 SQLite/DuckDB comparisons across BINARY/NOCASE/RTRIM or NOACCENT collations, one/two fixed keys, four plans, two labels and three sort modes matched independent SQL denominator calculations. | Other dialects or collation operators. |
| Specification composition might misnumber duplicate occurrences, keys or helper masks. | Seed 151108 generated 35 independently enumerated families, crossed with fixed keys absent/present and three duplicate policies: 48 required refusals and 162 exact accepted plans matched. All 162 accepted Total-share admission/value/double checks passed. | Larger plans or another concrete composition boundary. |
| A later dynamic unnamed frame might silently overwrite an eligible share source. | Five source/position/schema boundaries were correctly refused; normal `.25, .75, 1` control passed. | No concrete pending candidate. |
| A top-level local factory might lose an authored missing constructor position before an empty splice. | All 25 constructor/consumer comparisons accepted an invalid inline factory; stored controls refused and valid no-empty controls matched. Confirmed residual entrance to existing #1489, not a new report. | Repair/regression under that existing contract. |

The factory witness was:

```r
.grouping = {
  f <- function() grouping_set(a, , !!!list())
  f()
}
```

Storing the same factory's result first produced the required Argument 2
refusal. An immediate literal factory was already correctly refused; a missing
argument in an immediate function's default also correctly refused. The
single selected existing test file `test-grouping-quoted-factories.R` passed
without skips; its assertions did not exercise the local factory witness. The
existing #1489 comment had explicitly reported the alias/factory residual as
the same outer-capture/trailing-omission cause. Newly published
[#1514](https://github.com/sayuks/marginplyr/issues/1514), read during final
reconciliation, concerned a distinct directly spelled constructor in an
evaluated quotation operand and documented a PR #1511 regression. No duplicate
factory Issue was opened by this investigation.

Harness errors were kept separate from product findings. An initial oracle
used R's `sum(numeric()) = 0` for SQL empty/all-NULL groups, where SQL returned
NULL, and used `ifelse()` to construct an empty expected share, producing
logical rather than double. Corrected ordinary-backend controls and typed
expectations passed all 200 comparisons. An initial generated closure AST
caused 16 explicit-enclosure controls to raise `bad value`; parser-normalized
public expressions gave all 48 expected explicit-setter refusals. An initial
sf probe used an unsupported dynamic selection in `.by`; completed probes
used `all_of(fixed)`. None of those harness failures counted as a defect.

## Reconciliation, environment and limits

The coordinator exported 777 all-state Issues with bodies/comments and 500
recent PR bodies, then used live searches and a recent-Issue refresh. The PR
export was bounded; live searches and relevant full reports supplemented it
rather than treating the export as exhaustive. Queries included `body member`,
`closure member replacement`, definition/enclosure replacement, closure
setters, `geometry`, `sf share`, and the known reports above. New reports
#1513/#1515 were fetched back with their bodies and labels. Concurrently
published #1512/#1514/#1516 were checked for overlapping subjects; #1514 was
read in full and distinguished above.

R was 4.6.1 on arm64 macOS. Installed versions included dplyr 1.2.1,
dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1, sf 1.1.2, vctrs 0.7.3,
rlang 1.3.0, tibble 3.3.1, tidyselect 1.2.1, DBI 1.3.0, DuckDB 1.5.5,
RSQLite 3.53.3 and pkgload 1.5.3. Runtime dependencies and optional controls
were separated in each Issue. No installation or dependency promotion was
needed.

After combining the three initial reports, the investigation revisited
summary evaluation, static/lexical lookup, member transfer, source validation,
Grouping capture, fixed-key equivalence, denominator projection, result names,
types and input ownership. No concrete promising executable hypothesis was
left pending before preparation. This was finite evidence, not proof over all
R expressions or all supported backends. Live PostgreSQL and other SQL
dialects, remote execution of the R closure witnesses, arbitrary callable
effects/custom classes, all structured-key by duplicate-plan combinations,
every version/platform and interruption behavior remained unverified. Such
work required a concrete new witness or the corresponding backend environment.

Reproduction code, raw logs, result tables and candidate records remained in
`/private/tmp/marginplyr-evidence-1511-root`,
`/private/tmp/marginplyr-evidence-1511-a`,
`/private/tmp/marginplyr-evidence-1511-b` and
`/private/tmp/marginplyr-evidence-1511-c`. Coordinator entries included
oracle.R, ordinary.R and replay.R; lane evidence included
carrier-matrix.R, factory-matrix.R, probe-collation.R and
03-sf-summary-geometry.R. Observation assertions established the dated
failures; desired-contract assertions intentionally failed at those boundaries
and were not passing package checks.

The submission added dated research prose for investigation readers only.
Package code, tests, installed files, dependencies, generated documentation,
generation sources, exclusions and verification tooling were unchanged.
The investigation source-package exclusion was unchanged. Those content and
consumer effects justified Review-ready `source`, with
`package build/check, complete suite and snapshots omitted`. No NEWS entry
was needed for this research-only change. Pinned verification, main integration
and separate review records belonged on the PR; CI monitoring was outside the
requested scope.
