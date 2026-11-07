# Margin summary, Grouping specification and Total share discovery

Investigated: 2026-11-08
Repository snapshot: `ad7284118124f11a39ac29b10f1d101399d79524`

The investigation started from freshly fetched main in the new worktree
`/private/tmp/marginplyr-summary-share-discovery-20261108`, on branch
`investigation/summary-share-discovery-20261108`. Repeated main fetches before
Issue publication resolved to the same SHA. Three fresh-context investigators
used detached worktrees at that revision; the coordinator used a separate Git
archive. Initial candidate generation was independent until all three lanes
reported. Each source copy ran at most one reproduction at a time, with
`TESTTHAT_PARALLEL=false`. No package implementation or package test was edited.
Database work used synthetic data and newly created local temporary SQLite or
DuckDB databases only.

## Contracts, history and independent expectations

The public summary, Grouping-constructor, Grouping-helper and share references
supplied the expected behavior. CONTEXT and relevant ADRs supplied the vocabulary
and boundaries: 0007-0009 for capture/Grouping identity, 0010/0014/0017 for shares,
0016/0018 for result ownership/order, 0019 for static spelling and lexical lookup,
0020/0025/0029 for lazy execution/input safety, 0026/0028 for nested selections
and local frame expansion, and 0033 for declared empty types.

Relevant sections of the November 7
[systematic](summary-spec-share-systematic-2026-11-07.md),
[contracts](summary-grouping-share-contracts-2026-11-07.md),
[invariants](summary-grouping-share-invariants-2026-11-07.md),
[transfer](summary-grouping-share-transfer-bughunt-2026-11-07.md) and
[carrier](summary-grouping-share-carrier-bughunt-2026-11-07.md) notes and the
November 6 summary hunts supplied historical controls. Source history included
the Grouping proxy, unnamed-frame, closure-carrier and SQLite repairs through
PR #1488. Those historical executions were not counted as new evidence.

Expected values and input generation were separate. The coordinator enumerated
Grouping-set occurrences explicitly, summarized each with ordinary dplyr, and
computed denominators from the input's fixed partition. A separate input
generator varied empty/single/multiple groups, duplicate facts, missing keys,
zero/negative/missing/nonfinite values and duplicate occurrences. Assertions
compared values, ratios, keys, occurrence identifiers and column order; agreement
between two Margin verbs was not used as an oracle. Other lane oracles used raw
input membership, literal expected vectors or same-backend ordinary operations.

## Confirmed new findings

Five independent repair slices were published with `bug` and `ready-for-agent`
labels. None depended on another new ticket. Each Issue contained its contract,
minimal reproducible prototype, expected/observed behavior, impact, SHA and
environment, actual dependencies, existing-report distinction and acceptance
criteria. They requested public behavior without prescribing a broad refactor.

| Boundary and hypothesis | Executed result and disposition | Issue |
| --- | --- | --- |
| Base closure converters could lose a known function's invocation-time dependency. | `base::as.function(f)` and zero-formal `base::Vectorize(f)` each missed 16 refusals. Direct invocation and repaired qualified conversion refused. Confirmed finite transfer gap. | [#1491](https://github.com/sayuks/marginplyr/issues/1491) |
| A definite singleton loop could leave the old ordinary function binding in the static scope. | A one-statement `for (i in 1L) h <- function() sum(prior)` missed 16 refusals; independent replacement and zero-iteration controls passed. Confirmed ordinary-binding gap. | [#1492](https://github.com/sayuks/marginplyr/issues/1492) |
| Exposing a recognized fresh closure environment could discard its proof without a sound dependency outcome. | Adding only `invisible(box)` made the unexposed-field refusal disappear; a local alias reproduced. Each form missed 16 refusals. Confirmed post-installation invalidation gap. | [#1493](https://github.com/sayuks/marginplyr/issues/1493) |
| An admitted sf geometry fixed key could leave a denominator mapping with an incompatible outer class. | Forty-eight Parent/Total-share cases raised `y should not have class sf; for spatial joins, use st_join`. Ordinary summaries and identical-column tibble shares passed. Confirmed denominator-join failure. | [#1494](https://github.com/sayuks/marginplyr/issues/1494) |
| A share-stage rename could revalidate already admitted dtplyr output names. | All 108 positive dot-dot source/ordinary-output/share-output cases failed during construction; 36 identifier-only controls passed. Ordinary `as.data.frame()` conversion worked. Confirmed share-stage name restriction. | [#1495](https://github.com/sayuks/marginplyr/issues/1495) |

### Dependency findings: values, controls and scope

The synthetic input was `g = c("a", "b")`, `x = c(1, 3)` and
`prior = c(10, 20)`. A local closure captured a formal named `prior`; removing
that formal before invocation exposed the enclosing summary mask. With
`prior = sum(x)` preceding the source, the prohibited source returned 1/3/4
eagerly and 10/20/30 under immutable dtplyr. Shares were respectively
.25/.75/1 and 1/3, 2/3, 1. With a share named `prior` preceding an ordinary
summary, the latter returned input sums 10/20/30 beside visible shares
.25/.75/1. The public contract required refusal in both directions.

The complete 208-call matrix crossed 13 forms, four input classes, two helpers
and both directions. It recorded 96 missing refusals, 48 correct refusal
controls and 64 successful legal controls. Eighty missing refusals concerned
the three new tickets; 16 concerned the existing installation defect discussed
below. Every successful result was compared with independent numeric vectors
and displayed-denominator ratios. The coordinator independently replayed all
208 calls from its archive. Serialized inputs remained identical.

Closed #1482 explicitly named `rlang::as_function()` and `compiler::cmpfun()`;
their repair did not establish the two base transfers. Closed #1475 specified
closure-list member writes; the uncovered singleton body assigned a local
function symbol. Closed #1476 covered fresh field installation; the new
exposure finding started with a correctly recognized field and lost it later.
The existing legal alias write replacing a dependent field with an independent
closure remained an acceptance constraint: blindly retaining an obsolete field
map would not be a valid repair.

Coverage inspection located the known-converter table, singleton list-write
tests and fresh-environment field tests. They exercised the repaired controls
but did not assert these three post-transfer/binding/exposure conditions. These
findings required marginplyr and its existing dplyr/rlang dependencies; the base
operations shipped with R. data.table and dtplyr were compatibility dependencies,
not new mandatory dependencies. Database execution of these closure expressions
was not established.

### sf denominator joining

Two rows with one shared point geometry, groups a/b and values 1/3 required
totals 1/3/4 and Total shares .25/.75/1. Removing only the share expression
succeeded; converting only the outer sf input to a tibble also succeeded with
the same geometry objects. Both helpers failed with two active-geometry names,
empty/populated input, explicit/implicit fixed keys and all three sort options.
Empty tibble controls retained double shares, and input serialization stayed
unchanged. The coordinator's independent minimal contract assertion failed on
the same sf join error after its ordinary and literal-value controls passed.

The share adapter passed an sf denominator mapping as the right-hand side of
an ordinary `left_join`. A separate ordinary-dplyr join reproduced the same sf
restriction; converting only the right outer frame to a tibble allowed the
geometry-key equality match. A Grand-total-only plan needed no such join and
succeeded. Closed #1000 and PR #1007 repaired ordinary summary-key restoration,
not this denominator mapping. Existing sf summary tests contained no share.
The requested behavior followed ADR 0016's delegation boundary and did not
require preservation of an outer sf class. Optional sf was the only additional
compatibility dependency for the reproduction.

### dtplyr output-name restoration

The minimal input had groups a/b and numeric x values 1/3. A source named `..1`
worked in ordinary dplyr and ordinary Margin summaries under `as.data.frame()`.
Adding a Total share made construction raise
`vctrs_error_names_cannot_be_dot_dot`. A safely named source produced the
expected .25/.75/1 shares. The final key-restoration rename in the windowed
share path revalidated the staged output names even with no fixed keys.

The 144-case matrix crossed `...`/`..1`/`..42`, four name roles, two helpers,
fixed-key presence and three sort requests. Source, unrelated ordinary output
and share-output names failed; identifiers alone succeeded. Controls asserted
ordinary conversion, ratios and unchanged source serialization. A separate
minimal desired-contract command exited 1 at construction. Default tibble
collection also refused those ordinary dtplyr names upstream; that separate
policy was not treated as a package defect.

Closed #827/#922 addressed ordinary output paths, #968 identifier finalization,
#999 special fixed-key reads and #1002/#1044 signed/whitespace references.
The existing lazy output tests verified ordinary dtplyr data-frame conversion;
local share-name tests and dtplyr signed-name tests did not cover this crossing.
Optional dtplyr, which imports data.table, sufficed; no database was involved.

## Existing defects and rejected candidates

- Literal `base::assign("read", callback, box)` into a fresh environment was
  confirmed as a remaining entrance to [#1476](https://github.com/sayuks/marginplyr/issues/1476).
  Its acceptance criterion covered installation into a fresh environment with a
  known field without limiting the setter spelling. A 144-case follow-up crossed
  six setters, two inputs, both helpers/directions and three scenarios. The five
  assign forms missed 40 refusals; eight direct-field refusal controls and 96
  legal independent-replacement/retained-formal controls passed. Four additional
  current-lexical-frame assign controls refused. This was an existing defect,
  not a sixth new Issue. Input, numeric and ratio assertions passed.
- A two-statement singleton member loop also lost the dependency. Commit
  `e7f41f10` had deliberately bounded #1475's transfer to one replacement after
  a control-flow concern. This was recorded as an existing limitation requiring
  a policy decision, rather than credited as a new independently scoped bug.
- dtplyr list/raw grouping restrictions reproduced with ordinary dplyr.
  Complex grouping alone succeeded, but an ordinary same-column-order union
  reproduced the Margin path's data.table complex-type restriction. Single
  grouping support therefore did not establish union support.
- SQLite list/complex and DuckDB raw/complex fixtures failed during source
  copy, before a Margin operation. A rejected DuckDB raw copy aborted that
  temporary connection; complex was rechecked on a fresh connection rather than
  inferred from the aborted one. Typed-missing raw dimensions were a documented
  rejection because raw has no missing value.
- The preliminary 80-case output-name sweep had 78 successes and two dtplyr
  dot-dot failures. Ordinary `collect()` showed a separate upstream tibble
  refusal; using the documented data-frame conversion distinguished the actual
  share-construction defect from that false lead.
- An early closure-matrix oracle incorrectly assumed eager source ratios for
  dtplyr. The failed execution was collected, the expected values were corrected
  from independent input/summary calculations, and a fresh replay passed. Two
  preliminary saved scripts also needed quote wrappers around language lists;
  final evidence used separately executable `alist()`-based scripts. Neither
  harness correction changed the package.
- A coordinator sf comparator initially compared delegated outer attributes.
  It was replaced with literal value/ratio assertions and geometry controls
  before the independent product failure was established.

## Wider verification and final coverage pass

The following were finite functional comparisons, not one combined sample or
a package-suite result. Initial lanes were integrated before the coordinator's
additional composed-Grouping and output-name comparisons.

| Area | Executed scope | Result |
| --- | --- | --- |
| Independent summary/share oracle | 1,920 cases: 80 generated inputs, three explicit set families, fixed/unfixed partitions, four measures | All values, ratios, keys, types, occurrence identifiers and output order matched the independently enumerated dplyr results |
| Composed Grouping algebra | 90 cases: identity product, composite rollup, product, overlapping product and union; empty/single/five-row input; fixed/unfixed; three duplicate policies | Explicit set-occurrence enumeration matched values, ratios, cardinality and column order or the required duplicate refusal |
| Fixed-key numeric/type boundary | 144 cases across 12 key kinds, four value vectors and three sort requests | Input-membership ratios and fixed-key prototypes matched; keys included NA/NaN/infinities, complex/raw, Date/time/duration, factors, list, packed frame and matrix |
| Share source admission | 216 cases across 27 eligible/ineligible fixtures, local/dtplyr, explicit/implicit fixed keys and checking enabled/disabled | Expected refusals inherited `marginplyr_error`; accepted shares retained double type and root one |
| Custom fixed-key equality | Six comparisons using a vctrs class grouping stored values modulo three | Raw-residue input oracle matched totals, ratios, fixed prototype and ungrouped result |
| Backend typed-key comparison | Local, dtplyr, live SQLite and live DuckDB; accepted character/numeric/factor/date/time/compound-key paths | Displayed-Grand-total ratio/double assertions passed; rejected cases were separately classified as above |
| Deferred failure/input safety | Six dtplyr derivations times type/cardinality/custom-error failure | All 18 constructed lazily, failed at collection, preserved every contributing input and retained the custom error class/message |

The numeric matrices crossed empty inputs, missing and duplicate facts,
zero/missing/nonfinite measures and duplicate Grand total occurrences. The
Grouping pass included overlapping units and renumbering after duplicate drop.
The output-name pass crossed source/input name overlap, pronoun-like names,
whitespace/newline names and both helpers. Ordinary summary frame expansion and
share dependency boundaries were also checked against the repaired controls.
No promising executable candidate from these lanes remained unclassified.

The fixed-key and custom-key matrices exercised sorting paths but did not
independently establish all row-order semantics. This investigation did not
execute remote PostgreSQL, hosted systems, Arrow's complete expression matrix,
database nonfinite behavior, other operating systems or dependency versions,
warnings/interrupt recovery, or arbitrary caller code/reflection. Such claims
would require those environments and explicit backend-specific controls.
The bounded two-statement loop policy remained a specification question, not
an unresolved assertion about the demonstrated one-statement failure.

## Tracker reconciliation, artifacts and environment

The initial complete all-state exports contained 765 Issues and 721 PR bodies
through #1488. Live follow-ups searched base conversion/Vectorize, singleton
binding, environment exposure/alias/assignment, sf/geometry/share, and dtplyr
dot-dot/share terms. Relevant full Issue threads and repair PRs were read to
distinguish root causes and acceptance scope. Newly opened
[#1489](https://github.com/sayuks/marginplyr/issues/1489), concerning authored
empty Grouping arguments and an empty splice, was read during final discovery;
no independent execution claim or duplicate report was made for it. The five
new Issue bodies and labels were fetched after publication and checked.

Reproduction code, raw logs, CSVs, session information, tracker snapshots and
lane findings stayed in temporary investigation storage under
`/private/tmp/marginplyr-discovery-root` and
`/private/tmp/marginplyr-discovery-{a,b,c}-artifacts`. The submitting worktree
contained this dated note only. Observation commands asserted exact observed
values and controls while recording defects as data; their zero exits did not
mean the public contract passed. Dedicated desired-contract commands for the
closure-environment, sf and dtplyr-name failures exited nonzero as expected.
All reproduction executions and owned work were collected to terminal
completion before submission preparation.

Environment: arm64 macOS Tahoe 26.6.2, R 4.6.1, marginplyr 0.1.0 from the pinned
source, dplyr 1.2.1, rlang 1.3.0, vctrs 0.7.3, tibble 3.3.1, data.table
1.18.6.1, dtplyr 1.3.3, sf 1.1.2, RSQLite 3.53.3 and DuckDB 1.5.5.
No package build/check or complete suite was used to discover these defects.
The PR's separate pinned local-verification and review records owned submission
readiness; the reproduction outcomes above did not certify a later commit.
