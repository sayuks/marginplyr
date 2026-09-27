# Preserve declared types in empty Margin results

Decision: accepted by the maintainer on 2026-09-28. Implementation: pending in
[#706](https://github.com/sayuks/marginplyr/issues/706).
[ADR 0033](../adr/0033-preserve-declared-types-in-empty-margin-results.md) owns
the contract and the rejected alternatives. This document specifies the work
for a separate implementation session; publishing it does not complete #706.

## Problem Statement

SQLite can collect a zero-row Grouping bit or Grouping identifier as logical,
even though its populated output is numeric. Type-based selection then omits
the helper, and schema-inferred storage can treat it as Boolean. Existing
`.id` and share promises already protect package-created outputs at the direct
result boundary. Users need direct Grouping helpers to follow that boundary
without changing populated values or the behavior of enclosing expressions.

## Solution

Retain existing identifier and share guarantees and implement ADR 0033's direct
helper guarantee for empty results. Keep backend-specific numeric
representations, preserve the existing direct collect/compute boundary, and
document that boundary together with the helper's value semantics. Reuse the
existing typed SQLite result machinery where it owns this responsibility.

## User Stories

1. As a summary user, I want an empty Grouping identifier to stay numeric, so
   that a filter matching no rows does not turn structural identity into Boolean.
2. As a summary user, I want Grouping bits to follow the same rule, so that
   numeric selection includes both kinds of helper output.
3. As a Margin user, I want existing integer `.id` and double share guarantees
   retained, so that this extension preserves the contracts I already use.
4. As a share user, I want nonempty all-missing prefixes to stay double, so that
   finite collection does not weaken the existing share contract.
5. As a user of local frames or dtplyr, I want helper outputs to remain integer,
   so that a remote fix does not change local behavior.
6. As a remote user, I want the backend's numeric representation retained, so
   that native and portable paths need not share an artificial integer cast.
7. As a lazy-input user, I want zero-row direct collection and materialization
   to preserve known types, so that I can inspect or store an empty result.
8. As an across user, I want a literal lambda returning a helper to receive the
   same guarantee as a directly written helper, so that column expansion does
   not change the contract.
9. As a user of dynamic names, I want declarations attached to the actual output
   names without extra evaluation, so that typing cannot create or rename columns.
10. As a summary author, I want arithmetic, conversions, and local inspection
    around a helper to retain their meaning, so that tracking its origin does
    not become an observable change to my expression.
11. As a Margin user, I want the same rows, names, Margin order, and result-class
    behavior, so that type preservation does not create a separate regression.
12. As a database user, I want construction to remain lazy and compute to retain
    destination and transaction safety, so that the fix adds no unrequested work.
13. As a user of empty input, I want any genuine Grand total row retained, so
    that empty input is not mistaken for an empty result.
14. As a maintainer, I want shared test setup without lost behavioral cases, so
    that maintenance can improve while the promises remain intact.

## Implementation Decisions

### Output identity and ownership

Carry the known helper output type through the existing summary expansion and
result finalization boundaries. Use actual resolved output names; a second
evaluation of selections, functions, or naming expressions is not an acceptable
way to discover them. Keep mixed across outputs distinct: only columns whose
entire result is a direct helper receive this declaration. Preserve static
Contextual-helper recognition, Assigned summary names, argument matching, and
SQL identifier equivalence under ADRs 0019, 0028, and 0032.

The prototype's attribute marker, mutable state, and access to dbplyr's expanded
select representation are candidate mechanisms, not required architecture.
Choose the smallest mechanism that satisfies the observable contract. If
metadata is used, it must not change ordinary evaluation, become visible in
returned values, or reach generated SQL as an extra expression.

### Result boundaries

Extend the existing live SQLite declaration path, including helper-only results
that previously did not activate it. Retain sorted and unsorted behavior, the
public-only projection, source-column anchors, finite-collection semantics,
and B-direct materialization. Activation must not introduce a tibble/data.frame
class change relative to the corresponding pre-extension path. This is a
regression constraint, not a new promise to restore arbitrary input attributes.

Inspect the affected paths on other supported backends and preserve the numeric
representations they already supply. A native/portable integer/double difference
is acceptable. Local and dtplyr integer guarantees remain exact. Simulation can
establish translation but cannot establish a live driver's collected R type.

Retain all existing `.id` and share behavior, including expansion identifiers,
Parent and Total shares, and nonempty all-missing prefixes. Ordinary lazy verbs
after a Margin result retain the delegation specified by ADR 0031. Ordinary
local operations after collection consume the already-typed R result.

### Execution and compatibility

Construct the lazy query without new input execution, type-sampling queries,
or evaluation of user expressions to guess a type. Preserve the exemptions and
explicit execution boundary of ADR 0020. Zero rows require no fabricated row or
partition. Existing compute destination admission, savepoint ownership,
rollback, ordering, and Sent query behavior remain governed by ADR 0031.

### Documentation and test consolidation

Update the Grouping-helper return documentation and the grouping-identity and
database guidance that describe remote types. State the direct-output and
direct-result boundaries, backend numeric variation, and the zero-row promise.
Keep identifier and share documentation consistent with their retained
guarantees. Regenerate affected help and other generated outputs through the
repository's documented workflow.

The evidence includes a smaller arrangement of the existing empty-type tests.
Consolidation may share fixtures and setup while preserving meaningful cases
and independent expected results. Neither the prototype patch nor its line
count is an acceptance target. Keep a broad test cleanup separate if it obscures
the helper regression and the proof that existing guarantees were retained.

## Testing Decisions

Use the existing public seam: Margin verbs followed by direct collect, finite
collect, or supported direct compute and collect. Observe actual columns, values,
types, row counts, and classes. Use DBI observations for the physical computed
schema. Prior art is the SQLite empty-declaration, declared-boundary, typed-order,
B-direct compute, Grouping-helper, share, and query-policy suites. Test effects
at those boundaries instead of pinning private helper names or metadata layout.

### Acceptance matrix

| Area | Required observations |
| --- | --- |
| Direct helpers | Both helpers; named and unnamed outputs; explicit grouping dimensions and the no-argument identifier; qualified and parenthesized spellings; one-expression blocks; helper-only and mixed ordinary-summary results |
| Across | Literal formula and function lambdas returning a helper, individually and in supported function lists; mixed helper, aggregate, arithmetic, and character outputs; static and dynamic names; actual names and baseline evaluation counts |
| Empty boundaries | Truly empty results and a finite zero-row fetch from a populated result; full/direct collect, finite collect, and supported direct compute followed by collect; numeric helpers with no phantom rows or columns |
| Plans and order | One-set and multi-set plans; rollup masks with distinct values beyond zero/one; fixed partitions; retained duplicate occurrences; none/first/last Margin order; text, NULL, and typed-missing labels |
| Existing declarations | Summary and expansion `.id`; Parent/Total shares, direct/across; empty, nonempty all-missing finite prefixes, and populated controls; source-dimension types |
| Ordinary expressions | Enclosing arithmetic, character conversion, local `typeof()`/`identical()` and syntax inspection retain baseline meaning and types; ordinary aggregates and a same-valued literal acquire no helper-specific type promise |
| Result shape | Exact public names, order, row count, values, and pre-extension class behavior; no internal metadata or sorting columns leak; helper-only activation covers the prototype's class regression |
| Backend controls | Local frame and dtplyr integers; live SQLite direct/materialized numeric results; available Arrow and native/portable DuckDB numeric results without cross-backend integer normalization; retain existing other-backend translation coverage |
| Laziness and compute | No new construction-time input reads or expression evaluations; existing destination, transaction, rollback, finite-limit, Margin order, and Sent query checks continue to pass |

The full Cartesian product is not required. Choose independent cases that reach
each condition and the known interactions: dynamic mixed across names, sorted
helper-only activation without `.id`, and zero-row finite collection. Preserve
the original label/order/plan matrices if consolidating the old tests. Use
independently calculated bit/mask expectations, not captured prototype output.

Include a numeric-selection assertion on the collected empty helper columns.
The Parquet experiment is supporting evidence rather than a mandatory
SQLite-plus-Arrow integration test: the repository's optional-backend policy
still applies. Separate backend tests can establish their own output types;
do not make one test depend on both optional backends to reproduce the motivating
storage example. Existing expected refusals on unsupported backend expressions
remain refusals.

### Completion gates

- Establish public regressions that fail on the pre-extension implementation.
- Resolve the prototype's result-class regression; preserve populated outputs
  and evaluation counts alongside the new empty-type expectations.
- Complete the documentation changes and required generation/verifiers.
- Run the fixed Review-ready check on a clean committed implementation, following
  [local-check ownership](../agents/local-checks.md), and record its terminal
  result and exact commit. The exploratory results do not substitute for it.
- Run Standards and Spec review against the merge-base with main and record
  the pushed snapshot and dispositions in the implementation PR under the
  [repository review rules](../agents/code-review.md).
- Report actual dependency versions and live backend coverage. Keep untested
  live backends explicit rather than treating simulated SQL as driver evidence.

## Out of Scope

- General zero-row type inference for ordinary summaries or arbitrary enclosing
  expressions, including `dplyr::n()`.
- Normalizing all remote helper outputs to R integer or extending supported
  Contextual-helper spellings and across forms.
- Propagating this guarantee through arbitrary later lazy transformations.
- Replacing the B-direct materializer, adding general attribute restoration,
  or changing the query policy.
- Shipping a prototype patch unchanged or deleting historical evidence.
- Implementing package behavior or closing #706 in the specification PR.

## Further Notes

#706 is the single implementation ticket. #653, #665, and #666 are completed
repairs whose guarantees must be retained, not new blockers. The accepted policy
supersedes the withdrawal recommendations in older discussion comments. The
[dated evidence index](../../investigation/zero-row-type-policy-2026-09-28.md)
links the exact prototype snapshots, reproducible observations, corrected cost
comparison, and known limitations. Start the implementation from the production
branch containing this specification, rather than merging the throwaway branch.
