# Adopt B-direct for SQLite typed Margin results

Decision: accepted by the maintainer. Specified: 2026-09-26. The shared
materializer for #661–#663 is implemented; #665/#666 remain separate work.
This specification implements the adopted design; it does not reopen the
A/B/C comparison without new contrary evidence.

## Problem Statement

Users need SQLite Margin results to keep their promised types, Margin order,
and public columns through collection and direct materialization. The dedicated
SQLite path also owns destination writes, transaction boundaries, and metadata
returned to dbplyr. Its current implementation can silently write to a different
schema (#661), reject caller-owned transactions (#662), and return a computed
table whose ordinary selection or renaming fails (#663). Empty expansion
identifiers (#665) and all-missing finite share prefixes (#666) still lose their
package-declared types.

The recent empty-type repair extended the dedicated path to unsorted summaries
with identifiers or shares. Individual fixes must not enlarge the package's
materialization machinery again merely to retain those types. Users must not pay
for a loss of correctness, input safety, or ordinary dplyr compatibility in
exchange for a smaller implementation.

## Solution

Adopt **B-direct**. Keep the type-preserving direct collection boundary. For
SQLite direct materialization, create the destination with a zero-row type
anchor, then insert the ordered public query directly into that typed table.
Do not materialize the full result into a package-owned intermediate table.

Preserve the existing result guarantees and ordinary downstream operations.
Resolve the destination consistently, refuse known unsafe unqualified-name
collisions before writes, and use a private savepoint without taking ownership
of a caller's transaction. Repair package-declared types at the result boundary
for empty and nonempty all-missing columns. Keep #664 as an independent
data.table bug fix.

This adoption includes two explicit operational changes: safe refusal of the
specified ambiguous destinations, and destination-metadata queries only within
an explicitly requested compute. It does not waive types or Margin order.

## User Stories

1. As a SQLite user, I want full direct collection to preserve typed-missing
   grouping dimensions, so that an all-missing result retains its promised type.
2. As a SQLite user, I want finite collection to return the requested prefix,
   so that limiting rows does not change the result's meaning.
3. As a SQLite user, I want zero-row collection to preserve package-declared
   types, so that empty and populated results have compatible schemas.
4. As an expansion user, I want an empty Grouping set identifier to be integer,
   so that filtering all observations does not change the identifier contract.
5. As a summary user, I want existing empty identifier and share guarantees to
   remain intact, so that the expansion repair does not regress summaries.
6. As a share user, I want Parent and Total shares to remain double in an
   all-missing nonempty prefix, so that a row limit does not turn them logical.
7. As a share user, I want direct and across-generated shares to follow the same
   rule, so that equivalent expressions have compatible results.
8. As a Margin user, I want first and last Margin order to retain its Grouping
   structure and missing-value placement, so that totals stay with their rows.
9. As a database user, I want direct collection of a materialized Margin result
   to preserve that order, so that compute is a usable alternative to collect.
10. As a database user, I want the physical table to contain only public result
    columns, so that internal sorting keys do not become part of my schema.
11. As an attached-database user, I want the complete requested identifier to
    survive materialization, so that another schema's table is never populated.
12. As a database user, I want creation, overwrite, indexes, insertion, analysis,
    and the returned table to address the same destination, so that a correct
    creation is not followed by an unrelated mutation or read.
13. As a database user, I want unsafe unqualified-name collisions refused before
    writes, so that an ambiguous name cannot damage another table.
14. As a database user, I want supported identifier spellings and quoting to be
    handled consistently, so that table-only identifiers do not bypass safety.
15. As a database user, I want ordinary temporary and persistent destinations,
    indexes, and analysis to keep working, so that safety does not reject normal
    materialization workflows.
16. As a transaction owner, I want compute to work inside my transaction, so
    that I retain the choice to commit or roll back all my work.
17. As a transaction owner, I want a failed compute to roll back only its own
    work, so that operations I completed earlier remain under my control.
18. As a database user, I want failure to restore an overwritten destination and
    leave no package-created resources, so that a failed attempt can be retried.
19. As a dplyr user, I want select, rename, filter, mutate, and arrange after
    compute to succeed, so that a Margin result remains a usable lazy table.
20. As a dplyr user, I want the documented ordering boundary to remain explicit,
    so that ordinary later verbs are not presented as preserving Margin order.
21. As a lazy-input user, I want query construction to avoid new input reads,
    so that preparing a Margin operation does not execute my data pipeline.
22. As a database user, I want destination checks to run only when I request
    materialization, so that direct collection does not acquire new schema work.
23. As an SQL auditor, I want the result record to match direct result rendering,
    so that the recorded query does not hide internal columns actually selected.
24. As an SQL auditor, I want the construction record distinguished from a full
    execution ledger, so that compute statements are not falsely attributed to
    another Margin call.
25. As a user running with warnings as errors, I want auditing to leave the
    original warning and error behavior unchanged, so that enabling it cannot
    make a failing query succeed.
26. As a user of ordinary SQL expressions, I want their driver-dependent types
    left to dbplyr and the driver, so that package repairs do not silently impose
    a new type contract on my aggregates.
27. As a user of other backends, I want the SQLite change to preserve my existing
    behavior, so that a backend-specific repair does not become a general rewrite.
28. As a maintainer, I want each existing Issue's acceptance criteria traceable
    to public behavior, so that a passing combined implementation does not hide
    an unresolved defect.
29. As a user exposed to #661, I want an interim write-safety measure available
    before the complete replacement is ready, so that the redesign does not
    justify leaving silent wrong-destination writes enabled.

## Implementation Decisions

### One collection boundary and one materialization owner

The user-approved test boundary is the existing public Margin verbs, collect,
compute, subsequent ordinary dplyr operations, and independent DBI observations.
Do not introduce a public testing API or a general backend abstraction for this
SQLite-only change.

The dedicated SQLite result boundary has two reasons to exist: source-column
anchors and package-created declared types. Its scope includes the supported
sorted and unsorted summaries and expansions, including text labels, one-set
plans, identifiers, and contextual shares where those reasons apply. It is not
accurately described as only sorted typed-missing summaries.

Direct sorted collection retains the outer typed compound query and removes
internal ordering columns at the R boundary. Unsorted collection retains its
ordinary dbplyr route plus the applicable declared-type repair. Preserve current
finite-limit validation, truncation, row-count, and warning behavior established
by #654. Sorted dedicated collection retains its successful fractional positive
limit truncation; delegated unsorted and materialized collection retain their
existing dbplyr behavior. The measured ordinary dbplyr version rejects that
fractional limit, so no cross-path parity is promised for that argument edge.

For direct compute, create the typed public destination from the zero-row
anchor and insert the public query once. For sorted results, that query carries
the existing Margin order. The package creates no full-result intermediate
stage. This removes one explicit full-result table copy on sorted compute; it
is not a promise that SQLite performs no internal sort or temporary I/O.

The destination's public schema supplies the types that direct collection
obtains through the outer anchor. Preserve character, integer, and double
source-dimension guarantees, character declarations for text Margin labels,
and the integer identifier/double share contracts.
Register expansion identifiers independently of applying existing declarations
to all-missing columns. Do not infer new types for arbitrary aggregate outputs.

### Destination identity and delegation

Retain a complete, correctly quoted destination identity throughout creation,
overwrite, indexes, insertion, analysis, and construction of the returned table.
Use supported public identifier APIs. Never reduce a qualified destination to a
bare name for follow-up SQL. Bare temporary destinations resolve to temp; bare
persistent destinations resolve to main. Preserve literal dots and quoted names
as identifier content rather than guessing that they name a schema.

Before any writes, refuse these unsafe bare-name combinations with a Package
condition that explains a usable correction:

- a persistent destination shadowed by a same-named temporary table;
- a temporary destination with overwrite requested, absent from temp but
  present with the same name in main.

Apply the same decision to strings, table-only DBI identifiers, dbplyr
identifiers, and literal-identifier forms. Explicit schema names are distinct
from these ambiguous bare cases. In particular, an explicit temp destination
with temporary materialization must not be conflated with an attached non-temp
schema that SQLite refuses for a temporary table.

Destination metadata may be queried only during an explicit compute and only
as needed to resolve/refuse unsafe writes. This is an explicit amendment to
ADR 0031's unqualified prohibition of extra schema queries. It grants no new
input-schema probe and no lazy-construction exemption under ADR 0020. One DBI
metadata call can issue several SQL statements; do not promise a fixed SQL
count independent of driver version.

Delegate supported destination creation, overwrite, and index behavior to
dbplyr after admission is safe. Retain the measured upstream qualified-index
limitation rather than adding a new index implementation: on a dependency
version where ordinary dbplyr rejects that request, preserve the failure and
atomically restore state. Ordinary bare-name indexes must still succeed.
Qualified destinations without the unsupported index request remain supported,
including overwrite and analysis. No upstream limitation permits unrelated
writes or loss of caller state.

### Savepoint ownership and failure behavior

Own one uniquely named savepoint for the complete mutation interval. Do not
unconditionally begin, commit, or roll back a caller-owned transaction. When an
outer transaction exists, release only the package's savepoint on success; the
caller must still be able to commit or roll back the result together with their
prior work. Without an outer transaction, successful release completes the
atomic materialization.

Accept supported logical in_transaction choices at the public compute boundary.
Both TRUE and FALSE retain the package's required atomic savepoint; neither
requests a competing nested BEGIN. FALSE does not mean that this dedicated
multi-statement operation sends no transaction-control SQL. Consume that option
at the materialization boundary rather than forwarding a second transaction
request to the inner dbplyr operation. Validate invalid values through the
normal public error conventions, not prototype assertions.

On an ordinary execution, insertion, index, or analysis failure, roll back and
release only the owned savepoint, restore the previous destination when
applicable, preserve input and unrelated tables, and allow a subsequent valid
operation. No package staging table should need cleanup because none is
created. Preserve the causal backend failure if cleanup itself fails; do not
claim successful rollback when it could not be completed.

Overwriting a table used by the input is not a new success guarantee. A rejected
self-overwrite must leave that input unchanged. Do not add full-result staging
merely to gain the extra successful self-overwrite demonstrated by B-stage.

### Order, result class, and downstream operations

Keep the existing Margin order key and its direct-result/direct-materialization
scope. Read the materialized table with an explicit ORDER BY on an available
implicit rowid alias. Keep window ordering empty so that raw rowid SQL does not
break dbplyr's selection/renaming metadata. Return a normal usable lazy table
without public or physical internal ordering columns.

If all three rowid aliases are shadowed under SQLite's case-insensitive naming,
refuse sorted direct materialization before writes. Direct collection remains
available; unsorted results must not inherit an irrelevant sorted-only refusal.
The new unsafe-destination refusals mean the earlier ADR statement that rowid
shadowing is the only materialization refusal must be amended.

Ordinary later dplyr operations use ordinary dbplyr semantics. They must execute
successfully on their supported inputs, but the package adds no promise that
they preserve the original Margin order or special direct-result type repairs.
The guarantee does not extend to caller mutation of the physical table, VACUUM,
or a new unarranged table object created after reconnecting.

### SQL audit boundary

The Sent query record belongs to the last tracked Margin verb or inspection
call during construction. Its result entry must match the SQL rendered for full
direct collection, including internal selected columns. Audit on/off must not
change query values, execution timing, source reads, warning frequency state,
or the original warning/error outcome.

Finite collection adds its requested LIMIT at execution; compute uses the public
query within its materialization statements. Destination metadata, savepoints,
DDL, INSERT, ANALYZE, and execution-time LIMIT are not retrospectively appended
to the global construction record. In particular, computing an older query
must not append its SQL to a newer Margin call's record. This is a construction
record, not an implementation of a complete DBI execution ledger. Amend broader
ADR/glossary wording to state this boundary explicitly.

### Decision and documentation ownership

Record this adoption as an amendment to ADR 0031, preserving its historical
reasoning and identifying implementation as pending until the replacement
lands. It owns the changed materialization mechanism, destination refusals,
metadata-query exception, and transaction contract. The specification owns
implementable acceptance and validation requirements; the dated investigation
owns measurements and sources, not the adoption decision.

The implementation must reconcile related decisions and public documentation:

- ADR 0018 retains the Margin order key and scope, and aligns the materialized
  rowid mechanism with empty window ordering.
- ADR 0020 keeps its existing unrequested-read exemptions and updates its account
  of explicitly requested SQLite execution and its corresponding policy gates.
- ADR 0027 and the Sent query glossary entry distinguish construction recording
  from materialization execution without changing audit/warning independence.
- ADR 0016 and public result-class explanations describe the actual declared-type
  boundary instead of limiting it to sorted typed-missing results.
- The summary, expansion, share, audit reference, and database guide distinguish
  direct/full, finite, materialized, and downstream behavior; explain safety
  refusals and relevant upstream limitations; and regenerate affected help and
  README output through the repository's normal generation workflow.

## Testing Decisions

Use external behavior and independently calculated expectations. A prototype's
output is evidence, not a golden result to copy. Observe both the returned query
and physical database state through DBI; a correct returned table alone does not
prove that another schema was left untouched. Prefer the existing public test
seams over assertions about helper names, private classes, or exact SQL layout.

### Required regression and acceptance matrix

| Area | Required observations |
|---|---|
| #661 qualified destinations | Both in_schema and DBI identifier forms; another schema's compatible same-name sentinel; sorted typed and unsorted identifier results; exact values, row count, public schema, types, order, overwrite, analysis, and untouched other schema |
| #661 unsafe bare destinations | Both dangerous main/temp directions; identifier spelling/quoting variants; refusal before mutations; tables, indexes, statistics, and returned identity; attached-only same-name controls and subsequent valid operations |
| #662 transactions | Outer transaction present/absent, TRUE/FALSE choices, success/failure, sorted/unsorted; caller marker preserved; caller commit verified after reconnect and caller rollback verified independently; overwritten target restoration, no resource leaks, retry |
| #663 downstream operations | Compute before select/rename for first/last order; independently expected values and row counts; direct materialized order; filter/mutate/arrange controls; physical public-only schema; partial/all rowid alias collisions |
| #665 empty expansion | Default call with only identifier supplied; filtered-empty input; explicit specifications; none/first/last; text, NULL and typed-missing labels; direct and supported materialized collection; integer identifier, zero rows, public names/order, source types; populated and local/driver controls |
| #666 share prefixes | Parent/Total, direct/across, sum/mean, missing and zero denominators, fixed partitions; zero rows, nonempty all-missing, mixed/nonmissing and unlimited results; direct and materialized prefixes, both order choices and unsorted controls; double shares and unchanged other columns |
| Existing successful behavior | Empty summary identifier/share guarantees from #653; source all-missing character/integer/double; fixed-key missingness, duplicate/single grouping sets, name collisions, finite validation and warning behavior from #654 |
| SQL audit and lazy construction | No new construction-time execution or input-schema reads; result/render equality; no added audit rows after finite collect/compute; q2 constructed after q1 then compute(q1) does not contaminate q2; #655 fresh-process audit on/off and warnings-as-errors controls |
| Delegated operations | Ordinary dbplyr controls for naming, indexes, analysis, validation and known upstream failures; no new aggregate/attribute promises or regressions on unaffected backend paths |

Add product-quality argument tests for names, temporary/overwrite/analyze,
in_transaction, n, supported forwarded arguments, SQL options, and deprecated
options where still supported. Invalid combinations must not mutate state or
produce internal assertion errors. Test explicit temp-schema behavior separately
from other schemas with temporary materialization. Include SQL keyword names,
case variants, quoting, and literal dots in the supported identifier forms.

Retain the current SQLite type/order regression tests and broader Margin order
tests as prior art. The existing audit fresh-process and query-policy suites are
also mandatory validation, including their non-SQLite controls where applicable.
Structural query-policy snapshots must continue to expose execution entry
points; changing a snapshot is not a substitute for proving construction stays
lazy. Observe the absence of a package-owned full-result stage at the database
execution boundary without depending on random table names.

### Completion gates and bounded unknowns

Before publishing a package-affecting change for review, run the repository's
fixed Review-ready check against a clean committed state and retain its actual
terminal result. It includes both linters, package spelling, the full suite,
strict line coverage, and the source-tarball check. Run required documentation
and policy verifiers and applicable release-matrix jobs. Review Standards and
Spec against the merge-base with main, provide the repository's review-finding
rules, and record the pushed snapshot and dispositions in the pull request.
Subsequent package changes invalidate earlier gate evidence.

The prototype did not discharge those gates. The supported release matrix owns
its existing R-version, operating-system, and backend coverage. Record the
actual dbplyr, DBI, RSQLite, and SQLite versions used; that matrix is not a
database-dependency-version matrix. Identify any additional dependency versions
checked during implementation and mark the rest unverified. Interface-validation,
identifier, audit, and ordinary rollback gaps above are implementation work, not
reasons to reselect A or C.

For connection loss, disk exhaustion, rollback/release failure, or competing
writers, preserve failure context and document what cleanup can establish.
Where safe deterministic failure injection is available, exercise it; otherwise
record the unverified scenario and its limitation explicitly. Do not promise
that savepoints preserve caller work when SQLite or the connection has already
aborted the whole transaction. Large-data runtime/memory benchmarking and
extended-attribute reconstruction are not release criteria for this change.

## Out of Scope

- Reopening A/B/C selection without new contradictory evidence.
- Installing the exploratory patch unchanged as production code.
- A new public API, general materialization framework, or rewrite of other
  backends' execution paths.
- Type guarantees for arbitrary ordinary SQL aggregates, arbitrary downstream
  transformations, or new extended R attributes.
- Extending Margin order to arbitrary derived tables or caller-modified storage.
- Repairing all dbplyr raw-SQL ordering or qualified-index behavior upstream.
- A complete execution-SQL audit service or a new streaming audit interface.
- Promising self-overwrite success, crash tolerance, or measured performance
  improvements beyond removing the explicit full-result intermediate copy.
- Implementing #664 inside the SQLite replacement.
- Closing implementation Issues or opening implementation pull requests merely
  because this specification or its evidence branch has been published.

## Further Notes

### Existing Issue disposition

| Issue | Disposition |
|---|---|
| #661 | Highest priority; retain destination correctness, input safety, and supported overwrite/index/analyze criteria; add unsafe bare-name/identifier/operation-identity coverage. Clarify the existing upstream qualified-index limitation and distinguish explicit temp from other qualified temporary destinations. An interim direct-compute stop is a separate safety measure and does not close the Issue. |
| #662 | Retain caller ownership, atomicity and recovery criteria. Refine the broad dbplyr flag-equivalence criterion: TRUE/FALSE are both accepted and retain the owned atomic savepoint without nested BEGIN; FALSE does not promise absence of transaction-control SQL. |
| #663 | Retain all criteria. Downstream success is required; downstream Margin order is not newly promised. |
| #664 | Independent ordinary data.table repair, with unchanged acceptance criteria. It need not wait for this SQLite implementation. |
| #665 | Retain all criteria; declare expansion identifiers without synthetic rows or partitions. |
| #666 | Retain all criteria; apply known declarations to all-missing prefixes without widening aggregate type promises. |

The shared materializer should have one implementation owner for #661–#663.
Identifier declaration and share-type repair remain separate verifiable
requirements even if one change implements both. Existing Issues are reused;
this specification does not create six replacement tickets. Coordination order
is not automatically a technical blocking dependency.

### Delivery sequence

1. If the complete safe replacement is not ready, contain #661 by refusing
   dedicated direct compute before any mutation. Preserve direct collect,
   compute outside that dedicated path, input and other schemas, and caller
   work. Do not recommend stripping the result class as a workaround: it also
   discards the type/order boundary. Lift containment only when the replacement
   passes the destination-safety and transaction matrix; containment alone does
   not close #661.
2. Establish public regressions and independent DBI state checks before changing
   the shared materializer. Implement declaration registration (#665) and
   all-missing application (#666) as separately verifiable requirements.
3. Replace the shared materializer for #661–#663 under one implementation owner,
   preserving direct collection and the type/order tests. Remove the obsolete
   full-result stage and its cleanup responsibilities once all acceptance
   behavior is covered.
4. Reconcile the related ADRs and public documentation, run the required gates,
   publish the implementation for review, and close each existing Issue only
   against its own satisfied criteria. #664 proceeds independently throughout.

The dated review measured common results of baseline 62/98, A 98/98,
B-stage 98/98, B-direct 98/98, and C 90/98. A still had an independently observed
unsafe bare-destination case; C lost all-missing materialized source types and
direct materialized order. B-direct additionally passed 77 public cases,
27 destination-safety cases, 64 existing tests/990 assertions, and SQLite audit
comparisons across 16 fresh processes/40 checks. These are scoped prototype
measurements, not a claim of completed release validation.

Base repository snapshot:
[6a5f611](https://github.com/sayuks/marginplyr/commit/6a5f611d41bd462f3c933edea1dd0b9861d6e892).
Dependency versions: R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0, DBI 1.3.0,
RSQLite/SQLite 3.53.3.

Permanent evidence:
[archive index](https://github.com/sayuks/marginplyr/tree/e6046acd2933d943aba71b81630287f8599262b1/investigation/sqlite-b-direct-2026-09-25),
[dated review](https://github.com/sayuks/marginplyr/blob/e6046acd2933d943aba71b81630287f8599262b1/investigation/sqlite-b-direct-2026-09-25/review/README.md),
[final B-direct patch](https://github.com/sayuks/marginplyr/blob/e6046acd2933d943aba71b81630287f8599262b1/investigation/sqlite-b-direct-2026-09-25/review/B/B-direct.patch).
The archive preserves original evidence bytes and provides separate portable
replay instructions; initial build scripts are not the final prototype source.
