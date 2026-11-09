---
status: accepted
---

# Grouping caller context and lazy factor-schema boundaries

Accepted: 2026-11-09
Implementation: pending in #1550 and #1551

The owner approved this specification for two independent implementation
tickets: context consistency, ordinary rlang injection semantics, prevention
of silent factor-value loss, conservative typed deferral, and reconstruction
of a lazy result after schema changes. No package implementation is included.

Evidence baseline: `5835fd6788d0fddd4cf42a2aa524a62072f36814`.
The [independent investigation](https://github.com/sayuks/marginplyr/blob/54f6c55ad43dc5b8eb4483a91b7dda4e775eab05/investigation/expand-grouping-independent-hunt-2026-11-09.md)
records the original observations. Later isolated R controls also reproduced
the injection difference using ordinary `rlang::enquo()` without marginplyr,
and distinguished caller mutation from dtplyr's `immutable = TRUE` promise.

## 1. One context rule for recognized constructor-shaped caller calls

[ADR 0008's common caller-context amendment](../adr/0008-centralize-grouping-specification-kind-rules.md#amendment-one-context-rule-for-recognized-constructor-shaped-caller-calls)
owns the evaluation, proof, prefix/resume and compatibility rules. The controls
below establish that accepted behavior without widening the spelling gate or
granting another source-reading exemption.

Acceptance:

- Cover all five constructor spellings and their existing admitted parent
  positions. Both specification-valued and ordinary-selection-valued returns
  obey the existing grammar and first-answer rule.
- Bare, `::`, `:::`, redundant head/whole-call parentheses and supported
  quosure/dot/embrace forwarding agree under equivalent captured environments.
  Preserve caller-authored namespace operations and injection boundaries.
- Local context exposes actual columns; supported lazy context exposes faithful
  available metadata under its documented schema/zero-row selection contract,
  rather than source values or unrecorded factor levels. Unknown calls on
  unsupported SQL contexts or unsafe dtplyr/DuckDB
  sources refuse before caller invocation, without processing source rows.
- Preserve occurrence/call independence, one invocation, warning/error
  identity, captured scope, caller bindings and source/specification objects.
- Assert prefix errors and the specified metadata/callback/typed-`.by` order;
  do not claim unchanged detection order for the deliberately deferred calls.
- Keep stock and proven name-only controls free of new metadata queries.
  Test capture operands and active/deferred binding controls too.
- Do not newly recognize foreign qualifiers, aliases or computed heads as
  nested constructors. Retain ordinary selection and top-level `.grouping`
  semantics.
- Exercise `inspect_grouping()` and the four Margin verbs using the shared
  consumer inventory; use deterministic counters and faithful-context
  negative controls, not only successful plan shapes.

## 2. Public injection follows its written evaluation boundary

The `do.call`/`alist` empty-splice observation is excluded from repair work
under [ADR 0007's public-injection boundary](../adr/0007-capture-user-expressions-at-public-verbs.md#amendment-public-injection-keeps-its-written-evaluation-boundary).
[The dated investigation's revisions](../../investigation/expand-grouping-independent-hunt-2026-11-09.md#revisions-2026-11-09) record the
ordinary-rlang control and the explicit decision to make no implementation
change for this observation.

## 3. Construction-time schema and safe factor processing

[ADR 0002's schema-lifetime amendment](../adr/0002-acquire-typed-metadata-once.md#amendment-a-lazy-result-retains-its-construction-time-schema-2026-11-09)
owns the schema precondition and reconstruction responsibility.
[ADR 0012's changed-factor-definition amendment](../adr/0012-distinguish-factor-na-levels-from-missing-margin-values.md#amendment-reject-changed-captured-factor-definitions-before-loss-2026-11-09)
owns the comparison, mapping failure, existing-interface and backend boundaries.
The following acceptance includes the native-factor route that an
encoder-only check would miss.

Acceptance:

- Reproduce the used-new-level loss on a reused dtplyr expansion. Reject the
  changed definition instead of returning a new `NA`; rebuilding the query
  against the changed input succeeds when otherwise valid.
- Cover level addition/removal/reordering, unused levels, ordered status,
  addition of an NA level, and a level matching an internal sentinel or the
  Margin label. Compare before encoding, so sentinel collisions and an NA
  level cannot hide a changed definition.
- Cover text labels, NULL labels with and without a captured NA level, the
  existing refusal of NA labels against a declared NA level, fixed keys and
  carried factor payloads. Native-factor paths with newly added NA levels
  must not evade validation merely because the old snapshot chose no encoder.
- Preserve exact valid factor values/codes/levels/order, real missing codes,
  NA-level observations and existing subclass-display behavior. Same-schema
  value changes remain valid. Return unchanged values and attributes from
  the validating step.
- Include empty inputs, input filtered to zero rows, duplicate occurrences,
  internal-looking column names and repeated collection. Verify that an empty
  execution cannot bypass the required factor-definition comparison.
- Exercise shared summary and nesting paths with controls proving that ordinary
  grouping/folding does not cause false mismatches. This includes the factor
  payload restored before folding and the row-wise nesting wrapper.
- Confirm construction and plan display read no additional source values;
  validation adds no query or extra source evaluation during execution. Keep
  caller inputs unchanged and recoverable after a rejected execution.
- Preserve SQL/Arrow contracts and DuckDB's ordinary ENUM cast behavior with
  focused controls. Do not advertise cross-backend schema-change detection.

## Documentation and independent delivery

The semantic owners are ADR 0008 for caller evaluation, ADR 0007 for public
capture, ADR 0002 for schema lifetime, and ADR 0012 for factor encoding and
restoration. Amend them and reconcile their direct references without adding
to the always-loaded context. Public help and release notes describing changed
runtime behavior land with the corresponding implementation.

Implementation tickets, with no blocking edge between them:

1. [#1550](https://github.com/sayuks/marginplyr/issues/1550), higher priority:
   refuse changed captured factor definitions before R branch processing loses
   source values.
2. [#1551](https://github.com/sayuks/marginplyr/issues/1551): give recognized
   constructor-shaped caller calls a faithful selection context under one
   conservative evaluation rule.

The approved submission contains contract documentation and publication of
these two independently verifiable tickets. Package fixes remain separate work;
the original investigation's no-implementation scope continues.
