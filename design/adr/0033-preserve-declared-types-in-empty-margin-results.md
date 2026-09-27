---
status: accepted
---

# Preserve declared types in empty Margin results

The maintainer accepted this decision on 2026-09-28 for #706. Implementation of
the Grouping-helper extension is pending. A Margin result keeps the types of
the columns whose types marginplyr determines, including when it contains zero
rows. Existing Grouping set identifier and share guarantees are retained, and
direct Grouping-helper outputs join that boundary.

## Decision

| Output owned by marginplyr | Type promise, including zero rows |
| --- | --- |
| Grouping set identifier requested through `.id` | R integer |
| Parent share or Total share | R double, including nonempty all-missing results |
| Direct Grouping bit or Grouping identifier output | R integer for local frames and dtplyr; a numeric backend representation for remote results |

Remote helper representations may differ between integer and double and between
native and portable paths. The package preserves the bit and mask values and
does not normalize every backend to R integer. Zero rows do not permit a known
numeric helper output to become logical.

A direct helper output is a summary whose entire result expression is the
Contextual helper call, or an output of a supported literal `across()` lambda
whose body is that expression. Redundant parentheses and a block containing
only that expression are transparent. Recognition follows ADR 0019's static
spelling rule. An enclosing conversion, arithmetic expression, local evaluation,
or caller function retains its own semantics and is not assigned a numeric type
merely because it contains a helper. Arbitrary summary outputs, including
ordinary `dplyr::n()`, remain delegated.

The promise applies to the Margin verb's returned local result and, for a lazy
input, its direct collection and supported direct materialization. It includes
finite collection returning zero rows. Later ordinary lazy verbs retain their
backend's semantics; they do not gain this direct-result guarantee. An empty
input that produces a Grand total row is a nonempty result and must keep its
values, cardinality, and types.

This is the property-ownership boundary of ADR 0016, using ADR 0031's existing
SQLite collection and materialization responsibilities. Source-column anchors,
Margin order, public projection, and materialization safety remain in force.
The extension adds no input-reading exemption to ADR 0020, no synthetic result
row, and no general type inference for user expressions. The
[implementation specification](../specs/zero-row-declared-types.md) owns the
acceptance cases and remaining implementation work.

## Why retain the promise

An empty numeric column collected as logical can establish a Boolean storage
schema. The [dated investigation](../../investigation/zero-row-type-policy-2026-09-28.md)
records a conditional Parquet example in which that schema changes later
populated identifiers. Keeping the declared type lets callers consume the
package-created column without a special empty-result repair.

Withdrawal also leaves the shared SQLite type machinery necessary for other
promised behavior. Much of the measured test reduction was available while
retaining the guarantees. The accepted trade-off gives the package responsibility
for the known output type, including the additional helper-provenance work.

## Considered options

- **Withdraw all zero-row promises.** Rejected: it weakens existing contracts
  and moves schema repair to callers without removing the shared materializer.
- **Keep `.id` and shares but leave all remote helper types delegated.**
  Rejected: a direct helper's numeric meaning is equally determined by the
  package, and its empty schema has the same downstream consequences.
- **Infer every expression's output type or normalize all remote numbers.**
  Rejected: enclosing expressions and backend numeric representations have
  owners other than marginplyr. This would widen both the promise and the work
  beyond the adopted direct-output boundary.

The prototype's implementation is evidence of feasibility, not the selected
production design. Its column-name handling worked in the measured cases, but
its result-class regression and incomplete validation must be resolved before
shipping. Test consolidation is permitted when the existing behavior matrix is
preserved; a line-reduction target is not part of this decision.
