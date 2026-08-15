# Centralize Grouping specification kind rules

Grouping specification kinds are interpreted during admission, structural
preflight, nesting validation, and expansion. We will make one private
strategy registry in the Grouping plan module authoritative for those
semantics. Each kind rule will identify its public constructor and provide
behavior for empty-argument validation, nested-specification validation, and
expansion. This removes the repeated kind lists and conditions identified in
Issue #14 without turning them into a table of flags and error strings.

## Decision

The registry will be keyed by the existing internal kind values `set`, `sets`,
`rollup`, `cube`, and `product`. Its order will preserve the current public
constructor order. Valid kinds and the constructor names recognized while
examining nested arguments will be derived from this registry rather than
maintained as separate vectors.

Every rule will own:

- the corresponding public constructor name;
- `validate_empty()` behavior;
- `validate_nested()` behavior; and
- `expand()` behavior.

The common traversal will validate the specification class and basic shape,
look up its kind rule, and invoke that behavior without switching on the kind.
It will recursively validate a nested specification before asking the parent
rule to validate the parent-child relationship. Existing error helpers and
kind-specific validators will continue to construct errors; the registry will
not become a catalog of Boolean permissions or message strings.

The rules express the existing grammar:

- `set` accepts column selections only and allows an empty argument list to
  represent the empty grouping set;
- `sets` accepts column selections or any valid nested kind, combines their
  families by union, and rejects an empty argument list;
- `rollup` and `cube` accept column selections or a non-empty `set` as a
  composite dimension, reject every other nested kind, and require at least
  one resolved dimension;
- `product` accepts column selections or any valid nested kind, combines their
  families by Cartesian product, and treats an empty argument list as its
  identity.

Structural preflight and selection-dependent validation remain separate.
Syntactically empty `sets`, `rollup`, and `cube` nodes are rejected before
metadata acquisition. A selection that resolves to no rollup or cube
dimensions, and a composite `set` that resolves to no columns, continue to be
rejected during selection resolution. The distinction preserves error
messages, detection order, and the rule that locally decidable errors precede
typed backend metadata reads.

The registry is a compilation-only detail. It, the source specification, and
its rules will not be stored in the Grouping plan. This decision does not
change the Grouping plan class or fields, make the plan opaque, or change how
executors and backend adapters consume it. A later opaque-plan change may add
a private constructor and accessors without changing this specification
grammar.

## Change targets

A later implementation of this decision should be confined primarily to
`R/grouping-plan.R`: introduce the private registry and rule lookup, route
early and nesting validation through the selected rule, derive nested
constructor recognition from the registry, and route family expansion through
the rule's expansion behavior. Existing expansion algorithms and error
helpers should remain focused functions and may be shared by multiple rules.

`R/grouping-spec.R` remains responsible for public constructors and quosure
capture. `R/margin-operation.R` remains responsible for lifecycle timing,
backend metadata acquisition order, and propagation of the public call.
Grouping executors and backend adapters continue to receive the same expanded
plan and require no rule awareness.

## Compatibility constraints

The implementation must not change:

- exported functions, arguments, syntax, or returned specification classes;
- grouping-set membership, ordering, duplicate handling, or Grouping
  identifiers;
- error condition classes, complete messages, public call contexts, or
  detection order;
- quosure environments or the number and timing of evaluations without a
  separately accepted decision;
- local, dtplyr, Arrow, or database behavior;
- metadata acquisition counts, laziness, SQL semantics, or backend dispatch;
  or
- the Grouping plan representation or its consumers.

## Test strategy

Before replacing the branches, add characterization coverage for:

- the five parent kinds against the five nested kinds, plus ordinary column
  selections;
- syntactically empty arguments for every kind;
- dimensions and composite sets that become empty only after selection
  resolution;
- child errors that must precede parent nesting errors;
- exact condition class, complete message, and public call context;
- grouping-set expansion and order for singleton, union, rollup, cube, and
  Cartesian-product behavior; and
- lazy inputs proving that locally detectable errors occur without a typed
  metadata read.

These tests should assert observable compiler and public-verb behavior, not
registry fields or helper layout. The existing local, operation-lifecycle,
backend, laziness, and SQL tests remain the integration gate. SQL assertions
should continue to test semantic query shape rather than byte-for-byte
formatting.

## Considered options

A flat rule table or parent-child permission matrix was rejected because
flags cannot express phase-sensitive empty rules, kind-specific errors, and
child-before-parent detection without recreating a switch in the interpreter.
A message catalog in that table would further separate errors from the
behavior that gives them meaning.

Kind-specific validators without expansion dispatch were rejected because
the authoritative kind list and dispatch would remain duplicated in the
expansion branch.

Kind-specific S3 subclasses were rejected because they would make class,
inheritance, printing, and serialized specification objects observably
different while merely moving the dispatch into `UseMethod()`.

A normalized or validated specification tree was deferred because caching
nested interpretation can change quosure or symbol evaluation counts. It is
not required to centralize the rules and would broaden the compatibility
surface of this change.

A separate rules file was rejected for now because the rules do not form an
independent module; they are the grammar used exclusively by Grouping plan
compilation. Making the Grouping plan opaque at the same time was also
deferred so rule consolidation is not mixed with changes to executor,
adapter, and test access to plan fields.

## Related decisions

The constructor names this registry derives are also the spellings that gate
evaluation of a nested specification argument. ADR 0019 registers every
statically recognized spelling in one place and derives the constructor family
from this table rather than restating it, and records why a constructor is not
a Contextual helper even though its spelling is read before anything runs.
