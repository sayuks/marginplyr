# Margin operation architecture

This document describes the contributor-facing architecture implemented by
marginplyr. The vocabulary in [`CONTEXT.md`](../CONTEXT.md) and the decisions
in [`adr/`](adr/) are authoritative.

## Lifecycle

Every exported Margin verb follows the same explicit lifecycle:

1. **Capture and admit.** The public verb captures `.by`, `.grouping`, and,
   for summaries, `...` exactly once. It rejects unsupported input classes and
   invalid verb-specific options before preparing an operation.
2. **Prepare.** `prepare_margin_operation()` validates common options,
   interprets persistent input groups as fixed keys, ungroups the input,
   discovers the backend and column names, validates an optional output
   Grouping set identifier name, acquires one typed metadata snapshot,
   compiles the Grouping specification into a Grouping plan, and derives
   Margin-column metadata. Locally detectable errors are rejected before the
   typed metadata read.
3. **Execute.** The verb passes the prepared Margin operation to exactly one
   of `execute_margin_summary()`, `execute_margin_expand()`, or
   `execute_margin_nest()`. The executor performs its schema-aware preflight,
   calls `validate_margin_operation()` immediately before low-level work, and
   selects or invokes the appropriate adapter.
4. **Finalize.** `finalize_margin_operation()` starts from an ungrouped
   result, restores factor Margin columns, places fixed keys and grouping
   dimensions followed by the optional Grouping set identifier first, and
   leaves row ordering unspecified by default. When `.sort` asks for a Margin
   order, it applies that order last — after restoration, so a factor
   dimension sorts by its restored levels, and after placement, so the
   `ORDER BY` is the outermost one. The key comes from the Grouping plan the
   module already holds, so no verb kind is passed in. See
   [ADR 0018](adr/0018-order-margin-results-by-grouping-structure.md).

`nest_with_margins()` returns the common ungrouped result.
`nest_by_with_margins()` collects that result when necessary, preserves its
special empty-input behavior, and adds row-wise grouping by the visible keys
only after common finalization.

The lifecycle is deliberately explicit:

```text
public verb
  -> prepare_margin_operation()
  -> execute_margin_summary() | execute_margin_expand() | execute_margin_nest()
  -> finalize_margin_operation()
  -> optional nest_by row-wise grouping
```

It is not a callback runner, and the shared module never records or switches
on a verb kind.

## Module responsibilities

### Margin operation (`R/margin-operation.R`)

Owns the shared prepare and finalize lifecycle, common option normalization,
grouped-input normalization, the package-private Margin operation value, the
shared semantic Margin label validation entry point, and propagation of the
public call in Package conditions. Preparation stores only canonical derived
state needed by execution and finalization; a Margin operation is single-use
state for one public call.

### Backend metadata (`R/backend-metadata.R`)

Owns backend-neutral column-name discovery, creation of the typed selection
proxy, column prototypes, and factor metadata. One typed proxy is acquired
during preparation and reused for Grouping plan compilation, summary
selections, prototypes, and factors. Arrow reads schema metadata; backends
that require a zero-row collection do it only at this seam.

Backend classification and capability discovery live separately in
`R/grouping-backend.R`. They describe whether metadata can be read, factors
can be restored, and native grouping sets are supported; they do not own the
Margin operation lifecycle.

### Grouping plan (`R/grouping-plan.R`)

Owns validation and compilation of a Grouping specification into the
backend-independent Grouping plan: fixed keys, resolved dimensions, expanded
grouping sets, their one-based occurrence identifiers, and the duplicate-set
policy. It resolves selections against the prepared metadata but does not read
a backend or execute a Margin operation.

Grouping specification kinds are governed by one private strategy registry in
this module. Each kind rule owns empty-argument validation, validation of
nested specifications, and expansion dispatch. Structural preflight remains
separate from validation that depends on resolved selections, and nested
specifications are validated before their parent-child relationship. The
registry is used only while compiling; neither it nor the source specification
is retained in the resulting Grouping plan.

A Nested specification position reads its argument by spelling, because the
alternative reading is a column selection and evaluating every nested call
would run a selection helper outside a selection context. A specification a
caller's own function returns therefore reaches selection resolution, where
tidyselect refuses it as an unusable subscript, and that one refusal is
replaced by a Package condition naming the recognized forms and the binding
that works. Being marginplyr's own report about a position of its own, it is
parentless, as a share selection naming something ineligible is. The refused
value is read from the condition rather than by evaluating the argument a
second time to identify it, which is what keeps the number and timing of
caller-quosure evaluations fixed. The replacement covers the argument the
position owns and not a part of one: a specification written inside a
selection keeps tidyselect's report, which names the sub-selection and is
accurate about it. Every other selection failure is re-raised as it arrived,
so an External condition still reaches the caller with its own class,
diagnostic, and cause.

### Margin label (`R/margin-label.R`)

Owns label normalization, factor/backend restrictions, optional collision
checks, and applying a label or typed missing value to an omitted dimension
in a portable branch. Preparation normalizes the label and creates the
metadata it needs; semantic validation is delayed until the verb executor has
completed its schema-aware preflight and is about to execute.

### Factor (`R/factor.R`)

Owns restoration of factor and ordered-factor Margin columns from the metadata
captured during preparation. Its backend methods preserve levels and ordering
after labels have been materialized; it does not acquire metadata itself.

This is the only type or attribute restoration in the package, and it exists
because marginplyr is what decomposed those factors. Every other class and
attribute of a result is whatever the underlying dplyr and vctrs operations
produced, and no verb restores one. See
[ADR 0016](adr/0016-delegate-result-class-and-attributes-to-dplyr.md).

### Summary selection (`R/summary-selections.R`)

Owns summary-only semantics: rejecting the removed `.groups` option — along
with the near misses that resemble it — and branch-local
grouping-context helpers, resolving `across()` and `pick()` selections while
excluding every fixed key and grouping dimension, predicting known output
names, and preventing summary outputs from overwriting grouping columns. These
checks occur before semantic label validation, including any opt-in lazy
collision query.

### Static expression reading (`R/utils.R`)

Owns what a summary expression *says*, and decides nothing about it: the name
and namespace a call carries, the head and arguments a walk descends into and
the node rebuilt around them, the expression an argument carries when it
arrived as an injected quosure, which arguments a call captures as language
rather than evaluating, which primitives resolve a name or evaluate language,
and which language object a call is statically known to build.

It also owns the one reading of a redundant pair of parentheses. `(` is the
identity function, so `(f)(x)`, `(f(x))`, and `f(x)` are one call written three
ways, and the readers here answer the three alike — which is what gives every
analysis above that property at once instead of each family recognizing
whichever forms someone wrote out (#178, ADR 0019). Name and operands are read
through the same unwrapping, so a node cannot be named as its content and
subscripted as its wrapper.

The unwrapping is syntactic and stops there: it reads a name that is written,
never one that would have to be looked up. So `(get("f"))(x)`, `(function(x)
x)(1)`, and `("f")(1)` carry no name for a spelling to be recognized by, under
the conservative #130 policy. What this module does with an unnamed head is a
separate question it answers separately — `static_callee_name()` resolves
`get("f")` and its siblings to the primitive they name, because the share
dependency walk has to over-report a lookup it cannot see through rather than
recognize a helper by it.

Saying what a part is written as belongs here for the same reason as reading
it. `call_part_label()` writes one back into a diagnostic and
`injected_quosure_clause()` says that an argument arrived by injection and what
it carries (#169) — both report what the expression says and neither
decides anything about it, since which message composes the clause, and whether
to refuse at all, stays with the analysis that asked. Two refusals in different
modules describe the same written form, so the words for it are one module's
rather than each caller's; `rlang::as_label()` reading `.data$region` as
`region` is the kind of thing that would otherwise be got right in one of them
and not the other.

Four analyses read through it and each decides for itself — the share
dependency walk, the two rewrites, and the three searches — so the module is
shallow by design where the ones below it are deep. It is also why the
dependency runs one way: a reader that lived in `R/share.R` would make the
grouping-context rewrite reach into the contextual-share module for a fact
that is not about shares (#179).

### Recognized spellings (`R/contextual-helpers.R`)

Owns which names a Margin verb recognizes before anything runs, and decides
nothing about what recognition then does. One table keyed by family carries
each family's spellings, the namespaces a recognized call may be qualified
with, and whether its spellings are Contextual helpers; every site that reads
one of those spellings derives from it, and their namespace test exists only
here (ADR 0019).

The language-capture primitives are read statically too and are deliberately
not among them. `language_capture_formal()` and `captured_call_parts()` above
carry `quote`, `substitute`, and `expression` with a `base` namespace test of
their own, and it is a different test: a capture is refused where the head
names a binding the analysis can see, so it answers to the calling environment
where every family in the registry refuses to.

That difference is what makes the parenthesis reading above split at one point.
A parenthesized head is a recognized spelling here and is *not* a capture there:
`(quote)(share)` evaluates its head as a value, so any binding wins and the walk
reports the read it would otherwise hide, while `(pick)(units)` is dplyr's
`pick()` however the caller has bound the name.

Two families derive their spellings from the module that owns them —
contextual shares from `share_kind_rules()`, Grouping specification
constructors from `grouping_kind_rules()` — so a helper added to either is
recognized without being written down twice. This module therefore reads two
deep modules as well as `R/utils.R`, and both of those two read it back;
stated baldly that is a cycle, and what keeps it from being one in practice is
that every entry in the table is a function, so a family lookup evaluates that
family's owner and no other. `R/utils.R` is not part of it: the dependency on
it runs one way, as it does from everything else.

That is the property worth checking rather than the layering, because the
layering alone would not say which module reaches which. Under it, the only
module that evaluates `share_kind_rules()` through this table is `R/share.R`
itself, asking about the spellings it owns — a module reading its own table
through a shared reader, which is what the section above describes rather than
what it forbids. What it forbids is the grouping-context rewrite reaching into
the contextual-share module for a fact that is not about shares, and a
`grouping_helper_name()` lookup evaluates the grouping family alone. Built
eagerly it would evaluate every family the table holds, which is that reach
arriving one module further out; `test-contextual-helpers.R` asserts it does
not.

### Contextual shares (`R/share.R`)

One deep private module owns every contextual-share responsibility: request
planning, denominator mapping, source validation, ratio calculation,
collision-safe temporary names and their cleanup, and backend adapter
dispatch. Its file is large because the module is deep, not because it is
several modules sharing a file; splitting it would move the seam without
shrinking the interface.

It serves two helpers, `share_of_parent()` and `share_of_total()`, which
differ only in their denominator. Everything else — the source contract, the
`across()` grammar, output naming, the value rules, and the backend
boundaries — is one implementation, which is why the shared machinery is named
`share_*`. Each request carries its *kind*, and `share_kind_rules()` is the one
table describing a kind: the name a caller writes, the terms its diagnostics
use, what it requires of the compiled Grouping plan, and the denominator
mapping it joins. Detection reads that table backwards, from a written name to
its kind, so no message and no branch names a helper independently and a third
helper is one entry with no second site to answer for it. See
[ADR 0017](adr/0017-calculate-total-shares-against-the-grand-total-set.md).

The names that stayed `parent_*` — `parent_set_ids()`,
`check_parent_grouping_spec()`, `check_parent_grouping_kind()`,
`build_lazy_parent_mapping()`, and `add_lazy_parent_join_keys()` — are the
ones that genuinely resolve a *parent* occurrence, and no Total share reaches
any of them.

The exported helpers in this file are only context guards: reaching either
body means the helper was called outside a Margin summary, so it always
raises. The rest of the module is private and is reached through four entry
points, and no other:

- `preflight_shares()`, called from the public verb's admission block
  before preparation, which reports which share kinds the call requests and
  rejects statically impossible forms;
- `plan_share_expressions()`, called by `plan_summary_expressions()`
  in the summary-selection module, which rewrites the captured summary
  expressions into ordinary summaries plus planned share requests;
- `wrap_share_sources()`, called from the same place immediately afterwards,
  which wraps each referenced source summary in the validator its backend can
  execute; and
- `execute_shares()`, called by `execute_margin_summary()`, which
  receives the prepared Margin operation, the staged ordinary-summary result,
  all planned requests, and the verb's `.check_share_source`. It needs the
  argument because whether a backend can establish an eligible source is
  settled here, and a backend that cannot is refused rather than read.

Planning and wrapping are two calls rather than one because the summary
selections have to be resolved between them: the plan names which summaries a
share depends on, resolution turns `across()` into the columns it expands to,
and only then can the validator be wrapped around the right expressions.

`share_grouping_spec_validator()` is additionally passed to
`prepare_margin_operation()` as a validation hook, so a Parent share's
rollup-only restriction is checked with the rest of grouping validation
instead of after it. It answers `NULL` unless a Parent share was requested:
whether a plan contains the Grand total set is a property of the compiled
plan, not of the specification, so a Total share's requirement is checked
against the plan by `check_share_grouping_kinds()`.

The responsibilities divide as follows:

- **Planning** analyzes the ordinary summaries preceding each request,
  retains candidate-name provenance for duplicate, expanded, ineligible, and
  unknown sources, validates direct and `across()` grammar, resolves
  name-based tidyselect against preceding ordinary summaries only, and checks
  output-name collisions against fixed keys, dimensions, ordinary summaries,
  the Grouping set identifier, and other contextual shares.
- **Validation** applies one eligible-type rule to every backend, and only
  where its answer comes from is a backend question — never from a row of the
  caller's data, which is ADR 0020. `share_source_checker()` chooses among
  three answers, once above adapter selection. A materialized result carries
  its summaries' own types, so `check_share_source_types()` reads them off it.
  Local and dtplyr operations additionally wrap the referenced source
  summaries in a type-and-cardinality validator inside the ordinary summary
  itself, so validation costs no extra pass over the input and no
  validation-only query, and cardinality stays theirs alone — a SQL aggregate
  returns one value per grouping row by construction. A database applies the
  rule itself and reports an ineligible summary at collection, unless its
  dialect converts a value of another type to a number instead of refusing it,
  in which case nothing applies the rule and the share is refused;
  `share_dialect_verdict()` decides which, once per dialect, with at most two
  queries referencing none of the caller's tables — a probe, and a control
  sent only when the probe is rejected, so that a dialect which refuses is
  told apart from one whose scaffolding or connection failed — and
  `.check_share_source = FALSE` calculates the share anyway. Arrow is rejected
  before any of this.
- **Mapping** is the one responsibility a kind supplies for itself, through
  `share_denominator_rule()`: which occurrence each row's denominator comes
  from, and the denominator rows with the columns they are matched on. A
  Parent share derives each grouping set's parent with `parent_set_ids()`,
  which skips duplicate occurrences while finding the next strictly less
  detailed set, and matches on the internal Grouping set identifier, the fixed
  keys, and one join key per dimension that carries the dimension's value only
  where the parent set includes it and is missing otherwise — computed by the
  same expression on both sides, and never from a displayed Margin label or
  the caller-visible `.id`. A Total share's denominator depends on `.by` and
  nothing else, so its mapping is one read of the Grand total occurrence
  matched on the fixed keys alone, with a constant column standing in when
  there are none.
- **Calculation** builds one shared mapping per requested kind, joins each
  once, and emits every ratio as an explicit double division guarded for a row
  that is its own denominator, a missing numerator, and a missing or zero
  denominator. Each request wrote a placeholder column at its position in the
  caller's summary expressions and the join overwrites that column in place,
  so a call requesting both kinds runs two passes without either pass being
  visible in the result's column order.
- **Cleanup** allocates every temporary — denominators, join keys, and the
  right-hand match names of the SQL join — through
  `new_margin_internal_names()` against the names already in the result, and
  drops all of them before returning.

Adapter selection is a lookup on the prepared backend kind, not on the staged
result's class, and every adapter takes the same five arguments. See
[ADR 0014](adr/0014-select-parent-share-adapters-from-prepared-backend-kind.md).
The mapping, calculation, and cleanup above are shared; there are three
adapters, and each says only what its backends do differently:

| Adapter | Backend kinds | Difference from the shared work |
|---|---|---|
| Local | `local` | Checks materialized source types first |
| General dbplyr | `duckdb`, `postgres`, `sql` | Missing-safe `sql_on` join |
| Lazy non-SQL | `dtplyr`, `other` | Nothing |

The lazy non-SQL adapter adding nothing is the point rather than an
oversight: it names the contract that a lazy non-SQL backend joins exactly as
local data does but cannot have its source types checked first, because
nothing is materialized to check. Its validation happens earlier, inside the
ordinary summary. Collapsing it into the local adapter would make that
difference invisible at the seam that has to honour it.

Arrow is rejected at the immediately earlier executor boundary because no
ordinary-summary query may be staged for a valid Arrow contextual-share
request. The rejection runs after request planning and common Margin-operation
validation, uses only the operation's prepared backend kind, and adds no
wrapper, hook, sentinel, or extension seam. Its reason is the numerator's
source summary, which both helpers share, so it names whichever helpers the
caller wrote rather than a fixed one. Other Arrow Margin operations continue
through the ordinary summary, expansion, and nesting paths.

### Conditions (`R/conditions.R`)

`abort_marginplyr()` is the only constructor for a Package condition. A
Package condition is raised exactly when the caller can avoid it by rewriting
the call within the documented public interface; unreachable invariants and
upstream defects use bare `stop()` or `stopifnot()`, and an External condition
propagates with its class, diagnostic, and cause untouched. `marginplyr_error`
is the only promised class. See
[ADR 0015](adr/0015-separate-package-conditions-from-internal-invariants.md).

The rule is not mechanically enforced, so both directions of the boundary are
review surface: a Package condition demoted to `stop()` silently leaves the
public contract, and an invariant promoted to `abort_marginplyr()` silently
enters it.

It also owns the one reading of a condition another package raised.
`condition_chain()` answers the conditions a `parent` chain holds, outermost
first, and decides nothing about them. tidyselect wraps a failure raised inside
a selection helper, so the two refusals that read such a failure — the
contextual-share `across()` diagnostic and the Nested specification one —
cannot count on the condition they caught carrying what they need: a bare
subscript is refused at the top of the chain and one inside `all_of()` a layer
below it. Each walked the chain for itself until this named it once (#193).
Each keeps its own question of the answer — one collects the refused character
subscripts, the other tests a class against an argument's label — because the
traversal is the only part they share.

The rest of the module owns the Condition context around an External condition
one grouping-set branch raises. `with_branch_conditions()` restates the
grouping values dplyr reports under the branch's internal key columns, and the
blamed call, in the names the caller wrote; it withholds a branch warning as it
is raised so that `report_branch_warnings()` can report a Repeated condition
once, with a count of the further grouping sets that raised it. Only the
portable adapter uses it, because it is the only path that evaluates a caller's
summary expression more than once. See
[ADR 0021](adr/0021-report-a-repeated-execution-condition-once.md).

### Native adapter (`R/grouping-adapter-native.R`)

Owns the dbplyr `GROUPING SETS` summary path. It rewrites grouping helpers for
SQL, reserves collision-free display flags, attaches the grouping sets to an
isolated lazy-query node, and renders that node for confirmed native dialects.
It receives the compiled plan and executor-prepared inputs; it does not
prepare, validate, or finalize a Margin operation.

### Portable adapter (`R/grouping-adapter-union.R`)

Owns branch materialization and `UNION ALL` composition. The summary path
summarizes each grouping set, checks dynamic names, restores visible grouping
keys, and labels omitted dimensions. Expansion emits one labelled input branch
per grouping set; nesting builds on that expansion in its verb executor. Like
the native adapter, it consumes derived inputs and does not own the lifecycle.

Because it is the one path that summarizes the caller's own expressions once
per grouping set, it is also the one that owes a Condition context: it wraps
the branch summary alone in `with_branch_conditions()`, so that the checks and
builders around it keep raising their Package conditions unchanged.

`combine_margin_branches()` is the one place the package combines a branch
list, and the contextual-share module calls it for its denominator mappings
rather than folding its own. It chooses its strategy from the branches, not
from the operation's backend: an eager list is combined in a single
`bind_rows()` behind an explicit column check that keeps the strictness
`union_all()` supplied, and a lazy one is paired and halved. That makes the
eager path one pass over the branches and the lazy path `O(n log n)` with
nesting depth `log2(n)`, in place of the quadratic pairwise fold both used to
take. It matters because a cube over ten dimensions is 1024 branches and the
local backend has no native grouping-sets capability to avoid them.

## Opaque Margin operation seam

The class name, fields, and constructor of a prepared Margin operation are
package-private and are not compatibility contracts. Public verb bodies and
tests pass the value as a whole. They must not inspect it, cache it, return it,
or construct it independently.

Direct field reads are confined to:

- the Margin operation module, for validation and finalization;
- the three dedicated verb executors, for their schema-aware preflight and
  calls into low-level adapters;
- `execute_shares()`, which reads the prepared backend kind to select an
  adapter and a source checker, and the caller-visible Grouping set identifier
  name to restore it after the join;
- the contextual-share source checkers, which read the prepared backend — its
  kind, for the one that asserts it may check nothing, and its dialect, for
  the one that asks whether that dialect converts — and the input it prepared;
  and
- the two contextual-share adapters, which read the Grouping plan and
  nothing else.

The native and portable Grouping adapters receive the specific derived values
they need, not the Margin operation itself. The contextual-share adapters and
source checkers take it whole because they are dispatch targets of one
signature rather than independent entry points. This boundary lets the
operation, Grouping plan, and backend representations change without spreading
field access through public verbs or adapters.

## Test seams

The primary compatibility seam is the four exported verbs:
`summarize_with_margins()`, `expand_with_margins()`,
`nest_with_margins()`, and `nest_by_with_margins()`. Tests should observe
rows, columns, types, grouping, errors, laziness, and SQL semantics through
those interfaces. They must not assert the Margin operation class, fields,
constructor shape, or internal helper order.

The test suite divides supporting contracts as follows:

- `test-grouping-interface.R` covers shared local behavior through the public
  verbs, including Grouping plans, labels, factors, duplicate policies,
  persistent groups, result grouping, summary selections, nesting, and the
  documented `marginplyr_error` handler.
- `test-margin-id.R` covers Grouping set occurrence identifiers across all
  public verbs, including local, native, portable, duplicate, nesting,
  collision, missing-value, zero-row, and laziness semantics.
- `test-margin-order.R` covers `.sort` across all four public verbs per ADR
  0018: the key and what follows from it, `"first"` reversing the Grouping
  bits alone, factor level order, missing values last, fixed-key contiguity,
  composite dimensions, duplicate occurrences, the `"none"` default, and the
  vocabulary error. It asserts rows rather than the key builder, because the
  ADR leaves each adapter to resolve the key in what its own query can name;
  its two rendered-SQL tests carry only what rows cannot show.
- `test-margin-label.R` covers Margin labels through the public verbs: named
  per-dimension labels, the eight-case factor NA contract of ADR 0012, label
  placement, collisions including unused factor levels, and typed missing
  values on dtplyr, Arrow, portable SQL, and DuckDB.
- `test-summarize-operation.R`, `test-expand-operation.R`, and
  `test-nest-operation.R` cover lifecycle ordering and the single typed
  metadata snapshot through public calls. `test-nest-operation.R` also covers
  the nesting duplicate-drop policy and quosure environments of both nesting
  verbs.
- `test-share.R` covers contextual-share semantics through
  `summarize_with_margins()`: rollup levels, typed grouping identity against
  the default Margin label, missing keys separated from displayed margins,
  the direct and `across()` grammar, source type and cardinality, output-name
  collisions, dependency provenance, single evaluation of captured
  expressions, and error precedence. Its Total-share tests state only what is
  the second denominator's own — which plans it accepts, interchangeable
  duplicate Grand total occurrences, the fixed-key stand-in used when `.by` is
  empty, and that each diagnostic names the helper the caller wrote.
- `test-grouping-backends.R` covers Arrow and dtplyr metadata behavior,
  native and portable SQL strategy, lazy query composition, collision checks,
  internal-name safety, and live DuckDB equivalence.
- `test-share-backends.R` covers contextual-share adapter behavior for both
  denominators, including targeted pre-query Arrow rejection, dtplyr
  execution-time validation, lazy SQL composition, live SQLite portable
  execution, and live DuckDB native-versus-portable results.
- `test-inspect-grouping.R` covers `inspect_grouping()` as an ordinary tibble
  per ADR 0013, including both formats, plan order, and that a lazy input is
  inspected without executing a Margin operation.
- `test-execution-conditions.R` covers the Condition context of ADR 0021
  through `summarize_with_margins()`: a warning every grouping set raises
  reported once with its count and under the caller's own column names,
  branches raising different diagnostics reported one by one, the caller's
  columns and verb in an error's context, the propagated class and cause, ten
  keys substituted without corruption, a Package condition raised beside them
  left alone, a warning a branch raised surviving a later branch's error, and
  the lazy non-goal. Two of its cases hold the identity to what
  a message says rather than to how it was laid out or what a value happens to
  contain: one runs at five console widths, because cli wraps a bullet it
  cannot fit, and one uses a grouping value and a caller diagnostic that carry
  a bullet marker of their own. Its two snapshots carry the rendered messages,
  because the identity is derived from rendered text and no structural
  assertion would see dplyr reword it. Its one test of an internal helper is
  the documented exception: the class half of a Repeated condition's identity,
  and an empty message, are both unreachable through a verb, because dplyr
  aggregates a branch's warnings into one condition of its own before
  signalling. It also covers the shared condition-chain reader, which is the
  module's and is reached without a verb: a chain arrives from a caller's own
  selection failure, so the tests build one and take the two tidyselect shapes
  from tidyselect itself. Neither consumer's diagnostic would report a
  traversal that changed shape, which is why the reader is not asserted through
  them alone.
- `test-get-col-names.R` and `test-factor.R` cover the focused metadata and
  factor backend contracts.
- `test-grouping-plan.R` covers the backend-independent Grouping
  specification compiler directly, including the complete kind-nesting
  grammar, phase-sensitive empty rules, error precedence, and expansion order.
- `test-documentation.R` covers the reference documentation's own contracts:
  that an italicised section cross-reference resolves in the topic it
  promises, that every user-facing topic offers related-topic links, and that
  the Grouping-identity comparison has exactly one canonical home.
- `test-utils.R` and `test-retail-sales.R` cover the shared assertion helpers
  and the documented properties of the bundled dataset.

Package conditions are not tested in one file. Each module's tests assert the
`marginplyr_error` class next to the behavior that raises it — the "use the
package condition seam" tests in `test-grouping-interface.R`,
`test-margin-label.R`, `test-summarize-operation.R`, `test-nest-operation.R`,
`test-inspect-grouping.R`, and `test-utils.R` — while the matching
"retain their class and cause" and "preserve user-expression conditions" tests
assert that an External condition keeps its original class. Keeping the two
halves adjacent is what makes the boundary in ADR 0015 reviewable.

Backend tests may instrument a backend seam or inspect semantic query shape,
but should not couple to the Margin operation representation or require
byte-for-byte SQL formatting.

### Release gates

One property of this suite is not observable from any single run of it: an
optional-backend test that skipped and one that passed look the same in a
green job. A backend contract therefore has a second seam, in
`.github/workflows/release-matrix.yaml`, where a dedicated job installs that
backend alone and fails if the contract did not execute.

What this asks of a test is that it never decide on its own whether it may
skip. A test behind an optional package routes through
`skip_if_suggest_absent()` or `suggest_available()` in
`tests/testthat/helper-optional-backends.R` — never `skip_if_not_installed()`
or `rlang::is_installed()` — because only the helper can be told, through
`MARGINPLYR_REQUIRED_SUGGESTS`, that this job promised to prove that package
and an absence is a failure. `test-optional-backends.R` covers the helper
itself. Snapshot expectations belong to the same seam: testthat skips them
under CRAN semantics, so they run only in those jobs.

A test title is part of that contract, because each job names the titles it
exists to execute. Renaming a contract test is expected to break its job.
`AGENTS.md` is the operational reference for the jobs, the verifier scripts,
and the sites an added backend has to touch.

## Extending backend support

The public verbs are intentionally not S3 generics, and the private adapters
are not a third-party extension API. Backend support is added inside
marginplyr so that all backends continue to share one Grouping plan and one
prepare-execute-finalize lifecycle.

A backend change should:

1. classify the backend and declare its capabilities in
   `R/grouping-backend.R`;
2. provide typed selection metadata through the single
   `grouping_selection_proxy()` seam without adding reads in a public verb or
   executor;
3. reuse the existing portable adapter unless native `GROUPING SETS` support
   is confirmed and covered by the native adapter contract;
4. name the new backend kind in `share_adapter()` and in
   `share_source_checker()`, choosing one of the existing contextual-share
   adapters and one of the existing source checkers, or adding one. Neither
   lookup has a default: an unnamed kind stops the operation rather than
   falling through to a plausible-looking join. Answer
   `wraps_share_sources_in_summary()` for it as well — it is the third
   kind-keyed decision and the only one that cannot stop, since a kind it
   answers `FALSE` for is simply a kind whose ordinary summaries do not
   evaluate R code and therefore cannot carry the cardinality rule inside
   themselves;
5. keep label rules, factor restoration, summary selection, and finalization
   in their owning modules rather than duplicating them in the adapter;
6. add contract coverage for metadata acquisition, observable results,
   laziness, SQL strategy and composition where applicable, identifier and
   label quoting, and native-versus-portable equivalence when both paths
   exist; and
7. wire the new coverage into the release gates, per `AGENTS.md`, so its
   contracts cannot pass by skipping.

The backend capability description selects between already prepared execution
strategies. It does not replace the Margin operation, specialize the Grouping
plan, or bypass common finalization.

## Repository placement

This file, `CONTEXT.md`, the ADRs, and
[`review-dispositions.md`](review-dispositions.md) are human-maintained
contributor material. They are separate from the generated package site in
`docs/`; `^CONTEXT\.md$` and `^design$` in `.Rbuildignore` exclude them from
source packages. Internal architecture is not duplicated into the README or
vignettes.

`review-dispositions.md` is the ledger that gives "review complete" an
auditable meaning: every observation from a recorded review is listed there as
fixed or rejected, with a test, command, or reproduction behind it. An ADR
records what was decided; the ledger records what was reviewed and what
happened to it.
