# Margin operation architecture

This document describes the contributor-facing architecture implemented by
marginplyr. The vocabulary in [`CONTEXT.md`](../CONTEXT.md) and the decisions
in [`adr/`](adr/) are authoritative.

## Lifecycle

Every exported margin verb follows the same explicit lifecycle:

1. **Capture and admit.** The public verb captures `.by`, `.grouping`, and,
   for summaries, `...` exactly once. It rejects unsupported input classes and
   invalid verb-specific options before preparing an operation.
2. **Prepare.** `prepare_margin_operation()` validates common options,
   interprets persistent input groups as fixed keys, ungroups the input,
   discovers the backend and column names, validates an optional output
   Grouping set identifier name, acquires one typed metadata snapshot,
   compiles the Grouping specification into a Grouping plan, and derives
   margin-column metadata. Locally detectable errors are rejected before the
   typed metadata read.
3. **Execute.** The verb passes the prepared Margin operation to exactly one
   of `execute_margin_summary()`, `execute_margin_expand()`, or
   `execute_margin_nest()`. The executor performs its schema-aware preflight,
   calls `validate_margin_operation()` immediately before low-level work, and
   selects or invokes the appropriate adapter.
4. **Finalize.** `finalize_margin_operation()` starts from an ungrouped
   result, restores factor margin columns, places fixed keys and grouping
   dimensions followed by the optional Grouping set identifier first, and
   leaves row ordering unspecified.

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
shared semantic margin-label validation entry point, and propagation of the
public call in user-facing errors. Preparation stores only canonical derived
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
a backend or execute a margin.

Grouping specification kinds are governed by one private strategy registry in
this module. Each kind rule owns empty-argument validation, validation of
nested specifications, and expansion dispatch. Structural preflight remains
separate from validation that depends on resolved selections, and nested
specifications are validated before their parent-child relationship. The
registry is used only while compiling; neither it nor the source specification
is retained in the resulting Grouping plan.

### Margin label (`R/margin-label.R`)

Owns label normalization, factor/backend restrictions, optional collision
checks, and applying a label or typed missing value to an omitted dimension
in a portable branch. Preparation normalizes the label and creates the
metadata it needs; semantic validation is delayed until the verb executor has
completed its schema-aware preflight and is about to execute.

### Factor (`R/factor.R`)

Owns restoration of factor and ordered-factor margin columns from the metadata
captured during preparation. Its backend methods preserve levels and ordering
after labels have been materialized; it does not acquire metadata itself.

### Summary selection (`R/summary-selections.R`)

Owns summary-only semantics: rejecting the removed `.groups` argument and
branch-local grouping-context helpers, resolving `across()` and `pick()`
selections while excluding every fixed key and grouping dimension, predicting
known output names, and preventing summary outputs from overwriting grouping
columns. These checks occur before semantic label validation, including any
opt-in lazy collision query.

### Parent share (`R/parent-share.R`)

Owns Parent-share request planning, parent mapping, source validation, ratio
calculation, collision-safe temporary names, and private backend adapter
dispatch. The adapter interface receives the prepared Margin operation, the
staged ordinary-summary result, and all planned requests together; selection
uses the prepared backend kind rather than the staged result's incidental
class.

Arrow is rejected at the immediately earlier executor boundary because no
ordinary-summary query may be staged for a valid Arrow Parent-share request.
The rejection runs after request planning and common Margin-operation
validation, uses only the operation's prepared backend kind, and adds no
wrapper, hook, sentinel, or extension seam. Other Arrow Margin operations
continue through the ordinary summary, expansion, and nesting paths.

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

## Opaque Margin operation seam

The class name, fields, and constructor of a prepared Margin operation are
package-private and are not compatibility contracts. Public verb bodies and
tests pass the value as a whole. They must not inspect it, cache it, return it,
or construct it independently.

Direct field reads are confined to:

- the Margin operation module, for validation and finalization; and
- the three dedicated verb executors, for their schema-aware preflight and
  calls into low-level adapters.

The native and portable adapters receive the specific derived values they
need, not the Margin operation itself. This boundary lets the operation,
Grouping plan, and backend representations change without spreading field
access through public verbs or adapters.

## Test seams

The primary compatibility seam is the four exported verbs:
`summarize_with_margins()`, `expand_with_margins()`,
`nest_with_margins()`, and `nest_by_with_margins()`. Tests should observe
rows, columns, types, grouping, errors, laziness, and SQL semantics through
those interfaces. They must not assert the Margin operation class, fields,
constructor shape, or internal helper order.

The test suite divides supporting contracts as follows:

- `test-grouping-interface.R` covers shared local behavior through the public
  verbs, including grouping plans, labels, factors, duplicate policies,
  persistent groups, result grouping, summary selections, and nesting.
- `test-margin-id.R` covers Grouping set occurrence identifiers across all
  public verbs, including local, native, portable, duplicate, nesting,
  collision, missing-value, and laziness semantics.
- `test-summarize-operation.R`, `test-expand-operation.R`, and
  `test-nest-operation.R` cover lifecycle ordering and the single typed
  metadata snapshot through public calls.
- `test-grouping-backends.R` covers Arrow and dtplyr metadata behavior,
  native and portable SQL strategy, lazy query composition, collision checks,
  internal-name safety, and live DuckDB equivalence.
- `test-parent-share-backends.R` covers Parent-share adapter behavior,
  including targeted pre-query Arrow rejection, dtplyr execution-time
  validation, lazy SQL composition, and live backend results.
- `test-get-col-names.R` and `test-factor.R` cover the focused metadata and
  factor backend contracts.
- `test-grouping-plan.R` covers the backend-independent Grouping
  specification compiler directly, including the complete kind-nesting
  grammar, phase-sensitive empty rules, error precedence, and expansion order.

Backend tests may instrument a backend seam or inspect semantic query shape,
but should not couple to the Margin operation representation or require
byte-for-byte SQL formatting.

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
4. keep label rules, factor restoration, summary selection, and finalization
   in their owning modules rather than duplicating them in the adapter; and
5. add contract coverage for metadata acquisition, observable results,
   laziness, SQL strategy and composition where applicable, identifier and
   label quoting, and native-versus-portable equivalence when both paths
   exist.

The backend capability description selects between already prepared execution
strategies. It does not replace the Margin operation, specialize the Grouping
plan, or bypass common finalization.

## Repository placement

This file, `CONTEXT.md`, and the ADRs are human-maintained contributor
material. They are separate from the generated package site in `docs/`;
`^CONTEXT\.md$` and `^design$` in `.Rbuildignore` exclude them from source
packages. Internal architecture is not duplicated into the README or
vignettes.
