# Select Parent-share adapters from the prepared backend kind

Parent shares are calculated by one deep private module behind a small
interface. Its backend behavior lives in explicit adapters, and the adapter is
chosen by looking up the backend kind that the Margin operation already
prepared — never by inspecting the class of the staged ordinary-summary
result, and never by a chain of capability predicates rebuilt at this seam.

## Amendment: the samplers are source checkers, and none of them samples

The decision above stands in full: there are still two lookups keyed on the
prepared backend kind, still with no default, and the adapter table is
unchanged. What is withdrawn is the second table's subject. It selected a
*sampler*, whose job was to obtain source values to judge, and ADR 0020
removed the reading those values came from.

Three things below therefore no longer describe the code:

- The sampler table's `Bounded probe` row — `duckdb`, `postgres`, `sql`,
  `other`, reading "One input row" — and the paragraph explaining that it
  "reads the summaries a share reads … over one row of the input". Nothing
  reads a row of the caller's data to apply the eligible-type rule. Those
  kinds now reach `check_dialect_share_sources()`, which asks the dialect
  itself, with a query referencing none of the caller's tables.
- `unsampled_share_sources()`, now `check_wrapped_share_sources()`. Its
  assertion is unchanged and still load-bearing for the same reason: a kind
  may only be left unasked because `wrap_share_sources()` put the rule inside
  its ordinary summary.
- The sampler signature's final argument. It was "the planned ordinary
  summaries", which a sampler needed to build its read; a checker does not
  read, and takes the verb's `.check_share_source` instead. The rule that
  every entry shares one signature is unchanged, and is still what lets an
  entry ignore an argument it has no use for.

`share_source_sampler()` is `share_source_checker()`, and the lookup's shape,
its lack of a default, and its grouping of kinds are all as decided above.
See ADR 0020, which also withdraws ADR 0010's cost justification for the read.

## Decision

Parent mapping, ratio calculation, and temporary-name cleanup are shared by
every backend. Two things vary, so there are two lookups on the same key, and
each entry carries only what its backends do differently.

How the ratio is joined:

| Adapter | Backend kinds | What it adds to the shared work |
|---|---|---|
| Row-matched | `local`, `dtplyr`, `other` | Nothing |
| General dbplyr | `duckdb`, `postgres`, `sql` | Missing-safe `sql_on` join |

Where the eligible-type rule reads the source values it judges:

| Sampler | Backend kinds | Where the source values come from |
|---|---|---|
| Materialized | `local` | The staged result; no read |
| Bounded probe | `duckdb`, `postgres`, `sql`, `other` | One input row |
| None | `dtplyr` | Nowhere |

The bounded probe reads the summaries a share reads — and only those — over
one row of the input, so the source comes back with the type the backend gives
it. The sampler that reads nothing is not missing a source: `dtplyr` carries
the rule inside its own ordinary summary, where it raises at execution.

Splitting them this way is the decision rather than an accident of
implementation. Whether a source summary is eligible is a property of that
summary, not of the join that follows it, so the rule is applied once above
adapter selection and the adapters never carry it. What is genuinely
backend-specific is only how the answer becomes readable, and that is a
different partition of the same kinds: `local` and `dtplyr` join alike but are
sampled differently, while `other` is sampled like a database but joins like
local data. Two lookups keep both partitions honest; one lookup would have to
pick a partition and hide the other inside a branch.

The row-matched adapter adds nothing at all. A lazy non-SQL backend joins
exactly as local data does, and once the type rule is no longer an adapter's
work, so does local data itself — merging them removes a difference that no
contract asks for. Merging either into the dbplyr adapter would give it a
`sql_on` condition it has no connection for.

`unsampled_share_sources()` asserts `wraps_share_sources_in_summary()` for
the kind it was reached with, so a kind may only be left unsampled because
`wrap_share_sources()` put the same rule inside its ordinary summary. That
assertion is what keeps the sampler table from silently disagreeing with the
planner about which backends validate themselves.

Arrow is in neither table. A valid Arrow Parent-share request is rejected at
the executor boundary immediately before ordinary summaries are staged, so no
Arrow adapter and no Arrow sampler is ever reached. ADR 0005 records why that
rejection is admissible after the typed metadata snapshot.

Neither lookup has a default. An unrecognized backend kind is a marginplyr
defect rather than something a caller can rewrite their way out of, so it
stops with a bare `stop()` under ADR 0015 rather than silently falling through
to the adapter or sampler that happens to look plausible.

Every adapter has the same signature: the prepared Margin operation, the
staged result, the planned requests, and the internal Grouping set identifier
name. Every sampler has one signature too: the operation, the staged result,
the planned requests, and the planned ordinary summaries. Adapters and
samplers are dispatch targets rather than independent entry points, so they
receive the operation whole; the adapters read the Grouping plan from it and
nothing else, and the samplers read the input it prepared. `execute_shares()`
reads the backend kind and the caller-visible Grouping set identifier name and
is the only place the operation crosses into the module.

The seam sits after ordinary Margin summaries are staged and before
Margin-operation finalization. Placing it there is what lets one adapter set
serve every verb-neutral guarantee: the shared finalizer still restores factor
Margin columns, orders columns, and leaves row order unspecified, and the
adapters never see a finalized result.

Adapters are private. They are not a third-party extension interface, and
adding a backend means naming its kind in the lookup inside marginplyr, as
described in `design/architecture.md`.

## Why not the staged result's class

The staged result's class is incidental to how the ordinary summaries happened
to execute, not to what the operation prepared. A DuckDB summary that reached
the portable `UNION ALL` adapter and a DuckDB summary that used native
`GROUPING SETS` are the same Parent-share problem and must take the same
branch; a local tibble collected out of a lazy pipeline by an unrelated step
would take the local branch and be sampled as a materialized result the lazy
contract never promised to produce. Reading the prepared kind keeps the branch
decision on state that was validated once during preparation and cannot drift
between the two query stages.

It also keeps the Arrow rejection honest. That rejection is decided from the
prepared kind before any query is constructed; if adapter selection used the
staged class, the same fact would be established twice from two different
sources of truth.

## Considered options

**One function with backend branches.** Rejected: the branches would grow with
every backend and would sit in the middle of the join, mixing the parts that
differ per backend with the mapping, calculation, and cleanup that do not.

**Capability predicates at the seam** — ask again whether the backend supports
missing-safe joins or materialized types. Rejected: capability discovery
belongs to `R/grouping-backend.R` and is already done during preparation.
Re-deriving it here would make the Parent-share module a second authority on
backend classification, free to disagree with the first.

**S3 dispatch on a backend class.** Rejected: it would make the adapter set a
public extension point by construction, which contradicts keeping one Grouping
plan and one lifecycle for every backend, and it would put the dispatch table
in the method registry where the grouping of kinds is no longer readable in
one place.

**A separate adapter per backend kind**, mirroring the six kinds one to one.
Rejected: most of them would repeat a body that already exists — three SQL
kinds share one, and `local`, `dtplyr`, and `other` share another — and the
duplication would invite them to drift into differences no contract asked
for. Backend kinds are grouped by what their Parent-share execution actually
needs.

**Keeping the type rule in the adapter that could run it.** Rejected: it was
reachable only from the local adapter, so which sources were rejected became a
property of the dialect the caller happened to use — a strict one raised its
own error naming an internal column, and a permissive one returned an
all-missing share column with the grand total's own-denominator `1` (#106).
Eligibility is not adapter-specific, so it is settled above adapter selection
and each backend contributes only the sample it can produce.
