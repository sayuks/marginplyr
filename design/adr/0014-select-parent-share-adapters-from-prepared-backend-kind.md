# Select Parent-share adapters from the prepared backend kind

Parent shares are calculated by one deep private module behind a small
interface. Its backend behavior lives in explicit adapters, and the adapter is
chosen by looking up the backend kind that the Margin operation already
prepared — never by inspecting the class of the staged ordinary-summary
result, and never by a chain of capability predicates rebuilt at this seam.

## Decision

Parent mapping, ratio calculation, and temporary-name cleanup are shared by
every backend. There are three adapters, and each carries only what its
backends do differently:

| Adapter | Backend kinds | What it adds to the shared work |
|---|---|---|
| Local | `local` | Checks the materialized source types first |
| General dbplyr | `duckdb`, `postgres`, `sql` | Missing-safe `sql_on` join |
| Lazy non-SQL | `dtplyr`, `other` | Nothing |

The third adapter adds nothing, and that is the decision rather than an
accident of implementation. A lazy non-SQL backend joins exactly as local data
does, and differs from local only in what it cannot do: there is no
materialized result to type-check, so its source validation has already
happened inside the ordinary summary. Naming that as its own adapter keeps
the difference visible at the seam obliged to honour it, and keeps the branch
where a future lazy non-SQL divergence belongs. Merging it into the local
adapter would give `dtplyr` a type check it cannot run; merging it into the
dbplyr adapter would give it a `sql_on` condition it has no connection for.

Arrow is not in the table. A valid Arrow Parent-share request is rejected at
the executor boundary immediately before ordinary summaries are staged, so no
Arrow adapter is ever reached. ADR 0005 records why that rejection is
admissible after the typed metadata snapshot.

The lookup has no default. An unrecognized backend kind is a marginplyr defect
rather than something a caller can rewrite their way out of, so it stops with
a bare `stop()` under ADR 0015 rather than silently falling through to the
adapter that happens to look plausible.

Every adapter has the same signature: the prepared Margin operation, the
staged result, the planned requests, and the internal Grouping set identifier
name. Adapters are dispatch targets rather than independent entry points, so
they receive the operation whole; they read the Grouping plan from it and
nothing else. `execute_shares()` reads the backend kind and the
caller-visible Grouping set identifier name and is the only place the
operation crosses into the module.

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
would take the local branch and quietly acquire validation the lazy contract
says it does not have. Reading the prepared kind keeps the branch decision on
state that was validated once during preparation and cannot drift between the
two query stages.

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
in the method registry where the three-way grouping is no longer readable in
one place.

**A separate adapter per backend kind**, mirroring the six kinds one to one.
Rejected: four of the six would repeat a body that already exists — three SQL
kinds share one, and `dtplyr` and `other` share another — and the duplication
would invite them to drift into differences no contract asked for. Backend
kinds are grouped by what their Parent-share execution actually needs.
