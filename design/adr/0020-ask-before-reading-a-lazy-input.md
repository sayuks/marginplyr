# Ask before reading a lazy input

marginplyr sends no query that reads a lazy input's data unless the caller
asked for one. A Margin verb applied to a lazy input builds a query and returns
it unexecuted; `dplyr::show_query()` runs nothing, and no row is read until the
caller executes the query themselves. `nest_by_with_margins()` is the one
exception, and it is one by its return type rather than by this decision: a
row-wise data frame is a local object, so the verb collects to build one at
all. It is documented as collecting, and it is not a lazy result that reads
without being asked.

Two queries are exempt, enumerated rather than derived from a shape. Neither
reads the caller's data, and each is justified separately because a single
justification covering both would be false of one of them:

1. **The zero-row read of the input**, `collect(head(.data, 0L))` in
   `grouping_selection_proxy()`, sent only to backends whose
   `collect_selection_proxy` capability grants it. Granting that capability to
   a further backend extends this exemption and is justified per backend at
   that point. Its warrant is that marginplyr decomposed the factors it
   restores — combining grouping sets by union forces one common type across
   branches, so a factor dimension is converted to text — and the levels and
   column prototypes it is rebuilt from have no other source. Returning no rows
   is not the warrant: a zero-row read still references the caller's table, and
   `investigation/query-cost-across-lazy-backends.md` records that no vendor
   documentation exempts `LIMIT 0` from BigQuery's rule that a `LIMIT` does not
   reduce the bytes a non-clustered table is billed for.
2. **At most two queries per SQL dialect**, sent the first time a share is
   requested on that dialect, asking whether the dialect converts non-numeric
   values to numbers rather than refusing them. Neither references a table of
   the caller's — `SELECT SUM('x') FROM (SELECT 1 AS z)` — so no reading of
   either touches their data, and the answer is a property of the dialect and
   is reused for every later connection sharing it. The bound stated here is
   amended below: only an answer is remembered, so it is per share request
   until the dialect answers.

   The second is a control, and it is sent only when the first raises. A
   raised query is how a refusing dialect is recognized, but it is also what a
   dialect whose scaffolding is invalid produces — `SELECT 1 AS z` has no
   `FROM`, which Oracle and SAP HANA both reject — and what a dropped
   connection or a permissions failure produces. Reading any of those as the
   refusal records the verdict that *proceeds*, so the rule would be switched
   off for the whole dialect precisely where it could not be applied. The
   control asks the one thing no dialect can refuse, summing the number the
   scaffolding already selects; a control that does not answer means the
   question could not be put here, and the share is refused.

The predicate for "no external system is involved" is `is.data.frame(.data)`,
and it is exact rather than an approximation of cheapness. Nothing
`grouping_backend()` reads distinguishes a local DuckDB file from a hosted
DuckDB service, an in-memory Arrow table from a dataset in object storage, RDS
PostgreSQL from Aurora Standard, or SQLite from BigQuery; the first of each
pair charges nothing per query and the second may charge for every read.

*Two claims in that paragraph no longer describe the code: "exact", and the
Arrow pair. The amendment* Two of the four pairs, and the word exact *below
corrects them; the paragraph is left as it was written, as ADR 0014's amendment
leaves the text it supersedes.*

One rule sets every default: **a check that reads the caller's data is asked
for, and a check that does not is not.**

- `.check_margin_label` scans the grouping columns, so it defaults to
  `is.data.frame(.data)`.
- `.check_share_source` reads nothing on any backend, so it defaults to `TRUE`.

The same rule splits the Margin-label collision check in two. A label equal to
a *declared* factor level is found in metadata that ADR 0002 already acquires,
so that collision is rejected on every backend whatever `.check_margin_label`
says. A label equal to an *observed* value is found only by reading, so that
collision is what `.check_margin_label` controls.

## Amendment: only an answer is remembered, so the bound is per request

Exemption 2's queries are unchanged, and so is which verdict refuses a share.
What changed is its bound: **at most two queries per share request until the
dialect answers**, not two per dialect.

"The answer is a property of the dialect and is reused for every later
connection sharing it" is true of the two outcomes
`investigation/share-source-eligibility-on-coercing-dialects.md` measured —
the dialect refused summing a string, or it converted it to a number — and each
of those is still recorded and still asked only once per dialect. It is not
true of a question that went unanswered, which is a fact about one attempt: a
dropped socket, a permissions blip, or a warehouse that was resuming produces
it on a connection whose dialect would answer perfectly well. Recording it
refused shares on that dialect for the rest of the session, on every later
connection carrying it, and left the caller only
`.check_share_source = FALSE` — which opts out of the rule rather than retrying
the question (#198). Nothing of that kind is recorded now, so the next share
request asks again.

Whether an unanswered attempt was transient is not asked, because it cannot be
read from a raised query. That is the same fact the control query above exists
because of, and it is why asking again is the whole of the remedy.

The cost falls only where a request is refused anyway. A dialect that genuinely
cannot answer — Oracle and SAP HANA, whose scaffolding this exemption's probe
lacks a `FROM` for — is asked twice per refused request and never on one that
succeeds, so no calculated share pays for it.

This is the line the design already drew one step earlier, for the same reason.
A connection that *cannot be asked* — a `dbplyr::simulate_*()` one, which
executes nothing — records nothing, precisely so that a live connection
carrying the same dialect does not inherit it. A transient failure on a live
connection belongs on that side of the line.

## Amendment: isolate RPostgres caller transactions (#774)

Exemption 2 retains its two table-free questions and measured-only cache. Its
statement bound expands inside a transaction begun through DBI with RPostgres:
**at most six statements per unanswered share request**, comprising at most two
questions and four savepoint controls. No statement reads the caller's tables.
Autocommit and cached answers retain their existing bounds.

PostgreSQL puts a transaction into an aborted state when the string probe
raises. Catching that R condition does not recover the transaction, so the
numeric control cannot answer and a caller's later commit cannot preserve
earlier work (#774). A savepoint isolates the questions from that earlier work.
After a failed probe, rollback to the savepoint precedes the numeric control;
every exit rolls back to and releases that savepoint. These are named-savepoint
controls, never termination of the caller's transaction. An unanswered attempt
is still not cached, and a failed cleanup cannot install a measured verdict.

RPostgres's public `postgresIsTransacting()` reads its DBI transaction state
without a query. It identifies the boundary this amendment protects; it does
not identify transactions begun with raw SQL. Simulators execute nothing, and
RPostgres's Redshift driver is excluded because Redshift lacks savepoints.
The savepoint statements are Sent queries during construction and are recorded
before sending when audit is enabled, under ADR 0027.

The live acceptance runner, `tools/postgres-share-transactions.R`, starts a fresh
R process per case and observes transaction state independently before caller
recovery. It covers standard driver mapping, cold and warmed connections,
Parent and Total shares, count and sum, audit on/off, read-only and SELECT-only
callers, commit and rollback, and retry after an unanswered attempt. The
driver-boundary tests cover cleanup failures without requiring a database in
CRAN checks.

## Amendment: two of the four pairs, and the word exact

The rule, the exemptions, and the predicate are unchanged. What is withdrawn is
a claim made in support of the predicate, in the paragraph above beginning "The
predicate for": that nothing `grouping_backend()` reads tells an in-memory Arrow
table from a dataset in object storage. It does — what it reads is the class
attribute, and `Table` and `RecordBatch` are different values of it from
`Dataset`. What that site does with them is a separate matter, and the reason
this is a withdrawn claim rather than a changed line: `grouping_backend()`
collapses all four into one `is_arrow`, because one backend kind is the right
answer for the question it asks. The distinction is available to it and it
declines to draw it, which is not the same as its being unavailable. Arrow's own
reading of a query's source separates a query over a table from a query over a
dataset, so all five Arrow shapes are told apart from the object alone (#254). A
`Table` is in this process's memory by definition, and there is no remote one
for it to be confused with.

The consequence is that "exact" was too strong for the predicate as a whole.
`is.data.frame(.data)` is `FALSE` for an in-memory Arrow table, where no
external system is involved, so it answers conservatively there rather than
exactly. Conservative is the right direction and the predicate stands: the
three pairs left above are genuinely indistinguishable, a rule that tracks four
vendors' pricing pages is the option this ADR already rejected, and widening
the predicate to admit a class as local would be a claim about a vendor's
product made from an R class.

What the correction costs is an argument, not a rule. ADR 0025 refuses a summary
an Absorbing backend would read the caller's input to compute, and it cannot be
justified by this ADR's cost reasoning, because the inputs that absorb are
exactly the ones no external system is involved in. It rests on this ADR's other
half instead — that when the caller's data is read is the caller's to decide —
and on what absorbing takes from them: every column of the input, chosen by
nobody. Leaving the sentence uncorrected would have made that argument look
available when it is not, which is the whole reason a supporting claim is worth
correcting on its own.

## Considered options

**Treat PostgreSQL as refusing without asking it.** Rejected for #774: that
would replace a measured outcome with a dialect name and withdraw the control's
distinction between refusal and an unanswered attempt.

**Open or terminate a transaction around the probe.** Rejected for #774: the
transaction already belongs to the caller. The savepoint protects earlier work
without acquiring its commit or rollback decision.

**A capability for backends whose queries are cheap.** Rejected: it cannot be
computed. `kind = "duckdb"` covers a hosted service reached through the same
driver, `kind = "arrow"` covers a dataset in object storage, and `kind = "sql"`
covers both SQLite and BigQuery. A capability table encoding cost would be a
table of guesses about which product a connection addresses.

**Pricing the backends.** Rejected on maintenance grounds before correctness.
`investigation/query-cost-across-lazy-backends.md` records four billing models
in use across the backends dbplyr reaches — per byte scanned, per unit of time,
per provisioned capacity with no per-query charge, and per I/O request — with
two of them appearing under a single `grouping_backend()` kind. A rule that
prices backends is a rule that tracks four vendors' pricing pages; a rule that
asks the caller before reading their data tracks nothing.

**An absolute rule with no exemption.** Rejected: withdrawing the zero-row read
withdraws factor levels and column prototypes on every lazy backend that has
them, so ADR 0012's factor contract stops holding on DuckDB and dtplyr by
default, and the declared-collision rejection above stops being free there. The
cost of the exemption is one query per operation that reads no rows; the cost of
removing it is a contract.

**Bounding reads by the rows they request.** Rejected as a cost argument, and
recorded here because ADR 0010 rests on it. A read bounded in rows is not
bounded in what it costs: a `LIMIT` does not reduce BigQuery's bytes billed on
a non-clustered table, a one-row query that starts a Snowflake warehouse costs
a minute of credits, every read on Aurora Standard is a billable I/O request,
and Athena bills a failed query like a successful one. Rows are the wrong unit.

**Require the runtime gate's controls to use qualified calls.** Rejected: the
catalog identifies an execution entry point by package and name, not by how a
caller reaches its binding. A qualified-only rule would leave the counter blind
to the attached `package:*` binding and make that blindness a condition every
later control has to remember. The gate traces both the namespace binding and
any distinct attached binding instead (#303).

## Documentation consequences

The reference carries a *When marginplyr queries your data* section stating
what is sent unrequested and what is not, because a caller cannot read it off
the argument list: the two exemptions are invisible at the call site, and the
differing defaults of the two `.check_*` arguments read as arbitrary without
the rule that produces them.

`vignettes/database_backends.qmd` states plainly what is given up when
`.check_margin_label` is left at its lazy default: a grouping column holding
the label yields two rows that cannot be told apart, and neither a reader nor
downstream code can recover which is the margin. It also names the two things
that reduce the exposure at no cost — the declared-level rejection, and keeping
`grouping_bit()` or `grouping_id()` in the result.

## Test strategy

The rule is not readable from any one file, so it is asserted rather than
described. A snapshot records the internal functions that reach an execution
entry point, and a second snapshot records the set of entry points scanned for,
so a scan that stopped covering `compute()` fails rather than reporting a clean
result. A third records which backend kinds hold `collect_selection_proxy`, so
extending exemption 1 is visible in a diff.

The runtime counter traces every binding a qualified or bare call can reach.
When a subject-tested entry forces its caller's promise, tracing is re-enabled
for that force alone, so an execution entry point nested in the argument remains
visible. Positive controls cover both shapes (#303).

Snapshots run only where `NOT_CRAN` is set: the `structure` and `backend` jobs,
the local Review-ready check, and the CI coverage run (ADR 0030). A plain CRAN
check does not execute this gate.

## Related decisions

ADR 0002 acquires the typed metadata this decision's first exemption exists to
obtain. ADR 0003 orders label validation relative to execution and is amended
by this decision only in what it may run without asking. ADR 0010 introduced
the one-row share-source read and is amended separately, since its decision
stands and only its cost justification is withdrawn. ADR 0012's factor contract
is what exemption 1 protects. ADR 0014 selects the per-kind entry that reads
the source, and is amended separately too: its two lookups stand, but the
second no longer selects a sampler, because there is nothing left to sample.
ADR 0025 applies this decision's other half to a read this package does not
issue, and is what the amendment above was written for. ADR 0027 records the
SQL marginplyr sends, and is where a third exemption for `EXPLAIN` was put to
this decision and refused; nothing here changed as a result.

Evidence: `investigation/query-cost-across-lazy-backends.md` and
`investigation/share-source-eligibility-on-coercing-dialects.md`.

## Amendment: declared factor levels are not metadata on Arrow

The declared half of the Margin-label collision check still contacts nothing
where marginplyr holds factor-level metadata. #492 narrows the former
all-backends statement for Arrow: an Arrow result carries a non-missing Margin
label as character rather than as a factor.

Arrow therefore does not reject a label equal to an unused declared factor
level, whatever `.check_margin_label` says. That level has no row in the
result, so it cannot itself make a source row and a margin row
indistinguishable. A source value equal to the label is an observed collision,
and Arrow rejects it when the caller sets `.check_margin_label = TRUE`.

This adds no read. ADR 0003's amendment *one half of the collision check
contacts nothing* is superseded only in its "every backend" clause.

## Amendment: inspect a direct Arrow reader through an unexecuted query

`inspect_grouping()` accepts a direct `RecordBatchReader` and normalizes it to
the same `arrow_dplyr_query` a caller would build with
`dplyr::select(reader, dplyr::everything())` (#510). Inspection compiles and
formats one Grouping plan; it builds no grouping-set branch, so it has none of
the reuse hazard that keeps a reader out of a Margin operation.

The normalization runs after general input admission and before
`prepare_grouping_plan()`. It constructs a query and does not call `collect()`,
`compute()`, or `arrow::as_arrow_table()`, so no batch is consumed. Summary and
expansion still reject a direct reader or a query whose source graph contains
one, and nesting still asks the caller to collect either shape.

`arrow::as_arrow_table()` joins the execution-entry-point catalog in
`test-query-policy.R`: it consumes a reader while materializing it. The runtime
gate has a positive control for that entry and asserts that direct-reader
inspection invokes no catalogued entry point. The public-verb test compares its
Grouping plan with the plans from a `Table` and a manually wrapped reader query,
then consumes the original reader to show every batch remains.

## Amendment (#625): a zero-row result does not permit full-source dtplyr work

The first exemption permits acquiring typed metadata without processing the
caller's source rows. It does not permit a dtplyr step to evaluate its upstream
operations over the full input and apply `head(0L)` only afterwards. Such an
evaluation reads the caller's data and may run their functions before they
collect the Margin result or finish inspecting the Grouping plan. The exemption
is therefore about the work performed, not only the row count returned.

For dtplyr, Margin operations and typed `inspect_grouping()` may use source
schema or a zero-row evaluation only where it faithfully preserves the input's
derived column types and factor levels. An arbitrary transformation over empty
roots does not establish that fidelity: its output metadata may depend on the
source values. When faithful metadata cannot be obtained without processing
source rows, the call refuses with a Package condition explaining the risk and
directing a caller who wants the work now to collect the input explicitly and
pass the resulting data frame. No implicit full-input evaluation or silent
substitution of empty-input metadata is allowed.

Name-only inspection does not acquire typed metadata. The existing refusal of
Mutable steps by Margin verbs and the protection of caller-owned tables during
inspection remain in force. This amendment narrows the dtplyr instance of the
first exemption; it does not alter DuckDB's zero-row metadata read or the
second exemption.

## Amendment (#1506): CSV auto-detection is source work during DuckDB binding

The first exemption does not permit CSV auto-detection to sample source values
while DuckDB binds a zero-row query. Captured SQL `read_csv()` and
`read_csv_auto()` calls must explicitly disable auto-detection with a literal
`auto_detect = false` before marginplyr requests typed metadata. An explicit
schema alone does not disable dialect or header inference. If that opt-out is
absent, a Package condition directs the caller to supply an established schema
or collect explicitly. Name-only inspection still uses captured names without
opening the source. Ordinary tables, explicit-schema CSV metadata, the dynamic
PIVOT refusal and the single typed snapshot remain unchanged.

## Amendment (#1519): CSV filename replacement scans also infer source values

DuckDB binds CSV filename relations such as `FROM 'path.csv'` through
`read_csv_auto`, so the CSV sampling refusal also applies to those captured
relations. It covers CSV and TSV suffixes, gzip and zstd compression, glob
patterns, and equivalent quoted or escaped spellings in relation positions.
Scalar strings and non-CSV relations do not establish an inferred CSV source.
Name-only inspection still uses captured names without opening a file; typed
inspection requires an established reader schema or explicit collection.

The accepted suffixes follow `ReadCSVReplacement` in the
[DuckDB 1.5 source][csv-replacement].

[csv-replacement]: https://github.com/duckdb/duckdb/blob/v1.5.0/src/function/table/read_csv.cpp

## Amendment (#1516): JSON schema and record inference are source work

The first exemption also excludes JSON sampling during DuckDB binding, for
`read_json`, `read_json_auto`, `read_ndjson`, `read_ndjson_auto` and JSON file
replacement scans. JSON requires established types and an established record
mode. A literal schema disables type inference; more than one literal column
also establishes record mode. With a single column, record mode must be
explicit, even with `auto_detect = false`: automatic record detection still
samples objects. Unresolved readers are refused before binding and direct the
caller to established metadata or explicit collection. Name-only inspection
continues to use captured names.

DuckDB's [JSON loading reference][json-loading] documents its inference options.
The additional record-mode boundary is implemented by
`JSONMultiFileInfo::BindReader` in the
[DuckDB 1.5 source][json-binding], and live tests distinguish schema-only binding
from sampling using malformed JSON contents.

[json-loading]: https://duckdb.org/docs/1.5/data/json/loading_json
[json-binding]: https://github.com/duckdb/duckdb/blob/v1.5.0/extension/json/json_multi_file_info.cpp

## Amendment (#1524): resolve relations before classifying filename fallback

A CSV or JSON suffix names a replacement scan only when DuckDB cannot resolve
the captured relation as a CTE or catalog relation. CTE visibility follows its
SQL scope. Under `WITH RECURSIVE` a CTE also sees its own name in its
recursive term, after the body's top-level `UNION`; in the anchor term, or in
a body without `UNION`, the name resolves as it would without the CTE (#1649). Its definition is
still checked for unsafe readers and unresolved filenames. Typed metadata
preflight may read DuckDB's system catalog names, stored view definitions,
macro names and configured search path once per operation to distinguish
existing relations from filename fallback.
That query binds no caller relation and reads no source values. It is not a
second input-type snapshot under ADR 0002 or a further exemption for reading
the input. Name-only inspection performs neither catalog nor typed reads.

Identifier comparison uses ADR 0032's ASCII-only equivalence throughout captured
SQL, decoded names, common table expressions and catalog lookup. Distinct
non-ASCII spellings remain distinct; the caller's SQL is unchanged.

A stored view's name alone does not establish safety. Preflight follows its
definition and nested views in their own catalog/schema search context, without
inheriting the caller's common table expressions. Existing reader checks apply
inside those definitions. Cycles, macro calls and table functions whose safety
is not established are refused before binding, with the option to supply
established reader metadata directly or collect explicitly. A table function's
safety is not established when authored SQL refuses it: `query()` and
`query_table()`, which bind SQL text given as a value, and a CSV or JSON reader
that has not disabled inference. Any other table function, such as
`read_parquet()`, `range()` or `unnest()`, is admitted inside a view as it is
when written directly (#1651). View definitions
and macro names belong to the same catalog acquisition, not additional queries.
After macro admission, stored-view arguments also accept DuckDB's parenthesized
named arguments, literal Boolean casts and literal `struct_pack` schema form.
These are recognized syntactically without evaluating expressions. Other
expressions do not establish a disabled inference option or a literal schema.
Macro lookup distinguishes call positions from stored table-alias and common
table expression column declarations. Declaring a name does not exempt an
actual call to the same name elsewhere in that definition.
The same catalog query includes DuckDB's keyword inventory from
[`duckdb_keywords()`][relation-keywords]. In stored view SQL,
[`FunctionExpression::ToString`][relation-function-printer] writes function names
through `KeywordHelper::WriteOptionallyQuoted`. Its
[default keyword category][relation-keyword-default] and
[quoting rule][relation-keyword-quoting] quote every keyword, including unreserved
ones. Unquoted keyword heads in that stored representation are therefore syntax,
while quoted function names remain eligible for macro lookup. This rule is
restricted to stored view SQL; it is not applied to authored captured SQL.

The catalog query is a Sent query under ADR 0027, including when an unresolved
filename is subsequently refused. The existing zero-row input query remains
the sole typed snapshot. `dbExistsTable()` is unsuitable here: DuckDB's DBI
method binds `SELECT * FROM <name> WHERE FALSE`, which can itself trigger the
replacement scan being guarded.

Catalog resolution follows `GetCatalogEntries` in DuckDB's
[catalog implementation][relation-catalog] and `SetPathsInternal` in its
[search-path implementation][relation-search-path].
View lookup follows `Binder::GetSearchPath` and `Binder::Bind(BaseTableRef&)` in
the [view binder][relation-view-binder], and `SetSearchPath` in the
[catalog entry retriever][relation-entry-retriever].

[relation-catalog]: https://github.com/duckdb/duckdb/blob/v1.5.0/src/catalog/catalog.cpp
[relation-search-path]: https://github.com/duckdb/duckdb/blob/v1.5.0/src/catalog/catalog_search_path.cpp
[relation-view-binder]: https://github.com/duckdb/duckdb/blob/v1.5.0/src/planner/binder/tableref/bind_basetableref.cpp
[relation-entry-retriever]: https://github.com/duckdb/duckdb/blob/v1.5.0/src/catalog/catalog_entry_retriever.cpp
[relation-keywords]: https://duckdb.org/docs/1.5/sql/meta/duckdb_table_functions#duckdb_keywords
[relation-function-printer]: https://github.com/duckdb/duckdb/blob/v1.5.0/src/include/duckdb/parser/expression/function_expression.hpp
[relation-keyword-default]: https://github.com/duckdb/duckdb/blob/v1.5.0/src/include/duckdb/parser/keyword_helper.hpp
[relation-keyword-quoting]: https://github.com/duckdb/duckdb/blob/v1.5.0/src/parser/keyword_helper.cpp

## Amendment (#1583): a registered data frame is not a hidden definition

A view that DuckDB's client API creates stores no SQL, so the #1524 rule above
cannot follow its definition. A data frame registered with
`duckdb::duckdb_register()` is such a view, and binding it reads only the R
frame's column types, never source rows, so preflight admits it. The catalog
query is unchanged; whether a `temp.main` view without SQL is a registered
frame is read from the `_registered_df_<name>` attribute the duckdb R package
keeps on the connection pointer to hold the frame alive. That attribute is
undocumented, so its absence fails closed. Any other view without stored SQL,
such as a relation view over `read_csv()`, remains refused before binding,
with a diagnostic naming the missing definition.

## Amendment (#1654): DuckDB's own scalar macros, while no caller macro exists

The #1524 rule refused every call whose head the catalog lists as a macro, so
DuckDB's own scalar macros, such as `nullif()`, `date_add()`, `list_sum()` or
`current_database()`, were refused in authored SQL, `sql()` fragments and
stored views although expanding one reads no source. Preflight admits a call
when its first catalog match is a scalar macro (`macro`, not `table_macro`) in
database `system` and the operation's catalog snapshot holds no macro or table
macro outside database `system`. Every other macro call is refused as before.

Both conditions are needed. A system table macro can read source work:
`histogram_values()` is defined over `query_table()`, and `histogram()` calls
it. A system macro's body is fixed text whose free names resolve in the
caller's search path, so one caller macro can redirect it: a caller-defined
`list_aggr` reading a CSV makes the zero-row type of `list_sum([v])` follow
the file's data row. The admitted macro's arguments are authored text that the
relation, reader and call checks still read. A finer rule, admitting unless a
caller macro's name occurs in the system macro's stored definition, would
refuse less for callers who define macros; it is not adopted.

## Amendment (#1650): the rendered statement is checked too

The checks above read only the captured lazy query's `sql()` fragments and
table paths, so a caller macro or a CSV source reached through any other
dbplyr spelling was bound: a call dbplyr passes through
(`mutate(y = hidden_first(v))`), a call it translates to a macro's name
(`tolower(g)` renders `LOWER(g)`, which a caller macro named `lower`
replaces), and verbatim SQL carried as `DBI::SQL()` or `ident_q()`. A caller
macro reached by the zero-row metadata statement is refused in every spelling,
as its authored spelling already was, and the CSV and JSON rules hold for
verbatim fragments in the same way.

The rendered statement is the only text that shows what a translation emits,
so the same relation, reader and call checks run over it inside
`sql_render.marginplyr_duckdb_selection()`, which already receives it on the
audit and collect paths (ADR 0027); preflight still does not render. A
refusal there is raised before the statement is sent and leaves no
`selection_proxy` row, and a translation failure is raised and recorded as
before, since the render fails before the check runs. The catalog snapshot is
the operation's one catalog query; a call-shaped head in the rendered
statement can be what first needs it.

The cost is accepted: `mutate(y = add_one(v))` over a harmless caller macro
returns no typed metadata, as `mutate(y = sql("add_one(v)"))` did not;
`compute()` or `collect()` first is the remedy the diagnostic names.
Narrowing the documented guarantee to authored SQL was rejected, because the
sampling is marginplyr's own query and passing an unknown function through is
how dplyr calls a database function.

## Amendment (#640): SQLite typed-order execution follows an explicit request

ADR 0031's live SQLite result builds its typed compound query without executing
it. Direct `collect()` reaches `dbplyr::db_collect()` only when requested.
Direct `compute()` may query destination metadata to avoid an unsafe name, then
uses a private savepoint, `dbplyr::db_compute()`, and `DBI::dbExecute()` to
create and populate the requested typed table. The destination checks and
zero-row type projection run only at that explicit compute boundary. The
structural query-policy snapshot covers these execution entry points so a
future construction-time call becomes visible. Neither exemption for
unrequested reads changes.

## Amendment (#1272): Arrow expansion shares one evaluated input

Collecting or computing a multi-set Arrow expansion evaluates its input once
into an Arrow Table. The grouping-set branches read that same Table so that
volatile values and row membership agree across copies. Each execution gets
its own Table and retains Arrow's nested column types.

The expansion constructs a plan and a reader factory using only metadata.
Arrow opens the reader while building its execution plan. For plan display
the factory supplies a schema-only reader. For execution it calls
`arrow::as_arrow_table()` on the input before Arrow starts the outer plan,
so the input can itself run R callbacks. The runtime read gate covers
construction and plan display. The reader-source refusal still applies at
verb entry.
