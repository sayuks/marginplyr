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
and it is conservative rather than an approximation of cheapness. Nothing
`grouping_backend()` reads distinguishes a local DuckDB file from a hosted
DuckDB service, RDS PostgreSQL from Aurora Standard, or SQLite from BigQuery;
the first of each pair charges nothing per query and the second may charge for
every read. It was written "exact" and given a fourth pair that does not hold;
the amendment below corrects both.

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

## Amendment: the predicate is conservative, and one Arrow pair was wrong

The rule, the exemptions, and the predicate are unchanged. What is withdrawn is
a claim made in support of the predicate: that nothing `grouping_backend()`
reads tells an in-memory Arrow table from a dataset in object storage. It does.
`inherits()` already separates `Table` and `RecordBatch` from `Dataset` there,
and Arrow's own reading of a query's source separates a query over one from a
query over the other, so all five Arrow shapes are told apart from the object
alone (#254). A `Table` is in this process's memory by definition, and there is
no remote one for it to be confused with.

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

Snapshots run only where `NOT_CRAN` is set, which is the `structure` job and
the `backend` jobs; a plain CRAN check does not execute this gate.

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
issue, and is what the amendment above was written for.

Evidence: `investigation/query-cost-across-lazy-backends.md` and
`investigation/share-source-eligibility-on-coercing-dialects.md`.
