# Record the SQL marginplyr sends

marginplyr keeps a record of the Sent queries of one call, and a caller reads it
back with `last_sent_queries()`. The record is off unless the caller sets
`options(marginplyr.audit_sql = TRUE)`; it holds one row per Sent query, with
the columns `purpose` and `sql` and no others; and it belongs to a single call,
emptied at the top of `prepare_grouping_plan()`. Each row is written before its
query is sent, so what the record holds is what was sent rather than what
succeeded, and a call that fails leaves readable everything it had already sent.

The capture is `dbplyr::sql_render()` and nothing else. It is client-side, sends
nothing, and is not one of the execution entry points
`lazy_execution_entry_points()` catalogs, so **ADR 0020 is not amended**: this
decision adds no third exemption to it and does not read the opt-in as the
caller having asked for a query. The four sites that do send — the zero-row
selection proxy, the observed-label-collision scan, and the dialect probe's
question and control — send exactly what they sent before this decision, whether
or not a caller is auditing.

`marginplyr.audit_sql` is the package's first option, and `last_sent_queries()`
is its first export that reads back what a previous call did; all fifteen others
are verbs, constructors, or grouping helpers. Neither has a precedent inside
this package to cite, which is why both are settled here rather than by
analogy.

## Why a record rather than a signal

The audit requirement is that every query marginplyr sends is recorded *and
distinguishable*. Nothing already available satisfies the second half.
`dbplyr::last_sql()` — one slot, overwritten by every render that funnels
through `db_sql_render()`, marginplyr's internal ones included — reports the
selection proxy after a DuckDB Margin verb, the label scan under
`.check_margin_label`, and on RSQLite a stale query about an unrelated table
from earlier in the session, with nothing saying which of those a reader has
(#381). An answer whose correctness rests on the caller reading in the right
order, and which is silent when they do not, is the defect this decision exists
to remove; reproducing it anywhere in the feature would not hold together, which
is what settles the accessor's refusals and the concurrency sentence below.

A `marginplyr_query_sql` condition delivered through `withCallingHandlers()` was
the proposed mechanism and is rejected. Its only capability the record lacks is
streaming across many calls, and the record is per-call, so a caller who wants
that loops and appends it themselves. Nothing was found that the condition alone
can do. Against that, it would be the package's first non-error condition, which
`?marginplyr` and `CONTEXT.md`'s *Package condition* entry both speak against.
ADR 0015 governs errors, so an inert signal falls outside its rule rather than
under it — and being outside a rule is not a warrant.

## The record holds the statement, not the execution

A Sent query is SQL. dtplyr's selection proxy reaches data.table and Arrow's
reads `arrow::schema()`; neither takes a row, so a dtplyr or Arrow call is
audited and records nothing. `last_sent_queries()`'s Rd states in one sentence
that the record promises the SQL marginplyr sent, not the execution marginplyr
caused. Without it, those zero rows are indistinguishable from a hole.

What is promised of a row is that its `sql` is the statement as it was sent.
That every one of the five is in fact a complete, executable single statement is
a fact about those five sites and not a contract: promising executability would
make marginplyr answerable for a property of a backend's parser.

The `purpose` vocabulary is `"result"`, `"selection_proxy"`,
`"observed_label_collision"`, `"share_dialect"`, and `"share_dialect_control"`.
**Only `"result"` is promised.** The sites moved substantially inside a year
(#378), and what an auditor needs is which record is their result; freezing the
names of marginplyr's internal queries is not part of that. ADR 0015 already
declines to promise error subclasses for the same reason.
`"observed_label_collision"` names the half of ADR 0020's split that reads —
the declared-level collision is found in metadata and sends nothing — and the
dialect probe is one site producing up to two rows, ADR 0020's exemption 2
counting the question and the control separately.

## Four answers, distinguished

Completeness is what the record is for, so every case in which it holds fewer
rows than a reader might expect is told apart from the others rather than
collapsed into an unaccounted-for zero:

1. **Nothing has been recorded in this session** — `last_sent_queries()` raises
   a Package condition.
2. **The last recorded call was not audited** — it raises a Package condition
   naming `marginplyr.audit_sql`. The option is read once, at the reset, and
   remembered with the record; otherwise a caller who sets it *after* the call
   would read zero rows under a live option, which is the silent wrong answer
   arriving through a side door.
3. **The call was audited and sent nothing** — a zero-row tibble. This is a
   correct answer and the only one that returns zero rows: a local input sends
   nothing by ADR 0020's `is.data.frame()` predicate, and a lazy backend without
   `collect_selection_proxy` sends nothing until the caller collects.
4. **A statement had no SQL form on this backend** — a row with
   `sql = NA_character_`. `dbplyr::sql_render()` passes unknown functions
   through and raises only where a translation is explicitly refused, and it
   raises at render rather than at build. Dropping the row is the hole; letting
   it raise would turn the option into something that fails a call that would
   otherwise have succeeded. The `NA` row accounts for the site and leaves the
   call unaffected, and the Rd says what it means.

Both refusals are Package conditions under ADR 0015: the caller avoids each by
rewriting a call they wrote — running a Margin verb first, or setting the
option. Recording itself reads `isTRUE(getOption("marginplyr.audit_sql"))` and
reports nothing at all, because a garbage option value must not turn a caller's
`summarize_with_margins()` into an error. The accessor is the surface that
reports it.

The return value is a `dplyr::tibble()`, which ADR 0013 already settled for an
inspection surface, and no print method, which ADR 0013 already declined for the
cost it puts on export and snapshot workflows. The Rd shows `writeLines(x$sql)`
for reading a multi-line statement instead.

## One call, and which call

The reset is at the top of `prepare_grouping_plan()`. The obvious placement —
`prepare_margin_operation()`, the one function all four Margin verbs converge on
— is wrong by one caller: `inspect_grouping()` calls `prepare_grouping_plan()`
directly, and the selection proxy is reached from inside it. Under the obvious
placement, `inspect_grouping()` appends its proxy query to whatever the previous
Margin operation left behind, and nothing says the record now spans two calls —
the `dbplyr::last_sql()` defect, rebuilt inside the feature written to answer
it.
`prepare_grouping_plan()` is the common ancestor of all five recorded sites and
of five entry points rather than four, and within a Margin operation it still
runs before every one of them.

Per-call rather than accumulating: saying which rows belong to which call
needs a call identifier this package has no concept of, while a caller who wants
accumulation loops and appends. Per-call is the half they cannot reconstruct.

Two consequences are documented rather than avoided. Two identical calls need
not produce identical records, because the dialect verdict is cached per
dialect for the session. And a package environment is invisible across `fork`,
PSOCK, and callr: a child inherits the parent's pre-fork record or starts
empty, and its writes never return, so a read after a parallel run reports the
last *serial* call. Six precedent Rd pages across five packages document no
position on this; marginplyr documents one, because a silently stale answer is
the defect this whole decision rests on calling a defect.

## What decides whether a site records

Every recorded site runs after `grouping_backend()` has classified the input, so
nothing classifies a second time. `grouping_backend()` returns `is_sql` as a
field — already computed there for the `kind` cascade — with this contract:

> Whether this input is a SQL backend. Equivalent to `dialect` being non-`NULL`,
> but `dialect` is the field dispatch reads, and its nullity does not announce
> itself as the test. The record reads this field and nothing else.

The record remembers it per call where the backend is computed, inside the same
function whose top holds the reset, so no recorded site is reached with the
backend unknown, `record_sent_query()` takes no backend argument, and neither
`validate_margin_label()` nor `check_observed_label_collision()` gains a
parameter about backends, which is not what either answers. This is the same
move the option's flag makes, and it adds no failure mode the option does not
already have: a site reached without a reset misreads the stale flag in exactly
the same way.

## Corrections

Four claims in the plan this decision settles were true of an earlier codebase
and are recorded as corrections rather than quietly fixed, because each was a
reasonable reading and each is now measured against the working tree.

- **The reset function.** `prepare_margin_operation()` is not the common
  ancestor; `prepare_grouping_plan()` is. See *One call, and which call*.
- **"The accessor never errors."** The cited precedent, `rlang::last_error()`,
  errors when empty (#380), and the accessor refuses in two cases here.
- **ADR 0021 does not reach a condition marginplyr authors.** Its scope is
  `summarize_margin_union()` and External conditions, so it decides nothing
  about how a warning this package raised would be repeated. With the file log
  rejected there is no such warning, and the throttling question has no subject.
- **The `----`-separated on-disk format is unparseable by its own payload.**
  SQL's line comment is `--`, so a `dbplyr::sql()` literal reaches the
  separator. This is secondary to the reason the file log is rejected, and is
  recorded because it is a fact about the format rather than about the feature.

## Considered options

**A `marginplyr_query_sql` condition.** Rejected: see *Why a record rather than
a signal*. A scan of 293 installed packages found none documenting a
user-written handler as the route to observing its ordinary operation; httr2
signals at exactly this site and documents its accessor instead (#380).

**A caller-supplied logging function**, `options(marginplyr.sql_logger =
function(sql, purpose) ...)`. Rejected: it inherits the condition's whole
argument — the record is per-call and a caller can loop — and adds a
callback-shaped option this package has no precedent for.

**A return value or attribute on the result.** Rejected: it cannot represent the
query the caller collects *after* marginplyr has returned, which is the one
purpose that is promised.

**A per-entry-point `.audit_sql` argument.** Rejected in favour of the session
option. ADR 0020's `.check_margin_label` and `.check_share_source` are arguments
because they decide *whether a query is sent*; this decides *whether a record is
kept*, which is a different kind of decision, and an audited session is a
session-level fact. The recorded sites are shared plumbing that five entry
points fan into.

**`"explain"` as a second capture method**, selected by
`marginplyr.audit_sql_method`. Rejected on two grounds recorded separately, so
that neither rests on the other.

- *The demand is already met.* The proposal predates the record's shape. With
  the `sql` column in place, a caller reaches a plan without marginplyr
  mediating: the `"result"` query comes back as an unexecuted lazy object, so
  `dplyr::explain()` applies to it directly, and the four internal statements
  are in the record for the caller to put to their own connection. The three
  internal queries are a zero-row read, a scan the caller asked for, and a
  constant referencing no table of theirs — the plan of a constant answers
  nothing.
- *It is not an audit record.* GoogleSQL has no `EXPLAIN` statement, so on
  BigQuery — the backend the cost argument was about — the statement simply
  fails; SQL Server has none outside Synapse dedicated pool, and SAP HANA's
  writes to a table rather than returning rows. The only method is
  `explain.tbl_sql`, so it raises on every `dbplyr::simulate_*()` connection,
  which is how this package reaches the dialects it has no driver for. The text
  is `print()` on a data frame, varying with `getOption("width")`, the caller's
  indexes and row counts, and DuckDB's `explain_output`; SQLite's own
  documentation says applications should not use `EXPLAIN` and names two past
  format breaks. And the proposed `tryCatch` made those failures silent, which
  contradicts recording a query before it is sent.

  This ground is reached before ADR 0020, not after it, so it would still refuse
  the feature if dbplyr registered a BigQuery `sql_query_explain()` method
  tomorrow.

**`"explain"` at every site except `"result"`,** since the other sites talk to
the backend a moment later anyway. Rejected: it does not avoid the amendment,
because ADR 0020 enumerates *queries* rather than sites — `EXPLAIN collect(head(
.data, 0L))` is not the query exemption 1 names. Independently it is worse than
either extreme, since a caller reading an empty log could not tell "nothing was
sent" from "this site does not record under this setting".

**`dbplyr::remote_query_plan()` as the capture.** Rejected with `"explain"`. It
returns a character vector rather than printing, so it needs no
`utils::capture.output()` — which corrects the plan but not the feature: it
inherits every dialect failure above, and the first ground applies to it
identically.

**An on-disk log written by the package**, `marginplyr.audit_sql_file`.
Rejected, and rejected as incompatible rather than as worthless. Its honest form
is write-or-stop: a record whose purpose is completeness must not silently
acquire a hole, and a file that stopped being written mid-session under a
suppressed warning is worse than no file, because a reader reads it as complete.
That form contradicts a garbage option value not being allowed to turn
`summarize_with_margins()` into an error. The proposed form avoids the
contradiction by warning and continuing — which *is* the hole — and buys the
avoidance with the package's first warning of its own, against a sentence
`?marginplyr` and `CONTEXT.md` state independently. Both routes are closed by
decisions made elsewhere, and this is recorded conditionally for that reason:
what would have to change for a file log to return is the treatment of a bad
option value, or the no-warning axiom.

**An exported `write_sent_queries(path)`.** Rejected separately, because the
argument above does not reach it: a writer the *caller* calls may raise a
Package condition on a bad path perfectly legitimately under ADR 0015. It is
rejected because it is one export standing over `utils::write.csv(
last_sent_queries(), path)`, and it still fixes a format the package then owns
and must keep. Holding no format is the feature: no one format answers an
auditor, and the tibble is what a caller writes to CSV, JSON, parquet, or a
database row from. `last_sent_queries()`'s Rd carries the recipe.

**A `time` column.** Rejected: row order already carries its ordering value,
recording a query before it is sent precludes measuring an elapsed one, and the
column would force every snapshot test and every Rd example to drop it. It was
proposed for the file log, which is not built. A caller accumulating across
calls adds their own timestamp — the same answer as the format above, one column
down.

**A backend column.** Rejected: the rows belong to one call whose backend the
caller already knows, and the `sql` shows the dialect.

**Accumulating across calls.** Rejected: see *One call, and which call*.

**An accessor that returns zero rows in every empty case.** Rejected: see *Four
answers, distinguished*.

**Deferring the render to the accessor,** so that a refused translation raises
where raising is allowed. Rejected: it needs a hidden column or a parallel store
to survive the two-column record, it would hold the caller's query object and
its connection reference in package state, and "recorded before it is sent"
would become "recorded before it is sent, filled in later".

**An `is_sql_lazy_query()` predicate** reading `tbl_lazy`, `dtplyr_step`, and
`arrow_input_classes()` at the recorder. Rejected, and recorded here because the
rejection rests on a general rule rather than on this feature: **the package
does not classify a second time by reading classes it has already read.**
`is_lazy_backend_input()` in the test suite already asks the package's own
classifier rather than the classes it reads, and its header says why; the
predicate is that move run backwards. Two narrower spellings are rejected with
it — `!is.null(backend$dialect)`, which names something other than what it
decides, and a `kind %in%` set at the recorder, which is a second place to keep
in step when a SQL kind is added.

## Test strategy

`lazy_execution_entry_points()` gains `dplyr::explain` and
`dbplyr::remote_query_plan`, both with `subject_test = FALSE`. This is
independent of the verdict above and is what makes the rejection structural
rather than prose: both reach `DBI::dbGetQuery()` from inside dbplyr, where the
static scan over `R/` cannot follow, so one line of either in `R/` would leave
the gate silent. `traced_execution_entry_points()` derives from the same catalog
and covers them without a second edit. The snapshot of entry points scanned for
moves by two rows; the snapshot of internal functions reaching one does not, and
that is the assertion.

Neither entry takes a positive control, and the absence is deliberate rather
than an omission. ADR 0020's controls exist to show the counter can count at
all, which the existing entries discharge. What these two assert is that
marginplyr does not call them, and a positive control for that would be
marginplyr calling `explain()` — the feature this ADR rejects.

`sql_render()` and `show_query()` stay absent from the catalog, so recording the
result query moves none of ADR 0020's snapshots. That is the same fact as the
opening paragraph's, asserted where it can fail.

The dialect probe's two rows are SQL by construction — the query is built on a
connection `DBI::dbIsValid()` has already vetted — and get no runtime test,
since code re-asking a condition that is always true tells a reader it is
variable.

## Documentation consequences

The reference carries a section on the record, and it is not called *Audit
signal*: there is no signal. It states that the feature is off by default, that
`"result"` is the only promised purpose, that a query is recorded before it is
sent and so survives a failing call, what each of the four answers above means,
the SQL-not-execution boundary, and the concurrency position. It names one
capture, there being one. `vignettes/database_backends.qmd` carries the same
material against a live backend, with the point that `"result"` is a render and
not an execution — the caller's own `collect()` is still what runs it.

## Related decisions

ADR 0020 is the rule this decision does not amend, and its *Related decisions*
gains one line naming this ADR as recording a query it did not exempt, so that a
reader of ADR 0020 can find that the question was put. Its exemptions 1 and 2
are three of the five recorded purposes, and its split of the Margin-label
collision check is why only the observed half takes a row. ADR 0013 fixes the
return class and forecloses the print method. ADR 0015 is what the accessor's
two refusals are, and what the recorder's silence about a bad option value is
measured against. ADR 0021 does not reach here, per *Corrections*.

Evidence: `investigation/what-dplyr-explain-sends-per-backend.md` for what
`explain()` sends per dialect and where the statement does not exist;
`investigation/session-state-and-last-call-accessors.md` for what CRAN policy
and *Writing R Extensions* permit a package environment, for the eight
last-call precedents and the shape none of them has, and for the fork, PSOCK,
and callr measurements behind the concurrency sentence;
`investigation/share-source-schema-vs-data-read.md` for the render/read
distinction the client-side capture rests on.
