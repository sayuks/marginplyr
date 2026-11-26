# Second eight-lane workflow hunt: share analysis, planning, the DuckDB preflight after its repairs, fix neighbours, PostgreSQL, documentation, refusals and uncovered inputs

Investigated: 2026-11-26
Repository snapshot: `0fe5f1e40daa14e2ba2b5246452fa668bc330f80`

The investigation ran against freshly fetched main in one worktree. No package
implementation, package test, dependency, public specification or generated
documentation was changed, and no repair was implemented. The snapshot was
built outside the checkout and installed into a private library, and every
investigator observed that installed build.

The starting points were chosen not to overlap those of
`eight-lane-workflow-hunt-2026-11-13.md`. Generated differential cases,
surviving mutants, the simulated dialects and the column-class axis, which that
hunt had run, were left out. Eight fresh-context investigators worked read-only
and reported before any candidate was shared:

| Lane | Starting point | Model and effort | Candidates |
| --- | --- | --- | --- |
| 1 | `R/share.R`'s dependency analysis, `R/static-bindings.R` and `R/summary-selections.R` read against ADRs 0010, 0017 and 0019, #1676 and #1680 | claude-opus-5-5, xhigh | 3 |
| 2 | `R/grouping-plan.R`, `R/grouping-set-limit.R`, `R/grouping-spec.R`, `R/grouping-context.R`, `R/inspect-grouping.R`, `R/sent-queries.R` and `R/summarize_with_margins.R` read against the ADRs and the Rd, with the grouping-set limit | claude-opus-5-5, xhigh | 5 |
| 3 | The DuckDB preflight after #1642, #1646, #1647, #1649, #1650, #1651, #1654 and #1668, in both directions | claude-opus-5-5, high | 5 |
| 4 | Shapes next to the other repairs merged after 2026-11-13 | claude-opus-5-5, high | 3 |
| 5 | The native grouping-sets path on a real PostgreSQL 17: ICU and non-deterministic collations, range and composite types, plans near the limit, savepoints under a time limit | claude-opus-5-5, high | 1 |
| 6 | The Rd topics, vignettes and README read as a contract, one executable check per behavioural sentence | claude-sonnet-5-5, high | 0 |
| 7 | Every Package condition reachable from a public call: class, blamed call, message and hint, consistency across inputs, timing | claude-sonnet-5-5, high | 4 |
| 8 | Arrow `RecordBatchReader` inputs, `R/backend-metadata.R`'s dtplyr admission and Arrow proxies, inputs above 300 rows, and the fourth-pass note's *Left for a decision* items | claude-sonnet-5-5, high | 3 |

The lane plan had been drawn from diff statistics and issue titles. Checked
against the snapshot, every named file and ADR existed, and one lane changed:
`R/margin-verbs.R` held no code — it is the roxygen source of
`man/margin_verbs.Rd` — so lane 2 read it as contract and took
`R/summarize_with_margins.R`, rewritten since the previous hunt, in its place.

Two pairs of the twenty-four candidates named one site and were checked
together, which left twenty-two. Each went through three checks. A refuter on
the other model from the finder (claude-fable-5-2 for a candidate found on
claude-opus-5-5, claude-opus-5-5 for one found on claude-sonnet-5-5, at high
effort, one fresh-context refuter per candidate) argued that the candidate was
not a defect, looking for the right native control and for an ADR, Rd sentence,
test or earlier decision that made the behaviour intended. A duplicate lookup
(claude-sonnet-5-5, high) searched the issue tracker, open and closed,
`investigation/`, `tests/testthat/` and `NEWS.md`. The coordinator
(claude-fable-5-2) replayed every candidate in a fresh R process, read the code
each mechanism names, replayed candidate answers in memory where a decision
depended on one, and decided.

Three things differed from that plan. The duplicate lookups of the first round
were run one per lane, seven in all, each told to treat its candidates
separately, because twenty-two candidates were about twice what the run had
been sized for. Five refuters ended on an API error and were run again on the
same model. And one dtplyr pair that had been merged turned out to hold two
defects (#1769 and #1770); its one refuter addressed both. A second round then
covered only the paths next to the confirmed findings; *Second round* says how
it was checked. In all the run used 12 investigators and 53 completed
verification agents, against a plan of about 25 of the latter.

Versions were R 4.6.2 on arm64 macOS, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3,
data.table 1.18.6.1, duckdb 2.0.1, RSQLite 3.53.3, RPostgres 1.4.10, DBI 1.3.0,
arrow 26.0.1.1, tidyr 1.3.2, rlang 1.3.0, vctrs 0.7.3, tidyselect 1.2.1, cli
3.6.6, sf 1.1.3 and haven 2.5.5. The previous hunt had run duckdb 1.5.5 and
arrow 25.0.1. SQL work used process-owned in-memory SQLite and DuckDB
databases, files under the session's temporary directory, and a temporary
PostgreSQL 17.11 cluster the maintainer had started, reached over a Unix
socket. Every PostgreSQL object was synthetic and lived in a schema the
investigation created and dropped; a listing of `hunt_%` schemas at the end
returned none. No existing database or outside data was read. DuckDB's `json`
and `icu` extensions could not be loaded.

## Method and independent expectations

Expected values never came from marginplyr internals or from agreement between
two marginplyr paths. Summaries and shares were compared with ordinary dplyr
summaries at each explicit grain, divided by hand and bound in Grouping-plan
order; PostgreSQL results with native dbplyr at each grain on the same
connection; a dtplyr step's typed plan with the plan of the same step
collected. The DuckDB preflight candidates were measured by value dependence:
whether a typed plan changed when only one data row of a file changed, with a
native lazy construction over an absent file and an explicit-schema reader as
controls. Refusal timing was measured from `last_sent_queries()` and from a
trace below the DBI generics.

Every difference on a backend was followed by the native control on that
backend before it was counted, and the refuter chose its control independently.
In the first round that step changed the standing of nine candidates: two were
rejected and seven were narrowed.

## Confirmed findings

Each script under `investigation/2026-11-26-second-eight-lane-workflow/` is run
from the repository root, prints the observed behaviour and exits zero.

| Boundary | Established effect | Script | Issue |
| --- | --- | --- | --- |
| A sorted summary with a share and a typed-missing label on a generic SQL backend | The statement ended with an `ORDER BY` over the private set identifier, which no operand selected, and `collect()` failed in the engine. | `generic-sql-sorted-share-typed-missing.R` | [#1768](https://github.com/sayuks/marginplyr/issues/1768) |
| A dtplyr join on differently named keys, or between tables that carry both key names | The key-type guard read dtplyr's `on` in the orientation of a full join only: the first was refused, and the second admitted an integer-against-double key, after which a typed plan chose other dimensions than the collected join's. | `dtplyr-join-key-orientation.R` | [#1769](https://github.com/sayuks/marginplyr/issues/1769) |
| A time limit expiring while a share request asks its dialect question | The package's handler consumed the limit and the verb returned; on SQLite "refuses" was recorded and a later share over a character source was calculated. | `share-dialect-question-time-limit.R` | [#1782](https://github.com/sayuks/marginplyr/issues/1782) |
| An Arrow share request with `.check_margin_label = TRUE` | The label collision query read the input before the Arrow share refusal, and a colliding value or a backend failure replaced that refusal. | `arrow-share-refusal-after-label-query.R` | [#1771](https://github.com/sayuks/marginplyr/issues/1771) |
| DuckDB 2.0's `read_single_csv_file()` | The reader was not on the preflight's list, and the zero-row metadata query sniffed the file. | `duckdb-read-single-csv-file.R` | [#1776](https://github.com/sayuks/marginplyr/issues/1776) |
| A backtick-quoted identifier in authored DuckDB SQL | The tokenizer did not read it as an identifier: a CSV relation, a sniffing reader and a caller macro passed the scan, and one safe statement was refused. | `duckdb-backtick-identifier.R` | [#1779](https://github.com/sayuks/marginplyr/issues/1779) |
| `compute()` of a DuckDB result of class `marginplyr_duckdb_env` | dbplyr's deprecated `cte` argument was refused with the unused-dots error. | `duckdb-env-compute-cte.R` | [#1777](https://github.com/sayuks/marginplyr/issues/1777) |
| A Package condition from the package's own `compute()` methods | The blamed call was the frame above `compute()`, `NULL` at top level, or the method's name. | `compute-refusal-blamed-call.R` | [#1783](https://github.com/sayuks/marginplyr/issues/1783) |
| A missing index column on SQLite's delayed-publication `compute()` path | The package's own check raised a bare `rlang::abort()` blaming an internal call, and a result with a pronoun-named column skipped the check. | `sqlite-compute-index-check.R` | [#1784](https://github.com/sayuks/marginplyr/issues/1784) |

The tickets of this note were published as independent repair slices, without
a confirmation checkpoint, as the maintainer had delegated. Those in this table
carry `bug` and `ready-for-agent`.

### A sorted share summary on a generic SQL backend

```r
summarize_with_margins(t, n = sum(v, na.rm = TRUE), p = share_of_total(n),
                       .grouping = rollup(a), .check_share_source = FALSE,
                       .margin_label = NULL, .sort = "last") |> collect()
#> Binder Error: Referenced column "..marginplyr_set_id_1" not found in FROM clause!
```

The connection was the forwarding `DBIConnection` subclass over in-memory
DuckDB that `tests/testthat/helper-generic-connection.R` defines, so the
portable path ran on an engine. The failing condition was a backend of kind
`"sql"` other than SQLite, a contextual share, a typed-missing label on at
least one dimension, two or more sets and a `.sort` other than `"none"`; it
held with `.id`, `rollup(a, b)`, `cube(a, b)`, `.by` and a mixed label list.
`.sort = "none"`, the default label, the share-less call, DuckDB natively,
SQLite and a data frame returned the hand-computed rows. Thirteen simulated
dialects rendered the same trailing `ORDER BY`; no MySQL, SQL Server, Oracle,
Snowflake or Redshift server was run.

By reading and by replay, `finalize_margin_operation()` orders the result and
drops the staged identifier, and its arm for `type_anchor_columns` then unions
a zero-row anchor in front of that ordered query, which dbplyr renders with the
`ORDER BY` trailing the compound. The refuter built the same composition
natively — an anchor unioned in front of a query arranged by a helper column
that was then removed — and it failed alike on the generic connection, DuckDB
and SQLite. The candidate's own control, a per-grain `union_all()` then
`arrange()`, had been the wrong kind of object. The finding is counted because
the caller wrote `.sort`, and the `arrange()`, the projection and the union are
all the package's. Unioning the anchor before ordering, replayed in memory by
the refuter, returned the expected rows under both sorts.

#1643's second pass had executed about 290 results on this connection; none
combined a typed-missing label with a share and a sort.

### A dtplyr join's key pair

`dtplyr:::step_join()` stores `parent = if (style == "left") y else x`,
`parent2` the other, and `on = if (style %in% c("left", "full")) on else
list(x = on$y, y = on$x)`. `dtplyr_join_set_types_match()` looks `on$x` up in
`parent` and `on$y` in `parent2`, which pairs the keys correctly for a full
join only. A trace showed the function consulted for all six styles, the
`setnames()`/`setcolorder()` wrappers of left, right and inner joins included.

With `by = c("a" = "b")` over plain integer keys, the left, right, inner, semi
and anti joins were refused and the full join admitted. With tables `x(a int,
b chr, k int)` and `y(b dbl, a chr)`, the guard compared `x$b` with `y$a`,
both character, and admitted every style but full; for the inner and right
joins the proxy typed `a` as double where `collect()` gave integer, and
`rollup(where(is.integer))` planned `(k) ()` on the step against `(a, k) (a)
()` on the collected frame. Swapping `on` for the non-full styles in memory
admitted the first shape in all six styles with equal plans and refused the
second in all six. Every dtplyr join under `tests/testthat/` used a same-name
key.

The duplicate lookup read the wrapped joins as bypassing the guard; the trace
and the in-memory replay showed otherwise.

### A time limit during the share dialect question

A 0.5 s round trip was simulated for the one statement by tracing the driver's
`dbSendQuery` method. Under `setTimeLimit(elapsed = 0.25, transient = TRUE)` a
numeric share on SQLite returned after 0.9 s with `share_dialect` recorded as
errored and handled and `share_dialect_control` as returned; a later share of
`max(code)` over `"10"`, `"20"`, `"30"` then returned 0.667, 1, 1, where both
calls are a `marginplyr_error` in a session without the limit. On PostgreSQL
17.11 inside `dbBegin()` the verb returned the same way, with the transaction
usable and no savepoint left. The same question written natively, with no
handler, ended with dbplyr's "Failed to collect lazy table ... reached elapsed
time limit" on both. Without any trace, the refuter saw 8 of 39 fresh in-memory
SQLite processes with limits of 4 to 80 ms return with "refuses" recorded, and
the finder's sweep of 37 fresh PostgreSQL processes returned normally in
several.

By reading, `probe_share_dialect_answer()` returns `"raised"` for any error
its `tryCatch()` takes, and a raised question with an answering control is
recorded as `"refuses"`. R delivers a time limit as an unclassed `simpleError`
and clears it once handled. Running the handler under `suspendInterrupts()`,
replayed in memory, ended the SQLite call with the time-limit error and left
the later character share refused; an interrupt at the same point already
propagated and recorded nothing.

Narrowed by the refutation, and not part of the ticket: a limit landing on the
control, on `DBI::dbIsValid()` or on the savepoint statements ended the call
with the "could not be asked" Package condition and recorded nothing, which the
database guide's "Handle any error from the call" covers; and a plain driver
error on the question alone was read as a refusal, which is the
transient-against-permanent distinction #198 put out of scope. With
`marginplyr.audit_sql = TRUE` the audit's render step consumed a limit once on
SQLite; it was not pursued.

### The Arrow share refusal after the label query

With `.check_margin_label = TRUE` and data holding `"Total"`, a share request
on an Arrow Table raised the label collision refusal, and following its remedy
led to the Arrow share refusal. With clean data one Arrow `summarise()` was
built and executed first, and an Arrow dataset whose Parquet file had been
removed raised Arrow's IOError in place of the share refusal. ADR 0005 places
this rejection "before query construction or execution" with the schema
snapshot as "the only backend read"; #1604 had moved the Grouping helper
refusals ahead of the same line. Moving the Arrow share block likewise was
replayed in memory by the refuter and changed one order among refusals that
read nothing: a share request that also labelled a list-typed dimension got the
share refusal first.

### The DuckDB preflight on DuckDB 2.0

`SELECT * FROM read_single_csv_file('<csv>')` gave the typed plan `(k)` for a
file whose one data row was `101` and `()` after that row was changed to text;
`read_csv()` was refused for both. The same held for the qualified name, a
CTE, a scalar subquery, a `sql()` fragment, a join input and a stored view, and
the explicit-schema form gave one plan. `duckdb_functions()` reported
`internal = TRUE` for all 98 table functions, so that flag did not mark the
function private. `read_single_json_file()` passed the preflight too; its
sampling could not be observed without the json extension.

`` FROM `p.csv` ``, `` TABLE `p.csv` ``, `` `read_csv_auto`('p.csv') ``, a
caller macro written `` `hidden_first`(v) `` and `` `query`('...') `` each gave
`(k)` then `()`, or `(v, y)` then `(v)`, and were refused in their
double-quoted or bare spelling. ``WITH `p.csv` AS (SELECT 1 AS k) SELECT *
FROM "p.csv"`` was refused where the double-quoted CTE was admitted. A view
created with backticks was safe, since DuckDB 2.0.1 stores its definition with
double quotes. By reading, `duckdb_metadata_tokens()` has no backtick token.
DuckDB 1.5.5 was not run for either finding.

### `compute()` of the package's own result classes

`compute(x, cte = TRUE)` on a `marginplyr_duckdb_env` expansion raised
"Arguments in `...` must be used", where the same verb's result without a
`.env` column computed with dbplyr's deprecation warning and
`sql_options = dbplyr::sql_options(cte = TRUE)` worked. The class survived
`select(-.env)`, so that result was refused too. #1644 had set the standard for
the SQLite sibling. A non-logical `temporary` reaching `if (!temporary)` in the
same method fails natively as well and was folded into the ticket, not counted.

For the `temporary`, `overwrite`, `analyze` and `in_transaction` checks, the
rowid-alias refusal and the index checks of both methods, `conditionCall()` was
`NULL` at top level and the caller's enclosing function inside one; native
`compute()` of an ordered summary blamed the `compute()` call. The temp-shadow
refusal named `compute.marginplyr_sqlite_typed_result(x, ...)`. By reading,
`call = rlang::caller_call()` is written inside the methods.

`compute(r, name = "t", temporary = TRUE, indexes = list("zz"))`, where `t`
resolves to an existing relation, raised an `rlang_error` blamed on
`sqlite_check_compute_indexes(anchor, indexes, "indexes")`. The delegated
paths, which propagate dbplyr's condition as ADR 0031 intends, were left out of
the ticket.

## Filed with a decision

Each of these needed a choice the contract did not make. The coordinator made
it from the ADRs, the Rd, the pinned tests and earlier tickets, replayed the
candidate answer in memory where one existed, and wrote the decision on the
ticket. The first seven carry `bug` or `enhancement` with `ready-for-agent`,
the next four `documentation` and `ready-for-agent`, and the last was closed
`wontfix`. #1788 began as a wording point the documentation lane reported
without counting; the coordinator raised it and it went through a refuter on
claude-opus-5-5 and a duplicate lookup like any candidate.

| Boundary | Established effect | Decision | Script | Issue |
| --- | --- | --- | --- | --- |
| A dtplyr join whose root tables hold a classed non-key column | Refused as unsafe metadata, where the zero-row types were the inputs'. | Hold only the paired key columns to the plain-vector rule. | `dtplyr-join-classed-column.R` | [#1770](https://github.com/sayuks/marginplyr/issues/1770) |
| A share of an `across()` output whose `.fns` is a literal `c()` | Refused as an unknown summary, where `list()` was accepted. | Keep the refusal; say which spellings name a source and hint at `list()`. | `share-source-across-c-fns.R` | [#1772](https://github.com/sayuks/marginplyr/issues/1772) |
| A share of a later `across()` output read through a function argument | Refused while the argument was an unforced promise; for a literal argument, refused on the first call of a line and accepted from the second. | Read a constant promise as a constant; keep not forcing other arguments, and say so in a hint. | `share-source-across-promise-argument.R` | [#1773](https://github.com/sayuks/marginplyr/issues/1773) |
| A share source reading a column of an earlier unnamed data-frame summary | Accepted, where the same read of a named summary was refused. | Refuse, at run time, from the frame's produced names. | `share-source-frame-summary-column.R` | [#1774](https://github.com/sayuks/marginplyr/issues/1774) |
| A bare reference to an earlier summary output named like a bound Grouping specification | Refused as a specification captured as a summary. | Compute it on data-frame inputs; keep the refusal on lazy ones. | `grouping-spec-summary-earlier-output.R` | [#1778](https://github.com/sayuks/marginplyr/issues/1778) |
| A caller DuckDB macro named after an operator or an implicitly called function | Bound by the zero-row metadata query with no call head for the scan to inspect. | Refuse typed metadata while the catalog holds a caller macro of a listed implicit name. | `duckdb-implicit-macro-name.R` | [#1780](https://github.com/sayuks/marginplyr/issues/1780) |
| A malformed Grouping helper call on DuckDB | Refused after the catalog query and the typed zero-row read. | Move the checks that need only the call ahead of metadata acquisition. | `duckdb-call-decidable-refusal-after-metadata.R` | [#1785](https://github.com/sayuks/marginplyr/issues/1785) |
| A name selection computed inside a helper, such as `all_of(sort(dims))` | Sent DuckDB's two metadata statements, against "Selecting columns by name sends no query". | Qualify the sentence. | `inspect-grouping-name-selection-query.R` | [#1775](https://github.com/sayuks/marginplyr/issues/1775) |
| DuckDB's `histogram()` aggregate | Refused as a macro call, a system table macro sharing its name. | Keep the position-blind name match; document the refusal. | `duckdb-histogram-aggregate-refusal.R` | [#1781](https://github.com/sayuks/marginplyr/issues/1781) |
| "No field of the condition names the argument it refuses" | Share refusals with their own subclass carried such fields. | Reword the sentence; keep the fields. | `condition-fields.R` | [#1786](https://github.com/sayuks/marginplyr/issues/1786) |
| The pointer to an "eight-case factor table" in `vignettes/grouping_identity.qmd` and ADR 0012 | `?margin_verbs` held no such table. | Reword the pointer and amend the ADR; do not restore the table. | none | [#1788](https://github.com/sayuks/marginplyr/issues/1788) |
| The count in the grouping-set limit refusal | Inexact once the count was not representable as a double. | None; closed `wontfix`. | `grouping-set-count-digits.R` | [#1787](https://github.com/sayuks/marginplyr/issues/1787) |

Three of these decisions are ones a maintainer could reasonably make the other
way:

- #1774 turns calls that returned plain dplyr's values into refusals. The Rd
  sentence is unconditional, the named twin is refused on the same local frame,
  and `.unpack = flag` already refused this read at run time.
- #1773 keeps a wrapper's variable argument refused, with a hint. Forcing the
  argument whenever the earlier summaries are proven pure would make every
  wrapper work, and was not chosen because #1428's criterion and a pinned test
  say an unevaluated argument is not evaluated early.
- #1781 keeps refusing a call that reads no source, because the repair would
  make the table-macro refusal depend on the relation scan.

### What the replays established

- **dtplyr joins with classed columns.** The refuter measured the zero-row
  evaluation as faithful for every non-key column in all six styles. Requiring
  paired columns identical in `typeof` and attributes, replayed in memory,
  accepted the `Date`-carrying join with proxy equal to collected and kept the
  three refusals `test-grouping-backends.R` pins. The decision is narrower on
  keys, which stay plain.
- **`c()` as a function list.** Natively `c(s = sum, list(a = mean, b = max))`
  names `x_s, x_a, x_b`, `c(p = list(...))` names `x_p.a, x_p.b`, and
  `c(s = sum, use.names = FALSE)` names `x_1`; dbplyr ignores `.names` for
  `c()` and dtplyr refuses it. Widening the recogniser to `c()` in memory
  computed the core form and broke four neighbours: a binder error on DuckDB
  with `.names`, two false `.id` collisions, and an untyped `set_names()`
  error.
- **An unforced argument.** The coordinator ran one source line,
  `function() report(d, "x")`, four times: refused, then accepted three times.
  Compiled code passes a constant argument as a value and interpreted code as a
  promise. Reading a lazy binding whose `substitute()`d expression is an
  unclassed atomic constant as that constant, in memory, accepted the line on
  every call, left a variable argument refused, and left the pinned
  `delayedAssign()` binding refused with zero reads. Following a promise to the
  caller's variable needs the promise's environment, for which base R has no
  accessor and rlang 1.3.0 exports none.
- **A frame summary's column.** Calling the existing `check_share_alias_reads()`
  with the frame's produced names, in memory, refused six spellings and the
  shadowing case, and left the pinned `tibble::tibble(s = NULL)` calls, a frame
  written after the source, an unread frame and share-less calls unchanged.
- **A name shared with a specification.** Skipping a bare symbol carried by a
  strictly earlier named summary computed `spec = 2, 1, 3` and `again = 2, 1,
  3` on data frames and data.tables. On dtplyr the same change returned the
  specification spliced in as `type` and `args` columns, which is why the
  decision stops at data-frame inputs.
- **Implicitly bound macro names.** With a caller macro named `count_star`,
  `count(t, g)` gave `(n)` then `()`; with one named `+`, `mutate(y = v + 1L)`,
  its authored form and a stored view gave `(v, y)` then `(v)`. Refusing while
  a caller macro's name equals any built-in function name, in memory, also
  refused `select(t, g, v)` under a caller macro named `lower` that the query
  never reached.
- **`histogram()`.** A positional match, in memory, admitted the aggregate's
  seven spellings and kept seventeen relation-position forms of the table
  macro refused. The existing gap in the relation scan (a parenthesised
  `SUMMARIZE FROM`) admitted the same two rows before and after.
- **Name selections.** Adding `sort` to the allow-list, in memory, sent no
  query with stock `sort()` and, with a caller `sort.character` that reads the
  selection data, silently returned `()` where native tidyselect gives
  `(region, store)` — #995 restored.
- **The factor table.** #1720 removed the table on purpose ("drop the factor
  table whose allowed rows its prose already states", commit `473e8dd9`), and
  #1722 removed the matching pointer from the Get started vignette the same
  day. The refuter ran all eight cases under both `.check_margin_label` values:
  only `NA_character_` with a declared `NA` level was refused, as the prose
  says.
- **Count digits.** 3^40 was stated as 12,157,665,459,056,928,768 against
  12,157,665,459,056,928,801. Counts exactly representable were stated
  correctly at any size, every `cube()` up to 1,023 columns included, and
  `data.table::CJ()` printed the identical figure for the identical product.

## Rejected candidates

- Under `.duplicates = "keep"` or `"error"`, a product of specifications whose
  leading factors already passed the grouping-set limit sent DuckDB's catalog
  query and zero-row read for a later `where()` factor before the size refusal,
  and an invalid `marginplyr.max_grouping_sets` value was refused after the
  same statements. PR #1748's review, round 5, had the shape in front of it
  ("`cube()`, `rollup()` and products read all their units first") and left the
  metadata queries as they were, on `?margin_verbs`' "Only a selection that
  needs a lazy input's column types ... sends a query first"; ADR 0046's
  amendment claims the early stop for a `grouping_sets()` union only; and the
  upfront option read had been removed in round 2 so that a specification's own
  refusals keep precedence, which a test pins. Counting the prefix first, in
  memory, changed 19 of 22 probed outcomes.
- With `marginplyr.audit_sql = TRUE`, a severity-free condition signalled while
  dbplyr translated left the recorded row with `sql` `NA` and no condition
  summary, where `dbplyr::sql_render()` returned the statement. #891 keeps such
  a condition local, which in R needs an unwind; letting it propagate, in
  memory, delivered three copies to the caller with the audit on against none
  with it off; `test-sent-query-general-conditions.R` pins the empty summary.
  This settles the seventh *Left for a decision* item of the fourth-pass note.

## Second round

Four investigators took only the paths next to the confirmed findings, and
every candidate got its own refuter on the other model and its own duplicate
lookup:

| Lane | Starting point | Model and effort | Candidates |
| --- | --- | --- | --- |
| A | Every handler in `R/` that turns a condition into a value, a verdict or another condition, under a time limit, a caller's error and an interrupt (next to #1782) | claude-opus-5-5, high | 5 |
| B | The portable path's final statement, executed on the generic connection over every arm of `finalize_margin_operation()` (next to #1768) | claude-opus-5-5, high | 1 |
| C | dtplyr's admission logic against what dtplyr stores for each step class (next to #1769 and #1770) | claude-sonnet-5-5, high | 1 |
| D | What the local planner believes about names before the call runs, and every decision that reads the state of a binding (next to #1772 to #1774 and #1778) | claude-opus-5-5, high | 3 |

Two candidates named one site and became one ticket. One further finding was
the coordinator's: a side observation in a refutation, replayed and then put
through a refuter on claude-opus-5-5 and a duplicate lookup (#1794).

| Boundary | Established effect | Decision, where one was needed | Script | Issue |
| --- | --- | --- | --- | --- |
| A dtplyr summary whose every summary expands to no output | The row of the Grand total set was missing, with no condition. | | `dtplyr-zero-output-summary-grand-total.R` | [#1794](https://github.com/sayuks/marginplyr/issues/1794) |
| A bare `.grouping` name that is bound, and a column, and fails to evaluate | The bare-column refusal replaced the caller's error, a time limit included, with no parent. | | `grouping-bare-column-refusal-replaces-error.R` | [#1789](https://github.com/sayuks/marginplyr/issues/1789) |
| A summary written as a bare name that is a caller's unevaluated argument | The #1699 check evaluated it early, twice when it failed, and consumed a time limit. | Do not read a promise or an active binding on a data-frame input; on a lazy input read it and catch nothing. | `grouping-spec-summary-check-reads-binding.R`, `grouping-spec-summary-check-time-limit.R` | [#1790](https://github.com/sayuks/marginplyr/issues/1790) |
| An unsorted share summary on DuckDB over a window-ordered input | The result recorded no window ordering, and a later window function ran in physical order. | | `duckdb-share-window-order.R` | [#1791](https://github.com/sayuks/marginplyr/issues/1791) |
| A dtplyr `mutate()` whose arithmetic uses parentheses, `^`, `%%` or `%/%` | Refused as unsafe metadata. | Admit the four heads under the existing checks. | `dtplyr-mutate-arithmetic-operators.R` | [#1792](https://github.com/sayuks/marginplyr/issues/1792) |
| A bare package function in `across()` whose lazy-load promise is unforced | Its output names were unknown, so a share was refused until the session had evaluated the function. | Read a lazy-load promise held by a namespace, `package:` or imports environment. | `across-lazy-load-function-name.R` | [#1793](https://github.com/sayuks/marginplyr/issues/1793) |
| `across(<cols>, list())` | Predicted to produce `<col>_`: a share of it failed at execution, and three valid calls were refused on lazy inputs. | | `across-empty-function-list.R` | [#1795](https://github.com/sayuks/marginplyr/issues/1795) |
| A time limit during the SQL audit's diagnostic render | Recorded as a handled render condition; the call ran on unbounded. | Run the render under `suspendInterrupts()`. | `audit-render-time-limit.R` | [#1796](https://github.com/sayuks/marginplyr/issues/1796) |
| The session memo of which kinds a nested position admits | Any error during its first derivation was stored as "not admitted". | Catch only `marginplyr_error`. | `nested-kinds-memo-time-limit.R` | [#1797](https://github.com/sayuks/marginplyr/issues/1797) |

All carry `bug` and `ready-for-agent`.

### The Grand total row on dtplyr

`summarize_with_margins(lazy_dt(d), across(starts_with("zzz"), sum), .grouping
= rollup(g))` collected two rows, `a` and `b`; a data frame, a data.table,
DuckDB, SQLite and Arrow returned `a`, `b`, `Total`, and so did dtplyr with no
summary at all, with `n = n()` beside the empty `across()`, and with
`.margin_label = NULL`. `rollup(g, h)` lost Total/Total, `cube(g, h)` returned
seven of eight rows, and `.id` lost set 2. The Rd says of dtplyr's zero-row
answer for a column-less summary that "Anything that puts a column in the
result -- one summary, one fixed key, one Margin dimension, or `.id` -- ends
the difference".

Native dtplyr answers an ungrouped zero-output `summarise()` with no rows, and
the candidate first offered that as the control. The refuter showed it stops a
step short: the same summary followed by `mutate(g = "Total")` returns one row,
because data.table creates a row when a scalar is assigned to a zero-column
table. By reading, `summarize_margin_union()` adds its `n()` placeholder on a
non-SQL backend only when no summary dot was written, and
`label_margin_branch()` writes the dtplyr label as `rep(label, .N)`, which has
length zero on `d[, 0L]`. Adding the placeholder for a dtplyr branch of the
empty set, replayed in memory by the coordinator and by the refuter, returned
every row and left the pinned no-summary, no-dimension call at zero rows.

### Conditions taken by a handler that was not written for them

Lane A enumerated 141 handler sites and read about 60. Beside #1782, three
consumed a time limit so that a call returned: the #1699 check
(`rlang::env_get()` inside `tryCatch(error = )`, returning after about 1 s
under a 0.2 s limit on local, dtplyr, Arrow, DuckDB and SQLite inputs), the
audit's diagnostic render (an error at 1.2 s with the audit off against a
return after 9.5 s with it on, in the coordinator's traced DuckDB replay; 1.59 s
against 12.80 s in the refuter's untraced SQLite case), and the nested-kinds
memo, where the limit was also stored as an answer for the session. The last
needed a delay injected into `rlang::quo()`: the refuter found no poisoning in
78 fresh processes without it.

For the #1699 check, lane D independently measured what the read costs without
any limit: an argument written `total = w` was evaluated when an earlier
summary failed, before the earlier summaries when they succeeded, and in calls
the package then refused; a failing one ran twice; a warning arrived as a bare
`simpleWarning` blamed on the internal `env_get()` call. Skipping lazy and
active bindings, replayed by both refuters, removed all of it and cost the
`.grouping =` hint for a specification passed through a wrapper's unevaluated
argument, which on DuckDB falls through to a dbplyr assertion; hence the
decision's split by input kind.

The bare-column refusal was the same family without a memo: with a wrapper
parameter named like a column, the builder's `simpleError`, a time limit and a
raising active binding were each replaced by "Column `store` must be wrapped in
a constructor". Firing the refusal only when a written name is unbound,
replayed by the refuter, let all of them through and kept every pinned shape.

### Names the planner had wrong

In a fresh process, `across(c(x, y), n_distinct, .names = "{.col}_n")` followed
by `share_of_total(y_n)` was refused, and accepted once `n_distinct(1)` had
run; `sd`, `var`, `IQR`, `mad`, `first`, `last` and `nth` were unforced at
startup too, and on Arrow `across(c(a = x), sd)` stayed refused for a whole
session. By reading, `known_across_function_names()` reads a lazy binding with
`substitute()` and finds a `lazyLoadDBfetch()` call. #1685 had met the
mechanism in the tests and kept the refusal on the premise, written in
`helper-test-environment.R`, that a user's session never meets such a promise.

`paste("x", character(0), sep = "_")` is `"x_"`, so `across(x, list())` was
predicted to produce `x_`; the `.names` branch of the same function returns
`character()` for no functions.

### The window ordering after a DuckDB share, and dtplyr arithmetic

`bind_duckdb_share_values()` returns `dplyr::tbl(con, sql, vars = vars)` over
the `AS MATERIALIZED` CTE of #1141 and, unlike `sql_expand_shared_result()`,
does not restore the window ordering afterwards. Restoring it in memory left
the rendered statement of 48 results identical. Lane B executed about 8,000
calls on the generic connection over every arm of
`finalize_margin_operation()` with the #1768 condition excluded and found no
other statement the engine refused, and no wrong value, type or order.

`dtplyr_value_stable_arithmetic()` admits calls headed by `+`, `-`, `*` and
`/`. Lane C compared about 4,500 random pipelines on the proxy and 1,000 on
the verbs with `collect()`; no admitted step differed, and the refused
schema-preserving steps were the four arithmetic heads.

### Second-round candidate not counted

A time limit expiring while the zero-row proxy of a metadata-safe dtplyr step
was built became "Can't safely determine column metadata for `.data` from this
dtplyr step", with no parent. The call ended with an error, nothing returned
and nothing was recorded, which is the case #1782 set aside on the guide's
"Handle any error from the call rather than matching the message"; untraced,
the refuter saw it only with limits of 1 to 19 ms. Carrying the caught
condition as the refusal's parent, as the DuckDB arm of the same file does, is
a cheap hardening that was not filed.

## Reported and not counted

- `print(grouping_sets(!!rollup(a), b))` showed `grouping_sets(<mrgn_gr_>, b)`,
  where `?grouping_set` says printing "shows the constructor call as written,
  nested constructors included". Not run through the checks.
- A Grouping specification passed positionally to `expand_with_margins()`, a
  nesting verb or `inspect_grouping()` landed in `.by` and got tidyselect's
  error; #1699 scoped its hint to `summarize_with_margins()`'s dots.
- `?margin_verbs` says a list-typed dimension under dtplyr and Arrow "accepts
  only `NULL` or `NA_character_`", and a summary over one failed at `collect()`
  with the backend's own error; native dtplyr and Arrow `group_by()` on a list
  column failed identically, and the expansion worked.
- On PostgreSQL a 4,096-set native summary took 0.75 s without `.id` and about
  20 s with `.id`, a share or `grouping_id()`; the time was not split between
  building the `CASE` of 4,096 branches in R and executing it.
- Under `.duplicates = "drop"`, a 2,048-set plan written with 1,600 repeated
  union branches took 2.4 GB; ADR 0046's amendment states that cost model.
- Arrow typed an `int64` column as integer in the schema proxy where
  `collect()` gave `integer64` beyond the int32 range; arrow's own
  `select(where(is.integer))` on the query agreed.

## The fourth-pass note's open items

Lane 8 re-established the eight *Left for a decision* items of
`margin-verbs-record-fourth-pass-hunt-2026-11-12.md` at the snapshot. The lazy
`across(w, ~ max(.x))` after a share named `w` read the input column as native
dbplyr does, and the Rd's sentence on a lazy input's alias limitations covers
it. The loss of Margin order after `select()` or `pull()` on DuckDB did not
reproduce on duckdb 2.0.1. `compute()` of a DuckDB expansion with a `.data`
column failed as native `compute()` of such a table does. A helper spelling
inside a formula used as data was still rewritten; dplyr itself rewrites
`quote(pick(w))`. The two wordings of the share-source refusal no longer
differed. `.keep = c(k = TRUE)` was accepted and `1L` refused, and a `.key`
with "bytes" encoding raised the same base error as `tidyr::nest()`. The
severity-free render condition is the second rejected candidate above. dtplyr
refused every Margin summary result as an input, shares or not, which is the
documented aggregate refusal. None was filed.

## Covered without a finding

Counts are the investigators' reports.

- Share analysis: about 120 share-source expressions and 35
  ordinary-after-share expressions against plain dplyr at each grain; 53
  `across()`-source cases; 25 wrapper shapes with 15 promise and position
  variants; 29 selection expressions on six backends.
- Planning and the limit: 34 option value kinds; about 520 generated plans and
  240 verb, backend and policy cases at the boundary; about 4,500 generated
  plans under three duplicate policies against an independent oracle; 27
  refused-plan time and memory runs; 288 identity comparisons and 2,568 Margin
  order results on six backends; DuckDB and PostgreSQL at exactly 4,096 sets;
  nine recorded-against-traced shapes.
- DuckDB preflight: DuckDB 2.0.1's 94 table functions, 4 system table macros,
  115 scalar macros, 508 keywords and `read_csv`'s 56 parameters against what
  the scan models; 60 authored shapes and 60 dbplyr pipelines without a caller
  macro; about 25 lexical spellings of a CSV relation, 26 relation positions
  and 14 replacement-scan shapes.
- Fix neighbours: about 1,600 lazy comparisons beside #1640, #1641 and #1643;
  about 60 `.grouping` and dots spellings beside #1699 and #1700; 70 SQLite
  result method calls beside #1644 and #1645; 60 injected-pronoun cases beside
  #1665.
- PostgreSQL: about 1,200 summary, expansion and share results against native
  dbplyr per grain; eight column types (ranges, a multirange, a composite, a
  domain, an enum, an array) under three label kinds; two non-deterministic and
  two deterministic ICU collations as dimension, `.by` key, share, sort,
  label-check, expansion and `compute()` roles; about 80 time-limit placements
  and eight interrupt and error injections into the probe and savepoint path,
  with `statement_timeout` and `lock_timeout` inside caller transactions; 948
  regression cases for #1640 and #1641.
- Documentation: every Rd topic, the five vignettes, the README and `NEWS.md`,
  with about 400 probes; no behavioural sentence that could be made executable
  failed.
- Refusals: about 120 of 178 `abort_marginplyr()` sites reached by about 650
  calls on two to eight input kinds, with class, blamed call through five call
  shapes, about 45 hints executed, and statements traced below DBI for about 90
  mistakes.
- Inputs: `RecordBatchReader` in about 40 shapes over four verbs, with the
  reader delivering all batches after every refusal and inspection; about
  1,500 generated dtplyr `mutate()` chains and 130 pipelines against
  `collect()`; 27 Arrow physical types under eight typed selectors; summaries
  at 20,000 to 1,000,000 rows on seven backends against per-grain native
  summaries.

## Not covered

No MySQL, MariaDB, SQL Server, Oracle, Snowflake or Redshift server was run,
and no DuckDB other than 2.0.1, so whether 1.5.5 has backtick identifiers or
`read_single_csv_file()` is unknown. The json extension could not be loaded, so
no JSON reader, `read_single_json_file()` included, was executed; nor were
extension readers that infer types from rows. Of the 141 handler sites lane A
listed, about 80 were not read, and the SQLite temporary-table and Arrow
warning handlers were not run under a limit. PostgreSQL expansions above 4,096
sets, a remote server with real latency, and transactions begun with a raw
`BEGIN` were not tried. The share dependency walk's environment-manipulation
shapes beyond the tables of #1676 to #1690, the dtplyr `.BY` and `.N` measure
walk, and grouped or row-wise inputs combined with shares were not examined.
Refusals were not checked on live PostgreSQL, and about 58 `abort_marginplyr()`
sites were not reached. Nesting, shares and expansions were not run at 10^6
rows. The rendered vignette pages were not read, only their sources run. The
duplicate lookups of the first round were batched by lane.
