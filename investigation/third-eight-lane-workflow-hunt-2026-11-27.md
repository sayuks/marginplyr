# Third eight-lane workflow hunt: remaining handlers, share analysis after its repairs, DuckDB versions and JSON, repair neighbours, live PostgreSQL, refusal precedence, copied vocabularies, scale and display

Investigated: 2026-11-27
Repository snapshot: `56c7679f35b266cb4c598afffa4fd63ed84d3cba`

The investigation ran against freshly fetched main in one worktree. No package
implementation, package test, dependency, public specification or generated
documentation was changed, and no repair was implemented. The snapshot was
built outside the checkout and installed into a private library, and every
investigator observed that installed build.

The starting points were chosen not to overlap those of
`eight-lane-workflow-hunt-2026-11-13.md` and
`second-eight-lane-workflow-hunt-2026-11-26.md`. Reading the documentation as a
contract, a value sweep on PostgreSQL, `RecordBatchReader` inputs, the
fourth-pass note's open items, generated differential cases, mutants, the
simulated dialects and the column-class axis, which those hunts had run, were
left out. Eight fresh-context investigators worked read-only and reported
before any candidate was shared:

| Lane | Starting point | Model and effort | Candidates |
| --- | --- | --- | --- |
| 1 | Every condition handler in `R/` the previous hunt's second round had not read, and the `suspendInterrupts()` regions, under a time limit, an interrupt, a caller's error and a warning | claude-opus-5-5, xhigh | 5 |
| 2 | `R/summary-selections.R` and `R/share.R` as they stood after #1772 to #1774, #1778, #1790, #1793 and #1795, with the environment-manipulation shapes outside the tables of #1676 to #1690 and grouped inputs combined with shares | claude-opus-5-5, xhigh | 5 |
| 3 | The DuckDB preflight on duckdb 1.5.6 and 2.0.1, and with the json extension loaded on 2.0.1 | claude-opus-5-5, high | 3 |
| 4 | Shapes next to the backend-side repairs #1768 to #1770, #1777, #1783, #1784, #1791, #1792 and #1794 | claude-opus-5-5, high | 3 |
| 5 | A real PostgreSQL 17, only what the previous hunt listed as not covered: refusals, #1782's repair, raw `BEGIN`, expansions above 4,096 sets, and where the time of a 4,096-set plan goes | claude-opus-5-5, high | 2 |
| 6 | Refusal precedence: calls holding two reasons to refuse them, per input kind and verb | claude-sonnet-5-5, high | 0 |
| 7 | Every hard-coded list in `R/` that copies a dependency's vocabulary, against the installed dependency | claude-sonnet-5-5, high | 6 |
| 8 | Nesting, shares and expansions at 10^6 rows; `print()`, `format()` and `str()`; a Grouping specification passed by position | claude-sonnet-5-5, high | 2 |

The lane plan had been drawn at this snapshot from file names only. Checked
against it, every named file and ADR existed and no lane was regrouped.
`R/margin-verbs.R`, `R/marginplyr-conditions.R`, `R/marginplyr-package.R` and
`R/retail_sales.R` held roxygen only and were read as contract. Two counts in
the plan were wrong and the lanes corrected them: a plain grep gave 95 handler
lines in 18 files, of which lane 1 found 76 outside roxygen, and of the sixteen
`suspendInterrupts()` lines only three were PR #1799's; a parse-level scan gave
180 `abort_marginplyr()` call sites where the plan said 186.

Each of the twenty-six candidates went through three checks. A refuter on the
other model from the finder (claude-fable-5-2 for a candidate found on
claude-opus-5-5, claude-opus-5-5 for one found on claude-sonnet-5-5, at high
effort, one fresh-context refuter per candidate) argued that the candidate was
not a defect, looking for the right native control and for an ADR, Rd sentence,
test or earlier decision that made the behaviour intended. A duplicate lookup
(claude-sonnet-5-5, high, one agent per lane, each told to treat its candidates
separately) searched the issue tracker, open and closed, `investigation/`,
`tests/testthat/` and `NEWS.md`. The coordinator (claude-fable-5-2) replayed
every candidate in a fresh R process, read the code each mechanism names,
replayed candidate answers in memory where a decision depended on one, and
decided. Two candidates from different lanes named one cause and one repair
(`eval.parent()`); each had its own refuter, both let it stand, and they became
one ticket. A second round then covered only the paths next to the confirmed
findings; *Second round* says how it was checked.

The first-round verification stopped once on an account usage limit with ten
of twenty-six refutations returned, and was resumed from its journal; no
first-round refuter needed its fallback model. In all the run used 12
investigators and 53 completed verification agents, with 8 further refuters
for the second-round re-run described under *Second round*, against a plan of
about 50 agents: forty-two candidates were about twice what the run had been
sized for.

Versions were R 4.6.2 on arm64 macOS, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3,
data.table 1.18.6.1, duckdb 2.0.1, RSQLite 3.53.3, RPostgres 1.4.10, DBI 1.3.0,
arrow 26.0.1.1, tidyr 1.3.2, rlang 1.3.0, vctrs 0.7.3, tidyselect 1.2.1, cli
3.6.6, sf 1.1.3 and haven 2.5.5. Two things no earlier hunt had run were
available from a local cache: duckdb 1.5.6, put in front of the library path
with the snapshot's own build of marginplyr, and DuckDB 2.0.1's json extension,
loaded by absolute path. No json binary matched 1.5.6. SQL work used
process-owned in-memory SQLite and DuckDB databases, files under the session's
temporary directory, and a temporary PostgreSQL 17.11 cluster the maintainer
had started, reached over a Unix socket. Every PostgreSQL object was synthetic
and lived in a schema or session temporary table the investigation created and
dropped; a listing of `hunt_%` schemas after each phase returned none. No
existing database or outside data was read.

## Method and independent expectations

Expected values never came from marginplyr internals or from agreement between
two marginplyr paths. Summaries and shares were compared with ordinary dplyr
summaries at each explicit grain, divided by hand and bound in Grouping-plan
order; PostgreSQL results with native dbplyr on the same connection; a nested
verb's value with the nested call evaluated alone; a share source's read with
plain `dplyr::summarise()` in which the share was an ordinary earlier summary
of the same name. The DuckDB preflight candidates were measured by value
dependence: whether a typed plan changed when only one data row of a file
changed, with a native lazy construction over an absent file and an
explicit-schema reader as controls. Statements sent before a refusal were read
from `last_sent_queries()` and from a trace below the DBI generics.

Every difference on a backend was followed by the native control on that
backend before it was counted, and the refuter chose its control independently.
In the first round that step changed the standing of eleven candidates: three
were refuted and eight were narrowed, two of those to documentation. In one
case it widened a candidate: the refuter of the `filter_out()` gap found the
shapes in which the result was a wrong value and not a refusal.

## Confirmed findings

Each script under `investigation/2026-11-27-third-eight-lane-workflow/` is run
from the repository root, prints the observed behaviour and exits zero. A
script that needs the json extension or the PostgreSQL cluster says so and runs
nothing where they are absent.

| Boundary | Established effect | Script | Issue |
| --- | --- | --- | --- |
| A deprecation signalled while a constructor argument is selected | The handler read every name in the argument and forced a wrapper's lazy argument named like a column, which tidyselect never evaluates; a failing one failed a call that returns natively. | `selection-deprecation-handler-forces-lazy-binding.R` | [#1803](https://github.com/sayuks/marginplyr/issues/1803) |
| A share `across()` selection written `all_of(c("revenue", "nope"))` | Refused as though the eligible `revenue` were unavailable; a helper's own subscript error inside the selection lost its condition. | `share-selection-all-of-missing-name.R` | [#1804](https://github.com/sayuks/marginplyr/issues/1804) |
| A time limit during the destination-name probe of SQLite `compute()` | The conservative arm of the probe took the limit as its answer and the call returned. | `sqlite-compute-name-probe-time-limit.R` | [#1805](https://github.com/sayuks/marginplyr/issues/1805) |
| A Margin verb refused while an enclosing Margin verb is running it | The refusal was blamed on the enclosing verb's call, whose arguments were valid. | `nested-margin-verb-blamed-call.R` | [#1806](https://github.com/sayuks/marginplyr/issues/1806) |
| `eval.parent()` in a summary on a local input | Not placed by the share dependency walk: a later summary read an earlier share's name unseen and returned a same-named input column's aggregate, and a share source reading an earlier alias was accepted. | `share-walk-eval-parent.R`, `share-walk-eval-parent-source.R` | [#1808](https://github.com/sayuks/marginplyr/issues/1808) |
| A wrapper's constant argument read by a later `across()` whose outputs are unknown for another reason | The refusal carried the unevaluated-binding hint on the interpreted first call only, and its `force()` remedy could not help. | `unevaluated-binding-hint-constant-argument.R` | [#1811](https://github.com/sayuks/marginplyr/issues/1811) |
| The json extension's `json_execute_serialized_sql()` | It binds SQL given as a value and was not on the preflight's opaque list; the zero-row metadata read sampled the file. | `duckdb-json-execute-serialized-sql.R` | [#1812](https://github.com/sayuks/marginplyr/issues/1812) |
| A `.geojson` or `.geojsonl` file name as a relation, with the json extension | A JSON replacement scan the suffix rule did not know; typed metadata followed a data row. | `duckdb-geojson-replacement-scan.R` | [#1814](https://github.com/sayuks/marginplyr/issues/1814) |
| A caller DuckDB macro named `factorial`, `regexp_matches` or `trunc` | Bound from postfix `!`, `~` and `INTERVAL (expr) unit` with no call head, and missing from the list of implicitly bound names. | `duckdb-implicit-names-factorial-regexp-matches.R` | [#1818](https://github.com/sayuks/marginplyr/issues/1818) |
| `filter_out()` or `nest_by()` nested in a summary expression | Missing from the independent data-mask registry: a nested `if_any(everything(), ...)` or `across(everything())` was expanded against the outer summary and returned a wrong value. | `nested-filter-out-data-mask.R` | [#1819](https://github.com/sayuks/marginplyr/issues/1819) |
| `rlang::env_parent()` and `rlang::env_get()` with their default `env` | Read as reaching no mask frame: a share source reading an earlier alias was accepted, and a summary reading an earlier share was dropped from the result without a condition. | `share-walk-env-parent-default.R` | [#1821](https://github.com/sayuks/marginplyr/issues/1821) |
| `print()` of a specification holding an injected nested specification | The nested specification printed as `<mrgn_gr_>`. | `print-injected-grouping-specification.R` | [#1823](https://github.com/sayuks/marginplyr/issues/1823) |

The tickets of this note were published as independent repair slices, without
a confirmation checkpoint, as the maintainer had delegated. Those in this table
carry `bug` and `ready-for-agent`.

### Planning code that read a binding tidyselect had not

Under `lifecycle_verbosity = "warning"`, which testthat sets, and on the first
such call of a session otherwise,
`function(data, store) inspect_grouping(data, .grouping = grouping_sets(c(sp, store)))`
called with `stop("never needed")` for `store` failed with that error, where
`dplyr::select(data, c(sp, store))` in the same wrapper returned `region,
store` and forced the argument zero times. By reading,
`selection_names_grouping_spec()` calls `rlang::env_get()` on every name the
argument reads and passes over active bindings only; its header says "A binding
is read only where reading it evaluates nothing tidyselect has not". The
refuter widened the trigger to any `lifecycle_warning_deprecated` signalled
during the selection (`c(.data$region, store)`), to a formal's default and a
`delayedAssign()` binding, and to every constructor and input kind; `.by` and
`all_of()` were not affected. Skipping lazy bindings, replayed in memory by the
refuter, removed the forcing and kept the refusal #1703 asked for when the name
holds a specification in a plain binding, a wrapper argument or a delayed
binding.

### A share selection's missing names

For a bare-name selection vctrs puts the missing name in the condition's `i`;
for `all_of()` it puts the whole subscript there and means `i[!i %in% names]`.
`share_selection_missing_names()` returned every character `i` in the chain and
the refusal named the first, so `all_of(c("revenue", "nope"))` was refused with
`marginplyr_share_source_unavailable_error` for `revenue`, where `c(revenue,
nope)`, `all_of(c("nope", "revenue"))` and tidyselect on the same proxy named
`nope`. The refuter found the same reader taking a character `i` from
conditions that were not about the proxy: a helper inside the selection that
selected from another frame was reported as "`revenue` unavailable" with the
helper's condition dropped, against #107. Reading `i[!i %in% names]` and
restricting the reader to the out-of-bounds condition whose `names` are the
proxy's, replayed in memory over 28 selection shapes, corrected every wrong
case and left every right one byte-identical.

### Handlers

Lane 1 listed 129 handler-related sites outside roxygen, read all of them in
context and exercised about 25. The regression replays of #1782, #1789, #1790,
#1796 and #1797 held at the snapshot.

A 0.5 s round trip was simulated for the statement `EXPLAIN SELECT 1 FROM
"res"` by tracing the driver's `dbSendQuery` method. Under
`setTimeLimit(elapsed = 0.25, transient = TRUE)`,
`compute(r, name = "res", temporary = TRUE)` of a sorted SQLite Margin summary
returned after 0.77 s with the correct rows; the same statement sent with
`DBI::dbGetQuery()` ended with the time-limit error, and so did the package's
own `compute()` when the delay was moved to its `INSERT`. By reading,
`sqlite_compute_name_resolves()` in its conservative mode answers `TRUE` for
any error but "no such table". Untraced, the finder's 238 runs and the
refuter's 2,300 trials never attributed a return to this handler; the refuter
reproduced it without a trace in one arrangement only, another process holding
`BEGIN EXCLUSIVE` while creating a main table of the requested name. Running the
probe under `suspendInterrupts()`, replayed in memory, ended that case with the
time-limit error in 5 of 5 runs.

A helper inside a local summary that called `inspect_grouping(lookup,
.grouping = "k")` produced "`.grouping` must be created with ..." blamed on the
outer `summarize_with_margins()` call. The candidate also asked for dplyr's
argument and group lines to be kept; #1531 and two pinned tests decide that a
Package condition raised by caller code in a summary arrives unwrapped with
`marginplyr_error` on top, so that part was dropped. Leaving a call that an
inner `with_margin_error_call()` had already set, replayed over 20 cases,
changed the reproduction alone.

### `eval.parent()` and the default environment of rlang's accessors

With `hidden <- data.frame(g, v, sh = c(100, 200, 300))`, `sh =
share_of_total(s)` followed by `z = { f <- function() sum(eval.parent(quote(sh))); f() }`
returned `z = 300, 300, 600`, the input column's sums, beside the share `sh =
0.375, 0.625, 1`; `sum(eval.parent(quote(sh), n = 0))` written directly did the
same. The `evalq(sh, parent.frame())` and `eval(quote(sh), parent.frame())`
twins were refused with "Ordinary summaries cannot use an earlier Total share".
Plain dplyr, with `sh` an ordinary earlier summary, returned that summary:
`eval.parent()` from `f` evaluates in the mask. A share source reading an
earlier alias the same way was accepted. Without an input column of the share's
name the call ended in dplyr's "object 'sh' not found". Lane 7 reported the
share-source direction independently from the evaluator table. Adding
`eval.parent` to `is_reflective_evaluation()` and mapping it onto `eval()` in
`evaluator_as_eval()`, replayed together in memory, refused every shape tried;
either alone left a route open.

`src = rlang::env_get(rlang::env_parent(), "zz")` after `zz = sum(v) * 100` was
accepted as a share source and returned the earlier summary, where the
`rlang::env_parent(rlang::current_env())` spelling was refused. In the other
direction, replayed by the coordinator, `obs = rlang::env_parent()$zz` after a
Parent share `zz` returned a result with columns `g, s, zz` and no `obs`, with
no condition, where the `current_env()` twin was refused and plain dplyr
returned `obs` equal to `zz`.

### A constant argument's hint

`function() report(d, "x")`, where `report()` passes its argument to a later
`across()` whose `.names` template the planner does not predict, was refused
with "reads `measures`, which is not evaluated before the call runs" on its
first call and with no hint from the second: compiled code passes the literal
as a value. `known_context_constant()` already reads such a promise as its
constant, and `is_unevaluated_binding()` does not. The second half of the
candidate, a `force(fn)` hint for an argument holding a list, was a recorded
decision of commit `7aa91679` and was dropped.

### The DuckDB preflight with the json extension and on two engine versions

`SELECT * FROM json_execute_serialized_sql(json_serialize_sql('SELECT * FROM
read_csv(...)'))` gave the typed plan `(k)` for a file whose one data row was
`101` and `()` after that row was changed to text; `read_csv()` written
directly and `query('...')` were refused for both. The same held for a stored
view, a literal serialized string, a comma join and a scalar subquery, and an
absent file ended with DuckDB's IO error.

`FROM 'p.geojson'`, `FROM "p.geojson"`, `TABLE "p.geojsonl"` and a stored view
gave `(k)` then `()`, with `EXPLAIN` naming `READ_JSON_AUTO`; `FROM 'p.json'`
was refused. By probing absent files, DuckDB 2.0.1 with json treated
`.geojson`, `.geojsonl`, their `.gz` and `.zst` forms and a `?` suffix as JSON
replacement scans, and not `.geojsonseq`, `.ndgeojson`, `.topojson`, `.jsonld`,
`.json5` or `.jsonc`. On 1.5.6, which had no json binary, the relation was a
catalog error.

Lane 7 defined a caller macro for each of 597 system function names and ran 90
syntax forms. `5!` reached a macro named `factorial` and `'a' ~ 'a'` one named
`regexp_matches`: the plan was `(v, y)` then `(v)`, where the call spellings
and a macro named `+` were refused. Neither syntax reached the macro on 1.5.6.
The refuter added `trunc`, reached from `INTERVAL (expr) DAY` on both versions.

Lane 3 also established that duckdb 1.5.6 met the declared minimum for every
ordinary verb tried, and that the 1.5 branches the DuckDB tests hold behaved on
1.5.6 as those tests assert.

### `filter_out()` and `nest_by()` nested in a summary

The candidate was a nested `pick()` of a column only the nested frame had,
which plain dplyr refuses too. The refuter found, and the coordinator replayed,
the shapes in which plain dplyr is right and the Margin summary is not: with
`nested_g <- tibble(g = c("x", "y", "z"), aa = 1:3)` and `.grouping =
rollup(g)`, `n = nrow(filter_out(nested_g, if_any(everything(), ~ .x == "x")))`
returned 3 in every row against 2 from plain `summarise()` and from the nested
call alone, the registered `filter(nested_g, !if_any(...))` returned 2, and
`n = ncol(nest_by(nested_g, across(everything())))` returned 2 against 3.
`filter_out()` entered dplyr in 1.2.0, the version `DESCRIPTION` requires.

### Printing an injected specification

`s <- grouping_sets(!!s, grouping_set())`, the idiom `?grouping_set` documents,
printed `grouping_sets(<mrgn_gr_>, grouping_set())`, as did `!!!list(...)` and
`rlang::inject()`. A literal nested constructor, a bound name and an injected
quosure printed legibly. The previous hunt had reported the shape without
checking it. Substituting the nested specification's own constructor call,
replayed in memory by the refuter, printed text that parsed and evaluated to an
equal specification and changed no pinned print case.

## Filed with a decision

Each of these needed a choice the contract did not make. The coordinator made
it from the ADRs, the Rd, the pinned tests and earlier tickets, replayed the
candidate answer in memory or re-ran the refuter's replay, and wrote the
decision on the ticket. The first eight carry `bug` and `ready-for-agent`, the
last two `documentation` and `ready-for-agent`.

| Boundary | Established effect | Decision | Script | Issue |
| --- | --- | --- | --- | --- |
| A caller function's own error while `.by` or a constructor argument is selected | Replaced by the parentless "select existing columns, not computed expressions" refusal whenever the call named an input column. | Refuse only for a failed lookup of a named column, or a call head the caller did not define. | `computed-selection-refusal-replaces-caller-error.R` | [#1802](https://github.com/sayuks/marginplyr/issues/1802) |
| Quoted language in a `do.call()` argument list on a local input | Not read by the share dependency walk; a later summary returned a same-named input column's aggregate. | Read the language constructors in a literal `list()`; leave language built at run time outside the walk. | `share-walk-do-call-quoted-language.R` | [#1807](https://github.com/sayuks/marginplyr/issues/1807) |
| A later `across()` selection or `.names` reading a package constant | Named no output until the session had forced the constant's lazy-load promise. | Read a package or base lazy-load promise as its value, as #1793 decided for functions. | `across-selection-lazy-load-constant.R` | [#1809](https://github.com/sayuks/marginplyr/issues/1809) |
| A share source produced by a preceding `across()` whose outputs are unknown | The refusal said the source was "a column of the input, not a summary defined earlier". | Tell "unknown" from "none" in the name prediction and say which. | `share-source-false-input-column-hint.R` | [#1810](https://github.com/sayuks/marginplyr/issues/1810) |
| A caller DuckDB macro named with an operator token | Bound by the zero-row metadata read with no call head. | Refuse typed metadata while the catalog holds a caller macro named only with operator characters. | `duckdb-operator-named-macro.R` | [#1813](https://github.com/sayuks/marginplyr/issues/1813) |
| An input carrying `dbplyr::window_frame()` | A lazy SQL expansion and a share summary dropped the frame and kept the window order, differently by backend. | The frame travels with the window order. | `sql-expansion-window-frame.R`, `sql-share-window-frame.R` | [#1815](https://github.com/sayuks/marginplyr/issues/1815) |
| `rlang::exec()` with a reflective function | Not read as `do.call()` is. | Read rlang's `exec` through the same branch, recognised by its binding. | `share-walk-rlang-exec.R` | [#1820](https://github.com/sayuks/marginplyr/issues/1820) |
| `rlang::local_bindings()`, `with_bindings()` and `env_bind_active()` given `zap()` | Not counted as removals. | Add them to the mutator table; a temporary removal counts as a removal. | `share-walk-rlang-local-bindings.R` | [#1822](https://github.com/sayuks/marginplyr/issues/1822) |
| `count()` or `summarise()` after a sorted lazy result | Refused by DuckDB and PostgreSQL for some result shapes, where the guide said the order is dropped with a warning. | Correct the guide and ADR 0018; no code. | `sorted-result-later-summarise.R` | [#1816](https://github.com/sayuks/marginplyr/issues/1816) |
| A share source with a set-returning SQL function other than a bare `UNNEST()` | Collected with multiplied rows and wrong shares, where `?share_of_parent` said it surfaces as a database error. | Reword the Rd sentence to the checked form and restore the caller obligation; no code. | `postgres-share-set-returning-source.R` | [#1817](https://github.com/sayuks/marginplyr/issues/1817) |

Four of these decisions are ones a maintainer could reasonably make the other
way:

- #1813 refuses typed metadata on a connection that holds any operator-named
  caller macro, including a harmless one, a statement that never reaches it,
  and macros named `=`, `<`, `<=`, `>`, `>=` and `!=`, which DuckDB 2.0.1 did
  not reach from syntax. ADR 0020 already accepts that cost for the listed
  names; excluding the six would be a second hand-kept list.
- #1815 changes the values of a later frame-sensitive aggregate over an
  expansion or a share summary of a framed input on DuckDB, PostgreSQL and
  generic SQL. Dropping the frame uniformly and documenting it, which is what
  dbplyr's own `collapse()` and `compute()` do, would change SQLite expansions
  and share-less summaries instead.
- #1822 refuses `{ zz <- 1; rlang::with_bindings(1, zz = zap()); zz }`, where
  the removal is temporary and R reads the local again.
- #1802 lets tidyselect's own out-of-bounds error through where a caller
  helper returns a name that is no column, in place of the refusal.

### What the replays established

- **The computed-expression refusal.** Natively, and in marginplyr for the
  same helper over a name that is no column or wrapped in `all_of()`, the
  caller's condition arrives as the parent of tidyselect's "In argument"
  error. The refuter replayed two rules: refusing only when the chain holds
  the failed lookup of a named column broke the pinned `format(dt)` and
  `toupper(dt)` shapes, where a column is named like `stats::dt`; adding "or a
  named column is bound to a function" kept them, but a test that forces
  nothing cannot see an unforced lazy-load promise, so `key_cols(dt)`
  propagated only while nothing had read `dt`. The coordinator replayed a
  third on the refuter's harness — a failed lookup of a named column, or a
  call head that is a primitive, an operator, a `pkg::fun` spelling or a
  function whose environment is a namespace: every caller-helper shape ended
  with the caller's condition as parent, every pinned shape and
  `rollup(toupper(letters))` stayed refused, and a caller's own transformer
  `my_upper(region)` stayed refused through the lookup arm. Recognising R's
  unbound-object error by comparing with the message R gives for the same
  symbol in `emptyenv()` held under `LANGUAGE=ja`.
- **`do.call()`.** Reading the constructors `static_language_values()` already
  reads for `eval()` in a literal `list()` refused `list(quote(sh))`,
  `list(as.name("sh"))`, `list(str2lang("sh"))` and the alias direction and
  kept seven legal controls. Marking the call unresolved for any other
  argument list refused four calls that work, among them `do.call(sum,
  as.list(v))`, and still missed a caller-held language object.
- **A package constant.** Reading a lazy binding in `baseenv()` or a package
  environment with its value accepted `state.abb`, `month.abb`, a namespace
  constant, a `.names` constant and the column-wise share on the first call,
  and left a caller's `delayedAssign()`, a wrapper argument and a delayed
  binding in an environment hand-named `package:fake` unread.
- **The false hint.** Dropping the input-column bullets whenever a preceding
  unnamed `across()` has no records also dropped the true hint where the
  `across()` provably cannot produce the name; a neutral "output names are not
  known" bullet on the same trigger was false for an `across()` that selects
  nothing. Neither changed which calls are accepted.
- **Operator-named macros.** DuckDB binds any operator token it has no rule
  for as a function of that name: the refuter reached 172 unlisted names of 320
  probed on 2.0.1 and 204 on 1.5.6, and `->>` needed no json extension. Adding
  `->>` to the list left `@@`, `<<=`, `!!`, the dplyr spelling `j %@@% "a"` and
  a stored view following the file. Refusing by the name's characters closed
  every shape on both versions with only the catalog query sent.
- **The window frame.** dbplyr's `collapse()` and `compute()` of the native
  composition gave exactly marginplyr's state, frame empty and order kept.
  Carrying the frame across the boundaries that carry the order left the
  result's rendered SQL and rows identical in 144 of 144 cases and made a
  later `sum(v)` equal the native composition in 16 of 16; restoring the
  staged frame on a share result left its SQL identical in 30 of 30 on each of
  DuckDB and SQLite. A `.sort = "first"` result then held a frame with no
  order, which dbplyr renders with a warning.
- **A later `count()`.** Native `tbl |> arrange(v) |> count()` failed with the
  identical error on the same DuckDB and PostgreSQL connections: dbplyr 2.6.0
  folds a later `summarise()` into an arranged plain-projection node and keeps
  its `ORDER BY`. A text Margin label makes the outer projection a computed
  `CASE`, which dbplyr nests. In the finder's matrix 106 of 480 native DuckDB
  cases and 260 of 480 on the generic connection failed per verb.
- **Set-returning share sources.** `SUM(v) * generate_series(1, 2)` returned
  six rows with two Grand total rows and every share divided by the larger;
  native dbplyr returned four rows for two groups. A row-count guard over the
  staged summary, replayed as raw SQL, caught the multi-row forms and could
  not catch a zero-row source.

## Rejected candidates

- A first share request inside a PostgreSQL transaction begun with a raw
  `BEGIN` sent the dialect question with no savepoint; the question aborted
  the transaction, the share was refused as "could not be asked", and the
  caller's later `COMMIT` returned without error and kept nothing. The native
  share on the same connection left the transaction usable. ADR 0020's
  amendment for #774 says of `postgresIsTransacting()`: "It identifies the
  boundary this amendment protects; it does not identify transactions begun
  with raw SQL", PR #776's review recorded the same boundary, and the guide
  scopes the guarantee to "a transaction begun through DBI". The refuter
  measured RPostgres's `dbWriteTable()` and dbplyr's `copy_to()` discarding a
  raw-`BEGIN` caller's earlier work in the same way. Always attempting the
  `SAVEPOINT` and reading its failure outside a transaction block as
  autocommit, replayed in memory, kept the transaction and the marker row in
  every raw shape and added one failing statement to each cold autocommit
  request, against the ADR's "Autocommit and cached answers retain their
  existing bounds". `postgres-raw-begin-share-probe.R` holds the observation.
- A Grouping specification passed by position to `expand_with_margins()`, a
  nesting verb or `inspect_grouping()` landed in `.by` and got tidyselect's
  "must be numeric or character, not a <margin_grouping_spec> object".
  `dplyr::summarise(d, .by = rollup(a))` raised the same class and text, no Rd
  or ADR promises a hint outside `summarize_with_margins()`'s dots, and the
  message named the caller's expression. This settles the second item the
  previous note reported without counting. `positional-grouping-specification.R`
  holds the observation.
- A share summary dropping its input's window frame was refuted as a defect
  of its own: no contract sentence names a frame, #1555 clears it on purpose
  inside the generated windows, and the expected value came from the
  share-less path. Both refuters put it in the decision #1815 makes.

## Second round

Four investigators took only the paths next to the confirmed findings. Every
candidate got its own refuter on the other model, and one duplicate lookup per
lane treated its candidates separately:

| Lane | Starting point | Model and effort | Candidates |
| --- | --- | --- | --- |
| A | What the share dependency walk places, against every exported function of base, methods, utils, stats and rlang that evaluates language, looks a name up or removes a binding in a caller-chosen environment (next to #1807, #1808 and #1820 to #1822) | claude-opus-5-5, high | 8 |
| B | Every family of the nested data-mask registry against the exports of dplyr and tidyr, and the lazy state a Margin result carries across the package's raw-SQL boundaries (next to #1819 and #1815) | claude-sonnet-5-5, high | 4 |
| C | The DuckDB preflight's remaining hand-kept names and its tokenizer, with the json extension loaded and on both engine versions (next to #1812 to #1814 and #1818) | claude-opus-5-5, high | 3 |
| D | Planning code that reads a caller's binding, and handlers that diagnose a caught condition and replace it (next to #1802 to #1804) | claude-opus-5-5, high | 1 |

Three candidates named the cause and repair of a first-round ticket and were
added to it: `rlang::invoke()` to #1820, and two reports of further verbs
missing from the data-mask registry to #1819. The other thirteen became
tickets, all `bug` and `ready-for-agent`.

Eight of lane A's and lane C's refutations first ran on claude-sonnet-5-5,
the fallback the run takes when the intended refuter fails twice, because
claude-fable-5-2 was over an account limit. They were run again on
claude-fable-5-2 once the limit lifted, and the decisions follow the second
set. The two sets agreed on five candidates and differed on three, each time
on something a pinned test or an earlier decision settled: the first proposed
refusing `pick(any_of("sh"))`, which the #1619 tests pin as working; it let
`delayedAssign()` be bound as a local, which the walk cannot time; and it let
the `env_get()` candidate stand whole where the second measured that full
parity with `get()` refuses eleven legal calls.

| Boundary | Established effect | Decision, where one was needed | Script | Issue |
| --- | --- | --- | --- | --- |
| A literal glue template in a summary after a share | Its components were not read: `glue("{round(100 * sh)}%")` ended in "object 'sh' not found", or returned a same-named input column's value. | Parse the literal template with glue's parser and walk its components; leave a template held in a variable outside. | `share-walk-glue-template.R` | [#1824](https://github.com/sayuks/marginplyr/issues/1824) |
| `as.list.environment()`, `lapply()` and `eapply()` over the data mask | Not read as `as.list()` is. | Route the named base materializers to the unresolved marker; not any callee handed the mask. | `share-walk-mask-materializers.R` | [#1825](https://github.com/sayuks/marginplyr/issues/1825) |
| An inheriting `rlang::env_get()` on an environment the walk cannot place | Read as reaching no mask frame, where `get()` on the same environment is refused. | Report inheriting reads only, unless the environment is proven external. | `share-walk-env-get-unplaced-environment.R` | [#1829](https://github.com/sayuks/marginplyr/issues/1829) |
| `match.fun(get)` | The naming primitive's argument was read only as a string. | Read a name or `::` call for `match.fun()`; leave `rlang::as_function()` outside. | `share-walk-match-fun-symbol.R` | [#1830](https://github.com/sayuks/marginplyr/issues/1830) |
| A function built in place by `rlang::new_function()` or `as.function(list(quote()))` | Not read, where `as.function(alist())` and `body<-` are. | Read the constructor as the literal it builds. | `share-walk-new-function-quoted-body.R` | [#1831](https://github.com/sayuks/marginplyr/issues/1831) |
| A later summary selecting an earlier share by a string | tidyselect's "Column `sh` doesn't exist" in place of the earlier-share refusal. | Read a literal string in a selection position; exclude `any_of()`. | `share-selected-by-string.R` | [#1832](https://github.com/sayuks/marginplyr/issues/1832) |
| A share source reading a local it bound with `assign()` | Refused as depending on an earlier alias of that name. | Bind a literal base `assign()` of any value; keep `delayedAssign()` and the other binders refused. | `share-source-assign-local.R` | [#1833](https://github.com/sayuks/marginplyr/issues/1833) |
| A parenthesised function literal or `pkg::fn` on the right of `%>%` in a summary | The rebuilt call lost the pair, and magrittr built an invalid call. | Restore the pair on a pipe's right-hand side and amend ADR 0019. | `pipe-parenthesized-function-literal.R` | [#1826](https://github.com/sayuks/marginplyr/issues/1826) |
| A verb on the right of magrittr's eager pipe `%!>%` | No data-mask ownership: a nested `if_any(everything(), ...)` returned 3 against 2. | | `magrittr-eager-pipe-data-mask.R` | [#1827](https://github.com/sayuks/marginplyr/issues/1827) |
| Unquoted non-ASCII identifiers and Unicode spaces in authored DuckDB SQL | Split by the tokenizer, so a view or macro of that name was never resolved and the zero-row read sampled the file. | | `duckdb-non-ascii-identifier.R` | [#1834](https://github.com/sayuks/marginplyr/issues/1834) |
| A `by(...)` call on duckdb 2.x | Dropped as SQL syntax before the macro check. | Remove `by` from the list, without a version condition. | `duckdb-by-call-head.R` | [#1835](https://github.com/sayuks/marginplyr/issues/1835) |
| `exclude(`, `replace(` or `rename(` after a multiplication | Dropped as a star modifier. | Keep the exemption only where the `*` starts a select-list item. | `duckdb-star-modifier-after-multiplication.R` | [#1836](https://github.com/sayuks/marginplyr/issues/1836) |
| A failed selection in a first unnamed `across()` whose output a share reads | The chain carried a generated `dplyr::all_of(resolve())` layer. | | `first-across-selection-generated-context.R` | [#1828](https://github.com/sayuks/marginplyr/issues/1828) |

Added to first-round tickets: `share-walk-rlang-invoke.R` (#1820),
`nested-superseded-verbs-data-mask.R` and
`nested-verb-reserved-helper-refusal.R` (#1819).

### The share dependency walk, read from the dependency's side

Lane A listed 271 exported functions of base, methods, utils, stats and rlang
with an environment-like formal or a `parent.frame()`/`caller_env()` default,
compared them by hand with what the walk models, and ran about 150 forward and
reverse probes, each with a plain dplyr control on the same data frame. The
forward probe was the first round's: with an input column named like the
share, `z = <a read of sh through the member>` after `sh = share_of_total(s)`
returned 300, 300, 600, the column's sums. Every refuter's strongest
counter-argument was #1652's sentence that "moving the boundary one spelling
at a time does not close it", and each replay bore it out for its own site:
after the repair, a language object or a function held in a variable, or the
mask handed to a caller's own function, still read the column. The decisions
therefore keep one line throughout, the one #1652 and #1807 drew: a named
callee written in place is read, a value built or held at run time is not, and
each ticket states its residue in the function's header.

- **Templates.** `glue::glue()`, `glue_data()`, `stringr::str_glue()`,
  `rlang::englue()`, `cli::format_inline()`, `pluralize()` and
  `format_message()` evaluate their components in the mask. Parsing the
  literal template with glue's parser refused all seven and kept twelve legal
  controls; it refused `glue_data(list(sh = 1), "{sh}")` and a template
  evaluated in an environment that cannot reach the mask until `.x` and
  `.envir` were modelled. Marking every non-literal template unresolved
  refused `glue(tmpl_ok)`, which reads nothing forbidden.
- **`env_get()`.** `get("sh", envir = new.env())` was refused and
  `rlang::env_get(new.env(), "sh", inherit = TRUE)` returned the column, and
  so did `local(sum(rlang::env_get(parent.frame(2), "sh", inherit = TRUE)))`:
  `scope_caller_frame()` answers `NA` for a frame that "may be any walked
  frame", and `static_environment_frames()` drops it.
- **`assign()`.** The one over-broad direction found: a local bound by
  `assign("zz", 2)` was read as the earlier alias `zz`. Thirteen controls in
  which dplyr really reads the alias stayed refused under the repair.

### Nested masks and pipes

Lane B probed about 60 dplyr and 35 tidyr spellings for whether they open a
data mask and whether the registry shields them. With `nested_g` holding a
column named like the outer dimension, `nrow(top_n(nested_g, 1,
if_any(everything(), ~ .x == "x")))` returned 3 against 1, `group_nest()` 2
against 3 and `tidyr::uncount()` 0 against 1; `cur_group_id()` inside such a
verb was refused as reserved. `nested_g %!>% filter(!if_any(everything(), ~ .x
== "x"))` returned 3 against 2 from native dplyr and from the `%>%` spelling.
`v %>% (function(x) sum(x))` failed with "invalid formal argument list" where
`summarise()` returned 7: ADR 0019 drops the pair around a function literal
because it has "no operands to group", which does not hold on a pipe's
right-hand side. The lazy-state half found nothing: every Margin result
observed was ungrouped, kept its usable window order under `.sort = "none"`,
and agreed with the native composition on 20 later verbs.

### The preflight's lexical layer

Lane C's candidates came from the tokenizer and the head filters, not from a
name list. `duckdb_metadata_tokens("SELECT mé(v) AS y FROM vüe")` returned
`m`, `é`, `(` ... `v`, `ü`, `e`. The refuter found the same split behind a
search path naming a non-ASCII schema, with no non-ASCII character in the
caller's code, and found that DuckDB strips a handful of Unicode spaces the
tokenizer reads as part of a token, so `read_csv_auto<U+3000>(file)` passed
too; admitting the non-ASCII range alone left those four shapes open. A sweep
of every `duckdb_keywords()` entry as a call head on both versions found `by`
alone reaching a caller macro, on 2.0.1 only. `v * exclude(v)` gave a plan for
a numeric CSV row, a binder error for text and an IO error for an absent file.

### A generated layer in a condition chain

Lane D read about 40 binding reads and 30 handlers. Every read it traced
guarded lazy and active bindings or fell under a recorded decision, and its
one candidate was a context: `across(c(v, nope), sum)` in first position with
a later share reading its output failed with an extra "In argument:
`dplyr::all_of(resolve())`" layer, against #1573's criterion. Wrapping the one
call in `with_deferred_selection()`, replayed in memory, made the chain equal
native dplyr's.

## Reported and not counted

- `nest_with_margins()` of a dtplyr step with 886,000 cells
  (`rollup(d, k)`, `.by = a`, 10^6 rows) took 171 s and peaked at 22 GB, about
  8 KB retained per cell, against 1.2 s and 0.7 GB for `tidyr::nest()`; the
  local tibble path took 38 s and 979 MB for the same cells. Growth was linear
  in cells (2.2, 4.3 and 8.2 GB at 125,000, 250,000 and 500,000 groups), and
  the profile showed `restore_margin_cell_metadata()` calling `setDT()` per
  cell. No contract sentence states a cost for nesting.
- On PostgreSQL a 4,096-set native summary with `.id` took 20.5 s to
  construct, 0.53 s to render and 0.55 s to execute; without `.id` the three
  were 0.12, 0.48 and 0.18 s, and with a share 19.8, 0.57 and 0.50 s. The time
  was R-side construction of the set-identifier `CASE` in
  `grouping_set_id_sql_expr()`, one `dbplyr::sql_glue2()` call per set and
  dimension at about 0.41 ms per term, linear at 256, 1,024 and 4,096 sets.
  This answers the split the previous note left open.
- A PostgreSQL expansion of a `cube()` with `.id` took 5.1, 23.2, 109 and
  262 s to construct at 256, 1,024, 4,096 and 8,192 sets, with rendering and
  execution under 0.6 s; the package limit applied as ADR 0046 says, and at
  16,384 sets PostgreSQL refused the statement for stack depth.
- `all_of(datasets::state.abb)` in a later `across()` was refused as an
  unknown share source before and after the promise was forced, `::` not being
  among the context-only operations.
- `withAutoprint(exprs = list(quote(z0 <- sum(sh))), evaluated = TRUE)` and
  `source(exprs = expression(...), local = TRUE)` inside a summary read an
  input column unseen, as `eval.parent()` did.
- On PostgreSQL the share-cardinality guard's message arrived inside
  `invalid input syntax for type boolean: "Share source n must return exactly
  one value per grouping row..."`.
- `man/inspect_grouping.Rd`'s list of refusals named the DuckDB CSV readers
  and not the JSON refusal.
- An injected character vector of length above one printed as `<chr: "a",
  "b">` in a specification.
- Left outside the share dependency walk by the second round's decisions,
  each still returning a same-named input column's value:
  `parent.env(parent.env(new.env()))$sh` and its `[[` and `as.list()` forms,
  `rlang::as_function("get")("sh")`, a mask carried through
  `rlang::env_clone()`, `(function(e) e$sh)(mask)`, and a template or a
  function body held in a variable. `pick(all_of(nm))` with the share's name
  in a variable kept tidyselect's missing-column error.
- Over SQLite through the generic forwarding connection, RSQLite typed a
  column from the outermost `SELECT`'s declared type, so the derived-table
  anchor of #1768 moved a typed-missing `Date` dimension to integer under
  `.sort = "last"`; no real generic backend types that way.

## Covered without a finding

Counts are the investigators' reports.

- Handlers: 129 handler-related sites outside roxygen read in context (76
  `tryCatch()`, `withCallingHandlers()` and `try_fetch()` lines, 28 restart
  lines, 18 interrupt-suspension lines, 7 `on.exit()` lines), about 25
  exercised; about 40 calls over seven input kinds for the computed-selection
  refusal; the SQLite temporary-table handlers by reading and by probe.
- Share analysis after its repairs: about 30 summary-and-share configurations
  on grouped inputs on six backends against a per-grain oracle, with no
  finding (a row-wise input is refused outright); about 130 `across()`
  output-name cases; about 100 environment-manipulation shapes on two inputs,
  each with a native control; 27 frame-column spellings; 11 constant-promise
  wrapper shapes over three calls.
- DuckDB: the `duckdb_functions()` difference between 1.5.6 and 2.0.1 (81
  additions, 23 removals, 33 system macros whose definitions differ, 19
  keyword additions); all 11 JSON table functions and about 40 JSON spellings;
  about 66 CSV spellings and 45 replacement-scan suffixes on both versions; 60
  syntax forms for implicit macro reachability on both, with all listed names
  reached; ordinary verbs on 1.5.6 in 12 variants and 78 typed-key cases on
  both versions.
- Repair neighbours: on the generic forwarding connection over DuckDB and over
  SQLite, 1,920 summary cases per engine under six later steps and 480 under
  nine, against a base-R per-grain oracle, with every arm of
  `finalize_margin_operation()` reached; 108 expansion cases on four
  connections against a row-copy oracle; 7,200 dtplyr join plan comparisons
  and 45 arithmetic expressions; 24 zero-output shapes beside #1794; about 75
  `compute()` calls on the package's result classes.
- PostgreSQL: about 40 refusal shapes with class, call, message and the
  statements sent before each, traced below DBI; on PostgreSQL a construction
  sent only the label query and the share dialect question with its
  savepoints. #1782's repair held on the live server: 174 fresh-process limit
  placements under `dbBegin()`, autocommit and raw `BEGIN`, 12
  `statement_timeout` runs and 25 `pg_cancel_backend()` runs left no state
  behind inside a DBI transaction. 18 column types under three label kinds.
- Refusal precedence: 145 of 180 `abort_marginplyr()` sites reached by about
  25,000 generated calls holding pairs of mistakes, on eleven input kinds and
  five verbs, with statements traced for every single mistake on DuckDB,
  SQLite and generic SQL. No pair's winner contradicted ADR 0005, ADR 0020 or
  `?marginplyr_conditions`, and the decided orders (#1785's limit, PR #1748,
  #1771) reproduced as recorded.
- Vocabularies: 14 families of `static_spelling_rules()`, the DuckDB implicit
  and value-function names, the reflective tables, the rlang mutator table,
  the selection operator and helper lists and the dtplyr and data.table lists,
  against the installed packages, with about 700 fresh-process probes. No list
  in `R/` copies dbplyr's translation tables.
- Scale: at 10^6 rows, summaries, shares, expansions and `cube(a, d, k)` on
  six backends and nesting on three matched the native aggregation at each
  grain, with a 10^4-level factor, `integer64` and overflow cases; a rollup
  summary took 0.1 to 1.1 s against 0.1 to 0.9 s natively, and shares over
  866,000 groups took 27.6 s on a tibble and 0.5 s on DuckDB.
- Display: about 60 prints of specifications, inspection results, lazy result
  headers and nested cells at three widths.

## Not covered

No MySQL, MariaDB, SQL Server, Oracle, Snowflake or Redshift server was run.
No JSON reader was run on DuckDB 1.5.6, so whether 1.5's json extension
replaces `.geojson` or binds `json_execute_serialized_sql()` is unknown; the
spatial, excel and other extensions could not be loaded, and compressed JSON
was tested by absent-file replacement only. PostgreSQL was reached over a
local socket, so no limit landed inside a long server-side statement; pool and
odbc connections were not tried. Of the handler sites, where a limit deferred
by a `suspendInterrupts()` region is next delivered was not measured, and
`sent_query_condition_message()` under a caller condition class with a slow
`conditionMessage()` method was read and not run. 35 `abort_marginplyr()` sites
were not reached, and only pairs of mistakes were generated. The share
dependency walk's candidates were run on local frames; `attach()`, `on.exit()`,
`sys.function()` and `Recall()` as readers were not examined. The vocabulary
lane did not diff vctrs and sf class names or tidyr verbs as nested mask
owners. At scale, peak memory was R's heap only, and PostgreSQL, Parquet and
CSV-backed DuckDB inputs and 4,096-set plans were not run at 10^6 rows. arrow
13, rlang 1.1.0, cli 3.4.0, dtplyr 1.3.2 and the other declared minimum
versions were not run. The duplicate lookups of both rounds were batched by
lane.

In the second round, lane A's candidates were run on data frames and
data.tables and, by the refuters, in part on dtplyr; the 90 functions of the
methods package with an environment-like formal were read by name only, and
purrr's mappers and converters were left to the #1652 boundary. Lane B did not
run Arrow inputs, base `with()`, `within()`, `subset()` and `transform()` or
data.table's `[` as nested masks, or pending `slice_*()` limits on PostgreSQL.
Lane C did not run JSON-source variants of its three candidates, Margin verbs
other than the summary and the inspection, or extension readers that could not
be loaded. Lane D did not read about 45 of 76 handler sites or run the
first-`across()` path on lazy inputs.
