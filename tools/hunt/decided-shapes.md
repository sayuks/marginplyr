# Decided shapes

What earlier hunts decided, for a finder to start elsewhere and a refuter to
cite. A shape listed here is not a finding; its neighbour is fair game. Each
hunt appends its own section when it records its note
(`design/agents/bug-hunt.md`, step 9), newest last. The issue or ADR named on
a line is the authority for it; this file is the index.

## Through the hunt of 2026-11-26

- #1653 (`wontfix`): an External condition raised by an expression marginplyr
  generated keeps its internal `dplyr::summarize()`/`dplyr::mutate()` context;
  ADR 0021 scopes the restatement to the caller's own expressions.
- #1747: under `setTimeLimit()`, dplyr's "missing value where TRUE/FALSE
  needed" is dplyr's own defect (tidyverse/dplyr#7872).
- **#1782's exclusion**: a call under a time limit that ends with an error,
  returns nothing and records nothing is not counted, whatever the error's
  wording or class (the database guide says "Handle any error from the call
  rather than matching the message"). A time limit counts only when the call
  *returns a value*, *stores something that later calls read* (a memo, a
  cache, a recorded verdict, `last_sent_queries()`), or *leaves state behind*
  (an open savepoint, a temporary table, a changed option).
- #198: telling a transient driver error from a permanent one on the share
  dialect question is out of scope.
- #891: a severity-free condition signalled during the SQL audit's diagnostic
  render stays local (the record keeps `sql` `NA`, no condition row); pinned
  by `test-sent-query-general-conditions.R`.
- PR #1748's review: under `.duplicates = "keep"`/`"error"`, a product of
  specifications whose prefix already passes the grouping-set limit still
  sends DuckDB's metadata statements for a later `where()` factor, and an
  invalid `marginplyr.max_grouping_sets` is refused after them. Decided.
- #1787 (`wontfix`): the limit refusal's count is inexact past 2^53.
- #1621, #1625 (`wontfix`, above). #1671 (`wontfix`): closures a base
  functional's callback creates. #1672–#1675 folded into #1676's single rule:
  removing a captured earlier alias counts as reading it. #1680: each closure
  tracking mechanism must decide an observable outcome. #1652: a function held
  as a value (`f <- get; f("sh")`) is beyond the static walk, documented.
- Limits the repairs of PR #1799 accepted (same shape is not a finding):
  1. #1782/#1796: the share dialect probe and the audit render run under
     `suspendInterrupts()`, so a time limit or Ctrl-C cannot cut a probe
     short; the true verdict may still be cached when the limit lands after
     it; DuckDB may end with the "could not be asked" refusal.
  2. #1785: on a DuckDB input a malformed `grouping_bit()`/`grouping_id()`
     call is refused before any statement even when a `.by`/`.grouping` error
     exists that only typed metadata would decide; every other input keeps the
     previous order.
  3. #1779: the backtick-identifier rule applies only with duckdb >= 2.0.0
     (1.5 tokenises backticks as operators).
  4. #1780: a statement with no table reference (`SELECT 1 + 1`) is not
     judged; views stored with a named call (`COUNT(*)`) keep the view
     condition. ADR 0020's amendment: a name DuckDB adds later is admitted
     until it is listed in `duckdb_implicit_functions`; the name match is
     position-blind (#1781), so `histogram(v)` is refused.
  5. #1768: the zero-row type anchor sits inside a derived table.
  6. #1777: an expansion without `.id` still fails under `cte = TRUE`, exactly
     as `sql_options(cte = TRUE)` does (dbplyr CTE rendering).
  7. #1790: the time-limit criterion is structural (no handler around the
     read).
- Reported earlier and not counted: a hive-partition column typed from a
  directory name; `sniff_csv()`; a 2,048-set plan written with 1,600 repeated
  union branches taking 2.4 GB under `.duplicates = "drop"` (ADR 0046 states
  the cost model); the dtplyr proxy's time limit turning into the "Can't
  safely determine column metadata" refusal (falls under #1782's exclusion).

Filed earlier and closed (fixed unless marked): #1618–#1624, #1640–#1654,
#1663–#1668, #1676, #1677, #1679, #1680, #1682–#1684, #1687, #1688,
#1699–#1703, #1728, #1745, #1749, and from the 2026-11-26 hunt #1768–#1797
(all repaired by PR #1799 except #1787). `gh issue view <n> --comments` shows
each. Read the three earlier notes before you start, so that you start
elsewhere: `investigation/second-eight-lane-workflow-hunt-2026-11-26.md`
(all of it), `investigation/eight-lane-workflow-hunt-2026-11-13.md` and the
*Revisions* of `investigation/margin-verbs-record-fourth-pass-hunt-2026-11-12.md`.
A repaired issue that reproduces again at the pinned commit **is** a finding
(a regression or an unmet acceptance criterion); say which.

## The hunt of 2026-11-27

Filed and repaired by PR #1838 unless marked:

- #1802 the computed-expression refusal for `.by` and constructor arguments
  replaces a caller function's own error (decided: refuse only for a failed
  lookup of a named column or a call head the caller did not define).
- #1803 the selection deprecation handler forces a lazy binding tidyselect
  never evaluates (`selection_names_grouping_spec()`).
- #1804 a share `across()` selection with `all_of(c("ok", "missing"))` names
  the first element as the unavailable summary; a helper's own subscript error
  loses its condition.
- #1805 SQLite `compute()`'s destination-name probe takes a time limit.
- #1806 a refused inner Margin verb is blamed on the enclosing verb's call.
- Share dependency walk, local inputs: #1807 quoted language in a literal
  `do.call()` argument list; #1808 `eval.parent()`; #1820 `rlang::exec()`;
  #1821 the default `env` of `rlang::env_parent()`/`env_get()`/`env_get_list()`;
  #1822 `rlang::local_bindings()`, `with_bindings()`, `env_bind_active()` with
  `zap()`. Seen by the refuters and not filed: `withAutoprint(exprs = ...,
  evaluated = TRUE)` and `source(exprs = expression(...), local = TRUE)` read an
  input column unseen; a language object built by `call()`/`rlang::syms()` or
  held in a caller variable is outside the walk by decision (#1652, #1807).
- #1809 a later `across()` selection or `.names` reading a package constant's
  lazy-load promise; #1810 a false "column of the input, not a summary defined
  earlier" hint; #1811 the unevaluated-binding hint for a constant argument.
- DuckDB preflight: #1812 `json_execute_serialized_sql()`; #1813 a caller macro
  named with an operator token; #1814 `.geojson`/`.geojsonl` replacement scans;
  #1818 `factorial`, `regexp_matches`, `trunc` bound from syntax.
- #1815 a lazy SQL expansion or share summary drops the input's
  `window_frame()` while keeping its window order (decided: the frame travels
  with the order).
- #1816 documentation: `count()`/`summarise()` after a sorted lazy result can
  be refused by the database (dbplyr folds the query; native `arrange() |>
  count()` fails alike). #1817 documentation: only a bare top-level `UNNEST()`
  share source is checked for cardinality.
- #1819 `filter_out()` and `nest_by()` are missing from the independent
  data-mask registry (`static_spelling_rules()$dplyr_mask`): a nested
  `if_any(everything(), ...)` or `across(everything())` is expanded against the
  outer summary and returns a wrong value.
- #1823 `print()` shows an injected nested specification as `<mrgn_gr_>`.

Rejected, so not findings: a share request inside a PostgreSQL
transaction begun with a raw `BEGIN` aborts it (ADR 0020's #774 amendment
scopes the savepoint to transactions begun through DBI; RPostgres's own
`dbWriteTable()` behaves alike); a Grouping specification passed by position to
`expand_with_margins()`, the nesting verbs or `inspect_grouping()` gets
tidyselect's `.by` error (native `.by` does the same; no contract sentence).

Second round, repaired by PR #1838:

- Share dependency walk, local inputs: #1824 the components of a literal glue
  template (a template held in a variable stays outside); #1825
  `as.list.environment()`, `lapply()` and `eapply()` over the mask; #1829 an
  inheriting `rlang::env_get()` on an environment the walk cannot place; #1830
  `match.fun(get)` (`rlang::as_function()` stays outside); #1831 a function
  built in place by `rlang::new_function()` or `as.function(list(quote()))`;
  #1832 a later summary selecting an earlier share by a string (`any_of()`
  excluded, which #1619's tests pin); #1833 a local bound by base `assign()`
  (`delayedAssign()` and the other binders stay refused).
- #1826 the parentheses around a function literal or `pkg::fn` on the right of
  a magrittr pipe; #1827 `%!>%` owns a data mask as `%>%` does (`%$%` does
  not).
- #1828 a failed first `across()` selection carried a generated
  `dplyr::all_of(resolve())` layer.
- DuckDB preflight: #1834 unquoted non-ASCII identifiers and Unicode spaces;
  #1835 `by(...)` is a call head on DuckDB 2.0; #1836 a star modifier is told
  from a multiplication.

## The sweeps of 2026-11-28

- #1843: seven more names DuckDB binds with no call head are listed, and
  `SHOW`, `DESCRIBE` and `SUMMARIZE` are refused by their keyword beside any
  caller macro. `tests/testthat/test-duckdb-implicit-reach.R` now asks DuckDB
  for this family, so a name it reaches from a form in
  `tests/testthat/helper-duckdb-reach.R` fails that test rather than waiting
  for a hunt. Still admitted by ADR 0020: a statement naming no relation and
  no call head, for which no catalog is read, and a name reached only from
  syntax the forms do not hold.
- #1844 (`wontfix`): a nested tidyr verb other than `uncount()` is read as
  ordinary code, so a column of the nested frame that shares an earlier
  summary's name is refused as a dependency. Registering tidyr's bare names
  would claim other packages' calls.
- #1845: the share dependency walk knows callees by name and the set is open
  (`cli::format_error("{sh}")`, `tibble::tibble_("sum(sh)")`,
  `glue::identity_transformer()`, `purrr::invoke_map()` on that date). One
  more callee the walk does not read is this issue, not a new one: the decided
  repair binds the share's name during a local summary.
  `tools/hunt/share-reader-sweep.R` lists the calls.
- #1846: no CI job runs DuckDB 1.5.
