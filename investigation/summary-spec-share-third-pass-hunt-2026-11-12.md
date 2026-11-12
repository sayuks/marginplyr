# Third-pass hunt over Margin summaries, Grouping specifications and shares

Investigated: 2026-11-12
Repository snapshot: `26846d0127f333250668e928e53b200d04c99ca5`

The investigation ran against freshly fetched main in one worktree. No package
implementation, package test, dependency, public specification or generated
documentation was changed, and no repair was implemented.

Six fresh-context investigators worked read-only in the same checkout from
separate starting points and reported before any candidate was shared: A from
the neighbourhood of the repairs merged since
`summary-spec-share-second-pass-hunt-2026-11-11.md` (#1582 to #1596), B from
the grammar, typing and plan interactions of Total shares, C from Grouping
specifications checked against SQL executed by hand on DuckDB, D from the
summary expressions `dplyr::summarise()` accepts, E from lazy backends and
their typed results, F from Margin order, Margin labels, argument validation
and Package conditions. The coordinator replayed every reported finding in a
fresh R process from its own script, and replayed the candidates left for a
decision that are quoted with output below.

A server restart during the run removed the investigators' scratch
directories. Every script under
`investigation/2026-11-12-summary-spec-share-third-pass/` was rebuilt and run
after it. The case counts under *Covered without a finding* are the
investigators' reports of runs made before the restart, whose generating
scripts were not all rebuilt.

Versions were R 4.6.1 on arm64 macOS, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3,
data.table 1.18.6.1, duckdb 1.5.5, RSQLite 3.53.3, arrow 25.0.1, tidyr 1.3.2,
rlang 1.3.0 and vctrs 0.7.3. SQL work used process-owned in-memory SQLite and
DuckDB databases, or files under the session's temporary directory, holding
synthetic tables. No existing database or outside data was read.

## Method and independent expectations

Expected values never came from marginplyr internals or from agreement between
two marginplyr paths. Summaries were compared with ordinary dplyr summaries at
each explicit grain, bound in Grouping-plan order. C wrote its own plan
expander and, separately, ran hand-written `GROUP BY GROUPING SETS` on DuckDB
as the reference for rows and `GROUPING()` values. F wrote the documented
Margin order key by hand. Share denominators came from a separate summary at
the Grand total grain inside each fixed partition. Every cross-backend
difference was followed by the native control on the differing backend before
it was counted.

Each candidate was checked against the issue tracker (open and closed, through
`gh issue list --search`), `investigation/`, `tests/testthat/`, `NEWS.md` and
the ADR governing it.

## Confirmed findings

Each script under `investigation/2026-11-12-summary-spec-share-third-pass/`
is run from the repository root, prints the observed behaviour and exits zero.

| Boundary | Established effect | Script | Issue |
| --- | --- | --- | --- |
| `dplyr::c_across()` in a local summary | Its selection reached the grouping keys and marginplyr's private columns, so values, and shares over them, were wrong with no diagnostic, or a valid call was refused naming a private column. | `c-across-reads-keys-and-private-columns.R` | [#1599](https://github.com/sayuks/marginplyr/issues/1599) |
| Sorted SQLite summary with a text Margin label, a fixed key, and neither `.id` nor a share | The fixed key lost its declared type: a `DATE` key collected as character, and an empty or all-missing key as logical. | `sqlite-sorted-labelled-fixed-key-type.R` | [#1600](https://github.com/sayuks/marginplyr/issues/1600) |
| `compute()` of an unsorted SQLite summary without `.id` or a share | The destination table was created with untyped columns where native `compute()` kept the declaration, so an empty or all-missing column collected as logical. | `sqlite-unsorted-compute-untyped.R` | [#1601](https://github.com/sayuks/marginplyr/issues/1601) |
| Parenthesized `list()` as `across()` `.fns` on a lazy input | Since #1582, DuckDB ignored `.names`, and dtplyr and Arrow refused a call that returned rows before it. | `across-parenthesized-fns-list.R` | [#1602](https://github.com/sayuks/marginplyr/issues/1602) |
| `.check_margin_label = TRUE` over an Arrow input whose ordering names one column twice | The check's query failed at construction, where the same call with the check off succeeded. | `arrow-repeated-sort-key-label-check.R` | [#1603](https://github.com/sayuks/marginplyr/issues/1603) |
| Invalid `grouping_bit()` or `grouping_id()` call with `.check_margin_label = TRUE` on a lazy input | The refusal came only after the label-collision query had read the input, and a collision in the data was reported in its place. | `helper-refusal-after-collision-query.R`, `helper-refusal-reads-input.R` | [#1604](https://github.com/sayuks/marginplyr/issues/1604) |
| A local summary that is `NULL` in some grouping sets only | A bare `simpleError` with no call reported "Union branch 2 does not have the columns of branch 1". | `null-summary-in-one-set.R` | [#1605](https://github.com/sayuks/marginplyr/issues/1605) |

`sqlite-type-boundary-matrix.R` maps the two SQLite findings across label,
sort, `.id`, share and plan.

The `to-tickets` workflow published these as seven repair slices with no
prerequisite prefactoring; splitting had been delegated without a
confirmation checkpoint. Six carry `bug` and `ready-for-agent` and can start
independently. #1601 was published with `bug` and `ready-for-human`, because
whether the typed-materialization promise reaches an unsorted result that
dbplyr materializes was a specification choice, and blocked by #1600, which
repairs the missing fixed-key anchor one of its two shapes shares. The
maintainer decided the same day that the promise reaches those results, and
#1601 was rewritten as a repair and relabelled `ready-for-agent`; the
blocking edge stayed.

### `dplyr::c_across()` in a local summary

```r
d <- tibble::tibble(g = c(100, 100, 200, 200, 300),
                    v = c(1, 2, 3, 4, 10), w = c(1, 0, 2, 5, 2))
summarize_with_margins(d, u = sum(c_across(everything())),
                       t = share_of_total(u),
                       .grouping = rollup(g), .margin_label = NULL)
```

`group_by(d, g) |> summarise(u = sum(c_across(everything())))` returned `4,
14, 12`, and the same summary over `select(d, -g)` returned `30`: native
dplyr never lets `c_across()` select a grouping column. The Margin summary
returned `204, 414, 312` and a Grand total of `1830`, with shares `0.111,
0.226, 0.170`. 204 is 4 plus the key over the group's two rows; 1830 is 30 plus
the key and its private copy, 900 each. With `.by = g` alone, or `group_by(d, g)` and no
`.grouping`, the values were the same wrong `204, 414, 312`.

With a character key the call was refused: ``Can't combine `g` <character> and
`v` <double>``. `c_across(-g)` named ``..marginplyr_key_1``, and a
`c_across()` written after a share named ``..marginplyr_share_1``. Data
frames, tibbles, grouped tibbles and direct data.tables reproduced.
`c_across(c(v, w))`, `c_across(where(is.numeric))` over non-numeric keys, a
call with no keys, and `pick()` or `across()` in the same position were
correct; 30 selection forms through `across()`, `pick()`, `if_any()` and
`if_all()` agreed with native grouped dplyr.

*Relationship to dplyr summaries* says that `across()` and `pick()` "cannot
select any column named in the complete Grouping plan", extending "dplyr's
grouping-column rule across every branch", and that "Every other name follows
ordinary lookup". `summarize_margin_union()` groups each branch by private
copies of the keys, which leaves the original key columns, and in a set that
omits a dimension its copy, in dplyr's mask as ordinary columns.
`rewrite_summary_selections()` hides them from the spellings
`R/contextual-helpers.R` lists, `across`, `if_any`, `if_all` and `pick`;
`c_across` is not among them. No file in the repository and no issue named
`c_across`.

The same mask was visible two other ways, neither counted here. A selection
helper reached through a caller's function,
`f <- function() dplyr::pick(dplyr::everything())`, returned
`g+a+v+w+..marginplyr_key_1` for the names on a Grand total row where native
dplyr returned `a+v+w`; the Rd says a helper reached at run time is not
rewritten. On dtplyr, `names(.SD)` inside a summary listed the key and the
private copy where native dtplyr listed neither.

### Sorted SQLite summary with a text label and a fixed key

```r
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
DBI::dbExecute(con, "CREATE TABLE t (dt DATE, g TEXT, x REAL)")
summarize_with_margins(tbl(con, "t"), s = sum(x, na.rm = TRUE),
                       .by = dt, .grouping = rollup(g), .sort = "last")
```

Native `summarise()` grouped by `dt`, the local data frame, and the same call
unsorted, with `.margin_label = NULL`, with `.id` or with a share all
collected `dt` as `Date`. With the default label and `.sort = "last"` or
`"first"` it collected as character. On a default connection, an `INTEGER`,
`TEXT` or `REAL` fixed key of a zero-row table, or of a table whose key was
all missing, collected as logical under the same call and as its declared
type under every sibling. A one-set plan reproduced, and `compute()` of the
affected result did too. DuckDB, Arrow, dtplyr and local inputs did not.

*Fixed columns and grouping dimensions* says "Columns in `.by` retain their
input types", and *Result class and attributes* that "Direct collection
preserves source types and restores package-declared types in empty or
all-missing columns". By reading, `summarize_with_margins()` left
`final_type_anchor` empty for this shape, while `finalize_margin_operation()`
declared the text-labelled dimension as character, which made
`sqlite_typed_result()` build the sorted result with no source anchor for the
fixed key. `test-sqlite-typed-order.R` covered fixed keys only with `.id` or a
typed-missing label. Closed #862, #1455 and #1484 concern
`expand_with_margins()`; #640, #651, #626 and #653 typed-missing dimensions,
identifiers and shares.

### `compute()` of an unsorted SQLite summary

Two shapes reached it. With no fixed key, a typed-missing label, a plan of
more than one set and neither `.id` nor a share, the dimension's destination
column was untyped: `CREATE TABLE out(i, s)` where native `compute()` of
`summarise()` grouped by `i` gave `out(i INT, s)`. With a fixed key and a text
label, again without `.id` or a share, the fixed key was untyped. In both, a
column that was empty or all missing collected from the destination as
logical. Direct `collect()` of the unsorted result kept the type in both
shapes. A one-set plan in the first shape, `.id`, a share, and `.sort` in the
first shape kept it through `compute()`.

*Result class and attributes* says "Direct materialization creates a typed
table with public columns only", and ADR 0031's scope names "sorted and
unsorted summaries". `design/specs/sqlite-b-direct.md` also says "delegated
unsorted and materialized collection retain their existing dbplyr behavior",
and the result in these shapes is a plain `tbl_SQLiteConnection` whose
`compute()` is dbplyr's `CREATE TABLE AS` over a compound select. Whether the
promise reaches these shapes is therefore a reading of two texts; that native
`compute()` and every sibling call kept the type is not. Closed #862 treated
the same symptom as a defect for expansions.
`test-sqlite-b-direct-compute.R` computed unsorted results only with `.id`.

### Parenthesized `list()` as `across()` `.fns`

```r
summarize_with_margins(
  x, across(c(v, w), (list(a = ~ sum(.x, na.rm = TRUE))),
            .names = "{.fn}_{.col}"),
  .grouping = rollup(g)
)
```

dplyr at grain `(g)`, the local data frame and SQLite named the outputs
`a_v, a_w`. DuckDB named them `v_a, w_a`; dtplyr raised "`.fns` argument to
dtplyr::across() must be a NULL, a function, formula, or list"; Arrow refused
with "Arrow cannot evaluate this summary". A's run of the same script against
an archive of `22043a46`, made before the restart and not repeated by the
coordinator, returned `a_v, a_w` on all three. Native dbplyr gave `v_a, w_a`
for the parenthesized list and native dtplyr raised the same error, so the
behaviour after #1582 is each backend's own, from which the earlier rebuild
had shielded the caller.

ADR 0019's amendment *a rebuilt call keeps the caller's pair* keeps a pair
around a rebuilt call and says "A pair around a qualified name or a function
literal stays dropped: ... dbplyr ignores `.names` for an `across()` whose
`.fns` is parenthesized, so keeping that pair would change the output names a
lazy `across()` gives today." `rebuild_static_call()` exempts `::`, `:::` and
`function`; a `list()` or `c()` of functions is the remaining `.fns` form and
is re-wrapped. `(list(sum, max))` with `.names = "{.col}.{.fn}"` and
`(c(a = sum, b = max))` changed as well, and `.by` alone reproduced. A bare
list, `(function(z) ...)`, `(\(z) ...)` and `(base::sum)` did not change. No
test passed a parenthesized `list()` as `.fns`.

### `.check_margin_label = TRUE` over a repeated Arrow sort key

`arrange(v) |> arrange(w, v)`, `arrange(v, desc(v))` and `arrange(v, v)` over
an Arrow table, followed by a Margin summary with the check on, failed at
construction with Arrow's "Invalid sort key column: Multiple matches for
FieldRef.Name(v)". With the check off the same call returned three rows.
A single key, two different keys, a repeated key that is itself the checked
dimension, `arrange() |> head()`, DuckDB and SQLite were correct, as were 16
further arranged shapes on Arrow with the check on. `expand_with_margins()`
shares the check and reproduced.

`check_observed_label_collision()` selects the columns it reads and then
collapses the query; Arrow itself cannot project away a column that occurs
twice among the sort keys, as `select(g)` over the same input shows, so the
boundary #1585 added is reached too late for this shape. A native Arrow
`summarise()` over any arranged input fails, so the reference is the same
call with the check off and #1585's first acceptance criterion. Only an
in-memory Table was tried.

### An invalid Grouping helper call is refused after the label check reads

With a lazy input and `.check_margin_label = TRUE`, `grouping_bit()` with no
argument, two arguments or a non-bare argument, and `grouping_bit()` or
`grouping_id()` naming a column outside the plan, were refused only after the
`observed_label_collision` query had been recorded. A share used inside a
larger expression, `cur_group_id()`, a summary overwriting a key and an
invalid `.sort` were refused with no query recorded. With the SQLite table
dropped first, the helper call raised "no such table: t" in place of its
refusal; with the parquet file behind an Arrow dataset deleted, Arrow's
IOError. Where the data held the label, the collision was the condition
reported, on SQLite and dtplyr. DuckDB reproduced. With the check off, which
is the lazy default, the refusal came first on every backend.

ADR 0005 says Margin verbs reject invalid arguments "before acquiring backend
metadata or executing a Margin label validation query", and that preparation
"must not contact a backend for an error that can already be determined from
the call and input metadata available locally". The helpers' arity and
bare-name checks are decidable from the call, and plan membership from the
resolved plan. Those checks live in `grouping_helper_vars()` and
`abort_not_a_grouping_column()`, reached through `rewrite_grouping_dots()`
from `stage_margin_summaries()`, which `execute_margin_summary()` calls after
`validate_margin_operation()`. The tests of the three refusal messages did
not combine them with the check or count queries.

### A local summary that is `NULL` in some grouping sets only

`summarize_with_margins(d, val = if (grouping_bit(g) == 1L) NULL else sum(v),
.grouping = rollup(g))` raised a `simpleError` whose call was `NULL`: "Union
branch 2 does not have the columns of branch 1. Only in branch 1: val". A
`NULL` at the detail grain, an unnamed scalar summary, an `across()` function
and a direct data.table reproduced. `NULL` in every set returned the result
without the column, and an unnamed frame against `NULL` raised the Package
condition #1441 added. `bind_rows()` of the per-grain dplyr summaries filled
the missing column with `NA`.

The message comes from the bare `stop()` in `check_branch_columns()`. #1441
cited ADR 0015 for a Package condition "when callers can rewrite the call" and
covered unnamed frame summaries. dtplyr failed with data.table's own message.

## Left for a decision

These were observed and are not counted, because the text that would decide
each one either pins the behaviour or does not exist.

After the maintainer read this list on the same day, seven of the twelve were
ticketed and `left-for-a-decision-replays.R` replays them beside their
controls: the dot-prefixed output names
([#1607](https://github.com/sayuks/marginplyr/issues/1607), `ready-for-human`), the collected class of a sorted SQLite
summary ([#1608](https://github.com/sayuks/marginplyr/issues/1608)), the no-Grand-total advice ([#1609](https://github.com/sayuks/marginplyr/issues/1609)), the
share helper's argument name ([#1610](https://github.com/sayuks/marginplyr/issues/1610)), the legacy locale
([#1611](https://github.com/sayuks/marginplyr/issues/1611)), the dtplyr type mix ([#1612](https://github.com/sayuks/marginplyr/issues/1612)) and the
parenthesized function with `.names` ([#1613](https://github.com/sayuks/marginplyr/issues/1613), blocked by #1602).
The replay added three measurements. Following the no-Grand-total advice
literally, with an empty `grouping_set()` as a further product factor, met
the same refusal. `share_of_parent(y = s)` was accepted as
`share_of_total(y = s)` was. Native dbplyr on DuckDB and SQLite and native
dtplyr named the outputs of `across(c(v, w), (sum), .names = "s_{.col}")`
`v, w`, so the DuckDB Margin summary matched its backend and the SQLite and
dtplyr Margin summaries were the ones that kept the template.
On that measurement the maintainer closed #1613 the same day as not planned:
the DuckDB result followed its backend and predated #1582, so it was neither
a defect of the package nor a regression.

- A dot-prefixed output name one edit from a short option was refused:
  `.y = mean(v)` raised "`.y` is not an argument ... Did you mean `.by`?", as
  did `.b`, `.i`, `.idx`, `.ids` and `.by2`, where `.x`, `.n` and `.key` were
  accepted and native `summarise()` accepts all of them.
  `nearest_summary_option()` applies an edit distance of one deliberately
  (#77) and `test-verb-argument-admission.R` pins `.ids`; no Rd or NEWS text
  describes the guard.
- A sorted SQLite summary without a helper collected as a bare `data.frame`,
  where native dbplyr, the unsorted call and DuckDB collected a tibble.
  `test-sqlite-grouping-helper-types.R` pins the class; *Result class and
  attributes* says classes follow the native summary through direct
  `collect()`.
- The refusal of a Total share over a plan with no Grand total set advised
  "Add an empty `grouping_set()` to the `grouping_sets()` specification" for
  `grouping_spec(grouping_set(a), rollup(g))` and for a bare
  `grouping_set(a)`, where the caller wrote no `grouping_sets()` and adding
  the empty set as a further product factor leaves the plan unchanged.
  ADR 0017 promises the fix named for `grouping_sets()`.
- `share_of_total(y = s)` was accepted and computed, although the helper's
  formal is `x`.
- Under `options(dplyr.legacy_locale = TRUE)` a local Margin order followed
  the system locale while dtplyr and SQLite stayed in C order.
  `order_margin_result()` calls `dplyr::arrange()` without `.locale`; *Margin
  order* says the order is independent of the locale and also that it
  promises what `arrange()` promises. The option is opt-in and deprecated.
- A dimension written outside an aggregate, `f = b` under `rollup(a, b)`,
  gave a size error locally, repeated rows on dtplyr, one source value on
  SQLite margin rows, `NA` on DuckDB margin rows and a refusal on Arrow. Each
  matched its native control, and no text says such a reference is
  unspecified.
- On dtplyr a dimension read inside a summary was the whole per-group vector,
  as on a local data frame, where native dtplyr reads one value;
  `rewrite_dtplyr_key_read()` does this deliberately, while the Rd says
  calculated summaries follow native evaluation "for their backend".
- dtplyr refused `if (grouping_bit(g) == 1L) NA else sum(v)` for mixing
  integer and logical between sets, where the local data frame returned an
  integer column.
- `across(c(v, w), (sum), .names = "s_{.col}")` named its outputs `v, w` on
  DuckDB and `s_v, s_w` on SQLite, local and dtplyr, at this snapshot and at
  `22043a46`.
- A Margin summary over an input holding a nested unlimited `arrange()`
  relayed dbplyr's "ORDER BY is ignored in subqueries" warning two or three
  times where the native summary relayed it once.
- The variable dimensions of a result were ordered by first appearance across
  the expanded sets in plan order, which for
  `grouping_spec(grouping_sets(v, cube(b, a)), grouping_set(a), v)` gave
  `v, a, b`; the Rd says "in plan order" without defining it for dimensions.
- Option refusals named `summarize_with_margins()` in their text when
  `summarise_with_margins()` was called; the blamed call was the alias.

## Rejected candidates

- `sum(v) %in% (c(3L, 14L, 17L))` failed on SQL where it worked at
  `22043a46`, and `sum((pick(v, w))$w)` raised a translation error where it
  had returned `NA`: native dbplyr does both for the caller's spelling, which
  #1582 chose to follow.
- dtplyr `s = NA_real_`, `u = NULL` beside another summary, `.env$x` inside a
  function, and an input column named `.N` or `.I` failed: native dtplyr
  failed identically.
- `xor(grouping_bit(g), grouping_bit(h))` was true for `(1, 1)` on SQL: dbplyr
  renders `xor()` without the grouping it needs, natively too.
- `a = 1L` gave one Grand total row on SQL where native dbplyr gives one row
  per input row: dbplyr's own rule for a summary without an aggregate.
- A `NaN` numerator or denominator gave a `NaN` share on DuckDB and `NA`
  locally: the Rd describes local `NaN` as missing.
- SQLite shares over Date, character or logical sources under
  `.check_share_source = FALSE`, and DuckDB's binder errors for such sources:
  documented.
- `rollup(all_of("no{pe}"))` failed inside cli interpolation:
  `dplyr::select(d, all_of("no{pe}"))` failed identically.
- `c(spec1, spec2)`, `list(spec)`, `grouping_id(a, a)`, `grouping_id("a")`
  and `rollup(everything())` overlapping `.by` were refused: each documented.
- `n_distinct(a)` on SQLite did not count a missing value: `COUNT(DISTINCT)`.
- `v = sum(v), w = v * 2` read a source row on SQLite and failed on DuckDB:
  native `summarise()` did the same on each.
- `.so = "last"`, `.margin = "T"` and `.keep = TRUE` became summaries: more
  than one edit from an option.
- SQLite errors carried two or three `purrr::map()` index layers: a native
  `union_all()` of two summaries carries them too.
- An Arrow zero-row factor key lost its levels and a DuckDB factor its `NA`
  level: native Arrow and DuckDB.
- A string label ordered numeric, Date and logical dimensions as text, and a
  cube's set order differed from PostgreSQL's: rejected in the earlier notes.

## Covered without a finding

- Recent repairs: 38 expressions around the #1582 rebuild on five backends,
  and 50 parenthesized forms compared between `22043a46` and this snapshot;
  20 helper placements under #1587 on five backends; 44 pipelines around
  `arrange()`; 35 `across()` naming forms; 41 special names in six roles on
  data.table and dtplyr; 336 labelled dtplyr results for a valid
  self-reference; 600 sorted calls over keys holding `NaN` and `NA`; 20
  DuckDB registration shapes.
- Total shares: about 420 calls on up to six inputs. 41 direct and 74
  `across()` share forms, each refusal the same Package condition on every
  input; 36 source types and values on five backends; the share a double on
  zero-row inputs, all-missing partitions, `collect()` and `compute()`; 25
  plans on five backends including every position of the Grand total set and
  duplicates kept or dropped; case-equivalent names on SQL; and DBI tracing
  that found no data-reading query at construction beyond the dialect probe
  over a literal.
- Grouping specifications: 611 generated specifications on six backends
  against hand-written DuckDB `GROUP BY GROUPING SETS`, for row multisets,
  `GROUPING()` against `grouping_id()` and `grouping_bit()` in every argument
  order, column order and `.id` against `inspect_grouping()`; 252
  specifications with dimensions read inside aggregates against per-grain
  dplyr; 278 product-heavy plans under each `.duplicates`; 28 selection
  spellings over six input shapes on seven backends; specifications stored,
  built by functions, spliced and serialized.
- Summary expressions: about 2,500 comparisons. Programmatic forms (`{{ }}`,
  `!!!`, glue names, `.data`, `.env`, `do.call`, mapped calls, nested Margin
  verbs); about 270 local argument sets over sizes, shapes and classes; about
  135 expressions on four lazy backends against each backend's native result;
  zero-row inputs under five plan kinds on seven inputs; about 95 condition
  comparisons for class, message and blamed call; and each expression
  evaluated once per group per set.
- Lazy backends: type matrices on DuckDB (18 types, and 540 ENUM cases),
  SQLite (13 declared types on both connection modes), Arrow (about 28 types
  on five input kinds) and dtplyr (13 classes); about 20 input compositions
  before the verb and about 35 verbs after it; the Sent query record over
  views, joins and `sql()` subqueries with every DBI generic traced; 57
  awkward identifiers in seven roles; 24 labels needing escaping.
- Presentation: exact Margin order on seven input kinds against a
  hand-written key, about 2,300 cases including non-hierarchical sets,
  duplicates, factor levels and both label positions; 15 collision scenarios
  under three check settings on five backends; about 100 wrong-type argument
  calls and about 85 condition probes with brace, quote and non-ASCII
  spellings through the alias, wrappers, `do.call` and pipes; 16 grouped or
  `.by` shapes on seven backends.

## Not covered

No remote dialect was executed. The `c_across()` mask was not examined in the
expansion and nesting verbs or on a row-wise input; the SQLite fixed-key type
was not examined in `nest_with_margins()`; the repeated Arrow sort key was not
tried on a Dataset or RecordBatch. Whether Package refusals other than the
Grouping helpers' also follow the label-collision query was not established:
the Arrow "cannot evaluate this summary" refusal and the share-source dialect
refusal were seen after it, and both depend on backend metadata. Interrupts,
real `sf` inputs, Arrow extension types, DuckDB nested types as dimensions,
SQLite virtual tables, `NaN` and infinite dimensions on SQL backends, plans
above 300 sets, and collations under `.sort` were not examined.
