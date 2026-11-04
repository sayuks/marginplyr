# Tidyselect across Margin verbs and contextual shares

Investigated: 2026-11-04
Tested package SHA: `7d5a18fadef58b399d2c7e0c0c644fd952eb4e6c`
Environment: R 4.6.1 on arm64 macOS, tidyselect 1.2.1, rlang 1.3.0,
dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1,
Arrow 25.0.1, DuckDB 1.5.5, RSQLite 3.53.3

## Selection contracts

The helper inventory came from the installed `tidyselect::vars_select_helpers`.
It contained `starts_with()`, `ends_with()`, `contains()`, `matches()`,
`num_range()`, `one_of()`, `everything()`, `last_col()`, `all_of()`,
`any_of()`, and `where()`. The tested operators included ranges, union,
intersection, difference, and ordered exclusions and re-inclusions.
The reference was tidyselect's evaluated result for each expression, which
also kept the distinction between `c(a, -b)` and `c(a, !b)` when the two
selected different columns. [Selection language](https://tidyselect.r-lib.org/reference/language.html)
and [evaluation options](https://tidyselect.r-lib.org/reference/eval_select.html).

- `.by` fixes keys and the arguments of all five Grouping constructors select
  dimensions. Each selection was compared with the corresponding positions
  and order from `eval_select()`. The plan oracle applied constructor
  semantics to those positions, including separate arguments versus one
  `c()` expression and composite dimensions.
  [Grouping specification](../../R/grouping-spec.R).
- Ordinary summary selectors were compared with dplyr after accounting for
  the keys excluded by the complete Grouping plan. A local ordinary summary
  can select preceding ordinary outputs. [Margin summary reference](../../R/summarize_with_margins.R).
- A column-wise Parent or Total share selects from preceding ordinary summary
  names. Predicates are refused under that contract. A direct share takes one
  bare summary name. [Contextual shares reference](../../R/share.R).

## Backend inspection matrix

[The backend matrix](backend-matrix.R) crossed 124 expressions with
`.by` and `grouping_set()`. It ran on a local tibble, a dtplyr root and a
derived step, an Arrow table and a derived Arrow query, a DuckDB table and a
derived DuckDB query, and a SQLite table. It used each backend's admitted
selection view as the reference: local values, a typed zero-row proxy, or a
name-only schema. It compared selected columns in order for accepted calls
and rejection for the invalid controls. It did not compare all condition
classes or messages.

| Input | Same ordered selection | Both rejected |
| --- | ---: | ---: |
| Local tibble | 238 | 10 |
| dtplyr root | 238 | 10 |
| Derived dtplyr step | 238 | 10 |
| Arrow table | 238 | 10 |
| Derived Arrow query | 238 | 10 |
| DuckDB table | 238 | 10 |
| Derived DuckDB query | 238 | 10 |
| SQLite table | 206 | 36 |
| Total | 1,872 | 106 |

The SQLite run omitted six placements involving `peek_data()`, for which the
test fixture had no matching predicate-capable proxy. It included explicit
refusals of `where()` and its combinations. DuckDB recorded one operation for
each successful inspection or predicate-result error, and zero for the strict
missing-name, offset, renaming, and missing-index errors. SQLite recorded
zero. An initial dtplyr fixture mutated a column after filtering, beyond the
recognized metadata-safe mutate grammar. That probe recorded 34 additional refusals in typed
placements and resolved its name-only placements. The table uses the same
arithmetic mutation directly on the root followed by filtering and projection.
The [dtplyr metadata classifier](../../R/backend-metadata.R) and the
[inspection contract](../../R/inspect-grouping.R) describe that boundary.

The [warning matrix](warning-matrix.R) compared the ordered delivered warning
classes and messages for 50 selector expressions in direct tidyselect, fixed
keys, and Grouping dimensions. It crossed single and repeated `one_of()`
misses with negation, accumulation, and `where()` before or after the helper.
The script printed no discrepancies. It ran with lifecycle warnings enabled.

## Findings

### F1. Column-wise shares accepted an indirect predicate

The literal selector `tidyselect::where(predicate)` failed with the
documented diagnostic for name-based selection. Binding `tidyselect::where`
to a different name, using a saved predicate through `.env`, negating that
predicate, and supplying a function literal all reached the predicate body.
The body received one synthetic integer position although the preceding
summary was a double. The positive case emitted a share and the negation
omitted it. Both Parent and Total shares diverged in all 40 indirect cases
across data.frame, tibble, eager data.table, dtplyr, and DuckDB.

The static spelling gate recognizes a literal `where()` and the share
selection evaluator admits predicates by default.
[Share selection resolver](../../R/share.R).

### F2. Later ordinary across outputs disappeared from share planning

With `x` equal to 1 and 3, an independent `prior = sum(y)`, and an ordinary
`across()` with `.names = "sum_{.col}"`, selecting `x / y` or
`one_of("x")` produced `sum_x` with values 1, 3, and 4 in the ordinary
summary. Adding a direct share of `sum_x` rejected that source as unknown.
Requesting shares with `across(starts_with("sum_"), ...)` silently left out
the share column. The equivalent selectors `x & !y` and `all_of("x")`
produced 0.25, 0.75, and 1 with either request style. The mismatch reached
all three eager frame classes in the replay. dtplyr and DuckDB matched
their supported controls.

The later-summary name cache treated `/` and `one_of()` as outside its
statically recognized ordinary selection grammar. The runtime summary still
evaluated the selector successfully.
[Summary name planning](../../R/summary-selections.R).

### F3. Excluded earlier summaries became false share dependencies

With `prior = sum(y)`, the ordinary scalar `source = sum(pick(c(x, -prior)))`
uses only `x`. A share of `source` nevertheless reported an earlier-summary
dependency on `prior`. The intersection `x & !prior` had the same result.
Using `all_of("prior")` or `any_of("prior")` in the exclusion preserved the
ordinary values and produced the expected shares. Positively selecting
`prior` earned the dependency refusal. The replay crossed four ordinary
consumers, two exclusions, both share helpers, and three eager frame classes.
All 48 bare-name exclusions diverged.

Dependency analysis included both the resolved selector outputs and the
syntactic bare names of the selection. That second contribution put the
excluded alias back into the dependency set.
[Summary selection](../../R/summary-selections.R).

### F4. Stringr patterns forced typed metadata for name matching

On a dtplyr step derived with `mutate(added = paste0(a, "x"))`,
`matches("^a")` selected `a, added`. An inline or prebound
`stringr::regex("^a")` instead triggered the unsafe metadata diagnostic.
Inline and bound `regex()`, `fixed()`, and `coll()` patterns reproduced
under complements and combinations in fixed keys, `grouping_set()`, and
`rollup()`. All 36 pattern placements refused. The 12 literal controls,
direct tidyselect, and ordinary `dplyr::select()` on the same lazy graph
agreed. Tidyselect documents stringr patterns for `matches()`.
[Pattern helper reference](https://tidyselect.r-lib.org/reference/starts_with.html).

On live DuckDB, a DBI trace recorded one zero-row query per malformed
`matches(stringr::regex("["))` placement across `inspect_grouping()`,
`expand_with_margins()`, and `summarize_with_margins()` with either `.by`
or `grouping_set()`.
The public SQL audit identified every traced call as `selection_proxy` and
recorded a `LIMIT 0` statement.
The six `matches("[")` controls raised without a query. The stringr path
deferred to typed metadata before delivering a diagnostic that the name-only
path could determine.
[Name-only helper classification](../../R/grouping-plan.R) and
[early-refusal policy](../../design/adr/0005-reject-local-errors-before-backend-reads.md).

### F5. Arrow renamed selections aggregated the alias field

In a two-key data set with `x = 1:2` and `y = 10:11`,
`across(c(y = x, -z), sum)` returned 10 and 11 under the output name
`y` instead of 1 and 2. The rollup grand total was 21 instead of 3.
The [replay](arrow-rename.R) also covers plain renaming and a named
`all_of()`. Six native Arrow comparisons and twelve Margin comparisons
diverged across a Table and a derived query. The query remained lazy
until the replay explicitly collected it. A separate probe also reproduced
with a RecordBatch and a Parquet Dataset. Local, dtplyr, SQLite, and DuckDB
aggregated the selected source in those probes. The Arrow control
`across(x, sum, .names = "y")` returned the intended values.

Native Arrow 25.0.1 reproduces the wrong value. Its installed
`across_setup()` uses the names of the selected frame as field names when
building the aggregation. The rename loses the original field identity at
that step.

### F6. Parenthesized exclusions changed meaning in ordinary summaries

For columns `x, y, z`, direct tidyselect resolves
`c(!ends_with("y"), (-starts_with("x")))` to `x, z, y`. A Margin
`across()` returned only `z`. The form without the parentheses selected
only `z` in both. The [mixed-exclusion replay](mixed-exclusion-backends.R)
compares both bare names and helpers across local data, dtplyr, live SQLite,
live DuckDB, and Arrow.

The local [four-consumer replay](parenthesized-exclusion.R) uses
`-c(any_of("missing"), (-starts_with("x")))`, which selects only `x`.
`across()`, `pick()`, `if_any()`, and `if_all()` diverged both first and
after an ordinary summary. The two inspection controls preserved the
selection. During ordinary summary rewriting, the inner negative operand
lost its parentheses. Tidyselect uses the written shape of the operands
directly inside `c()` to distinguish a subtractive argument from a positive
complement. The rewritten call therefore asked a different selection question.
[Summary selection rewrite](../../R/summary-selections.R).

## Coverage outside the finding replays

| Comparison | Executed | Result |
| --- | ---: | --- |
| Ordered Grouping plan, local | 10,432 | All matched |
| Ordered Grouping plan, simulated PostgreSQL | 1,632 | All matched |
| Public Margin verbs against explicit selected names | 6,496 | All matched |
| Separate constructor arguments against explicit name vectors | 1,210 | All matched |
| Ordinary local across, pick, if_any, if_all | 5,144 | 5,134 successful matches and 10 shared refusals |
| Later local across and named packed across | 2,572 | 2,050 successful matches and 522 shared refusals |
| Four ordinary summary consumers with fixed keys and a rollup | 756 | 734 successful matches and 22 shared refusals |
| Summary .by with a rollup and British spelling | 186 | 184 successful matches and two shared renaming refusals |
| Seeded nested summary selectors | 568 | 564 matches and four F6 divergences |
| Column-wise share name selections | 3,366 | All matched |

The [constructor matrix](constructor-selection-matrix.R) crossed the 11
helpers as ordered pairs under ten syntax templates. It covered all five
constructors, two composite placements, and fixed keys. It compared ordered
plans, error categories, and delivered warning classes and messages. The
[sibling matrix](sibling-selectors.R) separately put the two helper operands
in different constructor arguments, with `-` or `!` on the second.

The [consumer matrix](constructor-verb-matrix.R) compared the results of
inspection, expansion, both nesting verbs, and both summary spellings with
explicit selected names. It compared 2,496 local results, 2,496 dtplyr
results, and 1,504 simulated PostgreSQL results. The simulation compared
rendered SQL and an inspected local plan. SQL nesting was outside that
comparison under its public backend boundary.

The [summary matrix](summary-matrix.R) checked names and values against
dplyr and recorded success or refusal for invalid cases. The four-consumer
rollup matrix compared both detail and total values. The detail reference
grouped by the fixed key and the Margin dimension. The total reference
removed the omitted dimension from the ordinary summary selection before
grouping by the fixed key. A stratified lazy
matrix added 744 comparisons per backend. dtplyr had 626 matching successes,
12 shared refusals, 104 cases where Margin metadata supported a predicate
that ordinary dtplyr refused, and two literal `NULL` defaults that differed.
SQLite had 468 matching successes, 274 shared refusals, and two literal
`NULL` differences. Arrow had 181 matching successes, 11 shared refusals,
551 Margin refusals where the native operation succeeded, and one literal
`NULL` difference. Native Arrow controls showed fallback into R for an
`across()` alias absent from the source and for the probe aggregates with
`pick()`, `if_any()`, and `if_all()`. The Margin refusal in those controls
follows its documented query policy. The literal `NULL` variants in lazy
dplyr defaulted to all columns, while the Margin selection followed the
empty tidyselect result used locally. These comparisons did not treat those
backend boundaries as regressions.
[Summary contract](../../R/summarize_with_margins.R).

The [share matrix](share-matrix.R) compared ordered output names, ratios,
and success/refusal against tidyselect on preceding summary names. It ran
703 forms for each share helper on named local summaries. It sampled 196
forms for each share helper on local and dtplyr first-`across()` summaries,
and on named data.table, DuckDB, and SQLite summaries. The
[focused share replay](share-findings.R) reproduced F1 through F3. F2
produced 18 false errors and 18 silent omissions on eager frames and passed
16 lazy comparisons. An additional `all_of()` callback could explicitly
inspect the same synthetic integer values with `peek_data()`. Disabling
selection predicates alone would leave that separate callback path available.
The callback contract was outside F1's scope.

An existing binary subtraction report also reproduced on the tested SHA.
The preflight treated a bare binary `-` as a name-only selection even
though tidyselect evaluated its operands as an ordinary call. The pinned
replay in the previous investigation separated unary complement, qualified
subtraction, and the caller promise forced against the name proxy.
[Existing #1233](https://github.com/sayuks/marginplyr/issues/1233) and
[prior replay](../2026-11-04-nesting-grouping-followup/binary-minus.R).

## Ticket disposition

| Finding | Issue |
| --- | --- |
| F1. Indirect predicates in share selections | [#1252](https://github.com/sayuks/marginplyr/issues/1252) |
| F2. Missing prior across outputs | [#1253](https://github.com/sayuks/marginplyr/issues/1253) |
| F3. False dependency from an exclusion | [#1254](https://github.com/sayuks/marginplyr/issues/1254) |
| F4. Typed metadata for stringr name matching | [#1255](https://github.com/sayuks/marginplyr/issues/1255) |
| F5. Arrow alias used as source | [#1256](https://github.com/sayuks/marginplyr/issues/1256) |
| F6. Parentheses lost around exclusion | [#1257](https://github.com/sayuks/marginplyr/issues/1257) |

The six independent tickets carry `ready-for-agent`. All-state searches
covered Issues and pull requests with the following queries. The listed
neighbors described distinct triggers.

| Findings | Queries | Neighboring reports |
| --- | --- | --- |
| F1 | `"name-based tidyselect" "predicate"`, `"share" "predicate" "bypass"`, `"share" "allow_predicates"` | [#191](https://github.com/sayuks/marginplyr/pull/191), static helper spelling |
| F2 | `"share" "one_of"`, `"share" "set difference"`, `"contextual share" "statically named"` | [#342](https://github.com/sayuks/marginplyr/issues/342), Grouping preflight and [#1183](https://github.com/sayuks/marginplyr/issues/1183), naming glue |
| F3 | `"earlier summary alias" "selection"`, `"excluded alias"`, `"share" "excluded" "alias"` | [#899](https://github.com/sayuks/marginplyr/issues/899), eligibility of named scalar output and [#998](https://github.com/sayuks/marginplyr/issues/998), dependencies of sibling outputs |
| F4 | `"metadata" "regex" dtplyr`, `"stringr" "tidyselect"`, `"matches" "regex"` | [#342](https://github.com/sayuks/marginplyr/issues/342), [#346](https://github.com/sayuks/marginplyr/issues/346), earlier name-only grammar |
| F5 | `"Arrow" "across" "rename"`, `"Arrow" "selection" "alias"`, `"Arrow" "wrong column"` | [#435](https://github.com/sayuks/marginplyr/issues/435), packing and [#992](https://github.com/sayuks/marginplyr/issues/992), hidden Dataset field names |
| F6 | `"across" "nested" "exclusion"`, `"summary" "parenthesized" "exclusion"`, `"tidyselect" "parentheses" "negative"` | [#869](https://github.com/sayuks/marginplyr/issues/869), parentheses around the across call and per-group evaluation |

Apache Arrow's tracker searches included `"across" "rename" "dplyr"`,
`"across" "all_of"`, and `"across" "wrong column"`. The results
[#38755](https://github.com/apache/arrow/issues/38755) and
[#37529](https://github.com/apache/arrow/issues/37529) covered different
behavior. The F5 ticket records the impact within marginplyr.

## Replay

Run the scripts with the archived tested revision as the working directory
and pass the path to the script from this note. The matrices are finite and
use the package versions listed above plus stringr 1.6.0 and DBI 1.3.0.
The inspection, constructor, and summary matrices sampled real and simulated
backends as described. No live PostgreSQL server or version matrix was used.

```sh
Rscript /path/to/constructor-selection-matrix.R
Rscript /path/to/constructor-verb-matrix.R local dtplyr postgres_sim
Rscript /path/to/sibling-selectors.R
Rscript /path/to/backend-matrix.R
Rscript /path/to/warning-matrix.R
Rscript /path/to/summary-matrix.R local
Rscript /path/to/summary-matrix.R scope
Rscript /path/to/summary-matrix.R backend
Rscript /path/to/summary-matrix.R by
Rscript /path/to/summary-matrix.R grouping
Rscript /path/to/summary-matrix.R deep
Rscript /path/to/share-matrix.R local simple both
Rscript /path/to/share-matrix.R local across both
Rscript /path/to/share-matrix.R data.table simple both
Rscript /path/to/share-matrix.R DuckDB simple both
Rscript /path/to/share-matrix.R SQLite simple both
Rscript /path/to/share-matrix.R dtplyr across both
Rscript /path/to/share-findings.R
Rscript /path/to/matches-patterns.R
Rscript /path/to/arrow-rename.R
Rscript /path/to/parenthesized-exclusion.R
Rscript /path/to/mixed-exclusion-backends.R
```

The five focused scripts at the end accept `--assert` to check the desired
behavior. A failing assertion is expected on the investigated SHA.
