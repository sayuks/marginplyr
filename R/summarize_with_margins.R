#' Summarize data with SQL-style grouping operations
#'
#' [summarize_with_margins()] extends [dplyr::summarize()] with grouping sets,
#' rollups, cubes, totals, and subtotals. The same interface works with local
#' data frames and lazy tables.
#'
#' Use it when one report needs several levels of detail—for example, stores,
#' region subtotals, and a company total—or when that report should use the
#' same code locally and through [dbplyr::tbl_lazy()]. The [Get started
#' guide][started] builds that report from a hand-written version, and is the
#' page to read before this one.
#'
#' [started]: https://sayuks.github.io/marginplyr/vignettes/get_started.html
#'
#' @param .data A data frame or lazy table.
#' @param ... Name-value pairs as used in [dplyr::summarize()]. Contextual
#'   helpers [grouping_bit()], [grouping_id()], [share_of_parent()], and
#'   [share_of_total()] can also be used here.
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns included in
#'   every grouping set. These columns never receive `.margin_label`. When
#'   `.data` is grouped and `.by` is `NULL`, its grouping columns are used as
#'   implicit fixed keys. A fixed key is a column of the input, so a selection
#'   cannot rename it: `c(area = region)` is an error rather than a fixed key
#'   named `area`. Rename the result afterwards with [dplyr::rename()].
#' @param .grouping A Grouping specification made with [grouping_set()],
#'   [grouping_sets()], [rollup()], [cube()], or [grouping_spec()]. `NULL`
#'   represents one empty grouping set. Any expression returning a
#'   specification can be written here, while a specification nested inside
#'   another must be a constructor call or a name bound to one, and a nested
#'   name the input also has a column for is refused rather than read either
#'   way; see [grouping_set()].
#' @param .margin_label A display label for dimensions omitted from a grouping
#'   set. An unnamed character scalar applies to every resolved Margin
#'   dimension. A named character vector or named list must name every
#'   resolved Margin dimension exactly once; order is irrelevant, and fixed
#'   `.by` columns must not be named. `NA_character_` and `NULL` use typed
#'   missing values instead of a display label; only the list can give one
#'   dimension a `NULL` while another takes a label, because `c()` drops a
#'   `NULL` element. See *Display labels and grouping identity*.
#' @param .margin_label_position Either `"last"` (the default) or `"first"`,
#'   and nothing else; see *Option arguments*.
#'   This controls the position of a non-missing synthetic label in factor and
#'   ordered-factor levels. It does not sort result rows and has no effect for
#'   `NA_character_` or `NULL`.
#' @param .check_margin_label A logical scalar controlling the half of the
#'   Margin label collision check that reads the data: whether any *value* of a
#'   Margin dimension is equal to that dimension's display label. Each
#'   dimension is checked independently, and a typed-missing label opts its
#'   own dimension out -- `NULL` or `NA_character_`, chosen per dimension in a
#'   named-list `.margin_label`.
#'   Every Margin verb uses the same default: `TRUE` for local data frames and
#'   `FALSE` for lazy inputs, which are read only when the caller asks. A label
#'   check after collecting a lazy input therefore defaults to `TRUE` instead
#'   of `FALSE`; set `.check_margin_label` explicitly when following a
#'   collect-first remediation.
#'   A label equal to a declared factor *level* is rejected wherever
#'   marginplyr holds the levels, whatever this argument says, because finding
#'   it sends no query. Left unchecked, a colliding value gives the result a
#'   margin row and a source row that no grouping column tells apart; keeping
#'   `.id` in the result distinguishes them at no added cost. See *Display
#'   labels and grouping identity* for the factor missing-value contract.
#' @param .check_share_source A logical scalar, `TRUE` by default on every
#'   backend, including lazy ones: establishing that a share's source summary
#'   is an eligible type reads none of your data. `FALSE` calculates a
#'   requested [share_of_parent()] or [share_of_total()] from a source whose
#'   eligibility marginplyr could not establish, which is a SQL dialect that
#'   converts a value of another type to a number rather than refusing it, and
#'   a backend that could not be asked which of those it does. It changes
#'   nothing where the rule can be applied — a local data frame, a `dtplyr`
#'   step, and a database that refuses an ineligible summary itself all apply
#'   it whatever this argument says. It is scoped to the call it is written in:
#'   there is no session default and no connection default that records the
#'   answer once. See *Contextual shares*.
#' @param .duplicates One of `"error"`, `"drop"`, or `"keep"`, and nothing
#'   else; see *Option arguments*. A duplicate is a grouping set whose columns
#'   are the same as an earlier set's once the Grouping specification is
#'   expanded, as in `grouping_sets(region, store, region)`. `"error"` (the
#'   default) refuses the call and names the positions. `"drop"` keeps the
#'   first occurrence of each set and removes the rest. `"keep"` returns the
#'   rows of every occurrence, so a duplicated set's rows appear once per
#'   occurrence; `.id` tells the copies apart.
#' @param .sort One of `"none"` (the default), `"last"`, or `"first"`, and
#'   nothing else; see *Option arguments*. `"none"` leaves row order
#'   unspecified. `"last"` places each margin row after the rows it
#'   summarizes and `"first"` before them, within each fixed `.by` key, with
#'   missing values last. See *Margin order*.
#' @param .id `NULL`, or one non-missing, non-empty character string naming an
#'   integer output column of one-based Grouping set identifiers. Each value
#'   identifies one occurrence in the resolved Grouping plan. The name must not
#'   collide with source columns, grouping keys, or other output columns. See
#'   *Grouping set identifiers*.
#'
#' @return An ungrouped data frame, or a lazy table when `.data` is lazy,
#'   with one row per group per grouping set. Its columns are the fixed `.by`
#'   keys, then the grouping dimensions, then `.id` when supplied, then the
#'   summaries in the order written. Its class follows [dplyr::summarize()];
#'   see *Result metadata*.
#'
#'   With a character `.margin_label`, the default, a grouping dimension that
#'   is not a factor becomes character so that it can hold the label:
#'   `.grouping = rollup(year)` over an integer `year` returns `year` as
#'   character, with `"Total"` on the margin row. A factor dimension instead
#'   gains the label as a new level. `.margin_label = NULL` or `NA_character_`
#'   keeps every dimension's type and marks margin rows with a missing value.
#'
#' @details
#' [grouping_sets()] forms a union of grouping families. [grouping_spec()]
#' combines its arguments by Cartesian product, matching comma-separated SQL
#' `GROUP BY` items. [grouping_set()] is also used to keep multiple columns
#' together as one composite dimension inside [rollup()] or [cube()].
#'
#' Grouping specifications accept column selections, not arbitrary SQL
#' expressions. Create computed grouping columns with [dplyr::mutate()] first.
#'
#' [summarize_with_margins()] and [summarise_with_margins()] are synonyms,
#' following [dplyr::summarize()] and [dplyr::summarise()].
#'
#' @section Differences from summarize():
#' - The result is always ungrouped, whatever groups the input had; see
#'   *Grouped and row-wise inputs*.
#' - Row order is unspecified. Use `.sort` for margins next to the rows they
#'   summarize, or [dplyr::arrange()] for any other order.
#' - A character `.margin_label` changes a non-factor grouping dimension to
#'   character; see Value. Use `.margin_label = NULL` or `NA_character_` to
#'   keep its type.
#' - A summary cannot overwrite a fixed key or grouping dimension. Give it a
#'   new name.
#' - [dplyr::cur_group()], [dplyr::cur_group_id()], and
#'   [dplyr::cur_group_rows()] are refused, because a result combining several
#'   grouping sets has no single group for them to describe. Use
#'   [grouping_bit()] or [grouping_id()] to identify margin rows.
#'
#' @section Fixed columns and grouping dimensions:
#' `.by` marks columns that are present in every grouping set, while
#' `.grouping` describes dimensions that can be omitted to form margins.
#' Columns in `.by` retain their input types and never receive
#' `.margin_label`.
#'
#' `.by = year` with `.grouping = rollup(region, store)` produces the same
#' grouping sets as
#' `.grouping = grouping_spec(grouping_set(year), rollup(region, store))`, but
#' the two are not interchangeable: a column supplied through `.grouping` is a
#' Margin dimension even when every grouping set contains it, so it takes part
#' in `.margin_label` type conversion and collision checks. Use `.by` for
#' columns that must always remain fixed.
#'
#' Both accept selection predicates through [tidyselect::where()]. On a lazy
#' input, a predicate receives columns derived from the schema or a zero-row
#' proxy rather than the source values, so use one that depends only on the
#' column type, such as `where(is.character)`. A value-dependent predicate
#' such as `where(~ any(.x == "x"))` can silently select different columns on
#' a lazy input than on a local one; marginplyr does not detect it. To select
#' on values, call [dplyr::collect()] first, and set `.check_margin_label`
#' explicitly, since its default changes to `TRUE` for the collected data.
#' Where a lazy backend cannot supply typed metadata without reading data, a
#' predicate is refused; select the columns by name, or collect first.
#'
#' @section Grouped and row-wise inputs:
#' When `.data` has been grouped with [dplyr::group_by()] and `.by` is `NULL`,
#' its grouping columns become implicit fixed keys: `group_by(year)` followed
#' by `.grouping = rollup(region)` computes what `.by = year` does on the
#' ungrouped data.
#'
#' As with [dplyr::summarize()] and [tidyr::nest()], a grouped input cannot
#' also supply `.by`; call [dplyr::ungroup()] first when replacing the existing
#' groups. A grouping column also cannot appear in `.grouping`, because one
#' column cannot be both a fixed key and a dimension that can be rolled up.
#' Grouped local data created with `.drop = FALSE` is rejected because empty
#' groups cannot be preserved on every backend. To retain empty keys, complete
#' the input before the Margin operation, as the [Complete absent keys before
#' margins][keys] guide shows.
#'
#' [summarize_with_margins()], [expand_with_margins()], and
#' [nest_with_margins()] always return ungrouped results, because a result
#' combining several grouping sets has no single grouping to retain.
#' [nest_by_with_margins()] instead returns a row-wise data frame. Row-wise
#' input is rejected; to keep row-wise identifiers as fixed keys, ungroup
#' first and select them with `.by`.
#'
#' [keys]: https://sayuks.github.io/marginplyr/vignettes/completing_keys.html
#'
#' @section Option arguments:
#' An argument documented as taking one of a listed set of strings —
#' `.duplicates`, `.margin_label_position`, and `.sort` on the Margin verbs,
#' and `.format` on [inspect_grouping()] — takes one of those strings, spelled
#' in full. An abbreviation is not shorthand for the value it begins, and
#' `NULL` is not shorthand for the default: both are errors naming the values
#' the argument accepts.
#'
#' To ask for the default, leave the argument out, or pass the whole
#' vocabulary exactly as the signature spells it. The second form lets a
#' function of your own repeat the signature and hand the argument on:
#'
#' ```r
#' my_report <- function(data, .sort = c("none", "last", "first")) {
#'   summarize_with_margins(
#'     data, revenue = sum(revenue),
#'     .grouping = rollup(region), .sort = .sort
#'   )
#' }
#' ```
#'
#' Any other vector of more than one value is an error, a reordering of the
#' vocabulary included. A wrapper that would rather not track the signature
#' resolves the value itself and passes one string.
#'
#' @section Result metadata:
#' The result's class, its dataset-wide attributes, and the attributes of
#' each column follow the native dplyr or tidyr operation the verb
#' corresponds to on the same backend, applied per grouping set and combined
#' in Grouping-plan order. marginplyr adds the following:
#'
#' - A source column's variable label (`attr(x, "label")`) is kept wherever
#'   that column appears in the result, including on zero rows. Calculated
#'   summaries and generated columns such as `.id` gain no label.
#' - Factor and ordered-factor dimensions keep their levels and ordered
#'   status; see *Display labels and grouping identity*. Classed columns such
#'   as [Date] and [POSIXct], including its `tzone`, are carried unchanged.
#' - A dimension's margin rows hold the label, or a typed missing value,
#'   without element names; its other rows keep the names the native
#'   operation keeps.
#'
#' These guarantees cover the returned result and direct [dplyr::collect()].
#' Later verbs and [dplyr::compute()] follow native backend rules. A lazy
#' result uses the input schema captured when it was built, so rebuild it
#' after changing column types, factor definitions, or labels.
#'
#' @section Grouping set identifiers:
#' When `.id` names an output column, each result row receives the one-based
#' position of its grouping set occurrence after applying `.duplicates`.
#' `"drop"` renumbers the occurrences it keeps. One grouping set has
#' identifier `1L`, and a zero-row result retains an integer `.id` column.
#'
#' `.id` records plan occurrence, not physical result order, and is not a
#' durable business key: reordering or deduplicating the Grouping
#' specification changes it. [grouping_bit()] documents how `.id` compares
#' with [inspect_grouping()]`$set_id`, [grouping_bit()], and [grouping_id()].
#'
#' @section Margin order:
#' `.sort` orders a result by the structure of its Grouping plan rather than by
#' displayed values. The key is the result's own leading grouping columns, left
#' to right, with each grouping dimension preceded by its Grouping bit and by
#' whether its value is missing:
#'
#' ```
#' is.na(by1), by1, ..., bit(d1), is.na(d1), d1, bit(d2), is.na(d2), d2, ...
#' ```
#'
#' A margin row therefore sits with the rows it summarizes instead of wherever
#' its `.margin_label` falls among that dimension's values, so the order does
#' not depend on the label or the locale. Fixed `.by` keys sort first, so each
#' partition is one contiguous block, and `.id`, when it names a column, breaks
#' the remaining ties. `"first"` reverses the Grouping bits alone: missing
#' values come last wherever they appear, on every backend. Factor dimensions
#' sort by their levels. `.margin_label_position` positions a synthetic factor
#' level and never a row.
#'
#' A Margin order promises what [dplyr::arrange()] promises in the C locale,
#' which it fixes: `options(dplyr.legacy_locale = TRUE)` does not redirect it.
#' It is a property of the object the verb returns: the row order of a local
#' data frame, or the ordering of a lazy result that [dplyr::collect()]
#' observes. Later verbs can change it. It composes with every `.duplicates`
#' policy the verb accepts.
#'
#' The [recipes guide][recipes] shows a Margin order before and after, and
#' shows the key written as an `arrange()` you control.
#'
#' [recipes]: https://sayuks.github.io/marginplyr/vignettes/recipes.html
#'
#' @section Display labels and grouping identity:
#' `.margin_label` is a display value, not the identity of a grouping set. An
#' unnamed scalar labels every resolved Margin dimension. A named vector
#' provides column-specific labels and must cover the resolved dimensions
#' exactly once; missing, unknown, duplicate, and empty names are rejected, as
#' are names from `.by`.
#'
#' Non-missing labels convert ordinary grouping dimensions to character. A
#' factor or ordered factor is rebuilt instead, preserving ordered status and
#' placing a new synthetic level last by default or first when
#' `.margin_label_position = "first"`. A label equal to any declared level,
#' used or unused, is rejected before any grouping set is built, whatever
#' `.check_margin_label` says. Rebuilding preserves the distinction between an
#' observation that uses a factor NA level and an actually missing factor
#' code.
#'
#' `NA_character_` and `NULL` both create a typed missing Margin value and do
#' not create a synthetic factor level, so `.margin_label_position` has no
#' effect for either, and neither takes part in the observed half of collision
#' validation. What separates them is a factor NA level: it conflicts with
#' `NA_character_` even when `.check_margin_label = FALSE`, while `NULL`
#' preserves it.
#'
#' A local packed data-frame, matrix, or array grouping dimension accepts only
#' `NULL` or `NA_character_` as its Margin label, because no scalar string can
#' represent such a key; split it into separate columns if it needs display
#' text.
#'
#' Factor columns follow this contract:
#'
#' | Margin label | NA level | Missing value | Result |
#' |---|---:|---:|---|
#' | `NA_character_` | yes | yes | Error: NA is already a factor level |
#' | `NA_character_` | yes | no | Error: NA is already a factor level |
#' | `NA_character_` | no | yes | Allowed; source missing values and margins require structural identity | # nolint: line_length_linter
#' | `NA_character_` | no | no | Allowed; use typed missing |
#' | `NULL` | yes | yes | Allowed; source missing values and margins require structural identity | # nolint: line_length_linter
#' | `NULL` | yes | no | Allowed; preserve the NA level and use typed missing |
#' | `NULL` | no | yes | Allowed; source missing values and margins require structural identity | # nolint: line_length_linter
#' | `NULL` | no | no | Allowed; use typed missing |
#'
#' A factor observation that uses an NA level can print as `<NA>` while
#' `is.na()` is false. A missing factor code has `is.na()` equal to true.
#' Source missing values and typed-missing Margin values may display
#' identically, so keep `.id` in the result when the difference matters.
#' Every row above is decided whatever `.check_margin_label` says.
#'
#' @section Relationship to dplyr summaries:
#' The `...` expressions use [dplyr::summarize()] data-masking semantics.
#' Source missing values and margin rows that display alike are told apart
#' by [grouping_bit()] or [grouping_id()] written as summaries.
#'
#' A few spellings mean something only because marginplyr rewrites them before
#' anything runs: [grouping_bit()], [grouping_id()], [share_of_parent()],
#' [share_of_total()], [dplyr::across()], [dplyr::if_any()], [dplyr::if_all()],
#' [dplyr::pick()], [dplyr::c_across()], and [tidyselect::where()]. They are
#' recognized by name, written bare or qualified with the package that owns
#' them, and never looked up where the call was written, so binding a function
#' of your own to one of those names does not change what this verb does.
#' `mypkg::across()` is an ordinary call. Every other name follows ordinary
#' lookup, [dplyr::n()] included.
#'
#' [dplyr::across()], [dplyr::pick()], and [dplyr::c_across()] cannot select
#' any column named in the complete Grouping plan. This extends dplyr's
#' grouping-column rule across every branch: a dimension remains excluded even
#' in a grouping set from which it is omitted.
#'
#' On a local data frame, a selection in a later summary sees earlier
#' summaries, as in dplyr: after `total = sum(value)`,
#' `across(total, ~ .x * 2)` selects `total`. A lazy input retains the alias
#' limitations of its backend.
#'
#' An unnamed summary whose value is a data frame is unpacked into the result
#' on a local input, and a named one is packed into one column, as
#' [dplyr::summarize()] does. A lazy input gives whatever shape its backend
#' gives.
#'
#' Summary results may not overwrite a fixed key or grouping dimension,
#' including through an unnamed data-frame-valued summary. Local dplyr allows
#' it but other backends may not, so marginplyr refuses it everywhere. Use a
#' new summary name, or rename the grouping column before this call.
#'
#' [dplyr::cur_group()], [dplyr::cur_group_id()],
#' [dplyr::cur_group_rows()], and the deprecated `cur_data*()` helpers are
#' rejected: each describes one branch's group or data mask, which has no
#' meaning across the grouping sets the result combines. They are rejected by
#' name, even when you have bound one to a function of your own.
#'
#' An expression captured as language -- the argument of [base::quote()],
#' [base::substitute()], [base::expression()], `base::bquote()`,
#' `rlang::expr()`, or `rlang::quo()` -- is a value the summary carries, and
#' marginplyr does not analyze or rewrite what is inside it. Evaluating it
#' with [base::eval()] or [rlang::eval_tidy()] runs it in the data mask, where
#' a visibly written capture is analyzed as the code it becomes. Text parsed
#' at run time, as by [base::str2lang()], is searched but not rewritten, so a
#' parsed `grouping_bit()` reaches the helper's own context guard.
#'
#' A misspelled option such as `.margin_lable` reaches this verb through
#' `...`, where it would become a summary column. A dot-prefixed summary name
#' that is an option name, or one insertion, deletion, or substitution from
#' one, is therefore an error naming the option it resembles: `.ids = "set"`
#' and `.y = mean(v)` are refused, although [dplyr::summarise()] would return
#' them as columns. A name further away, such as `.margin_lable` or
#' `.sotr = "last"`, is not caught and becomes a summary column; check option
#' names when a result has an unexpected column.
#'
#' @section Summaries a lazy backend cannot carry:
#' Two lazy inputs cannot carry back a summary with no columns in it: asked
#' for no summaries and given no fixed key or grouping dimension, the answer
#' is one row holding nothing, whatever the input held. A `data.table` reads
#' its row count from its first column, so a `dtplyr` input collects to zero
#' rows where a local data frame returns one, and a SQL table of no columns
#' cannot be written at all, so a database input raises the error dbplyr
#' renders one with. Each is what [dplyr::summarize()] itself answers for that
#' lazy input; an Arrow input answers as a local one does. Anything that puts
#' a column in the result -- one summary, one fixed key, one grouping
#' dimension, or `.id` -- ends the difference.
#'
#' One summary an Arrow table or record batch cannot carry -- or a query built
#' on either -- is one Arrow's own engine cannot evaluate. Arrow answers such an
#' expression by reading the whole input -- every column of it, not only the
#' ones the summary names -- and computing it in R. marginplyr refuses it
#' instead, before a row is read, and names the two rewrites that compute it:
#' collect the input first, and select the columns the summary needs before
#' collecting, which is the narrowing Arrow's own route cannot do for you.
#'
#' Which expressions those are is Arrow's to decide and moves with its version,
#' so they are not listed here. Among the shapes refused are a group collapsed
#' into a single value, such as pasting one, a subset written inside an
#' aggregate, and a statistic over two columns at once; ordinary numeric
#' summaries, and arithmetic over them, are evaluated by Arrow and stay lazy.
#' An Arrow dataset, and a query built on one, raise Arrow's own refusal for
#' the same expressions instead, because a dataset never reads itself into R;
#' they are otherwise unaffected.
#'
#' @section Contextual shares:
#' [share_of_parent()] and [share_of_total()] calculate a preceding named
#' numeric scalar summary's ratio to the same summary on another row of the
#' result, for local data and supported lazy dbplyr and dtplyr inputs. Both
#' support direct named expressions and a constrained [dplyr::across()] form
#' for multiple preceding summaries, and both partition the calculation by the
#' fixed `.by` keys.
#'
#' They differ only in the denominator. [share_of_parent()] divides by the
#' immediate less detailed [rollup()] level, so it requires one pure
#' [rollup()]; composite dimensions move together, and duplicate occurrences
#' skip identical sets when choosing the parent. [share_of_total()] divides by
#' the Grand total set, so it accepts any Grouping specification whose plan
#' contains one, including [cube()].
#'
#' Arrow inputs reject both before a summary query is built; collect the
#' input first when local share execution is appropriate, and set
#' `.check_margin_label` explicitly on the re-run.
#'
#' A row that is its own denominator receives `1.0`. Missing numerators or
#' denominators and zero denominators receive `NA_real_`; other finite ratios
#' are not clamped. Matching is structural, so `.id`, missing grouping values,
#' and displayed Margin labels do not determine the denominator.
#'
#' The source must be a unique, preceding, self-contained integer or double
#' scalar summary. Local data frames reject an ineligible source before any
#' share is calculated, and `dtplyr` steps when executed. A dbplyr backend is
#' asked, without reading your data, whether its SQL dialect converts a value
#' of another type to a number rather than refusing it; where it converts, or
#' cannot be asked, the share is refused unless `.check_share_source = FALSE`.
#' Asking takes at most two queries that reference none of your tables, sent
#' once per dialect that answers and again on the next share request where it
#' could not answer. A connection built with a `dbplyr::simulate_*()`
#' constructor executes nothing and so cannot be asked. Within a transaction
#' begun through DBI with RPostgres, up to four savepoint statements isolate
#' these questions, leaving commit or rollback to you.
#'
#' [share_of_parent()] is the canonical reference for the complete
#' direct-expression, source, ordering, value, empty-input, and `across()`
#' contracts of both helpers.
#'
#' @section Arrow inputs:
#' Arrow's tabular dplyr inputs are a `Table`, `RecordBatch`, `Dataset`,
#' `RecordBatchReader`, or an `arrow_dplyr_query` built from them. marginplyr's
#' contract depends on both the verb and the query's sources:
#'
#' | Input | Summary or expansion | Inspection | Nesting |
#' | --- | --- | --- | --- |
#' | `Table`, `RecordBatch`, or `Dataset` | Accepted | Accepted | Refused; collect first | # nolint: line_length_linter
#' | Query with no reader source | Accepted | Accepted | Refused; collect first | # nolint: line_length_linter
#' | `RecordBatchReader` | Refused; materialize first | Accepted | Refused; collect first | # nolint: line_length_linter
#' | Query with any reader source | Refused; materialize first | Accepted | Refused; collect first | # nolint: line_length_linter
#' | Non-tabular Arrow object | General input refusal | General input refusal | General input refusal | # nolint: line_length_linter
#'
#' A translatable summary or expansion stays an Arrow query. For a summary or
#' generated Margin label transformation Arrow cannot translate, a `Table` and
#' `RecordBatch` (and queries built on them) would fall back to R, so marginplyr
#' refuses it before a row is read. Collect the input first to perform that
#' operation locally, selecting the columns you need before collecting. A
#' `Dataset` and a query built on one instead keep Arrow's own
#' unsupported-expression error.
#'
#' A `RecordBatchReader` is one-pass, while a Margin operation may build one
#' branch per grouping set, so a reader and any query whose source graph
#' contains one are refused. [arrow::as_arrow_table()] consumes every batch
#' once and materializes the reusable `Table` those branches require.
#' [inspect_grouping()] executes no branch, so it accepts both without
#' consuming either. Other Arrow objects, such as a `Scanner` or an `Array`,
#' are not dplyr inputs; a Scanner materializes with `$ToTable()`.
#'
#' Arrow does not hold factor levels in the schema marginplyr reads, so a
#' non-missing `.margin_label` converts a factor dimension to character, its
#' levels and ordered status are lost, and a Margin order sorts it by value.
#' For the same reason, a label equal to an unused declared level is allowed,
#' and an observed collision is refused only when
#' `.check_margin_label = TRUE`. `NA_character_` and `NULL` convert nothing.
#'
#' @section Database backend coverage:
#' Summaries on DuckDB and PostgreSQL use one native `GROUP BY GROUPING SETS`
#' query. Other database backends, and every expansion, use a portable
#' `UNION ALL` query with one branch per grouping set and the same result.
#' Asking for a Margin order never changes which of the two runs.
#'
#' Results are executed and compared with local ones on DuckDB, SQLite, and
#' Microsoft SQL Server. SQL generation, not execution, is verified for
#' PostgreSQL, Access, SAP HANA, Hive, Impala, MariaDB, MySQL, Oracle, Amazon
#' Redshift, Snowflake, Spark SQL, and Teradata, and for generic DBI and ODBC
#' connections. Arrow and `dtplyr` are also supported lazy backends.
#'
#' On SQLite and DuckDB, identifiers differing only in ASCII letter case name
#' the same column, so a result refuses two public columns that would collide
#' under that rule and names both spellings.
#'
#' DuckDB stores a factor as an `ENUM`, which marginplyr rebuilds with its
#' levels; an `ENUM` carries no ordered flag, so an ordered factor returns
#' unordered. Other SQL backends store a factor as text before marginplyr
#' reads it.
#'
#' A lazy result's Margin order is the outermost query's `ORDER BY`, which
#' [dplyr::collect()] and [dplyr::compute()] observe. Whether it survives
#' later verbs depends on dbplyr's query flattening and is not promised. It is
#' not a window ordering, so a window function over a sorted result still
#' needs [dbplyr::window_order()]. For direct [dplyr::compute()] on SQLite,
#' see *[SQLite materialization][summarize_with_margins]*.
#'
#' @section SQLite materialization:
#' On a live SQLite connection, direct [dplyr::compute()] of a result
#' returned by [summarize_with_margins()] or [expand_with_margins()] writes a
#' typed table holding only the public columns, and later dplyr verbs on it
#' use ordinary dbplyr behavior. A sorted result is read back in Margin order
#' through an implicit rowid alias, so sorted compute refuses before writing
#' when the public columns shadow all three of `rowid`, `oid`, and `_rowid_`;
#' direct collection still works. An unqualified destination name that could
#' address a different schema, such as a bare persistent name shadowed by a
#' temporary table, is refused before writing; give an explicit schema or
#' another name.
#'
#' Compute runs inside its own savepoint, so an existing caller transaction
#' keeps its commit and rollback, whatever `in_transaction` says. If an
#' ordinary error or a catchable R interrupt arrives before the savepoint is
#' released, marginplyr restores an overwritten destination's data, schema,
#' indexes, and affected statistics, or removes a new destination, then
#' releases the savepoint; source tables and earlier caller work are kept.
#' This requires a surviving connection and successful cleanup;
#' [marginplyr_conditions] describes what an interrupt or a failed cleanup
#' reports.
#' Once the savepoint is released, the outermost release persists the result
#' and an inner one leaves it to the caller's commit or rollback.
#'
#' There is no recovery guarantee for native SQLite cancellation, which can
#' abort the whole transaction, nor for invalid connections, process
#' termination, fatal signals, or a timeout that does not unwind through R.
#'
#' @section When marginplyr queries your data:
#' A Margin verb applied to a lazy input builds a query and returns it
#' unexecuted. [dplyr::show_query()] runs nothing, and no row is read until
#' you execute the query yourself.
#'
#' Building it can still send internal queries that read no source values:
#'
#' - **A zero-row read of the input**, sent only to a backend whose factor
#'   columns marginplyr must decompose and later restore -- currently
#'   `dtplyr` and DuckDB -- to recover their levels and column prototypes. It
#'   references your table but reads none of it, as the zero-row read
#'   [dplyr::tbl()] already sends does. A derived `dtplyr` step whose types
#'   cannot be found that way is refused; call [dplyr::collect()] on it
#'   explicitly and pass the data frame.
#' - **One DuckDB catalog query per operation at most**, reading table and
#'   view names, stored view definitions, macro names, and the search path, to
#'   check that the input hides no source work behind a view or macro before
#'   its types are read. It binds no table of yours. Opaque macros, `query()`,
#'   `query_table()`, and views without stored SQL are refused, except a data
#'   frame registered with `duckdb::duckdb_register()`.
#'
#' A query is not safe merely because it returns no rows: some services bill
#' by the bytes a query scans, and `LIMIT 0` is not exempt. marginplyr cannot
#' tell a billed connection from a free one, so `.check_margin_label` defaults
#' to `is.data.frame(.data)` and a lazy input is checked only when asked. The
#' observed check scans every input row of the checked columns and retrieves
#' one row of collision counts.
#'
#' With recording enabled before the call, [last_sent_queries()] lists the
#' SQL prepared for these internal queries in the order sent.
#'
#' @family summarize and expand data with margins
#' @export
#' @examples
#' # Build one monthly management report with store detail, region subtotals,
#' # and a company total. `set` numbers the grouping sets: 1 is store detail,
#' # 2 is the region subtotal, and 3 is the company total for each month.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   units = sum(units),
#'   revenue = sum(revenue),
#'   .by = c(year, month),
#'   .grouping = rollup(region, store),
#'   .id = "set"
#' )
#'
#' # `.sort` puts each subtotal after the stores it summarizes, and each
#' # year's company total last within that year.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .by = year,
#'   .grouping = rollup(region, store),
#'   .sort = "last"
#' )
#'
#' # Existing dplyr groups are implicit fixed keys. The calculation below is
#' # equivalent to `.by = c(year, month)`, but its result is still ungrouped.
#' grouped_report <- retail_sales |>
#'   dplyr::group_by(year, month) |>
#'   summarize_with_margins(
#'     revenue = sum(revenue),
#'     .grouping = rollup(region, store)
#'   )
#' dplyr::group_vars(grouped_report)
#'
#' # Moving year and month into the rollup extends the hierarchy from store
#' # detail through monthly, annual, and all-period totals.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   level = grouping_id(year, month, region, store),
#'   .grouping = rollup(year, month, region, store)
#' )
#'
#' # Grouping expressions are created with mutate() before summarizing.
#' summarize_with_margins(
#'   .data = dplyr::mutate(
#'     retail_sales,
#'     period = paste(year, month, sep = "-")
#'   ),
#'   revenue = sum(revenue),
#'   .grouping = rollup(period, region)
#' )
#'
#' # Change the display label, or use NULL to retain the input column types.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = rollup(year),
#'   .margin_label = "All years"
#' )
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = rollup(region, store),
#'   .margin_label = c(region = "All regions", store = "All stores")
#' )
#' # With `NULL`, `year` stays an integer and the margin row holds `NA`; a
#' # tibble's type header shows it.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   year_is_total = grouping_bit(year),
#'   .grouping = rollup(year),
#'   .margin_label = NULL
#' ) |>
#'   dplyr::as_tibble()
#'
#' # Ordered factors remain ordered, and a new label is placed by
#' # `.margin_label_position`. An existing level -- used or unused -- is
#' # rejected as a label.
#' priority_data <- data.frame(
#'   priority = ordered(
#'     c("standard", "premium"),
#'     levels = c("standard", "premium", "unused")
#'   ),
#'   value = c(1, 2)
#' )
#' try(summarize_with_margins(
#'   .data = priority_data,
#'   total = sum(value),
#'   .grouping = rollup(priority),
#'   .margin_label = "unused"
#' ))
#' priority_result <- summarize_with_margins(
#'   .data = priority_data,
#'   total = sum(value),
#'   .grouping = rollup(priority),
#'   .margin_label = "All priorities",
#'   .margin_label_position = "first"
#' )
#' is.ordered(priority_result$priority)
#' levels(priority_result$priority)
#'
#' # A typed-missing Margin label prints like a real missing value, and
#' # `grouping_bit()` tells them apart. A named list can label one dimension
#' # and leave another missing.
#' status_data <- data.frame(
#'   region = c("east", "east", "west"),
#'   status = factor(c("active", NA, "active"), levels = c("active", "idle")),
#'   value = c(1, 2, 3)
#' )
#' summarize_with_margins(
#'   .data = status_data,
#'   total = sum(value),
#'   status_is_total = grouping_bit(status),
#'   .grouping = rollup(region, status),
#'   .margin_label = list(region = "All regions", status = NULL)
#' )
#'
#' # A direct Parent share, multiple measures through two ordered across()
#' # expressions, and a post-summary calculation.
#' direct_parent <- summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   revenue_share = share_of_parent(revenue),
#'   .grouping = rollup(region, store)
#' )
#' multiple_parents <- summarize_with_margins(
#'   .data = retail_sales,
#'   dplyr::across(c(units, revenue), sum),
#'   dplyr::across(
#'     c(units, revenue),
#'     share_of_parent,
#'     .names = "{.col}_share"
#'   ),
#'   .grouping = rollup(region, store)
#' )
#' dplyr::mutate(
#'   .data = direct_parent,
#'   revenue_percent = 100 * revenue_share
#' )
#'
#' # Empty input without `.by` still has one Grand total row, whose share is
#' # one; with `.by` it has no rows. Both keep a double share column.
#' empty_sales <- retail_sales[0, ]
#' empty_grand_total <- summarize_with_margins(
#'   .data = empty_sales,
#'   revenue = sum(revenue),
#'   revenue_share = share_of_parent(revenue),
#'   .grouping = rollup(region)
#' )
#' c(
#'   rows = nrow(empty_grand_total),
#'   type = typeof(empty_grand_total$revenue_share)
#' )
#' empty_partitions <- summarize_with_margins(
#'   .data = empty_sales,
#'   revenue = sum(revenue),
#'   revenue_share = share_of_parent(revenue),
#'   .by = year,
#'   .grouping = rollup(region)
#' )
#' c(
#'   rows = nrow(empty_partitions),
#'   type = typeof(empty_partitions$revenue_share)
#' )
#'
#' # across() and pick() treat every fixed key and Margin dimension as a
#' # grouping column, including dimensions omitted from a subtotal branch.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   dplyr::across(
#'     c(units, revenue),
#'     sum,
#'     .names = "total_{.col}"
#'   ),
#'   measures = paste(
#'     names(dplyr::pick(c(units, revenue))),
#'     collapse = ", "
#'   ),
#'   .by = year,
#'   .grouping = rollup(region),
#'   .sort = "last"
#' )
#'
#' # DuckDB executes a native GROUP BY GROUPING SETS query.
#' # Runs only when a recent enough DuckDB is installed.
#' source(system.file("suggests", "guard.R", package = "marginplyr"))
#' if (
#'   marginplyr_suggest_available("DBI") &&
#'   marginplyr_suggest_available("duckdb")
#' ) {
#'   # `shared_home = FALSE` keeps DuckDB's extension cache and stored secrets
#'   # inside the session's temporary directory instead of `~/.duckdb`.
#'   con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
#'
#'   sales_db <- dplyr::copy_to(
#'     con,
#'     retail_sales,
#'     name = "retail_sales",
#'     temporary = TRUE,
#'     overwrite = TRUE
#'   )
#'   query <- summarize_with_margins(
#'     .data = sales_db,
#'     revenue = sum(revenue, na.rm = TRUE),
#'     level = grouping_id(region, store),
#'     .by = c(year, month),
#'     .grouping = rollup(region, store),
#'     .sort = "last"
#'   )
#'
#'   dplyr::show_query(query)
#'   result <- dplyr::collect(query)
#'   DBI::dbDisconnect(con)
#'   result
#' }
summarize_with_margins <- function(.data,
                                   ...,
                                   .by = NULL,
                                   .grouping = NULL,
                                   .margin_label = "Total",
                                   .margin_label_position = c("last", "first"),
                                   .check_margin_label = is.data.frame(.data),
                                   .check_share_source = TRUE,
                                   .duplicates = c("error", "drop", "keep"),
                                   .id = NULL,
                                   .sort = c("none", "last", "first")) {
  force(.data)
  suspendInterrupts({
    # The scope is read from this live frame by active_sent_query_scope().
    sent_query_scope <- reset_sent_queries() # nolint: object_usage_linter.
  })
  call <- rlang::current_call()
  dots <- rlang::enquos(...)
  grouping_quo <- capture_public_grouping(
    rlang::enquo0(.grouping), substitute(.grouping)
  )
  by_quo <- rlang::enquo(.by)

  share_kinds <- with_margin_error_call(
    {
      assert_margin_input(.data)
      assert_reusable_margin_input(.data)
      normalize_margin_options(
        .margin_label = .margin_label,
        .margin_label_position = .margin_label_position,
        .check_margin_label = .check_margin_label,
        .duplicates = .duplicates,
        .sort = .sort,
        duplicates_choices = margin_duplicates_choices,
        .id = .id
      )
      assert_logical_scalar(.check_share_source)
      check_option_named_summaries(dots)
      # Ahead of the two readings of a summary expression below, and after the
      # one check that reads only names: an empty argument is refused rather
      # than bound to a local.
      check_empty_summaries(dots)
      check_summary_context_helpers(dots)
      preflight_shares(dots)
    },
    call = call
  )

  operation <- prepare_margin_operation(
    .data,
    by_quo = by_quo,
    grouping_quo = grouping_quo,
    .margin_label = .margin_label,
    .margin_label_position = .margin_label_position,
    .check_margin_label = .check_margin_label,
    .duplicates = .duplicates,
    .sort = .sort,
    duplicates_choices = margin_duplicates_choices,
    # A summary result holds the grouping columns and the caller's outputs, so
    # the fixed `.by` keys are the whole of what it carries from the input. A
    # summary output is free to reuse another column's name, which is why this
    # is not the input's column names narrowed to what the result happens to
    # hold: that name would be rebuilt from the input column's levels.
    carried_columns = function(data_vars, plan) plan$by,
    # A summary's rows do not depend on an unlimited input ordering.
    keeps_input_order = FALSE,
    .id = .id,
    validate_grouping = share_grouping_spec_validator(share_kinds),
    validate_plan_names = function(fixed, dimensions, data_vars, complete) {
      if (
        complete && length(dots) > 0L &&
          !contains_share_helper(rlang::quo_get_expr(dots[[1L]]))
      ) {
        preflight_static_selection(
          dots[[1L]],
          grouping_name_proxy(setdiff(data_vars, c(fixed$columns, dimensions)))
        )
      }
    },
    call = call
  )
  execution <- execute_margin_summary(
    operation,
    dots,
    check_share_source = .check_share_source
  )
  typed_dimensions <- names(operation$margin_labels)[vapply(
    operation$margin_labels,
    is_missing_margin_label,
    logical(1)
  )]
  # Share staging, SQLite Margin order, and the public `.id` projection can
  # wrap the union's source-column anchor. The finalizer anchors the result.
  has_anchor <- length(share_kinds) > 0L ||
    sqlite_final_anchor_path(operation)
  existing_anchor_needed <- (
    identical(operation$backend$kind, "sql") &&
      has_anchor &&
      length(typed_dimensions) > 0L &&
      length(operation$plan$sets) > 1L
  ) || (
    sqlite_declared_type_result(operation) &&
      (length(share_kinds) > 0L || !is.null(operation$set_id_name))
  )
  # A source key and a summary output both reach generated `.env` references.
  sql_env_column <- operation$backend$is_sql &&
    ".env" %in% get_col_names(execution$result, dplyr::everything())
  sqlite_env_column <- sql_env_column && live_sqlite_margin_result(operation)
  sqlite_boolean_column <- live_sqlite_margin_result(operation) &&
    length(sqlite_boolean_columns(dplyr::tbl_vars(execution$result))) > 0L
  anchor_needed <- existing_anchor_needed || (
    sqlite_declared_type_result(operation) &&
      length(execution$declared_types) > 0L
  ) || sqlite_env_column || sqlite_boolean_column
  # The typed result's companion anchors declare these columns where the query
  # would lose them; the query's own types are already right. `CREATE TABLE AS`
  # over a compound SELECT drops a typed-missing dimension's declaration, and
  # the ordering wrapper and a text label's typed destination hide a fixed
  # key's.
  compound_typed_dimension <- length(operation$plan$sets) > 1L &&
    length(typed_dimensions) > 0L
  hidden_fixed_key <- length(operation$plan$by) > 0L &&
    (margin_sorting(operation) ||
       length(typed_dimensions) < length(operation$margin_labels))
  companion_anchor_needed <- !anchor_needed &&
    sqlite_declared_type_result(operation) &&
    (compound_typed_dimension || hidden_fixed_key)
  final_type_anchor <- if (anchor_needed || companion_anchor_needed) {
    if (live_sqlite_margin_result(operation)) {
      always_present <- Reduce(intersect, operation$plan$sets)
      unique(c(operation$plan$by, typed_dimensions, always_present))
    } else {
      c(operation$plan$by, typed_dimensions)
    }
  } else {
    character()
  }
  finalize_margin_operation(
    operation, execution, type_anchor_columns = final_type_anchor,
    anchor_query = !companion_anchor_needed,
    sql_env_column = sql_env_column
  )
}

execute_margin_summary <- function(operation, dots, check_share_source) {
  check_margin_operation(operation)
  with_margin_error_call(
    {
      plan <- operation$plan
      group_vars <- c(plan$by, plan$dimensions)
      summary_plan <- plan_summary_expressions(
        dots,
        data_proxy = operation$data_proxy,
        data_vars = operation$data_vars,
        plan = plan,
        backend_kind = operation$backend$kind,
        set_id_name = operation$set_id_name,
        call = operation$call,
        local_data = if (identical(operation$backend$kind, "local")) {
          operation$data
        } else {
          NULL
        }
      )
      summaries <- summary_plan$summaries
      dots <- summaries$dots
      selection_proxy <- summary_selection_proxy(
        operation$data_proxy,
        data_vars = operation$data_vars,
        group_vars = group_vars
      )
      public_dot_names <- names(dots)
      if (identical(operation$backend$kind, "local")) {
        # A local frame discards its Assigned summary name (ADR 0028).
        # The branch checks the names its value actually retains.
        public_dot_names[!is.na(summaries$assigned_names)] <- ""
      }
      summary_output_names <- unique(c(
        public_dot_names[nzchar(public_dot_names)],
        summary_plan$predictable_names,
        known_summary_output_names(
          dots, selection_proxy,
          defer_local = identical(operation$backend$kind, "local")
        )
      ))
      check_margin_sql_public_names(
        unique(c(group_vars, summary_output_names,
                 operation$set_id_name)),
        operation$backend
      )
      check_summary_group_overwrite(
        summary_output_names,
        group_vars = group_vars
      )
      check_margin_id_collision(
        operation$set_id_name,
        summary_output_names,
        "a summary output"
      )
      reserved_names <- unique(c(
        names(dots),
        operation$data_vars,
        summary_output_names,
        summary_frame_candidates(dots),
        operation$set_id_name
      ))
      has_shares <- length(summary_plan$requests) > 0L
      share_names <- unlist(
        lapply(summary_plan$requests, `[[`, "outputs"), use.names = FALSE
      )
      local_share <- has_shares && identical(
        operation$backend$kind, "local"
      )
      if (local_share) {
        share_positions <- match(share_names, names(summaries$dots))
        stopifnot(!anyNA(share_positions))
        share_aliases <- stats::setNames(new_margin_internal_names(
          length(share_names), used_names = reserved_names,
          prefix = "..marginplyr_share_"
        ), share_names)
        share_tokens <- stats::setNames(lapply(
          share_names, function(name) new.env(parent = emptyenv())
        ), unname(share_aliases))
        for (i in seq_along(share_positions)) {
          position <- share_positions[[i]]
          alias <- unname(share_aliases[[i]])
          existing <- rlang::expr(dplyr::pick(dplyr::any_of(!!alias)))
          summaries$dots[[position]] <- rlang::new_quosure(
            rlang::call2(
              marginplyr_private_call("local_share_marker"),
              alias, share_tokens[[i]], existing
            ),
            env = environment()
          )
          names(summaries$dots)[position] <- alias
        }
        reserved_names <- c(reserved_names, unname(share_aliases))
        summaries$selection_state$share_aliases <- unname(share_aliases)
        summaries$selection_state$share_markers <- share_tokens
      }
      declare_sqlite_types <- sqlite_declared_type_result(operation)
      summaries$grouping_type_state <- if (declare_sqlite_types) {
        new.env(parent = emptyenv())
      } else {
        NULL
      }
      declared_id <- if (!declare_sqlite_types ||
                           is.null(operation$set_id_name)) {
        character()
      } else {
        stats::setNames("integer", operation$set_id_name)
      }

      # A Grouping helper's refusals are decided by the call and the plan, so
      # ADR 0005 places them ahead of the label query below; staging rewrites
      # the same dots again, per branch.
      check_grouping_helper_calls(dots, plan)
      validate_margin_operation(operation)

      if (
        has_shares &&
          identical(operation$backend$kind, "arrow")
      ) {
        abort_arrow_shares(share_request_kinds(summary_plan$requests))
      }

      staged_result <- stage_margin_summaries(
        operation,
        summaries = summaries,
        reserved_names = reserved_names,
        keep_set_identity = has_shares,
        keep_typed_keys = "parent" %in%
          share_request_kinds(summary_plan$requests) ||
          (has_shares && identical(operation$backend$kind, "dtplyr"))
      )

      if (declare_sqlite_types) {
        outputs <- summaries$grouping_type_state$names
        declared_id <- c(declared_id, stats::setNames(
          rep("integer", length(outputs)), outputs
        ))
      }

      if (local_share) {
        staged_result <- restore_local_share_names(
          staged_result, share_aliases, share_tokens,
          summary_plan$requests
        )
      }

      if (has_shares) {
        return(new_margin_execution(
          execute_shares(
            operation,
            staged_result = staged_result,
            requests = summary_plan$requests,
            check_share_source = check_share_source
          ),
          sort_id = margin_summary_stage_sort_id(staged_result),
          # sf moves active summary geometry behind scalar outputs. The
          # per-dot runtime names retain their order, including unnamed frames.
          output_order = if (local_share &&
                               inherits(staged_result$result, "sf")) {
            intersect(
              unlist(summaries$selection_state$summary_aliases,
                     use.names = FALSE),
              names(staged_result$result)
            )
          } else {
            character()
          },
          declared_types = if (declare_sqlite_types) {
            c(declared_id, stats::setNames(
              rep("double", length(share_names)), share_names
            ))
          } else {
            character()
          }
        ))
      }
      new_margin_execution(
        margin_summary_stage_result(staged_result),
        sort_id = margin_summary_stage_sort_id(staged_result),
        declared_types = declared_id
      )
    },
    call = operation$call
  )
}

# Returns one token in a list column at the share's original dot position.
# The caller gives an alias absent from input and known output names. A frame
# that created the alias earlier is captured by the injected `pick()` before
# this call runs; expanding that selection avoids reading unrelated outputs.
local_share_marker <- function(alias, token, existing) {
  if (alias %in% names(existing)) {
    abort_share_alias_collision(alias)
  }
  structure(list(token), marginplyr_share_token = token)
}

# Rejects an ordinary output using a reserved local share alias. The caller
# supplies the alias that was present in the summary mask or staged result.
abort_share_alias_collision <- function(alias) {
  abort_marginplyr(paste0(
    "Ordinary summary output {.var {alias}} conflicts with an internal ",
    "share column."
  ))
}

# Checks each local branch before binding can discard zero-row list attributes.
# The caller passes a dplyr branch and its per-alias tokens; nonempty columns
# carry token cells, while an empty marker carries its token as an attribute.
check_local_share_markers <- function(result, markers) {
  for (alias in names(markers)) {
    value <- result[[alias]]
    valid <- if (length(value) > 0L) {
      is.list(value) && all(vapply(
        value, identical, logical(1), markers[[alias]]
      ))
    } else {
      identical(attr(value, "marginplyr_share_token"), markers[[alias]])
    }
    if (!valid) {
      abort_share_alias_collision(alias)
    }
  }
  invisible(NULL)
}

# Restores public share names after all local branches have been combined.
# The caller passes aliases and tokens whose markers passed each branch check;
# nonempty result cells are checked again before the placeholders are reset.
restore_local_share_names <- function(staged_result, aliases,
                                      tokens, requests) {
  result <- margin_summary_stage_result(staged_result)
  conflicts <- intersect(names(result), names(aliases))
  if (length(conflicts) > 0L) {
    name <- conflicts[[1L]]
    pair <- Filter(function(pair) identical(pair$output, name),
                   share_pairs(requests))[[1L]]
    # The cli template reads this binding; codetools cannot follow it.
    kind <- pair$kind # nolint: object_usage_linter.
    abort_marginplyr(paste0(
      "{share_kind_modifier(kind)} output name {.var {name}} conflicts ",
      "with an ordinary summary."
    ))
  }
  for (alias in unname(aliases)) {
    value <- result[[alias]]
    if (!is.list(value) || !all(vapply(
      value, identical, logical(1), tokens[[alias]]
    ))) {
      abort_share_alias_collision(alias)
    }
    result[[alias]] <- rep(NA_real_, nrow(result))
  }
  result_names <- names(result)
  result_names[match(unname(aliases), result_names)] <- names(aliases)
  staged_result$result <- rlang::set_names(result, result_names)
  staged_result
}

# `summaries` is what `new_summary_arguments()` built: the caller's summary
# dots beside the caller's label for each, one value because the pair may not
# drift apart between the planning that built it and the adapter that reads it.
stage_margin_summaries <- function(operation,
                                   summaries,
                                   reserved_names,
                                   keep_set_identity,
                                   keep_typed_keys = FALSE) {
  plan <- operation$plan
  set_id_name <- operation$set_id_name
  if (keep_set_identity) {
    set_id_name <- new_margin_internal_names(
      1L,
      used_names = reserved_names,
      prefix = "..marginplyr_set_id_",
      backend = operation$backend
    )
    reserved_names <- c(reserved_names, set_id_name)
  }

  # Which adapter runs stays a function of the duplicate policy, of an
  # identifier that has to number occurrences, and of a Grouping helper that
  # has to be a literal (ADR 0009). A Margin order needs Grouping bits rather
  # than occurrences, so the identifier it may add below is allocated after
  # this decision and never changes it.
  use_native <- supports_grouping_sets(
    operation$data,
    plan,
    backend = operation$backend
  ) && !(
    !is.null(set_id_name) &&
      identical(plan$duplicates, "keep")
  ) && !grouping_helper_needs_literal(
    summaries$dots,
    dbplyr::remote_con(operation$data)
  )

  sort_id <- margin_sort_identifier(
    operation,
    set_id_name = set_id_name,
    used_names = reserved_names
  )
  if (!is.null(sort_id)) {
    set_id_name <- sort_id
    reserved_names <- unique(c(reserved_names, set_id_name))
  }

  # Parent matching and dtplyr cardinality validation read these before
  # display conversion can merge distinct typed values. The share executor
  # drops them before finalization.
  parent_key_names <- character()
  if (keep_typed_keys) {
    parent_key_names <- new_margin_internal_names(
      length(plan$dimensions),
      used_names = reserved_names,
      prefix = "..marginplyr_parent_original_",
      backend = operation$backend
    )
    names(parent_key_names) <- plan$dimensions
    reserved_names <- c(reserved_names, unname(parent_key_names))
  }

  # Both branches above may replace the caller's `.id` with a name allocated
  # here -- for keeping set identity under a share, or for a Margin order. The
  # adapters check their result names against whichever they were handed, and
  # only this frame can still tell the two apart. Getting it wrong is not
  # cosmetic: `check_margin_id_collision()` names `.id` in its message, which a
  # caller who wrote no `.id` cannot act on.
  set_id_is_internal <- !identical(set_id_name, operation$set_id_name)

  result <- tryCatch(
    {
      if (use_native) {
        # What this adapter restates is the one error dbplyr raises
        # translating the caller's expressions, and that error alone
        # (ADR 0022).
        summarize_margin_native(
          operation$data,
          summaries = summaries,
          plan = plan,
          margin_labels = operation$margin_labels,
          reserved_names = reserved_names,
          call = operation$call,
          set_id_name = set_id_name,
          set_id_is_internal = set_id_is_internal,
          parent_key_names = parent_key_names
        )
      } else {
        input_window_order <- if (operation$backend$records_window_order) {
          dbplyr::op_sort(operation$data)
        } else {
          list()
        }
        summarize_margin_union(
          operation$data,
          summaries = summaries,
          plan = plan,
          margin_labels = operation$margin_labels,
          column_info = operation$column_info,
          backend = operation$backend,
          reserved_names = reserved_names,
          set_id_name = set_id_name,
          set_id_is_internal = set_id_is_internal,
          call = operation$call,
          input_window_order = input_window_order,
          parent_key_names = parent_key_names
        )
      }
    },
    error = function(cnd) {
      parent <- cnd$parent
      # dplyr wraps a Package condition raised while it evaluates an unnamed
      # frame; return that condition with its class and diagnostic intact.
      if (inherits(parent, "marginplyr_error")) {
        stop(parent)
      }
      stop(cnd)
    }
  )
  # The adapters check against the identifier they were handed, which is the
  # allocated one wherever the two branches above replaced the caller's, so
  # this is the frame that can still ask the caller's own (ADR 0028).
  if (set_id_is_internal) {
    check_margin_id_collision(
      operation$set_id_name,
      margin_col_names(result),
      "a summary output"
    )
  }
  new_margin_summary_stage(
    result, set_id_name, sort_id = sort_id,
    parent_key_names = parent_key_names
  )
}

new_margin_summary_stage <- function(result, set_id_name, sort_id = NULL,
                                     parent_key_names = character()) {
  structure(
    list(
      result = result,
      set_id_name = set_id_name,
      sort_id = sort_id,
      parent_key_names = parent_key_names
    ),
    class = "marginplyr_summary_stage"
  )
}

check_margin_summary_stage <- function(staged_result) {
  stopifnot(inherits(staged_result, "marginplyr_summary_stage"))
  invisible(staged_result)
}

margin_summary_stage_result <- function(staged_result) {
  check_margin_summary_stage(staged_result)
  staged_result$result
}

margin_summary_stage_set_id <- function(staged_result) {
  check_margin_summary_stage(staged_result)
  staged_result$set_id_name
}

margin_summary_stage_sort_id <- function(staged_result) {
  check_margin_summary_stage(staged_result)
  staged_result$sort_id
}

#' @rdname summarize_with_margins
#' @export
summarise_with_margins <- summarize_with_margins
