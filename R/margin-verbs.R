#' Behavior shared by the Margin verbs
#'
#' [summarize_with_margins()], [expand_with_margins()], [nest_with_margins()],
#' and [nest_by_with_margins()] resolve their inputs into a Grouping plan --
#' the ordered list of grouping sets the Grouping specification in `.grouping`
#' expands to, which [inspect_grouping()] returns -- and label, identify, and
#' order their results by the rules on this page. Each verb's own page states
#' what is particular to it and links here for the rest.
#'
#' @section Fixed columns and Margin dimensions:
#' `.by` marks fixed keys: columns present in every grouping set, which retain
#' their input types and never receive `.margin_label`. Every column
#' `.grouping` selects is a Margin dimension: one a grouping set can omit to
#' form a margin -- a subtotal or total row -- where it then holds the Margin
#' label. A grouped input's grouping columns are fixed keys, as `.by` columns
#' are.
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
#' on values, call [dplyr::collect()] first.
#' DuckDB, Arrow, and `dtplyr` inputs supply those types; SQLite, PostgreSQL,
#' and every other SQL connection cannot without a query, so a predicate is
#' refused there. Select the columns by name, or collect first.
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
#' the input before the Margin operation, as *Keep unused factor levels* in
#' [Complete absent keys before margins][keys] shows.
#'
#' Row-wise input is rejected; to keep row-wise identifiers as fixed keys,
#' ungroup first and select them with `.by`. Each verb's page states the
#' grouping of its result.
#'
#' [keys]: https://sayuks.github.io/marginplyr/vignettes/completing_keys.html#keep-unused-factor-levels
#'
#' @section Display labels and grouping identity:
#' `.margin_label` is a display value, not the identity of a grouping set;
#' each verb's Value states the type a dimension takes to hold a label. A
#' factor or ordered factor is rebuilt to hold a non-missing label, preserving
#' ordered status and placing the new level last by default or first when
#' `.margin_label_position = "first"`. A label equal to any declared level,
#' used or unused, is rejected before any grouping set is built, whatever
#' `.check_margin_label` says. Rebuilding preserves the distinction between an
#' observation that uses a factor NA level and an actually missing factor
#' code.
#'
#' `NA_character_` and `NULL` both create a typed missing Margin value and no
#' factor level. What separates them is a factor NA level: `NA_character_` on
#' a factor that has one is refused, because NA is already a factor level,
#' even when `.check_margin_label = FALSE`, while `NULL` preserves the level.
#' Every other combination of label and factor is allowed.
#'
#' A local packed data-frame, matrix, or array Margin dimension accepts only
#' `NULL` or `NA_character_` as its Margin label, because no scalar string can
#' represent such a key; split it into separate columns if it needs display
#' text.
#' The same holds for a list-column dimension, locally or under dtplyr, and
#' for an Arrow list, large list, or fixed-size list dimension. A DuckDB
#' `LIST` dimension takes a label: DuckDB converts it to text itself, in a
#' spelling that keeps distinct lists apart.
#'
#' A factor observation that uses an NA level can print as `<NA>` while
#' `is.na()` is false. A missing factor code has `is.na()` equal to true.
#' Source missing values and typed-missing Margin values may display
#' identically, so keep `.id` in the result when the difference matters.
#'
#' @section Margin order:
#' `.sort` orders a result by a key built from its Grouping plan and its
#' values. The key is the result's own leading key columns, left
#' to right, with each Margin dimension preceded by its Grouping bit -- `1` on a
#' row whose grouping set omits that dimension, `0` otherwise -- and by
#' whether its value is missing:
#'
#' ```
#' is.na(by1), by1, ..., bit(d1), is.na(d1), d1, bit(d2), is.na(d2), d2, ...
#' ```
#'
#' A margin row therefore sits with the rows it summarizes instead of wherever
#' its `.margin_label` falls among that dimension's values: the label's text
#' never decides where a margin goes, and neither does the locale. Within a
#' level, rows sort ascending by value, with a source `NA` after the named
#' values. Fixed `.by` keys sort first, so each partition is one contiguous
#' block, and `.id`, when it names a column, breaks the remaining ties.
#' `"first"` reverses the Grouping bits alone: missing values come last
#' wherever they appear, on every backend.
#'
#' Whether there is a label does decide how a non-factor dimension sorts: under
#' a non-missing `.margin_label` it is character, as each verb's Value says,
#' so a numeric dimension sorts as text, `"10"` before `"9"`. Keep its type
#' with `.margin_label = NULL`, or make it a factor first. Factor dimensions
#' sort by their levels, except on Arrow, which sorts a factor by value; see
#' *Arrow inputs*.
#' `.margin_label_position` positions a synthetic factor level and never a
#' row.
#'
#' A Margin order promises what [dplyr::arrange()] promises in the C locale,
#' which it fixes: `options(dplyr.legacy_locale = TRUE)` does not redirect it.
#' It is a property of the object the verb returns: the row order of a local
#' data frame, or the ordering of a lazy result that [dplyr::collect()]
#' observes. Later verbs can change it. It composes with every `.duplicates`
#' policy the verb accepts.
#'
#' *Put each subtotal with the rows it summarizes* in
#' [Recipes for common reporting tasks][recipes] shows a Margin order before
#' and after, and shows the key written as an `arrange()` you control.
#'
#' [recipes]: https://sayuks.github.io/marginplyr/vignettes/recipes.html#put-each-subtotal-with-the-rows-it-summarizes
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
#' [grouping_bit()] and [grouping_id()] can be called only inside
#' [summarize_with_margins()]. To learn which grouping set a row of
#' [expand_with_margins()], [nest_with_margins()], or [nest_by_with_margins()]
#' came from, pass `.id` and join it to the `set_id` column of
#' [inspect_grouping()] called with the same `.by` and `.grouping`, as *Add a
#' level column: detail, subtotal or total* in [Recipes for common reporting
#' tasks][recipes] does. Without `.id`, a typed-missing `.margin_label` leaves a
#' margin row indistinguishable from a source row whose dimension is missing, so
#' summarizing an expansion again by its grouping columns merges the two.
#'
#' [recipes]: https://sayuks.github.io/marginplyr/vignettes/recipes.html#add-a-level-column-detail-subtotal-or-total
#'
#' @section Grouping plan size:
#' A Grouping plan holding more than 4,096 grouping sets is refused with a
#' `"marginplyr_error"`, by every verb and on every backend, before any
#' grouping set is computed. Only a selection that needs a lazy input's column
#' types, such as `where(is.character)`, sends a query first: the metadata
#' reads *When marginplyr queries your data* lists. The count is the plan's
#' after `.duplicates` is applied, the rows [inspect_grouping()] returns, so a
#' specification that writes many repeats of a few sets passes under
#' `.duplicates = "drop"`, except that a Cartesian product whose leading
#' factors alone pass the limit is refused even where a later factor would
#' merge their sets back within it. A [cube()] of n dimensions is 2^n grouping
#' sets, so a `cube()` of 13 columns is refused; a selection such as
#' `cube(where(is.character))` reaches that on a wide table without naming a
#' column. Use fewer Margin dimensions or a narrower Grouping specification,
#' or set `options(marginplyr.max_grouping_sets = )` to a larger whole number,
#' or to `Inf` for no limit.
#'
#' The limit keeps a plan from exhausting memory in your R session; it does
#' not make a large plan cheap, and a database's own limits, such as
#' PostgreSQL's refusal of more than 4,096 native grouping sets, still apply to
#' a plan the raised option admits. *Grouping specifications from user input*
#' in [Database and lazy backends][db-size] says what a plan within the limit
#' still costs and what bounds an application's call.
#'
#' [db-size]: https://sayuks.github.io/marginplyr/vignettes/database_backends.html#grouping-specifications-from-user-input
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
#' vocabulary exactly as the signature spells it, which lets a function of your
#' own repeat `.sort = c("none", "last", "first")` and hand it on.
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
#' @section Arrow inputs:
#' Arrow's tabular dplyr inputs are a `Table`, `RecordBatch`, `Dataset`,
#' `RecordBatchReader`, or an `arrow_dplyr_query` built from them. A summary or
#' expansion of one stays an Arrow query, except that a `RecordBatchReader`, and
#' a query whose source graph contains one, is refused: a reader is one-pass,
#' while a Margin operation may build one branch per grouping set, so
#' [arrow::as_arrow_table()] has to materialize the reusable `Table` first. The
#' nesting verbs refuse every Arrow input; collect it first. Other Arrow
#' objects, such as a `Scanner` or an `Array`, are not dplyr inputs; a Scanner
#' materializes with `$ToTable()`. *Which Arrow inputs are supported* in
#' [Database and lazy backends][db] tabulates these answers per verb.
#'
#' [db]: https://sayuks.github.io/marginplyr/vignettes/database_backends.html#which-arrow-inputs-are-supported
#'
#' For a summary or generated Margin label transformation Arrow's own engine
#' cannot evaluate, a `Table` and `RecordBatch` (and queries built on them)
#' would fall back to R by reading every column of the input, not only the
#' ones the expression names. marginplyr refuses such an expression instead,
#' before a row is read, and names the two rewrites that compute it: collect
#' the input first, and select the columns the summary needs before
#' collecting, which is the narrowing Arrow's own route cannot do for you. A
#' `Dataset` and a query built on one never read themselves into R, so they
#' keep Arrow's own unsupported-expression error instead.
#'
#' Which expressions those are is Arrow's to decide and moves with its version,
#' so they are not listed here. Among the summaries refused are a group
#' collapsed into a single value, such as pasting one, a subset written inside
#' an aggregate, and a statistic over two columns at once; ordinary numeric
#' summaries, and arithmetic over them, are evaluated by Arrow and stay lazy.
#'
#' Arrow does not hold factor levels in the schema marginplyr reads, so a
#' non-missing `.margin_label` converts a factor dimension to character, and its
#' levels and ordered status are lost. A Margin order sorts a factor by value on
#' Arrow whatever the label, fixed `.by` keys included, even where the result
#' column keeps its levels.
#' For the same reason, a label equal to an unused declared level is allowed,
#' and an observed collision is refused only when
#' `.check_margin_label = TRUE`. `NA_character_` and `NULL` convert nothing.
#'
#' @section Database backend coverage:
#' Most summaries on DuckDB and PostgreSQL use one native
#' `GROUP BY GROUPING SETS` query. Other database backends, every expansion,
#' and the summaries listed under *Portable fallback SQL* in
#' [Database and lazy backends][db] use a portable `UNION ALL` query with one
#' branch per grouping set and the same result. Asking for a Margin order never
#' changes which of the two runs. The text of a converted dimension is each
#' backend's own cast, so it can differ from a local result's; *Converted
#' dimension text* in that guide tabulates it. The *Backend support matrix*
#' there sets each backend family's answers side by side, including what
#' becomes of a factor dimension.
#'
#' Continuous integration executes Margin queries on these backends:
#'
#' * DuckDB, SQLite, Arrow, and `dtplyr`: the test suite.
#' * PostgreSQL, on a live server: Parent and Total shares, including inside
#'   a transaction the caller holds, typed missing dimensions, the observed
#'   label check, expansion input, and window-ordered input. The rest of its
#'   coverage is SQL generation.
#' * Microsoft SQL Server, on a live server: portable summaries, compared with
#'   the local result. Its expansion coverage is SQL generation.
#'
#' SQL generation, not execution, is verified for Access, SAP HANA, Hive,
#' Impala, MariaDB, MySQL, Oracle, Amazon Redshift, Snowflake, Spark SQL, and
#' Teradata, and for generic DBI and ODBC connections.
#'
#' [db]: https://sayuks.github.io/marginplyr/vignettes/database_backends.html#portable-fallback-sql
#'
#' On SQLite and DuckDB, identifiers differing only in ASCII letter case name
#' the same column, so a result refuses two public columns that would collide
#' under that rule and names both spellings.
#'
#' A lazy result's Margin order is the outermost query's `ORDER BY`, which
#' [dplyr::collect()] and [dplyr::compute()] observe. Whether it survives
#' later verbs depends on dbplyr's query flattening and is not promised. It is
#' not a window ordering, so a window function over a sorted result still
#' needs [dbplyr::window_order()]. For direct [dplyr::compute()] on SQLite,
#' see *[SQLite materialization][summarize_with_margins]* in
#' [summarize_with_margins()].
#'
#' @section When marginplyr queries your data:
#' A summary or expansion of a lazy input builds a query and returns it
#' unexecuted. [dplyr::show_query()] runs nothing, and no row is read until
#' you execute the query yourself. The nesting verbs' pages state what each
#' does with a `dtplyr` step.
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
#'   its types are read. It binds no table of yours. Table macros, macros you
#'   define, `query()`, `query_table()`, the JSON extension's
#'   `json_execute_serialized_sql()`, and views without stored SQL are
#'   refused, except a data frame registered with `duckdb::duckdb_register()`.
#'   A function sharing its name with a table macro is refused as that macro
#'   is, wherever the call stands; in a database holding no macro of yours,
#'   DuckDB's own `histogram()` aggregate is the one such function. Call
#'   [dplyr::compute()] first to use it.
#'   Other table functions, such as `read_parquet()` or `range()`, are
#'   admitted inside a view as they are when written directly.
#'   A macro you define is refused however the query reaches it: written in
#'   SQL, or called from a dplyr verb such as `mutate(y = my_macro(x))`,
#'   including a function dbplyr translates to that macro's name. A macro
#'   named after an operator or a function DuckDB calls for other syntax,
#'   such as `+` or `count_star` (behind `COUNT(*)`), refuses typed
#'   metadata on its connection, since any statement could reach it. Call
#'   [dplyr::compute()] or [dplyr::collect()] first to use one.
#'   DuckDB's own scalar macros, such as `nullif()`, are admitted only while
#'   the database holds no macro you defined, since one could replace a name
#'   inside their bodies.
#' - **At most two queries per share request until the SQL dialect answers**,
#'   for a summary that requests a share, referencing none of your tables;
#'   see *Parent shares use a staged lazy query* in
#'   [Database and lazy backends][db].
#'
#' [db]: https://sayuks.github.io/marginplyr/vignettes/database_backends.html#parent-shares-use-a-staged-lazy-query
#'
#' A query is not safe merely because it returns no rows: some services bill
#' by the bytes a query scans, and `LIMIT 0` is not exempt. marginplyr cannot
#' tell a billed connection from a free one, so `.check_margin_label` defaults
#' to `is.data.frame(.data)` and a lazy input is checked only when asked;
#' *[dtplyr inputs][nest_by_with_margins]* in [nest_by_with_margins()] states
#' the one exception. The observed check scans every input row of the checked
#' columns and retrieves one row of collision counts.
#'
#' With recording enabled before the call, [last_sent_queries()] lists the
#' SQL prepared for these internal queries in the order sent.
#'
#' @name margin_verbs
#' @family summarize and expand data with margins
NULL
