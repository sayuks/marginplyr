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
#' on values, call [dplyr::collect()] first, and set `.check_margin_label`
#' explicitly, since its default changes to `TRUE` for the collected data.
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
#' the input before the Margin operation, as the [Complete absent keys before
#' margins][keys] guide shows.
#'
#' Row-wise input is rejected; to keep row-wise identifiers as fixed keys,
#' ungroup first and select them with `.by`. Each verb's page states the
#' grouping of its result.
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
#' displayed values. The key is the result's own leading key columns, left
#' to right, with each Margin dimension preceded by its Grouping bit -- `1` on a
#' row whose grouping set omits that dimension, `0` otherwise -- and by
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
#' Non-missing labels convert ordinary Margin dimensions to character. A
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
#' A local packed data-frame, matrix, or array Margin dimension accepts only
#' `NULL` or `NA_character_` as its Margin label, because no scalar string can
#' represent such a key; split it into separate columns if it needs display
#' text.
#' The same holds for a list-column dimension, locally or under dtplyr, and
#' for an Arrow list, large list, or fixed-size list dimension.
#'
#' Factor columns follow this contract:
#'
#' | Margin label | NA level | Missing value | Result |
#' |---|---:|---:|---|
#' | `NA_character_` | yes | yes | Error: NA is already a factor level |
#' | `NA_character_` | yes | no | Error: NA is already a factor level |
#' | `NA_character_` | no | yes | Allowed; source missing values and margins display alike, so tell them apart with `.id` | # nolint: line_length_linter
#' | `NA_character_` | no | no | Allowed; use typed missing |
#' | `NULL` | yes | yes | Allowed; source missing values and margins display alike, so tell them apart with `.id` | # nolint: line_length_linter
#' | `NULL` | yes | no | Allowed; preserve the NA level and use typed missing |
#' | `NULL` | no | yes | Allowed; source missing values and margins display alike, so tell them apart with `.id` | # nolint: line_length_linter
#' | `NULL` | no | no | Allowed; use typed missing |
#'
#' A factor observation that uses an NA level can print as `<NA>` while
#' `is.na()` is false. A missing factor code has `is.na()` equal to true.
#' Source missing values and typed-missing Margin values may display
#' identically, so keep `.id` in the result when the difference matters.
#' Every row above is decided whatever `.check_margin_label` says.
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
#' materializes with `$ToTable()`. *Which Arrow inputs are supported* in the
#' [database guide][db] tabulates these answers per verb.
#'
#' [db]: https://sayuks.github.io/marginplyr/vignettes/database_backends.html
#'
#' A summary Arrow cannot evaluate is refused as *[Summaries a lazy backend
#' cannot carry][summarize_with_margins]* in [summarize_with_margins()] states,
#' and a generated Margin label transformation Arrow cannot translate is
#' refused on the same terms.
#'
#' Arrow does not hold factor levels in the schema marginplyr reads, so a
#' non-missing `.margin_label` converts a factor dimension to character, its
#' levels and ordered status are lost, and a Margin order sorts it by value.
#' For the same reason, a label equal to an unused declared level is allowed,
#' and an observed collision is refused only when
#' `.check_margin_label = TRUE`. `NA_character_` and `NULL` convert nothing.
#'
#' @section Database backend coverage:
#' Most summaries on DuckDB and PostgreSQL use one native
#' `GROUP BY GROUPING SETS` query. Other database backends, every expansion,
#' and the summaries listed under *Portable fallback SQL* in the
#' [database guide][db] use a portable `UNION ALL` query with one branch per
#' grouping set and the same result. Asking for a Margin order never changes
#' which of the two runs. The text of a converted dimension is each backend's
#' own cast, so it can differ from a local result's; *Converted dimension
#' text* in the database guide tabulates it. The *Backend support matrix*
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
#' [db]: https://sayuks.github.io/marginplyr/vignettes/database_backends.html
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
#'   define, `query()`, `query_table()`, and views without stored SQL are
#'   refused, except a data frame registered with `duckdb::duckdb_register()`.
#'   Other table functions, such as `read_parquet()` or `range()`, are
#'   admitted inside a view as they are when written directly.
#'   A macro you define is refused however the query reaches it: written in
#'   SQL, or called from a dplyr verb such as `mutate(y = my_macro(x))`,
#'   including a function dbplyr translates to that macro's name. Call
#'   [dplyr::compute()] or [dplyr::collect()] first to use one.
#'   DuckDB's own scalar macros, such as `nullif()`, are admitted only while
#'   the database holds no macro you defined, since one could replace a name
#'   inside their bodies.
#' - **A table-free dialect question**, for a summary that requests a share,
#'   until the SQL dialect answers; *[Lazy execution
#'   boundaries][share_of_parent]* in [share_of_parent()] states what it asks.
#'
#' A query is not safe merely because it returns no rows: some services bill
#' by the bytes a query scans, and `LIMIT 0` is not exempt. marginplyr cannot
#' tell a billed connection from a free one, so `.check_margin_label` defaults
#' to `is.data.frame(.data)` and a lazy input is checked only when asked,
#' except that [nest_by_with_margins()], which reads a `dtplyr` step's rows
#' when called, also checks one by default. The
#' observed check scans every input row of the checked columns and retrieves
#' one row of collision counts.
#'
#' With recording enabled before the call, [last_sent_queries()] lists the
#' SQL prepared for these internal queries in the order sent.
#'
#' @name margin_verbs
#' @family summarize and expand data with margins
NULL
