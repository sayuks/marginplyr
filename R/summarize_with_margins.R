#' Summarize data with SQL-style grouping operations
#'
#' [summarize_with_margins()] extends [dplyr::summarize()] with grouping sets,
#' rollups, cubes, totals, and subtotals. The same interface works with local
#' data frames and lazy tables.
#'
#' Use it when one report needs several levels of detail—for example, stores,
#' region subtotals, and a company total—or when that report should use the
#' same code locally and through [dbplyr::tbl_lazy()].
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
#' @param .grouping A grouping specification made with [grouping_set()],
#'   [grouping_sets()], [rollup()], [cube()], or [grouping_spec()]. `NULL`
#'   represents one empty grouping set. Any expression returning a
#'   specification can be written here, while a specification nested inside
#'   another must be a constructor call or a name bound to one; see
#'   [grouping_set()].
#' @param .margin_label A display label for dimensions omitted from a grouping
#'   set. An unnamed character scalar applies to every resolved Margin
#'   dimension. A named character vector must name every resolved Margin
#'   dimension exactly once; order is irrelevant, and fixed `.by` columns must
#'   not be named. `NA_character_` and `NULL` use typed missing values instead
#'   of a display label. See *Display labels and grouping identity*.
#' @param .margin_label_position Either `"last"` (the default) or `"first"`.
#'   This controls the position of a non-missing synthetic label in factor and
#'   ordered-factor levels. It does not sort result rows and has no effect for
#'   `NA_character_` or `NULL`.
#' @param .check_margin_label A logical scalar controlling the half of the
#'   Margin label collision check that reads the data: whether any *value* of a
#'   Margin dimension is equal to that dimension's display label. Each
#'   dimension is checked independently, and `.margin_label = NULL` opts out.
#'   Every Margin verb uses the same default: `TRUE` for local data frames and
#'   `FALSE` for lazy inputs, which are read only when the caller asks. A label
#'   equal to a declared factor *level* is rejected on every backend whatever
#'   this argument says, because the levels are already known and finding it
#'   sends no query. Left unchecked, a colliding value gives the result a
#'   margin row and a source row that no grouping column tells apart; keeping
#'   [grouping_bit()] or [grouping_id()] in the result distinguishes them at no
#'   added cost. See *Display labels and grouping identity* for the factor
#'   missing-value contract.
#' @param .check_share_source A logical scalar, `TRUE` by default on every
#'   backend, including lazy ones: establishing that a share's source summary
#'   is an eligible type reads none of your data. `FALSE` calculates a
#'   requested [share_of_parent()] or [share_of_total()] from a source whose
#'   eligibility marginplyr could not establish, which is a SQL dialect that
#'   converts a value of another type to a number rather than refusing it, and
#'   a backend that could not be asked which of those it does. It changes
#'   nothing where the rule can be applied — a local data frame, a `dtplyr`
#'   step, and a database that refuses an ineligible summary itself all apply
#'   it whatever this argument says. See *Contextual shares*.
#' @param .duplicates One of `"error"`, `"drop"`, or `"keep"`, controlling
#'   duplicate grouping sets after expansion.
#' @param .sort One of `"none"` (the default), `"last"`, or `"first"`. `"none"`
#'   leaves row order unspecified. The other two order the result by the
#'   structure of its Grouping plan: within each fixed `.by` key, every grouping
#'   dimension contributes its Grouping bit and its missingness before its own
#'   value, so a margin row sits with the rows it summarizes rather than
#'   wherever `.margin_label` falls among that dimension's values, and missing
#'   values come last wherever they appear. `"last"`
#'   places margins after the rows they summarize and `"first"` before them.
#'   The order is a property of the returned object, as with
#'   [dplyr::arrange()], and may not survive further verbs applied to a lazy
#'   result. See *Margin order*.
#' @param .id `NULL`, or one non-missing, non-empty character string naming an
#'   integer output column of one-based Grouping set identifiers. Each value
#'   identifies one occurrence in the resolved Grouping plan. The name must not
#'   collide with source columns, grouping keys, summary outputs, or a nesting
#'   `.key`.
#'
#' @return An ungrouped data frame, or a lazy table when `.data` is lazy. Its
#'   class and attributes follow [dplyr::summarize()]; see *Result class and
#'   attributes*.
#'   Result row order is unspecified unless `.sort` asks for a Margin order;
#'   see *Margin order*, or use [dplyr::arrange()] for any other presentation
#'   order.
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
#' Confirmed SQL backends use one `GROUP BY GROUPING SETS` query. Other lazy
#' backends use a portable `UNION ALL` adapter with the same semantics.
#' [summarize_with_margins()] and [summarise_with_margins()] are synonyms,
#' following [dplyr::summarize()] and [dplyr::summarise()].
#'
#' @section Fixed columns and grouping dimensions:
#' `.by` marks columns that are present in every grouping set, while
#' `.grouping` describes dimensions that can be omitted to form margins.
#' Columns in `.by` retain their input types, never receive `.margin_label`,
#' and return `0` from [grouping_bit()].
#'
#' At the grouping-set level, `.grouping` alone can reproduce structures that
#' use `.by`. For example, `.by = year` is structurally equivalent to
#' `.grouping = grouping_set(year)`. Similarly, `.by = year` together with
#' `.grouping = rollup(region, store)` produces the same grouping sets as
#' `.grouping = grouping_spec(grouping_set(year), rollup(region, store))`.
#'
#' These forms are not completely interchangeable in the current
#' implementation. A column supplied through `.grouping` is treated as a
#' margin dimension even when every expanded grouping set contains it.
#' Consequently, it participates in `.margin_label` type conversion and
#' collision checks. Use `.by` for columns that must always remain fixed, and
#' use `.grouping` for dimensions that may become totals.
#'
#' @section Grouped and row-wise inputs:
#' When `.data` has been grouped with [dplyr::group_by()] and `.by` is `NULL`,
#' its grouping columns become implicit fixed keys. For example,
#' `group_by(year)` followed by `.grouping = rollup(region)` is computationally
#' equivalent to using `.by = year` on the ungrouped data. This rule is the
#' same for local data frames and lazy tables.
#'
#' As with [dplyr::summarize()] and [tidyr::nest()], a grouped input cannot
#' also supply `.by`; call [dplyr::ungroup()] first when replacing the existing
#' groups. A grouping column also cannot appear in `.grouping`, because one
#' column cannot be both a fixed key and a dimension that can be rolled up.
#' Grouped local data created with `.drop = FALSE` is rejected because empty
#' factor groups do not have a consistent equivalent in SQL and other lazy
#' backends.
#'
#' Unlike the default output of [dplyr::summarize()] on grouped data,
#' [summarize_with_margins()], [expand_with_margins()], and
#' [nest_with_margins()] always return ungrouped results. Arbitrary grouping
#' sets contain multiple grains, so there is no single meaningful grouping
#' hierarchy to retain.
#' [nest_by_with_margins()] instead returns a row-wise data frame grouped by
#' all visible fixed keys, grouping dimensions, and `.id` when supplied.
#' Row-wise input is rejected; call [dplyr::ungroup()] first.
#'
#' @section Result class and attributes:
#' Each Margin verb follows the same class and attribute rules as the dplyr
#' verb it is built from: [summarize_with_margins()] those of
#' [dplyr::summarize()], and [expand_with_margins()] and the nesting verbs
#' those of [dplyr::mutate()] combined with [dplyr::union_all()]. Passing a
#' plain data frame therefore returns a plain data frame and passing a tibble
#' returns a tibble.
#'
#' The input class is not guaranteed to be preserved, and neither are
#' object-level attributes of the input or attributes of columns marginplyr
#' does not rewrite. A data frame subclass survives only where dplyr can
#' reconstruct it, so a subclass with no [dplyr::dplyr_reconstruct()] method
#' is lost by [dplyr::summarize()] itself. Attributes on a column that carries
#' no class are dropped wherever branches are combined, because that is what
#' the vctrs rules for combining bare vectors do with them. Attach the
#' attributes a result must carry after the Margin operation, as with any
#' dplyr pipeline.
#'
#' Factor and ordered-factor columns are the one exception, because
#' marginplyr decomposes them to insert `.margin_label` and rebuilds them
#' itself. Their levels and ordering are preserved; see *Display labels and
#' grouping identity*. Classed columns such as [Date] and [POSIXct], including
#' its `tzone`, are carried through by dplyr and vctrs unchanged.
#'
#' @section Grouping set identifiers:
#' When `.id` names an output column, each result row receives the one-based
#' position of its Grouping set occurrence after applying `.duplicates`.
#' `"drop"` renumbers retained occurrences, while supported `"keep"` paths give
#' identical duplicate sets distinct identifiers. One Grouping set has
#' identifier `1L`, and a zero-row result retains an integer `.id` column.
#'
#' Output columns are ordered as fixed keys, variable dimensions, `.id`, then
#' ordinary output columns. For [nest_with_margins()], `.id` is an outer key
#' and is not included inside the nested data. For
#' [nest_by_with_margins()], it is also a row-wise grouping key.
#'
#' `.id` records plan occurrence, not physical result order, and is not a
#' durable business key: reordering or deduplicating the Grouping
#' specification changes it. Use [dplyr::arrange()] when order matters.
#' [grouping_bit()] documents how `.id` compares with
#' [inspect_grouping()]`$set_id`, [grouping_bit()], and [grouping_id()].
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
#' its `.margin_label` falls among that dimension's values, which is what keeps
#' the order independent of the label and of the locale. Two rules follow from
#' the key rather than being separate: fixed `.by` keys sort first because they
#' come first, so each partition is one contiguous block whose internal order
#' does not depend on any other partition; and `.id` breaks the remaining ties
#' when it names a column, which puts duplicate occurrences of one grouping set
#' next to each other and in Grouping plan order. A composite dimension needs no
#' rule of its own, because its columns share one Grouping bit.
#'
#' `"first"` reverses the Grouping bits alone. Missing values and ordinary
#' values stay ascending, because the choice positions margins and not missing
#' values. Every column in the key carries a missingness term, fixed `.by` keys
#' included, so missing values come last wherever they appear on every backend,
#' including those whose own default is the opposite. Under
#' `.margin_label = NULL` a source missing value and a margin still display
#' alike, but they are separated by position, because their Grouping bits
#' differ.
#'
#' Factor and ordered-factor dimensions sort by their restored levels rather
#' than by their rendering. `.margin_label_position` positions a synthetic
#' factor level and never a row: it changes `levels()` and nothing else, so the
#' two options stay independent and no combination of them is wrong.
#'
#' A Margin order promises exactly what [dplyr::arrange()] promises. It is a
#' property of the object the verb returns: on local data frames and `dtplyr`
#' steps that is the row order, and on lazy tables it is the outermost query's
#' `ORDER BY`, which [dplyr::collect()] and [dplyr::compute()] both observe:
#' a materialized result carries the Margin order rather than losing it.
#' Whether the order survives further verbs applied to a lazy result is not
#' promised, because that depends on dbplyr's query flattening, which
#' marginplyr does not own and which changes between releases.
#'
#' What a lazy result does not carry is a dbplyr window ordering. The key reads
#' Grouping bits from a column the result does not expose, so no ordering over
#' the returned columns alone reproduces it, and marginplyr leaves none
#' recorded rather than record a truncated one. A window function written over
#' a sorted lazy result therefore needs [dbplyr::window_order()], exactly as it
#' would over an unsorted one, and asking for a Margin order discards any
#' window ordering the input carried. `.sort = "none"` records no order and
#' clears none.
#'
#' Asking for a Margin order never costs a native `GROUP BY GROUPING SETS`
#' plan, and never changes which adapter runs. It composes with every
#' `.duplicates` policy the verb accepts with no diagnostic, and lazy inputs
#' stay lazy.
#'
#' The [recipes guide][recipes] shows a Margin order before and after, and
#' shows a join dropping one from a lazy result with no diagnostic naming this
#' package.
#'
#' [recipes]: https://sayuks.github.io/marginplyr/vignettes/recipes.html
#'
#' @section Relationship to dplyr summaries:
#' The `...` expressions use [dplyr::summarize()] data-masking semantics.
#'
#' A few spellings mean something only because marginplyr rewrites them before
#' anything runs. Those are recognized by spelling and are never looked up in
#' the environment the call was written in, so binding a function of your own
#' to one of their names does not change what this verb does with it. They are
#' [grouping_bit()] and [grouping_id()]; [share_of_parent()] and
#' [share_of_total()]; [dplyr::across()], [dplyr::if_any()],
#' [dplyr::if_all()], and [dplyr::pick()]; [tidyselect::where()]; and the
#' branch-local helpers rejected below. A spelling is recognized when the name
#' matches and it is written bare or qualified with the package that owns it,
#' so `dplyr::across()` is the same request as `across()` while
#' `mypkg::across()` is an ordinary call to another package's function.
#'
#' Redundant parentheses change nothing, because `(` is the identity function:
#' `(pick)(units)` and `(pick(units))` are the same request as `pick(units)`. A
#' head that has to be evaluated to know what it calls is not a spelling at all,
#' so `get("pick")(units)` is an ordinary call.
#'
#' Every other name follows ordinary lookup, [dplyr::n()] included, and so do
#' the Grouping specification constructors: a nested `rollup(region)` is
#' evaluated because of how it is spelled, but what runs is whatever `rollup`
#' is bound to where you wrote it.
#'
#' [dplyr::across()] and [dplyr::pick()] cannot select any column named in the
#' complete grouping plan. This extends dplyr's grouping-column rule across
#' every branch: a dimension remains excluded even in a grouping set from
#' which it is omitted.
#'
#' Summary results may not overwrite a fixed key or grouping dimension,
#' including through a data-frame-valued summary. The local dplyr backend can
#' overwrite an existing variable and reuse a newly created summary in a
#' later expression, but other backends may not. marginplyr rejects grouping
#' key overwrites so that grouping identity and behavior stay portable.
#' Use a new summary name, or rename the grouping column before this call.
#'
#' [dplyr::cur_group()], [dplyr::cur_group_id()],
#' [dplyr::cur_group_rows()], and the deprecated `cur_data*()` helpers are
#' rejected. They describe one branch-local grouping or data mask, whereas a
#' margin result combines several grouping sets and their identifiers, row
#' positions, or columns would not have one global meaning. Use
#' [grouping_bit()] and [grouping_id()] to identify margin levels. They are
#' rejected by spelling like the rest, which is stricter than
#' [dplyr::summarize()]: a caller who has bound `cur_group_id` to a function
#' of their own still gets the refusal, because reading that binding would
#' mean resolving a call head against the calling environment.
#'
#' Every rule above reads the expression the data mask evaluates. An expression
#' captured as language data — the argument of a plainly written
#' [base::quote()], [base::substitute()], or [base::expression()] — is a value
#' the summary carries, so marginplyr neither analyzes nor rewrites what is
#' inside it: a captured helper name requests nothing, creates no dependency on
#' an earlier summary, and reaches the backend as written. Evaluating one with
#' [base::eval()], [rlang::eval_tidy()], or [rlang::eval_bare()] runs it in the
#' data mask, so what it holds is analyzed as the code it becomes and every
#' rule above applies to it.
#'
#' Both halves are read statically, so both stop where a static reading does.
#' A capture is one where the call names the primitive plainly, qualified with
#' `base::` or not; any other spelling — another namespace, or a head computed
#' at run time — is analyzed as ordinary code, which can refuse a call that
#' only carries language. An evaluation is followed wherever the language it
#' runs can be read without running the call, which covers a capture written
#' out and text parsed from a literal, but not language a summary builds while
#' it runs.
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
#' contains one, including [cube()]; duplicate Grand total occurrences hold
#' the same values and are interchangeable.
#'
#' Arrow inputs reject both after expression planning and common
#' Margin-operation validation but before constructing a summary query. Other
#' Arrow Margin operations remain supported and lazy. Explicitly collect an
#' Arrow input first when local share execution is appropriate.
#'
#' A row that is its own denominator receives `1.0`. Missing numerators or
#' denominators and zero denominators receive `NA_real_`; other finite ratios
#' are not clamped. Matching is structural, so `.id`, missing grouping values,
#' and displayed Margin labels do not determine the denominator.
#'
#' The source must be a unique, preceding, self-contained integer or double
#' scalar summary. Lazy execution preserves collision-safe Grouping set
#' metadata through ordinary aggregation, calculates the requested shares
#' through one shared mapping per denominator kind, and then removes the
#' metadata before returning the requested column order.
#'
#' Local data frames reject an ineligible source before any share is
#' calculated. `dtplyr` steps stay lazy and report the same conditions during
#' explicit execution, before an invalid grouping row is emitted. General
#' dbplyr backends read the ordinary summaries over one input row and reject
#' an ineligible source with the same condition; cardinality remains a
#' local-and-`dtplyr` rule, because a SQL aggregate returns one value per
#' grouping row by construction.
#'
#' [share_of_parent()] is the canonical reference for the complete
#' direct-expression, source, ordering, value, empty-input, and `across()`
#' contracts of both helpers.
#'
#' @section Display labels and grouping identity:
#' `.margin_label` is a display value, not the identity of a grouping set. An
#' unnamed scalar labels every resolved Margin dimension. A named vector
#' provides column-specific labels and must cover the resolved dimensions
#' exactly once; missing, unknown, duplicate, and empty names are rejected, as
#' are names from `.by`.
#'
#' Non-missing labels convert ordinary grouping dimensions to character. A
#' factor or ordered factor is reconstructed after the Margin operation,
#' preserving ordered status and placing a new synthetic level last by default
#' or first when `.margin_label_position = "first"`. A label equal to any
#' declared level, used or unused, is rejected before any grouping set is
#' built, whatever `.check_margin_label` says: see that argument above.
#' Reconstruction preserves the distinction between an observation that uses a
#' factor NA level and an actually missing factor code.
#'
#' `NA_character_` and `NULL` both create a typed missing Margin value and do
#' not create a synthetic factor level. Position is therefore a no-op for
#' either value. `NA_character_` still participates in collision validation;
#' `NULL` opts out. A factor NA level is a structural conflict for
#' `NA_character_` even when `.check_margin_label = FALSE`.
#'
#' With `.check_margin_label = TRUE`, factor columns follow this contract:
#'
#' | Margin label | NA level | Missing value | Result |
#' |---|---:|---:|---|
#' | `NA_character_` | yes | yes | Error: NA is already a factor level |
#' | `NA_character_` | yes | no | Error: NA is already a factor level |
#' | `NA_character_` | no | yes | Error: the label collides with a value |
#' | `NA_character_` | no | no | Allowed; use typed missing |
#' | `NULL` | yes | yes | Allowed; source missing values and margins require structural identity | # nolint: line_length_linter
#' | `NULL` | yes | no | Allowed; preserve the NA level and use typed missing |
#' | `NULL` | no | yes | Allowed; source missing values and margins require structural identity | # nolint: line_length_linter
#' | `NULL` | no | no | Allowed; use typed missing |
#'
#' A factor observation that uses an NA level can print as `<NA>` while
#' `is.na()` is false. A missing factor code has `is.na()` equal to true.
#' Source missing values and typed-missing Margin values may display
#' identically, so keep a structural identity column when the difference
#' matters: `.id` is available from every Margin verb, and
#' [summarize_with_margins()] can additionally write [grouping_bit()] or
#' [grouping_id()] as summaries. `.check_margin_label` controls only the
#' observed row of this table -- whether `NA_character_` collides with an
#' actual missing value when no NA level is declared; the declared rows above
#' it are checked whatever this argument says. See `.check_margin_label`
#' above for its default and *When marginplyr queries your data* for why the
#' two halves differ.
#'
#' @section Backend extension design:
#' Unlike [dplyr::summarize()], the public margin verbs are intentionally not
#' S3 generics. They prepare one operation around a backend-independent
#' grouping plan, pass it to a verb-specific executor, and apply common
#' finalization. One typed selection-metadata snapshot is acquired during
#' preparation. Native `GROUPING SETS` and portable `UNION ALL` adapters
#' consume the prepared plan; they do not own validation or finalization.
#' These adapters are implementation details rather than an extension API, so
#' support for a new backend should be added to marginplyr itself with
#' metadata, result, laziness, and SQL-strategy contract tests.
#'
#' @section Database backend coverage:
#' DuckDB and PostgreSQL use native `GROUP BY GROUPING SETS` SQL. Automated
#' tests execute DuckDB queries against a live in-memory database and verify
#' PostgreSQL SQL with dbplyr's simulator.
#'
#' The portable `UNION ALL` SQL path is executed end to end for contextual
#' shares against a live in-memory SQLite database. It is also verified with
#' dbplyr simulators for Access, SAP HANA, Hive, Impala, MariaDB, Microsoft SQL
#' Server, MySQL, Oracle, Amazon Redshift, Snowflake, Spark SQL, SQLite, and
#' Teradata, plus generic DBI and ODBC connections. Simulator coverage verifies
#' SQL generation, not execution against every database server.
#'
#' Arrow and dtplyr are also tested lazy backends, but they are not SQL
#' database connections.
#'
#' @section When marginplyr queries your data:
#' Every Margin verb applied to a lazy input builds a query and returns it
#' unexecuted. [dplyr::show_query()] runs nothing, and no row is read until
#' you execute the query yourself -- [nest_by_with_margins()] excepted,
#' because its row-wise return shape exists only locally; see its own
#' documentation for when it collects.
#'
#' Two queries reach your connection without being asked for, and neither
#' reads a row of your data:
#'
#' - **A zero-row read of the input**, sent only to a backend whose factor
#'   columns marginplyr must decompose and later restore -- currently
#'   `dtplyr` and DuckDB -- to recover the levels and column prototypes that
#'   decomposition loses. It references your table but reads none of it, and
#'   it is not a shape marginplyr introduced: [dplyr::tbl()] already sends an
#'   equivalent zero-row read for any table reference, on any dbplyr backend.
#' - **One query per SQL dialect**, sent once per dialect, the first time a
#'   share is requested there with `.check_share_source` at its default of
#'   `TRUE`, asking whether the dialect converts a non-numeric value to a
#'   number rather than refusing it. It references none of your tables, so
#'   reading it touches none of your data, and the answer is a property of
#'   the dialect, reused for every later connection that shares it. A
#'   connection that cannot be asked -- one built with a
#'   `dbplyr::simulate_*()` constructor, which executes nothing -- is treated
#'   as unable to answer, which refuses the share by default the same way a
#'   dialect known to convert does; see `.check_share_source` on
#'   [summarize_with_margins()].
#'
#' Neither query is safe merely because it returns no rows. A read bounded in
#' rows is not bounded in what it costs: BigQuery states plainly that a
#' `LIMIT` does not reduce the bytes billed for a non-clustered table, and no
#' vendor documentation exempts `LIMIT 0` from that rule.
#'
#' marginplyr does not try to tell whether a backend bills for a query,
#' because nothing it can read distinguishes a free connection from a billed
#' one of the same kind -- a local DuckDB file from a hosted DuckDB service,
#' or RDS PostgreSQL from Aurora Standard. `is.data.frame(.data)` is the only
#' predicate that answers "no external system is involved" exactly rather
#' than approximately, and it is what sets the default of every check that
#' reads your data: a check that reads it is asked for, and a check that does
#' not is not. `.check_margin_label` scans the grouping columns, so every
#' Margin verb defaults it to `is.data.frame(.data)`.
#' [summarize_with_margins()]'s `.check_share_source` reads nothing on any
#' backend, so it defaults to `TRUE` there.
#'
#' @family summarize and expand data with margins
#' @export
#' @examples
#' # Build one monthly management report with store detail, region subtotals,
#' # and a company total.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   units = sum(units),
#'   revenue = sum(revenue),
#'   .by = c(year, month),
#'   .grouping = rollup(region, store),
#'   .id = "set"
#' )
#'
#' # `.sort` puts each subtotal with the rows it summarizes and the company
#' # total last, whatever the Margin label sorts as.
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
#' # The retained type is the point of `NULL`, and a plain data frame prints
#' # the Margin row as a bare `NA` whatever `year` now holds. A tibble's type
#' # header is what shows that `year` is still <int> rather than character.
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
#' # `.margin_label_position`. An existing level -- used or unused -- is never
#' # available for reuse: it is rejected whatever `.check_margin_label` says.
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
#'   .margin_label = "unused",
#'   .check_margin_label = FALSE
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
#' # `.check_margin_label = FALSE` still controls only the observed half of
#' # the check: whether a missing value already in the column collides with
#' # `NA_character_` when no NA level is declared. Disabled, the margin row
#' # and the real missing-value row print alike; `grouping_bit()` tells them
#' # apart.
#' status_data <- data.frame(
#'   status = factor(c("active", NA), levels = c("active", "inactive")),
#'   value = c(1, 2)
#' )
#' try(summarize_with_margins(
#'   .data = status_data,
#'   total = sum(value),
#'   .grouping = rollup(status),
#'   .margin_label = NA_character_
#' ))
#' summarize_with_margins(
#'   .data = status_data,
#'   total = sum(value),
#'   is_total = grouping_bit(status),
#'   .grouping = rollup(status),
#'   .margin_label = NA_character_,
#'   .check_margin_label = FALSE
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
#' # Empty unpartitioned input has one Grand total set row, whose share is
#' # one; fixed `.by` input has no partitions. Both retain a double
#' # Parent-share column.
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
#' # across() and pick() treat every fixed key and margin dimension as a
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
#'   .grouping = rollup(region)
#' ) |>
#'   dplyr::arrange(year, region)
#'
#' # DuckDB executes a native GROUP BY GROUPING SETS query. The guard shipped
#' # with marginplyr keeps this example runnable without DuckDB, and withholds
#' # it just the same when the installed DuckDB is older than the version
#' # DESCRIPTION requires -- `shared_home` below is an error there, not a
#' # degraded result.
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
#'     .grouping = rollup(region, store)
#'   ) |>
#'     dplyr::arrange(year, month, region, store)
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
  call <- rlang::current_call()
  dots <- rlang::enquos(...)
  grouping_quo <- rlang::enquo(.grouping)
  by_quo <- rlang::enquo(.by)

  share_kinds <- with_margin_error_call(
    {
      assert_margin_input(.data)
      assert_lazy_table(.data)
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
    .id = .id,
    validate_grouping = share_grouping_spec_validator(share_kinds),
    call = call
  )
  execution <- execute_margin_summary(
    operation,
    dots,
    check_share_source = .check_share_source
  )
  finalize_margin_operation(operation, execution)
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
        call = operation$call
      )
      dots <- summary_plan$dots
      summary_selection_proxy <- dplyr::select(
        operation$data_proxy,
        dplyr::all_of(setdiff(
          operation$data_vars,
          unique(group_vars)
        ))
      )
      summary_output_names <- unique(c(
        names(dots)[nzchar(names(dots))],
        known_summary_output_names(dots, summary_selection_proxy)
      ))
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
        operation$data_vars,
        summary_output_names,
        operation$set_id_name
      ))
      has_shares <- length(summary_plan$requests) > 0L

      validate_margin_operation(operation)

      if (
        has_shares &&
          identical(operation$backend$kind, "arrow")
      ) {
        abort_arrow_shares(share_request_kinds(summary_plan$requests))
      }

      staged_result <- stage_margin_summaries(
        operation,
        dots = dots,
        reserved_names = reserved_names,
        keep_set_identity = has_shares
      )

      if (has_shares) {
        return(new_margin_execution(
          execute_shares(
            operation,
            staged_result = staged_result,
            requests = summary_plan$requests,
            check_share_source = check_share_source
          ),
          sort_id = margin_summary_stage_sort_id(staged_result)
        ))
      }
      new_margin_execution(
        margin_summary_stage_result(staged_result),
        sort_id = margin_summary_stage_sort_id(staged_result)
      )
    },
    call = operation$call
  )
}

stage_margin_summaries <- function(operation,
                                   dots,
                                   reserved_names,
                                   keep_set_identity) {
  plan <- operation$plan
  set_id_name <- operation$set_id_name
  if (keep_set_identity) {
    set_id_name <- new_margin_internal_names(
      1L,
      used_names = reserved_names,
      prefix = "..marginplyr_set_id_"
    )
    reserved_names <- c(reserved_names, set_id_name)
  }

  # Which adapter runs stays a function of the duplicate policy and of an
  # identifier that has to number occurrences. A Margin order needs Grouping
  # bits rather than occurrences, so the identifier it may add below is
  # allocated after this decision and never changes it.
  use_native <- supports_grouping_sets(
    operation$data,
    plan,
    backend = operation$backend
  ) && !(
    !is.null(set_id_name) &&
      identical(plan$duplicates, "keep")
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
        summarize_margin_native(
          operation$data,
          dots = dots,
          plan = plan,
          margin_labels = operation$margin_labels,
          reserved_names = reserved_names,
          set_id_name = set_id_name,
          set_id_is_internal = set_id_is_internal
        )
      } else {
        summarize_margin_union(
          operation$data,
          dots = dots,
          plan = plan,
          margin_labels = operation$margin_labels,
          column_info = operation$column_info,
          reserved_names = reserved_names,
          set_id_name = set_id_name,
          set_id_is_internal = set_id_is_internal
        )
      }
    },
    error = function(cnd) {
      parent <- cnd$parent
      if (keep_set_identity && inherits(parent, "marginplyr_error")) {
        stop(parent)
      }
      stop(cnd)
    }
  )
  new_margin_summary_stage(result, set_id_name, sort_id = sort_id)
}

new_margin_summary_stage <- function(result, set_id_name, sort_id = NULL) {
  structure(
    list(
      result = result,
      set_id_name = set_id_name,
      sort_id = sort_id
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
