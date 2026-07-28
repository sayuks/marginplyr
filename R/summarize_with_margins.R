#' Summarize data with SQL-style grouping operations
#'
#' [summarize_with_margins()] extends [dplyr::summarize()] with grouping sets,
#' rollups, cubes, totals, and subtotals. The same interface works with local
#' data frames and lazy tables.
#'
#' @param .data A data frame or lazy table.
#' @param ... Name-value pairs as used in [dplyr::summarize()]. Contextual
#'   helpers [grouping_bit()] and [grouping_id()] can also be used here.
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns included in
#'   every grouping set. These columns never receive `.margin_label`. When
#'   `.data` is grouped and `.by` is `NULL`, its grouping columns are used as
#'   implicit fixed keys.
#' @param .grouping A grouping specification made with [grouping_set()],
#'   [grouping_sets()], [rollup()], [cube()], or [grouping_spec()]. `NULL`
#'   represents one empty grouping set.
#' @param .groups `NULL` or `"drop"`. Unlike [dplyr::summarize()], margin
#'   summaries always drop grouping metadata because a result containing
#'   multiple grouping sets has no single `drop_last` hierarchy.
#' @param .margin_label A character scalar used to display columns omitted from
#'   a grouping set. The default is `"Total"`. Use `NULL` to keep typed missing
#'   values instead of inserting a display label.
#' @param .check_margin_label A logical scalar. If `TRUE`, check whether the
#'   display label already occurs in a grouping column.
#' @param .duplicates One of `"error"`, `"drop"`, or `"keep"`, controlling
#'   duplicate grouping sets after expansion.
#' @param .sort A logical scalar. If `TRUE`, sort by `.by` followed by grouping
#'   dimensions. It defaults to `TRUE` for local data frames and `FALSE` for
#'   lazy tables. With `FALSE`, local groups retain first-appearance order;
#'   lazy result order is unspecified unless an explicit order is requested.
#'
#' @return An ungrouped data frame, or a lazy table when `.data` is lazy.
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
#' [nest_with_margins()] always return ungrouped results. For
#' [summarize_with_margins()], this is why `.groups` only accepts `NULL` and
#' `"drop"`. Arbitrary grouping sets contain multiple grains, so there is no
#' single meaningful `drop_last`, `"keep"`, or `"rowwise"` structure.
#' [nest_by_with_margins()] instead returns a row-wise data frame grouped by
#' all visible fixed keys and grouping dimensions. Row-wise input is rejected;
#' call [dplyr::ungroup()] first.
#'
#' @section Relationship to dplyr summaries:
#' The `...` expressions use [dplyr::summarize()] data-masking semantics.
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
#' [grouping_bit()] and [grouping_id()] to identify margin levels.
#'
#' @section Display labels and grouping identity:
#' `.margin_label` is a display value, not the identity of a grouping set.
#' When the source data can contain the same value, retain [grouping_bit()] or
#' [grouping_id()] in the result. The eager default
#' `.check_margin_label = TRUE` detects collisions for local data. Lazy tables
#' default to `FALSE` because checking would execute an extra query; opt in
#' when the additional scan is appropriate.
#'
#' @section Backend extension design:
#' Unlike [dplyr::summarize()], the public margin verbs are intentionally not
#' S3 generics. They first compile and validate one backend-independent
#' grouping plan so duplicate handling, labels, grouping identifiers, and
#' output grouping cannot drift between methods. Backend-specific schema and
#' execution capabilities are isolated behind a non-exported adapter layer.
#' That adapter is an implementation detail rather than an extension API;
#' support for a new backend should be added to marginplyr itself.
#'
#' @section Database backend coverage:
#' DuckDB and PostgreSQL use native `GROUP BY GROUPING SETS` SQL. Automated
#' tests execute DuckDB queries against a live in-memory database and verify
#' PostgreSQL SQL with dbplyr's simulator.
#'
#' The portable `UNION ALL` SQL path is verified with dbplyr simulators for
#' Access, SAP HANA, Hive, Impala, MariaDB, Microsoft SQL Server, MySQL,
#' Oracle, Amazon Redshift, Snowflake, Spark SQL, SQLite, and Teradata, plus
#' generic DBI and ODBC connections. Simulator coverage verifies SQL
#' generation, not execution against every database server.
#'
#' Arrow and dtplyr are also tested lazy backends, but they are not SQL
#' database connections.
#'
#' @family summarize and expand data with margins
#' @export
#' @examples
#' # Build one monthly management report with store detail, region subtotals,
#' # and a company total.
#' summarize_with_margins(
#'   retail_sales,
#'   units = sum(units),
#'   revenue = sum(revenue),
#'   .by = c(year, month),
#'   .grouping = rollup(region, store)
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
#'   retail_sales,
#'   revenue = sum(revenue),
#'   level = grouping_id(year, month, region, store),
#'   .grouping = rollup(year, month, region, store)
#' )
#'
#' # Grouping expressions are created with mutate() before summarizing.
#' summarize_with_margins(
#'   dplyr::mutate(
#'     retail_sales,
#'     period = paste(year, month, sep = "-")
#'   ),
#'   revenue = sum(revenue),
#'   .grouping = rollup(period, region)
#' )
#'
#' # Change the display label, or use NULL to retain the input column types.
#' summarize_with_margins(
#'   retail_sales,
#'   revenue = sum(revenue),
#'   .grouping = rollup(year),
#'   .margin_label = "All years"
#' )
#' summarize_with_margins(
#'   retail_sales,
#'   revenue = sum(revenue),
#'   year_is_total = grouping_bit(year),
#'   .grouping = rollup(year),
#'   .margin_label = NULL
#' )
#'
#' # across() and pick() treat every fixed key and margin dimension as a
#' # grouping column, including dimensions omitted from a subtotal branch.
#' summarize_with_margins(
#'   retail_sales,
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
#'   .groups = "drop",
#'   .sort = FALSE
#' )
#'
#' # DuckDB executes a native GROUP BY GROUPING SETS query. The optional
#' # dependency guard keeps this example runnable without DuckDB installed.
#' if (
#'   requireNamespace("DBI", quietly = TRUE) &&
#'   requireNamespace("duckdb", quietly = TRUE)
#' ) {
#'   con <- suppressMessages(DBI::dbConnect(duckdb::duckdb()))
#'
#'   sales_db <- dplyr::copy_to(
#'     con,
#'     retail_sales,
#'     name = "retail_sales",
#'     temporary = TRUE,
#'     overwrite = TRUE
#'   )
#'   query <- summarize_with_margins(
#'     sales_db,
#'     revenue = sum(revenue, na.rm = TRUE),
#'     level = grouping_id(region, store),
#'     .by = c(year, month),
#'     .grouping = rollup(region, store),
#'     .sort = TRUE
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
                                   .groups = NULL,
                                   .margin_label = "Total",
                                   .check_margin_label = is.data.frame(.data),
                                   .duplicates = c("error", "drop", "keep"),
                                   .sort = is.data.frame(.data)) {
  assert_lazy_table(.data)
  if (!is.null(.groups) && !identical(.groups, "drop")) {
    stop(
      "`summarize_with_margins()` only supports `.groups = \"drop\"` ",
      "or `NULL`.",
      call. = FALSE
    )
  }
  assert_logical_scalar(.check_margin_label)
  assert_logical_scalar(.sort)
  .margin_label <- normalize_margin_label(.margin_label)
  .duplicates <- match.arg(.duplicates)

  dots <- rlang::enquos(...)
  check_summary_context_helpers(dots)
  grouping_quo <- rlang::enquo(.grouping)
  by_quo <- rlang::enquo(.by)
  grouping_spec <- rlang::eval_tidy(grouping_quo)

  input <- prepare_margin_input(.data, by_quo)
  .data <- input$data
  by <- input$by
  backend <- grouping_backend(.data)
  data_vars <- get_col_names(.data, dplyr::everything())
  data_proxy <- grouping_selection_proxy(.data, backend = backend)
  plan <- compile_grouping_spec(
    grouping_spec,
    data_vars = data_vars,
    data_proxy = data_proxy,
    .by = by,
    .duplicates = .duplicates
  )
  dots <- resolve_summary_selections(
    dots,
    data_proxy = data_proxy,
    data_vars = data_vars,
    group_vars = c(plan$by, plan$dimensions),
    normalize_across_names = identical(backend$kind, "dtplyr")
  )
  summary_selection_proxy <- dplyr::select(
    data_proxy,
    dplyr::all_of(setdiff(
      data_vars,
      unique(c(plan$by, plan$dimensions))
    ))
  )
  summary_output_names <- unique(c(
    names(dots)[nzchar(names(dots))],
    known_summary_output_names(dots, summary_selection_proxy)
  ))
  check_summary_group_overwrite(
    summary_output_names,
    group_vars = c(plan$by, plan$dimensions)
  )
  reserved_names <- unique(c(data_vars, summary_output_names))

  column_info <- margin_column_info(
    data_proxy,
    plan$dimensions,
    backend = backend
  )
  validate_margin_label(
    .data,
    dimensions = plan$dimensions,
    .margin_label = .margin_label,
    .check_margin_label = .check_margin_label,
    column_info = column_info,
    backend = backend
  )

  result <- if (supports_grouping_sets(.data, plan, backend = backend)) {
    summarize_grouping_sets(
      .data,
      dots = dots,
      plan = plan,
      .margin_label = .margin_label,
      reserved_names = reserved_names
    )
  } else {
    summarize_margin_union(
      .data,
      dots = dots,
      plan = plan,
      .margin_label = .margin_label,
      column_info = column_info,
      reserved_names = reserved_names
    )
  }

  finish_margin_result(
    result,
    plan = plan,
    factor_info = column_info$factors,
    .margin_label = .margin_label,
    .sort = .sort
  )
}

#' @rdname summarize_with_margins
#' @export
summarise_with_margins <- summarize_with_margins

summarize_impl <- function(.data,
                           ...,
                           .by) {
  dplyr::summarize(
    .data = .data,
    ...,
    .by = dplyr::all_of(.by)
  )
}

assert_margin_name <- function(data, col_names, margin_name) {
  assert_string_scalar(margin_name)
  stopifnot(is.character(col_names), !anyNA(col_names))

  data <- dplyr::select(.data = data, dplyr::all_of(col_names))
  checks <- lapply(
    col_names,
    function(col) {
      column <- rlang::sym(col)
      if (is.na(margin_name)) {
        rlang::expr(any(is.na(!!column), na.rm = TRUE))
      } else {
        rlang::expr(
          any(!!column == !!margin_name, na.rm = TRUE)
        )
      }
    }
  )
  names(checks) <- col_names
  found <- dplyr::collect(dplyr::summarize(data, !!!checks))
  found <- vapply(
    col_names,
    function(col) {
      nrow(found) > 0L && isTRUE(found[[col]][[1L]])
    },
    logical(1)
  )

  if (!any(found)) {
    return(invisible(NULL))
  }

  bad_cols <- paste0("`", names(found)[found], "`", collapse = ", ")
  label <- if (is.na(margin_name)) "NA" else paste0('"', margin_name, '"')
  stop(
    label,
    " is already present in grouping column",
    if (sum(found) == 1L) " " else "s ",
    bad_cols,
    ". Choose another `.margin_label` or set ",
    "`.check_margin_label = FALSE`.",
    call. = FALSE
  )
}

get_col_names <- function(data, ...) {
  selected <- dplyr::select(.data = data, ...)
  # Drop the grouping metadata attached to dplyr's variable-name vector.
  as.character(dplyr::tbl_vars(selected))
}
