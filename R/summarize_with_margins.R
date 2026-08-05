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
#'   implicit fixed keys.
#' @param .grouping A grouping specification made with [grouping_set()],
#'   [grouping_sets()], [rollup()], [cube()], or [grouping_spec()]. `NULL`
#'   represents one empty grouping set.
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
#' @param .check_margin_label A logical scalar. If `TRUE`, check each Margin
#'   dimension independently for a value or factor level that collides with its
#'   display label. `NULL` bypasses collision checks. See *Display labels and
#'   grouping identity* for the factor missing-value contract. Every Margin
#'   verb uses the same default: `TRUE` for local data frames and `FALSE` for
#'   lazy inputs, where checking would require an additional query.
#' @param .duplicates One of `"error"`, `"drop"`, or `"keep"`, controlling
#'   duplicate grouping sets after expansion.
#' @param .id `NULL`, or one non-missing, non-empty character string naming an
#'   integer output column of one-based Grouping set identifiers. Each value
#'   identifies one occurrence in the resolved Grouping plan. The name must not
#'   collide with source columns, grouping keys, summary outputs, or a nesting
#'   `.key`.
#'
#' @return An ungrouped data frame, or a lazy table when `.data` is lazy. Its
#'   class and attributes follow [dplyr::summarize()]; see *Result class and
#'   attributes*.
#'   Result row order is unspecified; use [dplyr::arrange()] when presentation
#'   order matters.
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
#' dbplyr backends are not executed or probed solely to validate an arbitrary
#' summary result's type or cardinality: statically detectable helper errors
#' remain targeted before execution, while an incompatible lazy expression may
#' instead fail with its database error at [dplyr::collect()].
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
#' preserving ordered status and placing the synthetic level last by default
#' or first when `.margin_label_position = "first"`. With collision checking
#' enabled, the complete factor domain is checked, including unused levels.
#' With checking disabled, an existing level may be reused and is moved to the
#' requested position. Reconstruction preserves the distinction between an
#' observation that uses a factor NA level and an actually missing factor code.
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
#' [grouping_id()] as summaries. The eager default
#' `.check_margin_label = TRUE` detects
#' collisions for local data. Lazy tables default to `FALSE` because checking
#' would execute an extra query; opt in when that scan is appropriate.
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
#' # Ordered factors remain ordered. A disabled collision check can reuse an
#' # unused level and move it to the requested position.
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
#'   .margin_label = "unused",
#'   .margin_label_position = "first",
#'   .check_margin_label = FALSE
#' )
#' is.ordered(priority_result$priority)
#' levels(priority_result$priority)
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
                                   .duplicates = c("error", "drop", "keep"),
                                   .id = NULL) {
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
        .id = .id
      )
      check_removed_groups_argument(dots)
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
    .id = .id,
    validate_grouping = share_grouping_spec_validator(share_kinds),
    call = call
  )
  result <- execute_margin_summary(operation, dots)
  finalize_margin_operation(operation, result)
}

execute_margin_summary <- function(operation, dots) {
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
        return(execute_shares(
          operation,
          staged_result = staged_result,
          requests = summary_plan$requests
        ))
      }
      margin_summary_stage_result(staged_result)
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

  result <- tryCatch(
    {
      if (supports_grouping_sets(
        operation$data,
        plan,
        backend = operation$backend
      ) && !(
        !is.null(set_id_name) &&
          identical(plan$duplicates, "keep")
      )) {
        summarize_margin_native(
          operation$data,
          dots = dots,
          plan = plan,
          margin_labels = operation$margin_labels,
          reserved_names = reserved_names,
          set_id_name = set_id_name
        )
      } else {
        summarize_margin_union(
          operation$data,
          dots = dots,
          plan = plan,
          margin_labels = operation$margin_labels,
          column_info = operation$column_info,
          reserved_names = reserved_names,
          set_id_name = set_id_name
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
  new_margin_summary_stage(result, set_id_name)
}

new_margin_summary_stage <- function(result, set_id_name) {
  structure(
    list(result = result, set_id_name = set_id_name),
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

#' @rdname summarize_with_margins
#' @export
summarise_with_margins <- summarize_with_margins
