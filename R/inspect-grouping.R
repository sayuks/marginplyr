#' Inspect a backend-independent Grouping plan
#'
#' `inspect_grouping()` returns the Grouping plan resolved from `.data`,
#' `.by`, and `.grouping` without executing a Margin operation. A Grouping plan
#' is the ordered list of grouping sets that the Grouping specification written
#' in `.grouping` expands to, and this function returns one row per
#' grouping-set occurrence in it. The result is
#' always a local, ungrouped, ordinary tibble, including for lazy inputs.
#'
#' @param .data A data frame or lazy table.
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns retained in
#'   every grouping set. For grouped input, the existing grouping columns are
#'   used and `.by` must be `NULL`. A fixed key is a column of the input, so a
#'   selection cannot rename it: `c(area = region)` is an error rather than a
#'   fixed key named `area`. Rename the result afterwards with
#'   [dplyr::rename()].
#' @param .grouping A Grouping specification created by [grouping_set()],
#'   [grouping_sets()], [rollup()], [cube()], or [grouping_spec()]. `NULL`
#'   represents the empty grouping set. Any expression returning a
#'   specification can be written here, while a specification nested inside
#'   another must be a constructor call or a name bound to one, and a nested
#'   name the input also has a column for is refused rather than read either
#'   way; see [grouping_set()]. The columns it selects are the plan's Margin
#'   dimensions, which a grouping set can omit; `.by` columns are fixed keys.
#' @param .duplicates One of `"error"`, `"drop"`, or `"keep"`, and nothing
#'   else; see *[Option arguments][margin_verbs]* in [margin_verbs]. A
#'   duplicate is a grouping set whose columns are the same as an earlier
#'   set's. `"error"` (the default) refuses the call and names the
#'   positions. `"drop"` keeps the first occurrence of each set and numbers
#'   `set_id` over the rows that remain. `"keep"` lists every occurrence,
#'   each with its own `set_id`.
#' @param .format `"text"` for compact display values or `"list"` for exact
#'   character and integer vectors, and nothing else; see
#'   *[Option arguments][margin_verbs]* in [margin_verbs].
#'   Text values are display-only; use `"list"` when column names contain
#'   separators or other non-syntactic characters.
#'
#' @return A local, ungrouped tibble with one row per grouping-set occurrence
#'   that remains after `.duplicates`, in Grouping-plan order;
#'   `.grouping = NULL` gives one row. Its columns are:
#'   - `set_id`: the one-based Grouping set identifier, an integer.
#'   - `fixed`: the fixed keys, from `.by` or the input's groups, the same in
#'     every row.
#'   - `included`: the Margin dimensions the set keeps.
#'   - `omitted`: the Margin dimensions the set leaves out, which hold the
#'     Margin label in a Margin verb's rows for it.
#'   - `grouping_bits`: `0L` for each included and `1L` for each omitted
#'     Margin dimension.
#'   - `grouping_id`: those bits encoded as an integer bit mask.
#'
#'   With `.format = "text"`, `fixed`, `included`, `omitted`, and
#'   `grouping_bits` are character. With `.format = "list"`, the first three
#'   are lists of character vectors and `grouping_bits` is a list of named
#'   integer vectors.
#'
#' @section Grouping identity:
#' `set_id` is the one-based Grouping set identifier a Margin verb's
#' [`.id`][summarize_with_margins] column holds for the same resolved plan.
#' `grouping_bits` instead describes whether each Margin dimension is
#' included (`0L`) or omitted (`1L`), and `grouping_id` encodes
#' that absence pattern as a bit mask. `grouping_id` is `NA_integer_` beyond
#' 31 Margin dimensions, while `grouping_bits` remains complete.
#'
#' [grouping_bit()] is the canonical reference for how these four values
#' relate and which of them distinguish duplicate grouping-set occurrences.
#'
#' @section Formats and ordinary tibble behavior:
#' The default `.format = "text"` represents column collections as `()`,
#' `(region)`, or `(region, store)`, and Grouping bits as text such as
#' `region=0, store=1`. This compact display is the default because IDE table
#' viewers, including Positron, commonly hide list-column contents.
#'
#' `.format = "list"` returns exact character vectors for `fixed`, `included`,
#' and `omitted`, plus named integer vectors for `grouping_bits`. Use it for
#' programmatic inspection and for names containing separators. The text
#' format is display-only and is not a serialization format.
#'
#' The result has only the ordinary `tbl_df`, `tbl`, and `data.frame` classes,
#' has no custom printer, and works with normal dplyr, export, and
#' snapshot-testing workflows. Its rows are always in Grouping-plan order.
#' That inspection guarantee does not imply physical result order for a Margin
#' operation.
#'
#' @section Lazy inputs:
#' Inspecting a lazy `.data` reads none of its rows and returns a local
#' tibble. Selecting columns by name sends no query. A type predicate such as
#' `where(is.character)` needs column types: on DuckDB, inspection sends
#' metadata queries that read no rows, a system-catalog check and a zero-row
#' read; Arrow and dtplyr inputs answer from their schema; other SQL
#' connections refuse the predicate.
#' *[Fixed columns and Margin dimensions][margin_verbs]* in [margin_verbs]
#' covers predicates by backend and those that depend on values, and
#' [last_sent_queries()] reads back the queries an inspection sent.
#'
#' Where column types cannot be established without reading rows, an
#' inspection that needs them is refused before they are requested:
#' - A derived dtplyr step whose types depend on processing source rows. Call
#'   [dplyr::collect()] on it explicitly and pass the resulting data frame.
#' - A DuckDB CSV reader that uses auto-detection, including filename
#'   replacement scans such as `FROM 'path.csv'`, because binding even a
#'   zero-row query can sample CSV values. Supply an explicit CSV schema
#'   with `auto_detect = false`, or collect explicitly first.
#'
#' Inspection also accepts:
#' - every tabular Arrow input, including a `RecordBatchReader`, which
#'   inspection does not consume. Non-tabular Arrow objects are refused.
#' - a dtplyr step with an input rooted at
#'   `dtplyr::lazy_dt(immutable = FALSE)`. Because inspection builds no
#'   grouping-set branch, it leaves every caller `data.table` unchanged.
#'   Margin verbs refuse such a step before building a branch, because
#'   data.table may write to the caller's table by reference in any branch;
#'   rebuild it with `immutable = TRUE` before passing it to one of them.
#'
#' The Grouping plan is not a SQL execution plan. Use [dplyr::show_query()]
#' for generated SQL and backend-native tools for an optimizer plan.
#'
#' *Inspect a Grouping plan as data* in [Grouping identity][guide] walks through
#' inspecting a plan and reading it back off a Margin result. *Show only the
#' subtotals, only the detail, or one level* in [Recipes for common reporting
#' tasks][recipes] uses a plan to choose which grouping set to keep, and *Add a
#' level column: detail, subtotal or total* in [Recipes for common reporting
#' tasks][level] joins one
#' against a Margin result's [`.id`][summarize_with_margins] column to name the
#' set behind every row. Because the plan is local, that join needs a `copy`
#' argument when the Margin result is lazy, and it does not preserve a Margin
#' order.
#'
#' [guide]: https://sayuks.github.io/marginplyr/vignettes/grouping_identity.html#inspect-a-grouping-plan-as-data
#' [recipes]: https://sayuks.github.io/marginplyr/vignettes/recipes.html#show-only-the-subtotals-only-the-detail-or-one-level
#' [level]: https://sayuks.github.io/marginplyr/vignettes/recipes.html#add-a-level-column-detail-subtotal-or-total
#'
#' @family grouping plans and grouping identity
#' @seealso [summarize_with_margins()] to run a Margin operation on the
#'   inspected plan.
#' @export
#' @examples
#' # The data comes first; the specification goes in `.grouping`.
#' inspect_grouping(
#'   .data = retail_sales,
#'   .by = c(year, month),
#'   .grouping = rollup(region, store)
#' )
#'
#' inspect_grouping(
#'   .data = retail_sales,
#'   .grouping = cube(product, channel),
#'   .format = "list"
#' )
#'
#' # A composite dimension adds or removes region and store together.
#' inspect_grouping(
#'   .data = retail_sales,
#'   .grouping = cube(grouping_set(region, store), channel)
#' )
#'
#' inspect_grouping(
#'   .data = retail_sales,
#'   .grouping = grouping_sets(
#'     grouping_set(region, store),
#'     grouping_set(region),
#'     grouping_set()
#'   )
#' )
#'
#' # Existing groups become fixed keys when `.by` is not supplied.
#' inspect_grouping(
#'   .data = dplyr::group_by(retail_sales, year, month),
#'   .grouping = rollup(region, store)
#' )
#'
#' # Duplicate occurrences error by default, or can be dropped or retained.
#' duplicate_specification <- grouping_sets(
#'   rollup(region, store),
#'   grouping_set()
#' )
#' try(inspect_grouping(
#'   .data = retail_sales,
#'   .grouping = duplicate_specification
#' ))
#' inspect_grouping(
#'   .data = retail_sales,
#'   .grouping = duplicate_specification,
#'   .duplicates = "drop"
#' )
#' inspect_grouping(
#'   .data = retail_sales,
#'   .grouping = duplicate_specification,
#'   .duplicates = "keep"
#' )
inspect_grouping <- function(.data,
                             .by = NULL,
                             .grouping = NULL,
                             .duplicates = c("error", "drop", "keep"),
                             .format = c("text", "list")) {
  force(.data)
  suspendInterrupts({
    # The scope is read from this live frame by active_sent_query_scope().
    sent_query_scope <- reset_sent_queries() # nolint: object_usage_linter.
  })
  call <- rlang::current_call()
  grouping_quo <- capture_public_grouping(
    rlang::enquo0(.grouping), substitute(.grouping)
  )
  by_quo <- rlang::enquo(.by)

  with_margin_error_call(
    {
      assert_margin_input(.data)
      .format <- match_margin_choice(
        .format,
        choices = grouping_format_choices,
        arg_name = ".format"
      )
      .duplicates <- match_margin_choice(
        .duplicates,
        choices = margin_duplicates_choices,
        arg_name = ".duplicates"
      )
      # Arrow can reject the schema during selection, so validate the Option
      # arguments before wrapping the reader. This consumes no batch
      # (ADR 0020).
      if (inherits(.data, "RecordBatchReader")) {
        .data <- dplyr::select(.data, dplyr::everything())
      }
      grouping <- prepare_grouping_plan(
        .data,
        by_quo = by_quo,
        grouping_quo = grouping_quo,
        .duplicates = .duplicates,
        duplicates_choices = margin_duplicates_choices,
        builds_margin_branches = FALSE,
        call = call
      )
      format_grouping_plan(grouping$plan, format = .format)
    },
    call = call
  )
}

# `inspect_grouping()`'s own option vocabulary; see the note in
# R/margin-operation.R on why the formal still spells it out.
grouping_format_choices <- c("text", "list")

format_grouping_plan <- function(plan, format) {
  stopifnot(inherits(plan, "margin_grouping_plan"))
  stopifnot(format %in% grouping_format_choices)

  included <- lapply(
    plan$sets,
    function(set) plan$dimensions[plan$dimensions %in% set]
  )
  omitted <- lapply(
    plan$sets,
    function(set) plan$dimensions[!plan$dimensions %in% set]
  )
  fixed <- rep(list(plan$by), length(plan$sets))
  grouping_bits <- lapply(
    seq_len(nrow(plan$grouping_masks)),
    function(i) {
      if (length(plan$dimensions) == 0L) {
        return(integer())
      }
      stats::setNames(
        as.integer(plan$grouping_masks[i, ]),
        plan$dimensions
      )
    }
  )

  if (identical(format, "text")) {
    fixed <- vapply(fixed, format_grouping_columns, character(1))
    included <- vapply(included, format_grouping_columns, character(1))
    omitted <- vapply(omitted, format_grouping_columns, character(1))
    grouping_bits <- vapply(
      grouping_bits,
      format_grouping_bits,
      character(1)
    )
  }

  dplyr::tibble(
    set_id = as.integer(plan$set_ids),
    fixed = fixed,
    included = included,
    omitted = omitted,
    grouping_bits = grouping_bits,
    grouping_id = grouping_plan_ids(plan$grouping_masks)
  )
}

format_grouping_columns <- function(columns) {
  paste0("(", paste(columns, collapse = ", "), ")")
}

format_grouping_bits <- function(bits) {
  if (length(bits) == 0L) {
    return("()")
  }
  paste0(names(bits), "=", bits, collapse = ", ")
}

grouping_plan_ids <- function(masks) {
  dimensions <- ncol(masks)
  if (dimensions > 31L) {
    return(rep(NA_integer_, nrow(masks)))
  }
  if (dimensions == 0L) {
    return(rep(0L, nrow(masks)))
  }

  weights <- 2 ^ rev(seq.int(0L, dimensions - 1L))
  as.integer(drop(masks %*% weights))
}
