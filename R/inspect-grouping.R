#' Inspect a backend-independent Grouping plan
#'
#' `inspect_grouping()` returns the Grouping plan resolved from `.data`,
#' `.by`, and `.grouping` without executing a Margin operation. The result is
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
#'   represents the empty grouping set.
#' @param .duplicates One of `"error"`, `"drop"`, or `"keep"`, controlling
#'   duplicate grouping-set occurrences.
#' @param .format `"text"` for compact display values or `"list"` for exact
#'   character and integer vectors. Text values are display-only; use
#'   `"list"` when column names contain separators or other non-syntactic
#'   characters.
#'
#' @return A local, ungrouped tibble with columns `set_id`, `fixed`,
#'   `included`, `omitted`, `grouping_bits`, and `grouping_id`.
#'
#' @section Grouping identity:
#' `set_id` is the one-based Grouping set identifier exposed by `.id` for the
#' same resolved plan. `grouping_bits` instead describes whether each variable
#' dimension is included (`0L`) or omitted (`1L`), and `grouping_id` encodes
#' that absence pattern as a bit mask. `grouping_id` is `NA_integer_` beyond
#' 31 variable dimensions, while `grouping_bits` remains complete.
#'
#' [grouping_bit()] is the canonical reference for how these four values
#' relate and which of them distinguish duplicate Grouping-set occurrences.
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
#' A lazy `.data` still returns a local tibble without reading source rows:
#' inspection uses captured column metadata and the backend-independent
#' Grouping plan. It is deliberately separate from a SQL execution plan. Use
#' [dplyr::show_query()] for generated SQL and backend-native tools for an
#' optimizer plan.
#'
#' The [grouping identity guide][guide] walks through inspecting a plan and
#' reading it back off a Margin result. The [recipes guide][recipes] uses a
#' plan to choose which grouping set to keep, and joins one against `.id` to
#' name the set behind every row. Because the plan is local, that join needs a
#' `copy` argument when the Margin result is lazy, and it does not preserve a
#' Margin order.
#'
#' [guide]: https://sayuks.github.io/marginplyr/vignettes/grouping_identity.html
#' [recipes]: https://sayuks.github.io/marginplyr/vignettes/recipes.html
#'
#' @family grouping plans and grouping identity
#' @seealso [summarize_with_margins()] to run a Margin operation on the
#'   inspected plan.
#' @export
#' @examples
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
  call <- rlang::current_call()
  grouping_quo <- rlang::enquo(.grouping)
  by_quo <- rlang::enquo(.by)

  with_margin_error_call(
    {
      assert_margin_input(.data)
      assert_lazy_table(.data)
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
      grouping <- prepare_grouping_plan(
        .data,
        by_quo = by_quo,
        grouping_quo = grouping_quo,
        .duplicates = .duplicates,
        duplicates_choices = margin_duplicates_choices,
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
