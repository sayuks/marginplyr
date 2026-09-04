#' Nest data with SQL-style grouping margins
#'
#' [nest_with_margins()] creates one nested data frame for every group in every
#' grouping set. It works with local data frames and `dtplyr` steps.
#'
#' @inheritParams summarize_with_margins
#' @inheritSection summarize_with_margins Fixed columns and grouping dimensions
#' @inheritSection summarize_with_margins Grouped and row-wise inputs
#' @inheritSection summarize_with_margins Option arguments
#' @inheritSection summarize_with_margins Result class and attributes
#' @inheritSection summarize_with_margins Grouping set identifiers
#' @inheritSection summarize_with_margins Margin order
#' @inheritSection summarize_with_margins Display labels and grouping identity
#' @inheritSection summarize_with_margins When marginplyr queries your data
#' @inheritSection summarize_with_margins Backend extension design
#' @param .data A local data frame or a `dtplyr` step. Other lazy tables are
#'   not supported because nesting creates list columns.
#' @param .key A string naming the list column. As in [tidyr::nest()],
#'   `NULL` uses `"data"`.
#' @param .keep Should fixed `.by` columns and grouping dimensions also be kept
#'   inside each nested data frame? If `TRUE`, the nested columns contain their
#'   original, pre-margin values rather than `.margin_label`.
#' @param .duplicates `"error"` or `"drop"`, and nothing else; see *Option
#'   arguments*. Nesting does not support the `"keep"` policy available in
#'   [summarize_with_margins()] and [expand_with_margins()].
#'
#' @section Relationship to tidyr and dplyr:
#' These functions are margin-aware counterparts, not drop-in replacements,
#' and do not implement every upstream feature. [nest_with_margins()] resembles
#' [tidyr::nest()] with `.by`: it supports `.key`, returns an ungrouped data
#' frame, and puts all non-key columns into one list column. Setting
#' `.keep = TRUE` also puts the original grouping keys in that inner data
#' frame. This corresponds to selecting those keys in [tidyr::nest()] (for
#' example, `nest(data = everything(), .by = region)`), but uses a logical
#' argument because margin dimensions must remain visible outside to identify
#' detail, subtotal, and total rows.
#'
#' [nest_with_margins()] does not implement the full `...` column
#' specification, multiple list columns, or `.names_sep`. Existing grouping
#' columns become implicit fixed keys, as they do for [tidyr::nest()], but
#' [nest_with_margins()] returns an ungrouped result instead of preserving the
#' input grouping.
#'
#' [nest_by_with_margins()] resembles [dplyr::nest_by()] and, like it, provides
#' `.key` and `.keep`, but selects fixed keys with `.by` rather than `...`.
#' With `.keep = TRUE`, the nested data contains the original, pre-margin
#' values of every outer key. Thus an outer `East / Total` subtotal retains
#' the original store values inside the nested data instead of replacing them
#' with `"Total"`. This is the closest margin-aware analogue of dplyr's rule
#' that `.keep` retains grouping columns.
#'
#' The list column is a regular list of data frames; its exact `vctrs_list_of`
#' subclass is not part of the API. Neither is the class of its elements,
#' which follows what the backend made of the cell expression and is tibbles
#' on both today. Only their being data frames holding the input's non-key
#' columns is promised, so `lapply(result$data, tibble::as_tibble)` is what a
#' caller who needs the class itself writes. [nest_with_margins()] follows
#' [tidyr::nest()] for an empty ungrouped input and returns zero outer rows.
#' [nest_by_with_margins()] follows [dplyr::nest_by()] and returns one row
#' containing the empty input when there are no grouping keys.
#'
#' When nesting leaves no payload column — every input column is a fixed key or
#' a grouping dimension, and `.keep` does not put them back — each nested data
#' frame still has one row per source row it stands for, as [dplyr::nest_by()]
#' does. That row count is promised; the class of such a cell is described
#' rather than promised, as every element class is.
#'
#' A `data.table` cannot hold rows without columns at all, which is a limit on
#' what an input can carry into `dtplyr` rather than anything nesting does: a
#' data frame with rows and no columns loses its rows on the way in, so
#' `dtplyr::lazy_dt(data.frame(row.names = 1:3))` is already empty before
#' marginplyr reads it and no behavior here can restore the three rows. Nest
#' an input that has rows and no columns locally when its row count matters.
#' This is the whole of the difference: a column-less input that reaches the
#' backend with the rows it had -- one with no rows either -- nests to the
#' same result on both backends.
#'
#' No input column name is reserved for internal bookkeeping. Temporary
#' grouping-set and `.keep` columns are generated collision-free and removed
#' before the result is returned.
#'
#' @return For a local input, an ungrouped data frame with one list column,
#'   whose class and attributes follow [dplyr::summarize()]; see *Result class
#'   and attributes*. A `dtplyr` input returns a lazy `dtplyr` step until
#'   collected. Result row order is unspecified unless `.sort` asks for a
#'   Margin order; see *Margin order*, or use [dplyr::arrange()] for any other
#'   presentation order.
#' @family summarize and expand data with margins
#' @export
#' @examples
#' # Keep the source rows behind each store, region subtotal, and company
#' # total so they can feed separate report sections.
#' january_sales <- dplyr::filter(
#'   retail_sales,
#'   year == 2026L,
#'   month == "Jan"
#' )
#' nested <- nest_with_margins(
#'   .data = january_sales,
#'   .grouping = rollup(region, store)
#' )
#' # `january_sales` is a plain data frame, so nesting it returns one too, and
#' # `nested` prints its list column as flattened values. A tibble prints each
#' # nested table as its dimensions instead.
#' nested |>
#'   dplyr::as_tibble() |>
#'   head()
#' nested$data[[1]]
#'
#' # Keep original region and store values inside each nested table as well.
#' # The outer columns still identify the margin level.
#' nested_with_keys <- nest_with_margins(
#'   .data = january_sales,
#'   .grouping = rollup(region, store),
#'   .keep = TRUE
#' )
#' names(nested_with_keys$data[[1]])
#' total <- dplyr::filter(
#'   nested_with_keys,
#'   region == "Total",
#'   store == "Total"
#' )
#' unique(total$data[[1]][c("region", "store")])
#'
#' # NULL uses the same default list-column name as tidyr::nest().
#' names(nest_with_margins(
#'   .data = january_sales,
#'   .by = region,
#'   .key = NULL
#' ))
#'
#' # Existing groups become fixed outer keys, while the result itself is
#' # ungrouped.
#' grouped_nested <- retail_sales |>
#'   dplyr::group_by(year) |>
#'   nest_with_margins(
#'     .grouping = rollup(region, store)
#'   )
#' dplyr::group_vars(grouped_nested)
#'
#' # The same operation stays lazy for a dtplyr input until collect(). The
#' # guard shipped with marginplyr reports dtplyr usable only at the version
#' # DESCRIPTION requires, so an older one withholds this rather than failing
#' # inside it.
#' source(system.file("suggests", "guard.R", package = "marginplyr"))
#' if (marginplyr_suggest_available("dtplyr")) {
#'   nested_dt <- january_sales |>
#'     dtplyr::lazy_dt() |>
#'     nest_with_margins(
#'       .grouping = rollup(region, store)
#'     )
#'   print(dplyr::show_query(nested_dt))
#'   dplyr::collect(nested_dt)
#' }
nest_with_margins <- function(.data,
                              .by = NULL,
                              .grouping = NULL,
                              .margin_label = "Total",
                              .margin_label_position = c("last", "first"),
                              .check_margin_label = is.data.frame(.data),
                              .duplicates = c("error", "drop"),
                              .id = NULL,
                              .sort = c("none", "last", "first"),
                              .key = "data",
                              .keep = FALSE) {
  reset_sent_queries()
  call <- rlang::current_call()
  if (is.null(.key)) {
    .key <- "data"
  }
  grouping_quo <- rlang::enquo(.grouping)
  by_quo <- rlang::enquo(.by)

  nest_margin_pipeline(
    .data = .data,
    by_quo = by_quo,
    grouping_quo = grouping_quo,
    .margin_label = .margin_label,
    .margin_label_position = .margin_label_position,
    .check_margin_label = .check_margin_label,
    .duplicates = .duplicates,
    .sort = .sort,
    .id = .id,
    .key = .key,
    .keep = .keep,
    call = call
  )
}

# The nesting verbs narrow the Margin `.duplicates` vocabulary: nested
# duplicate sets would share indistinguishable outer keys, so `"keep"` is not
# offered. See the note in R/margin-operation.R on why the formals still spell
# it out.
nest_duplicates_choices <- c("error", "drop")

nest_margin_pipeline <- function(.data,
                                 by_quo,
                                 grouping_quo,
                                 .margin_label,
                                 .margin_label_position,
                                 .check_margin_label,
                                 .duplicates,
                                 .sort,
                                 .id,
                                 .key,
                                 .keep,
                                 call) {
  stopifnot(rlang::is_quosure(by_quo), rlang::is_quosure(grouping_quo))

  with_margin_error_call(
    {
      # General admission first, then the narrower constraint: an input dplyr
      # cannot group is not a nesting problem, and reporting it as one would
      # answer a caller who supplied a matrix with the classes that nest.
      assert_margin_input(.data)
      assert_nest_possible(.data)
      assert_logical_scalar(.keep)
      assert_string_scalar(.key)
      if (is.na(.key)) {
        abort_marginplyr("{.arg .key} must not be missing.")
      }
      if (!nzchar(.key)) {
        abort_marginplyr("{.arg .key} must not be empty.")
      }
      options <- normalize_margin_options(
        .margin_label = .margin_label,
        .margin_label_position = .margin_label_position,
        .check_margin_label = .check_margin_label,
        .duplicates = .duplicates,
        .sort = .sort,
        duplicates_choices = nest_duplicates_choices,
        .id = .id
      )
      set_id_name <- options$set_id_name
      .margin_label <- options$margin_label
      .margin_label_position <- options$margin_label_position
      .check_margin_label <- options$check_margin_label
      .duplicates <- options$duplicates
      .sort <- options$sort
      check_margin_id_collision(set_id_name, .key, "nesting `.key`")
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
    duplicates_choices = nest_duplicates_choices,
    # Nesting folds all but the grouping columns into `.key`, so every input
    # column reaches the result -- as a column of its own, or inside a cell
    # (#421). `execute_margin_nest()` rebuilds the second kind before folding
    # it, which is the half of the route that lets this name them all.
    carried_columns = function(data_vars, plan) data_vars,
    .id = .id,
    call = call
  )
  execution <- execute_margin_nest(
    operation,
    .key = .key,
    .keep = .keep
  )
  finalize_margin_operation(operation, execution)
}

execute_margin_nest <- function(operation, .key, .keep) {
  check_margin_operation(operation)
  with_margin_error_call(
    {
      plan <- operation$plan
      group_cols <- c(plan$by, plan$dimensions)
      if (.key %in% group_cols) {
        # `{(.key)}` rather than `{.key}`: cli reads a `{}` expression opening
        # with a dot as one of its own styles, and refuses the literal outright
        # from 3.4.0 -- the floor DESCRIPTION states. The parentheses are the
        # spelling cli's own diagnostic names, and every argument this package
        # refuses is spelled with a leading dot.
        abort_marginplyr(paste0(
          "{.arg .key} ({.var {(.key)}}) must not be a ",
          "grouping column."
        ))
      }

      internal_names <- new_margin_internal_names(
        as.integer(is.null(operation$set_id_name)) +
          if (.keep) length(group_cols) else 0L,
        used_names = unique(c(
          operation$data_vars,
          operation$set_id_name,
          .key
        )),
        prefix = "..marginplyr_nest_"
      )
      set_col <- if (is.null(operation$set_id_name)) {
        internal_names[[1L]]
      } else {
        operation$set_id_name
      }
      keep_cols <- if (.keep && length(group_cols) > 0L) {
        stats::setNames(
          utils::tail(internal_names, length(group_cols)),
          group_cols
        )
      } else {
        character()
      }
      data <- operation$data
      column_info <- operation$column_info
      if (length(keep_cols) > 0L) {
        keep_exprs <- lapply(group_cols, margin_column_pronoun)
        names(keep_exprs) <- unname(keep_cols)
        data <- dplyr::mutate(data, !!!keep_exprs)
        # A `.keep` copy is made here, after `prepare_margin_operation()` read
        # the input's schema, so no `factor_info` entry names it and the encode
        # arm would not see it. The copy holds the source column's values, so
        # the source column's entry describes it under the internal name. It
        # carries no Margin label, which is what puts it on the same route as a
        # payload column: rebuilt on its declared levels with none appended.
        column_info$factors <- c(
          column_info$factors,
          lapply(
            Filter(
              function(info) info$col %in% names(keep_cols),
              column_info$factors
            ),
            function(info) {
              info$col <- unname(keep_cols[[info$col]])
              info
            }
          )
        )
      }

      validate_margin_operation(operation)

      expanded <- expand_margin_union(
        data,
        plan = plan,
        margin_labels = operation$margin_labels,
        column_info = column_info,
        backend = operation$backend,
        set_id_name = set_col
      )

      # Nesting always expands through the portable adapter and already carries
      # a Grouping set identifier, so a Margin order costs it no column of its
      # own; the identifier is retained past the nest and dropped once the
      # finalizer has ordered by it.
      sorting <- margin_sorting(operation)
      # One split, read by both halves: what is folded into a cell is rebuilt
      # here, and what stays a column of its own is what the finalizer is left.
      # Deriving the two separately would let them drift into a finalizer that
      # rebuilds a column the fold has taken away, or skips one it kept.
      folded <- vapply(
        column_info$factors,
        function(info) !(info$col %in% group_cols),
        logical(1)
      )
      # Before the fold, because that is the last point a payload column is a
      # column. The union has already turned the values on a declared NA level
      # into missing, so rebuilding inside the cell would restore the level
      # with nothing left on it (#421).
      expanded <- restore_margin_factors(
        expanded,
        factor_info = column_info$factors[folded],
        margin_labels = operation$margin_labels,
        position = operation$margin_label_position
      )
      new_margin_execution(
        nest_expanded_margins(
          expanded,
          group_cols = group_cols,
          set_col = set_col,
          keep_cols = keep_cols,
          .key = .key,
          drop_set_col = is.null(operation$set_id_name) && !sorting
        ),
        sort_id = if (sorting) set_col else NULL,
        factor_info = column_info$factors[!folded]
      )
    },
    call = operation$call
  )
}

# The expression building one cell, given the columns it is to hold: a named
# character vector whose names are the names the cell's columns take and whose
# values are the columns of the expanded step they read. An empty one is a
# nesting that has no payload column left.
nest_cell_expr <- function(cell_cols) {
  if (length(cell_cols) == 0L) {
    # A nesting that removes every payload column still stands for a known
    # number of source rows per cell, and once the columns are gone the count
    # is the only thing left to carry it. `n()` is that count, and dtplyr
    # translates it to `.N`, so one expression serves both backends. The cell
    # is a tibble on either, because a `data.table` cannot hold rows without
    # columns — `dim()` reads its row count from its first column, so a
    # column-less one is always empty.
    quote(dplyr::tibble(.rows = dplyr::n()))
  } else {
    # `list()`, whose only formal is `...`, because dtplyr translates a
    # `pick()` standing where a value stands into a literal `data.table()`
    # call carrying one named argument per column: a column named for one of
    # that function's formals is taken as that argument, so `key` and
    # `check.names` raise and `keep.rownames` and `stringsAsFactors` are
    # absorbed and leave the column out of every cell (#424). The conversion
    # is a step of its own for the same reason, `tibble()` having formals a
    # column could be named for too.
    columns <- rlang::set_names(
      lapply(unname(cell_cols), rlang::sym),
      names(cell_cols)
    )
    rlang::expr(dplyr::as_tibble(list(!!!columns)))
  }
}

nest_expanded_margins <- function(.data,
                                  group_cols,
                                  set_col,
                                  keep_cols,
                                  .key,
                                  drop_set_col = TRUE) {
  outer_cols <- c(group_cols, set_col)
  # `get_col_names()` rather than `colnames()`, which reads `dimnames()` and
  # so answers `NULL` for a `dtplyr` step — every payload column would then
  # look absent and be dropped from every cell.
  cell_cols <- rlang::set_names(
    setdiff(get_col_names(.data, dplyr::everything()), outer_cols)
  )
  if (length(keep_cols) > 0L) {
    # `.keep = TRUE` nests a copy of each grouping column, made upstream under
    # an internal name so that the outer key and the copy can disagree. The
    # cell gives each copy back the name the caller wrote, and the grouping
    # columns lead it, which is what `.keep` promises. `order()` is stable, so
    # the rest keep the order the input gave them.
    restored <- match(unname(cell_cols), unname(keep_cols))
    named <- !is.na(restored)
    names(cell_cols)[named] <- names(keep_cols)[restored[named]]
    leading <- match(
      names(cell_cols),
      group_cols,
      nomatch = length(cell_cols) + 1L
    )
    cell_cols <- cell_cols[order(leading)]
  }

  result <- dplyr::summarize(
    .data,
    "{.key}" := list(!!nest_cell_expr(cell_cols)),
    .by = dplyr::all_of(outer_cols)
  )

  if (drop_set_col) {
    result <- dplyr::select(result, -dplyr::all_of(set_col))
  }
  result
}

utils::globalVariables(":=")
