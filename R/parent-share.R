#' Calculate a summary's share of its rollup parent
#'
#' `share_of_parent()` is a contextual summary helper for
#' [summarize_with_margins()]. It divides one preceding named numeric scalar
#' summary by the corresponding value in the immediately less detailed
#' [rollup()] level. Fixed `.by` columns partition the calculation.
#'
#' The helper is contextual because its denominator belongs to another
#' Grouping-set row. It can be used only inside
#' [summarize_with_margins()] with one pure [rollup()]. Direct calls, ordinary
#' [dplyr::summarize()] calls, [dplyr::mutate()] calls, [grouping_sets()],
#' [cube()], and [grouping_spec()] are rejected.
#'
#' @section Direct Parent shares:
#' A direct call must be the complete right-hand side of an explicitly named
#' summary. Its argument must be the bare name of one eligible ordinary summary
#' written earlier in the same call:
#'
#' ```
#' # Supported
#' revenue = sum(revenue)
#' revenue_share = share_of_parent(revenue)
#'
#' # Rejected: aggregate or calculated arguments
#' revenue_share = share_of_parent(sum(revenue))
#' revenue_share = share_of_parent(revenue + tax)
#' # Rewrite: define one self-contained ordinary summary first
#' revenue = sum(revenue)
#' revenue_share = share_of_parent(revenue)
#'
#' # Rejected: wrapping or scaling the helper
#' revenue_percent = 100 * share_of_parent(revenue)
#' # Rewrite: derive from the finished Parent share afterwards
#' result |> dplyr::mutate(revenue_percent = 100 * revenue_share)
#' ```
#'
#' A direct call cannot be unnamed, use a string, use a forward reference,
#' redefine the source name, or use a Parent share as another Parent share's
#' source. Each rejected form and its rewrite are listed below.
#'
#' @section Eligible source summaries:
#' The source must be defined exactly once before the Parent share. It must be
#' a top-level named summary or a statically named output from a preceding
#' [dplyr::across()], return one plain integer or double per grouping row, and
#' be self-contained. A source cannot depend on an earlier summary alias.
#'
#' Integer, double, and the corresponding database numeric/decimal scalar
#' summaries are eligible. The Parent share is always a double. Logical,
#' complex, date-time, duration, character, factor, list, data-frame, and other
#' semantic classes are not converted implicitly. Convert intentionally in the
#' ordinary summary:
#'
#' ```
#' # Rejected: a Date summary is not a numeric measure
#' first_date = min(date)
#' date_share = share_of_parent(first_date)
#'
#' # Supported when the conversion has domain meaning
#' first_day = as.double(min(date))
#' day_share = share_of_parent(first_day)
#' ```
#'
#' Scalar means exactly one value per grouping row. Zero-length and multi-value
#' summaries cannot be matched to one parent key. A quantile call with several
#' probabilities is one common multi-value result. Define each statistic as a
#' separate scalar summary column:
#'
#' ```
#' # Rejected
#' revenue_quantile = stats::quantile(revenue, c(0.25, 0.75))
#' quantile_share = share_of_parent(revenue_quantile)
#'
#' # Supported
#' revenue_q25 = stats::quantile(revenue, 0.25)
#' revenue_q75 = stats::quantile(revenue, 0.75)
#' q25_share = share_of_parent(revenue_q25)
#' q75_share = share_of_parent(revenue_q75)
#' ```
#'
#' Forward references, overwritten names, aggregate or calculated arguments,
#' strings, and columns expanded from unnamed data-frame-valued summaries are
#' rejected. Move the source before the helper, define it exactly once, and
#' give data-frame outputs top-level names or create them with a preceding
#' `across()`.
#'
#' An unnamed data-frame-valued summary is an expression such as
#' `tibble::tibble(revenue_total = sum(revenue))` supplied in `...` without a
#' top-level output name. dplyr expands its columns into the result, but their
#' static provenance is not portable enough for a Parent-share dependency:
#'
#' ```
#' # Rejected
#' tibble::tibble(revenue_total = sum(revenue))
#' revenue_share = share_of_parent(revenue_total)
#'
#' # Supported
#' revenue_total = sum(revenue)
#' revenue_share = share_of_parent(revenue_total)
#' ```
#'
#' A source name must be defined exactly once. If a later calculation was
#' intended to refine the earlier value, combine the complete calculation into
#' one ordinary summary:
#'
#' ```
#' # Rejected
#' net = sum(revenue)
#' net = net - sum(discount)
#' net_share = share_of_parent(net)
#'
#' # Supported
#' net = sum(revenue) - sum(discount)
#' net_share = share_of_parent(net)
#' ```
#'
#' A source expression may aggregate source-data columns, but it cannot depend
#' on an earlier summary alias in the same call. Combine that calculation into
#' one self-contained ordinary summary so it remains portable to databases
#' that cannot reuse a `SELECT` alias:
#'
#' ```
#' # Rejected
#' gross = sum(revenue)
#' net = gross - sum(discount)
#' net_share = share_of_parent(net)
#'
#' # Supported
#' net = sum(revenue) - sum(discount)
#' net_share = share_of_parent(net)
#' ```
#'
#' Later independent ordinary summaries are allowed. A later summary cannot
#' use a Parent share in the same call because many database backends cannot
#' reuse aliases in one summary projection. Use a following [dplyr::mutate()]
#' for percentages, rounding, labels, or other derived values.
#'
#' Dependency validation follows the expressions' written order even though
#' the implementation can evaluate ordinary summaries in one internal stage.
#' Final columns retain the user's written expression order, including the
#' expansion position of each `across()` call; internal staging does not
#' reorder the public result.
#'
#' @section Column-wise Parent shares:
#' In `across(.cols, share_of_parent, .names = ...)`, `.cols` sees only
#' preceding eligible ordinary summaries. Source-data columns, fixed `.by`
#' keys, variable grouping dimensions, and earlier Parent shares are outside
#' this selection context. [dplyr::everything()] therefore means every
#' preceding ordinary summary, not every input column.
#'
#' Name-based tidyselect is supported: direct names, ranges, Boolean
#' combinations, positive and negative selections, [dplyr::all_of()],
#' [dplyr::any_of()], [dplyr::everything()], and name-pattern helpers such as
#' [dplyr::starts_with()]. Type/value predicates such as
#' `where(is.numeric)` are rejected because lazy backends do not reliably
#' expose arbitrary summary-result prototypes before execution. Select names
#' explicitly instead:
#'
#' ```
#' # Rejected
#' across(where(is.numeric), share_of_parent, .names = "{.col}_share")
#'
#' # Supported
#' across(c(revenue, units), share_of_parent, .names = "{.col}_share")
#' ```
#'
#' `.fns` must be the direct bare helper `share_of_parent` or the explicit
#' `marginplyr::share_of_parent`. Formulas, anonymous functions, and function
#' lists are rejected, even if they contain only this helper. Use two ordered
#' `across()` expressions so the dependency is explicit:
#'
#' ```
#' # Rejected
#' across(
#'   c(units, revenue),
#'   list(total = sum, share = share_of_parent)
#' )
#'
#' # Supported
#' across(c(units, revenue), sum)
#' across(
#'   c(units, revenue),
#'   share_of_parent,
#'   .names = "{.col}_share"
#' )
#' ```
#'
#' Additional function arguments are rejected; missing-value handling belongs
#' to the preceding aggregate. `.unpack = TRUE` is also rejected because one
#' Parent share is already one scalar double column:
#'
#' ```
#' # Rejected
#' across(
#'   revenue,
#'   share_of_parent,
#'   na.rm = TRUE,
#'   .names = "{.col}_share",
#'   .unpack = TRUE
#' )
#'
#' # Supported
#' revenue = sum(revenue, na.rm = TRUE)
#' across(
#'   revenue,
#'   share_of_parent,
#'   .names = "{.col}_share",
#'   .unpack = FALSE
#' )
#' ```
#'
#' `.names` is required. Generated names must be non-empty and unique and must
#' not collide with fixed or variable grouping keys, ordinary summaries,
#' source columns that remain in the result, `.id`, or another Parent share.
#' Change the template or rename the conflicting output:
#'
#' ```
#' # Rejected: overwrites each source summary
#' across(revenue, share_of_parent, .names = "{.col}")
#'
#' # Supported
#' across(revenue, share_of_parent, .names = "{.col}_share")
#' ```
#'
#' @section Rejected forms and supported rewrites:
#' This checklist keeps every rejection next to the form that should replace
#' it:
#'
#' - **Wrong context:** `share_of_parent(total)` by itself, inside
#'   `dplyr::summarize()`, or inside `dplyr::mutate()` is rejected. Define
#'   `total = sum(value)` and `share = share_of_parent(total)` inside
#'   [summarize_with_margins()]; derive from the finished `share` in a following
#'   `dplyr::mutate()`.
#' - **Unsupported Grouping specification:** `grouping_sets()`, `cube()`, and
#'   `grouping_spec()` do not define one Parent chain. Replace them with one
#'   pure `rollup()` or omit the Parent-share request.
#' - **Unnamed direct output:** `share_of_parent(total)` supplied without
#'   `share =` is rejected. Use `share = share_of_parent(total)`.
#' - **Non-bare source:** `share_of_parent(sum(value))`,
#'   `share_of_parent(total + tax)`, and `share_of_parent("total")` are
#'   rejected. First define `total = sum(value)` and then use
#'   `share = share_of_parent(total)`.
#' - **Forward reference:** `share = share_of_parent(total)` before
#'   `total = sum(value)` is rejected. Move the `total` summary before
#'   `share`.
#' - **Repeated source name:** defining `net` twice is rejected. Use one
#'   complete expression such as `net = sum(revenue) - sum(discount)` before
#'   `net_share = share_of_parent(net)`.
#' - **Unnamed data-frame-valued source:** an unnamed
#'   `tibble::tibble(total = sum(value))` cannot provide `total`. Rewrite it as
#'   the top-level `total = sum(value)` or create a statically named column with
#'   a preceding `across()`.
#' - **Summary-alias dependency:** `gross = sum(value)`,
#'   `net = gross - sum(discount)` is rejected when `net` is a source. Use
#'   `net = sum(value) - sum(discount)`.
#' - **Wrapped Parent share:** `percent = 100 * share_of_parent(total)` is
#'   rejected. Create `share = share_of_parent(total)`, then use
#'   `dplyr::mutate(percent = 100 * share)` on the result.
#' - **Parent-share dependency:** a Parent share cannot source another Parent
#'   share or an ordinary summary later in the same call. Create all requested
#'   Parent shares from ordinary summaries, then derive further columns in
#'   `dplyr::mutate()`.
#' - **Non-numeric or non-scalar source:** semantic classes, zero-length
#'   results, and `quantile(value, c(0.25, 0.75))` are rejected. Convert only
#'   when meaningful and create one scalar summary per output, such as
#'   `q25 = quantile(value, 0.25)` and `q25_share = share_of_parent(q25)`.
#' - **Ineligible `across()` selection:** source columns, grouping keys, and
#'   previous Parent shares are rejected. Select only preceding ordinary
#'   summaries, for example `across(c(total, count), share_of_parent, ...)`.
#' - **Predicate selection:** `where(is.numeric)` is rejected. Use explicit
#'   names, `all_of()`, `any_of()`, or another name-based selector.
#' - **Indirect `.fns`:** `~share_of_parent(.x)`,
#'   `\(x) share_of_parent(x)`, and `list(share_of_parent)` are rejected. Pass
#'   bare `share_of_parent` or `marginplyr::share_of_parent` directly.
#' - **Aggregate and share in one function list:**
#'   `across(value, list(total = sum, share = share_of_parent))` is rejected.
#'   Use one `across(value, sum)` followed by a second
#'   `across(value, share_of_parent, .names = "{.col}_share")`.
#' - **Additional function arguments:** passing `na.rm = TRUE` to the
#'   Parent-share `across()` is rejected. Handle it in the preceding
#'   `total = sum(value, na.rm = TRUE)`, then select `total`.
#' - **Unpacking:** `.unpack = TRUE` is rejected. Omit `.unpack` or use
#'   `.unpack = FALSE`.
#' - **Missing or empty names:** omitted `.names` and `.names = ""` are
#'   rejected. Supply a non-empty template such as
#'   `.names = "{.col}_share"`.
#' - **Duplicate names:** selecting multiple sources with
#'   `.names = "share"` is rejected. Include `{.col}` in the template.
#' - **Grouping-key collision:** `.names = "region"` (or any fixed or variable
#'   key) is rejected. Use a new name such as `{.col}_share`.
#' - **Ordinary-summary collision:** `.names = "{.col}"` overwrites the source
#'   and is rejected. Use `.names = "{.col}_share"`.
#' - **`.id` collision:** `.names = "set"` with `.id = "set"` is rejected.
#'   Rename either output, for example `.id = "occurrence"` and
#'   `.names = "{.col}_share"`.
#' - **Parent-share collision:** reusing a direct or generated Parent-share
#'   name is rejected. Give each Parent share one unique output name.
#'
#' @section Value rules:
#' The parent is the immediate strictly less detailed [rollup()] level within
#' each fixed `.by` partition. Composite dimensions are added or removed
#' together. Duplicate Grouping set occurrences remain in the result but are
#' skipped while finding the next coarser parent.
#'
#' A root Parent share is `1.0`, even when its source is zero or missing.
#' A missing numerator, missing denominator, or zero denominator gives
#' `NA_real_`; local `NaN` is missing. Other finite ratios are unclamped
#' doubles, so negative values and values above one are retained. Parent
#' matching uses internal Grouping set metadata rather than `.id` or displayed
#' Margin labels. Missing fixed or variable keys are matched with missing-safe
#' identity rather than ordinary SQL `NULL = NULL`.
#'
#' Parent shares never synthesize or complete keys:
#'
#' | Empty input | Rows | Parent-share value and type |
#' |---|---:|---|
#' | Without fixed `.by` keys | One root row | `1.0`, double |
#' | With fixed `.by` keys | Zero rows | Empty double vector |
#'
#' Missing detail or subtotal combinations are not completed.
#'
#' @section Lazy execution boundaries:
#' Parent-share execution supports local data frames and lazy dbplyr and dtplyr
#' inputs for one pure [rollup()], including composite dimensions. Lazy results
#' remain lazy: ordinary summaries are followed by one Parent-share mapping and
#' join shared by every requested measure.
#'
#' Arrow inputs reject Parent shares after expression planning and common
#' Margin-operation validation but before constructing a summary query. Other
#' Arrow Margin operations remain supported and lazy. Explicitly collect an
#' Arrow input first when local Parent-share execution is appropriate.
#'
#' General dbplyr backends are not queried solely to discover an arbitrary
#' summary result's type or cardinality. Statically detectable syntax and
#' dependency errors remain local, while an incompatible lazy summary may
#' report its backend error when [dplyr::collect()] executes the staged query.
#' The portable value guarantee covers finite numbers, missing values, and
#' zero denominators. Infinite values and backend-specific `NaN`
#' representations are outside that guarantee because supported SQL dialects
#' do not share one portable finite-value predicate. Normalize potentially
#' non-finite summaries explicitly with operations supported by the backend.
#'
#' marginplyr does not run a schema query, execute the staged query, or collect
#' it solely to improve type or cardinality errors. This preserves laziness and
#' makes [dplyr::show_query()] non-executing; runtime-only incompatibilities
#' remain errors at the backend execution boundary.
#'
#' @param x The bare name of one preceding eligible ordinary summary.
#'
#' @return A double vector when used inside [summarize_with_margins()].
#' @export
#' @examples
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   revenue_share = share_of_parent(revenue),
#'   .by = c(year, month),
#'   .grouping = rollup(region, store)
#' )
#'
#' # Multiple measures use two ordered across() expressions. Selection for the
#' # second across() sees the ordinary summaries created by the first.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   dplyr::across(c(units, revenue), sum),
#'   dplyr::across(
#'     c(units, revenue),
#'     share_of_parent,
#'     .names = "{.col}_share"
#'   ),
#'   .by = c(year, month),
#'   .grouping = rollup(region, store)
#' )
#'
#' # Derived percentages belong in a post-summary mutate().
#' parent_report <- summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   revenue_share = share_of_parent(revenue),
#'   .grouping = rollup(region, store)
#' )
#' dplyr::mutate(
#'   .data = parent_report,
#'   revenue_percent = 100 * revenue_share
#' )
#'
#' # Missing fixed and included keys use missing-safe Parent lookup. The
#' # Grouping bit distinguishes an included missing group from its subtotal.
#' missing_keys <- data.frame(
#'   partition = c(NA_character_, NA_character_, "B", "B"),
#'   group = c("x", NA_character_, "x", NA_character_),
#'   value = c(3, 1, 6, 2)
#' )
#' summarize_with_margins(
#'   .data = missing_keys,
#'   total = sum(value),
#'   share = share_of_parent(total),
#'   group_is_margin = grouping_bit(group),
#'   .by = partition,
#'   .grouping = rollup(group),
#'   .margin_label = NULL
#' )
#'
#' # Empty input without `.by` has one root share. With `.by`, there are no
#' # partitions; both results retain a double Parent-share column.
#' empty_keys <- missing_keys[0, ]
#' empty_root <- summarize_with_margins(
#'   .data = empty_keys,
#'   total = sum(value),
#'   share = share_of_parent(total),
#'   .grouping = rollup(group)
#' )
#' c(rows = nrow(empty_root), type = typeof(empty_root$share))
#' empty_partitions <- summarize_with_margins(
#'   .data = empty_keys,
#'   total = sum(value),
#'   share = share_of_parent(total),
#'   .by = partition,
#'   .grouping = rollup(group)
#' )
#' c(
#'   rows = nrow(empty_partitions),
#'   type = typeof(empty_partitions$share)
#' )
#'
#' # Rejected forms report their supported context or rewrite.
#' try(share_of_parent(revenue))
#' try(dplyr::summarize(
#'   .data = retail_sales,
#'   revenue_share = share_of_parent(revenue)
#' ))
#' try(summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   dplyr::across(
#'     dplyr::where(is.numeric),
#'     share_of_parent,
#'     .names = "{.col}_share"
#'   ),
#'   .grouping = rollup(region, store)
#' ))
share_of_parent <- function(x) {
  abort_marginplyr(
    paste0(
      "`share_of_parent()` can only be used inside ",
      "`summarize_with_margins()` with a `rollup()`. To derive a value from ",
      "an existing Parent share, use a following `dplyr::mutate()`."
    )
  )
}

preflight_parent_shares <- function(dots) {
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    dot_names <- rep("", length(dots))
  }
  has_parent_shares <- FALSE

  for (i in seq_along(dots)) {
    quo <- dots[[i]]
    expr <- rlang::quo_get_expr(quo)
    output_name <- dot_names[[i]]

    if (is_parent_share_call(expr)) {
      validate_parent_direct_syntax(expr, output_name)
      has_parent_shares <- TRUE
      next
    }
    if (is_across_call(expr) && contains_parent_share(expr)) {
      preflight_parent_across_syntax(expr, output_name)
      has_parent_shares <- TRUE
      next
    }
    if (contains_parent_share(expr)) {
      abort_marginplyr(
        paste0(
          "`share_of_parent()` must be the complete right-hand side of a ",
          "named summary, or the direct `.fns` argument of `across()`. ",
          "Create the Parent share as its own named summary, then use a ",
          "following `dplyr::mutate()` for derived values."
        )
      )
    }
  }

  has_parent_shares
}

check_parent_grouping_spec <- function(grouping_spec) {
  kind <- if (is.null(grouping_spec)) NULL else grouping_spec$type
  if (!identical(kind, "rollup")) {
    abort_marginplyr(
      paste0(
        "`share_of_parent()` requires `.grouping` to be one pure `rollup()`. ",
        "`grouping_sets()`, `cube()`, `grouping_spec()`, and other grouping ",
        "specifications do not define one unambiguous parent. Rewrite ",
        "`.grouping` as one `rollup()` or omit the Parent share."
      )
    )
  }
  invisible(NULL)
}

plan_parent_share_expressions <- function(dots,
                                          selection_proxy,
                                          plan,
                                          set_id_name,
                                          validate_cardinality = FALSE) {
  stopifnot(is.list(dots))
  stopifnot(inherits(plan, "margin_grouping_plan"))
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    dot_names <- rep("", length(dots))
  }

  analyses <- analyze_ordinary_summaries(dots, selection_proxy)
  ordinary_records <- unlist(
    lapply(analyses, `[[`, "records"),
    recursive = FALSE
  )
  ordinary_names <- vapply(
    ordinary_records,
    `[[`,
    character(1),
    "name"
  )
  ordinary_counts <- table(ordinary_names)
  planning_context <- list(
    all_records = ordinary_records,
    ordinary_counts = ordinary_counts,
    conflicting_names = unique(c(
      plan$by,
      plan$dimensions,
      set_id_name
    ))
  )

  planned_dots <- as.list(dots)
  requests <- list()
  preceding_ordinary <- list()
  preceding_parent_names <- character()

  for (i in seq_along(dots)) {
    quo <- dots[[i]]
    expr <- rlang::quo_get_expr(quo)
    env <- rlang::quo_get_env(quo)
    output_name <- dot_names[[i]]

    if (is_parent_share_call(expr)) {
      request <- plan_direct_parent_share(
        expr,
        output_name = output_name,
        preceding = preceding_ordinary,
        parent_names = preceding_parent_names,
        context = planning_context
      )
      planned_dots[[i]] <- rlang::new_quosure(
        NA_real_,
        env = rlang::empty_env()
      )
      requests <- c(requests, list(request))
      preceding_parent_names <- c(
        preceding_parent_names,
        request$outputs
      )
      next
    }

    if (is_across_call(expr) && contains_parent_share(expr)) {
      request <- plan_across_parent_share(
        expr,
        env = env,
        output_name = output_name,
        preceding = preceding_ordinary,
        parent_names = preceding_parent_names,
        context = planning_context
      )
      if (length(request$outputs) == 0L) {
        planned_dots[i] <- list(NULL)
        next
      }
      planned_dots[[i]] <- parent_share_placeholder(request$outputs)
      requests <- c(requests, list(request))
      preceding_parent_names <- c(
        preceding_parent_names,
        request$outputs
      )
      next
    }

    if (contains_parent_share(expr)) {
      abort_marginplyr(
        paste0(
          "`share_of_parent()` must be the complete right-hand side of a ",
          "named summary, or the direct `.fns` argument of `across()`."
        )
      )
    }

    parent_dependency <- expression_alias_dependencies(
      expr,
      preceding_parent_names
    )
    if (length(parent_dependency) > 0L) {
      abort_marginplyr(
        paste0(
          "Ordinary summaries cannot use an earlier Parent share (`",
          parent_dependency[[1L]],
          "`) in the same `summarize_with_margins()` call. Use a following ",
          "`dplyr::mutate()` for derived values."
        )
      )
    }
    preceding_ordinary <- c(preceding_ordinary, analyses[[i]]$records)
  }

  if (length(requests) > 0L) {
    check_parent_grouping_kind(plan)
  }

  cardinality <- if (isTRUE(validate_cardinality)) {
    parent_cardinality_records(analyses, requests)
  } else {
    list()
  }

  keep <- !vapply(planned_dots, is.null, logical(1))
  kept_dots <- planned_dots[keep]
  widths <- vapply(
    kept_dots,
    function(dot) {
      if (inherits(dot, "marginplyr_parent_placeholders")) {
        length(dot)
      } else {
        1L
      }
    },
    integer(1)
  )
  flattened_positions <- cumsum(c(
    1L,
    utils::head(widths, -1L)
  ))
  names(flattened_positions) <- as.character(which(keep))
  for (i in seq_along(cardinality)) {
    original_position <- as.character(cardinality[[i]]$position)
    cardinality[[i]]$position <- unname(
      flattened_positions[[original_position]]
    )
  }
  planned_dots <- unlist(
    lapply(
      kept_dots,
      function(dot) {
        if (inherits(dot, "marginplyr_parent_placeholders")) {
          unclass(dot)
        } else {
          list(dot)
        }
      }
    ),
    recursive = FALSE
  )
  list(
    dots = planned_dots,
    requests = requests,
    cardinality = cardinality
  )
}

parent_cardinality_records <- function(analyses, requests) {
  pairs <- parent_share_pairs(requests)
  records <- unlist(
    lapply(analyses, `[[`, "records"),
    recursive = FALSE
  )
  cardinality <- list()
  seen_sources <- character()

  for (pair in pairs) {
    if (pair$source %in% seen_sources) {
      next
    }
    source_records <- Filter(
      function(record) identical(record$name, pair$source),
      records
    )
    if (length(source_records) != 1L) {
      next
    }
    cardinality <- c(cardinality, list(list(
      position = source_records[[1L]]$position,
      parent_output = pair$output,
      source_summary = pair$source,
      across_input = source_records[[1L]]$across_input,
      across_function = source_records[[1L]]$across_function
    )))
    seen_sources <- c(seen_sources, pair$source)
  }
  cardinality
}

wrap_parent_sources <- function(dots,
                                cardinality,
                                call,
                                backend_kind) {
  positions <- unique(vapply(
    cardinality,
    `[[`,
    integer(1),
    "position"
  ))

  for (position in positions) {
    checks <- Filter(
      function(record) identical(record$position, position),
      cardinality
    )
    quo <- dots[[position]]
    expr <- rlang::quo_get_expr(quo)
    if (is_across_call(expr)) {
      if (identical(backend_kind, "dtplyr")) {
        wrapped <- wrap_dtplyr_parent_across(
          expr,
          checks = checks,
          call = call
        )
        dots[[position]] <- rlang::new_quosure(
          wrapped,
          env = rlang::quo_get_env(quo)
        )
        next
      }
      parent_outputs <- vapply(
        checks,
        `[[`,
        character(1),
        "parent_output"
      )
      names(parent_outputs) <- vapply(
        checks,
        `[[`,
        character(1),
        "source_summary"
      )
      wrapped <- rlang::call2(
        parent_private_call("check_parent_across"),
        expr,
        parent_outputs = parent_outputs,
        call = rlang::call2("quote", call)
      )
    } else {
      check <- checks[[1L]]
      is_dtplyr <- identical(backend_kind, "dtplyr")
      wrapped <- rlang::call2(
        parent_private_call(if (is_dtplyr) {
          "check_dtplyr_parent_source"
        } else {
          "check_parent_scalar"
        }),
        expr,
        parent_output = check$parent_output,
        source_summary = check$source_summary,
        !!!if (is_dtplyr) {
          list(call_text = parent_call_text(call))
        } else {
          list(call = rlang::call2("quote", call))
        }
      )
    }
    dots[[position]] <- rlang::new_quosure(
      wrapped,
      env = rlang::quo_get_env(quo)
    )
  }
  dots
}

wrap_dtplyr_parent_across <- function(expr, checks, call) {
  parsed <- parse_across_arguments(expr)
  call_args <- parsed$call_args
  fns_index <- parsed$fns_index
  recognized_positions <- c(
    parsed$cols_index,
    parsed$fns_index,
    parsed$names_index,
    parsed$unpack_index
  )
  forwarded_positions <- setdiff(
    seq_along(call_args),
    recognized_positions[recognized_positions > 0L]
  )
  forwarded_args <- call_args[forwarded_positions]
  if (fns_index == 0L) {
    functions <- list(rlang::expr(~.x))
    function_names <- ""
    fns_is_list <- FALSE
  } else {
    fns <- call_args[[fns_index]]
    fns_is_list <- rlang::is_call(fns, "list")
    if (fns_is_list) {
      functions <- as.list(fns)[-1L]
      function_names <- names(functions)
    } else {
      functions <- list(fns)
      function_names <- ""
    }
  }
  if (is.null(function_names)) {
    function_names <- rep("", length(functions))
  }
  can_inline_forwarded <- !any(vapply(
    functions,
    rlang::is_call,
    logical(1),
    name = "~"
  ))

  for (function_index in seq_along(functions)) {
    function_checks <- Filter(
      function(check) identical(check$across_function, function_index),
      checks
    )
    if (length(function_checks) == 0L) {
      if (can_inline_forwarded && length(forwarded_args) > 0L) {
        functions[[function_index]] <- inline_dtplyr_forwarded_fn(
          functions[[function_index]],
          forwarded_args = forwarded_args
        )
      }
      next
    }
    inputs <- vapply(
      function_checks,
      `[[`,
      character(1),
      "across_input"
    )
    keep <- !duplicated(inputs)
    inputs <- inputs[keep]
    parent_outputs <- vapply(
      function_checks[keep],
      `[[`,
      character(1),
      "parent_output"
    )
    source_summaries <- vapply(
      function_checks[keep],
      `[[`,
      character(1),
      "source_summary"
    )
    functions[[function_index]] <- wrap_dtplyr_parent_function(
      functions[[function_index]],
      mapping = new_parent_validation_mapping(
        inputs = inputs,
        parent_outputs = parent_outputs,
        source_summaries = source_summaries
      ),
      forwarded_args = if (can_inline_forwarded) {
        forwarded_args
      } else {
        list()
      },
      call = call
    )
  }

  wrapped_fns <- if (fns_is_list) {
    value <- rlang::call2("list", !!!functions)
    names(value)[-1L] <- function_names
    value
  } else {
    functions[[1L]]
  }
  if (fns_index == 0L) {
    call_args <- append(call_args, list(.fns = wrapped_fns))
  } else {
    call_args[[fns_index]] <- wrapped_fns
  }
  if (can_inline_forwarded && length(forwarded_positions) > 0L) {
    call_args <- call_args[-forwarded_positions]
  }
  rlang::call2(expr[[1L]], !!!call_args)
}

inline_dtplyr_forwarded_fn <- function(fn, forwarded_args) {
  rlang::call2(
    "~",
    rlang::call2(
      fn,
      # `.x` is the data.table lambda pronoun, quoted here rather than bound.
      rlang::expr(.x), # nolint: object_usage_linter.
      !!!forwarded_args
    )
  )
}

wrap_dtplyr_parent_function <- function(fn, mapping, forwarded_args, call) {
  value <- if (rlang::is_call(fn, "~")) {
    fn[[2L]]
  } else {
    rlang::call2(
      fn,
      # `.x` is the data.table lambda pronoun, quoted here rather than bound.
      rlang::expr(.x), # nolint: object_usage_linter.
      !!!forwarded_args
    )
  }
  input <- rlang::call2(
    parent_private_call("dtplyr_parent_input_name"),
    # `.x` is the data.table lambda pronoun, quoted here rather than bound.
    rlang::expr(.x) # nolint: object_usage_linter.
  )
  mapping_expr <- rlang::call2(
    parent_private_call("new_parent_validation_mapping"),
    inputs = mapping$inputs,
    parent_outputs = mapping$parent_outputs,
    source_summaries = mapping$source_summaries
  )
  validator <- rlang::call2(
    parent_private_call("check_dtplyr_parent_scalar"),
    value,
    input = input,
    mapping = mapping_expr,
    call_text = parent_call_text(call)
  )
  rlang::call2("~", validator)
}

check_dtplyr_parent_scalar <- function(value,
                                       input,
                                       mapping,
                                       call_text) {
  position <- match(input, mapping$inputs)
  if (is.na(position)) {
    return(value)
  }
  check_dtplyr_parent_source(
    value,
    parent_output = mapping$parent_outputs[[position]],
    source_summary = mapping$source_summaries[[position]],
    call_text = call_text
  )
}

check_dtplyr_parent_source <- function(value,
                                       parent_output,
                                       source_summary,
                                       call_text) {
  check_parent_scalar(
    value,
    parent_output = parent_output,
    source_summary = source_summary,
    call = str2lang(call_text)
  )
}

dtplyr_parent_input_name <- function(value) {
  deparse(substitute(value))
}

new_parent_validation_mapping <- function(inputs,
                                          parent_outputs,
                                          source_summaries) {
  stopifnot(
    length(inputs) == length(parent_outputs),
    length(inputs) == length(source_summaries)
  )
  list(
    inputs = inputs,
    parent_outputs = parent_outputs,
    source_summaries = source_summaries
  )
}

parent_private_call <- function(name) {
  rlang::call2(
    ":::",
    rlang::sym("marginplyr"),
    rlang::sym(name)
  )
}

check_parent_across <- function(value, parent_outputs, call) {
  for (source_summary in names(parent_outputs)) {
    check_parent_scalar(
      value[[source_summary]],
      parent_output = parent_outputs[[source_summary]],
      source_summary = source_summary,
      call = call
    )
  }
  value
}

check_parent_scalar <- function(value,
                                parent_output,
                                source_summary,
                                call) {
  if (length(value) != 1L) {
    abort_marginplyr(
      paste0(
        "Parent share `", parent_output, "` requires source summary `",
        source_summary, "` to return exactly one value per grouping row. ",
        "Define `", source_summary, "` as one scalar summary; for multiple ",
        "statistics, create separate named summaries and a Parent share for ",
        "each one."
      ),
      class = "marginplyr_parent_cardinality_error",
      parent_output = parent_output,
      source_summary = source_summary,
      call = call
    )
  }
  if (
    !typeof(value) %in% c("integer", "double") ||
      is.object(value)
  ) {
    detected_type <- if (is.object(value)) class(value) else typeof(value)
    abort_marginplyr(
      paste0(
        "Parent share `", parent_output, "` requires source summary `",
        source_summary,
        "` to be a plain integer or double scalar; detected type ",
        paste(detected_type, collapse = "/"),
        ". Convert it explicitly in the ordinary summary."
      ),
      parent_output = parent_output,
      source_summary = source_summary,
      call = call
    )
  }
  value
}

parent_call_text <- function(call) {
  paste(deparse(call, width.cutoff = 500L), collapse = "\n")
}

analyze_ordinary_summaries <- function(dots, selection_proxy) {
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    dot_names <- rep("", length(dots))
  }
  analyses <- vector("list", length(dots))
  preceding_names <- character()

  for (i in seq_along(dots)) {
    quo <- dots[[i]]
    output_name <- dot_names[[i]]
    expr <- rlang::quo_get_expr(quo)
    env <- rlang::quo_get_env(quo)
    if (contains_parent_share(expr)) {
      analyses[[i]] <- list(records = list())
      next
    }

    if (nzchar(output_name)) {
      output_names <- output_name
      eligible <- !is_across_call(expr)
    } else if (is_across_call(expr)) {
      output_names <- known_across_output_names(
        expr,
        env,
        selection_proxy
      )
      eligible <- TRUE
    } else {
      output_names <- known_data_frame_output_names(
        expr,
        env,
        selection_proxy
      )
      eligible <- FALSE
    }

    selected_dependencies <- if (is_across_call(expr)) {
      intersect(
        known_across_source_names(
          expr,
          env,
          selection_proxy
        ),
        preceding_names
      )
    } else {
      character()
    }
    dependencies <- unique(c(
      expression_alias_dependencies(expr, preceding_names),
      selected_dependencies
    ))
    across_inputs <- if (is_across_call(expr)) {
      inputs <- known_across_source_names(
        expr,
        env,
        selection_proxy
      )
      rep(inputs, each = length(known_across_function_names(
        parse_across_arguments(expr)
      )))
    } else {
      rep(NA_character_, length(output_names))
    }
    across_functions <- if (is_across_call(expr)) {
      function_count <- length(known_across_function_names(
        parse_across_arguments(expr)
      ))
      rep(
        seq_len(function_count),
        times = length(across_inputs) / function_count
      )
    } else {
      rep(NA_integer_, length(output_names))
    }
    records <- Map(
      function(name, across_input, across_function) {
        list(
          name = name,
          position = i,
          eligible = eligible,
          dependencies = dependencies,
          across_input = across_input,
          across_function = across_function
        )
      },
      output_names,
      across_inputs,
      across_functions
    )
    analyses[[i]] <- list(records = records)
    preceding_names <- c(preceding_names, output_names)
  }

  analyses
}

plan_direct_parent_share <- function(expr,
                                     output_name,
                                     preceding,
                                     parent_names,
                                     context) {
  args <- validate_parent_direct_syntax(expr, output_name)
  source <- rlang::as_name(args[[1L]])
  validate_parent_share_request(
    outputs = output_name,
    sources = source,
    preceding = preceding,
    parent_names = parent_names,
    context = context
  )
  list(outputs = output_name, sources = source)
}

validate_parent_direct_syntax <- function(expr, output_name) {
  if (!nzchar(output_name)) {
    abort_marginplyr(
      paste0(
        "A direct `share_of_parent()` summary must have an explicit output ",
        "name. Rewrite it as `name = share_of_parent(source)`."
      )
    )
  }
  args <- rlang::call_args(expr)
  if (length(args) != 1L || !rlang::is_symbol(args[[1L]])) {
    abort_marginplyr(
      paste0(
        "`", output_name, " = share_of_parent(...)` requires exactly one ",
        "bare name of a preceding ordinary summary. Define the scalar ",
        "summary first, then pass its name directly to `share_of_parent()`."
      )
    )
  }
  args
}

plan_across_parent_share <- function(expr,
                                     env,
                                     output_name,
                                     preceding,
                                     parent_names,
                                     context) {
  syntax <- validate_parent_across_syntax(expr, env, output_name)
  args <- syntax$args
  names_template <- syntax$names_template

  selectable <- vapply(
    preceding,
    function(record) {
      isTRUE(record$eligible) &&
        length(record$dependencies) == 0L &&
        context$ordinary_counts[[record$name]] == 1L
    },
    logical(1)
  )
  preceding_names <- unique(vapply(
    preceding[selectable],
    `[[`,
    character(1),
    "name"
  ))
  if (contains_selection_predicate(args$cols)) {
    abort_parent_predicate()
  }
  sources <- resolve_parent_share_selection(
    args$cols,
    env = env,
    preceding_names = preceding_names,
    preceding = preceding,
    context = context
  )
  outputs <- vapply(
    sources,
    function(source) {
      expand_across_name(names_template, source, "1", env)
    },
    character(1)
  )

  validate_parent_share_request(
    outputs = outputs,
    sources = sources,
    preceding = preceding,
    parent_names = parent_names,
    context = context
  )
  list(outputs = outputs, sources = sources)
}

validate_parent_across_syntax <- function(expr, env, output_name) {
  args <- preflight_parent_across_syntax(expr, output_name)
  if (!is.null(args$unpack)) {
    unpack <- rlang::eval_tidy(args$unpack, env = env)
    if (!isFALSE(unpack)) {
      abort_marginplyr(
        paste0(
          "Parent-share `across()` requires `.unpack = FALSE` or an ",
          "omitted `.unpack` argument."
        )
      )
    }
  }
  names_template <- rlang::eval_tidy(args$names, env = env)
  if (
    !is.character(names_template) ||
      length(names_template) != 1L ||
      is.na(names_template)
  ) {
    abort_marginplyr(
      paste0(
        "Parent-share `across()` `.names` must be one non-missing character ",
        "template."
      )
    )
  }
  list(args = args, names_template = names_template)
}

preflight_parent_across_syntax <- function(expr, output_name) {
  if (nzchar(output_name)) {
    abort_marginplyr(
      paste0(
        "An `across()` Parent-share expression must be unnamed; use its ",
        "required `.names` argument to name the output columns."
      )
    )
  }
  args <- parse_across_arguments(expr)
  if (!is_parent_share_function(args$fns)) {
    abort_marginplyr(
      paste0(
        "For Parent shares, `across()` `.fns` must be `share_of_parent` or ",
        "`marginplyr::share_of_parent` directly. Use two ordered `across()` ",
        "expressions instead of a formula, anonymous function, or function ",
        "list."
      )
    )
  }
  if (length(args$additional) > 0L) {
    abort_marginplyr(
      paste0(
        "Parent-share `across()` does not accept additional function ",
        "arguments: ",
        paste0("`", args$additional, "`", collapse = ", "),
        ". Put missing-value handling in the preceding ordinary summary."
      )
    )
  }
  if (is.null(args$names)) {
    abort_marginplyr(
      paste0(
        "Parent-share `across()` requires an explicit `.names` argument, ",
        "for example `.names = \"{.col}_share\"`."
      )
    )
  }
  if (!is.null(args$unpack) && is.logical(args$unpack)) {
    if (length(args$unpack) != 1L || !isFALSE(args$unpack)) {
      abort_marginplyr(
        paste0(
          "Parent-share `across()` requires `.unpack = FALSE` or an ",
          "omitted `.unpack` argument."
        )
      )
    }
  }
  if (contains_selection_predicate(args$cols)) {
    abort_parent_predicate()
  }
  args
}

validate_parent_share_request <- function(outputs,
                                          sources,
                                          preceding,
                                          parent_names,
                                          context) {
  if (length(outputs) == 0L) {
    return(invisible(NULL))
  }
  if (any(!nzchar(outputs))) {
    abort_marginplyr(
      "Parent-share output names must not be empty."
    )
  }
  if (anyDuplicated(outputs)) {
    abort_marginplyr(
      paste0(
        "Parent-share output names must be unique; duplicate name `",
        outputs[[anyDuplicated(outputs)]],
        "` was generated."
      )
    )
  }

  preceding_names <- vapply(preceding, `[[`, character(1), "name")
  all_names <- vapply(
    context$all_records,
    `[[`,
    character(1),
    "name"
  )
  for (i in seq_along(sources)) {
    source <- sources[[i]]
    output <- outputs[[i]]
    if (source %in% parent_names) {
      abort_marginplyr(
        paste0(
          "Parent share `", output, "` cannot use Parent share `", source,
          "` as its source."
        )
      )
    }
    if (!source %in% preceding_names) {
      if (source %in% all_names) {
        abort_marginplyr(
          paste0(
            "Parent share `", output, "` must refer to an ordinary summary ",
            "defined before it; `", source, "` is a forward reference."
          )
        )
      }
      abort_marginplyr(
        paste0(
          "Parent share `", output, "` refers to unknown preceding ordinary ",
          "summary `", source, "`."
        )
      )
    }
    if (
      !is.na(context$ordinary_counts[[source]]) &&
        context$ordinary_counts[[source]] != 1L
    ) {
      abort_marginplyr(
        paste0(
          "Parent share `", output, "` requires source summary `", source,
          "` to be defined exactly once. Use one uniquely named ordinary ",
          "summary."
        )
      )
    }
    record <- preceding[[max(which(preceding_names == source))]]
    if (!isTRUE(record$eligible)) {
      abort_marginplyr(
        paste0(
          "Parent share `", output, "` cannot use `", source,
          "` because it was expanded from a data-frame-valued summary. ",
          "Rewrite it as a top-level named summary or a preceding `across()` ",
          "output."
        )
      )
    }
    if (length(record$dependencies) > 0L) {
      abort_marginplyr(
        paste0(
          "Parent share `", output, "` cannot use source summary `", source,
          "` because it depends on earlier summary alias `",
          record$dependencies[[1L]],
          "`. Combine the calculation into one ordinary summary expression."
        )
      )
    }
  }

  conflicts <- intersect(
    outputs,
    unique(c(
      context$conflicting_names,
      all_names,
      parent_names
    ))
  )
  if (length(conflicts) > 0L) {
    abort_marginplyr(
      paste0(
        "Parent-share output name `", conflicts[[1L]],
        "` conflicts with a grouping key, `.id`, ordinary summary, source ",
        "summary, or earlier Parent share."
      )
    )
  }
  invisible(NULL)
}

check_parent_grouping_kind <- function(plan) {
  if (!identical(plan$kind, "rollup")) {
    abort_marginplyr(
      paste0(
        "`share_of_parent()` requires `.grouping` to be one pure `rollup()`. ",
        "`grouping_sets()`, `cube()`, `grouping_spec()`, and other grouping ",
        "specifications do not define one unambiguous parent. Rewrite ",
        "`.grouping` as one `rollup()` or omit the Parent share."
      )
    )
  }
  invisible(NULL)
}

execute_parent_shares <- function(operation,
                                  staged_result,
                                  requests) {
  check_margin_operation(operation)
  check_margin_summary_stage(staged_result)
  if (length(requests) == 0L) {
    return(margin_summary_stage_result(
      staged_result
    ))
  }

  result <- margin_summary_stage_result(
    staged_result
  )
  staged_set_id_name <- margin_summary_stage_set_id(
    staged_result
  )
  adapter <- parent_share_adapter(operation$backend$kind)
  result <- adapter(
    operation,
    result = result,
    requests = requests,
    set_id_name = staged_set_id_name
  )
  if (!is.null(operation$set_id_name)) {
    result <- dplyr::mutate(
      result,
      "{operation$set_id_name}" := .data[[staged_set_id_name]]
    )
  }
  dplyr::select(
    result,
    -dplyr::all_of(staged_set_id_name)
  )
}

parent_share_adapter <- function(backend_kind) {
  adapters <- list(
    local = execute_local_parent_shares,
    duckdb = execute_dbplyr_parent_shares,
    postgres = execute_dbplyr_parent_shares,
    sql = execute_dbplyr_parent_shares,
    dtplyr = execute_non_sql_parent_shares,
    other = execute_non_sql_parent_shares
  )
  adapter <- adapters[[backend_kind]]
  if (is.null(adapter)) {
    stop(
      "Unknown Parent-share backend kind: ", backend_kind,
      call. = FALSE
    )
  }
  adapter
}

execute_local_parent_shares <- function(operation,
                                        result,
                                        requests,
                                        set_id_name) {
  check_local_parent_share_types(result, requests)
  apply_joined_parent_shares(
    result,
    requests = requests,
    plan = operation$plan,
    set_id_name = set_id_name,
    sql_join = FALSE
  )
}

execute_dbplyr_parent_shares <- function(operation,
                                         result,
                                         requests,
                                         set_id_name) {
  apply_joined_parent_shares(
    result,
    requests = requests,
    plan = operation$plan,
    set_id_name = set_id_name,
    sql_join = TRUE
  )
}

execute_non_sql_parent_shares <- function(operation,
                                          result,
                                          requests,
                                          set_id_name) {
  apply_joined_parent_shares(
    result,
    requests = requests,
    plan = operation$plan,
    set_id_name = set_id_name,
    sql_join = FALSE
  )
}

apply_joined_parent_shares <- function(result,
                                       requests,
                                       plan,
                                       set_id_name,
                                       sql_join) {
  parent_ids <- parent_set_ids(plan)
  root_ids <- plan$set_ids[is.na(parent_ids)]
  pairs <- parent_share_pairs(requests)
  sources <- unique(vapply(pairs, `[[`, character(1), "source"))
  result_names <- get_col_names(result, dplyr::everything())
  denominator_names <- new_margin_internal_names(
    length(sources),
    used_names = result_names,
    prefix = "..marginplyr_parent_value_"
  )
  names(denominator_names) <- sources

  child_ids <- plan$set_ids[!is.na(parent_ids)]
  if (length(child_ids) > 0L) {
    mapping <- build_lazy_parent_mapping(
      result,
      child_ids = child_ids,
      parent_ids = parent_ids,
      sources = sources,
      denominator_names = denominator_names,
      plan = plan,
      set_id_name = set_id_name
    )
    join_key_names <- new_margin_internal_names(
      length(plan$dimensions),
      used_names = c(result_names, denominator_names),
      prefix = "..marginplyr_parent_key_"
    )
    names(join_key_names) <- plan$dimensions
    result <- add_lazy_parent_join_keys(
      result,
      plan = plan,
      parent_ids = parent_ids,
      set_id_name = set_id_name,
      join_key_names = join_key_names
    )
    mapping <- add_lazy_parent_join_keys(
      mapping,
      plan = plan,
      parent_ids = parent_ids,
      set_id_name = set_id_name,
      join_key_names = join_key_names
    )
    mapping <- dplyr::select(
      mapping,
      dplyr::all_of(c(
        set_id_name,
        plan$by,
        unname(join_key_names),
        unname(denominator_names)
      ))
    )
    join_names <- c(set_id_name, plan$by, unname(join_key_names))
    if (sql_join) {
      right_join_names <- new_margin_internal_names(
        length(join_names),
        used_names = c(
          result_names,
          denominator_names,
          join_key_names
        ),
        prefix = "..marginplyr_parent_match_"
      )
      rename_pairs <- rlang::set_names(
        rlang::syms(join_names),
        right_join_names
      )
      mapping <- dplyr::rename(mapping, !!!rename_pairs)
      result <- dplyr::left_join(
        result,
        mapping,
        sql_on = lazy_parent_sql_on(
          con = dbplyr::remote_con(result),
          left_names = join_names,
          right_names = right_join_names
        ),
        x_as = "LHS",
        y_as = "RHS"
      )
    } else {
      right_join_names <- character()
      result <- dplyr::left_join(
        result,
        mapping,
        by = join_names,
        na_matches = "na"
      )
    }
  }

  share_exprs <- lapply(
    pairs,
    function(pair) {
      source <- pair$source
      denominator <- denominator_names[[source]]
      if (length(child_ids) == 0L) {
        return(rlang::expr(1.0))
      }
      rlang::expr(
        dplyr::if_else(
          .data[[!!set_id_name]] %in% !!root_ids,
          1.0,
          dplyr::if_else(
            is.na(.data[[!!source]]) |
              is.na(.data[[!!denominator]]) |
              .data[[!!denominator]] == 0,
            NA_real_,
            as.double(.data[[!!source]]) /
              as.double(.data[[!!denominator]])
          )
        )
      )
    }
  )
  names(share_exprs) <- vapply(pairs, `[[`, character(1), "output")
  result <- dplyr::mutate(result, !!!share_exprs)

  internal_names <- c(
    unname(denominator_names),
    if (exists("right_join_names", inherits = FALSE)) {
      right_join_names
    } else {
      character()
    },
    if (exists("join_key_names", inherits = FALSE)) {
      unname(join_key_names)
    } else {
      character()
    }
  )
  internal_names <- intersect(
    internal_names,
    get_col_names(result, dplyr::everything())
  )
  if (length(internal_names) > 0L) {
    result <- dplyr::select(result, -dplyr::all_of(internal_names))
  }
  result
}

lazy_parent_sql_on <- function(con, left_names, right_names) {
  stopifnot(length(left_names) == length(right_names))
  # Both are read only from the glue string below, which codetools cannot see.
  # nolint start: object_usage_linter.
  left_alias <- "LHS"
  right_alias <- "RHS"
  # nolint end
  terms <- Map(
    function(left_name, right_name) {
      dbplyr::sql_glue2(
        con,
        paste0(
          "(({.id left_alias}.{.id left_name} = ",
          "{.id right_alias}.{.id right_name}) OR ",
          "({.id left_alias}.{.id left_name} IS NULL AND ",
          "{.id right_alias}.{.id right_name} IS NULL))"
        )
      )
    },
    left_names,
    right_names
  )
  dbplyr::sql(paste(
    vapply(terms, as.character, character(1)),
    collapse = " AND "
  ))
}

parent_share_pairs <- function(requests) {
  unlist(
    lapply(
      requests,
      function(request) {
        Map(
          function(output, source) {
            list(output = output, source = source)
          },
          request$outputs,
          request$sources
        )
      }
    ),
    recursive = FALSE
  )
}

build_lazy_parent_mapping <- function(result,
                                      child_ids,
                                      parent_ids,
                                      sources,
                                      denominator_names,
                                      plan,
                                      set_id_name) {
  group_vars <- unique(c(plan$by, plan$dimensions))
  key_exprs <- lapply(
    group_vars,
    function(var) rlang::expr(.data[[!!var]])
  )
  names(key_exprs) <- group_vars
  denominator_exprs <- lapply(
    sources,
    function(source) rlang::expr(.data[[!!source]])
  )
  names(denominator_exprs) <- unname(denominator_names[sources])

  mappings <- lapply(
    child_ids,
    function(child_id) {
      parent_id <- parent_ids[[child_id]]
      parent_rows <- dplyr::filter(
        result,
        .data[[set_id_name]] == !!parent_id
      )
      child_id_expr <- stats::setNames(
        list(rlang::expr(as.integer(!!child_id))),
        set_id_name
      )
      dplyr::transmute(
        parent_rows,
        !!!key_exprs,
        !!!child_id_expr,
        !!!denominator_exprs
      )
    }
  )
  Reduce(dplyr::union_all, mappings)
}

add_lazy_parent_join_keys <- function(result,
                                      plan,
                                      parent_ids,
                                      set_id_name,
                                      join_key_names) {
  join_key_exprs <- lapply(
    plan$dimensions,
    function(dimension) {
      matching_child_ids <- plan$set_ids[vapply(
        plan$set_ids,
        function(set_id) {
          parent_id <- parent_ids[[set_id]]
          !is.na(parent_id) && dimension %in% plan$sets[[parent_id]]
        },
        logical(1)
      )]
      rlang::expr(
        dplyr::if_else(
          .data[[!!set_id_name]] %in% !!matching_child_ids,
          .data[[!!dimension]],
          NA
        )
      )
    }
  )
  names(join_key_exprs) <- unname(join_key_names[plan$dimensions])
  dplyr::mutate(result, !!!join_key_exprs)
}

check_local_parent_share_types <- function(result, requests) {
  pairs <- parent_share_pairs(requests)
  checked_sources <- character()

  for (pair in pairs) {
    source <- pair$source
    if (source %in% checked_sources) {
      next
    }
    values <- result[[source]]
    if (
      !typeof(values) %in% c("integer", "double") ||
        is.object(values)
    ) {
      detected_type <- if (is.object(values)) class(values) else typeof(values)
      abort_marginplyr(
        paste0(
          "Parent share `", pair$output, "` requires source summary `", source,
          "` to be a plain integer or double scalar; detected type ",
          paste(detected_type, collapse = "/"),
          ". Convert it explicitly in the ordinary summary."
        )
      )
    }
    checked_sources <- c(checked_sources, source)
  }
  invisible(NULL)
}

parent_set_ids <- function(plan) {
  result <- rep(NA_integer_, length(plan$sets))
  variable_sets <- lapply(
    plan$sets,
    setdiff,
    y = plan$by
  )
  for (i in seq_along(variable_sets)) {
    child <- variable_sets[[i]]
    candidates <- which(vapply(
      variable_sets,
      function(parent) {
        length(parent) < length(child) && all(parent %in% child)
      },
      logical(1)
    ))
    candidates <- candidates[candidates > i]
    if (length(candidates) > 0L) {
      result[[i]] <- candidates[[1L]]
    }
  }
  result
}

parent_share_placeholder <- function(outputs) {
  placeholders <- lapply(
    outputs,
    function(output) {
      rlang::new_quosure(NA_real_, env = rlang::empty_env())
    }
  )
  names(placeholders) <- outputs
  structure(placeholders, class = "marginplyr_parent_placeholders")
}

is_parent_share_call <- function(expr) {
  rlang::is_call(expr) &&
    identical(rlang::call_name(expr), "share_of_parent") &&
    (is.null(rlang::call_ns(expr)) ||
       identical(rlang::call_ns(expr), "marginplyr"))
}

contains_parent_share <- function(expr) {
  if (is_parent_share_function(expr)) {
    return(TRUE)
  }
  if (!rlang::is_call(expr)) {
    return(FALSE)
  }
  if (is_parent_share_call(expr)) {
    return(TRUE)
  }
  any(vapply(
    as.list(expr)[-1L],
    contains_parent_share,
    logical(1)
  ))
}

is_parent_share_function <- function(expr) {
  if (rlang::is_symbol(expr)) {
    return(identical(rlang::as_name(expr), "share_of_parent"))
  }
  rlang::is_call(expr, "::") &&
    length(expr) == 3L &&
    rlang::is_symbol(expr[[2L]], "marginplyr") &&
    rlang::is_symbol(expr[[3L]], "share_of_parent")
}

is_across_call <- function(expr) {
  rlang::is_call(expr) &&
    identical(rlang::call_name(expr), "across") &&
    (is.null(rlang::call_ns(expr)) ||
       identical(rlang::call_ns(expr), "dplyr"))
}

resolve_parent_share_selection <- function(expr,
                                           env,
                                           preceding_names,
                                           preceding,
                                           context) {
  if (rlang::is_symbol(expr)) {
    source <- rlang::as_name(expr)
    if (!source %in% preceding_names) {
      abort_parent_source_name(source, preceding, context)
    }
  }
  proxy <- stats::setNames(
    as.list(seq_along(preceding_names)),
    preceding_names
  )
  tryCatch(
    names(tidyselect::eval_select(
      rlang::new_quosure(expr, env = env),
      data = proxy,
      strict = TRUE,
      allow_rename = FALSE
    )),
    error = function(cnd) {
      abort_parent_selection_error(cnd, preceding, context)
    }
  )
}

abort_parent_selection_error <- function(cnd, preceding, context) {
  missing <- parent_selection_missing_names(cnd)
  if (length(missing) == 0L) {
    abort_marginplyr(
      paste0(
        "Invalid Parent-share `across()` selection. Select only eligible ",
        "preceding ordinary summaries by name: ",
        conditionMessage(cnd)
      )
    )
  }

  abort_parent_source_name(missing[[1L]], preceding, context)
}

abort_parent_source_name <- function(source, preceding, context) {
  all_names <- vapply(
    context$all_records,
    `[[`,
    character(1),
    "name"
  )
  occurrences <- sum(all_names == source)
  if (occurrences > 1L) {
    abort_marginplyr(
      paste0(
        "`across()` can't select source summary `", source,
        "` for `share_of_parent()` because summary `", source,
        "` was defined more than once. Define it once with a complete ",
        "ordinary summary expression, then select that unique preceding ",
        "summary by name."
      ),
      class = "marginplyr_parent_source_duplicate_error",
      source_summary = source
    )
  }
  if (occurrences == 1L) {
    abort_marginplyr(
      paste0(
        "`across()` can't select source summary `", source,
        "` for `share_of_parent()` because summary `", source,
        "` is not available as a unique, preceding, self-contained ordinary ",
        "summary. Define it as a top-level named summary or a statically ",
        "named output from a preceding `across()`. Select only eligible ",
        "preceding ordinary summaries by name."
      ),
      class = "marginplyr_parent_source_unavailable_error",
      source_summary = source
    )
  }

  preceding_candidates <- unique(vapply(
    preceding,
    `[[`,
    character(1),
    "name"
  ))
  abort_marginplyr(
    paste0(
      "`across()` refers to unknown summary `", source,
      "` for `share_of_parent()`. Select only eligible preceding ordinary ",
      "summaries by name",
      if (length(preceding_candidates) > 0L) {
        paste0(
          ", such as `", preceding_candidates[[1L]], "`."
        )
      } else {
        "."
      }
    ),
    class = "marginplyr_parent_source_unknown_error",
    source_summary = source
  )
}

parent_selection_missing_names <- function(cnd) {
  current <- if (is.character(cnd$i)) cnd$i else character()
  parent <- cnd$parent
  if (inherits(parent, "condition")) {
    current <- c(current, parent_selection_missing_names(parent))
  }
  unique(current[nzchar(current)])
}

contains_selection_predicate <- function(expr) {
  if (rlang::is_symbol(expr)) {
    return(FALSE)
  }
  if (!rlang::is_call(expr)) {
    return(FALSE)
  }
  if (identical(rlang::call_name(expr), "where")) {
    return(TRUE)
  }
  any(vapply(
    as.list(expr)[-1L],
    contains_selection_predicate,
    logical(1)
  ))
}

abort_parent_predicate <- function() {
  abort_marginplyr(
    paste0(
      "Parent-share `across()` only supports name-based tidyselect. Replace ",
      "`where()` or another type/value predicate with explicit summary names."
    )
  )
}

expression_alias_dependencies <- function(expr, aliases) {
  if (length(aliases) == 0L) {
    return(character())
  }
  intersect(unique(expression_data_symbols(expr)), aliases)
}

expression_data_symbols <- function(expr) {
  if (rlang::is_symbol(expr)) {
    return(rlang::as_name(expr))
  }
  if (!rlang::is_call(expr)) {
    return(character())
  }
  if (identical(rlang::call_name(expr), "get") && length(expr) >= 2L) {
    if (get_has_external_env(expr)) {
      return(character())
    }
    args <- rlang::call_args(expr)
    arg_names <- names(args)
    if (is.null(arg_names)) {
      arg_names <- rep("", length(args))
    }
    name_index <- match("x", arg_names, nomatch = 0L)
    if (name_index == 0L) {
      name_index <- which(arg_names == "")[[1L]]
    }
    name <- args[[name_index]]
    if (
      is.character(name) &&
        length(name) == 1L &&
        !is.na(name)
    ) {
      return(name)
    }
  }
  if (
    rlang::call_name(expr) %in% c("$", "[[") &&
      length(expr) >= 3L &&
      rlang::is_symbol(expr[[2L]])
  ) {
    pronoun <- rlang::as_name(expr[[2L]])
    if (identical(pronoun, ".env")) {
      return(character())
    }
    if (identical(pronoun, ".data")) {
      column <- expr[[3L]]
      if (rlang::is_symbol(column)) {
        return(rlang::as_name(column))
      }
      if (
        is.character(column) &&
          length(column) == 1L &&
          !is.na(column)
      ) {
        return(column)
      }
      return(character())
    }
  }
  args <- as.list(expr)[-1L]
  unique(unlist(
    lapply(args, expression_data_symbols),
    use.names = FALSE
  ))
}

get_has_external_env <- function(expr) {
  args <- rlang::call_args(expr)
  arg_names <- names(args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(args))
  }
  if (any(arg_names %in% c("pos", "envir"))) {
    return(TRUE)
  }

  unnamed_count <- sum(arg_names == "")
  x_is_named <- "x" %in% arg_names
  unnamed_count > as.integer(!x_is_named)
}
