#' Calculate a summary's share of its rollup parent or of the grand total
#'
#' `share_of_parent()` and `share_of_total()` are contextual summary helpers
#' for [summarize_with_margins()]. Each divides one preceding named numeric
#' scalar summary by the same summary on another row of the same result. Only
#' the denominator differs:
#'
#' | Helper | Denominator | Accepted `.grouping` |
#' |---|---|---|
#' | `share_of_parent()` | The immediately less detailed [rollup()] level | One pure [rollup()] | # nolint: line_length_linter
#' | `share_of_total()` | The Grand total set, in which every grouping dimension is omitted | Any specification whose plan contains one | # nolint: line_length_linter
#'
#' Fixed `.by` columns partition both calculations, so a fixed key never
#' contributes to another partition's denominator. Everything else on this
#' page — eligible sources, the direct and `across()` grammar, output naming,
#' the value rules, and the backend boundaries — is the same for both helpers,
#' and both are written the same way:
#'
#' ```
#' revenue = sum(revenue)
#' parent_share = share_of_parent(revenue)
#' total_share = share_of_total(revenue)
#' ```
#'
#' A helper is contextual because its denominator belongs to another
#' grouping-set row, so it can be used only inside
#' [summarize_with_margins()]. Direct calls, ordinary [dplyr::summarize()]
#' calls, and [dplyr::mutate()] calls are rejected.
#'
#' `share_of_parent()` additionally requires one pure [rollup()];
#' [grouping_sets()], [cube()], and [grouping_spec()] are rejected. A grouping
#' set can have several strictly less detailed sets in the same plan, and
#' nothing in the plan says which of them is *the* parent.
#'
#' `share_of_total()` has no such choice to make: a Grand total set is not
#' selected, and a plan either contains one or does not. It therefore accepts
#' any Grouping specification whose plan contains one. [rollup()] and [cube()]
#' always produce one; [grouping_sets()] produces one only when it includes an
#' empty [grouping_set()], and a specification without one is rejected naming
#' that fix.
#'
#' Neither helper reaches a level between the two. For a denominator at a
#' named ancestor level, move that dimension into `.by` and use
#' `share_of_total()`; the [recipes guide][recipes] writes that out, and the
#' join for the case where the levels above the ancestor have to stay in the
#' same result.
#'
#' [recipes]: https://sayuks.github.io/marginplyr/vignettes/recipes.html
#'
#' @section Direct shares:
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
#' # Rewrite: derive from the finished share afterwards
#' result |> dplyr::mutate(revenue_percent = 100 * revenue_share)
#' ```
#'
#' A direct call cannot be unnamed, use a string, use a forward reference,
#' redefine the source name, or use a share as another share's source.
#'
#' @section Eligible source summaries:
#' The source must be defined exactly once before the share. It must be
#' a top-level named summary or a statically named output from a preceding
#' [dplyr::across()], return one plain integer or double per grouping row, and
#' be self-contained. A source cannot depend on an earlier summary alias.
#'
#' Integer, double, and the corresponding database numeric/decimal scalar
#' summaries are eligible. A share is always a double. Logical,
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
#' summaries cannot be matched to one denominator row. A quantile call with
#' several probabilities is one common multi-value result. Define each
#' statistic as a separate scalar summary column:
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
#' static provenance is not portable enough for a share's dependency:
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
#' Naming an [dplyr::across()] does the mirror image of that:
#' `total = across(c(revenue, units), sum)` packs both results into one
#' data-frame-valued `total` column, so `total` is not a scalar source. Drop
#' the name to get one statically named column per selected column, or define
#' each summary at top level.
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
#' use a share in the same call because many database backends cannot
#' reuse aliases in one summary projection. Use a following [dplyr::mutate()]
#' for percentages, rounding, labels, or other derived values.
#'
#' Dependency validation follows the expressions' written order even though
#' the implementation can evaluate ordinary summaries in one internal stage.
#' Final columns retain the user's written expression order, including the
#' expansion position of each `across()` call; internal staging does not
#' reorder the public result.
#'
#' @section Column-wise shares:
#' In `across(.cols, share_of_parent, .names = ...)` and its
#' `share_of_total` counterpart, `.cols` sees only preceding eligible ordinary
#' summaries. Source-data columns, fixed `.by` keys, variable grouping
#' dimensions, and earlier shares are outside this selection context.
#' [dplyr::everything()] therefore means every preceding ordinary summary, not
#' every input column.
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
#' `.fns` must be one direct bare helper — `share_of_parent`,
#' `share_of_total`, or either written as `marginplyr::share_of_parent`.
#' Formulas, anonymous functions, and function lists are rejected, even if
#' they contain only a helper. Use two ordered `across()` expressions so the
#' dependency is explicit:
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
#' share is already one scalar double column:
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
#' source columns that remain in the result, `.id`, or another share of either
#' kind. Change the template or rename the conflicting output:
#'
#' ```
#' # Rejected: overwrites each source summary
#' across(revenue, share_of_parent, .names = "{.col}")
#'
#' # Supported
#' across(revenue, share_of_parent, .names = "{.col}_share")
#' ```
#'
#' @section Value rules:
#' Both helpers divide within each fixed `.by` partition, and neither ever
#' synthesizes or completes keys. A missing numerator, missing denominator, or
#' zero denominator gives `NA_real_`; local `NaN` is missing. Other finite
#' ratios are unclamped doubles, so negative values and values above one are
#' retained. Denominator matching uses internal grouping set metadata rather
#' than `.id` or displayed Margin labels, and missing fixed or variable keys
#' are matched with missing-safe identity rather than ordinary SQL
#' `NULL = NULL`.
#'
#' A Parent share's denominator is the immediate strictly less detailed
#' [rollup()] level. Composite dimensions are added or removed together. The
#' Grand total set has no parent, so its Parent share is `1.0`, even when its
#' source is zero or missing. Duplicate grouping set occurrences remain in the
#' result but are skipped while finding the next coarser parent, because a
#' parent must be strictly less detailed.
#'
#' A Total share's denominator is the Grand total set. Every row of it,
#' duplicate occurrences included, has a Total share of `1.0`, even when its
#' source is zero or missing. Which grand total occurrence supplies the
#' denominator is not specified.
#'
#' Empty input gives one Grand total set row or no rows at all, and the share
#' column keeps its type either way:
#'
#' | Empty input | Rows | Share value and type |
#' |---|---:|---|
#' | Without fixed `.by` keys | One row of the Grand total set | `1.0`, double |
#' | With fixed `.by` keys | Zero rows | Empty double vector |
#'
#' Missing detail or subtotal combinations are not completed. The [completing
#' keys guide][keys] shows how to complete the input facts before the summary,
#' where synthetic facts are meant to reach every level.
#'
#' [keys]: https://sayuks.github.io/marginplyr/vignettes/completing_keys.html
#'
#' @section Lazy execution boundaries:
#' Share execution supports local data frames and lazy dbplyr and dtplyr
#' inputs, including composite dimensions. Lazy results remain lazy. Each
#' share uses the source value displayed on its denominator row, including
#' when the source summary is stateful or nondeterministic. SQL shares require
#' window-function support. A dialect without it, such as Access, reports an
#' unsupported window when rendering or executing the query. Explicitly
#' collect that input first to calculate shares locally.
#' On live SQLite, direct [dplyr::collect()] keeps a declared double share
#' when the result is empty or a finite prefix contains only missing shares.
#' Direct [dplyr::compute()] writes a typed table containing only public
#' columns, and a sorted result
#' reads back in Margin order. A sorted compute refuses before writing when
#' public columns shadow all SQLite rowid aliases. Later dplyr verbs use
#' ordinary dbplyr behavior, including its ordering rules. See
#' [summarize_with_margins()] for destination and transaction details.
#'
#' Syntax, source-name, written-order, and `across()` errors are always
#' reported locally, before execution, on every backend. The eligible-type
#' rule is also enforced on every backend, and no backend calculates a share
#' from a source it has shown to be ineligible. None of it reads a row of your
#' data.
#'
#' The summary that rule judges is the one the backend computed, not the one
#' the same expression would produce in R, so an aggregate whose result type
#' differs across backends is eligible on some and refused on others. `max()`
#' over a logical column is one: R coerces it to an integer, which the rule
#' accepts, while a database whose `MAX` returns a boolean has produced an
#' ineligible summary and refuses the share.
#'
#' What differs is what establishes the rule, and whether the
#' exactly-one-value cardinality rule is established with it:
#'
#' | Backend | What establishes the source rules |
#' |---|---|
#' | Local data frame | Both, from the result, before any share |
#' | `dtplyr` step | Both, at execution, before an invalid row |
#' | Arrow | Neither; shares are rejected outright |
#' | General dbplyr | Type from the dialect, and cardinality as below |
#'
#' Arrow inputs reject both helpers after expression planning and common
#' Margin-operation validation but before constructing a summary query. The
#' reason belongs to the source summary they share, so it applies to both, and
#' the diagnostic names whichever helpers the call used. Other Arrow Margin
#' operations remain supported and lazy, apart from a summary Arrow's own
#' engine cannot evaluate, which is refused for its own reason and before any
#' row is read; see [summarize_with_margins()]. Explicitly collect an Arrow
#' input first when local share execution is appropriate. Collection changes
#' `.check_margin_label`'s default from `FALSE` to `TRUE`, so choose that
#' policy explicitly on the re-run.
#'
#' A `dtplyr` step remains a native lazy query: no validation-only query is
#' added and nothing is collected on your behalf. Its execution-time
#' diagnostics keep the share's output name, the source summary name, and
#' the original public call, so they read like the local ones.
#'
#' A general dbplyr backend evaluates the source summary itself, so the rule
#' is the dialect's to apply rather than marginplyr's to read. Nothing of
#' yours is read to establish it: the staged query stays lazy,
#' [dplyr::show_query()] remains non-executing, and no query is run over your
#' data to improve an error.
#'
#' Which of two things a dialect does is what decides the case, and asking
#' takes at most two queries referencing none of your tables: a probe, and —
#' only when the probe is rejected — a control, which is what tells a dialect
#' that genuinely refuses apart from one whose connection or SQL scaffolding
#' failed and could not answer at all. An answer is a property of the dialect,
#' so it settles the case once and is reused for every later connection
#' carrying that dialect. Where it refuses an
#' ineligible summary, that refusal is the answer
#' and reaches you as the database's own diagnostic when [dplyr::collect()]
#' executes the staged query; the internal denominator column is named after
#' the summary to rewrite so that diagnostic is actionable. Where it converts
#' a value of another type to a number instead, it applies no rule at all and
#' no reading of your data would recover one, so the share is refused rather
#' than calculated from values nothing has checked —
#' `.check_share_source = FALSE` calculates it from sources you have
#' established yourself. A backend that cannot be asked, such as a
#' `dbplyr::simulate_*()` connection, is refused the same way.
#'
#' A question left unanswered refuses the share the same way, but nothing about
#' it is remembered. Neither outcome was read there, which is where a dropped
#' connection, a permissions failure, a dialect whose SQL scaffolding this
#' question lacks, and a query that could not be built against the connection
#' all end up — so nothing was established about the dialect, and the next
#' share request there asks again rather than inheriting one attempt's failure.
#' A connection that has recovered gets the verdict its dialect earns.
#'
#' Inside a transaction begun through DBI with RPostgres, the table-free
#' questions are isolated by a savepoint. Up to four additional transaction
#' control statements preserve earlier work and leave commit or rollback to
#' you. They read none of your tables and are included in [last_sent_queries()]
#' when auditing is enabled.
#'
#' Ordinary SQL aggregates return one value per grouping row. On DuckDB
#' and PostgreSQL, the result query checks a bare, single-argument
#' `UNNEST()` at the top level of a direct [dbplyr::sql()] share source, such
#' as `SUM(amount) + UNNEST(ARRAY[10])`. An array of zero or more than one
#' element causes a database error on execution. The check recognizes SQL
#' expressions with standard quotes and without comments. Other SQL
#' expressions must also return one value per grouping row.
#'
#' The portable value guarantee covers finite numbers, missing values, and
#' zero denominators. Infinite values and backend-specific `NaN`
#' representations are outside that guarantee because supported SQL dialects
#' do not share one portable finite-value predicate. Normalize potentially
#' non-finite summaries explicitly with operations supported by the backend.
#'
#' A function of your own may forward the name its caller wrote by injecting
#' it: `!!rlang::enquo(name)` is accepted wherever a bare name is, and so is
#' `!!rlang::ensym(name)`. Only the name is read. It is resolved among the
#' preceding summaries, as every bare name here is, so the environment
#' [rlang::enquo()] captured is not consulted and an injection carrying
#' anything but a name is refused exactly where writing that expression out
#' would be.
#'
#' @param x The bare name of one preceding eligible ordinary summary.
#'
#' @return A double vector when used inside [summarize_with_margins()].
#' @family contextual summary helpers
#' @seealso [summarize_with_margins()], the only verb these helpers can be
#'   used in; [rollup()], the only Grouping specification `share_of_parent()`
#'   accepts; and [cube()] and [grouping_sets()], which `share_of_total()`
#'   also accepts whenever the plan contains a Grand total set.
#' @export
#' @examples
#' # Both denominators in one call: the parent level, and the whole.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   revenue_share = share_of_parent(revenue),
#'   revenue_of_total = share_of_total(revenue),
#'   .by = c(year, month),
#'   .grouping = rollup(region, store)
#' )
#'
#' # A Total share accepts any plan containing the Grand total set, so a cube
#' # can report what fraction of the whole each cell is.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   revenue_of_total = share_of_total(revenue),
#'   .by = year,
#'   .grouping = cube(region, store)
#' )
#'
#' # Multiple measures use two ordered across() expressions. Selection for the
#' # second across() sees the ordinary summaries created by the first.
#' summarize_with_margins(
#'   .data = retail_sales,
#'   dplyr::across(c(units, revenue), sum),
#'   dplyr::across(
#'     c(units, revenue),
#'     share_of_total,
#'     .names = "{.col}_of_total"
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
#' # Missing fixed and included keys use missing-safe denominator lookup, and
#' # a missing fixed key is still its own partition. The Grouping bit
#' # distinguishes an included missing group from its subtotal.
#' missing_keys <- data.frame(
#'   partition = c(NA_character_, NA_character_, "B", "B"),
#'   group = c("x", NA_character_, "x", NA_character_),
#'   value = c(3, 1, 6, 2)
#' )
#' summarize_with_margins(
#'   .data = missing_keys,
#'   total = sum(value),
#'   share = share_of_parent(total),
#'   of_total = share_of_total(total),
#'   group_is_margin = grouping_bit(group),
#'   .by = partition,
#'   .grouping = rollup(group),
#'   .margin_label = NULL
#' )
#'
#' # Empty input without `.by` has one Grand total set row, whose share is
#' # one. With `.by`, there are no partitions; both results retain a double
#' # share column.
#' empty_keys <- missing_keys[0, ]
#' empty_grand_total <- summarize_with_margins(
#'   .data = empty_keys,
#'   total = sum(value),
#'   share = share_of_parent(total),
#'   .grouping = rollup(group)
#' )
#' c(
#'   rows = nrow(empty_grand_total),
#'   type = typeof(empty_grand_total$share)
#' )
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
#' # Rejected forms report their supported context or rewrite, naming the
#' # helper that was written.
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
#'     share_of_total,
#'     .names = "{.col}_share"
#'   ),
#'   .grouping = rollup(region, store)
#' ))
#'
#' # A Parent share needs one pure rollup(); a Total share needs a plan that
#' # contains the Grand total set.
#' try(summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   revenue_share = share_of_parent(revenue),
#'   .grouping = cube(region, store)
#' ))
#' try(summarize_with_margins(
#'   .data = retail_sales,
#'   revenue = sum(revenue),
#'   revenue_of_total = share_of_total(revenue),
#'   .grouping = grouping_sets(grouping_set(region), grouping_set(store))
#' ))
share_of_parent <- function(x) {
  abort_marginplyr(c(
    paste0(
      "{.fun share_of_parent} can only be used inside ",
      "{.fun summarize_with_margins} with a {.fun rollup}."
    ),
    i = paste0(
      "To derive a value from an existing Parent share, use a following ",
      "{.fun dplyr::mutate}."
    )
  ))
}

#' @rdname share_of_parent
#' @export
share_of_total <- function(x) {
  abort_marginplyr(c(
    paste0(
      "{.fun share_of_total} can only be used inside ",
      "{.fun summarize_with_margins} with a Grouping plan that contains the ",
      "Grand total set."
    ),
    i = paste0(
      "To derive a value from an existing Total share, use a following ",
      "{.fun dplyr::mutate}."
    )
  ))
}

# Returns the share kinds the call requests, which is what the verb needs to
# decide which grouping requirement to check before the plan is compiled.
preflight_shares <- function(dots) {
  dot_names <- names(dots)
  kinds <- character()

  for (i in seq_along(dots)) {
    quo <- dots[[i]]
    expr <- rlang::quo_get_expr(quo)
    output_name <- dot_names[[i]]

    if (is_share_helper_call(expr)) {
      validate_share_direct_syntax(expr, output_name)
      kinds <- c(kinds, share_helper_call_kind(expr))
      next
    }
    if (is_across_call(expr) && contains_share_helper(expr)) {
      preflight_share_across_syntax(expr, output_name)
      kinds <- c(kinds, share_across_kind(expr))
      next
    }
    kind <- share_expression_kind(expr)
    if (!is.null(kind)) {
      abort_share_helper_position(kind, complete = TRUE)
    }
  }

  unique(kinds)
}

abort_share_helper_position <- function(kind, complete) {
  # The first of four sites in this file that raise from two calls rather than
  # one, and the shape is the same at each: what varies between the two is a
  # whole element of the message vector, or a whole clause inside one. The
  # alternatives are both worse. An `if` inside the template is a branch the
  # structural gate never sees, since what it reads is the source expression
  # and this one would be inside a string; a message vector assembled around an
  # `if` is a computed template, which the gate does see and refuses; and an
  # `i` bullet whose content is sometimes empty renders as a bullet marker with
  # nothing after it. What the shape costs is the element the two calls share,
  # written out in each.
  if (complete) {
    abort_marginplyr(c(
      paste0(
        "{.fun {share_helper_name(kind)}} must be the complete right-hand ",
        "side of a named summary, or the direct {.arg .fns} argument of ",
        "{.fun across}."
      ),
      i = paste0(
        "Create the {share_kind_label(kind)} as its own named summary, then ",
        "use a following {.fun dplyr::mutate} for derived values."
      )
    ))
  }
  abort_marginplyr(paste0(
    "{.fun {share_helper_name(kind)}} must be the complete right-hand side ",
    "of a named summary, or the direct {.arg .fns} argument of ",
    "{.fun across}."
  ))
}

# Arrow rejects every contextual share for one reason — the numerator's source
# summary cannot be proved scalar before the query is built — so the rejection
# names whichever helpers the caller wrote rather than a fixed one. Its
# vocabulary lives here; the executor decides when to raise it.
abort_arrow_shares <- function(kinds) {
  kinds <- intersect(share_kind_names(), kinds)
  abort_marginplyr(c(
    paste0(
      "Arrow backends do not support {share_kind_label_plurals(kinds)} ",
      "because marginplyr cannot enforce their scalar-summary contract ",
      "safely before an Arrow query is constructed."
    ),
    i = "Other Arrow Margin operations remain supported.",
    i = paste0(
      "Omit {.fun {share_helper_names(kinds)}} or explicitly collect the ",
      "data before calling {.fun summarize_with_margins}."
    ),
    i = paste0(
      "Collection changes the default of {.arg .check_margin_label} from ",
      "{.code FALSE} to {.code TRUE}; set it explicitly when ",
      "re-running this call."
    )
  ))
}

# Only a Parent share can be refused from the Grouping specification alone.
# Whether a plan contains the Grand total set is a property of the compiled
# plan, so a Total share is checked there instead, by
# `check_share_grouping_kinds()`.
share_grouping_spec_validator <- function(kinds) {
  if (!"parent" %in% kinds) {
    return(NULL)
  }
  check_parent_grouping_spec
}

check_parent_grouping_spec <- function(grouping_spec) {
  # The field is read bare rather than through `grouping_spec_kind()`, because
  # `validate_grouping_spec_early()` has run by the time this validator is
  # called and is what establishes there is a kind to read.
  kind <- if (is.null(grouping_spec)) NULL else grouping_spec$type
  # Classified rather than compared as read (#317, ADR 0008): what this reader
  # holds is the caller's own specification and not a plan, whose kind field is
  # already the name.
  if (!identical(grouping_kind_name(kind), "rollup")) {
    abort_ambiguous_parent()
  }
  invisible(NULL)
}

# One refusal with two raising sites, because a Parent share can be refused
# from the Grouping specification alone and again from the compiled plan, and
# the caller has made one mistake either way. The flat form wrote the sentence
# out at both; nothing required that, and nothing requires it now. The
# structural gate reads the message argument of every `abort_marginplyr()` call
# in the namespace, so a literal at this call is in its view exactly as a
# literal at each site would be -- what the gate refuses is a template computed
# or bound elsewhere, which this is not.
#
# `call` defaults to the call of this function's own caller, so the refusal
# blames the site rather than this constructor, which is what raising it at
# each site does.
abort_ambiguous_parent <- function(call = rlang::caller_call()) {
  abort_marginplyr(
    c(
      paste0(
        "{.fun share_of_parent} requires {.arg .grouping} to be one pure ",
        "{.fun rollup}."
      ),
      i = paste0(
        "{.fun grouping_sets}, {.fun cube}, {.fun grouping_spec}, and other ",
        "Grouping specifications do not define one unambiguous parent."
      ),
      i = paste0(
        "Rewrite {.arg .grouping} as one {.fun rollup} or omit the Parent ",
        "share."
      )
    ),
    call = call
  )
}

# Returns only records confirmed as ordinary summary outputs. The caller has
# ordinary analysis records, whose frame argument candidates still need the
# frame's runtime value before they can establish an output name.
confirmed_share_records <- function(records) {
  Filter(function(record) {
    !identical(record$eligibility, "frame_candidate")
  }, records)
}

plan_share_expressions <- function(dots,
                                   selection_proxy,
                                   plan,
                                   set_id_name,
                                   validate_cardinality = FALSE,
                                   validate_ordinary_cardinality = FALSE,
                                   defer_local = FALSE) {
  stopifnot(is.list(dots))
  stopifnot(inherits(plan, "margin_grouping_plan"))
  dot_names <- names(dots)

  analyses <- analyze_ordinary_summaries(
    dots, selection_proxy, defer_local = defer_local
  )
  ordinary_records <- unlist(
    lapply(analyses, `[[`, "records"),
    recursive = FALSE
  )
  # Constructor argument names help diagnose a proposed source, but are not
  # confirmed outputs until its frame has been evaluated.
  certain_records <- confirmed_share_records(ordinary_records)
  ordinary_names <- vapply(
    certain_records,
    `[[`,
    character(1),
    "name"
  )
  ordinary_counts <- table(ordinary_names)
  planning_context <- list(
    all_records = ordinary_records,
    certain_records = certain_records,
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
  preceding_shares <- new_share_names()

  for (i in seq_along(dots)) {
    quo <- dots[[i]]
    expr <- rlang::quo_get_expr(quo)
    env <- rlang::quo_get_env(quo)
    output_name <- dot_names[[i]]

    if (is_share_helper_call(expr)) {
      request <- plan_direct_share(
        expr,
        output_name = output_name,
        preceding = preceding_ordinary,
        shares = preceding_shares,
        context = planning_context
      )
      planned_dots[[i]] <- rlang::new_quosure(
        NA_real_,
        env = rlang::empty_env()
      )
      requests <- c(requests, list(request))
      preceding_shares <- add_share_names(preceding_shares, request)
      next
    }

    if (is_across_call(expr) && contains_share_helper(expr)) {
      request <- plan_across_share(
        expr,
        env = env,
        output_name = output_name,
        preceding = preceding_ordinary,
        shares = preceding_shares,
        context = planning_context
      )
      if (length(request$outputs) == 0L) {
        planned_dots[i] <- list(NULL)
        next
      }
      planned_dots[[i]] <- share_placeholder(request$outputs)
      requests <- c(requests, list(request))
      preceding_shares <- add_share_names(preceding_shares, request)
      next
    }

    kind <- share_expression_kind(expr)
    if (!is.null(kind)) {
      abort_share_helper_position(kind, complete = FALSE)
    }

    share_dependency <- expression_alias_dependencies(
      expr,
      preceding_shares$names,
      env = env,
      mask = c(
        colnames(selection_proxy),
        vapply(preceding_ordinary, `[[`, character(1), "name"),
        preceding_shares$names
      ),
      reparented = analyses[[i]]$reparented
    )
    if (length(share_dependency) > 0L) {
      dependency <- share_dependency[[1L]]
      # Read only from the cli template below, which codetools cannot see.
      # Bound rather than written there because the expression does not fit a
      # template line, and splitting one across the `paste0()` that keeps the
      # line inside the margin would hide it from the reader entirely.
      # nolint start: object_usage_linter.
      dependency_label <- share_kind_label(share_name_kind(
        preceding_shares,
        dependency
      ))
      # nolint end
      abort_marginplyr(c(
        paste0(
          "Ordinary summaries cannot use an earlier {dependency_label} ",
          "({.var {dependency}}) in the same {.fun summarize_with_margins} ",
          "call."
        ),
        i = "Use a following {.fun dplyr::mutate} for derived values."
      ))
    }
    preceding_ordinary <- c(preceding_ordinary, analyses[[i]]$records)
  }

  if (length(requests) > 0L) {
    check_share_grouping_kinds(plan, share_request_kinds(requests))
  }

  cardinality <- if (isTRUE(validate_cardinality)) {
    share_cardinality_records(analyses, requests)
  } else {
    list()
  }
  ordinary_cardinality <- if (isTRUE(validate_ordinary_cardinality)) {
    share_ordinary_checks(analyses, requests)
  } else {
    list()
  }

  keep <- !vapply(planned_dots, is.null, logical(1))
  kept_dots <- planned_dots[keep]
  widths <- vapply(
    kept_dots,
    function(dot) {
      if (inherits(dot, "marginplyr_share_placeholders")) {
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
  for (i in seq_along(ordinary_cardinality)) {
    original_position <- as.character(ordinary_cardinality[[i]]$position)
    ordinary_cardinality[[i]]$position <- unname(
      flattened_positions[[original_position]]
    )
  }
  planned_dots <- unlist(
    lapply(
      kept_dots,
      function(dot) {
        if (inherits(dot, "marginplyr_share_placeholders")) {
          unclass(dot)
        } else {
          list(dot)
        }
      }
    ),
    recursive = FALSE
  )
  result <- list(
    dots = planned_dots,
    requests = requests,
    cardinality = cardinality,
    ordinary_cardinality = ordinary_cardinality,
    # Which incoming dot each planned dot came from, for the same reason
    # `flattened_positions` above exists: a share dot is dropped and a
    # placeholder expands into one dot per output, so a consumer holding one
    # value per dot the caller wrote cannot subscript it by position.
    origin_positions = rep(which(keep), widths)
  )
  if (defer_local) plan_local_across_checks(result) else result
}

# Add one check-only dot immediately after each later bare across source.
# The across call retains dplyr's expansion and function invocation; the new
# dot reads each produced summary from the same mask without adding output.
plan_local_across_checks <- function(plan) {
  positions <- unique(vapply(plan$cardinality, `[[`, integer(1), "position"))
  checked <- integer()
  checks_by_position <- list()
  for (position in positions) {
    quo <- plan$dots[[position]]
    expr <- rlang::quo_get_expr(quo)
    if (position == 1L || nzchar(rlang::names2(plan$dots)[[position]]) ||
          !rlang::is_call(expr, "across", ns = c("", "dplyr"))) next
    parsed <- parse_across_arguments(expr)
    if (length(parsed$additional) > 0L || !across_unpack_is_false(
      parsed$unpack, rlang::quo_get_env(quo)
    )) next
    checked <- c(checked, position)
    checks_by_position[[as.character(position)]] <- Filter(function(record) {
      record$position == position
    }, plan$cardinality)
  }
  if (length(checked) == 0L) return(plan)
  dots <- list()
  origins <- integer()
  for (position in seq_along(plan$dots)) {
    dots <- c(dots, plan$dots[position])
    origins <- c(origins, position)
    if (!position %in% checked) next
    checks <- checks_by_position[[as.character(position)]]
    values <- lapply(checks, function(check) {
      rlang::call2(base::`[[`, rlang::sym(".data"), check$source_summary)
    })
    validator <- rlang::new_quosure(rlang::call2(
      check_local_across_sources, rlang::call2(base::list, !!!values), checks
    ), rlang::quo_get_env(plan$dots[[position]]))
    dots <- c(dots, stats::setNames(list(validator), ""))
    origins <- c(origins, position)
  }
  remap <- function(record) {
    if (record$position %in% checked) record$post_across <- TRUE
    record$position <- match(record$position, origins)
    record
  }
  plan$dots <- dots
  plan$origin_positions <- plan$origin_positions[origins]
  plan$cardinality <- lapply(plan$cardinality, remap)
  plan$ordinary_cardinality <- lapply(plan$ordinary_cardinality, remap)
  plan
}

# Validate saved summary chunks before the following ordinary dot can run.
# NULL leaves dplyr's output shape and the caller's column order unchanged.
check_local_across_sources <- function(values, checks) {
  for (i in seq_along(checks)) {
    check <- checks[[i]]
    check_share_scalar(
      values[[i]], check$share_output, check$source_summary, check$share_kind
    )
  }
  NULL
}

# Ordinary summaries outside a share source can expand the staged grouped
# result after the source itself has passed its scalar check. Keep their
# caller-facing names for the dtplyr execution-time refusal.
share_ordinary_checks <- function(analyses, requests) {
  if (length(requests) == 0L) {
    return(list())
  }
  source_names <- share_source_names(requests)
  pair <- share_pairs(requests)[[1L]]
  records <- unlist(lapply(analyses, `[[`, "records"), recursive = FALSE)
  lapply(Filter(function(record) {
    !record$name %in% source_names &&
      identical(record$eligibility, "eligible") &&
      is.na(record$across_input)
  }, records), function(record) {
    list(
      position = record$position,
      summary = record$name,
      share_output = pair$output,
      share_kind = pair$kind
    )
  })
}

share_cardinality_records <- function(analyses, requests) {
  pairs <- share_pairs(requests)
  records <- unlist(
    lapply(analyses, `[[`, "records"),
    recursive = FALSE
  )
  # A candidate cannot identify the source expression to wrap at runtime.
  records <- confirmed_share_records(records)
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
      share_output = pair$output,
      source_summary = pair$source,
      share_kind = pair$kind,
      across_input = source_records[[1L]]$across_input,
      across_function = source_records[[1L]]$across_function,
      dependency_expr = source_records[[1L]]$dependency_expr
    )))
    seen_sources <- c(seen_sources, pair$source)
  }
  cardinality
}

# The backends whose ordinary summaries evaluate R code, so
# `wrap_share_sources()` below can put the eligible-type and cardinality rules
# inside the summary itself. The planner reads it to decide whether to wrap,
# and `check_wrapped_share_sources()` asserts it: a kind is asked nothing at
# execution only because it carries the rule in its own summary.
wraps_share_sources_in_summary <- function(backend_kind) {
  backend_kind %in% c("local", "dtplyr")
}

# The dots with each share's source expression wrapped in the check that
# applies the eligible-type and cardinality rules to it. Only dtplyr is given
# the caller's call here, and as text: its checks raise at `collect()`, after
# the verb call has returned, so nothing else can supply one. A local check
# raises inside the verb call, where `with_margin_error_call()` sets the call
# -- and staging one into the expression is worse than redundant there,
# because `dplyr::summarise()` defuses the staged dot a second time and
# expands whatever injection the caller's own spelling holds, in a mask where
# their wrapper's formal is not bound (#357).
wrap_share_sources <- function(dots,
                               cardinality,
                               call,
                               backend_kind) {
  cardinality <- Filter(function(record) !isTRUE(record$post_across),
                        cardinality)
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
    # The planned source retains across identity before local namespace
    # isolation replaces its runtime head with a function-value call.
    if (is_across_call(checks[[1L]]$dependency_expr)) {
      if (identical(backend_kind, "dtplyr")) {
        wrapped <- wrap_dtplyr_share_across(
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
      source_summaries <- vapply(
        checks,
        `[[`,
        character(1),
        "source_summary"
      )
      share_outputs <- stats::setNames(
        vapply(checks, `[[`, character(1), "share_output"),
        source_summaries
      )
      share_kinds <- stats::setNames(
        vapply(checks, `[[`, character(1), "share_kind"),
        source_summaries
      )
      wrapped <- rlang::call2(
        marginplyr_private_call("check_share_across"),
        expr,
        share_outputs = share_outputs,
        share_kinds = share_kinds
      )
    } else {
      check <- checks[[1L]]
      is_dtplyr <- identical(backend_kind, "dtplyr")
      validator <- if (is_dtplyr) {
        dtplyr_share_private_call("check_dtplyr_share_source")
      } else {
        marginplyr_private_call("check_share_scalar")
      }
      wrapped <- rlang::call2(
        validator,
        expr,
        share_output = check$share_output,
        source_summary = check$source_summary,
        share_kind = check$share_kind,
        !!!if (is_dtplyr) list(call_text = share_call_text(call))
      )
    }
    dots[[position]] <- rlang::new_quosure(
      wrapped,
      env = wrapped_summary_env(quo)
    )
  }
  dots
}

# Wrap each resolved direct ordinary dot with its cardinality check. Each
# check names a position in `dots` and the share that requires scalar rows.
wrap_dtplyr_share_ordinary <- function(dots, checks, call) {
  for (check in checks) {
    position <- check$position
    quo <- dots[[position]]
    expr <- rlang::quo_get_expr(quo)
    wrapped <- rlang::call2(
      dtplyr_share_private_call("check_dtplyr_share_ordinary"),
      expr,
      summary = check$summary,
      share_output = check$share_output,
      share_kind = check$share_kind,
      call_text = share_call_text(call)
    )
    dots[[position]] <- rlang::new_quosure(
      wrapped,
      env = wrapped_summary_env(quo)
    )
  }
  dots
}

# Return one ordinary summary value for one grouped row. The caller supplies
# the evaluated value and the share request whose result needs that shape.
check_dtplyr_share_ordinary <- function(value, summary, share_output,
                                        share_kind, call_text) {
  if (NROW(value) != 1L) {
    abort_marginplyr(
      c(
        paste0(
          "{share_kind_label(share_kind)} {.var {share_output}} cannot be ",
          "calculated because ordinary summary {.var {summary}} expanded ",
          "the grouped result."
        ),
        i = paste0(
          "When requesting a share, return one value per grouping row ",
          "from {.var {summary}}."
        )
      ),
      class = "marginplyr_share_cardinality_error",
      share_output = share_output,
      expanded_summary = summary,
      call = parse_call_text(call_text)
    )
  }
  value
}

wrap_dtplyr_share_across <- function(expr, checks, call) {
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
  fns_is_list <- rlang::is_call(parsed$fns, "list")
  if (fns_is_list) {
    functions <- static_call_args(parsed$fns)
    function_names <- names(functions)
  } else {
    functions <- list(parsed$fns)
    function_names <- ""
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
    share_outputs <- vapply(
      function_checks[keep],
      `[[`,
      character(1),
      "share_output"
    )
    source_summaries <- vapply(
      function_checks[keep],
      `[[`,
      character(1),
      "source_summary"
    )
    share_kinds <- vapply(
      function_checks[keep],
      `[[`,
      character(1),
      "share_kind"
    )
    functions[[function_index]] <- wrap_dtplyr_share_function(
      functions[[function_index]],
      mapping = new_share_validation_mapping(
        inputs = inputs,
        share_outputs = share_outputs,
        source_summaries = source_summaries,
        share_kinds = share_kinds
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
  call_args[[fns_index]] <- wrapped_fns
  if (can_inline_forwarded && length(forwarded_positions) > 0L) {
    call_args <- call_args[-forwarded_positions]
  }
  rebuild_static_call(expr, call_args)
}

# The argument a dtplyr lambda binds to each column it is mapped over. Named
# as a string rather than written bare, so that static analysis reads the
# pronoun as data rather than as a variable this function expects to find.
dtplyr_lambda_pronoun <- function() {
  rlang::sym(".x")
}

inline_dtplyr_forwarded_fn <- function(fn, forwarded_args) {
  rlang::call2(
    "~",
    rlang::call2(
      fn,
      dtplyr_lambda_pronoun(),
      !!!forwarded_args
    )
  )
}

wrap_dtplyr_share_function <- function(fn, mapping, forwarded_args, call) {
  value <- if (rlang::is_call(fn, "~")) {
    fn[[2L]]
  } else {
    rlang::call2(
      fn,
      dtplyr_lambda_pronoun(),
      !!!forwarded_args
    )
  }
  input <- rlang::call2(
    dtplyr_share_private_call("dtplyr_share_input_name"),
    dtplyr_lambda_pronoun()
  )
  mapping_expr <- rlang::call2(
    dtplyr_share_private_call("new_share_validation_mapping"),
    inputs = mapping$inputs,
    share_outputs = mapping$share_outputs,
    source_summaries = mapping$source_summaries,
    share_kinds = mapping$share_kinds
  )
  validator <- rlang::call2(
    dtplyr_share_private_call("check_dtplyr_share_scalar"),
    value,
    input = input,
    mapping = mapping_expr,
    call_text = share_call_text(call)
  )
  rlang::call2("~", validator)
}

# Alias supported special inputs after planning and across translation.
# Lexical scopes keep their own names; uncertain ownership is refused.
protect_dtplyr_share_measure <- function(step) {
  if (inherits(step, "dtplyr_step_group")) {
    step$parent <- protect_dtplyr_share_measure(step$parent)
    return(step)
  }
  validators <- c("check_dtplyr_share_source", "check_dtplyr_share_scalar",
                  "check_dtplyr_share_ordinary")
  if (!inherits(step, "dtplyr_step_subset") ||
        !any(all.names(step$j) %in% validators)) {
    return(step)
  }
  measures <- intersect(
    step$parent$vars, c(".BY", ".SD", ".GRP", ".NGRP", ".SDall", ".xSD", ".iSD")
  )
  if (!length(measures)) {
    return(step)
  }
  margin_safe_column_reads(
    step$parent, measures,
    function(source, safe_name) {
      step$parent <- source
      for (measure in measures) {
        step$j <- rebuild_static_call(step$j, lapply(
          static_call_args(step$j), rewrite_dtplyr_share_measure,
          alias = safe_name(measure), measure = measure
        ))
      }
      step
    },
    reserved = step$vars
  )
}

# Whether evaluated syntax may bind or remove the input name in its scope.
# Function literals establish a separate scope; unresolved assignments count.
dtplyr_share_binds_measure <- function(expr, measure = ".BY") {
  expr <- unparenthesized_value(expr)
  if (!rlang::is_call(expr)) {
    return(FALSE)
  }
  if (is_generated_dtplyr_share_call(expr)) {
    return(any(vapply(static_call_args(expr), dtplyr_share_binds_measure,
                      logical(1), measure = measure)))
  }
  name <- static_call_name(expr)
  if (identical(name, "function")) {
    return(FALSE)
  }
  callee <- resolved_callee_name(expr, name)
  head <- unparenthesized_value(static_call_head(expr))
  if (identical(callee, "do.call") ||
        (is.null(callee) && !identical(static_call_name(head), "function"))) {
    return(TRUE)
  }
  assigned <- lookup_assignment_names(expr)
  mutation <- scope_mutation_call(expr, character(), NULL)
  if (!is.null(mutation) &&
        identical(static_call_name(mutation), "list2env")) {
    # Translated syntax carries no captured callee environment, so unqualified
    # key constructors cannot establish which names this setter replaces.
    assigned <- c(assigned, list2env_setter_names(mutation))
  }
  if (any(c(measure, unresolved_lookup_name()) %in% assigned)) {
    return(TRUE)
  }
  if (!is.null(name) && name %in% c("rm", "remove") &&
        !measure %in% removal_retained_bound(expr, measure)) {
    return(TRUE)
  }
  parts <- searched_call_parts(expr)
  parts <- parts[!vapply(parts, rlang::is_missing, logical(1))]
  any(vapply(parts, dtplyr_share_binds_measure, logical(1), measure = measure))
}

# Replace translated special column reads, retaining lexical scopes, field names
# and captures. Refuse scopes with unresolved input/lexical reads.
# Freeze the generated across input-name read before aliasing so its
# validation mapping continues to name the caller's selected column.
rewrite_dtplyr_share_measure <- function(expr, alias, measure = ".BY") {
  if (rlang::is_symbol(expr, measure)) {
    return(rlang::sym(alias))
  }
  if (!rlang::is_call(expr)) {
    return(expr)
  }
  if (is_generated_dtplyr_share_call(expr)) {
    if (identical(expr[[1L]], dtplyr_share_private_call(
      "dtplyr_share_input_name"
    ))) {
      return(deparse(expr[[2L]]))
    }
    args <- lapply(static_call_args(expr), function(part) {
      rewrite_dtplyr_share_measure(part, alias, measure)
    })
    return(rebuild_static_call(expr, args))
  }
  name <- static_call_name(expr)
  callee <- resolved_callee_name(expr, name)
  # An explicit mask can own the name or fall back to this input. Neither its
  # bindings nor its type can be established by running caller code here.
  if (!is.null(callee) &&
        callee %in% c("eval", "evalq", "with", "eval_tidy", "eval_bare") &&
        length(static_call_args(expr)) > 1L &&
        any(c(measure, unresolved_lookup_name()) %in%
              expression_data_symbols(expr))) {
    abort_marginplyr(c(
      paste0(
        "dtplyr summaries with shares cannot safely distinguish input ",
        "and explicit evaluation environment {.code {measure}} reads."
      ),
      i = paste0("Rename the input column {.code {measure}} ",
                 "or evaluate this summary separately.")
    ))
  }
  if (identical(name, "function")) {
    parts <- static_call_args(expr)
    if (measure %in% names(parts[[1L]])) {
      return(expr)
    }
    values <- c(function_formal_defaults(expr), parts[2L])
    values <- values[!vapply(values, rlang::is_missing, logical(1))]
    rebinds <- any(vapply(
      values, dtplyr_share_binds_measure, logical(1), measure = measure
    ))
  } else {
    rebinds <- dtplyr_share_binds_measure(expr, measure)
  }
  if (rebinds) {
    reads <- expression_data_symbols(expr)
    if (any(c(measure, unresolved_lookup_name()) %in% reads)) {
      abort_marginplyr(c(
        paste0(
          "dtplyr summaries with shares cannot safely distinguish input ",
          "and lexical {.code {measure}} reads in this expression."
        ),
        i = "Rename the lexical binding or the input column {.code {measure}}."
      ))
    }
    return(expr)
  }
  if (!is.null(name) && name %in% c("$", "@")) {
    args <- static_call_args(expr)
    args[1L] <- list(rewrite_dtplyr_share_measure(args[[1L]], alias, measure))
    return(rebuild_static_call(expr, args))
  }
  rewrite_evaluated_call_parts(expr, function(part) {
    rewrite_dtplyr_share_measure(part, alias, measure)
  })
}

check_dtplyr_share_scalar <- function(value,
                                      input,
                                      mapping,
                                      call_text) {
  position <- match(input, mapping$inputs)
  if (is.na(position)) {
    return(value)
  }
  check_dtplyr_share_source(
    value,
    share_output = mapping$share_outputs[[position]],
    source_summary = mapping$source_summaries[[position]],
    share_kind = mapping$share_kinds[[position]],
    call_text = call_text
  )
}

check_dtplyr_share_source <- function(value,
                                      share_output,
                                      source_summary,
                                      share_kind,
                                      call_text) {
  check_share_scalar(
    value,
    share_output = share_output,
    source_summary = source_summary,
    share_kind = share_kind,
    # Read at `collect()`, where no frame above can answer a failure. A text
    # this cannot read costs the caller the call their diagnostic names, and
    # must not cost them the diagnostic itself (ADR 0015, #360).
    call = parse_call_text(call_text)
  )
}

dtplyr_share_input_name <- function(value) {
  deparse(substitute(value))
}

new_share_validation_mapping <- function(inputs,
                                         share_outputs,
                                         source_summaries,
                                         share_kinds) {
  stopifnot(
    length(inputs) == length(share_outputs),
    length(inputs) == length(source_summaries),
    length(inputs) == length(share_kinds)
  )
  list(
    inputs = inputs,
    share_outputs = share_outputs,
    source_summaries = source_summaries,
    share_kinds = share_kinds
  )
}

check_share_across <- function(value, share_outputs, share_kinds) {
  for (source_summary in names(share_outputs)) {
    check_share_scalar(
      value[[source_summary]],
      share_output = share_outputs[[source_summary]],
      source_summary = source_summary,
      share_kind = share_kinds[[source_summary]]
    )
  }
  value
}

# The rules a share's source must satisfy, applied to one value. `call` is
# dtplyr's alone: only its check raises after the verb call has returned, so
# only it has to name the call itself. Everywhere else this raises inside the
# operation, which names it.
check_share_scalar <- function(value,
                               share_output,
                               source_summary,
                               share_kind,
                               call = NULL) {
  if (length(value) != 1L) {
    abort_marginplyr(
      c(
        paste0(
          "{share_kind_label(share_kind)} {.var {share_output}} requires ",
          "source summary {.var {source_summary}} to return exactly one ",
          "value per grouping row."
        ),
        i = paste0(
          "Define {.var {source_summary}} as one scalar summary; for ",
          "multiple statistics, create separate named summaries and a ",
          "{share_kind_label(share_kind)} for each one."
        )
      ),
      class = "marginplyr_share_cardinality_error",
      share_output = share_output,
      source_summary = source_summary,
      call = call
    )
  }
  if (!is_share_source_type(value)) {
    abort_share_source_type(
      value,
      share_output = share_output,
      source_summary = source_summary,
      share_kind = share_kind,
      call = call
    )
  }
  value
}

# The eligible-type rule and its diagnostic have two raising sites — the check
# wrapped around each summary expression, and the local backend's re-check of
# the collected result. They share one definition so the handler-visible fields
# cannot drift away from the message again.
is_share_source_type <- function(value) {
  typeof(value) %in% c("integer", "double") &&
    !is.object(value) && is.null(dim(value))
}

abort_share_source_type <- function(value,
                                    share_output,
                                    source_summary,
                                    share_kind,
                                    call) {
  # Joined here rather than interpolated as a vector, because the slash is how
  # a class vector is spelled rather than a list of subjects cli's defaults
  # would serialise with an `and`.
  #
  # Bound rather than written in the template for a reason the structural gate
  # cannot enforce: an `if` inside a template is a branch the gate never sees,
  # because what it reads is the source expression and this one would be inside
  # a string. Keeping the branch in R is what leaves it reviewable at all.
  # Read only from the template below, which codetools cannot see.
  # nolint start: object_usage_linter.
  detected_type <- paste(
    if (is.object(value) || !is.null(dim(value))) {
      class(value)
    } else {
      typeof(value)
    },
    collapse = "/"
  )
  # nolint end
  abort_marginplyr(
    c(
      paste0(
        "{share_kind_label(share_kind)} {.var {share_output}} requires ",
        "source summary {.var {source_summary}} to be a plain integer or ",
        "double scalar."
      ),
      # Alone in a bullet per ADR 0023's condition 2: a class vector is as long
      # as the value's class chain, which the caller's data decides.
      i = "Detected type {detected_type}.",
      i = "Convert it explicitly in the ordinary summary."
    ),
    share_output = share_output,
    source_summary = source_summary,
    call = call
  )
}

# The caller's call as text a dtplyr share can carry to `collect()`. What it
# returns parses: `check_dtplyr_share_source()` reads it back where nothing can
# answer a failure.
#
# `deparse()` is not total over calls. `do.call()` records the evaluated
# arguments, so a call reaching here can hold the input itself -- a
# `data.table`, whose `.internal.selfref` externalptr `deparse()` writes as
# `<pointer: ...>` -- or an environment, neither of which R can write as
# source. The parts that fail are replaced before the call is written, rather
# than the read being allowed to fail (#360).
share_call_text <- function(call) {
  text <- deparse_call_text(writable_call(call))
  # An invariant, not a Package condition (ADR 0015): `writable_call()` answers
  # every part that does not read back, so a text that still does not is a
  # defect here. It is stated at the site that writes the text, inside the verb
  # call, because the site that reads it is past the point where stopping is
  # affordable.
  stopifnot(call_text_reads_back(text))
  text
}

# One expression as the source that would write it.
deparse_call_text <- function(expr) {
  paste(deparse(expr, width.cutoff = 500L), collapse = "\n")
}

# Whether `text` can be read back as one expression.
#
# Asked instead of testing `parse_call_text()` for `NULL`, because `NULL` is
# also what the text `"NULL"` reads as. A caller's `.margin_label = NULL` is a
# part `deparse()` writes perfectly well, and conflating the two answers put a
# name in its place.
call_text_reads_back <- function(text) {
  tryCatch(
    {
      str2lang(text)
      TRUE
    },
    error = function(cnd) FALSE
  )
}

# The call a text stands for, or `NULL` where the text cannot be read as one.
# The two answers are one answer at the only site that asks: a text this cannot
# read and a text reading as `NULL` both leave the condition no call to name.
parse_call_text <- function(text) {
  tryCatch(str2lang(text), error = function(cnd) NULL)
}

# `expr` -- a call -- with every part `deparse()` cannot write as source
# replaced by a name spelling the class of what stood there. Only the parts
# that fail are touched, which is why the test is a trial rather than a type: a
# `data.frame` and a Grouping specification both round-trip, and replacing them
# would thin a call that reads correctly today.
#
# A symbol is answered before the trial. `deparse()` writes a non-syntactic
# name bare when it is asked for one alone -- `unit count`, which does not
# parse -- and backquotes it inside the call it is written in, so trying one on
# its own reports a failure the text this produces does not have.
writable_call <- function(expr) {
  if (rlang::is_symbol(expr) || deparse_round_trips(expr)) {
    return(expr)
  }
  if (rlang::is_call(expr)) {
    for (position in seq_along(expr)) {
      # The empty argument is not a value, and standing a name in for it would
      # write an argument the caller did not pass (#351).
      if (rlang::is_missing(expr[[position]])) {
        next
      }
      part <- writable_call(expr[[position]])
      # `expr[[position]] <- NULL` deletes the argument rather than setting it,
      # and `NULL` comes back from the walk only when the caller wrote one --
      # every replacement is a name. So the argument is left where it is.
      if (!is.null(part)) {
        expr[[position]] <- part
      }
    }
    if (deparse_round_trips(expr)) {
      return(expr)
    }
  }
  unwritable_name(expr)
}

# Whether `deparse()` writes `expr` as source that reads back as an expression.
deparse_round_trips <- function(expr) {
  text <- tryCatch(deparse_call_text(expr), error = function(cnd) NULL)
  !is.null(text) && call_text_reads_back(text)
}

# What stands in a call for a value `deparse()` cannot write as source. A name
# rather than a string, so that the part reads as the position it occupies, and
# non-syntactic so that no caller's own spelling collides with it.
unwritable_name <- function(value) {
  rlang::sym(paste0("<", class(value)[[1L]], ">"))
}

# Whether every share source is among the preceding explicitly named summaries.
# The caller supplies preflighted syntax. Prediction runs no caller code.
shares_use_named_summaries <- function(dots) {
  preceding <- character()
  for (i in seq_along(dots)) {
    expr <- rlang::quo_get_expr(dots[[i]])
    if (!contains_share_helper(expr)) {
      preceding <- c(preceding, rlang::names2(dots)[[i]])
      next
    }
    source <- if (is_share_helper_call(expr)) {
      unwrap_injected_args(static_call_args(expr))[[1L]]
    } else {
      parse_across_arguments(expr)$cols
    }
    if (!share_selects_named_summaries(
      source, preceding, rlang::quo_get_env(dots[[i]])
    )) {
      return(FALSE)
    }
  }
  TRUE
}

# Recognize explicit positive selections without evaluating their expressions.
# Inside all_of/any_of, symbols are values and c() uses ordinary lookup.
share_selects_named_summaries <- function(expr, preceding, env,
                                          literal = FALSE) {
  if (rlang::is_quosure(expr)) {
    return(share_selects_named_summaries(
      rlang::quo_get_expr(expr), preceding, rlang::quo_get_env(expr), literal
    ))
  }
  if (is_name_part(expr)) {
    return(!literal && rlang::as_name(expr) %in% preceding)
  }
  if (is.character(expr)) {
    return(!anyNA(expr) && all(expr %in% preceding))
  }
  if (rlang::is_call(expr, c("c", "("))) {
    if (literal && rlang::is_call(expr, "c") &&
          !identical(ordinary_function_binding("c", env), base::c)) {
      return(FALSE)
    }
    return(all(vapply(
      static_call_args(expr), share_selects_named_summaries, logical(1),
      preceding = preceding, env = env, literal = literal
    )))
  }
  name <- static_call_name(expr)
  if (is.null(name) || !name %in% c("all_of", "any_of")) {
    return(FALSE)
  }
  namespace <- static_call_ns(expr)
  known <- if (is.null(namespace)) {
    identical(
      ordinary_function_binding(name, env, owners = c("dplyr", "tidyselect")),
      getExportedValue("tidyselect", name)
    )
  } else {
    namespace %in% c("dplyr", "tidyselect")
  }
  args <- static_call_args(expr)
  known && length(args) == 1L && share_selects_named_summaries(
    args[[1L]], preceding, env, literal = TRUE
  )
}

# Records ordinary outputs and dependencies for a call with a contextual share.
analyze_ordinary_summaries <- function(dots, selection_proxy,
                                       defer_local = FALSE) {
  dot_names <- names(dots)
  analyses <- vector("list", length(dots))
  preceding_names <- character()
  reparented <- list()
  independent_shares <- defer_local && shares_use_named_summaries(dots)
  for (i in seq_along(dots)) {
    quo <- dots[[i]]
    output_name <- dot_names[[i]]
    expr <- rlang::quo_get_expr(quo)
    env <- rlang::quo_get_env(quo)
    if (contains_share_helper(expr)) {
      analyses[[i]] <- list(records = list())
      next
    }
    reparented <- c(reparented, lookup_reparented_environments(
      expr, env, c(colnames(selection_proxy), preceding_names)
    ))

    # Local dplyr resolves selections after preceding summaries have entered
    # its mask. Predict only literal names here, leaving naming expressions
    # and selection predicates to that execution.
    if (defer_local && contains_summary_selection(expr) &&
          (i > 1L || nzchar(output_name) || independent_shares)) {
      output_names <- if (nzchar(output_name)) {
        output_name
      } else if (is_across_call(expr)) {
        predictable_local_across_names(list(quo), c(
          names(selection_proxy), preceding_names
        ))
      } else {
        character()
      }
      eligibility <- if (is_across_call(expr) && nzchar(output_name)) {
        "named_across"
      } else if (nzchar(output_name) || is_across_call(expr)) {
        "eligible"
      } else {
        "expanded"
      }
      available <- unique(c(names(selection_proxy), preceding_names))
      proxy <- vctrs::new_data_frame(
        stats::setNames(rep(list(NA), length(available)), available),
        n = 1L
      )
      analyses[[i]] <- list(records = ordinary_summary_records(
        expr, env, proxy, output_names, output_name,
        eligibility, i, preceding_names, reparented, deferred_selection = TRUE
      ), reparented = reparented)
      preceding_names <- c(preceding_names, output_names)
      next
    }

    if (nzchar(output_name)) {
      output_names <- output_name
      eligibility <- if (is_across_call(expr)) "named_across" else "eligible"
    } else if (is_across_call(expr)) {
      output_names <- if (defer_local) {
        tryCatch(
          known_across_output_names(expr, env, selection_proxy),
          vctrs_error_subscript_oob = function(cnd) character()
        )
      } else {
        known_across_output_names(expr, env, selection_proxy)
      }
      eligibility <- "eligible"
    } else {
      output_names <- if (identical(
        data_frame_valued_summary_kind(expr), "frame"
      )) {
        frame_argument_candidates(expr, env)
      } else if (defer_local) {
        tryCatch(
          known_data_frame_output_names(expr, env, selection_proxy),
          vctrs_error_subscript_oob = function(cnd) character()
        )
      } else {
        known_data_frame_output_names(expr, env, selection_proxy)
      }
      eligibility <- if (identical(
        data_frame_valued_summary_kind(expr), "frame"
      )) "frame_candidate" else "expanded"
    }

    records <- ordinary_summary_records(
      expr, env, selection_proxy, output_names, output_name,
      eligibility, i, preceding_names, reparented
    )
    analyses[[i]] <- list(records = records, reparented = reparented)
    # An argument name alone cannot establish a preceding alias dependency.
    if (!identical(eligibility, "frame_candidate")) {
      preceding_names <- c(preceding_names, output_names)
    }
  }

  analyses
}

# Records each known ordinary output against the aliases preceding its call.
# The proxy includes those aliases when local selections have been deferred.
# Deferred selections report their actual inputs during summary evaluation.
# An output with known across provenance has an explicit column argument.
# Unknown provenance retains the whole call's dependencies.
ordinary_summary_records <- function(expr, env, proxy, output_names,
                                     output_name, eligibility, position,
                                     preceding_names, reparented,
                                     deferred_selection = FALSE) {
  expands <- is_across_call(expr) && !nzchar(output_name) &&
    length(output_names) > 0L
  provenance <- across_output_provenance(
    expr, env, proxy, output_names, expands_own_names = expands
  )
  selected <- if (expands) {
    known_across_source_names(expr, env, proxy)
  } else {
    character()
  }
  Map(function(name, across_input, across_function) {
    dependency_expr <- expr
    inputs <- selected
    if (!is.na(across_input)) {
      parsed <- parse_across_arguments(expr)
      args <- parsed$call_args
      args[[parsed$cols_index]] <- rlang::sym(across_input)
      if (is_known_function_list_call(parsed$fns, env)) {
        args[parsed$fns_index] <- static_call_args(parsed$fns)[across_function]
      }
      dependency_expr <- rebuild_static_call(expr, args)
      inputs <- across_input
    }
    dependencies <- unique(c(
      expression_alias_dependencies(
        dependency_expr, preceding_names, env,
        c(colnames(proxy), preceding_names), reparented = reparented,
        deferred_selection = deferred_selection
      ),
      intersect(inputs, preceding_names)
    ))
    list(
      name = name,
      position = position,
      eligibility = eligibility,
      dependencies = dependencies,
      dependency_expr = dependency_expr,
      across_input = across_input,
      across_function = across_function
    )
  }, output_names, provenance$inputs, provenance$functions)
}

# Which selected column and which `.fns` entry produced each output name. Only
# an `across()` that expanded its own names has that correspondence: one name
# per (column, function) pair, in that order. A named `across()` packs every
# pair into the single column the caller named, so its one name came from all
# of them and from none in particular; recording `NA` there rather than zipping
# the two lists is what keeps that name from being multiplied into one record
# per selected column (#105). A single-column named `across()` is why this
# takes the caller's naming rather than comparing counts: one output and one
# column agree in count while still standing for a pack.
across_output_provenance <- function(expr,
                                     env,
                                     data_proxy,
                                     output_names,
                                     expands_own_names) {
  unknown <- list(
    inputs = rep(NA_character_, length(output_names)),
    functions = rep(NA_integer_, length(output_names))
  )
  if (!expands_own_names) {
    return(unknown)
  }

  inputs <- known_across_source_names(expr, env, data_proxy)
  function_count <- length(known_across_function_names(
    parse_across_arguments(expr), env
  ))
  # A `.names` template the analysis could not expand leaves the output names
  # unknown, so the pairs it would have named cannot be matched to them.
  if (length(inputs) * function_count != length(output_names)) {
    return(unknown)
  }

  list(
    inputs = rep(inputs, each = function_count),
    functions = rep(seq_len(function_count), times = length(inputs))
  )
}

# The share outputs written so far, each with its kind: a diagnostic naming
# one of them names it in the caller's terms, and only that output's own kind
# can supply the words.
new_share_names <- function() {
  list(names = character(), kinds = character())
}

add_share_names <- function(shares, request) {
  list(
    names = c(shares$names, request$outputs),
    kinds = c(shares$kinds, rep(request$kind, length(request$outputs)))
  )
}

share_name_kind <- function(shares, name) {
  shares$kinds[[match(name, shares$names)]]
}

plan_direct_share <- function(expr,
                              output_name,
                              preceding,
                              shares,
                              context) {
  args <- validate_share_direct_syntax(expr, output_name)
  source <- rlang::as_name(args[[1L]])
  kind <- share_helper_call_kind(expr)
  validate_share_request(
    outputs = output_name,
    sources = source,
    preceding = preceding,
    shares = shares,
    context = context,
    kind = kind
  )
  list(outputs = output_name, sources = source, kind = kind)
}

validate_share_direct_syntax <- function(expr, output_name) {
  # Read only from the cli templates below, which codetools cannot see. Bound
  # rather than written in them because the expression does not fit beside the
  # output name in a template line.
  # nolint start: object_usage_linter.
  helper <- share_helper_name(share_helper_call_kind(expr))
  # nolint end
  if (!nzchar(output_name)) {
    abort_marginplyr(c(
      "A direct {.fun {helper}} summary must have an explicit output name.",
      i = "Rewrite it as {.code name = {helper}(source)}."
    ))
  }
  args <- static_call_args(expr)
  # Read through an injected quosure for the reason `unwrap_injected_quosure()`
  # gives, and asked with `is_name_part()` rather than `rlang::is_symbol()` so
  # that the question here is the same question `grouping_helper_vars()` asks of
  # a bare grouping column. One answer covers all four helpers, which is what
  # #169 asks for.
  #
  # The second half of that question is not inert here, and the arity is what
  # makes it reachable: `share_of_total(, )` is two arguments to the parser and
  # is refused for the count, but `share_of_total(x = )` is *one* argument and
  # it is empty. ADR 0019's *an injected name is read for the name it carries*
  # records what asking it with `is_name_part()` changed here, and why the
  # alternative was rejected.
  carried <- unwrap_injected_args(args)
  if (length(carried) != 1L || !is_name_part(carried[[1L]])) {
    # `injected_quosure_clause()` is a whole sentence assembled around a
    # deparsed caller expression, so it stays an interpolated value and gains
    # no markup: ADR 0023's injection rule is what makes a caller's braces
    # inert, and it holds because cli reads the template and not the value.
    #
    # It carries its own leading space and is empty at a call that injected
    # nothing, so it follows the remedy inside that bullet rather than taking
    # one of its own, which would sometimes be a bullet marker with nothing
    # after it. That is also where the flat form put it, so the clause still
    # ends the message and every pin reading it composes as it did.
    # `R/utils.R` re-authors it in a later group of #223's phase 3.
    abort_marginplyr(c(
      paste0(
        "{.code {output_name} = {helper}(...)} requires exactly one bare ",
        "name of a preceding ordinary summary."
      ),
      i = paste0(
        "Define the scalar summary first, then pass its name directly to ",
        "{.fun {helper}}.",
        "{injected_quosure_clause(args)}"
      )
    ))
  }
  carried
}

plan_across_share <- function(expr,
                              env,
                              output_name,
                              preceding,
                              shares,
                              context) {
  kind <- share_across_kind(expr)
  syntax <- validate_share_across_syntax(expr, env, output_name)
  args <- syntax$args
  names_template <- syntax$names_template

  selectable <- vapply(
    preceding,
    function(record) {
      identical(record$eligibility, "eligible") &&
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
  sources <- resolve_share_selection(
    args$cols,
    env = env,
    preceding_names = preceding_names,
    preceding = preceding,
    context = context,
    kind = kind,
    error_call = expr
  )
  outputs <- expand_across_name(
    names_template, sources, "1", env, recycle_scalar = TRUE
  )

  validate_share_request(
    outputs = outputs,
    sources = sources,
    preceding = preceding,
    shares = shares,
    context = context,
    kind = kind
  )
  list(outputs = outputs, sources = sources, kind = kind)
}

validate_share_across_syntax <- function(expr, env, output_name) {
  kind <- share_across_kind(expr)
  args <- preflight_share_across_syntax(expr, output_name)
  # The parsed value also represents an omitted or empty argument as NULL.
  unpack_is_literal_null <- args$unpack_index > 0L &&
    is.null(args$call_args[[args$unpack_index]])
  if (!is.null(args$unpack) || unpack_is_literal_null) {
    unpack <- rlang::eval_tidy(args$unpack, env = env)
    if (!isFALSE(unpack)) {
      abort_share_across_unpack(kind)
    }
  }
  names_template <- rlang::eval_tidy(args$names, env = env)
  if (
    !is.character(names_template) ||
      length(names_template) != 1L ||
      is.na(names_template)
  ) {
    abort_marginplyr(paste0(
      "{share_kind_modifier(kind)} {.fun across} {.arg .names} must be one ",
      "non-missing character template."
    ))
  }
  list(args = args, names_template = names_template)
}

preflight_share_across_syntax <- function(expr, output_name) {
  kind <- share_across_kind(expr)
  if (nzchar(output_name)) {
    abort_marginplyr(c(
      paste0(
        "An {.fun across} {share_kind_modifier(kind)} expression must be ",
        "unnamed."
      ),
      i = "Use its required {.arg .names} argument to name the output columns."
    ))
  }
  args <- parse_across_arguments(expr)
  if (!is_share_helper_function(args$fns)) {
    # Read only from the cli template below, which codetools cannot see. Bound
    # rather than written there because the template names it twice, once
    # qualified, and neither spelling fits beside the other on a line.
    # nolint start: object_usage_linter.
    helper <- share_helper_name(kind)
    # nolint end
    abort_marginplyr(c(
      paste0(
        "For {share_kind_label(kind)}s, {.fun across} {.arg .fns} must be ",
        "{.code {helper}} or {.code marginplyr::{helper}} directly."
      ),
      i = paste0(
        "Use two ordered {.fun across} expressions instead of a formula, ",
        "anonymous function, or function list."
      )
    ))
  }
  if (length(args$additional) > 0L) {
    # The arguments arrive alone in an `i` bullet, per ADR 0023's condition 2:
    # how many of them there are is the caller's decision.
    abort_marginplyr(c(
      paste0(
        "{share_kind_modifier(kind)} {.fun across} does not accept ",
        "additional function arguments:"
      ),
      i = "{.arg {args$additional}}.",
      i = "Put missing-value handling in the preceding ordinary summary."
    ))
  }
  if (is.null(args$names)) {
    # `{{` is glue's escape for a literal brace. The example names a `.names`
    # template, and cli would otherwise read `{.col}` as inline markup of its
    # own and refuse the refusal.
    abort_marginplyr(c(
      paste0(
        "{share_kind_modifier(kind)} {.fun across} requires an explicit ",
        "{.arg .names} argument."
      ),
      i = "For example {.code .names = \"{{.col}}_share\"}."
    ))
  }
  if (!is.null(args$unpack) && is.logical(args$unpack)) {
    if (length(args$unpack) != 1L || !isFALSE(args$unpack)) {
      abort_share_across_unpack(kind)
    }
  }
  if (contains_selection_predicate(args$cols)) {
    abort_share_predicate(kind)
  }
  args
}

abort_share_across_unpack <- function(kind) {
  abort_marginplyr(paste0(
    "{share_kind_modifier(kind)} {.fun across} requires ",
    "{.code .unpack = FALSE} or an omitted {.arg .unpack} argument."
  ))
}

validate_share_request <- function(outputs,
                                   sources,
                                   preceding,
                                   shares,
                                   context,
                                   kind) {
  if (length(outputs) == 0L) {
    return(invisible(NULL))
  }
  if (!all(nzchar(outputs))) {
    abort_marginplyr(
      "{share_kind_modifier(kind)} output names must not be empty."
    )
  }
  if (anyDuplicated(outputs)) {
    abort_marginplyr(c(
      "{share_kind_modifier(kind)} output names must be unique.",
      i = paste0(
        "Duplicate name {.var {outputs[[anyDuplicated(outputs)]]}} was ",
        "generated."
      )
    ))
  }

  preceding_names <- vapply(preceding, `[[`, character(1), "name")
  all_names <- vapply(
    context$all_records,
    `[[`,
    character(1),
    "name"
  )
  label <- share_kind_label(kind)
  for (i in seq_along(sources)) {
    source <- sources[[i]]
    output <- outputs[[i]]
    if (source %in% shares$names) {
      abort_marginplyr(paste0(
        "{label} {.var {output}} cannot use ",
        "{share_kind_label(share_name_kind(shares, source))} ",
        "{.var {source}} as its source."
      ))
    }
    if (!source %in% preceding_names) {
      if (source %in% all_names) {
        abort_marginplyr(c(
          paste0(
            "{label} {.var {output}} must refer to an ordinary summary ",
            "defined before it."
          ),
          i = "{.var {source}} is a forward reference."
        ))
      }
      abort_marginplyr(paste0(
        "{label} {.var {output}} refers to unknown preceding ordinary ",
        "summary {.var {source}}."
      ))
    }
    source_count <- context$ordinary_counts[source]
    if (!is.na(source_count) && source_count != 1L) {
      abort_marginplyr(c(
        paste0(
          "{label} {.var {output}} requires source summary {.var {source}} ",
          "to be defined exactly once."
        ),
        i = "Use one uniquely named ordinary summary."
      ))
    }
    source_records <- preceding[which(preceding_names == source)]
    certain_source <- confirmed_share_records(source_records)
    record <- if (length(certain_source) > 0L) {
      utils::tail(certain_source, 1L)[[1L]]
    } else {
      utils::tail(source_records, 1L)[[1L]]
    }
    if (!identical(record$eligibility, "eligible")) {
      abort_ineligible_share_source(
        label = label,
        output = output,
        source = source,
        eligibility = record$eligibility
      )
    }
    if (length(record$dependencies) > 0L) {
      abort_share_alias_dependency(
        kind, output, source, record$dependencies[[1L]]
      )
    }
  }

  conflicts <- intersect(
    outputs,
    unique(c(
      context$conflicting_names,
      vapply(context$certain_records, `[[`, character(1), "name"),
      shares$names
    ))
  )
  if (length(conflicts) > 0L) {
    abort_marginplyr(paste0(
      "{share_kind_modifier(kind)} output name {.var {conflicts[[1L]]}} ",
      "conflicts with a grouping key, {.arg .id}, ordinary summary, source ",
      "summary, or earlier contextual share."
    ))
  }
  invisible(NULL)
}

# Check dynamically expanded names against the reads of later share sources.
check_share_alias_reads <- function(output_names, checks) {
  for (check in checks) {
    dependencies <- if (unresolved_lookup_name() %in% check$symbols) {
      output_names
    } else {
      intersect(check$symbols, output_names)
    }
    if (length(dependencies) > 0L) {
      abort_share_alias_dependency(
        check$share_kind, check$share_output, check$source_summary,
        dependencies[[1L]]
      )
    }
  }
  invisible(NULL)
}

# Refuse one preceding alias read by a Contextual share's source expression.
abort_share_alias_dependency <- function(kind, output, source, dependency) {
  abort_marginplyr(c(
    paste0(
      "{share_kind_label(kind)} {.var {output}} cannot use source summary ",
      "{.var {source}} because it depends on earlier summary alias ",
      "{.var {dependency}}."
    ),
    i = "Combine the calculation into one ordinary summary expression."
  ))
}

# An ineligible source is refused in the terms of the summary the caller wrote,
# because the two ways a name can be ineligible need opposite rewrites. A
# column expanded from an unnamed data-frame-valued summary needs a top-level
# name; a named `across()` already has one, and needs that name dropped so
# dplyr writes a column per selected column instead of packing them all into
# it. Telling the second caller to add a top-level name would name the summary
# they already named (#105).
abort_ineligible_share_source <- function(label, output, source, eligibility) {
  # Two calls, in the shape `abort_share_helper_position()` records. Here the
  # two cases differ in the refusal and in the remedy alike, and the source
  # summary is named in both, so a clause assembled around it would carry a
  # subject flat where the same name takes `{.var}` everywhere else.
  if (identical(eligibility, "named_across")) {
    abort_marginplyr(c(
      paste0(
        "{label} {.var {output}} cannot use {.var {source}} because a named ",
        "{.fun across} packs its outputs into one data-frame-valued column."
      ),
      i = paste0(
        "Drop the {.code {source} =} name so each selected column is named ",
        "on its own, or define {.var {source}} as a top-level summary."
      )
    ))
  }
  abort_marginplyr(c(
    paste0(
      "{label} {.var {output}} cannot use {.var {source}} because it was ",
      "expanded from a data-frame-valued summary."
    ),
    i = paste0(
      "Rewrite it as a top-level named summary or a preceding ",
      "{.fun across} output."
    )
  ))
}

# Each kind states what the compiled plan must provide. A call requesting both
# must satisfy both, which is why this checks every requested kind rather than
# choosing one.
check_share_grouping_kinds <- function(plan, kinds) {
  for (kind in intersect(share_kind_names(), kinds)) {
    share_kind_rule(kind)$check_grouping(plan)
  }
  invisible(NULL)
}

check_parent_grouping_kind <- function(plan) {
  # A plan records the name its compilation classified, so this comparison is
  # against a bare string and needs no classification of its own (#317).
  if (!identical(plan$kind, "rollup")) {
    abort_ambiguous_parent()
  }
  invisible(NULL)
}

check_total_grouping_kind <- function(plan) {
  if (length(grand_total_occurrence_ids(plan)) > 0L) {
    return(invisible(NULL))
  }
  abort_marginplyr(c(
    paste0(
      "{.fun share_of_total} requires {.arg .grouping} to produce the Grand ",
      "total set, in which every grouping dimension is omitted."
    ),
    i = "{.fun rollup} and {.fun cube} always produce it.",
    i = paste0(
      "Add an empty {.fun grouping_set} to the {.fun grouping_sets} ",
      "specification, or omit the Total share."
    )
  ))
}

# Enforce scalar cardinality where a DuckDB or PostgreSQL share source uses
# UNNEST directly in its SQL projection. The adapter supplies resolved output
# names, so an ordinary summary without a share retains its SQL unchanged.
guard_sql_share_projections <- function(select, sources, con, kind, keys) {
  for (index in which(select$name %in% sources)) {
    expr <- select$expr[[index]]
    if (inherits(expr, "sql")) {
      select$expr[[index]] <- guard_share_unnest(
        expr, select$name[[index]], con, kind, keys
      )
    }
  }
  select
}

# Wrap a bare top-level UNNEST's single array argument. Only direct projection
# calls are recognized: an UNNEST inside a scalar subquery can legitimately
# feed several rows to its own aggregate. Token spans keep quoted contents
# and nested arguments from being mistaken for calls or argument separators.
guard_share_unnest <- function(expr, source, con, kind, keys) {
  sql <- as.character(expr)
  if (!grepl("unnest", sql, ignore.case = TRUE)) {
    return(expr)
  }
  tokens_at <- gregexpr(
    paste0("\"(?:[^\"]|\"\")*\"|'(?:[^'\\\\]|'')*'|",
           "/\\*|--|[[:alpha:]_][[:alnum:]_]*|[^[:space:]]"),
    sql, perl = TRUE
  )
  tokens <- regmatches(sql, tokens_at)[[1L]]
  # These spellings need dialect-specific lexical rules. Leave them to SQL.
  if (any(tokens %in% c("\"", "'", "$", "\\", ";", "/*", "--"))) {
    return(expr)
  }
  position <- seq_along(tokens)
  depth <- cumsum((tokens %in% c("(", "[")) - (tokens %in% c(")", "]")))
  valid_depth <- all(depth >= 0L) && utils::tail(depth, 1L) == 0L
  outer_depth <- 0L
  while (valid_depth && tokens[[outer_depth + 1L]] == "(" &&
           which(position > outer_depth & depth == outer_depth)[[1L]] ==
             length(tokens) - outer_depth) {
    outer_depth <- outer_depth + 1L
  }
  upper <- toupper(tokens)
  projection <- !upper[[outer_depth + 1L]] %in%
    c("SELECT", "WITH", "VALUES", "TABLE")
  starts <- which(
    valid_depth & projection & depth == outer_depth & upper == "UNNEST" &
      c(tokens[-1L], "") == "(" & c("", tokens[-length(tokens)]) != "."
  )

  offset <- as.integer(tokens_at[[1L]])
  width <- attr(tokens_at[[1L]], "match.length")
  for (start in rev(starts)) {
    end <- which(position > start & depth == outer_depth)[[1L]]
    arguments <- position > start + 1L & position < end
    separators <- depth == outer_depth + 1L & tokens == ","
    if (!any(arguments) || any(arguments & separators)) {
      next
    }
    first <- offset[[start + 1L]] + width[[start + 1L]]
    last <- offset[[end]] - 1L
    guarded <- sql_single_share_array(substr(sql, first, last), source, con,
                                      kind, keys)
    sql <- paste0(substr(sql, 1L, first - 1L), guarded,
                  substring(sql, last + 1L))
  }
  dbplyr::sql(sql)
}

# Check the array before UNNEST can discard or replicate its grouping row.
# Both engines bind the array once, in the scope of that grouping row.
sql_single_share_array <- function(array, source, con, kind, keys) {
  message <- dbplyr::escape(paste0(
    "Share source ", source, " must return exactly one value per grouping row."
  ), con = con)
  if (kind == "duckdb") {
    return(paste0(
      "list_transform([", array, "], lambda a: CASE WHEN len(a) = 1 ",
      "THEN a ELSE error(", message, ") END)[1]"
    ))
  }

  offset <- "0"
  if (length(keys) > 0L) {
    # OFFSET accepts NULL as zero. The outer grouping key keeps this a
    # per-group subplan even when the array has only volatile scalar calls.
    key <- dbplyr::escape(dbplyr::ident(keys[[1L]]), con = con)
    offset <- paste0("CASE WHEN ", key, " IS NULL THEN NULL ELSE 0 END")
  }
  # The offset keeps the payload and count on one evaluation. Including the
  # evaluated count prevents folding an invalid cast before the CASE runs.
  paste0(
    "(SELECT a FROM (SELECT ", array, " AS a OFFSET ", offset, ") AS v ",
    "WHERE CASE WHEN cardinality(a) = 1 THEN TRUE ELSE CAST(CONCAT(",
    message, ", ' Count: ', COALESCE(cardinality(a), 0)) AS BOOLEAN) END)"
  )
}

# Applies a nonempty set of requested shares to the staged summary result.
execute_shares <- function(operation,
                           staged_result,
                           requests,
                           check_share_source) {
  check_margin_operation(operation)
  check_margin_summary_stage(staged_result)
  result <- margin_summary_stage_result(
    staged_result
  )
  staged_set_id_name <- margin_summary_stage_set_id(
    staged_result
  )
  parent_key_names <- staged_result$parent_key_names
  # Source eligibility is settled once before selecting the ratio adapter.
  check_share_sources(
    operation,
    result = result,
    requests = requests,
    check_share_source = check_share_source
  )
  if (identical(operation$backend$kind, "duckdb")) {
    result <- bind_duckdb_share_values(result)
  }
  if (identical(operation$backend$kind, "dtplyr")) {
    result <- guard_dtplyr_grouped_result(
      result,
      plan = operation$plan,
      set_id_name = staged_set_id_name,
      parent_key_names = parent_key_names,
      pair = share_pairs(requests)[[1L]],
      call = operation$call
    )
  }
  adapter <- share_adapter(operation$backend$kind)
  # Each pass overwrites the placeholders in the caller's written order.
  for (kind in share_request_kinds(requests)) {
    result <- adapter(
      operation,
      result = result,
      requests = Filter(
        function(request) identical(request$kind, kind),
        requests
      ),
      set_id_name = staged_set_id_name,
      kind = kind,
      parent_key_names = parent_key_names
    )
  }
  if (length(parent_key_names) > 0L) {
    result <- drop_margin_columns(result, unname(parent_key_names))
  }
  if (!is.null(operation$set_id_name)) {
    result <- margin_safe_column_reads(
      result, c(staged_set_id_name, operation$set_id_name),
      function(data, safe_name) {
        dplyr::mutate(
          data,
          "{safe_name(operation$set_id_name)}" :=
            .data[[safe_name(staged_set_id_name)]]
        )
      }
    )
  }
  # A Margin order reads its Grouping bits from the same staged identifier, so
  # it is the finalizer that drops it on that path, after the `ORDER BY`.
  if (identical(
    margin_summary_stage_sort_id(staged_result),
    staged_set_id_name
  )) {
    return(result)
  }
  drop_margin_staged_identifier(result, staged_set_id_name, operation$backend)
}

# Keep DuckDB's displayed sources and every share window on one evaluation.
# The CTE is executed only when the caller executes the returned query.
bind_duckdb_share_values <- function(result) {
  con <- dbplyr::remote_con(result)
  vars <- margin_col_names(result)
  name <- new_margin_internal_names(
    1L, used_names = vars, prefix = "..marginplyr_share_stage_",
    backend = grouping_backend(result)
  )
  query <- dbplyr::sql_render(sql_quote_generated_columns(result))
  sql <- dbplyr::build_sql(
    "WITH ", dbplyr::ident(name), " AS MATERIALIZED (", query,
    ") SELECT * FROM ", dbplyr::ident(name), con = con
  )
  dplyr::tbl(con, sql, vars = vars)
}

# Refuse a staged dtplyr result with more than one row per occurrence and
# grouping key. The filter runs only when the caller executes the lazy graph.
guard_dtplyr_grouped_result <- function(result, plan, set_id_name,
                                        parent_key_names, pair, call) {
  dimensions <- if (length(parent_key_names) > 0L) {
    unname(parent_key_names)
  } else {
    plan$dimensions
  }
  key_names <- unique(c(set_id_name, plan$by, dimensions))
  margin_safe_column_reads(
    result, key_names,
    function(data, safe_name) {
      key_exprs <- lapply(key_names, function(name) {
        margin_column_pronoun(safe_name(name))
      })
      check <- rlang::call2(
        dtplyr_share_private_call("assert_dtplyr_grouped_keys"),
        !!!key_exprs,
        share_output = pair$output,
        share_kind = pair$kind,
        call_text = share_call_text(call)
      )
      dplyr::filter(data, !!check)
    }
  )
}

# Return TRUE when aligned staged key columns identify one row per key. The
# caller evaluates this predicate inside the lazy graph at explicit execution.
assert_dtplyr_grouped_keys <- function(..., share_output, share_kind,
                                       call_text) {
  keys <- list(...)
  names(keys) <- as.character(seq_along(keys))
  if (vctrs::vec_duplicate_any(vctrs::new_data_frame(keys))) {
    abort_marginplyr(
      c(
        paste0(
          "{share_kind_label(share_kind)} {.var {share_output}} cannot be ",
          "calculated because another ordinary summary expanded the grouped ",
          "result."
        ),
        i = "When requesting a share, return one value per grouping row."
      ),
      class = "marginplyr_share_cardinality_error",
      share_output = share_output,
      call = parse_call_text(call_text)
    )
  }
  TRUE
}

share_adapter <- function(backend_kind) {
  adapters <- list(
    local = execute_row_matched_shares,
    duckdb = execute_dbplyr_shares,
    postgres = execute_dbplyr_shares,
    sql = execute_dbplyr_shares,
    dtplyr = execute_dtplyr_shares,
    other = execute_row_matched_shares
  )
  adapter <- adapters[[backend_kind]]
  if (is.null(adapter)) {
    stop(
      "Unknown contextual-share backend kind: ", backend_kind,
      call. = FALSE
    )
  }
  adapter
}

execute_row_matched_shares <- function(operation,
                                       result,
                                       requests,
                                       set_id_name,
                                       kind,
                                       parent_key_names) {
  apply_joined_shares(
    result,
    requests = requests,
    plan = operation$plan,
    set_id_name = set_id_name,
    kind = kind,
    parent_key_names = parent_key_names
  )
}

execute_dtplyr_shares <- function(operation,
                                  result,
                                  requests,
                                  set_id_name,
                                  kind,
                                  parent_key_names) {
  apply_windowed_shares(
    result,
    requests = requests,
    plan = operation$plan,
    set_id_name = set_id_name,
    kind = kind,
    parent_key_names = parent_key_names,
    sql_window = FALSE
  )
}

execute_dbplyr_shares <- function(operation,
                                  result,
                                  requests,
                                  set_id_name,
                                  kind,
                                  parent_key_names) {
  apply_windowed_shares(
    result,
    requests = requests,
    plan = operation$plan,
    set_id_name = set_id_name,
    kind = kind,
    parent_key_names = parent_key_names,
    sql_window = TRUE
  )
}

# How the eligible-type rule is settled, chosen from the prepared backend kind
# exactly as the adapter above is. Every entry enforces the same rule and none
# of them reads a row of the caller's data, which is what ADR 0020 requires of
# each of them separately. Like the adapters, the lookup has no default: an
# unrecognized kind is a marginplyr defect rather than something a caller can
# rewrite.
share_source_checker <- function(backend_kind) {
  checkers <- list(
    local = check_typed_share_sources,
    duckdb = check_dialect_share_sources,
    postgres = check_dialect_share_sources,
    sql = check_dialect_share_sources,
    dtplyr = check_wrapped_share_sources,
    other = check_dialect_share_sources
  )
  checker <- checkers[[backend_kind]]
  if (is.null(checker)) {
    stop(
      "Unknown contextual-share source-checker backend kind: ", backend_kind,
      call. = FALSE
    )
  }
  checker
}

check_share_sources <- function(operation,
                                result,
                                requests,
                                check_share_source) {
  checker <- share_source_checker(operation$backend$kind)
  checker(
    operation,
    result = result,
    requests = requests,
    check_share_source = check_share_source
  )
}

# A materialized result carries the summaries' own types, so the rule is read
# off the result the operation already produced and nothing is asked of
# anybody.
check_typed_share_sources <- function(operation,
                                      result,
                                      requests,
                                      check_share_source) {
  values <- as.list(result)
  check_share_source_types(
    values[intersect(share_source_names(requests), names(values))],
    requests = requests,
    call = operation$call
  )
}

# `wrap_share_sources()` put the same rule inside the ordinary summary for the
# backends that evaluate summaries in R and stay lazy, where it raises at
# execution with the caller's own call. Asking here would collect their input
# on the caller's behalf to say what they already say themselves.
check_wrapped_share_sources <- function(operation,
                                        result,
                                        requests,
                                        check_share_source) {
  stopifnot(wraps_share_sources_in_summary(operation$backend$kind))
  invisible(NULL)
}

# A database evaluates the source summary itself, so the rule is the dialect's
# to apply rather than marginplyr's to read. Where the dialect refuses an
# ineligible summary, that refusal is the answer, and it reaches the caller as
# the database's own diagnostic when they execute the query.
#
# Where the dialect converts a value of another type to a number instead, it
# applies no rule at all, and no reading of the caller's data recovers one:
# every source comes back a number whatever it holds, which is what
# `investigation/share-source-eligibility-on-coercing-dialects.md` measured on
# the dialect #106 was filed about. The share is therefore refused rather than
# calculated from values nothing has checked, and `.check_share_source = FALSE`
# is how a caller who knows their own sources calculates it anyway.
check_dialect_share_sources <- function(operation,
                                        result,
                                        requests,
                                        check_share_source) {
  if (!isTRUE(check_share_source)) {
    return(invisible(NULL))
  }
  verdict <- share_dialect_verdict(operation$data, backend = operation$backend)
  if (identical(verdict, "refuses")) {
    return(invisible(NULL))
  }
  abort_share_source_dialect(
    share_request_kinds(requests),
    verdict = verdict,
    call = operation$call
  )
}

# One question about a dialect: does it convert a value of another type to a
# number rather than refusing it? ADR 0020's second exemption is authoritative
# for the queries it is asked with, for why asking reads none of the caller's
# data, and for the bound its amendment sets. What is reused is an answer, and
# the cache write below is where that is enforced.
share_dialect_verdict <- function(data, backend) {
  con <- share_dialect_connection(data)
  if (!share_dialect_can_be_asked(con)) {
    return("unknown")
  }
  key <- paste(class(backend$dialect), collapse = "\n")
  cached <- share_dialect_verdicts[[key]]
  if (!is.null(cached)) {
    return(cached)
  }
  verdict <- probe_share_dialect(con)
  # An invariant, not a Package condition (ADR 0015). Four sites act on this
  # string -- refusing the share, or describing why -- and none of them has a
  # default, so one this frame does not recognise would reach the caller as
  # `"unknown"`'s diagnostic, that their backend could not be asked, for a
  # dialect that was asked and answered. The cache write just below is the one
  # site that does have a default, and it fails closed: a verdict it does not
  # recognise is not recorded, so such a dialect would also be asked again on
  # every later request while continuing to misreport. This assertion is what
  # stops both.
  stopifnot(verdict %in% share_dialect_verdict_names())
  # Only a measured outcome is recorded, so an unanswered question is asked
  # again on the next request rather than reused. ADR 0020's *only an answer
  # is remembered, so the bound is per request* is authoritative for what an
  # unanswered attempt is a fact about, for the bound that follows from it,
  # and for what asking again costs.
  if (verdict %in% share_dialect_measured_names()) {
    share_dialect_verdicts[[key]] <- verdict
  }
  verdict
}

# The two answers that are properties of the dialect, because
# `investigation/share-source-eligibility-on-coercing-dialects.md` measured
# both: the dialect rejected summing a string, or it converted it to a number.
# These are the two the cache write above records.
share_dialect_measured_names <- function() {
  c("refuses", "converts")
}

# The three answers the dialect question can have: the two measured outcomes,
# and `"unknown"` for every case that read neither, which refuses the share.
# Written as the measured pair plus that one so the split the cache turns on is
# structural -- a verdict added to this vocabulary alone is not recorded, and a
# question asked again costs a query where a wrong fact cached against a
# dialect costs every later connection carrying it.
share_dialect_verdict_names <- function() {
  c(share_dialect_measured_names(), "unknown")
}

# One entry per dialect class whose question was answered, written the first
# time a share is requested on a connection carrying that dialect and the
# dialect answers.
share_dialect_verdicts <- new.env(parent = emptyenv())

share_dialect_connection <- function(data) {
  if (!inherits(data, "tbl_lazy")) {
    return(NULL)
  }
  dbplyr::remote_con(data)
}

# Whether the question below can be put to this connection at all, asked before
# it is put rather than read out of the answer. A connection that executes
# nothing -- `dbplyr::simulate_sqlite()` and its siblings, or one already
# disconnected -- raises whatever it is sent, and a raised query is how a
# dialect that refuses an ineligible summary is recognized. Reading a simulated
# connection's failure as that refusal would record it against the dialect's
# own class, where a later live connection carrying the same dialect would find
# it: the SQLite dialect would answer "refuses" because a simulator was built
# first, and the protection would be off with nothing said about it.
#
# `DBI::dbIsValid()` is a question about the connection and sends no query. It
# needs no availability guard: reaching here means the input is a `tbl_lazy`,
# so dbplyr is loaded, and dbplyr imports DBI (the `DBI = FALSE` case in
# `AGENTS.md`'s dependency metadata). A connection with no method for it is one
# nothing can be asked of, which is the answer this wants.
share_dialect_can_be_asked <- function(con) {
  if (is.null(con)) {
    return(FALSE)
  }
  isTRUE(tryCatch(DBI::dbIsValid(con), error = function(cnd) FALSE))
}

# Only the two outcomes the investigation measured are read as answers: a
# number came back, which is the conversion, or the dialect rejected summing a
# string, which is the refusal the rule is then left to. Everything else -- a
# query that cannot be built against this connection at all, an empty result,
# a result of another shape or type -- is no reading of the dialect, and falls
# to "unknown", which refuses the share. Falling the other way would switch
# the protection off wherever the question went unanswered and say nothing
# about having done so.
#
# A raised query is not by itself the refusal, and reading it as one is how
# the protection came to be switched off exactly where it was needed. The
# scaffolding `SELECT 1 AS z` reaches the database verbatim -- `dbplyr::sql()`
# is passed through untranslated -- and it has no `FROM`, which is a syntax
# error on Oracle, which requires `FROM DUAL`, and on SAP HANA, which requires
# `FROM DUMMY`. A dropped connection or a permissions failure raises just the
# same. Every one of those would have been recorded as "this dialect refuses
# an ineligible summary", which is the verdict that proceeds, so a share on
# such a dialect was calculated with the rule silently off and the answer
# cached for every later connection carrying it.
#
# The refusal is therefore only read from a query that raised where the same
# scaffolding demonstrably works. The control asks the one thing no dialect
# can refuse -- summing the number the scaffolding already selects -- so a
# control that does not come back with that number says the question could
# not be put here, whatever the reason, and "unknown" refuses the share. It is
# only sent when the probe raised, so a dialect that converts still answers in
# one query and the second is the price of telling the two failures apart.
#
# `vars` is what keeps each of them to the one query its answer needs: without
# it dbplyr asks the connection for the query's fields before it can build a
# `tbl`, which is a further query for a schema this frame already knows.
# RPostgres caller transactions isolate these questions with the controls
# authorized by ADR 0020's amendment for #774.
probe_share_dialect <- function(con) {
  savepoint <- NULL
  on.exit({
    if (!is.null(savepoint)) {
      suspendInterrupts({
        execute_share_probe_control(con, "ROLLBACK TO SAVEPOINT", savepoint)
        execute_share_probe_control(con, "RELEASE SAVEPOINT", savepoint)
      })
    }
  }, add = TRUE)
  # A real RPostgres connection carries its loaded driver's transaction state.
  # Simulators execute nothing; Redshift does not support PostgreSQL savepoints.
  if (inherits(con, "PqConnection") &&
        !inherits(con, c("TestConnection", "RedshiftConnection"))) {
    established <- tryCatch(suspendInterrupts({
      if (RPostgres::postgresIsTransacting(con)) {
        name <- as.character(DBI::dbQuoteIdentifier(
          con, basename(tempfile("marginplyr_share_"))
        ))
        execute_share_probe_control(
          con, "SAVEPOINT", name, error_handling = "handled"
        )
        # Cleanup is armed before deferred interrupts can be delivered.
        savepoint <- name
      }
      TRUE
    }), error = function(cnd) FALSE)
    if (!established) {
      return("unknown")
    }
  }
  probe <- probe_share_dialect_answer(
    con,
    quote(sum("x", na.rm = TRUE)),
    purpose = "share_dialect"
  )
  if (identical(probe, "raised")) {
    if (!is.null(savepoint)) {
      execute_share_probe_control(con, "ROLLBACK TO SAVEPOINT", savepoint)
    }
    control <- probe_share_dialect_answer(
      con,
      quote(sum(z, na.rm = TRUE)),
      purpose = "share_dialect_control",
      control = TRUE
    )
    if (identical(control, "answered")) {
      return("refuses")
    }
    return("unknown")
  }
  if (identical(probe, "answered")) {
    return("converts")
  }
  "unknown"
}

# Sends one PostgreSQL savepoint statement for an already quoted name. These
# controls protect only the table-free probe (ADR 0020, amendment for #774).
execute_share_probe_control <- function(con, command, name,
                                        error_handling = "propagated") {
  sql <- dbplyr::sql(paste(command, name))
  with_sent_query(
    "share_dialect_transaction", sql, DBI::dbExecute(con, sql),
    error_handling = error_handling
  )
  invisible(NULL)
}

# One table-free question, and which of three things happened to it: executing
# it raised, it came back with exactly one number, or neither -- it could not
# be built against this connection at all, or what came back was not one
# number. The last two are one answer here because the only two callers treat
# them alike: neither is a reading of the dialect.
#
# This vocabulary gets no guard, where `share_dialect_verdict_names()` does,
# and the difference is what each mistake would cost. A wrong verdict is
# cached under the dialect and described to the caller, so it misreports and
# keeps misreporting. A status is read once, by the frame above, which sends
# every value it does not recognise to `"unknown"` -- the answer that refuses
# the share. An unrecognised status therefore fails closed by construction,
# and that is the property worth writing down rather than asserting.
#
# `purpose` is the name this query is recorded under (ADR 0027). `control` says
# which of the two questions this is, which is what decides the reading below.
probe_share_dialect_answer <- function(con, expr, purpose, control = FALSE) {
  # Bound outside the wraps below for the reason `probe_share_dialect_holds()`
  # binds its number outside its own: what each of them reads as something the
  # connection did is an error.
  scaffold <- share_probe_scaffold()$sql
  query <- tryCatch(
    dplyr::summarize(dplyr::tbl(con, scaffold, vars = "z"), p = !!expr),
    error = function(cnd) NULL
  )
  if (is.null(query)) {
    return("unanswerable")
  }
  answer <- tryCatch(
    list(value = suppressMessages(suppressWarnings(with_sent_query(
      purpose, query, dplyr::collect(query),
      error_handling = "handled", warning_handling = "suppressed"
    )))),
    error = function(cnd) NULL
  )
  if (is.null(answer)) {
    return("raised")
  }
  value <- answer$value
  if (!is.data.frame(value) || nrow(value) != 1L || ncol(value) != 1L) {
    return("unanswerable")
  }
  if (!probe_share_dialect_holds(value[[1L]], control = control)) {
    return("unanswerable")
  }
  "answered"
}

# Whether the one value a query came back with is the answer its caller asked
# for. Not `is_share_source_type()`: that is the eligible-type rule about a
# caller's summary, and it rejects a classed value, so it also rejects what a
# driver mapped a database type to. RPostgres alone offers four mappings for
# the `bigint` PostgreSQL sums an integer to -- `bit64::integer64`, `integer`,
# `numeric`, and `character`, the first a classed double and the last not a
# number at all -- and the caller chooses between them when they connect
# (#440).
#
# The probe asks only that a number came back, which is the conversion. The
# control asks for the number the scaffolding selected, in whatever type the
# mapping produced, and tests the type as well as the value: `==` alone is the
# weaker question of what R coerces equal to that number, which several base
# types answer without being it.
#
# The comparison is wrapped because a class that passes the type test can raise
# inside `==` rather than return `FALSE` -- `units` and `vctrs::new_vctr` both
# do -- and nothing between here and the caller catches it, where every other
# way this function can fail already falls to "unanswerable". The number is
# bound first so that only the comparison is inside the wrap: an error raised
# by marginplyr's own frame is a defect, and reading it as a dialect that could
# not answer would hide it behind a refused share.
probe_share_dialect_holds <- function(value, control) {
  if (!control) {
    return(is.numeric(value))
  }
  number <- share_probe_scaffold()$number
  (is.numeric(value) || is.character(value)) &&
    isTRUE(tryCatch(value == number, error = function(cnd) FALSE))
}

# The table-free scaffolding both questions are asked against, and the number
# it selects. One definition because the control's reading is that its result
# is that number, so a scaffolding edited on its own would read every control
# as unanswerable.
share_probe_scaffold <- function() {
  list(sql = dbplyr::sql("SELECT 1 AS z"), number = 1)
}

# The refusal names whichever helpers the caller wrote, as the Arrow one does
# and for the same reason: what cannot be established belongs to the source
# summary they share. Which of the two unestablished cases it is stays in the
# message, because the rewrite differs -- a dialect that converts is answered
# by knowing your own sources, and a backend that could not be asked is
# answered by finding out why.
abort_share_source_dialect <- function(kinds, verdict, call) {
  kinds <- intersect(share_kind_names(), kinds)
  # An invariant, not a Package condition (ADR 0015), and the reason this
  # branch is written as a check rather than an `else`: `"refuses"` never
  # reaches here, so the only alternative to `"converts"` is `"unknown"`. An
  # unrecognised verdict would otherwise be described to the caller as a
  # backend that could not be asked, which is a different fact.
  stopifnot(identical(verdict, "converts") || identical(verdict, "unknown"))
  # Read only from the cli templates below, which codetools cannot see. Both
  # verdicts name both of them, so writing them there would put four copies of
  # an expression in the sentences this re-authoring exists to make readable.
  # nolint start: object_usage_linter.
  labels <- share_kind_label_plurals(kinds)
  helpers <- share_helper_names(kinds)
  # nolint end
  # Two calls, choosing a whole subordinate clause -- what could not be
  # established -- which is what ADR 0023's third amendment admits. What the
  # arms share is the opening of the refusal and the remedy that follows it.
  #
  # Binding the clause in R and interpolating it would remove that repetition,
  # and it is what `abort_share_source_type()` does -- but what it binds there
  # is a *value*, the class the data turned out to hold, where this clause is
  # prose marginplyr wrote. ADR 0023's *Two rules are gated* is what refuses a
  # template bound outside the call it raises from.
  if (identical(verdict, "converts")) {
    abort_marginplyr(
      c(
        paste0(
          "marginplyr cannot establish that the source summaries of {labels} ",
          "are plain integer or double scalars on this backend, because its ",
          "SQL dialect converts a value of another type to a number rather ",
          "than refusing it, so an ineligible source summary is ",
          "indistinguishable from an eligible one."
        ),
        i = paste0(
          "Set {.code .check_share_source = FALSE} to calculate ",
          "{.fun {helpers}} from sources you have established yourself, or ",
          "explicitly collect the data before calling ",
          "{.fun summarize_with_margins}."
        ),
        i = paste0(
          "Collection changes the default of {.arg .check_margin_label} from ",
          "{.code FALSE} to {.code TRUE}; set it explicitly when ",
          "re-running this call."
        )
      ),
      call = call
    )
  }
  abort_marginplyr(
    c(
      paste0(
        "marginplyr cannot establish that the source summaries of {labels} ",
        "are plain integer or double scalars on this backend, because it ",
        "could not be asked whether its SQL dialect converts a value of ",
        "another type to a number rather than refusing it, and a dialect ",
        "that converts rejects nothing."
      ),
      i = paste0(
        "Set {.code .check_share_source = FALSE} to calculate ",
        "{.fun {helpers}} from sources you have established yourself, or ",
        "explicitly collect the data before calling ",
        "{.fun summarize_with_margins}."
      ),
      i = paste0(
        "Collection changes the default of {.arg .check_margin_label} from ",
        "{.code FALSE} to {.code TRUE}; set it explicitly when ",
        "re-running this call."
      )
    ),
    call = call
  )
}

# The two lists every share refusal builds from the kinds a call used: the
# pluralised labels it names the helpers by, and the helpers it tells the
# caller to omit or opt out of. Both refusals -- this file's dialect one and
# the Arrow one -- name the same two, so they are written once here rather
# than kept in step by eye.
#
# Each answers a vector and leaves the joining to cli, which serialises one
# with `and` at the site (ADR 0023). The joined phrases these replaced spelled
# that `and` themselves, and for two kinds the bytes are the same either way.
share_kind_label_plurals <- function(kinds) {
  paste0(vapply(kinds, share_kind_label, character(1)), "s")
}

share_helper_names <- function(kinds) {
  vapply(kinds, share_helper_name, character(1))
}

# The internal column carrying each source summary's denominator, named after
# that summary because the name reaches the caller. A dialect that refuses an
# ineligible source refuses it while casting this column, and it is the column
# its diagnostic quotes rather than the summary the caller wrote — which is
# what left #106's DuckDB half reading as `..marginplyr_share_value_1`, a
# marginplyr temporary a reader can do nothing with. Naming the summary makes
# the same diagnostic actionable without marginplyr adding one of its own.
#
# Everything else is `new_margin_internal_names()`'s collision rule unchanged,
# one call per source with the names allocated so far added to `used_names`, so
# that two sources cannot be handed one name.
share_denominator_names <- function(sources, used_names, backend = NULL) {
  denominator_names <- character()
  for (source in sources) {
    denominator_names <- c(
      denominator_names,
      new_margin_internal_names(
        1L,
        used_names = c(used_names, denominator_names),
        prefix = paste0("..marginplyr_denominator_of_", source, "_"),
        backend = backend
      )
    )
  }
  names(denominator_names) <- sources
  denominator_names
}

apply_joined_shares <- function(result,
                                requests,
                                plan,
                                set_id_name,
                                kind,
                                parent_key_names) {
  rule <- share_kind_rule(kind)
  target_ids <- rule$target_ids(plan)
  own_denominator_ids <- plan$set_ids[is.na(target_ids)]
  pairs <- share_pairs(requests)
  sources <- share_source_names(requests)
  result_names <- margin_col_names(result)
  backend <- grouping_backend(result)
  denominator_names <- share_denominator_names(
    sources,
    used_names = result_names,
    backend = backend
  )

  key_names <- character()

  joined_ids <- plan$set_ids[!is.na(target_ids)]
  if (length(joined_ids) > 0L) {
    denominator <- rule$build_denominator(
      result,
      plan = plan,
      target_ids = target_ids,
      sources = sources,
      denominator_names = denominator_names,
      set_id_name = set_id_name,
      used_names = c(result_names, denominator_names),
      parent_key_names = parent_key_names
    )
    result <- denominator$result
    mapping <- denominator$mapping
    join_names <- denominator$join_names
    # sf keeps unselected active geometry in its private mapping and refuses
    # an sf join input. Project after conversion so only geometry keys remain.
    if (inherits(mapping, "sf")) {
      mapping <- dplyr::select(
        dplyr::as_tibble(mapping),
        dplyr::all_of(c(join_names, unname(denominator_names)))
      )
    }
    key_names <- denominator$key_names
    result <- dplyr::left_join(
      result, mapping, by = join_names, na_matches = "na"
    )
  }

  result <- apply_share_ratios(
    result, pairs, sources, denominator_names, own_denominator_ids,
    joined_ids, set_id_name
  )

  internal_names <- c(
    unname(denominator_names),
    key_names
  )
  internal_names <- intersect(
    internal_names,
    margin_col_names(result)
  )
  if (length(internal_names) > 0L) {
    result <- drop_margin_columns(result, internal_names)
  }
  result
}

# Add shares by reading denominator rows within the same staged relation.
# The caller supplies typed keys retained before Margin-label conversion.
apply_windowed_shares <- function(result, requests, plan, set_id_name, kind,
                                  parent_key_names, sql_window) {
  # dtplyr evaluates generated calls in the root's environment; SQL needs the
  # unqualified operator for dbplyr translation.
  membership <- if (sql_window) {
    rlang::sym("%in%")
  } else {
    rlang::call2(base::`::`, rlang::sym("base"), rlang::sym("%in%"))
  }
  target_ids <- share_kind_rule(kind)$target_ids(plan)
  joined_ids <- plan$set_ids[!is.na(target_ids)]
  sources <- share_source_names(requests)
  backend <- grouping_backend(result)
  denominator_names <- share_denominator_names(
    sources, margin_col_names(result), backend
  )
  fixed_names <- stats::setNames(plan$by, plan$by)
  # dbplyr reads window keys as symbols, including pronoun-like column names.
  if (sql_window && length(fixed_names) > 0L) {
    fixed_names[] <- new_margin_internal_names(
      length(fixed_names),
      used_names = c(margin_col_names(result), denominator_names),
      prefix = "..marginplyr_share_partition_",
      backend = grouping_backend(result)
    )
    result <- dplyr::rename(result, dplyr::all_of(
      stats::setNames(names(fixed_names), unname(fixed_names))
    ))
  }
  targets <- unique(target_ids[!is.na(target_ids)])
  for (index in seq_along(targets)) {
    target <- targets[[index]]
    dimensions <- intersect(plan$dimensions, plan$sets[[target]])
    keys <- c(unname(fixed_names), unname(parent_key_names[dimensions]))
    result <- margin_safe_column_reads(
      result, c(keys, sources, set_id_name),
      function(data, safe_name) {
        data <- dplyr::group_by(
          data, !!!rlang::syms(vapply(keys, safe_name, character(1),
                                      USE.NAMES = FALSE))
        )
        denominators <- lapply(sources, function(source) {
          value <- margin_column_pronoun(safe_name(source))
          identifier <- margin_column_pronoun(safe_name(set_id_name))
          denominator <- if (sql_window) {
            # An unframed aggregate window spans the entire partition.
            rlang::expr(max(dplyr::if_else(
              (!!identifier) == !!target, !!value, NA
            ), na.rm = TRUE))
          } else {
            rlang::call2(
              dtplyr_share_private_call("dtplyr_share_denominator"),
              value, identifier, target
            )
          }
          if (index > 1L) {
            denominator <- rlang::expr(dplyr::if_else(
              !!rlang::call2(membership, identifier,
                             plan$set_ids[target_ids %in% target]),
              !!denominator,
              !!margin_column_pronoun(denominator_names[[source]])
            ))
          }
          denominator
        })
        names(denominators) <- unname(denominator_names)
        dplyr::ungroup(dplyr::mutate(data, !!!denominators))
      },
      reserved = unname(denominator_names)
    )
  }
  result <- apply_share_ratios(
    result, share_pairs(requests), sources, denominator_names,
    plan$set_ids[is.na(target_ids)], joined_ids, set_id_name,
    qualify = !sql_window, duckdb = identical(backend$kind, "duckdb")
  )
  result <- drop_margin_columns(result, intersect(
    unname(denominator_names), margin_col_names(result)
  ))
  if (sql_window && length(fixed_names) > 0L) {
    result <- dplyr::rename(result, dplyr::all_of(fixed_names))
    # PostgreSQL ORDER BY expressions read restored keys from their input.
    result$lazy_query <- dbplyr::lazy_select_query(result$lazy_query)
  }
  result
}

# Select the one denominator value in a dtplyr partition at execution time.
# Grouped-result validation has already established one row per occurrence.
dtplyr_share_denominator <- function(value, identifier, target) {
  value[match(target, identifier)]
}

# Bind the namespace operator of a generated deferred share call. dtplyr
# preserves this computed head while translating its data-column arguments.
dtplyr_share_private_call <- function(name) {
  rlang::call2(base::`:::`, rlang::sym("marginplyr"), rlang::sym(name))
}

# Whether a translated call carries the package-selected namespace operator.
# Caller-authored qualified heads retain their ordinary symbolic operator.
is_generated_dtplyr_share_call <- function(expr) {
  head <- expr[[1L]]
  is.call(head) && length(head) == 3L &&
    identical(head[[1L]], base::`:::`) &&
    identical(head[[2L]], rlang::sym("marginplyr"))
}

# Apply the shared value rules after denominator columns have been attached.
apply_share_ratios <- function(result, pairs, sources, denominator_names,
                               own_denominator_ids, joined_ids, set_id_name,
                               qualify = FALSE, duckdb = FALSE) {
  outputs <- vapply(pairs, `[[`, character(1), "output")
  margin_safe_column_reads(
    result, c(sources, outputs),
    function(share_result, safe_name) {
      share_exprs <- lapply(pairs, function(pair) {
        source <- margin_column_pronoun(safe_name(pair$source))
        denominator <- denominator_names[[pair$source]]
        # A double multiplier can increase the source's decimal scale (#429).
        # Refusing dialects reject character sources at multiplication (#429).
        numerator <- rlang::expr((!!source) * 1L)
        if (duckdb) {
          # DuckDB multiplies an INTERVAL by an integer, but cannot subtract
          # an integer from the product. Binding checks this even when no
          # row reaches the arithmetic, including a typed SQL NULL (#1229).
          numerator <- rlang::expr((!!numerator) - 0L)
        }
        if (length(joined_ids) == 0L) {
          # No denominator was joined, so nothing else in the staged query
          # names the source: the null check binds it here (#446).
          # Both arms are `1.0`, and `IS NULL` never returns SQL NULL,
          # so no third value leaks from the CASE dbplyr renders without
          # an ELSE.
          return(rlang::expr(
            dplyr::if_else(
              is.na(!!numerator),
              1.0,
              1.0
            )
          ))
        }
        rlang::expr(
          dplyr::if_else(
            !!rlang::call2("%in%", margin_column_pronoun(set_id_name),
                           own_denominator_ids),
            1.0,
            dplyr::if_else(
              # DuckDB can widen a comparison with decimal zero to a scale
              # that cannot hold a wide integer or decimal denominator.
              is.na(!!source) |
                is.na(!!margin_column_pronoun(denominator)) |
                (!!margin_column_pronoun(denominator)) == 0L,
              NA_real_,
              # The denominator holds the source type, so only the
              # numerator needs the binding check. The cast makes the ratio
              # a double.
              as.double(!!numerator) /
                as.double(!!margin_column_pronoun(denominator))
            )
          )
        )
      })
      if (qualify) {
        share_exprs <- lapply(share_exprs, qualify_dtplyr_share_ratio)
      }
      names(share_exprs) <- vapply(
        outputs, safe_name, character(1), USE.NAMES = FALSE
      )
      dplyr::mutate(share_result, !!!share_exprs)
    }
  )
}

# Bind base calls in a package-generated ratio expression for dtplyr's
# root-environment evaluation. The expression contains no caller summaries.
qualify_dtplyr_share_ratio <- function(expr) {
  if (!is.call(expr)) {
    return(expr)
  }
  parts <- lapply(as.list(expr), qualify_dtplyr_share_ratio)
  name <- expr[[1L]]
  if (is.symbol(name) && as.character(name) %in%
        c("is.na", "as.double", "*", "/", "==", "|", "%in%")) {
    parts[[1L]] <- rlang::call2(base::`::`, rlang::sym("base"), expr[[1L]])
  }
  as.call(parts)
}

# What a kind contributes to the join above is two entries of
# `share_kind_rules()`: which occurrence each row's denominator comes from, and
# the denominator rows themselves with the columns they are matched on. A
# builder returns the result it was given, because a kind may have to add
# matching columns to both sides.
build_parent_denominator <- function(result,
                                     plan,
                                     target_ids,
                                     sources,
                                     denominator_names,
                                     set_id_name,
                                     used_names,
                                     parent_key_names) {
  mapping <- build_lazy_parent_mapping(
    result,
    child_ids = plan$set_ids[!is.na(target_ids)],
    parent_ids = target_ids,
    sources = sources,
    denominator_names = denominator_names,
    plan = plan,
    set_id_name = set_id_name,
    used_names = used_names,
    parent_key_names = parent_key_names
  )
  key_ids <- parent_join_key_ids(plan, target_ids)
  join_key_names <- new_margin_internal_names(
    length(plan$dimensions),
    used_names = used_names,
    prefix = "..marginplyr_parent_key_",
    backend = grouping_backend(result)
  )
  names(join_key_names) <- plan$dimensions
  result <- add_lazy_parent_join_keys(
    result,
    plan = plan,
    key_ids = key_ids,
    set_id_name = set_id_name,
    join_key_names = join_key_names,
    parent_key_names = parent_key_names
  )
  mapping <- add_lazy_parent_join_keys(
    mapping,
    plan = plan,
    key_ids = key_ids,
    set_id_name = set_id_name,
    join_key_names = join_key_names,
    parent_key_names = parent_key_names
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
  list(
    result = result,
    mapping = mapping,
    join_names = c(set_id_name, plan$by, unname(join_key_names)),
    key_names = unname(join_key_names)
  )
}

# A Total share's denominator depends on `.by` and nothing else, so its
# mapping is the grand total rows reduced to one row per fixed partition and
# matched on the fixed keys alone. See ADR 0017.
build_total_denominator <- function(result,
                                    plan,
                                    target_ids,
                                    sources,
                                    denominator_names,
                                    set_id_name,
                                    used_names,
                                    parent_key_names) {
  denominator_id <- unique(target_ids[!is.na(target_ids)])
  stopifnot(length(denominator_id) == 1L)
  mapping <- margin_safe_column_reads(
    result, c(plan$by, sources),
    function(source_data, safe_name) {
      key_exprs <- lapply(plan$by, function(name) {
        margin_column_pronoun(safe_name(name))
      })
      names(key_exprs) <- plan$by
      denominator_exprs <- lapply(sources, function(name) {
        margin_column_pronoun(safe_name(name))
      })
      names(denominator_exprs) <- unname(denominator_names[sources])
      dplyr::transmute(
        dplyr::filter(
          source_data,
          .data[[set_id_name]] == !!denominator_id
        ),
        !!!key_exprs,
        !!!denominator_exprs
      )
    }
  )

  if (length(plan$by) > 0L) {
    return(list(
      result = result,
      mapping = mapping,
      join_names = plan$by,
      key_names = character()
    ))
  }

  # Without fixed keys there is one denominator row and nothing to match it
  # on. A constant column on both sides supplies the shared join key.
  partition_name <- new_margin_internal_names(
    1L,
    used_names = used_names,
    prefix = "..marginplyr_total_key_",
    backend = grouping_backend(result)
  )
  partition_expr <- stats::setNames(
    list(rlang::expr(1L)),
    partition_name
  )
  list(
    result = dplyr::mutate(result, !!!partition_expr),
    mapping = dplyr::mutate(mapping, !!!partition_expr),
    join_names = partition_name,
    key_names = partition_name
  )
}

share_pairs <- function(requests) {
  unlist(
    lapply(
      requests,
      function(request) {
        Map(
          function(output, source) {
            list(
              output = output,
              source = source,
              kind = request$kind
            )
          },
          request$outputs,
          request$sources
        )
      }
    ),
    recursive = FALSE
  )
}

# The source summaries a set of requests reads, each named once however many
# shares are calculated from it.
share_source_names <- function(requests) {
  unique(vapply(share_pairs(requests), `[[`, character(1), "source"))
}

build_lazy_parent_mapping <- function(result,
                                      child_ids,
                                      parent_ids,
                                      sources,
                                      denominator_names,
                                      plan,
                                      set_id_name,
                                      used_names,
                                      parent_key_names) {
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
      margin_safe_column_reads(
        parent_rows, c(plan$by, unname(parent_key_names), sources),
        function(source_data, safe_name) {
          mapping_exprs <- parent_mapping_exprs(
            plan,
            sources = sources,
            denominator_names = denominator_names,
            parent_key_names = parent_key_names,
            safe_name = safe_name
          )
          dplyr::transmute(
            source_data,
            !!!mapping_exprs$keys,
            !!!child_id_expr,
            !!!mapping_exprs$denominators
          )
        }
      )
    }
  )
  combine_margin_branches(mappings)
}

# Project the staged keys and sources for a row-matched Parent mapping.
# The caller inserts the child occurrence identifier between them.
parent_mapping_exprs <- function(plan, sources, denominator_names,
                                 parent_key_names, safe_name = identity) {
  group_vars <- unique(c(plan$by, unname(parent_key_names)))
  key_exprs <- lapply(group_vars, function(name) {
    margin_column_pronoun(safe_name(name))
  })
  names(key_exprs) <- group_vars
  denominator_exprs <- lapply(sources, function(name) {
    margin_column_pronoun(safe_name(name))
  })
  names(denominator_exprs) <- unname(denominator_names[sources])
  list(keys = key_exprs, denominators = denominator_exprs)
}

# Child occurrences whose Parent retains each dimension, in plan order.
# `parent_ids` is indexed by Grouping set identifier and holds a Parent
# occurrence identifier or NA at every position.
parent_join_key_ids <- function(plan, parent_ids) {
  ids <- lapply(plan$dimensions, function(dimension) {
    plan$set_ids[vapply(plan$set_ids, function(set_id) {
      parent_id <- parent_ids[[set_id]]
      !is.na(parent_id) && dimension %in% plan$sets[[parent_id]]
    }, logical(1))]
  })
  stats::setNames(ids, plan$dimensions)
}

# Non-SQL joins use missing keys for dimensions the Parent omits. `key_ids`
# identifies which child occurrences retain each dimension in their Parent.
add_lazy_parent_join_keys <- function(result,
                                      plan,
                                      key_ids,
                                      set_id_name,
                                      join_key_names,
                                      parent_key_names) {
  join_key_exprs <- lapply(
    plan$dimensions,
    function(dimension) {
      rlang::expr(
        dplyr::if_else(
          (!!margin_column_pronoun(set_id_name)) %in% !!key_ids[[dimension]],
          !!margin_column_pronoun(parent_key_names[[dimension]]),
          NA
        )
      )
    }
  )
  names(join_key_exprs) <- unname(join_key_names[plan$dimensions])
  dplyr::mutate(result, !!!join_key_exprs)
}

# `values` is what a backend holding its summaries' own types can say about the
# source summaries, keyed by source name. A source it does not name is one the
# result does not carry, which is not a verdict: the rule is what an eligible
# type is, never how many backends can be asked.
check_share_source_types <- function(values, requests, call) {
  typed <- names(values)

  for (pair in share_pairs(requests)) {
    source <- pair$source
    if (!source %in% typed) {
      next
    }
    value <- values[[source]]
    if (is_share_source_type(value)) {
      next
    }
    abort_share_source_type(
      value,
      share_output = pair$output,
      source_summary = source,
      share_kind = pair$kind,
      call = call
    )
  }
  invisible(NULL)
}

# The grand total occurrences of a plan: those omitting every variable
# grouping dimension. There is at most one unless duplicates were kept. Named
# for the occurrences it returns, because `total_set_ids()` below returns the
# other shape — one denominator per occurrence, in plan order.
grand_total_occurrence_ids <- function(plan) {
  variable_sets <- lapply(plan$sets, setdiff, y = plan$by)
  plan$set_ids[lengths(variable_sets) == 0L]
}

# The denominator occurrence of every grouping set, in the shape
# `parent_set_ids()` returns: `NA` where the row is its own denominator, and
# otherwise the occurrence supplying it. Which Grand total occurrence is
# used is not part of the contract (ADR 0017, amendment #919).
total_set_ids <- function(plan) {
  result <- rep(NA_integer_, length(plan$sets))
  grand_total_ids <- grand_total_occurrence_ids(plan)
  if (length(grand_total_ids) == 0L) {
    return(result)
  }
  result[!plan$set_ids %in% grand_total_ids] <- grand_total_ids[[1L]]
  result
}

# The parent occurrence identifier for every occurrence in a compiled pure
# rollup Grouping plan, or missing for an occurrence of the Grand total set.
# Its caller has already established that the plan came from one pure
# `rollup()`.
parent_set_ids <- function(plan) {
  result <- rep(NA_integer_, length(plan$sets))
  variable_sets <- lapply(
    plan$sets,
    setdiff,
    y = plan$by
  )
  # The compiled pure rollup is detailed to coarse. The next distinct run
  # supplies every occurrence in the preceding run's immediate Parent.
  run_start <- 1L
  for (i in seq_along(variable_sets)) {
    if (i == 1L || identical(variable_sets[[i]], variable_sets[[i - 1L]])) {
      next
    }
    result[seq.int(run_start, i - 1L)] <- i
    run_start <- i
  }
  result
}

share_placeholder <- function(outputs) {
  placeholders <- lapply(
    outputs,
    function(output) {
      rlang::new_quosure(NA_real_, env = rlang::empty_env())
    }
  )
  names(placeholders) <- outputs
  structure(placeholders, class = "marginplyr_share_placeholders")
}

# Every contextual share helper, keyed by the kind of denominator its request
# resolves to. It is the one place a helper is described: the name a caller
# writes, the two forms a diagnostic needs to name it, what it requires of the
# compiled Grouping plan, and its row-matched denominator mapping. Detection
# reads the table backwards, from a written name to its kind. A third helper is
# one entry here and nothing else in this module.
#
# Both term forms are written out because a message uses whichever its sentence
# needs, and deriving one from the other would make the wording of every
# message depend on a rule about hyphens rather than on this table.
share_kind_rules <- function() {
  list(
    parent = list(
      helper = "share_of_parent",
      label = "Parent share",
      modifier = "Parent-share",
      check_grouping = check_parent_grouping_kind,
      target_ids = parent_set_ids,
      build_denominator = build_parent_denominator
    ),
    total = list(
      helper = "share_of_total",
      label = "Total share",
      modifier = "Total-share",
      check_grouping = check_total_grouping_kind,
      target_ids = total_set_ids,
      build_denominator = build_total_denominator
    )
  )
}

share_kind_rule <- function(kind) {
  rule <- share_kind_rules()[[kind]]
  if (is.null(rule)) {
    stop("Unknown contextual-share kind: ", kind, call. = FALSE)
  }
  rule
}

# The kinds in the order this module names them, which is the order a message
# listing several uses. A caller's writing order is not it: the same two
# helpers should be listed the same way whichever was written first.
share_kind_names <- function() {
  names(share_kind_rules())
}

share_helper_name <- function(kind) {
  share_kind_rule(kind)$helper
}

share_kind_label <- function(kind) {
  share_kind_rule(kind)$label
}

share_kind_modifier <- function(kind) {
  share_kind_rule(kind)$modifier
}

# The kind a written helper name resolves to, or `NULL` when it names no
# helper.
share_named_kind <- function(name) {
  rules <- share_kind_rules()
  helpers <- vapply(rules, `[[`, character(1), "helper")
  kind <- names(rules)[match(name, helpers, nomatch = 0L)]
  if (length(kind) == 0L) {
    return(NULL)
  }
  kind
}

# A node that is no call, and a name no share helper carries, both answer
# `NULL` from the shared reader, which is the answer a guard would have
# returned.
share_helper_call_kind <- function(expr) {
  share_named_kind(static_spelling_name(expr, "share"))
}

share_helper_function_kind <- function(expr) {
  share_named_kind(static_spelling_reference_name(expr, "share"))
}

is_share_helper_call <- function(expr) {
  !is.null(share_helper_call_kind(expr))
}

is_share_helper_function <- function(expr) {
  !is.null(share_helper_function_kind(expr))
}

# The kind of the first share helper anywhere in an expression. A rejected
# expression is named after the helper the caller wrote, and that helper can
# sit anywhere inside it — wrapped in arithmetic, or behind a formula in
# `.fns` — so the search is the same one that decides whether the expression
# concerns this module at all.
share_expression_kind <- function(expr) {
  kind <- share_helper_function_kind(expr)
  if (!is.null(kind)) {
    return(kind)
  }
  if (!rlang::is_call(expr)) {
    return(NULL)
  }
  kind <- share_helper_call_kind(expr)
  if (!is.null(kind)) {
    return(kind)
  }
  # By subscript for the reason `static_call_args()` gives. An empty argument
  # holds no share helper, so it answers `NULL` like any other unrecognized
  # shape and the walk carries on to the arguments after it. A captured one
  # holds no request either, however it is spelled inside: `quote(
  # share_of_total(units))` is a language object the caller is carrying, and
  # naming it a Total share written in the wrong position refused a call that
  # asks for no share at all (#179).
  #
  # The language the call evaluates is searched beside them, because that is a
  # helper the call does use: `eval(quote(share_of_total(total)))` reaches the
  # helper itself, which answers for a position it cannot see and names a
  # Grouping plan the caller already has.
  arguments <- searched_call_parts(expr)
  for (index in seq_along(arguments)) {
    kind <- share_expression_kind(arguments[[index]])
    if (!is.null(kind)) {
      return(kind)
    }
  }
  NULL
}

contains_share_helper <- function(expr) {
  !is.null(share_expression_kind(expr))
}

# An `across()` expression is named after its `.fns` when that is a helper,
# and otherwise after whichever helper it does contain: a rejected
# `.fns = ~share_of_total(.x)` is still a Total-share request as far as the
# caller is concerned.
share_across_kind <- function(expr) {
  kind <- share_helper_function_kind(parse_across_arguments(expr)$fns)
  if (!is.null(kind)) {
    return(kind)
  }
  share_expression_kind(expr)
}

share_request_kinds <- function(requests) {
  unique(vapply(requests, `[[`, character(1), "kind"))
}

is_across_call <- function(expr) {
  # Asked of any expression, not only of a call: a node that is no call has no
  # name, and no name matches.
  is_static_spelling_call(expr, "selection", "across")
}

# `error_call` is the caller's own `across()` call rather than this frame,
# because whatever tidyselect raises here is kept as the parent of the reported
# condition. Left at its default the chain would name this function and a line
# of this file, which is an internal frame no caller can act on. It reaches the
# conditions tidyselect raises itself, which is every one a caller can act on
# by rewriting the selection; a condition tidyselect re-signals from vctrs, such
# as a scalar out-of-bounds subscript, keeps the call vctrs gave it, and nothing
# short of rewriting that condition would change it.
resolve_share_selection <- function(expr,
                                    env,
                                    preceding_names,
                                    preceding,
                                    context,
                                    kind,
                                    error_call) {
  # Through the injected reading, so that a selection a wrapper forwarded is
  # asked the question the written one is asked, rather than reaching
  # `eval_select()` as an unusable selection (ADR 0019's amendment, #169).
  # Asked with `is_name_part()` for the reason `validate_share_across_syntax()`
  # gives, and read twice rather than bound, because what it carries can be the
  # empty argument.
  if (is_name_part(unwrap_injected_quosure(expr))) {
    source <- rlang::as_name(unwrap_injected_quosure(expr))
    if (!source %in% preceding_names) {
      abort_share_source_name(source, preceding, context, kind)
    }
  }
  proxy <- stats::setNames(
    as.list(seq_along(preceding_names)),
    preceding_names
  )
  # A quosure carries the environment `env` would supply, and wrapping one
  # hands `eval_select()` a quosure inside a quosure -- the lambda shorthand,
  # which it refuses. `resolve_summary_selection()` reads a selection the same
  # way, for the same reason (#350).
  selection <- if (rlang::is_quosure(expr)) {
    expr
  } else {
    rlang::new_quosure(expr, env = env)
  }
  tryCatch(
    names(tidyselect::eval_select(
      selection,
      data = proxy,
      strict = TRUE,
      allow_rename = FALSE,
      # The proxy carries positions. Tidyselect must reject a function
      # before applying it, including inside a compound selection.
      allow_predicates = FALSE,
      error_call = error_call
    )),
    error = function(cnd) {
      if (is_unsupported_predicate(cnd)) {
        abort_share_predicate(kind, parent = cnd)
      }
      abort_share_selection_error(cnd, preceding, context, kind)
    }
  )
}

# A selection naming something ineligible is marginplyr's own report and stays
# parentless. Anything else -- a selection expression that raised, or tidyselect
# rejecting the selection -- is an External condition, so it becomes the parent
# rather than being flattened into the message. That keeps the class and the
# chain a caller sees identical to the one the ordinary summary path propagates
# for the same expression, with only marginplyr's context added on top.
abort_share_selection_error <- function(cnd, preceding, context, kind) {
  missing <- share_selection_missing_names(cnd)
  if (length(missing) == 0L) {
    abort_marginplyr(
      c(
        "Invalid {share_kind_modifier(kind)} {.fun across} selection.",
        i = "Select only eligible preceding ordinary summaries by name."
      ),
      parent = cnd
    )
  }

  abort_share_source_name(missing[[1L]], preceding, context, kind)
}

abort_share_source_name <- function(source, preceding, context, kind) {
  # Read only from the four cli templates below, which codetools cannot see.
  # nolint start: object_usage_linter.
  helper <- share_helper_name(kind)
  # nolint end
  all_names <- vapply(
    context$all_records,
    `[[`,
    character(1),
    "name"
  )
  certain_names <- vapply(
    context$certain_records, `[[`, character(1), "name"
  )
  occurrences <- if (source %in% certain_names) {
    sum(certain_names == source)
  } else {
    sum(all_names == source)
  }
  if (occurrences > 1L) {
    abort_marginplyr(
      c(
        paste0(
          "{.fun across} can't select source summary {.var {source}} for ",
          "{.fun {helper}} because summary {.var {source}} was defined more ",
          "than once."
        ),
        i = paste0(
          "Define it once with a complete ordinary summary expression, then ",
          "select that unique preceding summary by name."
        )
      ),
      class = "marginplyr_share_source_duplicate_error",
      source_summary = source
    )
  }
  if (occurrences == 1L) {
    abort_marginplyr(
      c(
        paste0(
          "{.fun across} can't select source summary {.var {source}} for ",
          "{.fun {helper}} because summary {.var {source}} is not available ",
          "as a unique, preceding, self-contained ordinary summary."
        ),
        i = paste0(
          "Define it as a top-level named summary or a statically named ",
          "output from a preceding {.fun across}."
        ),
        i = "Select only eligible preceding ordinary summaries by name."
      ),
      class = "marginplyr_share_source_unavailable_error",
      source_summary = source
    )
  }

  preceding_candidates <- unique(vapply(
    preceding,
    `[[`,
    character(1),
    "name"
  ))
  # Two calls, in the shape `abort_share_helper_position()` records. Here what
  # varies is whether the remedy can name a summary the caller could have
  # selected; a call with none is answered by the same sentence without its
  # example.
  if (length(preceding_candidates) > 0L) {
    abort_marginplyr(
      c(
        paste0(
          "{.fun across} refers to unknown summary {.var {source}} for ",
          "{.fun {helper}}."
        ),
        i = paste0(
          "Select only eligible preceding ordinary summaries by name, such ",
          "as {.var {preceding_candidates[[1L]]}}."
        )
      ),
      class = "marginplyr_share_source_unknown_error",
      source_summary = source
    )
  }
  abort_marginplyr(
    c(
      paste0(
        "{.fun across} refers to unknown summary {.var {source}} for ",
        "{.fun {helper}}."
      ),
      i = "Select only eligible preceding ordinary summaries by name."
    ),
    class = "marginplyr_share_source_unknown_error",
    source_summary = source
  )
}

# The names a failed selection gave, in the order the chain holds them. What
# each one turns out to be -- a summary defined twice, one that is ineligible,
# or a name no summary answers at all -- is `abort_share_source_name()`'s
# question, so this says only what was named. A subscript that is not character
# names nothing it can report, and neither does the empty string, which
# `all_of(c(""))` puts in `i` where no summary could answer it.
share_selection_missing_names <- function(cnd) {
  subscripts <- unlist(
    lapply(
      condition_chain(cnd),
      function(condition) {
        if (is.character(condition$i)) condition$i else character()
      }
    ),
    use.names = FALSE
  )
  # `unlist()` answers `NULL` for a chain that held no condition at all, and
  # this answers a character vector, as `expression_data_symbols()` and
  # `static_character_value()` do for the same reason. A chain holding a
  # condition that names nothing already answers `character()` on the line
  # above, so only an argument that is no condition reaches this -- which
  # `abort_share_selection_error()` cannot pass, its `cnd` being a
  # `tryCatch()` handler's own argument. It is kept because a reader answering
  # `NULL` on one branch is one a caller cannot store, and asserted directly in
  # `test-share.R` rather than left as a branch nothing runs.
  if (is.null(subscripts)) {
    return(character())
  }
  unique(subscripts[nzchar(subscripts)])
}

contains_selection_predicate <- function(expr) {
  # A symbol needs no test of its own: it is no call, so the walk below has
  # nothing to descend into and the guard covers it.
  if (!rlang::is_call(expr)) {
    return(FALSE)
  }
  if (is_static_spelling_call(expr, "predicate", "where")) {
    return(TRUE)
  }
  any(vapply(
    searched_call_parts(expr),
    contains_selection_predicate,
    logical(1)
  ))
}

# Refuse a predicate with the share-specific remedy. An evaluated selector
# carries tidyselect's condition, whose call points to the selection.
abort_share_predicate <- function(kind, parent = NULL) {
  abort_marginplyr(c(
    paste0(
      "{share_kind_modifier(kind)} {.fun across} only supports name-based ",
      "tidyselect."
    ),
    i = paste0(
      "Replace {.fun where} or another type/value predicate with explicit ",
      "summary names."
    )
  ), parent = parent)
}

# A lookup the walk could not resolve is reported as a read of every alias in
# scope. That is the over-reporting the contract asks for, applied where the
# alias set is known: the walk itself cannot name what `get(name)` reads, and
# a call with no alias to read is left alone, so an unresolvable lookup is
# refused where it could hide a dependency and is legal everywhere else (#173).
#
# The messages downstream name `dependencies[[1L]]`, so an over-reported
# refusal names an alias the caller may not have written. The remedy those
# messages give is the one the caller needs either way -- move the derived
# value into a following `mutate()`, or fold the alias into one expression --
# and naming the marker instead would put a name no summary has into a message
# about the caller's own summaries.
expression_alias_dependencies <- function(expr, aliases, env = emptyenv(),
                                          mask = character(),
                                          reparented = list(),
                                          deferred_selection = FALSE) {
  if (length(aliases) == 0L) {
    return(character())
  }
  symbols <- unique(expression_data_symbols(
    expr, lookup = list(
      env = env, mask = mask, rebound = lookup_rebound_names(expr, env, mask),
      function_rebound = lookup_rebound_names(
        expr, env, mask, function_lookup = TRUE
      ),
      reparented = reparented, deferred_selection = deferred_selection
    )
  ))
  if (unresolved_lookup_name() %in% symbols) {
    return(aliases)
  }
  intersect(symbols, aliases)
}

# `bound` carries the names a construct inside the expression has bound, so a
# symbol matching one is a read of that binding rather than of a column. It is
# threaded through the recursion rather than applied to the result, because the
# two are not the same answer: `(function(share) share)(share)` reads the
# column in argument position while the body reads the formal, and
# `(function(share) .data$share)(value)` reads the column through a pronoun no
# local binding shadows. Filtering the output collapses both, silently (#130).
#
# A function definition populates it, and so do the statement-level binding
# constructs. `<-` and `=` bind their target and `for` binds its index, each
# into the bottom of the data mask, so the name is a new binding rather than a
# column read; `{` binds nothing of its own but carries what its statements
# bind to the statements after them. An implicit base `local()` evaluates
# its body in a child frame.
# `lookup` carries the captured environment, mask names, and rebindings.
# A quosure supplies its own environment without changing the latter two.
expression_data_symbols <- function(expr, bound = character(), lookup = NULL) {
  scoped_data_symbols(expr, list(list(bound = bound)), lookup)
}

# Read mask dependencies with the current lexical frame first in scope.
scoped_data_symbols <- function(expr, scope, lookup) {
  if (rlang::is_quosure(expr)) {
    lookup$env <- rlang::quo_get_env(expr)
  }
  # `is_name_part()` rather than `rlang::is_symbol()`, because the walk
  # descends into every part of a call and an empty argument is a symbol whose
  # name is `""`: the read of a column by that name was reported to every
  # caller of this list. An empty part is no call either, so it falls to the
  # line below and names nothing, which is the answer (#174).
  if (is_name_part(expr)) {
    name <- rlang::as_name(expr)
    bound <- scope_bound_names(scope)
    if (name %in% bound ||
          ("..." %in% bound && is_numbered_dot_reference(expr))) {
      return(scope_promise_symbols(name, scope, lookup))
    }
    return(name)
  }
  if (!rlang::is_call(expr)) {
    return(character())
  }
  # Redundant parentheses come off before any branch below reads the node. This
  # walk reads shapes as well as names -- `length(expr)`, `expr[[2L]]`, the
  # formals of a definition -- and the shared name read sees through a pair of
  # them, so a `(f(x))` reaching a branch unwrapped would be named `f` and
  # subscripted as the wrapper. Restarting once here keeps the two readings one
  # reading for every branch at once (#178).
  #
  # It restarts rather than filters, so `(share)` re-enters at the top and is
  # reported by the symbol branch above exactly as a bare `share` is.
  if (is_parenthesized(expr)) {
    return(scoped_data_symbols(unparenthesized_value(expr), scope, lookup))
  }
  # A call whose head is itself a call -- `fns$total(x)`, an inline lambda --
  # has no name, so `call_name()` returns `NULL`. Every comparison below is
  # written to be NULL-safe, since such a call is simply not the shape this
  # analysis recognizes and must fall through to its parts (#100).
  # `pkg::fun` is the one head shape the data mask does not evaluate. `::` and
  # `:::` take both operands literally -- neither is a lookup -- so a call to
  # one reads nothing, in a head or anywhere else. Walking every non-symbol
  # head is what brought this node into the walk's reach, and reporting its
  # parts rejects `dplyr::n()` in any call holding a summary named `n`, which
  # is how the vignettes here write it. The same over-report already reached
  # argument position, so answering it once at the node covers both.
  if (rlang::is_call(expr, c("::", ":::"))) {
    return(character())
  }
  # A formula is a call to `~`, and the general walk below already treats it as
  # one, so it is never asked for a name. `static_call_name()` is why, and it
  # is the same answer every other analysis in this package reads (#163).
  call_name <- static_call_name(expr)
  # The length is part of deciding whether this is a function definition at
  # all, not a precondition to check inside. A node built by hand rather than
  # parsed -- `rlang::call2("function")` arriving through injection -- can
  # carry a head named `function` without formals or a body, and it falls
  # through to the general walk below, which reports whatever its parts hold.
  # Answering `character()` for it instead would be a silent miss, which is
  # the one direction this walk is not allowed to be wrong in.
  if (identical(call_name, "function") && length(expr) >= 3L) {
    if (isTRUE(lookup$defer_definitions)) {
      return(character())
    }
    return(definition_data_symbols(expr, scope, lookup))
  }
  # `statement_reads_and_scope()` is the one place that knows which nodes bind
  # and what each of them binds. Here only its reads are wanted: an expression
  # position has nothing after it for a binding to reach, which is what a bare
  # `for (i in v) i + share` as a whole summary expression is. Inside a block
  # the other half of its answer is what carries the binding along, and
  # `is_binding_statement()` names `{` because a block opens no scope of its
  # own: a name one statement binds is bound for the statements after it and
  # for nothing before them. Walking them in order is what tells
  # `{ tmp <- share; tmp }`, which reads only the share, from
  # `{ tmp + share; tmp <- 1 }`, which reads the column `tmp` before anything
  # binds it. Collecting a block's bindings first and filtering the whole block
  # with them would answer the second one silently wrong (#162).
  if (is_binding_statement(call_name, expr)) {
    return(statement_reads_and_scope(expr, scope, lookup)$reads)
  }
  # A name a summary expression reaches through one of the reflective
  # primitives is the same read as the symbol would be, so it follows the same
  # dependency rule: `get("share")` and `share` resolve one binding. Only
  # `get()` was read that way, so `get0("share")`, `exists("share")`,
  # `mget("share")` and every name built for `eval()` reached the staging
  # placeholder instead -- silently for most of them (#173).
  #
  # Both branches add what the primitive resolves to the walk of the call's own
  # parts rather than answering in its place, because a reflective call
  # evaluates its arguments in the mask exactly as any other call does:
  # `get(name)` really does read `name` there.
  #
  # The callee name rather than the name read above, because a head that names
  # one of these primitives without being a symbol carries no call name of its
  # own. `(get)` is the redundant-parenthesis shape #130 recorded, and a head
  # built by `match.fun()` or `getFunction()` names the same primitive through
  # one whose whole purpose is to name a function.
  callee_name <- resolved_callee_name(expr, call_name)
  if (is_reflective_lookup(callee_name)) {
    return(unique(c(
      call_part_symbols(expr, scope, lookup),
      reflective_lookup_symbols(expr, scope_bound_names(scope), lookup)
    )))
  }
  if (!is.null(call_name) && call_name %in% c("env_get", "env_get_list") &&
        is_known_namespace_frame_call(expr, "rlang", scope, lookup)) {
    return(unique(c(
      call_part_symbols(expr, scope, lookup),
      inheriting_env_get_symbols(
        expr, scope, lookup, bulk = identical(call_name, "env_get_list")
      )
    )))
  }
  if (identical(call_name, "as.list") &&
        is_known_base_frame_call(expr, call_name, scope, lookup)) {
    return(unique(c(
      call_part_symbols(expr, scope, lookup),
      materialized_env_symbols(expr, scope, lookup)
    )))
  }
  if (is_reflective_evaluation(callee_name)) {
    return(unique(c(
      call_part_symbols(expr, scope, lookup),
      evaluated_language_symbols(expr, scope, lookup)
    )))
  }
  if (identical(callee_name, "do.call")) {
    invocation <- scope_empty_do_call(expr, scope, lookup)
    return(unique(c(
      call_part_symbols(invocation %||% expr, scope, lookup),
      deferred_call_symbols(expr)
    )))
  }
  if (
    !is.null(call_name) &&
      call_name %in% c("$", "[[") &&
      length(expr) >= 3L &&
      is_name_part(unparenthesized_value(expr[[2L]]))
  ) {
    # The bound set is deliberately not consulted here. `.data` is dplyr's
    # contract for reaching a column whatever else is in scope, so a local
    # binding of that column's name does not shadow it and
    # `(function(share) .data$share)(value)` really does read the column.
    # Applying the bound set to a pronoun-resolved name is the silent miss
    # this whole walk exists to prevent. `.data` bound as a formal is left
    # alone for the same reason, in the over-reporting direction: a caller who
    # does that has already left the contract, and a diagnostic is the safe
    # side to be wrong on.
    pronoun <- rlang::as_name(unparenthesized_value(expr[[2L]]))
    if (identical(pronoun, ".env")) {
      if (identical(call_name, "$")) {
        return(character())
      }
      return(pronoun_index_symbols(expr, scope, lookup))
    }
    if (identical(pronoun, ".data")) {
      if (rlang::is_missing(expr[[3L]])) {
        return(pronoun_index_symbols(expr, scope, lookup))
      }
      if (identical(call_name, "$") && is_name_part(expr[[3L]])) {
        return(rlang::as_name(expr[[3L]]))
      }
      # An evaluated index may read the mask and name a different column.
      # Unknown targets retain the lookup marker without running caller code.
      target <- static_character_value(expr[[3L]])
      if (is.null(target)) {
        target <- unresolved_lookup_name()
      }
      return(unique(c(target, pronoun_index_symbols(expr, scope, lookup))))
    }
  }
  if (!is.null(call_name) &&
        call_name %in% c("$", "[[", "getElement", ".subset2") &&
        (length(expr) == 3L ||
           (identical(call_name, "[[") && length(expr) == 4L &&
              identical(names(expr)[4L], "exact") &&
              (isTRUE(unparenthesized_value(expr[[4L]])) ||
                 isFALSE(unparenthesized_value(expr[[4L]]))))) &&
        is_static_mask_parent(expr[[2L]], scope, lookup) &&
        is_known_base_frame_call(expr, call_name, scope, lookup)) {
    if (identical(call_name, "$") && is_name_part(expr[[3L]])) {
      return(rlang::as_name(expr[[3L]]))
    }
    if (rlang::is_string(unparenthesized_value(expr[[3L]]))) {
      return(unparenthesized_value(expr[[3L]]))
    }
    # An unknown member still reads the proven mask; its key is not evaluated
    # during planning, and the key expression can carry its own dependencies.
    return(unique(c(
      unresolved_lookup_name(), call_part_symbols(expr, scope, lookup)
    )))
  }

  # Any other `a$b` or `a@b` names `b` literally: the field or slot name is
  # fixed text rather than a lookup, so only the object is walked. Collecting
  # the name made `cfg$share` claim a dependency on a column named `share`,
  # which the share analysis then read as an ordinary summary using an earlier
  # share, and put the two spellings of one access -- `cfg$share` and
  # `cfg[["share"]]` -- into disagreement (#101). `[[` is not the same shape:
  # its index is evaluated, so `cfg[[bucket]]` really does read `bucket` and
  # falls through to the walk below.
  if (
    !is.null(call_name) &&
      call_name %in% c("$", "@") &&
      length(expr) >= 2L
  ) {
    return(scoped_data_symbols(expr[[2L]], scope, lookup))
  }
  # R resolves a bare head through function lookup, which skips non-function
  # bindings such as a share named `sum` in `sum(x)`. Function lookup can
  # demand a pending lexical promise. The walk reports reads of that syntax
  # in the frame where the promise evaluates. A computed head instead
  # follows ordinary value lookup in the data mask, including reads of an
  # earlier share reached while computing the function (#130).
  #
  # Parentheses around a head make it a value expression in R. The lookup
  # for `(fns[[bucket]])(x)` therefore reads both the object and the index,
  # just as a computed head using `$` reads the object and one using `if`
  # reads its condition.
  #
  # The head comes first so that the walk returns symbols in source order,
  # which is user-visible: the guard names `share_dependency[[1L]]`, so this
  # decides which share an expression reading two of them is reported against.
  #
  # A formula falls through here like any other call, so `~ .x + share`
  # reports `.x`. That is deliberate. A formula is not intrinsically a lambda
  # -- it becomes one only where something calls `as_function()` on it, which
  # this walk cannot see -- so `.x` is over-reported, in the direction whose
  # errors are diagnostics rather than silence. Suppressing it outright would
  # miss a genuine read of a summary named `.x`, and suppressing it only under
  # `across()` would make the walk depend on its own call position, which is
  # what left function definitions behaving differently in a head (#130).
  call_part_symbols(expr, scope, lookup)
}

# Reads evaluated inside a pronoun subscript, excluding the pronoun itself.
# The target binding belongs to the pronoun; its indices retain this mask.
pronoun_index_symbols <- function(expr, scope, lookup) {
  unique(c(character(), unlist(lapply(
    static_call_args(expr)[-1L], scoped_data_symbols,
    scope = scope, lookup = lookup
  ), use.names = FALSE)))
}

# Reads of evaluated call arguments and the head, including syntax demanded
# by a lexical promise in function position. The supplied scope describes the
# frames visible before the call. Reflective branches add resolved names to
# these reads.
#
# An argument a call captures as language is not one of those parts, and this
# is where `quote(share)` stops being read as the column `share`: the boundary
# is drawn once here, so every branch above that ends at the parts -- the
# pronoun, the binding constructs, the reflective primitives -- inherits it
# rather than testing for a capture of its own (#179).
call_part_symbols <- function(expr, scope, lookup) {
  call_head <- static_call_head(expr)
  call_name <- static_call_name(expr)
  parts <- scope_call_args(expr, call_name, scope_bound_names(scope), lookup)
  selection <- if (isTRUE(lookup$deferred_selection)) {
    static_spelling_name(expr, "selection")
  }
  selection_positions <- if (identical(selection, "pick")) {
    seq_along(parts)
  } else if (!is.null(selection)) {
    parse_across_arguments(expr)$cols_index
  } else {
    integer()
  }
  closure <- scope_call_closure(expr, scope, lookup)
  reads <- character()
  # A computed head evaluates before invocation, but creating a function value
  # does not demand its body or defaults.
  if (!is_name_part(call_head) && !rlang::is_symbol(call_head)) {
    head_lookup <- lookup
    head_lookup$defer_definitions <- !is.null(closure)
    reads <- scoped_data_symbols(call_head, scope, head_lookup)
    scope <- scope_after_removals(call_head, scope, lookup)
  }
  if (!is.null(closure)) {
    for (variant in scope_closure_variants(closure)) {
      if (length(variant$unknown_definition_parts) > 0L) {
        reads <- c(reads, unresolved_lookup_name())
        next
      }
      if (scope_closure_is_active(variant, scope)) next
      reads <- c(reads, definition_data_symbols(
        variant$definition, scope_closure_scope(variant, scope), lookup, expr,
        if (length(variant$frames) > 0L) 1L else variant$enclosing,
        closure_origin = variant$enclosing + length(variant$frames),
        caller = 1L + length(variant$frames)
      ))
    }
  } else if (is_name_part(call_head)) {
    name <- rlang::as_name(call_head)
    reads <- scope_promise_symbols(name, scope, lookup, function_lookup = TRUE)
    scope <- scope_after_promise(name, scope, lookup, function_lookup = TRUE)
  }
  local <- implicit_local_parts(expr, call_name, scope, lookup)
  if (!is.null(local)) {
    return(unique(c(
      reads, scoped_data_symbols(local$body, local$scope, lookup)
    )))
  }
  # Unknown callees may execute callback arguments. Only literal carriers keep
  # the function values in their arguments deferred.
  parts_lookup <- lookup
  parts_lookup$defer_definitions <- isTRUE(lookup$defer_definitions) &&
    ((!is.null(call_name) && call_name %in% c("list", "[[") &&
        is_known_base_frame_call(expr, call_name, scope, lookup)) ||
       !is.null(scope_remapped_closure_list(expr, call_name, scope, lookup)))
  for (index in seq_along(parts)) {
    read_symbols <- if (index %in% selection_positions) {
      selection_dependency_symbols
    } else {
      scoped_data_symbols
    }
    reads <- c(reads, read_symbols(parts[[index]], scope, parts_lookup))
    scope <- scope_after_possible_removals(parts[[index]], scope, lookup)
  }
  unique(reads)
}

# Read evaluated leaves of a deferred selector in the caller's scope.
# Runtime selection supplies the actual columns after tidyselect set operations.
selection_dependency_symbols <- function(expr, scope, lookup) {
  if (rlang::is_quosure(expr)) {
    lookup$env <- rlang::quo_get_env(expr)
    return(selection_dependency_symbols(rlang::quo_get_expr(expr), scope,
                                        lookup))
  }
  if (is_name_part(expr)) {
    return(scope_promise_symbols(rlang::as_name(expr), scope, lookup))
  }
  if (is_selection_data_pronoun(expr)) {
    return(character())
  }
  if (is_selection_walk_call(expr)) {
    return(unlist(lapply(
      as.list(expr)[-1L], selection_dependency_symbols, scope = scope,
      lookup = lookup
    ), use.names = FALSE))
  }
  scoped_data_symbols(expr, scope, lookup)
}

# A statically selected literal branch, without evaluating its selector.
# NULL leaves selection unknown; a one-element list retains an empty argument.
scope_literal_branch <- function(expr, scope, lookup) {
  arm <- literal_if_parts(expr, static_call_name(expr),
                          scope_bound_names(scope), lookup)
  if (!is.null(arm)) {
    return(arm)
  }
  if (!identical(static_call_name(expr), "switch") || length(expr) < 3L ||
        is_parenthesized(static_call_head(expr)) ||
        !is_known_base_frame_call(expr, "switch", scope, lookup)) {
    return(NULL)
  }
  literal_switch_parts(expr)
}

# Recognize literal function syntax without constructing an R closure.
literal_function_definition <- function(expr) {
  expr <- unparenthesized_value(expr)
  if (rlang::is_call(expr, "function") && length(expr) >= 3L) expr else NULL
}

# Model a known closure invoked by base do.call() with no arguments, in the
# current caller frame. A computed head retains value lookup for `what`.
scope_empty_do_call <- function(expr, scope, lookup) {
  if (!identical(static_call_name(expr), "do.call") ||
        is_parenthesized(static_call_head(expr)) ||
        !is_known_base_frame_call(expr, "do.call", scope, lookup)) {
    return(NULL)
  }
  parts <- static_matched_arguments(expr, base::do.call)
  if (!setequal(names(parts), c("what", "args")) ||
        any(vapply(parts, rlang::is_missing, logical(1)))) {
    return(NULL)
  }
  arguments <- unparenthesized_value(parts$args)
  if (!identical(static_call_name(arguments), "list") ||
        length(arguments) != 1L ||
        is_parenthesized(static_call_head(arguments)) ||
        !is_known_base_frame_call(arguments, "list", scope, lookup) ||
        is.null(scope_value_closure(parts$what, scope, lookup))) {
    return(NULL)
  }
  rlang::call2(rlang::call2("(", parts$what))
}

# Resolve a visible literal function in call position. Its enclosing index
# refers to the full stack, which also holds the dynamic caller.
scope_call_closure <- function(expr, scope, lookup) {
  head <- static_call_head(expr)
  if (!is_name_part(head)) {
    return(scope_value_closure(head, scope, lookup))
  }
  scope_closure_binding(rlang::as_name(head), scope, function_lookup = TRUE)
}

# Resolve visible closure values without evaluating their carrier or index.
scope_value_closure <- function(expr, scope, lookup) {
  head <- unparenthesized_value(expr)
  definition <- literal_function_definition(head)
  if (!is.null(definition)) {
    return(list(definition = definition, enclosing = 1L))
  }
  arm <- scope_literal_branch(head, scope, lookup)
  if (length(arm) == 1L && !rlang::is_missing(arm[[1L]])) {
    return(scope_value_closure(arm[[1L]], scope, lookup))
  }
  if (identical(static_call_name(head), "{") &&
        is_known_base_frame_call(head, "{", scope, lookup)) {
    statements <- static_call_args(head)
    if (length(statements) == 0L) {
      return(NULL)
    }
    for (index in seq_len(length(statements) - 1L)) {
      scope <- statement_reads_and_scope(
        statements[[index]], scope, lookup, read_symbols = no_scope_reads
      )$scope
    }
    return(scope_value_closure(statements[[length(statements)]], scope, lookup))
  }
  returned <- scope_factory_closure(head, scope, lookup)
  if (!is.null(returned)) return(returned)
  name <- unparenthesized_value(head)
  if (is_name_part(name)) {
    name <- rlang::as_name(name)
    closure <- scope_closure_binding(name, scope, function_lookup = FALSE)
    if (!is.null(closure)) return(closure)
    dot <- scope_numbered_dot(name, scope, scope_visible_frames(scope))
    if (!is.null(dot)) {
      closures <- scope[[dot$binding]]$dot_closures
      if (dot$slot <= length(closures)) {
        closure <- closures[[dot$slot]]
        if (!is.null(closure)) {
          closure <- scope_shift_closure_elements(
            list(closure), dot$binding
          )[[1L]]
          return(closure)
        }
      }
    }
    promise <- scope_promise(name, scope, function_lookup = FALSE,
                             visible = scope_visible_frames(scope))
    return(scope_promise_closure(promise, lookup))
  }
  operator <- static_call_name(head)
  replacement <- scope_definition_replacement(head, scope, lookup)
  if (!is.null(replacement)) return(replacement)
  transfer_lookup <- lookup
  transfer_lookup$rebound <- lookup$function_rebound %||% lookup$rebound
  if (!is.null(operator) &&
        operator %in% c("identity", "invisible", "force", "match.fun") &&
        is_known_base_frame_call(head, operator, scope, transfer_lookup)) {
    value <- base_call_argument(
      head, if (operator == "match.fun") "FUN" else "x"
    )
    if (length(value) == 1L) {
      return(scope_value_closure(value[[1L]], scope, lookup))
    }
  }
  if (identical(operator, "as.function") &&
        is_known_base_frame_call(head, operator, scope, transfer_lookup)) {
    value <- base_call_argument(head, "x")
    closure <- if (length(value) == 1L) {
      scope_value_closure(value[[1L]], scope, lookup)
    }
    closure <- scope_filter_closure(closure, function(variant) {
      !isTRUE(variant$dispatch_unknown) && scope_implicit_class_default(
        operator, "base", variant$dispatch_classes %||% "function",
        scope, lookup
      )
    })
    if (!is.null(closure)) return(closure)
  }
  if (identical(operator, "Vectorize") &&
        is_known_base_frame_call(head, operator, scope, transfer_lookup)) {
    parts <- static_matched_arguments(head, base::Vectorize)
    if ("FUN" %in% names(parts) && !"vectorize.args" %in% names(parts)) {
      closure <- scope_value_closure(parts$FUN, scope, lookup)
      # An empty formal list makes Vectorize return FUN unchanged.
      closure <- scope_filter_closure(closure, function(variant) {
        length(variant$definition[[2L]]) == 0L
      })
      if (!is.null(closure)) return(closure)
    }
  }
  if (identical(operator, "as_function") &&
        is_known_namespace_frame_call(head, "rlang", scope, transfer_lookup)) {
    value <- base_call_argument(head, "x", rlang::as_function)
    if (length(value) == 1L) {
      return(scope_value_closure(value[[1L]], scope, lookup))
    }
  }
  if (identical(operator, "cmpfun") &&
        is_known_namespace_frame_call(head, "compiler", scope,
                                      transfer_lookup)) {
    value <- base_call_argument(head, "f", compiler::cmpfun)
    if (length(value) == 1L) {
      return(scope_value_closure(value[[1L]], scope, lookup))
    }
  }
  if (identical(operator, "get") &&
        is_known_base_frame_call(head, operator, scope, transfer_lookup)) {
    value <- base_call_argument(head, "x")
    target <- base_call_argument(head, c("envir", "pos"))
    mode <- base_call_argument(head, "mode")
    inherited <- base_call_argument(head, "inherits")
    key <- if (length(value) == 1L) static_character_value(value[[1L]])
    frame <- if (length(target) == 0L) {
      1L
    } else {
      static_environment_frame(target[[1L]], scope, lookup)
    }
    mode <- if (length(mode) == 0L) {
      "any"
    } else {
      static_character_value(mode[[1L]])
    }
    inherited <- if (length(inherited) == 0L) {
      TRUE
    } else {
      unparenthesized_value(inherited[[1L]])
    }
    if (length(key) == 1L && !is.null(frame) && frame <= length(scope) &&
          length(mode) == 1L && mode %in% c("any", "function") &&
          (identical(inherited, TRUE) || identical(inherited, FALSE))) {
      visible <- if (inherited) scope_visible_frames(scope, frame) else frame
      return(scope_closure_binding(
        key, scope, function_lookup = identical(mode, "function"),
        visible = visible
      ))
    }
  }
  member <- scope_closure_carrier_member(head, scope, lookup)
  if (!is.null(member$definition) || !is.null(member$alternatives)) {
    member
  }
}

# Resolve stock closure setters without evaluating the replacement expression.
# An unreadable replacement retains a callable with unresolved dependencies.
scope_definition_replacement <- function(expr, scope, lookup) {
  operator <- static_call_name(expr)
  if (is.null(operator) ||
        !operator %in% c("body<-", "formals<-", "environment<-")) return(NULL)
  methods_body <- identical(operator, "body<-") &&
    is_known_package_frame_call(expr, operator, "methods", scope, lookup)
  if (!methods_body &&
        !is_known_base_frame_call(expr, operator, scope, lookup)) return(NULL)
  parts <- static_matched_arguments(
    expr, args(get(operator, envir = baseenv()))
  )
  if (!all(c("fun", "value") %in% names(parts))) return(NULL)
  closure <- scope_value_closure(parts$fun, scope, lookup)
  if (is.null(closure)) return(NULL)
  if (identical(operator, "environment<-")) {
    enclosing <- static_environment_frame(parts$value, scope, lookup)
    getter <- identical(static_call_name(parts$value), "environment") &&
      is_known_base_frame_call(parts$value, "environment", scope, lookup)
    original <- if (getter) {
      argument <- base_call_argument(parts$value, "fun")
      if (length(argument) == 1L) {
        scope_value_closure(argument[[1L]], scope, lookup)
      }
    }
    originals <- scope_closure_variants(original)
    original <- if (length(originals) == 1L &&
                      !"enclosure" %in%
                        originals[[1L]]$unknown_definition_parts) {
      originals[[1L]]
    }
    external <- if (is.null(enclosing)) {
      scope_external_enclosure(parts$value, scope, lookup)
    }
    return(scope_map_closure_elements(list(closure), function(variant) {
      variant$frames <- if (!is.null(original)) {
        original$frames
      } else if (!is.null(external)) {
        list(external)
      }
      variant$enclosing <- original$enclosing %||% enclosing %||%
        (length(scope) + 1L)
      variant$unknown_definition_parts <- setdiff(
        variant$unknown_definition_parts, "enclosure"
      )
      if (is.null(enclosing) && is.null(original) && is.null(external)) {
        variant$unknown_definition_parts <- union(
          variant$unknown_definition_parts, "enclosure"
        )
      }
      variant
    })[[1L]])
  }
  value <- scope_definition_value(parts$value, scope, lookup)
  if (!is.null(value) &&
        (rlang::is_missing(value[[1L]]) || is.object(value[[1L]]))) {
    value <- NULL
  }
  part <- if (identical(operator, "body<-")) "body" else "formals"
  slot <- if (identical(part, "body")) 3L else 2L
  if (identical(part, "formals") && !is.null(value) &&
        !(is.null(value[[1L]]) || is.pairlist(value[[1L]]) ||
            is.list(value[[1L]]))) value <- NULL
  if ("envir" %in% names(parts)) value <- NULL
  scope_map_closure_elements(list(closure), function(variant) {
    if (methods_body && !is.null(value)) {
      # The omitted envir dispatches as missing. Only a plain closure and a
      # known replacement reaching base retain a precise proof.
      method <- methods::selectMethod(
        methods::`body<-`, c("function", "missing", class(value[[1L]])[[1L]])
      )
      if (isTRUE(variant$dispatch_unknown) ||
            !identical(variant$dispatch_classes %||% "function", "function") ||
            !identical(method@.Data, base::`body<-`)) value <- NULL
    }
    if (is.null(value)) {
      variant$definition[slot] <- list(NULL)
      variant$unknown_definition_parts <- union(
        variant$unknown_definition_parts, part
      )
    } else {
      if (identical(part, "formals")) value <- list(as.pairlist(value[[1L]]))
      variant$definition[slot] <- value
      variant$unknown_definition_parts <- setdiff(
        variant$unknown_definition_parts, part
      )
    }
    variant
  })[[1L]]
}

# Retain bindings of a literal fresh or existing independent enclosure without
# forcing promises or active bindings. Unknown bindings prove no independence.
scope_external_enclosure <- function(expr, scope, lookup) {
  operator <- static_call_name(expr)
  if (!is.null(operator) &&
        operator %in% c("baseenv", "emptyenv", "new.env", "list2env") &&
        is_known_base_frame_call(expr, operator, scope, lookup)) {
    if (operator %in% c("baseenv", "emptyenv") && length(expr) == 1L) {
      return(list(bound = character()))
    }
    fields <- scope_fresh_env_fields(expr, scope, lookup)
    if (!is.null(fields)) {
      parts <- static_matched_arguments(
        expr, args(get(operator, envir = baseenv()))
      )
      parent <- if ("parent" %in% names(parts)) {
        scope_external_enclosure(parts$parent, scope, lookup)
      }
      if (!is.null(parent)) {
        keys <- names(fields)
        parent$bound <- union(keys, parent$bound)
        parent$kinds[keys] <- lapply(fields, function(field) {
          if (length(scope_closure_variants(field)) > 0L) "function"
        })
        parent$closures[keys] <- fields
        return(parent)
      }
    }
  }
  bound <- scope_bound_names(scope)
  if (!lookup_environment_is_external(expr, bound, lookup)) return(NULL)
  env <- lookup_environment_value(expr, bound, lookup)
  bindings <- character()
  kinds <- list()
  while (!identical(env, emptyenv()) && !identical(env, baseenv())) {
    keys <- setdiff(ls(env, all.names = TRUE), bindings)
    for (key in keys) {
      bindings <- c(bindings, key)
      if (bindingIsActive(key, env) || rlang::env_binding_are_lazy(env, key)) {
        next
      }
      value <- get(key, envir = env, inherits = FALSE)
      kinds[[key]] <- scope_literal_kind(value)
    }
    env <- parent.env(env)
  }
  list(bound = names(kinds), kinds = kinds)
}

# Read literal definition values, captures and formal-list member writes.
# A list wrapper distinguishes a known NULL from an unreadable value.
scope_definition_value <- function(expr, scope, lookup) {
  expr <- unparenthesized_value(expr)
  if (rlang::is_missing(expr) || is.object(expr)) return(NULL)
  if (!is.language(expr)) return(list(expr))
  if (rlang::is_call(expr, n = 1L) &&
        identical(static_call_name(expr), "quote") &&
        is_known_base_frame_call(expr, "quote", scope, lookup)) {
    return(static_call_args(expr))
  }
  operator <- static_call_name(expr)
  if (is.null(operator) || !is_known_base_frame_call(
    expr, operator, scope, lookup
  )) return(NULL)
  if (identical(operator, "formals")) {
    parts <- static_matched_arguments(expr, base::formals)
    if (!"fun" %in% names(parts)) return(NULL)
    closure <- scope_value_closure(parts$fun, scope, lookup)
    values <- lapply(scope_closure_variants(closure), function(variant) {
      if (!"formals" %in% variant$unknown_definition_parts) {
        list(variant$definition[[2L]])
      }
    })
    values <- unique(values)
    if (length(values) == 1L) return(values[[1L]])
  }
  if (operator %in% c("alist", "list", "pairlist")) {
    values <- static_call_args(expr)
    if (!identical(operator, "alist")) {
      values <- lapply(values, scope_definition_value,
                       scope = scope, lookup = lookup)
      if (any(lengths(values) != 1L)) return(NULL)
      values <- lapply(values, `[[`, 1L)
    }
    return(list(as.pairlist(values)))
  }
  if (operator %in% c("$<-", "[[<-")) {
    definition <- if (identical(operator, "$<-")) {
      function(x, name, value) NULL
    } else {
      function(x, i, value) NULL
    }
    parts <- static_matched_arguments(expr, definition)
    key_name <- if (identical(operator, "$<-")) "name" else "i"
    if (!all(c("x", key_name, "value") %in% names(parts))) return(NULL)
    parent <- scope_definition_value(parts$x, scope, lookup)
    value <- scope_definition_value(parts$value, scope, lookup)
    key <- if (identical(operator, "$<-") && is_name_part(parts[[key_name]])) {
      rlang::as_name(parts[[key_name]])
    } else {
      scope_literal_list_index(parts[[key_name]], scope, lookup)
    }
    if (is.null(parent) || is.null(value) || length(key) != 1L ||
          !(is.pairlist(parent[[1L]]) || is.list(parent[[1L]]))) return(NULL)
    updated <- tryCatch({
      values <- as.list(parent[[1L]])
      values[[key]] <- value[[1L]]
      list(as.pairlist(values))
    }, error = function(cnd) NULL)
    return(updated)
  }
  NULL
}

# Expand a visible replacement path into setter calls, keeping ordinary heads.
# Only paths reaching a local closure's definition or enclosure belong here.
scope_definition_write <- function(target, value) {
  path <- list()
  carrier <- unparenthesized_value(target)
  while (rlang::is_call(carrier)) {
    operator <- static_call_name(carrier)
    if (is.null(operator) || length(carrier) < 2L ||
          rlang::is_missing(carrier[[2L]])) return(NULL)
    path[[length(path) + 1L]] <- carrier
    carrier <- unparenthesized_value(carrier[[2L]])
  }
  definitions <- which(vapply(path, function(step) {
    static_call_name(step) %in% c("body", "formals", "environment")
  }, logical(1)))
  if (length(definitions) == 0L || !is_name_part(carrier)) {
    return(NULL)
  }
  definition <- definitions[[1L]]
  for (step in path[seq_len(definition)]) {
    args <- static_call_args(step)
    names(args) <- replace(rlang::names2(args), 1L, "")
    value <- rlang::call2(paste0(static_call_name(step), "<-"), !!!args,
                          value = value, .ns = static_call_ns(step))
  }
  list(target = path[[definition]][[2L]], value = value)
}

# Select a visible member without evaluating its carrier or key.
scope_closure_carrier_member <- function(head, scope, lookup) {
  operator <- static_call_name(head)
  if (is.null(operator) ||
        !operator %in% c("[[", "$", "getElement", ".subset2") ||
        !length(head) %in% c(3L, 4L) ||
        !is_known_base_frame_call(head, operator, scope, lookup)) {
    return(NULL)
  }
  # A computed accessor head uses value lookup, including non-functions.
  if (operator %in% c("getElement", ".subset2") &&
        !is_name_part(static_call_head(head)) &&
        !rlang::is_call(static_call_head(head), c("::", ":::"))) return(NULL)
  parts <- static_call_args(head)
  if (identical(operator, "getElement")) {
    parts <- static_matched_arguments(head, base::getElement)
    if (!all(c("object", "name") %in% names(parts))) return(NULL)
    parts <- parts[c("object", "name")]
  }
  # The primitive takes its operands positionally but removes these controls.
  if (identical(operator, ".subset2") &&
        argument_names(parts)[[2L]] %in% c("exact", "drop")) return(NULL)
  partial <- identical(operator, "$")
  if (length(parts) == 3L) {
    if (!operator %in% c("[[", ".subset2") ||
          !identical(names(parts)[3L], "exact") ||
          rlang::is_missing(parts[[3L]])) {
      return(NULL)
    }
    exact <- unparenthesized_value(parts[[3L]])
    if (!identical(exact, TRUE) &&
          !(identical(operator, "[[") && identical(exact, FALSE))) return(NULL)
    partial <- identical(exact, FALSE)
  }
  if (rlang::is_missing(parts[[2L]])) return(NULL)
  path <- if (identical(operator, "$")) {
    # Dollar lookup takes a written name, even in function-call syntax.
    if (is_name_part(parts[[2L]])) {
      rlang::as_name(parts[[2L]])
    } else if (!is.object(parts[[2L]]) && rlang::is_string(parts[[2L]])) {
      parts[[2L]]
    }
  } else {
    scope_literal_list_index(parts[[2L]], scope, lookup)
  }
  if (length(path) == 0L) return(NULL)
  frame <- scope_fresh_env_binding(parts[[1L]], scope)
  if (!is.null(frame)) {
    if (!operator %in% c("$", "[[") || length(head) != 3L ||
          !rlang::is_string(path)) return(NULL)
    name <- rlang::as_name(unparenthesized_value(parts[[1L]]))
    fields <- scope[[frame]]$closure_environments[[name]]
    return(scope_shift_closure_elements(fields[path], frame)[[1L]])
  }
  elements <- scope_closure_list(parts[[1L]], scope, lookup)
  scope_list_path_member(elements, path, partial)
}

# Select the written path separately in each known list layout.
scope_list_path_member <- function(elements, path, partial) {
  members <- lapply(scope_list_choices(elements), function(choice) {
    index <- scope_list_member_index(choice, path[[1L]], partial)
    if (is.null(index)) return(NULL)
    member <- choice[[index]]
    if (length(path) == 1L) return(member)
    scope_list_path_member(member$elements, path[-1L], partial)
  })
  scope_join_members(members)
}

# Locate one existing member under exact or partial plain-list indexing.
scope_list_member_index <- function(elements, key, partial = FALSE) {
  # R list lookup never matches an empty name to an unnamed element.
  index <- if (rlang::is_string(key) && nzchar(key)) {
    if (partial) {
      pmatch(key, names(elements), duplicates.ok = TRUE)
    } else {
      match(key, names(elements))
    }
  } else if (typeof(key) %in% c("integer", "double", "logical") &&
               is.finite(key)) {
    # Index a plain sequence so R handles fractional and invalid positions.
    tryCatch(seq_along(elements)[[key]], error = function(cnd) NULL)
  }
  if (!is.null(index) && !is.na(index)) index else NULL
}

# Read unclassed literal list indices, including signed numbers and known base
# concatenations, without evaluating any part of the caller's expression.
scope_literal_list_index <- function(expr, scope, lookup) {
  if (rlang::is_missing(expr)) return(NULL)
  expr <- unparenthesized_value(expr)
  if (is.object(expr)) return(NULL)
  if (typeof(expr) %in% c("character", "integer", "double", "logical")) {
    if (!anyNA(expr)) return(expr)
    return(NULL)
  }
  operator <- static_call_name(expr)
  if (is.null(operator) || !operator %in% c("-", "c") ||
        !is_known_base_frame_call(expr, operator, scope, lookup)) return(NULL)
  parts <- static_call_args(expr)
  if (identical(operator, "-")) {
    if (length(parts) != 1L || rlang::is_missing(parts[[1L]])) return(NULL)
    value <- unparenthesized_value(parts[[1L]])
    if (!is.object(value) && typeof(value) %in% c("integer", "double") &&
          length(value) == 1L && is.finite(value)) return(-value)
    return(NULL)
  }
  if (any(names(parts) %in% c("recursive", "use.names"))) return(NULL)
  values <- lapply(parts, scope_literal_list_index,
                   scope = scope, lookup = lookup)
  if (any(vapply(values, is.null, logical(1)))) return(NULL)
  do.call(base::c, unname(values))
}

# Known callable results retain accompanying list candidates. Each result is
# rebased with its own invoked and captured frames before candidates merge.
scope_factory_closure <- function(expr, scope, lookup) {
  returned <- scope_factory_results(expr, scope, lookup,
                                    scope_closure_list_element)
  if (is.null(returned)) return(NULL)
  values <- lapply(returned$results, function(result) {
    if (!is.null(result$value)) {
      scope_map_closure_elements(list(result$value), function(closure) {
        scope_rebase_factory_closure(closure, result)
      })[[1L]]
    }
  })
  member <- scope_join_members(values)
  if (length(scope_closure_variants(member)) > 0L) member
}

# Known list results retain each candidate's names, positions and frames.
scope_factory_lists <- function(expr, scope, lookup) {
  returned <- scope_factory_results(expr, scope, lookup, scope_closure_list)
  if (is.null(returned)) return(NULL)
  scope_join_lists(lapply(returned$results, function(result) {
    if (is.null(result$value)) return(NULL)
    scope_map_closure_elements(result$value, function(closure) {
      scope_rebase_factory_closure(closure, result)
    })
  }))
}

# Run the existing bounded reader independently for each known factory.
scope_factory_results <- function(expr, scope, lookup, read_value) {
  factory <- if (rlang::is_call(expr)) scope_call_closure(expr, scope, lookup)
  if (is.null(factory)) return(NULL)
  list(
    alternatives = !is.null(factory$alternatives),
    results = lapply(scope_closure_variants(factory), function(candidate) {
      scope_factory_result(expr, scope, lookup, read_value, candidate)
    })
  )
}

# Read one factory's result in its invoked and captured frames. A supplied
# factory is an already-resolved single literal closure descriptor.
scope_factory_result <- function(expr, scope, lookup, read_value,
                                 factory) {
  if (length(factory$unknown_definition_parts) > 0L ||
        scope_closure_is_active(factory, scope)) {
    return(NULL)
  }
  definition <- factory$definition
  captured <- length(factory$frames)
  frame <- function_scope_frame(definition[[2L]], expr)
  frame$enclosing <- if (captured > 0L) 1L else factory$enclosing
  frame$caller <- 1L + captured
  frame$closure_origin <- factory$enclosing + captured
  frame$definition <- definition
  inner <- c(list(frame), scope_closure_scope(factory, scope))
  result <- scope_returned_value(definition[[3L]], inner, lookup, read_value)
  result$factory_frames <- 1L + captured
  result$enclosing <- factory$enclosing
  result
}

# Retain factory frames closed over by a returned member. Closures from the
# live stack only need their offsets moved past the departing frames.
scope_rebase_factory_closure <- function(closure, result) {
  if (!is.null(closure$alternatives)) {
    closure$alternatives <- lapply(
      closure$alternatives, scope_rebase_factory_closure, result = result
    )
    return(closure)
  }
  if (closure$enclosing <= result$factory_frames) {
    frames <- result$scope[seq.int(closure$enclosing, result$factory_frames)]
    frames <- lapply(frames, function(frame) {
      frame$invoked <- FALSE
      frame
    })
    closure$frames <- c(closure$frames, frames)
    closure$enclosing <- result$enclosing
  } else {
    closure$enclosing <- closure$enclosing - result$factory_frames
  }
  closure
}

# Read a selected result and surviving bindings in the supplied frame.
# Nonfinal expressions contribute effects, but only an explicit return exits.
# Only syntax is inspected; caller code is never invoked to discover a value.
scope_returned_value <- function(expr, scope, lookup, read_value,
                                 final = TRUE) {
  expr <- unparenthesized_value(expr)
  name <- static_call_name(expr)
  arm <- scope_literal_branch(expr, scope, lookup)
  if (length(arm) == 1L && !rlang::is_missing(arm[[1L]])) {
    return(scope_returned_value(
      arm[[1L]], scope, lookup, read_value, final = final
    ))
  }
  if (identical(name, "return") && length(expr) <= 2L &&
        is_known_base_frame_call(expr, name, scope, lookup)) {
    result <- list(value = NULL, scope = scope)
    if (length(expr) == 2L && !rlang::is_missing(expr[[2L]])) {
      result <- scope_returned_value(expr[[2L]], scope, lookup, read_value)
    }
    result$returned <- TRUE
    return(result)
  }
  if (identical(name, "{") &&
        is_known_base_frame_call(expr, name, scope, lookup)) {
    statements <- static_call_args(expr)
    for (index in seq_along(statements)) {
      if (rlang::is_missing(statements[[index]])) {
        next
      }
      result <- scope_returned_value(
        statements[[index]], scope, lookup, read_value,
        final = final && index == length(statements)
      )
      if (result$returned || index == length(statements)) {
        return(result)
      }
      scope <- result$scope
    }
    return(list(value = NULL, scope = scope, returned = FALSE))
  }
  if (!final) {
    scope <- statement_reads_and_scope(
      expr, scope, lookup, read_symbols = no_scope_reads
    )$scope
  }
  list(value = if (final) read_value(expr, scope, lookup),
       scope = scope, returned = FALSE)
}

# Closure and nested-list elements, relative to the current stack.
# Each element captures its value before the next element changes bindings.
scope_closure_list <- function(expr, scope, lookup) {
  if (rlang::is_missing(expr)) return(NULL)
  expr <- unparenthesized_value(expr)
  if (is.null(expr)) return(list())
  if (is_name_part(expr)) {
    name <- rlang::as_name(expr)
    for (index in scope_visible_frames(scope)) {
      frame <- scope[[index]]
      if (!name %in% frame$bound) {
        next
      }
      elements <- frame$closure_lists[[name]] %||%
        frame$closures[[name]]$elements
      if (is.null(elements)) {
        promise <- scope_promise(name, scope, function_lookup = FALSE,
                                 visible = index)
        if (is.null(promise)) return(NULL)
        elements <- scope_closure_list(
          promise$expression, promise$scope[promise$evaluation], lookup
        )
        if (is.null(elements)) return(NULL)
        # These descriptors already count the evaluation frame as index one.
        index <- promise$evaluation[[1L]] - 1L
      }
      return(scope_shift_closure_elements(elements, index))
    }
  }
  operator <- static_call_name(expr)
  arm <- scope_literal_branch(expr, scope, lookup)
  if (length(arm) == 1L && !rlang::is_missing(arm[[1L]])) {
    return(scope_closure_list(arm[[1L]], scope, lookup))
  }
  if (identical(operator, "{") &&
        is_known_base_frame_call(expr, operator, scope, lookup)) {
    result <- scope_returned_value(expr, scope, lookup, scope_closure_list)
    # An explicit return exits the enclosing function before the list is used.
    return(if (!isTRUE(result$returned)) result$value)
  }
  transfer_lookup <- lookup
  transfer_lookup$rebound <- lookup$function_rebound %||% lookup$rebound
  if (!is.null(operator) &&
        operator %in% c("identity", "invisible", "force") &&
        is_known_base_frame_call(expr, operator, scope, transfer_lookup)) {
    value <- base_call_argument(expr, "x")
    if (length(value) == 1L) {
      return(scope_closure_list(value[[1L]], scope, lookup))
    }
  }
  if (identical(static_call_name(expr), "list") &&
        is_known_base_frame_call(expr, "list", scope, lookup)) {
    parts <- scope_list_arguments(expr, scope)
    elements <- vector("list", length(parts))
    names(elements) <- names(parts)
    for (index in seq_along(parts)) {
      elements[index] <- list(
        scope_closure_list_element(parts[[index]], scope, lookup)
      )
      scope <- statement_reads_and_scope(
        parts[[index]], scope, lookup, read_symbols = no_scope_reads
      )$scope
    }
    return(elements)
  }
  if (identical(static_call_name(expr), "c") &&
        is_known_base_frame_call(expr, "c", scope, lookup)) {
    parts <- static_call_args(expr)
    if (any(names(parts) %in% c("recursive", "use.names"))) return(NULL)
    values <- vector("list", length(parts))
    names(values) <- names(parts)
    for (index in seq_along(parts)) {
      values[index] <- list(scope_closure_list(parts[[index]], scope, lookup))
      if (is.null(values[[index]])) return(NULL)
      scope <- statement_reads_and_scope(
        parts[[index]], scope, lookup, read_symbols = no_scope_reads
      )$scope
    }
    return(scope_list_product(values, function(parts) do.call(base::c, parts)))
  }
  if (identical(static_call_name(expr), "append") &&
        is_known_base_frame_call(expr, "append", scope, lookup)) {
    parts <- static_matched_arguments(
      rlang::call2("append", !!!static_call_args(expr)), base::append
    )
    if (!all(c("x", "values") %in% names(parts))) return(NULL)
    x <- scope_closure_list(parts$x, scope, lookup)
    scope <- statement_reads_and_scope(
      parts$x, scope, lookup, read_symbols = no_scope_reads
    )$scope
    values <- scope_closure_list(parts$values, scope, lookup)
    if (is.null(x) || is.null(values)) return(NULL)
    explicit <- "after" %in% names(parts) && !rlang::is_missing(parts$after)
    after <- if (explicit) {
      unparenthesized_value(parts$after)
    }
    if (explicit && (
      is.object(after) || !typeof(after) %in% c("integer", "double") ||
        length(after) != 1L || !is.finite(after) || after < 0 ||
        after != trunc(after)
    )) return(NULL)
    return(scope_list_product(list(x = x, values = values), function(parts) {
      base::append(parts$x, parts$values, after = after %||% length(parts$x))
    }))
  }
  if (identical(operator, "[") && length(expr) %in% c(3L, 4L) &&
        is_known_base_frame_call(expr, "[", scope, lookup)) {
    if (length(expr) == 4L &&
          (!identical(names(expr)[4L], "drop") ||
             identical(names(expr)[3L], "drop") ||
             !rlang::is_bool(unparenthesized_value(expr[[4L]])))) return(NULL)
    elements <- scope_closure_list(expr[[2L]], scope, lookup)
    if (is.null(elements)) return(NULL)
    if (rlang::is_missing(expr[[3L]])) return(elements)
    key <- unparenthesized_value(expr[[3L]])
    if (is.language(key)) {
      key <- scope_literal_list_index(key, scope, lookup)
      if (is.null(key)) return(NULL)
    }
    if (is.object(key) ||
          !typeof(key) %in%
            c("NULL", "integer", "double", "logical", "character")) {
      return(NULL)
    }
    if (typeof(key) %in% c("integer", "double") &&
          (!all(is.finite(key)) || (any(key < 0) && any(key > 0)))) {
      return(NULL)
    }
    return(scope_map_lists(elements, function(choice) choice[key]))
  }
  remapped <- scope_remapped_closure_list(expr, operator, scope, lookup)
  if (!is.null(remapped)) return(remapped)
  member <- scope_closure_carrier_member(expr, scope, lookup)
  if (!is.null(member)) return(member$elements)
  scope_factory_lists(expr, scope, lookup)
}

# Expand a literal list's finite dots as reads in their owning frame.
# Numbered reads retain each promise's evaluation frame and argument tag.
# Unproven expansions retain the original syntax for the list walk.
scope_list_arguments <- function(expr, scope) {
  parts <- static_call_args(expr)
  splice <- which(vapply(parts, identical, logical(1), quote(...)))
  if (length(splice) != 1L) return(parts)
  for (index in scope_visible_frames(scope)) {
    frame <- scope[[index]]
    if (!"..." %in% frame$bound) next
    missing <- which(vapply(frame$dots, rlang::is_missing, logical(1)))
    retained <- which(lengths(frame$dot_closures) > 0L)
    if (!isTRUE(frame$invoked) || length(frame$unmatched) > 0L ||
          !all(missing %in% retained)) return(parts)
    dots <- lapply(paste0("..", seq_along(frame$dots), recycle0 = TRUE),
                   rlang::sym)
    names(dots) <- names(frame$dots)
    return(append(parts[-splice], dots, after = splice - 1L))
  }
  parts
}

# Remap established plain-list descriptors with matched literal arguments.
scope_remapped_closure_list <- function(expr, name, scope, lookup) {
  if (is.null(name) || is_parenthesized(static_call_head(expr))) return(NULL)
  rule <- switch(name,
    rev = list(formals = "x", dispatch = TRUE),
    unname = list(formals = c("obj", "force")),
    structure = list(formals = c(".Data", "names")),
    setNames = list(owner = "stats", formals = c("object", "nm")),
    # The primitive rep() exposes dots; rep.int() names the supported pair.
    rep = list(formals = c("x", "times"), definition = args(base::rep.int)),
    as.list = list(formals = "x", dispatch = TRUE),
    tail = list(owner = "utils", formals = c("x", "n"), dispatch = TRUE,
                method = "tail.default")
  )
  if (is.null(rule)) return(NULL)
  owner <- rule$owner %||% "base"
  if (!is_known_package_frame_call(expr, name, owner, scope, lookup)) {
    return(NULL)
  }
  definition <- rule$definition %||% get(
    rule$method %||% name, envir = asNamespace(owner), inherits = FALSE
  )
  formals <- rule$formals
  parts <- static_matched_arguments(expr, definition)
  if (!formals[[1L]] %in% names(parts) ||
        !all(names(parts) %in% formals) || anyDuplicated(names(parts)) ||
        any(vapply(parts, rlang::is_missing, logical(1)))) return(NULL)
  elements <- scope_closure_list(parts[[formals[[1L]]]], scope, lookup)
  if (is.null(elements)) return(NULL)

  if (isTRUE(rule$dispatch) &&
        !scope_implicit_class_default(name, owner, "list", scope, lookup)) {
    return(NULL)
  }

  scope_map_lists(elements, function(elements) {
    switch(name,
      rev = base::rev.default(elements),
      as.list = elements,
      unname = if (!"force" %in% names(parts) ||
                     rlang::is_bool(unparenthesized_value(parts$force))) {
        base::unname(elements)
      },
      structure = if ("names" %in% names(parts)) {
        scope_list_literal_names(elements, parts$names, scope, lookup)
      },
      setNames = if ("nm" %in% names(parts)) {
        scope_list_literal_names(elements, parts$nm, scope, lookup)
      },
      rep = {
        times <- scope_literal_list_count(parts, "times", 1L, scope, lookup)
        # Retain small literal repetitions without expanding an unbounded list.
        limit <- max(1024L, length(elements)) / max(1L, length(elements))
        if (!is.null(times) && times >= 0L && times <= limit) {
          base::rep(elements, times = times)
        }
      },
      tail = {
        n <- scope_literal_list_count(parts, "n", 6L, scope, lookup)
        if (!is.null(n)) definition(elements, n = n)
      }
    )
  })
}

# Read one finite integer count without evaluating a supplied expression.
scope_literal_list_count <- function(parts, name, default, scope, lookup) {
  if (!name %in% names(parts)) return(default)
  count <- scope_literal_list_index(parts[[name]], scope, lookup)
  if (typeof(count) %in% c("integer", "double") && length(count) == 1L &&
        is.finite(count) && count == trunc(count)) count
}

# Prove that known dispatch classes reach the package's default method.
# Inspect possible method bindings without invoking or forcing caller code.
scope_implicit_class_default <- function(name, owner, class, scope, lookup) {
  methods <- paste0(name, c(paste0(".", class), ".default"))
  default_method <- paste0(name, ".default")
  rebound <- lookup$function_rebound %||% lookup$rebound
  if (is.null(lookup$env) ||
    any(c(methods, unresolved_lookup_name()) %in% rebound) ||
    any(methods %in% scope_bound_names(scope)) ||
    !lookup_environment_is_external(
      lookup$env, character(), lookup, safe_parents = FALSE
    )) return(FALSE)

  namespace <- asNamespace(owner)
  default <- get(default_method, envir = namespace, inherits = FALSE)
  registry <- get(".__S3MethodsTable__.", envir = namespace, inherits = FALSE)
  absent <- rlang::zap()
  for (method in methods) {
    for (env in list(lookup$env, registry)) {
      binding <- ordinary_function_binding(method, env, owner, absent)
      if (!identical(binding, absent) &&
            !(identical(method, default_method) &&
                identical(binding, default))) return(FALSE)
    }
  }
  TRUE
}

# Retain only statically visible callable or plain-list member values.
scope_closure_list_element <- function(expr, scope, lookup) {
  closure <- scope_value_closure(expr, scope, lookup)
  if (!is.null(closure)) return(closure)
  elements <- scope_closure_list(expr, scope, lookup)
  if (!is.null(elements)) list(elements = elements) else NULL
}

# Move every contained closure between stack and binding-relative offsets.
scope_shift_closure_elements <- function(elements, offset) {
  scope_map_closure_elements(elements, function(closure) {
    closure$enclosing <- closure$enclosing + offset
    closure
  })
}

# Known literal closure variants of a descriptor; other outcomes stay opaque.
scope_closure_variants <- function(closure) {
  if (is.null(closure)) return(list())
  if (!is.null(closure$alternatives)) return(closure$alternatives)
  if (!is.null(closure$definition)) list(closure) else list()
}

# A static predicate selects variants with proven unchanged results.
# Unproven transfer outcomes remain opaque rather than erasing known results.
scope_filter_closure <- function(closure, predicate) {
  variants <- scope_closure_variants(closure)
  proven <- vapply(variants, predicate, logical(1))
  if (!any(proven)) return(NULL)
  if (all(proven)) return(closure)
  list(alternatives = variants[proven])
}

# A list descriptor can hold several layouts without aligning their members.
# The tag belongs only to parser-created descriptors, never caller values.
scope_list_choices <- function(elements) {
  if (is.null(elements) || rlang::is_zap(elements)) return(list())
  attr(elements, "marginplyr_lists", exact = TRUE) %||% list(elements)
}

# Join known list results while retaining each candidate's names and indices.
scope_join_lists <- function(values) {
  choices <- unique(unlist(lapply(values, scope_list_choices),
                           recursive = FALSE))
  if (length(choices) == 0L) return(NULL)
  if (length(choices) == 1L) return(choices[[1L]])
  structure(list(), marginplyr_lists = choices)
}

# Apply an existing operation to each layout, retaining unhandled inputs.
scope_map_lists <- function(elements, transform) {
  values <- lapply(scope_list_choices(elements), function(choice) {
    value <- transform(choice)
    if (rlang::is_zap(value)) choice else value
  })
  scope_join_lists(values)
}

# Combine candidate arguments before applying an existing multi-list operation.
scope_list_product <- function(values, transform) {
  combinations <- list(list())
  for (value in values) {
    next_combinations <- list()
    for (prefix in combinations) {
      for (choice in scope_list_choices(value)) {
        next_combinations <- c(next_combinations, list(c(prefix, list(choice))))
      }
    }
    combinations <- next_combinations
  }
  scope_join_lists(lapply(combinations, function(parts) {
    names(parts) <- names(values)
    transform(parts)
  }))
}

# Known callable and nested-list possibilities for one selected member.
scope_join_members <- function(members) {
  members <- unique(Filter(Negate(is.null), members))
  if (length(members) == 0L) return(NULL)
  if (length(members) == 1L) return(members[[1L]])
  variants <- unique(unlist(lapply(members, scope_closure_variants),
                            recursive = FALSE))
  elements <- scope_join_lists(lapply(members, "[[", "elements"))
  result <- list()
  if (length(variants) > 0L) result$alternatives <- variants
  if (!is.null(elements)) result$elements <- elements
  if (length(result) > 0L) result
}

# Join field values without choosing which conditional path executes.
# Descriptors on both sides use offsets relative to the same binding frame.
scope_possible_fields <- function(before, after) {
  if (is.null(before) || is.null(after)) return(NULL)
  keys <- union(names(before), names(after))
  fields <- lapply(keys, function(key) {
    first <- before[[key]]
    second <- after[[key]]
    if (identical(first, second)) return(first)
    scope_join_members(list(first, second))
  })
  stats::setNames(fields, keys)
}

# Environment identities are exact until conditional paths select several.
scope_environment_identities <- function(identity) {
  if (is.environment(identity)) list(identity) else identity$possible
}

# Direct copies share a token even when their underlying object is conditional.
scope_environment_alias <- function(identity) {
  if (is.environment(identity)) identity else identity$alias
}

# Transform closure descriptors at any depth in a known list.
scope_map_closure_elements <- function(elements, transform) {
  if (!is.null(attr(elements, "marginplyr_lists", exact = TRUE))) {
    return(scope_map_lists(elements, function(choice) {
      scope_map_closure_elements(choice, transform)
    }))
  }
  lapply(elements, function(element) {
    if (!is.null(element$definition)) {
      return(transform(element))
    } else if (!is.null(element$alternatives)) {
      element$alternatives <- scope_map_closure_elements(
        element$alternatives, transform
      )
    }
    if (!is.null(element$elements)) {
      element$elements <- scope_map_closure_elements(
        element$elements, transform
      )
    }
    element
  })
}

# Find a local literal closure using function or value lookup as written.
scope_closure_binding <- function(name, scope, function_lookup,
                                  visible = scope_visible_frames(scope)) {
  for (index in visible) {
    frame <- scope[[index]]
    if (!name %in% frame$bound ||
          (function_lookup && identical(frame$kinds[[name]], "value"))) {
      next
    }
    if (!identical(frame$kinds[[name]], "function")) {
      break
    }
    closure <- frame$closures[[name]]
    if (!is.null(closure)) {
      return(scope_shift_closure_elements(list(closure), index)[[1L]])
    }
    promise <- scope_promise(name, scope, function_lookup, visible = index)
    if (!is.null(promise)) {
      definition <- literal_function_definition(promise$expression)
      if (!is.null(definition)) {
        return(list(
          definition = definition, enclosing = promise$evaluation[[1L]]
        ))
      }
    }
    break
  }
  NULL
}

# Stop following recursive calls to the same literal and enclosing frame.
scope_closure_is_active <- function(closure, scope) {
  any(vapply(seq_along(scope), function(index) {
    frame <- scope[[index]]
    identical(frame$definition, closure$definition) &&
      identical(index + (frame$closure_origin %||% frame$enclosing),
                closure$enclosing)
  }, logical(1)))
}

# Retained factory frames precede the live stack. Their last frame links its
# lexical enclosure and original caller to that stack at invocation.
scope_closure_scope <- function(closure, scope) {
  frames <- closure$frames
  if (length(frames) == 0L) {
    return(scope)
  }
  frames[[length(frames)]]$enclosing <- closure$enclosing
  frames[[length(frames)]]$caller <- closure$enclosing
  c(frames, scope)
}

# Match function formals to pending defaults in their own frame
# and pending arguments in the caller. Unmatched syntax stays conservative.
function_scope_frame <- function(parameters, invocation) {
  formals <- as.list(parameters)
  defaults <- formals[!vapply(formals, rlang::is_missing, logical(1))]
  supplied <- dots <- unmatched <- list()
  if (!is.null(invocation) && length(invocation) > 1L) {
    actual <- static_call_args(invocation)
    matched <- static_matched_arguments(
      rlang::call2(".inline", !!!actual),
      rlang::new_function(parameters, NULL, emptyenv()),
      expand_dots = FALSE
    )
    if (is.null(matched)) {
      unmatched <- actual
    } else {
      dots <- as.list(matched[["..."]])
    }
    supplied <- matched[setdiff(names(matched), "...")]
    supplied <- supplied[!vapply(supplied, rlang::is_missing, logical(1))]
    defaults[names(supplied)] <- NULL
  }
  list(
    bound = names(formals), defaults = defaults, arguments = supplied,
    dots = dots, unmatched = unmatched,
    kinds = Filter(Negate(is.null), lapply(
      c(defaults, supplied), scope_literal_kind
    )),
    invoked = !is.null(invocation)
  )
}

# Read defaults and the body under the definition's formals. A closure can
# be called through syntax the walk does not enter, so inspect its definition
# as well as visible calls. Only a call can affect its enclosure.
# Frame offsets distinguish the lexical parent, live closure origin and
# dynamic caller when a retained factory frame lies between them.
definition_data_symbols <- function(expr, scope, lookup, invocation = NULL,
                                    enclosing = 1L,
                                    closure_origin = enclosing, caller = 1L) {
  if (is.null(invocation)) {
    # A definition can capture names without visibly demanding their promises.
    scope <- lapply(scope, function(frame) {
      frame$defaults <- frame$arguments <- frame$dots <- NULL
      frame$dot_closures <- NULL
      frame
    })
  }
  frame <- function_scope_frame(expr[[2L]], invocation)
  frame$enclosing <- enclosing
  frame$closure_origin <- closure_origin
  frame$caller <- caller
  frame$definition <- expr
  inner <- c(list(frame), scope)
  unique(unlist(
    lapply(
      c(frame$defaults, list(expr[[3L]])),
      scoped_data_symbols, scope = inner, lookup = lookup
    ),
    use.names = FALSE
  ))
}

# The current frame and its lexical parents, skipping dynamic callers.
scope_visible_frames <- function(scope, index = 1L) {
  visible <- integer()
  while (index <= length(scope)) {
    visible <- c(visible, index)
    index <- index + (scope[[index]]$enclosing %||% 1L)
  }
  visible
}

# Names visible from the current frame, regardless of which one owns them.
scope_bound_names <- function(scope) {
  unlist(lapply(scope[scope_visible_frames(scope)], "[[", "bound"),
         use.names = FALSE)
}

# Numbered dots select a position in the first visible dots binding.
scope_numbered_dot <- function(name, scope, visible) {
  if (!startsWith(name, "..") ||
        !is_numbered_dot_reference(rlang::sym(name))) return(NULL)
  for (index in visible) {
    if ("..." %in% scope[[index]]$bound) {
      return(list(binding = index, slot = as.double(substring(name, 3L))))
    }
  }
  NULL
}

# Find the first visible binding and the frame in which its pending syntax
# runs. Clear the promise before following recursive reads.
scope_promise <- function(name, scope, function_lookup,
                          visible = seq_along(scope)) {
  dot <- if (!function_lookup) scope_numbered_dot(name, scope, visible)
  if (!is.null(dot)) {
    index <- dot$binding
    slot <- dot$slot
    dots <- scope[[index]]$dots
    if (slot > length(dots) || rlang::is_missing(dots[[slot]])) {
      return(NULL)
    }
    # Consume the promise while retaining the position and argument tag.
    scope[[index]]$dots[slot] <- list(rlang::missing_arg())
    return(list(
      expression = dots[[slot]], scope = scope, binding = index, slot = slot,
      evaluation = seq.int(index + (scope[[index]]$caller %||% 1L),
                           length(scope))
    ))
  }
  for (index in visible) {
    frame <- scope[[index]]
    if (!name %in% frame$bound ||
          (function_lookup && identical(frame$kinds[[name]], "value"))) {
      next
    }
    if (name %in% names(frame$defaults)) {
      expression <- frame$defaults[[name]]
      scope[[index]]$defaults[name] <- NULL
      return(list(
        expression = expression, scope = scope,
        evaluation = seq.int(index, length(scope)), binding = index
      ))
    }
    if (name %in% names(frame$arguments)) {
      expression <- frame$arguments[[name]]
      scope[[index]]$arguments[name] <- NULL
      return(list(
        expression = expression, scope = scope,
        evaluation = seq.int(index + (frame$caller %||% 1L), length(scope)),
        binding = index
      ))
    }
    break
  }
  NULL
}

# Read a pending closure value in its evaluation frame and restore its offset.
scope_promise_closure <- function(promise, lookup) {
  if (is.null(promise)) return(NULL)
  frames <- promise$evaluation
  closure <- scope_value_closure(
    promise$expression, promise$scope[frames], lookup
  )
  if (!is.null(closure)) {
    closure <- scope_shift_closure_elements(
      list(closure), frames[[1L]] - 1L
    )[[1L]]
  }
  closure
}

# The dependencies of syntax that a lexical read may force.
scope_promise_symbols <- function(name, scope, lookup,
                                  function_lookup = FALSE) {
  if (!function_lookup) {
    closure <- scope_closure_binding(name, scope, function_lookup = FALSE)
    if (!is.null(closure)) {
      # Selecting a tracked list member does not invoke callable alternatives.
      if (isTRUE(lookup$defer_definitions) && !is.null(closure$elements)) {
        return(character())
      }
      variants <- scope_closure_variants(closure)
      return(unique(unlist(lapply(variants, function(variant) {
        if (length(variant$unknown_definition_parts) > 0L) {
          return(unresolved_lookup_name())
        }
        if (scope_closure_is_active(variant, scope)) return(character())
        enclosure <- if (variant$enclosing <= length(scope)) {
          seq.int(variant$enclosing, length(scope))
        } else {
          integer()
        }
        definition_data_symbols(
          variant$definition, c(variant$frames, scope[enclosure]), lookup,
          closure_origin = 1L + length(variant$frames)
        )
      }), use.names = FALSE)))
    }
  }
  promise <- scope_promise(name, scope, function_lookup,
                           visible = scope_visible_frames(scope))
  if (is.null(promise)) {
    return(character())
  }
  scoped_data_symbols(
    promise$expression, promise$scope[promise$evaluation], lookup
  )
}

# Effects of demanding a pending promise in its evaluation frame.
scope_after_promise <- function(name, scope, lookup, function_lookup = FALSE) {
  promise <- scope_promise(name, scope, function_lookup,
                           visible = scope_visible_frames(scope))
  if (is.null(promise)) {
    return(scope)
  }
  scope <- promise$scope
  frames <- promise$evaluation
  binding <- promise$binding
  closure <- if (!is.null(binding)) scope_promise_closure(promise, lookup)
  elements <- if (is.null(closure) && is.null(promise$slot)) {
    scope_closure_list(promise$expression, scope[frames], lookup)
  }
  scope[frames] <- statement_reads_and_scope(
    promise$expression, scope[frames], lookup, read_symbols = no_scope_reads
  )$scope
  # A forced argument retains its value even if its source is later removed.
  if (!is.null(promise$slot)) {
    if (!is.null(closure) && "..." %in% scope[[binding]]$bound) {
      closure <- scope_shift_closure_elements(list(closure), -binding)[[1L]]
      scope[[binding]]$dot_closures[[promise$slot]] <- closure
    }
  } else if (!is.null(closure) && name %in% scope[[binding]]$bound) {
    closure <- scope_shift_closure_elements(list(closure), -binding)[[1L]]
    scope[[binding]]$kinds[[name]] <- "function"
    scope[[binding]]$closures[[name]] <- closure
  } else if (!is.null(elements) && name %in% scope[[binding]]$bound) {
    scope[[binding]]$kinds[[name]] <- "value"
    scope[[binding]]$closure_lists[[name]] <- scope_shift_closure_elements(
      elements, frames[[1L]] - binding - 1L
    )
  }
  scope
}

# Locate the first visible binding with proven fresh environment fields.
scope_fresh_env_binding <- function(expr, scope) {
  expr <- unparenthesized_value(expr)
  if (!is_name_part(expr)) return(NULL)
  name <- rlang::as_name(expr)
  for (index in scope_visible_frames(scope)) {
    frame <- scope[[index]]
    if (name %in% frame$bound) {
      return(if (!is.null(frame$closure_environments[[name]])) index)
    }
  }
  NULL
}

# Update every known alias of a fresh carrier with frame-relative fields.
scope_set_environment_fields <- function(scope, frame, name, fields) {
  identity <- scope[[frame]]$closure_environment_ids[[name]]
  identities <- scope_environment_identities(identity)
  for (index in seq_along(scope)) {
    aliases <- names(scope[[index]]$closure_environment_ids)
    for (alias in aliases) {
      target <- scope[[index]]$closure_environment_ids[[alias]]
      # intersect() coerces environment lists; compare the tokens themselves.
      possible <- scope_environment_identities(target)
      common <- any(vapply(possible, function(token) {
        any(vapply(identities, identical, logical(1), y = token))
      }, logical(1)))
      if (common) {
        updated <- if (!is.null(fields)) {
          scope_shift_closure_elements(fields, frame - index)
        }
        same_binding <- identical(index, frame) && identical(alias, name)
        definite_alias <- identical(scope_environment_alias(identity),
                                    scope_environment_alias(target))
        scope[[index]]$closure_environments[[alias]] <-
          if (same_binding || definite_alias) {
            updated
          } else {
            scope_possible_fields(
              scope[[index]]$closure_environments[[alias]], updated
            )
          }
      }
    }
  }
  scope
}

# Describe fields of a fresh environment from a verified base constructor.
# The selected literal list keeps the closures' original enclosing frames.
scope_fresh_env_fields <- function(expr, scope, lookup) {
  operator <- static_call_name(expr)
  if (is.null(operator) || !operator %in% c("new.env", "list2env") ||
        is_parenthesized(static_call_head(expr)) ||
        !is_known_base_frame_call(expr, operator, scope, lookup)) {
    return(NULL)
  }
  if (identical(operator, "new.env")) return(list())
  args <- static_matched_arguments(expr, base::list2env)
  if (!"x" %in% names(args) || rlang::is_missing(args$x) ||
        !is.null(unparenthesized_value(args$envir))) {
    return(NULL)
  }
  values <- args$x
  if (!identical(static_call_name(values), "list") ||
        is_parenthesized(static_call_head(values)) ||
        !is_known_base_frame_call(values, "list", scope, lookup)) return(NULL)
  elements <- scope_closure_list(values, scope, lookup)
  keys <- names(elements)
  if (length(elements) != length(keys) || anyNA(keys) || !all(nzchar(keys))) {
    return(NULL)
  }
  # list2env() assigns repeated names in order, keeping the last value.
  elements[!duplicated(keys, fromLast = TRUE)]
}

# A definite local binding replaces any promise in this frame. Literal
# values establish whether function lookup stops or skips the binding.
# value_scope holds bindings before the value expression evaluates.
scope_bind_local <- function(scope, name, value = NULL, lookup = NULL,
                             value_scope = scope) {
  closure <- scope_value_closure(value, value_scope, lookup)
  elements <- if (!is.null(closure)) {
    closure$elements
  } else if (!missing(value)) {
    scope_closure_list(value, value_scope, lookup)
  }
  fields <- scope_fresh_env_fields(value, value_scope, lookup)
  carrier <- scope_fresh_env_binding(value, value_scope)
  identity <- if (!is.null(fields)) new.env(parent = emptyenv())
  if (!is.null(carrier)) {
    source <- rlang::as_name(unparenthesized_value(value))
    fields <- scope_shift_closure_elements(
      value_scope[[carrier]]$closure_environments[[source]], carrier
    )
    identity <- value_scope[[carrier]]$closure_environment_ids[[source]]
    # A local alias does not let caller code mutate the carrier.
    for (index in seq_along(scope)) {
      for (alias in names(value_scope[[index]]$closure_environments)) {
        if (alias %in% scope[[index]]$bound && identical(
          scope[[index]]$closure_environment_ids[[alias]],
          value_scope[[index]]$closure_environment_ids[[alias]]
        )) {
          scope[[index]]$closure_environments[[alias]] <-
            value_scope[[index]]$closure_environments[[alias]]
        }
      }
    }
  }
  environment_frames <- static_environment_frames(value, value_scope, lookup)
  environment_definite <- !is.null(
    static_environment_frame(value, value_scope, lookup)
  )
  scope[[1L]]$bound <- union(scope[[1L]]$bound, name)
  scope[[1L]]$defaults[name] <- NULL
  scope[[1L]]$arguments[name] <- NULL
  scope[[1L]]$kinds[[name]] <- if (!is.null(closure)) {
    "function"
  } else if (!is.null(elements) || !is.null(fields) || environment_definite) {
    "value"
  } else if (!missing(value)) {
    scope_literal_kind(value)
  }
  scope[[1L]]$closures[[name]] <- if (!is.null(closure)) {
    scope_shift_closure_elements(list(closure), -1L)[[1L]]
  }
  scope[[1L]]$closure_lists[[name]] <- if (!is.null(elements)) {
    scope_shift_closure_elements(elements, -1L)
  }
  scope[[1L]]$closure_environments[[name]] <- if (!is.null(fields)) {
    scope_shift_closure_elements(fields, -1L)
  }
  scope[[1L]]$closure_environment_ids[[name]] <- identity
  # Offsets stay relative to the binding's frame when a child scope is added.
  scope[[1L]]$environments[[name]] <- if (length(environment_frames) > 0L) {
    list(
      offsets = environment_frames - 1L,
      definite = environment_definite
    )
  }
  scope
}

# Bind tracked list elements in the current frame, discarding the old value.
# Element descriptors arrive with offsets relative to the current scope.
scope_bind_closure_list <- function(scope, name, elements) {
  scope <- scope_bind_local(scope, name)
  scope[[1L]]$kinds[[name]] <- "value"
  scope[[1L]]$closure_lists[[name]] <- scope_shift_closure_elements(
    elements, -1L
  )
  scope
}

# Carry writes to closure attributes, list members or fresh environment fields.
# Stored closures use offsets relative to the current frame.
scope_after_carrier_write <- function(scope, target, value, lookup,
                                      value_scope) {
  if (!rlang::is_call(target) || rlang::is_missing(value)) return(scope)
  replacement <- scope_definition_write(target, value)
  if (!is.null(replacement)) {
    if (is_name_part(unparenthesized_value(replacement$target))) {
      return(scope_bind_local(
        scope, rlang::as_name(unparenthesized_value(replacement$target)),
        replacement$value, lookup, value_scope
      ))
    }
    return(scope_after_carrier_write(
      scope, replacement$target, replacement$value, lookup, value_scope
    ))
  }
  updated <- scope_closure_attribute_write(
    scope, target, value, lookup, value_scope
  )
  if (!is.null(updated)) return(updated)
  members <- c("[", "[[", "$")
  name_setters <- c("names", "attr", "attributes")
  path <- list()
  carrier <- unparenthesized_value(target)
  while (rlang::is_call(carrier)) {
    operator <- static_call_name(carrier)
    allowed <- if (length(path) == 0L) c(members, name_setters) else members
    if (is.null(operator) || !operator %in% allowed ||
          length(carrier) < 2L || rlang::is_missing(carrier[[2L]])) {
      return(scope)
    }
    path[[length(path) + 1L]] <- carrier
    carrier <- unparenthesized_value(carrier[[2L]])
  }
  if (length(path) == 0L || !is_name_part(carrier)) return(scope)
  name <- rlang::as_name(carrier)
  frame <- scope_fresh_env_binding(carrier, scope)
  if (!is.null(frame)) {
    return(scope_after_environment_write(scope, frame, name, path, value,
                                         lookup, value_scope))
  }
  elements <- value
  carrier_changed <- FALSE
  for (index in seq_along(path)) {
    target <- path[[index]]
    parent <- scope_closure_list(target[[2L]], value_scope, lookup)
    if (is.null(parent)) return(scope)
    operator <- static_call_name(target)
    setter <- paste0(operator, "<-")
    replacement <- rlang::call2(setter, .ns = static_call_ns(target))
    elements <- if (!is_known_base_frame_call(
      replacement, setter, value_scope, lookup
    )) {
      # An inner setter can mutate an unshared member before this call.
      if (!carrier_changed && scope_replacement_keeps_input(
        replacement, target, value_scope, lookup
      )) parent
    } else if (operator %in% name_setters) {
      scope_list_write_names(target, parent, elements, lookup, value_scope)
    } else {
      scope_list_write_elements(
        target, parent, elements, lookup, value_scope, nested = index > 1L
      )
    }
    if (rlang::is_zap(elements)) return(scope)
    carrier_changed <- !identical(elements, parent)
  }
  if (is.null(elements)) return(scope_bind_local(scope, name))
  scope_bind_closure_list(scope, name, elements)
}

# Follow a proven setter result; base setters retain the closure with separate
# dispatch classes. Unknown attributes cannot establish default dispatch.
scope_closure_attribute_write <- function(scope, target, value, lookup,
                                          value_scope) {
  operator <- static_call_name(target)
  if (is.null(operator) || !operator %in% c("class", "attr", "attributes") ||
        length(target) < 2L || !is_name_part(target[[2L]]) ||
        is.null(scope_value_closure(target[[2L]], value_scope, lookup))) {
    return(NULL)
  }
  name <- rlang::as_name(target[[2L]])
  setter <- paste0(operator, "<-")
  replacement <- rlang::call2(setter, .ns = static_call_ns(target))
  if (!is_known_base_frame_call(replacement, setter, value_scope, lookup)) {
    if (scope_replacement_keeps_input(
      replacement, target, value_scope, lookup
    )) {
      return(scope_bind_local(scope, name, target[[2L]], lookup, value_scope))
    }
    args <- static_call_args(target)
    names(args) <- replace(rlang::names2(args), 1L, "")
    invocation <- rlang::call2(static_call_head(replacement), !!!args,
                               value = value)
    return(scope_bind_local(scope, name, invocation, lookup, value_scope))
  }
  scope <- scope_bind_local(scope, name, target[[2L]], lookup, value_scope)
  if (identical(operator, "attr") && length(target) == 3L &&
        rlang::is_string(unparenthesized_value(target[[3L]])) &&
        !identical(unparenthesized_value(target[[3L]]), "class")) {
    # A base non-class attribute write preserves the closure's dispatch classes.
    return(scope)
  }
  classes <- NULL
  if (identical(operator, "class") && length(target) == 2L) {
    classes <- if (is.null(unparenthesized_value(value))) {
      "function"
    } else {
      static_character_value(value, function(expr) {
        identical(static_call_name(expr), "c") &&
          is_known_base_frame_call(expr, "c", value_scope, lookup)
      })
    }
    if (!is.null(classes) && length(classes) == 0L) classes <- "function"
  }
  # Update one outer callable, retaining its associated list element metadata.
  update_dispatch <- function(closure) {
    closure$dispatch_classes <- classes
    closure$dispatch_unknown <- is.null(classes)
    closure
  }
  closure <- scope[[1L]]$closures[[name]]
  if (is.null(closure$alternatives)) {
    closure <- update_dispatch(closure)
  } else {
    closure$alternatives <- lapply(scope_closure_variants(closure),
                                   update_dispatch)
  }
  scope[[1L]]$closures[[name]] <- closure
  scope
}

# A base $<- or [[<- on a fresh local carrier changes an exact field in place.
# An unknown write or a write through an enclosing frame loses the field proof.
scope_after_environment_write <- function(scope, frame, name, path, value,
                                          lookup, value_scope) {
  target <- path[[1L]]
  operator <- static_call_name(target)
  setter <- paste0(operator, "<-")
  replacement <- rlang::call2(setter, .ns = static_call_ns(target))
  fields <- NULL
  if (frame == 1L && length(path) == 1L && length(target) == 3L &&
        operator %in% c("$", "[[") &&
        is_known_base_frame_call(replacement, setter, value_scope, lookup)) {
    key <- if (identical(operator, "$") && is_name_part(target[[3L]])) {
      rlang::as_name(target[[3L]])
    } else {
      static_character_value(target[[3L]])
    }
    if (rlang::is_string(key) && nzchar(key)) {
      fields <- scope[[frame]]$closure_environments[[name]]
      member <- scope_closure_list_element(value, value_scope, lookup)
      fields[key] <- scope_shift_closure_elements(list(member), -1L)
    }
  }
  scope <- scope_set_environment_fields(scope, frame, name, fields)
  if (is.null(fields)) scope_bind_local(scope, name) else scope
}

# Apply a base member setter to known list elements. An enclosing setter gets
# the updated carrier's descriptors. NULL means unknown; zap() means unhandled.
scope_list_write_elements <- function(target, elements, value, lookup,
                                      value_scope, nested) {
  if (!is.null(attr(elements, "marginplyr_lists", exact = TRUE))) {
    return(scope_map_lists(elements, function(choice) {
      scope_list_write_elements(target, choice, value, lookup, value_scope,
                                nested)
    }))
  }
  if (length(target) != 3L) return(rlang::zap())
  operator <- static_call_name(target)
  if (rlang::is_missing(target[[3L]])) return(rlang::zap())
  key <- unparenthesized_value(target[[3L]])
  if (is.object(key)) return(rlang::zap())
  delete <- !nested && is.null(unparenthesized_value(value))
  if (identical(operator, "[")) {
    if (!typeof(key) %in%
          c("NULL", "integer", "double", "logical", "character") ||
          anyNA(key)) return(rlang::zap())
    if (typeof(key) %in% c("integer", "double") &&
          (!all(is.finite(key)) || (any(key < 0) && any(key > 0)))) {
      return(rlang::zap())
    }
    replacement <- if (nested) {
      value
    } else {
      scope_closure_list(value, value_scope, lookup)
    }
    if (is.null(replacement)) {
      return(if (nested) NULL else rlang::zap())
    }
    if (delete) {
      elements[key] <- NULL
      return(elements)
    }
    return(scope_map_lists(replacement, function(choice) {
      # An unhandled replacement retains the target, not the replacement list.
      if (length(choice) == 0L) return(elements)
      updated <- elements
      updated[key] <- choice
      updated
    }))
  }

  path <- if (identical(operator, "[[")) {
    scope_literal_list_index(key, value_scope, lookup)
  } else if (is_name_part(key)) {
    rlang::as_name(key)
  } else if (rlang::is_string(key)) {
    key
  }
  if (length(path) == 0L) return(rlang::zap())
  member <- if (nested) {
    if (!is.null(value)) list(elements = value)
  } else if (!delete) {
    scope_closure_list_element(value, value_scope, lookup)
  }
  scope_replace_list_member(elements, path, member, delete)
}

# Replace or delete at a known exact path. Recursive [[<- traverses plain
# list members directly and can extend only the innermost tracked list.
scope_replace_list_member <- function(elements, path, member, delete) {
  if (!is.null(attr(elements, "marginplyr_lists", exact = TRUE))) {
    return(scope_map_lists(elements, function(choice) {
      scope_replace_list_member(choice, path, member, delete)
    }))
  }
  key <- path[[1L]]
  index <- scope_list_member_index(elements, key)
  if (length(path) > 1L) {
    if (is.null(index) || is.null(elements[[index]]$elements)) {
      return(rlang::zap())
    }
    child <- scope_replace_list_member(
      elements[[index]]$elements, path[-1L], member, delete
    )
    if (rlang::is_zap(child)) return(child)
    elements[[index]]$elements <- child
  } else {
    if (is.null(index)) {
      if (!rlang::is_string(key) &&
            !(is.numeric(key) && trunc(key) == length(elements) + 1L)) {
        return(rlang::zap())
      }
      index <- length(elements) + 1L
    }
    if (delete) {
      elements[index] <- NULL
    } else {
      elements[index] <- list(member)
      if (rlang::is_string(key)) names(elements)[index] <- key
    }
  }
  elements
}

# Prove that a visible replacement closure returns the original input argument.
# The argument match uses a marker in place of R's untagged temporary input.
scope_replacement_keeps_input <- function(replacement, target, scope, lookup) {
  closure <- scope_call_closure(replacement, scope, lookup)
  if (is.null(closure) || !is.null(closure$alternatives) ||
        length(closure$unknown_definition_parts) > 0L) return(FALSE)
  definition <- closure$definition
  result <- unparenthesized_value(definition[[3L]])
  if (rlang::is_call(result, "{", n = 1L)) {
    frame <- list(
      bound = names(definition[[2L]]),
      enclosing = if (length(closure$frames) > 0L) 1L else closure$enclosing
    )
    inner <- c(list(frame), scope_closure_scope(closure, scope))
    if (!is_known_base_frame_call(result, "{", inner, lookup)) return(FALSE)
    result <- unparenthesized_value(result[[2L]])
  }
  if (!is_name_part(result)) return(FALSE)
  input <- new.env(parent = emptyenv())
  args <- static_call_args(target)
  args[1L] <- list(input)
  names(args) <- replace(rlang::names2(args), 1L, "")
  call <- rlang::call2(static_call_name(replacement), !!!args, value = NULL)
  matched <- static_matched_arguments(
    call, rlang::new_function(definition[[2L]], NULL, emptyenv()),
    expand_dots = FALSE
  )
  identical(matched[[rlang::as_name(result)]], input)
}

# Update a tracked plain list's literal names for a known base setter.
# Return NULL for an unproven whole-list value or zap() for an unhandled write.
scope_list_write_names <- function(target, elements, value, lookup,
                                   value_scope) {
  operator <- static_call_name(target)
  replace_attributes <- identical(operator, "attributes")
  unknown <- if (replace_attributes) NULL else rlang::zap()
  if (identical(operator, "attr")) {
    if (length(target) != 3L ||
          rlang::is_missing(target[[3L]]) ||
          !identical(unparenthesized_value(target[[3L]]), "names")) {
      return(rlang::zap())
    }
  } else if (length(target) != 2L) {
    return(rlang::zap())
  }
  value <- unparenthesized_value(value)
  if (replace_attributes && !is.null(value)) {
    if (!rlang::is_call(value, "list") ||
          !is_known_base_frame_call(value, "list", value_scope, lookup)) {
      return(unknown)
    }
    attributes <- static_call_args(value)
    if (any(vapply(attributes, rlang::is_missing, logical(1))) ||
          !all(nzchar(rlang::names2(attributes)))) return(unknown)
    # Classes can dispatch and dimensions can change character indexing.
    dispatch_attributes <- names(attributes) %in% c("class", "dim", "dimnames")
    if (!all(vapply(attributes[dispatch_attributes], is.null, logical(1)))) {
      return(unknown)
    }
    selected <- which(names(attributes) == "names")
    if (length(selected) > 1L) return(unknown)
    value <- if (length(selected) == 1L) attributes[[selected]] else NULL
  }
  scope_list_literal_names(elements, value, value_scope, lookup, unknown)
}

# Replace names with known literal labels, using plain-list padding rules.
scope_list_literal_names <- function(elements, value, scope, lookup,
                                     unknown = NULL) {
  if (!is.null(attr(elements, "marginplyr_lists", exact = TRUE))) {
    return(scope_map_lists(elements, function(choice) {
      scope_list_literal_names(choice, value, scope, lookup, unknown)
    }))
  }
  value <- unparenthesized_value(value)
  labels <- if (!is.null(value)) {
    static_character_value(value, function(expr) {
      identical(static_call_name(expr), "c") &&
        is_known_base_frame_call(expr, "c", scope, lookup)
    })
  }
  if (!is.null(value) && is.null(labels)) return(unknown)
  if (length(labels) > length(elements)) return(unknown)
  names(elements) <- labels
  elements
}

# Whether the nearest local binding proves that a bare call head is a
# caller-defined function.
scope_has_local_function <- function(scope, name) {
  for (frame in scope[scope_visible_frames(scope)]) {
    kind <- frame$kinds[[name]]
    if (name %in% frame$bound && !identical(kind, "value")) {
      return(identical(kind, "function"))
    }
  }
  FALSE
}

# Return the invoked frame and the surviving outer bindings. Its pending
# arguments still hold syntax whose demand the body did not establish.
scope_after_closure_call <- function(expr, closure, scope, lookup) {
  if (!is.null(closure$alternatives)) {
    after <- scope
    for (variant in closure$alternatives) {
      reached <- scope_after_closure_call(expr, variant, scope, lookup)[-1L]
      after <- scope_possible_losses(after, reached)
    }
    return(c(list(list(arguments = static_call_args(expr))), after))
  }
  if (length(closure$unknown_definition_parts) > 0L) {
    return(c(list(list(arguments = static_call_args(expr))), scope))
  }
  definition <- closure$definition
  frame <- function_scope_frame(definition[[2L]], expr)
  captured <- length(closure$frames)
  frame$enclosing <- if (captured > 0L) 1L else closure$enclosing
  frame$closure_origin <- closure$enclosing + captured
  frame$caller <- 1L + captured
  frame$definition <- definition
  inner <- c(list(frame), scope_closure_scope(closure, scope))
  if (scope_closure_is_active(closure, scope)) {
    return(c(inner[1L], inner[-seq_len(1L + captured)]))
  }
  after <- statement_reads_and_scope(
    definition[[3L]], inner, lookup, read_symbols = no_scope_reads
  )$scope
  c(after[1L], after[-seq_len(1L + captured)])
}

# Effect-only walks use the statement rules without collecting dependencies.
no_scope_reads <- function(expr, scope, lookup) {
  character()
}

# Whether this node changes what is bound around it: `<-` and `=` bind their
# target, `for` binds its index, and `{` carries whatever its statements bind.
# The length is part of the answer for the reason it is at the `function` node:
# a call built by hand rather than parsed can carry one of these heads without
# the operands the grammar guarantees, and it must fall through to the general
# walk, which reports whatever its parts hold, rather than be read for an
# operand it does not have.
#
# `call_name` is passed rather than read here because the caller has already
# read it, and reading a node's name once is what the analysis sites in this
# package were folded down to (#163).
#
# `<<-` is deliberately absent. It assigns past the environment it runs in, so
# what it binds is not decidable from the expression, and reporting its target
# as a read leaves the error on the diagnostic side rather than the silent one.
is_binding_statement <- function(call_name, expr) {
  if (is.null(call_name)) {
    return(FALSE)
  }
  identical(call_name, "{") ||
    (call_name %in% c("<-", "=") && length(expr) >= 3L) ||
    (identical(call_name, "for") && length(expr) >= 4L)
}

# A block walks statements in order. Definite bindings carry into the
# next statement, and a removal can expose a name in an enclosing frame.
# A selected explicit return leaves later statements unreachable.
block_reads_and_scope <- function(expr, scope, lookup, read_symbols,
                                  read_result_symbols) {
  reads <- character()
  statements <- static_call_args(expr)
  for (index in seq_along(statements)) {
    step <- statement_reads_and_scope(
      statements[[index]], scope, lookup, read_symbols,
      if (index == length(statements)) read_result_symbols else read_symbols
    )
    reads <- c(reads, step$reads)
    scope <- step$scope
    if (isTRUE(step$returned)) {
      return(list(reads = unique(reads), scope = scope, returned = TRUE))
    }
  }
  list(reads = unique(reads), scope = scope)
}

# Find one closure assignment through base blocks.
scope_singleton_loop_write <- function(expr, scope, lookup) {
  while (rlang::is_call(expr, "{", n = 1L) &&
           is_known_base_frame_call(expr, "{", scope, lookup)) {
    expr <- expr[[2L]]
  }
  name <- static_call_name(expr)
  if (!rlang::is_call(expr, c("<-", "="), n = 2L) ||
        !(is_name_part(expr[[2L]]) || rlang::is_call(expr[[2L]])) ||
        !is_known_base_frame_call(expr, name, scope, lookup)) {
    return(NULL)
  }
  value <- expr[[3L]]
  if (is_name_part(value) || !is.null(literal_function_definition(value))) expr
}

# Read a statement and carry forward its removals and definite local bindings.
# A separate result reader can restrict the read of a returned value.
statement_reads_and_scope <- function(expr, scope, lookup = NULL,
                                      read_symbols = scoped_data_symbols,
                                      read_result_symbols = read_symbols) {
  expr <- unparenthesized_value(expr)
  call_name <- static_call_name(expr)
  arm <- scope_literal_branch(expr, scope, lookup)
  if (length(arm) > 0L && !rlang::is_missing(arm[[1L]])) {
    return(statement_reads_and_scope(
      arm[[1L]], scope, lookup, read_symbols, read_result_symbols
    ))
  }
  if (identical(call_name, "{")) {
    return(block_reads_and_scope(
      expr, scope, lookup, read_symbols, read_result_symbols
    ))
  }
  if (!is_binding_statement(call_name, expr)) {
    step <- list(
      reads = read_result_symbols(expr, scope, lookup),
      scope = scope_after_removals(expr, scope, lookup)
    )
    if (identical(call_name, "return") && length(expr) <= 2L &&
          is_known_base_frame_call(expr, call_name, scope, lookup)) {
      step$returned <- TRUE
    }
    return(step)
  }
  if (identical(call_name, "for")) {
    index <- expr[[2L]]
    sequence <- expr[[3L]]
    if (is_redundant_parens(sequence, ns = "") &&
          is_known_base_frame_call(rlang::call2("("), "(", scope, lookup)) {
      while (is_redundant_parens(sequence, ns = "") &&
               !rlang::is_missing(sequence[[2L]])) {
        sequence <- sequence[[2L]]
      }
    }
    literal_loop <- length(expr) == 4L && is_name_part(index) &&
      !rlang::is_missing(sequence) &&
      (is.null(sequence) ||
         (is.atomic(sequence) && !is.object(sequence) &&
            length(sequence) <= 1L)) &&
      is_known_base_frame_call(expr, call_name, scope, lookup)
    # R binds the index even for an empty sequence. The sequence runs first.
    reads <- read_symbols(expr[[3L]], scope, lookup)
    inner <- scope_after_removals(expr[[3L]], scope, lookup)
    if (literal_loop) {
      inner <- scope_bind_local(inner, rlang::as_name(index), sequence, lookup)
      if (length(sequence) == 0L) {
        return(list(reads = reads, scope = inner))
      }
      write <- scope_singleton_loop_write(expr[[4L]], inner, lookup)
      if (!is.null(write)) {
        step <- statement_reads_and_scope(write, inner, lookup, read_symbols)
        step$reads <- unique(c(reads, step$reads))
        return(step)
      }
    } else if (is_name_part(index)) {
      inner <- scope_bind_local(inner, rlang::as_name(index))
    }
    return(list(
      reads = unique(c(reads, read_symbols(expr[[4L]], inner, lookup))),
      scope = scope_after_removals(expr[[4L]], inner, lookup)
    ))
  }
  value <- read_symbols(expr[[3L]], scope, lookup)
  retained <- scope_after_removals(expr[[3L]], scope, lookup)
  if (is_name_part(expr[[2L]])) {
    return(list(
      reads = value,
      scope = scope_bind_local(retained, rlang::as_name(expr[[2L]]), expr[[3L]],
                               lookup, value_scope = scope)
    ))
  }
  # Read a replacement target and the value in source order. Its eventual
  # binding depends on the shape of the target.
  list(
    reads = unique(c(read_symbols(expr[[2L]], scope, lookup), value)),
    scope = scope_after_carrier_write(
      scope_after_removals(expr[[2L]], retained, lookup),
      expr[[2L]], expr[[3L]], lookup, value_scope = scope
    )
  )
}

# The body and child scope of a known base local() with its default environment.
# Explicit environments continue through the ordinary call walk.
implicit_local_parts <- function(expr, name, scope, lookup) {
  if (identical(name, "local") &&
        is_known_base_frame_call(expr, name, scope, lookup) &&
        length(base_call_argument(expr, "envir")) == 0L) {
    body <- base_call_argument(expr, "expr")
    if (length(body) > 0L) {
      child <- list(bound = character())
      list(body = body[[1L]], scope = c(list(child), scope))
    }
  }
}

# Carry visible removals through expressions. Literal closures and implicit
# base local() can carry effects out of a child frame. Conditional removals may
# run; literal FALSE arms cannot.
scope_after_removals <- function(expr, scope, lookup) {
  if (rlang::is_quosure(expr)) {
    lookup$env <- rlang::quo_get_env(expr)
  }
  if (is_name_part(expr)) {
    name <- rlang::as_name(expr)
    carrier <- scope_fresh_env_binding(expr, scope)
    if (!is.null(carrier)) {
      scope <- scope_set_environment_fields(scope, carrier, name, NULL)
    }
    return(scope_after_promise(name, scope, lookup))
  }
  if (!rlang::is_call(expr)) {
    return(scope)
  }
  if (is_parenthesized(expr)) {
    return(scope_after_removals(unparenthesized_value(expr), scope, lookup))
  }
  bound <- scope_bound_names(scope)
  name <- static_call_name(expr)
  if (identical(name, "invisible") && length(expr) == 2L &&
        !is.null(scope_fresh_env_binding(expr[[2L]], scope)) &&
        is_known_base_frame_call(expr, name, scope, lookup)) {
    return(scope_after_promise(
      rlang::as_name(unparenthesized_value(expr[[2L]])), scope, lookup
    ))
  }
  if (identical(name, "do.call")) {
    invocation <- scope_empty_do_call(expr, scope, lookup)
    if (!is.null(invocation)) {
      return(scope_after_removals(invocation, scope, lookup))
    }
  }
  assignment <- scope_local_closure_assignment(expr, scope, lookup)
  if (!is.null(assignment)) {
    retained <- scope_after_removals(assignment$value, scope, lookup)
    return(scope_bind_local(
      retained, assignment$name, assignment$value, lookup, value_scope = scope
    ))
  }
  if (identical(name, "force") && length(expr) == 2L &&
        is_known_package_call(expr, name, bound, lookup)) {
    return(statement_reads_and_scope(
      expr[[2L]], scope, lookup, read_symbols = no_scope_reads
    )$scope)
  }
  if (length(bound) == 0L && !any(lengths(lapply(scope, "[[", "bound")) > 0L)) {
    return(scope)
  }
  if (is_binding_statement(name, expr)) {
    after <- statement_reads_and_scope(
      expr, scope, lookup, read_symbols = no_scope_reads
    )$scope
    return(scope_possible_losses(scope, after))
  }
  if (identical(name, "function")) {
    return(scope)
  }
  local <- implicit_local_parts(expr, name, scope, lookup)
  if (!is.null(local)) {
    after <- statement_reads_and_scope(
      local$body, local$scope, lookup, read_symbols = no_scope_reads
    )$scope
    return(after[-1L])
  }
  mutation_lookup <- lookup
  if (!is.null(name) && name %in% c("rm", "remove") &&
        scope_has_local_function(scope, name)) {
    mutation_lookup$local_functions <- name
  }
  mutation <- scope_mutation_call(expr, bound, mutation_lookup)
  head <- static_call_head(expr)
  closure <- scope_call_closure(expr, scope, lookup)
  parts <- c(
    scope_call_args(expr, name, bound, lookup), evaluated_language_parts(expr)
  )
  # A proven base accessor reads the field without exposing its carrier.
  if (length(expr) == 3L && !is.null(name) && name %in% c("$", "[[") &&
        !is.null(scope_fresh_env_binding(expr[[2L]], scope)) &&
        is_known_base_frame_call(expr, name, scope, lookup)) {
    parts <- if (identical(name, "[[")) as.list(expr)[3L] else list()
  }
  if (is_name_part(head)) {
    scope <- scope_after_promise(
      rlang::as_name(head), scope, lookup, function_lookup = TRUE
    )
  } else if (rlang::is_call(head)) {
    scope <- scope_after_removals(head, scope, lookup)
  }
  if (!is.null(closure)) {
    after <- scope_after_closure_call(expr, closure, scope, lookup)
    scope <- after[-1L]
    parts <- c(after[[1L]]$arguments, after[[1L]]$dots, after[[1L]]$unmatched)
  }
  apply_effects <- scope_after_possible_removals
  if (length(literal_if_parts(expr, name, bound, lookup)) > 0L) {
    apply_effects <- scope_after_removals
  }
  for (index in seq_along(parts)) {
    scope <- apply_effects(parts[[index]], scope, lookup)
  }
  if (!is.null(mutation) &&
        static_call_name(mutation) %in% c("rm", "remove")) {
    scope <- scope_after_removal_call(mutation, scope, lookup)
  }
  if (!is.null(name) &&
        name %in% c("<<-", "assign", "delayedAssign", "makeActiveBinding")) {
    scope <- scope_forget_kinds(scope, lookup_assignment_names(expr))
  }
  if (!is.null(mutation) && identical(static_call_name(mutation), "list2env")) {
    scope <- scope_forget_kinds(scope, list2env_setter_names(mutation, lookup))
  }
  scope
}

# A literal base assignment installs a visible closure in the current frame
# when its evaluation and assignment environments are both that frame.
scope_local_closure_assignment <- function(expr, scope, lookup) {
  transfer_lookup <- lookup
  transfer_lookup$rebound <- lookup$function_rebound %||% lookup$rebound
  callee <- static_call_name(expr)
  if (length(scope) == 0L || is.null(callee) ||
        !callee %in% c("assign", "delayedAssign") ||
        !is_known_base_frame_call(expr, callee, scope, transfer_lookup)) {
    return(NULL)
  }
  name <- base_call_argument(expr, "x")
  value <- base_call_argument(expr, "value")
  delayed <- identical(callee, "delayedAssign")
  target <- base_call_argument(
    expr, if (delayed) "assign.env" else c("envir", "pos")
  )
  inherited <- if (!delayed) base_call_argument(expr, "inherits")
  evaluation <- if (delayed) base_call_argument(expr, "eval.env")
  local_target <- length(target) == 0L ||
    identical(static_environment_frame(target[[1L]], scope, lookup), 1L)
  local_evaluation <- length(evaluation) == 0L ||
    identical(static_environment_frame(evaluation[[1L]], scope, lookup), 1L)
  if (length(name) != 1L || !rlang::is_string(name[[1L]]) ||
        length(value) != 1L || !local_target || !local_evaluation ||
        (length(inherited) > 0L && !identical(inherited[[1L]], FALSE))) {
    return(NULL)
  }
  if (delayed && is.null(literal_function_definition(value[[1L]]))) {
    return(NULL)
  }
  if (is.null(scope_value_closure(value[[1L]], scope, lookup))) {
    return(NULL)
  }
  list(name = name[[1L]], value = value[[1L]])
}

# Reflective or nonlocal writes can invalidate a known literal binding kind.
scope_forget_kinds <- function(scope, assigned) {
  lapply(scope, function(frame) {
    frame$kinds <- if (unresolved_lookup_name() %in% assigned) {
      NULL
    } else {
      frame$kinds[!names(frame$kinds) %in% assigned]
    }
    frame$closures <- frame$closures[
      names(frame$closures) %in% names(frame$kinds)
    ]
    frame$closure_lists <- frame$closure_lists[
      names(frame$closure_lists) %in% names(frame$kinds)
    ]
    frame$closure_environments <- frame$closure_environments[
      names(frame$closure_environments) %in% names(frame$kinds)
    ]
    frame$closure_environment_ids <- frame$closure_environment_ids[
      names(frame$closure_environment_ids) %in% names(frame$kinds)
    ]
    # A possible environment target can survive a loss of its binding kind.
    frame$environments <- if (unresolved_lookup_name() %in% assigned) {
      NULL
    } else {
      frame$environments[!names(frame$environments) %in% assigned]
    }
    frame
  })
}

# An argument or conditional arm may never be evaluated. Keep its possible
# removals without assuming it consumed defaults or installed new bindings.
scope_after_possible_removals <- function(expr, scope, lookup) {
  scope_possible_losses(scope, scope_after_removals(expr, scope, lookup))
}

# Preserve pending promises in surviving bindings on a possible path. Both
# stacks contain the same lexical frames in the same order.
scope_possible_losses <- function(scope, after) {
  if (identical(scope, after)) {
    return(scope)
  }
  Map(function(before, reached) {
    bound <- intersect(before$bound, reached$bound)
    frame <- retain_scope_bindings(before, bound)
    stable <- vapply(names(frame$kinds), function(name) {
      identical(frame$kinds[[name]], reached$kinds[[name]])
    }, logical(1))
    frame$kinds <- frame$kinds[stable]
    stable_closures <- vapply(names(frame$closures), function(name) {
      identical(frame$closures[[name]], reached$closures[[name]])
    }, logical(1))
    frame$closures <- frame$closures[stable_closures]
    stable_lists <- vapply(names(frame$closure_lists), function(name) {
      identical(frame$closure_lists[[name]], reached$closure_lists[[name]])
    }, logical(1))
    frame$closure_lists <- frame$closure_lists[stable_lists]
    for (name in names(frame$closure_environment_ids)) {
      fields <- scope_possible_fields(
        frame$closure_environments[[name]], reached$closure_environments[[name]]
      )
      first <- frame$closure_environment_ids[[name]]
      second <- reached$closure_environment_ids[[name]]
      identity <- if (!is.null(fields)) {
        if (identical(first, second)) {
          first
        } else {
          list(
            alias = new.env(parent = emptyenv()),
            possible = unique(c(scope_environment_identities(first),
                                scope_environment_identities(second)))
          )
        }
      }
      frame$closure_environments[[name]] <- fields
      frame$closure_environment_ids[[name]] <- identity
    }
    environment_names <- intersect(
      union(names(before$environments), names(reached$environments)), bound
    )
    possible_environments <- lapply(environment_names, function(name) {
      first <- before$environments[[name]]
      second <- reached$environments[[name]]
      if (identical(first, second)) return(first)
      list(offsets = union(first$offsets, second$offsets), definite = FALSE)
    })
    frame$environments <- stats::setNames(
      possible_environments, environment_names
    )
    frame
  }, scope, after)
}

# Apply a visible removal to its own frame or a statically visible caller.
# Unresolved targets retain the conservative current-frame treatment.
scope_after_removal_call <- function(expr, scope, lookup) {
  target <- base_call_argument(expr, c("envir", "pos"))
  index <- 1L
  if (length(target) > 0L) {
    envir <- unparenthesized_value(target[[1L]])
    bound <- scope_bound_names(scope)
    depth <- static_parent_frame_depth(envir, scope, lookup)
    if (!is.null(depth)) {
      index <- scope_caller_frame(depth, scope)
      if (is.null(index)) {
        return(scope)
      }
    } else if (!is.null(lookup_environment_value(envir, bound, lookup))) {
      return(scope)
    }
  }
  args <- static_call_args(expr)
  removed <- literal_removal_names(
    args[!argument_names(args) %in% c("envir", "pos", "inherits")],
    is_concat = function(part) {
      identical(static_call_name(part), "c") &&
        is_known_base_frame_call(part, "c", scope, lookup)
    }
  )
  inheritance <- base_call_argument(expr, "inherits")
  inherits <- if (length(inheritance) > 0L) {
    unparenthesized_value(inheritance[[1L]])
  }
  # Base removal coerces the first element of a literal inheritance flag.
  targets <- if (is.atomic(inherits) && !is.object(inherits) &&
                   length(inherits) > 0L &&
                   isTRUE(as.logical(inherits[[1L]]))) {
    scope_visible_frames(scope, index)
  } else {
    index
  }
  for (index in targets) {
    bound <- scope[[index]]$bound
    retained <- if (is.null(removed)) character() else setdiff(bound, removed)
    scope[[index]] <- retain_scope_bindings(scope[[index]], retained)
    if (!is.null(removed)) {
      removed <- setdiff(removed, bound)
    }
  }
  scope
}

# Literal depth of a recognized base caller-frame expression, or NULL
# when the target is unknown. No frame expression is evaluated.
static_parent_frame_depth <- function(expr, scope, lookup) {
  name <- static_call_name(expr)
  if (identical(name, "parent.frame") &&
        is_known_base_frame_call(expr, name, scope, lookup)) {
    return(static_frame_depth_argument(expr))
  }
  if (identical(name, "sys.frame") && length(expr) == 2L &&
        is_known_base_frame_call(expr, name, scope, lookup)) {
    args <- static_call_args(expr)
    # An empty `which` uses the global frame, as does `sys.frame()`.
    if (rlang::is_missing(args[[1L]])) {
      return(NULL)
    }
    parent <- unparenthesized_value(args[[1L]])
    if (argument_names(args)[[1L]] %in% c("", "which") &&
          identical(static_call_name(parent), "sys.parent") &&
          is_known_base_frame_call(parent, "sys.parent", scope, lookup)) {
      return(static_frame_depth_argument(parent))
    }
  }
  NULL
}

# Resolve a known caller depth through active calls, skipping retained lexical
# frames of a factory that has already returned.
scope_caller_frame <- function(depth, scope) {
  index <- 1L
  while (depth > 0L && index <= length(scope)) {
    frame <- scope[[index]]
    if (!isTRUE(frame$invoked)) {
      return(NULL)
    }
    index <- index + (frame$caller %||% 1L)
    depth <- depth - 1L
  }
  if (depth == 0L && index <= length(scope)) index else NULL
}

# Positive literal depth for a recognized base `parent.frame()` or
# `sys.parent()` call, including the default when `n` is omitted.
static_frame_depth_argument <- function(expr) {
  args <- static_call_args(expr)
  if (length(args) == 0L) {
    return(1L)
  }
  if (length(args) == 1L && argument_names(args)[[1L]] %in% c("", "n")) {
    if (rlang::is_missing(args[[1L]])) {
      return(1L)
    }
    depth <- unparenthesized_value(args[[1L]])
    # Base uses the integer part of a numeric depth.
    if (!is.object(depth) && is.numeric(depth) && length(depth) == 1L &&
          is.finite(depth) && depth >= 1 && depth < .Machine$integer.max + 1) {
      as.integer(depth)
    }
  }
}

# Check a base callee against lexical frames before ordinary caller lookup.
# Bare call heads skip known non-functions in those frames.
is_known_base_frame_call <- function(expr, name, scope, lookup) {
  is_known_package_frame_call(expr, name, "base", scope, lookup)
}

# Resolve a standard package function through the caller's lexical frames.
is_known_package_frame_call <- function(expr, name, owner, scope, lookup) {
  if (!is.null(static_call_ns(expr))) {
    return(is_known_namespace_frame_call(expr, owner, scope, lookup))
  }
  function_lookup <- is_name_part(static_call_head(expr))
  shadowed <- any(vapply(scope[scope_visible_frames(scope)], function(frame) {
    name %in% frame$bound &&
      (!function_lookup || !identical(frame$kinds[[name]], "value"))
  }, logical(1)))
  is_known_package_call(expr, name, name[shadowed], lookup, owner)
}

# A qualified callee is known only when its operator resolves to base.
# Namespace operators use function lookup, including lexical non-function skips.
is_known_namespace_frame_call <- function(expr, namespace, scope, lookup) {
  if (!identical(static_call_ns(expr), namespace)) {
    return(FALSE)
  }
  operator <- selection_namespace_operator(expr)
  lookup$rebound <- lookup$function_rebound %||% lookup$rebound
  is_known_base_frame_call(rlang::call2(operator), operator, scope, lookup)
}

# Discard pending defaults whose bindings have been removed.
retain_scope_bindings <- function(frame, bound) {
  frame$bound <- bound
  frame$defaults <- frame$defaults[names(frame$defaults) %in% bound]
  frame$arguments <- frame$arguments[names(frame$arguments) %in% bound]
  if (!"..." %in% bound) {
    frame$dots <- frame$dot_closures <- NULL
  }
  frame$kinds <- frame$kinds[names(frame$kinds) %in% bound]
  frame$closures <- frame$closures[names(frame$closures) %in% bound]
  frame$closure_lists <- frame$closure_lists[
    names(frame$closure_lists) %in% bound
  ]
  frame$closure_environments <- frame$closure_environments[
    names(frame$closure_environments) %in% bound
  ]
  frame$closure_environment_ids <- frame$closure_environment_ids[
    names(frame$closure_environment_ids) %in% bound
  ]
  frame$environments <- frame$environments[names(frame$environments) %in% bound]
  frame
}

# Translated dtplyr syntax has no frame information for named arguments.
removal_retained_bound <- function(expr, bound) {
  args <- static_call_args(expr)
  if (any(nzchar(argument_names(args)))) {
    return(character())
  }
  removed <- literal_removal_names(args)
  if (is.null(removed)) character() else setdiff(bound, removed)
}

# Visible parent replacements: target identities and mask-excluding parents.
# An unreadable target is TRUE; no caller expression is run.
lookup_reparented_environments <- function(
  expr, env, mask, rebound = lookup_rebound_names(expr, env, mask)) {
  if (rlang::is_quosure(expr)) {
    env <- rlang::quo_get_env(expr)
  }
  if (!rlang::is_call(expr)) {
    return(list())
  }
  if (is_parenthesized(expr)) {
    return(lookup_reparented_environments(
      unparenthesized_value(expr), env, mask, rebound
    ))
  }
  name <- static_call_name(expr)
  if (identical(name, "function") && length(expr) >= 3L) {
    rebound <- unique(c(rebound, names(as.list(expr[[2L]]))))
  }
  targets <- list()
  target <- parent <- list()
  mutation <- scope_mutation_call(expr, character(), list(
    env = env, mask = mask, rebound = rebound
  ))
  if (!is.null(name) && name %in% c("<-", "=", "<<-") &&
        length(expr) >= 3L &&
        identical(static_call_name(expr[[2L]]), "parent.env")) {
    target <- call_formal_argument(expr[[2L]], "env")
    parent <- as.list(expr)[3L]
  } else if (!is.null(mutation) &&
               identical(static_call_name(mutation), "parent.env<-")) {
    target <- base_call_argument(mutation, "env")
    parent <- base_call_argument(mutation, "value")
  }
  if (length(target) > 0L && length(parent) > 0L) {
    parent <- unparenthesized_value(parent[[1L]])
    parent_name <- static_call_name(parent)
    safe_parent <- !is.null(parent_name) &&
      parent_name %in% c("baseenv", "emptyenv") &&
      is_known_package_call(parent, parent_name, character(), list(
        env = env, rebound = rebound
      ))
    value <- lookup_environment_value(target[[1L]], character(), list(
      env = env, mask = mask, rebound = rebound
    ))
    targets <- list(list(
      value = if (is.null(value)) TRUE else value, safe_parent = safe_parent
    ))
  }
  c(targets, unlist(lapply(
    searched_call_parts(expr),
    lookup_reparented_environments, env = env, mask = mask, rebound = rebound
  ), recursive = FALSE))
}

# Whether an explicit environment path reaches the mask's own bindings.
is_static_mask_parent <- function(expr, scope, lookup) {
  (length(scope) + 1L) %in% static_environment_frames(expr, scope, lookup)
}

# A definite target is required for writes that change a single known frame.
static_environment_frame <- function(expr, scope, lookup) {
  frames <- static_environment_frames(expr, scope, lookup, definite = TRUE)
  if (length(frames) == 1L) frames else NULL
}

# Resolve visible environment values to lexical frame indices.
# The index after the summary frame denotes dplyr's column mask. Possible
# targets survive conditional writes; no caller expression is run.
static_environment_frames <- function(expr, scope, lookup, definite = FALSE) {
  expr <- unparenthesized_value(expr)
  if (is_name_part(expr)) {
    name <- rlang::as_name(expr)
    for (index in scope_visible_frames(scope)) {
      frame <- scope[[index]]
      if (name %in% frame$bound) {
        environment <- frame$environments[[name]]
        return(if (!is.null(environment) &&
                     (!definite || isTRUE(environment$definite))) {
          index + environment$offsets
        } else {
          integer()
        })
      }
    }
    return(integer())
  }
  name <- static_call_name(expr)
  arm <- scope_literal_branch(expr, scope, lookup)
  if (length(arm) == 1L && !rlang::is_missing(arm[[1L]])) {
    return(static_environment_frames(arm[[1L]], scope, lookup, definite))
  }
  if (identical(name, "identity") &&
        is_known_base_frame_call(expr, name, scope, lookup)) {
    value <- base_call_argument(expr, "x")
    if (length(value) == 1L && !rlang::is_missing(value[[1L]])) {
      return(static_environment_frames(value[[1L]], scope, lookup, definite))
    }
  }
  if (identical(name, "{") &&
        is_known_base_frame_call(expr, name, scope, lookup)) {
    statements <- static_call_args(expr)
    if (length(statements) == 0L) {
      return(integer())
    }
    for (index in seq_len(length(statements) - 1L)) {
      scope <- statement_reads_and_scope(
        statements[[index]], scope, lookup, read_symbols = no_scope_reads
      )$scope
    }
    return(static_environment_frames(
      statements[[length(statements)]], scope, lookup, definite
    ))
  }
  if (length(static_call_args(expr)) == 0L &&
        ((identical(name, "environment") &&
            is_known_base_frame_call(expr, name, scope, lookup)) ||
           (identical(name, "current_env") &&
              is_known_namespace_frame_call(expr, "rlang", scope, lookup)))) {
    return(1L)
  }
  depth <- static_parent_frame_depth(expr, scope, lookup)
  if (!is.null(depth)) {
    return(scope_caller_frame(depth, scope))
  }
  if (identical(name, "as.environment") && length(expr) == 2L &&
        is_known_base_frame_call(expr, name, scope, lookup)) {
    return(static_environment_frames(expr[[2L]], scope, lookup, definite))
  }
  base_parent <- identical(name, "parent.env") && length(expr) == 2L &&
    is_known_base_frame_call(expr, name, scope, lookup)
  rlang_parent <- identical(name, "env_parent") &&
    is_known_namespace_frame_call(expr, "rlang", scope, lookup)
  if (!base_parent && !rlang_parent) {
    return(integer())
  }
  args <- if (base_parent) {
    static_matched_arguments(expr, base::parent.env)
  } else {
    static_matched_arguments(expr, rlang::env_parent)
  }
  frames <- static_environment_frames(args$env, scope, lookup, definite)
  steps <- if ("n" %in% names(args)) unparenthesized_value(args$n) else 1L
  if (length(frames) == 0L || is.object(steps) || !is.numeric(steps) ||
        length(steps) != 1L || !is.finite(steps) || steps < 0 ||
        steps > length(scope) || steps != as.integer(steps)) {
    return(integer())
  }
  unique(unlist(lapply(frames, function(frame) {
    remaining <- steps
    while (remaining > 0L && frame <= length(scope)) {
      frame <- frame + (scope[[frame]]$enclosing %||% 1L)
      remaining <- remaining - 1L
    }
    if (remaining == 0L) frame else integer()
  }), use.names = FALSE))
}

# Resolve an existing zero-argument caller closure whose entire body is a
# known base caller-frame read. Inspecting that body never invokes the closure.
static_returned_caller_frame <- function(expr, scope, lookup) {
  if (!rlang::is_call(expr)) {
    return(NULL)
  }
  head <- static_call_head(expr)
  if (!is_name_part(head) || length(static_call_args(expr)) != 0L ||
        is.null(lookup$env)) {
    return(NULL)
  }
  name <- rlang::as_name(head)
  rebound <- lookup$function_rebound %||% lookup$rebound
  if (name %in% c(scope_bound_names(scope), lookup$mask, rebound) ||
        unresolved_lookup_name() %in% rebound ||
        !lookup_environment_is_external(lookup$env, character(), lookup,
                                        safe_parents = FALSE)) {
    return(NULL)
  }
  callee <- ordinary_function_binding(name, lookup$env)
  if (!is.function(callee) || is.primitive(callee) ||
        length(formals(callee)) != 0L) {
    return(NULL)
  }
  if (!lookup_environment_is_external(
    environment(callee), character(), lookup, safe_parents = FALSE
  )) {
    return(NULL)
  }
  lookup$env <- environment(callee)
  depth <- static_parent_frame_depth(body(callee), list(), lookup)
  if (!is.null(depth)) scope_caller_frame(depth - 1L, scope) else NULL
}

# Converting a known mask to a list retains an unresolved dependency.
# The conversion does not inherit bindings from a lexical frame.
materialized_env_symbols <- function(expr, scope, lookup) {
  args <- static_matched_arguments(expr, base::as.list)
  if (is_static_mask_parent(args$x, scope, lookup)) {
    return(unresolved_lookup_name())
  }
  character()
}

# A lookup on the mask reads its own bindings regardless of inheritance.
# A possibly inheriting lookup follows visible lexical bindings unless a known
# stop frame ends the search before the mask.
# A nonliteral key retains an unresolved dependency on a mask-reaching path.
inheriting_env_get_symbols <- function(expr, scope, lookup, bulk = FALSE) {
  definition <- if (bulk) rlang::env_get_list else rlang::env_get
  args <- static_matched_arguments(expr, definition)
  frames <- static_environment_frames(args$env, scope, lookup)
  if (length(frames) == 0L) {
    return(character())
  }
  name <- if (bulk) {
    static_character_value(args$nms, function(expr) {
      identical(static_call_name(expr), "c") &&
        is_known_base_frame_call(expr, "c", scope, lookup)
    })
  } else {
    unparenthesized_value(args$nm)
  }
  if (is.null(name) || (!bulk && !rlang::is_string(name))) {
    name <- unresolved_lookup_name()
  }
  unique(unlist(lapply(frames, function(frame) {
    if (frame > length(scope)) {
      return(name)
    }
    if (!"inherit" %in% names(args) ||
          identical(unparenthesized_value(args$inherit), FALSE)) {
      return(character())
    }
    if ("last" %in% names(args)) {
      last <- static_environment_frame(args$last, scope, lookup)
      if (is.null(last)) {
        last <- static_returned_caller_frame(args$last, scope, lookup)
      }
      if (!is.null(last) && last %in% scope_visible_frames(scope, frame)) {
        return(character())
      }
    }
    unique(unlist(lapply(name, function(name) {
      scoped_data_symbols(
        rlang::sym(name), scope[seq.int(frame, length(scope))], lookup
      )
    }), use.names = FALSE))
  }), use.names = FALSE))
}

# The names a reflective lookup resolves in the mask.
#
# An omitted environment searches from the expression's local bindings. An
# explicit environment may bypass them, so it cannot use that exemption.
#
# A name that is not statically knowable is the undecidable shape #130's
# contract resolves toward over-reporting, and the marker is how this walk says
# so. Evaluating the argument to find out what it holds is the one thing this
# analysis may not do -- it runs while the call is planned, on the caller's own
# code.
reflective_lookup_symbols <- function(expr, bound, lookup = NULL) {
  environment <- base_call_argument(
    expr, c("envir", "frame", "pos", "where")
  )
  inherited <- base_call_argument(expr, "inherits")
  inherits <- if (length(inherited) > 0L) {
    !identical(unparenthesized_value(inherited[[1L]]), FALSE)
  } else {
    !identical(resolved_callee_name(expr), "mget")
  }
  if (length(environment) > 0L && lookup_environment_is_external(
    environment[[1L]], bound, lookup, inherits
  )) {
    return(character())
  }
  argument <- call_formal_argument(expr, "x")
  if (length(argument) == 0L) {
    # Nothing to look up, and nothing wrong with the walk: the call raises R's
    # own condition for the missing argument when it runs.
    return(character())
  }
  looked_up <- static_character_value(argument[[1L]])
  if (is.null(looked_up)) {
    return(unresolved_lookup_name())
  }
  if (length(environment) > 0L) looked_up else setdiff(looked_up, bound)
}

# What a `do.call()` reads beyond its own parts. It invokes the function it
# names in an environment that defaults to the caller's, which under a data
# mask is the mask, so `do.call("get", list("share"))` performs the lookup
# `get("share")` performs. What that lookup is handed sits in a list built at
# run time, which this walk cannot read, so a `do.call()` of a reflective
# primitive is reported as a lookup it cannot resolve.
#
# A known closure invocation follows the ordinary call walk.
deferred_call_symbols <- function(expr) {
  argument <- call_formal_argument(expr, "what")
  if (length(argument) == 0L) {
    return(character())
  }
  named <- static_callee_name(argument[[1L]])
  if (is_reflective_lookup(named) || is_reflective_evaluation(named)) {
    return(unresolved_lookup_name())
  }
  character()
}

# The names the language object handed to `eval()` reads.
#
# An `envir` argument is deliberately not an exemption here, unlike at the
# lookup primitives above. `eval()`'s `enclos` defaults to `parent.frame()`,
# which under a data mask is the mask, so a supplied `envir` that is a list or
# a data frame leaves the mask on the lookup path -- `eval(as.name("share"),
# list(a = 1))` reads the share. Which of the two an argument evaluates to is
# not decidable here, so the node resolves toward over-reporting.
evaluated_language_symbols <- function(expr, scope, lookup) {
  values <- evaluated_language_parts(expr)
  if (is.null(values) || isTRUE(attr(values, "unresolved"))) {
    return(unresolved_lookup_name())
  }
  unique(unlist(
    lapply(values, scoped_data_symbols, scope = scope, lookup = lookup),
    use.names = FALSE
  ))
}
