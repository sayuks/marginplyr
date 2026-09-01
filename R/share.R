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
#' Grouping-set row, so it can be used only inside
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
#' redefine the source name, or use a share as another share's source. Each
#' rejected form and its rewrite are listed below.
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
#'   `grouping_spec()` do not define one Parent chain. For a Parent share,
#'   replace them with one pure `rollup()` or omit the request. For a Total
#'   share only a plan without a Grand total set is rejected; add an empty
#'   `grouping_set()` to the `grouping_sets()` specification.
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
#' - **Named `across()` source:** `total = across(c(revenue, units), sum)`
#'   packs both results into one data-frame-valued `total` column, so `total`
#'   is not a scalar source. Drop the `total =` name to get one column per
#'   selected column, or define each summary at top level.
#' - **Summary-alias dependency:** `gross = sum(value)`,
#'   `net = gross - sum(discount)` is rejected when `net` is a source. Use
#'   `net = sum(value) - sum(discount)`.
#' - **Wrapped share:** `percent = 100 * share_of_parent(total)` is
#'   rejected. Create `share = share_of_parent(total)`, then use
#'   `dplyr::mutate(percent = 100 * share)` on the result.
#' - **Share dependency:** a share cannot source another share of either kind,
#'   or an ordinary summary later in the same call. Create all requested
#'   shares from ordinary summaries, then derive further columns in
#'   `dplyr::mutate()`.
#' - **Non-numeric or non-scalar source:** semantic classes, zero-length
#'   results, and `quantile(value, c(0.25, 0.75))` are rejected. Convert only
#'   when meaningful and create one scalar summary per output, such as
#'   `q25 = quantile(value, 0.25)` and `q25_share = share_of_parent(q25)`.
#' - **Ineligible `across()` selection:** source columns, grouping keys, and
#'   previous shares are rejected. Select only preceding ordinary
#'   summaries, for example `across(c(total, count), share_of_parent, ...)`.
#' - **Predicate selection:** `where(is.numeric)` is rejected. Use explicit
#'   names, `all_of()`, `any_of()`, or another name-based selector.
#' - **Indirect `.fns`:** `~share_of_parent(.x)`,
#'   `\(x) share_of_total(x)`, and `list(share_of_parent)` are rejected. Pass
#'   the bare helper, or `marginplyr::share_of_parent`, directly.
#' - **Aggregate and share in one function list:**
#'   `across(value, list(total = sum, share = share_of_parent))` is rejected.
#'   Use one `across(value, sum)` followed by a second
#'   `across(value, share_of_parent, .names = "{.col}_share")`.
#' - **Additional function arguments:** passing `na.rm = TRUE` to a share's
#'   `across()` is rejected. Handle it in the preceding
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
#' - **Share collision:** reusing a direct or generated share name is
#'   rejected. Give each share one unique output name.
#'
#' @section Value rules:
#' Both helpers divide within each fixed `.by` partition, and neither ever
#' synthesizes or completes keys. A missing numerator, missing denominator, or
#' zero denominator gives `NA_real_`; local `NaN` is missing. Other finite
#' ratios are unclamped doubles, so negative values and values above one are
#' retained. Denominator matching uses internal Grouping set metadata rather
#' than `.id` or displayed Margin labels, and missing fixed or variable keys
#' are matched with missing-safe identity rather than ordinary SQL
#' `NULL = NULL`.
#'
#' A Parent share's denominator is the immediate strictly less detailed
#' [rollup()] level. Composite dimensions are added or removed together. The
#' Grand total set has no parent, so its Parent share is `1.0`, even when its
#' source is zero or missing. Duplicate Grouping set occurrences remain in the
#' result but are skipped while finding the next coarser parent, because a
#' parent must be strictly less detailed.
#'
#' A Total share's denominator is the Grand total set. Every row of it,
#' duplicate occurrences included, has a Total share of `1.0`, even when its
#' source is zero or missing. Duplicate Grand total occurrences aggregate the
#' same rows and therefore hold the same values, so they are interchangeable
#' rather than skipped and which one supplies the denominator is not
#' specified.
#'
#' Empty input gives one Grand total set row or no rows at all, and the share
#' column keeps its type either way:
#'
#' | Empty input | Rows | Share value and type |
#' |---|---:|---|
#' | Without fixed `.by` keys | One row of the Grand total set | `1.0`, double |
#' | With fixed `.by` keys | Zero rows | Empty double vector |
#'
#' Missing detail or subtotal combinations are not completed.
#'
#' @section Lazy execution boundaries:
#' Share execution supports local data frames and lazy dbplyr and dtplyr
#' inputs, including composite dimensions. Lazy results remain lazy: ordinary
#' summaries are followed by one denominator mapping and join per requested
#' kind, shared by every measure requested of that kind, however many measures
#' are requested.
#'
#' Syntax, source-name, written-order, and `across()` errors are always
#' reported locally, before execution, on every backend. The eligible-type
#' rule is also enforced on every backend, and no backend calculates a share
#' from a source it has shown to be ineligible. None of it reads a row of your
#' data. What differs is what establishes the rule, and whether the
#' exactly-one-value cardinality rule is established with it:
#'
#' | Backend | What establishes the source rules |
#' |---|---|
#' | Local data frame | Both, from the result, before any share |
#' | `dtplyr` step | Both, at execution, before an invalid row |
#' | Arrow | Neither; shares are rejected outright |
#' | General dbplyr | Type only, by asking the dialect itself |
#'
#' Arrow inputs reject both helpers after expression planning and common
#' Margin-operation validation but before constructing a summary query. The
#' reason belongs to the source summary they share, so it applies to both, and
#' the diagnostic names whichever helpers the call used. Other Arrow Margin
#' operations remain supported and lazy, apart from a summary Arrow's own
#' engine cannot evaluate, which is refused for its own reason and before any
#' row is read; see [summarize_with_margins()]. Explicitly collect an Arrow
#' input first when local share execution is appropriate.
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
#' Cardinality is not established this way at all: a SQL aggregate returns one
#' value per grouping row by construction, so there is nothing for a dialect
#' to convert. A non-scalar summary therefore remains a runtime-only
#' incompatibility, reported by the database at [dplyr::collect()] rather than
#' raised by marginplyr before the query is returned.
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
  if (is.null(dot_names)) {
    dot_names <- rep("", length(dots))
  }
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
        "grouping specifications do not define one unambiguous parent."
      ),
      i = paste0(
        "Rewrite {.arg .grouping} as one {.fun rollup} or omit the Parent ",
        "share."
      )
    ),
    call = call
  )
}

plan_share_expressions <- function(dots,
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
      preceding_shares$names
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
  list(
    dots = planned_dots,
    requests = requests,
    cardinality = cardinality,
    # Which incoming dot each planned dot came from, for the same reason
    # `flattened_positions` above exists: a share dot is dropped and a
    # placeholder expands into one dot per output, so a consumer holding one
    # value per dot the caller wrote cannot subscript it by position.
    origin_positions = rep(which(keep), widths)
  )
}

share_cardinality_records <- function(analyses, requests) {
  pairs <- share_pairs(requests)
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
      share_output = pair$output,
      source_summary = pair$source,
      share_kind = pair$kind,
      across_input = source_records[[1L]]$across_input,
      across_function = source_records[[1L]]$across_function
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
        share_private_call("check_share_across"),
        expr,
        share_outputs = share_outputs,
        share_kinds = share_kinds
      )
    } else {
      check <- checks[[1L]]
      is_dtplyr <- identical(backend_kind, "dtplyr")
      wrapped <- rlang::call2(
        share_private_call(if (is_dtplyr) {
          "check_dtplyr_share_source"
        } else {
          "check_share_scalar"
        }),
        expr,
        share_output = check$share_output,
        source_summary = check$source_summary,
        share_kind = check$share_kind,
        !!!if (is_dtplyr) list(call_text = share_call_text(call))
      )
    }
    dots[[position]] <- rlang::new_quosure(
      wrapped,
      env = rlang::quo_get_env(quo)
    )
  }
  dots
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
  # `parsed$fns` is `NULL` for a `.fns` the caller omitted, which is the
  # identity lambda `across()` applies in its place. It answers the same for a
  # `.fns` left empty, but no share reaches here with one:
  # `preflight_share_across_syntax()` requires the helper itself in that
  # position and refuses anything else, an empty argument included. Asking the
  # value rather than the index is still what this is written on, because the
  # index is a position and the position is a separate question -- the one the
  # write-back below asks, where an empty `.fns` would be replaced in place and
  # an absent one appended (#174).
  if (is.null(parsed$fns)) {
    functions <- list(rlang::expr(~.x))
    function_names <- ""
    fns_is_list <- FALSE
  } else {
    fns_is_list <- rlang::is_call(parsed$fns, "list")
    if (fns_is_list) {
      functions <- static_call_args(parsed$fns)
      function_names <- names(functions)
    } else {
      functions <- list(parsed$fns)
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
  if (fns_index == 0L) {
    call_args <- append(call_args, list(.fns = wrapped_fns))
  } else {
    call_args[[fns_index]] <- wrapped_fns
  }
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
    share_private_call("dtplyr_share_input_name"),
    dtplyr_lambda_pronoun()
  )
  mapping_expr <- rlang::call2(
    share_private_call("new_share_validation_mapping"),
    inputs = mapping$inputs,
    share_outputs = mapping$share_outputs,
    source_summaries = mapping$source_summaries,
    share_kinds = mapping$share_kinds
  )
  validator <- rlang::call2(
    share_private_call("check_dtplyr_share_scalar"),
    value,
    input = input,
    mapping = mapping_expr,
    call_text = share_call_text(call)
  )
  rlang::call2("~", validator)
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

share_private_call <- function(name) {
  rlang::call2(
    ":::",
    rlang::sym("marginplyr"),
    rlang::sym(name)
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
  typeof(value) %in% c("integer", "double") && !is.object(value)
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
    if (is.object(value)) class(value) else typeof(value),
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
    if (contains_share_helper(expr)) {
      analyses[[i]] <- list(records = list())
      next
    }

    if (nzchar(output_name)) {
      output_names <- output_name
      eligibility <- if (is_across_call(expr)) "named_across" else "eligible"
    } else if (is_across_call(expr)) {
      output_names <- known_across_output_names(
        expr,
        env,
        selection_proxy
      )
      eligibility <- "eligible"
    } else {
      output_names <- known_data_frame_output_names(
        expr,
        env,
        selection_proxy
      )
      eligibility <- "expanded"
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
    provenance <- across_output_provenance(
      expr,
      env,
      selection_proxy,
      output_names,
      expands_own_names = is_across_call(expr) && !nzchar(output_name)
    )
    across_inputs <- provenance$inputs
    across_functions <- provenance$functions
    records <- Map(
      function(name, across_input, across_function) {
        list(
          name = name,
          position = i,
          eligibility = eligibility,
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
    parse_across_arguments(expr)
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
  if (contains_selection_predicate(args$cols)) {
    abort_share_predicate(kind)
  }
  sources <- resolve_share_selection(
    args$cols,
    env = env,
    preceding_names = preceding_names,
    preceding = preceding,
    context = context,
    kind = kind,
    error_call = expr
  )
  outputs <- vapply(
    sources,
    function(source) {
      expand_across_name(names_template, source, "1", env)
    },
    character(1)
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
  if (!is.null(args$unpack)) {
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
  if (any(!nzchar(outputs))) {
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
    if (
      !is.na(context$ordinary_counts[[source]]) &&
        context$ordinary_counts[[source]] != 1L
    ) {
      abort_marginplyr(c(
        paste0(
          "{label} {.var {output}} requires source summary {.var {source}} ",
          "to be defined exactly once."
        ),
        i = "Use one uniquely named ordinary summary."
      ))
    }
    record <- preceding[[max(which(preceding_names == source))]]
    if (!identical(record$eligibility, "eligible")) {
      abort_ineligible_share_source(
        label = label,
        output = output,
        source = source,
        eligibility = record$eligibility
      )
    }
    if (length(record$dependencies) > 0L) {
      abort_marginplyr(c(
        paste0(
          "{label} {.var {output}} cannot use source summary {.var {source}} ",
          "because it depends on earlier summary alias ",
          "{.var {record$dependencies[[1L]]}}."
        ),
        i = "Combine the calculation into one ordinary summary expression."
      ))
    }
  }

  conflicts <- intersect(
    outputs,
    unique(c(
      context$conflicting_names,
      all_names,
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

execute_shares <- function(operation,
                           staged_result,
                           requests,
                           check_share_source) {
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
  # The eligible-type rule is a property of the source summary, not of the
  # join that follows, so it is settled once here rather than inside the
  # adapter that happens to run. Only where its answer comes from is a backend
  # question, and that is what the checker below decides.
  check_share_sources(
    operation,
    result = result,
    requests = requests,
    check_share_source = check_share_source
  )
  adapter <- share_adapter(operation$backend$kind)
  # One adapter pass per requested kind. Every pass reads the same staged
  # result and writes over the placeholder column each request reserved in the
  # caller's written order, so the passes are independent and their order is
  # not visible in the result.
  for (kind in share_request_kinds(requests)) {
    result <- adapter(
      operation,
      result = result,
      requests = Filter(
        function(request) identical(request$kind, kind),
        requests
      ),
      set_id_name = staged_set_id_name,
      kind = kind
    )
  }
  if (!is.null(operation$set_id_name)) {
    result <- dplyr::mutate(
      result,
      "{operation$set_id_name}" := .data[[staged_set_id_name]]
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
  dplyr::select(
    result,
    -dplyr::all_of(staged_set_id_name)
  )
}

share_adapter <- function(backend_kind) {
  adapters <- list(
    local = execute_row_matched_shares,
    duckdb = execute_dbplyr_shares,
    postgres = execute_dbplyr_shares,
    sql = execute_dbplyr_shares,
    dtplyr = execute_row_matched_shares,
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
                                       kind) {
  apply_joined_shares(
    result,
    requests = requests,
    plan = operation$plan,
    set_id_name = set_id_name,
    kind = kind,
    sql_join = FALSE
  )
}

execute_dbplyr_shares <- function(operation,
                                  result,
                                  requests,
                                  set_id_name,
                                  kind) {
  apply_joined_shares(
    result,
    requests = requests,
    plan = operation$plan,
    set_id_name = set_id_name,
    kind = kind,
    sql_join = TRUE
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
probe_share_dialect <- function(con) {
  probe <- probe_share_dialect_answer(con, quote(sum("x", na.rm = TRUE)))
  if (identical(probe, "raised")) {
    control <- probe_share_dialect_answer(con, quote(sum(z, na.rm = TRUE)))
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
probe_share_dialect_answer <- function(con, expr) {
  query <- tryCatch(
    dplyr::summarize(
      dplyr::tbl(con, dbplyr::sql("SELECT 1 AS z"), vars = "z"),
      p = !!expr
    ),
    error = function(cnd) NULL
  )
  if (is.null(query)) {
    return("unanswerable")
  }
  answer <- tryCatch(
    list(value = suppressMessages(suppressWarnings(dplyr::collect(query)))),
    error = function(cnd) NULL
  )
  if (is.null(answer)) {
    return("raised")
  }
  value <- answer$value
  if (
    !is.data.frame(value) ||
      nrow(value) != 1L ||
      ncol(value) != 1L ||
      !is_share_source_type(value[[1L]])
  ) {
    return("unanswerable")
  }
  "answered"
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
share_denominator_names <- function(sources, used_names) {
  denominator_names <- character()
  for (source in sources) {
    denominator_names <- c(
      denominator_names,
      new_margin_internal_names(
        1L,
        used_names = c(used_names, denominator_names),
        prefix = paste0("..marginplyr_denominator_of_", source, "_")
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
                                sql_join) {
  rule <- share_kind_rule(kind)
  target_ids <- rule$target_ids(plan)
  own_denominator_ids <- plan$set_ids[is.na(target_ids)]
  pairs <- share_pairs(requests)
  sources <- share_source_names(requests)
  result_names <- get_col_names(result, dplyr::everything())
  denominator_names <- share_denominator_names(
    sources,
    used_names = result_names
  )

  # The cleanup at the end of this function drops whatever internal columns
  # were added, so empty is what it needs when every occurrence is its own
  # denominator and no join happens — and for `right_join_names`, also on the
  # non-SQL path, which joins by name and renames nothing.
  key_names <- character()
  right_join_names <- character()

  joined_ids <- plan$set_ids[!is.na(target_ids)]
  if (length(joined_ids) > 0L) {
    denominator <- rule$build_denominator(
      result,
      plan = plan,
      target_ids = target_ids,
      sources = sources,
      denominator_names = denominator_names,
      set_id_name = set_id_name,
      used_names = c(result_names, denominator_names)
    )
    result <- denominator$result
    mapping <- denominator$mapping
    join_names <- denominator$join_names
    key_names <- denominator$key_names
    if (sql_join) {
      right_join_names <- new_margin_internal_names(
        length(join_names),
        used_names = c(
          result_names,
          denominator_names,
          key_names
        ),
        prefix = "..marginplyr_share_match_"
      )
      rename_pairs <- rlang::set_names(
        rlang::syms(join_names),
        right_join_names
      )
      mapping <- dplyr::rename(mapping, !!!rename_pairs)
      result <- dplyr::left_join(
        result,
        mapping,
        sql_on = lazy_share_sql_on(
          con = dbplyr::remote_con(result),
          left_names = join_names,
          right_names = right_join_names
        ),
        x_as = "LHS",
        y_as = "RHS"
      )
    } else {
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
      if (length(joined_ids) == 0L) {
        return(rlang::expr(1.0))
      }
      rlang::expr(
        dplyr::if_else(
          (!!margin_column_pronoun(set_id_name)) %in% !!own_denominator_ids,
          1.0,
          dplyr::if_else(
            is.na(!!margin_column_pronoun(source)) |
              is.na(!!margin_column_pronoun(denominator)) |
              (!!margin_column_pronoun(denominator)) == 0,
            NA_real_,
            as.double(!!margin_column_pronoun(source)) /
              as.double(!!margin_column_pronoun(denominator))
          )
        )
      )
    }
  )
  names(share_exprs) <- vapply(pairs, `[[`, character(1), "output")
  result <- dplyr::mutate(result, !!!share_exprs)

  internal_names <- c(
    unname(denominator_names),
    right_join_names,
    key_names
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
                                     used_names) {
  mapping <- build_lazy_parent_mapping(
    result,
    child_ids = plan$set_ids[!is.na(target_ids)],
    parent_ids = target_ids,
    sources = sources,
    denominator_names = denominator_names,
    plan = plan,
    set_id_name = set_id_name
  )
  join_key_names <- new_margin_internal_names(
    length(plan$dimensions),
    used_names = used_names,
    prefix = "..marginplyr_parent_key_"
  )
  names(join_key_names) <- plan$dimensions
  result <- add_lazy_parent_join_keys(
    result,
    plan = plan,
    parent_ids = target_ids,
    set_id_name = set_id_name,
    join_key_names = join_key_names
  )
  mapping <- add_lazy_parent_join_keys(
    mapping,
    plan = plan,
    parent_ids = target_ids,
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
  list(
    result = result,
    mapping = mapping,
    join_names = c(set_id_name, plan$by, unname(join_key_names)),
    key_names = unname(join_key_names)
  )
}

# A Total share's denominator depends on `.by` and nothing else, so its
# mapping is the Grand total rows reduced to one row per fixed partition and
# matched on the fixed keys alone. See ADR 0017.
build_total_denominator <- function(result,
                                    plan,
                                    target_ids,
                                    sources,
                                    denominator_names,
                                    set_id_name,
                                    used_names) {
  denominator_id <- unique(target_ids[!is.na(target_ids)])
  stopifnot(length(denominator_id) == 1L)
  key_exprs <- lapply(plan$by, margin_column_pronoun)
  names(key_exprs) <- plan$by
  denominator_exprs <- lapply(sources, margin_column_pronoun)
  names(denominator_exprs) <- unname(denominator_names[sources])
  mapping <- dplyr::transmute(
    dplyr::filter(
      result,
      .data[[set_id_name]] == !!denominator_id
    ),
    !!!key_exprs,
    !!!denominator_exprs
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
  # on. A constant column on both sides keeps that case on the same join as
  # every other, including the missing-safe SQL one, rather than adding a
  # second join shape that only this case would exercise.
  partition_name <- new_margin_internal_names(
    1L,
    used_names = used_names,
    prefix = "..marginplyr_total_key_"
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

lazy_share_sql_on <- function(con, left_names, right_names) {
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
                                      set_id_name) {
  group_vars <- unique(c(plan$by, plan$dimensions))
  key_exprs <- lapply(group_vars, margin_column_pronoun)
  names(key_exprs) <- group_vars
  denominator_exprs <- lapply(
    sources,
    function(source) margin_column_pronoun(source)
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
  combine_margin_branches(mappings)
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
          (!!margin_column_pronoun(set_id_name)) %in% !!matching_child_ids,
          !!margin_column_pronoun(dimension),
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

# The Grand total occurrences of a plan: those omitting every variable
# grouping dimension. There is at most one unless duplicates were kept. Named
# for the occurrences it returns, because `total_set_ids()` below returns the
# other shape — one denominator per occurrence, in plan order.
grand_total_occurrence_ids <- function(plan) {
  variable_sets <- lapply(plan$sets, setdiff, y = plan$by)
  plan$set_ids[lengths(variable_sets) == 0L]
}

# The denominator occurrence of every grouping set, in the shape
# `parent_set_ids()` returns: `NA` where the row is its own denominator, and
# otherwise the occurrence supplying it. Duplicate Grand total occurrences
# hold the same values, so any of them answers for every other set; which one
# is used is not part of the contract.
total_set_ids <- function(plan) {
  result <- rep(NA_integer_, length(plan$sets))
  grand_total_ids <- grand_total_occurrence_ids(plan)
  if (length(grand_total_ids) == 0L) {
    return(result)
  }
  result[!plan$set_ids %in% grand_total_ids] <- grand_total_ids[[1L]]
  result
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
# compiled Grouping plan, and the denominator mapping it joins. Detection reads
# the table backwards, from a written name to its kind. A third helper is one
# entry here and nothing else in this module.
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
      error_call = error_call
    )),
    error = function(cnd) {
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
  occurrences <- sum(all_names == source)
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

abort_share_predicate <- function(kind) {
  abort_marginplyr(c(
    paste0(
      "{share_kind_modifier(kind)} {.fun across} only supports name-based ",
      "tidyselect."
    ),
    i = paste0(
      "Replace {.fun where} or another type/value predicate with explicit ",
      "summary names."
    )
  ))
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
expression_alias_dependencies <- function(expr, aliases) {
  if (length(aliases) == 0L) {
    return(character())
  }
  symbols <- unique(expression_data_symbols(expr))
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
# bind to the statements after them. `local()` needs no case at all: it binds
# nothing either, and whichever of these its argument is answers it (#162).
expression_data_symbols <- function(expr, bound = character()) {
  # `is_name_part()` rather than `rlang::is_symbol()`, because the walk
  # descends into every part of a call and an empty argument is a symbol whose
  # name is `""`: the read of a column by that name was reported to every
  # caller of this list. An empty part is no call either, so it falls to the
  # line below and names nothing, which is the answer (#174).
  if (is_name_part(expr)) {
    name <- rlang::as_name(expr)
    if (name %in% bound) {
      return(character())
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
    return(expression_data_symbols(unparenthesized_value(expr), bound))
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
    return(definition_data_symbols(expr, bound))
  }
  # `statement_reads_and_bound()` is the one place that knows which nodes bind
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
    return(statement_reads_and_bound(expr, bound)$reads)
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
      call_part_symbols(expr, bound),
      reflective_lookup_symbols(expr, bound)
    )))
  }
  if (is_reflective_evaluation(callee_name)) {
    return(unique(c(
      call_part_symbols(expr, bound),
      evaluated_language_symbols(expr, bound)
    )))
  }
  if (identical(callee_name, "do.call")) {
    return(unique(c(
      call_part_symbols(expr, bound),
      deferred_call_symbols(expr)
    )))
  }
  if (
    !is.null(call_name) &&
      call_name %in% c("$", "[[") &&
      length(expr) >= 3L &&
      rlang::is_symbol(expr[[2L]])
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
    pronoun <- rlang::as_name(expr[[2L]])
    if (identical(pronoun, ".env")) {
      return(character())
    }
    if (identical(pronoun, ".data")) {
      # By subscript, because `.data[[, 1]]` puts an empty argument in the
      # index position: `column <- expr[[3L]]` binds R's missing marker there
      # and raises `missingArgError` on the first read of that name (#174). It
      # names no column, which is what an unreadable index already answers.
      if (is_name_part(expr[[3L]])) {
        return(rlang::as_name(expr[[3L]]))
      }
      if (
        is.character(expr[[3L]]) &&
          length(expr[[3L]]) == 1L &&
          !is.na(expr[[3L]])
      ) {
        return(expr[[3L]])
      }
      return(character())
    }
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
    return(expression_data_symbols(expr[[2L]], bound))
  }
  # Element 1 is the function position. A bare symbol there is dropped, and it
  # is the only head shape that can be: R resolves a symbol in that position
  # through function lookup, which skips non-function bindings, so a share
  # named `sum` cannot shadow `sum(x)`. Every other head is evaluated in the
  # data mask exactly like an argument, so a read hidden in one bypasses the
  # guard against an ordinary summary using an earlier share (#130).
  #
  # This names the one shape that is excluded rather than listing the shapes
  # that are walked. #100 could justify only `[[` at the time and listed it,
  # which left `(fns[[bucket]])(x)` -- a head that is a call to `(`, not to
  # `[[` -- slipping past, together with `$`, `if`/`else`, and computed heads.
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
  call_part_symbols(expr, bound)
}

# The reads of a call's own parts: its evaluated arguments, and its head when
# the head is not a bare symbol. This is the general walk above, reached by name
# so that the reflective branches can add their resolved names to it instead of
# answering in its place.
#
# An argument a call captures as language is not one of those parts, and this
# is where `quote(share)` stops being read as the column `share`: the boundary
# is drawn once here, so every branch above that ends at the parts -- the
# pronoun, the binding constructs, the reflective primitives -- inherits it
# rather than testing for a capture of its own (#179).
call_part_symbols <- function(expr, bound) {
  call_head <- static_call_head(expr)
  # The bound set travels into the capture reading, which is where a name this
  # expression has bound to a function of its own stops being read as the
  # primitive it spells. This walk is the only caller that has one to pass.
  parts <- evaluated_call_args(expr, bound = bound)
  if (!rlang::is_symbol(call_head)) {
    parts <- c(list(call_head), parts)
  }
  reads <- unlist(
    lapply(parts, expression_data_symbols, bound = bound),
    use.names = FALSE
  )
  # `unlist()` answers `NULL` for a walk that reached no part at all, and every
  # member of this family answers a character vector: `intersect()` and `%in%`
  # read the two alike, but `expect_identical()` does not, and a caller storing
  # the answer would find one branch of the walk typeless. A call whose parts
  # are all captured is the shape that made it reachable for a name-carrying
  # call rather than only for `f()` (#179).
  if (is.null(reads)) {
    return(character())
  }
  unique(reads)
}

# A function definition binds its formals, so its body reads the mask only
# through the names they do not cover. The formals' default values are mask
# reads too: a default is evaluated in the function's own frame, whose
# enclosure is the definition environment -- the mask -- so
# `(function(y = share) y)()` reads the share. The walk collected nothing from
# the formals pairlist at all, which is why that one was silent (#130).
#
# Defaults are scoped against every formal rather than the ones written before
# them. R evaluates them lazily in a frame that already holds all the formals,
# so `(function(a = b, b = 1) a)()` is 1, and `k` in `function(share, k =
# share)` names the formal rather than the column.
#
# The srcref at element 4 is deliberately not walked: it is not code the mask
# ever evaluates.
definition_data_symbols <- function(expr, bound) {
  formals_list <- as.list(expr[[2L]])
  inner <- unique(c(bound, names(formals_list)))
  defaults <- formals_list[
    !vapply(formals_list, rlang::is_missing, logical(1))
  ]
  unique(unlist(
    lapply(
      c(defaults, list(expr[[3L]])),
      expression_data_symbols,
      bound = inner
    ),
    use.names = FALSE
  ))
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

# A block's statements, walked in source order with the bound set growing as
# they bind, and the set the statement after the block would see. Only a
# statement that always runs grows it: an assignment nested inside anything
# conditional -- `if (p) tmp <- 1`, a loop body over a sequence that may be
# empty -- may not, so the name it would bind stays a read, which over-reports
# rather than missing the read of a column that really did reach the mask
# (#162).
#
# Named for both halves of what it answers rather than for the `_data_symbols`
# family, whose members all return a character vector of reads. A reader who
# took this for one of those would pass a list to `intersect()`.
block_reads_and_bound <- function(expr, bound) {
  reads <- character()
  # By subscript for the reason `static_call_args()` gives. A `{` block cannot
  # hold an empty statement -- the parser produces none -- so this is written
  # the way the rule asks rather than to repair anything reachable here.
  statements <- static_call_args(expr)
  for (index in seq_along(statements)) {
    step <- statement_reads_and_bound(statements[[index]], bound)
    reads <- c(reads, step$reads)
    bound <- step$bound
  }
  list(reads = unique(reads), bound = bound)
}

# One statement's reads together with the bound set the statement after it
# sees. A node that binds nothing returns the set it was given.
statement_reads_and_bound <- function(expr, bound) {
  # A nested block and a redundant parenthesis are transparent here for the
  # reason the enclosing block is: neither opens a scope, and both always run,
  # so `{ { tmp <- share }; tmp }` and `{ (tmp <- share); tmp }` bind `tmp` for
  # what follows exactly as the unwrapped statement does.
  #
  # The parenthesis comes off through the shared reading rather than through a
  # branch of its own, so that the name read below and the operands the branches
  # read beside it are read from one node (#178).
  if (is_parenthesized(expr)) {
    return(statement_reads_and_bound(unparenthesized_value(expr), bound))
  }
  call_name <- static_call_name(expr)
  if (identical(call_name, "{")) {
    return(block_reads_and_bound(expr, bound))
  }
  # `rm()` and its alias `remove()` are the only statements that take a name
  # out of scope again, and losing one from the set is the direction this walk
  # is not allowed to be wrong in: after `{ tmp <- 1; rm(tmp); tmp }` the last
  # read reaches the column once more, so a share named `tmp` is read there and
  # the guard owes the caller a diagnostic. Everything else the walk does not
  # recognize can only add bindings, which over-reports and is safe to ignore.
  if (!is.null(call_name) && call_name %in% c("rm", "remove")) {
    return(list(
      reads = expression_data_symbols(expr, bound),
      bound = removal_retained_bound(expr, bound)
    ))
  }
  if (!is_binding_statement(call_name, expr)) {
    return(list(reads = expression_data_symbols(expr, bound), bound = bound))
  }
  if (identical(call_name, "for")) {
    # The index survives the loop: R binds it in the enclosing environment, and
    # binds it even when the sequence is empty, so `{ for (i in v) NULL; i }`
    # reads no column `i`. The sequence is read before the index is bound, the
    # body after.
    inner <- bound
    if (is_name_part(expr[[2L]])) {
      inner <- unique(c(bound, rlang::as_name(expr[[2L]])))
    }
    return(list(
      reads = unique(c(
        expression_data_symbols(expr[[3L]], bound),
        expression_data_symbols(expr[[4L]], inner)
      )),
      bound = inner
    ))
  }
  value <- expression_data_symbols(expr[[3L]], bound)
  if (is_name_part(expr[[2L]])) {
    return(list(
      reads = value,
      bound = unique(c(bound, rlang::as_name(expr[[2L]])))
    ))
  }
  # A replacement form -- `names(x) <- v` -- reads its target before it rebuilds
  # it, so the target is walked and reported, in source order ahead of the
  # value. It rebinds the object too, but the walk does not record that: which
  # name a replacement call rebinds depends on the shape it is nested in, and
  # leaving it unbound over-reports.
  list(
    reads = unique(c(expression_data_symbols(expr[[2L]], bound), value)),
    bound = bound
  )
}

# What survives an `rm()` of the bound set it is given. A name written
# literally -- as a symbol or a string -- is the only removal readable here, so
# anything else empties the set rather than being ignored: `rm(list = names)`
# removes whatever that vector holds, and `rm(x, envir = e)` may remove nothing
# at all. Both leave every later read reported, which is the over-reporting
# side, while ignoring them would hide a read of a column that came back into
# view (#162).
removal_retained_bound <- function(expr, bound) {
  args <- static_call_args(expr)
  arg_names <- argument_names(args)
  removed <- character()
  for (i in seq_along(args)) {
    # By subscript throughout, and never `arg <- args[[i]]`: an argument the
    # caller left empty is bound to that name as R's missing marker, and the
    # first read of it raises `missingArgError` naming this frame's variable
    # (#168, #174). Such an argument is no literal, so it empties the bound set
    # like any other unreadable removal and the expression evaluates -- `rm(x,
    # )` is refused by `rm()` itself, and that is the error the caller sees.
    literal <- if (is_name_part(args[[i]])) {
      rlang::as_name(args[[i]])
    } else if (is.character(args[[i]]) && !anyNA(args[[i]])) {
      args[[i]]
    } else {
      NULL
    }
    if (nzchar(arg_names[[i]]) || is.null(literal)) {
      return(character())
    }
    removed <- c(removed, literal)
  }
  setdiff(bound, removed)
}

lookup_has_external_env <- function(expr) {
  call_supplies_other_argument(
    expr,
    "x",
    c("pos", "envir", "where", "frame")
  )
}

# The names a reflective lookup resolves in the mask.
#
# An environment argument terminates the search: all four primitives require an
# environment there -- `get("x", envir = list(x = 1))` is an error, not a
# lookup in that list -- so a call supplying one reads no column, and code
# reaching deliberately outside the mask keeps working beside a share of the
# same name.
#
# The bound set applies here, unlike at the pronoun below: these perform
# ordinary name resolution, so a local binding is what they find.
#
# A name that is not statically knowable is the undecidable shape #130's
# contract resolves toward over-reporting, and the marker is how this walk says
# so. Evaluating the argument to find out what it holds is the one thing this
# analysis may not do -- it runs while the call is planned, on the caller's own
# code.
reflective_lookup_symbols <- function(expr, bound) {
  if (lookup_has_external_env(expr)) {
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
  setdiff(looked_up, bound)
}

# What a `do.call()` reads beyond its own parts. It invokes the function it
# names in an environment that defaults to the caller's, which under a data
# mask is the mask, so `do.call("get", list("share"))` performs the lookup
# `get("share")` performs. What that lookup is handed sits in a list built at
# run time, which this walk cannot read, so a `do.call()` of a reflective
# primitive is reported as a lookup it cannot resolve.
#
# A `do.call()` of anything else needs no answer here: its arguments are
# values by the time it runs, and the walk has already reported the expressions
# that produced them.
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
evaluated_language_symbols <- function(expr, bound) {
  values <- evaluated_language_parts(expr)
  if (is.null(values)) {
    return(unresolved_lookup_name())
  }
  unique(unlist(
    lapply(values, expression_data_symbols, bound = bound),
    use.names = FALSE
  ))
}

# The name the walk reports for a read it could not resolve.
#
# #130 fixed the walk as two-valued -- a name is read or it is not -- and this
# keeps that shape rather than adding a third answer to it: the marker is a
# name, carried in the vector every branch already unions, and the one site
# that reads it is `expression_alias_dependencies()`, which turns it into a
# read of every alias in scope. Returning a record instead, as
# `statement_reads_and_bound()` does for the two answers a binding statement
# has, would rewrite every branch of the walk to carry a second field that
# only three of them can set.
#
# The alternatives are the two #130 rules out. Reporting nothing is the silence
# the whole walk exists to prevent, and refusing the call outright would reject
# `get(name)` wherever a summary precedes it, which is legal code.
#
# The `..marginplyr` prefix is the one this package's internal names carry. A
# column of this name would be read as unresolvable rather than as itself,
# which is the over-reporting direction, so a collision costs a diagnostic and
# never a silent miss.
unresolved_lookup_name <- function() {
  "..marginplyr_unresolved_lookup"
}
