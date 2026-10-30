# Carry one evaluated unnamed summary through dplyr's common-type pass.
# Its original prototype decides expansion; only scalar common types retain
# the Assigned summary name. The caller has checked output names and types.
new_local_summary_value <- function(value, name,
                                    ptype = vctrs::vec_ptype(value)) {
  columns <- if (is.data.frame(value)) {
    as.list(value)
  } else {
    stats::setNames(list(value), name)
  }
  vctrs::new_data_frame(
    columns, n = vctrs::vec_size(value),
    class = "margin_summary", ptype = ptype,
    assigned_name = name
  )
}

# Combine original types before deciding whether their columns expand.
# Both inputs belong to the same unnamed summary in one local branch.
#' @importFrom vctrs vec_ptype2
#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.margin_summary.margin_summary <- function(x, y, ...) {
  ptype <- vctrs::vec_ptype2(attr(x, "ptype"), attr(y, "ptype"), ...)
  new_local_summary_value(ptype, attr(x, "assigned_name"), ptype)
}

# Cast each group's carrier to the common expanded schema. An unspecified
# original value supplies missing rows of that schema, without its own column.
#' @importFrom vctrs vec_cast
#' @exportS3Method vctrs::vec_cast
vec_cast.margin_summary.margin_summary <- function(x, to, ...) {
  ptype <- attr(to, "ptype")
  if (inherits(attr(x, "ptype"), "vctrs_unspecified") && is.data.frame(ptype)) {
    value <- vctrs::vec_init(ptype, vctrs::vec_size(x))
  } else {
    value <- vctrs::df_cast(x, to, ...)
  }
  new_local_summary_value(value, attr(to, "assigned_name"), ptype)
}
