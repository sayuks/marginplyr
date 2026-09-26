# PROTOTYPE ONLY: metadata injection into a direct SQLite Margin result.
# Run from the prototype worktree: Rscript R/prototype-706-helper-types.R
suppressPackageStartupMessages(pkgload::load_all(
  Sys.getenv("MARGINPLYR_SOURCE", "."), quiet = TRUE
))

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
on.exit(DBI::dbDisconnect(con))
source <- dplyr::copy_to(
  con, tibble::tibble(g = "a", v = 1), "prototype_706_source",
  temporary = TRUE
)

# This is deliberately outside the package. It checks whether its existing
# direct SQLite boundary can carry two more package-declared integer columns.
prototype_declare <- function(x, source, declared) {
  if (!inherits(x, "marginplyr_sqlite_typed_result")) {
    public <- as.character(dplyr::tbl_vars(x))
    raw <- x
    class(x) <- c("marginplyr_sqlite_typed_result", class(x))
    attr(x, "marginplyr_public_query") <- raw
    attr(x, "marginplyr_public_columns") <- public
    attr(x, "marginplyr_order_keys") <- character()
    attr(x, "marginplyr_original_query") <- x$lazy_query
  }
  all_declared <- c(attr(x, "marginplyr_declared_types"), declared)
  attr(x, "marginplyr_declared_types") <- all_declared
  attr(x, "marginplyr_public_anchor") <- marginplyr:::sql_margin_type_anchor(
    source, attr(x, "marginplyr_public_query"),
    source_columns = character(), declared_types = all_declared
  )
  x
}

prototype_direct_declarations <- function(dots) {
  direct <- vapply(dots, function(dot) {
    !is.null(marginplyr:::grouping_helper_name(rlang::quo_get_expr(dot)))
  }, logical(1))
  stats::setNames(rep("integer", sum(direct)), names(dots)[direct])
}

declared_helpers <- prototype_direct_declarations(rlang::quos(
  bit = grouping_bit(g),
  mask = grouping_id(),
  wrapped = as.integer(grouping_id())
))
print(declared_helpers)

run_case <- function(rows, sort, set_id) {
  input <- if (rows == "empty") dplyr::filter(source, v < 0) else source
  direct <- summarize_with_margins(
    input,
    bit = grouping_bit(g),
    mask = grouping_id(),
    wrapped = as.integer(grouping_id()),
    .grouping = grouping_set(g), .id = set_id,
    .margin_label = NULL, .sort = sort
  )
  declared <- prototype_declare(direct, source, declared_helpers)
  type_vec <- function(value) vapply(value, typeof, character(1))
  data.frame(
    rows = rows, sort = sort, set_id = !is.null(set_id),
    boundary = c("current collect", "prototype collect",
                 "current compute", "prototype compute"),
    bit = c(
      type_vec(dplyr::collect(direct))[["bit"]],
      type_vec(dplyr::collect(declared))[["bit"]],
      type_vec(dplyr::collect(dplyr::compute(direct)))[["bit"]],
      type_vec(dplyr::collect(dplyr::compute(declared)))[["bit"]]
    ),
    mask = c(
      type_vec(dplyr::collect(direct))[["mask"]],
      type_vec(dplyr::collect(declared))[["mask"]],
      type_vec(dplyr::collect(dplyr::compute(direct)))[["mask"]],
      type_vec(dplyr::collect(dplyr::compute(declared)))[["mask"]]
    ),
    wrapped = c(
      type_vec(dplyr::collect(direct))[["wrapped"]],
      type_vec(dplyr::collect(declared))[["wrapped"]],
      type_vec(dplyr::collect(dplyr::compute(direct)))[["wrapped"]],
      type_vec(dplyr::collect(dplyr::compute(declared)))[["wrapped"]]
    )
  )
}

cases <- list()
for (rows in c("populated", "empty")) {
  for (sort in c("none", "last")) {
    for (set_id in list(NULL, "sid")) {
      # A sorted result has an existing dedicated boundary even without .id.
      # The unsorted no-.id result exercises the manual boundary construction.
      cases[[length(cases) + 1L]] <- run_case(rows, sort, set_id)
    }
  }
}
print(do.call(rbind, cases), row.names = FALSE)
