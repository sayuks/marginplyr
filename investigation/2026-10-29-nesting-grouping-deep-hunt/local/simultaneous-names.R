args <- commandArgs(TRUE)
pkgload::load_all(if (length(args)) args[[1L]] else ".", quiet = TRUE)
set.seed(29102971)
source_names <- c("a", "b", "c", ".", ".data", ".env", ".rows", ".SD", ".SDall", ".xSD", ".iSD", ".N", ".I", ".GRP", ".BY", ".NGRP", ".ROW", "T", "F", "TRUE", "FALSE", "NULL", "NA", "key", "check.names", "keep.rownames", "stringsAsFactors", "data", "set", "..marginplyr_nest_1", "..marginplyr_column_1", "..marginplyr_dtplyr_column_1", "x y", "a\nb", "a\tb", "日本語", "{.env}", "1", "")
source_names <- source_names[nzchar(source_names)]
output_names <- c("data", ".rows", "..1", "...", "..0", "..01", ".SD", ".xSD", "T", ".data", ".env", "{.env}", "a\nb", "日本語")
make_value <- function(kind, n) {
  at <- rep(seq_len(6L), length.out = n)
  switch(kind,
    integer = c(1L, 2L, NA_integer_, 1L, 2L, NA_integer_)[at],
    character = c("a", "b", NA_character_, "a", "Total", "")[at],
    factor = {
      f <- structure(c(1L, 2L, NA_integer_, 3L, 4L, 1L), levels = c("b", NA_character_, "..marginplyr_missing_factor_code", "a", "unused"), class = "factor")
      f[at]
    },
    double = c(0, -0, NA_real_, NaN, Inf, 1e-40)[at],
    packed = tibble::tibble(x = c(1L, 2L, NA_integer_, 1L, 2L, 0L)[at], z = c("a", "b", NA_character_, "a", "z", "")[at]),
    matrix = matrix(rep(c(1L, 2L, NA_integer_, 1L, 2L, 0L)[at], 2L), ncol = 2L),
    list = list(NULL, list(x=1L), quote(a+b), 1L, NA, list(x=1L))[at]
  )
}
make_spec <- function(sets) {
  rlang::eval_bare(rlang::call2("grouping_sets", !!!lapply(sets, function(x) rlang::call2("grouping_set", rlang::call2("all_of", x)))))
}
# Cells are compared to independent source projections, never to the other verb.
compared <- successful <- rejected <- 0L
for (case in seq_len(1200L)) {
  n <- sample(c(0L, 1L, 2L, 7L, 11L), 1L)
  fields <- sample(source_names, sample(4:8, 1L))
  values <- lapply(seq_along(fields), function(i) make_value(sample(c("integer", "character", "factor", "double", "packed", "matrix", "list"), 1L), n))
  x <- tibble::as_tibble(stats::setNames(values, fields), .name_repair = "minimal")
  x$row_id <- seq_len(n)
  chosen <- sample(fields, sample(0:min(4L, length(fields)), 1L))
  fixed <- if (length(chosen) && sample(c(TRUE, FALSE), 1L)) chosen[[1L]] else character()
  dims <- setdiff(chosen, fixed)
  sets <- list(dims, utils::head(dims, max(0L, length(dims) - 1L)), character())
  sets <- sets[!duplicated(vapply(sets, function(s) paste(s, collapse="\r"), character(1)))]
  spec <- make_spec(sets)
  key <- sample(setdiff(output_names, chosen), 1L)
  id <- sample(setdiff(c(output_names, "case_set"), c(names(x), key)), 1L)
  keep <- sample(c(TRUE, FALSE), 1L)
  sorting <- sample(c("none", "first", "last"), 1L)
  verb <- sample(c("nest_with_margins", "nest_by_with_margins"), 1L)
  before <- serialize(x, NULL)
  result <- tryCatch(get(verb)(x, .by = dplyr::all_of(fixed), .grouping = spec, .margin_label = NULL, .key = key, .id = id, .keep = keep, .sort = sorting), error = identity)
  stopifnot(identical(serialize(x,NULL), before))
  if (inherits(result, "error")) {
    stopifnot(verb == "nest_by_with_margins", ".rows" %in% c(chosen, id), inherits(result, "marginplyr_error"))
    rejected <- rejected + 1L
    next
  }
  successful <- successful + 1L
  stopifnot(identical(names(result), c(fixed, dims, id, key)), !dplyr::is_grouped_df(result))
  if (verb == "nest_by_with_margins") stopifnot(inherits(result, "rowwise_df"), identical(dplyr::group_vars(result), c(fixed, dims, id)))
  expected_columns <- if (keep) c(fixed, dims, setdiff(names(x), c(fixed,dims))) else setdiff(names(x), c(fixed,dims))
  for (set in seq_along(sets)) {
    group_names <- c(fixed, sets[[set]])
    memberships <- if (length(group_names)) vctrs::vec_group_loc(tibble::as_tibble(as.list(x)[group_names], .name_repair="minimal"))$loc else list(seq_len(n))
    if (!n) memberships <- list()
    cells <- result[[key]][result[[id]] == set]
    # Only the no-key rowwise empty-input special case creates an empty cell.
    if (!n && verb == "nest_by_with_margins" && !length(chosen)) memberships <- list(integer())
    stopifnot(length(cells) == length(memberships))
    expected_members <- sort(vapply(memberships, function(ix) paste(ix, collapse=","), character(1)))
    observed_members <- sort(vapply(cells, function(cell) paste(cell$row_id, collapse=","), character(1)))
    stopifnot(identical(expected_members, observed_members))
    for (cell in cells) {
      stopifnot(identical(names(cell), expected_columns))
      expected <- vctrs::vec_slice(x, cell$row_id)
      for (column in expected_columns) {
        stopifnot(isTRUE(all.equal(cell[[column]], expected[[column]], check.attributes = TRUE)))
        compared <- compared + 1L
      }
    }
  }
}
cat("PASS:", successful, "accepted calls;", rejected, "documented rowwise refusals;", compared, "exact cell-column projections; seed 29102971\n")
