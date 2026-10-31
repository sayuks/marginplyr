pkgload::load_all(quiet = TRUE)
set.seed(31103172)
pool <- c(".data", ".env", ".rows", ".SD", ".N", ".I", "T", "F", "..1", "..01", "...", "{x}", "}", "x`y", "x\\y", "x\ny", "x\ry", "x\ty", "e\u0301", "\u00e9", "\u200b", "\U0001f9ed", "a b", "..marginplyr_nest_1", "..marginplyr_cell_1", "by", "key", "row.names", "check.names", "stringsAsFactors", "n", "data")
checks <- projections <- refusals <- upstream_refusals <- 0L
for (iteration in seq_len(240L)) {
  selected <- sample(pool, 5L)
  n <- sample(c(0L, 1L, 7L), 1L)
  original <- tibble::tibble(
    fixed = rep(c("x", "y", NA_character_), length.out = n),
    dimension = rep(c("a", "b", "a", NA_character_), length.out = n),
    payload = I(matrix(seq_len(n * 2L), nrow = n, ncol = 2L)),
    row_id = seq_len(n)
  )
  names(original)[1:3] <- selected[1:3]
  before <- serialize(original, NULL)
  upstream <- tryCatch(dplyr::select(original, dplyr::everything()), error = identity)
  keep <- sample(c(FALSE, TRUE), 1L)
  label <- if (sample(c(FALSE, TRUE), 1L)) NULL else "Total"
  for (verb_name in c("nest_with_margins", "nest_by_with_margins")) {
    verb <- get(verb_name)
    call <- rlang::expr(verb(original, .by = dplyr::all_of(!!selected[[1L]]),
      .grouping = rollup(dplyr::all_of(!!selected[[2L]])), .id = !!selected[[5L]],
      .key = !!selected[[4L]], .keep = !!keep, .margin_label = !!label,
      .sort = !!sample(c("none", "first", "last"), 1L)))
    result <- tryCatch(rlang::eval_tidy(call), error = identity)
    if (inherits(upstream, "vctrs_error_names_cannot_be_dot_dot")) {
      stopifnot(inherits(result, "vctrs_error_names_cannot_be_dot_dot"))
      upstream_refusals <- upstream_refusals + 1L
      next
    }
    expected_refusal <- verb_name == "nest_by_with_margins" && ".rows" %in% selected[c(1L, 2L, 5L)]
    if (expected_refusal) {
      stopifnot(inherits(result, "marginplyr_error"))
      refusals <- refusals + 1L
      next
    }
    if (inherits(result, "error")) {
      print(list(iteration = iteration, selected = selected, verb = verb_name, keep = keep, label = label, n = n))
      print(result)
      stop(conditionMessage(result))
    }
    fixed <- selected[[1L]]
    dimension <- selected[[2L]]
    sets <- list(c(fixed, dimension), fixed)
    for (set in seq_along(sets)) {
      expected <- vctrs::vec_group_loc(original[sets[[set]]])$loc
      observed <- lapply(result[[selected[[4L]]]][result[[selected[[5L]]]] == set], function(cell) cell$row_id)
      normalize <- function(groups) groups[order(vapply(groups, function(ids) ids[[1L]], integer(1)))]
      stopifnot(identical(normalize(expected), normalize(observed)))
    }
    for (cell in result[[selected[[4L]]]]) {
      wanted <- if (keep) names(original) else setdiff(names(original), selected[1:2])
      stopifnot(identical(names(cell), wanted))
      stopifnot(identical(cell[[selected[[3L]]]], original[[selected[[3L]]]][cell$row_id, , drop = FALSE]))
      if (keep) for (name in selected[1:2]) stopifnot(identical(cell[[name]], original[[name]][cell$row_id]))
      projections <- projections + 1L
    }
    checks <- checks + 1L
  }
  stopifnot(identical(before, serialize(original, NULL)))
}
cat("constructed_calls", checks + refusals + upstream_refusals, "accepted", checks,
    "documented_rowwise_refusals", refusals, "upstream_dot_dot_source_refusals", upstream_refusals,
    "exact_cell_projections", projections, "source_mutations", 0L, "\n")
