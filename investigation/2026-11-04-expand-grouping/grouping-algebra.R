pkgload::load_all(".", quiet = TRUE)
data <- tibble::tibble(fixed = "k", a = 1L, b = "b", c = TRUE, d = 2)
vars <- setdiff(names(data), "fixed")
leaf <- function(cols) {
  list(expr = rlang::call2("grouping_set", rlang::call2("all_of", cols, .ns = "tidyselect")),
       sets = list(cols))
}
construct <- function(kind, args, expected) {
  list(expr = rlang::call2(kind, !!!lapply(args, "[[", "expr")), sets = expected)
}
unit_sets <- function(units, kind) {
  n <- length(units)
  if (identical(kind, "rollup")) {
    return(lapply(rev(seq.int(0L, n)), function(i) unique(as.character(unlist(units[seq_len(i)])))))
  }
  indices <- expand.grid(rep(list(c(FALSE, TRUE)), n))
  selected <- as.matrix(indices)
  row_keys <- apply(selected, 1L, function(x) paste(ifelse(x, 0L, 1L), collapse = ""))
  lapply(order(-rowSums(selected), row_keys), function(i) unique(as.character(unlist(units[selected[i, ]]))))
}
compose <- function(args) {
  Reduce(
    function(left, right) {
      do.call(c, lapply(left, function(lhs) lapply(right, function(rhs) union(lhs, rhs))))
    },
    lapply(args, "[[", "sets"), init = list(character())
  )
}
units <- list("a", "b", c("b", "a"), c("a", "c"), c("d", "a"))
families <- list(leaf(character()), leaf(c("b", "a")))
for (kind in c("rollup", "cube")) {
  for (row in seq_len(125L)) {
    ids <- as.integer(expand.grid(rep(list(seq_along(units)), 3L))[row, ])
    selected <- units[ids]
    families[[length(families) + 1L]] <- construct(kind, lapply(selected, leaf), unit_sets(selected, kind))
  }
}
set.seed(4104)
for (i in seq_len(120L)) {
  args <- families[sample.int(length(families), 2L)]
  families[[length(families) + 1L]] <- construct(
    "grouping_sets", args, do.call(c, lapply(args, "[[", "sets"))
  )
  # Avoid growing the independent product oracle beyond this finite sweep.
  if (prod(lengths(lapply(args, "[[", "sets"))) <= 200L) {
    families[[length(families) + 1L]] <- construct("grouping_spec", args, compose(args))
  }
}
success <- 0L
refusals <- 0L
for (entry in families) {
  sets <- entry$sets
  dimensions <- unique(as.character(unlist(sets, use.names = FALSE)))
  sets <- lapply(sets, function(cols) intersect(dimensions, cols))
  duplicate <- duplicated(sets)
  spec <- eval(entry$expr)
  for (duplicate_policy in c("error", "drop", "keep")) {
    for (grouped in c(FALSE, TRUE)) {
      result <- tryCatch(
        if (grouped) {
          inspect_grouping(dplyr::group_by(data, fixed), .grouping = spec,
                           .duplicates = duplicate_policy, .format = "list")
        } else {
          inspect_grouping(data, .by = fixed, .grouping = spec,
                           .duplicates = duplicate_policy, .format = "list")
        },
        error = identity
      )
      if (duplicate_policy == "error" && any(duplicate)) {
        stopifnot(inherits(result, "marginplyr_error"), grepl("Duplicate grouping sets", conditionMessage(result), fixed = TRUE))
        refusals <- refusals + 1L
      } else {
        expected <- if (duplicate_policy == "drop") sets[!duplicate] else sets
        omitted <- lapply(expected, function(cols) setdiff(dimensions, cols))
        bits <- lapply(omitted, function(cols) stats::setNames(as.integer(dimensions %in% cols), dimensions))
        bits <- lapply(bits, function(bit) if (!length(bit)) integer() else bit)
        ids <- vapply(bits, function(bit) {
          Reduce(function(total, digit) 2L * total + digit, bit, init = 0L)
        }, integer(1L))
        stopifnot(
          identical(class(result), c("tbl_df", "tbl", "data.frame")),
          identical(result$set_id, seq_along(expected)),
          identical(result$included, expected),
          identical(result$omitted, omitted),
          identical(result$fixed, rep(list("fixed"), length(expected))),
          identical(result$grouping_bits, bits),
          identical(result$grouping_id, ids)
        )
        success <- success + 1L
      }
    }
  }
}
cat("families", length(families), "exact", success, "required duplicate refusals", refusals, "\n")
for (n in c(0L, 1L, 16L, 29L, 30L, 31L, 32L, 33L, 64L)) {
  wide_vars <- if (n) paste0("dim", seq_len(n)) else character()
  wide <- tibble::as_tibble(c(list(fixed = "k"), stats::setNames(as.list(seq_len(n)), wide_vars)))
  spec <- eval(rlang::call2("rollup", !!!rlang::syms(wide_vars)))
  if (n == 0L) spec <- grouping_spec()
  res <- inspect_grouping(wide, .by = fixed, .grouping = spec, .format = "list")
  stopifnot(
    identical(res$set_id, seq_len(n + 1L)),
    identical(res$grouping_id, if (n <= 31L) as.integer(2 ^ seq.int(0L, n) - 1) else rep(NA_integer_, n + 1L)),
    all(lengths(res$grouping_bits) == n),
    identical(res$omitted[[n + 1L]], wide_vars)
  )
}
cat("9 grouping identifier widths 0/1/16/29/30/31/32/33/64 matched\n")
stored <- local({
  names <- c("b", "a")
  unit <- grouping_set(tidyselect::all_of(names))
  family <- grouping_sets(unit, grouping_set(c), grouping_set())
  grouping_spec(grouping_set(d), family)
})
copy <- unserialize(serialize(stored, NULL))
for (format in c("list", "text")) {
  stopifnot(identical(
    inspect_grouping(data, .grouping = stored, .format = format),
    inspect_grouping(data, .grouping = copy, .format = format)
  ))
}
stopifnot(identical(capture.output(print(copy)), "<marginplyr Grouping specification: grouping_spec>"))
cat("serialized local closure and nested-name plan matched both formats and printed public constructor\n")
