# Dated investigation evidence for marginplyr main ac6ad79, 2026-10-28.
# Run from the audited repository root or provide that root as the first argument.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)

x <- sf::st_as_sf(
  data.frame(g = c("a", "b"), x = 1:2, y = 3:4),
  coords = c("x", "y")
)
source_copy <- x

# Both upstream nesting counterparts can represent the source cells.
upstream_nest <- tidyr::nest(x, .by = g)
upstream_nest_by <- dplyr::nest_by(x, g)
stopifnot(
  nrow(upstream_nest) == 2L,
  nrow(upstream_nest_by) == 2L,
  all(vapply(upstream_nest$data, nrow, integer(1)) == 1L),
  all(vapply(upstream_nest_by$data, nrow, integer(1)) == 1L)
)
cat("PASS upstream tidyr::nest() and dplyr::nest_by() controls\n")

# A plain tibble carrying the same sfc column succeeds, including margins.
plain <- tibble::as_tibble(x)
plain_result <- nest_with_margins(
  plain, .grouping = rollup(g), .keep = TRUE, .id = "set", .sort = "last"
)
stopifnot(
  nrow(plain_result) == 3L,
  identical(plain_result$set, c(1L, 1L, 2L)),
  identical(plain_result$data[[3L]]$geometry, plain$geometry)
)
cat("PASS tibble with the same geometry payload\n")

# The intermediate select is valid; restoring its names with set_names leaves
# sf_column pointing to the temporary alias. No outer sf class is required.
aliases <- stats::setNames(names(x), seq_along(x))
selected <- dplyr::select(x, dplyr::all_of(aliases))
stopifnot(attr(selected, "sf_column") %in% names(selected))
renamed <- rlang::set_names(selected, names(x))
cat(
  "Temporary rename control: sf_column =", attr(renamed, "sf_column"),
  "; actual columns =", paste(names(renamed), collapse = ", "), "\n"
)

failures <- character()
for (verb_name in c("nest_with_margins", "nest_by_with_margins")) {
  verb <- get(verb_name)
  for (fixed in c(FALSE, TRUE)) {
    label <- paste(verb_name, if (fixed) ".by = g" else "no grouping")
    actual <- tryCatch(
      if (fixed) verb(x, .by = g) else verb(x),
      error = identity
    )
    if (inherits(actual, "error")) {
      cat("FAIL", label, ":", conditionMessage(actual), "\n")
      failures <- c(failures, label)
      next
    }
    cells <- actual$data
    stopifnot(
      nrow(actual) == if (fixed) 2L else 1L,
      sum(vapply(cells, nrow, integer(1))) == 2L,
      all(vapply(cells, function(cell) "geometry" %in% names(cell), logical(1)))
    )
  }
}
stopifnot(identical(x, source_copy))
if (length(failures)) {
  stop("Confirmed sf nesting failures: ", paste(failures, collapse = "; "))
}
cat("PASS sf nesting source-cell fidelity\n")
