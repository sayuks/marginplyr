# Run from the repository root. --assert requires the intended public result.
suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))

key <- table(c("a", "b", "b"))
input <- tibble::tibble(key = key, rid = seq_along(key))
control <- dplyr::union_all(
  dplyr::mutate(input, sid = 1L),
  dplyr::mutate(input, key = vctrs::vec_init(key, length(key)), sid = 2L)
)
control <- dplyr::relocate(control, sid, .after = key)
included <- expand_with_margins(
  input, .grouping = grouping_set(key), .margin_label = NULL
)
stopifnot(
  identical(included, input), nrow(control) == 4L,
  inherits(control$key, "table"), identical(dim(control$key), 4L)
)
# The column is also admitted by ordinary grouping, independently of expansion.
stopifnot(identical(dplyr::group_vars(dplyr::group_by(input, key)), "key"))

actual <- tryCatch(
  expand_with_margins(
    input, .grouping = rollup(key), .margin_label = NULL,
    .id = "sid", .sort = "none"
  ),
  error = identity
)
cat("ordinary result: rows", nrow(control), "class", class(control$key),
    "shape", dim(control$key), "\n")
cat("included-only: match; ordinary grouping: accepted\n")
if (inherits(actual, "error")) {
  cat("expansion:", class(actual)[[1L]], conditionMessage(actual), "\n")
} else {
  cat("expansion matches ordinary:", identical(actual, control), "\n")
}
cat("subset prototype class:", class(key[NA_integer_]),
    "shape:", dim(key[NA_integer_]), "\n")
cat("vec_init prototype class:", class(vctrs::vec_init(key, 1L)),
    "shape:", dim(vctrs::vec_init(key, 1L)), "\n")
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(identical(actual, control))
}
