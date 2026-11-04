pkgload::load_all(quiet = TRUE)
source <- tibble::tibble(
  g = I(as.POSIXlt(c("2020-01-02", "2020-01-01", "2020-01-02"), tz = "UTC")),
  v = 1:3
)
native <- dplyr::nest_by(source, g, .keep = TRUE)
normalized <- vctrs::vec_slice(source, seq_len(nrow(source)))
stopifnot(is.data.frame(normalized$g), identical(lapply(native$g, as.vector), lapply(vctrs::vec_slice(normalized$g, 2:1), as.vector)))
empty <- vctrs::vec_init(native$g, 1L)
expected <- dplyr::bind_rows(
  dplyr::mutate(dplyr::ungroup(native), set = 1L),
  tibble::tibble(g = empty, data = list(normalized), set = 2L)
)
projection <- function(x) list(
  components = lapply(x$g, as.vector),
  members = lapply(x$data, function(cell) cell$v),
  kept_components = lapply(x$data, function(cell) lapply(cell$g, as.vector)),
  id = x$set
)
stopifnot(identical(expected$set, c(1L, 1L, 2L)),
  identical(lapply(expected$data, function(cell) cell$v), list(2L, c(1L, 3L), 1:3)),
  all(vctrs::vec_detect_missing(expected$g[3L, ])))
cat("Native typed-missing branch composition succeeds\n")
for (verb in list(nest_with_margins, nest_by_with_margins)) {
  control <- verb(normalized, .grouping = rollup(g), .margin_label = NULL,
    .keep = TRUE, .id = "set", .sort = "last")
  stopifnot(identical(projection(control), projection(expected)))
  cat("Normalized source works for the same Margin verb\n")
  result <- tryCatch(verb(source, .grouping = rollup(g), .margin_label = NULL,
    .keep = TRUE, .id = "set", .sort = "last"), error = identity)
  if (inherits(result, "error")) {
    cat(conditionMessage(result), "\n")
    stopifnot(inherits(result, "vctrs_error_ptype2"))
  }
  if ("--assert" %in% commandArgs(TRUE)) {
    stopifnot(!inherits(result, "error"), identical(projection(result), projection(expected)))
  } else stopifnot(inherits(result, "vctrs_error_ptype2"))
}
