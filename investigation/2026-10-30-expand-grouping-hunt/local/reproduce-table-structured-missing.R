# A wholly missing table row is refused because its equality proxy includes keys.
suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
key <- as.table(matrix(1:2, nrow = 1L))
input <- tibble::tibble(key = key)
prototype <- vctrs::vec_init(key, 1L)
stopifnot(all(is.na(prototype)), identical(dim(prototype), c(1L, 2L)))
control <- dplyr::union_all(
  input, dplyr::mutate(input, key = vctrs::vec_init(key, 1L))
)
stopifnot(nrow(control) == 2L, identical(dim(control$key), c(2L, 2L)),
          all(is.na(vctrs::vec_slice(control$key, 2L))))
actual <- tryCatch(
  expand_with_margins(
    input, .grouping = rollup(key), .margin_label = NULL, .sort = "none"
  ),
  error = identity
)
cat("prototype scalar missingness:", as.vector(is.na(prototype)), "\n")
cat("prototype row missingness:", vctrs::vec_detect_missing(prototype), "\n")
cat("unclassed prototype row missingness:",
    vctrs::vec_detect_missing(unclass(prototype)), "\n")
cat("equality proxy:\n")
print(vctrs::vec_proxy_equal(prototype))
cat("ordinary result rows:", nrow(control), "shape:", dim(control$key), "\n")
if (inherits(actual, "error")) {
  cat("expansion:", class(actual)[[1L]], conditionMessage(actual), "\n")
} else {
  cat("expansion matches ordinary:", identical(actual, control), "\n")
}
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(identical(actual, control))
}
