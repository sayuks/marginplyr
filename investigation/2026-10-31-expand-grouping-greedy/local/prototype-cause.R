# Dated discrimination of three ranked causes: prototype loss, unsupported
# ordinary union, and failure of the missingness validation itself.
devtools::load_all(quiet = TRUE)
for (family in c("dist", "logLik")) {
  key <- if (family == "dist") {
    stats::dist(matrix(c(0, 1), ncol = 1L))
  } else {
    stats::logLik(stats::lm(mpg ~ wt, data = mtcars))
  }
  input <- tibble::tibble(key = key, row = 1L)
  missing <- vctrs::vec_init(key, 1L)
  scalar_subset <- key[NA_integer_]
  cat("Family:", family, "\n")
  cat("Scalar-subset class:", class(scalar_subset), "\n")
  cat("vec_init class:", class(missing), "\n")
  cat("Scalar-subset missing:", vctrs::vec_detect_missing(scalar_subset), "\n")
  cat("vec_init missing:", vctrs::vec_detect_missing(missing), "\n")
  combined <- dplyr::union_all(input, dplyr::mutate(input, key = !!missing))
  stopifnot(nrow(combined) == 2L, inherits(combined$key, family))
  cat("Ordinary branch combination: passed\n")
  actual <- tryCatch(expand_with_margins(
    input, .grouping = rollup(key), .margin_label = NULL
  ), error = identity)
  cat("Expansion condition:", class(actual)[[1L]], "\n")
  if (inherits(actual, "error")) cat(conditionMessage(actual), "\n")
}
# numeric_version's two missing representations compare equal through vctrs;
# the constructor's ordinary `[NA]` representation is usable in base Ops.
x <- numeric_version("1.1")
a <- x[NA_integer_]
b <- vctrs::vec_init(x, 1L)
cat("numeric_version missingness:", is.na(a), is.na(b), "\n")
cat("numeric_version value equality:", vctrs::vec_equal(a, b, na_equal = TRUE), "\n")
cat("numeric_version scalar-subset comparison:", a == x, "\n")
