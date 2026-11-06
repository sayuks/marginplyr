Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)

# Independent witness: a rank-three list array, including an actual NULL cell.
x <- array(list(11L, NULL, "x", 19L), dim = c(2L, 1L, 2L))
source <- tibble::tibble(g = x, payload = c(31L, 47L))
missing <- vctrs::vec_init(x, 1L)
stopifnot(all(vctrs::vec_detect_missing(missing)), identical(dim(missing), c(1L, 1L, 2L)))
omitted <- dplyr::mutate(source, g = vctrs::vec_rep(missing, nrow(source)))
ordinary <- dplyr::union_all(source, omitted)
stopifnot(nrow(ordinary) == 4L, identical(dim(ordinary$g), c(4L, 1L, 2L)),
          identical(ordinary$payload, c(31L, 47L, 31L, 47L)),
          all(vctrs::vec_detect_missing(ordinary$g)[3:4]))
included <- expand_with_margins(source, .grouping = grouping_set(g), .margin_label = NULL)
payload <- expand_with_margins(source, .grouping = grouping_set(), .margin_label = NULL)
stopifnot(identical(included$g, x), identical(payload$g, x))
actual <- tryCatch(expand_with_margins(source, .grouping = rollup(g),
                                     .margin_label = NULL), error = identity)
print(list(ordinary = ordinary, included_control = included,
           observed = if (inherits(actual, "error")) conditionMessage(actual) else actual))
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(!inherits(actual, "error"), identical(actual$g, ordinary$g),
            identical(actual$payload, ordinary$payload))
}
