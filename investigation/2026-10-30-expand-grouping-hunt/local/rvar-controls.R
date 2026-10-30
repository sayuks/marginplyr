# Related missingness behavior and a control for environment-only identity signals.
suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
value <- posterior::rvar(matrix(c(1, 2, 3, 4), nrow = 2L))
prototype <- value[NA_integer_]
stopifnot(is.array(value), is.list(value), all(is.na(prototype)),
          all(is.na(posterior::draws_of(prototype))))
cat("missing rvar: is.na", is.na(prototype), "vec_detect_missing",
    vctrs::vec_detect_missing(prototype), "\n")
input <- tibble::tibble(key = value, rid = 1:2)
control <- dplyr::union_all(
  input, dplyr::mutate(input, key = vctrs::vec_init(value, 2L))
)
stopifnot(all(is.na(posterior::draws_of(control$key)[, 3:4])))
refusal <- tryCatch(
  expand_with_margins(
    input, .grouping = rollup(key), .margin_label = NULL
  ),
  error = identity
)
cat("dimension expansion:",
    if (inherits(refusal, "error")) class(refusal)[[1L]] else "success", "\n")
payload <- tibble::tibble(key = c("b", "a"), value = value, rid = 1:2)
actual <- expand_with_margins(
  payload, .grouping = rollup(key), .margin_label = NULL, .id = "sid"
)
expected <- dplyr::union_all(
  dplyr::mutate(payload, sid = 1L),
  dplyr::mutate(payload, key = NA_character_, sid = 2L)
)
expected <- dplyr::relocate(expected, sid, .after = key)
stopifnot(isTRUE(all.equal(actual, expected)),
          identical(posterior::draws_of(actual$value),
                    posterior::draws_of(expected$value)))
cat("payload draw matrices: identical; all.equal: TRUE\n")

ordering <- tryCatch(vctrs::vec_order(value), error = identity)
stopifnot(inherits(ordering, "error"))
cat("ordinary rvar ordering:", conditionMessage(ordering), "\n")
