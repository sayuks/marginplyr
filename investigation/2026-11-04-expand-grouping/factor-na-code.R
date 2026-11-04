pkgload::load_all(quiet = TRUE)

is.na.display_na_factor <- function(x) is.na(as.character.factor(x))
vctrs::s3_register("vctrs::vec_ptype2", "display_na_factor.display_na_factor",
                   function(x, y, ...) vctrs::vec_slice(x, integer()))
vctrs::s3_register("vctrs::vec_cast", "display_na_factor.display_na_factor",
                   function(x, to, ...) x)

input <- tibble::tibble(
  g = structure(c(1L, 2L, NA_integer_), levels = c("a", NA_character_),
                class = c("display_na_factor", "factor")),
  row_id = 1:3
)
raw_codes <- function(x) as.integer(x)
stopifnot(
  identical(is.na(input$g), c(FALSE, TRUE, TRUE)),
  identical(vctrs::vec_detect_missing(input$g), c(FALSE, FALSE, TRUE)),
  identical(raw_codes(dplyr::union_all(input, input)$g), rep(c(1L, 2L, NA_integer_), 2L)),
  identical(dplyr::group_rows(dplyr::group_by(input, g)), vctrs::list_of(
    1L, 2L, 3L, .ptype = integer()
  ))
)
expected <- c(1L, 2L, NA_integer_)
labelled <- function(data) {
  expand_with_margins(data, .grouping = grouping_set(g))
}
typed <- function(data) {
  expand_with_margins(data, .grouping = grouping_set(g), .margin_label = NULL)
}
local <- raw_codes(labelled(input)$g)
native_lazy <- raw_codes(dplyr::collect(dtplyr::lazy_dt(input))$g)
lazy <- raw_codes(dplyr::collect(labelled(dtplyr::lazy_dt(input)))$g)
typed_local <- raw_codes(typed(input)$g)
typed_lazy <- raw_codes(dplyr::collect(typed(dtplyr::lazy_dt(input)))$g)
print(list(expected = expected, local = local, native_lazy = native_lazy,
           lazy = lazy, typed_local = typed_local, typed_lazy = typed_lazy))
stopifnot(identical(expected, native_lazy), identical(expected, typed_local))
if ("--assert" %in% commandArgs(TRUE)) {
  stopifnot(identical(local, expected), identical(lazy, expected),
            identical(typed_lazy, expected))
}
