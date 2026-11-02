# Distinguish an empty geometry from a representable typed-missing vector value.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
x <- sf::st_sfc(sf::st_point(c(1, 2)))
prototype <- vctrs::vec_init(x, 1L)
print(prototype)
print(is.na(prototype))
print(vctrs::vec_detect_missing(prototype))
print(sf::st_is_empty(prototype))
control <- s2::as_s2_geography("POINT (1 2)")
print(vctrs::vec_detect_missing(vctrs::vec_init(control, 1L)))
stopifnot(
  !is.na(prototype), !vctrs::vec_detect_missing(prototype),
  sf::st_is_empty(prototype),
  vctrs::vec_detect_missing(vctrs::vec_init(control, 1L))
)
