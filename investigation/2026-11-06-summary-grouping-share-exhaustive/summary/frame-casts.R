# A dated research witness; run from the investigated source root.
Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
contract_mode <- '--assert' %in% commandArgs(trailingOnly = TRUE)
named_control <- '--named-control' %in% commandArgs(trailingOnly = TRUE)
stopifnot(!named_control || contract_mode)
run_margin <- function(data, expr) {
  dot <- stats::setNames(list(expr), if (named_control) 'result' else '')
  result <- rlang::eval_tidy(rlang::call2(
    summarize_with_margins, data, !!!dot, .by = quote(g)
  ))
  if (named_control) dplyr::bind_cols(result['g'], result$result) else result
}

new_distance <- function(x = double(), unit = 'm') {
  vctrs::new_data_frame(list(distance = as.double(x)), n = length(x),
                       class = paste0('distance_', unit))
}
vec_ptype2.distance_m.distance_m <- function(x, y, ...) new_distance()
vec_ptype2.distance_cm.distance_cm <- function(x, y, ...) new_distance(unit = 'cm')
vec_ptype2.distance_m.distance_cm <- function(x, y, ...) new_distance()
vec_ptype2.distance_cm.distance_m <- function(x, y, ...) new_distance()
vec_cast.distance_m.distance_m <- function(x, to, ...) x
vec_cast.distance_cm.distance_cm <- function(x, to, ...) x
vec_cast.distance_m.distance_cm <- function(x, to, ...) new_distance(x$distance / 100)
vec_cast.distance_cm.distance_m <- function(x, to, ...) new_distance(x$distance * 100, 'cm')

make_distance <- function(x) {
  if (x[[1L]] == 1) new_distance(1) else new_distance(100, 'cm')
}
d <- tibble(g = 1:2, x = 1:2)
expected <- summarise(d, make_distance(x), .by = g)
actual <- run_margin(d, quote(make_distance(x)))
cat('DIRECT vec_c canonical values\n')
print(vctrs::vec_c(new_distance(1), new_distance(100, 'cm')))
cat('UNNAMED ordinary dplyr\n'); print(expected)
cat('UNNAMED Margin\n'); print(actual)
stopifnot(identical(expected$distance, c(1, 1)))
if (!contract_mode) stopifnot(identical(actual$distance, c(1, 100)))

expected_named <- summarise(d, result = make_distance(x), .by = g)
actual_named <- summarize_with_margins(d, result = make_distance(x), .by = g)
cat('NAMED ordinary dplyr\n'); print(expected_named)
cat('NAMED Margin\n'); print(actual_named)
stopifnot(identical(actual_named, expected_named))

single_type <- function(x) new_distance(sum(x))
stopifnot(identical(summarise(d, single_type(x), .by = g),
                    summarize_with_margins(d, single_type(x), .by = g)))
cat('SINGLE TYPE control agreed\n')

# A pick-containing expression takes the Assigned summary name path.
frame_from_pick <- function(frame) make_distance(frame$x)
expected_pick <- summarise(d, frame_from_pick(pick(x)), .by = g)
actual_pick <- run_margin(d, quote(frame_from_pick(pick(x))))
cat('PICK ordinary dplyr\n'); print(expected_pick)
cat('PICK Margin\n'); print(actual_pick)
stopifnot(identical(expected_pick$distance, c(1, 1)))
if (!contract_mode) stopifnot(
          identical(actual_pick$distance, c(1, 100)))

if (contract_mode) {
  stopifnot(identical(actual$distance, expected$distance),
            identical(actual_pick$distance, expected_pick$distance))
}
cat(if (contract_mode) 'CONTRACT PASSED' else 'DATED WITNESS CONFIRMED',
    '; named and homogeneous-type controls passed\n')
