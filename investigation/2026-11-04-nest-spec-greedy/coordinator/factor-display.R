Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
library(marginplyr)
as.character.hunt_factor <- function(x, ...) {
  plain <- as.character.factor(x)
  observed <- !is.na(plain)
  plain[observed] <- sprintf("[%s]", plain[observed])
  plain
}
registerS3method("vec_ptype2", "hunt_factor.hunt_factor", function(x, y, ...) {
  vctrs::vec_slice(x, integer())
}, envir = asNamespace("vctrs"))
registerS3method("vec_cast", "hunt_factor.hunt_factor", function(x, to, ...) x,
                 envir = asNamespace("vctrs"))
g <- factor("a")
class(g) <- c("hunt_factor", "factor")
x <- tibble::tibble(g = g, row = 1L)
cat("SHA: 78ebbc36200bdc5cef70838dd4e772c5c4a1401c\n")
before <- serialize(x, NULL)
control <- tidyr::nest(x, .by = g)
stopifnot(identical(as.integer(control$g), 1L))
cat("Native outer code:", as.integer(control$g), "\n")
results <- lapply(list(nest_with_margins, nest_by_with_margins), function(verb) {
  ans <- verb(x, .grouping = grouping_set(g), .keep = TRUE)
  cat("Margin outer code:", as.integer(ans$g), "inner code:", as.integer(ans$data[[1L]]$g), "\n")
  stopifnot(identical(as.integer(ans$data[[1L]]$g), 1L))
  typed_missing <- verb(x, .grouping = grouping_set(g), .margin_label = NULL)
  cat("Typed-missing-label code:", as.integer(typed_missing$g), "class:", class(typed_missing$g), "\n")
  stopifnot(identical(as.integer(typed_missing$g), 1L))
  ans
})
stopifnot(identical(serialize(x, NULL), before))
stopifnot(all(vapply(results, function(ans) identical(as.integer(ans$g), 1L), logical(1))))
