Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
data <- tibble::tibble(a = c("east", "west"), b = 1L)
makeActiveBinding("hunt_pattern_source", function() {
  if (is.character(tidyselect::peek_data()$a)) "^a$" else "^b$"
}, globalenv())
caller <- rlang::env(globalenv(), paste0 = "ordinary nonfunction binding")
body_as_vector <- quote(function() {
  paste0 <- as.vector(as.vector("hunt_pattern_source", "symbol"), "function")
  paste0()
})
body_literal_function <- quote(function() {
  paste0 <- function() hunt_pattern_source
  paste0()
})
run <- function(pattern) {
  force(pattern)
  tryCatch(marginplyr::inspect_grouping(
    data, .by = b,
    .grouping = marginplyr::grouping_set(matches(pattern())), .format = "list"
  ), error = identity)
}
actual <- run(eval(body_as_vector, caller))
stopifnot(inherits(actual, "marginplyr_error"))
cat("Original: false fixed-key overlap\n")
control <- run(eval(body_literal_function, caller))
stopifnot(identical(control$included, list("a")))
cat("Literal closure instead of as.vector: correct a dimension\n")
rm("paste0", envir = caller)
control <- run(eval(body_as_vector, caller))
stopifnot(identical(control$included, list("a")))
cat("No enclosing nonfunction callee binding: correct a dimension\n")
