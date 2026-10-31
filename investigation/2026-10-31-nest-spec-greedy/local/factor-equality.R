pkgload::load_all(quiet = TRUE)

new_casefold_factor <- function(x, na_level = FALSE) {
  result <- factor(x, levels = c("A", "a", "B", "b", if (na_level) NA_character_), exclude = NULL)
  class(result) <- c("hunt_casefold_factor", "factor")
  result
}
base_factor <- function(x) { class(x) <- "factor"; x }
registerS3method("vec_proxy_equal", "hunt_casefold_factor", function(x, ...) {
  tolower(as.character(base_factor(x)))
}, asNamespace("vctrs"))
registerS3method("vec_ptype2", "hunt_casefold_factor.hunt_casefold_factor", function(x, y, ...) {
  vctrs::vec_slice(x, integer())
}, asNamespace("vctrs"))
registerS3method("vec_cast", "hunt_casefold_factor.hunt_casefold_factor", function(x, to, ...) x, asNamespace("vctrs"))

input <- tibble::tibble(g = new_casefold_factor(c("A", "a"), TRUE), row_id = 1:2)
expected <- list(1:2)
stopifnot(identical(vctrs::vec_group_loc(input$g)$loc, expected))
stopifnot(identical(lapply(dplyr::nest_by(input, g)$data, function(x) x$row_id), expected))
combined <- dplyr::union_all(input, input)
stopifnot(inherits(combined$g, "hunt_casefold_factor"))
stopifnot(identical(vctrs::vec_group_loc(combined$g)$loc, list(1:4)))
cat("ordinary dplyr source and union controls preserved one source group\n")

results <- lapply(list(nest_with_margins, nest_by_with_margins), function(verb) {
  verb(input, .grouping = grouping_set(g))
})
observed <- lapply(results, function(result) lapply(result$data, function(cell) cell$row_id))
print(observed)

controls <- lapply(list(nest_with_margins, nest_by_with_margins), function(verb) {
  without_na <- input
  without_na$g <- new_casefold_factor(c("A", "a"), FALSE)
  list(no_na_level = verb(without_na, .grouping = grouping_set(g)),
       fixed_key = verb(input, .by = g))
})
stopifnot(all(vapply(controls, function(pair) all(vapply(pair, function(result) {
  identical(lapply(result$data, function(cell) cell$row_id), expected)
}, logical(1))), logical(1))))
cat("removing the unused NA level or selecting the source as a fixed key restored one cell\n")

if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(all(vapply(observed, identical, logical(1), expected)))
} else {
  stopifnot(all(vapply(observed, identical, logical(1), list(1L, 2L))))
  cat("DATED OBSERVATION: both nesting verbs split one group into two cells\n")
}
