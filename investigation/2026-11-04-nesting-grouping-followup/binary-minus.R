args <- commandArgs(trailingOnly = TRUE)
source <- if (length(args) && args[[1L]] != "--assert") args[[1L]] else "."
pkgload::load_all(source, quiet = TRUE)
d <- tibble::tibble(a = c("same", "same"), b = 1:2)
direct <- function(a = if (is.character(tidyselect::peek_data()$a)) 3L else 2L,
                   b = 1L) names(tidyselect::eval_select(rlang::quo(a - b), d))
nest <- function(verb,
                 a = if (is.character(tidyselect::peek_data()$a)) 3L else 2L,
                 b = 1L) verb(d, .grouping = grouping_set(a - b))
stopifnot(identical(direct(), "b"))
for (verb in list(nest_with_margins, nest_by_with_margins)) {
  result <- nest(verb)
  cat("Nest outer key and rows:", names(result)[1L], nrow(result), "\n")
  stopifnot(nrow(verb(d, .grouping = grouping_set(b))) == 2L)
}
stopifnot(identical(inspect_grouping(d, .by = -a)$fixed, "(b)"))
# This mixed specification must not run a caller binding in its discarded pass.
seen <- character()
e <- new.env()
makeActiveBinding("a", function() {
  type <- typeof(tidyselect::peek_data()$a)
  seen <<- c(seen, type)
  if (identical(type, "character")) 3L else 2L
}, e)
e$b <- 1L
q <- rlang::new_quosure(quote(a - b), e)
mixed <- inspect_grouping(d, .grouping = grouping_sets(!!q, tidyselect::where(is.character)),
                         .duplicates = "keep", .format = "list")
cat("Mixed selection proxy types:", paste(seen, collapse = ", "), "\n")
print(mixed$included)
stopifnot(identical(mixed$included, list("b", "a")))
if ("--assert" %in% args) {
  stopifnot(all(vapply(list(nest_with_margins, nest_by_with_margins),
                      function(verb) nrow(nest(verb)) == 2L, logical(1L))),
            identical(seen, "character"))
} else {
  stopifnot(identical(seen, c("integer", "character")))
}
