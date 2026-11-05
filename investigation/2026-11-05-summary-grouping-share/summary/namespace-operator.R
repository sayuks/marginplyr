pkgload::load_all(".", quiet = TRUE)

pick <- dplyr::pick
mutate <- dplyr::mutate
examples <- list(
  list("::", "base", "quote", quote(sum(pick(x))), 33L),
  list("::", "tidyselect", "all_of", quote(sum(pick(x))), 33L),
  list("::", "dplyr", "pick", quote((function() sum(pick(x)))()), 99L),
  list(":::", "marginplyr", "local_summary_selection",
       quote(sum(pick(x))), 33L),
  list(":::", "marginplyr", "nested_mask_thunk",
       quote(sum(mutate(data.frame(z = x), z = z)$z)), 99L)
)

observe <- function(verb, input, case, shadow = TRUE) {
  env <- rlang::env()
  hits <- 0L
  if (shadow) {
    env[[case[[1L]]]] <- function(pkg, name) {
      pkg <- as.character(substitute(pkg))
      name <- as.character(substitute(name))
      if (identical(pkg, case[[2L]]) && identical(name, case[[3L]])) {
        hits <<- hits + 1L
        return(switch(
          name,
          quote = function(...) as.name("y"),
          all_of = function(...) "y",
          pick = function(...) tibble::tibble(x = 99L),
          local_summary_selection = function(...) "y",
          nested_mask_thunk = function(...) function() data.frame(z = 99L)
        ))
      }
      if (identical(case[[1L]], ":::")) {
        get(name, envir = asNamespace(pkg))
      } else {
        getExportedValue(pkg, name)
      }
    }
  }
  expr <- rlang::new_quosure(case[[4L]], env)
  out <- verb(input, answer = !!expr)
  list(value = out$answer, hits = hits)
}

data <- tibble::tibble(x = 1:3, y = 10:12)
inputs <- list(as.data.frame(data), data, data.table::as.data.table(data))
assert <- "--assert" %in% commandArgs(TRUE)
for (input in inputs) {
  for (case in examples) {
    ordinary <- observe(dplyr::summarise, input, case)
    unshadowed <- observe(summarize_with_margins, input, case, shadow = FALSE)
    actual <- observe(summarize_with_margins, input, case)
    stopifnot(
      identical(ordinary, list(value = 6L, hits = 0L)),
      identical(unshadowed, ordinary)
    )
    expected <- if (assert) ordinary else list(value = case[[5L]], hits = 1L)
    stopifnot(identical(actual, expected))
    cat(class(input)[1L], case[[1L]], case[[2L]], case[[3L]], "ordinary",
        ordinary$value, "Margin", actual$value, "caller hits", actual$hits, "\n")
  }
}
