args <- commandArgs(trailingOnly = TRUE)
source <- if (length(args) && args[[1]] != "--assert") args[[1]] else "."
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(source, quiet = TRUE)
d <- tibble::tibble(a = c("x", "x"), b = 1:2)
calls <- 0L
check <- function(run, a = {
  calls <<- calls + 1L
  if (is.character(tidyselect::peek_data()$a)) 3L else 2L
}, b = 1L) run(rlang::quo(a - b))
cat("R:", as.character(getRversion()), R.version$platform, "\n")
cat("Dependencies:", paste(vapply(c("dplyr", "rlang", "tidyselect", "dtplyr", "tibble"),
    function(pkg) paste(pkg, packageVersion(pkg)), character(1)), collapse = ", "), "\n")
direct <- check(function(q) names(tidyselect::eval_select(q, d)))
stopifnot(identical(direct, "b"), calls == 1L)
cat("Direct:", direct, "\n")
results <- list()
for (backend in c("local", "dtplyr")) {
  input <- if (backend == "local") d else dtplyr::lazy_dt(d)
  for (position in c(".by", "grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")) {
    for (consumer in c("inspect", "nest", "nest_by")) {
      for (spelling in c("bare", "wrapped", "walk", "qualified")) {
        calls <- 0L
        actual <- check(function(q) {
          env <- rlang::quo_get_env(q)
          wrapped <- rlang::call2("(", rlang::call2("(", quote(a - b)))
          expr <- switch(spelling, bare = quote(a - b), wrapped = wrapped,
                         walk = quote(c(a - b)), qualified = quote(base::`-`(a, b)))
          q <- rlang::new_quosure(expr, env)
          spec <- if (position == ".by") NULL else
            rlang::eval_tidy(rlang::call2(position, q))
          by <- if (position == ".by") q else rlang::quo(NULL)
          switch(consumer,
            inspect = {
              plan <- inspect_grouping(input, .by = !!by, .grouping = spec, .format = "list")
              if (position == ".by") plan$fixed[[1L]] else plan$included[[1L]]
            },
            nest = setdiff(names(dplyr::collect(nest_with_margins(input, .by = !!by, .grouping = spec))), "data"),
            nest_by = setdiff(names(dplyr::collect(nest_by_with_margins(input, .by = !!by, .grouping = spec))), "data"))
        })
        results[[length(results) + 1L]] <- data.frame(backend, position, consumer, spelling,
                                                     actual = paste(actual, collapse = ","), calls)
      }
    }
  }
}
out <- do.call(rbind, results)
print(with(out, table(backend, spelling, actual, calls)))
stopifnot(all(out$calls == 1L), all(out$actual[out$spelling == "qualified"] == direct))
if ("--assert" %in% args) stopifnot(all(out$actual == direct)) else stopifnot(
  all(out$actual[out$spelling != "qualified"] == "a")
)
