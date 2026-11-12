# Investigated SHA: 03e3cef502f7332b69e50608740033043d27f2b4
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A frame-level tsp attribute makes both nesting verbs fail where native nesting carries it.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
d <- data.frame(a = c("x", "x", "y"), b = c("p", "q", "p"), v = 1:3)
attr(d, "tsp") <- c(1, 3, 1)  # a valid frame-level attribute: 3 columns, start 1, end 3, frequency 1
run <- function(tag, expr) {
  out <- tryCatch({
    r <- dplyr::ungroup(dplyr::collect(expr))
    paste("ok; first cell attributes:", paste(names(attributes(r$data[[1]])), collapse = ", "))
  }, error = function(e) paste0("ERROR <", paste(class(e), collapse = "/"), "> in `",
                                deparse(conditionCall(e))[1], "`: ", conditionMessage(e)))
  cat(sprintf("%-44s %s\n", tag, out))
}
run("native tidyr::nest(d, .by = a)", tidyr::nest(d, .by = a))
run("native dplyr::nest_by(d)", dplyr::nest_by(d))
run("native tidyr::nest(lazy_dt(d), data = c(b, v))", tidyr::nest(dtplyr::lazy_dt(d), data = c(b, v)))
run("nest_with_margins(d, .by = a)", nest_with_margins(d, .by = a))
run("nest_with_margins(d, rollup(a))", nest_with_margins(d, .grouping = rollup(a)))
run("nest_by_with_margins(d)", nest_by_with_margins(d, .keep = FALSE, .grouping = rollup(a)))
run("nest_with_margins(lazy_dt(d), rollup(a))", nest_with_margins(dtplyr::lazy_dt(d), .grouping = rollup(a)))
run("nest_with_margins(tibble, rollup(a))", nest_with_margins(tibble::as_tibble(d), .grouping = rollup(a)))
run("nest_with_margins(d, rollup(a), .keep = TRUE)", nest_with_margins(d, .grouping = rollup(a), .keep = TRUE))
