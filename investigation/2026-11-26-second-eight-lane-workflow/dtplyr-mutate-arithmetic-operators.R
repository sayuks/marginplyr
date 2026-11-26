# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A dtplyr mutate() whose arithmetic uses parentheses, a power, a modulus or an integer division is refused as unsafe metadata.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr); library(dtplyr); library(marginplyr)})
x <- data.frame(region = c("east", "west", "east"), n = c(1L, 2L, 3L), m = c(4L, 5L, 6L))
lx <- lazy_dt(x)
forms <- list(
  `n + m (control)` = quote(n + m),
  `(n + m)` = quote((n + m)),
  `(n + m) * 2L` = quote((n + m) * 2L),
  `n %/% 2L` = quote(n %/% 2L),
  `n %% 2L` = quote(n %% 2L),
  `n ^ 2` = quote(n^2)
)
for (nm in names(forms)) {
  step <- mutate(lx, z = !!forms[[nm]])
  planned <- tryCatch(
    paste(inspect_grouping(step, .grouping = rollup(where(is.integer)))$included, collapse = " | "),
    error = function(e) paste("REFUSED:", sub("\n.*", "", conditionMessage(e))))
  coll <- as_tibble(collect(step))
  ctl <- paste(inspect_grouping(coll, .grouping = rollup(where(is.integer)))$included, collapse = " | ")
  zero <- collect(head(step, 0))
  same_types <- identical(vapply(zero, typeof, ""), vapply(coll, typeof, ""))
  cat(sprintf("%-16s step: %s\n%-16s collected frame: %s   [type of z in collect(head(step,0)) == collect(step): %s (%s)]\n",
              nm, planned, "", ctl, same_types, typeof(coll$z)))
}
