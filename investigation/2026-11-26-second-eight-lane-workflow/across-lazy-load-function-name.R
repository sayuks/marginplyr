# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A bare function name whose binding is an unforced lazy-load promise is not read as one function, so a share of its across() output is refused until the session has evaluated the function.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr)})
d <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30))
show <- function(e) tryCatch(print(as.data.frame(e)), error = function(c)
  cat("ERROR <", paste(class(c), collapse = "/"), ">:", conditionMessage(c), "\n"))
lazy <- function() cat("  [package:dplyr `n_distinct` is an unforced promise:",
  rlang::env_binding_are_lazy(as.environment("package:dplyr"), "n_distinct"), "]\n")

cat("Expected (contract + hand calculation): y_n = 2, 1, 3; s = 2/3, 1/3, 1 on every call\n")
lazy()
cat("\n1. Margin call, first time in this session:\n")
show(summarize_with_margins(d, across(c(x, y), n_distinct, .names = "{.col}_n"),
                            s = share_of_total(y_n), .grouping = rollup(g)))
cat("\n2. Default naming, share of the across() output `y`:\n")
show(summarize_with_margins(d, across(c(x, y), n_distinct), s = share_of_total(y),
                            .grouping = rollup(g)))
cat("\n3. Column-wise share of the across() output:\n")
show(summarize_with_margins(d, across(c(x, y), n_distinct, .names = "{.col}_n"),
                            across(y_n, share_of_total, .names = "{.col}_s"), .grouping = rollup(g)))
lazy()
cat("\n4. Native control: plain dplyr::summarise() at each explicit grain\n")
print(summarise(d, across(c(x, y), n_distinct, .names = "{.col}_n"), .by = g))
print(summarise(select(d, -g), across(c(x, y), n_distinct, .names = "{.col}_n")))
lazy()
cat("\n5. The call from step 1 again, unchanged:\n")
show(summarize_with_margins(d, across(c(x, y), n_distinct, .names = "{.col}_n"),
                            s = share_of_total(y_n), .grouping = rollup(g)))
cat("\n6. Arrow, renamed across() with bare `sd` (stats), before and after sd() has run once:\n")
at <- arrow::arrow_table(d)
cat("  [package:stats `sd` is an unforced promise:", rlang::env_binding_are_lazy(as.environment("package:stats"), "sd"), "]\n")
show(collect(summarize_with_margins(at, across(c(a = x), sd), .grouping = rollup(g))))
invisible(sd(c(1, 2)))
cat("  [package:stats `sd` is an unforced promise:", rlang::env_binding_are_lazy(as.environment("package:stats"), "sd"), "]\n")
show(collect(summarize_with_margins(at, across(c(a = x), sd), .grouping = rollup(g))))
cat("  native, data frame at each grain:\n")
print(summarise(d, across(c(a = x), sd), .by = g)); print(summarise(d, across(c(a = x), sd)))
