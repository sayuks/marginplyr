# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# The grouping-set limit refusal prints its count as the decimal expansion of a double, which is not the count once the count is not exactly representable.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages({library(dplyr)})
cols <- c(paste0("a", 1:40), paste0("b", 1:40))
d <- as.data.frame(stats::setNames(lapply(cols, function(i) c("x", "y")), cols))
A <- rlang::syms(paste0("a", 1:40)); B <- rlang::syms(paste0("b", 1:40))
spec <- rlang::expr(grouping_spec(!!!lapply(1:40, function(i) rlang::expr(rollup(!!A[[i]], !!B[[i]])))))
# Exact 3^40 by schoolbook multiplication on decimal digits (independent of doubles).
mul_small <- function(digits, k) { carry <- 0; for (i in rev(seq_along(digits))) { v <- digits[i] * k + carry; digits[i] <- v %% 10; carry <- v %/% 10 }
  while (carry > 0) { digits <- c(carry %% 10, digits); carry <- carry %/% 10 }; digits }
dg <- 1; for (i in 1:40) dg <- mul_small(dg, 3)
exact <- paste(dg, collapse = "")
exact_fmt <- gsub("(\\d)(?=(\\d{3})+$)", "\\1,", exact, perl = TRUE)
cat("EXPECTED: either the exact count 3^40 =", exact_fmt, "or no count (as for an infinite one)\n")
for (dup in c("keep", "error")) {
  msg <- tryCatch({inspect_grouping(d, .grouping = !!spec, .duplicates = dup); "returned"}, error = function(e) strsplit(conditionMessage(e), "\n")[[1]][1])
  cat("OBSERVED (", dup, "): ", msg, "\n", sep = "")
}
cat("CONTROL : 2^60 (exact in a double), grouping_spec of 60 one-column rollups:\n")
spec2 <- rlang::expr(grouping_spec(!!!lapply(c(A, B)[1:60], function(s) rlang::expr(rollup(!!s)))))
cat("   ", tryCatch({inspect_grouping(d, .grouping = !!spec2); "returned"}, error = function(e) strsplit(conditionMessage(e), "\n")[[1]][1]), "\n")
cat("   2^60 exact = 1,152,921,504,606,846,976\n")
invisible(0)
