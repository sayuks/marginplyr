# Investigated SHA: 22043a461c5c48662ff54ea0a1238f00ac316059
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
suppressMessages({pkgload::load_all(".", quiet = TRUE); library(dplyr)})
d <- tibble::tibble(x = c(complex(real = NaN, imaginary = 0), NA, complex(real = NaN, imaginary = 0), NA, 1+0i, 1+0i),
                    g = c("a", "a", "b", "b", "a", "b"), v = c(1L, 2L, 4L, 8L, 16L, 32L))
cat("== native: NaN and NA complex keys are separate groups\n"); print(as.data.frame(summarise(d, s = sum(v), .by = x)))
cat("== complex fixed key, .sort = 'last'\n"); print(as.data.frame(summarize_with_margins(d, s = sum(v), .by = x, .grouping = rollup(g), .sort = "last")))
cat("== control: double fixed key (repaired by #1571)\n")
print(as.data.frame(summarize_with_margins(mutate(d, x = Re(x)), s = sum(v), .by = x, .grouping = rollup(g), .sort = "last")))
