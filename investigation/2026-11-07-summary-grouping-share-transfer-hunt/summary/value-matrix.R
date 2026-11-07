Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(commandArgs(trailingOnly = TRUE)[1L], quiet = TRUE)
library(dplyr)
data <- tibble(g = c(1L, 2L), x = c(1, 2))
values <- list(
  integer = 1L, double = 1.5, logical = NA, null = NULL,
  char = "a", raw = as.raw(1), complex = 1+2i,
  factor = factor("a"), date = as.Date("2026-01-01"),
  posixct = as.POSIXct("2026-01-01", tz = "UTC"),
  posixlt = as.POSIXlt("2026-01-01", tz = "UTC"),
  difftime = as.difftime(1, units = "hours"),
  matrix = matrix(1:2, nrow = 1), array = array(1:4, c(1,2,2)),
  list = list(1), listof = vctrs::list_of(1L),
  df = data.frame(y = 1), tibble = tibble(y = 1),
  emptydf = data.frame(row.names = 1L), unspecified = vctrs::unspecified(1L)
)
capture <- function(expr) tryCatch(suppressWarnings(force(expr)), error = identity)
results <- list()
for (first in names(values)) for (second in names(values)) {
  f <- function(x) if (x[[1L]] == 1) values[[first]] else values[[second]]
  a <- capture(summarise(data, f(x), .by = g))
  b <- capture(summarize_with_margins(data, f(x), .grouping = grouping_set(g)))
  same <- if (inherits(a,"error") || inherits(b,"error")) {
    inherits(a,"error") && inherits(b,"error")
  } else identical(select(a, -g), select(b, -g))
  if (!same) {
    results[[length(results) + 1L]] <- list(first=first,second=second,a=a,b=b)
    cat(first,second,"\n"); print(a); print(b)
  }
}
saveRDS(results, "/private/tmp/marginplyr-bughunt-20261107-summary/value-results.rds")
cat("discrepancies",length(results),"of",length(values)^2,"\n")
