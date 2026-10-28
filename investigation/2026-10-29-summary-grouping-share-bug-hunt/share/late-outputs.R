devtools::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
data <- data.frame(g = c("a", "b"), x = c(1, 3))
late <- function(x) tibble(s = 100 * sum(x))
late_p <- function(x) tibble(p = 100 * sum(x))
late_g <- function(x) tibble(g = 100 * sum(x))
late_set <- function(x) tibble(set = 100 * sum(x))
forms <- alist(late(x), tibble(s = 100 * sum(x)), late_p(x), tibble(p = 100 * sum(x)), late_g(x), late_set(x),
  across(x, ~ tibble(s = 100 * sum(.x)), .unpack = "{inner}"),
  across(x, sum, .names = paste0("", "s")),
  across(x, sum, .names = paste0("", "p")))
for (i in seq_along(forms)) for (before in c(TRUE, FALSE)) {
  expr <- forms[[i]]
  qs <- if (before) rlang::quos(s = sum(x), !!expr, p = share_of_total(s)) else
    rlang::quos(s = sum(x), p = share_of_total(s), !!expr)
  ans <- tryCatch(rlang::inject(summarize_with_margins(data, !!!qs,
    .grouping = rollup(g), .id = "set", .margin_label = NULL)), error = identity)
  cat("\nFORM", i, "before", before, paste(deparse(expr), collapse = " "), "\n")
  if (inherits(ans, "error")) cat(class(ans)[[1L]], conditionMessage(ans), "\n") else print(ans)
}
