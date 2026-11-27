# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# The unknown-source refusal says a source is "not a summary defined earlier" when a preceding across() produces it (#1810).
# Candidate 4: when a later unnamed across() produces an input column's name
# but its outputs are not known before the call runs, the unknown-source
# refusal says that name "is a column of the input, not a summary defined
# earlier in the call" and points at an unrelated summary.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages(library(dplyr))

d <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30))
cols <- c("x", "y")
refusal <- function(expr) tryCatch({
  r <- expr
  paste("accepted, s =", paste(round(r$s, 4), collapse = " "))
}, error = function(e) paste0("<", class(e)[[1L]], ">\n      ", gsub("\n", "\n      ", conditionMessage(e))))

cat("Expected: a refusal (the outputs are not known before the call runs) that does not\n",
    "claim `x` is not a summary defined earlier; #1793 counted this very hint, for an\n",
    "across() whose outputs were unknown, as false ('the refusal adds a false hint') and\n",
    "required it gone. Plain dplyr shows the across() defines `x`.\n\n", sep = "")

cases <- list(
  "n = n(), across(where(is.numeric), sum)" = quote(summarize_with_margins(
    d, n = n(), across(where(is.numeric), sum), s = share_of_total(x), .grouping = rollup(g))),
  "md = median(x), across(all_of(cols), sum)" = quote(summarize_with_margins(
    d, md = median(x), across(all_of(cols), sum), s = share_of_total(x), .grouping = rollup(g))),
  "n = n(), across(c(x, y), sum, .names = \"{sub('q', '', .col)}\")" = quote(summarize_with_margins(
    d, n = n(), across(c(x, y), sum, .names = "{sub('q', '', .col)}"), s = share_of_total(x),
    .grouping = rollup(g)))
)
natives <- list(
  quote(summarise(d, n = n(), across(where(is.numeric), sum), .by = g)),
  quote(summarise(d, md = median(x), across(all_of(cols), sum), .by = g)),
  quote(summarise(d, n = n(), across(c(x, y), sum, .names = "{sub('q', '', .col)}"), .by = g))
)
for (i in seq_along(cases)) {
  cat("--", names(cases)[[i]], ", s = share_of_total(x)\n", sep = "")
  cat("   observed: ", refusal(eval(cases[[i]])), "\n", sep = "")
  nat <- eval(natives[[i]])
  cat("   native summarise() columns: ", paste(names(nat), collapse = ", "),
      "; x = ", paste(nat$x, collapse = ", "), "\n\n", sep = "")
}

cat("The hinted remedy computes another quantity; writing the across() first is what works:\n")
cat("   share_of_total(n) as hinted: ", refusal(summarize_with_margins(
  d, n = n(), across(where(is.numeric), sum), s = share_of_total(n), .grouping = rollup(g))), "\n")
cat("   across() first:              ", refusal(summarize_with_margins(
  d, across(where(is.numeric), sum), n = n(), s = share_of_total(x), .grouping = rollup(g))), "\n")
cat("   by hand x / total x:         ", paste(round(c(3, 5, 8) / 8, 4), collapse = " "), "\n")
invisible(0)
