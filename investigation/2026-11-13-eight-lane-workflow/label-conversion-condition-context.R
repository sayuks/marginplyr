# Investigated SHA: 6165f4768400a28520cc8977e056da0db49a32ed
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A dimension whose class has no character form fails in a generated label step, and the condition blames an internal dplyr call.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
new_rational <- function(n, d) vctrs::new_rcrd(list(n = n, d = d), class = "vf_rational")
format.vf_rational <- function(x, ...) paste0(vctrs::field(x, "n"), "/", vctrs::field(x, "d"))
registerS3method("format", "vf_rational", format.vf_rational)
d <- tibble::tibble(r = new_rational(1:2, 3:4), v = c(1, 2))
show <- function(label, expr) {
  e <- tryCatch({ expr; NULL }, error = function(e) e)
  if (is.null(e)) return(cat(sprintf("%-42s returned\n", label)))
  lines <- strsplit(conditionMessage(e), "\n")[[1]]
  cat(sprintf("%-42s <%s> call: %s | %s\n", label, class(e)[1], substr(paste(deparse(conditionCall(e)), collapse = " "), 1, 60), substr(lines[1], 1, 70)))
}
cat("as a typed-missing label the class works:\n")
print(as.data.frame(summarize_with_margins(d, s = sum(v), .grouping = rollup(r), .margin_label = NA_character_)))
show("summarize, default check", summarize_with_margins(d, s = sum(v), .grouping = rollup(r)))
show("summarize, .check_margin_label = FALSE", summarize_with_margins(d, s = sum(v), .grouping = rollup(r), .check_margin_label = FALSE))
show("expand, default check", expand_with_margins(d, .grouping = rollup(r)))
show("nest, default check", nest_with_margins(d, .grouping = rollup(r)))
show("a caller summary that errors", summarize_with_margins(d, s = stop("boom"), .grouping = rollup(v), .margin_label = NULL))
show("native mutate(r = as.character(r))", mutate(d, r = as.character(r)))
show("native summarize() of the same check", dplyr::summarize(d, r = sum(dplyr::if_else(as.character(r) == "(all)", 1L, 0L, missing = 0L), na.rm = TRUE)))
