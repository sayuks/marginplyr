# Dated follow-up evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else "."
path <- file.path(repo, "investigation", "2026-10-26-nesting-specification-followup",
                  "typed-selection-algebra.R")
generator <- new.env(parent = globalenv())
# Reuse the exact fixture, seed, and generator definitions before its test loop.
for (part in parse(path)) {
  if (rlang::is_call(part, "for")) break
  eval(part, generator)
}
constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
constructor_nodes <- function(expr) {
  if (!is.call(expr)) return(0L)
  parts <- as.list(expr)[-1L]
  children <- vapply(parts, constructor_nodes, integer(1))
  own <- as.integer(is.symbol(expr[[1L]]) && as.character(expr[[1L]]) %in% constructors)
  own + max(c(0L, children))
}
counts <- replicate(2000L, constructor_nodes(generator$random_tree()$expr))
stopifnot(max(counts) == 5L)
cat("ASTs measured:", length(counts), "; maximum constructor nodes on a path:",
    max(counts), "; maximum depth with root zero:", max(counts) - 1L, "\n")
